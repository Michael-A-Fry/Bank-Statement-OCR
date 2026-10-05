# score_auto.R -- score the AUTOMATIC reader against a test set's answer key.
#
#   Rscript tools/synth/score_auto.R <dir> [--mode cold|trained] [--out results.csv] [--only SUBSTR]
#
# The definitions below are fixed BEFORE the reader is built, so the reader is
# measured, not the measuring stick adjusted to suit it. Do not change what counts
# as right or wrong here to make a number move; change the reader.
#
# Every statement lands in exactly one cell of the outcome matrix:
#   auto_right   the reader said "proven" or "layout_match" and every figure is right
#   AUTO_WRONG   the reader said "proven" or "layout_match" and ANY figure is wrong,
#                missing or extra. This must be ZERO. It is the silent failure.
#   check_right  "check": a person is asked; the reading on screen is right
#   check_wrong  "check": a person is asked; the reading on screen needs fixing
#   unread       "unread": nothing read; the reason is shown
#
# A figure is right when the (date, signed amount) pair matches the answer key in
# printed order: the longest common subsequence of the two lists of pairs, so a
# missing row, an extra row and a wrong figure all count against it. Redacted
# figures are NA on both sides and must be NA in the reading too.
#
# Statements the answer key marks `decidable: false` (the green-flag set) can only
# be answered correctly by asking: an "auto" outcome on one is AUTO_WRONG even when
# the figures happen to be right, because the reader guessed.
#
# --mode cold     every statement alone, with nothing learned (the first time a
#                 layout is ever seen).
# --mode trained  statements are read bank by bank, layout family by family, in
#                 file-name order, and every PROVEN reading's layout is handed to the
#                 next reading as a learned layout -- the way the tool is used after
#                 "upload all your statements for this bank".
suppressMessages({ library(yaml); library(jsonlite) })
Sys.setenv(ENGINE_ROOT = normalizePath("."))
for (f in list.files("R", "[.]R$", full.names = TRUE)) source(f)
source("tools/synth/truth.R")

a <- commandArgs(TRUE)
opt <- function(name, dflt) { i <- match(name, a); if (is.na(i) || i == length(a)) dflt else a[i + 1L] }
dir <- a[1]; mode <- opt("--mode", "cold"); outcsv <- opt("--out", ""); only <- opt("--only", "")

.key <- function(d, v) paste(ifelse(is.na(d), "NA", d),
                             ifelse(is.na(v), "NA", sprintf("%.2f", round(v, 2))), sep = "|")
.lcs <- function(x, y) {
  n <- length(x); m <- length(y); if (!n || !m) return(0L)
  prev <- integer(m + 1L)
  for (i in seq_len(n)) { cur <- integer(m + 1L)
    for (j in seq_len(m)) cur[j + 1L] <- if (x[i] == y[j]) prev[j] + 1L else max(prev[j + 1L], cur[j])
    prev <- cur }
  prev[m + 1L]
}

score_one <- function(path, layouts) {
  cs <- sub("\\.(pdf|csv|xlsx|xls)$", "", basename(path))
  tr <- read_truth(file.path(dir, paste0(cs, ".truth.json")))
  want <- .key(vapply(tr$want, function(r) r$date %||% NA_character_, ""),
               vapply(tr$want, function(r) r$amount, 0))
  decidable <- !identical(tr$decidable, FALSE)
  t0 <- Sys.time()
  rd <- tryCatch(auto_read(read_input(path), layouts = layouts),
                 error = function(e) list(outcome = "unread", why = paste("ERROR:", conditionMessage(e))))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  tx <- rd$transactions
  got <- if (is.data.frame(tx) && nrow(tx))
    .key(as.character(tx$date), suppressWarnings(as.numeric(tx$amount))) else character(0)
  m <- .lcs(want, got)
  perfect <- m == length(want) && length(got) == m
  auto <- (rd$outcome %||% "unread") %in% c("proven", "layout_match")
  cell <- if (auto && perfect && decidable) "auto_right"
          else if (auto) "AUTO_WRONG"
          else if (identical(rd$outcome, "check")) (if (perfect) "check_right" else "check_wrong")
          else "unread"
  list(row = data.frame(case = cs,
    kind = if (grepl("_scan$", cs)) "scan" else sub(".*\\.", "", path),
    bank = tr$bank %||% NA_character_, layout = tr$layout %||% NA_character_,
    decidable = decidable, outcome = rd$outcome %||% NA_character_, cell = cell,
    want = length(want), got = length(got), right = m, missing = length(want) - m,
    extra = length(got) - m, secs = round(secs, 2),
    why = substr(gsub("[\r\n]+", " ", rd$why %||% ""), 1, 140), stringsAsFactors = FALSE),
    reading = rd)
}

files <- list.files(dir, "\\.(pdf|csv|xlsx|xls)$", full.names = TRUE)
files <- files[file.exists(sub("\\.(pdf|csv|xlsx|xls)$", ".truth.json", files))]
if (nzchar(only)) files <- files[grepl(only, basename(files), fixed = TRUE)]
rows <- list()
if (identical(mode, "trained")) {
  fam <- vapply(files, function(f) {
    tr <- jsonlite::fromJSON(sub("\\.(pdf|csv|xlsx|xls)$", ".truth.json", f), simplifyVector = FALSE)
    paste(tr$bank %||% "?", sep = "/")
  }, "")
  for (b in sort(unique(fam))) {
    layouts <- list()
    for (f in sort(files[fam == b])) {
      s <- score_one(f, layouts)
      rows[[length(rows) + 1L]] <- s$row
      rd <- s$reading
      if (identical(rd$outcome, "proven") && !is.null(rd$template) && is.null(rd$matched_layout))
        layouts[[length(layouts) + 1L]] <- rd$template
    }
  }
} else {
  rows <- parallel::mclapply(files, function(f) score_one(f, list())$row,
                             mc.cores = max(1L, min(3L, parallel::detectCores() - 1L)))
}
out <- do.call(rbind, rows)
if (nzchar(outcsv)) utils::write.csv(out, outcsv, row.names = FALSE)
cells <- c("auto_right", "AUTO_WRONG", "check_right", "check_wrong", "unread")
cat(sprintf("\n%s  mode=%s  statements=%d\n", basename(dir), mode, nrow(out)))
for (k in sort(unique(out$kind))) {
  x <- out[out$kind == k, ]
  n <- table(factor(x$cell, levels = cells))
  cat(sprintf("  %-5s n=%3d  auto_right=%3d (%5.1f%%)  AUTO_WRONG=%d  check_right=%d  check_wrong=%d  unread=%d\n",
    k, nrow(x), n[["auto_right"]], 100 * n[["auto_right"]] / nrow(x), n[["AUTO_WRONG"]],
    n[["check_right"]], n[["check_wrong"]], n[["unread"]]))
}
bad <- out[out$cell == "AUTO_WRONG", ]
if (nrow(bad)) { cat("\nAUTO_WRONG (must be zero):\n"); print(bad[, c("case", "outcome", "want", "got", "right", "why")], row.names = FALSE) }
invisible(out)
