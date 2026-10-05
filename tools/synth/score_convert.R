# score_convert.R -- score CONVERSION end to end against a test set's answer key.
#
#   Rscript tools/synth/score_convert.R <dir> [--mode cold|trained] [--out results.csv]
#                                             [--only SUBSTR] [--skip SUBSTR]
#
# The same outcome cells as tools/synth/score_auto.R, measured one level up: each
# statement goes through convert_statement() -- the bank, the learned-layout store,
# the reader, the outputs -- and the figures scored are the ones in the CSV it
# WRITES, read back from disk. So what is scored is what an analyst downloads.
#
#   auto_right   status "ok" and every figure in the CSV is right
#   AUTO_WRONG   status "ok" and ANY figure is wrong, missing or extra. Must be ZERO.
#   check_right  "needs_review": a person is asked; the figures written are right
#   check_wrong  "needs_review": a person is asked; the figures need fixing
#   unread       "unsupported" or "failed": nothing written; the reason is shown
#
# A figure is right when the (date, signed amount) pair matches the answer key in
# printed order (longest common subsequence, as score_auto.R). Statements the key
# marks `decidable: false` can only be answered right by asking.
#
# The bank is the one the answer key names, given as the person's pick.
#
# --mode cold     every statement alone, with an empty layout store.
# --mode trained  bank by bank, in file-name order, through ONE temporary layout
#                 store per bank: every conversion learns exactly as it would on
#                 the server (provisional layouts, promoted after three proofs
#                 from at least two different accounts).
suppressMessages({ library(yaml); library(jsonlite) })
Sys.setenv(ENGINE_ROOT = normalizePath("."))
for (f in list.files("R", "[.]R$", full.names = TRUE)) source(f)
source("tools/synth/truth.R")

a <- commandArgs(TRUE)
opt <- function(name, dflt) { i <- match(name, a); if (is.na(i) || i == length(a)) dflt else a[i + 1L] }
dir <- a[1]; mode <- opt("--mode", "cold"); outcsv <- opt("--out", "")
only <- opt("--only", ""); skip <- opt("--skip", "")

# A zero is 0.00 whichever column printed it: the answer key writes a money-out
# zero as -0, and a file must never show -0.00 (counted separately, below).
.key <- function(d, v) paste(ifelse(is.na(d), "NA", d),
                             ifelse(is.na(v), "NA", sprintf("%.2f", round(v, 2) + 0)), sep = "|")
.lcs <- function(x, y) {
  n <- length(x); m <- length(y); if (!n || !m) return(0L)
  prev <- integer(m + 1L)
  for (i in seq_len(n)) { cur <- integer(m + 1L)
    for (j in seq_len(m)) cur[j + 1L] <- if (x[i] == y[j]) prev[j] + 1L else max(prev[j + 1L], cur[j])
    prev <- cur }
  prev[m + 1L]
}

work <- tempfile("score_convert_")
dir.create(work)
on.exit(unlink(work, recursive = TRUE), add = TRUE)

# .written(res) -- the (date, amount) pairs in the CSV the conversion wrote, read
# back as text so nothing is re-typed on the way in.
.written <- function(res) {
  p <- if ("csv" %in% names(res$outputs)) res$outputs[["csv"]] else NA_character_
  if (is.na(p) || !file.exists(p)) return(list(keys = character(0), negzero = 0L))
  x <- utils::read.csv(p, colClasses = "character", check.names = FALSE)
  if (!nrow(x)) return(list(keys = character(0), negzero = 0L))
  am <- x$amount
  list(keys = .key(ifelse(nzchar(x$date), x$date, NA), suppressWarnings(as.numeric(am))),
       negzero = sum(grepl("^-0([.]0+)?$", am)))
}

score_one <- function(path, ldir) {
  cs <- sub("\\.(pdf|csv|xlsx|xls)$", "", basename(path))
  tr <- read_truth(file.path(dir, paste0(cs, ".truth.json")))
  want <- .key(vapply(tr$want, function(r) r$date %||% NA_character_, ""),
               vapply(tr$want, function(r) r$amount, 0))
  decidable <- !identical(tr$decidable, FALSE)
  out <- file.path(work, "out", cs)
  t0 <- Sys.time()
  res <- tryCatch(convert_statement(path, bank = tr$bank, outdir = out, logdir = file.path(work, "logs"),
                                    formats = "csv", layouts_dir = ldir, tracking_dir = NA,
                                    requested_by = "score_convert"),
                  error = function(e) list(status = "failed", reason = paste("ERROR:", conditionMessage(e))))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  got <- .written(res)
  m <- .lcs(want, got$keys)
  perfect <- m == length(want) && length(got$keys) == m
  st <- res$status %||% "failed"
  cell <- if (identical(st, "ok") && perfect && decidable) "auto_right"
          else if (identical(st, "ok")) "AUTO_WRONG"
          else if (identical(st, "needs_review")) (if (perfect) "check_right" else "check_wrong")
          else "unread"
  data.frame(case = cs,
    kind = if (grepl("_scan$", cs)) "scan" else sub(".*\\.", "", path),
    bank = tr$bank %||% NA_character_, decidable = decidable, status = st,
    outcome = res$outcome %||% NA_character_, basis = res$feed_basis %||% NA_character_,
    layout = res$run_log$layout %||% NA_character_, learn = res$run_log$learn_action %||% NA_character_,
    cell = cell, want = length(want), got = length(got$keys), right = m,
    missing = length(want) - m, extra = length(got$keys) - m, negzero = got$negzero,
    secs = round(secs, 2),
    why = substr(gsub("[\r\n]+", " ", res$reason %||% ""), 1, 140), stringsAsFactors = FALSE)
}

files <- list.files(dir, "\\.(pdf|csv|xlsx|xls)$", full.names = TRUE)
files <- files[file.exists(sub("\\.(pdf|csv|xlsx|xls)$", ".truth.json", files))]
if (nzchar(only)) files <- files[grepl(only, basename(files), fixed = TRUE)]
if (nzchar(skip)) files <- files[!grepl(skip, basename(files), fixed = TRUE)]
bank_of <- vapply(files, function(f)
  jsonlite::fromJSON(sub("\\.(pdf|csv|xlsx|xls)$", ".truth.json", f))$bank %||% "?", "")
rows <- list()
if (identical(mode, "trained")) {
  for (b in sort(unique(bank_of))) {
    ldir <- file.path(work, "layouts", .layout_slug(b))
    for (f in sort(files[bank_of == b])) rows[[length(rows) + 1L]] <- score_one(f, ldir)
  }
} else {
  rows <- parallel::mclapply(seq_along(files), function(i)
    score_one(files[i], file.path(work, "layouts", paste0("cold", i))),
    mc.cores = max(1L, min(3L, parallel::detectCores() - 1L)))
}
out <- do.call(rbind, rows)
if (nzchar(outcsv)) utils::write.csv(out, outcsv, row.names = FALSE)
cells <- c("auto_right", "AUTO_WRONG", "check_right", "check_wrong", "unread")
cat(sprintf("\n%s  convert  mode=%s  statements=%d\n", basename(dir), mode, nrow(out)))
for (k in sort(unique(out$kind))) {
  x <- out[out$kind == k, ]
  n <- table(factor(x$cell, levels = cells))
  cat(sprintf("  %-5s n=%3d  auto_right=%3d (%5.1f%%)  AUTO_WRONG=%d  check_right=%d  check_wrong=%d  unread=%d\n",
    k, nrow(x), n[["auto_right"]], 100 * n[["auto_right"]] / nrow(x), n[["AUTO_WRONG"]],
    n[["check_right"]], n[["check_wrong"]], n[["unread"]]))
}
cat(sprintf("  -0.00 written: %d\n", sum(out$negzero)))
bad <- out[out$cell == "AUTO_WRONG", ]
if (nrow(bad)) { cat("\nAUTO_WRONG (must be zero):\n"); print(bad[, c("case", "outcome", "want", "got", "right", "why")], row.names = FALSE) }
invisible(out)
