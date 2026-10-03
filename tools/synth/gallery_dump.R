# gallery_dump.R -- for each case, record what the automatic reader picked
# (columns per page, roles), what it decided and why, which checks
# failed, and where its figures differ from the answer key.
#   Rscript tools/synth/gallery_dump.R <engine_dir> <cases.tsv> <out_dir>
# cases.tsv: case, set, kind, path, cache (a saved read_input() .rds, or empty).
# Then: python3 tools/synth/gallery_draw.py <out_dir> <png_dir>
suppressMessages({ library(yaml); library(jsonlite) })
a <- commandArgs(TRUE); src <- a[1]; cases <- read.delim(a[2], stringsAsFactors = FALSE); out <- a[3]
Sys.setenv(ENGINE_ROOT = src)
owd <- setwd(src)
for (f in list.files("R", "[.]R$", full.names = TRUE)) source(f)
source("tools/synth/truth.R")
setwd(owd)
key <- function(d, v) paste(ifelse(is.na(d), "NA", d), ifelse(is.na(v), "NA", sprintf("%.2f", round(v, 2))), sep = "|")
align <- function(x, y) {       # LCS alignment: which of x and y are matched
  n <- length(x); m <- length(y); L <- matrix(0L, n + 1L, m + 1L)
  for (i in seq_len(n)) for (j in seq_len(m))
    L[i + 1L, j + 1L] <- if (x[i] == y[j]) L[i, j] + 1L else max(L[i, j + 1L], L[i + 1L, j])
  mx <- rep(FALSE, n); my <- rep(FALSE, m); i <- n; j <- m
  while (i > 0 && j > 0) {
    if (x[i] == y[j]) { mx[i] <- TRUE; my[j] <- TRUE; i <- i - 1L; j <- j - 1L }
    else if (L[i, j + 1L] >= L[i + 1L, j]) i <- i - 1L else j <- j - 1L
  }
  list(x = mx, y = my)
}
for (k in seq_len(nrow(cases))) {
  cs <- cases$case[k]; path <- cases$path[k]; cache <- cases$cache[k]
  tr <- read_truth(sub("\\.(pdf|csv|xlsx)$", ".truth.json", path))
  input <- if (nzchar(cache) && file.exists(cache)) readRDS(cache) else read_input(path)
  rd <- tryCatch(auto_read(input), error = function(e) list(outcome = "unread", why = conditionMessage(e)))
  tx <- rd$transactions
  got_d <- if (is.data.frame(tx) && nrow(tx)) as.character(tx$date) else character(0)
  got_a <- if (is.data.frame(tx) && nrow(tx)) suppressWarnings(as.numeric(tx$amount)) else numeric(0)
  want_d <- vapply(tr$want, function(r) r$date %||% NA_character_, "")
  want_a <- vapply(tr$want, function(r) r$amount, 0)
  al <- align(key(want_d, want_a), key(got_d, got_a))
  chk <- rd$checks
  failed <- if (is.data.frame(chk) && nrow(chk)) chk[!chk$ok, c("check", "why"), drop = FALSE] else data.frame()
  cols <- rd$columns
  rec <- list(case = cs, set = cases$set[k], path = path, kind = cases$kind[k],
    outcome = rd$outcome, why = rd$why, failed_checks = failed,
    columns = if (is.data.frame(cols)) cols else data.frame(),
    page_width = input$page_width, page_height = input$page_height,
    proof = rd$proof[setdiff(names(rd$proof %||% list()), "detail")],
    candidates = if (is.data.frame(rd$candidates)) rd$candidates else data.frame(),
    want_n = length(want_a), got_n = length(got_a), right = sum(al$x),
    missing = data.frame(row = which(!al$x), date = want_d[!al$x], amount = want_a[!al$x],
                         description = vapply(tr$want[!al$x], function(r) r$description %||% "", "")),
    extra = data.frame(row = which(!al$y), date = got_d[!al$y], amount = got_a[!al$y],
                       description = if (length(got_a)) as.character(tx$description)[!al$y] else character(0),
                       flags = if (length(got_a) && "flags" %in% names(tx)) as.character(tx$flags)[!al$y] else character(0)),
    truth_note = tr$note %||% "", features = tr$features %||% list())
  writeLines(toJSON(rec, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null"),
             file.path(out, paste0(cs, ".json")))
  cat(cs, rd$outcome, sprintf("right %d/%d", sum(al$x), length(want_a)), "\n")
}
