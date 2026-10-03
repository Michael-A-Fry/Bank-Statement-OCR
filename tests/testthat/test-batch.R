# Tests for batch conversion (R/batch.R) -- a whole case folder converted in one
# unattended run, one row per file.
#
# The failures these exist to prevent:
#   * one unreadable file costing the other twenty-nine their conversion;
#   * a row that looks converted when it was not (a layout beside a file nothing
#     read);
#   * a bank chosen for one file landing on its neighbour;
#   * `failing_check` splitting one kind of failure across several strings, which
#     is the one thing that makes the column worth having.

# ---- fixtures: CSV only, so nothing here needs poppler/tesseract -------------
.b_write <- function(dir, name, lines) { p <- file.path(dir, name); writeLines(lines, p); p }

# A table that is not a statement: rows of figures with no date anywhere.
.B_UNKNOWN <- c("colA;colB;colC", "1;2;3")

# .b_run(paths, ...) -- convert a batch into throwaway folders, so a test never
# writes into the repo, the real run log or the real layout store.
.b_run <- function(paths, ...) {
  d <- tempfile("batch_"); dir.create(d)
  b <- convert_batch(paths, outdir = file.path(d, "out"), logdir = file.path(d, "logs"),
                     layouts_dir = file.path(d, "layouts"), tracking_dir = NA,
                     formats = "csv", ...)
  attr(b, "logdir") <- file.path(d, "logs")
  attr(b, "layouts") <- file.path(d, "layouts")
  b
}

# The shipped wording maps. `failing_check` carries engine CODES and the SCREEN
# words them, so what these tests hold is the join: every code this file can emit
# is a key of the map its prefix names. ui_labels.R stays free to be reworded.
.b_labels <- function() {
  f <- file.path(engine_root(), "ui_labels.R")
  expect_true(file.exists(f))
  e <- new.env(parent = globalenv())
  sys.source(f, envir = e)
  e
}

.b_case <- function() {
  dir <- tempfile(); dir.create(dir)
  file.copy(proven_csv(), file.path(dir, "a_bnz.csv"))
  .b_write(dir, "b_unknown.csv", .B_UNKNOWN)
  file.copy(unproven_csv(), file.path(dir, "c_unproven.csv"))
  dir
}

test_that("an unreadable file becomes a row and never stops the rest", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # the broken file goes FIRST, so a batch that stops at the first problem fails
  # this test rather than quietly passing it.
  paths <- c(file.path(dir, "not_here_at_all.csv"), file.path(dir, "a_bnz.csv"))
  b <- .b_run(paths)
  expect_equal(nrow(b), 2L)
  expect_identical(b$status, c("failed", "ok"))
  expect_equal(b$rows[1], 0L)
  expect_false(is.na(b$failing_check[1]))            # the reason is on the row...
  expect_true(grepl("not_here_at_all", b$message[1])) # ...and names the file
  expect_gt(b$rows[2], 0L)                            # the good file still ran
})

test_that("every file goes through the front door: one run-log record each", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- c(file.path(dir, "a_bnz.csv"), file.path(dir, "b_unknown.csv"),
             file.path(dir, "gone.csv"))
  b <- .b_run(paths)
  expect_equal(length(list.files(file.path(attr(b, "logdir"), "runs"))), 3L)
  expect_equal(length(unique(vapply(b$result, function(r) r$run_id %||% "", ""))), 3L)
})

test_that("a converted file reports its bank, outcome and row count", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(file.path(dir, "a_bnz.csv"))
  expect_identical(b$status, "ok")
  expect_identical(b$outcome, "proven")
  expect_identical(b$bank, "bnz")                     # pre-filled from the statement
  expect_true(is.na(b$chosen))                        # nobody chose it
  expect_true(is.na(b$layout))                        # first of its kind: nothing to match
  expect_equal(b$rows, 5L)
  expect_true(is.na(b$failing_check))                 # nothing went wrong -> nothing to say
  # ...and the second statement of the same design is read with the layout the
  # first one started
  b2 <- convert_batch(c(file.path(dir, "a_bnz.csv"), proven_csv(nz_test_account("02", "0018", "01"))),
                      outdir = tempfile(), logdir = tempfile(), layouts_dir = tempfile("ly_"),
                      tracking_dir = NA, formats = "csv")
  expect_identical(b2$status, c("ok", "ok"))
})

test_that("nothing read is never shown beside a layout", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(file.path(dir, "b_unknown.csv"))
  expect_identical(b$status, "unsupported")
  expect_identical(b$outcome, "unread")
  expect_true(is.na(b$layout))
  expect_equal(b$rows, 0L)
})

test_that("each file can be read as the bank chosen for it; the rest are pre-filled", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- file.path(dir, c("a_bnz.csv", "c_unproven.csv"))
  b <- .b_run(paths, banks = c(NA, "ANZ"))
  expect_true(is.na(b$chosen[1]))
  expect_identical(b$bank[1], "bnz")
  expect_identical(b$chosen[2], "ANZ")
  expect_identical(b$bank[2], "anz")
  # an empty choice means pre-fill, the same as no choice at all
  b2 <- .b_run(paths, banks = c("", NA))
  expect_true(all(is.na(b2$chosen)))
})

test_that("a bank list or fix list that does not line up with the files is refused", {
  expect_error(convert_batch(c("a.csv", "b.csv"), banks = "ANZ"), "2 files")
  expect_error(convert_batch(c("a.csv", "b.csv"), overrides = list(NULL)), "2 files")
})

test_that("a fix is handed to its own file only", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  amb <- ambiguous_csv()
  paths <- c(amb, amb)
  b <- .b_run(paths, banks = c("Rimu Bank", "Rimu Bank"),
              overrides = list(list(roles = c(debit = "debit", credit = "credit")), NULL))
  expect_identical(b$result[[1]]$person$fix, "roles")
  # file 2 has no fix of its own, but the fix file 1 proved was learned at once,
  # so the same design now converts on its own
  expect_true(is.na(b$result[[2]]$person$fix))
  expect_identical(b$status, c("ok", "ok"))
})

# ---------------------------------------------------------------------------
# failing_check -- the column the whole feature is for
# ---------------------------------------------------------------------------

test_that("files that failed the same way carry the identical failing_check", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .b_write(dir, "d_unknown.csv", .B_UNKNOWN)
  paths <- c(file.path(dir, "b_unknown.csv"), file.path(dir, "a_bnz.csv"),
             file.path(dir, "d_unknown.csv"))
  b <- .b_run(paths)
  expect_identical(b$failing_check[1], b$failing_check[3])
  sorted <- b[order(b$failing_check, na.last = TRUE), ]
  expect_identical(sorted$failing_check[1], sorted$failing_check[2])
})

test_that("failing_check carries the engine code, not a sentence", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(c(file.path(dir, "c_unproven.csv"), file.path(dir, "gone.csv")), banks = c("ANZ", NA))
  # the reader's own check that stopped the proof
  expect_match(b$failing_check[1], "^reading:[a-z_]+$")
  expect_true(sub("^reading:", "", b$failing_check[1]) %in% TRACK_CHECKS)
  expect_identical(b$failing_check[2], "diag:unreadable")
})

test_that("the reader's failed check beats a reconciliation check and a diagnostic", {
  res <- list(status = "needs_review",
              reading = list(list(outcome = "check",
                                  checks = data.frame(check = c("balance_chain", "unique"), ok = c(TRUE, FALSE),
                                                      why = "", stringsAsFactors = FALSE))),
              kpis = data.frame(name = "balance_reconciliation", status = "fail", stringsAsFactors = FALSE))
  expect_identical(.failing_check(res), "reading:unique")
  # a proven statement of a bundle says nothing; the one that did not prove speaks
  res$reading <- c(list(list(outcome = "proven", checks = data.frame(check = "x", ok = FALSE, why = ""))), res$reading)
  expect_identical(.failing_check(res), "reading:unique")
})

test_that("a bundle's per-statement tag is stripped so grouping still works", {
  res <- list(status = "needs_review",
              kpis = data.frame(name = "balance_reconciliation [statement 2]",
                                status = "fail", stringsAsFactors = FALSE))
  expect_identical(.failing_check(res), "check:balance_reconciliation")
})

test_that("a failing check wins over a diagnostic, and a diagnostic over the status", {
  kpis <- data.frame(name = c("dates_readable", "balance_reconciliation"),
                     status = c("pass", "fail"), stringsAsFactors = FALSE)
  diag <- data.frame(category = c("row_parse", "ocr"), severity = c("high", "info"),
                     stringsAsFactors = FALSE)
  expect_identical(.failing_check(list(status = "needs_review", kpis = kpis, diagnostics = diag)),
                   "check:balance_reconciliation")
  kpis$status <- c("pass", "pass")
  expect_identical(.failing_check(list(status = "needs_review", kpis = kpis, diagnostics = diag)),
                   "diag:row_parse")
  info_only <- data.frame(category = "none", severity = "info", stringsAsFactors = FALSE)
  expect_identical(.failing_check(list(status = "needs_review", kpis = kpis, diagnostics = info_only)),
                   "status:needs_review")
})

test_that("every prefix names a wording map, and the code is a key of it", {
  # THE SEAM. batch.R emits codes; the screen turns them into sentences (ui_labels.R).
  # If a prefix named no map, or carried a code that map has never heard of, the
  # "What to check" column would show a raw snake_case code.
  L <- .b_labels()
  # Every hard check the reader can fail (the tracking allowlist, plus the
  # spreadsheet reader's own agreement check).
  emitted <- c(
    paste0("reading:", union(TRACK_CHECKS, "reader_agrees")),
    .failing_check(list(status = "needs_review",
                        kpis = data.frame(name = "balance_reconciliation", status = "fail",
                                          stringsAsFactors = FALSE))),
    paste0("diag:", c("not_read", "not_proven", "unreadable")),
    .failing_check(list(status = "needs_review")))
  maps <- c(reading = "READING_CHECK_PLAIN", check = "CHECK_PLAIN", diag = "DIAG_PLAIN", status = "STATUS_PLAIN")
  kinds <- sub(":.*$", "", emitted)
  expect_true(all(kinds %in% names(maps)))
  unworded <- emitted[!vapply(seq_along(emitted), function(i)
    sub("^[^:]*:", "", emitted[i]) %in% names(L[[maps[[kinds[i]]]]] %||% list()), logical(1))]
  expect_identical(unworded, character(0))
})

# ---------------------------------------------------------------------------
# What the `result` column holds
# ---------------------------------------------------------------------------

test_that("the result column is the whole result, transaction rows included", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(file.path(dir, "a_bnz.csv"))
  r <- b$result[[1]]
  expect_true(is.data.frame(r$feed_rows))
  expect_equal(nrow(r$feed_rows), b$rows)
  expect_null(r$dropped_feed_rows)
  expect_true(any(grepl("\\.csv$", r$outputs)))   # and the rows are on disk too
})

test_that("the app no longer asks convert_batch to keep the rows", {
  # THE OTHER HALF OF THE SAME CHANGE, and it fails loudly if only one half lands.
  # convert_batch has no `keep_rows` formal now, and convert_document() has no
  # `...`, so a stale `keep_rows = TRUE` is forwarded into it and EVERY file in
  # the case comes back "failed: unused argument". Nothing else in the suite runs
  # the app's batch path, so this seam is the only thing that would notice.
  expect_false("keep_rows" %in% names(formals(convert_batch)))
  # This used to grep app.R for one exact call spelling. When the batch moved into
  # a child process the call moved to R/jobs.R, the grep found nothing, and the
  # guard did not fail loudly -- it errored on `src[integer(0)]`, which reads as a
  # broken test rather than a broken seam. Ask the invariant of every caller
  # instead, wherever it lives, so moving the call cannot silently disarm this.
  roots <- c(file.path(engine_root(), "app.R"),
             list.files(file.path(engine_root(), "R"), "\\.R$", full.names = TRUE))
  callers <- Filter(function(f) any(grepl("convert_batch(", readLines(f, warn = FALSE),
                                         fixed = TRUE)), roots)
  expect_gt(length(callers), 0L)          # if nobody calls it, this guard is asleep
  for (f in callers) {
    src <- readLines(f, warn = FALSE)
    for (i in grep("convert_batch(", src, fixed = TRUE)) {
      blk <- src[i:min(length(src), i + 16L)]
      expect_false(any(grepl("keep_rows", blk, fixed = TRUE)),
                   info = sprintf("%s:%d forwards keep_rows into convert_batch",
                                  basename(f), i))
    }
  }
})

# ---------------------------------------------------------------------------
# Usable from a console, deterministic, and safe to print
# ---------------------------------------------------------------------------

test_that("progress is a plain callback, called once per file in order", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- c(file.path(dir, "a_bnz.csv"), file.path(dir, "b_unknown.csv"))
  seen <- list()
  .b_run(paths, progress = function(i, n, file) seen[[length(seen) + 1L]] <<- list(i, n, file))
  expect_equal(length(seen), 2L)
  expect_equal(vapply(seen, function(x) x[[1]], numeric(1)), c(1, 2))
  expect_true(all(vapply(seen, function(x) x[[2]], numeric(1)) == 2))
  expect_identical(vapply(seen, function(x) x[[3]], ""), paths)
})

test_that("a broken progress callback does not cost the case its run", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- c(file.path(dir, "a_bnz.csv"), file.path(dir, "b_unknown.csv"))
  b <- .b_run(paths, progress = function(i, n, file) stop("the progress bar broke"))
  expect_equal(nrow(b), 2L)
  expect_identical(b$status, c("ok", "unsupported"))
})

test_that("R/batch.R calls no Shiny function, so a console run needs none", {
  # Comments are stripped first: the constraint is about the CODE, and the
  # comments have to be free to explain why the callback exists.
  code <- sub("#.*$", "", readLines(file.path(engine_root(), "R", "batch.R"), warn = FALSE))
  expect_false(any(grepl("shiny", code, ignore.case = TRUE)))
})

test_that("same inputs give the same rows in the same order", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- c(file.path(dir, "a_bnz.csv"), file.path(dir, "b_unknown.csv"))
  cols <- c("file", "status", "outcome", "bank", "chosen", "layout", "rows", "trust",
            "failing_check", "message")
  # everything the screen shows is identical run to run; only the run ids and
  # timestamps INSIDE the result vary, which is by design.
  expect_identical(.b_run(paths)[, cols], .b_run(paths)[, cols])
})

# ---------------------------------------------------------------------------
# batch_summary
# ---------------------------------------------------------------------------

test_that("batch_summary counts every file and hides no status", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- c(file.path(dir, "a_bnz.csv"), file.path(dir, "b_unknown.csv"),
             file.path(dir, "gone.csv"))
  s <- batch_summary(.b_run(paths))
  expect_identical(names(s), c("status", "n"))
  expect_equal(sum(s$n), length(paths))            # nothing uncounted
  expect_equal(s$n[s$status == "ok"], 1L)
  expect_equal(s$n[s$status == "unsupported"], 1L)
  expect_equal(s$n[s$status == "failed"], 1L)
  # every status is listed even at zero, so the printed shape never changes
  expect_true(all(BATCH_STATUSES %in% s$status))
  expect_equal(s$n[s$status == "needs_review"], 0L)
  expect_identical(s$status[seq_along(BATCH_STATUSES)], BATCH_STATUSES)
})

test_that("batch_summary appends an unexpected status rather than dropping it", {
  s <- batch_summary(data.frame(status = c("ok", "something_new", NA),
                                stringsAsFactors = FALSE))
  expect_equal(sum(s$n), 3L)
  expect_equal(s$n[s$status == "something_new"], 1L)
  expect_equal(s$n[s$status == "?"], 1L)           # an NA status is still counted
})


# ---------------------------------------------------------------------------
# EACH FILE'S VERDICT THE MOMENT IT EXISTS. The Convert table fills in row by row
# while a case runs; this is the engine half of that -- a callback after each file
# with that file's row, and never its transactions.
test_that("done() is called after each file, in order, with that file's verdict", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- file.path(dir, c("a_bnz.csv", "b_unknown.csv"))
  seen <- list()
  b <- .b_run(paths, done = function(i, n, f, row) seen[[length(seen) + 1L]] <<- list(i = i, n = n, f = f, row = row))
  expect_length(seen, 2L)
  expect_identical(vapply(seen, function(x) x$i, integer(1)), 1:2)
  expect_identical(vapply(seen, function(x) x$n, integer(1)), c(2L, 2L))
  expect_identical(vapply(seen, function(x) x$row$status, ""), b$status)
  # a few short fields -- the transactions are never in it
  expect_false("result" %in% names(seen[[1]]$row))
  expect_true(all(c("status", "rows", "trust", "failing_check") %in% names(seen[[1]]$row)))
  # a done() that errors costs nothing
  expect_identical(.b_run(paths, done = function(...) stop("boom"))$status, b$status)
})
