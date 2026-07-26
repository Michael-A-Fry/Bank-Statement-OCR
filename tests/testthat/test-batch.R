# Tests for batch conversion (R/batch.R) -- a whole case folder converted in one
# unattended run, one row per file.
#
# The failures these exist to prevent:
#   * one unreadable file costing the other twenty-nine their conversion;
#   * a row that looks converted when it was not (a template id beside a file no
#     template read, rows that are really a memory-saving NULL);
#   * `failing_check` splitting one kind of failure across several wordings, which
#     is the one thing that makes the column worth having.

# ---- fixtures: CSV only, so nothing here needs poppler/tesseract -------------
.b_write <- function(dir, name, lines) { p <- file.path(dir, name); writeLines(lines, p); p }

# A statement a shipped template reads cleanly.
.B_ANZ <- c(
  "Type,Details,Particulars,Code,Reference,Amount,Date,ForeignCurrencyAmount,ConversionCharge",
  "Visa Purchase,Acme Inc,Acme LLB Inc,Smith Vj,,-23.40,19/06/2014,,",
  "Credit,Payroll Ltd,Salary,,,2000.00,19/06/2014,,")
# A layout nothing matches.
.B_UNKNOWN <- c("colA;colB;colC", "1;2;3")

# .b_run(paths, ...) -- convert a batch into throwaway out/log directories, so a
# test never writes into the repo and never pollutes the real run log.
.b_run <- function(paths, ...) {
  outdir <- tempfile(); dir.create(outdir)
  logdir <- tempfile(); dir.create(logdir)
  b <- convert_batch(paths, outdir = outdir, logdir = logdir,
                     templates_dir = templates_dir(),
                     user_templates_dir = "does_not_exist",
                     formats = "csv", ...)
  attr(b, "outdir") <- outdir
  attr(b, "logdir") <- logdir
  b
}

# The shipped wording maps, read the same way the app reads them. Tests assert
# against THESE rather than against a copied sentence, so ui_labels.R stays free
# to be reworded without silently dropping the check.
.b_labels <- function() {
  f <- file.path(engine_root(), "ui_labels.R")
  expect_true(file.exists(f))
  e <- new.env(parent = globalenv())
  sys.source(f, envir = e)
  e
}

.b_case <- function() {
  dir <- tempfile(); dir.create(dir)
  .b_write(dir, "a_anz.csv", .B_ANZ)
  .b_write(dir, "b_unknown.csv", .B_UNKNOWN)
  dir
}

# ---------------------------------------------------------------------------
# The frame itself
# ---------------------------------------------------------------------------

test_that("one row per file, in the order given, with the promised columns", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # deliberately NOT alphabetical, and with a repeat: the caller's order and the
  # caller's list are what come back.
  paths <- c(file.path(dir, "b_unknown.csv"), file.path(dir, "a_anz.csv"),
             file.path(dir, "b_unknown.csv"))
  b <- .b_run(paths)

  expect_true(is.data.frame(b))
  expect_identical(names(b), c("file", "status", "bank", "template_id", "rows",
                               "trust", "failing_check", "message", "result"))
  expect_identical(b$file, paths)          # order kept, duplicate kept
  expect_equal(nrow(b), 3L)
  expect_true(is.integer(b$rows))          # a screen sorts this numerically
  expect_true(is.list(b$result))
})

test_that("an unreadable file becomes a row and never stops the rest", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # the broken file goes FIRST, so a batch that stops at the first problem fails
  # this test rather than quietly passing it.
  paths <- c(file.path(dir, "not_here_at_all.csv"), file.path(dir, "a_anz.csv"))
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
  paths <- c(file.path(dir, "a_anz.csv"), file.path(dir, "b_unknown.csv"),
             file.path(dir, "gone.csv"))
  b <- .b_run(paths)
  # one record per file, exactly as a single conversion writes -- including the
  # file that could not be read, which is work the tool did and must record.
  expect_equal(length(list.files(file.path(attr(b, "logdir"), "runs"))), 3L)
  # and the run id on each row's result is the id of the record just written
  expect_equal(length(unique(vapply(b$result, function(r) r$run_id %||% "", ""))), 3L)
})

test_that("a converted file reports its bank, template and row count", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(file.path(dir, "a_anz.csv"))
  expect_identical(b$status, "ok")
  expect_identical(b$bank, "ANZ")
  expect_identical(b$template_id, "anz_everyday_csv")
  expect_equal(b$rows, 2L)
  expect_true(b$trust %in% c("high", "medium", "low"))
  expect_true(is.na(b$failing_check))     # nothing went wrong -> nothing to say
})

test_that("a template id never appears beside a file no template read", {
  # convert_statement keeps the CLOSEST template on an unsupported result (it is
  # evidence for whoever builds the missing one). On this frame that would read as
  # "this template was used", so it must be blank -- and this asserts the trap is
  # real, not just that the column happens to be NA.
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(file.path(dir, "b_unknown.csv"))
  expect_identical(b$status, "unsupported")
  expect_true(is.na(b$template_id))
  expect_true(is.na(b$bank))
  expect_false(is.na(b$result[[1]]$template_id %||% NA))  # the closest match IS kept
})

# ---------------------------------------------------------------------------
# failing_check -- the column the whole feature is for
# ---------------------------------------------------------------------------

test_that("files that failed the same way carry the identical failing_check", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .b_write(dir, "c_unknown.csv", .B_UNKNOWN)
  paths <- c(file.path(dir, "b_unknown.csv"), file.path(dir, "a_anz.csv"),
             file.path(dir, "c_unknown.csv"))
  b <- .b_run(paths)
  # identical strings, so sorting gathers them and one fix clears both
  expect_identical(b$failing_check[1], b$failing_check[3])
  sorted <- b[order(b$failing_check, na.last = TRUE), ]
  expect_identical(sorted$failing_check[1], sorted$failing_check[2])
})

test_that("failing_check uses the wording the screen uses, not a raw code", {
  L <- .b_labels()
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(c(file.path(dir, "b_unknown.csv"), file.path(dir, "gone.csv")))
  expect_identical(b$failing_check[1], unname(L$DIAG_PLAIN[["unknown_format"]]))
  expect_identical(b$failing_check[2], unname(L$DIAG_PLAIN[["unreadable"]]))
})

test_that("a failing check is named in plain words and beats the diagnostics", {
  L <- .b_labels()
  fix <- fixture("tests/testthat/fixtures/kiwibank_broken_balance.csv")
  expect_true(file.exists(fix))
  b <- .b_run(fix)
  expect_identical(b$status, "needs_review")
  k <- b$result[[1]]$kpis
  first_fail <- k$name[k$status == "fail"][1]
  expect_false(is.na(first_fail))                        # the fixture must fail one
  expect_identical(b$failing_check, unname(L$CHECK_PLAIN[[first_fail]]))
})

test_that("a bundle's per-statement tag is stripped so grouping still works", {
  # An auto-split run tags every check "<name> [statement N]". That says WHERE in
  # the bundle, not WHAT went wrong; left in, one failure kind would scatter into
  # as many groups as the bundle has statements.
  L <- .b_labels()
  res <- list(status = "needs_review",
              kpis = data.frame(name = "balance_reconciliation [statement 2]",
                                status = "fail", stringsAsFactors = FALSE))
  expect_identical(.failing_check(res, L),
                   unname(L$CHECK_PLAIN[["balance_reconciliation"]]))
})

test_that("a failing check wins over a diagnostic, and a diagnostic over the status", {
  L <- .b_labels()
  kpis <- data.frame(name = c("dates_readable", "balance_reconciliation"),
                     status = c("pass", "fail"), stringsAsFactors = FALSE)
  diag <- data.frame(category = c("row_parse", "ocr"), severity = c("high", "info"),
                     stringsAsFactors = FALSE)
  # tier 1: the check, even though a high diagnostic is also present
  expect_identical(.failing_check(list(status = "needs_review", kpis = kpis, diagnostics = diag), L),
                   unname(L$CHECK_PLAIN[["balance_reconciliation"]]))
  # tier 2: no failing check -> the most severe diagnostic that is not context
  kpis$status <- c("pass", "pass")
  expect_identical(.failing_check(list(status = "needs_review", kpis = kpis, diagnostics = diag), L),
                   unname(L$DIAG_PLAIN[["row_parse"]]))
  # tier 3: nothing specific at all -> the verdict, never a blank cell
  info_only <- data.frame(category = "none", severity = "info", stringsAsFactors = FALSE)
  expect_identical(.failing_check(list(status = "needs_review", kpis = kpis, diagnostics = info_only), L),
                   unname(L$STATUS_PLAIN[["needs_review"]]))
})

test_that("an unknown code falls back to the code itself rather than a blank", {
  L <- .b_labels()
  res <- list(status = "needs_review",
              kpis = data.frame(name = "a_check_that_does_not_exist", status = "fail",
                                stringsAsFactors = FALSE))
  expect_identical(.failing_check(res, L), "a_check_that_does_not_exist")
  # and with no wording file loaded at all, the raw code still comes through
  expect_identical(.failing_check(res, new.env()), "a_check_that_does_not_exist")
})

# ---------------------------------------------------------------------------
# Bounded memory
# ---------------------------------------------------------------------------

test_that("a 50-file case does not hold 50 transaction tables, and says so", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(file.path(dir, "a_anz.csv"))
  r <- b$result[[1]]
  # NULL, not a partially-matched marker: `$feed_rows` must not resolve to the
  # flag that records the drop, or a reader sees a value where the data was.
  expect_null(r$feed_rows)
  expect_true(isTRUE(r$dropped_feed_rows))
  # the count survives the drop, and the rows themselves are on disk
  expect_equal(b$rows, 2L)
  expect_true(any(grepl("\\.csv$", r$outputs)))
})

test_that("keep_rows = TRUE keeps the full table for a caller that asks", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  b <- .b_run(file.path(dir, "a_anz.csv"), keep_rows = TRUE)
  r <- b$result[[1]]
  expect_true(is.data.frame(r$feed_rows))
  expect_equal(nrow(r$feed_rows), b$rows)
  expect_null(r$dropped_feed_rows)
})

test_that("a form is counted in the values read, never in transactions", {
  # A form (an IRD summary, a KiwiSaver letter) has no transaction table, so
  # `rows` must not report 0 as though nothing came out -- nor the number of
  # fields the template declares, which would overstate an empty read.
  expect_equal(.rows_of(list(kind = "form", n_values = 3L, n_fields = 9L,
                             run_log = list(row_count = 0L))), 3L)
  expect_equal(.rows_of(list(kind = "statement", run_log = list(row_count = 12L))), 12L)
  expect_equal(.rows_of(list(status = "failed")), 0L)
})

# ---------------------------------------------------------------------------
# Usable from a console, deterministic, and safe to print
# ---------------------------------------------------------------------------

test_that("progress is a plain callback, called once per file in order", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- c(file.path(dir, "a_anz.csv"), file.path(dir, "b_unknown.csv"))
  seen <- list()
  .b_run(paths, progress = function(i, n, file) seen[[length(seen) + 1L]] <<- list(i, n, file))
  expect_equal(length(seen), 2L)
  expect_equal(vapply(seen, function(x) x[[1]], numeric(1)), c(1, 2))
  expect_true(all(vapply(seen, function(x) x[[2]], numeric(1)) == 2))
  expect_identical(vapply(seen, function(x) x[[3]], ""), paths)
})

test_that("a broken progress callback does not cost the case its run", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- c(file.path(dir, "a_anz.csv"), file.path(dir, "b_unknown.csv"))
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
  paths <- c(file.path(dir, "a_anz.csv"), file.path(dir, "b_unknown.csv"))
  cols <- c("file", "status", "bank", "template_id", "rows", "trust",
            "failing_check", "message")
  # everything the screen shows is identical run to run; only the run ids and
  # timestamps INSIDE the result vary, which is by design.
  expect_identical(.b_run(paths)[, cols], .b_run(paths)[, cols])
})

test_that("no files gives an empty frame, not an error", {
  b <- convert_batch(character(0))
  expect_equal(nrow(b), 0L)
  expect_identical(names(b), c("file", "status", "bank", "template_id", "rows",
                               "trust", "failing_check", "message", "result"))
  expect_equal(sum(batch_summary(b)$n), 0L)
})

# ---------------------------------------------------------------------------
# batch_summary
# ---------------------------------------------------------------------------

test_that("batch_summary counts every file and hides no status", {
  dir <- .b_case(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- c(file.path(dir, "a_anz.csv"), file.path(dir, "b_unknown.csv"),
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
