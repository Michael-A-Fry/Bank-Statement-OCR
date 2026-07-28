#!/usr/bin/env Rscript
# run_tests.R -- the single test runner for everyone.
# Sources all engine functions, loads packages, runs testthat over tests/testthat.

# Force a UTF-8 locale FIRST -- the same guard, in the same words, as app.R and
# run.R. ALL THREE need it: they all source the same engine, and the engine reads
# bank statements, which carry pound signs, euro signs, macrons and accented
# payees. On a host whose default locale is C/ASCII (this container, and any bare
# Windows service account) R cannot translate those bytes, and the failure is
# silent -- a value comes back NA, or short of a byte, with only a console warning.
#
# WHY THE SUITE ESPECIALLY: a suite that runs in a locale nobody deploys in has
# proved nothing about the deployment. app.R has forced UTF-8 for a long time, so
# every test of engine behaviour was being run in the ONE locale the app never
# uses. Both halves are covered now: this line makes the suite match the app and
# the CLI, and test-labels.R holds the engine itself to being locale-independent
# by running the same input under C and under C.UTF-8 and demanding one answer.
suppressWarnings(for (.loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"))
  if (nzchar(Sys.setlocale("LC_CTYPE", .loc))) break)

.this_dir <- function() {
  args <- commandArgs(FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1]))))
  getwd()
}

root <- normalizePath(file.path(.this_dir(), ".."))

suppressWarnings(suppressMessages({
  library(testthat)
  for (p in c("yaml", "jsonlite", "openxlsx", "readxl", "pdftools")) {
    requireNamespace(p, quietly = TRUE)
  }
}))

for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# Make root discoverable to helpers/tests.
Sys.setenv(ENGINE_ROOT = root)

reporter <- testthat::SummaryReporter$new()
res <- testthat::test_dir(
  file.path(root, "tests", "testthat"),
  reporter = reporter,
  stop_on_failure = FALSE
)

df <- as.data.frame(res)
n_fail  <- sum(df$failed)
n_error <- sum(df$error)
n_warn  <- sum(df$warning)
# SKIPPED is counted and reported too. Without it "all green" cannot tell a test
# that PASSED from one that never RAN: ~100 skip guards (missing tesseract/poppler,
# a renamed fixture) can dark the whole PDF/OCR/Excel/redaction surface while the
# summary still prints a clean board -- which is how an untested template shipped.
n_skip  <- if (!is.null(df$skipped)) sum(as.integer(df$skipped)) else 0L
n_ok    <- sum(df$nb) - n_fail - n_error

cat("\n==== Test summary ====\n")
cat(sprintf("files:   %d\n", length(unique(df$file))))
cat(sprintf("tests:   %d\n", nrow(df)))
cat(sprintf("passed:  %d\n", sum(df$passed)))
cat(sprintf("failed:  %d\n", n_fail))
cat(sprintf("errors:  %d\n", n_error))
cat(sprintf("warnings:%d\n", n_warn))
cat(sprintf("skipped: %d\n", n_skip))

if (n_skip > 0) {
  cat("\nSKIPPED tests did not run - this board is NOT a clean bill of health.\n",
      "Usually a missing tool (tesseract / poppler / an R package) or a renamed\n",
      "fixture. Install what is missing and run again. To accept the gap knowingly\n",
      "(e.g. a machine that deliberately has no OCR), set BSO_ALLOW_SKIPS=1.\n", sep = "")
}
allow_skips <- nzchar(Sys.getenv("BSO_ALLOW_SKIPS"))
if (n_fail > 0 || n_error > 0 || (n_skip > 0 && !allow_skips)) {
  quit(status = 1)
}
quit(status = 0)
