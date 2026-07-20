#!/usr/bin/env Rscript
# run_tests.R -- the single test runner for everyone.
# Sources all engine functions, loads packages, runs testthat over tests/testthat.

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
n_ok    <- sum(df$nb) - n_fail - n_error

cat("\n==== Test summary ====\n")
cat(sprintf("files:   %d\n", length(unique(df$file))))
cat(sprintf("tests:   %d\n", nrow(df)))
cat(sprintf("passed:  %d\n", sum(df$passed)))
cat(sprintf("failed:  %d\n", n_fail))
cat(sprintf("errors:  %d\n", n_error))
cat(sprintf("warnings:%d\n", n_warn))

if (n_fail > 0 || n_error > 0) {
  quit(status = 1)
}
quit(status = 0)
