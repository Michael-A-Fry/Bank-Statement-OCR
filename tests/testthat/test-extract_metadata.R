# Tests for GENERIC statement metadata + multi-statement detection.
# All pattern-based -- no per-bank / per-sample hardcoding.

IF_META_PDF <- "samples/raw/anz/anz_investmentfunds_statement_guide_sample.pdf"

test_that("generic metadata is extracted from a real statement", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(IF_META_PDF)))
  m <- extract_metadata(read_input(fixture(IF_META_PDF)))
  expect_equal(m$pages_actual, 3L)      # actual PDF pages (front + back + terms)
  expect_equal(m$pages_stated, 2L)      # "Page X of 2"
  expect_match(m$period_start, "1 April 2025", fixed = TRUE)
  expect_match(m$period_end, "31 March 2026", fixed = TRUE)
})

test_that("multi-statement detection needs a STRONG signal (not page-1 count alone)", {
  # two distinct accounts -> flagged
  m <- list(n_accounts = 2, n_periods = 1, page1_markers = 2)
  expect_true(detect_multiple_statements(NULL, m)$likely_multiple)
  # one account but repeated 'Page 1 of N' (e.g. a guide) -> NOT flagged
  m2 <- list(n_accounts = 1, n_periods = 1, page1_markers = 3)
  expect_false(detect_multiple_statements(NULL, m2)$likely_multiple)
  # two distinct statement periods -> flagged
  m3 <- list(n_accounts = 0, n_periods = 2, page1_markers = 1)
  expect_true(detect_multiple_statements(NULL, m3)$likely_multiple)
})

test_that("account and card regexes are generic", {
  input <- list(pages = c(
    "Account 01-0902-0123456-00 statement",
    "Card 4835-****-****-6843 ending"))
  m <- extract_metadata(input)
  expect_true("01-0902-0123456-00" %in% m$accounts)
  expect_equal(m$n_accounts, 2L)
})

test_that("metadata reaches the convert result and the workbook", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(IF_META_PDF)))
  out <- tempfile("md_out_")
  res <- convert_statement(fixture(IF_META_PDF), outdir = out,
                           templates_dir = templates_dir(), logdir = tempfile("l_"))
  expect_false(is.null(res$metadata))
  expect_equal(res$metadata$pages_actual, 3L)
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  wb <- openxlsx::loadWorkbook(res$outputs[["xlsx"]])
  expect_true("Metadata" %in% names(wb))
})
