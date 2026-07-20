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

test_that("multi-statement detection keys on PERIODS, not account count", {
  # two distinct periods -> flagged as a genuine bundle
  m3 <- list(n_accounts = 0, n_periods = 2, page1_markers = 1)
  expect_true(detect_multiple_statements(NULL, m3)$likely_multiple)
  # several accounts but ONE period -> NOT a bundle; it's a combined statement.
  # (Real data: a single ANZ statement showed 5 account numbers via transfer
  # narratives yet had one continuous running balance.)
  m <- list(n_accounts = 5, n_periods = 1, page1_markers = 1)
  d <- detect_multiple_statements(NULL, m)
  expect_false(d$likely_multiple)
  expect_true(d$combined_accounts)
  # one account, repeated 'Page 1 of N' (e.g. a guide) -> NOT flagged
  m2 <- list(n_accounts = 1, n_periods = 1, page1_markers = 3)
  expect_false(detect_multiple_statements(NULL, m2)$likely_multiple)
})

test_that("period is found from LABELLED opening/closing dates (not just a range)", {
  # Westpac/ASB style: two labelled dates on separate lines, no "from X to Y".
  input <- list(kind = "pdf", pages = paste(sep = "\n",
    "Westpac Everyday",
    "Statement Opening date:  10 June 2026",
    "Statement Closing date:   9 July 2026",
    "15 Jun DD Rates 863.90"))
  m <- extract_metadata(input)
  expect_match(m$period_start, "10 June 2026", fixed = TRUE)
  expect_match(m$period_end, "9 July 2026", fixed = TRUE)
  expect_true(m$n_periods >= 1)   # so year-less transaction dates can resolve
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

test_that("an oversized page (>2880 pt) raises a diagnostic (Hubdoc-style pre-flight)", {
  tx <- data.frame(row_id = 1L, date = "2025-01-01", date_raw = "1/1/25", description = "a",
    amount = -1, amount_raw = "-1", direction = "debit", balance = NA_real_,
    balance_raw = NA_character_, particulars = NA_character_, code = NA_character_,
    reference = NA_character_, other_party = NA_character_, type = NA_character_,
    currency = "NZD", flags = "", stringsAsFactors = FALSE)
  d <- build_diagnostics("ok", parsed = list(transactions = tx), recon = list(kpis = NULL),
    metadata = list(multi = list(likely_multiple = FALSE), pages = 2, max_page_pt = 3000))
  expect_true(any(d$category == "oversized_page"))
})

test_that("a real statement's page size is within limits", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(IF_META_PDF)))
  m <- extract_metadata(read_input(fixture(IF_META_PDF)))
  expect_true(is.finite(m$max_page_pt))
  expect_lt(m$max_page_pt, 2880)   # A4 ~842 pt
})
