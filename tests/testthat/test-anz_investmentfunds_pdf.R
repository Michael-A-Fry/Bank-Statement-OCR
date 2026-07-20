# Tests for the PDF transaction-table path (R/parse_pdf_table.R) via a real
# bank-published populated table (ANZ Investment Funds statement guide).

SAMPLE_IF_PDF <- "samples/raw/anz/anz_investmentfunds_statement_guide_sample.pdf"

test_that("the PDF template is detected on page text", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SAMPLE_IF_PDF)))
  tp <- load_templates(templates_dir())
  input <- read_input(fixture(SAMPLE_IF_PDF))
  det <- detect_statement(input, tp)
  expect_true(det$matched)
  expect_identical(det$template_id, "anz_investmentfunds_pdf")
})

test_that("the transaction table is extracted correctly (no gaps/annotations)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SAMPLE_IF_PDF)))
  tp <- load_templates(templates_dir())
  input <- read_input(fixture(SAMPLE_IF_PDF))
  tx <- parse_statement(input, tp[["anz_investmentfunds_pdf"]])$transactions
  expect_equal(nrow(tx), 8L)
  # dates parsed to ISO, none dropped or spurious
  expect_false(any(is.na(tx$date)))
  expect_equal(tx$date[1], "2025-04-02")
  expect_equal(tx$date[8], "2026-03-25")
  # verbatim descriptions + correct signed amounts (incl. a comma-thousands value)
  expect_equal(tx$description[1], "PIE Tax")
  expect_equal(tx$amount[1], -603.91)
  expect_equal(tx$description[3], "Withdrawal")
  expect_equal(tx$amount[3], -4000.00)
  expect_equal(tx$amount[5], 4000.00)
  # raw amount preserved verbatim (currency symbol kept)
  expect_equal(tx$amount_raw[3], "-$4,000.00")
  # no false malformed/redacted flags
  expect_true(all(tx$flags == ""))
})

test_that("extras (units / unit price) are captured, keyed by row", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SAMPLE_IF_PDF)))
  tp <- load_templates(templates_dir())
  input <- read_input(fixture(SAMPLE_IF_PDF))
  ex <- parse_statement(input, tp[["anz_investmentfunds_pdf"]])$extras
  expect_equal(nrow(ex), 8L)
  expect_true(all(c("units", "unit_price") %in% names(ex)))
})

test_that("a PDF converts end-to-end via convert_statement", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SAMPLE_IF_PDF)))
  out <- tempfile("pdf_out_")
  res <- convert_statement(fixture(SAMPLE_IF_PDF), outdir = out,
                           templates_dir = templates_dir(), logdir = tempfile("l_"))
  expect_identical(res$template_id, "anz_investmentfunds_pdf")
  expect_true(res$status %in% c("ok", "needs_review"))
  expect_true(file.exists(res$outputs[["xlsx"]]))
  expect_true(file.exists(res$outputs[["csv"]]))
})
