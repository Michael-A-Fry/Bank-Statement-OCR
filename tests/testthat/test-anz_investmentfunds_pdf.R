# Tests for the PDF transaction-table path (R/parse_pdf_table.R) via a real
# bank-published populated table (ANZ Investment Funds statement guide): the table
# reader with the fixture template, and the automatic reader with none.

SAMPLE_IF_PDF <- "samples/raw/anz/anz_investmentfunds_statement_guide_sample.pdf"

test_that("the automatic reader reads the table to the golden figures, and asks", {
  # A fund statement has no running balance, and the guide prints a dated example
  # sentence on page 1: nothing proves the reading, so a person looks -- with the
  # same eight rows the fixture template reads.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SAMPLE_IF_PDF)))
  expect_auto_read_golden(SAMPLE_IF_PDF, "tests/testthat/expected/anz_investmentfunds_pdf.csv",
                          outcomes = "check")
})

test_that("the transaction table is extracted correctly (no gaps/annotations)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SAMPLE_IF_PDF)))
  input <- read_input(fixture(SAMPLE_IF_PDF))
  tx <- parse_statement(input, fixture_template("anz_investmentfunds_pdf"))$transactions
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
  input <- read_input(fixture(SAMPLE_IF_PDF))
  ex <- parse_statement(input, fixture_template("anz_investmentfunds_pdf"))$extras
  expect_equal(nrow(ex), 8L)
  expect_true(all(c("units", "unit_price") %in% names(ex)))
})

test_that("a PDF converts end-to-end via convert_statement, for a person to check", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SAMPLE_IF_PDF)))
  res <- convert_sandbox()(fixture(SAMPLE_IF_PDF))
  expect_identical(res$status, "needs_review")
  expect_identical(res$feed_basis, "none")
  expect_true(file.exists(res$outputs[["xlsx"]]))
  expect_true(file.exists(res$outputs[["csv"]]))
  # what the person is shown is the statement's own rows, read back from the file
  got <- utils::read.csv(res$outputs[["csv"]], stringsAsFactors = FALSE)
  exp <- read_core_csv(fixture("tests/testthat/expected/anz_investmentfunds_pdf.csv"))
  expect_identical(got$date, exp$date)
  expect_equal(got$amount, exp$amount)
})
