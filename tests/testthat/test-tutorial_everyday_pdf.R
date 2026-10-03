# Golden-file test for the TUTORIAL sample: a synthetic PDF statement (Kowhai Bank
# NZ) that exercises the tricky real-world features -- Withdrawals/Deposits as two
# columns and day+month-only dates with the year taken from the statement period.
# Proves the whole PDF path end-to-end on stored, PII-free data: the automatic
# reader with no template, and the table reader with the fixture template.
# Regenerate the PDF with samples/raw/tutorial/make_sample_statement.R (or the
# .html via Chromium print-to-pdf).

FIXTURE  <- "samples/raw/tutorial/sample_everyday_statement.pdf"
EXPECTED <- "tests/testthat/expected/tutorial_everyday_pdf.csv"

test_that("the automatic reader proves the tutorial sample to its golden snapshot", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(FIXTURE)))
  expect_auto_read_golden(FIXTURE, EXPECTED, outcomes = "proven")
})

test_that("the tutorial sample converts with no clicks, to the golden figures", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(FIXTURE)))
  res <- convert_sandbox()(fixture(FIXTURE))
  expect_identical(res$status, "ok")
  expect_identical(res$feed_basis, "proven")
  got <- utils::read.csv(res$outputs[["csv"]], stringsAsFactors = FALSE)
  exp <- read_core_csv(fixture(EXPECTED))
  expect_identical(got$date, exp$date)
  expect_equal(got$amount, exp$amount)
  expect_equal(got$balance, exp$balance)
})

test_that("the fixture template parses the sample to its golden snapshot", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(FIXTURE)))
  expect_statement_ok(FIXTURE, EXPECTED, template_id = "tutorial_everyday_pdf")
})

test_that("the tutorial sample reconciles (opening + all rows = closing)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(FIXTURE)))
  res <- parse_fixture(FIXTURE, "tutorial_everyday_pdf")
  tx <- res$parsed$transactions
  expect_equal(nrow(tx), 12L)
  expect_equal(round(1250.00 + sum(tx$amount, na.rm = TRUE), 2), 2716.50)
  # year taken from the "from 1 May 2026 to 31 May 2026" period
  expect_true(all(startsWith(tx$date, "2026-05-")))
  # two-column amounts resolved into signed values + direction
  expect_identical(tx$direction[tx$description == "SALARY ACME LTD"], "credit")
  expect_identical(tx$direction[tx$description == "EFTPOS COFFEE HOUSE"], "debit")
  # running balance continuity holds
  expect_false(any(res$recon$kpis$status == "fail"))
})
