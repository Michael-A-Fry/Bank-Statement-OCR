# Golden-file test for the wizard TUTORIAL sample: a synthetic PDF statement
# (Kowhai Bank NZ) that exercises the tricky real-world features -- Withdrawals/
# Deposits as two columns and day+month-only dates with the year taken from the
# statement period. Proves the whole PDF-template path end-to-end on stored,
# PII-free data. Regenerate the PDF with samples/raw/tutorial/make_sample_statement.R
# (or the .html via Chromium print-to-pdf).

FIXTURE  <- "samples/raw/tutorial/sample_everyday_statement.pdf"
EXPECTED <- "tests/testthat/expected/tutorial_everyday_pdf.csv"

test_that("the tutorial sample PDF detects and parses to its golden snapshot", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(FIXTURE)))
  templates <- load_templates(templates_dir())
  input <- read_input(fixture(FIXTURE))
  det <- detect_statement(input, templates)
  expect_true(det$matched)
  expect_identical(det$template_id, "tutorial_everyday_pdf")
  expect_statement_ok(FIXTURE, EXPECTED, template_id = "tutorial_everyday_pdf")
})

test_that("the tutorial sample reconciles (opening + all rows = closing)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(FIXTURE)))
  res <- parse_fixture(FIXTURE)
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
