# Golden-file + guarantee tests for the Kiwibank everyday CSV export: the table reader
# with the fixture template, and the automatic reader with none.

FIXTURE  <- "samples/raw/kiwibank/kiwibank_transaction_01.csv"
EXPECTED <- "tests/testthat/expected/kiwibank_everyday_csv.csv"

test_that("the automatic reader proves the export, with the golden figures", {
  # The running balance proves every row. The export prints two dates a row
  # (Effective Date, Transaction Date) and the arithmetic cannot say which is the
  # transaction's, so the dates are held to being one whole column of the file's
  # own -- the other kept as an extra column -- rather than to the golden, which
  # took Transaction Date. test-generalisation.R holds the choice to not moving
  # when the columns are reordered.
  rd <- expect_auto_read_golden(FIXTURE, EXPECTED, outcomes = "proven",
                                fields = c("amount", "direction", "balance", "description"))
  raw <- utils::read.csv(fixture(FIXTURE), stringsAsFactors = FALSE, check.names = FALSE)
  both <- list(raw[["Effective Date"]], raw[["Transaction Date"]])
  took <- which(vapply(both, identical, NA, rd$transactions$date))
  expect_length(took, 1L)
  expect_identical(rd$parsed$extras$date2, both[[3L - took[1]]])
})

test_that("parsed core table equals the golden snapshot", {
  expect_statement_ok(FIXTURE, EXPECTED, template_id = "kiwibank_everyday_csv")
})

test_that("descriptions are verbatim and amounts signed", {
  res <- parse_fixture(FIXTURE, "kiwibank_everyday_csv")
  tx <- res$parsed$transactions
  expect_identical(tx$description,
                   c("Sushi", "PAY Alice The Bar Drinks Bob",
                     "PAY Alice Pool Bob", "PAK N SAVE SYLVIA PARK AUCKLAND"))
  expect_equal(tx$amount, c(-9.00, -15.00, -3.15, -42.02))
  expect_identical(tx$direction, c("debit", "debit", "debit", "debit"))
  # Transaction Date parsed to ISO
  expect_identical(tx$date,
                   c("2025-08-22", "2025-08-22", "2025-08-23", "2025-08-23"))
  # Transaction Code maps to `type`
  expect_identical(tx$type,
                   c("EFTPOS PURCHASE", "DIRECT DEBIT",
                     "DIRECT DEBIT", "EFTPOS PURCHASE"))
  # NZ fields verbatim; empty cells -> NA
  expect_identical(tx$particulars, c(NA, "The Bar", "Pool", NA))
  expect_identical(tx$code, c(NA, "Drinks", NA, NA))
  expect_identical(tx$reference, c(NA, "Bob", "Bob", NA))
  expect_identical(tx$other_party, c(NA, "Alice", "Alice", NA))
  # balance column present and populated
  expect_equal(tx$balance, c(895.69, 880.69, 877.54, 835.52))
})

test_that("no rows dropped and no false malformed/redaction flags", {
  res <- parse_fixture(FIXTURE, "kiwibank_everyday_csv")
  expect_equal(nrow(res$parsed$transactions), 4L)
  expect_true(all(res$parsed$transactions$flags == ""))
  expect_equal(nrow(res$parsed$provenance), 4L)
})

test_that("reconciliation KPIs are deterministic; balance continuity passes", {
  res <- parse_fixture(FIXTURE, "kiwibank_everyday_csv")
  k <- res$recon$kpis
  expect_false(any(k$status == "fail"))
  # balance column present -> continuity KPI must run and pass
  expect_equal(k$status[k$name == "running_balance_continuity"], "pass")
  expect_equal(k$status[k$name == "transaction_count"], "pass")
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  expect_true(res$recon$trust$level %in% c("high", "medium"))
})
