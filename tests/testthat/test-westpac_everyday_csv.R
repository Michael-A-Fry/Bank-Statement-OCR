# Golden-file + guarantee tests for the Westpac everyday CSV export: the table reader
# with the fixture template, and the automatic reader with none.

FIXTURE  <- "samples/raw/westpac/westpac_transaction_export_01.csv"
EXPECTED <- "tests/testthat/expected/westpac_everyday_csv.csv"

test_that("the automatic reader reads the export to the golden figures, and asks", {
  # No balance and no totals: nothing proves the reading, so a person looks. The
  # export has two text columns a row (Other Party, Description); the reader takes
  # Other Party as the description and keeps the other as an extra column, so the
  # description is held to the file rather than to the golden.
  rd <- expect_auto_read_golden(FIXTURE, EXPECTED, outcomes = "check",
                                fields = c("date", "amount", "direction", "balance"))
  raw <- utils::read.csv(fixture(FIXTURE), stringsAsFactors = FALSE, check.names = FALSE)
  expect_identical(rd$transactions$description, raw[["Other Party"]])
  expect_identical(rd$parsed$extras$text1, raw[["Description"]])
})

test_that("parsed core table equals the golden snapshot", {
  expect_statement_ok(FIXTURE, EXPECTED, template_id = "westpac_everyday_csv")
})

test_that("descriptions are verbatim and amounts signed", {
  res <- parse_fixture(FIXTURE, "westpac_everyday_csv")
  tx <- res$parsed$transactions
  expect_identical(tx$description,
                   c("ONLINE BANKING", "EFTPOS TRANSACTION",
                     "BILL PAYMENT", "DEPOSIT"))
  expect_equal(tx$amount, c(-500, -3.80, -456.78, 5678.90))
  expect_identical(tx$direction, c("debit", "debit", "debit", "credit"))
  expect_identical(tx$date,
                   c("2014-12-20", "2014-12-21", "2014-12-22", "2014-12-23"))
  # verbatim other-party field: comma and apostrophe preserved byte-for-byte
  expect_identical(tx$other_party,
                   c("To 12-3456-7890123-45-02", "Bob's Cafe",
                     "LOAN - INTEREST", "Acme, Inc."))
  # Analysis Code maps to `code`; empty code cell -> NA
  expect_identical(tx$code, c("Transfer", "1111   11111", "repayment", NA))
  expect_identical(tx$reference,
                   c("11:11-11111", "11:11-11112", "11:11-11113", "11:11-11114"))
  # no balance column for this export
  expect_true(all(is.na(tx$balance)))
})

test_that("no rows dropped and no false malformed/redaction flags", {
  res <- parse_fixture(FIXTURE, "westpac_everyday_csv")
  expect_equal(nrow(res$parsed$transactions), 4L)
  expect_true(all(res$parsed$transactions$flags == ""))
  # provenance covers every row
  expect_equal(nrow(res$parsed$provenance), 4L)
})

test_that("reconciliation KPIs are deterministic and non-failing", {
  res <- parse_fixture(FIXTURE, "westpac_everyday_csv")
  k <- res$recon$kpis
  expect_false(any(k$status == "fail"))
  expect_equal(k$status[k$name == "transaction_count"], "pass")
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  expect_true(res$recon$trust$level %in% c("high", "medium"))
})
