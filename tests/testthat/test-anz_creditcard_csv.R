# Golden-file + guarantee tests for the ANZ credit card CSV export: the table reader
# with the fixture template, and the automatic reader with none.

FIXTURE  <- "samples/raw/anz/anz_creditcard_01.csv"
EXPECTED <- "tests/testthat/expected/anz_creditcard_csv.csv"

test_that("the automatic reader reads the export to the golden figures, and asks", {
  # A card export with no balance: nothing proves which way its signs run, so a
  # person looks -- but every figure it shows them is the statement's.
  expect_auto_read_golden(FIXTURE, EXPECTED, outcomes = "check")
})

test_that("parsed core table equals the golden snapshot", {
  expect_statement_ok(FIXTURE, EXPECTED, template_id = "anz_creditcard_csv")
})

test_that("descriptions are verbatim and type_dc signs applied", {
  res <- parse_fixture(FIXTURE, "anz_creditcard_csv")
  tx <- res$parsed$transactions
  # interior double spaces preserved byte-for-byte; only outer whitespace trimmed
  expect_identical(tx$description, c(
    "Auckland      Nz", "Silverdale Nz", "Direct Credit Payment",
    "Auckland      Nzl", "N Shore City  Nzl", "Auckland      Nz",
    "Auckland      Nz", "Coffee Club Sylvia Nz"))
  # D -> negative (debit), C -> positive (credit)
  expect_equal(tx$amount, c(-4.5, -27.19, 54, -40, -3.02, -15, 15, -23.4))
  expect_identical(tx$direction, c("debit", "debit", "credit", "debit",
                                   "debit", "debit", "credit", "debit"))
  expect_identical(tx$type, c("D", "D", "C", "D", "D", "D", "C", "D"))
  # dates normalised from %d/%m/%Y to ISO
  expect_identical(tx$date, c("2020-02-15", "2020-02-13", "2020-02-13",
                              "2020-02-12", "2020-02-10", "2020-01-31",
                              "2020-01-31", "2020-01-30"))
  # credit card export carries no balance column
  expect_true(all(is.na(tx$balance)))
})

test_that("no rows dropped and no false malformed/redaction flags", {
  res <- parse_fixture(FIXTURE, "anz_creditcard_csv")
  expect_equal(nrow(res$parsed$transactions), 8L)
  expect_true(all(res$parsed$transactions$flags == ""))
  expect_equal(nrow(res$parsed$provenance), 8L)
})

test_that("reconciliation KPIs are deterministic and non-failing", {
  res <- parse_fixture(FIXTURE, "anz_creditcard_csv")
  k <- res$recon$kpis
  expect_false(any(k$status == "fail"))
  expect_equal(k$status[k$name == "transaction_count"], "pass")
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  expect_true(res$recon$trust$level %in% c("high", "medium"))
})
