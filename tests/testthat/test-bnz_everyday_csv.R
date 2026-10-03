# Golden-file + guarantee tests for the BNZ everyday CSV export: the table reader
# with the fixture template, and the automatic reader with none.

FIXTURE  <- "samples/raw/bnz/bnz_transaction_export_01.csv"
EXPECTED <- "tests/testthat/expected/bnz_everyday_csv.csv"

test_that("the automatic reader reads the export to the golden figures, and asks", {
  # No balance and no totals: nothing proves the reading, so a person looks --
  # but every figure it shows them is the statement's.
  expect_auto_read_golden(FIXTURE, EXPECTED, outcomes = "check")
})

test_that("parsed core table equals the golden snapshot", {
  expect_statement_ok(FIXTURE, EXPECTED, template_id = "bnz_everyday_csv")
})

test_that("descriptions are verbatim and amounts signed", {
  res <- parse_fixture(FIXTURE, "bnz_everyday_csv")
  tx <- res$parsed$transactions
  expect_identical(tx$description, c("PAK N SAVE", "Savings", "Friend Share"))
  expect_equal(tx$amount, c(-25, -100, 11.5))
  expect_identical(tx$direction, c("debit", "debit", "credit"))
  expect_identical(tx$date, c("2026-01-01", "2026-01-02", "2026-01-03"))
  # verbatim NZ fields and the "---" other-party placeholder preserved
  expect_identical(tx$particulars, c("TEST CITY", NA, "Software split"))
  expect_identical(tx$other_party[1], "---")
  # no balance column for this export
  expect_true(all(is.na(tx$balance)))
})

test_that("no rows dropped and no false malformed/redaction flags", {
  res <- parse_fixture(FIXTURE, "bnz_everyday_csv")
  expect_equal(nrow(res$parsed$transactions), 3L)
  expect_true(all(res$parsed$transactions$flags == ""))
  # provenance covers every row
  expect_equal(nrow(res$parsed$provenance), 3L)
})

test_that("reconciliation KPIs are deterministic and non-failing", {
  res <- parse_fixture(FIXTURE, "bnz_everyday_csv")
  k <- res$recon$kpis
  expect_false(any(k$status == "fail"))
  expect_equal(k$status[k$name == "transaction_count"], "pass")
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  expect_true(res$recon$trust$level %in% c("high", "medium"))
})
