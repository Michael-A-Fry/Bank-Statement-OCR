# Golden-file + guarantee tests for the ASB everyday CSV template.
# The ASB FastNet export carries a metadata preamble (Created date; Bank/Branch/
# Account; From/To date; Avail Bal; Ledger Balance) before the real header row
# `Date,Unique Id,Tran Type,Cheque Number,Payee,Memo,Amount`.

FIXTURE  <- "samples/raw/asb/asb_transaction_export_01.csv"
EXPECTED <- "tests/testthat/expected/asb_everyday_csv.csv"

test_that("detection picks asb_everyday_csv unambiguously past the preamble", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture(FIXTURE))
  det <- detect_statement(input, templates, hint_bank = "ASB")
  expect_true(det$matched)
  expect_identical(det$template_id, "asb_everyday_csv")
  expect_gte(det$score, templates[["asb_everyday_csv"]]$min_score)
})

test_that("parsed core table equals the golden snapshot", {
  expect_statement_ok(FIXTURE, EXPECTED,
                      template_id = "asb_everyday_csv", bank = "ASB")
})

test_that("descriptions are verbatim, dates ISO and amounts signed", {
  res <- parse_fixture(FIXTURE, bank = "ASB")
  tx <- res$parsed$transactions
  # verbatim descriptions (hyphens/digits/spaces preserved byte-for-byte)
  expect_identical(tx$description, c(
    "Savings", "EFTPOS", "12-3456-7890123-45 001 INTEREST", "Wages"))
  # signed amounts: debits negative, the wage credit positive
  expect_equal(tx$amount, c(-500.00, -3.80, -456.78, 5678.90))
  expect_identical(tx$direction, c("debit", "debit", "debit", "credit"))
  # dates normalised from the ASB %Y/%m/%d layout, raw kept verbatim
  expect_identical(tx$date, c("2014-12-20", "2014-12-21", "2014-12-22", "2014-12-23"))
  expect_identical(tx$date_raw, c("2014/12/20", "2014/12/21", "2014/12/22", "2014/12/23"))
  # reference <- Unique Id, type <- Tran Type (both verbatim)
  expect_identical(tx$reference, c("2014122001", "2014122101", "2014122201", "2014122301"))
  expect_identical(tx$type, c("XFER", "POS", "DEBIT", "DIRECTDEP"))
  # this export carries no running balance column
  expect_true(all(is.na(tx$balance)))
})

test_that("no rows dropped past the preamble and no false flags", {
  res <- parse_fixture(FIXTURE, bank = "ASB")
  expect_equal(nrow(res$parsed$transactions), 4L)
  expect_true(all(res$parsed$transactions$flags == ""))
  expect_false(any(grepl("malformed|redacted", res$parsed$transactions$flags)))
  # provenance covers every parsed row (completeness)
  expect_equal(nrow(res$parsed$provenance), 4L)
})

test_that("reconciliation KPIs are deterministic and non-failing", {
  res <- parse_fixture(FIXTURE, bank = "ASB")
  k <- res$recon$kpis
  expect_false(any(k$status == "fail"))
  expect_equal(k$status[k$name == "transaction_count"], "pass")
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  expect_true(res$recon$trust$level %in% c("high", "medium"))
})
