# Golden-file + guarantee tests for the ANZ everyday CSV template.

FIXTURE  <- "samples/raw/anz/anz_transaction_export_01.csv"
EXPECTED <- "tests/testthat/expected/anz_everyday_csv.csv"

test_that("detection picks anz_everyday_csv unambiguously", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture(FIXTURE))
  det <- detect_statement(input, templates, hint_bank = "ANZ")
  expect_true(det$matched)
  expect_identical(det$template_id, "anz_everyday_csv")
  expect_gte(det$score, templates[["anz_everyday_csv"]]$min_score)
  # not confused with the ANZ credit-card export template
  expect_gt(det$candidates$score[det$candidates$id == "anz_everyday_csv"],
            max(det$candidates$score[det$candidates$id != "anz_everyday_csv"]))
})

test_that("parsed core table equals the golden snapshot", {
  expect_statement_ok(FIXTURE, EXPECTED,
                      template_id = "anz_everyday_csv", bank = "ANZ")
})

test_that("descriptions are verbatim and amounts signed", {
  res <- parse_fixture(FIXTURE, bank = "ANZ")
  tx <- res$parsed$transactions
  expect_identical(tx$description, c(
    "Acme Inc", "Payroll Ltd", "Water Services", "1234-****-****-4321",
    "Collage fund bank-00", "General Savings", "Anz  1234567 Queen St"))
  expect_equal(tx$amount, c(-23.40, 2000, -46.96, -288, -100, -20, -80))
  expect_identical(tx$direction, c("debit", "credit", "debit", "debit",
                                   "debit", "debit", "debit"))
  expect_identical(tx$date[1:3], c("2014-06-19", "2014-06-19", "2014-06-18"))
  # verbatim NZ fields (double spaces / masks preserved, blanks -> NA)
  expect_identical(tx$particulars[c(1, 7)], c("Acme LLB Inc", "Anz  S3A1234"))
  expect_identical(tx$code, c("Smith Vj", NA, "33 Queen St", NA, "Jane", NA,
                              "Queen St"))
  expect_identical(tx$type[4], "Debit Transfer")
  # this export carries no balance column
  expect_true(all(is.na(tx$balance)))
})

test_that("no rows dropped and no false malformed/redaction flags", {
  res <- parse_fixture(FIXTURE, bank = "ANZ")
  expect_equal(nrow(res$parsed$transactions), 7L)
  expect_true(all(res$parsed$transactions$flags == ""))
  # masked account numbers are not redactions
  expect_false(any(grepl("redacted", res$parsed$transactions$flags)))
  # provenance covers every row
  expect_equal(nrow(res$parsed$provenance), 7L)
})

test_that("reconciliation KPIs are deterministic and non-failing", {
  res <- parse_fixture(FIXTURE, bank = "ANZ")
  k <- res$recon$kpis
  expect_false(any(k$status == "fail"))
  expect_equal(k$status[k$name == "transaction_count"], "pass")
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  expect_true(res$recon$trust$level %in% c("high", "medium"))
})
