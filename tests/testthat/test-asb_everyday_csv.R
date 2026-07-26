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

# ---------------------------------------------------------------------------
# The SECOND committed ASB export (#42). asb_transaction_export_03.csv is the
# same FastNet layout produced eleven years later: identical header, identical
# preamble shape -- and dates written 13/10/2025 instead of 2025/10/13. With the
# template pinning one format this file matched ASB confidently (score 7/6) and
# then returned EVERY date as NA with dates_readable = fail. Both eras must read.
# ---------------------------------------------------------------------------

FIXTURE_03 <- "samples/raw/asb/asb_transaction_export_03.csv"

test_that("the 2025 ASB export detects as ASB and reads EVERY date", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture(FIXTURE_03))
  det <- detect_statement(input, templates)
  expect_true(det$matched)
  expect_identical(det$template_id, "asb_everyday_csv")

  tx <- parse_statement(input, templates[["asb_everyday_csv"]])$transactions
  expect_identical(tx$date, c("2025-10-13", "2025-10-17"))
  expect_identical(tx$date_raw, c("13/10/2025", "17/10/2025"))   # raw kept verbatim
  expect_false(any(is.na(tx$date)))
  expect_equal(tx$amount, c(-50.00, -88.39))
  expect_identical(tx$description, c("EFTPOS", "BILL PAYMENT TO Electricity"))
  expect_identical(tx$reference, c("2025101304", "2025101703"))
})

test_that("the 2014 export still reads under the SAME multi-format template", {
  # the whole point of the list: one template, both eras, neither guessed.
  templates <- load_templates(templates_dir())
  expect_identical(as.character(templates[["asb_everyday_csv"]]$columns$date$format),
                   c("%Y/%m/%d", "%d/%m/%Y"))
  tx <- parse_statement(read_input(fixture(FIXTURE)),
                        templates[["asb_everyday_csv"]])$transactions
  expect_identical(tx$date, c("2014-12-20", "2014-12-21", "2014-12-22", "2014-12-23"))
})

test_that("a date column no candidate format reads comes back NA, never guessed", {
  # Fail closed: swap in a template whose candidates cannot read this file at all.
  # Every date must be NA (with the raw preserved) and dates_readable must FAIL --
  # not a plausible reading under some other format.
  templates <- load_templates(templates_dir())
  t <- templates[["asb_everyday_csv"]]
  t$columns$date$format <- list("%b %d %Y", "%Y-%m-%d")
  p <- parse_statement(read_input(fixture(FIXTURE_03)), t)
  expect_true(all(is.na(p$transactions$date)))
  expect_identical(p$transactions$date_raw, c("13/10/2025", "17/10/2025"))
  k <- reconcile(p, t)$kpis
  expect_equal(k$status[k$name == "dates_readable"], "fail")
})

test_that("the spreadsheet padding rows are not emitted as phantom transactions", {
  # This export is padded out with 24 lines of bare commas (",,,,,,"). They used to
  # become 24 empty transactions, every one flagged 'malformed' -- which buries a
  # genuinely malformed row and puts 24 invented rows in the accountant's workbook.
  input <- read_input(fixture(FIXTURE_03))
  templates <- load_templates(templates_dir())
  p <- parse_statement(input, templates[["asb_everyday_csv"]])
  expect_equal(nrow(p$transactions), 2L)
  expect_true(all(p$transactions$flags == ""))
  # the omission is STATED, not silent, and excluded from the completeness proof
  # so it compares like with like.
  expect_equal(p$padding_line_count, 24L)
  expect_equal(p$source_line_count, 2L)
  k <- reconcile(p, templates[["asb_everyday_csv"]])$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  expect_equal(k$status[k$name == "dates_readable"], "pass")
})

# ---------------------------------------------------------------------------
# The TAB-delimited twin (#42, delimiter half). ASB publishes the same FastNet
# export as a comma CSV and as a .tdv: asb_transaction_export_01.csv and
# _02.tdv are the same statement, the same preamble and the same column names --
# only the separator differs. With the template pinning "," the tab file's header
# never split into columns at all, so it scored 0 and was reported unsupported,
# even though a committed sample of it has been sitting in the repo all along.
#
# The rule is the same all-or-nothing one as the date format: keep the delimiter
# under which the header carries EVERY fingerprinted column, and only that one.
# ---------------------------------------------------------------------------

FIXTURE_TDV <- "samples/raw/asb/asb_transaction_export_02.tdv"

test_that("the tab-delimited ASB export detects as ASB with a full score", {
  templates <- load_templates(templates_dir())
  det <- detect_statement(read_input(fixture(FIXTURE_TDV)), templates)
  expect_true(det$matched)
  expect_identical(det$template_id, "asb_everyday_csv")
  expect_gte(det$score, templates[["asb_everyday_csv"]]$min_score)
})

test_that("the .tdv parses to the SAME golden table as its comma twin", {
  # the strongest statement of the guarantee: same statement, same separator-
  # independent result, compared against the golden the CSV already had.
  expect_statement_ok(FIXTURE_TDV, EXPECTED,
                      template_id = "asb_everyday_csv", bank = "ASB")
})

test_that("the wrong separator can never 'sort of' work", {
  # resolve_delimiter must reject a candidate outright rather than fall back to a
  # partial split: a tab-delimited header split on commas is ONE field, which can
  # never carry all seven fingerprinted column names.
  templates <- load_templates(templates_dir())
  t <- templates[["asb_everyday_csv"]]
  hdr_tab   <- "Date\tUnique Id\tTran Type\tCheque Number\tPayee\tMemo\tAmount"
  hdr_comma <- "Date,Unique Id,Tran Type,Cheque Number,Payee,Memo,Amount"
  expect_identical(resolve_delimiter(hdr_tab, t), "\t")
  expect_identical(resolve_delimiter(hdr_comma, t), ",")
  # no candidate fits -> the FIRST declared one, i.e. exactly the old behaviour,
  # so the file simply fails to score and stays honestly unsupported
  expect_identical(resolve_delimiter("Something;Else;Entirely", t), ",")
})
