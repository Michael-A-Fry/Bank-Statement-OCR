# Reconciliation KPI coverage (build-contract sections 8, 11.3). Exercises the
# pass AND fail branches that no bank fixture reaches: balance_reconciliation,
# dates_within_period, running_balance_continuity(fail), transaction_count
# (stated), and no_unparsed_rows completeness.

# .parsed(...) -- assemble a minimal parsed object from a core-shaped list plus
# header overrides, so KPI branches can be driven directly.
.parsed <- function(tx, header = list(), source_line_count = NA_integer_,
                    multiline_extra = 0L) {
  base_h <- list(
    bank = "X", statement_type = "e", template_id = "t", template_version = 1,
    account_number = NA, account_name = NA, period_start = NA, period_end = NA,
    opening_balance = NA_real_, closing_balance = NA_real_, currency = "NZD",
    source_file = "x.csv", source_sha256 = "s", page_count = NA, row_count = nrow(tx))
  for (k in names(header)) base_h[[k]] <- header[[k]]
  list(transactions = coerce_core(tx),
       extras = data.frame(row_id = integer(0)),
       header = base_h,
       provenance = data.frame(row_id = seq_len(nrow(tx))),
       source_line_count = source_line_count,
       multiline_extra = multiline_extra)
}

.tx <- function(amount, balance = NA, date = NA, flags = "") {
  data.frame(row_id = seq_along(amount), amount = amount, balance = balance,
             date = date, flags = flags, stringsAsFactors = FALSE)
}

test_that("balance_reconciliation PASSES on consistent opening/sum/closing", {
  p <- .parsed(.tx(c(-10, 40, -5)),
               header = list(opening_balance = 100, closing_balance = 125),
               source_line_count = 3)
  k <- reconcile(p)$kpis
  expect_equal(k$status[k$name == "balance_reconciliation"], "pass")
})

test_that("balance_reconciliation FAILS and trust is low when tampered", {
  p <- .parsed(.tx(c(-10, 40, -5)),
               header = list(opening_balance = 100, closing_balance = 999),
               source_line_count = 3)
  r <- reconcile(p)
  expect_equal(r$kpis$status[r$kpis$name == "balance_reconciliation"], "fail")
  expect_equal(r$trust$level, "low")
})

test_that("dates_within_period flags an out-of-period date", {
  p_ok <- .parsed(.tx(c(-10, 40), date = c("2020-01-05", "2020-01-20")),
                  header = list(period_start = "2020-01-01", period_end = "2020-01-31"),
                  source_line_count = 2)
  expect_equal(reconcile(p_ok)$kpis$status[
    reconcile(p_ok)$kpis$name == "dates_within_period"], "pass")
  p_bad <- .parsed(.tx(c(-10, 40), date = c("2020-01-05", "2020-02-20")),
                   header = list(period_start = "2020-01-01", period_end = "2020-01-31"),
                   source_line_count = 2)
  expect_equal(reconcile(p_bad)$kpis$status[
    reconcile(p_bad)$kpis$name == "dates_within_period"], "fail")
})

test_that("running_balance_continuity FAILS on a broken balance column", {
  # balance jumps inconsistently with amount at row 2.
  p <- .parsed(.tx(c(-9, -15, -3.15),
                   balance = c(895.69, 999.99, 877.54)),
               source_line_count = 3)
  r <- reconcile(p)
  expect_equal(r$kpis$status[r$kpis$name == "running_balance_continuity"], "fail")
  expect_equal(r$trust$level, "low")
})

test_that("running_balance_continuity fail branch via a real fixture", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("tests/testthat/fixtures/kiwibank_broken_balance.csv"))
  p <- parse_statement(input, templates[["kiwibank_everyday_csv"]])
  r <- reconcile(p, templates[["kiwibank_everyday_csv"]])
  expect_equal(r$kpis$status[r$kpis$name == "running_balance_continuity"], "fail")
  expect_equal(r$trust$level, "low")
})

test_that("a signed statement with one-sign amounts and no balance fails direction (P2-10)", {
  signed <- list(amount_sign = "signed")
  # all money-in, no balance to cross-check -> unverifiable direction -> fail.
  p_pos <- .parsed(.tx(c(10, 20, 30)), source_line_count = 3)
  k <- reconcile(p_pos, signed)$kpis
  expect_equal(k$status[k$name == "amount_direction"], "fail")
  # mixed signs -> the +/- is genuinely present -> no such KPI.
  p_mix <- .parsed(.tx(c(-10, 20)), source_line_count = 2)
  expect_false("amount_direction" %in% reconcile(p_mix, signed)$kpis$name)
  # one-sign but WITH a balance column -> continuity can verify -> no such KPI.
  p_bal <- .parsed(.tx(c(10, 20), balance = c(110, 130)), source_line_count = 2)
  expect_false("amount_direction" %in% reconcile(p_bal, signed)$kpis$name)
})

test_that("continuity bridges a blank middle balance, catching a hidden break (P2-3)", {
  # 100, NA, 130 with amounts 0/+20/+5: the 130 should be 125. The old loop
  # skipped both pairs around the NA and reported a clean pass; the bridge sees it.
  p <- .parsed(.tx(c(0, 20, 5), balance = c(100, NA, 130)), source_line_count = 3)
  k <- reconcile(p)$kpis
  expect_equal(k$status[k$name == "running_balance_continuity"], "fail")
  # a bridge that DOES reconcile (100 + 20 + 5 == 125) passes.
  p2 <- .parsed(.tx(c(0, 20, 5), balance = c(100, NA, 125)), source_line_count = 3)
  expect_equal(reconcile(p2)$kpis$status[
    reconcile(p2)$kpis$name == "running_balance_continuity"], "pass")
  # a blank amount INSIDE the gap makes the bridge unverifiable -> surfaced, not
  # silently passed as verified.
  p3 <- .parsed(.tx(c(0, NA, 5), balance = c(100, NA, 130)), source_line_count = 3)
  det <- reconcile(p3)$kpis$detail[reconcile(p3)$kpis$name == "running_balance_continuity"]
  expect_match(det, "unverifiable")
})

test_that("no_unparsed_rows does NOT fail on a legitimate multi-line record (P2-2)", {
  # 3 physical data lines, 2 parsed rows, 1 extra line accounted for by a quoted
  # embedded newline -> complete, must PASS (was crying wolf on every such file).
  p <- .parsed(.tx(c(-10, 40)), source_line_count = 3, multiline_extra = 1)
  k <- reconcile(p)$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  # but an UNaccounted extra line (no multi-line record) still fails loudly.
  p2 <- .parsed(.tx(c(-10, 40)), source_line_count = 3, multiline_extra = 0)
  expect_equal(reconcile(p2)$kpis$status[
    reconcile(p2)$kpis$name == "no_unparsed_rows"], "fail")
})

test_that("transaction_count matches / mismatches a stated count", {
  p_match <- .parsed(.tx(c(-10, 40)), header = list(stated_count = 2),
                     source_line_count = 2)
  expect_equal(reconcile(p_match)$kpis$status[
    reconcile(p_match)$kpis$name == "transaction_count"], "pass")
  p_miss <- .parsed(.tx(c(-10, 40)), header = list(stated_count = 5),
                    source_line_count = 2)
  expect_equal(reconcile(p_miss)$kpis$status[
    reconcile(p_miss)$kpis$name == "transaction_count"], "fail")
})

test_that("no_unparsed_rows FAILS when a source line was not accounted for", {
  # 3 source data lines but only 2 parsed rows -> a record was lost/merged.
  p <- .parsed(.tx(c(-10, 40)), source_line_count = 3)
  k <- reconcile(p)$kpis
  row <- k[k$name == "no_unparsed_rows", ]
  expect_equal(row$status, "fail")
  expect_equal(as.integer(row$expected), 3L)
})

test_that("no_unparsed_rows FAILS on a malformed row even if counts align", {
  p <- .parsed(.tx(c(-10, 40), flags = c("malformed", "")), source_line_count = 2)
  k <- reconcile(p)$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "fail")
})
