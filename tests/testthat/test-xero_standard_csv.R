# Tests for the cross-bank Xero-standard import (one layout, many banks): the
# table reader with the fixture template, and the automatic reader with none.

XERO_BANKS <- c("anz", "asb", "bnz", "kiwibank", "westpac")
xero_fixture <- function(b) sprintf("samples/raw/%s/%s_xero_import_sample_01.csv", b, b)

test_that("the automatic reader proves every bank's Xero export, figure for figure", {
  # Each export carries a running balance, so the arithmetic proves the reading;
  # the figures must then be exactly the ones the fixture template reads.
  t <- fixture_template("xero_standard_csv")
  for (b in XERO_BANKS) {
    f <- fixture(xero_fixture(b))
    skip_if_not(file.exists(f))
    input <- read_input(f)
    rd <- auto_read(input)
    expect_identical(rd$outcome, "proven", info = b)
    want <- parse_statement(input, t)$transactions
    for (fld in c("date", "amount", "direction", "balance"))
      expect_equal(rd$transactions[[fld]], want[[fld]], info = paste(b, fld))
  }
})

test_that("debit/credit column drives the sign; balance continuity holds", {
  t <- fixture_template("xero_standard_csv")
  f <- fixture(xero_fixture("anz"))
  skip_if_not(file.exists(f))
  parsed <- parse_statement(read_input(f), t)
  recon <- reconcile(parsed, t)
  tx <- parsed$transactions
  expect_equal(nrow(tx), 8L)
  expect_equal(tx$amount[tx$description == "Payroll deposit"], 4850.00)   # credit -> +
  expect_equal(tx$amount[tx$description == "Office supplies"], -312.54)   # debit  -> -
  expect_equal(recon$kpis$status[recon$kpis$name == "running_balance_continuity"], "pass")
  expect_true(all(c("memo", "source_currency") %in% names(parsed$extras)))
  expect_true(all(tx$flags == ""))
})
