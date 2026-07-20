# Promote the remaining amount_sign styles to tested (build-contract section 5):
# debit_credit_cols (separate money-in/out columns) and dr_cr_suffix (123.45 DR).

test_that("debit_credit_cols: separate columns set the sign", {
  tpl <- list(id = "t", bank = "x", statement_type = "s", format = "delimited",
    version = 1, min_score = 1, currency = "NZD", delimiter = ",",
    amount_sign = "debit_credit_cols",
    columns = list(date = list(source = "Date", format = "%d/%m/%Y"),
                   description = list(source = "Description"),
                   debit = list(source = "Debit"), credit = list(source = "Credit"),
                   balance = list(source = "Balance")),
    fingerprint = list(header_contains_all = c("Date")))
  tx <- parse_statement(read_input(fixture("tests/testthat/fixtures/debit_credit_cols.csv")), tpl)$transactions
  expect_equal(nrow(tx), 3L)
  expect_equal(tx$amount[tx$description == "Salary"], 2500.00)   # credit -> +
  expect_equal(tx$amount[tx$description == "Rent"], -1200.00)    # debit  -> -
  expect_true(is.na(tx$amount[tx$description == "Opening"]))     # blank both -> NA
  expect_true(all(tx$flags == ""))                              # blank != malformed
})

test_that("dr_cr_suffix: trailing DR/CR sets the sign", {
  tpl <- list(id = "t", bank = "x", statement_type = "s", format = "delimited",
    version = 1, min_score = 1, currency = "NZD", delimiter = ",",
    amount_sign = "dr_cr_suffix",
    columns = list(date = list(source = "Date", format = "%d/%m/%Y"),
                   description = list(source = "Description"),
                   amount = list(source = "Amount"), balance = list(source = "Balance")),
    fingerprint = list(header_contains_all = c("Date")))
  tx <- parse_statement(read_input(fixture("tests/testthat/fixtures/dr_cr_suffix.csv")), tpl)$transactions
  expect_equal(nrow(tx), 2L)
  expect_equal(tx$amount[tx$description == "Salary"], 2500.00)   # CR -> +
  expect_equal(tx$amount[tx$description == "Rent"], -1200.00)    # DR -> -
  expect_true(all(tx$flags == ""))
})
