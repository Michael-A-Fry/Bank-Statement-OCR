# parse_amount unit coverage for ALL four sign styles, incl. edge cases
# (build-contract section 5). Guards the R `$` partial-match landmine on type_dc.

test_that("signed style: sign, direction, verbatim raw", {
  r <- parse_amount(c("-23.40", "2,000.00", "", "abc"), "signed")
  expect_equal(r$value, c(-23.40, 2000, NA, NA))
  expect_identical(r$direction, c("debit", "credit", NA, NA))
  expect_identical(r$raw, c("-23.40", "2,000.00", "", "abc"))
})

test_that("dr_cr_suffix: DR negative, CR positive, invalid suffix -> NA", {
  r <- parse_amount(c("123.45 DR", "10.00 CR", "5.00 ZZ", "7.00"),
                    "dr_cr_suffix")
  expect_equal(r$value, c(-123.45, 10.00, NA, NA))
  expect_identical(r$direction, c("debit", "credit", NA, NA))
})

test_that("debit_credit_cols: credit +, debit -, both blank -> NA (not 0)", {
  r <- parse_amount(NULL, "debit_credit_cols",
                    list(debit = c("", "5.00", ""), credit = c("10.00", "", "")))
  expect_equal(r$value, c(10.00, -5.00, NA))
  expect_identical(r$direction, c("credit", "debit", NA))
  # raw reflects whichever column carried the value
  expect_identical(r$raw[1], "10.00")
  expect_identical(r$raw[2], "5.00")
})

test_that("type_dc: type column controls sign; magnitude always absolute", {
  r <- parse_amount(c("4.50", "2000.00"), "type_dc",
                    list(type = c("D", "C"), type_debit_value = "D"))
  expect_equal(r$value, c(-4.50, 2000.00))
  expect_identical(r$direction, c("debit", "credit"))
})

test_that("type_dc: absent type key defaults to credit (no `$` partial match)", {
  # Regression: `opts$type` used to partial-match `type_debit_value`, flipping
  # every row to debit. Exact indexing must default all rows to credit.
  r <- parse_amount(c("4.50", "2000.00"), "type_dc",
                    list(type_debit_value = "D"))
  expect_equal(r$value, c(4.50, 2000.00))
  expect_identical(r$direction, c("credit", "credit"))
})

test_that("clean_description is verbatim: only outer whitespace trimmed", {
  expect_identical(
    clean_description(c("  O'Connor & Sons  ", "A  B", "café")),
    c("O'Connor & Sons", "A  B", "café"))
})
