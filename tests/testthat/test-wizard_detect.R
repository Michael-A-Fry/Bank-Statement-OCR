# Tests for the wizard auto-detection helpers (R/wizard_detect.R) -- the layer
# that lets a zero-background analyst confirm plain-English guesses.

test_that("delimiter detection", {
  expect_equal(detect_delimiter(fixture("samples/raw/bnz/bnz_transaction_export_01.csv")), ",")
})

test_that("date format detection disambiguates 2- vs 4-digit years", {
  expect_equal(detect_date_format(c("01/01/26", "02/01/26")), "%d/%m/%y")
  expect_equal(detect_date_format(c("31/12/2025", "01/01/2026")), "%d/%m/%Y")
  expect_equal(detect_date_format(c("2025-08-22", "2025-08-23")), "%Y-%m-%d")
  expect_equal(detect_date_format(c("31 Dec 2025")), "%d %b %Y")
  expect_equal(detect_date_format(c("")), "")
})

test_that("amount style detection", {
  expect_equal(detect_amount_style(c("Date", "Amount", "Payee")), "signed")
  df_dc <- data.frame(Card = c("x", "y"), Type = c("D", "C"), Amount = c("5", "10"),
                      stringsAsFactors = FALSE)
  expect_equal(detect_amount_style(c("Card", "Type", "Amount"), df_dc), "type_dc")
  expect_equal(detect_amount_style(c("Date", "Debit", "Credit", "Balance")), "debit_credit_cols")
  df_sfx <- data.frame(Date = "1", Amount = c("45.00 DR", "10.00 CR"), stringsAsFactors = FALSE)
  expect_equal(detect_amount_style(c("Date", "Amount"), df_sfx), "dr_cr_suffix")
})

test_that("field mapping guesses", {
  h <- c("Date", "Amount", "Payee", "Particulars", "Code", "Balance")
  expect_equal(guess_mapping(h, "description"), "Payee")
  expect_equal(guess_mapping(h, "balance"), "Balance")
  expect_equal(guess_mapping(h, "other_party"), "(none)")
})

test_that("date_format_label is human-readable", {
  expect_match(date_format_label("%d/%m/%Y"), "day/month/year", fixed = TRUE)
})
