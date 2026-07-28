# Tests for the template drafter (R/draft.R).
#
# W1. THE CURRENCY IS A LABEL ON MONEY, SO A WRONG ONE IS A WRONG FIGURE.
#
# A real pound-sterling statement drafted as NZD. The transactions table printed
# a pound figure in the debit column of a row whose CURRENCY column said NZD, and
# the tiles above that same table totalled those pounds as dollars. The drafter
# had already read the symbol well enough to PRINT it -- it just stamped the
# constant anyway. The currency is now read off the money the drafter matched.
#
# Every glyph here is a \u escape, like the sources: a pound sign written straight
# into a .R file parses as two bytes of unknown encoding under the deployment's C
# locale and matches nothing.

.dc_page <- function(mark) {
  mk <- function(txt, x, y, w, h = 10) data.frame(text = txt, x = x, y = y,
    width = w, height = h, space = TRUE, stringsAsFactors = FALSE)
  rbind(
    mk(c("Date", "Details", "Money Out", "Balance"), c(19, 91, 282, 371), 227,
       c(14, 20, 34, 23)),
    mk(c("1st", "November", "2018", "ATM",
         paste0(mark, "10.00"), paste0(mark, "20.00")),
       c(19, 31, 64, 91, 303, 382), 247, c(8, 31, 14, 18, 19, 19)),
    mk(c("3rd", "November", "2018", "Cinema",
         paste0(mark, "6.50"), paste0(mark, "13.50")),
       c(19, 31, 64, 91, 307, 382), 257, c(8, 31, 14, 22, 16, 19)))
}
.dc_draft <- function(mark)
  .draft_pdf(list(kind = "pdf", words = list(.dc_page(mark))), "s", "Bank")

test_that("a drafted template carries the currency its own figures are printed in", {
  pound <- "\u00a3"
  expect_identical(.dc_draft(pound)$currency, "GBP")
  # ...and the NZ statements this is deployed for do not move.
  expect_identical(.dc_draft("$")$currency, "NZD")
  expect_identical(.dc_draft("")$currency, "NZD")
})

test_that(".draft_currency reads the symbol the money test matched", {
  pound <- "\u00a3"; euro <- "\u20ac"; yen <- "\u00a5"
  expect_identical(.draft_currency(c(paste0(pound, "10.00"),
                                     paste0(pound, "1,234.56"))), "GBP")
  expect_identical(.draft_currency(paste0(euro, "10.00")), "EUR")
  expect_identical(.draft_currency(paste0(yen, "1,000.00")), "JPY")
  # a negated figure is the same money, whichever side of the glyph the minus is
  expect_identical(.draft_currency(paste0("-", pound, "10.00")), "GBP")
  expect_identical(.draft_currency(paste0(pound, "-10.00")), "GBP")
  # a balance marker printed after the figure does not hide the symbol before it
  expect_identical(.draft_currency(paste0(pound, "17,731.96 OD")), "GBP")
})

test_that("a dollar sign names three currencies, so it names none", {
  # NZD/AUD/USD are indistinguishable from the glyph, and NZD is right here.
  expect_identical(.draft_currency(c("$10.00", "$1,234.56")), "NZD")
  expect_identical(.draft_currency(c("10.00", "1,234.56")), "NZD")
  expect_identical(.draft_currency(character(0)), "NZD")
  expect_identical(.draft_currency(NULL), "NZD")
  expect_identical(.draft_currency(NA_character_), "NZD")
})

test_that("a printed ISO code outranks the glyph, being unambiguous where it is not", {
  pound <- "\u00a3"
  expect_identical(.draft_currency(c("AUD 25.00", "AUD 4.50")), "AUD")
  expect_identical(.draft_currency("GBP10.00"), "GBP")
  expect_identical(.draft_currency(c("AUD $25.00", "AUD $4.50")), "AUD")
  expect_identical(.draft_currency(paste0("GBP ", pound, "10.00")), "GBP")
})

test_that("evidence that disagrees leaves the default standing, and does not guess", {
  pound <- "\u00a3"
  # A template carries ONE currency. Two marks on the figures cannot be resolved
  # from the page, so the analyst confirms it -- exactly as with a bank name.
  expect_identical(.draft_currency(c(paste0(pound, "10.00"), "$25.00")), "NZD")
  expect_identical(.draft_currency(c("USD 25.00", "AUD 30.00")), "NZD")
  # a mark that is not a currency this build knows is evidence of nothing
  expect_identical(.draft_currency("*10.00"), "NZD")
  # ...and a code that is not printed against a figure is not saying what the
  # statement is in. "USD 25.00" IS; "settled in USD" is not.
  expect_identical(.draft_currency(c("25.00", "settled in USD")), "NZD")
})

test_that("a delimited draft reads the money columns, never the description", {
  pound <- "\u00a3"
  p <- tempfile(fileext = ".csv")
  on.exit(unlink(p), add = TRUE)
  writeLines(c("Date,Description,Amount,Balance",
    paste0("01/05/2024,ATM,-", pound, "10.00,", pound, "990.00"),
    paste0("02/05/2024,SALARY,", pound, "500.00,", pound, "1490.00")),
    p, useBytes = TRUE)
  expect_identical(draft_template(p)$currency, "GBP")
  # A foreign purchase named in the DESCRIPTION is one transaction, not the
  # currency of the statement -- reading it would restamp every row in the file.
  writeLines(c("Date,Description,Amount,Balance",
    "01/05/2024,VISA PURCHASE USD 25.00,-25.00,990.00",
    "02/05/2024,SALARY,500.00,1490.00"), p)
  expect_identical(draft_template(p)$currency, "NZD")
})
