# Tests for the PDF table parser's generic features that real statements need:
#  - debit/credit as two separate columns (Withdrawals | Deposits)
#  - year-less dates ("05 Jan") with the year taken from the statement period
# Uses a SYNTHETIC word-box table (no real statement committed).

# .word(text, x, y, w) -- one word box; centre x = x + w/2 drives column choice.
.mk_words <- function() {
  rows <- list(
    # header row (y=10) -- date cell won't parse, so it is dropped
    c("Date", 45, 10, 20), c("Withdrawals", 340, 10, 50), c("Deposits", 420, 10, 40), c("Balance", 490, 10, 40),
    # 05 Jan  COFFEE            4.50 (debit)                 95.50
    c("05", 45, 40, 12), c("Jan", 60, 40, 16), c("COFFEE", 90, 40, 45),
    c("4.50", 355, 40, 22), c("95.50", 488, 40, 30),
    # 06 Jan  SALARY                        1000.00 (credit) 1095.50
    c("06", 45, 70, 12), c("Jan", 60, 70, 16), c("SALARY", 90, 70, 45),
    c("1000.00", 415, 70, 34), c("1095.50", 485, 70, 34)
  )
  data.frame(
    text   = vapply(rows, `[`, "", 1),
    x      = as.numeric(vapply(rows, `[`, "", 2)),
    y      = as.numeric(vapply(rows, `[`, "", 3)),
    width  = as.numeric(vapply(rows, `[`, "", 4)),
    height = rep(10, length(rows)),
    stringsAsFactors = FALSE)
}

.mk_input <- function() list(
  kind = "pdf",
  path = tempfile(fileext = ".pdf"),   # does not exist -> pdf_pagesize skipped
  pages = c("Statement period from 1 Jan 2026 to 31 Jan 2026\nDate Withdrawals Deposits Balance"),
  words = list(.mk_words()),
  meta = list(page_count = 1L))

.tmpl <- list(
  id = "synth_pdf", bank = "SYNTH", statement_type = "everyday", format = "pdf",
  version = 1, currency = "NZD",
  table = list(row_tol = 3, date_format = "%d %b", amount_sign = "debit_credit_cols",
    columns = list(
      date = list(x_min = 40, x_max = 80), description = list(x_min = 80, x_max = 330),
      debit = list(x_min = 330, x_max = 395), credit = list(x_min = 395, x_max = 472),
      balance = list(x_min = 472, x_max = 545))))

test_that("PDF parser splits debit/credit columns and injects the period year", {
  parsed <- parse_pdf_table(.mk_input(), .tmpl)
  tx <- parsed$transactions
  expect_equal(nrow(tx), 2L)                       # header row dropped
  expect_equal(tx$date, c("2026-01-05", "2026-01-06"))   # year 2026 from the period
  expect_equal(tx$date_raw, c("05 Jan", "06 Jan"))       # raw stays verbatim, no year
  expect_equal(tx$amount, c(-4.50, 1000.00))             # debit negative, credit positive
  expect_equal(tx$direction, c("debit", "credit"))
  expect_equal(tx$balance, c(95.50, 1095.50))
})

test_that("the two-column PDF template validates", {
  expect_length(validate_template(c(.tmpl, list(
    min_score = 1, fingerprint = list(page_contains_all = list("Withdrawals"))))), 0)
})
