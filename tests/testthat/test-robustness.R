# Robustness coverage for the silent-corruption classes found in the audit:
#   * money parsing: European decimal comma, space/apostrophe thousands,
#     accounting negatives (parens / trailing minus), currency symbols, and
#     DR/CR/OD balance markers -- none may be silently mis-valued.
#   * dates: ordinal suffixes ("1st April") and the 4-letter "Sept".
#   * PDF path: overdrawn balances keep their sign; an unparseable-but-present
#     amount is flagged "malformed" so no_unparsed_rows can see it; summary-line
#     detection is tight (plurals) so real transactions are never dropped.
#   * reconcile: 2-digit-year period bounds resolve for dates_within_period.

# ---- money parsing (parse_amount signed -> .num) --------------------------
test_that(".num reads every real-world money format without silent corruption", {
  v <- parse_amount(c("1.234,56", "1,234.56", "1 234.56", "1'234.56",
                      "(123.45)", "123.45-", "$-5.00", "12,34", "1,234"),
                    "signed")$value
  expect_equal(v, c(1234.56, 1234.56, 1234.56, 1234.56,
                    -123.45, -123.45, -5.00, 12.34, 1234))
})

test_that("signed amounts honour DR/OD as negative and CR as positive", {
  v <- parse_amount(c("100.00 DR", "50.00 OD", "10.00 CR", "7.00"), "signed")$value
  expect_equal(v, c(-100, -50, 10, 7))
})

test_that("currency symbols are stripped, value preserved", {
  expect_equal(parse_amount(c("£1,000.00", "€2.500,00"), "signed")$value,
               c(1000, 2500))
})

# ---- dates: ordinals + Sept, raw kept verbatim ----------------------------
test_that("parse_date handles ordinal suffixes and 4-letter Sept", {
  d <- parse_date(c("1st April 2024", "3rd September 2024", "21st June 2024"), "%d %B %Y")
  expect_equal(d$iso, c("2024-04-01", "2024-09-03", "2024-06-21"))
  d2 <- parse_date(c("12 Sept 2024", "1st Apr 2024"), "%d %b %Y")
  expect_equal(d2$iso, c("2024-09-12", "2024-04-01"))
  expect_identical(d$raw[1], "1st April 2024")            # raw kept verbatim, unmodified
})

# ---- label money extraction is sign-aware ---------------------------------
test_that(".value_from_line captures the sign marker with the money token", {
  expect_identical(.value_from_line("Closing balance 1,234.56 DR", "money"), "1,234.56 DR")
  expect_identical(.value_from_line("Opening balance (500.00)", "money"), "(500.00)")
  expect_identical(.value_from_line("Balance 1.234,56", "money"), "1.234,56")
  # a whole-dollar "1,234" with no cents must NOT be mis-read as "1,23"
  expect_true(is.na(.value_from_line("Total 1,234", "money")))
})

test_that("two labels on one line each resolve to their OWN value", {
  # opening/closing dates on a single physical line must not collapse to the
  # first date; extraction is anchored to the right of each label.
  spec_o <- list(any_of = "opening date", value = "date")
  spec_c <- list(any_of = "closing date", value = "date")
  line <- "Opening date 1 Jan 26   Closing date 31 Jan 26"
  expect_equal(match_label(spec_o, line)$value, "1 Jan 26")
  expect_equal(match_label(spec_c, line)$value, "31 Jan 26")
})

# ---- PDF: synthetic word-box helpers --------------------------------------
.rb_words <- function(rows) {
  data.frame(
    text   = vapply(rows, `[`, "", 1),
    x      = as.numeric(vapply(rows, `[`, "", 2)),
    y      = as.numeric(vapply(rows, `[`, "", 3)),
    width  = as.numeric(vapply(rows, `[`, "", 4)),
    height = rep(10, length(rows)), stringsAsFactors = FALSE)
}
.rb_input <- function(words, pages = "period from 1 Jan 2026 to 31 Jan 2026")
  list(kind = "pdf", path = tempfile(fileext = ".pdf"),
       pages = pages, words = list(words), meta = list(page_count = 1L))
.rb_tmpl <- function(style = "signed")
  list(id = "s", bank = "S", statement_type = "e", format = "pdf", version = 1,
       currency = "NZD", table = list(row_tol = 3, date_format = "%d %b",
       amount_sign = style, columns = list(
         date = list(x_min = 40, x_max = 74), description = list(x_min = 74, x_max = 360),
         amount = list(x_min = 360, x_max = 470), balance = list(x_min = 470, x_max = 545))))

test_that("an overdrawn PDF balance keeps its negative sign (OD marker)", {
  w <- .rb_words(list(c("05", 45, 40, 12), c("Jan", 60, 40, 16), c("COFFEE", 110, 40, 45),
                      c("4.50", 415, 40, 25), c("95.50", 488, 40, 30), c("OD", 520, 40, 15)))
  tx <- parse_pdf_table(.rb_input(w), .rb_tmpl("signed"))$transactions
  expect_equal(tx$balance, -95.50)
  expect_identical(tx$balance_raw, "95.50 OD")   # raw verbatim
})

test_that("a kept PDF row whose amount cannot be parsed is flagged malformed", {
  # dr_cr_suffix style, but the amount cells carry no DR/CR marker -> direction
  # unknown -> value NA. The row is still a dated money line, so it is kept and
  # must be flagged so no_unparsed_rows fails rather than passing silently.
  w <- .rb_words(list(c("05", 45, 40, 12), c("Jan", 60, 40, 16), c("RENT", 110, 40, 45), c("500.00", 415, 40, 30)))
  p <- parse_pdf_table(.rb_input(w), .rb_tmpl("dr_cr_suffix"))
  expect_true(is.na(p$transactions$amount[1]))
  expect_true(grepl("malformed", p$transactions$flags[1]))
  k <- reconcile(p)$kpis
  expect_identical(k$status[k$name == "no_unparsed_rows"], "fail")
})

test_that(".is_summary drops only true summary lines, never real transactions", {
  count_kept <- function(desc) {
    toks <- strsplit(desc, " ")[[1]]
    rows <- c(list(c("05", 45, 40, 12), c("Jan", 60, 40, 16)),
              lapply(seq_along(toks), function(i) c(toks[i], 90 + i, 40, 20)),
              list(c("10.00", 415, 40, 25), c("95.50", 488, 40, 30)))
    nrow(parse_pdf_table(.rb_input(.rb_words(rows)), .rb_tmpl("signed"))$transactions)
  }
  # real transactions that merely LOOK summary-ish -> kept
  expect_equal(count_kept("Total Credit Union deposit"), 1L)
  expect_equal(count_kept("Total Payment to ACME"), 1L)
  expect_equal(count_kept("Transfer carried forward interest"), 1L)
  # genuine summary rows -> dropped
  expect_equal(count_kept("Opening Balance"), 0L)
  expect_equal(count_kept("Balance Brought Fwd"), 0L)
  expect_equal(count_kept("Total Credits"), 0L)
  expect_equal(count_kept("Carried Forward"), 0L)
})

# ---- year-less dates with NO resolvable period: preserve, never drop -------
test_that("year-less PDF dates with no period are preserved, not dropped", {
  # No period anywhere and a year-less date_format: rather than silently drop the
  # whole statement, keep the rows with date_iso = NA + a date_unresolved flag so
  # the transactions (amount, description, verbatim raw date) survive for review.
  w <- .rb_words(list(c("13", 45, 40, 12), c("Aug", 60, 40, 16), c("COFFEE", 110, 40, 45),
                      c("4.50", 415, 40, 25), c("95.50", 488, 40, 30)))
  input <- .rb_input(w, pages = "ASB statement\nAccount 12-3456-7890123-00")  # no period, no year
  tx <- parse_pdf_table(input, .rb_tmpl("signed"))$transactions
  expect_equal(nrow(tx), 1L)                        # NOT dropped to zero
  expect_true(is.na(tx$date[1]))                    # year genuinely unknown -> no ISO
  expect_identical(tx$date_raw[1], "13 Aug")        # raw date kept verbatim
  expect_equal(tx$amount[1], 4.50)                  # data preserved
  expect_true(grepl("date_unresolved", tx$flags[1]))
})

# ---- reconcile: 2-digit-year period bounds --------------------------------
test_that("dates_within_period resolves a 2-digit-year period", {
  w <- .rb_words(list(c("05", 45, 40, 12), c("Jan", 60, 40, 16), c("COFFEE", 110, 40, 45),
                      c("4.50", 415, 40, 25), c("95.50", 488, 40, 30)))
  input <- .rb_input(w, pages = "Opening date 1 Jan 26\nClosing date 31 Jan 26")
  p <- parse_pdf_table(input, .rb_tmpl("signed"))
  k <- reconcile(p)$kpis
  expect_identical(k$status[k$name == "dates_within_period"], "pass")
})

# ---- delimited: debit_credit_cols malformed detection ---------------------
test_that("a debit_credit_cols row with an unparseable amount is flagged", {
  csv <- tempfile(fileext = ".csv")
  writeLines(c("Date,Description,Withdrawal,Deposit",
               "2024-01-05,COFFEE,xx,",       # 'xx' -> no numeric value -> malformed
               "2024-01-06,SALARY,,1000.00"), csv)
  input <- read_input(csv)
  tmpl <- list(id = "dc", bank = "S", statement_type = "e", format = "delimited",
    version = 1, currency = "NZD", amount_sign = "debit_credit_cols",
    columns = list(date = list(source = "Date", format = "%Y-%m-%d"),
      description = list(source = "Description"),
      debit = list(source = "Withdrawal"), credit = list(source = "Deposit")))
  tx <- parse_statement(input, tmpl)$transactions
  expect_true(is.na(tx$amount[1]))
  expect_true(grepl("malformed", tx$flags[1]))
  expect_equal(tx$amount[2], 1000.00)          # the clean row is unaffected
  expect_false(grepl("malformed", tx$flags[2]))
})
