# test-auto-read.R -- the automatic reader and its arithmetic prover (spec section
# 4, Appendix A1): token typing, cells, the column model, roles by arithmetic,
# every hard check, the repair search, CSV/Excel, layouts, and metamorphic tests on
# a shipped fixture. The rule under test throughout: a reading is called automatic
# only when it is right; anything the statement cannot prove goes to a person.
#
# Statements are written here as lines of text, so each test says exactly what is
# on the page: one word box per word, 5pt a character, 12pt a line, so runs of
# spaces are the gaps between cells.

ar_pdf <- function(..., width = 600) {
  pages <- list(...)
  words <- lapply(pages, function(lines) {
    rows <- lapply(seq_along(lines), function(i) {
      m <- gregexpr("\\S+", lines[i])[[1]]
      if (m[1] < 0) return(NULL)
      tx <- regmatches(lines[i], list(m))[[1]]
      data.frame(width = nchar(tx) * 5, height = 8, x = 20 + (as.numeric(m) - 1) * 5,
                 y = 20 + i * 12, space = TRUE, text = tx, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  })
  list(kind = "pdf", path = "", sha256 = NA_character_,
       pages = vapply(pages, paste, "", collapse = "\n"), words = words,
       page_width = rep(width, length(pages)), page_height = rep(800, length(pages)),
       page_ocr = rep(FALSE, length(pages)), meta = list())
}

ar_head <- c("Kauri Bank                         Statement period 1 Feb 2026 to 28 Feb 2026", "",
             "Date     Details                          Withdrawals     Deposits      Balance")
ar_rows <- c(
  "         Opening balance                                                  1,000.00",
  "03 Feb   EFTPOS RIVERSIDE DAIRY                      12.40                    987.60",
  "05 Feb   SALARY MATAI HOLDINGS                                  3,120.00    4,107.60",
  "09 Feb   DD CITY COUNCIL RATES                      268.15                  3,839.45",
  "14 Feb   TRANSFER TO SAVINGS                        400.00                  3,439.45",
  "21 Feb   VISA HARBOUR FUEL                           96.72                  3,342.73",
  "26 Feb   CREDIT INTEREST                                            2.18    3,344.91",
  "         Closing balance                                                  3,344.91")
ar_want <- c(-12.40, 3120.00, -268.15, -400.00, -96.72, 2.18)

key_of <- function(rd) paste(rd$transactions$date, sprintf("%.2f", rd$transactions$amount))
ar_auto <- function(rd) rd$outcome %in% c("proven", "layout_match")
ok_of <- function(rd, name) rd$checks$ok[rd$checks$check == name]

# A reading is either the right one or not automatic: the one rule every
# metamorphic test below asserts.
expect_right_or_flagged <- function(rd, want_amounts) {
  if (ar_auto(rd)) expect_equal(round(rd$transactions$amount, 2), want_amounts)
  else expect_true(rd$outcome %in% c("check", "unread"))
}

# ---- token typing -------------------------------------------------------------------

test_that("a bare number is never a date; real date shapes are, with every format they read under", {
  f <- .ar_date_formats()
  r <- .ar_date_fmts(c("12", "2026", "03 Feb", "2026-02-03", "03/02/2026", "Feb", "12.50"), f)
  expect_equal(r[1:2], c("", ""))
  expect_true(grepl("%d %b", r[3], fixed = TRUE))
  expect_true(grepl("%Y-%m-%d", r[4], fixed = TRUE))
  expect_true(grepl("%d/%m/%Y", r[5], fixed = TRUE) && grepl("%m/%d/%Y", r[5], fixed = TRUE))
  expect_equal(r[6:7], c("", ""))
})

test_that("money is two decimals; units, times and references are not", {
  m <- function(s) grepl(.AR_MONEY_RX, s, perl = TRUE)
  expect_true(all(m(c("12.50", "1,234.56", "(5.00)", "-5.00", "5.00-", "150.00CR", "3,120.00", "1.234,56"))))
  expect_false(any(m(c("12", "12:30", "203.3800", "2.5", "06-5338-3559428-003", "INV"))))
})

test_that("each figure says how it carries its sign", {
  expect_equal(.ar_sign_kind(c("12.50 CR", "12.50DR", "(12.50)", "-12.50", "12.50-", "+12.50", "12.50", "5.00 OD")),
               c("CR", "DR", "()", "-lead", "-trail", "+", "", "OD"))
})

test_that("OCR's glued date pieces are split; figures are never touched", {
  w <- data.frame(x = c(10, 60), y = 10, width = c(30, 40), height = 8, text = c("01Dec", "1,234.50"),
                  stringsAsFactors = FALSE)
  s <- .ar_ocr_split(w)
  expect_equal(s$text, c("01", "Dec", "1,234.50"))
  expect_equal(s$x[1:2], c(10, 22))
})

# ---- cells and the column model --------------------------------------------------------

test_that("a figure set inside a description with a word space is part of it, not a cell", {
  pg <- .ar_page(ar_pdf(c("03 Feb   VISA HARBOUR USD 25.00 FUEL        96.72      3,342.73"))$words[[1]], 1,
                 list(width = 600, height = 800), 600, 800, 3, .ar_date_formats(), .ar_markers())
  ph <- pg$ph[pg$ph$kind == "money", ]
  expect_equal(ph$standalone, c(FALSE, TRUE, TRUE))
  expect_equal(ph$text[1], "USD 25.00")
})

test_that("one stray figure can never become a column", {
  rows <- ar_rows; rows[6] <- "21 Feb   VISA HARBOUR FUEL       USD 25.00           96.72                  3,342.73"
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  roles <- rd$template$auto$roles
  expect_equal(roles[roles != "other"], c("debit", "credit", "balance"))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), ar_want)
  # The stray figure is kept with its row, as words, never as money.
  expect_true(any(grepl("25.00", unlist(rd$parsed$extras[5, ]), fixed = TRUE)) ||
              grepl("25.00", rd$transactions$description[5], fixed = TRUE))
})

test_that("figures are grouped by their right edge, and a page printed further right still lines up", {
  p2 <- c(ar_head[3],
    "28 Feb   ATM WITHDRAWAL                              40.00                  3,304.91",
    "         Closing balance                                                  3,304.91")
  p2 <- paste0("      ", p2)   # the whole table 30pt to the right on page 2
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows[-8]), p2))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(ar_want, -40.00))
  expect_equal(length(rd$template$table$columns_by_page), 2L)
  expect_gt(rd$template$table$columns_by_page[[2]]$balance$x_min, rd$template$table$columns_by_page[[1]]$balance$x_min)
})

test_that("the reading fills the whole contract, signature included", {
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  expect_true(all(c("outcome", "why", "template", "parsed", "recon", "transactions", "proof", "checks",
                    "candidates", "columns", "matched_layout") %in% names(rd)))
  expect_true(all(c("kind", "links", "held", "unique", "pages_with_rows", "pages_used", "derived") %in% names(rd$proof)))
  expect_equal(names(rd$checks), c("check", "ok", "why"))
  expect_equal(names(rd$candidates), c("source", "passed", "why"))
  expect_true(all(c("page", "field", "kind", "x_min", "x_max", "ink_min", "ink_max", "heading") %in% names(rd$columns)))
  sg <- rd$template$signature
  expect_equal(names(sg), c("kind", "roles", "date_format", "money_style", "sign_markers", "balance_freq",
                            "newest_first", "heading_tokens", "producer", "rel_x", "extras", "col_headings"))
  expect_equal(sg$kind, "pdf")
  expect_equal(sg$roles, c("date", "description", "debit", "credit", "balance"))
  expect_equal(sg$date_format, "%d %b")
  expect_equal(sg$money_style, "debit_credit_cols")
  expect_equal(sg$balance_freq, "every")
  expect_false(sg$newest_first)
  expect_true(all(c("balance", "deposits", "withdrawals") %in% sg$heading_tokens))
  # The heading over each column, in the roles' order.
  expect_equal(sg$col_headings, c("date", "details", "withdrawals", "deposits", "balance"))
  expect_equal(length(sg$rel_x), 5L)
  expect_true(is.character(sg$extras))
  expect_equal(rd$proof$kind, "chain")
  expect_true(rd$proof$unique)
})

# ---- roles by arithmetic ------------------------------------------------------------------

test_that("a payee column is words, never a column of figures to choose a money role for", {
  # Westpac prints "Name of other party" beside its details. Only other1, other2
  # (figures the reader could not place) are money; other_party is text, so Please
  # check never offers it a "Money out" dropdown and the layout names it as itself.
  h <- c("Kauri Bank                         Statement period 1 Feb 2026 to 28 Feb 2026", "",
         "Date     Party     Details                               Withdrawals     Deposits      Balance")
  rows <- c(
    "         Opening balance                                                            1,000.00",
    "03 Feb   Dairy     EFTPOS RIVERSIDE DAIRY 4410 KHANDALLAH        12.40                  987.60",
    "05 Feb   Matai     SALARY MATAI HOLDINGS LIMITED FEB                      3,120.00    4,107.60",
    "09 Feb   Council   DIRECT DEBIT CITY COUNCIL RATES Q3           268.15                3,839.45",
    "14 Feb   Own       TRANSFER TO SAVINGS ACCOUNT 02 ONLINE        400.00                3,439.45",
    "21 Feb   Harbour   VISA HARBOUR FUEL STOP 9921 PETONE            96.72                3,342.73",
    "26 Feb   Kauri     CREDIT INTEREST PAID FOR FEBRUARY                          2.18    3,344.91",
    "         Closing balance                                                            3,344.91")
  rd <- auto_read(ar_pdf(c(h, rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(unique(rd$columns$kind[rd$columns$field == "other_party"]), "text")
  expect_true("other_party" %in% rd$template$signature$roles)
  expect_false("other" %in% rd$template$signature$roles)
})

test_that("the arithmetic, not the heading, decides which column is money out", {
  # An opening balance marked CR leaves the arithmetic one way to read the
  # columns, whatever the (swapped) headings say.
  h <- ar_head; h[3] <- "Date     Details                          Deposits     Withdrawals      Balance"
  rows <- ar_rows; rows[1] <- "         Opening balance                                               1,000.00 CR"
  rd <- auto_read(ar_pdf(c(h, rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), ar_want)
  # With nothing marked, the reading and its negation both add up, and the
  # swapped headings contradict the rows' wording: a person decides.
  expect_right_or_flagged(auto_read(ar_pdf(c(h, ar_rows))), ar_want)
})

test_that("a statement listing the newest transaction first proves the other way round", {
  rows <- c(ar_rows[8], rev(ar_rows[2:7]), ar_rows[1])
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_equal(rd$outcome, "proven")
  expect_true(rd$template$signature$newest_first)
  expect_equal(round(rd$transactions$amount, 2), rev(ar_want))
})

# N226: with no running balance the arithmetic holds read either way round, so the
# reading took oldest first and told a file listed newest first, consistently,
# that "the dates go backwards at row 2". The dates settle what the arithmetic
# leaves open, and every reason says which way round the statement reads and why.
test_that("newest first with nothing in the arithmetic to say so: the dates say it, and the reasons are true", {
  h <- c(ar_head[1:2], "Date     Details                          Withdrawals     Deposits")
  rows <- c("09 Feb   DD CITY COUNCIL RATES                      268.15",
            "05 Feb   SALARY MATAI HOLDINGS                                  3,120.00",
            "03 Feb   EFTPOS RIVERSIDE DAIRY                      12.40")
  rd <- auto_read(ar_pdf(c(h, "         Opening balance          1,000.00", rows, "         Closing balance          3,839.45")))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-268.15, 3120, -12.40))
  expect_true(rd$template$signature$newest_first)
  expect_match(rd$why, "newest transaction first, as its dates show", fixed = TRUE)
  expect_identical(rd$checks$why[rd$checks$check == "dates_in_order"], "The dates run in order, newest first.")
  # Nothing to add up at all: a person looks, told the real reason, not the order.
  ex <- auto_read(list(kind = "delimited", path = "", sha256 = NA_character_, meta = list(ext = "csv"),
                       lines = c("Date,Details,Amount", "19/02/2026,DD CITY COUNCIL RATES,-268.15",
                                 "15/02/2026,SALARY MATAI HOLDINGS,3120.00", "13/02/2026,EFTPOS RIVERSIDE DAIRY,-12.40")))
  expect_equal(ex$outcome, "check")
  expect_false(grepl("backwards", ex$why))
  expect_true(ok_of(ex, "dates_in_order"))
  # A row out of place in a newest-first statement is named in its own order.
  bad <- c(ar_rows[8], rev(ar_rows[2:7]), ar_rows[1])
  bad[5] <- sub("^09 Feb", "15 Feb", bad[5])
  rb <- auto_read(ar_pdf(c(ar_head, bad)))
  expect_false(ar_auto(rb))
  expect_identical(rb$checks$why[rb$checks$check == "dates_in_order"],
                   "Row 4 is dated after the row above it, but the statement lists the newest transaction first.")
  # Oldest first, as ever.
  ro <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  expect_identical(ro$checks$why[ro$checks$check == "dates_in_order"], "The dates run in order.")
})

test_that("a balance printed once per day proves the day's rows together", {
  rows <- ar_rows
  rows[3] <- "05 Feb   SALARY MATAI HOLDINGS                                  3,120.00"
  rows[4] <- "05 Feb   DD CITY COUNCIL RATES                      268.15                  3,839.45"
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(rd$template$signature$balance_freq, "some")
  expect_equal(round(rd$transactions$amount, 2), ar_want)
})

test_that("unsigned amounts take the one sign the balance allows, or none at all", {
  h <- c(ar_head[1:2], "Date     Details                                  Amount       Balance")
  rows <- c("         Opening balance                                       1,000.00",
            "03 Feb   EFTPOS RIVERSIDE DAIRY                   12.40         987.60",
            "05 Feb   SALARY MATAI HOLDINGS                 3,120.00       4,107.60",
            "09 Feb   DD CITY COUNCIL RATES                   268.15       3,839.45")
  rd <- auto_read(ar_pdf(c(h, rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-12.40, 3120.00, -268.15))
  expect_true(all(grepl("sign_from_balance", rd$transactions$flags)))
  # Two unsigned amounts under one balance step: 50 - 50 and -50 + 50 both fit.
  ch <- .ar_chain(c(50, 50), c(TRUE, TRUE), c(NA, 1000), data.frame(pos = 0, val = 1000, src = "open@summary"), "old")
  expect_true(ch$steps$ambiguous[1])
  expect_true(all(is.na(ch$signs)))
})

test_that("a reading and its exact negation are told apart by the account type, never guessed", {
  card <- c("Kiwibank                           Credit card statement",
            "Card number 4086 **** 7146        Statement period 1 May 2025 to 31 May 2025",
            "Credit limit 10,000.00            Minimum payment 20.00",
            "Previous balance                 1,000.00",
            "New balance                        900.00", "",
            "Date         Details                                   Amount",
            "03/05/2025   CORNER FOODS                               50.00",
            "12/05/2025   PAYMENT RECEIVED THANK YOU               -200.00",
            "20/05/2025   MAIN ST CAFE                               50.00")
  rd <- auto_read(ar_pdf(card))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-50, 200, -50))
  expect_true(rd$template$auto$liab)
  # The same rows with nothing saying it is a card: the arithmetic cannot choose.
  plain <- card[-c(1:3)]
  rd2 <- auto_read(ar_pdf(plain))
  expect_false(ar_auto(rd2))
})

test_that("an advert for a credit card never turns an everyday account round", {
  h <- c("Kauri Bank   Everyday account   Statement period 1 Feb 2026 to 28 Feb 2026",
         "Apply for a credit card today: no annual fee, credit limit up to 10,000.", "",
         "Date     Details                                  Amount       Balance")
  rows <- c("         Opening balance                                       1,000.00",
            "03 Feb   EFTPOS RIVERSIDE DAIRY                  -12.40         987.60",
            "05 Feb   SALARY MATAI HOLDINGS                 3,120.00       4,107.60",
            "09 Feb   ATM WITHDRAWAL QUEEN ST                -268.15       3,839.45")
  rd <- auto_read(ar_pdf(c(h, rows)))
  expect_right_or_flagged(rd, c(-12.40, 3120, -268.15))
})

test_that("a zero printed in the money-out column stays a money-out zero", {
  rows <- c(ar_rows[1:2], "04 Feb   MONTHLY FEE WAIVED                          0.00                    987.60", ar_rows[3:8])
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_equal(rd$outcome, "proven")
  expect_identical(1 / rd$transactions$amount[2], -Inf)
})

test_that("a row printed over two lines, date above its figures, is one row", {
  rows <- c(ar_rows[1],
            "03 Feb   EFTPOS PURCHASE",
            "         RIVERSIDE DAIRY                             12.40                    987.60",
            ar_rows[3:8])
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(rd$transactions$date[1], "2026-02-03")
  expect_equal(round(rd$transactions$amount, 2), ar_want)
})

test_that("summary figures printed in a box beside other print are found", {
  pg <- .ar_page(ar_pdf(c("RD 2                          Previous balance          1,497.28"))$words[[1]], 1,
                 list(width = 600, height = 800), 600, 800, 3, .ar_date_formats(), .ar_markers())
  expect_equal(pg$seg$class, "open")
  expect_equal(pg$seg$value_text, "1,497.28")
  expect_equal(.ar_anchor_class(c("total for card ending 5133 j sample", "total fitness gym", "closing totals",
                                  "balance c/f", "previous balance")),
               c("total", "", "total", "close", "open"))
})

test_that("a card's sections and its totals line are never rows", {
  card <- c("Kiwibank   Credit card statement   Credit limit 10,000.00   Minimum payment 20.00",
            "Card number 4086 **** 7146        Statement period 1 May 2025 to 31 May 2025",
            "Opening balance                 1,000.00",
            "Closing balance                   900.00", "",
            "Date         Details                                   Amount",
            "             J SAMPLE 4777 **** 5133",
            "03/05/2025   CORNER FOODS                               50.00",
            "12/05/2025   PAYMENT RECEIVED THANK YOU               -200.00",
            "             Total for card ending 5133               -150.00",
            "             K SAMPLE 4777 **** 7406",
            "20/05/2025   MAIN ST CAFE                               50.00",
            "             Total for card ending 7406                 50.00")
  rd <- auto_read(ar_pdf(card))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-50, 200, -50))
})

test_that("a bundle of statements is proven statement by statement", {
  box <- function(p0, p1, op, cl) c(paste("Kauri Bank                         Statement period", p0, "to", p1),
    paste("Opening balance", op), paste("Closing balance", cl), "", ar_head[3])
  s1 <- c(box("1 Feb 2026", "28 Feb 2026", "1,000.00", "3,344.91"), ar_rows[2:7])
  s2 <- c(box("1 Mar 2026", "31 Mar 2026", "3,344.91", "3,304.91"),
          "02 Mar   ATM WITHDRAWAL                              40.00                  3,304.91")
  rd <- auto_read(ar_pdf(s1, s2))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(ar_want, -40))
})

# N219: a home loan prints a "Loan summary" box (opening balance, interest,
# repayments, closing balance, rate) besides its table's own opening and closing
# lines, and often again on its next page. Two "opening balance" wordings are not
# two statements: they state one statement's balance twice.
ar_loan_box <- c("Kauri Bank                         Home loan statement",
  "Loan account 12-3456-0123456-90    Statement period 1 Feb 2026 to 28 Feb 2026", "",
  "Loan summary",
  "Opening balance                    250,000.00 DR",
  "Interest charged                     1,027.40",
  "Repayments                           2,000.00",
  "Closing balance                    249,027.40 DR",
  "Interest rate                          5.29%", "",
  "Date     Details                         Debits      Credits         Balance")
ar_loan_rows <- c(
  "         Opening balance                                          250,000.00 DR",
  "05 Feb   REPAYMENT                                  1,000.00      249,000.00 DR",
  "19 Feb   REPAYMENT                                  1,000.00      248,000.00 DR",
  "28 Feb   INTEREST                      1,027.40                   249,027.40 DR",
  "         Closing balance                                          249,027.40 DR")

test_that("a loan's summary box, printed beside its table or on every page, is one statement", {
  for (inp in list(ar_pdf(c(ar_loan_box, ar_loan_rows)),
                   ar_pdf(c(ar_loan_box, ar_loan_rows[1:3]), c(ar_loan_box, ar_loan_rows[4:5])))) {
    rd <- auto_read(inp)
    expect_equal(rd$outcome, "proven")
    expect_equal(round(rd$transactions$amount, 2), c(1000, 1000, -1027.40))
    m <- extract_metadata(inp)
    expect_equal(m$n_balance_blocks, 1L)
    mu <- detect_multiple_statements(inp, m)
    expect_false(mu$likely_multiple)
    expect_false(any(grepl("block appears", mu$reasons)))
    expect_null(bundle_segments(inp, m))
  }
  # The box alone, out of the table, never becomes a second statement's.
  a <- list(list(class = "open", in_table = FALSE, before_rows = 0L, value_text = "250,000.00", figs = NA),
            list(class = "open", in_table = FALSE, before_rows = 2L, value_text = "250,000.00", figs = NA))
  expect_false(.ar_separate_boxes(a, 3L, FALSE, "auto"))
  a[[2]]$value_text <- "248,000.00"
  expect_true(.ar_separate_boxes(a, 3L, FALSE, "auto"))
  expect_false(.ar_separate_boxes(a, 2L, FALSE, "auto"))   # the second box starts no rows
})

test_that("a page set on its side is turned upright", {
  inp <- ar_pdf(c(ar_head, ar_rows))
  w <- inp$words[[1]]
  side <- w; side$x <- 800 - (w$y + w$height); side$y <- w$x; side$width <- w$height; side$height <- w$width
  inp$words[[1]] <- side; inp$page_width <- 800; inp$page_height <- 600
  rd <- auto_read(inp)
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), ar_want)
})

test_that("a scan's leftover skew is measured from its lines and taken out", {
  w <- ar_pdf(c(ar_head, ar_rows))$words[[1]]
  a <- 0.8 * pi / 180
  sk <- w; sk$y <- w$y + (w$x - 300) * sin(a)
  back <- .ar_deskew(sk, 600, 800)
  ln <- .group_rows(sort(back$y), 3)
  expect_equal(length(unique(ln)), length(unique(.group_rows(sort(w$y), 3))))
  expect_identical(.ar_deskew(w, 600, 800), w)
})

# ---- every hard check ----------------------------------------------------------------------

test_that("every hard check is reported, and a statement that passes them all is proven", {
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  want <- c("rows_read", "rows_match_columns", "pages_with_rows", "words_used_once", "lines_accounted",
            "dates_settled", "balance_chain", "chain_across_pages", "opening_closing", "printed_totals",
            "dates_readable", "dates_in_order", "dates_in_period", "signs_settled", "no_derived_amounts",
            "amounts_read", "unique", "rows_proven")
  expect_true(all(want %in% rd$checks$check))
  expect_false(any(rd$checks$ok %in% FALSE))
  expect_equal(rd$outcome, "proven")
})

test_that("a balance step that does not add up holds the statement back", {
  rows <- ar_rows; rows[5] <- "14 Feb   TRANSFER TO SAVINGS                        410.00                  3,439.45"
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_false(ar_auto(rd))
  expect_false(ok_of(rd, "balance_chain"))
})

test_that("a printed closing balance that the movements do not reach holds it back", {
  rows <- ar_rows; rows[8] <- "         Closing balance                                                  3,400.00"
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_false(ar_auto(rd))
})

test_that("printed totals must match the rows", {
  tot <- "         Total                                      777.27     3,122.18"
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows[1:7], tot, ar_rows[8])))
  expect_equal(rd$outcome, "proven")
  expect_true(ok_of(rd, "printed_totals"))
  bad <- "         Total                                      700.00     3,122.18"
  rd2 <- auto_read(ar_pdf(c(ar_head, ar_rows[1:7], bad, ar_rows[8])))
  expect_false(ar_auto(rd2))
  expect_false(ok_of(rd2, "printed_totals"))
})

test_that("dates must read, run in order and sit inside the period", {
  rows <- ar_rows; rows[3] <- "02 Feb   SALARY MATAI HOLDINGS                                  3,120.00    4,107.60"
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_false(ar_auto(rd)); expect_false(ok_of(rd, "dates_in_order"))
  h <- ar_head; h[1] <- "Kauri Bank                         Statement period 1 Mar 2026 to 31 Mar 2026"
  rd2 <- auto_read(ar_pdf(c(h, ar_rows)))
  expect_false(ar_auto(rd2)); expect_false(ok_of(rd2, "dates_in_period"))
})

test_that("a date that reads as day-month and month-day equally well is not proven", {
  h <- c("Kauri Bank", "", ar_head[3])
  rows <- c("           Opening balance                                                  1,000.00",
            "01/02/2026 EFTPOS RIVERSIDE DAIRY                      12.40                    987.60",
            "01/03/2026 SALARY MATAI HOLDINGS                                  3,120.00    4,107.60",
            "02/04/2026 DD CITY COUNCIL RATES                      268.15                  3,839.45")
  rd <- auto_read(ar_pdf(c(h, rows)))
  expect_false(ar_auto(rd))
  expect_false(ok_of(rd, "dates_settled"))
})

test_that("an amount filled in from the balance always goes to a person", {
  rows <- ar_rows; rows[5] <- "14 Feb   TRANSFER TO SAVINGS                                                3,439.45"
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_equal(rd$outcome, "check")
  expect_false(ok_of(rd, "no_derived_amounts"))
  expect_equal(rd$proof$derived, 1L)
  expect_equal(round(rd$transactions$amount[4], 2), -400)
})

test_that("a page with transaction lines that gave no rows holds the statement back", {
  # Page 2 prints its dates another way; the table reader reads page 1's way, so
  # page 2's rows do not come through -- and that must stop the reading.
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows[1:7]),
    c("Date         Details                      Withdrawals     Deposits      Balance",
      "2026-02-28   ATM WITHDRAWAL                          40.00                  3,304.91")))
  expect_false(ar_auto(rd))
  expect_true(any(rd$checks$ok[rd$checks$check %in% c("pages_with_rows", "rows_match_columns")] %in% FALSE))
})

test_that("a dated line with a figure after the last balance is never read as proven", {
  stray <- c("", "", "", "", "28 Feb   Interest rate effective this period          4.25")
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows, stray)))
  expect_false(ar_auto(rd))
})

test_that("a description running into a figure column is read right or held back", {
  rows <- ar_rows
  rows[3] <- "05 Feb   SALARY MATAI HOLDINGS LIMITED WAGES WEEK 6             3,120.00    4,107.60"
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_right_or_flagged(rd, ar_want)
  expect_true("words_used_once" %in% rd$checks$check)
})

test_that("without a running balance or totals nothing is proven; the reading is shown for checking", {
  h <- c(ar_head[1:2], "Date     Details                          Withdrawals     Deposits")
  rows <- c("03 Feb   EFTPOS RIVERSIDE DAIRY                      12.40",
            "05 Feb   SALARY MATAI HOLDINGS                                  3,120.00",
            "09 Feb   DD CITY COUNCIL RATES                      268.15")
  rd <- auto_read(ar_pdf(c(h, rows)))
  expect_equal(rd$outcome, "check")
  expect_equal(rd$proof$kind, "none")
})

test_that("each account of a combined statement is proven from its own opening to its own closing", {
  acct2 <- c("Online saver",
    "Date     Details                          Withdrawals     Deposits      Balance",
    "         Opening balance                                                  9,000.00",
    "05 Feb   TFR FROM EVERYDAY                                     500.00    9,500.00",
    "         Closing balance                                                  9,500.00")
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows, acct2)))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(ar_want, 500))
})

test_that("a missing page between carried-forward and brought-forward balances is caught", {
  p1 <- c(ar_head, ar_rows[1:4], "         Balance carried forward                                          3,839.45")
  p3 <- c(ar_head[3], "         Balance brought forward                                          3,000.00",
          "26 Feb   CREDIT INTEREST                                            2.18    3,002.18")
  rd <- auto_read(ar_pdf(p1, p3))
  expect_false(ar_auto(rd))
})

# ---- guards against a quiet wrong answer (each case below was once proven: wrong, or by a guess) ---

ar_stag <- c(ar_rows[1],
  "03 Feb   EFTPOS PURCHASE",
  "         RIVERSIDE DAIRY                             12.40                    987.60",
  "05 Feb   SALARY",
  "         MATAI HOLDINGS                                         3,120.00    4,107.60",
  "05 Feb   DD CITY",
  "         COUNCIL RATES                              268.15                  3,839.45",
  "         Closing balance                                                  3,839.45")

test_that("a dated line the rows leave out is never lost: its date would go to another row", {
  expect_equal(auto_read(ar_pdf(c(ar_head, ar_stag)))$outcome, "proven")
  # A figure set tight in the date line stops it joining its figure line below.
  s <- ar_stag; s[2] <- "03 Feb   EFTPOS PURCHASE USD 25.00"
  rd <- auto_read(ar_pdf(c(ar_head, s)))
  expect_false(ar_auto(rd))
  expect_false(ok_of(rd, "dated_lines_used"))
})

test_that("a row with no date on a statement that prints every row's date is not given the one above", {
  rows <- ar_rows; rows[4] <- "05 Feb   DD CITY COUNCIL RATES                      268.15                  3,839.45"
  expect_equal(auto_read(ar_pdf(c(ar_head, rows)))$outcome, "proven")
  rows[5] <- "         TRANSFER TO SAVINGS                        400.00                  3,439.45"
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_false(ar_auto(rd))
  expect_false(ok_of(rd, "dates_carried"))
})

test_that("a page missing from the file is caught by the page numbers when the rows still add up", {
  p1 <- c(paste(ar_head[1], "  Page 1 of 2"), ar_head[2:3], ar_rows[1:4])
  p2 <- c("Kauri Bank                                         Page 2 of 2", ar_head[3], ar_rows[5:8])
  expect_equal(auto_read(ar_pdf(p1, p2))$outcome, "proven")
  last <- auto_read(ar_pdf(p1))
  expect_false(ar_auto(last))
  expect_false(ok_of(last, "pages_complete"))
  expect_false(ar_auto(auto_read(ar_pdf(p2))))
  # No page numbers, but the table ends by carrying its balance to a next page.
  cf <- c(ar_head, ar_rows[1:4], "         Balance carried forward                                          3,839.45")
  rd <- auto_read(ar_pdf(cf))
  expect_false(ar_auto(rd))
  expect_false(ok_of(rd, "pages_complete"))
  expect_true(is.na(.ar_page_labels_ok(c("x", "y"), 2L)$ok))
  # A bundle numbers each statement from 1.
  expect_true(.ar_page_labels_ok(c("Page 1 of 2", "Page 2 of 2", "Page 1 of 1"), 3L)$ok)
  expect_false(.ar_page_labels_ok(c("Page 1 of 3", "Page 3 of 3"), 2L)$ok)
})

test_that("card wording inside the rows never makes an everyday account a card", {
  h <- c("Kauri Bank   Everyday account   Statement period 1 Feb 2026 to 28 Feb 2026", "",
         "Date     Details                                  Amount       Balance")
  rows <- c("         Opening balance                                       1,000.00",
            "03 Feb   CREDIT CARD PAYMENT CARD NUMBER 4111      -12.40         987.60",
            "05 Feb   MATAI HOLDINGS                         3,120.00       4,107.60",
            "09 Feb   CITY COUNCIL MINIMUM PAYMENT             -268.15       3,839.45")
  rd <- auto_read(ar_pdf(c(h, rows)))
  expect_right_or_flagged(rd, c(-12.40, 3120, -268.15))
  expect_false(isTRUE(rd$template$auto$liab))
})

test_that("the account type alone never decides which way round: something on the page must agree", {
  # Merchant names only, an "Amount" heading, nothing saying card or not: read as an
  # everyday account or as a card paid down, both add up.
  h <- c("Kauri Bank   Statement period 1 Feb 2026 to 28 Feb 2026", "",
         "Date     Details                                  Amount       Balance")
  rows <- c("         Opening balance                                       1,000.00",
            "03 Feb   CORNER FOODS                             -50.00         950.00",
            "09 Feb   MAIN ST CAFE                             -20.00         930.00")
  expect_false(ar_auto(auto_read(ar_pdf(c(h, rows)))))
  # One row that says which way it went settles it.
  rows <- c(rows, "12 Feb   SALARY MATAI HOLDINGS                   300.00       1,230.00")
  rd <- auto_read(ar_pdf(c(h, rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-50, -20, 300))
})

test_that("a money heading that names the columns the other way stops the account type deciding", {
  # Nothing but the account type says which way round this reads; the headings
  # say the opposite, so a person decides.
  card <- c("Kiwibank   Credit card statement   Credit limit 10,000.00   Minimum payment 20.00", "",
            "Date     Details                          Deposits     Withdrawals      Balance",
            "         Opening balance                                                  1,000.00",
            "03 May   CORNER FOODS                                50.00                  1,050.00",
            "09 May   MAIN ST CAFE                                20.00                  1,070.00")
  expect_false(ar_auto(auto_read(ar_pdf(card))))
})

test_that("a statement that does not prove itself is never converted on a provisional layout's word", {
  # The card's own statement says it is a card; the second copy does not, so only
  # the layout learned from the first says which way round it reads.
  card <- c("Kiwibank                           Credit card statement",
            "Card number 4086 **** 7146        Statement period 1 May 2025 to 31 May 2025",
            "Credit limit 10,000.00            Minimum payment 20.00",
            "Previous balance                 1,000.00",
            "New balance                        900.00", "",
            "Date         Details                                   Amount",
            "03/05/2025   CORNER FOODS                               50.00",
            "12/05/2025   PAYMENT RECEIVED THANK YOU               -200.00",
            "20/05/2025   MAIN ST CAFE                               50.00")
  lay <- auto_read(ar_pdf(card))
  expect_equal(lay$outcome, "proven")
  plain <- card[-c(1:3)]
  prov <- lay$template
  prov$layout <- list(id = "x", version = 1L, status = "provisional", signature = prov$signature)
  expect_false(ar_auto(auto_read(ar_pdf(plain), layouts = list(prov))))
  # Once proven, the layout settles it.
  rd <- auto_read(ar_pdf(plain), layouts = list(lay$template))
  expect_right_or_flagged(rd, c(-50, 200, -50))
})

# ---- the repair search ------------------------------------------------------------------------

test_that("the repair search is bounded, named, and only runs when nothing passed", {
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  expect_equal(rd$candidates$source, "content")
  rows <- ar_rows; rows[5] <- "14 Feb   TRANSFER TO SAVINGS                        410.00                  3,439.45"
  rd2 <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_true(all(grepl("^(content|repair:)", rd2$candidates$source)))
  expect_lte(nrow(rd2$candidates), 4L)
})

test_that("a figure set close to its description is recovered by a narrower cell split, and only then", {
  # The money-in figure sits one character from the description: not a cell of
  # its own at the page's usual split, a cell at the narrower one.
  rows <- c("         Opening balance                                          1,000.00",
            "03 Feb   EFTPOS RIVERSIDE DAIRY            12.40                    987.60",
            "05 Feb   SALARY MATAI HOLDINGS                    3,120.00        4,107.60",
            "09 Feb   DD CITY COUNCIL RATES            268.15                  3,839.45",
            "14 Feb   DEPOSIT MOBILE CHEQUE 12 3456 9 10.00                    3,849.45",
            "26 Feb   CREDIT INTEREST                               2.18        3,851.63")
  rd <- auto_read(ar_pdf(c(ar_head, rows)))
  expect_right_or_flagged(rd, c(-12.40, 3120, -268.15, 10, 2.18))
})

# ---- layouts --------------------------------------------------------------------------------

test_that("a proven layout is re-checked as a candidate and named when it matches", {
  first <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  rows <- ar_rows; rows[2] <- "03 Feb   EFTPOS CORNER FOODS                         22.40                    977.60"
  rows[3:7] <- c("05 Feb   SALARY MATAI HOLDINGS                                  3,120.00    4,097.60",
                 "09 Feb   DD CITY COUNCIL RATES                      268.15                  3,829.45",
                 "14 Feb   TRANSFER TO SAVINGS                        400.00                  3,429.45",
                 "21 Feb   VISA HARBOUR FUEL                           96.72                  3,332.73",
                 "26 Feb   CREDIT INTEREST                                            2.18    3,334.91")
  rows[8] <- "         Closing balance                                                  3,334.91"
  rd <- auto_read(ar_pdf(c(ar_head, rows)), layouts = list(first$template))
  expect_equal(rd$outcome, "proven")
  expect_false(is.null(rd$matched_layout))
})

# N225: a reading proven on its own content, of a design the store already holds,
# came back with matched_layout NULL whenever it differed from the stored layout in
# a detail the reader's strict test holds to (here: listed newest first). The run
# log then named no layout, and Admin's count of layouts in use fell short.
test_that("a proven reading of a stored layout's design names that layout", {
  d <- tempfile("ly_"); dir.create(d)
  first <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  expect_identical(layout_learn(first, "kauri", paste(rep("ab", 32), collapse = ""), d)$action, "created")
  stored <- layouts_load(d, "kauri")
  same <- auto_read(ar_pdf(c(ar_head, ar_rows)), layouts = stored)
  expect_identical(same$matched_layout, "kauri_1@1")
  newest <- auto_read(ar_pdf(c(ar_head, ar_rows[8], rev(ar_rows[2:7]), ar_rows[1])), layouts = stored)
  expect_equal(newest$outcome, "proven")
  expect_identical(newest$matched_layout, layout_match(newest$template$signature, stored)$ref)
  expect_identical(newest$matched_layout, "kauri_1@1")
  # Nothing stored, nothing named.
  expect_null(auto_read(ar_pdf(c(ar_head, ar_rows)))$matched_layout)
})

test_that("with no balance and no totals, a matching proven layout gives layout_match", {
  h <- c(ar_head[1:2], "Date     Details                          Withdrawals     Deposits")
  rows <- c("03 Feb   EFTPOS RIVERSIDE DAIRY                      12.40",
            "05 Feb   SALARY MATAI HOLDINGS                                  3,120.00",
            "09 Feb   DD CITY COUNCIL RATES                      268.15")
  lay <- auto_read(ar_pdf(c(h, "         Opening balance          1,000.00", rows,
                            "         Closing balance          3,839.45")))
  expect_equal(lay$outcome, "proven")
  rd <- auto_read(ar_pdf(c(h, rows)), layouts = list(lay$template))
  expect_equal(rd$outcome, "layout_match")
  expect_equal(round(rd$transactions$amount, 2), c(-12.40, 3120, -268.15))
  # A provisional layout carries nothing.
  prov <- lay$template; prov$layout <- list(id = "x", version = 1L, status = "provisional", signature = prov$signature)
  expect_equal(auto_read(ar_pdf(c(h, rows)), layouts = list(prov))$outcome, "check")
})

# N218, reverted: on a statement whose days are all 12 or less, 03/02 is 3 February
# and 2 March alike, and with no day over 12 nothing in the dates tells them apart.
# A proven layout once settled it with its own stored order; a month/day file read
# against a day/month layout then came out with every date wrong, on the proven
# path too (the running balance holds whichever way the dates are read). Now only
# the statement settles it -- here, a printed period only one order fits -- and
# otherwise a person reads the dates, whatever layouts are handed in.
test_that("a learned layout never settles day-month against month-day; the statement's own period may", {
  for (us in c(FALSE, TRUE)) {
    fm <- function(d) if (us) sprintf("02/%02d/2026", d) else sprintf("%02d/02/2026", d)
    want <- c("2026-02-03", "2026-02-05", "2026-02-09")
    h <- c(ar_head[1], "", "Date         Details                          Withdrawals     Deposits")
    h_noper <- c("Kauri Bank", "", h[3])
    row <- function(d, s) paste0(fm(d), s)
    body <- function(d) c(row(d[1], "   EFTPOS RIVERSIDE DAIRY                      12.40"),
                          row(d[2], "   SALARY MATAI HOLDINGS                                  3,120.00"),
                          row(d[3], "   DD CITY COUNCIL RATES                      268.15"))
    lay <- auto_read(ar_pdf(c(h, "             Opening balance          1,000.00", body(c(3, 15, 19)),
                              "             Closing balance          3,839.45")))
    expect_equal(lay$outcome, "proven", info = us)
    # No period: nothing on the statement says which order, layout or not.
    small <- ar_pdf(c(h_noper, body(c(3, 5, 9))))
    for (L in list(list(), list(lay$template))) {
      rd <- auto_read(small, layouts = L)
      expect_equal(rd$outcome, "check", info = us)
      expect_false(isTRUE(ok_of(rd, "dates_settled")), info = us)
    }
    # The same rows with a running balance: the arithmetic proves every figure, and
    # still not the dates.
    bal <- ar_pdf(c(c(h_noper[1:2], paste0(h[3], "      Balance")),
                    paste0(fm(3), "   EFTPOS RIVERSIDE DAIRY                      12.40                    987.60"),
                    paste0(fm(5), "   SALARY MATAI HOLDINGS                                  3,120.00    4,107.60"),
                    paste0(fm(9), "   DD CITY COUNCIL RATES                      268.15                  3,839.45")))
    expect_false(ar_auto(auto_read(bal, layouts = list(lay$template))), info = us)
    # A printed period (February) only one order fits: the statement settles it,
    # and the dates are read that way.
    rd <- auto_read(ar_pdf(c(h, body(c(3, 5, 9)))), layouts = list(lay$template))
    expect_true(isTRUE(ok_of(rd, "dates_settled")), info = us)
    expect_equal(rd$transactions$date, want, info = us)
    expect_right_or_flagged(rd, c(-12.40, 3120, -268.15))
  }
  # The same in a CSV export with no period: a proven layout settles nothing.
  csv <- function(d, open = NULL, close = NULL) list(kind = "delimited", path = "", sha256 = NA_character_,
    meta = list(ext = "csv"), lines = c("Date,Details,Amount", open,
      sprintf("02/%02d/2026,%s,%s", d, c("EFTPOS RIVERSIDE", "SALARY MATAI", "DD COUNCIL"), c("-12.40", "3120.00", "-268.15")),
      close))
  lay <- auto_read(csv(c(3, 15, 19), ",Opening balance,1000.00", ",Closing balance,3839.45"))
  expect_equal(lay$outcome, "proven")
  rd <- auto_read(csv(c(3, 5, 9)), layouts = list(lay$template))
  expect_equal(rd$outcome, "check")
  expect_false(isTRUE(ok_of(rd, "dates_settled")))
})

test_that("a layout that would read the figures differently stops a reading being unique", {
  # A layout that says the first figure column is money IN, where the arithmetic
  # cannot tell (no balance, no totals): both readings are shown as a check.
  h <- c(ar_head[1:2], "Date     Details                          Withdrawals     Deposits")
  rows <- c("03 Feb   EFTPOS RIVERSIDE DAIRY                      12.40",
            "05 Feb   SALARY MATAI HOLDINGS                                  3,120.00")
  base <- auto_read(ar_pdf(c(ar_head, ar_rows)))$template
  swapped <- base; swapped$auto$roles <- c("credit", "debit")
  swapped$signature$roles <- c("date", "description", "credit", "debit")
  rd <- auto_read(ar_pdf(c(h, rows)), layouts = list(swapped))
  expect_false(ar_auto(rd) && !identical(round(rd$transactions$amount, 2), c(-12.40, 3120)))
})

# ---- CSV and Excel ----------------------------------------------------------------------------

ar_csv <- function(lines) list(kind = "delimited", path = "", sha256 = NA_character_, lines = lines,
                               meta = list(ext = "csv"))

test_that("a CSV is mapped by content and proven by its own balance column", {
  rd <- auto_read(ar_csv(c("Txn Dt,Narrative,Amt,Bal",
    "13/02/2026,EFTPOS RIVERSIDE DAIRY,-12.40,987.60",
    "15/02/2026,SALARY MATAI HOLDINGS,3120.00,4107.60",
    "19/02/2026,DD CITY COUNCIL RATES,-268.15,3839.45")))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-12.40, 3120, -268.15))
  expect_equal(rd$template$signature$kind, "delimited")
  expect_equal(rd$template$signature$roles, c("date", "description", "amount", "balance"))
})

test_that("CSV headings are only a vote: swapped headings do not swap the figures", {
  # A balance printed with CR leaves the arithmetic one way to read the columns,
  # whatever the headings say.
  rd <- auto_read(ar_csv(c("Date,Details,Deposits,Withdrawals,Balance",
    "13/02/2026,EFTPOS RIVERSIDE DAIRY,12.40,,987.60 CR",
    "15/02/2026,SALARY MATAI HOLDINGS,,3120.00,4107.60 CR",
    "19/02/2026,DD CITY COUNCIL RATES,268.15,,3839.45 CR")))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-12.40, 3120, -268.15))
  # Plain balances: the columns and their exact negation both add up. The rows'
  # own wording outvotes the swapped headings, or a person decides.
  rd2 <- auto_read(ar_csv(c("Date,Details,Deposits,Withdrawals,Balance",
    "13/02/2026,EFTPOS RIVERSIDE DAIRY,12.40,,987.60",
    "15/02/2026,SALARY MATAI HOLDINGS,,3120.00,4107.60",
    "19/02/2026,DD CITY COUNCIL RATES,268.15,,3839.45")))
  expect_right_or_flagged(rd2, c(-12.40, 3120, -268.15))
  # With nothing in the rows to say which way, the swapped headings stop it.
  rd3 <- auto_read(ar_csv(c("Date,Details,Deposits,Withdrawals,Balance",
    "13/02/2026,RIVERSIDE DAIRY,12.40,,987.60",
    "15/02/2026,MATAI HOLDINGS,,3120.00,4107.60",
    "19/02/2026,CITY COUNCIL RATES,268.15,,3839.45")))
  expect_false(ar_auto(rd3))
})

test_that("a CSV with a D/C indicator column, an opening row and newest first", {
  rd <- auto_read(ar_csv(c("Account,03-4211-4282016-009", "",
    "Txn Date,Narrative,Amount,DR/CR,Running Balance",
    "09-Feb-2026,DD CITY COUNCIL RATES,268.15,DR,3839.45",
    "05-Feb-2026,SALARY MATAI HOLDINGS,3120.00,CR,4107.60",
    "03-Feb-2026,EFTPOS RIVERSIDE DAIRY,12.40,DR,987.60",
    "01-Feb-2026,OPENING BALANCE,,,1000.00")))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-268.15, 3120, -12.40))
  expect_true(rd$template$signature$newest_first)
  expect_equal(rd$template$amount_sign, "type_dc")
})

test_that("a CSV row whose date did not read is never left out quietly", {
  rows <- c("Date,Details,Amount,Balance",
    "13/02/2026,EFTPOS RIVERSIDE DAIRY,-12.40,987.60",
    "15/02/2026,SALARY MATAI HOLDINGS,3120.00,4107.60",
    "19/02/2026,DD CITY COUNCIL RATES,-268.15,3839.45")
  expect_equal(auto_read(ar_csv(rows))$outcome, "proven")
  rd <- auto_read(ar_csv(c(rows, ",ATM WITHDRAWAL,-40.00,3799.45")))
  expect_false(ar_auto(rd))
  expect_false(ok_of(rd, "lines_accounted"))
})

test_that("a CSV with nothing to add up goes to a person", {
  rd <- auto_read(ar_csv(c("Date,Details,Amount",
    "03/02/2026,EFTPOS RIVERSIDE DAIRY,-12.40", "05/02/2026,SALARY MATAI HOLDINGS,3120.00")))
  expect_equal(rd$outcome, "check")
})

test_that("an Excel sheet with serial dates and no heading on its date column is read by content", {
  tb <- data.frame(a = c("Processed", "46056", "46058"), b = c("Narrative", "EFTPOS RIVERSIDE", "SALARY"),
                   c = c("Money Out", "12.4", NA), d = c("Money In", NA, "3120"),
                   e = c("Running Balance", "987.6", "4107.600000000001"), stringsAsFactors = FALSE)
  names(tb) <- c("Rimu Bank transactions", "...2", "...3", "...4", "...5")
  rd <- auto_read(list(kind = "excel", path = "", sha256 = NA_character_, table = tb,
                       meta = list(ext = "xlsx", preamble = character(0))))
  expect_equal(round(rd$transactions$amount, 2), c(-12.40, 3120))
  expect_equal(rd$transactions$date, c("2026-02-03", "2026-02-05"))
  expect_equal(rd$template$signature$kind, "excel")
})

# ---- never throws, deterministic ---------------------------------------------------------------

test_that("the reader never throws and says why it could not read", {
  for (inp in list(list(kind = "pdf", words = list()), list(kind = "zip"), list(kind = "delimited", lines = character(0)),
                   ar_pdf(c("Hello", "no table here")), NULL)) {
    rd <- expect_no_error(auto_read(inp))
    expect_equal(rd$outcome, "unread")
    expect_true(nzchar(rd$why))
  }
})

test_that("the same input gives the same reading", {
  a <- auto_read(ar_pdf(c(ar_head, ar_rows))); b <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  a$secs <- b$secs <- NULL
  expect_identical(a, b)
})

# ---- the table reader's per-page columns --------------------------------------------------------

test_that("parse_pdf_table reads each page with its own columns, and is unchanged without them", {
  rd <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  tpl <- rd$template
  inp <- ar_pdf(c(ar_head, ar_rows))
  a <- parse_pdf_table(inp, tpl)
  tpl2 <- tpl; tpl2$table$columns_by_page <- NULL
  b <- parse_pdf_table(inp, tpl2)
  expect_equal(a$transactions$amount, b$transactions$amount)
  # A page-2 copy shifted 40pt right reads only through its own columns.
  sh <- inp; sh$words[[1]]$x <- sh$words[[1]]$x + 40
  tpl3 <- tpl; tpl3$table$columns_by_page <- lapply(tpl$table$columns_by_page, function(cl)
    lapply(cl, function(b) list(x_min = b$x_min + 40, x_max = b$x_max + 40)))
  expect_equal(parse_pdf_table(sh, tpl3)$transactions$amount, a$transactions$amount)
})

# ---- metamorphic tests on a shipped fixture ------------------------------------------------------

fx_path <- function() fixture("tests/testthat/fixtures/anz_everyday_pdf_sample.pdf")
fx_input <- function() { skip_if_not(file.exists(fx_path())); read_input(fx_path()) }

test_that("the shipped sample is proven as printed", {
  inp <- fx_input()
  rd <- auto_read(inp)
  expect_equal(rd$outcome, "proven")
  expect_equal(nrow(rd$transactions), 6L)
})

fx_want <- function() round(auto_read(fx_input())$transactions$amount, 2)

test_that("metamorphic: shifting a column gives the same reading or a flag", {
  inp <- fx_input(); want <- fx_want()
  w <- inp$words[[1]]
  bal <- grepl("^[0-9,]+\\.[0-9]{2}$", w$text) & w$x > 0.8 * max(w$x)
  w$x[bal] <- w$x[bal] + 25
  inp$words[[1]] <- w
  expect_right_or_flagged(auto_read(inp), want)
})

test_that("metamorphic: a stray USD 25.00 in a description gives the same reading or a flag", {
  inp <- fx_input(); want <- fx_want()
  w <- inp$words[[1]]
  d <- which(grepl("^[0-9]{2}$", w$text))[1]
  ln <- w[abs(w$y - w$y[d]) < 2, ]
  desc <- ln[order(ln$x), ][3, ]
  add <- data.frame(width = c(15, 22), height = desc$height, x = desc$x + desc$width + c(25, 43),
                    y = desc$y, space = TRUE, text = c("USD", "25.00"), stringsAsFactors = FALSE)
  for (nm in setdiff(names(w), names(add))) add[[nm]] <- NA
  inp$words[[1]] <- rbind(w, add[, names(w)])
  expect_right_or_flagged(auto_read(inp), want)
})

test_that("metamorphic: deleting a row is never read as complete", {
  inp <- fx_input()
  w <- inp$words[[1]]
  d <- which(grepl("^[0-9]{2}$", w$text) & w$x < 0.2 * max(w$x))
  y <- w$y[d[3]]
  inp$words[[1]] <- w[abs(w$y - y) >= 2, ]
  rd <- auto_read(inp)
  expect_false(ar_auto(rd))
})

test_that("metamorphic: swapping two headings changes nothing", {
  inp <- fx_input(); want <- fx_want()
  w <- inp$words[[1]]
  i <- which(tolower(w$text) %in% c("withdrawals", "debits", "debit"))[1]
  j <- which(tolower(w$text) %in% c("deposits", "credits", "credit"))[1]
  skip_if(is.na(i) || is.na(j))
  tmp <- w$text[i]; w$text[i] <- w$text[j]; w$text[j] <- tmp
  inp$words[[1]] <- w
  rd <- auto_read(inp)
  expect_right_or_flagged(rd, want)
})

test_that("metamorphic: dropping a page of a bundle is caught or still right", {
  p <- fixture("tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf")
  skip_if_not(file.exists(p))
  inp <- read_input(p)
  full <- auto_read(inp)
  expect_equal(full$outcome, "proven")
  drop2 <- inp
  for (k in c("words", "pages", "page_width", "page_height", "page_ocr")) if (!is.null(drop2[[k]])) drop2[[k]] <- drop2[[k]][-2]
  rd <- auto_read(drop2)
  expect_false(ar_auto(rd))
})

# ---- metamorphic tests on every row of a synthetic statement --------------------------------------
# Each mutation of the printed page must give the right reading OF THE MUTATED PAGE
# or a person, never an automatic wrong answer.

test_that("metamorphic: a duplicated row, at any position, is never read as proven", {
  for (i in 2:7) {
    rows <- append(ar_rows, ar_rows[i], after = i)
    expect_false(ar_auto(auto_read(ar_pdf(c(ar_head, rows)))), info = paste("row", i))
  }
})

test_that("metamorphic: deleting any row is caught", {
  for (i in 2:7) expect_false(ar_auto(auto_read(ar_pdf(c(ar_head, ar_rows[-i])))), info = paste("row", i))
})

test_that("metamorphic: one digit changed in any amount gives that figure or a flag", {
  amt_rx <- "[0-9,]+\\.[0-9]{2}"
  for (i in 2:7) {
    rows <- ar_rows
    m <- regmatches(rows[i], gregexpr(amt_rx, rows[i]))[[1]][1]
    changed <- sub("([0-9])([.][0-9]{2})$", "9\\2", m)
    if (identical(changed, m)) changed <- sub("([0-9])([.][0-9]{2})$", "8\\2", m)
    rows[i] <- sub(m, changed, rows[i], fixed = TRUE)
    want <- ar_want; want[i - 1L] <- sign(want[i - 1L]) * .num(changed)
    expect_right_or_flagged(auto_read(ar_pdf(c(ar_head, rows))), want)
  }
})

test_that("metamorphic: two rows' figures swapped are read as printed or flagged", {
  # Opening and closing only (no running balance): the sum is unchanged, so the
  # arithmetic cannot see the swap; the reading must then be the swapped print.
  h <- c(ar_head[1:2], "Date     Details                          Withdrawals     Deposits")
  rows <- c("         Opening balance                          1,000.00",
            "03 Feb   EFTPOS RIVERSIDE DAIRY                      12.40",
            "09 Feb   DD CITY COUNCIL RATES                      268.15",
            "14 Feb   TRANSFER TO SAVINGS                        400.00",
            "         Closing balance                            319.45")
  sw <- rows; sw[2] <- sub("12.40", "268.15", rows[2], fixed = TRUE); sw[3] <- sub("268.15", " 12.40", rows[3], fixed = TRUE)
  expect_right_or_flagged(auto_read(ar_pdf(c(h, sw))), c(-268.15, -12.40, -400))
  # With a running balance the swap contradicts it, and calling the balance column
  # something else to make opening + movements = closing hold must not prove it.
  rows <- ar_rows
  rows[2] <- sub("12.40", "96.72", ar_rows[2], fixed = TRUE)
  rows[6] <- sub("96.72", "12.40", ar_rows[6], fixed = TRUE)
  expect_false(ar_auto(auto_read(ar_pdf(c(ar_head, rows)))))
})

test_that("metamorphic: dropping any page of a numbered statement is caught", {
  pg <- function(k, body) c(sprintf("Kauri Bank   Statement period 1 Feb 2026 to 28 Feb 2026   Page %d of 3", k), "", ar_head[3], body)
  p <- list(pg(1, ar_rows[1:3]), pg(2, ar_rows[4:5]), pg(3, ar_rows[6:8]))
  expect_equal(auto_read(do.call(ar_pdf, p))$outcome, "proven")
  for (k in 1:3) expect_false(ar_auto(auto_read(do.call(ar_pdf, p[-k]))), info = paste("page", k))
})

test_that("metamorphic: one page's table set further right reads the same", {
  p1 <- c(ar_head, ar_rows[1:4])
  p2 <- paste0("        ", c(ar_head[3], ar_rows[5:8]))
  expect_right_or_flagged(auto_read(ar_pdf(p1, p2)), ar_want)
})

# ---- a person's roles (opts$roles, from Please check) ---------------------------------------

test_that("a person's roles are read on their own and still have to add up", {
  base <- auto_read(ar_pdf(c(ar_head, ar_rows)))
  expect_equal(base$outcome, "proven")
  expect_identical(base$template$auto$roles, c("debit", "credit", "balance"))
  # the same roles, given by a person: read on their own, proven by the arithmetic
  same <- auto_read(ar_pdf(c(ar_head, ar_rows)), opts = list(roles = c("debit", "credit", "balance")))
  expect_equal(same$outcome, "proven")
  expect_identical(same$candidates$source, "content")       # no layout or repair stands in
  expect_equal(round(same$transactions$amount, 2), ar_want)
  # money out and in swapped: read as an ordinary account (the statement's own
  # type), the balance does not add up -- never "proven" with every sign inverted
  sw <- auto_read(ar_pdf(c(ar_head, ar_rows)), opts = list(roles = c("credit", "debit", "balance")))
  expect_false(ar_auto(sw))
  # roles for the wrong number of columns are refused in words
  bad <- auto_read(ar_pdf(c(ar_head, ar_rows)), opts = list(roles = c("amount", "balance")))
  expect_false(ar_auto(bad))
  expect_match(bad$why, "roles given are for 2", fixed = TRUE)
})

test_that("with nothing to add up, the reading shown is the person's roles", {
  csv <- function(lines) { p <- tempfile(fileext = ".csv"); writeLines(lines, p); read_input(p) }
  # (A "Batch" column of 1001, 1002, ... is an identifier, not a figure: the second
  # figure column here is a fee.)
  inp <- csv(c("Date,Details,Amount,Fee", "14/04/2025,Salary,2500.00,0.00", "15/04/2025,Rent,-1200.00,0.50",
               "16/04/2025,Bread,-3.50,0.00"))
  rd <- auto_read(inp, opts = list(roles = c("amount", "other")))
  expect_equal(rd$outcome, "check")
  expect_equal(rd$transactions$amount, c(2500, -1200, -3.5))
  ids <- csv(c("Date,Details,Amount,Batch", "14/04/2025,Salary,2500.00,1001", "15/04/2025,Rent,-1200.00,1002"))
  expect_identical(auto_read(ids)$template$auto$roles, "amount")
})

test_that("a code or reference printed with leading zeros is never a figure", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("Date,Details,Code,Amount,Balance",
               "13/04/2025,Opening balance,,,1000.00",
               "14/04/2025,Salary,007,2500.00,3500.00",
               "15/04/2025,Rent,0012345,-1200.00,2300.00",
               "17/04/2025,Bread,000001,-3.50,2296.50"), p)
  rd <- auto_read(read_input(p))
  expect_equal(rd$outcome, "proven")
  expect_identical(rd$template$auto$roles, c("amount", "balance"))
  expect_identical(rd$transactions$code, c("007", "0012345", "000001"))
})

test_that("an account-number column is never the description", {
  p <- tempfile(fileext = ".csv")
  writeLines(c("Date,Amount,Payee,This Party Account",
               "14/04/2025,2500.00,Salary,11-1111-1111111-00",
               "15/04/2025,-1200.00,Rent,11-1111-1111111-00"), p)
  rd <- auto_read(read_input(p))
  expect_identical(rd$transactions$description, c("Salary", "Rent"))
})

# ---- which column is which in an export (N211, N213, N214, N215) ----------------------------

# Every column of a CSV, put in another order; the same reading must come back.
ar_reorder <- function(lines, perm) vapply(lines, function(l) {
  f <- strsplit(l, ",", fixed = TRUE)[[1]]
  f <- c(f, rep("", max(0L, length(perm) - length(f))))
  paste(f[perm], collapse = ",")
}, "", USE.NAMES = FALSE)

test_that("of two date columns, the one headed as the transaction date is taken, wherever it sits", {
  # Kiwibank prints Effective Date and Transaction Date; they differ on row 1. The
  # date column was the first one found, so reversing the columns changed the dates.
  kb <- c("Account number,Effective Date,Transaction Date,Description,Amount,Balance",
          "38-8106-0601663-00,2025-08-21,2025-08-22,EFTPOS SUSHI,-9.00,895.69",
          "38-8106-0601663-00,2025-08-22,2025-08-22,PAY Alice The Bar,-15.00,880.69",
          "38-8106-0601663-00,2025-08-23,2025-08-23,EFTPOS PAK N SAVE,-42.02,838.67")
  for (perm in list(1:6, 6:1, c(3, 1, 5, 2, 4, 6))) {
    rd <- auto_read(ar_csv(ar_reorder(kb, perm)))
    expect_equal(rd$outcome, "proven", info = paste(perm, collapse = ""))
    expect_equal(rd$transactions$date, c("2025-08-22", "2025-08-22", "2025-08-23"), info = paste(perm, collapse = ""))
    expect_equal(rd$parsed$extras$date2, c("2025-08-21", "2025-08-22", "2025-08-23"), info = paste(perm, collapse = ""))
  }
  # A card's TransactionDate beats its ProcessedDate; a bare "Date" beats "Processed Date".
  expect_equal(.ar_date_heading_rank("TransactionDate"), 2L)
  expect_equal(.ar_date_heading_rank("Date of Transaction"), 2L)
  expect_equal(.ar_date_heading_rank("Date"), 1L)
  expect_equal(vapply(c("ProcessedDate", "Effective Date", "Value Date", "Date Processed", ""),
                      .ar_date_heading_rank, 0L, USE.NAMES = FALSE), rep(0L, 5))
})

test_that("two date columns that differ, with nothing saying which is the transaction's, go to a person", {
  two <- c("Date,Details,Date,Amount,Balance",
           "21/08/2025,EFTPOS SUSHI,22/08/2025,-9.00,895.69",
           "22/08/2025,PAY Alice,22/08/2025,-15.00,880.69",
           "23/08/2025,EFTPOS PAK N SAVE,24/08/2025,-42.02,838.67")
  shown <- NULL
  for (perm in list(1:5, 5:1, c(3, 2, 1, 4, 5))) {
    rd <- auto_read(ar_csv(ar_reorder(two, perm)))
    expect_equal(rd$outcome, "check", info = paste(perm, collapse = ""))
    expect_false(ok_of(rd, "dates_settled"))
    expect_match(rd$why, "two date columns, \"Date\" and \"Date\", that give different dates on 2 row\\(s\\)")
    # What is shown does not depend on where the columns sit.
    shown <- shown %||% rd$transactions$date
    expect_equal(rd$transactions$date, shown, info = paste(perm, collapse = ""))
  }
  expect_equal(shown, c("2025-08-21", "2025-08-22", "2025-08-23"))   # the earlier: made before processed
  # Two date columns that agree on every row settle nothing and need nothing.
  same <- two; same[c(2, 4)] <- c("21/08/2025,EFTPOS SUSHI,21/08/2025,-9.00,895.69", "23/08/2025,EFTPOS PAK N SAVE,23/08/2025,-42.02,838.67")
  expect_equal(auto_read(ar_csv(ar_reorder(same, 5:1)))$outcome, "proven")
})

test_that("an id column of long whole numbers is never money", {
  # ASB prints a Unique Id on every row; read as money, it became the balance.
  asb <- c("Date,Unique Id,Tran Type,Payee,Memo,Amount,Balance",
           "2026/02/03,2026020301,POS,RIVERSIDE DAIRY,EFTPOS,-12.40,987.60",
           "2026/02/05,2026020501,DIRECTDEP,MATAI HOLDINGS,Salary,3120.00,4107.60",
           "2026/02/09,2026020901,DEBIT,CITY COUNCIL,Rates,-268.15,3839.45")
  rd <- auto_read(ar_csv(asb))
  expect_equal(rd$outcome, "proven")
  expect_identical(rd$template$auto$roles, c("amount", "balance"))
  expect_equal(round(rd$transactions$amount, 2), c(-12.40, 3120, -268.15))
  nb <- auto_read(ar_csv(sub(",[0-9.]+$", "", sub("^(.*),Balance$", "\\1", asb))))
  expect_identical(nb$template$auto$roles, "amount")
  expect_false("balance" %in% nb$template$auto$fields)
  expect_true(nb$outcome %in% c("check", "unread"))
  # The rule, on its own: long, headed as an id, or a running sequence -- and never
  # a plain column of whole amounts.
  expect_true(.ar_tab_id_column(c("2014122001", "2014122101")))
  expect_true(.ar_tab_id_column(c("12", "40"), "Cheque Number"))
  expect_true(.ar_tab_id_column(c("100234", "100235", "100236")))
  expect_false(.ar_tab_id_column(c("54", "100", "20"), "Amount"))
  expect_false(.ar_tab_id_column(c("1500", "2000", "2500"), "Balance"))
  expect_false(.ar_tab_id_column(c("12.40", "2014122001")))
})

test_that("in a card export with foreign-currency columns the NZD amount is the amount", {
  # Original Amount (in its own currency) and a conversion charge sit beside the
  # NZD amount. Nothing adds up, so a person looks -- at the NZD amounts, with no
  # foreign figure taken for a balance.
  fx <- c("Date,Description,Original Amount,Currency,NZD Amount,Conversion Charge",
          "03/02/2026,RIVERSIDE DAIRY,-12.40,NZD,-12.40,",
          "05/02/2026,AMAZON US,-30.00,USD,-51.00,-1.31",
          "09/02/2026,PAYMENT RECEIVED THANK YOU,200.00,NZD,200.00,",
          "14/02/2026,HARBOUR FUEL,-96.72,NZD,-96.72,")
  for (perm in list(1:6, 6:1)) {
    rd <- auto_read(ar_csv(ar_reorder(fx, perm)))
    expect_equal(rd$outcome, "check", info = paste(perm, collapse = ""))
    expect_equal(round(rd$transactions$amount, 2), c(-12.40, -51.00, 200, -96.72), info = paste(perm, collapse = ""))
    expect_false("balance" %in% rd$template$auto$roles, info = paste(perm, collapse = ""))
  }
  # ANZ's card export: the foreign amount as text, the charge as a figure.
  anz <- c("Card,Type,Amount,Details,TransactionDate,ProcessedDate,ForeignCurrencyAmount,ConversionCharge",
           "4835-****-****-0311,D,12.40,RIVERSIDE DAIRY,03/02/2026,04/02/2026,,",
           "4835-****-****-0311,D,52.31,AMAZON US,05/02/2026,06/02/2026,30.00 USD,1.31",
           "4835-****-****-0311,C,200.00,PAYMENT RECEIVED THANK YOU,09/02/2026,09/02/2026,,")
  rd <- auto_read(ar_csv(anz))
  expect_identical(rd$template$auto$roles, c("amount", "other"))
  expect_equal(round(rd$transactions$amount, 2), c(-12.40, -52.31, 200))
  expect_equal(rd$transactions$date, c("2026-02-03", "2026-02-05", "2026-02-09"))
  # With a running balance the arithmetic decides, and agrees.
  bal <- c("Date,Description,Original Amount,Currency,NZD Amount,Balance",
           ",Opening balance,,,,1000.00",
           "03/02/2026,RIVERSIDE DAIRY,-12.40,NZD,-12.40,987.60",
           "05/02/2026,AMAZON US,-30.00,USD,-52.31,935.29",
           "14/02/2026,PAYMENT RECEIVED THANK YOU,200.00,NZD,200.00,1135.29")
  rd <- auto_read(ar_csv(bal))
  expect_equal(rd$outcome, "proven")
  expect_identical(rd$template$auto$roles, c("other", "amount", "balance"))
  expect_identical(.wa_money_role("ForeignCurrencyAmount"), "other")
  expect_identical(.wa_money_role("NZD Amount"), "amount")
})

test_that("an id or a hash is never the description while a column of words exists", {
  # A Xero export's unique_id ("KIWIBANK-20250401-000") was longer than any
  # description, so it was taken for one.
  xero <- c("transaction_date,description,amount,debit_credit,balance,currency,unique_id,memo",
            "01/04/2025,Opening balance,0.00,credit,11980.55,NZD,KIWIBANK-20250401-000,Starting balance",
            "02/04/2025,Payroll deposit,4850.00,credit,16830.55,NZD,KIWIBANK-20250402-001,Payroll ACH",
            "03/04/2025,Office supplies,312.54,debit,16518.01,NZD,KIWIBANK-20250403-002,Staples invoice 88321")
  for (perm in list(1:8, 8:1)) {
    rd <- auto_read(ar_csv(ar_reorder(xero, perm)))
    expect_equal(rd$transactions$description, c("Opening balance", "Payroll deposit", "Office supplies"),
                 info = paste(perm, collapse = ""))
  }
  # No column headed as the description: the words win over a longer hash.
  hx <- c("Date,Ref,Narrative,Amount",
          "03/02/2026,9f3ac81d0b4e7a2265c1,EFTPOS DAIRY,-12.40",
          "05/02/2026,77ab03e9c1d24f8e90aa,SALARY,3120.00")
  expect_identical(auto_read(ar_csv(hx))$transactions$description, c("EFTPOS DAIRY", "SALARY"))
  hy <- sub("Narrative", "Words", hx)
  expect_identical(auto_read(ar_csv(hy))$transactions$description, c("EFTPOS DAIRY", "SALARY"))
})

test_that("a page cut out of a bundle is read again from its own page of the file", {
  inp <- ar_pdf(c(ar_head, ar_rows), c(ar_head, ar_rows), c(ar_head, ar_rows))
  expect_identical(.ar_file_page(inp, 2L), 2L)
  expect_identical(.ar_file_page(.subinput_pages(inp, 3L), 1L), 3L)
})

# ---- other tables in the pack ------------------------------------------------------------------
# Measured on a real ANZ home-loan pack: page 1 printed an "Upcoming automatic
# payments" table (a date and an amount on each line) before the statement, and the
# "every page with transactions gave rows" check held a fully proven statement back.

ar_cover <- c("Kauri Bank                         Statement of Accounts", "",
  "Upcoming automatic payments",
  "Account number      Payee            Frequency       Payment date      Payment amount",
  "01-0001-0000001-00  Sam Checking     WEEKLY             21 Feb 26              125.00",
  "                    Debit            WEEKLY             23 Feb 26              125.00",
  "                    Go               WEEKLY             26 Feb 26              300.00")

test_that("another table's dated figures on a cover page do not hold back a proven statement", {
  rd <- auto_read(ar_pdf(ar_cover, c(ar_head, ar_rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), ar_want)
  expect_true(isTRUE(ok_of(rd, "other_tables")))
})

test_that("another table is set aside only when the printed opening and closing balances add up", {
  # Without the opening and closing lines nothing confirms the lines elsewhere are
  # not this statement's missing rows, so a person looks.
  rd <- auto_read(ar_pdf(ar_cover, c(ar_head, ar_rows[2:7])))
  expect_false(ar_auto(rd))
  expect_false(isTRUE(ok_of(rd, "other_tables")))
})

test_that("a scanned page whose OCR timed out is never read as proven", {
  inp <- ar_pdf(c(ar_head, ar_rows))
  inp$meta$ocr_timed_out <- 2L
  rd <- auto_read(inp)
  expect_equal(rd$outcome, "check")
  expect_false(ok_of(rd, "ocr_complete"))
})

test_that("a totals line printing the balance stands as the closing balance", {
  # ANZ prints "Totals at end of period" with the withdrawals total and the balance,
  # and no line called "Closing balance". With the cover page's other table set
  # aside, that balance is what confirms no rows are missing.
  h <- c("Kauri Bank                         Statement period 19 Aug 2026 to 19 Aug 2026", "",
         "Date     Details                          Withdrawals     Deposits      Balance")
  rows <- c("19 Aug   Opening balance                                                19,477.46 OD",
            "19 Aug   DD VET                                     157.00                19,634.46 OD",
            "19 Aug   AP GO EXPENSES                             300.00                19,934.46 OD",
            "19 Aug   DD DEBIT TRANSFER                           30.00                19,964.46 OD",
            "         Totals at end of period                   $487.00       $0.00   $19,964.46 OD")
  rd <- auto_read(ar_pdf(ar_cover, c(h, rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), c(-157, -300, -30))
  expect_true(isTRUE(ok_of(rd, "opening_closing")))
  # The same figure wrong in the totals line breaks the chain: a person looks.
  bad <- rows; bad[5] <- sub("19,964.46", "19,999.99", bad[5], fixed = TRUE)
  expect_false(ar_auto(auto_read(ar_pdf(ar_cover, c(h, bad)))))
  # Printed under the table after a gap, it is still the table's totals row.
  gap <- c(rows[1:4], "", "", rows[5])
  rd <- auto_read(ar_pdf(ar_cover, c(h, gap)))
  expect_equal(rd$outcome, "proven")
  expect_true(isTRUE(ok_of(rd, "opening_closing")))
})

test_that("a summary box's single total in the balance column is not a closing balance", {
  # "Total withdrawals 8,827.73" in an account summary above the table lines up with
  # the balance column; read as a balance it broke every chain it touched.
  h <- c("Kauri Bank                         Statement period 1 Feb 2026 to 28 Feb 2026", "",
         "                                                  Total withdrawals      777.27",
         "                                                  Total deposits       3,122.18", "",
         "Date     Details                          Withdrawals     Deposits      Balance")
  rd <- auto_read(ar_pdf(c(h, ar_rows)))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), ar_want)
})
