# test-recipes-designs.R -- the shipped recipes for the remaining real designs the
# owner described (docs/context/RECIPE_STATEMENTS): Bank of China Online Saver,
# American Express Gold Card, TSB Saver Plus (text and scanned) and HSBC Everyday,
# and the recipe words they need: a period in the design's own pattern, the
# design's own words for its opening and closing balance, what a plain figure
# means, a date column with no heading, subtotals, summary totals that are not the
# table's, sections in their own date order, and a date printed once per day.
#
# Each statement is written as lines of text (one word box per word, 5pt a
# character, 12pt a line), from the block's own sample with placeholder names.
# The rule throughout: the recipe proposes, the statement's arithmetic decides.

dz_pdf <- function(...) {
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
       page_width = rep(600, length(pages)), page_height = rep(800, length(pages)),
       page_ocr = rep(FALSE, length(pages)), meta = list())
}
# dz_line(text1, at1, text2, at2, ...) -- one printed line: each text starts at its
# column, or ends at it when the column is given negative (right aligned).
dz_line <- function(...) {
  a <- list(...); s <- rep(" ", 130)
  for (k in seq(1, length(a), 2)) {
    t <- a[[k]]; at <- a[[k + 1L]]
    if (at < 0) at <- -at - nchar(t) + 1L
    ch <- strsplit(t, "")[[1]]; s[at:(at + length(ch) - 1L)] <- ch
  }
  sub("\\s+$", "", paste(s, collapse = ""))
}
dz_recipe <- function(id) {
  rcs <- recipes_load(fixture("recipes"))
  expect_identical(attr(rcs, "problems"), character(0))
  Filter(function(r) identical(r$id, id), rcs)
}
dz_read <- function(inp, rc) recipe_first(inp, opts = list(recipes = rc))
dz_edit <- function(inp, from, to) {
  for (p in seq_along(inp$words)) inp$words[[p]]$text[inp$words[[p]]$text == from] <- to
  inp$pages <- gsub(from, to, inp$pages, fixed = TRUE)
  inp
}

# ---- Bank of China Online Saver (block 29) ----------------------------------------

boc_page <- function(rows = boc_rows, open = "0.33", close = "0.36") c(
  "BANK OF CHINA", "STATEMENT OF ACCOUNT", "Statement Reference No. 0001",
  "From(YYYYMMDD):20211001 To(YYYYMMDD):20211031", "Online Saver Statmt NZD-PB", "",
  paste0("Currency/币种:NZD Previous Period Balance/上期余额: ", open),
  dz_line("Tr.D.", 1, "Val.D.", 9, "Vou. No./Trans. No. /Purpose/Details", 17, "Tran. Amount", -90, "Balance", -104),
  dz_line("YYMMDD", 1, "YYMMDD", 9),
  rows, "",
  paste0("Balance for the Period/本期余额: ", close),
  "Num of Transactions of Debit: 1 Debit Amount: 3,842.90", "Num of Transactions of Credit: 2 Credit Amount: 3,842.93",
  "", "Page 1 of 1")
boc_rows <- c(
  dz_line("211015", 1, "211015", 9, "LOCAL CLEARING AND INWARD REMITTANCE", 17, "3,842.90", -90, "3,843.23", -104),
  dz_line("PAYER A REF: HOME LOAN", 17),
  dz_line("211018", 1, "211018", 9, "LOAN REPAYMENT", 17, "-3,842.90", -90, "0.33", -104),
  dz_line("BANK OF CHINA PERSONAL BANKING LOAN REP", 17),
  dz_line("211031", 1, "211101", 9, "INTEREST SETTLEMENT", 17, "0.03", -90, "0.36", -104))

test_that("Bank of China Online Saver: read under its bilingual heading and proven", {
  rc <- dz_recipe("bank_of_china_online_saver_statmt_nzd_pb")
  rd <- dz_read(dz_pdf(boc_page()), rc)
  expect_identical(rd$outcome, "proven")
  tx <- rd$transactions
  expect_equal(tx$amount, c(3842.90, -3842.90, 0.03))
  expect_identical(format(tx$date), c("2021-10-15", "2021-10-18", "2021-10-31"))
  # the description takes in the lines under the row
  expect_match(tx$description[1], "LOCAL CLEARING AND INWARD REMITTANCE PAYER A REF: HOME LOAN", fixed = TRUE)
  # its own words for the opening and closing balance, and its own period pattern
  expect_equal(rd$parsed$header$opening_balance, 0.33)
  expect_equal(rd$parsed$header$closing_balance, 0.36)
  expect_identical(rd$parsed$header$period_start, "20211001")
})

test_that("Bank of China Online Saver: a figure that does not add up is never proven", {
  rc <- dz_recipe("bank_of_china_online_saver_statmt_nzd_pb")
  bad <- boc_rows; bad[5] <- sub("0.03", "0.08", bad[5], fixed = TRUE)
  expect_false(identical(dz_read(dz_pdf(boc_page(bad)), rc)$outcome, "proven"))
  # a closing balance that is not the opening plus the rows
  expect_false(identical(dz_read(dz_pdf(boc_page(close = "0.39")), rc)$outcome, "proven"))
})

test_that("Bank of China Online Saver: a statement with no entries is proven only with equal balances", {
  rc <- dz_recipe("bank_of_china_online_saver_statmt_nzd_pb")
  none <- dz_line("No entries for this period.", 17)
  expect_identical(dz_read(dz_pdf(boc_page(none, open = "0.36", close = "0.36")), rc)$outcome, "proven")
  expect_false(identical(dz_read(dz_pdf(boc_page(none, open = "0.33", close = "0.36")), rc)$outcome, "proven"))
})

test_that("plus_means settles only a reading against its exact negation", {
  rc <- dz_recipe("bank_of_china_online_saver_statmt_nzd_pb")[[1]]
  expect_identical(rc$plus_means, "money_in")
  # without it, the arithmetic alone cannot say which way round a savings account's
  # plain figures run, and the reading is not proven
  rc0 <- rc; rc0$plus_means <- NA_character_
  rd <- dz_read(dz_pdf(boc_page()), list(rc0))
  expect_false(identical(rd$outcome, "proven"))
  expect_identical(.rc_validate(list(recipe = "x", format = 1, version = 1, bank = "anz",
    recognise = list(all = "a"), table = list(header = c("Date", "Details", "Debit", "Credit"),
      columns = list(date = list(under = "Date"), description = list(under = "Details"),
                     debit = list(under = "Debit"), credit = list(under = "Credit"))),
    dates = list(format = "%d/%m/%Y"), money = list(style = "debit_credit_cols", plus_means = "money_in")))$error,
    "`money: plus_means:` is for a signed amount column.")
})

# ---- American Express Gold Card (block 3) -------------------------------------------

amex_page <- function(adj = "0.20 CR", close = "1,543.30") c(
  "American Express", "Statement of Account", "Statement Period From March 27, 2026 to April 28, 2026",
  "Opening Balance 765.84", "New Credits 766.04", "New Debits 1,543.50",
  paste("Closing Balance", close), paste("Amount Payable", close), "",
  dz_line("Details", 9, "Foreign Spending", -70, "Amount $", -90),
  dz_line("17 Apr", 1, "PAYMENT - THANK YOU", 9, "765.84 CR", -93),
  dz_line("New Transactions for PERSON A", 1),
  dz_line("28 Mar", 1, "MERCHANT A", 9, "122.16", -90),
  dz_line("10 Apr", 1, "MERCHANT B", 9, "355.00", -90),
  dz_line("15 Apr", 1, "MERCHANT C", 9, "188.60", -90),
  dz_line("Total of new transactions for PERSON A", 1, "665.76", -90),
  dz_line("New Transactions for PERSON B", 1),
  dz_line("29 Mar", 1, "MERCHANT D SYDNEY", 9, "30.00 AUSTRALIAN DOLLAR", 55, "31.74", -90),
  dz_line("NZD 31.74 includes conversion commission of NZD .77", 9),
  dz_line("03 Apr", 1, "MERCHANT E", 9, "846.00", -90),
  dz_line("Total of new transactions for PERSON B", 1, "877.74", -90),
  dz_line("OTHER ACCOUNT TRANSACTIONS", 1),
  dz_line("16 Apr", 1, "CREDIT ADJUSTMENT", 9, adj, if (grepl("CR", adj)) -93 else -90),
  dz_line("Total of other account transactions", 1, "0.20 CR", -93), "",
  "Please check all transactions carefully and immediately advise us of any unauthorised use of the Card.",
  "Page 1 of 1")

test_that("American Express: cardholder sections, a date with no heading, CR credits, proven by the summary", {
  rc <- dz_recipe("amex_american_express_gold_card")
  rd <- dz_read(dz_pdf(amex_page()), rc)
  expect_identical(rd$outcome, "proven")
  tx <- rd$transactions
  # charges are money out, the payment and the credit (CR) money in; the section
  # totals are not rows
  expect_equal(tx$amount, c(765.84, -122.16, -355.00, -188.60, -31.74, -846.00, 0.20))
  expect_identical(format(tx$date[1:2]), c("2026-04-17", "2026-03-28"))
  # the foreign-currency sub-line is the row's description; the foreign amount is
  # not money moved
  expect_match(tx$description[5], "MERCHANT D SYDNEY NZD 31.74 includes conversion commission", fixed = TRUE)
  expect_false(grepl("AUSTRALIAN", tx$description[5], fixed = TRUE))
  expect_false(any(abs(tx$amount) == 30))
})

test_that("American Express: a wrong figure, or a missing CR, is never proven", {
  rc <- dz_recipe("amex_american_express_gold_card")
  expect_false(identical(dz_read(dz_pdf(amex_page(close = "1,543.90")), rc)$outcome, "proven"))
  # the credit printed without its CR is a charge: opening + rows no longer closes
  expect_false(identical(dz_read(dz_pdf(amex_page(adj = "0.20")), rc)$outcome, "proven"))
})

test_that("in_order: false lets sections run their own dates; every date must still be in the period", {
  rc <- dz_recipe("amex_american_express_gold_card")
  expect_false(rc[[1]]$in_order)
  # a row dated before the period is outside it, whatever the sections
  inp <- dz_pdf(sub("28 Mar", "20 Mar", amex_page(), fixed = TRUE))
  expect_false(identical(dz_read(inp, rc)$outcome, "proven"))
  # a recipe that says its dates run in order holds the same statement back
  r1 <- rc[[1]]; r1$in_order <- TRUE
  expect_false(identical(dz_read(dz_pdf(amex_page()), list(r1))$outcome, "proven"))
})

# ---- TSB Saver Plus (blocks 64 and 65) ------------------------------------------------

tsb_head <- function() c("TSB", "Your Statement", "Statement Number 12", "Period 17May26 - 19Aug26",
  "Saver Plus 00-0000-0000000-000",
  dz_line("Date", 1, "Serial", 10, "Transaction details", 18, "Withdrawals", -80, "Deposits", -95, "Balance", -110))
tsb_pages <- function(fee = "0.25", close = "$4,178.05") list(
  c(tsb_head(), dz_line("Opening balance", 18, "54.84", -110),
    dz_line("17May26", 1, "BUSINESS A", 18, "4,604.92", -95, "4,659.76", -110),
    dz_line("21May26", 1, "MERCHANT A Purchase", 18, "86.96", -80, "4,572.80", -110),
    dz_line("22May26", 1, "MERCHANT B Purchase", 18, "10.00", -80, "4,562.80", -110),
    dz_line("MERCHANT C Purchase", 18, "5.00", -80, "4,557.80", -110),
    dz_line("Payment Service Charge", 18, fee, -80, "4,557.55", -110),
    dz_line("Carried forward", 18, "102.21", -80, "4,604.92", -95, "4,557.55", -110), "", "Page 1 of 2"),
  c(tsb_head(), dz_line("Brought forward", 18, "102.21", -80, "4,604.92", -95, "4,557.55", -110),
    dz_line("23May26", 1, "MERCHANT D", 18, "360.00", -80, "4,197.55", -110),
    dz_line("MERCHANT E Purchase", 18, "19.50", -80, "4,178.05", -110),
    dz_line("Closing totals", 18, "$481.71", -80, "$4,604.92", -95, close, -110), "", "Page 2 of 2"))

test_that("TSB Saver Plus: a date printed once per day is carried down, over two pages, and proven", {
  rc <- dz_recipe("tsb_saver_plus")
  rd <- dz_read(do.call(dz_pdf, tsb_pages()), rc)
  expect_identical(rd$outcome, "proven")
  tx <- rd$transactions
  expect_equal(tx$amount, c(4604.92, -86.96, -10.00, -5.00, -0.25, -360.00, -19.50))
  expect_identical(format(tx$date), c("2026-05-17", "2026-05-21", "2026-05-22", "2026-05-22", "2026-05-22",
                                      "2026-05-23", "2026-05-23"))
  expect_identical(grepl("date_carried", tx$flags), c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE))
  # the service charge is a row of its own
  expect_identical(tx$description[5], "Payment Service Charge")
})

test_that("TSB Saver Plus: a wrong fee is never proven, and a recipe that does not carry dates holds it back", {
  rc <- dz_recipe("tsb_saver_plus")
  expect_false(identical(dz_read(do.call(dz_pdf, tsb_pages(fee = "0.35")), rc)$outcome, "proven"))
  r1 <- rc[[1]]; r1$carried <- FALSE
  rd <- dz_read(do.call(dz_pdf, tsb_pages()), list(r1))
  expect_false(identical(rd$outcome, "proven"))
})

# The scanned TSB design: the same statement as a picture, read by OCR.
tsb_cells <- function() {
  L <- function(...) lapply(list(...), function(x) x)
  hd <- list(L(c("TSB", 40, "l")), L(c("Your Statement", 40, "l")), L(c("Statement Number 12", 40, "l")),
             L(c("Period 17May26 - 19Aug26", 40, "l")), L(c("Saver Plus", 40, "l")),
             L(c("Date", 40, "l"), c("Serial", 95, "l"), c("Transaction details", 140, "l"), c("Withdrawals", 400, "r"),
               c("Deposits", 470, "r"), c("Balance", 545, "r")))
  row <- function(d, desc, dr = NULL, cr = NULL, bal) {
    out <- list(); if (nzchar(d)) out[[1]] <- c(d, 40, "l")
    out[[length(out) + 1L]] <- c(desc, 140, "l")
    if (!is.null(dr)) out[[length(out) + 1L]] <- c(dr, 400, "r")
    if (!is.null(cr)) out[[length(out) + 1L]] <- c(cr, 470, "r")
    out[[length(out) + 1L]] <- c(bal, 545, "r"); out
  }
  c(hd, list(row("", "Opening balance", bal = "54.84"),
             row("17May26", "BUSINESS A", cr = "4,604.92", bal = "4,659.76"),
             row("21May26", "MERCHANT A Purchase", dr = "86.96", bal = "4,572.80"),
             row("22May26", "MERCHANT B Purchase", dr = "10.00", bal = "4,562.80"),
             row("", "MERCHANT C Purchase", dr = "5.00", bal = "4,557.80"),
             row("", "Payment Service Charge", dr = "0.25", bal = "4,557.55"),
             row("23May26", "MERCHANT D", dr = "360.00", bal = "4,197.55"),
             row("", "Closing totals", dr = "$462.21", cr = "$4,604.92", bal = "$4,197.55"),
             list(), L(c("Page 1 of 1", 40, "l"))))
}

test_that("TSB Saver Plus scanned: the picture-only statement is OCR'd and proven with the same recipe", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  skip_if_not(requireNamespace("magick", quietly = TRUE))
  tp <- tempfile(fileext = ".pdf"); sp <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(tp, sp)), add = TRUE)
  grDevices::pdf(tp, width = 595 / 72, height = 842 / 72)
  grid::grid.newpage()
  cells <- tsb_cells()
  for (i in seq_along(cells)) for (cl in cells[[i]])
    grid::grid.text(cl[1], x = grid::unit(as.numeric(cl[2]), "points"), y = grid::unit(800 - i * 16, "points"),
                    just = if (cl[3] == "l") "left" else "right", gp = grid::gpar(fontsize = 10))
  grDevices::dev.off()
  magick::image_write(magick::image_read_pdf(tp, density = 200), sp, format = "pdf", density = "200x200")
  inp <- read_input(sp)
  expect_true(isTRUE(inp$page_ocr[1]))
  rd <- dz_read(inp, dz_recipe("tsb_saver_plus"))
  expect_identical(rd$outcome, "proven")
  expect_equal(rd$transactions$amount, c(4604.92, -86.96, -10.00, -5.00, -0.25, -360.00))
})

# ---- HSBC Everyday (block 47) --------------------------------------------------------

hsbc_head <- function() dz_line("Date", 1, "Transaction Details", 12, "Deposits", -80, "Withdrawals", -96,
                                "Balance (DR=Debit)", -115)
hsbc_pages <- function(more = NULL) c(list(
  c("HSBC", "Composite Statement", "Summary of Your Portfolio", "Statement Period From 30OCT2020 to 30NOV2020",
    "TOTAL DEPOSITS 46,787.40", "", "HSBC EVERYDAY A/C", hsbc_head(),
    dz_line("30OCT2020", 1, "BALANCE BROUGHT FORWARD", 12, "46,787.40", -115),
    dz_line("02NOV2020", 1, "EFTPOS MERCHANT A", 12, "2,299.98", -96, "44,487.42", -115),
    dz_line("MERCHANT B 01NOV20 10:15", 12, "153.45", -96, "44,333.97", -115),
    dz_line("10NOV2020", 1, "SALARY", 12, "2,000.00", -80, "46,333.97", -115),
    dz_line("BALANCE CARRIED FORWARD", 12, "46,333.97", -115), "", sprintf("Page 1 of %d", 2L + length(more))),
  c("HSBC EVERYDAY A/C", hsbc_head(),
    dz_line("BALANCE BROUGHT FORWARD", 12, "46,333.97", -115),
    dz_line("20NOV2020", 1, "TRANSFER", 12, "12,222.50", -96, "34,111.47", -115),
    dz_line("30NOV2020", 1, "CLOSING BALANCE", 12, "34,111.47", -115),
    dz_line("Transaction Turnover", 12, "2,000.00", -80, "14,675.93", -96),
    dz_line("Transaction Count", 12, "4", -80), "", "Statement Details", "MULTI CURRENCY A/C", sprintf("Page 2 of %d", 2L + length(more)))),
  more)

test_that("HSBC Everyday: portfolio totals are not the table's, dates carried, proven", {
  rc <- dz_recipe("hsbc_hsbc_everyday_a_c")
  rd <- dz_read(do.call(dz_pdf, hsbc_pages()), rc)
  expect_identical(rd$outcome, "proven")
  expect_equal(rd$transactions$amount, c(-2299.98, -153.45, 2000.00, -12222.50))
  expect_identical(format(rd$transactions$date[2]), "2020-11-02")
  # the date inside the description is not the row's date
  expect_match(rd$transactions$description[2], "01NOV20", fixed = TRUE)
})

test_that("HSBC Everyday: a second account section continued on its own page is never proven", {
  rc <- dz_recipe("hsbc_hsbc_everyday_a_c")
  usd <- list(c("MULTI CURRENCY A/C", hsbc_head(),
    dz_line("30OCT2020", 1, "BALANCE BROUGHT FORWARD", 12, "1,200.00", -115),
    dz_line("05NOV2020", 1, "INTEREST", 12, "0.40", -80, "1,200.40", -115),
    dz_line("30NOV2020", 1, "CLOSING BALANCE", 12, "1,200.40", -115), "", "Page 3 of 3"))
  rd <- dz_read(do.call(dz_pdf, hsbc_pages(usd)), rc)
  expect_false(identical(rd$outcome, "proven"))
})

# ---- the recipe words ---------------------------------------------------------------

test_that("the new recipe words are checked when a recipe loads", {
  base <- function(...) {
    y <- list(recipe = "x", format = 1, version = 1, bank = "anz", recognise = list(all = "a"),
              table = list(header = c("Details", "Amount"),
                           columns = list(date = list(left_of = "Details"), description = list(under = "Details"),
                                          amount = list(under = "Amount"))),
              dates = list(format = "%d %b", year = "period"), period = list(label = "Period"))
    m <- list(...); for (k in names(m)) y[[k]] <- m[[k]]
    .rc_validate(y)
  }
  expect_null(base()$error)
  expect_true(is.na(base()$cols$under[1]))
  expect_match(base(table = list(header = c("Details", "Amount"), columns = list(description = list(under = "Details"),
    date = list(left_of = "Details"), amount = list(under = "Amount"))))$error, "only the first column")
  expect_match(base(table = list(header = c("Details", "Amount"), columns = list(date = list(left_of = "Amount"),
    description = list(under = "Details"), amount = list(under = "Amount"))))$error, "heading of the column after it")
  expect_match(base(period = list(label = "Period", format = "%d %b"))$error, "must print a year")
  expect_match(base(period = list(label = "Period", format = "YYYYMMDD"))$error, "not a plain date pattern")
  expect_match(base(balances = list(start = "x"))$error, "opening")
  expect_match(base(dates = list(format = "%d %b", year = "period", carried = "yes"))$error, "carried")
  expect_match(base(dates = list(format = "%d %b", year = "period", in_order = "no"))$error, "in_order")
  expect_match(base(money = list(style = "signed", plus_means = "up"))$error, "money_in or money_out")
  # a label's words are each a printed word: "No./Trans." is one word
  expect_identical(.rc_words("Vou. No./Trans. No."), c("vou", "no./trans", "no"))
  expect_identical(.rc_words("Balance (DR=Debit)"), c("balance", "dr=debit"))
})
