# test-attacks.R -- the adversarial attack on the automatic reader, kept as
# regression tests. An attack found thirteen ways to get an AUTOMATIC result
# ("proven" or "layout_match") with a wrong date, amount, sign or row count; each
# case here is one of them, or one of the controls the attack ran beside them.
#
# The one rule every test holds the reader to: the result is right, or it is not
# automatic. Losing automation is allowed; a silently wrong figure is not. Where a
# fix should also keep a control automatic, the test says so too, so the fixes
# cannot quietly turn into "send everything to a person".
#
# Statements are written as lines of text, one word box per word, 5pt a character
# and 12pt a line (the same geometry as test-auto-read.R), so each test shows what
# is on the page. Layouts are learned the way the tool learns them: three proven
# statements from two accounts through layout_learn(), then read back from the
# store, so what a layout file keeps (and an old one lacks) is part of the test.

# ---- statement builders ------------------------------------------------------------------

atk_pdf <- function(...) {
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
atk_csv <- function(lines) list(kind = "delimited", path = "", sha256 = NA_character_, lines = lines,
                                meta = list(ext = "csv"))

atk_fmtm <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 2, big.mark = ","))
atk_rpad <- function(s, w) formatC(s, width = -w)
atk_lpad <- function(s, w) formatC(s, width = w)
# One table line: date (9 characters), details (35), then right-aligned figure
# cells of 14 characters each; and the heading row of the same shape.
atk_line <- function(date, desc, figs)
  paste0(atk_rpad(date, 9), atk_rpad(desc, 35), paste(vapply(figs, function(f) atk_lpad(f, 14), ""), collapse = ""))
atk_heads <- function(h)
  paste0(atk_rpad("Date", 9), atk_rpad("Details", 35), paste(vapply(h, function(x) atk_lpad(x, 14), ""), collapse = ""))

ATK_H <- c("Withdrawals", "Deposits")
ATK_MAR <- "Statement period 1 Mar 2026 to 31 Mar 2026"

# A statement with no running balance: money-out and money-in columns. `box`
# prints the opening and closing balances above the table (that is what proves a
# teaching statement); `swap` prints money in first, under the headings given.
atk_nobal <- function(tx, opening = 1000, heads = ATK_H, box = TRUE, period = ATK_MAR, swap = FALSE,
                      top = "Kauri Bank", extra_top = character(0), extra_bottom = character(0), account = NULL) {
  closing <- round(opening - sum(tx$out, na.rm = TRUE) + sum(tx$inn, na.rm = TRUE), 2)
  l <- c(paste0(atk_rpad(top, 35), period %||% ""), if (!is.null(account)) paste("Account number", account), extra_top, "")
  if (box) l <- c(l, paste0(atk_rpad("Opening balance", 20), atk_fmtm(opening)),
                  paste0(atk_rpad("Closing balance", 20), atk_fmtm(closing)), "")
  l <- c(l, atk_heads(heads))
  for (i in seq_len(nrow(tx))) {
    f <- c(atk_fmtm(tx$out[i]), atk_fmtm(tx$inn[i]))
    l <- c(l, atk_line(tx$date[i], tx$desc[i], if (swap) rev(f) else f))
  }
  c(l, extra_bottom)
}

# A statement with a running balance, opening and closing lines in the table.
atk_bal <- function(tx, opening, top_right, account = NULL, bottom = character(0)) {
  b <- opening
  out <- c(paste0(atk_rpad("Kauri Bank", 35), top_right), if (!is.null(account)) paste("Account number", account), "",
           paste0(atk_rpad("Date", 9), atk_rpad("Details", 35), atk_lpad("Withdrawals", 14), atk_lpad("Deposits", 14),
                  atk_lpad("Balance", 14)),
           atk_line("", "Opening balance", c("", "", atk_fmtm(opening))))
  for (i in seq_len(nrow(tx))) {
    b <- round(b - ifelse(is.na(tx$out[i]), 0, tx$out[i]) + ifelse(is.na(tx$inn[i]), 0, tx$inn[i]), 2)
    out <- c(out, atk_line(tx$date[i], tx$desc[i], c(atk_fmtm(tx$out[i]), atk_fmtm(tx$inn[i]), atk_fmtm(b))))
  }
  c(out, atk_line("", "Closing balance", c("", "", atk_fmtm(b))), bottom)
}

atk_tx <- function(date, desc, out, inn) data.frame(date = date, desc = desc, out = out, inn = inn,
                                                    stringsAsFactors = FALSE)
# The truth for a table: amount = in - out, the date in `year`.
atk_truth <- function(tx, year = 2026, fmt = "%d %b %Y")
  data.frame(date = format(as.Date(paste(tx$date, year), fmt), "%Y-%m-%d"),
             amount = round(ifelse(is.na(tx$inn), 0, tx$inn) - ifelse(is.na(tx$out), 0, tx$out), 2),
             stringsAsFactors = FALSE)

# The three February statements a layout is learned from, from two accounts.
atk_teach_tx <- function(k) atk_tx(
  c("03 Feb", "05 Feb", "09 Feb", "14 Feb", "21 Feb", "26 Feb"),
  c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES",
    "TRANSFER TO SAVINGS", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
  c(round(10 + k * 1.37, 2), NA, round(250 + k * 3.11, 2), 400 + k, round(90 + k * 2.2, 2), NA),
  c(NA, 3120 + k * 10, NA, NA, NA, round(2 + k * 0.13, 2)))
atk_acct <- function(k) sprintf("99-0001-01234%02d-00", 10 + (k %% 2))
atk_sha <- function(k) paste(rep(sprintf("%02x", k), 32), collapse = "")
ATK_FEB <- "Statement period 1 Feb 2026 to 28 Feb 2026"

# atk_learn(inputs) -- read each teaching statement, learn it into a fresh store as
# the tool does (each proven, from two accounts), and return the store's layouts.
atk_learn <- function(inputs) {
  d <- tempfile("atk_layouts_"); dir.create(d)
  for (k in seq_along(inputs)) {
    rd <- auto_read(inputs[[k]])
    expect_identical(rd$outcome, "proven")
    layout_learn(rd, "Kauri Bank", atk_sha(k), d, accounts = atk_acct(k))
  }
  L <- layouts_load(d, "Kauri Bank")
  expect_length(L, 1L)
  expect_identical(L[[1]]$layout$status, "proven")
  structure(L, dir = d)
}
atk_pdf_layouts <- function(heads = ATK_H)
  atk_learn(lapply(1:3, function(k) atk_pdf(atk_nobal(atk_teach_tx(k), heads = heads, period = ATK_FEB,
                                                      account = atk_acct(k)))))
atk_bal_layouts <- function()
  atk_learn(lapply(1:3, function(k) atk_pdf(atk_bal(atk_teach_tx(k), 1000 + 50 * k, ATK_FEB, atk_acct(k)))))
# The everyday CSV export: Date,Details,Amount (signed), opening and closing lines.
atk_teach_csv <- function(k, header = "Date,Details,Amount", account = TRUE) {
  tx <- atk_teach_tx(k); amt <- ifelse(is.na(tx$inn), -tx$out, tx$inn)
  d <- format(as.Date(paste(tx$date, "2026"), "%d %b %Y"), "%d/%m/%Y")
  op <- 1000 + 10 * k
  atk_csv(c(if (account) paste("Account:", atk_acct(k)), sprintf("Opening balance: %.2f", op), header,
            sprintf("%s,%s,%.2f", d, tx$desc, amt), sprintf("Closing balance: %.2f", round(op + sum(amt), 2))))
}
atk_csv_layouts <- function() atk_learn(lapply(1:3, atk_teach_csv))

# ---- the rule ------------------------------------------------------------------------------

atk_auto <- function(rd) rd$outcome %in% c("proven", "layout_match")
# Right, or not automatic: never an automatic result with any figure off.
expect_right_or_not_auto <- function(rd, want) {
  if (atk_auto(rd)) {
    got <- rd$transactions
    expect_equal(nrow(got), nrow(want))
    if (nrow(got) == nrow(want)) {
      expect_equal(as.character(got$date), want$date)
      expect_equal(round(got$amount, 2), want$amount)
    }
  } else expect_true(rd$outcome %in% c("check", "unread"))
}
atk_ok <- function(rd, name) rd$checks$ok[rd$checks$check == name]

# The March statement most attacks are variations of.
ATK_TX <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "17 Mar", "25 Mar"),
                 c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
                 c(18.50, NA, 268.15, 20.00, NA), c(NA, 3120.00, NA, NA, 4.10))

# ---- the year: stated, never guessed (FIX 1) ------------------------------------------------

test_that("a00: a running-balance statement still proves on its own", {
  rd <- auto_read(atk_pdf(atk_bal(atk_teach_tx(1), 1050, ATK_FEB)))
  expect_identical(rd$outcome, "proven")
  expect_right_or_not_auto(rd, atk_truth(atk_teach_tx(1)))
})

test_that("a02: a December statement issued in January takes the year before, never the issue date's", {
  L <- atk_bal_layouts()
  tx <- atk_tx(c("03 Dec", "08 Dec", "15 Dec", "22 Dec", "29 Dec"),
               c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
               c(14.20, NA, 268.15, 88.10, NA), c(NA, 3120.00, NA, NA, 2.40))
  # No period printed, only the day the statement was issued.
  input <- atk_pdf(atk_bal(tx, 2200, "Statement date 5 Jan 2026", account = "99-0001-0123499-00"))
  for (lys in list(list(), L)) {
    rd <- auto_read(input, layouts = lys)
    expect_right_or_not_auto(rd, atk_truth(tx, 2025))
    # The balance proves every figure and the issue date settles the year.
    expect_identical(rd$outcome, "proven")
    expect_identical(rd$transactions$date[1], "2025-12-03")
  }
  # A "statement date" that is not when the statement was issued (here the first
  # day of its month): every row would land eleven months before it, which no
  # statement does, so the date settles nothing and a person reads the year.
  rs <- auto_read(atk_pdf(atk_bal(tx, 2200, "Statement date 1 Dec 2025")))
  expect_right_or_not_auto(rs, atk_truth(tx, 2025))
  expect_false(atk_auto(rs))
  expect_false(atk_ok(rs, "year_settled"))
  # A January row on the same statement stays in the issue date's year.
  tx2 <- tx; tx2$date <- c("20 Dec", "24 Dec", "29 Dec", "02 Jan", "04 Jan")
  rd <- auto_read(atk_pdf(atk_bal(tx2, 2200, "Statement date 5 Jan 2026")))
  expect_identical(rd$transactions$date, c("2025-12-20", "2025-12-24", "2025-12-29", "2026-01-02", "2026-01-04"))
})

test_that("a03: a copyright footer's year is not the statement's (no balance)", {
  L <- atk_pdf_layouts()
  tx <- atk_tx(c("05 Jan", "09 Jan", "14 Jan", "21 Jan", "28 Jan"),
               c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
               c(14.20, NA, 268.15, 88.10, NA), c(NA, 3120.00, NA, NA, 2.40))
  rd <- auto_read(atk_pdf(atk_nobal(tx, box = FALSE, period = "Transaction listing",
                                    extra_bottom = c("", "", "Copyright 2025 Kauri Bank Limited. All rights reserved."))),
                  layouts = L)
  expect_right_or_not_auto(rd, atk_truth(tx, 2026))
  expect_identical(rd$outcome, "check")
  expect_false(atk_ok(rd, "year_settled"))
  expect_match(rd$why, "prints no year", fixed = TRUE)
})

test_that("a19: a copyright footer's year is not the statement's, even when the balance proves every figure", {
  L <- atk_bal_layouts()
  tx <- atk_tx(c("05 Jan", "09 Jan", "14 Jan", "21 Jan", "28 Jan"),
               c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
               c(14.20, NA, 268.15, 88.10, NA), c(NA, 3120.00, NA, NA, 2.40))
  input <- atk_pdf(atk_bal(tx, 2200, "Transaction history",
                           bottom = c("", "", "Copyright 2025 Kauri Bank Limited. All rights reserved.")))
  for (lys in list(list(), L)) {
    rd <- auto_read(input, layouts = lys)
    expect_right_or_not_auto(rd, atk_truth(tx, 2026))
    expect_identical(rd$outcome, "check")
    expect_false(atk_ok(rd, "year_settled"))
    # Every figure adds up: it is the year alone that holds it back.
    expect_true(atk_ok(rd, "balance_chain"))
  }
})

test_that("a15: a month named as the period ('Statement for December 2025') settles the year", {
  L <- atk_pdf_layouts()
  tx <- atk_tx(c("03 Dec", "08 Dec", "15 Dec", "22 Dec", "29 Dec"),
               c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
               c(14.20, NA, 268.15, 88.10, NA), c(NA, 3120.00, NA, NA, 2.40))
  rd <- auto_read(atk_pdf(atk_nobal(tx, box = FALSE, period = "Statement for December 2025",
                                    extra_top = c("Statement date 5 Jan 2026", "", "", ""))), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(tx, 2025))
  expect_identical(rd$transactions$date[1], "2025-12-03")
  expect_true(atk_ok(rd, "year_settled"))
})

test_that("a month-year period is read only when tied to the word statement, and only when there is one", {
  expect_identical(.month_period("Kauri Bank   Statement for December 2025"), c("01 December 2025", "31 December 2025"))
  expect_identical(.month_period("Statement period: Feb 2028"), c("01 February 2028", "29 February 2028"))
  expect_identical(.month_period("Your March 2026 statement"), c("01 March 2026", "31 March 2026"))
  expect_null(.month_period("Copyright 2025 Kauri Bank Limited"))
  expect_null(.month_period("New rates from March 2026"))
  expect_null(.month_period("Statement for December 2025 ... Statement for January 2026"))
  md <- extract_metadata(atk_pdf(c("Kauri Bank   Statement for December 2025", "Statement date 5 Jan 2026")))
  expect_identical(c(md$period_start, md$period_end), c("01 December 2025", "31 December 2025"))
})

test_that("a CSV whose dates print no year is never automatic: the year would be the clock's", {
  rd <- auto_read(atk_csv(c("Date,Details,Amount,Balance", ",Opening balance,,1000.00",
                            "03 Dec,EFTPOS RIVERSIDE DAIRY,-14.20,985.80",
                            "08 Dec,SALARY MATAI HOLDINGS,3120.00,4105.80",
                            "15 Dec,DD CITY COUNCIL RATES,-268.15,3837.65")))
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "year_settled"))
})

# ---- layout_match needs the statement to confirm the layout (FIX 2) --------------------------

test_that("a01: money columns swapped under heading words the reader does not know", {
  L <- atk_pdf_layouts(heads = c("Payments", "Receipts"))
  tx <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "17 Mar", "25 Mar"),
               c("POS HARBOUR CAFE", "WAGES MATAI HOLDINGS", "RATES INSTALMENT", "MOBILE TOPUP", "REBATE ACC"),
               c(18.50, NA, 268.15, 20.00, NA), c(NA, 3120.00, NA, NA, 45.10))
  rd <- auto_read(atk_pdf(atk_nobal(tx, heads = c("Receipts", "Payments"), box = FALSE, swap = TRUE)), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(tx))
  expect_identical(rd$outcome, "check")
  expect_match(rd$why, "headings over its columns are not the layout's", fixed = TRUE)
  # The control: the same columns the right way round still match the layout.
  ok <- auto_read(atk_pdf(atk_nobal(tx, heads = c("Payments", "Receipts"), box = FALSE)), layouts = L)
  expect_identical(ok$outcome, "layout_match")
  expect_right_or_not_auto(ok, atk_truth(tx))
})

test_that("a13: a file that prints no heading row never matches a layout (PDF and CSV)", {
  L <- atk_pdf_layouts()
  tx <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "17 Mar", "25 Mar"),
               c("POS HARBOUR CAFE", "WAGES MATAI HOLDINGS", "RATES INSTALMENT", "MOBILE TOPUP", "REBATE ACC"),
               c(18.50, NA, 268.15, 20.00, NA), c(NA, 3120.00, NA, NA, 45.10))
  lines <- atk_nobal(tx, box = FALSE, swap = TRUE)
  lines <- lines[!grepl("^Date ", lines)]
  rd <- auto_read(atk_pdf(lines), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(tx))
  expect_identical(rd$outcome, "check")
  expect_match(rd$why, "prints no heading row", fixed = TRUE)
  # Header-less CSV, credit before debit, against a Date,Details,Debit,Credit layout.
  teach <- function(k) {
    t <- atk_teach_tx(k); d <- format(as.Date(paste(t$date, "2026"), "%d %b %Y"), "%d/%m/%Y")
    op <- 1000 + 10 * k; cl <- round(op - sum(t$out, na.rm = TRUE) + sum(t$inn, na.rm = TRUE), 2)
    atk_csv(c(sprintf("Opening balance: %.2f", op), "Date,Details,Debit,Credit",
              sprintf("%s,%s,%s,%s", d, t$desc, ifelse(is.na(t$out), "", sprintf("%.2f", t$out)),
                      ifelse(is.na(t$inn), "", sprintf("%.2f", t$inn))), sprintf("Closing balance: %.2f", cl)))
  }
  Lc <- atk_learn(lapply(1:3, teach))
  d <- format(as.Date(paste(tx$date, "2026"), "%d %b %Y"), "%d/%m/%Y")
  csv <- sprintf("%s,%s,%s,%s", d, tx$desc, ifelse(is.na(tx$inn), "", sprintf("%.2f", tx$inn)),
                 ifelse(is.na(tx$out), "", sprintf("%.2f", tx$out)))
  rc <- auto_read(atk_csv(csv), layouts = Lc)
  expect_right_or_not_auto(rc, atk_truth(tx))
  expect_identical(rc$outcome, "check")
  expect_match(rc$why, "prints no heading row", fixed = TRUE)
  # The same file with its heading row matches the layout and reads right.
  rh <- auto_read(atk_csv(c("Date,Details,Debit,Credit",
                            sprintf("%s,%s,%s,%s", d, tx$desc, ifelse(is.na(tx$out), "", sprintf("%.2f", tx$out)),
                                    ifelse(is.na(tx$inn), "", sprintf("%.2f", tx$inn))))), layouts = Lc)
  expect_identical(rh$outcome, "layout_match")
  expect_right_or_not_auto(rh, atk_truth(tx))
})

test_that("a16: swapped columns under known headings, another bank's statement, an unknown bank", {
  L <- atk_pdf_layouts()
  tx <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "17 Mar", "25 Mar"),
               c("POS HARBOUR CAFE", "WAGES MATAI HOLDINGS", "RATES INSTALMENT", "MOBILE TOPUP", "REBATE ACC"),
               c(18.50, NA, 268.15, 20.00, NA), c(NA, 3120.00, NA, NA, 45.10))
  want <- atk_truth(tx)
  # (i) swapped, the known headings moved with them; (iii) the same as another bank.
  for (top in c("Kauri Bank", "Totara Bank")) {
    rd <- auto_read(atk_pdf(atk_nobal(tx, heads = rev(ATK_H), box = FALSE, swap = TRUE, top = top)), layouts = L)
    expect_right_or_not_auto(rd, want)
    expect_false(atk_auto(rd))
  }
  # (ii) a statement that names ASB, converted with Kauri Bank picked: with nothing
  # to add up it is not converted on Kauri Bank's layout.
  asb <- atk_pdf(atk_nobal(tx, box = FALSE, top = "ASB Bank Limited", account = "12-3040-0123456-00"))
  real <- get("read_input", envir = globalenv())
  assign("read_input", function(path, ...) asb, envir = globalenv())
  on.exit(assign("read_input", real, envir = globalenv()), add = TRUE)
  d <- tempfile("atk_conv_"); dir.create(d)
  p <- file.path(d, "asb.pdf"); writeLines("%PDF-1.4", p)
  r <- convert_statement(p, bank = "Kauri Bank", outdir = file.path(d, "out"), logdir = file.path(d, "logs"),
                         layouts_dir = attr(L, "dir"), tracking_dir = file.path(d, "tracking"),
                         requested_by = "tester", formats = "csv")
  expect_identical(r$status, "needs_review")
  expect_false(r$outcome %in% c("proven", "layout_match"))
  expect_match(r$reason, "confirm the bank first", fixed = TRUE)
})

test_that("a04: a card export sharing the everyday export's header is not read with everyday signs", {
  L <- atk_csv_layouts()
  card <- data.frame(date = c("03/03/2026", "06/03/2026", "11/03/2026", "18/03/2026", "24/03/2026"),
                     desc = c("COUNTDOWN KILBIRNIE", "PAYMENT RECEIVED THANK YOU", "AIR NZ ONLINE", "Z ENERGY PETONE", "NETFLIX.COM"),
                     printed = c(84.20, -500.00, 312.00, 96.40, 22.99), stringsAsFactors = FALSE)
  rd <- auto_read(atk_csv(c("Date,Details,Amount", sprintf("%s,%s,%.2f", card$date, card$desc, card$printed))), layouts = L)
  expect_right_or_not_auto(rd, data.frame(date = format(as.Date(card$date, "%d/%m/%Y")), amount = -card$printed,
                                          stringsAsFactors = FALSE))
  expect_identical(rd$outcome, "check")
  expect_match(rd$why, "PAYMENT RECEIVED", fixed = TRUE)
  # A statement that declares itself a card is not read on an everyday layout either.
  rd2 <- auto_read(atk_csv(c("Credit card statement", "Credit limit 5000.00  Minimum payment 25.00",
                             "Date,Details,Amount", sprintf("%s,%s,%.2f", card$date, toupper(c("a", "b", "c", "d", "e")),
                                                            card$printed))), layouts = L)
  expect_false(atk_auto(rd2))
  # The control: an everyday export of the layout, its wording agreeing, matches.
  ok <- auto_read(atk_csv(c("Date,Details,Amount", "13/03/2026,EFTPOS RIVERSIDE DAIRY,-14.20",
                            "16/03/2026,SALARY MATAI HOLDINGS,3120.00", "21/03/2026,DD CITY COUNCIL,-268.15")), layouts = L)
  expect_identical(ok$outcome, "layout_match")
  expect_equal(round(ok$transactions$amount, 2), c(-14.20, 3120.00, -268.15))
})

test_that("a08: a reversal printed with CR or a trailing minus in the money-out column", {
  L <- atk_pdf_layouts()
  tx <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "12 Mar", "25 Mar"),
               c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "DD REVERSAL CITY COUNCIL", "CREDIT INTEREST"),
               c(18.50, NA, 268.15, NA, NA), c(NA, 3120.00, NA, 268.15, 4.10))
  for (mk in c("268.15 CR", "268.15CR", "268.15-")) {
    lines <- atk_nobal(tx, box = FALSE)
    lines[grepl("DD REVERSAL", lines)] <- atk_line("12 Mar", "DD REVERSAL CITY COUNCIL", c(mk, ""))
    rd <- auto_read(atk_pdf(lines), layouts = L)
    expect_right_or_not_auto(rd, atk_truth(tx))
    expect_identical(rd$outcome, "check", info = mk)
    expect_match(rd$why, "never printed", fixed = TRUE, info = mk)
  }
})

test_that("a08 variant: a CR in the money-out column, on a layout whose deposits print CR", {
  # The layout's own statements print CR on every deposit, so CR is no new marker;
  # in the money-out column it is still the opposite of its column.
  teach <- function(k) {
    l <- atk_nobal(atk_teach_tx(k), period = ATK_FEB, account = atk_acct(k))
    dep <- grepl("SALARY|CREDIT INTEREST", l)
    l[dep] <- paste0(l[dep], " CR")
    atk_pdf(l)
  }
  L <- atk_learn(lapply(1:3, teach))
  expect_true("CR" %in% L[[1]]$layout$signature$sign_markers)
  tx <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "12 Mar", "25 Mar"),
               c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "DD REVERSAL CITY COUNCIL", "CREDIT INTEREST"),
               c(18.50, NA, 268.15, NA, NA), c(NA, 3120.00, NA, 268.15, 4.10))
  lines <- atk_nobal(tx, box = FALSE)
  dep <- grepl("SALARY|CREDIT INTEREST", lines)
  lines[dep] <- paste0(lines[dep], " CR")
  lines[grepl("DD REVERSAL", lines)] <- atk_line("12 Mar", "DD REVERSAL CITY COUNCIL", c("268.15 CR", ""))
  rd <- auto_read(atk_pdf(lines), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(tx))
  expect_identical(rd$outcome, "check")
  expect_match(rd$why, "money-out column", fixed = TRUE)
})

test_that("a04 variant: a card export with no telling wording at all is not read on the layout's signs", {
  L <- atk_csv_layouts()
  printed <- c(84.20, 312.00, 96.40, 22.99)
  rd <- auto_read(atk_csv(c("Date,Details,Amount",
                            sprintf("%s,%s,%.2f", c("13/03/2026", "16/03/2026", "18/03/2026", "24/03/2026"),
                                    c("COUNTDOWN KILBIRNIE", "AIR NZ ONLINE", "Z ENERGY PETONE", "NETFLIX.COM"), printed))),
                  layouts = L)
  expect_right_or_not_auto(rd, data.frame(date = c("2026-03-13", "2026-03-16", "2026-03-18", "2026-03-24"),
                                          amount = -printed, stringsAsFactors = FALSE))
  expect_identical(rd$outcome, "check")
  expect_match(rd$why, "which way its amounts run", fixed = TRUE)
})

test_that("a07 and a17: pending, scheduled and upcoming items are not the statement's rows", {
  L <- atk_pdf_layouts()
  want <- atk_truth(ATK_TX)
  base <- atk_nobal(ATK_TX, box = FALSE)
  decoy <- c(atk_line("03 Apr", "RENT HARBOUR PROPERTY", c("400.00", "")), atk_line("07 Apr", "INSURANCE TOWER", c("85.30", "")))
  pend <- c(atk_line("30 Mar", "EFTPOS HARBOUR CAFE", c("6.50", "")), atk_line("31 Mar", "REFUND STORE", c("", "19.99")))
  cases <- list(
    i = list(c(base, "", "Upcoming automatic payments", atk_heads(ATK_H), decoy)),
    ii = list(base, c("Kauri Bank", "", "Upcoming automatic payments", "", atk_heads(ATK_H), decoy)),
    iii = list(c(base, "Pending transactions (not yet processed)", pend)))
  for (nm in names(cases)) {
    rd <- auto_read(do.call(atk_pdf, cases[[nm]]), layouts = L)
    expect_right_or_not_auto(rd, want)
    expect_false(atk_auto(rd), info = nm)
  }
  rd <- auto_read(atk_pdf(c(base, "Pending transactions (not yet processed)", pend)), layouts = L)
  expect_false(atk_ok(rd, "table_unbroken") %in% TRUE)
  # a17: a "Scheduled payments" table under the rows, dated inside the period.
  tx4 <- ATK_TX[1:4, ]
  lines <- c(atk_nobal(tx4, box = FALSE, period = "Transaction listing 1 Mar 2026 to 31 Mar 2026"),
             "", "Scheduled payments", atk_heads(ATK_H),
             atk_line("25 Mar", "AP RENT HARBOUR PROPERTY", c("400.00", "")),
             atk_line("28 Mar", "AP INSURANCE TOWER", c("85.30", "")))
  rd <- auto_read(atk_pdf(lines), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(tx4))
  expect_identical(rd$outcome, "check")
  expect_match(rd$why, "Scheduled payments", fixed = TRUE)
})

test_that("a07 control: a section break means nothing where the balance proves every row", {
  # The same break under a statement with a running balance: the arithmetic
  # accounts for every row, so the check does not apply and the reading proves.
  tx <- atk_teach_tx(1)
  rd <- auto_read(atk_pdf(c(atk_bal(tx, 1050, ATK_FEB), "", "Scheduled payments", "There are none.")))
  expect_identical(rd$outcome, "proven")
  expect_right_or_not_auto(rd, atk_truth(tx))
})

test_that("a12: an undated detail line with a figure is not a transaction on a layout that dates every row", {
  L <- atk_pdf_layouts()
  tx <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "25 Mar"),
               c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "AMAZON MARKETPLACE SEATTLE", "CREDIT INTEREST"),
               c(18.50, NA, 41.20, NA), c(NA, 3120.00, NA, 4.10))
  second <- list(i = atk_line("", "  FOREIGN AMOUNT", c("USD 25.00", "")),
                 iv_same_day_elsewhere = atk_line("", "  FOREIGN AMOUNT USD", c("25.00", "")),
                 ii = atk_line("", "  FOREIGN AMOUNT USD", c("25.00", "")),
                 iii = atk_line("", "  INCL OFFSHORE FEE", c("1.04", "")))
  for (nm in names(second)) {
    t <- tx
    if (nm == "iv_same_day_elsewhere") t$date[1] <- "04 Mar"
    lines <- atk_nobal(t, box = FALSE)
    lines <- append(lines, second[[nm]], after = which(grepl("AMAZON", lines)))
    rd <- auto_read(atk_pdf(lines), layouts = L)
    expect_right_or_not_auto(rd, atk_truth(t))
    expect_identical(rd$outcome, "check", info = nm)
  }
})

test_that("a06: a dated row described as a total is never dropped without the arithmetic to show it", {
  L <- atk_pdf_layouts()
  for (desc in c("TOTAL FEES", "TOTAL", "TOTAL TRANSACTIONS")) {
    tx <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "17 Mar", "25 Mar", "31 Mar"),
                 c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", desc, "VISA HARBOUR FUEL", "CREDIT INTEREST", "ATM KILBIRNIE"),
                 c(18.50, NA, 12.50, 20.00, NA, 60.00), c(NA, 3120.00, NA, NA, 4.10, NA))
    rd <- auto_read(atk_pdf(atk_nobal(tx, box = FALSE)), layouts = L)
    expect_right_or_not_auto(rd, atk_truth(tx))
    expect_identical(rd$outcome, "check", info = desc)
  }
})

test_that("a18: an undated daily-total line in the money-out column", {
  L <- atk_pdf_layouts()
  tx <- atk_tx(c("02 Mar", "04 Mar", "04 Mar", "10 Mar", "17 Mar"),
               c("EFTPOS RIVERSIDE DAIRY", "EFTPOS HARBOUR CAFE", "EFTPOS CITY PARKING", "SALARY MATAI HOLDINGS", "VISA HARBOUR FUEL"),
               c(18.50, 6.50, 4.00, NA, 20.00), c(NA, NA, NA, 3120.00, NA))
  for (w in c("Daily total", "Total spent", "Total for the day")) {
    lines <- atk_nobal(tx, box = FALSE)
    lines <- append(lines, atk_line("", w, c("10.50", "")), after = max(which(grepl("^04 Mar", lines))))
    rd <- auto_read(atk_pdf(lines), layouts = L)
    expect_right_or_not_auto(rd, atk_truth(tx))
    expect_false(atk_auto(rd), info = w)
  }
})

# ---- day-month order: only the statement settles it (FIX 3) ---------------------------------

test_that("a05 and a20: a learned layout does not settle day-month against month-day", {
  L <- atk_csv_layouts()
  desc <- c("EFTPOS DAIRY", "SALARY MATAI", "RATES", "ATM")
  amt <- c(-12.40, 3120.00, -268.15, -40.00)
  # (a05 i) day/month, every day 12 or less, in order either way; (a05 ii) a
  # month/day export; (a05 iii) one row; (a20 i) month/day, in order either way.
  cases <- list(
    list(d = c("01/03/2026", "02/03/2026", "05/03/2026", "09/03/2026"), f = "%d/%m/%Y", a = amt, s = desc),
    list(d = c("01/05/2026", "01/09/2026", "02/03/2026", "02/11/2026"), f = "%m/%d/%Y", a = amt, s = desc),
    list(d = "04/03/2026", f = "%d/%m/%Y", a = -12.40, s = "EFTPOS DAIRY"),
    list(d = c("01/02/2026", "01/03/2026", "01/05/2026", "01/09/2026", "01/12/2026"), f = "%m/%d/%Y",
         a = c(-12.40, 3120.00, -268.15, -40.00, 2.18), s = c(desc, "INTEREST")))
  for (k in seq_along(cases)) {
    cc <- cases[[k]]
    rd <- auto_read(atk_csv(c("Date,Details,Amount", sprintf("%s,%s,%.2f", cc$d, cc$s, cc$a))), layouts = L)
    expect_right_or_not_auto(rd, data.frame(date = format(as.Date(cc$d, cc$f)), amount = cc$a, stringsAsFactors = FALSE))
    expect_false(atk_auto(rd), info = k)
  }
  # (a20 ii) a running balance proves every figure on a month/day statement, and
  # the dates still read both ways: a person reads them.
  md <- c("01/02/2026", "01/03/2026", "01/05/2026", "01/09/2026", "01/12/2026")
  nl <- function(date, desc, figs) paste0(atk_rpad(date, 13), atk_rpad(desc, 32), paste(vapply(figs, function(f) atk_lpad(f, 14), ""), collapse = ""))
  bal_pdf <- function(dates, desc, out, inn, opening, top, account = NULL) {
    b <- opening
    l <- c(top, if (!is.null(account)) paste("Account number", account), "",
           paste0(atk_rpad("Date", 13), atk_rpad("Details", 32), atk_lpad("Withdrawals", 14), atk_lpad("Deposits", 14), atk_lpad("Balance", 14)),
           nl("", "Opening balance", c("", "", atk_fmtm(opening))))
    for (i in seq_along(dates)) {
      b <- round(b - ifelse(is.na(out[i]), 0, out[i]) + ifelse(is.na(inn[i]), 0, inn[i]), 2)
      l <- c(l, nl(dates[i], desc[i], c(atk_fmtm(out[i]), atk_fmtm(inn[i]), atk_fmtm(b))))
    }
    c(l, nl("", "Closing balance", c("", "", atk_fmtm(b))))
  }
  Lp <- atk_learn(lapply(1:3, function(k) {
    t <- atk_teach_tx(k); d <- format(as.Date(paste(t$date, "2026"), "%d %b %Y"), "%d/%m/%Y")
    atk_pdf(bal_pdf(d, t$desc, t$out, t$inn, 1000 + 50 * k, paste0(atk_rpad("Kauri Bank", 35), ATK_FEB), atk_acct(k)))
  }))
  out <- c(14.20, NA, 268.15, 88.10, NA); inn <- c(NA, 3120.00, NA, NA, 2.40)
  input <- atk_pdf(bal_pdf(md, c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
                           out, inn, 2200, "Kauri Bank                         Account activity"))
  for (lys in list(list(), Lp)) {
    rd <- auto_read(input, layouts = lys)
    expect_right_or_not_auto(rd, data.frame(date = format(as.Date(md, "%m/%d/%Y")),
                                            amount = round(ifelse(is.na(inn), 0, inn) - ifelse(is.na(out), 0, out), 2),
                                            stringsAsFactors = FALSE))
    expect_identical(rd$outcome, "check")
    expect_false(atk_ok(rd, "dates_settled"))
  }
})

# ---- controls: what the attack could not break, and must still work -----------------------

test_that("a09: a statement printed newest first against a layout learned oldest first", {
  L <- atk_pdf_layouts()
  tx <- ATK_TX[5:1, ]
  rd <- auto_read(atk_pdf(atk_nobal(tx, box = FALSE)), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(tx))
  btx <- atk_tx(c("12 Jan", "05 Jan", "28 Dec", "20 Dec"),
                c("CREDIT INTEREST", "VISA HARBOUR FUEL", "SALARY MATAI HOLDINGS", "EFTPOS RIVERSIDE DAIRY"),
                c(NA, 20.00, NA, 18.50), c(4.10, NA, 3120.00, NA))
  key <- atk_truth(btx); key$date <- c("2026-01-12", "2026-01-05", "2025-12-28", "2025-12-20")
  rd <- auto_read(atk_pdf(atk_nobal(btx, box = FALSE, period = "Statement period 15 Dec 2025 to 14 Jan 2026")), layouts = L)
  expect_right_or_not_auto(rd, key)
})

test_that("a10, a11, a14: duplicates, odd money print and rows after a gap still match the layout", {
  L <- atk_pdf_layouts()
  # a10: two identical coffees on one day.
  dup <- atk_tx(c("02 Mar", "04 Mar", "04 Mar", "04 Mar", "25 Mar"),
                c("EFTPOS RIVERSIDE DAIRY", "EFTPOS HARBOUR CAFE", "EFTPOS HARBOUR CAFE", "SALARY MATAI HOLDINGS", "CREDIT INTEREST"),
                c(18.50, 6.50, 6.50, NA, NA), c(NA, NA, NA, 3120.00, 4.10))
  rd <- auto_read(atk_pdf(atk_nobal(dup, box = FALSE)), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(dup))
  expect_identical(rd$outcome, "layout_match")
  # a11: a large amount with a space for thousands, a currency sign set apart, plain.
  tx <- ATK_TX[c(1, 2, 3, 5), ]
  for (v in c("3 120.00", "$ 3,120.00", "3,120.00")) {
    lines <- atk_nobal(tx, box = FALSE)
    lines[grepl("SALARY", lines)] <- atk_line("04 Mar", "SALARY MATAI HOLDINGS", c("", v))
    rd <- auto_read(atk_pdf(lines), layouts = L)
    expect_right_or_not_auto(rd, atk_truth(tx))
  }
  # a14: rows after a wide gap; the last rows carried to a page 2 with no heading;
  # a row dated in a second style.
  Lp <- atk_nobal(ATK_TX, box = FALSE)
  n <- length(Lp)
  rd <- auto_read(atk_pdf(c(Lp[1:(n - 2)], rep("", 5), Lp[(n - 1):n])), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(ATK_TX))
  rd <- auto_read(atk_pdf(Lp[1:(n - 2)], c("Kauri Bank", "", Lp[(n - 1):n])), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(ATK_TX))
  L3 <- Lp; L3[grepl("DD CITY", L3)] <- atk_line("10/03", "DD CITY COUNCIL RATES", c("268.15", ""))
  rd <- auto_read(atk_pdf(L3), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(ATK_TX))
})

test_that("a layout file written before headings were kept still loads, and carries no layout_match", {
  L <- atk_pdf_layouts()
  d <- attr(L, "dir")
  f <- list.files(d, "[.]yaml$", recursive = TRUE, full.names = TRUE)
  f <- f[length(f)]
  y <- yaml::read_yaml(f)
  y$layout$signature$col_headings <- NULL
  writeLines(yaml::as.yaml(y), f)
  old <- layouts_load(d, "Kauri Bank")
  expect_length(old, 1L)
  expect_length(attr(old, "problems"), 0L)
  expect_length(old[[1]]$layout$signature$col_headings, 0L)
  rd <- auto_read(atk_pdf(atk_nobal(ATK_TX, box = FALSE)), layouts = old)
  expect_identical(rd$outcome, "check")
  expect_match(rd$why, "learned without a heading", fixed = TRUE)
  expect_right_or_not_auto(rd, atk_truth(ATK_TX))
  # It still reads and proves a statement whose own arithmetic proves it.
  rp <- auto_read(atk_pdf(atk_nobal(ATK_TX, box = TRUE)), layouts = old)
  expect_identical(rp$outcome, "proven")
  expect_right_or_not_auto(rp, atk_truth(ATK_TX))
})

test_that("the new checks are tracked and worded", {
  for (ck in c("year_settled", "table_unbroken", "summary_lines_checked")) {
    expect_true(ck %in% TRACK_CHECKS, info = ck)
  }
})
