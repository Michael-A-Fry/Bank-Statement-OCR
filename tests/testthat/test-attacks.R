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
  # The titled pending section is set aside before the columns are measured
  # (round 2); with nothing to add up, setting it aside is what holds it back.
  expect_false(atk_ok(rd, "sections_set_aside") %in% TRUE)
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
  for (ck in c("year_settled", "table_unbroken", "summary_lines_checked",
               "rows_once", "one_statement", "rows_between_ends", "one_side_per_row",
               "ends_printed", "sections_set_aside", "currency_own", "workbook_plain")) {
    expect_true(ck %in% TRACK_CHECKS, info = ck)
  }
})

# ==== round 2: a second attack, and a stress test of damaged copies ===========================
# A second attack found 27 more ways to an automatic result with a wrong figure,
# and a stress test of damaged copies of known statements found seven. Each fix is
# one general rule; each case below is the attack's (or the stress test's) own
# shape, written small, beside the control that must still work.

ATK_HB <- atk_heads(c("Withdrawals", "Deposits", "Balance"))
# A running-balance page: `top` lines, the heading row, an in-table opening
# balance, the rows, and (unless `close = FALSE`) a closing balance.
atk_bal_page <- function(tx, opening, top, close = TRUE, open = TRUE) {
  b <- opening; rows <- character(0)
  for (i in seq_len(nrow(tx))) {
    b <- round(b - ifelse(is.na(tx$out[i]), 0, tx$out[i]) + ifelse(is.na(tx$inn[i]), 0, tx$inn[i]), 2)
    rows <- c(rows, atk_line(tx$date[i], tx$desc[i], c(atk_fmtm(tx$out[i]), atk_fmtm(tx$inn[i]), atk_fmtm(b))))
  }
  structure(c(top, "", ATK_HB, if (open) atk_line("", "Opening balance", c("", "", atk_fmtm(opening))), rows,
              if (close) atk_line("", "Closing balance", c("", "", atk_fmtm(b)))), closing = b)
}
ATK_MAY <- atk_tx(c("05 May", "12 May", "19 May", "30 May"),
                  c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "CREDIT INTEREST"),
                  c(14.20, NA, 268.15, NA), c(NA, 3120.00, NA, 2.40))

# ---- dates and periods ------------------------------------------------------------------------

test_that("r2 p09 p05 p06 p16: the labelled statement period wins over every other date on the page", {
  want <- atk_truth(ATK_MAY, 2025)
  tops <- list(
    rwt_notice = c("Kauri Bank", "Your RWT certificate for 1 Apr 2024 to 31 Mar 2025 is now available.",
                   "Statement period 1 May 2025 to 31 May 2025"),
    fixed_rate = c("Kauri Bank", "Fixed rate 5.49% p.a. for the period 12 Jan 2024 to 12 Jan 2026",
                   "Statement period 1 May 2025 to 31 May 2025"),
    account_opened = c("Kauri Bank", "Account opening date 12 Mar 2019", "Statement opening date 1 May 2025",
                       "Statement closing date 31 May 2025"),
    loan_start = c("Kauri Bank", "Loan start date 12 Mar 2019      Loan term 30 years", "Statement opening date 1 May 2025",
                   "Statement closing date 31 May 2025"),
    en_dash = c("Kauri Bank", "Statement period 1 May 2025 – 31 May 2025", "Our terms and conditions issued 1 Mar 2024 apply"),
    em_dash = c("Kauri Bank", "Statement period 1 May 2025 — 31 May 2025", "Duplicate statement   Date issued 3 Jun 2026"),
    dots = c("Kauri Bank", "Statement period 1 May 2025 ... 31 May 2025", "Duplicate statement   Date issued 3 Jun 2026"),
    month_above = c("Kauri Bank", "Your linked term deposit matures on 15 May 2026", "Statement period 1 May 2025 ... 31 May 2025"))
  for (nm in names(tops)) {
    rd <- auto_read(atk_pdf(atk_bal_page(ATK_MAY, 1500, tops[[nm]])))
    expect_right_or_not_auto(rd, want)
    # The balance proves every figure and the labelled period settles every year.
    expect_identical(rd$outcome, "proven", info = nm)
    expect_identical(rd$transactions$date[1], "2025-05-05", info = nm)
  }
  md <- extract_metadata(atk_pdf(tops$rwt_notice))
  expect_identical(c(md$period_start, md$period_end, md$period_source), c("1 May 2025", "31 May 2025", "labelled"))
  expect_identical(md$period_ranges, "1 May 2025 to 31 May 2025")
  # Two ranges, neither labelled, that do not join: the period is a guess.
  md2 <- extract_metadata(atk_pdf(c("Your RWT certificate for 1 Apr 2024 to 31 Mar 2025", "Transactions 1 May 2025 to 31 May 2025")))
  expect_true(md2$period_unsure)
  rd <- auto_read(atk_pdf(atk_bal_page(ATK_MAY, 1500, c("Kauri Bank", "Your RWT certificate for 1 Apr 2024 to 31 Mar 2025",
                                                        "Transactions 1 May 2025 to 31 May 2025"))))
  expect_right_or_not_auto(rd, want)
  expect_false(atk_ok(rd, "year_settled"))
})

test_that("r2 p14 p16: only the statement's own label dates it, and no pattern reads across a line break", {
  expect_true(.label_starts_phrase("Statement date 3 Apr 2026", "statement date"))
  expect_true(.label_starts_phrase("Your statement date: 3 Apr 2026", "statement date"))
  expect_true(.label_starts_phrase("Duplicate statement   Date issued 3 Feb 2027", "date issued"))
  expect_false(.label_starts_phrase("Updated terms and conditions issued 1 Jun 2025 apply", "issued"))
  expect_false(.label_starts_phrase("Visa Debit card ending 1234 issued 15 Apr 2025", "issued"))
  expect_false(.label_starts_phrase("Account opening date 12 Mar 2019", "opening date"))
  expect_true(.label_starts_phrase("Statement Opening date 1 Jan 26 Closing date 31 Jan 26", "closing date"))
  md <- extract_metadata(atk_pdf(c("Kauri Bank", "Updated terms and conditions issued 1 Jun 2025 apply to this account.",
                                   "Statement date 3 Apr 2026")))
  expect_identical(md$statement_date, "3 Apr 2026")
  expect_null(.month_period("Your term deposit matures on 15 January 2027\nStatement period 1 January 2026"))
  expect_null(.month_period("Rates changed in January\n2025 statement"))
  # p14 (layout path): a no-balance March statement with a notice above its date.
  L <- atk_pdf_layouts()
  for (top in list(c("Updated terms and conditions issued 1 Jun 2025 apply to this account.", "Statement date 3 Apr 2026"),
                   c("Visa Debit card ending 1234 issued 15 Apr 2025", "Printed 3 Apr 2026"))) {
    rd <- auto_read(atk_pdf(atk_nobal(ATK_TX, box = FALSE, period = "Transaction listing", extra_top = top)), layouts = L)
    expect_right_or_not_auto(rd, atk_truth(ATK_TX))
  }
})

test_that("r2 p01: a period longer than a year does not settle a day-and-month row's year", {
  tx <- atk_tx(c("06 Oct", "14 Oct", "20 Oct", "28 Oct"),
               c("DEPOSIT MATAI HOLDINGS", "EFTPOS RIVERSIDE DAIRY", "AP 0012 RENT", "CREDIT INTEREST"),
               c(NA, 45.10, 400.00, NA), c(1500.00, NA, NA, 0.85))
  rd <- auto_read(atk_pdf(atk_bal_page(tx, 12.34, c("Kauri Bank", "Statement period 1 Oct 2024 to 31 Oct 2025"))))
  expect_right_or_not_auto(rd, atk_truth(tx, 2025))
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "year_settled"))
  expect_true(atk_ok(rd, "balance_chain"))
  # The order can pin a year: a September row is only ever 2025 here, so an
  # October row after it is too -- but only a row read in that year is settled.
  per <- list(as.Date(c("2024-10-01", "2025-10-31")))
  expect_length(.ar_year_unpinned(as.Date(c("2025-09-15", "2025-10-06")), 1:2, per, "old"), 0L)
  expect_identical(.ar_year_unpinned(as.Date(c("2025-09-15", "2024-10-06")), 1:2, per, "old"), 2L)
  expect_identical(.ar_year_unpinned(as.Date(c("2024-10-06", "2024-10-14")), 1:2, per, "old"), 1:2)
  # A period of a month: one year each, nothing to settle.
  expect_length(.ar_year_unpinned(as.Date(c("2026-03-02", "2026-03-04")), 1:2, list(as.Date(c("2026-03-01", "2026-03-31"))), "old"), 0L)
})

# ---- nothing counted twice, one statement, the whole statement --------------------------------

ATK_P1 <- atk_tx(c("02 Mar", "04 Mar", "06 Mar"), c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES"),
                 c(12.40, NA, 268.15), c(NA, 3120, NA))
ATK_P2 <- atk_tx(c("10 Mar", "13 Mar", "17 Mar"), c("VISA HARBOUR FUEL", "EFTPOS COUNTDOWN", "TRANSFER FROM SAVINGS"),
                 c(88.10, 142.35, NA), c(NA, NA, 600))
ATK_P3 <- atk_tx(c("20 Mar", "26 Mar", "31 Mar"), c("AP RENT J SMITH", "EFTPOS PAK N SAVE", "CREDIT INTEREST"),
                 c(450, 96.72, NA), c(NA, NA, 2.18))
ATK_MARTOP <- c(paste0(atk_rpad("Kauri Bank", 35), ATK_MAR), "Account number 99-0001-0123410-00")

test_that("r2 p04 and the stress test's dup_page: a page or a run of rows printed twice is never counted twice", {
  # (vi) the same one-page statement twice in one file.
  one <- atk_bal_page(rbind(ATK_P1, ATK_P2), 1000, ATK_MARTOP)
  rd <- auto_read(atk_pdf(one, one))
  expect_right_or_not_auto(rd, atk_truth(rbind(ATK_P1, ATK_P2)))
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "rows_once"))
  # (iv) a page of carried balances in the file twice, its rows netting to zero.
  zero <- atk_tx(c("13 Mar", "13 Mar"), c("TRANSFER TO SAVINGS", "TRANSFER FROM SAVINGS"), c(600, NA), c(NA, 600))
  pg <- function(tx, op, first, last) {
    l <- atk_bal_page(tx, op, ATK_MARTOP, open = FALSE, close = FALSE)
    c(l[1:4], atk_line("", if (first) "Opening balance" else "Balance brought forward", c("", "", atk_fmtm(op))), l[-(1:4)],
      atk_line("", if (last) "Closing balance" else "Balance carried forward", c("", "", atk_fmtm(attr(l, "closing")))))
  }
  a <- pg(ATK_P1, 1000, TRUE, FALSE); c1 <- 1000 - 12.40 + 3120 - 268.15
  b <- pg(zero, c1, FALSE, FALSE); cc <- pg(ATK_P3, c1, FALSE, TRUE)
  rd <- auto_read(atk_pdf(a, b, b, cc))
  expect_right_or_not_auto(rd, atk_truth(rbind(ATK_P1, zero, ATK_P3)))
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "rows_once"))
  # (viii) a CSV export with opening and closing rows, pasted to itself.
  csv <- function() {
    tx <- rbind(ATK_P1, ATK_P2); b <- 1000; out <- c("Date,Description,Amount,Balance", "01/03/2026,Opening Balance,,1000.00")
    for (i in seq_len(nrow(tx))) {
      amt <- round(ifelse(is.na(tx$inn[i]), 0, tx$inn[i]) - ifelse(is.na(tx$out[i]), 0, tx$out[i]), 2); b <- round(b + amt, 2)
      out <- c(out, sprintf("%s/03/2026,%s,%.2f,%.2f", substr(tx$date[i], 1, 2), tx$desc[i], amt, b))
    }
    c(out, sprintf("31/03/2026,Closing Balance,,%.2f", b))
  }
  cc <- csv()
  rd <- auto_read(atk_csv(c(cc, cc[-1])))
  expect_right_or_not_auto(rd, atk_truth(rbind(ATK_P1, ATK_P2)))
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "rows_once"))
  # The control: the export once proves itself.
  ok <- auto_read(atk_csv(cc))
  expect_identical(ok$outcome, "proven")
  expect_right_or_not_auto(ok, atk_truth(rbind(ATK_P1, ATK_P2)))
  # Two identical coffees in a row are one repeated row, not a repeated run.
  expect_true(.ar_repeats(data.frame(date = c("2026-03-04", "2026-03-04", "2026-03-05", "2026-03-06"),
                                     description = c("CAFE", "CAFE", "DAIRY", "FUEL"), amount = c(-6.5, -6.5, -1, -2),
                                     balance = NA))$ok)
})

test_that("r2 p03 p04 and the stress test's bundle: a closing balance then a new opening balance is never one chain", {
  B <- atk_tx(c("12 Mar", "31 Mar"), c("TRANSFER FROM CHEQUE", "CREDIT INTEREST"), c(NA, NA), c(500, 4.10))
  A <- atk_bal_page(ATK_P1, 1000, c(paste0(atk_rpad("Kauri Bank", 35), ATK_MAR), "", "Kauri Everyday 99-0001-0123410-00"))
  Bp <- atk_bal_page(B, 8000, "Kauri Bonus Saver 99-0001-0123410-01")
  # (i) two accounts in one table; (iii) the second on its own page.
  for (doc in list(atk_pdf(c(A, Bp[-c(2, 3)])), atk_pdf(A, Bp))) {
    rd <- auto_read(doc)
    expect_right_or_not_auto(rd, atk_truth(ATK_P1))
    expect_false(atk_auto(rd))
    expect_false(atk_ok(rd, "one_statement"))
  }
  # Three consecutive statements of one account (each opens on the last one's
  # closing), and the same pages out of order: read whole, never one chain.
  s1 <- atk_bal_page(ATK_P1, 1000, ATK_MARTOP)
  s2 <- atk_bal_page(ATK_P2, attr(s1, "closing"), ATK_MARTOP)
  s3 <- atk_bal_page(ATK_P3, attr(s2, "closing"), ATK_MARTOP)
  for (doc in list(atk_pdf(s1, s2, s3), atk_pdf(s1, s3, s2))) {
    rd <- auto_read(doc)
    expect_right_or_not_auto(rd, atk_truth(rbind(ATK_P1, ATK_P2, ATK_P3)))
    expect_false(atk_auto(rd))
    expect_false(atk_ok(rd, "one_statement"))
  }
})

# A bundle of statements stays automatic only when they join up: each proven with
# both ends printed and, in date order, each opening on the previous closing. A
# statement missing from the middle (or another account's) breaks the join.
for (.join in c(TRUE, FALSE)) local({
  joined <- .join
  test_that(sprintf("r2 stress bundle: statements that %s", if (joined) "join up stay automatic" else "do not join go to a person"), {
  apr <- atk_tx(c("02 Apr", "09 Apr"), c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS"), c(20.00, NA), c(NA, 3120))
  s1 <- atk_bal_page(ATK_P1, 1000, c(ATK_MARTOP, ""))
  s2 <- atk_bal_page(apr, attr(s1, "closing") + if (joined) 0 else 250,
                     c(paste0(atk_rpad("Kauri Bank", 35), "Statement period 1 Apr 2026 to 30 Apr 2026"),
                       "Account number 99-0001-0123410-00"))
  input <- atk_pdf(c(s1, "", "Page 1 of 1"), c(s2, "", "Page 1 of 1"))
  expect_length(bundle_segments(input), 2L)
  real <- get("read_input", envir = globalenv())
  assign("read_input", function(path, ...) input, envir = globalenv())
  on.exit(assign("read_input", real, envir = globalenv()), add = TRUE)
  d <- tempfile("atk_bundle_"); dir.create(d)
  p <- file.path(d, "bundle.pdf"); writeLines("%PDF-1.4", p)
  r <- convert_statement(p, bank = "Kauri Bank", outdir = file.path(d, "out"), logdir = file.path(d, "logs"),
                         layouts_dir = file.path(d, "layouts"), tracking_dir = file.path(d, "tracking"),
                         requested_by = "tester", formats = "csv")
  expect_identical(vapply(r$reading, function(x) x$outcome, ""), c("proven", "proven"))
  if (joined) expect_identical(r$status, "ok")
  else { expect_identical(r$status, "needs_review"); expect_match(r$reason, "holds 2 statements", fixed = TRUE) }
  })
})

test_that("r2 p17: a statement whose end is not in the file is never proven complete", {
  all <- rbind(ATK_P1, ATK_P2, ATK_P3)
  full <- atk_bal_page(all, 1000, ATK_MARTOP)
  n <- length(full)
  # The file stops after the first page: an opening balance in the table, or in a
  # box above it, and no closing balance, totals or page numbers.
  p1 <- full[1:(5 + 6)]
  box <- c(ATK_MARTOP, paste0(atk_rpad("Opening balance", 20), "1,000.00"), "", ATK_HB, full[6:(5 + 6)])
  for (doc in list(atk_pdf(p1), atk_pdf(box))) {
    rd <- auto_read(doc)
    expect_right_or_not_auto(rd, atk_truth(all))
    expect_false(atk_auto(rd))
    expect_false(atk_ok(rd, "ends_printed"))
  }
  # The controls: both pages; or the page numbers say there is only one.
  rd <- auto_read(atk_pdf(p1, c(ATK_MARTOP, "", ATK_HB, full[12:n])))
  expect_identical(rd$outcome, "proven")
  expect_right_or_not_auto(rd, atk_truth(all))
  rd <- auto_read(atk_pdf(c(atk_bal_page(ATK_P1, 1000, ATK_MARTOP, close = FALSE), "", "Page 1 of 1")))
  expect_identical(rd$outcome, "proven")
  expect_true(atk_ok(rd, "ends_printed"))
  # The other end: the file starts at the statement's second page (its rows still
  # chain to the closing balance), or a statement listed newest first has lost its
  # last page -- its oldest rows and its opening balance.
  rd <- auto_read(atk_pdf(c(ATK_MARTOP, "", ATK_HB, full[12:n])))
  expect_right_or_not_auto(rd, atk_truth(all))
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "ends_printed"))
  newest <- c(ATK_MARTOP, "", ATK_HB, atk_line("", "Closing balance", c("", "", atk_fmtm(attr(full, "closing")))),
              rev(full[6:(n - 1)])[1:5])
  rd <- auto_read(atk_pdf(newest))
  expect_right_or_not_auto(rd, atk_truth(all[nrow(all):1, ]))
  expect_false(atk_auto(rd))
})

# ---- this statement's own facts ---------------------------------------------------------------

atk_signed_page <- function(tx, opening, top, side = character(0), foot = character(0)) {
  sl <- function(d, s, a, b) paste0(atk_rpad(d, 9), atk_rpad(s, 35), atk_lpad(a, 14), atk_lpad(b, 14))
  sf <- function(x) ifelse(x < 0, paste0("-", atk_fmtm(-x)), atk_fmtm(x))
  b <- opening; rows <- character(0)
  for (i in seq_len(nrow(tx))) { b <- round(b + tx$amt[i], 2); rows <- c(rows, sl(tx$date[i], tx$desc[i], sf(tx$amt[i]), sf(b))) }
  c(top, "", side, if (length(side)) "", paste0(atk_rpad("Date", 9), atk_rpad("Details", 35), atk_lpad("Amount", 14), atk_lpad("Balance", 14)),
    sl("", "Opening balance", "", sf(opening)), rows, sl("", "Closing balance", "", sf(b)), foot)
}

test_that("r2 p02: card wording counts only where it describes this account", {
  tx <- data.frame(date = c("02 Mar", "06 Mar", "09 Mar", "16 Mar", "23 Mar", "30 Mar"),
                   desc = c("D/C MATAI HOLDINGS LTD", "DEBIT COUNTDOWN ONEHUNGA", "A/P 0012 J SMITH RENT", "DEBIT Z ENERGY PENROSE",
                            "TFR 99-0001-0555123-01", "DEBIT SPARK NZ"),
                   amt = c(3120.00, -142.35, -450.00, -88.10, -500.00, -79.99), stringsAsFactors = FALSE)
  want <- data.frame(date = sprintf("2026-03-%s", substr(tx$date, 1, 2)), amount = tx$amt, stringsAsFactors = FALSE)
  top <- c(paste0(atk_rpad("Kauri Bank", 35), ATK_MAR), "Kauri Everyday Account  99-0001-0123410-00")
  side <- c("Your other accounts with us", "Kauri Visa Platinum   Credit limit 8,000.00   Available credit 6,512.40",
            "Minimum payment 25.00   Payment due 18 Apr 2026")
  foot <- c("", "", "Kauri Visa: credit limit up to 10,000.00. Minimum payment just 3% each month.",
            "Available credit shown on your card statement. Payment due 25 days after statement date.")
  for (doc in list(atk_pdf(atk_signed_page(tx, 1200, top, side = side)), atk_pdf(atk_signed_page(tx, 1200, top, foot = foot)),
                   atk_pdf(atk_signed_page(tx, 1200, top), c("Important information", "", "If you hold a Kauri credit card,",
                           "your minimum payment and payment due date are on your card statement.")))) {
    rd <- auto_read(doc)
    expect_right_or_not_auto(rd, want)
  }
  # The control: a card's own statement -- its title, its card number, its own
  # summary -- still reads as a card: purchases are money out.
  card <- data.frame(date = c("02 Mar", "06 Mar", "12 Mar", "20 Mar"),
                     desc = c("COUNTDOWN KILBIRNIE", "AIR NZ ONLINE", "PAYMENT RECEIVED THANK YOU", "Z ENERGY PETONE"),
                     amt = c(84.20, 312.00, -500.00, 96.40), stringsAsFactors = FALSE)
  ctop <- c(paste0(atk_rpad("Kauri Bank", 35), "Credit card statement"), "Card number 4987 **** **** 1234", ATK_MAR, "",
            "Credit limit 8,000.00   Available credit 6,512.40", "Minimum payment 25.00   Payment due 18 Apr 2026")
  rd <- auto_read(atk_pdf(atk_signed_page(card, 1487.60, ctop)))
  expect_identical(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), -card$amt)
})

test_that("r2 p11: a row with figures in both money out and money in is never automatic", {
  tx <- atk_tx(c("03 Mar", "04 Mar", "17 Mar", "18 Mar"),
               c("D/C MATAI HOLDINGS LTD", "SWEEP TO 99-0001-0123410-00", "D/C HARBOUR TRADING", "SWEEP TO 99-0001-0123410-00"),
               c(NA, 3120, NA, 845.50), c(3120, NA, 845.50, NA))
  l <- atk_bal_page(tx, 0, ATK_MARTOP)
  l <- append(l, atk_line("", "Turnover", c("3,965.50", "3,965.50", "0.00")), after = length(l) - 1L)
  rd <- auto_read(atk_pdf(l))
  expect_right_or_not_auto(rd, atk_truth(tx))
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "one_side_per_row"))
  # layout path (p16 iii): a day-total line printing both columns' totals.
  L <- atk_pdf_layouts()
  ln <- atk_nobal(ATK_TX, box = FALSE)
  ln <- append(ln, atk_line("", "Day total", c("18.50", "3,120.00")), after = which(grepl("SALARY", ln)))
  rd <- auto_read(atk_pdf(ln), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(ATK_TX))
  expect_false(atk_auto(rd))
})

test_that("r2 p14: a line printed above the in-table opening balance is not one of the statement's rows", {
  for (held in list(atk_line("01 Mar", "UNCLEARED CHEQUE DEPOSIT", c("", "500.00", "")),
                    atk_line("01 Mar", "CARD HOLD - GRAND HOTEL", c("200.00", "", "")))) {
    l <- atk_bal_page(ATK_P1, 1000, ATK_MARTOP)
    l <- append(l, held, after = which(grepl("^Date", l)))
    rd <- auto_read(atk_pdf(l))
    expect_right_or_not_auto(rd, atk_truth(ATK_P1))
    expect_false(atk_auto(rd))
    expect_false(atk_ok(rd, "rows_between_ends"))
  }
})

test_that("r2 p12: a titled pending section is set aside, and the rest must still add up", {
  pend <- c("", "Pending transactions (not yet processed)",
            atk_line("29 Mar", "VISA GRAND HOTEL PRE-AUTH", c("200.00", "", "")),
            atk_line("30 Mar", "VISA GRAND HOTEL PRE-AUTH RELEASED", c("", "200.00", "")))
  box <- function(tx) c(paste0(atk_rpad("Opening balance", 20), atk_fmtm(1000)),
                        paste0(atk_rpad("Closing balance", 20), atk_fmtm(round(1000 - sum(tx$out, na.rm = TRUE) + sum(tx$inn, na.rm = TRUE), 2))), "")
  l <- atk_bal_page(ATK_P1, 1000, c(ATK_MARTOP, "", box(ATK_P1)), open = FALSE, close = FALSE)
  for (doc in list(c(l, pend), c(atk_nobal(ATK_P1, period = ATK_MAR), pend))) {
    rd <- auto_read(atk_pdf(doc))
    expect_right_or_not_auto(rd, atk_truth(ATK_P1))
    # The pending pair cancels: set aside, the four posted rows prove themselves.
    expect_identical(rd$outcome, "proven")
    expect_identical(nrow(rd$transactions), 3L)
    expect_true(atk_ok(rd, "sections_set_aside"))
  }
  # Set aside and the balances no longer add up: a person looks.
  l2 <- atk_bal_page(ATK_P1, 1000, c(ATK_MARTOP, "", box(rbind(ATK_P1, atk_tx("29 Mar", "X", 200, NA)))), open = FALSE, close = FALSE)
  rd <- auto_read(atk_pdf(c(l2, "", "Pending transactions", atk_line("29 Mar", "VISA GRAND HOTEL", c("200.00", "", "")))))
  expect_false(atk_auto(rd))
})

test_that("r2 stress col_swap: of two date columns the transaction date wins by its heading, never by place", {
  # A card statement (opening and closing in a box, one Amount column) printing
  # the transaction date and the processed date.
  tr <- c("17 Mar", "18 Mar", "22 Mar", "25 Mar"); pr <- c("19 Mar", "19 Mar", "24 Mar", "26 Mar")
  desc <- c("THE DAILY GRIND", "WEBSHOP INTL", "PAYMENT RECEIVED THANK YOU", "TOTARA FUELS")
  amt <- c("15.50", "154.21", "845.04 CR", "65.98")
  build <- function(h1, h2, d1, d2) {
    hl <- paste0(atk_rpad(h1, 12), atk_rpad(h2, 12), atk_rpad("Details", 35), atk_lpad("Amount", 14))
    rows <- paste0(atk_rpad(d1, 12), atk_rpad(d2, 12), atk_rpad(desc, 35), atk_lpad(amt, 14))
    c(paste0(atk_rpad("Kauri Bank", 35), "Credit card statement"), "Card number 4987 **** **** 1234",
      "Statement period 15 Mar 2026 to 14 Apr 2026", "", paste0(atk_rpad("Opening balance", 20), "3,091.55"),
      paste0(atk_rpad("Closing balance", 20), "2,482.20"), "", hl, rows)
  }
  want <- data.frame(date = format(as.Date(paste(tr, "2026"), "%d %b %Y")), amount = c(-15.50, -154.21, 845.04, -65.98),
                     stringsAsFactors = FALSE)
  for (doc in list(build("Transaction", "Processed", tr, pr), build("Processed", "Transaction", pr, tr),
                   build("Date", "Processed", tr, pr), build("Processed", "Date", pr, tr))) {
    rd <- auto_read(atk_pdf(doc))
    expect_identical(rd$outcome, "proven")
    expect_right_or_not_auto(rd, want)
  }
  # No heading says which, and the two columns give different dates: a person reads them.
  rd <- auto_read(atk_pdf(build("Date", "Date", pr, tr)))
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "dates_settled"))
})

# ---- exports with nothing to add up (layout_match) --------------------------------------------

# Three proven teaching CSVs (two accounts), with the columns `header` names and
# each row written by `row`.
atk_csv_layouts2 <- function(header, row) atk_learn(lapply(1:3, function(k) {
  tx <- atk_teach_tx(k); amt <- ifelse(is.na(tx$inn), -tx$out, tx$inn)
  d <- format(as.Date(paste(tx$date, "2026"), "%d %b %Y"), "%d/%m/%Y"); op <- 1000 + 10 * k
  atk_csv(c(paste("Account:", atk_acct(k)), sprintf("Opening balance: %.2f", op), header, row(d, tx$desc, amt, k),
            sprintf("Closing balance: %.2f", round(op + sum(amt), 2))))
}))
ATK_CSV_A <- data.frame(d = c("02/03/2026", "04/03/2026", "10/03/2026", "17/03/2026", "25/03/2026"),
                        desc = c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES", "VISA HARBOUR FUEL",
                                 "TRANSFER TO SAVINGS"), amt = c(-18.50, 3120.00, -268.15, -20.00, -500.00), stringsAsFactors = FALSE)
atk_csv_want <- function(x = ATK_CSV_A) data.frame(date = format(as.Date(x$d, "%d/%m/%Y")), amount = x$amt, stringsAsFactors = FALSE)

test_that("r2 c03 c04 c06: a status, currency or account column holding what the layout never saw goes to a person", {
  # c04: a Status column the layout learned as "Posted".
  L <- atk_csv_layouts2("Date,Details,Amount,Status", function(d, s, a, k) sprintf("%s,%s,%.2f,Posted", d, s, a))
  expect_identical(as.character(unlist(L[[1]]$layout$signature$col_values))[4], "cat:posted")
  ok <- auto_read(atk_csv(c("Date,Details,Amount,Status", sprintf("%s,%s,%.2f,Posted", ATK_CSV_A$d, ATK_CSV_A$desc, ATK_CSV_A$amt))), layouts = L)
  expect_identical(ok$outcome, "layout_match")
  for (st in list(c(rep("Posted", 4), "Pending"), c(rep("Cleared", 4), "Uncleared"), c(rep("", 4), "PENDING"))) {
    rd <- auto_read(atk_csv(c("Date,Details,Amount,Status", sprintf("%s,%s,%.2f,%s", ATK_CSV_A$d, ATK_CSV_A$desc, ATK_CSV_A$amt, st))),
                    layouts = L)
    expect_right_or_not_auto(rd, atk_csv_want(ATK_CSV_A[1:4, ]))
    expect_false(atk_auto(rd))
  }
  # c04 iv: no Status column, the row's own wording says it is pending.
  Lp <- atk_csv_layouts()
  d4 <- ATK_CSV_A$desc; d4[5] <- paste("PENDING -", d4[5])
  rd <- auto_read(atk_csv(c("Date,Details,Amount", sprintf("%s,%s,%.2f", ATK_CSV_A$d, d4, ATK_CSV_A$amt))), layouts = Lp)
  expect_false(atk_auto(rd))
  # c06: a Currency column learned as NZD; a USD export, and a mixed one.
  Lc <- atk_csv_layouts2("Date,Details,Amount,Currency", function(d, s, a, k) sprintf("%s,%s,%.2f,NZD", d, s, a))
  for (cur in list(rep("USD", 5), c("NZD", "NZD", "AUD", "NZD", "AUD"))) {
    rd <- auto_read(atk_csv(c("Date,Details,Amount,Currency", sprintf("%s,%s,%.2f,%s", ATK_CSV_A$d, ATK_CSV_A$desc, ATK_CSV_A$amt, cur))),
                    layouts = Lc)
    expect_false(atk_auto(rd))
  }
  # c03: an Account column -- the layout keeps how many accounts, never the numbers.
  La <- atk_csv_layouts2("Account,Date,Details,Amount", function(d, s, a, k) sprintf("%s,%s,%s,%.2f", atk_acct(k), d, s, a))
  expect_identical(as.character(unlist(La[[1]]$layout$signature$col_values))[1], "acct:1")
  f <- list.files(attr(La, "dir"), "[.]yaml$", recursive = TRUE, full.names = TRUE)
  expect_false(any(grepl("0123410|0123411", unlist(lapply(f, readLines)))))
  acc <- c(rep("99-0001-0123410-00", 5), "99-0001-0123410-01")
  x <- rbind(ATK_CSV_A, data.frame(d = "31/03/2026", desc = "CREDIT INTEREST", amt = 3.12))
  rd <- auto_read(atk_csv(c("Account,Date,Details,Amount", sprintf("%s,%s,%s,%.2f", acc, x$d, x$desc, x$amt))), layouts = La)
  expect_right_or_not_auto(rd, atk_csv_want())
  expect_false(atk_auto(rd))
  ok <- auto_read(atk_csv(c("Account,Date,Details,Amount", sprintf("%s,%s,%s,%.2f", acc[1], ATK_CSV_A$d, ATK_CSV_A$desc, ATK_CSV_A$amt))),
                  layouts = La)
  expect_identical(ok$outcome, "layout_match")
})

test_that("r2 c06 v: a statement whose own title names another currency is never put out as NZD", {
  L <- atk_pdf_layouts()
  tx <- atk_tx(c("02 Mar", "04 Mar", "10 Mar", "17 Mar", "25 Mar"),
               c("TFR FROM NZD ACCOUNT", "INWARD TT HARBOUR LTD", "OUTWARD TT SUPPLIER", "SERVICE FEE", "CREDIT INTEREST"),
               c(NA, NA, 1200.00, 15.00, NA), c(500.00, 2400.00, NA, NA, 1.10))
  rd <- auto_read(atk_pdf(atk_nobal(tx, box = FALSE, top = "Kauri Bank USD Call Account", account = "99-0001-0123499-30 (USD)")),
                  layouts = L)
  expect_false(atk_auto(rd))
  expect_false(atk_ok(rd, "currency_own"))
})

test_that("r2 c05: a last row worded as a total or a balance is accounted for, or a person looks", {
  L <- atk_csv_layouts()
  base <- c("Date,Details,Amount", sprintf("%s,%s,%.2f", ATK_CSV_A$d, ATK_CSV_A$desc, ATK_CSV_A$amt))
  for (tail_row in c("31/03/2026,Net movement for period,2313.35", "31/03/2026,Interest earned year to date,45.10",
                     "31/03/2026,Closing Balance as at 31/03/2026,1313.35")) {
    rd <- auto_read(atk_csv(c(base, tail_row)), layouts = L)
    expect_right_or_not_auto(rd, atk_csv_want())
    expect_false(atk_auto(rd), info = tail_row)
  }
  expect_identical(auto_read(atk_csv(base), layouts = L)$outcome, "layout_match")
})

test_that("r2 c08 c10: a workbook with a second sheet of dated rows, or hidden rows, goes to a person", {
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  d <- tempfile("atk_xlsx_"); dir.create(d)
  rows <- function(x) rbind(c("Date", "Details", "Amount"), cbind(x$d, x$desc, sprintf("%.2f", x$amt)))
  sheet <- function(x) rbind(c("Opening balance: 1000.00", "", ""), rows(x),
                             c(sprintf("Closing balance: %.2f", round(1000 + sum(x$amt), 2)), "", ""))
  book <- function(name, sheets, hide = integer(0), hidden_sheet = NULL) {
    wb <- openxlsx::createWorkbook()
    for (nm in names(sheets)) { openxlsx::addWorksheet(wb, nm); openxlsx::writeData(wb, nm, sheets[[nm]], colNames = FALSE) }
    if (length(hide)) openxlsx::setRowHeights(wb, 1, rows = hide, heights = 0)
    if (!is.null(hidden_sheet)) openxlsx::sheetVisibility(wb)[match(hidden_sheet, names(sheets))] <- "hidden"
    p <- file.path(d, name); openxlsx::saveWorkbook(wb, p, overwrite = TRUE); p
  }
  pend <- data.frame(d = c("27/03/2026", "28/03/2026"), desc = c("EFTPOS HARBOUR CAFE", "VISA DEBIT HOTEL"), amt = c(-6.50, -250.00))
  one <- read_input(book("one.xlsx", list(Transactions = sheet(ATK_CSV_A))))
  expect_identical(one$meta$dated_sheets, 1L); expect_identical(one$meta$hidden_rows, 0L)
  rd <- auto_read(one)
  expect_identical(rd$outcome, "proven")
  for (p in c(book("two.xlsx", list(Transactions = sheet(ATK_CSV_A), Pending = rows(pend))),
              book("hidden_sheet.xlsx", list(Transactions = sheet(ATK_CSV_A), Data = rows(pend)), hidden_sheet = "Data"))) {
    rd <- auto_read(read_input(p))
    expect_right_or_not_auto(rd, atk_csv_want())
    expect_false(atk_auto(rd))
    expect_false(atk_ok(rd, "workbook_plain"))
  }
  # Hidden rows: two declined items kept in the sheet but not shown.
  x <- rbind(ATK_CSV_A[1:3, ], data.frame(d = c("12/03/2026", "20/03/2026"), desc = c("DECLINED VISA DEBIT", "DECLINED EFTPOS"),
                                          amt = c(-310, -45.99)), ATK_CSV_A[4:5, ])
  p <- book("hidden_rows.xlsx", list(Transactions = rbind(rows(x))), hide = c(5, 6))
  inp <- read_input(p)
  expect_identical(inp$meta$hidden_rows, 2L)
  rd <- auto_read(inp, layouts = list())
  expect_false(atk_auto(rd))
})

test_that("r2 p16 (layout): a row borrowing the date above is never automatic, whatever the layout learned", {
  carry <- function(tx, ...) {
    l <- atk_nobal(tx, ...)
    for (i in seq_len(nrow(tx))[-1]) if (tx$date[i] == tx$date[i - 1]) {
      k <- which(startsWith(l, tx$date[i]) & grepl(tx$desc[i], l, fixed = TRUE))[1]
      substr(l[k], 1, 6) <- "      "
    }
    l
  }
  L <- atk_learn(lapply(1:3, function(k) {
    tx <- atk_teach_tx(k); tx$date <- c("03 Feb", "05 Feb", "05 Feb", "14 Feb", "21 Feb", "21 Feb")
    atk_pdf(carry(tx, period = ATK_FEB, account = atk_acct(k)))
  }))
  tx <- atk_tx(c("02 Mar", "04 Mar", "04 Mar", "10 Mar"), c("EFTPOS RIVERSIDE DAIRY", "AMAZON MARKETPLACE", "SALARY MATAI HOLDINGS",
                                                            "CREDIT INTEREST"), c(18.50, 41.20, NA, NA), c(NA, NA, 3120, 4.10))
  ln <- carry(tx, box = FALSE)
  ln <- append(ln, atk_line("", "  FOREIGN AMOUNT USD", c("25.00", "")), after = which(grepl("AMAZON", ln)))
  rd <- auto_read(atk_pdf(ln), layouts = L)
  expect_right_or_not_auto(rd, atk_truth(tx))
  expect_false(atk_auto(rd))
})

test_that("r2 p11 p12 (layout): a second table set apart by other print is never merged into the statement's", {
  L <- atk_pdf_layouts()
  tx <- ATK_TX[1:4, ]
  B <- atk_tx(c("25 Mar", "31 Mar"), c("TRANSFER FROM CHEQUE", "CREDIT INTEREST"), c(NA, NA), c(500, 3.12))
  rowsB <- vapply(seq_len(nrow(B)), function(i) atk_line(B$date[i], B$desc[i], c("", atk_fmtm(B$inn[i]))), "")
  pA <- atk_nobal(tx, box = FALSE, account = "99-0001-0123499-00")
  top <- paste0(atk_rpad("Kauri Bank", 35), ATK_MAR)
  docs <- list(
    other_account = atk_pdf(pA, c(top, "Account number 99-0001-0123499-01", "Online Saver", "", atk_heads(ATK_H), rowsB)),
    other_account_no_heading = atk_pdf(pA, c(top, "Account number 99-0001-0123499-01", "Online Saver", "", rowsB)),
    recent_activity = atk_pdf(pA, c(top, "", "Recent activity", "", atk_heads(ATK_H),
                                    atk_line("01 Apr", "EFTPOS HARBOUR CAFE", c("6.50", "")))))
  for (nm in names(docs)) {
    rd <- auto_read(docs[[nm]], layouts = L)
    expect_right_or_not_auto(rd, atk_truth(tx))
    expect_false(atk_auto(rd), info = nm)
  }
  # The control: the same statement carried on to page 2 under its masthead and
  # heading row again still matches the layout.
  p2 <- atk_nobal(ATK_TX[4:5, ], box = FALSE)
  rd <- auto_read(atk_pdf(atk_nobal(ATK_TX[1:3, ], box = FALSE), p2), layouts = L)
  expect_identical(rd$outcome, "layout_match")
  expect_right_or_not_auto(rd, atk_truth(ATK_TX))
})
