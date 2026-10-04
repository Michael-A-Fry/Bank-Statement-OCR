# test-blocks.R -- a statement pack read a table at a time (R/auto_read_blocks.R).
#
# A pack prints other tables beside the statement's own: a cover page with an
# account summary and a fee list, a pending section, another account's
# mini-statement, a back page with a rate table. Read as one table nothing adds up;
# read a table at a time, the statement's own table proves on its own, and the
# other account is returned apart and labelled -- never mixed into the rows.
#
# Pages are lines of text: one word box per word, 5pt a character, 12pt a line.

blk_pdf <- function(...) {
  pages <- list(...)
  words <- lapply(pages, function(lines) do.call(rbind, lapply(seq_along(lines), function(i) {
    m <- gregexpr("\\S+", lines[i])[[1]]
    if (m[1] < 0) return(NULL)
    tx <- regmatches(lines[i], list(m))[[1]]
    data.frame(width = nchar(tx) * 5, height = 8, x = 20 + (as.numeric(m) - 1) * 5, y = 20 + i * 12,
               space = TRUE, text = tx, stringsAsFactors = FALSE)
  })))
  list(kind = "pdf", path = "", sha256 = NA_character_, pages = vapply(pages, paste, "", collapse = "\n"),
       words = words, page_width = rep(600, length(pages)), page_height = rep(800, length(pages)),
       page_ocr = rep(FALSE, length(pages)), meta = list())
}

blk_own <- nz_test_account()
blk_oth <- nz_test_account(suffix = "01")

# Page 1: the cover -- an account summary (two figures a line), a fee list (one
# figure a line on one right edge, with card wording in it) and a pending section.
blk_cover <- c(
  sprintf("Kauri Bank                                   Account number %s", blk_own),
  "Everyday account statement                   Statement period 1 Feb 2026 to 28 Feb 2026", "",
  "Your accounts at a glance", "",
  "Account                      Opening         Closing",
  "Everyday                    1,000.00        3,344.91",
  "Bonus Saver                 5,000.00        5,312.50", "", "",
  "Standard fees", "",
  "Account maintenance fee                         5.00",
  "Overdraft fee                                  15.00",
  "Cash advance (credit card)                      2.50",
  "Unarranged overdraft                           10.00", "", "",
  "Pending transactions", "",
  "27 Feb   EFTPOS HARBOUR CAFE                    8.50")
# Page 2: the statement's own table, opened and closed in its own words.
blk_head <- c(sprintf("Kauri Bank   Everyday   %s", blk_own), "",
              "Date     Details                          Withdrawals     Deposits      Balance")
blk_rows <- c(
  "03 Feb   EFTPOS RIVERSIDE DAIRY                      12.40                    987.60",
  "05 Feb   SALARY MATAI HOLDINGS                                  3,120.00    4,107.60",
  "09 Feb   DD CITY COUNCIL RATES                      268.15                  3,839.45",
  "14 Feb   TRANSFER TO SAVINGS                        400.00                  3,439.45",
  "21 Feb   VISA HARBOUR FUEL                           96.72                  3,342.73",
  "26 Feb   CREDIT INTEREST                                            2.18    3,344.91")
blk_open  <- "         Opening balance                                                  1,000.00"
blk_close <- "         Closing balance                                                  3,344.91"
blk_stmt <- c(blk_head, blk_open, blk_rows, blk_close)
blk_want <- c(-12.40, 3120.00, -268.15, -400.00, -96.72, 2.18)
# Page 3: another account's mini-statement, in columns of its own.
blk_other <- c(
  sprintf("Bonus Saver %s - shown for your information", blk_oth), "",
  "Date      Details                     Paid out     Paid in       Balance",
  "          Opening balance                                       5,000.00",
  "10 Feb    INTEREST                                  12.50       5,012.50",
  "14 Feb    TRANSFER FROM EVERYDAY                   400.00       5,412.50",
  "20 Feb    ATM WITHDRAWAL                100.00                  5,312.50",
  "          Closing balance                                       5,312.50")
blk_other_want <- c(12.50, 400.00, -100.00)
# Page 4: the back page -- terms, and a rate table (two figures a line).
blk_back <- c("Important information", "",
              "Please check this statement and tell us about anything that looks wrong.", "",
              "Term deposit rates            6 months     12 months",
              "Under $10,000                     4.10          4.35",
              "$10,000 and over                  4.50          4.75")

blk_auto <- function(rd) rd$outcome %in% c("proven", "layout_match")
ok_blk <- function(rd, name) isTRUE(rd$checks$ok[rd$checks$check == name])

# blk_pdf_file(...) -- the same pages written as a real PDF (a monospaced font, each
# word drawn at its column: 5pt a character, 12pt a line), for convert_statement.
blk_pdf_file <- function(...) {
  p <- tempfile("pack_", fileext = ".pdf")
  grDevices::cairo_pdf(p, width = 8.27, height = 11.69, onefile = TRUE, family = "DejaVu Sans Mono")
  for (lines in list(...)) {
    graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    graphics::plot.new(); graphics::plot.window(xlim = c(0, 595), ylim = c(842, 0))
    for (i in seq_along(lines)) {
      m <- gregexpr("\\S+", lines[i])[[1]]
      if (m[1] < 0) next
      tx <- regmatches(lines[i], list(m))[[1]]
      for (k in seq_along(tx)) graphics::text(20 + (m[k] - 1) * 5, 40 + i * 12, tx[k], adj = c(0, 0),
                                              cex = 8.33 / 12, family = "DejaVu Sans Mono")
    }
  }
  grDevices::dev.off()
  p
}

test_that("a pack read a table at a time: the statement's own rows, right, and nothing else", {
  rd <- auto_read(blk_pdf(blk_cover, blk_stmt, blk_other, blk_back))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), blk_want)
  expect_equal(as.character(rd$transactions$date),
               c("2026-02-03", "2026-02-05", "2026-02-09", "2026-02-14", "2026-02-21", "2026-02-26"))
  # It is the table-at-a-time reading that proves: read whole, nothing adds up.
  expect_false(rd$candidates$passed[rd$candidates$source == "content"])
  expect_true(rd$candidates$passed[rd$candidates$source == "repair:tables_apart"])
  expect_true(ok_blk(rd, "tables_set_aside"))
  # Never a row of another table: not the other account's, not the pending item,
  # not a fee, a rate or a summary figure.
  amt <- abs(round(rd$transactions$amount, 2))
  expect_false(any(c(8.50, 100.00, 12.50, 5.00, 15.00, 2.50, 10.00, 4.10, 4.75) %in% amt))
  # The fee list's "Cash advance (credit card)" is not this account's wording.
  expect_false(isTRUE(rd$template$auto$liab))
})

test_that("another account's table is returned apart, labelled, and proven on its own", {
  rd <- auto_read(blk_pdf(blk_cover, blk_stmt, blk_other, blk_back))
  expect_length(rd$other_accounts, 1L)
  o <- rd$other_accounts[[1]]
  expect_equal(o$account, blk_oth)
  expect_equal(round(o$tx$amount, 2), blk_other_want)
  expect_equal(o$proof$kind, "chain")
  # None of its rows is among the statement's, and none of the statement's among its.
  k <- function(tx) paste(tx$date, sprintf("%.2f", tx$amount))
  expect_length(intersect(k(o$tx), k(rd$transactions)), 0L)
})

test_that("on the same page, another account's table is still kept apart", {
  rd <- auto_read(blk_pdf(blk_cover, c(blk_stmt, "", "", blk_other), blk_back))
  expect_equal(rd$outcome, "proven")
  expect_equal(round(rd$transactions$amount, 2), blk_want)
  expect_equal(vapply(rd$other_accounts, function(o) o$account, ""), blk_oth)
})

test_that("convert writes the other account to its own output, never into the statement's", {
  cv <- convert_sandbox()
  p <- blk_pdf_file(blk_cover, blk_stmt, blk_other, blk_back)
  res <- cv(p, bank = "Kauri Bank", formats = c("csv", "json", "xlsx"))
  expect_equal(res$status, "ok")
  csv <- utils::read.csv(res$outputs[["csv"]], colClasses = "character")
  expect_equal(round(as.numeric(csv$amount), 2), blk_want)
  js <- jsonlite::fromJSON(res$outputs[["json"]], simplifyVector = TRUE)
  expect_equal(round(js$transactions$amount, 2), blk_want)
  expect_equal(js$other_accounts$account, blk_oth)
  expect_equal(round(js$other_accounts$transactions[[1]]$amount, 2), blk_other_want)
  expect_true("Other accounts" %in% openxlsx::getSheetNames(res$outputs[["xlsx"]]))
  oa <- openxlsx::read.xlsx(res$outputs[["xlsx"]], "Other accounts")
  expect_equal(unique(oa$account), blk_oth)
  expect_equal(round(as.numeric(openxlsx::read.xlsx(res$outputs[["xlsx"]], "Transactions")$amount), 2), blk_want)
  expect_true(any(grepl("other account table", res$messages, fixed = TRUE)))
  expect_length(res$other_accounts, 1L)
  # The feed takes the statement's rows only.
  expect_equal(round(as.numeric(res$feed_rows$amount), 2), blk_want)
})

test_that("a table set aside is allowed only when the statement's own opening and closing confirm the rows", {
  # The same pack, but the statement prints no opening or closing balance of its
  # own; the page numbers show every page is there, so its end is not in question.
  pg <- function(lines, k) c(lines, "", sprintf("Page %d of 4", k))
  rd <- auto_read(blk_pdf(pg(blk_cover, 1), pg(c(blk_head, blk_rows), 2), pg(blk_other, 3), pg(blk_back, 4)))
  expect_false(blk_auto(rd))
  ctx <- .ar_pdf_context(blk_pdf(pg(blk_cover, 1), pg(c(blk_head, blk_rows), 2), pg(blk_other, 3), pg(blk_back, 4)))
  blk <- .ar_block_reading(ctx, .ar_pdf_pages(ctx))
  expect_null(blk$pick)
  expect_length(blk$others, 0L)
})

test_that("another account printed in the statement's own columns sends the file to a person", {
  # A second section in the same columns, titled with another account's number:
  # one statement of several accounts (all its rows are the statement's), or
  # another account's activity printed for information (none is). Nothing says
  # which, so neither reading is automatic.
  sect <- c("", sprintf("Bonus Saver   %s", blk_oth), "",
            "Date     Details                          Withdrawals     Deposits      Balance",
            "         Opening balance                                                  5,000.00",
            "10 Feb   INTEREST                                                  12.50    5,012.50",
            "14 Feb   TRANSFER FROM EVERYDAY                                   400.00    5,412.50",
            "20 Feb   ATM WITHDRAWAL                             100.00                  5,312.50",
            "         Closing balance                                                  5,312.50")
  rd <- auto_read(blk_pdf(blk_cover, c(blk_stmt, sect), blk_back))
  expect_false(blk_auto(rd))
  expect_length(rd$other_accounts, 0L)
  expect_true(any(grepl("statement of several accounts", rd$notes, fixed = TRUE)))
})

test_that("a statement page printed further over is still the statement's: never a statement cut at a carried balance", {
  # Page 2 of the statement sits 30pt further right, with the balance carried from
  # page 1, and page numbers on every page. Read a table at a time, its rows must
  # either all be read or the file go to a person: page 1 alone, closed by its
  # carried-forward line, is never the statement.
  cf <- "         Balance carried forward                                          3,839.45"
  bf <- "         Balance brought forward                                          3,839.45"
  s1 <- c(blk_head, blk_open, blk_rows[1:3], cf)
  pg <- function(lines, k) c(lines, "", sprintf("Page %d of 5", k))
  for (head2 in list(blk_head[3], character(0))) {   # page 2 with and without its heading row
    s2 <- paste0("      ", c(head2, bf, blk_rows[4:6], blk_close))
    rd <- auto_read(blk_pdf(pg(blk_cover, 1), pg(s1, 2), pg(s2, 3), pg(blk_other, 4), pg(blk_back, 5)))
    if (blk_auto(rd)) expect_equal(round(rd$transactions$amount, 2), blk_want)
    else expect_true(rd$outcome %in% c("check", "unread"))
  }
  # The same with the page 2 block on its own: a carried balance never closes it.
  ctx <- .ar_pdf_context(blk_pdf(pg(blk_cover, 1), pg(s1, 2), pg(blk_other, 3)))
  blk <- .ar_block_reading(ctx, .ar_pdf_pages(ctx))
  expect_null(blk$pick)
})

test_that("a statement whose first page is printed in other columns is never read from its second page alone", {
  # Page 1 of the statement prints an extra Fee column, so it is not the same
  # table as page 2; page 2 opens on the balance page 1 carried forward and has
  # more rows. Page 2 read alone adds up from that brought-forward balance to the
  # closing balance -- but that balance is printed in the table set aside, which is
  # where it came from, so page 1's rows would be lost: a person reads it.
  f <- function(d, desc, fee = "", out = "", inn = "", bal = "") sprintf("%-9s%-33s%4s%14s%13s%13s", d, desc, fee, out, inn, bal)
  a <- c(blk_head[1:2], "Date     Details                          Fee   Withdrawals     Deposits      Balance",
         f("", "Opening balance", bal = "1,000.00"), f("02 Feb", "ACCOUNT FEE", "1.00", bal = "999.00"),
         f("03 Feb", "EFTPOS RIVERSIDE DAIRY", out = "11.40", bal = "987.60"), f("04 Feb", "ACCOUNT FEE", "0.50", bal = "987.10"),
         f("", "Balance carried forward", bal = "987.10"))
  b <- c(blk_head[3],
         "         Balance brought forward                                            987.10",
         "05 Feb   SALARY MATAI HOLDINGS                                  3,120.50    4,107.60",
         blk_rows[3:6], blk_close)
  pg <- function(lines, k) c(lines, "", sprintf("Page %d of 5", k))
  rd <- auto_read(blk_pdf(pg(blk_cover, 1), pg(a, 2), pg(b, 3), pg(blk_other, 4), pg(blk_back, 5)))
  expect_false(blk_auto(rd))
})

test_that("a page that cannot be read is never left behind by a brought-forward balance", {
  # Page 1's dates cannot be read (as on an upside-down or garbled page), so its
  # rows form no table; page 2 opens on the balance page 1 carried forward and adds
  # up to the closing balance. The carried figure is printed on page 1, so page 1's
  # rows are this statement's: a person reads it.
  bad <- function(l) sub("^0", "O", l)
  s1 <- c(blk_head, blk_open, bad(blk_rows[1:3]),
          "         Balance carried forward                                          3,839.45")
  s2 <- c(blk_head[3], "         Balance brought forward                                          3,839.45",
          blk_rows[4:6], blk_close)
  rd <- auto_read(blk_pdf(blk_cover, s1, s2, blk_other, blk_back))
  expect_false(blk_auto(rd) && nrow(rd$transactions) < length(blk_want))
})

test_that("a file that reads whole is never read a table at a time", {
  rd <- auto_read(blk_pdf(c(blk_cover[1:2], "", blk_stmt)))
  expect_equal(rd$outcome, "proven")
  expect_false("repair:tables_apart" %in% rd$candidates$source)
  expect_length(rd$other_accounts, 0L)
})

test_that("the statement's own wording for its start, and a 'Total movements' line, are understood", {
  expect_equal(.ar_anchor_class(c("balance at start of period", "balance at start", "total movements",
                                  "total movements this period")), c("open", "open", "total", "total"))
  expect_equal(.ar_total_side("Total movements"), "both")
})
