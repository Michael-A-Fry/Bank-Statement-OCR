#!/usr/bin/env Rscript
# make_pdf_fixtures.R -- generate the SYNTHETIC one-page PDF statements that give
# the shipped PDF templates a golden-file test.
#
# WHY these exist. "Proven template" is what gates the Qlik feed (R/feed.R), but
# proof was only ever "a YAML file sits in templates/". anz_everyday_pdf,
# asb_everyday_pdf and westpac_everyday_pdf had NO parsing test at all, so their
# column bands, date format and debit/credit split were unverified -- exactly the
# templates whose output flows straight to the dashboards.
#
# WHY synthetic. Real statements are personal financial data and are never
# committed (see samples/_private_staging/README.md). Each file below is invented
# people, invented accounts and an invented balance history, drawn at the SAME
# page coordinates the real layout uses, so it exercises the template's actual
# x-bands without carrying anyone's data. Every one reconciles: opening + the
# transactions = the printed closing balance.
#
# Run:  Rscript tests/testthat/fixtures/make_pdf_fixtures.R
# Writes: tests/testthat/fixtures/<template_id>_sample.pdf
#
# Regenerate the goldens afterwards (they are the engine's own parse, eyeballed):
#   see tests/HOWTO-add-template-test.md.

out_dir <- file.path("tests", "testthat", "fixtures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fmt <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 2, big.mark = ","))

# open_a4(path) -- A4 in POINTS, so the user coordinates below ARE the PDF
# coordinates the templates' x_min/x_max are expressed in. cairo renders an ASCII
# hyphen faithfully (base pdf() maps "-" to U+2212, which breaks account numbers).
open_a4 <- function(path) {
  if (capabilities("cairo")) cairo_pdf(path, width = 8.27, height = 11.69, onefile = TRUE)
  else pdf(path, width = 8.27, height = 11.69, onefile = TRUE)
  # xaxs/yaxs = "i": without it R pads the window by 4% at each end and every
  # coordinate below lands ~19pt to the right of where the template expects it.
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot.new(); plot.window(xlim = c(0, 595), ylim = c(842, 0))
}
# L/R -- draw text left- or right-aligned at an exact x, in points.
L <- function(x, y, s, cex = 0.85, font = 1) text(x, y, s, adj = c(0, 0), cex = cex, font = font, family = "sans")
R <- function(x, y, s, cex = 0.85, font = 1) text(x, y, s, adj = c(1, 0), cex = cex, font = font, family = "sans")

# running_balance(opening, wdl, dep) -- the printed Balance column.
running_balance <- function(opening, wdl, dep) {
  b <- opening
  vapply(seq_along(wdl), function(i) {
    b <<- round(b - (if (is.na(wdl[i])) 0 else wdl[i]) + (if (is.na(dep[i])) 0 else dep[i]), 2); b
  }, numeric(1))
}

# ---------------------------------------------------------------- ANZ everyday
# Bands: date 40-80 | description 80-330 | withdrawals 330-395 | deposits 395-472
#        | balance 472-545.  Dates are day+month only; the year comes from the
# statement period line.
make_anz <- function() {
  opening <- 2410.55
  tx <- data.frame(stringsAsFactors = FALSE,
    date = c("03 Feb", "05 Feb", "09 Feb", "14 Feb", "21 Feb", "26 Feb"),
    detail = c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES",
               "TRANSFER TO SAVINGS", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
    wdl = c(12.40, NA, 268.15, 400.00, 96.72, NA),
    dep = c(NA, 3120.00, NA, NA, NA, 2.18))
  tx$bal <- running_balance(opening, tx$wdl, tx$dep)
  closing <- tx$bal[nrow(tx)]

  open_a4(file.path(out_dir, "anz_everyday_pdf_sample.pdf"))
  L(40, 60, "Kauri Bank New Zealand", cex = 1.4, font = 2)
  L(40, 84, "Statement of Accounts", cex = 1.0)
  L(40, 120, "ALEX RIVERA")
  L(40, 138, "22 Sample Road, Hamilton 3204")
  L(40, 176, "Account number", font = 2); L(200, 176, "01-2345-0678901-00")
  L(40, 194, "Statement period", font = 2); L(200, 194, "from 1 Feb 2026 to 28 Feb 2026")
  L(40, 212, "Opening balance", font = 2);  L(200, 212, paste0("$", fmt(opening)))
  L(40, 230, "Closing balance", font = 2);  L(200, 230, paste0("$", fmt(closing)))
  # transaction table
  hy <- 280
  L(40, hy, "Date", font = 2)
  L(80, hy, "Transaction type and details", font = 2)
  R(392, hy, "Withdrawals", font = 2)
  R(469, hy, "Deposits", font = 2)
  R(542, hy, "Balance", font = 2)
  y <- hy + 26
  for (i in seq_len(nrow(tx))) {
    L(42, y, tx$date[i]); L(82, y, tx$detail[i])
    if (!is.na(tx$wdl[i])) R(392, y, fmt(tx$wdl[i]))
    if (!is.na(tx$dep[i])) R(469, y, fmt(tx$dep[i]))
    R(542, y, fmt(tx$bal[i]))
    y <- y + 22
  }
  L(40, y + 24, "Page 1 of 1", cex = 0.8)
  invisible(dev.off())
}

# ------------------------------------------------------- ANZ multi-statement
# A BUNDLE: two consecutive statements for one account in one PDF -- the commonest
# real shape a forensic accountant is handed (a year of statements downloaded in
# one go). Drawn at exactly the same coordinates as make_anz(), so it exercises
# the SHIPPED anz_everyday_pdf bands, and shaped so the split engine's safety
# conditions are genuinely met, not bypassed:
#   * each statement starts a fresh "Page 1 of N" footer -> the declared boundary;
#   * statement 1 runs over TWO pages ("Page 2 of 2") so the segmenter has to work
#     out page RANGES, not just "one statement per page";
#   * the two statements carry DISTINCT periods -> the independent count that
#     split.R requires before it will commit;
#   * each statement's opening + its own transactions = its own printed closing,
#     so every segment reconciles on its own terms.
# Together those let the test prove the split is right, not merely that it happened.
make_anz_bundle <- function() {
  # page(): one statement page -- header block only on its first page, column
  # labels repeated on every page (as real statements do).
  page <- function(period, acct, opening, closing, tx, first, pno, ptot) {
    plot.new(); plot.window(xlim = c(0, 595), ylim = c(842, 0))
    L(40, 60, "Kauri Bank New Zealand", cex = 1.4, font = 2)
    L(40, 84, "Statement of Accounts", cex = 1.0)
    if (first) {
      L(40, 120, "ALEX RIVERA")
      L(40, 138, "22 Sample Road, Hamilton 3204")
      L(40, 176, "Account number", font = 2); L(200, 176, acct)
      L(40, 194, "Statement period", font = 2); L(200, 194, period)
      L(40, 212, "Opening balance", font = 2); L(200, 212, paste0("$", fmt(opening)))
      L(40, 230, "Closing balance", font = 2); L(200, 230, paste0("$", fmt(closing)))
    }
    hy <- 280
    L(40, hy, "Date", font = 2)
    L(80, hy, "Transaction type and details", font = 2)
    R(392, hy, "Withdrawals", font = 2)
    R(469, hy, "Deposits", font = 2)
    R(542, hy, "Balance", font = 2)
    y <- hy + 26
    for (i in seq_len(nrow(tx))) {
      L(42, y, tx$date[i]); L(82, y, tx$detail[i])
      if (!is.na(tx$wdl[i])) R(392, y, fmt(tx$wdl[i]))
      if (!is.na(tx$dep[i])) R(469, y, fmt(tx$dep[i]))
      R(542, y, fmt(tx$bal[i]))
      y <- y + 22
    }
    L(40, y + 24, sprintf("Page %d of %d", pno, ptot), cex = 0.8)
  }

  # ---- statement 1: February 2026, two pages ----
  op1 <- 2410.55
  tx1 <- data.frame(stringsAsFactors = FALSE,
    date = c("03 Feb", "05 Feb", "09 Feb", "14 Feb", "21 Feb", "26 Feb"),
    detail = c("EFTPOS RIVERSIDE DAIRY", "SALARY MATAI HOLDINGS", "DD CITY COUNCIL RATES",
               "TRANSFER TO SAVINGS", "VISA HARBOUR FUEL", "CREDIT INTEREST"),
    wdl = c(12.40, NA, 268.15, 400.00, 96.72, NA),
    dep = c(NA, 3120.00, NA, NA, NA, 2.18))
  tx1$bal <- running_balance(op1, tx1$wdl, tx1$dep)
  cl1 <- tx1$bal[nrow(tx1)]

  # ---- statement 2: March 2026, one page; opens where February closed ----
  op2 <- cl1
  tx2 <- data.frame(stringsAsFactors = FALSE,
    date = c("04 Mar", "12 Mar", "25 Mar"),
    detail = c("EFTPOS TOTARA GROCER", "SALARY MATAI HOLDINGS", "DD RIMU INSURANCE"),
    wdl = c(58.20, NA, 143.65),
    dep = c(NA, 3120.00, NA))
  tx2$bal <- running_balance(op2, tx2$wdl, tx2$dep)
  cl2 <- tx2$bal[nrow(tx2)]

  path <- file.path(out_dir, "anz_everyday_pdf_bundle_sample.pdf")
  if (capabilities("cairo")) cairo_pdf(path, width = 8.27, height = 11.69, onefile = TRUE)
  else pdf(path, width = 8.27, height = 11.69, onefile = TRUE)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  page("from 1 Feb 2026 to 28 Feb 2026", "01-2345-0678901-00", op1, cl1, tx1[1:4, ], TRUE,  1, 2)
  page("from 1 Feb 2026 to 28 Feb 2026", "01-2345-0678901-00", op1, cl1, tx1[5:6, ], FALSE, 2, 2)
  page("from 1 Mar 2026 to 31 Mar 2026", "01-2345-0678901-00", op2, cl2, tx2,        TRUE,  1, 1)
  invisible(dev.off())
}

# ---------------------------------------------------------------- ASB everyday
# Bands: date 33-72 | description 82-328 | debit 328-415 | credit 415-478
#        | balance 478-545.  The column labels carry ASB's "$" suffix, which is
# what the template now fingerprints on.
make_asb <- function() {
  opening <- 875.20
  tx <- data.frame(stringsAsFactors = FALSE,
    date = c("06 Mar", "11 Mar", "17 Mar", "23 Mar", "30 Mar"),
    detail = c("EFTPOS TOTARA GROCER", "PAY MATAI HOLDINGS", "BILL PAYMENT POWER CO",
               "ATM CASH WITHDRAWAL", "REFUND ONLINE STORE"),
    wdl = c(48.65, NA, 152.30, 120.00, NA),
    dep = c(NA, 1890.45, NA, NA, 33.10))
  tx$bal <- running_balance(opening, tx$wdl, tx$dep)
  closing <- tx$bal[nrow(tx)]

  open_a4(file.path(out_dir, "asb_everyday_pdf_sample.pdf"))
  L(33, 60, "Totara Savings Bank", cex = 1.4, font = 2)
  L(33, 84, "Streamline account statement", cex = 1.0)
  L(33, 120, "SAM OKAFOR")
  L(33, 138, "9 Example Terrace, Napier 4110")
  L(33, 176, "Account number", font = 2); L(200, 176, "12-3456-0789012-50")
  L(33, 194, "Statement period", font = 2); L(200, 194, "from 1 Mar 2026 to 31 Mar 2026")
  L(33, 212, "Opening balance", font = 2);  L(200, 212, paste0("$", fmt(opening)))
  L(33, 230, "Closing balance", font = 2);  L(200, 230, paste0("$", fmt(closing)))
  hy <- 280
  L(33, hy, "Date", font = 2)
  L(82, hy, "Transaction", font = 2)
  R(412, hy, "Debit/Withdrawal $", font = 2)
  R(475, hy, "Deposit $", font = 2)
  R(542, hy, "Balance $", font = 2)
  y <- hy + 26
  for (i in seq_len(nrow(tx))) {
    L(35, y, tx$date[i]); L(84, y, tx$detail[i])
    if (!is.na(tx$wdl[i])) R(412, y, fmt(tx$wdl[i]))
    if (!is.na(tx$dep[i])) R(475, y, fmt(tx$dep[i]))
    R(542, y, fmt(tx$bal[i]))
    y <- y + 22
  }
  L(33, y + 24, "Page 1 of 1", cex = 0.8)
  invisible(dev.off())
}

# ------------------------------------------------------------ Westpac everyday
# Bands: date 40-78 | description 90-355 | debit 355-400 | credit 400-470
#        | balance 470-535.  Westpac prints an "OPENING BALANCE" line above the
# first transaction, which is what the template fingerprints on (with "Your
# transactions"); the parser treats it as a summary line, not a transaction.
make_westpac <- function() {
  opening <- 5240.00
  tx <- data.frame(stringsAsFactors = FALSE,
    date = c("02 Apr", "08 Apr", "15 Apr", "19 Apr", "27 Apr"),
    detail = c("DD RIMU INSURANCE", "SALARY MATAI HOLDINGS", "EFTPOS KOWHAI CAFE",
               "TRANSFER TO LOAN", "VISA AIRLINE BOOKING"),
    wdl = c(88.90, NA, 6.50, 950.00, 412.30),
    dep = c(NA, 2760.15, NA, NA, NA))
  tx$bal <- running_balance(opening, tx$wdl, tx$dep)
  closing <- tx$bal[nrow(tx)]

  open_a4(file.path(out_dir, "westpac_everyday_pdf_sample.pdf"))
  L(40, 60, "Rimu Bank Limited", cex = 1.4, font = 2)
  L(40, 84, "Everyday account statement", cex = 1.0)
  L(40, 120, "JORDAN TE AWA")
  L(40, 138, "5 Specimen Lane, Dunedin 9016")
  L(40, 176, "Account number", font = 2); L(200, 176, "03-4567-0890123-00")
  L(40, 194, "Statement period", font = 2); L(200, 194, "from 1 Apr 2026 to 30 Apr 2026")
  L(40, 212, "Opening balance", font = 2);  L(200, 212, paste0("$", fmt(opening)))
  L(40, 230, "Closing balance", font = 2);  L(200, 230, paste0("$", fmt(closing)))
  L(40, 268, "Your transactions", cex = 1.0, font = 2)
  hy <- 296
  L(40, hy, "Date", font = 2)
  L(90, hy, "Transaction", font = 2)
  R(397, hy, "Withdrawals", font = 2)
  R(467, hy, "Deposits", font = 2)
  R(532, hy, "Balance", font = 2)
  L(90, hy + 24, "OPENING BALANCE"); R(532, hy + 24, fmt(opening))
  y <- hy + 48
  for (i in seq_len(nrow(tx))) {
    L(42, y, tx$date[i]); L(92, y, tx$detail[i])
    if (!is.na(tx$wdl[i])) R(397, y, fmt(tx$wdl[i]))
    if (!is.na(tx$dep[i])) R(467, y, fmt(tx$dep[i]))
    R(532, y, fmt(tx$bal[i]))
    y <- y + 22
  }
  L(90, y, "CLOSING BALANCE"); R(532, y, fmt(closing))
  L(40, y + 26, "Page 1 of 1", cex = 0.8)
  invisible(dev.off())
}

make_anz(); make_anz_bundle(); make_asb(); make_westpac()
cat("wrote fixtures to", out_dir, "\n")
