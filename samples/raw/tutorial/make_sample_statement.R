#!/usr/bin/env Rscript
# make_sample_statement.R -- generate a REALISTIC but fully SYNTHETIC bank
# statement PDF for the template-wizard tutorial and its golden test.
#
# Why synthetic and not a real statement: real statements are personal financial
# data; even redacted, we never commit them. This one has invented people,
# accounts and a fictional bank ("Kowhai Bank NZ"), so it is safe to ship, is
# reproducible, and deliberately packs in the tricky real-world features a
# maintainer must learn to handle:
#   * Withdrawals + Deposits as TWO columns (not one signed amount)
#   * day+month-only dates ("02 May") -- the year lives in the statement period
#   * multi-line rows (a card/reference detail line under some transactions)
#   * a "Balance brought forward" line and a running Balance column
#   * a summary block with opening/closing balance wording
#   * two pages (front summary + transactions), like a real statement
#
# Run:  Rscript samples/raw/tutorial/make_sample_statement.R
# Writes: samples/raw/tutorial/sample_everyday_statement.pdf

`%||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a)) b else a
out <- "samples/raw/tutorial/sample_everyday_statement.pdf"

# --- transaction data (reconciles: opening + deposits - withdrawals = closing) --
opening <- 1250.00
tx <- data.frame(stringsAsFactors = FALSE,
  date   = c("02 May","03 May","05 May","07 May","10 May","12 May",
             "15 May","18 May","20 May","24 May","28 May","31 May"),
  detail = c("EFTPOS COFFEE HOUSE","SALARY ACME LTD","DD POWER CO 123456",
             "VISA NEW WORLD KH","ATM CASH WITHDRAWAL","TRANSFER TO SAVINGS 02-1234-0567890-25",
             "REFUND TRADEME","DD RENT PROP MGR","VISA Z ENERGY","CREDIT INTEREST",
             "EFTPOS COUNTDOWN","DD SUNSHINE INSURANCE"),
  sub    = c("","","","4835******1234 Orig date 06/05/2026","","",
             "","","4835******1234 Orig date 19/05/2026","","",""),
  wdl    = c(5.50,NA,180.20,85.40,100.00,500.00,NA,650.00,72.30,NA,123.45,60.00),
  dep    = c(NA,3200.00,NA,NA,NA,NA,42.00,NA,NA,1.35,NA,NA))
bal <- opening; tx$bal <- NA_real_
for (i in seq_len(nrow(tx))) {
  bal <- bal - (tx$wdl[i] %||% 0) + (tx$dep[i] %||% 0)
  if (is.na(tx$wdl[i])) bal <- bal + 0
  tx$bal[i] <- round(bal, 2)
}
closing <- tx$bal[nrow(tx)]
fmt <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = 2, big.mark = ","))

# cairo_pdf renders ASCII hyphens faithfully (base pdf() maps "-" to U+2212).
if (capabilities("cairo")) cairo_pdf(out, width = 8.27, height = 11.69, onefile = TRUE)
else pdf(out, width = 8.27, height = 11.69, onefile = TRUE)   # A4
op <- par(mar = c(0, 0, 0, 0))

# ---- PAGE 1: summary --------------------------------------------------------
plot.new(); plot.window(xlim = c(0, 595), ylim = c(842, 0))
T <- function(x, y, s, adj = c(0,0), cex = 1, font = 1, fam = "sans")
  text(x, y, s, adj = adj, cex = cex, font = font, family = fam)
T(45, 70, "Kowhai Bank NZ", cex = 1.6, font = 2)
T(45, 95, "Statement of Account")
T(45, 150, "JAMIE SAMPLE"); T(45, 168, "1 Example Street"); T(45, 186, "Wellington 6011")
T(45, 250, "Account name", font = 2); T(200, 250, "Everyday account")
T(45, 272, "Account number", font = 2); T(200, 272, "02-1234-0567890-00")
T(45, 294, "Statement period", font = 2); T(200, 294, "from 1 May 2026 to 31 May 2026")
T(45, 340, "Opening balance", font = 2); T(200, 340, paste0("$", fmt(opening)))
T(45, 362, "Closing balance", font = 2); T(200, 362, paste0("$", fmt(closing)))
T(45, 384, "Total withdrawals", font = 2); T(200, 384, paste0("$", fmt(sum(tx$wdl, na.rm = TRUE))))
T(45, 406, "Total deposits", font = 2); T(200, 406, paste0("$", fmt(sum(tx$dep, na.rm = TRUE))))
T(45, 470, "Page 1 of 2", cex = 0.8)

# ---- PAGE 2: transactions ---------------------------------------------------
plot.new(); plot.window(xlim = c(0, 595), ylim = c(842, 0))
T <- function(x, y, s, adj = c(0,0), cex = 0.85, font = 1)
  text(x, y, s, adj = adj, cex = cex, font = font, family = "sans")
T(45, 55, "Everyday account 02-1234-0567890-00 - continued", font = 2)
# header
hy <- 85
T(45,  hy, "Date", font = 2); T(95, hy, "Transaction details", font = 2)
T(392, hy, "Withdrawals", font = 2, adj = c(1,0))
T(468, hy, "Deposits", font = 2, adj = c(1,0))
T(545, hy, "Balance", font = 2, adj = c(1,0))
T(95, 108, "Balance brought forward"); T(545, 108, fmt(opening), adj = c(1,0))
y <- 130
for (i in seq_len(nrow(tx))) {
  T(45,  y, tx$date[i]); T(95, y, tx$detail[i])
  if (!is.na(tx$wdl[i])) T(392, y, fmt(tx$wdl[i]), adj = c(1,0))
  if (!is.na(tx$dep[i])) T(468, y, fmt(tx$dep[i]), adj = c(1,0))
  T(545, y, fmt(tx$bal[i]), adj = c(1,0))
  y <- y + 22
  if (nzchar(tx$sub[i])) { T(95, y - 6, tx$sub[i], cex = 0.75); y <- y + 14 }
}
T(45, y + 30, "Page 2 of 2", cex = 0.8)
par(op); invisible(dev.off())
cat("wrote", out, "\n")
