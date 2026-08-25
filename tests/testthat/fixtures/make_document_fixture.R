# make_document_fixture.R -- generate a synthetic multi-table PDF for the
# document extractor to be exercised against END TO END, through pdftools.
#
# WHY IT IS GENERATED AND NOT COMMITTED. The documents this feature is for are
# forty-page reports carrying thirty-odd tables, and not one of them can go in a
# repository: they are real client financial data. So the SHAPES are reproduced
# here with invented content, at exact page coordinates, and every awkward thing
# the real documents do is deliberately present:
#
#   page 1  front matter -- four label/value pairs, each oriented DIFFERENTLY
#           (value right of label, value under label, a typed date, and the
#           awkward one: the VALUE first with its label after it)
#   page 2  a short table that fits on one page
#   page 3  a long table STARTING PART-WAY DOWN, under a paragraph
#   page 4  ...the same table continuing, full page, header REPEATED
#   page 5  ...it ENDS part-way down, and TWO MORE TABLES follow it on that page
#   page 6  a statement of position: column 1 is row DEFINITIONS, columns 2-6 are
#           DATE headers, the last column is a Total, and the bottom row is a
#           total with blanks in it (the "may be blank" case)
#   page 7  page 2's table again for a SECOND account, shifted 40pt down the page,
#           so text-anchoring can be tested against a layout that MOVED
#
# THE UNIT TESTS DO NOT USE THIS. They build the same shapes as word geometry
# (tests/testthat/helper-doc.R), which needs no PDF toolchain and cannot fail
# because a font metric moved. This file is for the one thing that cannot test:
# that a real PDF, read back through pdftools, still comes out right.
#
# Run:      Rscript tests/testthat/fixtures/make_document_fixture.R [out.pdf]
# Or call:  make_document_fixture("somewhere.pdf")

make_document_fixture <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (capabilities("cairo")) cairo_pdf(path, width = 8.27, height = 11.69, onefile = TRUE)
  else pdf(path, width = 8.27, height = 11.69, onefile = TRUE)
  # Close the device we opened, and NOTHING ELSE. Restoring `par` on exit runs
  # after dev.off(), and par() with no device open silently opens one
  # (Rplots.pdf) that then leaks into everything after it -- see the note in
  # test-detect_redaction.R, where exactly this cost three false failures. The
  # device is temporary; its par settings die with it and need no restoring.
  dev_no <- grDevices::dev.cur()
  on.exit(if (dev_no %in% grDevices::dev.list()) grDevices::dev.off(dev_no), add = TRUE)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")

  # Points, y measured DOWN from the top of the page -- the same convention
  # pdftools::pdf_data() reports word boxes in, so a coordinate written here is
  # the coordinate that comes back.
  new_page <- function() {
    plot.new(); plot.window(xlim = c(0, 595), ylim = c(842, 0))
  }
  L <- function(x, y, s, cex = 0.8, font = 1)
    text(x, y, s, adj = c(0, 0), cex = cex, font = font, family = "sans")
  R <- function(x, y, s, cex = 0.8, font = 1)
    text(x, y, s, adj = c(1, 0), cex = cex, font = font, family = "sans")
  money <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

  # .rows(x_at, y0, step, cells) -- a block of rows at fixed x positions. Every
  # table here is drawn this way, so a test can name the exact band a column
  # occupies and the exact y a row sits on.
  .rows <- function(x_at, y0, step, cells, right = integer(0)) {
    for (i in seq_along(cells)) {
      y <- y0 + (i - 1) * step
      for (j in seq_along(cells[[i]])) {
        v <- cells[[i]][j]
        if (is.na(v) || !nzchar(v)) next
        if (j %in% right) R(x_at[j], y, v) else L(x_at[j], y, v)
      }
    }
    y0 + (length(cells) - 1) * step
  }

  # ---- page 1: front matter, four DIFFERENT arrangements --------------------
  new_page()
  L(40, 60, "NORTHWIND TRUSTEE SERVICES", cex = 1.3, font = 2)
  L(40, 100, "Consolidated position report", cex = 0.95)
  L(40, 140, "Prepared for", font = 2); L(200, 140, "Ambrose Family Trust")
  L(40, 180, "Client reference", font = 2); L(40, 202, "NW-99-9999-9999")
  L(40, 242, "Generated", font = 2); L(200, 242, "07/04/2025")
  # the awkward one: the value is printed FIRST and its label follows it
  L(40, 282, "1,240.55"); L(150, 282, "Total contributions", font = 2)
  L(40, 322, "This document is generated. Figures are as at the date shown.", cex = 0.75)

  # ---- pages 2 and 7: the same table, the second time 40pt lower ------------
  xs <- c(40, 240, 400, 500); rt <- c(3, 4)
  acct_head <- function(y) {
    L(xs[1], y, "Account", font = 2); L(xs[2], y, "Type", font = 2)
    R(xs[3], y, "Opening", font = 2);  R(xs[4], y, "Closing", font = 2)
  }
  new_page()
  L(40, 60, "Account summary", cex = 1.1, font = 2)
  acct_head(100)
  .rows(xs, 120, 18, list(
    c("Everyday 99-9999-9999999-99", "Transaction", money(1240.55), money(1810.20)),
    c("Savings 99-9999-9999999-01",  "Savings",     money(15000.00), money(15420.75)),
    c("Term 99-9999-9999999-02",     "TermDeposit", money(50000.00), money(50000.00)),
    c("Card 9999-9999-9999-9999",    "CreditCard",  money(-820.10), money(-410.05))),
    right = rt)

  # ---- pages 3-5: the long schedule ----------------------------------------
  tx <- c(40, 110, 400, 500); tx_rt <- c(3, 4)
  tx_head <- function(y) {
    L(tx[1], y, "Date", font = 2); L(tx[2], y, "Description", font = 2)
    R(tx[3], y, "Amount", font = 2); R(tx[4], y, "Balance", font = 2)
  }
  mk <- function(n, from) lapply(seq_len(n), function(i) {
    k <- from + i - 1L
    c(sprintf("%02d/04/2025", ((k - 1L) %% 28L) + 1L), sprintf("PAYMENT-%03d", k),
      money(round(-400 + k * 17.5, 2)), money(round(1240.55 + k * 12.25, 2)))
  })
  new_page()
  L(40, 60, "Notes to the position", cex = 1.1, font = 2)
  L(40, 100, "The transaction detail below covers the whole period.", cex = 0.8)
  L(40, 140, "Narrative that is NOT part of any table.", cex = 0.8)
  tx_head(200); .rows(tx, 220, 16, mk(28, 1L), right = tx_rt)      # STARTS at y=200
  new_page()
  tx_head(60);  .rows(tx, 80, 16, mk(44, 29L), right = tx_rt)      # header REPEATED
  new_page()
  tx_head(60);  .rows(tx, 80, 16, mk(11, 73L), right = tx_rt)      # ENDS at y~240
  L(40, 300, "Fees charged", cex = 1.0, font = 2)                  # second table
  L(40, 330, "Fee", font = 2); L(240, 330, "Basis", font = 2); R(500, 330, "Amount", font = 2)
  .rows(c(40, 240, 500), 350, 18, list(
    c("AccountKeeping", "per month", money(5.00)),
    c("Transaction",    "per item",  money(0.30)),
    c("Overdraft",      "per day",   money(3.50))), right = 3)
  L(40, 440, "Contact details", cex = 1.0, font = 2)                # third table
  L(40, 470, "Channel", font = 2); L(240, 470, "Detail", font = 2); L(400, 470, "Hours", font = 2)
  .rows(c(40, 240, 400), 490, 18, list(
    c("Telephone", "0800-999-999",    "24/7"),
    c("Web",       "example.invalid", "24/7"),
    c("Post",      "PO-Box-99",       "Weekdays")))

  # ---- page 6: definitions, date headers, a Total, and a row with blanks ----
  new_page()
  fs_x <- c(40, 250, 315, 380, 445, 510, 570); fs_rt <- 2:7
  L(40, 60, "Statement of position by period", cex = 1.1, font = 2)
  L(fs_x[1], 100, "Item", font = 2)
  for (j in 2:6) R(fs_x[j], 100, sprintf("31/0%d/2025", j - 1), font = 2)
  R(fs_x[7], 100, "Total", font = 2)
  .rows(fs_x, 124, 18, list(
    c("OpeningBalance", "1,240.55", "1,810.20", "1,905.44", "2,110.00", "2,004.18", "1,240.55"),
    c("Contributions",    "500.00",   "500.00",   "500.00",   "500.00",   "500.00", "2,500.00"),
    c("Withdrawals",     "-120.35",  "-404.76",  "-295.44",  "-605.82",  "-118.90", "-1,545.27"),
    c("Interest",          "10.00",     "0.00",     "0.00",     "0.00",    "12.40",    "22.40"),
    c("Fees",              "-5.00",    "-5.00",    "-5.00",    "-5.00",    "-5.00",   "-25.00")),
    right = fs_rt)
  # the closing row: blanks in the middle ON PURPOSE -- the "a column may
  # legitimately be empty" case the fill-rate check must not fail on.
  .rows(fs_x, 214, 18, list(c("ClosingBalance", "", "", "", "", "", "1,810.20")),
        right = fs_rt)

  # ---- page 7: page 2's table again, MOVED 40pt down, second account --------
  new_page()
  L(40, 100, "Account summary", cex = 1.1, font = 2)      # was y=60 on page 2
  acct_head(140)
  .rows(xs, 160, 18, list(
    c("Everyday 99-9999-9999999-77", "Transaction", money(880.00), money(915.60)),
    c("Savings 99-9999-9999999-78",  "Savings",     money(2400.00), money(2412.30))),
    right = rt)

  invisible(normalizePath(path, mustWork = FALSE))
}

# Run as a script -> write the fixture. source()d -> define the function and stop.
# sys.nframe() is 0 only for a top-level expression under Rscript; source() always
# adds a frame, so this is the one test that does not need a flag passed in.
if (sys.nframe() == 0L && !interactive()) {
  .args <- commandArgs(trailingOnly = TRUE)
  .out <- if (length(.args)) .args[1] else
    file.path("tests", "testthat", "fixtures", "complex_document_sample.pdf")
  cat("wrote", make_document_fixture(.out), "\n")
}
