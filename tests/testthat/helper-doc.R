# helper-doc.R -- a synthetic multi-table DOCUMENT, built as word geometry.
#
# WHY NOT A PDF. The engine under test (R/tables.R, R/tables_detect.R,
# R/doc_extract.R) consumes exactly one thing: per-page word boxes with x, y,
# width, height and text. Building those directly instead of rendering a PDF and
# reading it back means the tests state the geometry they are asserting on, run
# with no pdftools / cairo / font stack present, and cannot fail because a font
# metric moved by a point. The PDF round-trip is covered separately, and skips
# where the tools are absent.
#
# The document below is deliberately awkward, and every awkwardness is one the
# real reports do:
#   page 1  four label/value pairs, each arranged DIFFERENTLY
#   page 2  a short table that fits on the page
#   page 3  a table that STARTS part-way down, under a paragraph
#   page 4  the same table continuing, header REPEATED
#   page 5  it ENDS part-way down and TWO more tables follow it
#   page 6  a statement of position: row definitions, date headers, a Total
#           column, and a closing row with blanks in the middle ON PURPOSE
#   page 7  page 2's table again for a second account, MOVED 40pt down the page

DOC_SIZE <- 9        # the type size every fixture line is set in
DOC_W <- 595.28
DOC_H <- 841.89

# doc_L(x, y, s) -- a line of words set from x, left aligned, one box per word,
# with a normal word space between them. The 0.5-per-character width is a stand-in
# for a font metric: it only has to be consistent, because every assertion below
# is about which BAND a word lands in, not about its exact width.
doc_L <- function(x, y, s, size = DOC_SIZE) {
  toks <- strsplit(as.character(s), " ", fixed = TRUE)[[1]]
  toks <- toks[nzchar(toks)]
  if (!length(toks)) return(NULL)
  out <- vector("list", length(toks)); cx <- x
  for (i in seq_along(toks)) {
    wdt <- nchar(toks[i]) * size * 0.5
    out[[i]] <- data.frame(text = toks[i], x = cx, y = y, width = wdt,
                           height = size, stringsAsFactors = FALSE)
    cx <- cx + wdt + size * 0.28
  }
  do.call(rbind, out)
}

# doc_R(xr, y, s) -- the same line, right aligned so it ENDS at xr. Money columns
# are set this way, which is why a column band cannot be derived from left edges.
doc_R <- function(xr, y, s, size = DOC_SIZE) {
  d <- doc_L(0, y, s, size)
  if (is.null(d)) return(NULL)
  d$x <- d$x + (xr - max(d$x + d$width))
  d
}

doc_page <- function(...) {
  parts <- Filter(Negate(is.null), list(...))
  if (!length(parts)) return(data.frame(text = character(0), x = numeric(0),
                                        y = numeric(0), width = numeric(0),
                                        height = numeric(0), stringsAsFactors = FALSE))
  d <- do.call(rbind, parts)
  d$ocr_conf <- rep(NA_real_, nrow(d))
  rownames(d) <- NULL
  d
}

# doc_input(pages) -- the read_pdf()-shaped object the engine consumes. The page
# TEXT is regenerated from the words, so fingerprint matching works on it too.
doc_input <- function(pages) {
  n <- length(pages)
  txt <- vapply(pages, function(w) {
    if (!nrow(w)) return("")
    w <- w[order(round(w$y / 3), w$x), , drop = FALSE]
    paste(tapply(w$text, round(w$y / 3), paste, collapse = " "), collapse = "\n")
  }, character(1))
  list(pages = unname(txt), words = pages, page_count = n,
       page_width = rep(DOC_W, n), page_height = rep(DOC_H, n),
       ok = TRUE)
}

.doc_money <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

# --- page 2 / page 7: "Account summary" ------------------------------------
# Same table, twice, the second time 40pt lower on the page and for a different
# account -- which is what a text anchor has to survive and a coordinate cannot.
doc_account_summary <- function(y0, accounts) {
  parts <- list(
    doc_L(40, y0 - 40, "Account summary"),
    doc_L(40, y0, "Account"), doc_L(240, y0, "Type"),
    doc_R(400, y0, "Opening"), doc_R(500, y0, "Closing"))
  y <- y0 + 20
  for (a in accounts) {
    parts <- c(parts, list(doc_L(40, y, a$account), doc_L(240, y, a$type),
                           doc_R(400, y, .doc_money(a$open)),
                           doc_R(500, y, .doc_money(a$close))))
    y <- y + 18
  }
  parts
}

# --- pages 3-5: the long transaction schedule -------------------------------
doc_tx_header <- function(y) list(
  doc_L(40, y, "Date"), doc_L(110, y, "Description"),
  doc_R(400, y, "Amount"), doc_R(500, y, "Balance"))

doc_tx_rows <- function(y0, n, first_day = 1L, step = 16) {
  parts <- list(); y <- y0
  for (i in seq_len(n)) {
    d <- ((first_day + i - 2L) %% 28L) + 1L
    parts <- c(parts, list(
      doc_L(40, y, sprintf("%02d/04/2025", d)),
      doc_L(110, y, sprintf("PAYMENT-%03d", i)),
      doc_R(400, y, .doc_money(round(-400 + i * 17.5, 2))),
      doc_R(500, y, .doc_money(round(1240.55 + i * 12.25, 2)))))
    y <- y + step
  }
  parts
}

# doc_fixture_pages() -- the seven pages, as a list of word data.frames.
doc_fixture_pages <- function() {
  # ---- page 1: front matter, four arrangements ----------------------------
  # Leading is 40pt, and the "label above value" pair sits 22pt apart -- which is
  # the case the learned row tolerance has to keep separate (see .doc_row_tol).
  p1 <- doc_page(
    doc_L(40, 60, "NORTHWIND TRUSTEE SERVICES", size = 13),
    doc_L(40, 100, "Consolidated position report"),
    # (a) label left, value right, same line
    doc_L(40, 140, "Prepared for"), doc_L(200, 140, "Ambrose Family Trust"),
    # (b) label above, value under it
    doc_L(40, 180, "Client reference"), doc_L(40, 202, "NW-99-9999-9999"),
    # (c) label left, value right -- a date rather than a name
    doc_L(40, 242, "Generated"), doc_L(200, 242, "07/04/2025"),
    # (d) the awkward one: the VALUE first, its label after it
    doc_L(40, 282, "1,240.55"), doc_L(150, 282, "Total contributions"),
    doc_L(40, 322, "This document is generated. Figures are as at the date shown."))

  # ---- page 2: a short table ----------------------------------------------
  p2 <- do.call(doc_page, doc_account_summary(100, list(
    list(account = "Everyday 99-9999-9999999-99", type = "Transaction",
         open = 1240.55, close = 1810.20),
    list(account = "Savings 99-9999-9999999-01", type = "Savings",
         open = 15000.00, close = 15420.75),
    list(account = "Term 99-9999-9999999-02", type = "TermDeposit",
         open = 50000.00, close = 50000.00),
    list(account = "Card 9999-9999-9999-9999", type = "CreditCard",
         open = -820.10, close = -410.05))))

  # ---- page 3: prose, then a table STARTING at y=200 ----------------------
  p3 <- do.call(doc_page, c(
    list(doc_L(40, 60, "Notes to the position"),
         doc_L(40, 100, "The transaction detail below covers the whole period."),
         doc_L(40, 140, "Narrative that is NOT part of any table.")),
    doc_tx_header(200), doc_tx_rows(220, 28)))

  # ---- page 4: the same table continuing, header REPEATED -----------------
  p4 <- do.call(doc_page, c(doc_tx_header(60), doc_tx_rows(80, 44, first_day = 5L)))

  # ---- page 5: it ENDS at y~250, then TWO more tables ---------------------
  p5 <- do.call(doc_page, c(
    doc_tx_header(60), doc_tx_rows(80, 11, first_day = 9L),
    list(doc_L(40, 300, "Fees charged"),
         doc_L(40, 330, "Fee"), doc_L(240, 330, "Basis"), doc_R(500, 330, "Amount"),
         doc_L(40, 350, "AccountKeeping"), doc_L(240, 350, "per month"), doc_R(500, 350, "5.00"),
         doc_L(40, 368, "Transaction"), doc_L(240, 368, "per item"), doc_R(500, 368, "0.30"),
         doc_L(40, 386, "Overdraft"), doc_L(240, 386, "per day"), doc_R(500, 386, "3.50"),
         doc_L(40, 440, "Contact details"),
         doc_L(40, 470, "Channel"), doc_L(240, 470, "Detail"), doc_L(400, 470, "Hours"),
         doc_L(40, 490, "Telephone"), doc_L(240, 490, "0800-999-999"), doc_L(400, 490, "24/7"),
         doc_L(40, 508, "Web"), doc_L(240, 508, "example.invalid"), doc_L(400, 508, "24/7"),
         doc_L(40, 526, "Post", size = DOC_SIZE), doc_L(240, 526, "PO-Box-99"), doc_L(400, 526, "Weekdays"))))

  # ---- page 6: a statement of position, blanks in the closing row ---------
  fs_x <- c(40, 250, 315, 380, 445, 510, 570)
  fs <- list(
    c("OpeningBalance", "1,240.55", "1,810.20", "1,905.44", "2,110.00", "2,004.18", "1,240.55"),
    c("Contributions",    "500.00",   "500.00",   "500.00",   "500.00",   "500.00", "2,500.00"),
    c("Withdrawals",     "-120.35",  "-404.76",  "-295.44",  "-605.82",  "-118.90", "-1,545.27"),
    c("Interest",          "10.00",     "0.00",     "0.00",     "0.00",    "12.40",    "22.40"),
    c("Fees",              "-5.00",    "-5.00",    "-5.00",    "-5.00",    "-5.00",   "-25.00"))
  parts <- list(doc_L(40, 60, "Statement of position by period"),
                doc_L(fs_x[1], 100, "Item"))
  for (j in 2:6) parts <- c(parts, list(doc_R(fs_x[j], 100, sprintf("31/0%d/2025", j - 1))))
  parts <- c(parts, list(doc_R(fs_x[7], 100, "Total")))
  y <- 124
  for (r in fs) {
    parts <- c(parts, list(doc_L(fs_x[1], y, r[1])))
    for (j in 2:7) parts <- c(parts, list(doc_R(fs_x[j], y, r[j])))
    y <- y + 18
  }
  # the closing row: the middle columns are EMPTY, and that is correct
  parts <- c(parts, list(doc_L(fs_x[1], y, "ClosingBalance"),
                         doc_R(fs_x[7], y, "1,810.20")))
  p6 <- do.call(doc_page, parts)

  # ---- page 7: page 2's table, 40pt lower, second account ----------------
  p7 <- do.call(doc_page, doc_account_summary(140, list(
    list(account = "Everyday 99-9999-9999999-77", type = "Transaction",
         open = 880.00, close = 915.60),
    list(account = "Savings 99-9999-9999999-78", type = "Savings",
         open = 2400.00, close = 2412.30))))

  list(p1, p2, p3, p4, p5, p6, p7)
}

doc_fixture_input <- function() doc_input(doc_fixture_pages())

# doc_fixture_template() -- a mode:document template drawn on the fixture above.
# The bands are the ones .doc_bands() would derive, rounded to whole points.
doc_fixture_template <- function() list(
  id = "northwind_position", bank = "Northwind", statement_type = "position report",
  format = "pdf", mode = "document", version = 1, doc_pages = 7L,
  ref_width = DOC_W, ref_height = DOC_H,
  fingerprint = list(page_contains_all = list("Consolidated position report")),
  pairs = list(
    prepared_for = list(label_text = "Prepared for",
      label = list(page = 1, x_min = 38, x_max = 100, y_min = 138, y_max = 151),
      value = list(page = 1, x_min = 198, x_max = 320, y_min = 138, y_max = 151),
      type = "text"),
    client_reference = list(label_text = "Client reference",
      label = list(page = 1, x_min = 38, x_max = 120, y_min = 178, y_max = 191),
      value = list(page = 1, x_min = 38, x_max = 140, y_min = 200, y_max = 213),
      type = "text"),
    generated = list(label_text = "Generated",
      label = list(page = 1, x_min = 38, x_max = 90, y_min = 240, y_max = 253),
      value = list(page = 1, x_min = 198, x_max = 260, y_min = 240, y_max = 253),
      type = "date"),
    total_contributions = list(label_text = "Total contributions",
      label = list(page = 1, x_min = 148, x_max = 240, y_min = 280, y_max = 293),
      value = list(page = 1, x_min = 38, x_max = 90, y_min = 280, y_max = 293),
      type = "money")),
  tables = list(
    account_summary = list(
      name = "Account summary",
      start = list(page = 2, y = 98), end = list(page = 2, y = 200),
      header_rows = 1L, follow = FALSE, min_fill = 0.5,
      anchor = list(header_text = list("Account", "Type", "Opening", "Closing"),
                    first_column = list("Everyday", "Savings")),
      columns = list(
        list(name = "Account", x_min = 30, x_max = 199, type = "text"),
        list(name = "Type",    x_min = 199, x_max = 325, type = "text"),
        list(name = "Opening", x_min = 325, x_max = 430, type = "money"),
        list(name = "Closing", x_min = 430, x_max = 510, type = "money"))),
    transactions = list(
      name = "Transaction detail",
      start = list(page = 3, y = 198), end = list(page = 5, y = 270),
      header_rows = 1L, follow = FALSE, min_fill = 0.5,
      anchor = list(header_text = list("Date", "Description", "Amount", "Balance"),
                    first_column = list("01/04/2025")),
      columns = list(
        list(name = "Date",        x_min = 30, x_max = 105, type = "date"),
        list(name = "Description", x_min = 105, x_max = 325, type = "text"),
        list(name = "Amount",      x_min = 325, x_max = 430, type = "money"),
        list(name = "Balance",     x_min = 430, x_max = 510, type = "money"))),
    position = list(
      name = "Statement of position",
      start = list(page = 6, y = 98), end = list(page = 6, y = 240),
      header_rows = 1L, follow = FALSE, min_fill = 0.5,
      anchor = list(header_text = list("Item", "Total"), first_column = list("OpeningBalance")),
      columns = list(
        list(name = "Item",  x_min = 30,  x_max = 200, type = "text"),
        list(name = "M1",    x_min = 200, x_max = 285, type = "money", may_be_blank = TRUE),
        list(name = "M2",    x_min = 285, x_max = 350, type = "money", may_be_blank = TRUE),
        list(name = "M3",    x_min = 350, x_max = 415, type = "money", may_be_blank = TRUE),
        list(name = "M4",    x_min = 415, x_max = 480, type = "money", may_be_blank = TRUE),
        list(name = "M5",    x_min = 480, x_max = 542, type = "money", may_be_blank = TRUE),
        list(name = "Total", x_min = 542, x_max = 600, type = "money")))),
  currency = "NZD")
