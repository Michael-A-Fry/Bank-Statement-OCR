# helper-doc-hard.R -- the AWKWARD documents.
#
# helper-doc.R builds one well-behaved report. This file builds the ones that
# break things: fifteen documents, each isolating a single way a real report
# defeats a table reader, so a failure names its own cause instead of arriving as
# "the big fixture went wrong".
#
# Every one of these is a shape a printed financial report actually uses. They are
# built as word geometry for the same reason as helper-doc.R -- exact, portable,
# and no PDF toolchain -- and they use its doc_L / doc_R / doc_page / doc_input.
#
#   hard_condensed          columns separated by a RULE, not by whitespace: 2pt
#                           gutters and "|" glyphs between the cells
#   hard_ruled              "+----+----+" rule lines between every row
#   hard_uneven_rows        row pitch alternating 10pt / 26pt inside ONE table
#   hard_underlined_total   rows at 12pt, then a 45pt gap, then a total row over
#                           an underline drawn as "_______"
#   hard_header_variants    the header repeats on later pages with "(continued)"
#                           appended, and once with its columns in another order
#   hard_side_by_side       two unrelated tables printed beside each other
#   hard_wrapped_cells      a description that wraps onto a second line
#   hard_two_row_header     "Balance" spanning "Opening" and "Closing" beneath it
#   hard_wide_numbers       amounts from 1.00 to 12,345,678.90 in one column
#   hard_indented_total     a total row whose label is indented past column 1
#   hard_space_thousands    "1 234.56" -- a space as the thousands separator
#   hard_footnotes          superscript markers as their own tiny word boxes
#   hard_landscape          a landscape page in a portrait document
#   hard_footer_noise       a running footer that looks tabular on every page
#   hard_same_header_twice  two DIFFERENT tables, consecutive pages, same header
#   hard_blank_column       a column that is empty on every row

# doc_S(x, y, s, size) -- one word box, exactly where it is put. The layout
# helpers in helper-doc.R space words for you; these documents need cells placed
# to the point, which is what "condensed" means.
doc_S <- function(x, y, s, size = DOC_SIZE)
  data.frame(text = as.character(s), x = as.numeric(x), y = as.numeric(y),
             width = nchar(as.character(s)) * size * 0.5, height = as.numeric(size),
             stringsAsFactors = FALSE)

# doc_cells_at(y, xs, vals, size) -- a row of single-word cells at fixed lefts.
doc_cells_at <- function(y, xs, vals, size = DOC_SIZE)
  do.call(rbind, lapply(seq_along(vals), function(j)
    if (nzchar(vals[j])) doc_S(xs[j], y, vals[j], size) else NULL))

# ---------------------------------------------------------------------------

# CONDENSED + RULED. Gutters of 2pt with a "|" glyph in them -- narrower than any
# word space, so a gap rule cannot separate the columns and the whole row reads as
# one cell. This is how a densely-set schedule with vertical rules prints.
hard_condensed <- function() {
  rows <- list(c("01/04", "OPENINGBAL", "0.00", "1,240.55"),
               c("02/04", "TFR-IN", "500.00", "1,740.55"),
               c("03/04", "FEE", "-5.00", "1,735.55"),
               c("04/04", "TFR-OUT", "-120.00", "1,615.55"),
               c("05/04", "INTEREST", "1.20", "1,616.75"))
  xs <- c(40, 78, 150, 200)           # each cell 2pt clear of the "|" beside it
  bar <- c(72, 144, 194)
  parts <- list(doc_S(40, 60, "Condensed", 10), doc_S(90, 60, "schedule", 10),
                doc_cells_at(90, xs, c("Date", "Detail", "Amount", "Balance")))
  for (b in bar) parts <- c(parts, list(doc_S(b, 90, "|")))
  y <- 106
  for (r in rows) {
    parts <- c(parts, list(doc_cells_at(y, xs, r)))
    for (b in bar) parts <- c(parts, list(doc_S(b, y, "|")))
    y <- y + 14
  }
  doc_input(list(do.call(doc_page, parts)))
}

# RULE LINES between every row, drawn as characters (many PDFs do exactly this
# rather than using vector strokes). A rule is not a row and must not become one.
hard_ruled <- function() {
  rule <- function(y) doc_S(40, y, "+--------+--------------+----------+")
  xs <- c(40, 130, 300)
  parts <- list(doc_S(40, 60, "Ruled", 10), rule(80),
                doc_cells_at(94, xs, c("Code", "Description", "Amount")), rule(108))
  y <- 122
  for (r in list(c("A100", "Widgets", "1,000.00"), c("A200", "Gadgets", "2,500.00"),
                 c("A300", "Sprockets", "375.25"), c("A400", "Flanges", "88.00"))) {
    parts <- c(parts, list(doc_cells_at(y, xs, r), rule(y + 14)))
    y <- y + 28
  }
  doc_input(list(do.call(doc_page, parts)))
}

# UNEVEN ROW HEIGHTS inside one table: 10pt then 26pt, alternating. A learned
# tolerance taken from the median (18 -> 10.8) merges every 10pt pair.
hard_uneven_rows <- function() {
  xs <- c(40, 150, 350)
  parts <- list(doc_S(40, 60, "Uneven", 10),
                doc_cells_at(90, xs, c("Ref", "Item", "Amount")))
  vals <- list(c("R1", "Alpha", "10.00"), c("R2", "Bravo", "20.00"),
               c("R3", "Charlie", "30.00"), c("R4", "Delta", "40.00"),
               c("R5", "Echo", "50.00"), c("R6", "Foxtrot", "60.00"))
  y <- 110; step <- c(10, 26)
  for (i in seq_along(vals)) {
    parts <- c(parts, list(doc_cells_at(y, xs, vals[[i]])))
    y <- y + step[(i %% 2) + 1L]
  }
  doc_input(list(do.call(doc_page, parts)))
}

# A TOTAL ROW under an underline, 45pt below the last detail row. The gap is the
# printer's way of setting a total apart, and it is much bigger than the row
# pitch -- so a rule that ends a table at a big gap would drop the total.
hard_underlined_total <- function() {
  xs <- c(40, 150, 350)
  parts <- list(doc_S(40, 60, "Schedule", 10),
                doc_cells_at(84, xs, c("Ref", "Item", "Amount")))
  y <- 100
  for (r in list(c("R1", "Alpha", "10.00"), c("R2", "Bravo", "20.00"),
                 c("R3", "Charlie", "30.00"), c("R4", "Delta", "40.00"))) {
    parts <- c(parts, list(doc_cells_at(y, xs, r))); y <- y + 12
  }
  parts <- c(parts, list(doc_S(340, y + 20, "__________")),      # the underline
                    list(doc_cells_at(y + 33, xs, c("", "TOTAL", "100.00"))))
  doc_input(list(do.call(doc_page, parts)))
}

# THE HEADER REPEATS, BUT NOT WORD FOR WORD: "(continued)" on page 2, and on
# page 3 the same columns in a different order.
hard_header_variants <- function() {
  xs <- c(40, 150, 350, 460)
  hdr <- function(y, extra = "") c(list(doc_cells_at(y, xs, c("Date", "Detail", "Amount", "Balance"))),
                                   if (nzchar(extra)) list(doc_S(500, y, extra)) else list())
  body <- function(y0, from, n) {
    out <- list(); y <- y0
    for (i in seq_len(n)) {
      out <- c(out, list(doc_cells_at(y, xs, c(sprintf("%02d/04", from + i - 1L),
                                               sprintf("ITEM-%03d", from + i - 1L),
                                               "10.00", sprintf("%d.00", 100 + i)))))
      y <- y + 14
    }
    out
  }
  p1 <- do.call(doc_page, c(list(doc_S(40, 60, "Schedule", 10)), hdr(90), body(110, 1L, 8)))
  p2 <- do.call(doc_page, c(hdr(60, "(continued)"), body(80, 9L, 10)))
  p3 <- do.call(doc_page, c(list(doc_cells_at(60, xs, c("Detail", "Date", "Balance", "Amount"))),
                            body(80, 19L, 6)))
  doc_input(list(p1, p2, p3))
}

# TWO TABLES SIDE BY SIDE. Geometrically this is one table of six columns, and a
# reader that says so is not wrong about the ink -- only about the document.
hard_side_by_side <- function() {
  L <- c(40, 110, 180); R <- c(320, 390, 460)
  parts <- list(doc_S(40, 60, "Holdings", 10), doc_S(320, 60, "Income", 10),
                doc_cells_at(84, L, c("Code", "Units", "Value")),
                doc_cells_at(84, R, c("Code", "Rate", "Paid")))
  y <- 104
  for (i in 1:5) {
    parts <- c(parts, list(doc_cells_at(y, L, c(sprintf("H%02d", i), sprintf("%d", i * 100), sprintf("%d.00", i * 250))),
                           doc_cells_at(y, R, c(sprintf("I%02d", i), sprintf("%d.5", i), sprintf("%d.00", i * 12)))))
    y <- y + 14
  }
  doc_input(list(do.call(doc_page, parts)))
}

# A DESCRIPTION THAT WRAPS. The second physical line belongs to the row above it.
hard_wrapped_cells <- function() {
  xs <- c(40, 120, 400)
  parts <- list(doc_S(40, 60, "Narrative", 10),
                doc_cells_at(84, xs, c("Date", "Description", "Amount")))
  parts <- c(parts,
    list(doc_cells_at(104, xs, c("01/04", "PAYMENT TO SUPPLIER FOR", "1,000.00")),
         doc_S(120, 116, "PROFESSIONAL SERVICES RENDERED"),
         doc_cells_at(136, xs, c("02/04", "TRANSFER", "250.00")),
         doc_cells_at(156, xs, c("03/04", "DIRECT DEBIT INSURANCE", "88.50")),
         doc_S(120, 168, "PREMIUM ANNUAL"),
         doc_cells_at(188, xs, c("04/04", "INTEREST", "1.20"))))
  doc_input(list(do.call(doc_page, parts)))
}

# A TWO-ROW HEADER: "Balance" spans the two columns printed under it.
hard_two_row_header <- function() {
  xs <- c(40, 150, 300, 400)
  parts <- list(doc_S(40, 60, "Positions", 10),
                doc_S(300, 84, "Balance"),
                doc_cells_at(100, xs, c("Account", "Type", "Opening", "Closing")))
  y <- 120
  for (r in list(c("A1", "Current", "100.00", "150.00"),
                 c("A2", "Savings", "200.00", "260.00"),
                 c("A3", "Term", "300.00", "300.00"))) {
    parts <- c(parts, list(doc_cells_at(y, xs, r))); y <- y + 16
  }
  doc_input(list(do.call(doc_page, parts)))
}

# AMOUNTS OF VERY DIFFERENT WIDTHS, right aligned: the left edge of the column
# moves by 60pt between rows, so a band derived from left edges is wrong.
hard_wide_numbers <- function() {
  parts <- list(doc_S(40, 60, "Valuation", 10),
                doc_S(40, 84, "Item"), doc_R(500, 84, "Value"))
  y <- 104
  for (r in list(c("Alpha", "1.00"), c("Bravo", "12,345,678.90"),
                 c("Charlie", "250.00"), c("Delta", "9,876,543.21"),
                 c("Echo", "3.50"))) {
    parts <- c(parts, list(doc_S(40, y, r[1]), doc_R(500, y, r[2]))); y <- y + 14
  }
  doc_input(list(do.call(doc_page, parts)))
}

# A TOTAL ROW INDENTED past the first column, so its label lands in column two.
hard_indented_total <- function() {
  xs <- c(40, 200, 400)
  parts <- list(doc_S(40, 60, "Costs", 10),
                doc_cells_at(84, xs, c("Code", "Description", "Amount")))
  y <- 104
  for (r in list(c("C1", "Rent", "1,000.00"), c("C2", "Power", "250.00"),
                 c("C3", "Water", "80.00"))) {
    parts <- c(parts, list(doc_cells_at(y, xs, r))); y <- y + 14
  }
  parts <- c(parts, list(doc_S(200, y, "Total"), doc_S(400, y, "1,330.00")))
  doc_input(list(do.call(doc_page, parts)))
}

# A SPACE AS THE THOUSANDS SEPARATOR: "1 234.56" is two word boxes.
hard_space_thousands <- function() {
  xs <- c(40, 150)
  parts <- list(doc_S(40, 60, "European", 10), doc_S(110, 60, "layout", 10),
                doc_cells_at(84, c(40, 150, 300), c("Ref", "Item", "Amount")))
  y <- 104
  for (r in list(c("R1", "Alpha", "1 234.56"), c("R2", "Bravo", "22 999.00"),
                 c("R3", "Charlie", "8.75"), c("R4", "Delta", "104 000.00"))) {
    parts <- c(parts, list(doc_cells_at(y, xs, r[1:2]), doc_L(300, y, r[3])))
    y <- y + 14
  }
  doc_input(list(do.call(doc_page, parts)))
}

# FOOTNOTE MARKERS as their own tiny boxes, hard against the value they mark.
hard_footnotes <- function() {
  xs <- c(40, 150, 350)
  parts <- list(doc_S(40, 60, "Notes", 10),
                doc_cells_at(84, xs, c("Ref", "Item", "Amount")))
  y <- 104
  for (i in 1:4) {
    parts <- c(parts, list(doc_cells_at(y, xs, c(sprintf("R%d", i), "Item", sprintf("%d.00", i * 10))),
                           doc_S(350 + nchar(sprintf("%d.00", i * 10)) * 4.5 + 0.5, y - 2, "*", 5)))
    y <- y + 14
  }
  parts <- c(parts, list(doc_S(40, y + 20, "* provisional")))
  doc_input(list(do.call(doc_page, parts)))
}

# A LANDSCAPE PAGE inside a portrait document: the same table, on a page 842
# wide, so anything stored in the first page's point space is displaced.
hard_landscape <- function() {
  xs <- c(40, 200, 500)
  mk <- function() {
    parts <- list(doc_S(40, 60, "Wide", 10),
                  doc_cells_at(84, xs, c("Code", "Description", "Amount")))
    y <- 104
    for (r in list(c("W1", "Alpha", "10.00"), c("W2", "Bravo", "20.00"),
                   c("W3", "Charlie", "30.00"))) {
      parts <- c(parts, list(doc_cells_at(y, xs, r))); y <- y + 14
    }
    do.call(doc_page, parts)
  }
  inp <- doc_input(list(mk(), mk()))
  inp$page_width  <- c(DOC_W, DOC_H)      # page 2 is landscape
  inp$page_height <- c(DOC_H, DOC_W)
  inp
}

# A RUNNING FOOTER that breaks into three cells on every page.
hard_footer_noise <- function() {
  xs <- c(40, 150, 350)
  page <- function(n) {
    parts <- list(doc_cells_at(84, xs, c("Ref", "Item", "Amount")))
    y <- 104
    for (i in 1:5) {
      parts <- c(parts, list(doc_cells_at(y, xs, c(sprintf("R%d%d", n, i), "Item", sprintf("%d.00", i)))))
      y <- y + 14
    }
    parts <- c(parts, list(doc_S(40, 800, "IN-CONFIDENCE"),
                           doc_S(250, 800, sprintf("Page-%d-of-2", n)),
                           doc_S(450, 800, "07/04/2025")))
    do.call(doc_page, parts)
  }
  doc_input(list(page(1), page(2)))
}

# TWO DIFFERENT TABLES with the SAME header, on consecutive pages. Following one
# onto the next page joins two schedules that have nothing to do with each other.
hard_same_header_twice <- function() {
  xs <- c(40, 150, 350)
  page <- function(title, prefix, n) {
    parts <- list(doc_S(40, 60, title, 10),
                  doc_cells_at(90, xs, c("Ref", "Item", "Amount")))
    y <- 110
    for (i in seq_len(n)) {
      parts <- c(parts, list(doc_cells_at(y, xs, c(sprintf("%s%d", prefix, i), "Item",
                                                   sprintf("%d.00", i * 11)))))
      y <- y + 14
    }
    do.call(doc_page, parts)
  }
  doc_input(list(page("AccountOne", "A", 6), page("AccountTwo", "B", 6)))
}

# A COLUMN THAT IS EMPTY ON EVERY ROW -- printed, headed, and never filled.
hard_blank_column <- function() {
  xs <- c(40, 150, 300, 420)
  parts <- list(doc_S(40, 60, "Ledger", 10),
                doc_cells_at(84, xs, c("Ref", "Item", "Note", "Amount")))
  y <- 104
  for (i in 1:5) {
    parts <- c(parts, list(doc_cells_at(y, xs, c(sprintf("R%d", i), "Item", "",
                                                 sprintf("%d.00", i * 10)))))
    y <- y + 14
  }
  doc_input(list(do.call(doc_page, parts)))
}

# doc_hard_all() -- every one of them, named, for a sweep.
doc_hard_all <- function() list(
  condensed         = hard_condensed(),
  ruled             = hard_ruled(),
  uneven_rows       = hard_uneven_rows(),
  underlined_total  = hard_underlined_total(),
  header_variants   = hard_header_variants(),
  side_by_side      = hard_side_by_side(),
  wrapped_cells     = hard_wrapped_cells(),
  two_row_header    = hard_two_row_header(),
  wide_numbers      = hard_wide_numbers(),
  indented_total    = hard_indented_total(),
  space_thousands   = hard_space_thousands(),
  footnotes         = hard_footnotes(),
  landscape         = hard_landscape(),
  footer_noise      = hard_footer_noise(),
  same_header_twice = hard_same_header_twice(),
  blank_column      = hard_blank_column())
