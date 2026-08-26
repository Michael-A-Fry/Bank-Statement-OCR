# test-doc_hard.R -- the awkward documents (helper-doc-hard.R).
#
# Sixteen shapes a real printed report uses that a table reader gets wrong. Nine
# of these were FAILING when the fixtures were first run against the engine; each
# test names the measurement that was taken then, so a regression says what it
# broke rather than only that something changed.
#
# Three are still limitations. They are here as tests too, asserting the CURRENT
# behaviour, because a limitation nobody wrote down is indistinguishable from a
# bug nobody noticed.

.hard_one <- function(inp, page = NULL) {
  p <- propose_tables(inp, pages = page)
  expect_equal(length(p), 1L)
  p[[1]]
}
.hard_cols <- function(tb) vapply(.doc_columns(tb), function(cc)
  as.character(cc$name %||% "")[1], character(1))
.hard_cell <- function(r, col) as.character(r$rows[[col]])

# ---------------------------------------------------------------------------
# Borders drawn as characters
# ---------------------------------------------------------------------------

test_that("a table whose columns are separated by a rule, not by whitespace, is still a table", {
  # 2pt gutters with a "|" in them -- narrower than any word space, so no gap rule
  # can separate the columns. Was: read as one cell per row.
  tb <- .hard_one(hard_condensed())
  expect_equal(length(.doc_columns(tb)), 4L)
  # ...and the "|" does not end up in the column's NAME.
  expect_identical(.hard_cols(tb), c("Date", "Detail", "Amount", "Balance"))
  r <- doc_table_rows(hard_condensed(), tb, NULL)
  expect_equal(r$n_rows, 5L)
  expect_identical(.hard_cell(r, "Balance")[1], "1,240.55")
})

test_that("+---+---+ rules between rows are borders, not rows", {
  # Was: ZERO tables proposed. A rule spanning the full width merges every column
  # band into one, so nothing on the page looked tabular at all.
  inp <- hard_ruled()
  tb <- .hard_one(inp)
  expect_equal(length(.doc_columns(tb)), 3L)
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 4L)
  expect_identical(.hard_cell(r, "Code"), c("A100", "A200", "A300", "A400"))
  expect_false(any(grepl("^[-+]+$", unlist(r$rows))))     # no rule became a row
})

test_that("a line of dashes is a rule and a line with a word in it is not", {
  mk <- function(s) data.frame(text = strsplit(s, " ", fixed = TRUE)[[1]],
                               stringsAsFactors = FALSE)
  expect_true(.doc_is_rule(mk("+-------+-------+")))
  expect_true(.doc_is_rule(mk("__________")))
  expect_true(.doc_is_rule(mk("---- ---- ----")))
  expect_true(.doc_is_rule(mk("........")))
  expect_false(.doc_is_rule(mk("A100 Widgets 1,000.00")))
  expect_false(.doc_is_rule(mk("-5.00")))
  expect_false(.doc_is_rule(mk("-")))                     # too short to be a rule
})

# ---------------------------------------------------------------------------
# Row heights
# ---------------------------------------------------------------------------

test_that("a table whose row heights alternate does not lose half its rows", {
  # THE WORST FAILURE THE FIXTURES FOUND. Rows at 10pt and 26pt alternating: the
  # median gap is 26, six tenths of that is 15.6, and every 10pt gap was swallowed
  # -- six rows came back as four, with "R2 R3" in one cell and "20.00 30.00" in
  # another. Two rows merged is exactly the silent corruption this project exists
  # to prevent, and nothing downstream could have caught it.
  inp <- hard_uneven_rows()
  tb <- .hard_one(inp)
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 6L)
  expect_identical(.hard_cell(r, "Ref"), c("R1", "R2", "R3", "R4", "R5", "R6"))
  expect_identical(.hard_cell(r, "Amount"),
                   c("10.00", "20.00", "30.00", "40.00", "50.00", "60.00"))
  expect_false(any(grepl(" ", .hard_cell(r, "Ref"))))     # nothing was merged
})

test_that("the row tolerance follows the tightest spacing a table uses, not the average", {
  expect_equal(.doc_row_tol(c(100, 110, 136, 146, 172, 182)), 6)   # 10 / 26 mixed
  expect_equal(.doc_row_tol(c(100, 118, 136, 154)), 18 * 0.6)      # evenly set
})

# ---------------------------------------------------------------------------
# The total row
# ---------------------------------------------------------------------------

test_that("a total set apart by an underline and a big gap is still part of the table", {
  # Rows at 12pt, then 45pt of white, an underline, and a two-cell total. Every
  # one of those breaks a run of tabular lines -- so the row somebody actually
  # came for was the one row left out. Was: 4 rows, no total.
  inp <- hard_underlined_total()
  tb <- .hard_one(inp)
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 5L)
  expect_true("TOTAL" %in% .hard_cell(r, "Item"))
  expect_identical(.hard_cell(r, "Amount")[5], "100.00")
  expect_false(any(grepl("_", unlist(r$rows))))            # the underline is not a row
})

test_that("a total row indented past the first column is still reached", {
  # Was: 3 rows, the total dropped. It fills two of the table's bands, which is
  # what the reach-down rule asks for -- the empty first column is not a reason to
  # leave it behind.
  inp <- hard_indented_total()
  tb <- .hard_one(inp)
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 4L)
  expect_true("Total" %in% .hard_cell(r, "Description"))
  expect_identical(.hard_cell(r, "Amount")[4], "1,330.00")
  expect_true(is.na(.hard_cell(r, "Code")[4]) || !nzchar(.hard_cell(r, "Code")[4]))
})

# ---------------------------------------------------------------------------
# Continuation across pages
# ---------------------------------------------------------------------------

test_that("a header that repeats with (continued) after it is still the same table", {
  # Was: THREE tables -- eight rows, then ten, then six -- because the header on
  # page 2 was not identical to page 1's. An eighteen-row schedule split in half
  # is not obviously wrong on screen, which is what makes it worth a test.
  inp <- hard_header_variants()
  props <- propose_tables(inp)
  expect_equal(length(props), 2L)
  expect_equal(props[[1]]$start$page, 1L)
  expect_equal(props[[1]]$end$page, 2L)
  expect_equal(props[[1]]$n_rows, 18L)
  # ...and page 3, which prints the SAME column names in a DIFFERENT ORDER, is a
  # different table. Same geometry is not the same meaning.
  expect_equal(props[[2]]$start$page, 3L)
  expect_identical(.hard_cols(props[[2]]), c("Detail", "Date", "Balance", "Amount"))
})

test_that("two different schedules with the same header are two tables", {
  # Was: ONE table of twelve rows under "AccountOne" -- six of them belonging to
  # another account, with nothing on screen to say so. The deciding fact is that
  # page 2 carries its OWN heading; a real continuation page opens with the
  # repeated column names and nothing above them.
  inp <- hard_same_header_twice()
  props <- propose_tables(inp)
  expect_equal(length(props), 2L)
  expect_identical(vapply(props, function(p) p$name, character(1)),
                   c("AccountOne", "AccountTwo"))
  expect_equal(props[[1]]$n_rows, 6L)
  expect_equal(props[[2]]$n_rows, 6L)
  r <- doc_table_rows(inp, props[[1]], NULL)
  expect_false(any(grepl("^B", .hard_cell(r, "Ref"))))   # none of account two's
})

test_that("a continuation is refused whenever the next page introduces itself", {
  a <- list(anchor = list(header_text = list("Ref", "Item")), .bands = list(
    list(x_min = 30, x_max = 100), list(x_min = 100, x_max = 200)))
  b <- a
  expect_true(.doc_continues(a, b))
  b$.own_heading <- TRUE
  expect_false(.doc_continues(a, b))                     # it has a title of its own
  b$.own_heading <- FALSE
  b$anchor$header_text <- list("Item", "Ref")
  expect_false(.doc_continues(a, b))                     # same words, other order
  b$anchor$header_text <- list("Ref", "Item", "(continued)")
  expect_true(.doc_continues(a, b))
  b$anchor$header_text <- list()                         # no header: geometry only
  expect_true(.doc_continues(a, b))
  b$.bands <- list(list(x_min = 300, x_max = 400), list(x_min = 400, x_max = 500))
  expect_false(.doc_continues(a, b))
})

# ---------------------------------------------------------------------------
# Cells
# ---------------------------------------------------------------------------

test_that("a description that wraps stays with the row it belongs to", {
  # Two ways to get this wrong, and the fix for one caused the other. A loose row
  # tolerance merges the wrap into its row by accident and merges real rows too; a
  # tight one splits it out correctly and then the one-cell line ENDS the table
  # (measured: zero tables proposed). The wrap is folded explicitly instead.
  inp <- hard_wrapped_cells()
  tb <- .hard_one(inp)
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 4L)
  expect_identical(.hard_cell(r, "Description")[1],
                   "PAYMENT TO SUPPLIER FOR PROFESSIONAL SERVICES RENDERED")
  expect_identical(.hard_cell(r, "Description")[3],
                   "DIRECT DEBIT INSURANCE PREMIUM ANNUAL")
  expect_identical(.hard_cell(r, "Date"), c("01/04", "02/04", "03/04", "04/04"))
})

test_that("a two-column table is a table, and a pair of labelled values is not", {
  # Was: ZERO tables. Two columns are allowed on a LONGER run -- four consecutive
  # rows in the same two bands is a schedule, not a coincidence -- which is why
  # the front matter of the ordinary fixture still proposes nothing.
  inp <- hard_wide_numbers()
  tb <- .hard_one(inp)
  expect_equal(length(.doc_columns(tb)), 2L)
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 5L)
  # right-aligned amounts 1.00 to 12,345,678.90: the column's LEFT edge moves 60pt
  # between rows, so a band taken from left edges would lose most of them
  expect_identical(.hard_cell(r, "Value")[2], "12,345,678.90")
  expect_identical(.hard_cell(r, "Value")[5], "3.50")
  expect_equal(nrow(r$spilled), 0L)
  expect_equal(length(propose_tables(doc_fixture_input(), pages = 1L)), 0L)
})

test_that("a space as the thousands separator does not split the figure", {
  inp <- hard_space_thousands()
  r <- doc_table_rows(inp, .hard_one(inp), NULL)
  expect_identical(.hard_cell(r, "Amount"),
                   c("1 234.56", "22 999.00", "8.75", "104 000.00"))
})

test_that("a footnote marker stays with its value rather than becoming a column", {
  inp <- hard_footnotes()
  tb <- .hard_one(inp)
  expect_equal(length(.doc_columns(tb)), 3L)
  r <- doc_table_rows(inp, tb, NULL)
  expect_identical(.hard_cell(r, "Amount")[1], "10.00 *")
  expect_equal(nrow(r$spilled), 0L)
})

test_that("a column that is empty on every row is reported, not dropped", {
  inp <- hard_blank_column()
  tb <- .hard_one(inp)
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 5L)
  expect_true("Note" %in% r$report$column)
  note <- r$report[r$report$column == "Note", , drop = FALSE]
  expect_equal(note$fill_rate, 0)
  expect_true(note$low_fill)                    # flagged, and the column stays
  expect_true(all(is.na(.hard_cell(r, "Note"))))
})

test_that("a two-row header does not put its spanning cell into the data", {
  inp <- hard_two_row_header()
  tb <- .hard_one(inp)
  expect_identical(.hard_cols(tb), c("Account", "Type", "Opening", "Closing"))
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 3L)
  expect_false("Balance" %in% .hard_cell(r, "Account"))
})

# ---------------------------------------------------------------------------
# Pages
# ---------------------------------------------------------------------------

test_that("a running footer is not proposed as a table and is not read into one", {
  inp <- hard_footer_noise()
  props <- propose_tables(inp)
  expect_equal(length(props), 2L)
  for (p in props) {
    r <- doc_table_rows(inp, p, NULL)
    expect_equal(r$n_rows, 5L)
    expect_false(any(grepl("IN-CONFIDENCE|Page-", unlist(r$rows))))
  }
})

test_that("a landscape page in a portrait document is read in its own space", {
  inp <- hard_landscape()
  expect_equal(inp$page_width, c(DOC_W, DOC_H))
  props <- propose_tables(inp, tmpl = list(ref_width = DOC_W, ref_height = DOC_H))
  expect_equal(length(props), 2L)
  for (p in props) expect_equal(p$n_rows, 3L)
})

# ---------------------------------------------------------------------------
# Still limitations -- asserted, so they are a decision and not an oversight
# ---------------------------------------------------------------------------

test_that("two tables printed side by side read as one, and that is visible", {
  # Geometrically this IS one table of six columns, and no rule available here can
  # say otherwise -- the ink is identical to a genuine six-column table. It is
  # drawn on the page as one table the moment it is proposed, which is where a
  # person splits it into two by drawing them. Asserted so the day something can
  # tell them apart, this test is what changes.
  inp <- hard_side_by_side()
  tb <- .hard_one(inp)
  expect_equal(length(.doc_columns(tb)), 6L)
  expect_identical(.hard_cols(tb), c("Code", "Units", "Value", "Code_1", "Rate", "Paid"))
  r <- doc_table_rows(inp, tb, NULL)
  expect_equal(r$n_rows, 5L)                  # nothing is LOST, only mis-grouped
  expect_equal(nrow(r$spilled), 0L)
})

test_that("a spanning header cell becomes the table's proposed name", {
  # "Balance" spans the two columns under it, so it is the nearest short line
  # above the run and gets offered as the name. Wrong, harmless, and one text box
  # on the builder fixes it -- the alternative is offering no name at all.
  expect_identical(.hard_one(hard_two_row_header())$name, "Balance")
})

# ---------------------------------------------------------------------------
# PAGE GEOMETRY -- both of these were found by running the engine over 81 PDFs
# from other people's test suites (tools/corpus). Neither is exotic: a report
# with a landscape appendix, and a page that is simply not A4.
# ---------------------------------------------------------------------------

# .mixed_input(...) -- pages of DIFFERENT sizes in one document, which is the
# case a single reference frame cannot describe.
.mixed_input <- function() {
  land <- doc_page(
    doc_L(40, 60, "Regional summary"),
    doc_L(40, 90, "Region"), doc_L(300, 90, "Units"), doc_L(500, 90, "Value"),
    doc_L(40, 110, "North"), doc_R(340, 110, "12"), doc_R(560, 110, "1,000.00"),
    doc_L(40, 130, "South"), doc_R(340, 130, "34"), doc_R(560, 130, "2,000.00"),
    doc_L(40, 150, "East"),  doc_R(340, 150, "56"), doc_R(560, 150, "3,000.00"))
  port <- doc_page(
    doc_L(40, 60, "Notes to the summary"),
    doc_L(40, 90, "Note"), doc_L(300, 90, "Detail"),
    doc_L(40, 110, "1"), doc_L(300, 110, "Provisional"),
    doc_L(40, 130, "2"), doc_L(300, 130, "Restated"),
    doc_L(40, 150, "3"), doc_L(300, 150, "Rounded"),
    doc_L(40, 170, "4"), doc_L(300, 170, "Estimated"),
    doc_L(40, 190, "5"), doc_L(300, 190, "Audited"))
  i <- doc_input(list(land, port))
  i$page_width  <- c(DOC_H, DOC_W)          # page 1 landscape, page 2 portrait
  i$page_height <- c(DOC_W, DOC_H)
  i
}

test_that("a landscape page is not squashed into a portrait frame", {
  i <- .mixed_input()
  # The frame is page 1's shape. Page 2 is the other way round, so scaling it
  # into that frame would stretch every word sideways and squash it vertically --
  # and every column band on that page would then claim the wrong words.
  tmpl <- list(ref_width = DOC_H, ref_height = DOC_W)
  native <- .doc_page_words(i, 2L, NULL)
  framed <- .doc_page_words(i, 2L, tmpl)
  expect_equal(framed$x, native$x)
  expect_equal(framed$y, native$y)
  # ...while a page the SAME way round as the frame is still mapped into it,
  # which is the thing the frame exists for.
  wide <- list(ref_width = DOC_H * 1.1, ref_height = DOC_W * 1.1)
  expect_gt(max(.doc_page_words(i, 1L, wide)$x), max(.doc_page_words(i, 1L, NULL)$x))
})

test_that("what is proposed on a mixed-orientation document is what is read back", {
  i <- .mixed_input()
  props <- propose_tables(i)
  expect_gte(length(props), 2L)
  tmpl <- document_template_from_proposal(
    id = "mixed", bank = "B", statement_type = "r",
    phrases = "Regional summary", tables = props, pairs = list(),
    doc_pages = .doc_npages(i))
  tmpl$ref_width <- DOC_H; tmpl$ref_height <- DOC_W      # page 1's shape
  ext <- extract_document(i, tmpl)
  for (k in seq_along(props))
    expect_equal(as.integer(ext$summary$rows[k]), as.integer(props[[k]]$n_rows),
                 info = sprintf("table '%s'", props[[k]]$name))
  expect_equal(sum(as.integer(ext$summary$unclaimed_words)), 0L)
})

test_that("the bottom of the page is the document's own, not A4's", {
  # band_bot decides whether a window is OPEN-ENDED, and an open window is the
  # one the row-run rule has to stop by watching the rows. Falling back to A4
  # made a table ending 741pt down a 612pt-tall landscape page look like it
  # stopped comfortably short of the bottom, so its end was treated as exact and
  # the stop rule never ran.
  i <- .mixed_input()
  tab <- list(name = "Regional summary",
              start = list(page = 1L, y = 80), end = list(page = 1L, y = DOC_W),
              header_rows = 1L, follow = FALSE,
              columns = list(list(name = "Region", x_min = 30, x_max = 280),
                             list(name = "Units", x_min = 280, x_max = 400),
                             list(name = "Value", x_min = 400, x_max = 580)))
  loc <- doc_locate_table(i, tab, NULL)
  expect_true(isTRUE(loc$windows[[1]]$open_end))
  # and a table that genuinely stops short of the bottom is still exact
  tab$end$y <- 200
  expect_false(isTRUE(doc_locate_table(i, tab, NULL)$windows[[1]]$open_end))
})

test_that("a one-column table is not ended by the first paragraph gap", {
  # The stop rule's floor was "at least two filled columns", which a one-column
  # table can never reach -- so every line of one looked wrong and the first gap
  # ended it. A block of prose or a list IS a one-column table, and they are
  # everywhere.
  parts <- list(doc_L(40, 60, "Improved operation scenario"))
  y <- 90
  for (k in 1:8) { parts <- c(parts, list(doc_L(40, y, sprintf("line %d of the first paragraph", k)))); y <- y + 14 }
  y <- y + 34                                    # a paragraph break: a big gap
  for (k in 1:6) { parts <- c(parts, list(doc_L(40, y, sprintf("line %d of the second paragraph", k)))); y <- y + 14 }
  i <- doc_input(list(do.call(doc_page, parts)))
  tab <- list(name = "Improved operation scenario",
              start = list(page = 1L, y = 80), end = list(page = 1L, y = DOC_H),
              header_rows = 0L, follow = FALSE,
              columns = list(list(name = "text", x_min = 0, x_max = DOC_W)))
  r <- doc_table_rows(i, tab, NULL)
  expect_equal(nrow(r$stops), 0L)
  expect_equal(r$n_rows, 14L)
})

# ---------------------------------------------------------------------------
# A ROTATED PAGE FACES THE OTHER WAY FROM ITS PAGE BOX
#
# pdf_pagesize reports the page box before /Rotate is applied; the text
# extractor and the renderer both report after it. Where they disagree,
# everything that trusts the box is measuring at right angles to the words --
# the builder draws a 612-wide picture whose own image is 792 wide, so nothing
# drawn on it lines up, and the band frame squashes every word on the page.
# Found on 11 pages of 212 across a corpus of other people's PDFs.
# ---------------------------------------------------------------------------

test_that("the page's space is the one its words are measured in", {
  wf <- function(x, y) data.frame(x = 0, y = 0, width = x, height = y)
  # ordinary portrait page, words inside it: left alone
  expect_equal(.pdf_page_space(612, 792, wf(500, 700)), c(612, 792))
  # words that do not fit the box but fit it on its side: turned
  expect_equal(.pdf_page_space(612, 792, wf(745, 501)), c(792, 612))
  expect_equal(.pdf_page_space(1008, 612, wf(586, 973)), c(612, 1008))
  # right on the boundary, within the two-point slack: still left alone
  expect_equal(.pdf_page_space(612, 792, wf(613, 792)), c(612, 792))
  # FITS NEITHER WAY -- something else is wrong with the page, and turning it
  # would only be a second wrong thing. Left exactly as it was.
  expect_equal(.pdf_page_space(612, 792, wf(900, 900)), c(612, 792))
  # nothing to go on: left alone
  expect_equal(.pdf_page_space(612, 792, NULL), c(612, 792))
  expect_equal(.pdf_page_space(612, 792, wf(10, 10)[0, ]), c(612, 792))
  # a page size that is not a page size is not repaired here
  expect_true(all(is.na(.pdf_page_space(NA, NA, wf(10, 10)))))
})

test_that("the picture and the page size cannot disagree about which way it faces", {
  # render_page_view takes its w/h from pdf_pagesize but its IMAGE from poppler,
  # which applies the rotation. The raster cannot be wrong about its own shape,
  # so it decides -- otherwise the caller draws a landscape image into a portrait
  # coordinate system and every box lands somewhere else.
  src <- readLines(file.path(engine_root(), "R", "read_input.R"), warn = FALSE)
  i <- grep("^render_page_view <- function", src)[1]
  skip_if(is.na(i), "render_page_view not found")
  blk <- paste(src[i:min(length(src), i + 40L)], collapse = "\n")
  expect_match(blk, "xor\\(ncol\\(ras\\) > nrow\\(ras\\), w > h\\)")
  expect_match(blk, "out <- list\\(ras = ras, w = w, h = h")
  expect_false(grepl("w = sz\\$width\\[page\\]", blk))
})

# ---------------------------------------------------------------------------
# THE BOTTOM EDGE OF THE PAGES IN BETWEEN
#
# Reported: "there's lots of text at the bottom of some pages which is not the
# table, but the table still spans multiple pages". "Where it ends" is a place on
# ONE page -- the last one. On every page between, doc_locate_table() closes the
# window at band$y_max, which is the bottom of the paper unless somebody says
# otherwise, so a footer under the table is read in as rows on every page but the
# last. The guide asks for that edge now; this is the proof that answering it
# does something.
# ---------------------------------------------------------------------------

# .bt_doc() -- three pages. Same table on each: a heading, four rows of figures,
# and then a page footer 120pt lower down that is not part of it.
.bt_doc <- function() {
  pg <- function(n) doc_page(
    doc_L(60, 90, "Region"), doc_L(300, 90, "Amount"),
    doc_L(60, 130, paste0("North", n)),  doc_R(360, 130, "1,000.00"),
    doc_L(60, 155, paste0("South", n)),  doc_R(360, 155, "2,000.00"),
    doc_L(60, 180, paste0("East", n)),   doc_R(360, 180, "3,000.00"),
    doc_L(60, 205, paste0("West", n)),   doc_R(360, 205, "4,000.00"),
    # THE FOOTER. Set at the pitch of the rows above it, which is the case that
    # matters: a footer with a wide gap above it is stopped by the row-run rule
    # already, so the only footers that reach the output are the ones printed
    # close enough to look like the next row. A source line and a page number
    # under a government table are exactly that.
    doc_L(60, 232, "Source: Ministry of Finance"),
    doc_R(400, 232, paste0("Page ", n, " of 3")),
    doc_L(60, 257, "(1) Revised estimates"),
    doc_L(60, 282, "(2) Excludes capital transfers"))
  doc_input(list(pg(1), pg(2), pg(3)))
}

.bt_table <- function(band_bot = NULL) {
  band <- list(x_min = 55, x_max = 420, y_min = 85)
  if (!is.null(band_bot)) band$y_max <- band_bot
  list(name = "regions", band = band,
       columns = list(
         list(name = "region", x_min = 55, x_max = 200, type = "text"),
         list(name = "amount", x_min = 290, x_max = 420, type = "money")),
       header_rows = 1L,
       anchor = list(header_text = list("Region", "Amount")),
       start = list(page = 1L, y = 85),
       end   = list(page = 3L, y = 215))
}

test_that("without a bottom edge, a footer under a multi-page table is read as rows", {
  # NOT a wish: this is what the reader does, and it is why the step exists.
  r <- doc_table_rows(.bt_doc(), .bt_table(), NULL)
  txt <- paste(unlist(lapply(r$rows, as.character)), collapse = " ")
  expect_true(grepl("Ministry of Finance", txt, fixed = TRUE))
})

test_that("a bottom edge keeps the footer out of every page but the last", {
  r <- doc_table_rows(.bt_doc(), .bt_table(band_bot = 230), NULL)
  txt <- paste(unlist(lapply(r$rows, as.character)), collapse = " ")
  expect_false(grepl("Ministry of Finance", txt, fixed = TRUE))
  expect_false(grepl("Page 1 of 3", txt, fixed = TRUE))
  # and the real rows all survive -- four a page, three pages
  expect_equal(nrow(r$rows), 12L)
  for (n in 1:3) expect_true(grepl(paste0("North", n), txt, fixed = TRUE))
})

test_that("the bottom edge does not truncate the LAST page, where the end rules", {
  # The last page is closed by `end`, not by the band. A bottom edge ABOVE the
  # declared end must not quietly shorten it.
  r <- doc_table_rows(.bt_doc(), .bt_table(band_bot = 230), NULL)
  txt <- paste(unlist(lapply(r$rows, as.character)), collapse = " ")
  expect_true(grepl("West3", txt, fixed = TRUE))
})

test_that("the bottom edge survives the trip from the screen into the template", {
  # THE FAULT THAT MADE THE STEP POINTLESS. The builder set band$y_max on the
  # draft, drew it on the page and wrote it in the "where it starts and stops"
  # panel -- and document_template_from_proposal() dropped it, so the reader
  # never saw it and the footer came back in the rows anyway. A control that
  # changes nothing is worse than no control: the question gets answered and
  # stays answered wrongly.
  drafted <- .bt_table(band_bot = 230)
  t <- document_template_from_proposal(
    id = "bt", bank = "Ministry", statement_type = "outlay",
    phrases = "Region", tables = list(drafted), pairs = list(), doc_pages = 3L)
  tab <- t$tables[[1]]
  expect_equal(.doc_num(tab$band$y_max), 230)
  # ...and it is the FULL trip that matters, not the one field
  t$ref_width <- DOC_W; t$ref_height <- DOC_H
  ext <- extract_document(.bt_doc(), t)
  txt <- paste(unlist(lapply(ext$tables[[1]]$rows, as.character)), collapse = " ")
  expect_false(grepl("Ministry of Finance", txt, fixed = TRUE))
  expect_equal(nrow(ext$tables[[1]]$rows), 12L)

  # ...and with no bottom edge SET, none is invented onto the template. An absent
  # y_max means "to the bottom of the page", which is a different statement from
  # "to 0" -- so only the numbers somebody actually set are written.
  plain <- document_template_from_proposal(
    id = "bt", bank = "Ministry", statement_type = "outlay",
    phrases = "Region", tables = list(.bt_table()), pairs = list(), doc_pages = 3L)
  expect_null(plain$tables[[1]]$band$y_max)
  expect_equal(.doc_num(plain$tables[[1]]$band$y_min), 85)   # what WAS set, kept
  # a table with no band at all carries none
  bare <- .bt_table(); bare$band <- NULL
  none <- document_template_from_proposal(
    id = "bt", bank = "Ministry", statement_type = "outlay",
    phrases = "Region", tables = list(bare), pairs = list(), doc_pages = 3L)
  expect_null(none$tables[[1]]$band)
})

# ---------------------------------------------------------------------------
# A ROW BROKEN ACROSS TWO LINES, INSIDE ONE WORD
#
# Reported: an email wrapped by the PDF came out "xys@ gmail.com". A space is
# right nearly always -- a wrapped description is two words -- and wrong exactly
# when the break falls inside one token.
# ---------------------------------------------------------------------------

test_that("a wrapped cell joins with a space, except where the token was broken", {
  expect_identical(.doc_join_wrapped("xys@", "gmail.com"), "xys@gmail.com")
  expect_identical(.doc_join_wrapped("firstname.last", "@co.nz"), "firstname.last@co.nz")
  expect_identical(.doc_join_wrapped("www.example", ".com"), "www.example.com")
  expect_identical(.doc_join_wrapped("NW-99-", "9999"), "NW-99-9999")
  expect_identical(.doc_join_wrapped("consoli-", "dated"), "consoli-dated")
  expect_identical(.doc_join_wrapped("C:\\Users", "\\beth"), "C:\\Users\\beth")
  # ...and the ordinary case is still a space
  expect_identical(.doc_join_wrapped("Payment to", "Acme Ltd"), "Payment to Acme Ltd")
  expect_identical(.doc_join_wrapped("Direct debit", "01 Feb"), "Direct debit 01 Feb")
  # A TRAILING FULL STOP IS DELIBERATELY NOT A JOIN. "Acme Ltd." followed by a
  # new sentence is far commoner than a token broken after a dot, and gluing
  # those is the worse mistake of the two.
  expect_identical(.doc_join_wrapped("Acme Ltd.", "Payment received"),
                   "Acme Ltd. Payment received")
  # nothing to join
  expect_identical(.doc_join_wrapped("", "gmail.com"), "gmail.com")
  expect_identical(.doc_join_wrapped("xys@", ""), "xys@")
})

test_that("the wrapped email survives the reader, not just the helper", {
  # The reported shape: a two-column table whose second row wraps its address.
  inp <- doc_input(list(doc_page(
    doc_L(60, 90, "Name"), doc_L(240, 90, "Contact"),
    doc_L(60, 130, "Beth Adams"), doc_L(240, 130, "beth.adams@"),
    doc_L(248, 146, "example.co.nz"),
    doc_L(60, 180, "Sam Okafor"), doc_L(240, 180, "sam@example.co.nz"))))
  tab <- list(name = "contacts",
    columns = list(list(name = "name", x_min = 55, x_max = 220, type = "text"),
                   list(name = "contact", x_min = 230, x_max = 420, type = "text")),
    header_rows = 1L, anchor = list(header_text = list("Name", "Contact")),
    start = list(page = 1L, y = 85), end = list(page = 1L, y = 200))
  r <- doc_table_rows(inp, tab, NULL)
  expect_equal(nrow(r$rows), 2L)
  expect_identical(r$rows$contact[1], "beth.adams@example.co.nz")
  expect_identical(r$rows$contact[2], "sam@example.co.nz")
  expect_identical(r$rows$name[1], "Beth Adams")
})

test_that("a value box drawn round a wrapped email reads it whole", {
  inp <- doc_input(list(doc_page(
    doc_L(40, 100, "Email"), doc_L(120, 100, "beth.adams@"),
    doc_L(128, 116, "example.co.nz"),
    doc_L(40, 160, "Consolidated position report"))))
  lb <- list(page = 1L, x_min = 39, x_max = 80, y_min = 99, y_max = 110)
  vb <- list(page = 1L, x_min = 118, x_max = 230, y_min = 96, y_max = 124)
  tmpl <- list(id = "e", mode = "document", format = "pdf", version = 1,
    ref_width = DOC_W, ref_height = DOC_H,
    fingerprint = list(page_contains_all = list("Consolidated position report")),
    tables = list(),
    pairs = list(email = list(label_text = "Email", label = lb, value = vb,
                              type = "text", where = .doc_pair_rel(lb, vb))))
  expect_identical(doc_pairs(inp, tmpl)$value[1], "beth.adams@example.co.nz")
})

# ---------------------------------------------------------------------------
# A WRAPPED CELL IS NOT ALWAYS INDENTED
#
# Reported: "one row with data on 3 different lines coming back as 3 rows. This
# edge case is where it's only two columns, where the first column's first row --
# and only row -- has data on three printed lines."
#
# The indent rule came from a bank statement, where the description column is set
# in from the date and its second line sits under the words rather than under the
# date. A REPORT left-aligns the wrapped line under the first, at the same x.
# ---------------------------------------------------------------------------

.wr_tab <- function(end_y = 215) list(name = "t",
  columns = list(list(name = "description", x_min = 55, x_max = 380, type = "text"),
                 list(name = "amount", x_min = 390, x_max = 440, type = "money")),
  header_rows = 1L, anchor = list(header_text = list("Description", "Amount")),
  start = list(page = 1L, y = 85), end = list(page = 1L, y = end_y))

test_that("a cell wrapped over three LEFT-ALIGNED lines is one row, not three", {
  inp <- doc_input(list(doc_page(
    doc_L(60, 90, "Description"), doc_R(430, 90, "Amount"),
    doc_L(60, 130, "Professional services rendered in"),
    doc_L(60, 146, "connection with the matter and"),
    doc_L(60, 162, "associated disbursements"),
    doc_R(430, 130, "1,250.00"),
    doc_L(60, 200, "Filing fee"), doc_R(430, 200, "80.00"))))
  r <- doc_table_rows(inp, .wr_tab(), NULL)
  expect_equal(nrow(r$rows), 2L)
  expect_identical(r$rows$description[1],
    "Professional services rendered in connection with the matter and associated disbursements")
  expect_identical(r$rows$amount[1], "1,250.00")
  expect_identical(r$rows$description[2], "Filing fee")
})

test_that("the THIRD wrapped line is measured from the line above it, not from the row", {
  # A gap measured from the row's top grows with every fold, so the third line
  # stops looking like a wrap and comes back as a row of its own. Two lines fold
  # and three do not is the shape of that bug; assert three.
  inp <- doc_input(list(doc_page(
    doc_L(60, 90, "Description"), doc_R(430, 90, "Amount"),
    doc_L(60, 130, "one"), doc_L(60, 146, "two"), doc_L(60, 162, "three"),
    doc_R(430, 130, "9.00"))))
  r <- doc_table_rows(inp, .wr_tab(180), NULL)
  expect_equal(nrow(r$rows), 1L)
  expect_identical(r$rows$description[1], "one two three")
})

test_that("a left-aligned line set APART is a row of its own, not a wrap", {
  # The guard that makes the un-indented rule safe: a sub-heading or a total is
  # printed with MORE air above it, not less.
  inp <- doc_input(list(doc_page(
    doc_L(60, 90, "Description"), doc_R(430, 90, "Amount"),
    doc_L(60, 130, "Filing fee"), doc_R(430, 130, "80.00"),
    doc_L(60, 152, "Court fee"),  doc_R(430, 152, "60.00"),
    doc_L(60, 200, "Disbursements"),
    doc_L(60, 222, "Courier"), doc_R(430, 222, "12.00"))))
  r <- doc_table_rows(inp, .wr_tab(235), NULL)
  expect_equal(nrow(r$rows), 4L)
  expect_identical(r$rows$description[3], "Disbursements")
  expect_true(is.na(r$rows$amount[3]) || !nzchar(r$rows$amount[3]))
})

test_that("the un-indented rule cannot fold a ONE-COLUMN table of prose", {
  # The regression this guard exists for: every line of a prose block is
  # left-aligned at the same x, a leading apart, and fills the one column there
  # is. Measured before the guard: a 14-line block became 1 row.
  parts <- list(doc_L(40, 60, "Improved operation scenario"))
  y <- 90
  for (k in 1:8) { parts <- c(parts, list(doc_L(40, y, sprintf("line %d of the first paragraph", k)))); y <- y + 14 }
  y <- y + 34
  for (k in 1:6) { parts <- c(parts, list(doc_L(40, y, sprintf("line %d of the second paragraph", k)))); y <- y + 14 }
  i <- doc_input(list(do.call(doc_page, parts)))
  tab <- list(name = "prose", start = list(page = 1L, y = 80),
              end = list(page = 1L, y = DOC_H), header_rows = 0L, follow = FALSE,
              columns = list(list(name = "text", x_min = 0, x_max = DOC_W)))
  expect_equal(doc_table_rows(i, tab, NULL)$n_rows, 14L)
})

test_that("the wrap rule itself answers each shape correctly", {
  d_same <- data.frame(x = 60, text = "more words", stringsAsFactors = FALSE)
  d_in   <- data.frame(x = 90, text = "more words", stringsAsFactors = FALSE)
  # indented: a generous gap is still a wrap
  expect_true(.doc_is_wrap(d_in, 60, 18, 14, 1L, 2L, 2L))
  # same left edge: tight gap folds, loose gap does not
  expect_true(.doc_is_wrap(d_same, 60, 15, 14, 1L, 2L, 2L))
  expect_false(.doc_is_wrap(d_same, 60, 30, 14, 1L, 2L, 2L))
  # ...and never on a one-column table, nor under a row that filled only one
  expect_false(.doc_is_wrap(d_same, 60, 15, 14, 1L, 1L, 2L))
  expect_false(.doc_is_wrap(d_same, 60, 15, 14, 1L, 2L, 1L))
  # a line that fills two columns is a row whatever else is true
  expect_false(.doc_is_wrap(d_same, 60, 15, 14, 2L, 2L, 2L))
})
