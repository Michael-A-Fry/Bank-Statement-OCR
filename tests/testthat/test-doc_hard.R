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
  # A LINE HAS A TYPE HEIGHT. These fixtures had none, which is a shape no real
  # caller produces - every words frame carries `height` - and it mattered the
  # moment the rule stopped measuring against a page-wide pitch statistic and
  # started measuring against the two lines themselves.
  d_same <- data.frame(x = 60, text = "more words", height = 9, stringsAsFactors = FALSE)
  d_in   <- data.frame(x = 90, text = "more words", height = 9, stringsAsFactors = FALSE)
  # 9pt type under a 9pt line: one line of room is 18, so the ceilings are
  # 25.2 for the indented shape and 20.7 for the same-left-edge one.
  # indented: a generous gap is still a wrap
  expect_true(.doc_is_wrap(d_in, 60, 18, 1L, 2L, 2L, 9))
  # same left edge: tight gap folds, loose gap does not
  expect_true(.doc_is_wrap(d_same, 60, 15, 1L, 2L, 2L, 9))
  expect_false(.doc_is_wrap(d_same, 60, 30, 1L, 2L, 2L, 9))
  # ...and never on a one-column table, nor under a row that filled only one
  expect_false(.doc_is_wrap(d_same, 60, 15, 1L, 1L, 2L, 9))
  expect_false(.doc_is_wrap(d_same, 60, 15, 1L, 2L, 1L, 9))
  # a line that fills two columns is a row whatever else is true
  expect_false(.doc_is_wrap(d_same, 60, 15, 2L, 2L, 2L, 9))
  # NO TYPE HEIGHT AT ALL is a refusal, not a guess. A wrong fold merges two real
  # rows into one, which is a wrong figure nobody can see.
  d_noh <- data.frame(x = 60, text = "more words", stringsAsFactors = FALSE)
  expect_false(.doc_is_wrap(d_noh, 60, 15, 1L, 2L, 2L, NA_real_))
})

# ---------------------------------------------------------------------------
# ONE ROW PRINTED OVER THREE LINES.
#
# Reported: "one row with data on 3 different lines coming back as 3 rows. This
# edge case is where it's only two columns, where the first column's first row
# and only row data is [on the first line]."
#
# The predicate was right; the number handed to it was measured over the wrong
# population. `pitch` was .doc_pitch(q = 0.25) over EVERY word in the reading
# window, and an open-ended window is the whole page - so a footnote block set
# tighter than the table dragged the quartile under the table's own leading, the
# ceiling fell with it, and every wrap was rejected.
# ---------------------------------------------------------------------------

test_that("a cell wrapped over three lines is one row, footnotes or not", {
  W <- function(text, x, y, h = 9) data.frame(text = text, x = x, y = y,
        width = nchar(text) * 5, height = h, stringsAsFactors = FALSE)
  mk <- function(footnote) {
    w <- rbind(
      W("Item", 60, 100),  W("Detail", 200, 100),
      W("A1", 60, 118),    W("the first part of a long", 200, 118),
                           W("description that keeps", 200, 130),
                           W("going onto a third line", 200, 142),
      W("A2", 60, 162),    W("short one", 200, 162))
    # set TIGHTER than the table's 12pt leading - this is what used to poison the
    # page-wide quartile and reject every wrap above it
    if (footnote) w <- rbind(w, W("note one", 60, 300, 6),
                                W("note two", 60, 306, 6),
                                W("note three", 60, 312, 6))
    list(kind = "pdf", words = list(w), pages = "x",
         page_width = 595, page_height = 842)
  }
  tab <- list(name = "T", start = list(page = 1, y = 95), end = list(page = 1, y = 200),
    header_rows = 1L, anchor = list(header_text = list("Item", "Detail")),
    columns = list(list(name = "Item", x_min = 55, x_max = 150),
                   list(name = "Detail", x_min = 195, x_max = 400)))
  tmpl <- list(ref_width = 595, ref_height = 842)
  for (footnote in c(TRUE, FALSE)) {
    r <- doc_table_rows(mk(footnote), tab, tmpl)$rows
    expect_equal(nrow(r), 2L)
    expect_equal(r$Item, c("A1", "A2"))
    # the three printed lines came back as ONE cell, joined with single spaces
    expect_equal(r$Detail[1],
      "the first part of a long description that keeps going onto a third line")
  }
})

# ---------------------------------------------------------------------------
# REPRINTED HEADINGS ON CONTINUATION PAGES.
#
# Reported: "sometimes there's headers on the next x pages that are being pulled
# as rows."
#
# Exactly ONE thing used to close the top of a continuation page's window: a
# >=0.6 wording match of the template's remembered anchor$header_text. When that
# missed, the window fell back to band$y_min - which nothing ever writes, so 0 -
# and `skip` was hard-coded to 0 for every page after the first. The window
# became the whole page and the reprinted heading was read as a row.
#
# The commonest way to miss needs no bad luck: the proposer marks a table with no
# figures on its heading line as has_header = FALSE, writing header_rows: 0 and
# anchor.header_text: [], so the wording test can never fire at all.
# ---------------------------------------------------------------------------

.a1_doc <- function(n_pages = 3L) {
  W <- function(text, x, y, h = 9) data.frame(text = text, x = x, y = y,
        width = nchar(text) * 5, height = h, stringsAsFactors = FALSE)
  page <- function(n, rows) {
    w <- rbind(W("Security", 60, 100), W("Units", 240, 100))   # reprinted heading
    y <- 118
    for (r in rows) { w <- rbind(w, W(r[1], 60, y), W(r[2], 240, y)); y <- y + 16 }
    rbind(w, W("Page", 60, 760, 7), W(as.character(n), 100, 760, 7))  # footer
  }
  vals <- list(list(c("AAA", "10"), c("BBB", "20")),
               list(c("CCC", "30"), c("DDD", "40")),
               list(c("EEE", "50"), c("FFF", "60")))
  list(kind = "pdf",
       words = lapply(seq_len(n_pages), function(i) page(i, vals[[i]])),
       pages = paste0("p", seq_len(n_pages)),
       page_width = rep(595, n_pages), page_height = rep(842, n_pages))
}
.a1_tab <- function(header_text = list()) list(
  name = "Holdings", start = list(page = 1, y = 100), end = list(page = 3, y = 200),
  band = list(y_min = 0, y_max = 740), header_rows = 1L,
  anchor = list(header_text = header_text),
  columns = list(list(name = "Security", x_min = 55, x_max = 200),
                 list(name = "Units", x_min = 235, x_max = 300)))

test_that("a heading reprinted on every page is not read as rows, with nothing remembered", {
  inp <- .a1_doc(); tmpl <- list(ref_width = 595, ref_height = 842)
  tab <- .a1_tab(list())                       # the template remembers NO wording
  loc <- doc_locate_table(inp, tab, tmpl)
  # the heading is learned off the table's OWN first page, so it is right even
  # when the template was drawn on a different copy
  expect_identical(loc$header_sig, "securityunits")
  r <- doc_table_rows(inp, tab, tmpl, loc)$rows
  expect_equal(nrow(r), 6L)
  expect_false(any(r$Security %in% "Security"))
  expect_equal(r$Units, c("10", "20", "30", "40", "50", "60"))
  # ...and it SAYS so, because "those lines are not rows" is a fact a reviewer needs
  expect_match(loc$detail, "heading is printed again on pages 2, 3")
})

test_that("the remembered wording still wins when it is there, and the signature is a fallback only", {
  inp <- .a1_doc(); tmpl <- list(ref_width = 595, ref_height = 842)
  loc <- doc_locate_table(inp, .a1_tab(list("Security", "Units")), tmpl)
  expect_identical(loc$anchor, "header")
  r <- doc_table_rows(inp, .a1_tab(list("Security", "Units")), tmpl, loc)$rows
  expect_equal(nrow(r), 6L)
})

test_that("a signature is refused when the learned heading carries a figure", {
  # A line with money in it is DATA. A signature made of data would match data,
  # and deleting a real row is the one thing this must never do.
  d <- list(structure(data.frame(text = c("Opening", "1,240.55"), x = c(60, 240),
                                 y = c(100, 100), width = 40, height = 9,
                                 stringsAsFactors = FALSE), top = 100))
  expect_identical(.doc_header_printed(d, 100, 1L), character(0))
})

test_that("the signature match is whole-line, so a data row cannot contain it", {
  sig <- "securityunits"
  row <- list(structure(data.frame(text = c("AAA", "10"), x = c(60, 240),
                                   y = 118, width = 40, height = 9,
                                   stringsAsFactors = FALSE), top = 118))
  expect_null(.doc_find_printed_header(row, sig))
  hdr <- list(structure(data.frame(text = c("Security", "Units"), x = c(60, 240),
                                   y = 100, width = 40, height = 9,
                                   stringsAsFactors = FALSE), top = 100))
  found <- .doc_find_printed_header(hdr, sig)
  expect_equal(found$n_lines, 1L)
})

test_that("a page reprinting only the first line of a two-line heading skips one, not two", {
  sig <- c("securityunits", "codeandname")
  lines <- list(
    structure(data.frame(text = c("Security", "Units"), x = c(60, 240), y = 100,
                         width = 40, height = 9, stringsAsFactors = FALSE), top = 100),
    structure(data.frame(text = c("AAA", "10"), x = c(60, 240), y = 116,
                         width = 40, height = 9, stringsAsFactors = FALSE), top = 116))
  found <- .doc_find_printed_header(lines, sig)
  expect_equal(found$n_lines, 1L)     # not 2 - the second line is a data row
})

# ---------------------------------------------------------------------------
# THE SAME TABLE, PRINTED FURTHER ACROSS THE PAGE.
#
# A table is found by its heading, which moves the reading window VERTICALLY to
# wherever it turned up. Nothing moved it horizontally, so the bands stayed where
# they were drawn on the one example document. Measured before the fix, on bands
# drawn at 55-150 and 195-260:
#
#   shift  located by  confidence  what came out
#     0pt  header      1.00        ABC/100  DEF/250  GHI/375
#    60pt  header      1.00        ABC/NA   DEF/NA   GHI/NA
#   120pt  header      1.00        NA/ABC   NA/DEF   NA/GHI    <- Code in Units
#
# Found with confidence 1.00, and Units reading as a full column of clean values
# every one of which was wrong.
# ---------------------------------------------------------------------------

.crux_doc <- function(dx = 0) {
  rows <- list(c("Code", "Units"), c("ABC", "100"), c("DEF", "250"), c("GHI", "375"))
  w <- do.call(rbind, lapply(seq_along(rows), function(i) data.frame(
    text = rows[[i]], x = c(60, 200) + dx, y = 100 + (i - 1) * 20,
    width = c(40, 40), height = 10, stringsAsFactors = FALSE)))
  list(kind = "pdf", words = list(w), pages = "x",
       page_width = 595, page_height = 842)
}
.crux_tab <- list(name = "Holdings",
  start = list(page = 1, y = 95), end = list(page = 1, y = 200), header_rows = 1L,
  anchor = list(header_text = list("Code", "Units")),
  columns = list(list(name = "Code", x_min = 55, x_max = 150),
                 list(name = "Units", x_min = 195, x_max = 260)))
.crux_tmpl <- list(ref_width = 595, ref_height = 842)

test_that("a table printed further across the page still reads into the right columns", {
  for (dx in c(0, 20, 40, 60, 80, 120, 200)) {
    inp <- .crux_doc(dx)
    rr <- doc_table_rows(inp, .crux_tab, .crux_tmpl)
    expect_identical(rr$rows$Code, c("ABC", "DEF", "GHI"), info = paste("shift", dx))
    expect_identical(rr$rows$Units, c("100", "250", "375"), info = paste("shift", dx))
    expect_equal(nrow(rr$spilled), 0L, info = paste("shift", dx))
  }
})

test_that("a table that already fits is never moved", {
  # The band is drawn WIDER than the heading word inside it, and not
  # symmetrically, so the raw difference between a heading cell's centre and its
  # band's centre carries a constant that has nothing to do with this document -
  # measured at -15pt on a table that had not moved at all. Asking "is anything
  # actually outside its band?" first is what separates the two.
  for (dx in c(0, 20, 40)) {
    loc <- doc_locate_table(.crux_doc(dx), .crux_tab, .crux_tmpl)
    expect_equal(loc$band_shift, 0, info = paste("shift", dx))
  }
})

test_that("a correction nobody can see is not made silently", {
  loc <- doc_locate_table(.crux_doc(120), .crux_tab, .crux_tmpl)
  expect_gt(loc$band_shift, 0)
  expect_match(loc$detail, "further right than the example")
  expect_match(loc$detail, "columns were moved to match")
})

test_that("the shift is refused when it cannot be trusted", {
  hdr <- function(...) {
    d <- data.frame(..., stringsAsFactors = FALSE)
    structure(d, top = min(d$y))
  }
  cols2 <- .crux_tab$columns
  # ONE column matched is a coincidence, not a measurement
  one <- hdr(text = "Code", x = 300, y = 100, width = 40, height = 10)
  expect_equal(.doc_band_shift(one, cols2), 0)
  # columns that DISAGREE mean this is not the same table shape - moving the
  # bands on that evidence would make it worse, not better
  disagree <- hdr(text = c("Code", "Units"), x = c(300, 210), y = 100,
                  width = 40, height = 10)
  expect_equal(.doc_band_shift(disagree, cols2), 0)
  # and nothing to match at all
  expect_equal(.doc_band_shift(NULL, cols2), 0)
  expect_equal(.doc_band_shift(one, list()), 0)
})

test_that("a table read from where it sat on the example is left alone", {
  # No heading was found, so there is nothing to measure the shift against. The
  # honest answer is to change nothing.
  blind <- .crux_tab; blind$anchor <- list(header_text = list("Nothing", "Here"))
  loc <- doc_locate_table(.crux_doc(120), blind, .crux_tmpl)
  expect_false(identical(loc$anchor, "header"))
  expect_equal(loc$band_shift, 0)
})

# ---------------------------------------------------------------------------
# AN UNLABELLED TOTAL FOLDED INTO THE ROW ABOVE IT (register I1)
#
# The exact mirror of the three-line row above. A right-aligned figure alone on a
# line is "indented" by construction, and the indented branch of the wrap rule
# had no guards at all, so the total was appended to the last row's amount -- and
# the parsed companion then took the TOTAL, so the row lost its own figure and
# the total lost its row. Two wrong figures from one fold.
# ---------------------------------------------------------------------------

hard_unlabelled_total <- function() doc_input(list(doc_page(
  doc_L(40, 60, "Item"), doc_R(500, 60, "Value"),
  doc_L(40, 80, "Alpha"),  doc_R(500, 80, "1,000.00"),
  doc_L(40, 98, "Beta"),   doc_R(500, 98, "2,000.00"),
  doc_L(40, 116, "Gamma"), doc_R(500, 116, "3,000.00"),
  doc_R(500, 134, "6,000.00"))))

hard_total_tab <- function() list(
  name = "Schedule", start = list(page = 1, y = 58), end = list(page = 1, y = 160),
  header_rows = 1L, follow = FALSE,
  anchor = list(header_text = list("Item", "Value")),
  columns = list(list(name = "Item",  x_min = 30,  x_max = 380, type = "text"),
                 list(name = "Value", x_min = 380, x_max = 520, type = "money")))

test_that("an unlabelled total under the last row is its own row, not part of it", {
  # MEASURED BEFORE THE FIX: 3 rows, the last one holding
  # Value = "3,000.00 6,000.00", and Value__value = the TOTAL.
  tab <- hard_total_tab()
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(s = tab))
  r <- doc_table_rows(hard_unlabelled_total(), tab, tm)
  expect_equal(r$n_rows, 4L)
  expect_identical(r$rows$Value, c("1,000.00", "2,000.00", "3,000.00", "6,000.00"))
  expect_identical(r$rows$Value__value, c(1000, 2000, 3000, 6000))
  expect_true(is.na(r$rows$Item[4]))          # it has no label, and none is invented
})

test_that("the indented shape carries the same structural guards as the flat one", {
  d_in <- data.frame(x = 90, text = "more words", height = 9, stringsAsFactors = FALSE)
  # a one-column table has no cell that can overflow, indented or not
  expect_false(.doc_is_wrap(d_in, 60, 18, 1L, 1L, 2L, 9))
  # ...nor can a line be the overflow of a row that filled only one column
  expect_false(.doc_is_wrap(d_in, 60, 18, 1L, 2L, 1L, 9))
  # and with both satisfied it still folds exactly as it did
  expect_true(.doc_is_wrap(d_in, 60, 18, 1L, 2L, 2L, 9))
})

test_that("a wrapped description still joins, because words are not figures", {
  # The guard is about a figure landing on a figure. A text column that runs on
  # is untouched by it.
  inp <- doc_input(list(doc_page(
    doc_L(40, 60, "Item"), doc_L(200, 60, "Detail"), doc_R(500, 60, "Value"),
    doc_L(40, 80, "Alpha"), doc_L(200, 80, "a long description that"),
    doc_R(500, 80, "1,000.00"),
    doc_L(200, 92, "runs onto a second line"),
    doc_L(40, 112, "Beta"), doc_L(200, 112, "short"), doc_R(500, 112, "2,000.00"))))
  tab <- list(name = "S", start = list(page = 1, y = 58), end = list(page = 1, y = 160),
              header_rows = 1L, follow = FALSE,
              anchor = list(header_text = list("Item", "Detail", "Value")),
              columns = list(list(name = "Item",   x_min = 30,  x_max = 190, type = "text"),
                             list(name = "Detail", x_min = 190, x_max = 380, type = "text"),
                             list(name = "Value",  x_min = 380, x_max = 520, type = "money")))
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(s = tab))
  r <- doc_table_rows(inp, tab, tm)
  expect_equal(r$n_rows, 2L)
  expect_identical(r$rows$Detail[1], "a long description that runs onto a second line")
  expect_identical(r$rows$Value__value, c(1000, 2000))
})

# ---------------------------------------------------------------------------
# A REAL ROW THAT ECHOES THE HEADER WORDING (register I4)
# ---------------------------------------------------------------------------

hard_echo_header <- function() doc_input(list(doc_page(
  doc_L(40, 60, "Fee"), doc_L(240, 60, "Basis"),
  doc_L(40, 80, "AccountKeeping"), doc_L(240, 80, "per month"),
  doc_L(40, 98, "Transaction"),    doc_L(240, 98, "per item"),
  doc_L(40, 116, "Fee"),           doc_L(240, 116, "basis under review"),
  doc_L(40, 134, "Overdraft"),     doc_L(240, 134, "per day"))))

hard_echo_tab <- function() list(
  name = "Fees", start = list(page = 1, y = 58), end = list(page = 1, y = 160),
  header_rows = 1L, follow = FALSE,
  anchor = list(header_text = list("Fee", "Basis")),
  columns = list(list(name = "Fee",   x_min = 30,  x_max = 230, type = "text"),
                 list(name = "Basis", x_min = 230, x_max = 520, type = "text")))

test_that("a genuine row that echoes the header wording is not deleted", {
  # MEASURED BEFORE THE FIX: 3 rows where the document prints 4, n_header 2,
  # fill_rate 1.00, confidence 1.00, and nothing anywhere said a row had gone.
  # The line scores 0.6 on "Fee"/"Basis" and carries no money token, because the
  # column has no decimals in it -- which is the ordinary case, not an exotic one.
  tab <- hard_echo_tab()
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(f = tab))
  r <- doc_table_rows(hard_echo_header(), tab, tm)
  expect_equal(r$n_rows, 4L)
  expect_identical(r$rows$Fee, c("AccountKeeping", "Transaction", "Fee", "Overdraft"))
  expect_identical(r$rows$Basis[3], "basis under review")
  expect_equal(r$n_header, 1L)                 # the one real heading, and only it
})

test_that("a heading reprinted at the top of a continuation window is still dropped", {
  # The restriction is to the TOP of a window, which is the only place a
  # reprinted heading is ever printed. The fixture's three-page schedule proves
  # it end to end: one heading per page, none of them read as rows.
  inp <- doc_fixture_input(); tm <- doc_fixture_template()
  r <- doc_table_rows(inp, tm$tables$transactions, tm)
  expect_equal(r$n_rows, 83L)
  expect_equal(r$n_header, 3L)
  expect_false(any(r$rows$Description %in% "Description"))
})

test_that("more lines skipped as a heading than the table can spend is said out loud", {
  # n_header was counted and read by nothing. A table that dropped more heading
  # lines than it has pages has deleted something, and that is the only place it
  # would ever show.
  # The heading is printed twice at the top of this table and header_rows says
  # one, so the wording rule takes the second line as well.
  inp <- doc_input(list(doc_page(
    doc_L(40, 60, "Fee"), doc_L(240, 60, "Basis"),
    doc_L(40, 78, "Fee"), doc_L(240, 78, "Basis"),
    doc_L(40, 98, "AccountKeeping"), doc_L(240, 98, "per month"),
    doc_L(40, 116, "Transaction"), doc_L(240, 116, "per item"))))
  tab <- hard_echo_tab()
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(f = tab))
  r <- doc_table_rows(inp, tab, tm)
  expect_equal(r$n_rows, 2L)
  expect_equal(r$n_header, 2L)                 # one more than it can spend
  expect_match(r$detail, "skipped as its heading", fixed = TRUE)
  # ...and an ordinary read says nothing at all.
  r2 <- doc_table_rows(hard_echo_header(), tab, tm)
  expect_false(grepl("skipped as its heading", r2$detail, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# A COMPARATIVE TABLE THAT GAINED A PERIOD COLUMN (register I2)
#
# Not the same fault as a table printed further across the page: that is a
# uniform translation and .doc_band_shift moves the bands to match. This is a
# change of column COUNT on a right-anchored table, and it shifts every figure
# one place across with every existing signal reading perfect.
# ---------------------------------------------------------------------------

hard_comparative <- function(cols) {
  xs <- seq(560 - (length(cols) - 1L) * 70, 560, by = 70)
  parts <- list(doc_L(40, 60, "Item"))
  for (j in seq_along(cols)) parts <- c(parts, list(doc_R(xs[j], 60, cols[j])))
  y <- 82
  for (r in list(c("Revenue", 100, 200, 300, 400, 500, 600),
                 c("Costs", 10, 20, 30, 40, 50, 60))) {
    parts <- c(parts, list(doc_L(40, y, r[1])))
    for (j in seq_along(cols))
      parts <- c(parts, list(doc_R(xs[j], y, sprintf("%s.00", r[1 + j]))))
    y <- y + 18
  }
  doc_input(list(do.call(doc_page, parts)))
}

hard_comparative_tab <- function() {
  cols <- c("Q1", "Q2", "Q3", "Q4", "Total")
  xs <- seq(560 - 4 * 70, 560, by = 70)
  list(name = "Comparative", start = list(page = 1, y = 58),
       end = list(page = 1, y = 140), header_rows = 1L, follow = FALSE,
       anchor = list(header_text = as.list(c("Item", cols))),
       columns = c(list(list(name = "Item", x_min = 30, x_max = xs[1] - 60,
                             type = "text")),
                   lapply(seq_along(cols), function(j)
                     list(name = cols[j], x_min = xs[j] - 60, x_max = xs[j] + 8,
                          type = "money"))))
}

test_that("a table that gained a column says so, instead of reading perfectly", {
  # MEASURED BEFORE THE FIX on this exact document: Q1 held Q2's figure, and
  # rows 2, unclaimed 0, low_fill FALSE on all six, found by its heading,
  # confidence 1.00. Not one number in the result was different from a correct
  # read of the copy the template was drawn on.
  tab <- hard_comparative_tab()
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(c = tab))
  r <- doc_table_rows(hard_comparative(c("Q1", "Q2", "Q3", "Q4", "Q5", "Total")),
                      tab, tm)
  # the figures ARE still shifted -- nothing is repaired, because a remapping
  # guessed off a header row is the tool inventing which quarter is which
  expect_identical(r$rows$Q1[1], "200.00")
  # ...and every one of the old signals still says nothing, which is the point
  expect_false(any(r$report$low_fill))
  expect_equal(nrow(r$spilled), 0L)
  expect_equal(r$confidence, 1)
  # the new one does
  expect_true(r$report$wrong_column[r$report$column == "Q1"])
  expect_identical(r$report$heading[r$report$column == "Q1"], "Q2")
  expect_identical(r$report$heading[r$report$column == "Q3"], "Q4")
  # Q4 now sits under "Q5", a heading this template has no column for, so it is
  # NOT reported: the rule only fires on a heading that provably belongs to
  # another of this table's own columns, which is what keeps a renamed column
  # (the fixture's M1..M5 over printed dates) from being flagged every time.
  expect_false(r$report$wrong_column[r$report$column == "Q4"])
  expect_match(r$detail, "gained or lost a column", fixed = TRUE)
})

test_that("the copy the template was drawn on reports no disagreement at all", {
  tab <- hard_comparative_tab()
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(c = tab))
  r <- doc_table_rows(hard_comparative(c("Q1", "Q2", "Q3", "Q4", "Total")), tab, tm)
  expect_identical(r$rows$Q1, c("100.00", "10.00"))
  expect_false(any(r$report$wrong_column))
  expect_true(all(!nzchar(r$report$heading)))
  expect_false(grepl("gained or lost", r$detail, fixed = TRUE))
})

test_that("a table read from where it sat reports no column disagreement", {
  # There is no heading in hand to compare against, so every column would look
  # like a disagreement about nothing.
  tab <- hard_comparative_tab()
  tab$anchor <- NULL
  tm <- list(mode = "document", ref_width = DOC_W, ref_height = DOC_H,
             tables = list(c = tab))
  inp <- hard_comparative(c("Q1", "Q2", "Q3", "Q4", "Q5", "Total"))
  loc <- doc_locate_table(inp, tab, tm)
  r <- doc_table_rows(inp, tab, tm, loc)
  if (!identical(loc$anchor, "header")) expect_false(any(r$report$wrong_column))
  expect_true(is.logical(r$report$wrong_column))
})

test_that("the whole fixture reports no column disagreement and no wrong kind", {
  # The regression guard: six real tables read correctly must light up none of
  # the new signals, or they are noise rather than evidence.
  ext <- extract_document(doc_fixture_input(), doc_fixture_template())
  expect_false(any(ext$report$wrong_column))
  expect_false(any(ext$report$wrong_kind))
  expect_false(any(grepl("gained or lost", ext$summary$note, fixed = TRUE)))
  expect_false(any(grepl("reads as the kind", ext$summary$note, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# A2 / A2b: however many tables the document has -- the proposer's half, and the
# matching rules the locator reads a repeating table by
# ---------------------------------------------------------------------------

.fund_pack <- function(letters_ = LETTERS[1:7]) {
  one <- function(letter) {
    parts <- list(doc_L(40, 60, "Holdings"),
                  doc_L(40, 100, "Security"), doc_R(330, 100, "Units"),
                  doc_R(430, 100, "Unit price"), doc_R(530, 100, "Market value"))
    y <- 124
    for (i in 1:6) {
      parts <- c(parts, list(doc_L(40, y, sprintf("SEC-%s%02d", letter, i)),
                             doc_R(330, y, .doc_money(100 * i)),
                             doc_R(430, y, .doc_money(1.25 * i)),
                             doc_R(530, y, .doc_money(125 * i))))
      y <- y + 18
    }
    do.call(doc_page, parts)
  }
  doc_input(lapply(letters_, one))
}

test_that("seven near-identical proposals fold into ONE that says where it repeats", {
  # A2. A holdings table per fund proposed seven entries -- seven things to read,
  # name and confirm where there is one thing to teach. They fold into one
  # carrying `occurrence: all` and remembering every place, which REMOVES six
  # things from the screen.
  #
  # THE FOLD NEEDS THE SAME PRINTED HEADING, not just the same columns. "Table
  # name + columns IS the table": two schedules printed under identical column
  # headings but different titles are different tables, and folding them is how
  # twelve rows ended up under one account's name (see the AccountOne/AccountTwo
  # test above, which is that bug). So this pack prints the same title on every
  # page, which is what a genuinely repeating table does.
  inp <- .fund_pack()
  cands <- list()
  for (p in 1:7) for (cd in .doc_page_candidates(inp, p)) cands[[length(cands) + 1L]] <- cd
  expect_equal(length(cands), 7L)

  f <- .doc_fold_repeats(cands)
  expect_equal(length(f), 1L)
  expect_identical(f[[1]]$occurrence, "all")
  expect_equal(length(f[[1]]$.places), 7L)
  expect_equal(vapply(f[[1]]$.places, function(q) as.integer(q$start$page), integer(1)), 1:7)
  expect_equal(f[[1]]$n_rows, 42L)             # every place's rows, added up
})

test_that("a folded table is READ in every place, not just the first", {
  # The fold is only safe because of this. Folding seven entries into one and
  # then reading one place would delete six funds from the output with a green
  # tick beside it -- the silent loss `occurrence: all` was refused for.
  inp <- .fund_pack()
  props <- propose_tables(inp)
  expect_equal(length(props), 1L)
  t <- document_template_from_proposal("fp", "Acme", "report",
         phrases = list("Holdings"), tables = props, pairs = list())
  expect_equal(length(t$tables), 1L)
  ext <- extract_document(inp, t)
  expect_equal(length(ext$tables), 7L)
  expect_equal(sum(vapply(ext$tables, function(x) NROW(x$rows), integer(1))), 42L)
  # each one says which page it came off, so two funds can never be read as one
  expect_true(all(grepl("page [1-7]\\)$", vapply(ext$tables, function(x) x$name, character(1)))))
})

test_that("two tables with the same columns but different titles are NOT folded", {
  # The other half of the same rule, and the one that is a wrong figure if it
  # goes: "AccountOne" and "AccountTwo" print identical column headings.
  inp <- hard_same_header_twice()
  props <- propose_tables(inp)
  expect_equal(length(props), 2L)
  expect_identical(vapply(props, function(p) p$name, character(1)),
                   c("AccountOne", "AccountTwo"))
  expect_null(props[[1]]$occurrence)
})

test_that("the fold refuses what geometry alone cannot vouch for", {
  # Two tables with NO header wording can never be folded: geometry says they are
  # the same SHAPE and never that they mean the same thing. And a table seen once
  # keeps exactly the shape it has always had -- no occurrence key, no places.
  none <- list(anchor = list(header_text = list()), n_rows = 3L,
               .bands = list(list(x_min = 30, x_max = 100), list(x_min = 100, x_max = 200)),
               start = list(page = 1, y = 10), end = list(page = 1, y = 90))
  b <- none; b$start$page <- 2; b$end$page <- 2
  expect_equal(length(.doc_fold_repeats(list(none, b))), 2L)

  one <- list(anchor = list(header_text = list("Ref", "Item")), n_rows = 4L,
              .bands = none$.bands, start = list(page = 1, y = 10),
              end = list(page = 1, y = 90))
  kept <- .doc_fold_repeats(list(one))
  expect_null(kept[[1]]$occurrence)
  expect_null(kept[[1]]$.places)

  # ...and a table whose bands sit somewhere else is a different table
  far <- one
  far$.bands <- list(list(x_min = 300, x_max = 400), list(x_min = 400, x_max = 500))
  expect_equal(length(.doc_fold_repeats(list(one, far))), 2L)
})

test_that("the fold waits for the half of the reader that can find every occurrence", {
  # Folding seven entries into one is right only if the reader then reads all
  # seven. While it reads one place per entry, folding would delete six tables
  # from the template with nothing on screen to say so -- the silent loss this
  # register exists to stop, arriving through the fix for it.
  expect_identical(.DOC_FOLD_READER, "doc_locate_repeats")
  expect_identical(.doc_fold_available(), exists("doc_locate_repeats", mode = "function"))
  # whichever half of that is true today, what is proposed is what gets read
  inp <- .fund_pack(LETTERS[1:3])
  props <- propose_tables(inp)
  for (p in props) {
    got <- if (length(p$.places %||% list()) > 1L)
      sum(vapply(p$.places, function(q) {
        m <- p; m$start <- q$start; m$end <- q$end
        as.integer(doc_table_rows(inp, m, NULL)$n_rows)
      }, integer(1)))
      else as.integer(doc_table_rows(inp, p, NULL)$n_rows)
    expect_equal(as.integer(p$n_rows), got)
  }
})

test_that("a header wording is matched WHOLE, so another table's header cannot win", {
  # A2b, measured. Matching was a substring of the whole normalised LINE, so one
  # header's words were found inside another's: against the line "Security Units
  # Unit price Market value", a DIFFERENT table's header "Code, Units, Price,
  # Value" scored 0.75 and cleared the 0.6 bar -- `price` inside `unitprice`,
  # `value` inside `marketvalue`. On a pack of forty similarly-shaped tables that
  # is the normal case, and the table found is confidently the wrong one.
  d <- .doc_lines(doc_page(doc_L(40, 100, "Security"), doc_R(330, 100, "Units"),
                           doc_R(430, 100, "Unit price"),
                           doc_R(530, 100, "Market value")))[[1]]
  expect_equal(.doc_header_score(d, list("Security", "Units", "Unit price", "Market value")), 1)
  expect_lt(.doc_header_score(d, list("Code", "Units", "Price", "Value")), 0.6)
  # ...and the tolerance worth keeping is kept: a wording still matches a heading
  # that carries MORE words after it.
  e <- .doc_lines(doc_page(doc_L(40, 100, "Opening balance"),
                           doc_R(400, 100, "Closing balance")))[[1]]
  expect_equal(.doc_header_score(e, list("Opening", "Closing")), 1)
})

test_that("variants are scored as the BEST of them, and a flat set is unchanged", {
  # A2b. `need` is one flat set today and unlist() would flatten a nested list
  # into one set -- scoring every variant together and matching none of them. So
  # the score branches on the STRUCTURE.
  d <- .doc_lines(doc_page(doc_L(40, 100, "Security"), doc_R(330, 100, "Units"),
                           doc_R(430, 100, "Unit price"),
                           doc_R(530, 100, "Market value")))[[1]]
  vars <- list(c("Code", "Units", "Price", "Value"),
               c("Security", "Units", "Unit price", "Market value"))
  expect_equal(.doc_header_score(d, vars), 1)
  expect_lt(.doc_header_score(d, as.list(unlist(vars))), 1)   # flattened: matches none
  expect_length(.doc_header_variants(list("Code", "Units")), 1L)   # a flat set stays flat
  expect_length(.doc_header_variants(vars), 2L)
  expect_equal(.doc_header_score(d, list("Security", "Units", "Unit price", "Market value")), 1)
})

test_that("a title is found however it was typed, and NULL when it is not there", {
  # A2b. A title says WHICH table this is; a header only says what SHAPE it is.
  lines <- .doc_lines(doc_page(doc_L(40, 60, "Portfolio  HOLDINGS, at period end"),
                               doc_L(40, 100, "Security"), doc_R(500, 100, "Value")))
  hit <- .doc_find_title(lines, list("Investments held", "portfolio holdings"))
  expect_false(is.null(hit))
  expect_equal(hit$index, 1L)
  expect_match(hit$text, "HOLDINGS")
  expect_null(.doc_find_title(lines, list("Fee schedule")))
  expect_null(.doc_find_title(lines, list()))
})

test_that("prose that merely NAMES the columns is not an occurrence of the table", {
  # A2. Three tests, all three required, and the third is the one that kills
  # prose: a sentence runs straight across every band as ONE cell. Measured
  # without it: a line scoring 1.00 on the wording and landing in two bands was
  # accepted as another copy of the table.
  bands <- list(list(x_min = 30, x_max = 240), list(x_min = 240, x_max = 400),
                list(x_min = 400, x_max = 520), list(x_min = 520, x_max = 600))
  hdr <- .doc_lines(doc_page(doc_L(40, 100, "Item"), doc_R(390, 100, "Q1"),
                             doc_R(510, 100, "Q2"), doc_R(590, 100, "Total")))[[1]]
  expect_true(.doc_is_occurrence(hdr, list("Item", "Total"), bands))

  prose <- .doc_lines(doc_page(doc_L(40, 300,
    "Total figures for the year are set out in the schedule below")))[[1]]
  expect_equal(.doc_header_score(prose, list("Total")), 1)   # the wording is there
  expect_equal(length(.doc_cells(prose, .doc_gap_for(prose))), 1L)  # ...as one cell
  expect_false(.doc_is_occurrence(prose, list("Total"), bands))

  # a different table's header does not clear the bar at all
  expect_false(.doc_is_occurrence(hdr, list("Code", "Units", "Price", "Value"), bands))
  # and a line printed nowhere near the declared bands is not one either
  away <- .doc_lines(doc_page(doc_L(40, 400, "Item"), doc_L(90, 400, "Total")))[[1]]
  expect_false(.doc_is_occurrence(away, list("Item", "Total"), bands))
})

# ---------------------------------------------------------------------------
# THE ROUND TRIP THAT UN-PARKED A PARKED TEMPLATE
#
# document_template_from_proposal(base = ) has always known how to keep what the
# builder never asks about, and NOTHING EVER PASSED IT: app.R's rb_template()
# called it with no base at all, so opening a saved report template and saving it
# again rebuilt it from a whitelist.
#
# Measured on the fixture template before the fix:
#   hidden   TRUE  -> LOST      a PARKED template came back live and could start
#                               claiming documents again, with nothing said
#   notes    set   -> LOST      why it was parked, gone with it
#   version  7     -> 1         so a template edited seven times was version 1
#                               seven times, and today's copy is indistinguishable
#                               from last month's in every record that keeps one
#   row_tol / max_gap survived, because the proposal round-trips those.
# (Register H6.)
# ---------------------------------------------------------------------------

test_that("saving over a template keeps what the builder never asks about", {
  t <- doc_fixture_template()
  t$hidden  <- TRUE
  t$notes   <- "parked while the layout settles"
  t$version <- 7
  prop <- document_proposal_from_template(t)
  ph <- unlist(t$fingerprint$page_contains_all %||% list())

  # WITHOUT a base -- a template being drawn for the first time -- the result is
  # byte for byte what it always was, and must stay that way.
  fresh <- document_template_from_proposal(t$id, t$bank, t$statement_type,
             phrases = ph, tables = prop$tables, pairs = prop$pairs)
  expect_null(fresh$hidden)
  expect_null(fresh$notes)
  expect_equal(fresh$version, 1)

  # WITH one, the loaded template is modified rather than rebuilt.
  again <- document_template_from_proposal(t$id, t$bank, t$statement_type,
             phrases = ph, tables = prop$tables, pairs = prop$pairs, base = t)
  expect_true(isTRUE(again$hidden))
  expect_identical(again$notes, "parked while the layout settles")
  # ...and the version goes UP, so the record can tell the copies apart
  expect_equal(again$version, 8)
  # the builder still owns what the builder owns
  expect_identical(again$id, t$id)
  expect_equal(length(again$tables), length(prop$tables))
})

test_that("the builder passes a base only while it is still the same template", {
  src <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  i <- grep("rb_template <- reactive", src)[1]
  skip_if(is.na(i), "rb_template not found")
  blk <- paste(src[i:min(length(src), i + 30L)], collapse = "\n")
  expect_match(blk, "base = base", fixed = TRUE)
  # THE RENAME GUARD, and it is the whole reason this is not just `base = rb_base()`.
  # Change the id in the box and Save writes a NEW template; carrying the old
  # one's fields into it would make a fresh template that is hidden on the day it
  # was created, at version 8, for reasons nobody can see anywhere on screen.
  expect_match(blk, "rb_editing\\(\\)")
  expect_match(blk, "identical\\(trimws\\(as\\.character\\(id\\)\\[1\\]\\), ed\\)")
  # and the base is dropped everywhere the id is, or it outlives its template
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "rb_editing\\(NA_character_\\); rb_base\\(NULL\\)")
})

# ---------------------------------------------------------------------------
# THE COMPUTED FLAG NOBODY READ
#
# R/tables.R works out properly that a band is sitting under ANOTHER of its own
# table's headings -- a comparative table right-anchored to the page (Q1..Q4,
# Total) that gains Q5 pushes every older column left, so Q1 holds Q2's figure
# and Total holds Q5's. It puts `wrong_column` on the per-column report and says
# so in the table's detail line.
#
# And nothing anywhere read it. `grep wrong_column` outside the line that
# computes it returned nothing: not the status, not the summary, not the screen.
# So the run came back **ok** -- and every other signal on it was TRUE, which is
# what makes this the dangerous shape: the rows are there, the columns are full,
# nothing is unclaimed, confidence is 1.00. Only the headings disagree, and the
# figures are real figures filed under the wrong names. (Register I2.)
# ---------------------------------------------------------------------------

test_that("a band under another column's heading is read, and holds the run", {
  rep <- function(wrong) data.frame(
    table = c("position", "position"), column = c("Q1", "Q6"),
    wrong_kind = c(FALSE, FALSE), wrong_column = c(FALSE, wrong),
    stringsAsFactors = FALSE)
  expect_length(.doc_wrong_columns(rep(FALSE)), 0L)
  expect_identical(.doc_wrong_columns(rep(TRUE)), "position")
  # named ONCE, however many columns of that table disagree -- a shifted table
  # disagrees in every column at once and the status names tables, not columns
  many <- data.frame(table = c("position", "position", "position"),
                     column = c("Q1", "Q2", "Q6"),
                     wrong_column = c(TRUE, TRUE, TRUE), stringsAsFactors = FALSE)
  expect_identical(.doc_wrong_columns(many), "position")
  # a report frame from a build that predates the column does nothing, rather
  # than erroring -- the same rule .doc_parse_failures is read by
  expect_length(.doc_wrong_columns(data.frame(table = "x", column = "y",
                                              stringsAsFactors = FALSE)), 0L)
  expect_length(.doc_wrong_columns(NULL), 0L)

  # AND IT GATES. The status line and the message both have to name it, or the
  # flag is computed, read, and still thrown away.
  src <- readLines(file.path(engine_root(), "R", "doc_extract.R"), warn = FALSE)
  i <- grep("res\\$status <- if \\(res\\$n_rows == 0L\\)", src)[1]
  skip_if(is.na(i), "the status gate was not found")
  blk <- paste(src[i:min(length(src), i + 45L)], collapse = "\n")
  expect_match(blk, "length\\(wrong_col\\)")
  expect_match(blk, "do not line up with the headings this copy prints", fixed = TRUE)
  # ...and it is carried on the result, so a log or a screen can say which table
  expect_match(paste(src, collapse = "\n"), "res\\$wrong_column_tables <- wrong_col")
})

# ---------------------------------------------------------------------------
# THE COLUMN WITH NO HEADING, WHICH IS THE TOTAL COLUMN
#
# Reported: "Some of the financial summaries have the top row as: Column name,
# statement x-y, statement t-y, period x-z, (blank). The blank is the total
# column, and has two blank rows then data for the total of each period."
#
# A box drawn round the headings cannot see a column whose heading is blank, and
# the totals sit to the RIGHT of that box - so they were not even unclaimed.
# Measured before the fix: four bands, four columns, four rows every one of which
# reported itself complete, and both totals silently gone.
# ---------------------------------------------------------------------------

.blank_total_page <- function() {
  doc_input(list(doc_page(
    doc_L(40, 60, "Financial summary"),
    doc_L(40,  96, "Item"), doc_R(240, 96, "Statement 1-3"),
    doc_R(360, 96, "Statement 4-6"), doc_R(470, 96, "Period 1-6"),
    # ...and nothing at all over the fifth column
    doc_L(40, 120, "Gross income"), doc_R(240,120,"1,000.00"), doc_R(360,120,"2,000.00"), doc_R(470,120,"3,000.00"),
    doc_L(40, 140, "Expenses"),     doc_R(240,140,"400.00"),   doc_R(360,140,"500.00"),   doc_R(470,140,"900.00"),
    doc_L(40, 160, "Net"),          doc_R(240,160,"600.00"),   doc_R(360,160,"1,500.00"), doc_R(470,160,"2,100.00"),
    doc_R(570, 160, "2,100.00"),    # the total column starts here: two blank rows above it
    doc_L(40, 180, "Tax payable"),  doc_R(240,180,"90.00"),    doc_R(360,180,"225.00"),   doc_R(470,180,"315.00"),
    doc_R(570, 180, "315.00"))))
}

test_that("a column with a blank heading is still a column", {
  inp <- .blank_total_page()
  hdr <- list(page = 1, x_min = 30, x_max = 520, y_min = 88, y_max = 104)

  # THE BOX ALONE CANNOT SEE IT, and that is not a bug in the box - there is no
  # ink in the header row to see. It is why the end has to be supplied.
  expect_equal(length(doc_columns_from_box(inp, hdr, NULL)), 4L)

  # WITH THE TABLE'S END, the projection of the whole table finds it: the totals
  # lower down are ink like any other.
  cols <- doc_columns_from_box(inp, hdr, NULL, y_to = 200)
  expect_equal(length(cols), 5L)
  nm <- vapply(cols, function(c) c$name, character(1))
  expect_identical(nm[1:4], c("Item", "Statement 1-3", "Statement 4-6", "Period 1-6"))
  # unnamed, not missing: present and renameable, which is the whole difference
  expect_match(nm[5], "^column_")
  expect_true(cols[[5]]$x_min > cols[[4]]$x_min)

  # ...AND THE FIGURES COME OUT, in the rows they were printed in, with the two
  # blank rows blank rather than shifted up.
  tab <- list(name = "Financial summary", start = list(page = 1, y = 88),
              end = list(page = 1, y = 200), header_rows = 1L, columns = cols,
              anchor = list(header_text = as.list(nm), first_column = list()))
  r <- doc_table_rows(inp, tab, NULL)
  expect_equal(r$n_rows, 4L)
  tot <- as.character(r$rows[[nm[5]]])
  expect_true(is.na(tot[1]) || !nzchar(tot[1]))
  expect_true(is.na(tot[2]) || !nzchar(tot[2]))
  expect_identical(tot[3], "2,100.00")
  expect_identical(tot[4], "315.00")
  # the columns it already read are untouched by the extra band
  expect_identical(as.character(r$rows[["Period 1-6"]]),
                   c("3,000.00", "900.00", "2,100.00", "315.00"))
})

test_that("the widened band search ignores lines that are not table rows", {
  # A footer or a sentence under the table is one cell wide. Letting it vote on
  # where the columns are would smear two real bands together or invent one.
  pages <- doc_input(list(doc_page(
    doc_L(40,  96, "Item"), doc_R(240, 96, "Amount"),
    doc_L(40, 120, "Gross income"), doc_R(240, 120, "1,000.00"),
    doc_L(40, 140, "Expenses"),     doc_R(240, 140, "400.00"),
    doc_L(40, 190, "This summary is prepared on a cash basis and is unaudited."))))
  hdr <- list(page = 1, x_min = 30, x_max = 300, y_min = 88, y_max = 104)
  cols <- doc_columns_from_box(pages, hdr, NULL, y_to = 210)
  expect_equal(length(cols), 2L)
  expect_identical(vapply(cols, function(c) c$name, character(1)), c("Item", "Amount"))
})
