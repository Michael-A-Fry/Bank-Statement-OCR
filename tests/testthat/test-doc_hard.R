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
