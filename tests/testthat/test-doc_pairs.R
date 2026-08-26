# test-doc_pairs.R -- WHICH SIDE the value is on, and moving one column's band.
#
# Both of these exist because of the same complaint: what the builder worked out
# for itself had to be visible, and changeable, and had to survive the next copy
# of the document. A label/value pair stored as a rigid offset ("38 points right,
# 0 down") is neither: nothing on screen says which side that is, and it misses
# as soon as the label is a word longer or the figure a digit wider -- which on a
# re-print it usually is.
#
# So a pair stores the SIDE, and the search runs in that direction. These tests
# hold that to its word, including the cases the offset used to get right by
# accident and the ones it got wrong.

# .pv_page(...) -- a page of word boxes, in the same shape read_pdf produces.
.pv_page <- function(...) doc_page(...)
.pv_input <- function(...) doc_input(list(...))

# .pv_tmpl(pairs) -- the smallest document template that carries some pairs.
.pv_tmpl <- function(pairs) list(
  id = "pv", mode = "document", format = "pdf", version = 1,
  ref_width = DOC_W, ref_height = DOC_H,
  fingerprint = list(page_contains_all = list("Consolidated position report")),
  pairs = pairs, tables = list())

# ---------------------------------------------------------------------------
# The relation itself
# ---------------------------------------------------------------------------

test_that("the side is read off the two boxes, in the four arrangements there are", {
  lb <- list(x_min = 100, x_max = 160, y_min = 200, y_max = 212)
  r <- .doc_pair_rel(lb, list(x_min = 180, x_max = 250, y_min = 200, y_max = 212))
  expect_identical(r$where, "right"); expect_equal(r$gap, 20)
  expect_equal(r$width, 70)
  r <- .doc_pair_rel(lb, list(x_min = 30, x_max = 80, y_min = 200, y_max = 212))
  expect_identical(r$where, "left"); expect_equal(r$gap, 20)
  r <- .doc_pair_rel(lb, list(x_min = 100, x_max = 160, y_min = 224, y_max = 236))
  expect_identical(r$where, "below"); expect_equal(r$gap, 12)
  r <- .doc_pair_rel(lb, list(x_min = 100, x_max = 160, y_min = 170, y_max = 182))
  expect_identical(r$where, "above"); expect_equal(r$gap, 18)
})

test_that("sharing a line means BESIDE, even when the boxes also differ in height", {
  # The awkward real case: a tall label box drawn round two words and a short
  # value box drawn round a figure. They overlap vertically, so this is side by
  # side -- not "below" merely because the value's top edge is lower.
  lb <- list(x_min = 100, x_max = 170, y_min = 200, y_max = 224)
  vb <- list(x_min = 220, x_max = 280, y_min = 208, y_max = 220)
  expect_identical(.doc_pair_rel(lb, vb)$where, "right")
})

test_that("boxes that overlap both ways still name a side rather than failing", {
  lb <- list(x_min = 100, x_max = 200, y_min = 200, y_max = 240)
  vb <- list(x_min = 150, x_max = 260, y_min = 210, y_max = 250)
  w <- .doc_pair_rel(lb, vb)$where
  expect_true(w %in% c("right", "left", "above", "below"))
  expect_identical(.doc_pair_where_text(list(where = "below")), "under it")
  expect_identical(.doc_pair_where_text(list(where = "left")), "to the left of it")
})

# ---------------------------------------------------------------------------
# What that buys: the pair survives the document being re-set
# ---------------------------------------------------------------------------

test_that("the value is found on the side it was on, not at the distance it was at", {
  # DRAWN ON: label at 40, value 60pt to its right, four characters wide.
  drawn <- .pv_input(.pv_page(
    doc_L(40, 100, "Prepared for"),
    doc_L(150, 100, "Ambrose Trust"),
    doc_L(40, 130, "Consolidated position report")))
  lb <- list(page = 1L, x_min = 39, x_max = 100, y_min = 99, y_max = 110)
  vb <- list(page = 1L, x_min = 149, x_max = 214, y_min = 99, y_max = 110)
  tmpl <- .pv_tmpl(list(prepared_for = list(
    label_text = "Prepared for", label = lb, value = vb, type = "text",
    where = .doc_pair_rel(lb, vb))))
  got <- doc_pairs(drawn, tmpl)
  expect_identical(got$value[1], "Ambrose Trust")
  expect_identical(got$found_by[1], "its wording")

  # RUN ON: the same document re-set. The label has moved 30pt down and 15pt
  # right, the gap beside it has widened, and the value itself is longer. A rigid
  # offset lands in the whitespace before the name and reads nothing.
  moved <- .pv_input(.pv_page(
    doc_L(55, 130, "Prepared for"),
    doc_L(260, 130, "Ambrose Family Trust Number Two"),
    doc_L(55, 160, "Consolidated position report")))
  got2 <- doc_pairs(moved, tmpl)
  expect_identical(got2$found_by[1], "its wording")
  expect_identical(got2$value[1], "Ambrose Family Trust Number Two")
})

test_that("a value printed UNDER its label is found under it on the next copy too", {
  drawn <- .pv_input(.pv_page(
    doc_L(40, 100, "Closing balance"),
    doc_R(160, 118, "1,240.55"),
    doc_L(40, 150, "Consolidated position report")))
  lb <- list(page = 1L, x_min = 39, x_max = 110, y_min = 99, y_max = 110)
  vb <- list(page = 1L, x_min = 120, x_max = 161, y_min = 117, y_max = 128)
  rel <- .doc_pair_rel(lb, vb)
  expect_identical(rel$where, "below")
  tmpl <- .pv_tmpl(list(closing_balance = list(
    label_text = "Closing balance", label = lb, value = vb, type = "money",
    where = rel)))
  expect_identical(doc_pairs(drawn, tmpl)$value[1], "1,240.55")

  # moved down the page, and a bigger figure
  moved <- .pv_input(.pv_page(
    doc_L(40, 300, "Closing balance"),
    doc_R(170, 320, "912,406.10"),
    doc_L(40, 350, "Consolidated position report")))
  got <- doc_pairs(moved, tmpl)
  expect_identical(got$found_by[1], "its wording")
  expect_identical(got$value[1], "912,406.10")
})

test_that("the search stops at the field next door instead of swallowing it", {
  # Two labelled figures on one line. The window that reaches far enough right to
  # survive a longer figure also reaches the NEXT label, and the run has to break
  # at the gutter between them.
  inp <- .pv_input(.pv_page(
    doc_L(40, 100, "Opening"), doc_L(110, 100, "1,000.00"),
    doc_L(300, 100, "Closing"), doc_L(370, 100, "2,000.00"),
    doc_L(40, 140, "Consolidated position report")))
  lb <- list(page = 1L, x_min = 39, x_max = 82, y_min = 99, y_max = 110)
  vb <- list(page = 1L, x_min = 109, x_max = 150, y_min = 99, y_max = 110)
  tmpl <- .pv_tmpl(list(opening = list(
    label_text = "Opening", label = lb, value = vb, type = "money",
    where = .doc_pair_rel(lb, vb))))
  got <- doc_pairs(inp, tmpl)
  expect_identical(got$value[1], "1,000.00")
  expect_false(grepl("2,000", got$raw[1]))
})

test_that("a side that is written on the template overrides what the boxes imply", {
  # The person knew the next copy prints the figure underneath. That has to beat
  # the geometry of the one copy they had in front of them.
  inp <- .pv_input(.pv_page(
    doc_L(40, 100, "Total"), doc_L(200, 100, "NOT THIS"),
    doc_R(120, 120, "88.00"),
    doc_L(40, 160, "Consolidated position report")))
  lb <- list(page = 1L, x_min = 39, x_max = 68, y_min = 99, y_max = 110)
  vb <- list(page = 1L, x_min = 199, x_max = 250, y_min = 99, y_max = 110)
  tmpl <- .pv_tmpl(list(total = list(
    label_text = "Total", label = lb, value = vb, type = "money",
    where = list(where = "below", gap = 10, cross = 0, width = 40, height = 11))))
  expect_identical(doc_pairs(inp, tmpl)$value[1], "88.00")
})

test_that("a template written before sides existed still reads, from its boxes", {
  # Older saved templates carry two boxes and no `where`. The relation is worked
  # out from them, so nothing anybody saved stops working.
  inp <- .pv_input(.pv_page(
    doc_L(40, 100, "Client reference"), doc_L(180, 100, "NW-99-9999-9999"),
    doc_L(40, 140, "Consolidated position report")))
  tmpl <- .pv_tmpl(list(client_reference = list(
    label_text = "Client reference", type = "text",
    label = list(page = 1L, x_min = 39, x_max = 118, y_min = 99, y_max = 110),
    value = list(page = 1L, x_min = 179, x_max = 260, y_min = 99, y_max = 110))))
  got <- doc_pairs(inp, tmpl)
  expect_identical(got$value[1], "NW-99-9999-9999")
  expect_identical(got$found_by[1], "its wording")
})

test_that("a template that names an impossible side is refused, not quietly ignored", {
  t <- .pv_tmpl(list(x = list(label_text = "A", type = "text",
    label = list(page = 1L, x_min = 1, x_max = 2, y_min = 1, y_max = 2),
    value = list(page = 1L, x_min = 3, x_max = 4, y_min = 1, y_max = 2),
    where = list(where = "sideways"))))
  expect_match(paste(validate_document_template(t), collapse = " "),
               "right/left/above/below")
  t$pairs$x$where$where <- "left"
  expect_equal(validate_document_template(t), character(0))
})

test_that("the side is written into the template the builder saves", {
  lb <- list(page = 1L, x_min = 39, x_max = 100, y_min = 99, y_max = 110)
  vb <- list(page = 1L, x_min = 149, x_max = 214, y_min = 99, y_max = 110)
  t <- document_template_from_proposal(
    id = "pv", bank = "B", statement_type = "r", phrases = "Only on this report",
    tables = list(),
    pairs = list(list(name = "prepared_for", label_text = "Prepared for",
                      label = lb, value = vb, type = "text")))
  expect_identical(t$pairs$prepared_for$where$where, "right")
  expect_equal(t$pairs$prepared_for$where$gap, 49)
})

# ---------------------------------------------------------------------------
# Adding and moving columns: gaps allowed, overlaps impossible
#
# The model is the COLUMN LIST, not a set of shared edges. Two goes at this were
# wrong in ways a real person hit within minutes, so both are pinned below as
# counter-examples: fed through doc_edge_click() twice a drag beside the table
# widened the last column over everything it crossed; rewritten to keep every
# edge it invented a column in the gap. What is drawn is what you get.
# ---------------------------------------------------------------------------

.cb <- function(...) lapply(list(...), function(v)
  list(name = sprintf("c%g", v[1]), x_min = v[1], x_max = v[2], type = "auto"))
.spans <- function(cols) t(vapply(cols, function(cc)
  c(.doc_num(cc$x_min), .doc_num(cc$x_max)), numeric(2)))

test_that("adding a column beyond the table adds ONE column, exactly where it was drawn", {
  # THE REPORTED FAULT. One column is carved out and somebody drags over the
  # second printed column, which lies to the right of it.
  cols <- .cb(c(50, 120))
  # what the two-clicks version did: one column stretched over both
  expect_equal(doc_edge_click(doc_edge_click(c(50, 120), 150, tol = 3), 260, tol = 3),
               c(50, 260))
  out <- doc_add_column(cols, 150, 260)
  expect_equal(length(out), 2L)                       # not three: no invented gap column
  expect_equal(.spans(out), rbind(c(50, 120), c(150, 260)))
  expect_false(isTRUE(attr(out, "clamped")))
  # and the first column is exactly where it was put
  expect_equal(.doc_num(out[[1]]$x_min), 50)
  expect_equal(.doc_num(out[[1]]$x_max), 120)
})

test_that("a new column is clamped off its neighbours rather than overlapping them", {
  cols <- .cb(c(50, 120), c(300, 420))
  # runs into the left neighbour: trimmed to its right-hand side
  out <- doc_add_column(cols, 100, 200)
  expect_equal(.spans(out), rbind(c(50, 120), c(120, 200), c(300, 420)))
  expect_true(isTRUE(attr(out, "clamped")))
  # between two, touching neither: exactly as drawn
  out <- doc_add_column(cols, 150, 260)
  expect_equal(.spans(out), rbind(c(50, 120), c(150, 260), c(300, 420)))
  expect_false(isTRUE(attr(out, "clamped")))
  # drawn backwards is the same request
  expect_equal(.spans(doc_add_column(cols, 260, 150)), .spans(out))
  # sorted left to right however it arrives
  expect_equal(.spans(doc_add_column(cols, 10, 40))[1, ], c(10, 40))
})

test_that("a drag that could not be a column adds nothing, and says nothing changed", {
  cols <- .cb(c(50, 120), c(300, 420))
  expect_equal(length(doc_add_column(cols, 200, 200)), 2L)     # no width
  expect_equal(length(doc_add_column(cols, NA, 300)), 2L)
  expect_equal(length(doc_add_column(cols, 200, Inf)), 2L)
  # ENTIRELY INSIDE a column that is already there: there is no room, so nothing
  # is added -- the caller shows a sentence saying why rather than silently
  # doing nothing.
  expect_equal(length(doc_add_column(cols, 60, 100)), 2L)
  # nothing carved yet: the drag is the first column
  expect_equal(.spans(doc_add_column(list(), 150, 260)), rbind(c(150, 260)))
})

test_that("moving a column moves that column and no other", {
  cols <- .cb(c(50, 120), c(300, 420), c(430, 500))
  out <- doc_set_column_band(cols, 2, 280, 400)
  expect_equal(.spans(out), rbind(c(50, 120), c(280, 400), c(430, 500)))
  expect_false(isTRUE(attr(out, "clamped")))
  # widened into a neighbour: clamped at it, and the neighbour does not move --
  # absorbing a column of figures because a drag was wide of the mark was the
  # behaviour before, and it cannot be undone without redrawing both.
  out <- doc_set_column_band(cols, 2, 280, 480)
  expect_equal(.spans(out), rbind(c(50, 120), c(280, 430), c(430, 500)))
  expect_true(isTRUE(attr(out, "clamped")))
  # drawn backwards is the same request
  expect_equal(.spans(doc_set_column_band(cols, 2, 400, 280)),
               .spans(doc_set_column_band(cols, 2, 280, 400)))
})

test_that("a move that could not be a column is refused rather than applied", {
  cols <- .cb(c(50, 120), c(300, 420))
  expect_equal(.spans(doc_set_column_band(cols, 1, 200, 200)), .spans(cols))
  expect_equal(.spans(doc_set_column_band(cols, 9, 10, 20)), .spans(cols))
  expect_equal(.spans(doc_set_column_band(cols, 0, 10, 20)), .spans(cols))
  expect_equal(.spans(doc_set_column_band(cols, 1, NA, 300)), .spans(cols))
  # dragged entirely inside the other column: nowhere to go, nothing changes
  expect_equal(.spans(doc_set_column_band(cols, 1, 320, 400)), .spans(cols))
})

test_that("however columns are added and moved, none of them ever overlaps", {
  # THE ONE INVARIANT LEFT. A gap is visible and counted (unclaimed words); an
  # overlap silently reads a figure into one column and loses it from the other.
  cols <- .cb(c(50, 120), c(300, 420), c(430, 500))
  for (x0 in c(10, 55, 130, 299, 425, 505))
    for (w in c(2, 45, 200, 400)) {
      for (out in list(doc_add_column(cols, x0, x0 + w),
                       doc_set_column_band(cols, 2, x0, x0 + w))) {
        sp <- .spans(out)
        expect_true(all(sp[, 2] > sp[, 1]), info = paste("x0", x0, "w", w))
        expect_false(is.unsorted(sp[, 1]), info = paste("x0", x0, "w", w))
        if (nrow(sp) > 1L)
          expect_true(all(sp[-1, 1] >= sp[-nrow(sp), 2] - 1e-9),
                      info = paste("overlap at x0", x0, "w", w))
      }
    }
})

test_that("whether the columns touch is a question the screen can ask", {
  expect_true(doc_columns_touch(.cb(c(50, 120), c(120, 300))))
  expect_false(doc_columns_touch(.cb(c(50, 120), c(150, 300))))
  expect_true(doc_columns_touch(.cb(c(50, 120))))
  expect_true(doc_columns_touch(list()))
})

# ---------------------------------------------------------------------------
# A column heading is not an amount
# ---------------------------------------------------------------------------

test_that("re-reading a column's name off the page never picks up a figure", {
  # A heading printed over several baselines makes the header row COUNT easy to
  # get one line too generous, and the next re-read then names a column
  # "Revenue Medical & Public Health 47,824,589". That name becomes a worksheet
  # column and a key in the long output, so it is not merely ugly.
  i <- doc_input(list(doc_page(
    doc_L(40, 60, "Regional summary"),
    doc_L(40, 90, "Region"), doc_L(200, 90, "Revenue"),
    doc_L(200, 104, "Medical"),
    doc_L(40, 130, "North"), doc_R(300, 130, "47,824,589"),
    doc_L(40, 152, "South"), doc_R(300, 152, "2,241,609"),
    doc_L(40, 174, "East"),  doc_R(300, 174, "14,874,821"))))
  tab <- list(name = "Regional summary",
              start = list(page = 1L, y = 85), end = list(page = 1L, y = 200),
              header_rows = 3L, follow = FALSE,       # one line too generous
              columns = list(list(name = "Region", x_min = 20, x_max = 180),
                             list(name = "Revenue", x_min = 180, x_max = 320)))
  nm <- doc_header_names(i, tab, NULL, doc_column_edges(tab))
  expect_false(any(grepl("[0-9],[0-9]", nm)), info = paste(nm, collapse = " | "))
  expect_match(nm[2], "Revenue")
})

# ---------------------------------------------------------------------------
# A TEMPLATE MADE ENTIRELY OF VALUES
#
# Reported: "when I don't define any tables, I'm getting error: invalid 'type'
# character of argument when I click read the whole document". A form is a whole
# template with no table on it at all, and it fell over at the last step -- after
# the work, on the screen that shows what came out.
# ---------------------------------------------------------------------------

test_that("an empty summary has the same column types as a full one", {
  # THE CAUSE, in one line. Ten character(0) columns reads as harmless; every
  # caller that adds up a number then meets sum(character(0)), which is an ERROR,
  # not a zero. A zero-row frame is a shape, and a shape that changes with the
  # row count is not one.
  s <- document_summary(list())
  expect_equal(nrow(s), 0L)
  for (k in c("rows", "columns", "thin_columns", "unclaimed_words"))
    expect_true(is.integer(s[[k]]), info = k)
  expect_true(is.numeric(s$confidence))
  # ...and the arithmetic every caller does is arithmetic, not an error
  expect_equal(sum(s$rows), 0L)
  expect_equal(sum(as.integer(s$thin_columns)), 0L)
  expect_equal(sum(!(s$found_by %in% "its heading")), 0L)
})

test_that("reading a document that is all values and no tables works end to end", {
  inp <- .pv_input(.pv_page(
    doc_L(40, 100, "IRD number"), doc_L(140, 100, "123-456-789"),
    doc_L(40, 130, "Consolidated position report")))
  lb <- list(page = 1L, x_min = 39, x_max = 100, y_min = 99, y_max = 110)
  vb <- list(page = 1L, x_min = 139, x_max = 210, y_min = 99, y_max = 110)
  tmpl <- .pv_tmpl(list(ird_number = list(
    label_text = "IRD number", label = lb, value = vb, type = "text",
    where = .doc_pair_rel(lb, vb))))
  ext <- extract_document(inp, tmpl)
  expect_equal(nrow(ext$summary), 0L)
  expect_equal(nrow(ext$pairs), 1L)
  expect_identical(ext$pairs$value[1], "123-456-789")
  # the sums the screen does over that summary
  expect_equal(sum(ext$summary$rows), 0L)
})

test_that("the values CSV is written once, and something offers it", {
  # Reported: "where do the value label pairs come out in the CSV long? I can
  # only see it on workbook". The file was never the problem -- write_document_
  # outputs() has always written <name>.values.csv. NOTHING OFFERED IT. The
  # Convert screen's CSV button resolves the first path ending .csv, which is the
  # tables stacked long, and a label/value pair is none of page/row/column/value.
  # So a document read for its labelled figures downloaded as a file with none of
  # them in it. Pinned here so the file keeps existing, and in test-doc_app.R so
  # a button keeps pointing at it.
  inp <- .pv_input(.pv_page(
    doc_L(40, 100, "IRD number"), doc_L(140, 100, "123-456-789"),
    doc_L(40, 130, "Consolidated position report")))
  lb <- list(page = 1L, x_min = 39, x_max = 100, y_min = 99, y_max = 110)
  vb <- list(page = 1L, x_min = 139, x_max = 210, y_min = 99, y_max = 110)
  ext <- extract_document(inp, .pv_tmpl(list(ird_number = list(
    label_text = "IRD number", label = lb, value = vb, type = "text",
    where = .doc_pair_rel(lb, vb)))))
  od <- file.path(tempdir(), paste0("vcsv", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(od, recursive = TRUE), add = TRUE)
  paths <- write_document_outputs(ext, od, "doc")
  v <- paths[grepl("\\.values\\.csv$", paths)]
  expect_length(v, 1L)                       # ONCE. Written twice it is a bug too.
  expect_true(file.exists(v))
  back <- utils::read.csv(v, stringsAsFactors = FALSE)
  expect_equal(nrow(back), 1L)
  expect_identical(as.character(back$value[1]), "123-456-789")
  # the long CSV is still the tables, and is still what ".csv" resolves to first
  expect_match(basename(paths[grepl("\\.csv$", paths)][1]), "tables-long")
  # and a template with no pairs writes no empty file pretending there were some
  ext0 <- ext; ext0$pairs <- ext$pairs[0, , drop = FALSE]
  od2 <- file.path(tempdir(), paste0("vcsv0", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(od2, recursive = TRUE), add = TRUE)
  expect_length(Filter(function(q) grepl("\\.values\\.csv$", q),
                       write_document_outputs(ext0, od2, "doc")), 0L)
})

test_that("a value's name is the key it comes out under: lower case, underscores", {
  # Reported: "can it just be lower case with _ between the label? ird_number".
  expect_identical(doc_suggest_name("IRD Number"), "ird_number")
  expect_identical(doc_suggest_name("Total paid (GST incl.)"), "total_paid_gst_incl")
  expect_identical(doc_suggest_name("  Closing   Balance  "), "closing_balance")
  expect_identical(doc_suggest_name("2012-13"), "2012_13")
})

# ---------------------------------------------------------------------------
# WHEN NOTHING RECOGNISES IT, WHAT IS IT?
#
# Reported: dropping a non-statement PDF into Convert offers the STATEMENT
# toolkit and nothing else. The card called the file "this statement" twice,
# which is the one thing nobody knows at that point -- all three pipelines have
# just said they cannot tell.
# ---------------------------------------------------------------------------

test_that("a line with a leading date and an amount is recognised whatever the date style", {
  # Deliberately looser than the label extractor's date. Statements print
  # "02 May", "02/05", "2 May 26" and "02-05-2026" as the leading date of a row;
  # requiring a parseable three-part date found 3 rows on a page of 11.
  expect_true(.doc_looks_txn_line("02 May EFTPOS COFFEE HOUSE 5.50 1,244.50"))
  expect_true(.doc_looks_txn_line("02/05 SALARY ACME LTD 3,200.00"))
  expect_true(.doc_looks_txn_line("2 May 26 DD POWER CO 180.20"))
  expect_true(.doc_looks_txn_line("02-05-2026 TRANSFER 12.00"))
  # a date with no amount, and an amount with no date, are both NOT rows
  expect_false(.doc_looks_txn_line("Statement period from 1 May 2026 to 31 May 2026"))
  expect_false(.doc_looks_txn_line("Closing balance $2,716.50"))
  expect_false(.doc_looks_txn_line("Regional health outlay"))
  expect_false(.doc_looks_txn_line(""))
})

test_that("the shape hint reads a statement as a statement and a report as a report", {
  # A STATEMENT: a run of dated amounts, one under the other.
  stmt <- doc_input(list(
    doc_page(doc_L(40, 60, "Kowhai Bank"), doc_L(40, 90, "Closing balance"),
             doc_R(300, 90, "2,716.50")),
    doc_page(doc_L(40, 60, "Date"), doc_L(120, 60, "Details"), doc_R(400, 60, "Amount"),
             doc_L(40, 90, "02 May"),  doc_L(120, 90, "EFTPOS"), doc_R(400, 90, "5.50"),
             doc_L(40, 112, "03 May"), doc_L(120, 112, "SALARY"), doc_R(400, 112, "3,200.00"),
             doc_L(40, 134, "05 May"), doc_L(120, 134, "POWER"),  doc_R(400, 134, "180.20"),
             doc_L(40, 156, "07 May"), doc_L(120, 156, "RENT"),   doc_R(400, 156, "500.00"),
             doc_L(40, 178, "09 May"), doc_L(120, 178, "FUEL"),   doc_R(400, 178, "88.10"))))
  h <- doc_shape_hint(stmt)
  expect_identical(h$looks, "statement")
  expect_gte(h$txn_lines, 4L)
  expect_match(h$sentence, "transaction row", fixed = TRUE)

  # A REPORT: a table of figures with no dates on the rows at all.
  rep <- doc_input(list(doc_page(
    doc_L(40, 60, "Regional outlay"),
    doc_L(40, 90, "Region"), doc_R(400, 90, "Amount"),
    doc_L(40, 120, "North"), doc_R(400, 120, "1,000.00"),
    doc_L(40, 142, "South"), doc_R(400, 142, "2,000.00"),
    doc_L(40, 164, "East"),  doc_R(400, 164, "3,000.00"),
    doc_L(40, 186, "West"),  doc_R(400, 186, "4,000.00"))))
  h2 <- doc_shape_hint(rep)
  expect_identical(h2$looks, "report")
  expect_equal(h2$txn_lines, 0L)
  expect_gte(h2$tables, 1L)

  # NOTHING TO GO ON is its own answer, not a guess at one of the two.
  h3 <- doc_shape_hint(doc_input(list(doc_page(doc_L(40, 60, "Dear Sir")))))
  expect_identical(h3$looks, "neither")
  expect_false(grepl("transaction row", h3$sentence, fixed = TRUE))
})

test_that("a cover page cannot dilute the page that answers the question", {
  # THE BEST PAGE, NOT THE TOTAL. A statement's first page is name, address and
  # four summary figures; its rows start on page 2. Summing across pages lets the
  # cover outvote the evidence.
  cover <- doc_page(doc_L(40, 60, "Kowhai Bank"), doc_L(40, 80, "JAMIE SAMPLE"),
                    doc_L(40, 100, "1 Example Street"), doc_L(40, 120, "Wellington 6011"),
                    doc_L(40, 140, "Account name Everyday"), doc_L(40, 160, "Page 1 of 2"))
  rows <- doc_page(
    doc_L(40, 60, "Date"), doc_R(400, 60, "Amount"),
    doc_L(40, 90, "02 May"),  doc_R(400, 90, "5.50"),
    doc_L(40, 112, "03 May"), doc_R(400, 112, "3,200.00"),
    doc_L(40, 134, "05 May"), doc_R(400, 134, "180.20"),
    doc_L(40, 156, "07 May"), doc_R(400, 156, "500.00"))
  expect_identical(doc_shape_hint(doc_input(list(cover, rows)))$looks, "statement")
})

test_that("an empty name is empty, and never the literal word table", {
  # Reported: "Call this value still defaulting sometimes to the table name rather
  # than the label." doc_suggest_name() falls back to "table" for anything it
  # cannot make a key out of -- right for a TABLE, and the reason a value came out
  # called "table": the debounce observer fed it the empty box, stored the
  # fallback on the draft, and the label drag then refused to overwrite a name
  # that was no longer empty.
  expect_identical(doc_suggest_name(""), "table")     # the fallback still exists
  expect_identical(doc_suggest_name("   "), "table")
  expect_identical(doc_suggest_name("!!!"), "table")
  # ...so the VALUE screen must not call it with an empty box. Asserted where the
  # decision is made rather than by changing a helper two other callers rely on.
  src <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  i <- grep("observeEvent\\(rb_vname_d\\(\\)", src)[1]
  skip_if(is.na(i))
  blk <- paste(src[i:(i + 6L)], collapse = "\n")
  expect_match(blk, "if \\(nzchar\\(typed\\)\\) doc_suggest_name\\(typed\\) else \"\"")
})

# ---------------------------------------------------------------------------
# THE WAY BACK INTO A SAVED REPORT TEMPLATE.
#
# Reported: "the template I set up for other, open in Admin, open template,
# opens bank statement template!" Part of the reason there was no other door to
# open is that nothing could turn a saved report template back into the boxes it
# was drawn from, so a saved template could only ever be edited as text.
# ---------------------------------------------------------------------------

test_that("a report template round-trips through the builder's own form", {
  t <- document_template_from_proposal(
    "rep1", "ACME", "report", c("Quarterly Holdings Report"),
    tables = list(list(
      name = "Holdings", start = list(page = 1, y = 100), end = list(page = 2, y = 700),
      band = list(y_min = 90, y_max = 720), header_rows = 2L, follow = TRUE,
      columns = list(list(name = "Code", x_min = 40, x_max = 100, type = "text"),
                     list(name = "Units", x_min = 110, x_max = 180, type = "number")))),
    pairs = list(list(
      name = "ird_number", label_text = "IRD number",
      label = list(x_min = 40, x_max = 120, y_min = 50, y_max = 62, page = 1),
      value = list(x_min = 130, x_max = 220, y_min = 50, y_max = 62, page = 1),
      type = "text")),
    doc_pages = 2L)

  p <- document_proposal_from_template(t)
  expect_length(p$tables, 1L)
  expect_length(p$pairs, 1L)
  # the NAME comes back on the draft, not just as the key it was filed under
  expect_identical(p$tables[[1]]$name, "Holdings")
  expect_identical(p$pairs[[1]]$name, "ird_number")
  expect_identical(p$pairs[[1]]$label_text, "IRD number")
  # the bottom edge survives -- the control that spent a release doing nothing
  expect_equal(p$tables[[1]]$band$y_max, 720)
  expect_length(.doc_columns(p$tables[[1]]), 2L)

  # and putting it straight back gives the SAME template: an edit that changes
  # nothing must save something identical, or opening a template quietly rewrites it
  t2 <- document_template_from_proposal(
    t$id, t$bank, t$statement_type, unlist(t$fingerprint$page_contains_all),
    p$tables, p$pairs, t$doc_pages)
  t2$ref_width <- t$ref_width; t2$ref_height <- t$ref_height
  expect_identical(t, t2)
})

test_that("a hand-written report template with no names still opens", {
  t <- list(id = "hand", mode = "document",
            fingerprint = list(page_contains_all = list("Annual Return 2024")),
            tables = list(holdings = list(columns = list(
              code = list(x_min = 10, x_max = 50)))),
            pairs = list(total = list(label_text = "Total")))
  p <- document_proposal_from_template(t)
  expect_identical(p$tables[[1]]$name, "holdings")   # the key it was filed under
  expect_identical(p$pairs[[1]]$name, "total")
  expect_identical(p$tables[[1]]$header_rows, 1L)    # the default, made explicit
})

test_that("nothing at all comes back as nothing, not an error", {
  p <- document_proposal_from_template(list(id = "x", mode = "document"))
  expect_identical(p, list(tables = list(), pairs = list()))
  expect_identical(document_proposal_from_template(NULL),
                   list(tables = list(), pairs = list()))
})

# ---------------------------------------------------------------------------
# H6. EVERY OPEN-AND-SAVE ROUND TRIP LOST PART OF THE TEMPLATE
#
# The register listed this under "done today" as identical. It was not.
# document_proposal_from_template -> document_template_from_proposal REBUILT the
# template from a whitelist, so opening a template and saving it again threw away
# everything the builder does not ask about -- including `hidden: true`, which
# means a PARKED template silently came back to life and could start claiming
# documents again, and `version`, a hardcoded literal 1, which is why every report
# template was version 1 for ever and today's copy was indistinguishable from last
# month's in every record kept.
# ---------------------------------------------------------------------------

test_that("an opened-and-saved template keeps everything the builder never asked about", {
  # Start from a template the BUILDER made, so nothing here is about the builder's
  # own normalising (may_be_blank made explicit, `where` derived from the boxes).
  t <- document_template_from_proposal(
    "rep1", "ACME", "report", c("Quarterly Holdings Report"),
    tables = list(list(
      name = "Holdings", start = list(page = 1, y = 100), end = list(page = 2, y = 700),
      band = list(y_min = 90, y_max = 720), header_rows = 2L, follow = TRUE,
      columns = list(list(name = "Code", x_min = 40, x_max = 100, type = "text"),
                     list(name = "Units", x_min = 110, x_max = 180, type = "number")))),
    pairs = list(list(name = "ird_number", label_text = "IRD number",
      label = list(x_min = 40, x_max = 120, y_min = 50, y_max = 62, page = 1),
      value = list(x_min = 130, x_max = 220, y_min = 50, y_max = 62, page = 1),
      type = "text")),
    doc_pages = 2L)
  # ...then everything a real template accumulates and the builder cannot see.
  t$hidden <- TRUE
  t$notes <- "parked while the new format is checked"
  t$version <- 7
  t$refines <- "rep0"
  t$tables$holdings$row_tol <- 4.5
  t$tables$holdings$max_gap <- 12
  t$tables$holdings$optional <- TRUE
  t$tables$holdings$doc_pages <- 9L
  t$ref_width <- 612; t$ref_height <- 792      # a Letter-sized example, not A4

  p <- document_proposal_from_template(t)
  back <- document_template_from_proposal(
    t$id, t$bank, t$statement_type, unlist(t$fingerprint$page_contains_all),
    p$tables, p$pairs, t$doc_pages, base = t)

  expect_true(isTRUE(back$hidden))            # a PARKED template stays parked
  expect_identical(back$notes, t$notes)
  expect_identical(back$refines, t$refines)
  expect_equal(back$tables$holdings$row_tol, 4.5)
  expect_equal(back$tables$holdings$max_gap, 12)
  expect_true(isTRUE(back$tables$holdings$optional))
  expect_equal(back$tables$holdings$doc_pages, 9L)
  expect_equal(back$ref_width, 612)           # the frame is part of the template
  expect_equal(back$ref_height, 792)
  # the version goes UP, it does not go back to 1
  expect_equal(back$version, 8)
  # ...and NOTHING ELSE moved: the whole template is unchanged bar the version.
  # Compared with every named list sorted, because the ORDER keys sit in is not
  # part of what a template says.
  srt <- function(x) if (is.list(x) && !is.null(names(x)))
                       lapply(x[order(names(x))], srt) else x
  was <- t; was$version <- 8
  expect_identical(srt(back), srt(was))
  # it is still a valid template after the round trip
  expect_identical(validate_document_template(back), character(0))
  # and with no base -- a template drawn for the first time -- nothing changes at
  # all: the version is 1 and the result is what it always was
  fresh <- document_template_from_proposal(
    t$id, t$bank, t$statement_type, unlist(t$fingerprint$page_contains_all),
    p$tables, p$pairs, t$doc_pages)
  expect_equal(fresh$version, 1)
  expect_null(fresh$hidden)
})

test_that("a value is never proposed under a label that names a person", {
  # G9b. label_text is captured verbatim off the page, stored in the template and
  # used as the wording the reader searches for. A pair proposed off the awkward
  # value-first arrangement "Mr John Smith  1,240.55" would write a customer's
  # name into templates/documents_user/, and would only ever match that one
  # customer's document anyway. Same gate as the fingerprint.
  p <- doc_page(
    doc_L(40, 60, "Prepared for"),    doc_L(200, 60, "Ambrose Family Trust"),
    doc_L(40, 100, "Mr John Smith"),  doc_L(200, 100, "1,240.55"),
    doc_L(40, 140, "Total contributions"), doc_L(200, 140, "4,000.00"))
  prs <- propose_pairs(doc_input(list(p)), page = 1L)
  labs <- vapply(prs, function(z) z$label_text, character(1))
  expect_false("Mr John Smith" %in% labs)
  expect_true("Prepared for" %in% labs)
  expect_true("Total contributions" %in% labs)
})
