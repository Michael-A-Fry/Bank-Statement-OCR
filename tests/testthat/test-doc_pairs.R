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
# Moving ONE column's band
# ---------------------------------------------------------------------------

test_that("putting a column somewhere moves its two edges and its neighbours give way", {
  e <- c(50, 120, 300, 420, 500)
  expect_equal(doc_set_column_band(e, 2, 110, 310), c(50, 110, 310, 420, 500))
  # widened until it steps clean over the divider beside it: that divider goes
  expect_equal(doc_set_column_band(e, 1, 40, 310), c(40, 310, 420, 500))
  # the last column, taking the right edge with it
  expect_equal(doc_set_column_band(e, 4, 430, 560), c(50, 120, 300, 430, 560))
  # drawn backwards is the same request
  expect_equal(doc_set_column_band(e, 2, 310, 110), c(50, 110, 310, 420, 500))
})

test_that("a band that could not be a column is refused rather than applied", {
  e <- c(50, 120, 300, 420, 500)
  expect_equal(doc_set_column_band(e, 2, 200, 200), e)   # no width
  expect_equal(doc_set_column_band(e, 9, 10, 20), e)     # no such column
  expect_equal(doc_set_column_band(e, 0, 10, 20), e)
  expect_equal(doc_set_column_band(e, 2, NA, 300), e)
  expect_equal(doc_set_column_band(c(50), 1, 10, 20), 50)
})

test_that("however a band is moved, the columns still tile the width", {
  e <- c(50, 120, 300, 420, 500)
  for (j in 1:4) for (x0 in c(30, 90, 250)) {
    e2 <- doc_set_column_band(e, j, x0, x0 + 140)
    cols <- doc_columns_from_edges(e2)
    expect_true(all(diff(e2) > 0))                       # sorted, no zero width
    for (k in seq_len(length(cols) - 1L))                # meeting, never overlapping
      expect_equal(.doc_num(cols[[k]]$x_max), .doc_num(cols[[k + 1L]]$x_min))
  }
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
