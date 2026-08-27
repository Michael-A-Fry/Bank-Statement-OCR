# test-doc_noise.R -- page furniture: the lines a report prints on every page.
#
# The fault these pin is the one the IRD analysis calls the biggest issue: a
# disclaimer, a confidentiality stamp and a "Page 4 of 44" sitting inside an open
# table's window and coming back as its first four rows. Every test here is a
# fact about the DOCUMENT (a line repeats, in a margin, carrying no figure), so
# none of them depends on a vocabulary of footer wordings.

# A report with a running header, a page footer, and a real table in between.
# `rows_money` decides whether the table's rows carry figures, which is one of
# the two guards and needs testing both ways.
noise_doc <- function(npages = 3, footer = "Page N of 3",
                      body = NULL, rows_money = TRUE) {
  pages <- lapply(seq_len(npages), function(p) {
    rows <- if (!is.null(body)) body(p) else {
      lapply(seq_len(4), function(i) {
        y <- 200 + i * 20
        if (rows_money) doc_L(60, y, sprintf("0%d/0%d/2024 Payment-%d -%d.20 1,2%d4.50",
                                             i, p, i * p, 40 + i, i))
        else doc_L(60, y, sprintf("Item-%d Type-%d Detail-%d", i, p, i * p))
      })
    }
    do.call(doc_page, c(
      list(doc_L(60, 28, "*** IN CONFIDENCE ***")),               # running header
      rows,
      list(doc_L(60, 812, sub("N", as.character(p), footer)))))   # page footer
  })
  doc_input(pages)
}

noise_tmpl <- function(...) {
  t <- list(ref_width = DOC_W, ref_height = DOC_H, tables = list())
  utils::modifyList(t, list(...))
}

test_that("a line that repeats in the same margin on every page is page furniture", {
  m <- doc_noise_mask(noise_doc(), noise_tmpl())
  expect_true(doc_is_noise(m, "*** IN CONFIDENCE ***", 28, DOC_H))
  expect_true(doc_is_noise(m, "Page 2 of 3", 812, DOC_H))
  # ...and the page NUMBER did not stop it being one line: 1, 2 and 3 of 3 are
  # the same furniture, which is what collapsing digit runs is for.
  expect_true(doc_is_noise(m, "Page 3 of 3", 812, DOC_H))
})

test_that("no vocabulary of wordings is involved - furniture nobody has ever seen is caught", {
  m <- doc_noise_mask(noise_doc(footer = "Kaitakawaenga Ref 88/QX-4 // Level 1 extract"),
                      noise_tmpl())
  expect_true(doc_is_noise(m, "Kaitakawaenga Ref 12/QX-4 // Level 1 extract", 812, DOC_H))
  # The old detector is a fixed list of footer phrasings, and this is not on it.
  expect_false(.is_footer_noise("Kaitakawaenga Ref 88/QX-4 // Level 1 extract"))
})

test_that("A ROW IS NEVER FURNITURE, however exactly it repeats", {
  # THE FAULT THIS PINS, found by this suite against the first implementation.
  # Collapsing digit runs makes every row of a numeric table normalise to one
  # string -- "01/02/2024 Payment-17 -45.20 1,234.50" and a different row on the
  # next page both become "#/#/# payment-# -#.# #,#.#". Four real rows were
  # suppressed from an 83-row table. Two guards stop it, and both are tested.
  m <- doc_noise_mask(noise_doc(), noise_tmpl())
  expect_false(doc_is_noise(m, "01/02/2024 Payment-17 -45.20 1,234.50", 240, DOC_H))
  # Guard 1: it carries money, so it is data wherever it sits -- even planted in
  # the footer's own margin.
  expect_false(doc_is_noise(m, "01/02/2024 Payment-17 -45.20 1,234.50", 812, DOC_H))
})

test_that("guard 2: a repeating line that MOVES down the page is not furniture", {
  # A table's last row lands wherever the table reached; furniture is printed by
  # the page template and lands at the same height every time. Money is taken out
  # of it so the money guard cannot be what passes this test.
  moving <- function(p) list(doc_L(60, 700 + p * 12, "Item-A Type-B Detail-C"))
  m <- doc_noise_mask(noise_doc(body = moving), noise_tmpl())
  expect_false(doc_is_noise(m, "Item-A Type-B Detail-C", 712, DOC_H))
  # ...while the footer above it, which does not move, still is.
  expect_true(doc_is_noise(m, "Page 1 of 3", 812, DOC_H))
})

test_that("a line in the BODY is never suppressed, however often it repeats", {
  same <- function(p) list(doc_L(60, 400, "Repeated body line with no figures"))
  m <- doc_noise_mask(noise_doc(body = same), noise_tmpl())
  expect_false(doc_is_noise(m, "Repeated body line with no figures", 400, DOC_H))
})

test_that("a one-page document learns nothing - there is no cross-page evidence", {
  m <- doc_noise_mask(noise_doc(npages = 1), noise_tmpl())
  expect_length(m$keys, 0L)
  expect_false(doc_is_noise(m, "Page 1 of 3", 812, DOC_H))
})

test_that("a template rule catches the disclaimer that is printed only ONCE", {
  # Recurrence cannot see a statutory paragraph on a cover page, because it does
  # not recur. That is what the explicit rules are for, and they are asked
  # anywhere on the page rather than only in a margin.
  m <- doc_noise_mask(noise_doc(npages = 1), noise_tmpl(page_noise = list(lines = list(
    list(match = "contains", value = "The information contained in this report"),
    list(match = "regex", value = "^Page [0-9]+ of [0-9]+$")))))
  expect_true(doc_is_noise(m, "The information contained in this report is gathered", 400, DOC_H))
  expect_true(doc_is_noise(m, "Page 7 of 44", 400, DOC_H))
  expect_false(doc_is_noise(m, "A genuine row of somebody's table", 400, DOC_H))
})

test_that("a rule with an unusable match mode or a broken regex is refused, not thrown", {
  m <- doc_noise_mask(noise_doc(npages = 1), noise_tmpl(page_noise = list(lines = list(
    list(match = "fuzzy", value = "anything"),      # not an implemented mode
    list(match = "regex", value = "([unclosed"),    # does not compile
    list(match = "contains", value = "keep me")))))
  expect_length(m$rules, 1L)
  expect_true(doc_is_noise(m, "please keep me", 400, DOC_H))
})

test_that("a table's own repeated column header is protected from recurrence", {
  # A repeated header recurs at the same height page after page and carries no
  # money, which is exactly the shape recurrence looks for. Suppressing it would
  # be worse than the disease: the reader finds the table BY its header.
  hdr <- function(p) list(doc_L(60, 60, "Tax Type Address Type Address Date Applied"))
  tm <- noise_tmpl(tables = list(address_history = list(
    name = "Address History",
    anchor = list(header_text = c("Tax Type", "Address Type", "Address",
                                  "Date Applied")))))
  m <- doc_noise_mask(noise_doc(body = hdr), tm)
  expect_false(doc_is_noise(m, "Tax Type Address Type Address Date Applied", 60, DOC_H))
})

test_that("suppressed lines are COUNTED and said, never silently dropped", {
  m <- doc_noise_mask(noise_doc(), noise_tmpl())
  expect_null(doc_noise_report(m, 0L))
  msg <- doc_noise_report(m, 3L)
  expect_match(msg, "3 lines of page furniture")
  expect_match(msg, "not read as data")
})

test_that("INTEGRATION: furniture inside a table's window does not become rows", {
  # The whole point, end to end. A four-row table with the report's header and
  # footer printed inside the band the template draws round it. Before this, the
  # extra lines came back as rows of the table.
  pages <- lapply(1:3, function(p) doc_page(
    doc_L(60, 28, "*** IN CONFIDENCE ***"),                       # running header
    doc_L(60, 100, "Ref"), doc_L(200, 100, "Detail"),             # the table's header
    doc_L(60, 120, sprintf("A-%d", p)), doc_L(200, 120, "Alpha"),
    doc_L(60, 140, sprintf("B-%d", p)), doc_L(200, 140, "Bravo"),
    doc_L(60, 812, sprintf("Page %d of 3", p))))                  # page footer
  inp <- doc_input(pages)
  tab <- list(name = "Refs", row_tol = 3,
              start = list(page = 1, y = 96), end = list(page = 3, y = DOC_H),
              anchor = list(header_text = c("Ref", "Detail")),
              header_rows = 1,
              columns = list(list(name = "Ref", x_min = 50, x_max = 190),
                             list(name = "Detail", x_min = 190, x_max = 400)))
  tm <- noise_tmpl(tables = list(refs = tab))
  r <- doc_table_rows(inp, tab, tm)
  # Six real rows, and NOTHING the report prints round them. The footer is the
  # only furniture inside the window -- the running header sits above where the
  # table opens -- and the table's own reprinted header is consumed as a header,
  # which is the existing behaviour and must stay that way.
  expect_equal(r$n_rows, 6L)
  expect_equal(r$n_noise, 3L)               # one footer per page
  expect_equal(r$n_header, 3L)              # one reprinted header per page
  expect_identical(sort(unique(r$rows$Detail)), c("Alpha", "Bravo"))
  expect_false(any(grepl("CONFIDENCE", unlist(r$rows), fixed = TRUE)))
  expect_false(any(grepl("Page", unlist(r$rows), fixed = TRUE)))
  expect_true(all(grepl("^[AB]-", r$rows$Ref)))
  expect_match(r$detail, "page furniture")
})
