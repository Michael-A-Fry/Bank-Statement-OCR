# test-doc_structure.R -- a section ends where the next section starts.
#
# The fault these pin: a short section followed to the bottom of its page
# swallowed the next section's heading and header as rows. Observed --
#
#   Nominated Persons History
#     row 1 = the single real record
#     row 2 = Address History                 <- the NEXT section's heading
#     row 3 = Tax Type Address Type Address   <- the NEXT section's header row
#
# and the reason no coordinate could fix it: the end position was measured on a
# copy whose earlier sections had the row counts they had that day.
#
# ORDER IS NEVER ENCODED, so the tests permute, omit and re-order sections and
# demand the same answer. If any of these start passing only for a particular
# sequence, the successor-list mistake has come back.

# Where each of the three columns is set. Cells are placed at their own x rather
# than run together from the left margin: a row written as one line puts every
# word inside the first band, which makes a passing row count mean nothing.
STRUCT_X <- c(60, 200, 340)

# One section: heading, column header, then its rows. Returns lines and the y it
# ended at, so a caller can stack sections in any order.
struct_section <- function(y, heading, cols, rows) {
  cells <- function(yy, v) lapply(seq_along(v), function(i) doc_L(STRUCT_X[i], yy, v[i]))
  out <- c(list(doc_L(60, y, heading)), cells(y + 18, cols))
  yy <- y + 18
  for (i in seq_along(rows)) {
    yy <- yy + 16
    out <- c(out, cells(yy, strsplit(rows[[i]], " ", fixed = TRUE)[[1]]))
  }
  list(lines = out, end = yy)
}

# A one-page document holding the named sections, in the order given.
struct_doc <- function(specs, start_y = 80) {
  lines <- list(); y <- start_y
  for (s in specs) {
    sec <- struct_section(y, s$heading, s$cols, s$rows)
    lines <- c(lines, sec$lines)
    y <- sec$end + 26
  }
  doc_input(list(do.call(doc_page, lines)))
}

# The three sections the tests use, as template tables. Deliberately NOT in the
# order any test prints them.
struct_tmpl <- function(v2 = TRUE, keys = c("nominated", "address", "registrations")) {
  mk <- function(name, cols) list(
    name = name, row_tol = 3,
    start = list(page = 1, y = 80), end = list(page = 1, y = DOC_H),
    anchor = list(header_text = cols), header_rows = 1,
    detector = list(headings = name, header_signatures = list(cols)),
    columns = list(list(name = cols[1], x_min = 50,  x_max = 190),
                   list(name = cols[2], x_min = 190, x_max = 330),
                   list(name = cols[3], x_min = 330, x_max = 470)))
  all <- list(
    nominated = mk("Nominated Persons History", c("Person", "Role", "Date")),
    address = mk("Address History", c("Type", "Address", "Applied")),
    registrations = mk("Tax Registrations", c("Tax", "Status", "Registered")))
  t <- list(ref_width = DOC_W, ref_height = DOC_H, tables = all[keys])
  if (v2) t$schema_version <- 2L
  t
}

NOMINATED <- list(heading = "Nominated Persons History",
                  cols = c("Person", "Role", "Date"),
                  rows = list("Smith Director 01/02/2020"))
ADDRESS <- list(heading = "Address History",
                cols = c("Type", "Address", "Applied"),
                rows = list("Postal BoxOne 03/04/2019", "Street WayTwo 05/06/2021"))
REGOS <- list(heading = "Tax Registrations",
              cols = c("Tax", "Status", "Registered"),
              rows = list("GST Active 07/08/2018"))

test_that("the section index holds every validated start, sorted by where it landed", {
  inp <- struct_doc(list(NOMINATED, ADDRESS, REGOS))
  ix <- doc_section_index(inp, struct_tmpl())
  expect_identical(ix$key, c("nominated", "address", "registrations"))
  expect_true(all(diff(ix$y) > 0))            # physical order, not template order
  expect_true(all(ix$score >= 0.6))           # every one carried header evidence
})

test_that("A HEADING STRING IS NOT A SECTION START without a header under it", {
  # The "No Information Held" case: a report listing the sections it holds no
  # data for. Each name is on its own line and matches a heading alias exactly,
  # so wording alone would close three open sections and lose their rows.
  inp <- doc_input(list(doc_page(
    doc_L(60, 80, "No Information Held"),
    doc_L(60, 100, "Nominated Persons History"),   # a VALUE, not a section
    doc_L(60, 116, "Tax Registrations"),           # a VALUE, not a section
    doc_L(60, 160, "Address History"),             # a real one...
    doc_L(60, 178, "Type Address Applied"),        # ...because THIS is under it
    doc_L(60, 194, "Postal BoxOne 03/04/2019"))))
  ix <- doc_section_index(inp, struct_tmpl())
  expect_identical(ix$key, "address")
  expect_equal(nrow(ix), 1L)
})

test_that("a section STOPS before the next section's heading", {
  # The headline fault. Nominated Persons has one row; Address History follows on
  # the same page. Its declared end is the bottom of the paper, so without the
  # boundary the window runs straight through the next two sections.
  inp <- struct_doc(list(NOMINATED, ADDRESS, REGOS))
  tm <- struct_tmpl()
  r <- doc_table_rows(inp, tm$tables$nominated, tm)
  expect_equal(r$n_rows, 1L)
  expect_identical(r$rows$Person, "Smith")
  expect_false(any(grepl("Address History", unlist(r$rows), fixed = TRUE)))
  expect_false(any(grepl("Postal", unlist(r$rows), fixed = TRUE)))
  expect_match(r$detail, "stopped where 'address' begins")
})

test_that("...and the LAST section still runs to the end of the document", {
  inp <- struct_doc(list(NOMINATED, ADDRESS, REGOS))
  tm <- struct_tmpl()
  r <- doc_table_rows(inp, tm$tables$registrations, tm)
  expect_equal(r$n_rows, 1L)                 # nothing follows it; it is not truncated
  expect_identical(r$rows$Tax, "GST")
})

test_that("ORDER INDEPENDENCE: every permutation of the sections reads the same", {
  # No successor is named anywhere, so the sequence in the document is the only
  # thing that decides boundaries. Six orders, one answer.
  perms <- list(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
  all <- list(NOMINATED, ADDRESS, REGOS)
  tm <- struct_tmpl()
  for (p in perms) {
    inp <- struct_doc(all[p])
    expect_equal(doc_table_rows(inp, tm$tables$nominated, tm)$n_rows, 1L,
                 info = paste("order", paste(p, collapse = "")))
    expect_equal(doc_table_rows(inp, tm$tables$address, tm)$n_rows, 2L,
                 info = paste("order", paste(p, collapse = "")))
    expect_equal(doc_table_rows(inp, tm$tables$registrations, tm)$n_rows, 1L,
                 info = paste("order", paste(p, collapse = "")))
  }
})

test_that("an ABSENT optional section does not disturb the sections around it", {
  # The whole reason coordinates could not work: every section that is missing
  # moves everything below it. The template still declares all three.
  inp <- struct_doc(list(NOMINATED, REGOS))         # Address History is not here
  tm <- struct_tmpl()
  expect_equal(doc_table_rows(inp, tm$tables$nominated, tm)$n_rows, 1L)
  expect_equal(doc_table_rows(inp, tm$tables$registrations, tm)$n_rows, 1L)
  ix <- doc_section_index(inp, tm)
  expect_false("address" %in% ix$key)
  # ...and the absent one is absent, not an error and not somebody else's rows.
  expect_identical(doc_locate_table(inp, tm$tables$address, tm)$anchor, "not_found")
})

test_that("a section that GAINS rows still bounds the one after it", {
  big <- ADDRESS
  big$rows <- as.list(sprintf("Postal Box-%d 0%d/04/2019", 1:9, 1:9))
  inp <- struct_doc(list(NOMINATED, big, REGOS))
  tm <- struct_tmpl()
  expect_equal(doc_table_rows(inp, tm$tables$nominated, tm)$n_rows, 1L)
  expect_equal(doc_table_rows(inp, tm$tables$address, tm)$n_rows, 9L)
  expect_equal(doc_table_rows(inp, tm$tables$registrations, tm)$n_rows, 1L)
})

test_that("a VERSION 1 template is not reinterpreted - capping is opt-in", {
  # The backward-compatibility promise. The same document, the same tables, no
  # schema_version: the old exact-coordinate behaviour, bleed and all.
  inp <- struct_doc(list(NOMINATED, ADDRESS, REGOS))
  v1 <- struct_tmpl(v2 = FALSE)
  expect_false(doc_sections_enabled(v1))
  r <- doc_table_rows(inp, v1$tables$nominated, v1)
  expect_gt(r$n_rows, 1L)                    # it still runs on, exactly as before
  # ...and switching it on is what changes the answer, nothing else.
  v2 <- struct_tmpl(v2 = TRUE)
  expect_true(doc_sections_enabled(v2))
  expect_equal(doc_table_rows(inp, v2$tables$nominated, v2)$n_rows, 1L)
})

test_that("boundary_policy turns it on without a schema_version bump", {
  v1 <- struct_tmpl(v2 = FALSE)
  v1$boundary_policy <- list(end_at_nearest_later_section = TRUE)
  expect_true(doc_sections_enabled(v1))
})

test_that("a template that declares only ONE table is bounded by nothing", {
  # Nothing else is configured, so there is no later validated start to stop at
  # and the table keeps the extent the older rules gave it. Proves capping never
  # invents a boundary out of a heading it was not told about.
  inp <- struct_doc(list(NOMINATED, ADDRESS, REGOS))
  tm <- struct_tmpl(keys = "nominated")
  expect_equal(nrow(doc_section_index(inp, tm)), 1L)
  expect_gt(doc_table_rows(inp, tm$tables$nominated, tm)$n_rows, 1L)
})

test_that("the index is deterministic - the same document always sorts the same", {
  inp <- struct_doc(list(NOMINATED, ADDRESS, REGOS))
  tm <- struct_tmpl()
  a <- doc_section_index(inp, tm)
  b <- doc_section_index(inp, tm)
  expect_identical(a, b)
  # ...and it does not depend on the order the tables were named in the template.
  tm2 <- struct_tmpl(keys = c("registrations", "address", "nominated"))
  expect_identical(doc_section_index(inp, tm2)$key, a$key)
})
