# test-doc_matrix.R -- the table whose columns are filing periods.
#
# The fault: a drafted template wrote `31-Mar-18` in as a permanent column name.
# That template is wrong for every customer with a different filing history, and
# wrong for the same customer next year. The years are DATA.
#
# The analysis's own acceptance list for this shape is the spine of these tests:
#   * different filing-year ranges produce the SAME output schema
#   * totals/summary columns are identified separately
#   * a missing year column does not shift values into the wrong year
#   * repeated matrix headers on continuation pages are consumed
#   * one date format is resolved for the WHOLE header set, not element-wise

MX_KEY_MAX <- 250
MX_X <- c(270, 370, 470, 570)          # where each period column is set

# A matrix page. `periods` are the header labels; `rows` is a list of
# list(measure, values) where values is a character vector as long as `periods`
# (NA for a cell the document leaves blank).
mx_page <- function(periods, rows, y0 = 100, header = TRUE) {
  lines <- list(); y <- y0
  if (header) {
    lines <- c(lines, list(doc_L(60, y, "Filing Period")))
    for (i in seq_along(periods))
      lines <- c(lines, list(doc_L(MX_X[i], y, periods[i])))
    y <- y + 20
  }
  for (r in rows) {
    lines <- c(lines, list(doc_L(60, y, r$measure)))
    for (i in seq_along(r$values)) {
      v <- r$values[i]
      if (!is.na(v) && nzchar(v)) lines <- c(lines, list(doc_L(MX_X[i], y, v)))
    }
    y <- y + 18
  }
  do.call(doc_page, lines)
}

mx_tmpl <- function(..., end_page = 1L) {
  tab <- list(
    name = "Individual Income Return", shape = "matrix", row_tol = 3,
    start = list(page = 1, y = 80), end = list(page = end_page, y = DOC_H),
    matrix = utils::modifyList(list(
      header_anchor = list("Filing Period"),
      row_key = list(name = "measure", x_min = 0, x_max = MX_KEY_MAX),
      periods = list(type = "date", formats = list("%d-%b-%y", "%d-%b-%Y"),
                     x_region = list(x_min = MX_KEY_MAX, x_max = 700)),
      summary_column = list(aliases = list("Total", "Totals"), optional = TRUE),
      output = list(name_column = "filing_period", value_column = "value"),
      blank_rows = "drop"), list(...)))
  list(ref_width = DOC_W, ref_height = DOC_H, schema_version = 2L,
       tables = list(iir = tab))
}

MX_ROWS <- list(
  list(measure = "Taxable Income", values = c("41,200.00", "38,905.00", "44,010.00")),
  list(measure = "Total RWT", values = c("1,204.55", "1,180.00", "1,300.10")),
  list(measure = "Refund", values = c("(1,204.55)", NA, "(880.10)")))

test_that("the period columns are read off the DOCUMENT, never from the template", {
  inp <- doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS)))
  tm <- mx_tmpl()
  h <- doc_matrix_header(inp, tm$tables$iir, tm)
  expect_false(is.null(h))
  expect_identical(h$cols$label, c("31-Mar-18", "31-Mar-19", "31-Mar-20"))
  expect_identical(h$cols$period, c("2018-03-31", "2019-03-31", "2020-03-31"))
  expect_true(all(h$cols$x_min < h$cols$x_max))
  # ...and nothing in the template mentions a year.
  expect_false(any(grepl("[0-9]{4}", unlist(tm$tables$iir$matrix))))
})

test_that("DIFFERENT YEAR RANGES GIVE THE SAME SCHEMA - the point of the shape", {
  tm <- mx_tmpl()
  a <- doc_matrix_extract(
    doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS))), tm$tables$iir, tm)
  b <- doc_matrix_extract(
    doc_input(list(mx_page(c("31-Mar-21", "31-Mar-22", "31-Mar-23"), MX_ROWS))), tm$tables$iir, tm)
  expect_identical(names(a$rows), names(b$rows))
  expect_identical(nrow(a$rows), nrow(b$rows))
  # ...and a DIFFERENT NUMBER of periods still gives that same schema.
  two <- lapply(MX_ROWS, function(r) list(measure = r$measure, values = r$values[1:2]))
  c2 <- doc_matrix_extract(
    doc_input(list(mx_page(c("31-Mar-22", "31-Mar-23"), two))), tm$tables$iir, tm)
  expect_identical(names(c2$rows), names(a$rows))
  expect_equal(c2$matrix$n_periods, 2L)
})

test_that("the years land on the right figures, in long form", {
  inp <- doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS)))
  tm <- mx_tmpl()
  r <- doc_matrix_extract(inp, tm$tables$iir, tm)
  ti <- r$rows[r$rows$measure == "Taxable Income", ]
  expect_identical(ti$filing_period, c("2018-03-31", "2019-03-31", "2020-03-31"))
  expect_identical(ti$value, c("41,200.00", "38,905.00", "44,010.00"))
  expect_equal(ti$value__value, c(41200, 38905, 44010))
})

test_that("a parenthesised figure is negative, and the page's own text is kept", {
  inp <- doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS)))
  tm <- mx_tmpl()
  r <- doc_matrix_extract(inp, tm$tables$iir, tm)
  rf <- r$rows[r$rows$measure == "Refund", ]
  expect_identical(rf$value[1], "(1,204.55)")      # verbatim, as printed
  expect_equal(rf$value__value[1], -1204.55)       # and read as a negative
})

test_that("A MISSING CELL DOES NOT SHIFT ITS NEIGHBOURS", {
  # The analysis names this one: "a missing summary cell must not shift values
  # left or right". Refund has nothing in 31-Mar-19; the 31-Mar-20 figure must
  # stay on 31-Mar-20, because every value is placed by WHERE IT SITS and never
  # by how many values came before it on the line.
  inp <- doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS)))
  tm <- mx_tmpl()
  r <- doc_matrix_extract(inp, tm$tables$iir, tm)
  rf <- r$rows[r$rows$measure == "Refund", ]
  expect_equal(nrow(rf), 2L)                       # two cells printed, two rows
  expect_identical(rf$filing_period, c("2018-03-31", "2020-03-31"))
  expect_equal(rf$value__value, c(-1204.55, -880.10))
})

test_that("a TOTAL column is marked as one and is not counted as a period", {
  rows <- list(list(measure = "Taxable Income",
                    values = c("41,200.00", "38,905.00", "124,115.00")))
  inp <- doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "Total"), rows)))
  tm <- mx_tmpl()
  r <- doc_matrix_extract(inp, tm$tables$iir, tm)
  expect_equal(r$matrix$n_periods, 2L)             # the Total is not a third period
  tot <- r$rows[r$rows$is_summary, ]
  expect_equal(nrow(tot), 1L)
  expect_equal(tot$value__value, 124115)
  expect_true(is.na(tot$filing_period))            # a total is not a period
  expect_match(r$detail, "the last column is a total")
})

test_that("a CONTINUATION PAGE reuses the column model - no header is required", {
  # Page 1 establishes the periods; page 2 just goes on listing measures. This is
  # the ordinary shape of these returns and the reason the header cannot be
  # required per page.
  more <- list(list(measure = "PTS Status", values = c("ISS", "ISS", "ISS")),
               list(measure = "Government Subsidies", values = c("500.00", NA, "750.00")))
  inp <- doc_input(list(
    mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS),
    mx_page(NULL, more, y0 = 100, header = FALSE)))
  tm <- mx_tmpl(end_page = 2L)
  r <- doc_matrix_extract(inp, tm$tables$iir, tm)
  expect_true(2L %in% r$rows$page)
  gs <- r$rows[r$rows$measure == "Government Subsidies", ]
  expect_identical(gs$filing_period, c("2018-03-31", "2020-03-31"))
  pts <- r$rows[r$rows$measure == "PTS Status", ]
  expect_identical(pts$value, c("ISS", "ISS", "ISS"))   # a code, not a number
  expect_true(all(is.na(pts$value__value)))             # and not forced to be one
})

test_that("a REPRINTED header on a continuation page is consumed, not read as a measure", {
  inp <- doc_input(list(
    mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS),
    mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"),
            list(list(measure = "Total RWT", values = c("9.00", "8.00", "7.00"))))))
  tm <- mx_tmpl(end_page = 2L)
  r <- doc_matrix_extract(inp, tm$tables$iir, tm)
  expect_false("Filing Period" %in% r$rows$measure)
  expect_false(any(r$rows$value %in% "31-Mar-18"))
})

test_that("ONE date format is resolved for the WHOLE header set", {
  # Element-wise resolution would read some headers under %d-%b-%y and the rest
  # under %d-%b-%Y, putting two periods under one name or inventing a year.
  expect_identical(.mx_resolve_format(c("31-Mar-18", "31-Mar-19"),
                                      c("%d-%b-%y", "%d-%b-%Y")), "%d-%b-%y")
  expect_identical(.mx_resolve_format(c("31-Mar-2018", "31-Mar-2019"),
                                      c("%d-%b-%y", "%d-%b-%Y")), "%d-%b-%Y")
  # None fits the whole set -> NA, and the header is refused rather than guessed.
  expect_true(is.na(.mx_resolve_format(c("31-Mar-18", "not a date"), c("%d-%b-%y"))))
})

test_that("a header whose columns are not periods is refused, not read", {
  rows <- list(list(measure = "Taxable Income", values = c("1.00", "2.00", "3.00")))
  inp <- doc_input(list(mx_page(c("Alpha", "Bravo", "Charlie"), rows)))
  tm <- mx_tmpl()
  expect_null(doc_matrix_header(inp, tm$tables$iir, tm))
  r <- doc_matrix_extract(inp, tm$tables$iir, tm)
  expect_identical(r$anchor, "not_found")
  expect_equal(r$n_rows, 0L)
  expect_match(r$detail, "no filing-period header was found")
})

test_that("a measure blank in EVERY period follows the template's policy", {
  rows <- c(MX_ROWS, list(list(measure = "Excess Imputation", values = c(NA, NA, NA))))
  inp <- doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), rows)))
  drop <- doc_matrix_extract(inp, mx_tmpl()$tables$iir, mx_tmpl())
  expect_false("Excess Imputation" %in% drop$rows$measure)
  expect_equal(drop$matrix$dropped_blank, 1L)
  expect_match(drop$detail, "blank_rows: drop")
  keep_t <- mx_tmpl(blank_rows = "keep")
  kept <- doc_matrix_extract(inp, keep_t$tables$iir, keep_t)
  expect_equal(kept$matrix$dropped_blank, 0L)
})

test_that("the whole document goes through extract_document unchanged", {
  inp <- doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS)))
  tm <- mx_tmpl()
  ex <- extract_document(inp, tm)
  expect_true("iir" %in% names(ex$tables))
  expect_equal(ex$tables$iir$n_rows, 8L)           # 3 + 3 + 2 (Refund has a gap)
  expect_identical(ex$tables$iir$name, "Individual Income Return")
})

test_that("the same document always reads the same", {
  inp <- doc_input(list(mx_page(c("31-Mar-18", "31-Mar-19", "31-Mar-20"), MX_ROWS)))
  tm <- mx_tmpl()
  expect_identical(doc_matrix_extract(inp, tm$tables$iir, tm)$rows,
                   doc_matrix_extract(inp, tm$tables$iir, tm)$rows)
})
