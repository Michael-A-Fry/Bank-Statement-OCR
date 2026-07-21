# Year-less date fallback: a statement printing "17 Sep" in its table must not
# lose rows just because the matched template declares a full date format. The
# year comes from the statement's own period (never guessed); every row read
# via the fallback is flagged date_alt_format and surfaced as a diagnostic.
# Evidence: a real ANZ PDF whose rows appeared in the X-ray's skipped table
# with "date didn't parse" while the period ("22 Aug 2018 - 19 Oct 2018")
# carried the year all along.

test_that("ordinal and Sept-style day-month strings normalise and parse", {
  expect_equal(parse_date("17th Sep 2018", "%d %b %Y")$iso, "2018-09-17")
  expect_equal(parse_date("1st Aug 2025", "%d %b %Y")$iso, "2025-08-01")
  expect_equal(parse_date("24 Sept 2018", "%d %b %Y")$iso, "2018-09-24")
})

test_that("a wrong-year-format template still reads year-less dates via the fallback, flagged", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE), "pdftools not installed")
  pdf <- fixture("samples/raw/tutorial/sample_everyday_statement.pdf")
  skip_if_not(file.exists(pdf), "tutorial sample not present")
  tpls <- load_templates(templates_dir())
  tmpl <- tpls[["tutorial_everyday_pdf"]]
  skip_if_not(!is.null(tmpl), "tutorial template not present")

  input <- read_input(pdf)

  # Baseline: the template's own (year-less) format reads all 12 rows cleanly.
  base <- parse_statement(input, tmpl)
  expect_equal(nrow(base$transactions), 12)

  # Break the declared format the way real templates drift: claim full dates.
  tmpl$table$date_format <- "%d/%m/%Y"
  out <- parse_statement(input, tmpl)

  # Same 12 rows survive - read via the fallback with the period's year - and
  # every fallback row is flagged, with identical dates to the clean parse.
  expect_equal(nrow(out$transactions), 12)
  expect_equal(out$transactions$date, base$transactions$date)
  expect_true(all(grepl("date_alt_format", out$transactions$flags)))
  expect_false(any(grepl("date_alt_format", base$transactions$flags)))

  # The X-ray mirror applies the same document-level rule: rows the fallback
  # reads are painted kept, not listed as "date didn't parse".
  lay <- inspect_pdf_layout(input, tmpl)
  kept <- sum(vapply(lay$pages, function(P)
    if (is.null(P$rows) || !nrow(P$rows)) 0L else sum(P$rows$kept), integer(1)))
  expect_gte(kept, 12)
})
