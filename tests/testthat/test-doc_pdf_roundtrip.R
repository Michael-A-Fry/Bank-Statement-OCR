# test-doc_pdf_roundtrip.R -- the document extractor against a REAL PDF, read
# back through pdftools.
#
# The unit tests (test-doc_tables.R) build word geometry directly, which is what
# makes them exact and portable. This file covers the one thing they cannot: that
# a PDF rendered by a graphics device, read back by pdftools, produces word boxes
# the proposer and the reader agree about. It therefore asserts STRUCTURE (how
# many tables, which pages, how many columns, that the two readings agree) and
# never exact coordinates -- those depend on the font metrics of whatever box this
# happens to run on, and pinning them would make the suite fail for the wrong
# reason.
#
# Skipped, loudly, wherever the PDF toolchain is not installed.

skip_if_no_pdf <- function() {
  testthat::skip_if_not(requireNamespace("pdftools", quietly = TRUE), "pdftools not installed")
  testthat::skip_if_not(capabilities("cairo") || capabilities("pdf"),
                        "no PDF graphics device")
}

doc_pdf_fixture <- function() {
  src <- file.path(engine_root(), "tests", "testthat", "fixtures",
                   "make_document_fixture.R")
  testthat::skip_if_not(file.exists(src), "fixture generator missing")
  local({ source(src, local = TRUE); make_document_fixture(
    file.path(tempdir(), "complex_document_sample.pdf")) })
}

test_that("a real seven-page PDF proposes the tables that are on it", {
  skip_if_no_pdf()
  path <- doc_pdf_fixture()
  expect_true(file.exists(path))
  inp <- read_input(path)
  expect_equal(.doc_npages(inp), 7L)

  props <- propose_tables(inp)
  # Six: the account summary, the schedule (pages 3-5 as ONE), fees, contacts,
  # the statement of position, and the second account summary on page 7.
  expect_gte(length(props), 5L)
  starts <- vapply(props, function(p) as.integer(p$start$page), integer(1))
  ends   <- vapply(props, function(p) as.integer(p$end$page), integer(1))
  expect_true(any(starts == 2L))
  expect_true(any(starts == 6L))
  expect_true(any(starts == 7L))

  # The schedule: proposed as one table spanning pages 3 to 5, four columns.
  i <- which(starts == 3L)
  expect_equal(length(i), 1L)
  sched <- props[[i]]
  expect_equal(ends[i], 5L)
  expect_equal(length(sched$columns), 4L)
  expect_equal(sched$n_rows, 83L)
  hdr <- vapply(sched$columns, function(cc) as.character(cc$name)[1], character(1))
  expect_identical(hdr, c("Date", "Description", "Amount", "Balance"))
})

test_that("what was proposed is what gets read back out", {
  skip_if_no_pdf()
  inp <- read_input(doc_pdf_fixture())
  props <- propose_tables(inp)
  tmpl <- document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report",
    tables = props, pairs = propose_pairs(inp, page = 1L), doc_pages = .doc_npages(inp))
  expect_equal(validate_document_template(tmpl), character(0))

  ext <- extract_document(inp, tmpl)
  expect_equal(length(ext$tables), length(props))
  # THE ASSERTION THAT MATTERS. The proposer counted the rows one way (runs of
  # tabular lines) and the reader counted them another (words inside a boundary,
  # assigned to bands). If those two ever disagree, the template a person
  # confirmed on screen is not the template that produces their file.
  for (k in seq_along(props)) {
    nm <- names(ext$tables)[k]
    expect_equal(ext$tables[[nm]]$n_rows, props[[k]]$n_rows,
                 info = sprintf("table '%s' (%s)", nm, props[[k]]$name))
  }
  # Nothing inside a table's boundary was left unclaimed by every column.
  expect_equal(sum(ext$summary$unclaimed_words), 0L)
  # ...and every one of them was found by its own heading, not by position.
  expect_true(all(ext$summary$found_by %in% c("its heading", "the rows it starts with")))
})

test_that("the label/value pairs survive the PDF round trip", {
  skip_if_no_pdf()
  inp <- read_input(doc_pdf_fixture())
  pr <- propose_pairs(inp, page = 1L)
  lab <- vapply(pr, function(p) as.character(p$label_text)[1], character(1))
  expect_true("Prepared for" %in% lab)
  expect_true("Client reference" %in% lab)
  expect_true("Total contributions" %in% lab)

  tmpl <- document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report", tables = list(), pairs = pr,
    doc_pages = .doc_npages(inp))
  got <- doc_pairs(inp, tmpl)
  v <- function(k) got$value[got$pair == k]
  expect_identical(v("prepared_for"), "Ambrose Family Trust")
  expect_identical(v("client_reference"), "NW-99-9999-9999")
  expect_identical(v("total_contributions"), "1,240.55")
  expect_true(all(got$found_by[got$pair %in%
    c("prepared_for", "client_reference", "total_contributions")] == "its wording"))
})

test_that("converting the PDF writes downloadable files and never touches the feed", {
  skip_if_no_pdf()
  inp <- read_input(doc_pdf_fixture())
  tdir <- file.path(tempdir(), paste0("docconv-", as.integer(Sys.time())))
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  tpl_dir <- file.path(tdir, "templates"); out_dir <- file.path(tdir, "out")

  tmpl <- document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report", tables = propose_tables(inp),
    pairs = propose_pairs(inp, page = 1L), doc_pages = .doc_npages(inp))
  save_document_template(tmpl, tpl_dir)

  res <- convert_tables(doc_pdf_fixture(), doc_dir = tpl_dir, outdir = out_dir)
  expect_identical(res$kind, "tables")
  expect_true(res$status %in% c("ok", "needs_review"))
  expect_identical(res$template_id, "northwind_position")
  expect_gt(res$n_rows, 90L)
  expect_true(length(res$outputs) > 0L)
  expect_true(all(file.exists(res$outputs)))
  expect_true(any(grepl("tables-long\\.csv$", res$outputs)))

  feed_dir <- file.path(tdir, "feed")
  expect_null(write_feed(res, list(feed = list(enabled = TRUE, feed_dir = feed_dir))))
  expect_false(dir.exists(feed_dir))
})
