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

# ---------------------------------------------------------------------------
# THE FRONT DOOR: what the verdict is allowed to say "ok" to
# ---------------------------------------------------------------------------

test_that("a report template can be forced, and an id that is not there is refused", {
  # L1. convert_tables() has always taken a template_id and NOTHING ever passed
  # one -- and worse, an id the library does not hold fell through to detection in
  # silence, so "read it with THIS one" could be answered by a different template.
  skip_if_no_pdf()
  tdir <- file.path(tempdir(), paste0("docforce-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  tpl <- file.path(tdir, "templates"); out <- file.path(tdir, "out")
  inp <- read_input(doc_pdf_fixture())
  t <- document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report", tables = propose_tables(inp),
    pairs = propose_pairs(inp, page = 1L), doc_pages = .doc_npages(inp))
  save_document_template(t, tpl)
  # a SECOND template that could never be detected here (its phrase is not printed)
  t2 <- t; t2$id <- "northwind_alternate"
  t2$fingerprint$page_contains_all <- list("Quarterly derivatives schedule")
  save_document_template(t2, tpl)

  forced <- convert_tables(doc_pdf_fixture(), doc_dir = tpl, outdir = out,
                           template_id = "northwind_alternate")
  expect_identical(forced$template_id, "northwind_alternate")
  expect_true(isTRUE(forced$forced_template))
  # ...and the person's choice outranks detection, which would have chosen the other
  auto <- convert_tables(doc_pdf_fixture(), doc_dir = tpl, outdir = out)
  expect_identical(auto$template_id, "northwind_position")

  missing <- convert_tables(doc_pdf_fixture(), doc_dir = tpl, outdir = out,
                            template_id = "no_such_template")
  expect_identical(missing$status, "unsupported")
  expect_match(paste(missing$messages, collapse = " "), "not in the library")
  expect_false(grepl("no_such_template", paste(missing$messages, collapse = " ")))
})

test_that("two report templates that both fit still convert, and the tie is named", {
  # H10 / L2, measured: two valid templates over one fixture, both phrases
  # genuinely printed, gave status "unsupported" and named neither of them.
  skip_if_no_pdf()
  tdir <- file.path(tempdir(), paste0("doctie-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  tpl <- file.path(tdir, "templates"); out <- file.path(tdir, "out")
  inp <- read_input(doc_pdf_fixture())
  t <- document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report", tables = propose_tables(inp),
    pairs = propose_pairs(inp, page = 1L), doc_pages = .doc_npages(inp))
  save_document_template(t, tpl)
  t2 <- t; t2$id <- "northwind_duplicate"
  save_document_template(t2, tpl)

  res <- convert_tables(doc_pdf_fixture(), doc_dir = tpl, outdir = out)
  expect_identical(res$status, "needs_review")          # read, not refused
  expect_true(res$template_id %in% c("northwind_position", "northwind_duplicate"))
  expect_gt(res$n_rows, 90L)
  msg <- paste(res$messages, collapse = " ")
  expect_match(msg, "fit this document equally well")
  expect_match(msg, "retire whichever template is the duplicate")
  expect_false(grepl("northwind_duplicate", msg, fixed = TRUE))   # no ids on the card
  expect_true(isTRUE(res$ambiguous))
})

test_that("words no column claimed decide the verdict, and are written beside it", {
  # G3, measured on the fixture read through a band 120pt out of place: status was
  # `ok`, confidence 1.00, and the four real closing balances were computed into
  # `spilled` and written NOWHERE. unclaimed_words is now a gate, not a footnote.
  skip_if_no_pdf()
  tdir <- file.path(tempdir(), paste0("docspill-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  tpl <- file.path(tdir, "templates"); out <- file.path(tdir, "out")
  inp <- read_input(doc_pdf_fixture())
  t <- document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report", tables = propose_tables(inp),
    pairs = propose_pairs(inp, page = 1L), doc_pages = .doc_npages(inp))
  save_document_template(t, tpl)
  clean <- convert_tables(doc_pdf_fixture(), doc_dir = tpl, outdir = out)
  expect_identical(clean$status, "ok")
  expect_equal(clean$unclaimed_words, 0L)
  expect_false(any(grepl("\\.unclaimed\\.csv$", clean$outputs)))

  # now narrow the LAST column of the first table until it stops claiming its words
  k <- names(t$tables)[1]
  cols <- t$tables[[k]]$columns
  j <- length(cols)
  cols[[j]]$x_min <- cols[[j]]$x_max - 2
  t$tables[[k]]$columns <- cols
  unlink(list.files(tpl, full.names = TRUE))
  save_document_template(t, tpl)
  bad <- convert_tables(doc_pdf_fixture(), doc_dir = tpl, outdir = out)
  expect_identical(bad$status, "needs_review")
  expect_gt(bad$unclaimed_words, 1L)
  u <- bad$outputs[grepl("\\.unclaimed\\.csv$", bad$outputs)]
  expect_length(u, 1L)
  expect_gt(nrow(utils::read.csv(u, stringsAsFactors = FALSE)), 0L)
})

test_that("the build stamp travels from the front door into the file", {
  # G1. A report converts in March and in September the defence asks how row 14
  # was derived; the file used to answer with sheet names and nothing else.
  skip_if_no_pdf()
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE), "openxlsx not installed")
  tdir <- file.path(tempdir(), paste0("docbuild-", as.integer(runif(1, 1, 1e6))))
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  tpl <- file.path(tdir, "templates"); out <- file.path(tdir, "out")
  inp <- read_input(doc_pdf_fixture())
  t <- document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report", tables = propose_tables(inp),
    pairs = propose_pairs(inp, page = 1L), doc_pages = .doc_npages(inp))
  save_document_template(t, tpl)
  res <- convert_tables(doc_pdf_fixture(), doc_dir = tpl, outdir = out,
                        run_id = "0123456789-20260826000000-abcd")
  expect_identical(res$build$template_id, "northwind_position")
  expect_identical(res$build$engine_version, engine_version())
  expect_false(is.na(res$build$source_sha256))
  expect_false(is.na(res$build$template_sha256))
  expect_identical(res$build$run_id, "0123456789-20260826000000-abcd")
  bs <- openxlsx::read.xlsx(res$outputs[grepl("\\.xlsx$", res$outputs)][1],
                            sheet = "Build")
  expect_identical(bs$value[bs$field == "source_sha256"], res$build$source_sha256)
  # ...and with no run id handed over, the file carries none rather than a guess
  res2 <- convert_tables(doc_pdf_fixture(), doc_dir = tpl, outdir = out)
  expect_null(res2$build$run_id)
})

# ---------------------------------------------------------------------------
# THE VERDICT THAT CONTRADICTED ITS OWN BODY, ON THE WHOLE ROUTE
#
# Driven in a browser: a report template whose heading was declared taller than
# the table MATCHES the document and reads nothing. The engine says so plainly
# -- "the Acme report fits this document but read no rows from any of its 1
# table(s)" -- and the diagnostics table an inch below still said `unknown_format`,
# "no templates match the supplied hints", at severity high.
#
# The cause is the running order: the statement pass builds the diagnostics
# before the engine that actually reads the document has spoken. So the swap
# happens where the document engine's answer is adopted, and this drives the
# whole of convert_document() to prove it, rather than the helper alone.
# ---------------------------------------------------------------------------

test_that("a report template that matched and read nothing says so in BOTH places", {
  skip_if_no_pdf()
  path <- doc_pdf_fixture()
  tdir <- file.path(tempdir(), "me-tpl"); unlink(tdir, recursive = TRUE); dir.create(tdir)
  odir <- file.path(tempdir(), "me-out"); unlink(odir, recursive = TRUE); dir.create(odir)
  on.exit(unlink(c(tdir, odir), recursive = TRUE), add = TRUE)

  inp <- read_input(path)
  t <- document_template_from_proposal("me_probe", "Acme", "report",
                                       phrases = header_phrases(inp),
                                       tables  = propose_tables(inp),
                                       pairs   = list())
  # ONE table, its heading declared taller than the table is. It still matches
  # (the fingerprint phrases are untouched) and it is still LOCATED -- every data
  # row is simply swallowed as heading, and there is no second table to rescue
  # the run. This is the shape the browser produced.
  t$tables <- t$tables[1]
  t$tables[[1]]$header_rows <- 200L
  save_document_template(t, tdir)

  res <- convert_document(path, kind = "other", doc_dir = tdir, outdir = odir,
                          formats = "csv", logdir = file.path(tempdir(), "me-logs"))

  expect_identical(res$status, "unsupported")
  # the engine's own sentence reached the screen...
  expect_match(res$messages[1], "fits this document but read no rows")
  expect_identical(res$doc_matched_empty, "me_probe")
  # ...AND THE TABLE BESIDE IT NOW AGREES WITH IT.
  expect_true("matched_but_empty" %in% res$diagnostics$category)
  expect_false("unknown_format" %in% res$diagnostics$category)
  # the two say the same thing, because they are the same string
  expect_match(res$diagnostics$detail[res$diagnostics$category == "matched_but_empty"],
               "fits this document but read no rows")
  # and the cure names the tab this route is actually on, not the statement
  # toolkit, which is what "open it in the toolkit" meant everywhere else
  expect_match(res$diagnostics$how_to_fix[res$diagnostics$category == "matched_but_empty"],
               "Add a template", fixed = TRUE)
  # nothing else in the table was disturbed by the swap
  expect_gte(nrow(res$diagnostics), 1L)
})
