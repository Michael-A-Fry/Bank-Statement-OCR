# Tests for the bulk statement audit (R/batch_audit.R) -- the "paste 250
# statements" review: status, gap clustering, editable draft recommendations, and
# a PII-safe combined report.

.mk_csv <- function(dir, name, lines) { p <- file.path(dir, name); writeLines(lines, p); p }

test_that("batch_audit summarises a mixed set: parsed, unsupported, gaps, drafts", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # two files a shipped template matches (Xero import), two that nothing matches
  xero <- c("*Date,*Amount,Payee,Description,Reference",
            "05/01/2024,-12.50,SECRETPAYEE,coffee,R1",
            "06/01/2024,100.00,ACME,pay,R2")
  .mk_csv(dir, "a_xero.csv", xero); .mk_csv(dir, "b_xero.csv", xero)
  weird <- c("colA;colB;colC", "1;2;3", "4;5;6")
  .mk_csv(dir, "x_unknown.csv", weird); .mk_csv(dir, "y_unknown.csv", weird)

  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  paths <- list.files(dir, full.names = TRUE)
  b <- batch_audit(paths, templates = tmpls)

  expect_equal(nrow(b$per_file), 4L)
  expect_true(all(c("status", "kind", "signature", "amount_style") %in% names(b$per_file)))
  expect_true(sum(b$per_file$status == "unsupported") >= 2)   # the two unknowns
  expect_true(b$feature_gaps$total == 4)
  expect_true(b$feature_gaps$unsupported >= 2)
  # the unsupported files cluster and each cluster gets an editable draft
  expect_true(nrow(b$clusters) >= 1)
  expect_true(length(b$recommendations) >= 1)
  expect_true(nzchar(b$recommendations[[1]]$draft_yaml))
})

test_that("the combined report leaks no PII", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .mk_csv(dir, "s.csv", c("*Date,*Amount,Payee",
                          "05/01/2024,-12.50,SECRETPAYEE12345", "06/01/2024,9.99,ANOTHERSECRET99"))
  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  rep <- format_batch_audit(batch_audit(list.files(dir, full.names = TRUE), templates = tmpls))
  expect_false(grepl("SECRETPAYEE12345", rep))
  expect_false(grepl("ANOTHERSECRET99", rep))
  expect_true(grepl("safe to share", rep))
})

# ---------------------------------------------------------------------------
# FORM documents are covered work, not a coverage gap (#31).
#
# The app's real front door is convert_document(): transaction pipeline first,
# then FORM (mode: fields) extraction. The bulk audit only ever asked the
# transaction half, so a working IRD / KiwiSaver document came back
# "unsupported" -- it inflated the gap count, was clustered as a layout to build,
# and consumed one of the N draft-template recommendations that should have gone
# to a real gap. A coverage report that under-reports coverage sends an analyst
# off to build a template that already exists.
# ---------------------------------------------------------------------------

test_that("a form-template match is reported as its own kind, not as a gap", {
  ftpls <- load_fields_templates(fields_templates_dir())
  skip_if_not(length(ftpls) > 0)
  form_src <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(form_src))
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  # the fixture must actually be a form match, or this test proves nothing
  skip_if_not(isTRUE(detect_form(read_input(form_src), ftpls)$matched))

  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  weird <- c("colA;colB;colC", "1;2;3", "4;5;6")
  .mk_csv(dir, "x_unknown.csv", weird)
  file.copy(form_src, file.path(dir, "a_form.pdf"))

  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  b <- batch_audit(list.files(dir, full.names = TRUE), templates = tmpls,
                   fields_templates = ftpls)

  row <- b$per_file[b$per_file$file_type == "pdf", ]
  expect_equal(nrow(row), 1L)
  expect_identical(row$status, "form")
  expect_identical(row$template, names(ftpls)[1])
  expect_true(row$detected)                 # covered -> counted as detected
  expect_gt(row$n_fields, 0L)               # a form is measured in FIELDS...
  expect_equal(row$n_rows, 0L)              # ...not transaction rows

  # it is NOT a gap: not counted as unsupported, not clustered, no draft spent
  expect_equal(b$feature_gaps$forms, 1L)
  expect_equal(b$feature_gaps$unsupported, 1L)          # only the unknown CSV
  expect_false(any(b$clusters$example_idx == row$idx))
  expect_equal(nrow(b$clusters), 1L)

  # the report says so out loud, and still leaks nothing
  rep <- format_batch_audit(b)
  expect_true(grepl("form \\(labelled-value\\) documents covered by a fields template: 1", rep))
  expect_true(grepl("forms excluded", rep))
})

test_that("form detection never changes a statement's verdict", {
  # The form pass runs only when NO transaction template matched, exactly as
  # convert_document() orders it -- so installing form templates can never
  # reclassify a statement the engine already handles.
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .mk_csv(dir, "s.csv", c("*Date,*Amount,Payee", "05/01/2024,-12.50,Shop"))
  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  paths <- list.files(dir, full.names = TRUE)
  none <- batch_audit(paths, templates = tmpls, fields_templates = list())
  some <- batch_audit(paths, templates = tmpls,
                      fields_templates = load_fields_templates(fields_templates_dir()))
  expect_identical(none$per_file$status, some$per_file$status)
  expect_equal(some$feature_gaps$forms, 0L)
})

# ---------------------------------------------------------------------------
# L4. A REPORT IS COVERED WORK TOO -- the identical fault as the form one above,
# one route later.
#
# convert_document() has a THIRD door after the form one: a mode:document
# template reading a report's many tables. This audit had never heard of it.
# Measured on the shipped document fixture: convert_tables() reads it as ok, 6
# tables, 102 rows, while batch_audit() on the SAME file returned detected FALSE,
# template anz_everyday_pdf, status unsupported, n_rows 0 -- clustered as a
# layout gap, consuming one of the eight draft recommendations, and the draft it
# produced was a BANK STATEMENT draft for a document that is not a statement.
# ---------------------------------------------------------------------------

.ba_doc_fixture <- function() {
  src <- file.path(engine_root(), "tests", "testthat", "fixtures",
                   "make_document_fixture.R")
  testthat::skip_if_not(file.exists(src), "fixture generator missing")
  testthat::skip_if_not(requireNamespace("pdftools", quietly = TRUE), "pdftools not installed")
  testthat::skip_if_not(capabilities("cairo") || capabilities("pdf"), "no PDF device")
  local({ source(src, local = TRUE); make_document_fixture(
    file.path(tempdir(), "ba_document_sample.pdf")) })
}

# .ba_doc_template(path) -- a document template drawn on that fixture, the same
# way test-doc_pdf_roundtrip.R builds one.
.ba_doc_template <- function(path) {
  inp <- read_input(path)
  document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report", tables = propose_tables(inp),
    pairs = propose_pairs(inp, page = 1L), doc_pages = .doc_npages(inp))
}

test_that("a report-template match is covered work, not a gap", {
  src <- .ba_doc_fixture()
  dtpls <- list(northwind_position = .ba_doc_template(src))

  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .mk_csv(dir, "x_unknown.csv", c("colA;colB;colC", "1;2;3", "4;5;6"))
  file.copy(src, file.path(dir, "a_report.pdf"))

  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  b <- batch_audit(list.files(dir, full.names = TRUE), templates = tmpls,
                   doc_templates = dtpls)

  # WHAT THE APP'S OWN FRONT DOOR MAKES OF THE SAME FILE. The assertion is that
  # the audit and the conversion AGREE -- pinning literal counts here would pin
  # today's proposer instead, and the fault was never the size of the number.
  conv <- convert_tables(src, doc_dir = local({
    d <- file.path(tempfile("tc_")); dir.create(d, recursive = TRUE)
    save_document_template(dtpls[[1]], d); d }), outdir = tempfile(), formats = "csv")
  expect_identical(conv$status, "ok")

  row <- b$per_file[b$per_file$file_type == "pdf", ]
  expect_equal(nrow(row), 1L)
  expect_identical(row$status, "report")
  expect_identical(row$template, "northwind_position")
  expect_identical(row$bank, "Northwind")
  expect_true(row$detected)
  # the numbers the analyst is looking for, not a flat zero
  expect_gt(row$n_tables, 0L)
  expect_gt(row$n_rows, 0L)
  expect_equal(row$n_tables, as.integer(conv$n_tables))
  expect_equal(row$n_rows, as.integer(conv$n_rows))
  expect_equal(row$n_fields, 0L)

  # NOT a gap: not unsupported, not clustered, no bank-statement draft spent
  expect_equal(b$feature_gaps$reports, 1L)
  expect_equal(b$feature_gaps$unsupported, 1L)     # only the unknown CSV
  expect_false(any(b$clusters$example_idx == row$idx))
  expect_equal(nrow(b$clusters), 1L)
  expect_equal(length(b$recommendations), 1L)

  rep <- format_batch_audit(b)
  expect_true(grepl("report \\(many-table\\) documents covered by a document template: 1", rep))
  expect_true(grepl(sprintf("holding %d table\\(s\\)", row$n_tables), rep))
  expect_true(grepl("forms excluded", rep))        # the form sentence still stands
})

test_that("report detection never changes a statement's or a form's verdict", {
  # The report pass runs only when NO transaction and NO form template matched,
  # exactly as convert_document() orders it -- so installing report templates can
  # never reclassify work the engine already handles.
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .mk_csv(dir, "s.csv", c("*Date,*Amount,Payee", "05/01/2024,-12.50,Shop"))
  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  paths <- list.files(dir, full.names = TRUE)
  none <- batch_audit(paths, templates = tmpls, doc_templates = list())
  some <- batch_audit(paths, templates = tmpls,
                      doc_templates = list(t = list(id = "t", mode = "document",
                        fingerprint = list(page_contains_all = list("Date")),
                        tables = list())))
  expect_identical(none$per_file$status, some$per_file$status)
  expect_equal(some$feature_gaps$reports, 0L)
})

test_that("the safe-to-share report keeps no words but layout vocabulary", {
  # G9. layout_signature()'s hint reaches format_batch_audit(), which is headed
  # "safe to share - no PII". On a document with no transaction header -- exactly
  # the class that reaches the form and report routes -- the hint used to be the
  # commonest words on the page, which on a real report are the people named on
  # it. Whatever R/layout.R's fallback does, nothing this file writes down may
  # carry a name.
  expect_identical(.layout_hint_safe("ambrose | whitcombe", "pdf"), "")
  expect_identical(.layout_hint_safe("amount | balance | date | description", "pdf"),
                   "amount | balance | date | description")
  expect_identical(.layout_hint_safe("ambrose | amount | whitcombe", "pdf"), "amount")
  # a delimited file's hint is its HEADER ROW: column names, structure by
  # construction, and no frequency fallback exists on that branch to abuse.
  expect_identical(.layout_hint_safe("*amount | *date | payee", "delimited"),
                   "*amount | *date | payee")
  expect_identical(.layout_hint_safe("", "pdf"), "")
  expect_identical(.layout_hint_safe(NULL, "pdf"), "")
})

# ---------------------------------------------------------------------------
# H7. ONE TEMPLATE, MANY EXAMPLES -- "I have just added table 41; do the other 40
# still read?". Without that answer every edit to a forty-table template is a
# change whose blast radius nobody can see.
# ---------------------------------------------------------------------------

test_that("template_check reads every table of a template on every example", {
  src <- .ba_doc_fixture()
  tmpl <- .ba_doc_template(src)
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  file.copy(src, file.path(dir, "2024-Q1.pdf"))
  file.copy(src, file.path(dir, "2024-Q2.pdf"))

  tc <- template_check(tmpl, list.files(dir, full.names = TRUE))
  expect_true(is.data.frame(tc$grid))
  expect_identical(names(tc$grid), names(.tc_grid_proto()))
  # one row per example per table READ -- that IS the grid. A template entry set
  # to be read wherever it appears is read once per place, so the grid counts
  # places, not entries.
  places <- sum(vapply(tmpl$tables, function(tb)
    if (identical(as.character(tb$occurrence %||% "once")[1], "all")) 2L else 1L,
    integer(1)))
  expect_equal(nrow(tc$grid), 2L * places)
  expect_equal(length(unique(tc$grid$file)), 2L)
  expect_true(all(tc$grid$rows > 0L))
  expect_true(all(tc$grid$detected))
  expect_true(all(tc$grid$found_by %in%
                  c("its heading", "the rows it starts with")))
  expect_true(all(tc$grid$unclaimed_words == 0L))
  # ONE sentence, plain words, no ids and no scores
  expect_length(tc$message, 1L)
  expect_true(grepl("^ok: ", tc$message))
  expect_false(grepl("northwind_position", tc$message, fixed = TRUE))
})

test_that("template_check names the tables an edit has stopped reading", {
  src <- .ba_doc_fixture()
  tmpl <- .ba_doc_template(src)
  # table 41: drawn on an example nobody has, and found by nothing on these
  tmpl$tables$ghost <- tmpl$tables[[1]]
  tmpl$tables$ghost$name <- "Nowhere schedule"
  tmpl$tables$ghost$start <- list(page = 99L, y = 100)
  tmpl$tables$ghost$anchor <- list(header_text = list("Zzzzzz", "Qqqqqq"))
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  file.copy(src, file.path(dir, "2024-Q1.pdf"))

  tc <- template_check(tmpl, list.files(dir, full.names = TRUE))
  g <- tc$grid[tc$grid$item == "ghost", ]
  expect_equal(nrow(g), 1L)
  # ABSENT, not empty: the two are different facts and the grid says which
  expect_equal(g$rows, 0L)
  expect_identical(g$found_by, "not on this document")
  # ...and the other tables are unaffected
  expect_true(all(tc$grid$rows[tc$grid$item != "ghost"] > 0L))
  expect_true(grepl("Nowhere schedule", tc$message, fixed = TRUE))
  expect_true(grepl("^needs_review: ", tc$message))
})

test_that("template_check refuses a bank statement template out loud", {
  # A statement is proved by its own arithmetic, and a folder of them is checked
  # by converting them. Refused with a sentence, never with an empty grid that
  # reads as "nothing wrong".
  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  skip_if_not(length(tmpls) > 0)
  tc <- template_check(tmpls[[1]], "anything.pdf")
  expect_equal(nrow(tc$grid), 0L)
  expect_identical(names(tc$grid), names(.tc_grid_proto()))
  expect_true(grepl("bank statement template", tc$message, fixed = TRUE))
})

test_that("template_check says so when no examples were given", {
  tc <- template_check(list(id = "t", mode = "document", tables = list()), character(0))
  expect_equal(nrow(tc$grid), 0L)
  expect_true(grepl("no example documents", tc$message, fixed = TRUE))
})

test_that("template_check works the same way for a form template", {
  ftpls <- load_fields_templates(fields_templates_dir())
  skip_if_not(length(ftpls) > 0)
  src <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(src))
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))

  tc <- template_check(ftpls[[1]], src)
  expect_identical(names(tc$grid), names(.tc_grid_proto()))
  expect_equal(nrow(tc$grid), length(ftpls[[1]]$fields))
  expect_true(any(tc$grid$rows > 0L))
  # a form has no columns for a word to fall outside of and nothing to be
  # confident about: NA, because "does not apply" is not "none"
  expect_true(all(is.na(tc$grid$confidence)))
  expect_true(all(is.na(tc$grid$unclaimed_words)))
  expect_length(tc$message, 1L)
})
