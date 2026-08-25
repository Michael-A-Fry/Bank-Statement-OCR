# test-doc_app.R -- the REPORT paradigm where it meets the app and the front door.
#
# app.R is not sourced by the suite (it needs a live Shiny session), so the
# screen-level promises are held to their word the way test-app-ui.R does it:
# read the file and assert the rule. The pure helper is lifted out and actually
# CALLED, because "the source mentions a predicate" is not the same fact as "the
# predicate answers correctly".

.da_src <- function() {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  readLines(app, warn = FALSE)
}
.da_joined <- function() paste(.da_src(), collapse = "\n")
# .da_block(pattern, n) -- n lines from the first line matching `pattern`.
.da_block <- function(src, pattern, n) {
  i <- grep(pattern, src)[1]
  skip_if(is.na(i), paste("not found:", pattern))
  paste(src[i:min(length(src), i + n - 1L)], collapse = "\n")
}

# ---------------------------------------------------------------------------
# ONE predicate for "is this a transaction statement?"
# ---------------------------------------------------------------------------

test_that("the transaction half of Convert is gated on ONE predicate, and it is right", {
  src <- .da_src()
  i <- grep("^\\.is_txn_result <- function", src)[1]
  skip_if(is.na(i), ".is_txn_result not found")
  f <- eval(parse(text = paste(src[i], collapse = "\n")))
  expect_true(f(list(status = "ok")))                    # a statement carries no kind
  expect_true(f(list(kind = "statement")))
  expect_false(f(list(kind = "form")))
  expect_false(f(list(kind = "tables")))
  # A result with no kind at all is the ORDINARY case, and `NULL %in% c(...)` is
  # logical(0) -- which `if` rejects outright. Guarding that is the difference
  # between this throwing on no conversion and on every one of them.
  expect_true(f(list()))
})

test_that("no output asks the form question for itself any more", {
  joined <- .da_joined()
  # The one place that is still allowed to name "form" alone is the FORM renderer
  # and its own conditionals -- everything about transactions goes through the
  # shared predicate, or a third kind of document has to be remembered in six
  # places and will not be.
  for (out in c("cv_headline", "cv_summary", "cv_rematch")) {
    blk <- regmatches(joined, regexpr(sprintf("(?s)output\\$%s <- render.{0,160}", out),
                                      joined, perl = TRUE))
    expect_match(blk, ".is_txn_result(res)", fixed = TRUE)
  }
})

test_that("a report result renders its own panel, and never the transactions half", {
  joined <- .da_joined()
  expect_match(joined, 'uiOutput("cv_tables")', fixed = TRUE)
  expect_match(joined, 'output$cv_is_tables <- reactive', fixed = TRUE)
  expect_match(joined, 'outputOptions(output, "cv_is_tables", suspendWhenHidden = FALSE)',
               fixed = TRUE)
  # the transactions panel excludes BOTH kinds, not just the form
  expect_match(joined, 'output.cv_is_tables != true', fixed = TRUE)
})

# ---------------------------------------------------------------------------
# What the report panel has to say
# ---------------------------------------------------------------------------

test_that("the report panel leads with how each table was found and what came out thin", {
  src <- .da_src()
  blk <- .da_block(src, "output\\$cv_tables <- renderUI", 45L)
  expect_match(blk, "found_by")            # how it was found
  expect_match(blk, "thin_columns")        # what came out mostly empty
  expect_match(blk, "unclaimed_words")     # what no column claimed
  expect_match(blk, "verdict verdict-")    # the SHARED verdict card, not a second one
  # and it says why the usual checks are absent rather than leaving a gap
  expect_match(blk, "running balance")
})

test_that("download only is said on the screen where the template is saved", {
  expect_match(.da_joined(), "never goes to the", fixed = TRUE)
})

test_that("one gesture means one thing, and the hint under the picture says which", {
  src <- .da_src()
  hint <- .da_block(src, "output\\$rb_hint <- renderUI", 14L)
  for (t in c("TITLE", "ROW OF COLUMN NAMES", "ENDS", "LABEL"))
    expect_match(hint, t, fixed = TRUE)
  # the drag is dispatched on the STEP, so it cannot mean two things at once
  brush <- .da_block(src, "observeEvent\\(input\\$rb_brush", 130L)
  for (t in c("rb$step == 1L", "rb$step == 2L", "rb$step == 3L",
              "rb$vstep == 1L", "rb$vstep == 2L"))
    expect_match(brush, t, fixed = TRUE)
})

test_that("the builder draws in the document's own point space, so what is drawn is what is saved", {
  src <- .da_src()
  blk <- .da_block(src, "rb_frame <- reactive", 8L)
  expect_match(blk, "page_width")
  expect_match(blk, "page_height")
  # the template carries that frame, or every box is displaced on a non-A4 page
  tpl <- .da_block(src, "rb_template <- reactive", 12L)
  expect_match(tpl, "ref_width")
  expect_match(tpl, "ref_height")
})

test_that("the per-row handlers exist once, not once per redraw", {
  src <- .da_src()
  # Created in fixed lapplys OUTSIDE the renderUI. Inside one, every redraw would
  # stack another observer on every row, so one click would fire many times.
  blk <- .da_block(src, "lapply\\(seq_len\\(\\.RB_MAX_ROWS\\)", 22L)
  for (id in c("rb_rm_", "rb_ed_", "rb_vrm_"))
    expect_match(blk, sprintf("observeEvent\\(input\\[\\[paste0\\(\"%s\"", id))
  cols <- .da_block(src, "lapply\\(seq_len\\(\\.RB_MAX_COLS\\)", 22L)
  for (id in c("rb_cx_", "rb_ck_"))
    expect_match(cols, sprintf("observeEvent\\(input\\[\\[paste0\\(\"%s\"", id))
  for (out in c("rb_saved", "rb_vsaved", "rb_cols")) {
    blk2 <- .da_block(src, sprintf("output\\$%s <- renderUI", out), 20L)
    expect_false(grepl("observeEvent", blk2, fixed = TRUE))
  }
})

test_that("a click means exactly one thing, and the screen says which", {
  src <- .da_src()
  # ONE control decides what a click does, and it is under the picture it
  # controls. A drag always means the same thing on a given tab. Between them
  # there are two gestures to keep straight instead of one gesture with three
  # meanings and a follow-up question.
  tools <- .da_block(src, "output\\$rb_tools <- renderUI", 12L)
  expect_match(tools, 'radioButtons\\("rb_mode"')
  for (m in c('"col"', '"start"', '"end"', '"off"')) expect_match(tools, m, fixed = TRUE)
  # every mode has a sentence saying what clicking will do
  hint <- .da_block(src, "output\\$rb_hint <- renderUI", 24L)
  expect_match(hint, "Click <b>between</b> two columns", fixed = TRUE)
  expect_match(hint, "where this table STARTS", fixed = TRUE)
  expect_match(hint, "where this table ENDS", fixed = TRUE)
  # an end before the start is refused where it happens, not explained later
  blk <- .da_block(src, "observeEvent\\(input\\$rb_click", 34L)
  expect_match(blk, "end the table before it starts")
})

test_that("the columns come from the header row, and stay a tiling of the width", {
  src <- .da_src()
  # Step 2 IS the columns: one drag round the row of column names, and every
  # header cell becomes a column named after it.
  blk <- .da_block(src, "if \\(on_tables && rb\\$step == 2L\\)", 22L)
  expect_match(blk, "doc_columns_from_box\\(")
  expect_match(blk, "doc_auto_end\\(")               # ...and the end is worked out
  # Adding one afterwards moves EDGES, so the columns still tile the width and a
  # gap or an overlap is not expressible.
  add <- .da_block(src, "if \\(on_tables && rb\\$step == 3L\\)", 14L)
  expect_match(add, "doc_column_edges\\(d\\)")
  expect_match(add, "doc_edge_click\\(")
  expect_match(add, "doc_columns_from_edges\\(")
  # ...and removing one re-reads the names, or a merged column is called column_2
  # where the page says "Type Opening".
  rm <- .da_block(src, "observeEvent\\(input\\[\\[paste0\\(\"rb_cx_\"", 16L)
  expect_match(rm, "doc_header_names\\(")
})

test_that("the builder starts blank and takes one table at a time", {
  src <- .da_src(); joined <- .da_joined()
  # No "find the tables". Reading the whole document and offering thirty nearly
  # right tables is the most expensive state to hand somebody.
  expect_false(grepl("Find the tables", joined, fixed = TRUE))
  expect_false(grepl("rb_scan", joined, fixed = TRUE))
  # Three steps, each one gesture, each said on the card.
  card <- .da_block(src, "output\\$rb_step <- renderUI", 60L)
  for (t in c("Step 1 of 3", "Step 2 of 3", "Step 3 of 3")) expect_match(card, t, fixed = TRUE)
  expect_match(card, "round the table's ", fixed = TRUE)
  expect_match(card, "row of column names", fixed = TRUE)
  # and the header is stated, not merely offered as a switch
  expect_match(card, "is <b>not</b> read as data", fixed = TRUE)
})

test_that("one builder covers a form and a report, and it says so", {
  joined <- .da_joined()
  # Two answers to the first question, not three -- and nothing anywhere selects a
  # third that no longer exists. (It did: the link from a Convert result set
  # ts_doctype to "report", which after the merge selects nothing at all, so the
  # page arrived showing the statement toolkit instead of the builder.)
  expect_match(joined, '= "statement",', fixed = TRUE)
  expect_match(joined, '= "other"),', fixed = TRUE)
  sel <- unlist(regmatches(joined, gregexpr(
    'updateRadioButtons\\(session, "ts_doctype", selected = "[a-z]+"', joined)))
  expect_true(length(sel) > 0L)
  expect_true(all(grepl('"other"$', sel)))
  # and the two things it can pull out are the two tabs
  expect_match(joined, 'tabPanel("Tables", value = "tables"', fixed = TRUE)
  expect_match(joined, 'tabPanel("Values", value = "values"', fixed = TRUE)
})

test_that("a table and a value are each saved, then the next one starts", {
  src <- .da_src()
  tb <- .da_block(src, "observeEvent\\(input\\$rb_savetab", 12L)
  expect_match(tb, "rb\\$tables <- c\\(rb\\$tables, list\\(d\\)\\)")
  expect_match(tb, "rb\\$step <- 1L")               # ...and it resets for the next
  vl <- .da_block(src, "observeEvent\\(input\\$rb_vsave", 12L)
  expect_match(vl, "rb\\$pairs <- c\\(rb\\$pairs, list\\(v\\)\\)")
  expect_match(vl, "rb\\$vstep <- 1L")
  # Editing takes it back OUT of the list, so Save always means the same thing.
  ed <- .da_block(src, "observeEvent\\(input\\[\\[paste0\\(\"rb_ed_\"", 12L)
  expect_match(ed, "rb\\$tables <- rb\\$tables\\[-i\\]")
  expect_match(ed, "rb\\$step <- 3L")
})

# ---------------------------------------------------------------------------
# The front door
# ---------------------------------------------------------------------------

test_that("convert_document tries the report path LAST, and only when nothing else read it", {
  f <- deparse(convert_document)
  joined <- paste(f, collapse = "\n")
  i_form <- regexpr("convert_form(", joined, fixed = TRUE)
  i_tab  <- regexpr("convert_tables(", joined, fixed = TRUE)
  expect_gt(i_form, 0L)
  expect_gt(i_tab, i_form)     # the least checkable answer comes after every other
})

test_that("a report converts through the front door as kind 'tables', and the run log says so", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE), "pdftools not installed")
  skip_if_not(capabilities("cairo") || capabilities("pdf"), "no PDF graphics device")
  src <- file.path(engine_root(), "tests", "testthat", "fixtures",
                   "make_document_fixture.R")
  skip_if_not(file.exists(src), "fixture generator missing")
  tdir <- file.path(tempdir(), paste0("docapp-", as.integer(Sys.time())))
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  pdf_path <- local({ source(src, local = TRUE)
    make_document_fixture(file.path(tdir, "report.pdf")) })

  inp <- read_input(pdf_path)
  tmpl <- document_template_from_proposal(
    id = "northwind_position", bank = "Northwind", statement_type = "position report",
    phrases = "Consolidated position report", tables = propose_tables(inp),
    pairs = propose_pairs(inp, page = 1L), doc_pages = .doc_npages(inp))
  save_document_template(tmpl, file.path(tdir, "doc_templates"))

  res <- convert_document(pdf_path, outdir = file.path(tdir, "out"),
                          templates_dir = file.path(engine_root(), "templates"),
                          user_templates_dir = NULL,
                          fields_dir = file.path(engine_root(), "fields_templates"),
                          doc_dir = file.path(tdir, "doc_templates"),
                          logdir = file.path(tdir, "logs"))
  expect_identical(res$kind, "tables")
  expect_true(res$status %in% c("ok", "needs_review"))
  expect_identical(res$run_log$kind, "tables")
  expect_identical(res$run_log$template_origin, "document")
  # A REPORT HAS TABLES, NOT TRANSACTIONS. row_count is the field every dashboard
  # and every drift report counts; a hundred table rows in it would inflate
  # figures a report has no business being part of.
  expect_identical(res$run_log$row_count, 0L)
  expect_gt(res$run_log$n_table_rows, 90L)
  expect_true(length(res$outputs) > 0L)

  # ...and it is still download-only after all that.
  feed <- file.path(tdir, "feed")
  expect_null(write_feed(res, list(feed = list(enabled = TRUE, feed_dir = feed))))
  expect_false(dir.exists(feed))
})

test_that("with no report templates installed nothing about the front door changes", {
  # The statement pass has to actually run for "unsupported" to mean anything, and
  # that needs the template loader.
  skip_if_not(requireNamespace("yaml", quietly = TRUE), "yaml not installed")
  tdir <- file.path(tempdir(), paste0("docapp-none-", as.integer(Sys.time())))
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  p <- file.path(tdir, "not-a-statement.csv")
  writeLines(c("alpha,beta", "1,2"), p)
  res <- convert_document(p, outdir = file.path(tdir, "out"),
                          templates_dir = file.path(engine_root(), "templates"),
                          user_templates_dir = NULL,
                          fields_dir = file.path(engine_root(), "fields_templates"),
                          doc_dir = file.path(tdir, "no-such-folder"),
                          logdir = file.path(tdir, "logs"))
  expect_identical(res$status, "unsupported")
  expect_identical(res$kind, "statement")
})
