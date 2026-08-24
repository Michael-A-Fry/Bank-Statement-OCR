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

test_that("the report builder is a third answer to the first question, not a hidden mode", {
  joined <- .da_joined()
  expect_match(joined, "= \"report\")", fixed = TRUE)          # the radio choice
  expect_match(joined, "input.ts_doctype == 'report'", fixed = TRUE)
  # download only, said on the screen where the template is saved
  expect_match(joined, "never goes to the dashboards", fixed = TRUE)
})

test_that("the builder finds the tables before it asks anyone to draw one", {
  src <- .da_src()
  blk <- .da_block(src, "observeEvent\\(input\\$rb_scan", 20L)
  expect_match(blk, "propose_tables\\(")
  expect_match(blk, "propose_pairs\\(")
  # every page, not a sample: a table on page 31 is the whole point
  expect_false(grepl("pages = 1", blk, fixed = TRUE))
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

test_that("the navigator's click handlers exist once, not once per redraw", {
  src <- .da_src()
  # Created in a fixed lapply OUTSIDE the renderUI. Inside it, every redraw would
  # stack another observer on every row, so one click would fire many times.
  blk <- .da_block(src, "lapply\\(seq_len\\(\\.RB_MAX_ROWS\\)", 16L)
  expect_match(blk, "observeEvent\\(input\\[\\[paste0\\(\"rb_pick_t\"")
  expect_match(blk, "observeEvent\\(input\\[\\[paste0\\(\"rb_pick_p\"")
  nav <- .da_block(src, "output\\$rb_nav <- renderUI", 33L)
  expect_false(grepl("observeEvent", nav, fixed = TRUE))
})

test_that("an end before the start is refused in the builder, not saved and explained later", {
  src <- .da_src()
  blk <- .da_block(src, "\\.rb_set_edge <- function", 24L)
  expect_match(blk, "end the table before it starts")
  expect_match(blk, "return\\(\\)")
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
    pairs = propose_pairs(inp, 1L), doc_pages = inp$page_count)
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
