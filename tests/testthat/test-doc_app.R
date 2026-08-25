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

# ---------------------------------------------------------------------------
# NOTHING HAPPENS UNTIL IT IS ASKED FOR.
#
# The version before this treated a drag as a request to make a table, so a
# mis-drag made one, a second look at the page made one, and correcting one made
# another. A gesture that always means "make me a new thing" cannot also mean
# "fix the thing I am looking at" -- and fixing is most of the work.
test_that("one armed intent at a time, named on screen, and nothing armed by default", {
  src <- .da_src(); joined <- .da_joined()
  # every mode has a sentence, and the sentence names the gesture in capitals
  ask <- .da_block(src, "\\.RB_ASK <- list\\(", 22L)
  for (m in c("title", "cols", "addcol", "colpos", "start", "end", "label", "value"))
    expect_match(ask, sprintf("\\n\\s*%s\\s*=", m))
  for (t in c("TITLE", "ROW OF COLUMN NAMES", "NEW COLUMN", "STARTS", "ENDS",
              "LABEL", "VALUE"))
    expect_match(ask, t, fixed = TRUE)
  # the banner across the top reads its sentence out of that one list...
  arm <- .da_block(src, "output\\$rb_arm <- renderUI", 20L)
  expect_match(arm, "\\.RB_ASK\\[\\[rb\\$mode")
  expect_match(arm, "Nothing is waiting for a drag")
  # ...and so does the hint under the picture, so they can never disagree
  hint <- .da_block(src, "output\\$rb_hint <- renderUI", 8L)
  expect_match(hint, "\\.RB_ASK\\[\\[rb\\$mode")
  # THE RULE ITSELF: a drag with nothing armed changes nothing and says so.
  brush <- .da_block(src, "observeEvent\\(input\\$rb_brush", 190L)
  expect_match(brush, "if \\(!nzchar\\(m\\)\\)")
  expect_match(brush, "Nothing was waiting for that drag, so nothing changed")
  # and it dispatches on the mode alone -- not on which tab happens to be open
  for (t in c('m == "title"', 'm == "cols"', 'm == "addcol"', 'm == "colpos"',
              'm == "label"', 'm == "value"'))
    expect_match(brush, t, fixed = TRUE)
  expect_false(grepl("on_tables", brush, fixed = TRUE))
  # nothing is armed unless a button armed it
  expect_match(joined, 'rb\\$mode <- "title"')
  expect_match(joined, 'actionButton\\("rb_addtable"')
  expect_match(joined, 'actionButton\\("rb_addval"')
  expect_match(joined, 'mode = "",')
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
  blk <- .da_block(src, "lapply\\(seq_len\\(\\.RB_MAX_ROWS\\)", 40L)
  for (id in c("rb_rm_", "rb_ed_", "rb_vrm_", "rb_ved_"))
    expect_match(blk, sprintf("observeEvent\\(input\\[\\[paste0\\(\"%s\"", id))
  cols <- .da_block(src, "lapply\\(seq_len\\(\\.RB_MAX_COLS\\)", 22L)
  for (id in c("rb_cx_", "rb_cs_"))
    expect_match(cols, sprintf("observeEvent\\(input\\[\\[paste0\\(\"%s\"", id))
  for (out in c("rb_saved", "rb_vsaved", "rb_cols")) {
    blk2 <- .da_block(src, sprintf("output\\$%s <- renderUI", out), 20L)
    expect_false(grepl("observeEvent", blk2, fixed = TRUE))
  }
})

# WHERE IT STARTS AND WHERE IT STOPS decide every row that comes out, so they are
# written on the screen in words with a button beside each -- not left as an
# undocumented click on the picture and a sentence in a hint.
test_that("the start and the end are on the screen in words, and easy to move", {
  src <- .da_src(); joined <- .da_joined()
  blk <- .da_block(src, "output\\$rb_where <- renderUI", 26L)
  expect_match(blk, "Starts", fixed = TRUE)
  expect_match(blk, "Ends", fixed = TRUE)
  expect_match(blk, "down the page", fixed = TRUE)        # said as a place, not a number
  expect_match(blk, '"rb_setstart"'); expect_match(blk, '"rb_setend"')
  expect_match(blk, '"rb_gostart"'); expect_match(blk, '"rb_goend"')
  # and the three answers people actually want, as one press each
  for (b in c("rb_endauto", "rb_endpage", "rb_endlast"))
    expect_match(blk, sprintf('"%s"', b))
  # a click only ever moves one of those two, and only when it was asked for
  cl <- .da_block(src, "observeEvent\\(input\\$rb_click", 32L)
  expect_match(cl, 'if \\(!\\(m %in% c\\("start", "end"\\)\\)\\) return\\(\\)')
  # an end before the start, or a start after the end, is refused where it
  # happens rather than explained later
  expect_match(cl, "past where the table ends")
  expect_match(cl, "above where the table starts")
  # and both markers are drawn on the page, named
  plot <- .da_block(src, "\\.rb_draw_table <- function", 52L)
  expect_match(plot, '"START')
  expect_match(plot, '"END"')
})

test_that("the columns come from the header row, and stay a tiling of the width", {
  src <- .da_src()
  # One drag round the row of column names, and every header cell becomes a
  # column named after it -- and the end is worked out from there.
  blk <- .da_block(src, 'if \\(m == "cols"\\)', 46L)
  expect_match(blk, "doc_columns_from_box\\(")
  expect_match(blk, "doc_auto_end\\(")
  # Adding one afterwards moves EDGES, so the columns still tile the width and a
  # gap or an overlap is not expressible.
  add <- .da_block(src, 'if \\(m == "addcol"\\)', 16L)
  expect_match(add, "doc_column_edges\\(d\\)")
  expect_match(add, "doc_edge_click\\(")
  expect_match(add, "doc_columns_from_edges\\(")
  # ...and removing one re-reads the names, or a merged column is called column_2
  # where the page says "Type Opening".
  rm <- .da_block(src, "\\.rb_drop_col <- function", 16L)
  expect_match(rm, "doc_header_names\\(")
})

# EVERYTHING THE TOOL WORKED OUT FOR ITSELF CAN BE CHANGED. That is the whole
# point of showing it: a column band, a column name, what is in it, and the
# order the tool put them in are all guesses until somebody has looked.
test_that("every column can be renamed, retyped, repositioned and deleted", {
  src <- .da_src(); joined <- .da_joined()
  # ONE panel edits ONE column, and it is built once -- an input rebuilt inside a
  # renderUI is destroyed on every keystroke that changes what it edits.
  for (id in c("rb_cname", "rb_ckind", "rb_cx0", "rb_cx1"))
    expect_match(joined, sprintf('"%s"', id))
  fill <- .da_block(src, "observeEvent\\(list\\(rb\\$colsel, rb\\$colver\\)", 14L)
  for (u in c("updateTextInput", "updateSelectInput", "updateNumericInput"))
    expect_match(fill, u, fixed = TRUE)
  # typing is debounced before it reaches the draft the list is drawn from
  expect_match(joined, "rb_cname_d <- debounce\\(")
  expect_match(joined, "rb_cx_d <- debounce\\(")
  expect_match(joined, "rb_name_d <- debounce\\(")
  # a band typed and a band dragged are the SAME request, through one function
  expect_match(joined, "doc_set_column_band\\(e, j, x0, x1\\)")
  expect_match(joined, "doc_set_column_band\\(e, j, box\\$x_min, box\\$x_max\\)")
  # and moving a column does not rename it back to whatever is printed above it
  mv <- .da_block(src, 'if \\(m == "colpos"\\)', 22L)
  expect_match(mv, "keep_nm")
  # ...and the one being edited is picked out on the picture
  expect_match(joined, "PALETTE\\$warn, lwd = 3\\)")
})

# The tool draws over a document somebody may need to READ. The column names it
# worked out are checked against the column names printed on the page -- so they
# float above the paper instead of sitting on it, and every layer switches off.
test_that("the tool's own labels float above the page, and each layer switches off", {
  src <- .da_src(); joined <- .da_joined()
  ui <- .da_block(src, 'checkboxGroupInput\\("rb_layers"', 8L)
  for (v in c("tables", "edges", "names", "values", "ends"))
    expect_match(ui, sprintf('= "%s"', v))
  # a strip of white ABOVE the paper, and the names drawn in it
  expect_match(joined, "\\.RB_GUTTER <- ")
  plot <- .da_block(src, "output\\$rb_plot <- renderPlot", 40L)
  expect_match(plot, "ylim = c\\(r\\$h, -\\.RB_GUTTER\\)")
  expect_match(plot, "input\\$rb_layers")
  nm <- .da_block(src, "\\.rb_draw_names <- function", 22L)
  expect_match(nm, "-\\.RB_GUTTER \\+")                # the names live in the strip
  expect_match(nm, "\\.rb_fit\\(")                     # trimmed to the width they have
  # every layer is actually consulted before anything is drawn
  for (l in c('"tables" %in% lay', '"edges" %in% lay', '"ends" %in% lay',
              '"names" %in% lay', '"values" %in% lay'))
    expect_match(joined, l, fixed = TRUE)
})

# EVERY COLUMN IS A COLUMN. The picture used to tint only the odd-numbered
# bands, meaning to show where one ends and the next begins -- but on screen
# that reads as "three of these are selected and three are not", while the list
# beside it says six. A stripe is fine; a stripe made of a thing and NOTHING is
# not. The one column with a thick outline round it is the one being edited, and
# it is the only thing on the picture that means "selected".
test_that("every column band is tinted, and only the edited one is outlined", {
  src <- .da_src(); joined <- .da_joined()
  blk <- .da_block(src, "\\.rb_draw_table <- function", 34L)
  # the fill is unconditional -- the alternation decides which SHADE, not whether
  expect_match(blk, "if (j %% 2L == 1L) dark else pale", fixed = TRUE)
  expect_match(blk, "pale <- if (lwd > 1)", fixed = TRUE)
  expect_match(blk, "dark <- if (lwd > 1)", fixed = TRUE)
  # the band's own rect is not guarded by which band it is
  expect_false(grepl("if (j %% 2L == 1L)\n            rect(", blk, fixed = TRUE))
  # and exactly one thing draws a thick outline round a single band
  sel <- grep("border = PALETTE\\$warn, lwd = 3\\)", src)
  expect_length(sel, 1L)
})

test_that("the builder starts blank and takes one table at a time", {
  src <- .da_src(); joined <- .da_joined()
  # No "find the tables". Reading the whole document and offering thirty nearly
  # right tables is the most expensive state to hand somebody.
  expect_false(grepl("Find the tables", joined, fixed = TRUE))
  expect_false(grepl("rb_scan", joined, fixed = TRUE))
  # It opens with one button and a sentence saying what a table is.
  expect_match(joined, "A table is rows and columns", fixed = TRUE)
  expect_match(joined, "A value is one figure or one word, with a label", fixed = TRUE)
  expect_match(joined, "created until you press it", fixed = TRUE)
  # the header is STATED as a choice between two sentences, not offered as a
  # switch whose off position has to be guessed at
  ui <- .da_block(src, 'radioButtons\\("rb_hdr"', 5L)
  expect_match(ui, "name the columns, and are not read as data", fixed = TRUE)
  expect_match(ui, "are data, like every other row", fixed = TRUE)
  # and every saved table can be taken back out and edited
  expect_match(joined, 'paste0\\("rb_ed_", i\\), "Edit"')
  ed <- .da_block(src, "observeEvent\\(input\\[\\[paste0\\(\"rb_ed_\"", 14L)
  expect_match(ed, "\\.rb_load_draft\\(d\\)")
  expect_match(ed, "rb\\$tables <- rb\\$tables\\[-i\\]")
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

test_that("a table and a value are each saved, and the screen goes back to idle", {
  src <- .da_src()
  tb <- .da_block(src, "observeEvent\\(input\\$rb_savetab", 14L)
  expect_match(tb, "rb\\$tables <- c\\(rb\\$tables, list\\(d\\)\\)")
  expect_match(tb, "rb\\$draft <- NULL")
  expect_match(tb, 'rb\\$mode <- ""')            # nothing armed after a save
  vl <- .da_block(src, "observeEvent\\(input\\$rb_vsave", 24L)
  expect_match(vl, "rb\\$pairs <- c\\(rb\\$pairs, list\\(v\\)\\)")
  expect_match(vl, "rb\\$vdraft <- NULL")
  expect_match(vl, 'rb\\$mode <- ""')
  # ...and the side is stored on the way in, whether it was worked out or chosen
  expect_match(vl, "\\.doc_pair_rel\\(v\\$label, v\\$value\\)")
  expect_match(vl, "input\\$rb_vwhere")
  # Editing takes it back OUT of the list, so Save always means the same thing.
  for (id in c("rb_ed_", "rb_ved_")) {
    ed <- .da_block(src, sprintf("observeEvent\\(input\\[\\[paste0\\(\"%s\"", id), 14L)
    expect_match(ed, "rb\\$(tables|pairs) <- rb\\$(tables|pairs)\\[-i\\]")
    expect_match(ed, "\\.rb_load_(draft|vdraft)\\(")
    expect_match(ed, "updateNumericInput\\(session, \"rb_page\"")
  }
})

# A HEADING CAN BE MORE THAN ONE LINE. "Revenue" over "Medical & Family" over
# "Welfare" is three printed lines and one heading; calling only the first of
# them a heading puts the other two into the table as rows of figures that are
# not figures. (Seen on a real government report.)
test_that("a heading is as tall as the box drawn round it", {
  src <- .da_src(); joined <- .da_joined()
  blk <- .da_block(src, 'if \\(m == "cols"\\)', 46L)
  expect_match(blk, "\\.doc_lines\\(below, d\\$row_tol\\)")
  expect_match(blk, "d\\$header_rows <- max\\(1L, min\\(8L")
  expect_match(joined, 'numericInput\\("rb_hdrn"')
  # the choice and the count are ONE setting, read together
  ob <- .da_block(src, "observeEvent\\(list\\(input\\$rb_hdr, input\\$rb_hdrn\\)", 10L)
  expect_match(ob, 'identical\\(input\\$rb_hdr, "data"\\)')
  expect_match(ob, "input\\$rb_hdrn")
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

# ---------------------------------------------------------------------------
# JUST AS EASY AS A BANK STATEMENT
#
# Measured, in a browser, on the two paths: a statement took 4 decisions (say
# what it is, choose the file, open the toolkit, Save) and a document took 10 --
# because the document builder asked by hand for the three things the statement
# toolkit fills in for you. It now fills the same three in the same way, and a
# document takes 7: the extra three are the irreducible ones, "+ Add a table"
# and the two drags that say WHICH table.
# ---------------------------------------------------------------------------

test_that("the document builder fills in the same three boxes the statement toolkit does", {
  src <- .da_src(); joined <- .da_joined()
  blk <- .da_block(src, "rb_id_auto <- reactiveVal", 44L)
  # the issuer, guessed from the filename the same way open_guided guesses it
  expect_match(blk, "tools::toTitleCase")
  expect_match(blk, 'updateTextInput\\(session, "rb_bank"')
  # the phrases the document prints about itself, found by the SAME helper the
  # statement drafter uses -- retyping a fingerprint by hand is where it goes wrong
  expect_match(blk, "header_phrases\\(rb_input\\(\\)\\)")
  expect_match(blk, 'updateTextAreaInput\\(session, "rb_fp"')
  # the save name, composed from the two, through the one shared function
  expect_match(blk, "\\.compose_id\\(input\\$rb_bank, input\\$rb_type")
  # ...and it stops following the moment she names it herself
  expect_match(blk, "rb_id_auto\\(\\)")
  expect_match(blk, "# hers now")
  # a table's TITLE beats its column names as a fingerprint, and the builder has
  # one as soon as a table is saved
  ttl <- .da_block(src, "observeEvent\\(rb\\$tables, \\{", 12L)
  expect_match(ttl, "rb_fp_auto\\(")
  expect_match(ttl, 'updateTextAreaInput\\(session, "rb_fp"')
  # neither ever overwrites something a person typed
  expect_match(ttl, "!identical\\(cur, trimws\\(rb_fp_auto\\(\\)")
})

test_that("every position is both draggable and typeable", {
  src <- .da_src(); joined <- .da_joined()
  for (id in c("rb_sp", "rb_sy", "rb_ep", "rb_ey"))
    expect_match(joined, sprintf('numericInput\\("%s"', id))
  # filled from the draft when a draft arrives and after every gesture that moves
  # a boundary -- never on a timer, because they are typed into
  expect_match(joined, "\\.rb_push_where <- function")
  expect_gte(length(grep("\\.rb_push_where\\(", src)), 5L)
  # typed positions are debounced and refused where a click would be refused
  blk <- .da_block(src, "rb_where_d <- debounce", 22L)
  expect_match(blk, "ep < sp \\|\\| \\(ep == sp && ey <= sy\\)")
  expect_match(blk, "end the table at or before it starts")
})

# ---------------------------------------------------------------------------
# ONE COLUMN IS ALMOST NEVER WHAT THE DRAG MEANT
#
# A box holding one unbroken run of words -- a sentence, the table's title, a
# wrapped heading -- has no gutter in it, so it makes ONE column and every row
# then arrives whole in a single cell. That is arithmetically correct and, nine
# times in ten, a box drawn round the wrong thing. Worse, a one-column table
# cannot have an unclaimed word or a mostly-empty column, so it sails through
# every check the verdict card makes and reports "every column filled".
# Measured: five of fifteen documents in a run over other people's PDFs.
# ---------------------------------------------------------------------------

test_that("one column out of a header drag is said out loud, twice", {
  src <- .da_src(); joined <- .da_joined()
  blk <- .da_block(src, 'if \\(m == "cols"\\)', 52L)
  expect_match(blk, "if \\(length\\(cols\\) == 1L\\)")
  expect_match(blk, "ONE column and each row arrives whole in it", fixed = TRUE)
  expect_match(blk, 'type = "warning"')
  # ...and again on the verdict card, so it is not only said at the moment of
  # the drag and then forgotten
  v <- .da_block(src, "output\\$rb_prev_status <- renderUI", 40L)
  expect_match(v, "one_col <- sum\\(as\\.integer\\(s\\$columns\\) == 1L\\)")
  expect_match(v, "each", fixed = TRUE)
  expect_match(v, "row arrives whole in one cell", fixed = TRUE)
  # and the clean tick is withheld while any table has one column
  expect_match(v, "if \\(!bad && !one_col\\)")
})
