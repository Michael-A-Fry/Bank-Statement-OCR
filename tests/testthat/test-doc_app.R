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

# THE WHOLE of a block, however long it grows. A fixed line count has to be
# widened by hand every time the code inside it gains a paragraph, and a window
# that has silently slid off the end of what it was checking passes for the
# wrong reason. Counts braces instead, so it ends where the block ends.
.da_whole <- function(src, pattern) {
  i <- grep(pattern, src)[1]
  skip_if(is.na(i), paste("not found:", pattern))
  bare <- gsub("#.*$", "", gsub('"[^"]*"', "", gsub("'[^']*'", "", src)))
  depth <- 0L
  for (k in seq(i, length(src))) {
    depth <- depth +
      nchar(gsub("[^{(]", "", bare[k])) - nchar(gsub("[^})]", "", bare[k]))
    if (k > i && depth <= 0L) return(paste(src[i:k], collapse = "\n"))
  }
  paste(src[i:length(src)], collapse = "\n")
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
  # WAS: c("cv_headline", "cv_summary", "cv_rematch"). Register B2 / B1 take
  # cv_rematch off this list on purpose: it is cv_edit now and it renders on ALL
  # THREE kinds, because a report has no reconciliation behind it and "open what
  # read this and adjust it" is the only correction that route has. The rule this
  # test holds -- everything about TRANSACTIONS goes through the one predicate --
  # is unchanged, and cv_edit still asks .is_txn_result before it says anything
  # about a bank.
  for (out in c("cv_headline", "cv_summary")) {
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
  ask <- .da_block(src, "\\.RB_ASK <- list\\(", 36L)
  for (m in c("title", "cols", "addcol", "colpos", "start", "end", "bottom",
              "label", "value"))
    expect_match(ask, sprintf("\\n\\s*%s\\s*=", m))
  for (t in c("TITLE", "ROW OF COLUMN NAMES", "NEW COLUMN", "STARTS", "ENDS",
              "CARRIES ON", "LABEL", "VALUE"))
    expect_match(ask, t, fixed = TRUE)
  # the banner across the top reads its sentence out of that one list...
  arm <- .da_block(src, "output\\$rb_arm <- renderUI", 34L)
  expect_match(arm, "\\.RB_ASK\\[\\[rb\\$mode")
  expect_match(arm, "Nothing is waiting for a drag")
  # ...and so does the hint under the picture, so they can never disagree
  hint <- .da_block(src, "output\\$rb_hint <- renderUI", 8L)
  expect_match(hint, "\\.RB_ASK\\[\\[rb\\$mode")
  # THE RULE ITSELF: a drag with nothing armed changes nothing and says so.
  brush <- .da_whole(src, "observeEvent\\(input\\$rb_brush")
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
  # .da_whole, not a line count: the Remove handler grew a confirmation (H13) and
  # a 40-line window then stopped short of the two value handlers and passed by
  # not looking at them.
  blk <- .da_whole(src, "lapply\\(seq_len\\(\\.RB_MAX_ROWS\\)")
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
  blk <- .da_whole(src, "output\\$rb_where <- renderUI")
  expect_match(blk, "Starts", fixed = TRUE)
  expect_match(blk, "Ends", fixed = TRUE)
  expect_match(blk, "down the page", fixed = TRUE)        # said as a place, not a number
  expect_match(blk, '"rb_setstart"'); expect_match(blk, '"rb_setend"')
  expect_match(blk, '"rb_gostart"'); expect_match(blk, '"rb_goend"')
  # and the three answers people actually want, as one press each
  for (b in c("rb_endauto", "rb_endpage", "rb_endlast"))
    expect_match(blk, sprintf('"%s"', b))
  # a click only ever moves one of those two, and only when it was asked for
  cl <- .da_block(src, "observeEvent\\(input\\$rb_click", 42L)
  expect_match(cl, 'if \\(!\\(m %in% c\\("start", "end", "bottom"\\)\\)\\) return\\(\\)')
  # an end before the start, or a start after the end, is refused where it
  # happens rather than explained later
  expect_match(cl, "past where the table ends")
  expect_match(cl, "above where the table starts")
  # and both markers are drawn on the page, named
  plot <- .da_block(src, "\\.rb_draw_table <- function", 78L)
  expect_match(plot, '"START')
  expect_match(plot, '"END"')
})

test_that("the columns come from the header row, and only the one you touch moves", {
  src <- .da_src()
  # One drag round the row of column names, and every header cell becomes a
  # column named after it -- and the end is worked out from there.
  blk <- .da_block(src, 'if \\(m == "cols"\\)', 46L)
  expect_match(blk, "doc_columns_from_box\\(")
  expect_match(blk, "doc_auto_end\\(")
  # ADDING A COLUMN IS A DRAG, NOT TWO CLICKS AND NOT AN EDGE MOVE. Fed through
  # doc_edge_click() twice, a drag beside the table moved the outer edge out to
  # it twice and gave one column stretched over everything the drag crossed.
  # Rewritten to keep every edge, it gave the new column PLUS an invented one in
  # the gap. The column list is the answer: what was drawn, where it was drawn.
  add <- .da_block(src, 'if \\(m == "addcol"\\)', 32L)
  expect_match(add, "doc_add_column\\(\\.doc_columns\\(d\\), box\\$x_min, box\\$x_max")
  expect_false(grepl("doc_edge_click\\(", add))
  expect_false(grepl("doc_columns_from_edges\\(", add))
  # A COLUMN IN THE MIDDLE MOVES THE ONES BESIDE IT, through the same function
  # whether dragged or typed.
  # WAS: expect_match(mv, "doc_set_column_band\\(old, j, box\\$x_min, box\\$x_max\\)")
  #      expect_match(.da_joined(), "doc_set_column_band\\(cols, j, x0, x1\\)")
  # Register D5 names that setter's behaviour as the fault - "the band setter
  # clamps and the screen says 'move the neighbour first', which is the control
  # refusing to do the obvious thing" - so the two assertions pinned the bug.
  # .rb_move_band is the same one-function rule with the neighbours pushed along.
  mv <- .da_block(src, 'if \\(m == "colpos"\\)', 30L)
  expect_match(mv, "\\.rb_move_band\\(old, j, box\\$x_min, box\\$x_max")
  expect_match(.da_joined(), "\\.rb_move_band\\(cols, j, x0, x1")
  # ...and deleting one leaves its space empty rather than handing it to a
  # neighbour, because whitespace between columns is now expressible.
  rm <- .da_block(src, "\\.rb_drop_col <- function", 14L)
  expect_match(rm, "cols\\[-j\\]")
  expect_false(grepl("doc_column_edges", rm))
})

# WHITESPACE BETWEEN COLUMNS IS ALLOWED. AN OVERLAP IS NOT.
#
# Reported: "do columns have to be joined? can there be white space between two
# column definitions?" They did, and the cost was visible -- adding a column
# either widened its neighbour or invented a column in the gap, on a report whose
# columns really do have space between them.
test_that("columns may have gaps, may never overlap, and the screen says which happened", {
  joined <- .da_joined()
  # the clamp is what makes an overlap unreachable, and it is SAID, not silent
  expect_match(joined, 'attr\\(cols, "clamped"\\)')
  expect_match(joined, "cannot overlap", fixed = TRUE)
  # a drag with nowhere to go is refused with a reason, not silently ignored
  expect_match(joined, "no room for a new one", fixed = TRUE)
})

# THE SENTENCE SAYING WHAT THE NEXT DRAG DOES HAS TO BE ON SCREEN WHEN YOU DRAG.
#
# It was written at the top of the page, which is not the top of the screen: the
# document image is 840px tall and the panel beside it is longer, so any real work
# happens scrolled down, and the instruction was then above the window. A rule
# nobody can read while doing the thing it governs is not a rule.
test_that("the instruction sticks to the top of the window, and the upload folds away", {
  src <- .da_src(); joined <- .da_joined()
  expect_match(joined, 'div\\(class = "rb-sticky", uiOutput\\("rb_arm"\\)\\)')
  css <- paste(readLines(file.path(engine_root(), "www", "app.css"), warn = FALSE),
               collapse = "\n")
  expect_match(css, ".rb-sticky", fixed = TRUE)
  expect_match(css, "position:sticky", fixed = TRUE)

  # Once a document is loaded the upload panel is four inches of answered
  # questions between the reader and the work, so it swaps for one line.
  expect_match(joined, "output.rb_has_doc == true && output.rb_up_open != true",
               fixed = TRUE)
  expect_match(joined, 'actionButton\\("rb_change_doc"')
  expect_match(joined, 'output\\$rb_docname <- renderText')
  # ...and the way back must be reachable: the kind-of-document radio folds away
  # with the panel, so the button that reopens it has to say so.
  btn <- sub('.*actionButton\\("rb_change_doc", "([^"]*)".*', "\\1",
             gsub("\n", " ", joined))
  expect_match(btn, "kind", fixed = TRUE)
  # every conditionalPanel flag it depends on survives being hidden
  for (o in c("rb_has_doc", "rb_up_open", "rb_docname"))
    expect_match(joined, sprintf('outputOptions\\(output, "%s", suspendWhenHidden = FALSE\\)', o))
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
  # WAS: doc_set_column_band(...) in both places - see register D5 and the
  # rewrite above; the request is now .rb_move_band, which pushes.
  expect_match(joined, "\\.rb_move_band\\(cols, j, x0, x1")
  expect_match(joined, "\\.rb_move_band\\(old, j, box\\$x_min, box\\$x_max")
  # and moving a column does not rename it back to whatever is printed above it
  mv <- .da_block(src, 'if \\(m == "colpos"\\)', 30L)
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
  expect_match(plot, "ylim = c\\(r\\$h, -gutter\\)")
  expect_match(plot, "input\\$rb_layers")
  # THE STRIP IS AS TALL AS THE NAMES NEED. Two rows per TABLE was fine for one
  # table and wrong the moment a page carries two: every table started again at
  # row one and wrote over the one before it. Placement is a page-level pass now,
  # and the window's top edge is worked out before the window is opened.
  expect_match(plot, "gutter <- max\\(\\.RB_GUTTER,")
  expect_match(plot, "\\.rb_name_rows\\(items\\)")
  expect_match(plot, "\\.rb_place_names\\(items, gutter\\)")
  nm <- .da_block(src, "\\.rb_place_names <- function", 16L)
  expect_match(nm, "-gutter \\+ 9")                    # the names live in the strip
  expect_match(nm, "\\.rb_fit\\(")                     # trimmed to the width they have
  expect_match(nm, "ends\\[k\\] <-")                    # and never over each other
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
  blk <- .da_block(src, "\\.rb_draw_table <- function", 60L)
  # the fill is unconditional -- the alternation decides which SHADE, not whether
  expect_match(blk, "if (j %% 2L == 1L) dark else pale", fixed = TRUE)
  expect_match(blk, "pale <- if (lwd > 1)", fixed = TRUE)
  expect_match(blk, "dark <- if (lwd > 1)", fixed = TRUE)
  # the band's own rect is not guarded by which band it is
  expect_false(grepl("if (j %% 2L == 1L)\n            rect(", blk, fixed = TRUE))
  # ...AND IT IS THE COLUMN'S OWN TWO EDGES, not the shared-divider view.
  # doc_column_edges() keeps every x_min and only the LAST x_max, so drawing from
  # it painted each gap as part of the column before it and ran the tint over the
  # column after -- reported as "it places the new column x axis on top of the
  # previous, overlapping". The model was right; the picture was not.
  expect_match(blk, "bands\\[\\[j\\]\\]\\[1\\], y0, bands\\[\\[j\\]\\]\\[2\\], y1")
  expect_false(grepl("edges <- doc_column_edges(tb)", blk, fixed = TRUE))
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
  # WAS: expect_match(ed, "rb\\$tables <- rb\\$tables\\[-i\\]")
  # Register H13 names exactly that line as the fault: "Edit takes a saved table
  # OUT of the template ... one click, no confirmation, no undo, and table 27 is
  # gone from a template that took months. Edit must COPY into the draft and
  # leave the saved table in place". So the draft is a copy, and where it goes
  # back to is remembered instead.
  expect_false(grepl("rb$tables <- rb$tables[-i]", ed, fixed = TRUE))
  expect_match(ed, "rb\\$edit_idx <- as\\.integer\\(i\\)")
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
  tb <- .da_whole(src, "observeEvent\\(input\\$rb_savetab")
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
  # Editing loads a draft and takes you to the page it is on.
  # WAS: expect_match(ed, "rb$(tables|pairs) <- rb$(tables|pairs)[-i]") for BOTH.
  # Register H13: a table must be COPIED into the draft, not cut out of the
  # template. A value pair is not named by H13 and still cuts, so the two are
  # asserted separately rather than by one regex that hides the difference.
  ed <- .da_whole(src, "observeEvent\\(input\\[\\[paste0\\(\"rb_ed_\"")
  expect_false(grepl("rb$tables <- rb$tables[-i]", ed, fixed = TRUE))
  expect_match(ed, "\\.rb_load_draft\\(")
  expect_match(ed, "updateNumericInput\\(session, \"rb_page\"")
  ved <- .da_whole(src, "observeEvent\\(input\\[\\[paste0\\(\"rb_ved_\"")
  expect_match(ved, "rb\\$pairs <- rb\\$pairs\\[-i\\]")
  expect_match(ved, "\\.rb_load_vdraft\\(")
  expect_match(ved, "updateNumericInput\\(session, \"rb_page\"")
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
  save_document_template(tmpl, file.path(tdir, "documents"))

  res <- convert_document(pdf_path, outdir = file.path(tdir, "out"),
                          templates_dir = templates_dir(),
                          user_templates_dir = NULL,
                          fields_dir = fields_templates_dir(),
                          doc_dir = file.path(tdir, "documents"),
                          logdir = file.path(tdir, "logs"))
  expect_identical(res$kind, "tables")
  expect_true(res$status %in% c("ok", "needs_review"))
  expect_identical(res$run_log$kind, "tables")
  expect_identical(res$run_log$template_origin, "document")
  # A REPORT HAS TABLES, NOT TRANSACTIONS. row_count is the field every dashboard
  # and every drift report counts; a hundred table rows in it would inflate
  # figures a report has no business being part of.
  #
  # AND IT IS NA, NOT A LITERAL ZERO (register G8). A zero is a MEASUREMENT -- it
  # says this run reconciled nothing wrong -- so every report and form run was
  # being counted in Admin as a clean zero-failure conversion beside statements
  # that genuinely reconciled. "No transactions to count" and "counted, none
  # failed" are different facts and a spreadsheet cannot tell them apart.
  expect_true(is.na(res$run_log$row_count))
  expect_true(is.na(res$run_log$kpi_fail_count))
  expect_gt(res$run_log$n_table_rows, 90L)
  expect_true(length(res$outputs) > 0L)

  # ...and it is still download-only after all that.
  feed <- file.path(tdir, "feed")
  expect_null(write_feed(res, list(feed = list(enabled = TRUE, feed_dir = feed))))
  expect_false(dir.exists(feed))
})

# ---------------------------------------------------------------------------
# WHAT SHE SAID IT IS OUTRANKS WHAT THE TOOL GUESSED.
#
# Reported: "I put a phrase printed on it, bang smack on front page, still used
# another template." A bank statement template matched a document that was not a
# statement, and because the report pass is only reached when the statement pass
# comes back unsupported, the report template carrying that phrase was never
# consulted. No scoring change fixes that in general - but nobody has to guess
# when the person holding the document already knows.
# ---------------------------------------------------------------------------

test_that("kind='other' takes every bank statement template off the table", {
  skip_if_not(requireNamespace("yaml", quietly = TRUE), "yaml not installed")
  tdir <- file.path(tempdir(), paste0("docapp-kind-", as.integer(Sys.time())))
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  # A file a SHIPPED statement template reads cleanly.
  p <- file.path(tdir, "s.csv")
  file.copy(file.path(engine_root(), "samples", "raw", "anz", "anz_transaction_export_01.csv"), p)
  skip_if_not(file.exists(p), "sample statement missing")

  common <- list(outdir = file.path(tdir, "out"), templates_dir = templates_dir(),
                 user_templates_dir = NULL, fields_dir = fields_templates_dir(),
                 doc_dir = file.path(tdir, "none"), logdir = file.path(tdir, "logs"))
  auto <- do.call(convert_document, c(list(p), common))
  expect_true(auto$status %in% c("ok", "needs_review"))     # it IS a statement

  other <- do.call(convert_document, c(list(p), common, list(kind = "other")))
  expect_identical(other$status, "unsupported")
  expect_identical(other$asked_kind, "other")
  expect_true(is.na(other$template_id %||% NA))
  # ...and the answer is on the run record, so the log says which search was run
  expect_identical(other$run_log$asked_kind, "other")
})

test_that("kind='statement' does not fall through to the other two paradigms", {
  f <- paste(deparse(convert_document), collapse = "\n")
  expect_true(grepl('kind <- match.arg(kind)', f, fixed = TRUE))
  # the early return has to come BEFORE convert_form, or "it is a statement"
  # still ends up being read as a form
  i_ret  <- regexpr('identical(kind, "statement")', f, fixed = TRUE)
  i_form <- regexpr("convert_form(", f, fixed = TRUE)
  expect_gt(i_ret, 0L)
  expect_lt(i_ret, i_form)
  # forcing an exact statement template IS saying it is a statement
  expect_true(grepl('kind <- "statement"', f, fixed = TRUE))
})

test_that("an empty statement set is how kind='other' is enforced, not a status rewrite", {
  expect_true("use_statement_templates" %in% names(formals(convert_statement)))
  expect_true(isTRUE(formals(convert_statement)$use_statement_templates))
  # The gate is on the LOADING of the set. A result rewritten after the fact
  # would already have parsed the file and written its outputs, and would still
  # be carrying the wrong template's id.
  f <- paste(deparse(convert_statement), collapse = " ")
  expect_true(grepl("isTRUE(use_statement_templates)", f, fixed = TRUE))
  expect_true(grepl("load_template_set(templates_dir, user_templates_dir)", f, fixed = TRUE))
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
                          templates_dir = templates_dir(),
                          user_templates_dir = NULL,
                          fields_dir = fields_templates_dir(),
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
  blk <- .da_block(src, "rb_id_auto <- reactiveVal", 72L)
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
  blk <- .da_whole(src, 'if \\(m == "cols"\\)')
  expect_match(blk, "if \\(length\\(cols\\) == 1L\\)")
  expect_match(blk, "ONE column and each row arrives whole in it", fixed = TRUE)
  expect_match(blk, 'type = "warning"')
  # ...and again on the verdict card, so it is not only said at the moment of
  # the drag and then forgotten
  v <- .da_block(src, "output\\$rb_prev_status <- renderUI", 60L)
  expect_match(v, "one_col <- sum\\(num\\(s\\$columns\\) == 1L\\)")
  expect_match(v, "each", fixed = TRUE)
  expect_match(v, "row arrives whole in one cell", fixed = TRUE)
  # and the clean tick is withheld while any table has one column
  expect_match(v, "if \\(!bad && n_tab && !one_col\\)")
})

# ---------------------------------------------------------------------------
# NOBODY EVER SAYS HOW MANY ROWS A TABLE HAS
#
# The box beside "Its top rows" was labelled "How many rows", sitting directly
# under a table -- which reads as "how many rows has this table", a question
# that is never asked here and could not be answered by looking. The number of
# DATA rows is read from the page between the start and the end. This is only
# how tall the heading is, and the screen now says so.
# ---------------------------------------------------------------------------

test_that("the heading-height box cannot be read as a row count", {
  src <- .da_src(); joined <- .da_joined()
  expect_match(joined, 'numericInput\\("rb_hdrn", "Heading rows"')
  expect_false(grepl('numericInput("rb_hdrn", "How many rows"', joined, fixed = TRUE))
  expect_match(joined, "You never say how many rows of DATA there are", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# TYPING MUST NOT FIGHT THE SCREEN
#
# The complaint this builder was rebuilt around was "every time I drag a box or
# type in call this table it updates and glitches and makes it SUPER hard to do
# anything". Three things have to hold, and all three are structural rather than
# a matter of care:
#
#   1  an input is built ONCE and filled in with update*Input. A Shiny input
#      rebuilt inside a renderUI is destroyed and recreated under the caret.
#   2  anything that feeds a list drawn from the draft is debounced, so the list
#      settles after typing stops instead of redrawing on every letter.
#   3  nothing writes back into a box the person may be typing in. Assigning a
#      reactiveValues field invalidates it EVEN WHEN THE VALUE IS IDENTICAL, so
#      a no-op re-selection rewrites the number under the caret.
#
# Measured in a browser at 45ms a keystroke: every character survives, no input
# is ever a different DOM node afterwards, drawing a box causes ZERO redraws
# while the mouse is held down, and a whole typed phrase costs one or two plot
# recalculations rather than one per letter.
# ---------------------------------------------------------------------------

test_that("nothing writes back into a box somebody may be typing in", {
  src <- .da_src()
  blk <- .da_block(src, "rb_cx_d <- debounce", 34L)
  # the guard itself: only move the selection when the band really moved
  expect_match(blk, "!identical\\(as\\.integer\\(jj\\), as\\.integer\\(j\\)\\)")
  expect_match(blk, "DO NOT RE-SELECT THE COLUMN IT ALREADY IS", fixed = TRUE)
  # and the reason, so nobody removes the guard as redundant
  expect_match(blk, "invalidates it even when the new value is", fixed = TRUE)
})

test_that("every box that feeds a redrawn list is debounced, and built once", {
  src <- .da_src(); joined <- .da_joined()
  # debounced: the table name, the column name, the column edges, the value name
  for (d in c("rb_name_d <- debounce", "rb_cname_d <- debounce",
              "rb_cx_d <- debounce", "rb_vname_d <- debounce",
              "rb_where_d <- debounce"))
    expect_match(joined, d, fixed = TRUE)
  # built ONCE in the UI, never inside a renderUI that the same typing redraws
  ui <- paste(src[seq_len(grep("^server <- function", src)[1])], collapse = "\n")
  for (id in c("rb_name", "rb_cname", "rb_ckind", "rb_cx0", "rb_cx1",
               "rb_vname", "rb_vtype", "rb_vwhere", "rb_hdrn"))
    expect_match(ui, sprintf('"%s"', id), fixed = TRUE,
                 info = paste(id, "is not built in the UI, so it lives inside a renderUI"))
  # ...and the lists that DO redraw carry no text input at all
  for (out in c("rb_cols", "rb_saved", "rb_vsaved")) {
    blk <- .da_block(src, sprintf("output\\$%s <- renderUI", out), 24L)
    expect_false(grepl("textInput(", blk, fixed = TRUE),
                 info = paste(out, "puts a textInput inside a redrawn list"))
    expect_false(grepl("numericInput(", blk, fixed = TRUE))
  }
})

# ---------------------------------------------------------------------------
# WHAT CAME OUT: BOTH HALVES, AND A BUTTON FOR EACH
#
# Reported, in one breath: "the read whole doc should have the tables AND label
# value pair", and "where do the value label pairs come out in the CSV long? I
# can only see it on workbook". Two symptoms of one gap -- the pairs were
# extracted, written and reported nowhere a person looks.
# ---------------------------------------------------------------------------

test_that("the preview shows the values as well as the tables", {
  joined <- .da_joined()
  expect_match(joined, 'uiOutput\\("rb_prev_values_head"\\)')
  expect_match(joined, 'tableOutput\\("rb_prev_values"\\)')
  expect_match(joined, 'output\\$rb_prev_values <- renderTable')
  # and the count is in the verdict line, so a values-only template is not
  # reported as "0 tables" and nothing else
  v <- .da_block(.da_src(), "output\\$rb_prev_status <- renderUI", 60L)
  expect_match(v, "n_val <- NROW\\(ext\\$pairs\\)")
  expect_match(v, "reads labelled values only", fixed = TRUE)
})

test_that("the values CSV has a button on both screens that produce one", {
  joined <- .da_joined()
  # the builder's preview
  expect_match(joined, 'downloadButton\\("rb_dl_values"')
  expect_match(joined, 'output\\$rb_dl_values <- downloadHandler')
  # and Convert, where a saved report template lands
  expect_match(joined, 'downloadButton\\("dl_values"')
  # resolved by the FULL suffix: ".csv" alone matches the long CSV first
  expect_match(joined, 'output\\$dl_values <- mk_dl\\("values\\\\\\\\.csv"\\)')
})

# ---------------------------------------------------------------------------
# A TEMPLATE WITH NO TABLES IS NOT AN ERROR
# ---------------------------------------------------------------------------

test_that("every count on the verdict card is forced to a number", {
  # Reported: "when I don't define any tables, I'm getting error: invalid 'type'
  # character of argument when I click read the whole document". A zero-row
  # summary used to be ten character(0) columns, and sum() over one is an error.
  # Fixed at the source (R/doc_extract.R types the empty frame); this is the
  # second lock, because the screen must not be the thing that falls over.
  v <- .da_block(.da_src(), "output\\$rb_prev_status <- renderUI", 60L)
  expect_match(v, "num <- function\\(v\\)")
  expect_false(grepl("sum\\(s\\$rows\\)", v))          # the exact line that threw
  for (f in c("num(s$thin_columns)", "num(s$unclaimed_words)", "num(s$rows)"))
    expect_match(v, f, fixed = TRUE)
})

# ---------------------------------------------------------------------------
# WHERE IT STARTS AND WHERE IT STOPS ARE STEPS, NOT SETTINGS
#
# Reported: "start and end also need to define where the table ends, there's lots
# of text at the bottom of some pages which is not the table, but the table still
# spans multiple pages. Make it part of the steps the header guides you through."
# ---------------------------------------------------------------------------

test_that("the guide walks columns, start, end, and the bottom edge of the pages between", {
  src <- .da_src(); joined <- .da_joined()
  step <- .da_block(src, "\\.rb_next_step <- function", 14L)
  expect_match(step, 'identical\\(done, "cols"\\)')
  expect_match(step, '"start"'); expect_match(step, '"end"'); expect_match(step, '"bottom"')
  # the bottom step is offered ONLY when there are pages in between
  expect_match(step, "p1 > p0")
  # armed by the guide, not by a drag: a table starts the walk, and each step
  # hands on to the next
  expect_match(joined, "rb\\$guide <- TRUE")
  expect_match(joined, 'rb\\$mode <- \\.rb_next_step\\("cols", d\\)')
  expect_match(joined, "rb\\$mode <- \\.rb_next_step\\(m, d\\)")
  # ...and it stops the moment somebody steers: Cancel, or Move it by hand
  for (b in c("rb_disarm", "rb_setstart", "rb_setend")) {
    blk <- .da_block(src, sprintf("observeEvent\\(input\\$%s,", b), 2L)
    expect_match(blk, "rb\\$guide <- FALSE", info = b)
  }
  # every armed step can be skipped, keeping what the tool worked out
  expect_match(joined, 'actionButton\\("rb_skip_step"')
  expect_match(joined, "observeEvent\\(input\\$rb_skip_step")
})

test_that("the bottom edge is stored where the reader actually uses it", {
  # doc_locate_table() closes a CONTINUATION page's window at band$y_max
  # (R/tables.R). Anywhere else and the setting would be a control that changes
  # nothing -- which is worse than not having one.
  cl <- .da_block(.da_src(), "observeEvent\\(input\\$rb_click", 42L)
  expect_match(cl, "b <- d\\$band")
  expect_match(cl, "b\\$y_max <- y")
  expect_match(cl, "above the top of the table", fixed = TRUE)
  # and it is on the screen in words, with a way to put it back
  w <- .da_whole(.da_src(), "output\\$rb_where <- renderUI")
  expect_match(w, "Bottom edge on the pages in between", fixed = TRUE)
  expect_match(w, '"rb_setbottom"')
  expect_match(w, '"rb_clearbottom"')
  expect_match(w, "footer", fixed = TRUE)
})

test_that("a value's name says what it will come out as, without rewriting the box", {
  joined <- .da_joined()
  expect_match(joined, 'textOutput\\("rb_vname_key"')
  expect_match(joined, 'output\\$rb_vname_key <- renderText')
  expect_match(joined, "Comes out as", fixed = TRUE)
  # normalised on SAVE, never while somebody is typing into it
  save <- .da_block(.da_src(), "observeEvent\\(input\\$rb_vsave", 24L)
  expect_match(save, 'updateTextInput\\(session, "rb_vname", value = v\\$name\\)')
  key <- .da_block(.da_src(), "output\\$rb_vname_key <- renderText", 6L)
  expect_match(key, "rb_vname_d\\(\\)")
  expect_false(grepl("updateTextInput", key))
})

test_that("a drag that changed nothing names the column somebody was moving", {
  # Reported: "when I click edit, and drag where I actually want the column, it
  # says nothing was waiting". True, and useless: the button they still had to
  # press was named nowhere in the sentence.
  brush <- .da_whole(.da_src(), "observeEvent\\(input\\$rb_brush")
  # WAS: expect_match(brush, "Drag its width on the page", fixed = TRUE)
  # That button was one of two ways to arm the same drag - pressing Edit beside
  # the column already arms it - and register D2 ("There are LOTS of buttons...
  # it NEEDS to be even more simple") is what took it off the screen. The rule
  # this test exists for is unchanged: the sentence must NAME the thing that is
  # still to be pressed.
  expect_match(brush, "press Edit beside it in the column list", fixed = TRUE)
  expect_match(brush, "rb\\$colsel")
})

# ---------------------------------------------------------------------------
# EDIT ARMS THE DRAG, AND THE SCREEN SAYS WHAT YOU ARE EDITING
#
# Asked for after "when I click edit, and drag where I actually want the column,
# it says nothing was waiting" -- twice. The armed-intent model is kept: it is
# still one thing at a time, still written across the top, still cancellable. The
# change is that pressing Edit on a specific thing IS a statement of intent about
# that thing, so it arms the gesture that acts on it.
# ---------------------------------------------------------------------------

test_that("Edit on a column arms the drag that moves it", {
  src <- .da_src()
  blk <- .da_block(src, 'observeEvent\\(input\\[\\[paste0\\("rb_cs_", j\\)\\]\\]', 20L)
  expect_match(blk, "rb\\$colsel <- j")
  expect_match(blk, 'rb\\$mode <- "colpos"')
  expect_match(blk, "rb\\$guide <- FALSE")     # pressing Edit is steering
})

test_that("Edit on a value arms the drag, and the other half is one press away", {
  src <- .da_src(); joined <- .da_joined()
  blk <- .da_block(src, 'observeEvent\\(input\\[\\[paste0\\("rb_ved_", i\\)\\]\\]', 20L)
  expect_match(blk, 'rb\\$mode <- "value"')
  expect_match(joined, 'actionButton\\("rb_arm_label"')
  expect_match(joined, 'actionButton\\("rb_arm_value"')
  expect_match(joined, "observeEvent\\(input\\$rb_arm_label")
  expect_match(joined, "observeEvent\\(input\\$rb_arm_value")
})

test_that("the banner names the thing being edited, and the page outlines it", {
  src <- .da_src(); joined <- .da_joined()
  subj <- .da_block(src, "\\.rb_subject <- function", 18L)
  expect_match(subj, '"colpos"')
  expect_match(subj, "rb\\$vdraft")
  arm <- .da_block(src, "output\\$rb_arm <- renderUI", 40L)
  expect_match(arm, "\\.rb_subject\\(\\)")
  expect_match(arm, "You are editing", fixed = TRUE)
  expect_match(arm, "outlined on the page", fixed = TRUE)
  # ...and that outline is really drawn
  expect_match(joined, "PALETTE\\$warn, lwd = 3\\)")
})

# ---------------------------------------------------------------------------
# EVERY TABLE ON SCREEN, NOT THE FIRST ONE
# ---------------------------------------------------------------------------

test_that("the preview shows every table's rows, and says so when it cannot", {
  src <- .da_src(); joined <- .da_joined()
  expect_match(joined, "\\.RB_MAX_PREV <- ")
  expect_match(joined, "\\.rb_prev_keys <- reactive")
  # one DT per table, DECLARED ONCE -- a DT built inside a renderUI has no
  # server-side render behind it and comes up blank
  expect_match(joined, 'lapply\\(seq_len\\(\\.RB_MAX_PREV\\), function\\(i\\) \\{')
  expect_match(joined, 'output\\[\\[paste0\\("rb_prev_tbl_", i\\)\\]\\] <- renderDT')
  expect_match(joined, 'DTOutput\\(paste0\\("rb_prev_tbl_", i\\)\\)')
  # and no silent cap: a screen that quietly stops reads as "that is all of them"
  head <- .da_block(src, "output\\$rb_prev_head <- renderUI", 40L)
  expect_match(head, "more table\\(s\\) are in the workbook")
  expect_match(head, "came out empty", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# WHERE THE TOOL TELLS YOU SOMETHING
# ---------------------------------------------------------------------------

test_that("notifications are centred, not tucked in the corner nobody watches", {
  css <- paste(readLines(file.path(engine_root(), "www", "app.css"), warn = FALSE),
               collapse = "\n")
  expect_match(css, "#shiny-notification-panel", fixed = TRUE)
  expect_match(css, "left:50%", fixed = TRUE)
  expect_match(css, "translateX(-50%)", fixed = TRUE)
  # readable at a glance rather than a corner toast
  expect_match(css, "font-size:14.5px", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# NO CONTROL THAT CANNOT WORK
#
# Found by walking the screens rather than from a report. Both of these were
# full-width, primary-coloured, and answered a fading toast when pressed -- so
# the most prominent control on the screen did nothing, and by the third press
# the person is looking for what is broken rather than for the empty box above.
# ---------------------------------------------------------------------------

test_that("Convert is off until there is a file and a QID, with the reason under it", {
  src <- .da_src(); joined <- .da_joined()
  expect_match(joined, 'uiOutput\\("cv_go_btn"\\)')
  blk <- .da_block(src, "output\\$cv_go_btn <- renderUI", 18L)
  expect_match(blk, "cv_qid\\(\\)")
  expect_match(blk, "input\\$cv_file")
  expect_match(blk, "disabled")
  # the two reasons are DIFFERENT JOBS and are never merged into "you cannot"
  # ...and it says FILE, not statement: half of what goes through this screen is
  # not a statement, and the button that starts it must not say otherwise.
  expect_match(blk, "Choose a file above", fixed = TRUE)
  expect_match(blk, "Enter your QID above", fixed = TRUE)
})

test_that("Read the whole document is off until there is something to read", {
  src <- .da_src()
  blk <- .da_block(src, "output\\$rb_preview_btn <- renderUI", 12L)
  expect_match(blk, "length\\(rb\\$tables\\)")
  expect_match(blk, "\\.rb_all_pairs\\(\\)")
  expect_match(blk, "disabled")
  expect_match(blk, "Save a table or a value first", fixed = TRUE)
})

test_that("the first screen does not contradict itself about which files it takes", {
  src <- .da_src(); joined <- .da_joined()
  # the KIND is asked before the file, so the picker can say what that job takes
  i_kind <- grep('radioButtons\\("ts_doctype"', src)[1]
  i_file <- grep('fileInput\\("ts_file"', src)[1]
  expect_true(i_kind < i_file)
  # ...and the picker's own label no longer lists formats the report half refuses
  expect_false(grepl('fileInput\\("ts_file", "One example of the document \\(', joined))
  expect_match(joined, "It has to be a PDF: you build this one by pointing at the page.",
               fixed = TRUE)
  # a spreadsheet chosen for a report is a DEAD END unless it is explained
  blk <- .da_block(src, "output\\$rb_need_doc <- renderUI", 22L)
  expect_match(blk, "file_ext")
  expect_match(blk, "has no page to point at", fixed = TRUE)
  expect_match(blk, "A bank or card statement", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# A DOCUMENT NOTHING RECOGNISED IS NOT "THIS STATEMENT"
# ---------------------------------------------------------------------------

# A CLEAR DECISION, OR A CLEAR WAY TO OVERRIDE IT. NEVER TWO BIG BUTTONS.
#
# The first go offered both doors side by side with the likelier one styled
# primary. That is neither: it hands somebody a choice without telling them they
# are making one, and the only difference between the two is a shade of green.
test_that("when the tool can tell, it decides, and the other answer is one link", {
  src <- .da_src()
  blk <- .da_block(src, "output\\$cv_teach <- renderUI", 240L)
  # it SAYS what it decided, in a sentence about the document
  expect_match(blk, "This looks like a report, not a bank statement.", fixed = TRUE)
  expect_match(blk, "This looks like a bank statement, but no template reads it yet.",
               fixed = TRUE)
  # ...never the old assertion, and never the old two-button card
  expect_false(grepl("No template read this statement", blk, fixed = TRUE))
  expect_false(grepl("Nothing recognised this document", blk, fixed = TRUE))
  # ONE primary button per decided branch, doing the decided thing
  expect_match(blk, 'actionButton\\("cv_teach_go_report", "Set it up as a report')
  expect_match(blk, 'actionButton\\("cv_teach_go", "Set it up as a bank statement')
  # and the override is the OTHER ANSWER, spelled out, not a second big button
  expect_match(blk, '\\.rb_no_such_thing|other\\("Not a report\\?"')
  expect_match(blk, 'other\\("Not a statement\\?"')
  expect_match(blk, "It is a bank or card statement", fixed = TRUE)
  expect_match(blk, "It is something else - a report, a form, a letter", fixed = TRUE)
  # the decision is CHECKABLE: the count it used is printed
  expect_match(blk, "hint\\$sentence")
  expect_match(blk, "cv_shape_hint\\(\\)")
})

test_that("when the tool cannot tell, it says so and asks", {
  # Two equal buttons are honest HERE and nowhere else -- dressing a coin toss as
  # a decision is the thing this card exists to stop.
  src <- .da_src()
  blk <- .da_block(src, "output\\$cv_teach <- renderUI", 240L)
  expect_match(blk, "The tool cannot tell what kind of document this is.", fixed = TRUE)
  expect_match(blk, "Which is it?", fixed = TRUE)
  expect_match(blk, 'actionButton\\("cv_teach_go", "A bank or card statement"')
  expect_match(blk, 'actionButton\\("cv_teach_go_report", "Anything else')
  # the same words Add a template uses, so the question is not learned twice
  ui <- .da_block(src, 'radioButtons\\("ts_doctype"', 5L)
  expect_match(ui, "A bank or card statement", fixed = TRUE)
  expect_match(ui, "Anything else", fixed = TRUE)
})

test_that("the two ids are never rendered at the same time", {
  # cv_teach_go is a BUTTON on one branch and the override LINK on another. Two
  # elements sharing an input id keep independent click counters, so if both ever
  # rendered together one of them would send a value identical to the current one
  # and its observer would never fire -- a dead control, invisible from R. They
  # are safe only because the branches return.
  src <- .da_src()
  blk <- .da_block(src, "output\\$cv_teach <- renderUI", 240L)
  for (b in c('identical\\(looks, "report"\\)', 'identical\\(looks, "statement"\\)'))
    expect_match(blk, paste0("if \\(", b, "\\)\\n\\s*return\\(box\\("))
})

test_that("the report door carries the file through it", {
  # Sending somebody to a tab where they must find and upload the same file again
  # is the small tax that turns "try the other one" into "give up".
  src <- .da_src()
  blk <- .da_block(src, "observeEvent\\(input\\$cv_teach_go_report", 8L)
  expect_match(blk, "rb_handoff\\(src\\$path\\)")
  expect_match(blk, 'updateRadioButtons\\(session, "ts_doctype", selected = "other"\\)')
  expect_match(blk, 'updateTabsetPanel\\(session, "main_tabs", selected = "Add a template"\\)')
})

test_that("the shape reading is computed once, and only when it is needed", {
  # It reads pages and runs the table proposer, so it is real work -- and it is
  # only ever wanted by the card that appears when nothing matched.
  src <- .da_src()
  blk <- .da_block(src, "cv_shape_hint <- reactive", 12L)
  expect_match(blk, "doc_shape_hint\\(inp\\)")
  expect_match(blk, "tryCatch")            # never throws onto the result screen
  expect_match(blk, "fallback")
})

# ---------------------------------------------------------------------------
# THE BLUE BOX MUST ALWAYS GO AWAY, AND IT MUST GO AWAY FIRST
#
# Reported: "click and drag just stopped working. Cancelled out of table create,
# created it again, RELOADED THE PAGE, still no drag. The blue box appeared and
# does not disappear even when the next column is added." And: "the box persisted
# while trying to select the bottom edge -- the % updates but the visual doesn't,
# and it keeps the area previously selected."
#
# observeEvent fires on CHANGE. resetBrush is what makes the next drag a change.
# Below three early returns and two calls that can throw, it is a stuck screen
# that survives a reload, because the stale value is on the SERVER.
# ---------------------------------------------------------------------------

test_that("the brush is reset before anything that can return or throw", {
  src <- .da_src()
  i <- grep("observeEvent\\(input\\$rb_brush", src)[1]
  skip_if(is.na(i))
  blk <- src[i:min(i + 12L, length(src))]
  code <- blk[!grepl("^\\s*#", blk)]
  k_reset <- grep("session\\$resetBrush", code)[1]
  expect_false(is.na(k_reset))
  # NOTHING that can return or throw may come first
  before <- paste(code[seq_len(k_reset - 1L)], collapse = " ")
  expect_false(grepl("return\\(", before))
  expect_false(grepl("\\.rb_box\\(|\\.rb_pg\\(|rb_frame\\(", before))
})

test_that("a click and a change of mode both clear the rectangle", {
  joined <- .da_joined(); src <- .da_src()
  # a click means "I am doing something else now"
  cl <- .da_block(src, "observeEvent\\(input\\$rb_click", 10L)
  expect_match(cl, "session\\$resetBrush\\(\"rb_brush\"\\)")
  # and a new question deserves a page with no leftover answer drawn on it
  expect_match(joined, "observeEvent\\(rb\\$mode, \\{ session\\$resetBrush")
  # three places, so no path leaves it behind
  expect_gte(length(grep("resetBrush", src)), 3L)
})

# ---------------------------------------------------------------------------
# A DRAG THAT SHOULD HAVE BEEN A CLICK
#
# Reported: "lots of the click-and-drags that should be clicks -- users will do
# this, we MUST account for it -- persist and seem to block other activity. The
# box persisted when trying to select the bottom edge between pages."
#
# The cause is in Shiny, not here. With no dblclick id the plot click is sent on
# MOUSEDOWN (shiny.js createClickInfo), so dragging to place a boundary sends the
# click at the press point FIRST and the brush at mouseup SECOND. The click does
# the job; the brush then arrives against whatever step came next.
#
# A dblclick id does not help -- it only DELAYS the click by dblclickDelay. So
# the reconciliation has to be ours: the click records where and when it landed,
# and a brush with a corner at that same point moments later is the tail of the
# same gesture and is swallowed in silence.
# ---------------------------------------------------------------------------

test_that("the boundary click records where and when it landed", {
  blk <- .da_block(.da_src(), "observeEvent\\(input\\$rb_click", 220L)
  expect_match(blk, "rb\\$click_at <- list\\(")
  expect_match(blk, "t = as\\.numeric\\(Sys\\.time\\(\\)\\)")
  expect_match(blk, "x = as\\.numeric\\(cl\\$x\\)")
  expect_match(blk, "y = as\\.numeric\\(cl\\$y\\)")
  # declared, or every read of it is an error the first time round
  expect_match(.da_joined(), "click_at = NULL")
})

test_that("a brush that starts where a click just landed is swallowed", {
  src <- .da_src()
  i <- grep("observeEvent\\(input\\$rb_brush", src)[1]
  skip_if(is.na(i))
  code <- src[i:min(i + 30L, length(src))]
  code <- code[!grepl("^\\s*#", code)]
  blk <- paste(code, collapse = "\n")
  # it reads the click the click observer wrote
  expect_match(blk, "ca <- rb\\$click_at")
  # any CORNER, because which corner is the press point depends on the direction
  # they dragged in
  expect_match(blk, "br\\$xmin, br\\$ymin")
  expect_match(blk, "br\\$xmax, br\\$ymax")
  # and it gives up quietly -- no notification, nothing changed, no rectangle
  k_ret <- grep("return\\(\\)", code)
  k_ca  <- grep("ca <- rb\\$click_at", code)[1]
  expect_true(any(k_ret > k_ca))
  swallow <- paste(code[k_ca:max(k_ret[k_ret > k_ca][1], k_ca)], collapse = " ")
  expect_false(grepl("showNotification", swallow))
  # one gesture only: the record is spent when it is used
  expect_match(swallow, "rb\\$click_at <- NULL")
})

test_that("the swallow window is short enough not to eat a real drag", {
  src <- .da_src()
  i <- grep("observeEvent\\(input\\$rb_brush", src)[1]
  skip_if(is.na(i))
  blk <- paste(src[i:min(i + 30L, length(src))], collapse = "\n")
  secs <- as.numeric(sub(".*Sys\\.time\\(\\)\\) - ca\\$t < ([0-9.]+).*", "\\1", blk))
  expect_true(is.finite(secs))
  expect_lte(secs, 3)      # longer and a deliberate second drag disappears
  expect_gte(secs, 1)      # shorter and a slow drag is not covered
  tol <- as.numeric(sub(".*abs\\(a - b\\) <= ([0-9.]+).*", "\\1", blk))
  expect_true(is.finite(tol))
  expect_lte(tol, 12)      # PDF points -- a corner further off is a real drag
})

# ---------------------------------------------------------------------------
# ADMIN SEES EVERY KIND, AND OPENS THE RIGHT EDITOR FOR IT.
#
# Reported: "even in the admin, other templates and other things are not even
# seen", and "the template I set up for other, open in Admin, open template,
# opens bank statement template!"
# ---------------------------------------------------------------------------

test_that("the Admin list is the whole library, not the statement half", {
  joined <- .da_joined()
  # WAS: all three of these were asserted inside output$adm_tpl_overview. The frame
  # is built once in adm_ov() now, because the row-click handler needs the SAME
  # frame to turn a row index into an id (register C1, which splits this table into
  # two headed bands). What is asserted is unchanged: one library, all three kinds.
  expect_match(.da_whole(.da_src(), "adm_ov <- reactive"), "library_overview\\(")
  expect_match(.da_whole(.da_src(), "adm_ov <- reactive"), "all_field_templates_admin\\(\\)")
  expect_match(.da_whole(.da_src(), "adm_ov <- reactive"), "all_doc_templates_admin\\(\\)")
  expect_match(.da_whole(.da_src(), "output\\$adm_tpl_overview <- renderDT"), "adm_ov\\(\\)")
  # hidden ones included, or the view that un-hides them cannot show them
  for (r in c("all_field_templates_admin <- reactive", "all_doc_templates_admin <- reactive")) {
    blk <- .da_block(.da_src(), r, 4L)
    expect_match(blk, "include_hidden = TRUE")
  }
  # and one keyed library behind every action on the tab
  expect_match(joined, "adm_lib <- reactive")
  expect_match(joined, "\\.adm_kind <- function")
})

test_that("every Admin action on a template dispatches on its kind", {
  src <- .da_src()
  # picked / previewed out of the library, not out of the statement set
  expect_match(.da_block(src, "observeEvent\\(input\\$adm_tpl_pick", 6L), "adm_lib\\(\\)")
  # hidden, deleted and saved in the folder that kind lives in
  for (p in c("observeEvent\\(input\\$adm_tpl_hide", "observeEvent\\(input\\$adm_tpl_delete,"))
    expect_match(.da_whole(src, p), "\\.adm_user_dir\\(\\.adm_kind\\(id\\)\\)")
  save <- .da_whole(src, "observeEvent\\(input\\$adm_tpl_save")
  expect_match(save, "\\.adm_save\\(kind, t\\)")
  expect_match(save, "\\.adm_kind_of\\(t\\)")     # the kind of the YAML in the box
  # ...and checked against its own rulebook, not the statement one
  val <- .da_whole(src, "observeEvent\\(input\\$adm_tpl_validate")
  expect_match(val, "\\.adm_validate\\(kind, t\\)")
  expect_false(grepl("validate_template\\(t\\)", val))
  # the three savers and the three validators, each named once
  joined <- .da_joined()
  for (f in c("save_fields_template\\(t, USER_FIELDS_DIR\\)",
              "save_document_template\\(t, USER_DOC_DIR\\)",
              "save_user_template\\(t, USER_TEMPLATES_DIR\\)",
              "validate_fields_template\\(t\\)", "validate_document_template\\(t\\)"))
    expect_match(joined, f)
})

test_that("opening a report template opens the report builder, not the statement toolkit", {
  blk <- .da_whole(.da_src(), "observeEvent\\(input\\$adm_tpl_bands")
  expect_match(blk, 'identical\\(kind, "document"\\)')
  expect_match(blk, "rb_open_template\\(t, smp\\$path\\)")
  # the statement route survives, on the else branch
  expect_match(blk, "open_guided\\(smp\\$path")
  # a form has no picture at all, and says so instead of opening an empty one
  expect_match(blk, 'identical\\(kind, "fields"\\)')
  # and the BUTTON says which editor it opens before it is pressed
  btn <- .da_block(.da_src(), "output\\$adm_tpl_bands_btn <- renderUI", 12L)
  expect_match(btn, "Open it in the report builder")
  expect_match(btn, "Redraw its columns on a real statement")
})

test_that("opening a saved template stops the builder guessing over it", {
  src <- .da_src(); joined <- .da_joined()
  blk <- .da_whole(src, "rb_open_template <- function")
  expect_match(blk, "document_proposal_from_template\\(t\\)")
  expect_match(blk, "rb\\$tables <- prop\\$tables")
  expect_match(blk, "rb\\$pairs  <- prop\\$pairs")
  # the identity boxes are filled from the template, not left saying "new_report"
  for (i in c("rb_bank", "rb_type", "rb_id", "rb_fp"))
    expect_match(blk, sprintf('"%s"', i))
  # the auto-suggesters stand down, or they overwrite what somebody saved
  expect_match(blk, "rb_id_auto\\(NA_character_\\); rb_fp_auto\\(NA_character_\\)")
  expect_match(blk, "rb_editing\\(")
  expect_match(joined, "if \\(!is\\.na\\(rb_editing\\(\\)\\)\\) return\\(\\)")
  # ...and it lands on the right tab with the right half of it open
  expect_match(blk, 'updateRadioButtons\\(session, "ts_doctype", selected = "other"\\)')
  expect_match(blk, 'selected = "Add a template"')
  # WAS: expect_match(joined,
  #        "observeEvent\\(input\\$ts_file, \\{ rb_editing\\(NA_character_\\) \\}")
  # ...which pinned register H5 exactly as it was reported: "the editing flag is
  # cleared the moment a second document is uploaded, which un-blocks the seeding
  # observers: the issuer is overwritten from the new FILENAME and the
  # fingerprint is overwritten wholesale", making "add tables from a second
  # example" impossible. A new document is a new template only when there is no
  # work on screen to be another example OF.
  expect_match(joined, "\\.rb_has_work <- function")
  up <- .da_whole(src, "observeEvent\\(input\\$ts_file, \\{$")
  expect_match(up, "if \\(!\\.rb_has_work\\(\\)\\) \\{ rb_editing\\(NA_character_\\)")
  expect_match(up, "another example", fixed = TRUE)
})

test_that("the builder says when Save will REPLACE a saved template", {
  blk <- .da_block(.da_src(), "output\\$rb_editing_note <- renderUI", 8L)
  expect_match(blk, "Editing the saved template")
  expect_match(blk, "Save replaces it")
  expect_match(blk, "Building a template from")
})

# ---------------------------------------------------------------------------
# STATEMENT, OR NOT: ASKED IN FRONT, ANSWERED ONCE.
# ---------------------------------------------------------------------------

test_that("Convert asks what the document is, and the engine is told", {
  src <- .da_src(); joined <- .da_joined()
  blk <- .da_block(src, 'radioButtons\\("cv_kind"', 6L)
  for (v in c('"auto"', '"statement"', '"other"')) expect_match(blk, v, fixed = TRUE)
  expect_match(blk, 'selected = "auto"')          # nobody has to answer it
  # ONE reading of it, like the bank
  expect_match(joined, "kind_choice <- reactive")
  args <- .da_whole(src, "convert_args <- function")
  expect_match(args, "kind = kind_choice\\(\\)")
  # ...and a case folder answers it once for the whole folder
  expect_match(joined, "kind = kind_choice\\(\\)[,)]")
  expect_gte(length(grep("kind = kind_choice\\(\\)", src)), 2L)
})

test_that("nothing bank-shaped is shown once she has said it is not a statement", {
  joined <- .da_joined()
  # the bank picker folds away
  expect_match(joined, "conditionalPanel\\(\"input\\.cv_kind != 'other'\",\\s*\\n\\s*selectInput\\(\"cv_bank_quick\"")
  # ...and something says why it is gone
  expect_match(joined, "No bank templates will be tried", fixed = TRUE)
  # WAS: the exact-template override folded away with it. Register L1 names that
  # as the fault -- "a form or report template cannot be forced, anywhere in the
  # product ... it only ever lists statement templates and is hidden the moment
  # she says something else" -- so the panel STAYS and its list follows the kind.
  # What must not survive is a BANK-shaped word on it.
  expect_false(grepl("conditionalPanel(\"input.cv_kind != 'other'\",\n          tags$details(class = \"adv-bank\"",
                     joined, fixed = TRUE))
  expect_match(joined, 'tags\\$summary\\("It picked the wrong template\\?"\\)')
  # and the list really is rebuilt off the kind, not off the bank alone
  expect_match(joined, 'other <- identical\\(kind_choice\\(\\), "other"\\)')
})

test_that("the teach card does not re-ask a question she already answered", {
  blk <- .da_whole(.da_src(), "output\\$cv_teach <- renderUI")
  expect_match(blk, "asked <- as\\.character\\(res\\$asked_kind")
  expect_match(blk, 'identical\\(asked, "other"\\)')
  expect_match(blk, 'identical\\(asked, "statement"\\)')
  # and the override is still there, as the other answer rather than a re-ask
  expect_match(blk, "Is it a bank statement after all\\?")
  expect_match(blk, "Not a statement after all\\?")
  # the branch that answers "other" must not send her to the statement toolkit
  i_other <- regexpr('identical(asked, "other")', blk, fixed = TRUE)
  i_stmt  <- regexpr('identical(asked, "statement")', blk, fixed = TRUE)
  expect_gt(i_other, 0L); expect_gt(i_stmt, i_other)
})

# ---------------------------------------------------------------------------
# THE FINGERPRINT, DRAGGED OFF THE PAGE.
#
# "I put a phrase printed on it, bang smack on front page, still used another
# template. Maybe another drag?" It was the one box on the builder that had to
# be typed, and matching is exact - so a retyped dash or a stray space matches
# nothing at all.
# ---------------------------------------------------------------------------

test_that("the identifying phrase can be dragged, like everything else here", {
  src <- .da_src(); joined <- .da_joined()
  expect_match(.da_block(src, "\\.RB_ASK <- list\\(", 40L), "A PHRASE PRINTED ON THIS DOCUMENT")
  expect_match(joined, 'actionButton\\("rb_arm_phrase"')
  arm <- .da_whole(src, "observeEvent\\(input\\$rb_arm_phrase")
  expect_match(arm, 'rb\\$mode <- "phrase"')
  expect_match(arm, "Upload the document")       # no page, no drag, and it says so
  brush <- .da_whole(src, "observeEvent\\(input\\$rb_brush")
  expect_match(brush, 'if \\(m == "phrase"\\)')
  expect_match(brush, "\\.rb_read\\(box\\)")
  # ADDED, not replaced: a fingerprint is a list, all of which must appear
  expect_match(brush, "paste\\(c\\(have, txt\\), collapse")
  # ...and it does not add the same phrase twice
  expect_match(brush, "txt %in% have")
  # it is hers now, so the suggester stops
  expect_match(brush, "rb_fp_auto\\(NA_character_\\)")
  # a single short word is the fault that turns "no template" into a wrong read,
  # and the loader refuses it at save time -- said now, not two screens later
  expect_match(brush, "n_words >= 2L \\|\\| nchar\\(txt\\) >= 10L")
})

# ---------------------------------------------------------------------------
# L1. "A form or report template cannot be forced, anywhere in the product."
# convert_form(template_id=) and convert_tables(template_id=) both existed and
# NOTHING in the app ever passed one -- the picker listed statement templates and
# was hidden the moment she said "something else".
# ---------------------------------------------------------------------------

test_that("the template picker offers the templates that fit the answer she gave", {
  src <- .da_src(); joined <- .da_joined()
  # it is no longer inside the statement-only panel
  i_sum <- grep('tags\\$summary\\("It picked the wrong template\\?"\\)', src)
  expect_length(i_sum, 1L)
  expect_false(grepl("input.cv_kind != 'other'", src[i_sum - 1L], fixed = TRUE))
  # ...and its list is built per kind, off the two other-route libraries
  expect_match(joined, 'other <- identical\\(kind_choice\\(\\), "other"\\)')
  expect_match(joined, "ts <- c\\(all_doc_templates\\(\\), all_field_templates\\(\\)\\)")
  # the id travels: one force_template, honoured whatever kind it turns out to be
  expect_match(joined, "force_template = force_tpl %\\|\\|% tpl_choice\\(\\)")
  # and the ENGINE really takes a template of any kind and routes on it
  fr <- readLines(file.path(engine_root(), "R", "forms.R"), warn = FALSE)
  expect_true(any(grepl("template_id = NULL\\)", fr)))
  expect_true(any(grepl("\\.forced_template_kind\\(want", fr)))
  # never a raw id on the picker: the other-route list goes through the same
  # name-maker every other picker on this page uses
  expect_match(joined, "if \\(length\\(ids\\)\\) ch <- c\\(ch, tpl_choices\\(ids\\)\\)")
})

test_that("a report template's name reaches the screen instead of its id", {
  # friendly_tpl fell through to the RAW ID for a report template -- it looked in
  # the statement set and the form set and nowhere else -- so a converted report's
  # "Read as:" chip printed an engine handle on a customer-facing screen.
  blk <- .da_block(.da_src(), "friendly_tpl <- function", 45L)
  expect_match(blk, "all_doc_templates\\(\\)\\[\\[tid\\]\\]")
  # and NOT through .tpl_label, which would name a trustee report "... statement"
  i_doc <- regexpr("all_doc_templates()[[tid]]", blk, fixed = TRUE)
  expect_gt(i_doc, 0L)
  expect_false(grepl(".tpl_label", substr(blk, i_doc, i_doc + 260L), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# J1. Browser-proven: she opens "It picked the wrong bank?", picks a template,
# then ticks "Something else". The disclosure is HIDDEN by a conditionalPanel --
# but hiding does not clear, the selection survives, and the engine turned a
# forced statement template into kind "statement" outright.
# ---------------------------------------------------------------------------

test_that("saying it is not a statement clears the bank and the forced template", {
  src <- .da_src(); joined <- .da_joined()
  # the bank picker is cleared OUT LOUD when the kind becomes "other"
  blk <- .da_whole(src, 'observeEvent\\(kind_choice\\(\\), \\{')
  expect_match(blk, 'identical\\(kind_choice\\(\\), "other"\\)')
  expect_match(blk, 'updateSelectInput\\(session, "cv_bank_quick", selected = ""\\)')
  # ...and the template selection cannot survive, because the choices it was in
  # are no longer the choices: the list is rebuilt on the kind and a selection
  # that is not among them is dropped
  expect_match(joined, 'selected = if \\(keep %in% ch\\) keep else ""')
  # the engine half is landed too: an answer she can SEE beats a template she
  # cannot, and it says so
  fr <- readLines(file.path(engine_root(), "R", "forms.R"), warn = FALSE)
  expect_true(any(grepl("contradiction <- TRUE", fr, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# J4. "There is no route from a converted report back to the template that read
# it - and the route that WAS written is dead code." cv_teach's report branch sat
# inside a conditionalPanel requiring the result NOT to be a tables result.
# ---------------------------------------------------------------------------

test_that("a converted report has a live route back to the template that read it", {
  src <- .da_src(); joined <- .da_joined()
  # the dead branches are gone from the panel that could never render them...
  blk <- .da_whole(src, "output\\$cv_teach <- renderUI")
  expect_false(grepl('identical(res$kind, "tables")', blk, fixed = TRUE))
  expect_false(grepl('identical(res$kind, "form")', blk, fixed = TRUE))
  # ...and the live door is OUTSIDE that panel, above the transactions half
  i_edit <- grep('uiOutput\\("cv_edit"\\)', src)
  i_panel <- grep("output.cv_is_tables != true", src)
  expect_length(i_edit, 1L)
  expect_true(all(i_edit < i_panel))
  # it opens the SAVED report template on the document, which is the whole point:
  # switching tab without either is what the deleted links did
  expect_match(joined, "rb_open_template\\(t, src\\$path\\)")
  # rb_open_template really carries the document under the boxes
  expect_match(.da_whole(src, "rb_open_template <- function"), "rb_handoff\\(path\\)")
})

# ---------------------------------------------------------------------------
# H13. "Edit takes a saved table OUT of the template and the button beside it
# says Throw it away. One click, no confirmation, no undo, and table 27 is gone
# from a template that took months."
# ---------------------------------------------------------------------------

test_that("editing a saved table copies it, and saving puts it back where it was", {
  src <- .da_src(); joined <- .da_joined()
  ed <- .da_whole(src, "observeEvent\\(input\\[\\[paste0\\(\"rb_ed_\"")
  # the saved table is NOT taken out of the list...
  expect_false(grepl("rb$tables <- rb$tables[-i]", ed, fixed = TRUE))
  # ...and where it goes back to is remembered
  expect_match(ed, "rb\\$edit_idx <- as\\.integer\\(i\\)")
  # Save REPLACES at that index rather than appending, or one edit would leave
  # two copies of the table on the template.
  tb <- .da_whole(src, "observeEvent\\(input\\$rb_savetab")
  expect_match(tb, "rb\\$tables\\[\\[idx\\]\\] <- d")
  expect_match(tb, "idx <- \\.doc_int\\(rb\\$edit_idx")
  expect_match(tb, "rb\\$edit_idx <- NA_integer_")
  # A draft that arrived any other way can never overwrite a saved table.
  ld <- .da_whole(src, "\\.rb_load_draft <- function")
  expect_match(ld, "rb\\$edit_idx <- NA_integer_")
})

test_that("cancel keeps the saved table, and says which of the two it did", {
  src <- .da_src(); joined <- .da_joined()
  # the button's words are rendered, because they are not the same words in the
  # two cases it covers
  expect_match(joined, 'uiOutput\\("rb_cancel_btn"')
  btn <- .da_whole(src, "output\\$rb_cancel_btn <- renderUI")
  expect_match(btn, "Cancel - keep the saved table", fixed = TRUE)
  expect_match(btn, "Throw it away", fixed = TRUE)
  # and cancelling an edit does not touch rb$tables
  cn <- .da_whole(src, "observeEvent\\(input\\$rb_cancel")
  expect_false(grepl("rb$tables <-", cn, fixed = TRUE))
  expect_match(cn, "rb\\$edit_idx <- NA_integer_")
  expect_match(cn, "as it was saved", fixed = TRUE)
})

test_that("removing a saved table has to be meant twice, and is said both times", {
  src <- .da_src()
  rm <- .da_whole(src, "observeEvent\\(input\\[\\[paste0\\(\"rb_rm_\"")
  # the first press ARMS and says so; only a second press on the SAME table
  # removes it
  expect_match(rm, "rb\\$rm_armed <- list\\(i = as\\.integer\\(i\\)")
  expect_match(rm, "Press Remove again", fixed = TRUE)
  expect_match(rm, "rb\\$tables <- rb\\$tables\\[-i\\]")
  # ...and the arming is compared against the table's NAME as well as its index,
  # so a list that has shifted under the person cannot remove the wrong one
  expect_match(rm, 'identical\\(as\\.character\\(a\\$name')
  # an index above the one removed is not left pointing at a different table
  expect_match(rm, "if \\(idx > i\\) idx - 1L else idx")
})

test_that("the saved-table list stops where its handlers stop, and says so", {
  # .RB_MAX_ROWS observers exist; the list used to render a pair of buttons for
  # every table with no cap, so on a 61-table template the last Edit did nothing
  # at all when pressed and nothing said why.
  src <- .da_src()
  blk <- .da_whole(src, "output\\$rb_saved <- renderUI")
  expect_match(blk, "min\\(length\\(rb\\$tables\\), \\.RB_MAX_ROWS\\)")
  expect_match(blk, "length\\(rb\\$tables\\) > \\.RB_MAX_ROWS")
  expect_match(blk, "cannot be edited here", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# D5. "If I need to expand or contract a column in the middle of others... the
# other columns NEED to adapt to it rather than just do nothing."
# ---------------------------------------------------------------------------

test_that("a column moved into its neighbour pushes it along, with a floor and a wall", {
  src <- .da_src()
  i <- grep("^  \\.rb_move_band <- function", src)[1]
  skip_if(is.na(i), ".rb_move_band not found")
  # the floor comes from the source too, so this tests the rule and not a number
  # copied out of it
  eval(parse(text = grep("^  \\.RB_MIN_COL <- ", src, value = TRUE)[1]))
  f <- eval(parse(text = .da_whole(src, "^  \\.rb_move_band <- function")))
  cols <- list(list(name = "a", x_min = 10, x_max = 50),
               list(name = "b", x_min = 50, x_max = 90),
               list(name = "c", x_min = 90, x_max = 130))
  xs <- function(z) vapply(z, function(cc) c(cc$x_min, cc$x_max), numeric(2))
  # widening the MIDDLE column to the right moves the third one along, keeping
  # its width - it does not stop dead at 90, which is the reported fault
  out <- f(cols, 2L, 50, 110, 400)
  expect_equal(as.numeric(xs(out)[, 2]), c(50, 110))
  expect_equal(as.numeric(xs(out)[, 3]), c(110, 150))
  expect_equal(as.numeric(xs(out)[, 1]), c(10, 50))     # nothing behind it moved
  expect_identical(attr(out, "pushed"), 1L)
  expect_identical(attr(out, "narrowed"), 0L)
  # ...and to the left it pushes the first one the other way
  out2 <- f(cols, 2L, 30, 90, 400)
  expect_equal(as.numeric(xs(out2)[, 1]), c(0, 30))
  expect_identical(attr(out2, "pushed"), 1L)
  # THE WALL. The page edge is a wall: what cannot keep its width is narrowed to
  # the floor rather than pushed off the paper, and that is reported.
  out3 <- f(cols, 2L, 50, 380, 400)
  expect_true(as.numeric(xs(out3)[2, 3]) <= 400)
  expect_gte(as.numeric(xs(out3)[2, 3]) - as.numeric(xs(out3)[1, 3]), .RB_MIN_COL)
  expect_identical(attr(out3, "narrowed"), 1L)
  # and a request that cannot fit even at the floor is trimmed to what fits,
  # never applied off the page
  out4 <- f(cols, 2L, 50, 460, 400)
  expect_true(all(as.numeric(xs(out4)[2, ]) <= 400))
  expect_true(isTRUE(attr(out4, "trimmed")))
  # a column that does not reach its neighbour moves alone
  out5 <- f(cols, 1L, 10, 40, 400)
  expect_identical(attr(out5, "pushed"), 0L)
  expect_equal(as.numeric(xs(out5)[, 2]), c(50, 90))
  # nonsense in, the columns back out unchanged
  expect_identical(f(cols, 9L, 10, 20, 400), cols)
  expect_identical(f(cols, 2L, 50, 51, 400)[[2]]$x_max, 90)
})

test_that("what the push did to the other columns is said, once, in plain words", {
  src <- .da_src()
  msg <- eval(parse(text = .da_whole(src, "^  \\.rb_push_msg <- function")))
  n <- function(p, nw = 0L, tr = FALSE) {
    x <- list(); attr(x, "pushed") <- p; attr(x, "narrowed") <- nw
    attr(x, "trimmed") <- tr; x
  }
  expect_identical(msg(n(0L)), "")                       # nothing moved, nothing said
  expect_match(msg(n(1L)), "1 column beside it moved along", fixed = TRUE)
  expect_match(msg(n(3L, 2L)), "2 had to be narrowed", fixed = TRUE)
  expect_match(msg(n(2L, 0L, TRUE)), "as far as the page allows", fixed = TRUE)
  # no ids, no numbers a person cannot act on, one sentence
  expect_false(grepl("x_min|attr|NA", msg(n(2L, 1L))))
  expect_length(gregexpr("\\.", msg(n(2L, 1L)))[[1]], 2L)
  # both ways of moving a band go through it
  joined <- .da_joined()
  expect_gte(length(grep("\\.rb_push_msg\\(", src)), 3L)
})

# ---------------------------------------------------------------------------
# D4. "The bottom edge on pages in between needs to default show up. Shouldn't
# need to work out end for me."
# ---------------------------------------------------------------------------

test_that("the bottom edge of the pages in between is set, drawn and correctable", {
  src <- .da_src(); joined <- .da_joined()
  fn <- .da_whole(src, "\\.rb_default_bottom <- function")
  # a one-page table has no pages in between, so it is left alone
  expect_match(fn, "if \\(is\\.na\\(p0\\) \\|\\| is\\.na\\(p1\\) \\|\\| p1 <= p0\\) return\\(d\\)")
  # a bottom edge somebody has already set is NEVER overwritten
  expect_match(fn, "if \\(is\\.finite\\(\\.doc_num\\(b\\$y_max, NA_real_\\)\\)\\) return\\(d\\)")
  # the answer is MEASURED with the reader's own stop rule on the table's own
  # start page, not guessed
  expect_match(fn, "probe\\$follow <- FALSE")
  expect_match(fn, "doc_table_rows\\(i, probe, fr\\)")
  expect_match(fn, "b\\$y_max <- round\\(y \\+ 2, 1\\)")
  # ...and every way of setting the end goes through it
  expect_gte(length(grep("\\.rb_default_bottom\\(d\\)", src)), 4L)
  # DRAWN, or it is the same silent assumption in a different place
  plot <- .da_whole(src, "\\.rb_draw_table <- function")
  expect_match(plot, 'ymax <- \\.doc_num\\(\\(tb\\$band')
  expect_match(plot, '"BOTTOM"')
  # and the panel says it in words, with the one control that moves it
  w <- .da_whole(src, "output\\$rb_where <- renderUI")
  expect_match(w, "worked out by reading this document", fixed = TRUE)
  expect_match(w, '"rb_setbottom"')
})

# ---------------------------------------------------------------------------
# A3 (findings). The drag box round the column names is a SAMPLE; a column that
# prints nothing inside it yields no band, and because the surviving bands tile,
# its territory goes to a neighbour - silently.
# ---------------------------------------------------------------------------

test_that("the columns are re-derived once over the whole table, and the gain is said", {
  src <- .da_src()
  blk <- .da_whole(src, 'if \\(m == "cols"\\)')
  # the end is known first, so there is a whole table to derive from
  i_end <- regexpr("doc_auto_end\\(", blk)
  i_fit <- regexpr("doc_fit_columns\\(i, d, fr\\)", blk)
  expect_gt(i_end, 0L); expect_gt(i_fit, i_end)
  # it may only ever ADD - never fewer columns than the box just named
  expect_match(blk, "if \\(length\\(fit\\) >= length\\(cols\\)\\)")
  # the remembered heading wording follows the columns it actually has
  expect_match(blk, "d\\$anchor\\$header_text <- as\\.list\\(vapply\\(cols")
  # NOTHING SILENT: a correction to what was just drawn is said, once
  expect_match(blk, "gained > 0L")
  expect_match(blk, "found by reading the whole table", fixed = TRUE)
  # ...and it runs ONCE, at the moment the columns are first derived: a band
  # somebody has since drawn or moved must never be re-derived under them
  expect_length(grep("doc_fit_columns\\(", src), 2L)   # here, and "work them out again"
})

# ---------------------------------------------------------------------------
# H4. "Loading a second example re-stamps the template's reference page size...
# the file says 612x792 while every band is still in 595x842 space."
# ---------------------------------------------------------------------------

test_that("an open template keeps the page size its boxes were drawn in", {
  src <- .da_src()
  fr <- .da_whole(src, "rb_frame <- reactive")
  # the template's own frame wins over whatever document is open
  expect_match(fr, "f <- rb\\$frame")
  expect_match(fr, "return\\(f\\)")
  # ...and it still falls back to the document's page size for a new template
  expect_match(fr, "page_width"); expect_match(fr, "page_height")
  op <- .da_whole(src, "rb_open_template <- function")
  expect_match(op, "t\\$ref_width")
  expect_match(op, "t\\$ref_height")
  expect_match(op, "rb\\$frame <- if")
  # a template being drawn for the first time has no frame of its own until the
  # first box is drawn - and then it pins one, or a second example of a different
  # page size moves every box already on screen
  expect_match(.da_joined(), "rb\\$frame <- NULL")
  pin <- .da_whole(src, "\\.rb_pin_frame <- function")
  expect_match(pin, "if \\(!is\\.null\\(rb\\$frame\\)\\) return")
  expect_match(pin, "rb\\$frame <- f")
  # and it can never be the reason a table failed to open
  expect_match(pin, "tryCatch\\(rb_frame\\(\\)")
  for (fn in c("\\.rb_load_draft <- function", "\\.rb_load_vdraft <- function"))
    expect_match(.da_whole(src, fn), "\\.rb_pin_frame\\(\\)", info = fn)
  # ...and a second example never re-stamps it
  up <- .da_whole(src, "observeEvent\\(input\\$ts_file, \\{$")
  expect_false(grepl("rb$frame <- rb_frame()", up, fixed = TRUE))
  # NOT SILENT: a page of a different size is read scaled, and that is said
  nt <- .da_whole(src, "output\\$rb_frame_note <- renderText")
  expect_match(nt, "a different size", fixed = TRUE)
  expect_match(.da_joined(), 'textOutput\\("rb_frame_note"')
})

# ---------------------------------------------------------------------------
# H5. "There is no add tables from a second example - and it is the mode the
# owner described."
# ---------------------------------------------------------------------------

test_that("a second example changes only the page, and the seeders stand down", {
  src <- .da_src(); joined <- .da_joined()
  up <- .da_whole(src, "observeEvent\\(input\\$ts_file, \\{$")
  # identity, tables and values all stand: the observer touches none of them
  for (bad in c("rb$tables <- list()", "rb$pairs <- list()",
                'updateTextInput(session, "rb_bank"',
                'updateTextAreaInput(session, "rb_fp"'))
    expect_false(grepl(bad, up, fixed = TRUE), info = bad)
  # the file just chosen IS the document: rb_doc() prefers a handed-over path,
  # so without this the picker did nothing after a template was opened
  expect_match(up, "rb_handoff\\(NULL\\)")
  # ...and it is not silent
  expect_match(up, "another example", fixed = TRUE)
  # the three seeding observers stand down while there is work on screen, or the
  # issuer is overwritten from the new filename and the phrases wholesale
  seed <- .da_block(src, "rb_id_auto <- reactiveVal", 72L)
  expect_match(seed, "if \\(length\\(rb\\$tables\\) \\|\\| length\\(rb\\$pairs\\)\\) return\\(\\)")
  # there IS a way back, and it is the only thing that empties the screen
  expect_match(joined, 'actionLink\\("rb_fresh"')
  fresh <- .da_whole(src, "observeEvent\\(input\\$rb_fresh")
  expect_match(fresh, "rb\\$tables <- list\\(\\)")
  expect_match(fresh, "rb_editing\\(NA_character_\\)")
  expect_match(fresh, "Nothing was saved to the library", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# L5. "Saving a report template neither clears the pickup queue nor guards
# against being silently shadowed."
# ---------------------------------------------------------------------------

test_that("saving a report template clears the pickup queue and cannot be shadowed", {
  src <- .da_src()
  blk <- .da_whole(src, "observeEvent\\(input\\$rb_save, \\{")
  # the same status word the statement toolkit writes, which is what takes the
  # document off Admin's "needs pickup" list
  expect_match(blk, 'set_upload_status\\(cv_upload_id\\(\\), "wizard_saved"')
  # ...but only for the upload this template was actually taught FROM
  expect_match(blk, "identical\\(as\\.character\\(h\\)\\[1\\], as\\.character\\(sc\\$path\\)\\[1\\]\\)")
  # a save under a curated id would be loaded from the curated folder first and
  # never used, so it is renamed and SAID rather than silently shadowed
  expect_match(blk, 'identical\\(as\\.character\\(cur\\$origin')
  expect_match(blk, 'paste0\\(t\\$id, "_custom"\\)')
  expect_match(blk, "would never have been used", fixed = TRUE)
  # and the library is reloaded, so the template is usable without a restart
  expect_match(blk, "tpl_bump\\(")
})

# ---------------------------------------------------------------------------
# D2, D3, D6. "There are LOTS of buttons, lots of things to interact with. It
# NEEDS to be even more simple." / "Save - this needs to be more prominent." /
# "Clear distinction between when I'm editing columns, tables etc."
# ---------------------------------------------------------------------------

test_that("the builder does not offer two controls for one job", {
  joined <- .da_joined()
  # "Drag its width on the page" armed the drag pressing Edit already arms, and
  # "Delete this column" did what the x on the column's own row does.
  expect_false(grepl('actionButton("rb_cdrag"', joined, fixed = TRUE))
  expect_false(grepl('actionButton("rb_cdel"', joined, fixed = TRUE))
  expect_false(grepl("input$rb_cdrag", joined, fixed = TRUE))
  expect_false(grepl("input$rb_cdel", joined, fixed = TRUE))
  # ...and the two things they did are still one press away
  expect_match(joined, 'observeEvent\\(input\\[\\[paste0\\("rb_cs_", j\\)\\]\\]')
  expect_match(joined, 'paste0\\("rb_cx_", j\\)')
})

test_that("the answer the tool worked out is on screen even when the control is folded away", {
  src <- .da_src(); joined <- .da_joined()
  # "It's top row" is collapsed, as asked -- but the ANSWER is printed in the
  # line above it, or folding the control away would hide a fact.
  i_sum <- grep("Change what the top rows are", src)[1]
  i_hdr <- grep('radioButtons\\("rb_hdr"', src)[1]
  expect_false(is.na(i_sum)); expect_true(i_sum < i_hdr)
  step <- .da_whole(src, "output\\$rb_step <- renderUI")
  expect_match(step, "its top row names the columns", fixed = TRUE)
  expect_match(step, "its top row is data", fixed = TRUE)
  # the save name is composed, so its box is folded away -- and printed
  expect_match(joined, 'textOutput\\("rb_id_note"')
  expect_match(.da_whole(src, "output\\$rb_id_note <- renderText"), "Saves as %s", fixed = TRUE)
})

test_that("Save pulls the eye, and says what it will do", {
  src <- .da_src(); joined <- .da_joined()
  expect_match(joined, 'actionButton\\("rb_save", "Save template",\\s*\\n?\\s*class = "btn-primary btn-lg rb-save"\\)')
  css <- readLines(file.path(engine_root(), "www", "app.css"), warn = FALSE)
  expect_true(any(grepl("^\\.rb-save\\{", css)))
  note <- .da_whole(src, "output\\$rb_save_note <- renderText")
  expect_match(note, "Nothing to save yet", fixed = TRUE)
  expect_match(note, "Replaces the saved template", fixed = TRUE)
  expect_match(note, "Adds %s to the library", fixed = TRUE)
  expect_match(joined, 'textOutput\\("rb_save_note"')
})

test_that("the banner always says which of the four things the next gesture is about", {
  src <- .da_src()
  about <- eval(parse(text = .da_whole(src, "^  \\.rb_about <- function")))
  expect_identical(about("title"), "TABLE")
  expect_identical(about("cols"), "TABLE")
  expect_identical(about("bottom"), "TABLE")
  expect_identical(about("addcol"), "COLUMN")
  expect_identical(about("colpos"), "COLUMN")
  expect_identical(about("label"), "VALUE")
  expect_identical(about("value"), "VALUE")
  expect_identical(about("phrase"), "DOCUMENT")
  expect_identical(about(""), "")
  expect_identical(about(NULL), "")
  # every mode the screen can be in has an answer
  ask <- .da_block(src, "\\.RB_ASK <- list\\(", 40L)
  for (m in c("title", "cols", "addcol", "colpos", "start", "end", "bottom",
              "label", "value", "phrase"))
    expect_true(nzchar(about(m)), info = m)
  # and it is written in the banner
  expect_match(.da_block(src, "output\\$rb_arm <- renderUI", 40L), "\\.rb_about\\(m\\)")
})

test_that("a column list written out of order still pushes the right neighbours", {
  # Everything that builds a column list sorts it, but a template written by
  # hand need not have - and "the one beside it" is meaningless on an unsorted
  # list, so the wrong columns would move.
  src <- .da_src()
  eval(parse(text = grep("^  \\.RB_MIN_COL <- ", src, value = TRUE)[1]))
  f <- eval(parse(text = .da_whole(src, "^  \\.rb_move_band <- function")))
  jumbled <- list(list(name = "c", x_min = 90, x_max = 130),
                  list(name = "a", x_min = 10, x_max = 50),
                  list(name = "b", x_min = 50, x_max = 90))
  out <- f(jumbled, 3L, 50, 110, 400)          # "b" is the middle one
  nm <- vapply(out, function(cc) cc$name, character(1))
  expect_identical(nm, c("a", "b", "c"))       # answered left to right
  expect_equal(out[[1]]$x_max, 50)             # "a" did not move
  expect_equal(c(out[[3]]$x_min, out[[3]]$x_max), c(110, 150))
})

test_that("the ways to say where it ends are folded, and the direct corrections are not", {
  # "There should be CLEAR distinctions between what's needed info wise NOW and
  # report level." The end and the bottom edge are worked out; the four
  # shortcuts are how to answer faster when one of them is wrong, so they fold.
  src <- .da_src()
  w <- .da_whole(src, "output\\$rb_where <- renderUI")
  i_sum <- regexpr("Other ways to say where it ends", w, fixed = TRUE)
  expect_gt(i_sum, 0L)
  for (b in c("rb_endauto", "rb_endpage", "rb_endlast", "rb_clearbottom"))
    expect_gt(regexpr(sprintf('"%s"', b), w, fixed = TRUE), i_sum, label = b)
  # the two that correct a boundary directly stay on the line they belong to
  for (b in c("rb_setstart", "rb_setend", "rb_gostart", "rb_goend", "rb_setbottom"))
    expect_lt(regexpr(sprintf('"%s"', b), w, fixed = TRUE), i_sum, label = b)
})

# ---------------------------------------------------------------------------
# ADMIN: THE LIBRARY AND WHAT IT IS SAYING
#
# "All templates are accessible, but broken down by statement and other... All
# templates should also be viewable in admin, but split by statement and other."
# "All feedback accessible in admin in same area, but ensure it can be traced to
# the statement and template."
# ---------------------------------------------------------------------------

# .ad_fun(name, also, consts) -- app.R's pure Admin helpers, lifted and CALLED.
# Same trick as test-app-ui.R's .ui_fun: read forward from the definition until it
# parses, evaluate it in an environment whose parent is globalenv (which carries
# the engine). A grep would only prove the source mentions a join; this proves the
# join puts the document name in the row.
.ad_fun <- function(name, also = character(0), consts = character(0)) {
  src <- .da_src()
  env <- new.env(parent = globalenv())
  for (nm in consts) {
    i <- grep(sprintf("^\\s*\\Q%s\\E <- ", nm), src, perl = TRUE)
    expect_length(i, 1L)
    assign(nm, eval(parse(text = src[i[1]])[[1]], envir = env), envir = env)
  }
  for (nm in unique(c(also, name))) {
    i <- grep(sprintf("^\\s*\\Q%s\\E <- function", nm), src, perl = TRUE)
    expect_length(i, 1L)
    got <- FALSE
    for (j in seq(i[1], min(i[1] + 250L, length(src)))) {
      f <- tryCatch(eval(parse(text = paste(src[i[1]:j], collapse = "\n"))[[1]], envir = env),
                    error = function(e) NULL)
      if (is.function(f)) { assign(nm, f, envir = env); got <- TRUE; break }
    }
    expect_true(got, info = paste("could not lift", nm, "out of app.R"))
  }
  get(name, envir = env)
}

test_that("C1: every kind of template lands in one of exactly two bands", {
  route <- .ad_fun(".adm_route", consts = ".ADM_ROUTES")
  expect_identical(route("statement"), "Bank statement")
  expect_identical(route("fields"),    "Other")
  expect_identical(route("document"),  "Other")
  # the run log's words for the same two kinds, so a rating on a form or a report
  # cannot fall out of the Other band just because the record spells it differently
  expect_identical(route("form"),   "Other")
  expect_identical(route("tables"), "Other")
  # and a kind nobody recognises is NEVER folded into a route it might not be
  expect_identical(route(NA), "Route not recorded")
  expect_identical(route(NULL), "Route not recorded")
  expect_identical(route("something_new"), "Route not recorded")
  # ...and it sorts AFTER the two bands the split is about, because the table is
  # grouped alphabetically on this column and the odd band must not land between
  # them
  bands <- c(route("statement"), route("fields"), route(NA))
  expect_identical(sort(bands), bands)
})

test_that("C1: the library is ONE table split into headed bands, not one list", {
  src <- .da_src()
  blk <- .da_whole(src, "output\\$adm_tpl_overview <- renderDT")
  # a real leading column carrying the route...
  expect_match(blk, "route =")
  expect_match(blk, "\\.adm_rowgroup\\(")
  grp <- .da_whole(src, "\\.adm_rowgroup <- function")
  # ...drawn as the band heading by DT, and HIDDEN, so the split costs no second
  # table, no second observer and no control
  expect_match(grp, "rowGroup")
  expect_match(grp, "dataSrc = 0L")
  expect_match(grp, "visible = FALSE")
  expect_match(grp, 'extensions = "RowGroup"')
  # the row-click handler must read the SAME frame the table was drawn from, or a
  # row index picks the wrong template the moment the two are built differently
  sel <- .da_whole(src, "observeEvent\\(input\\$adm_tpl_overview_rows_selected")
  expect_match(sel, "adm_ov\\(\\)")
  expect_match(sel, "ov\\$id\\[i\\]")
})

test_that("C2: a rating carries the document it was left on and the template that read it", {
  fbo <- .ad_fun(".adm_feedback_overview", also = ".adm_route", consts = ".ADM_ROUTES")
  fb <- data.frame(
    ts = c("2026-08-20T01:00:00Z", "2026-08-21T01:00:00Z", "2026-08-22T01:00:00Z"),
    run_id = c("r1", "r2", "r3"),
    verdict = c("correct", "wrong", "minor"),
    flagged = c(FALSE, TRUE, TRUE),
    comment = c(NA, "the closing balance is wrong", ""),
    requested_by = c("beth", "beth", "sam"),
    template_id = c("anz_pdf", "acme_report", "gone_tpl"),
    stringsAsFactors = FALSE)
  runs <- data.frame(
    run_id = c("r1", "r2"),
    source_file = c("/tmp/uploads/SMITH - ANZ - March.pdf", "/tmp/uploads/quarterly.pdf"),
    kind = c("statement", "tables"), stringsAsFactors = FALSE)
  lib <- list(
    anz_pdf     = list(id = "anz_pdf", mode = "statement", bank = "ANZ", statement_type = "everyday"),
    acme_report = list(id = "acme_report", mode = "document", bank = "ACME"))

  out <- fbo(fb, runs, lib)
  # EVERY rating, not just the flagged ones -- a form has no reconciliation behind
  # it, so a "correct" is evidence too
  expect_equal(nrow(out), 3L)
  expect_true(all(c("route", "when", "document", "template", "verdict", "comment",
                    "who", "template_id", "run_id") %in% names(out)))
  # the statement band first, then Other, then anything whose route is unknown
  expect_identical(out$route, c("Bank statement", "Other", "Route not recorded"))
  # the document name comes off the RUN log -- the feedback record has never held it
  expect_identical(out$document[out$run_id == "r1"], "SMITH - ANZ - March.pdf")
  # the template is NAMED, never a bare id, and the id survives as its own column
  expect_identical(out$template[out$run_id == "r2"], "ACME")
  expect_identical(out$template_id[out$run_id == "r2"], "acme_report")
  # a template that has since been deleted does not silently take a route with it
  expect_identical(out$route[out$run_id == "r3"], "Route not recorded")
  # and neither a missing run record nor a blank comment prints as an empty cell
  expect_identical(out$document[out$run_id == "r3"], "not recorded")
  expect_identical(out$comment[out$run_id == "r3"], "not recorded")
  expect_identical(out$comment[out$run_id == "r1"], "not recorded")

  # A DELETED TEMPLATE falls back to the RUN's own kind rather than to nothing.
  runs2 <- rbind(runs, data.frame(run_id = "r3", source_file = "/tmp/f.pdf",
                                  kind = "form", stringsAsFactors = FALSE))
  expect_identical(fbo(fb, runs2, lib)$route[fbo(fb, runs2, lib)$run_id == "r3"], "Other")
  # nothing at all is an empty frame with the promised columns, not an error
  expect_equal(nrow(fbo(NULL, NULL, list())), 0L)
  expect_equal(nrow(fbo(data.frame(), NULL, list())), 0L)
})

test_that("C2: the feedback table sits beside the templates and leads back to one", {
  src <- .da_src(); joined <- .da_joined()
  # rendered on the Templates tab, split the same way as the library above it
  expect_match(joined, 'DTOutput\\("adm_tpl_feedback"\\)')
  expect_match(.da_whole(src, "output\\$adm_tpl_feedback <- renderDT"), "\\.adm_rowgroup\\(")
  # traceable means a ROUTE to the template, not a printed id
  clk <- .da_whole(src, "observeEvent\\(input\\$adm_tpl_feedback_rows_selected")
  expect_match(clk, 'updateSelectInput\\(session, "adm_tpl_pick"')
  # ...and it says so when that template is no longer in the library, rather than
  # selecting nothing and looking broken
  expect_match(clk, "no longer in the library")
  # the old flagged-only table on a different tab is GONE, not a second copy
  expect_false(grepl("output$adm_feedback <- renderDT", joined, fixed = TRUE))
})

test_that("J2: a template that would not load is NAMED on screen, not silently absent", {
  src <- .da_src()
  blk <- .da_whole(src, "output\\$adm_tpl_load_errors <- renderUI")
  # all three loaders collect the reasons; all three are read
  for (r in c("all_templates\\(\\)", "all_field_templates_admin\\(\\)",
              "all_doc_templates_admin\\(\\)"))
    expect_match(blk, sprintf('attr\\(%s, "load_errors"\\)', r))
  # it says the consequence, not just the count -- a template that did not load is
  # not in use, which is the fact that explains "my template stopped working"
  expect_match(blk, "not in use")
  # and it is at the TOP of the tab, above the library it is about
  ui <- grep('uiOutput\\("adm_tpl_load_errors"\\)', src)[1]
  tbl <- grep('DTOutput\\("adm_tpl_overview"\\)', src)[1]
  expect_true(!is.na(ui) && !is.na(tbl) && ui < tbl)

  # END TO END: a report template the loader refuses really does come back with a
  # reason attached, which is the thing app.R now reads.
  d <- tempfile("j2_"); dir.create(d)
  yaml::write_yaml(list(id = "broken_rep", mode = "document",
                        fingerprint = list(page_contains_all = list("x")),
                        tables = list()), file.path(d, "broken.yaml"))
  got <- load_document_templates(d, NULL, include_hidden = TRUE)
  expect_length(got, 0L)
  why <- as.character(attr(got, "load_errors") %||% character(0))
  expect_true(length(why) >= 1L)
  expect_match(why[1], "broken_rep")
})

test_that("H7: one button asks whether the rest of the template still reads", {
  src <- .da_src(); joined <- .da_joined()
  expect_match(joined, 'actionButton\\("adm_tpl_check"')
  blk <- .da_whole(src, "observeEvent\\(input\\$adm_tpl_check")
  expect_match(blk, "req\\(admin_ok\\(\\)\\)")
  expect_match(blk, "template_check\\(t, paths")
  # THE EXAMPLES ARE WORKED OUT, NOT ASKED FOR: no folder picker, no second
  # template chooser -- it uses the template already selected and the documents
  # this box has already read with it.
  expect_match(blk, "\\.tpl_examples\\(id\\)")
  expect_match(blk, "adm_lib\\(\\)\\[\\[id\\]\\]")
  ex <- .da_whole(src, "\\.tpl_examples <- function")
  expect_match(ex, "read_uploads\\(UPLOADS_DIR\\)")
  expect_match(ex, "u\\$template == tid")
  expect_match(ex, "!u\\$purged")           # a purged upload has no file to read
  expect_match(ex, "utils::head\\(ps, n\\)")   # bounded: it blocks the one process
  # a stale grid over a template that has since been changed is the wrong figure
  # that looks right, one screen along
  expect_match(joined, "adm_check\\(NULL\\)")
  # and it never adds a control of its own for the folder
  expect_false(grepl('fileInput("adm_tpl_check', joined, fixed = TRUE))
})

test_that("H9: a template saved by somebody else is not silently overwritten", {
  src <- .da_src()
  save <- .da_whole(src, "observeEvent\\(input\\$adm_tpl_save")
  # what was OPENED, compared with what is on disk NOW -- not with the app's cache
  expect_match(save, "\\.adm_on_disk\\(kind, tid\\)")
  expect_match(save, "adm_tpl_opened\\(\\)")
  expect_match(save, "template_sha256")
  # it refuses ONCE and says so; the next press means it (the pattern Remove uses
  # in the builder), so no new control is needed to confirm
  expect_match(save, "adm_tpl_forced")
  expect_match(save, "Somebody else on this server saved this template")
  expect_match(save, "press Save once more")
  # the box is stamped when it is FILLED, or there is nothing to compare against
  pick <- .da_whole(src, "observeEvent\\(input\\$adm_tpl_pick,")
  expect_match(pick, "adm_tpl_opened\\(list\\(")
  # and it reads the USER folders, which are the only ones a save can write to
  disk <- .da_whole(src, "\\.adm_on_disk <- function")
  expect_match(disk, "USER_FIELDS_DIR")
  expect_match(disk, "USER_DOC_DIR")
  expect_match(disk, "USER_TEMPLATES_DIR")
})

test_that("K7: Admin reads a bounded slice of the history and says which slice", {
  hist <- .ad_fun(".adm_history", consts = ".ADM_HISTORY_MAX")
  d <- tempfile("k7_"); dir.create(file.path(d, "runs"), recursive = TRUE)
  dir.create(file.path(d, "archive"), recursive = TRUE)
  for (i in 1:5)
    writeLines(jsonlite::toJSON(list(run_id = paste0("live", i), status = "ok"),
                                auto_unbox = TRUE),
               file.path(d, "runs", sprintf("live%02d.json", i)))
  writeLines(vapply(1:40, function(i)
    as.character(jsonlite::toJSON(list(run_id = paste0("arch", i), status = "ok"),
                                  auto_unbox = TRUE)), character(1)),
    file.path(d, "archive", "runs-2025.jsonl"))

  # UNBOUNDED IS WHAT FROZE EVERY BROWSER: 20,000 archived rows took 10.5s in the
  # one process the team shares. Given a budget it reads that many and no more.
  small <- hist(d, "runs", budget = 7L)
  expect_equal(nrow(small), 7L)
  expect_identical(unname(attr(small, "kept_of")[["total"]]), 45L)
  expect_identical(unname(attr(small, "kept_of")[["read"]]), 7L)
  # the LIVE folder first -- it is what has not been archived yet, so it is newest
  expect_true(all(grepl("^live", small$run_id[1:5])))
  # ...and with room for everything, everything is read and nothing is claimed to
  # be missing
  whole <- hist(d, "runs", budget = 500L)
  expect_equal(nrow(whole), 45L)
  expect_identical(unname(attr(whole, "kept_of")[["total"]]), 45L)
  # an empty folder is an empty frame, not an error
  e <- tempfile("k7e_"); dir.create(e)
  expect_equal(nrow(hist(e, "runs")), 0L)

  # and the screen is wired to it -- for the RATINGS too, which used to read only
  # the live folder, so every rating older than the rollup had already vanished
  # from the one screen that is meant to hold all of them
  la <- .da_whole(.da_src(), "load_admin <- function")
  expect_match(la, '\\.adm_history\\(LOGDIR, "runs"\\)')
  expect_match(la, '\\.adm_history\\(LOGDIR, "feedback"\\)')
  expect_false(grepl("read_runs_all", la, fixed = TRUE))
  # a partial picture that does not admit it is partial is what the charter forbids
  note <- .da_whole(.da_src(), "output\\$adm_history_note <- renderUI")
  expect_match(note, "kept_of")
  expect_match(note, "logs/archive/")
})

test_that("L6: the drift table says which route each failing template belongs to", {
  blk <- .da_whole(.da_src(), "output\\$adm_drift <- renderDT")
  expect_match(blk, "\\.adm_route\\(")
  expect_match(blk, "template_drift\\(d\\$runs\\)")
  # the engine defines health per kind now; a report that is not reconciling is
  # not a fact, so the wording above the table must not promise one
  ui <- .da_block(.da_src(), 'h4\\("Templates that started failing recently"\\)', 12L)
  expect_false(grepl("stop adding up", ui, fixed = TRUE))
  expect_match(ui, "whichever kind it is")
})

test_that("1c: Admin is two tabs, and the control count did not go up", {
  src <- .da_src()
  # the Admin sub-tabs -- the only tabPanel( at this indent in the file
  i <- grep("^        tabPanel\\($", src)
  names_ <- trimws(gsub('[",]', "", src[i + 1L]))
  expect_identical(names_, c("Templates", "Health"))
  # nothing was DELETED to get there -- every control that did something is still
  # somewhere on one of the two
  joined <- .da_joined()
  for (id in c("adm_meta_level", "adm_meta_save", "adm_lex_save", "adm_word_add",
               "adm_sugg_approve", "adm_ba_run", "adm_dict_add", "adm_rollup",
               "adm_purge_uploads", "adm_up_wizard", "adm_inbox_wizard"))
    expect_match(joined, sprintf('"%s"', id), fixed = TRUE)
  # THE BUDGET. One control was added (the template check) and two removed (the
  # metadata category list, which repeated the level above it, and the
  # single-statement audit picker, a third route to an export two other controls
  # already offer). It must never drift back up.
  pat <- paste0("(actionButton|actionLink|textInput|textAreaInput|numericInput|",
                "selectInput|selectizeInput|checkboxInput|checkboxGroupInput|",
                "radioButtons|fileInput|sliderInput|downloadButton|downloadLink)",
                "[(][\"][a-zA-Z0-9_]+")
  ids <- sub('.*"', "", unlist(regmatches(src, gregexpr(pat, src))))
  expect_lte(sum(startsWith(ids, "adm_")), 44L)
  expect_lte(length(ids), 180L)
  expect_false("adm_meta_cats" %in% ids)
  expect_false("adm_audit_one" %in% ids)
  expect_true("adm_tpl_check" %in% ids)
})
