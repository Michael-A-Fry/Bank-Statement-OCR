# Static invariants over the FRONT END of app.R.
#
# app.R is not sourced by the suite (it needs a live Shiny session), so the rules
# that only exist on screen are held to their word the way test-admin_auth.R and
# test-app-adoption.R do it: read the file and assert the rule. These are the
# screen-level promises -- one visual language for "how did it go", never naming a
# template that did not read the statement, and never leaving the user in front of
# a dead end with nothing to try.

.ui_src <- function() {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  readLines(app, warn = FALSE)
}
# .ui_fun(name, also) -- one of app.R's PURE helpers, lifted out and made callable.
#
# Most of this file has to assert on source text, because app.R needs a live
# Shiny session to run. The small pure functions inside it do not: they are
# ordinary closures over nothing but each other. So the definition is found by
# name, read forward until it parses (which is exactly where the function ends),
# and evaluated. That turns "the source mentions a date check" into "the date
# check actually rejects 01/02/2014" -- a fact about behaviour, which survives
# any rewording of the code around it. `also` names the helpers it calls, which
# are put in the same environment so it can find them.
#
# `consts` does the same job for plain VALUES -- a shared regex, a threshold. A
# helper that reads a module-level constant is unliftable without them, and
# inlining the constant into each caller instead is exactly the duplication the
# constant exists to remove (three copies of the auto-split tag regex is how the
# proof strip came to be the one reader that did not know about it).
.ui_fun <- function(name, also = character(0), consts = character(0)) {
  src <- .ui_src()
  # globalenv carries the engine (%||%, safe); ui_labels.R sits between, because
  # app.R sources it and several of these helpers read a wording map out of it.
  # It can only ADD names to the chain, never shadow one app.R defines.
  lab <- new.env(parent = globalenv())
  sys.source(file.path(engine_root(), "ui_labels.R"), envir = lab)
  env <- new.env(parent = lab)
  for (nm in consts) {
    i <- grep(sprintf("^\\s*\\Q%s\\E <- ", nm), src, perl = TRUE)
    testthat::expect_length(i, 1L)
    assign(nm, eval(parse(text = src[i[1]])[[1]], envir = env), envir = env)
  }
  for (nm in unique(c(also, name))) {
    i <- grep(sprintf("^\\s*\\Q%s\\E <- function", nm), src, perl = TRUE)
    testthat::expect_length(i, 1L)
    got <- FALSE
    for (j in seq(i[1], min(i[1] + 250L, length(src)))) {
      f <- tryCatch(eval(parse(text = paste(src[i[1]:j], collapse = "\n"))[[1]], envir = env),
                    error = function(e) NULL)
      if (is.function(f)) { assign(nm, f, envir = env); got <- TRUE; break }
    }
    testthat::expect_true(got, info = paste("could not lift", nm, "out of app.R"))
  }
  get(name, envir = env)
}
.ui_block <- .src_block   # brace-balanced, in helper.R
# The stylesheet, as text. It used to be three tags$style(HTML(...)) blocks inside
# app.R; it is now www/app.css, served off disk by Shiny (no CDN, so air-gapping
# is untouched). The rules below are about the CSS wherever it lives, so they read
# it from its own file -- and would have gone quietly, vacuously green if they had
# kept scanning app.R after the move.
.css_src <- function() {
  p <- file.path(engine_root(), "www", "app.css")
  skip_if_not(file.exists(p))
  readLines(p, warn = FALSE)
}

# ---------------------------------------------------------------------------
# The design tokens. There used to be TWO :root blocks; the first was shadowed by
# the second, so a maintainer editing a colour there saw nothing change on screen.
# Helpers some of the tests below share.
.nonascii_symbols <- function(path) {
  types <- c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS", "SYMBOL_SUB",
             "SYMBOL_PACKAGE", "SLOT")
  ex <- tryCatch(parse(path, keep.source = TRUE), error = function(e) e)
  # In a C locale the parser REFUSES the file outright, which IS the deployment
  # failure -- report it as one rather than skipping the file it happens in.
  if (inherits(ex, "error"))
    return(sprintf("%s: will not parse (%s)", basename(path), conditionMessage(ex)))
  pd <- utils::getParseData(ex)
  if (is.null(pd) || !nrow(pd)) return(character(0))
  tok <- pd[pd$terminal & pd$token %in% types, , drop = FALSE]
  bad <- tok[grepl("[^ -~\t]", tok$text, useBytes = TRUE), , drop = FALSE]
  if (!nrow(bad)) return(character(0))
  sprintf("%s:%d %s '%s'", basename(path), bad$line1, bad$token, bad$text)
}

.ui_control_ids <- function(src = .ui_src()) {
  pat <- paste0("(actionButton|actionLink|textInput|textAreaInput|numericInput|",
                "selectInput|selectizeInput|checkboxInput|checkboxGroupInput|",
                "radioButtons|fileInput|sliderInput|downloadButton|downloadLink)",
                "[(][\"][a-zA-Z0-9_]+")
  sub(".*[\"]", "", unlist(regmatches(src, gregexpr(pat, src))))
}

.ident <- function(cfg, request) {
  f <- .ui_fun("detected_identity_info",
               also = c(".ident_cfg", ".req_header_name", ".raw_header_spellings"))
  e <- environment(f)
  assign("CONFIG", list(app = cfg), envir = e)
  assign("session", list(user = NULL, request = request), envir = e)
  assign("current_user", function() "svc_statementstudio", envir = e)
  f()
}

.IDENT_ON <- list(identity_header = "X-Remote-User",
                  identity_shared_secret = "s3cret-from-the-proxy",
                  identity_secret_header = "X-Statement-Studio-Secret")

.hdrs <- function(...) {
  h <- list(...)
  raw <- if (length(h))
    setNames(unlist(h), tolower(gsub("^HTTP_", "", gsub("_", "-", names(h)))))
    else character(0)
  c(h, list(HEADERS = raw))
}


test_that("the design tokens are declared in exactly one place", {
  joined <- paste(.css_src(), collapse = "\n")
  expect_equal(length(gregexpr(":root\\{", joined)[[1]]), 1L)
  # ...and the surviving block still declares every token the CSS consumes, or a
  # surface loses its colour. --panel was the one only the deleted block had.
  root <- sub("(?s).*:root\\{(.*?)\\}.*", "\\1", joined, perl = TRUE)
  used <- unique(gsub(".*var\\((--[a-z0-9-]+)\\).*", "\\1",
                      unlist(regmatches(joined, gregexpr("var\\(--[a-z0-9-]+\\)", joined)))))
  expect_identical(sort(setdiff(used, unlist(regmatches(root, gregexpr("--[a-z0-9-]+", root))))),
                   character(0))
})

# The stylesheet is a FILE now, and a file can fail to travel. If www/ is missing
# from an install every screen renders as bare Bootstrap -- so app.R must link it,
# must not have quietly grown a second copy inline, and must say so out loud at
# startup rather than let a user conclude the tool is broken.
test_that("the design system is one linked stylesheet, and its absence is announced", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, 'tags\\$link\\(rel = "stylesheet"')
  expect_match(joined, 'href = sprintf\\("app.css\\?v=%s", engine_version\\(\\)\\)')
  # no CSS left behind in app.R, and no second stylesheet to drift from this one
  expect_false(grepl("tags$style(", joined, fixed = TRUE))
  expect_length(grep("app\\.css", list.files(file.path(engine_root(), "www")), value = TRUE), 1L)
  # a missing stylesheet is loud, not a mystery
  expect_match(joined, "STYLESHEET NOT FOUND", fixed = TRUE)
  expect_match(joined, 'APP_CSS <- file.path\\("www", "app.css"\\)')
})

# THE TEST ABOVE FORBIDS STYLE *TAGS* AND PROVED NOTHING ABOUT COLOUR. It passed
# while app.R carried 41 distinct hex values across 99 uses, several of them
# near-misses of the token they meant (#b00020 for --bad:#b3261e, #137333 for
# --ok:#0f7a37, #c77700 for --warn:#b7791f). Nothing looked wrong on screen --
# which is why it would never have corrected itself.
#
# It cannot simply be "no hex in app.R": the X-ray draws with base-R graphics,
# which cannot read a CSS variable, so those colours HAVE to exist as R values.
# The rule that is actually true is that a colour with a token is spelled the same
# in both languages, so this asserts the two lists agree, value for value.
test_that("the R palette and the stylesheet's tokens are the same colours", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # Take the LINE, not a regex over the whole file: `.` does not cross newlines,
  # so a whole-file sub() silently matched nothing and eval'd all of app.R.
  i <- grep("^PALETTE <- list\\(", src)
  expect_length(i, 1L)
  pal <- eval(parse(text = src[i]))
  css <- paste(readLines(file.path(engine_root(), "www", "app.css"), warn = FALSE),
               collapse = "\n")
  token <- function(nm) {
    m <- regmatches(css, regexpr(sprintf("--%s:#[0-9a-fA-F]{6}", nm), css))
    expect_length(m, 1L)                       # the token must exist to be matched
    sub(sprintf("--%s:", nm), "", m)
  }
  for (nm in names(pal))
    expect_identical(tolower(pal[[nm]]), tolower(token(nm)),
                     info = sprintf("PALETTE$%s and --%s disagree", nm, nm))

  # and the near-misses may not come back. Only the three that were WRONG are
  # forbidden: #a15c00 was never a near-miss of anything, it is the real value of
  # --meta, so it legitimately appears once -- in the PALETTE line this test just
  # read. Forbidding it too would have meant forbidding the fix.
  code <- src[!grepl("^\\s*#", src)]
  code <- code[-grep("^PALETTE <- list\\(", code)]
  for (gone in c("#b00020", "#137333", "#c77700"))
    expect_false(any(grepl(gone, code, fixed = TRUE)),
                 info = sprintf("%s is back in app.R; use PALETTE / var(--token)", gone))
  # ...and the rules really did all arrive: the classes the screen names must exist
  css <- paste(.css_src(), collapse = "\n")
  for (cls in c("\\.verdict-high", "\\.verdict-medium", "\\.verdict-low", "\\.stat-grid",
                "\\.dl-hero", "\\.chip-warn", "\\.hub-card-go", "\\.app-header",
                "table\\.plan-table", "#ss-busy", "body\\.ss-run", "\\.split-table"))
    expect_match(css, cls, info = cls)
  # the deployment box runs a C locale: a byte the browser has to guess at in a
  # file served as text/css is a needless way to lose a rule.
  expect_false(any(grepl("[^\x01-\x7f]", css, useBytes = TRUE)))
})

# ---------------------------------------------------------------------------
# One visual language for "how did it go". The success headline and the
# didn't-go-well card used to be two separately hand-coloured boxes.
test_that("every result verdict uses the shared verdict card", {
  src <- .ui_src()
  # 45, not 30: the block now also carries the note recording why the tie headline
  # is gated on `unsupported`. The window only has to reach the end of cv_status.
  block <- .ui_block(src, "output\\$cv_status <- renderUI", 58L)
  expect_match(block, 'paste0\\("verdict verdict-", lvl\\)')
  expect_match(block, 'class = "verdict-title"')
  # no second, hand-rolled palette
  expect_false(grepl('pal\\[\\["bg"\\]\\]', block))
})

# ---------------------------------------------------------------------------
# When the tool asks for a second pair of eyes, the evidence must not be one
# collapsed click behind the chart.
test_that("Checks & detail opens itself whenever something was flagged", {
  src <- .ui_src()
  block <- .ui_block(src, "output\\$cv_detail <- renderUI", 20L)
  expect_match(block, 'any\\(k\\$status %in% "fail"\\)')
  expect_match(block, "open = if \\(open_it\\) NA else NULL")
  # a clean pass still starts tidy
  expect_match(block, 'open_it <- !isTRUE\\(\\(res\\$status %\\|\\|% ""\\) == "ok"\\) \\|\\| isTRUE\\(any_failed\\)')
})

# ---------------------------------------------------------------------------
# Admin: the common case -- "this bank writes a word we've never met" -- is now a
# plain-English control, and it writes through the SAME engine function the
# Approve button uses, so there is one way a word gets into the vocabulary.
test_that("teaching the engine a word does not need YAML, and has one write path", {
  src <- .ui_src()
  block <- .ui_block(src, "observeEvent\\(input\\$adm_word_add", 44L)
  expect_match(block, "req\\(admin_ok\\(\\)\\)")            # privileged, like every other
  expect_match(block, "lexicon_append\\(kind, tolower\\(w\\), LEXICON_PATH\\)")
  expect_false(grepl("writeLines", block, fixed = TRUE))    # no second writer
  # ONE FORM FOR BOTH FILES. There were three teach-a-word forms on this screen --
  # a wording for a labelled value, a word for the recognition vocabulary, and the
  # vocabulary form again pre-filled from the harvested list. They are one form
  # now: the word is typed once, and "What it means" decides which file is
  # written, because which file a wording lives in is a fact about the wording and
  # not a question the person typing it can answer.
  expect_match(block, "dictionary_append\\(fld, w, path = DICT_PATH\\)")
  expect_match(block, 'startsWith\\(sel, "dict:"\\)')
  joined <- paste(src, collapse = "\n")
  for (gone in c("adm_dict_field", "adm_dict_phrase", "adm_dict_add",
                 "adm_sugg_tok", "adm_sugg_dir", "adm_sugg_approve"))
    expect_false(grepl(sprintf('"%s"', gone), joined, fixed = TRUE), info = gone)
  # the harvested list hands its word to that one form rather than spelling it out
  # a second time -- and nothing is written until Teach it is pressed
  expect_match(joined, "input\\$adm_sugg_tokens_rows_selected")
  expect_match(.ui_block(src, "observeEvent\\(input\\$adm_sugg_tokens_rows_selected", 10L),
               'updateTextInput\\(session, "adm_word_text"')
  # the whole-file editor is kept, not deleted -- just no longer the front door
  expect_match(joined, 'textAreaInput\\("adm_lex_edit"')
  expect_match(joined, 'textAreaInput\\("adm_dict_edit"')
  expect_match(joined, "Edit the whole vocabulary file", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# NOBODY who uses this app has the Admin password. So Admin must never appear in
# the wording a user reads: not as a card on About, not as "go to Admin ->
# Templates" in an error, not as "ask your administrator". Every one of those is
# an instruction the reader cannot follow, and it makes the tool feel like it is
# withholding something. Maintainer routes belong inside the Admin tab only.
test_that("no wording anywhere tells a user to go to Admin", {
  # Scanned across every file whose text can reach a screen -- the app, the label
  # maps, and the engine's own messages and how-to-fix lines, which are printed
  # verbatim on the Convert page. Console messages (message()/warning()) are for
  # whoever starts the server, not for a user, so they are excluded.
  files <- c(file.path(engine_root(), c("app.R", "ui_labels.R", "ui_content.R")),
             list.files(file.path(engine_root(), "R"), "[.]R$", full.names = TRUE))
  files <- files[file.exists(files)]
  # Instructions, not the bare word: "admin" appears legitimately in code and in
  # comments. What must never reach a user is a direction to a place they cannot
  # open, or a person they do not have.
  banned <- c("Admin ->", "Admin \u2192", "Admin tab", "Open Admin",
              "your administrator", "the administrator", "ask an admin", "Admin >")
  offenders <- character(0)
  for (f in files) {
    # useBytes throughout: this deploys in a C locale, where a plain grep over a
    # source line containing a dash or a block glyph raises "unable to translate".
    # The patterns are ASCII (bar the arrow, which is compared as its UTF-8 bytes),
    # so byte-wise matching is identical and simply survives those lines.
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines, useBytes = TRUE)]   # comments are for maintainers
    lines <- lines[!grepl("message\\(|warning\\(|stop\\(", lines, useBytes = TRUE)]
    for (b in banned) {
      hit <- grep(b, lines, fixed = TRUE, value = TRUE, useBytes = TRUE)
      if (length(hit)) offenders <- c(offenders, sprintf("%s: %s", basename(f), trimws(hit)))
    }
  }
  expect_identical(offenders, character(0),
                   info = paste("user-visible text points at Admin:",
                                paste(offenders, collapse = " | ")))
})

test_that("the About page does not advertise Admin", {
  joined <- paste(.ui_src(), collapse = "\n")
  expect_false(grepl("Open Admin", joined, fixed = TRUE))
  expect_false(grepl("ab_go_admin", joined, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Never tell the screen what the tool cannot do. Copy that explains WHY an input
# is needed ("there is no sign-in on this server") is a description of a weakness,
# shown to everyone who opens the page - including anyone who should not be on it.
# The QID field says what it is FOR; where the limitation is written down is the
# maintainer's documentation.
test_that("no screen text advertises the absence of a sign-in", {
  files <- c(file.path(engine_root(), c("app.R", "ui_labels.R", "ui_content.R")),
             list.files(file.path(engine_root(), "R"), "[.]R$", full.names = TRUE))
  files <- files[file.exists(files)]
  banned <- c("no sign-in", "no sign in", "There is no sign",
              "not secured", "no password on", "anyone can access")
  offenders <- character(0)
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines, useBytes = TRUE)]   # comments are for maintainers
    for (b in banned) {
      hit <- grep(b, lines, fixed = TRUE, value = TRUE, useBytes = TRUE)
      if (length(hit)) offenders <- c(offenders, sprintf("%s: %s", basename(f), trimws(hit)))
    }
  }
  expect_identical(offenders, character(0),
                   info = paste("screen text describes a security weakness:",
                                paste(offenders, collapse = " | ")))
})

# ---------------------------------------------------------------------------
# THE PROOF IS THE PRODUCT. Only failing checks were shown, which reads as "no
# news is good news" - wrong for a tool whose output has to be defensible. The
# reason to trust a conversion is that the opening balance plus every transaction
# equals the closing balance the statement prints, and a PASSING proof said
# nothing at all: it sat in a panel nobody opens on a clean run.
test_that("the checks that matter are on screen whether they pass or fail", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "output\\$cv_proof <- renderUI", fixed = FALSE)
  # the cardinal proof first, then completeness, continuity, dates
  i <- grep("\\.PROOF_CHECKS <- c\\(", src)
  expect_length(i, 1L)
  blk <- paste(src[i:(i + 2)], collapse = " ")
  for (nm in c("balance_reconciliation", "no_unparsed_rows",
               "running_balance_continuity", "dates_readable"))
    expect_match(blk, nm, fixed = TRUE)
  # ...and it is rendered in the DEFAULT view, not behind the disclosure
  i_proof <- grep('uiOutput\\("cv_proof"\\)', src)
  i_more  <- grep('uiOutput\\("cv_more_toggle"\\)', src)
  expect_length(i_proof, 1L)
  expect_true(i_proof < min(i_more))
})

test_that("every check named in the proof strip has plain-English wording", {
  # plain_check() falls back to the raw code, and a forensic reviewer who sees
  # "no_unparsed_rows" on screen stops trusting the screen.
  e <- new.env(parent = globalenv())
  sys.source(file.path(engine_root(), "ui_labels.R"), envir = e)
  for (nm in c("balance_reconciliation", "no_unparsed_rows",
               "running_balance_continuity", "dates_readable"))
    expect_true(nm %in% names(e$CHECK_PLAIN), info = paste("no plain wording for", nm))
})

# Base R takes #rrggbb or #rrggbbaa and raises "invalid RGB specification" on the
# three-digit CSS form. One of those sat in the shared area_line() helper, so the
# two line charts rendered nothing but that error message on every statement that
# had the column to draw them from.
test_that("no chart colour is three-digit hex", {
  src <- .ui_src()
  hits <- grep('"#[0-9a-fA-F]{3}"', src, perl = TRUE, value = TRUE)
  hits <- hits[!grepl("^\\s*#", hits)]          # comments explain the rule
  expect_identical(hits, character(0))
  # WHY THIS NO LONGER ASSERTS AN ERROR.
  #
  # `col2rgb("#666")` WAS an error on the R this shipped on, which is how the
  # "this page could not be drawn" fallback managed to crash on its own colour.
  # Newer R accepts three-digit hex, so asserting the error pins a fact about
  # whichever R happens to be installed rather than about this code -- and it
  # failed on every run here, which is exactly the kind of permanently red line
  # that teaches people to read past red.
  #
  # The RULE stands either way, and it is the line above that enforces it: the
  # app is read by whatever R the offline bundle installs on the day, and
  # six-digit is the form every version of R has always accepted. Assert the
  # form we use, not the failure of the form we do not.
  expect_silent(grDevices::col2rgb("#ffffff"))
  # ...and the palette the app really hands grDevices is six-digit, every entry.
  pal_line <- grep("^PALETTE <- list\\(", src, value = TRUE)[1]
  expect_false(is.na(pal_line))
  pal <- eval(parse(text = pal_line))
  expect_true(all(grepl("^#[0-9a-fA-F]{6}$", unlist(pal))))
  for (col in unlist(pal)) expect_silent(grDevices::col2rgb(col))
})

test_that("the proof strip carries the check that catches the commonest error", {
  # Amounts and the debit/credit mapping are what actually go wrong; that check
  # used to surface only on failure, which is the wrong way round for the error
  # a reviewer most wants to see confirmed.
  src <- .ui_src()
  i <- grep("\\.PROOF_CHECKS <- c\\(", src)
  blk <- paste(src[i:(i + 2)], collapse = " ")
  expect_match(blk, "amount_direction", fixed = TRUE)
  e <- new.env(parent = globalenv())
  sys.source(file.path(engine_root(), "ui_labels.R"), envir = e)
  expect_true("amount_direction" %in% names(e$CHECK_PLAIN))
})

# ---------------------------------------------------------------------------
# A CASE FOLDER IS THE SAME SCREEN, NOT A SECOND ONE.
#
# Ten to fifty statements arrive as one case. The temptation is a Batch tab with
# its own picker, its own Convert button, its own "who ran this" and its own,
# thinner result view - four copies of things that already exist and four places
# for the two answers to drift apart. It is the same question asked of more
# files, so it is the same control: one picker that takes several, and a row per
# file that OPENS the ordinary result page.
test_that("a batch is more files in the same picker, not a second screen", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  # ONE picker, and it takes several files
  i_file <- grep('fileInput\\("cv_file"', src)
  expect_length(i_file, 1L)
  expect_match(paste(src[i_file:(i_file + 3)], collapse = " "), "multiple = TRUE", fixed = TRUE)
  # ONE Convert button, on the Convert tab -- no separate batch tab or trigger.
  # It is rendered server-side (one uiOutput, two branches: on, and off with the
  # reason under it), so the UI carries exactly one PLACE for it.
  expect_length(grep('uiOutput\\("cv_go_btn"\\)', src), 1L)
  expect_length(grep('output\\$cv_go_btn <- renderUI', src), 1L)
  expect_false(grepl('tabPanel\\("Batch"', joined))
  # the case table is rendered on Convert, above the result it opens -- and it is
  # the Convert table itself, not a second one (see "ONE TABLE, BEFORE AND AFTER")
  i_batch <- grep('uiOutput\\("cv_plan"\\)', src)
  i_status <- grep('uiOutput\\("cv_status"\\)', src)
  expect_length(i_batch, 1L)
  expect_true(i_batch < min(i_status))
})

test_that("the case table puts what went wrong first, by meaning not by spelling", {
  # Every file that failed the same way must gather together so they can be fixed
  # together. Alphabetical order on the verdict would scatter them ("Could not
  # read" before "No template"), so the rows are ordered off the engine's own
  # worst-last status order, with the failure kind as the tie-break.
  #
  # The table used to be a DataTable with a hidden sort key, column indexes and a
  # click direction to get right; it is now the Convert table carrying its results,
  # and the order is simply the order the rows are drawn in. Asserted as an ORDER.
  src <- .ui_src()
  blk <- .src_block(src, "output\\$cv_plan <- renderUI", 120L)
  expect_match(blk, "sev <- match\\(b\\$status, BATCH_STATUSES, nomatch = length\\(BATCH_STATUSES\\) \\+ 1L\\)")
  expect_match(blk, "ord <- order\\(-sev, as\\.character\\(b\\$failing_check\\), ord\\)")
  expect_match(blk, "trs <- lapply\\(ord, function\\(i\\)")
  expect_false(grepl("length(BATCH_STATUSES) + 1L -", blk, fixed = TRUE))
  # the engine's order really is worst-last, which -sev re-reads
  expect_identical(BATCH_STATUSES, c("ok", "needs_review", "unsupported", "failed"))
  # ...and the key really does put the worst first, with an unrecognised verdict at
  # the very top (nobody has words for it yet)
  st <- c("ok", "failed", "needs_review", "something_new", "unsupported")
  key <- match(st, BATCH_STATUSES, nomatch = length(BATCH_STATUSES) + 1L)
  expect_identical(st[order(-key)],
                   c("something_new", "failed", "unsupported", "needs_review", "ok"))
})

test_that("a long batch shows progress per file", {
  # A 50-file case that looks frozen is a case the analyst kills half way.
  #
  # REWRITTEN for the move off this process. The bar cannot be driven from inside
  # the loop any more -- the loop is in another process -- so the child writes one
  # line per file and the poll reads it. Same promise, two halves, and both are
  # asserted because either alone leaves the analyst watching a blank bar.
  jobs <- paste(readLines(file.path(engine_root(), "R", "jobs.R"), warn = FALSE), collapse = "\n")
  expect_match(jobs, "progress = \\.job_progress_writer\\(jobdir\\)")
  expect_match(jobs, 'sprintf\\("%d/%d %s\\\\n", i, n, basename\\(f\\)\\)')  # WHICH file
  blk <- .ui_block(.ui_src(), "job_say <- function", 20L)
  expect_match(blk, "job_progress\\(h\\)")
  expect_match(blk, '"%d of %d - %s", p\\$i, p\\$n, p\\$file', fixed = FALSE)
  # ...and it really does come back as a number and a name, not a blob of text
  expect_true(all(c("i", "n", "file") %in% names(formals(function(i, n, file) NULL))))
})

test_that("nobody waits in silence: a queued conversion says how many are ahead", {
  # THE POINT OF THE QUEUE. Capping concurrency without saying so would replace a
  # frozen page with a slow one and tell the analyst nothing either way.
  src <- .ui_src()
  blk <- .ui_block(src, "job_say <- function", 20L)
  expect_match(blk, 'identical\\(st, "queued"\\)')
  expect_match(blk, "job_queue_ahead\\(h\\)")
  expect_match(blk, "ahead of yours", fixed = TRUE)
  # it is spoken through the SAME progress panel the centred overlay follows, so
  # there is no second, quieter place for a wait to hide
  expect_match(paste(src, collapse = "\n"), "Progress\\$new\\(session\\)")
  # ...and the WORDING carries no engine word. Read off the real string literals
  # (the parser's own STR_CONST tokens), not a regex over the source: a naive
  # quote-pairing scan matches across the code between two unrelated strings and
  # reports words that are only ever in identifiers.
  f <- .ui_fun("job_say")
  pd <- utils::getParseData(parse(text = paste(deparse(body(f)), collapse = "\n"),
                                  keep.source = TRUE))
  lits <- pd$text[pd$terminal & pd$token == "STR_CONST"]
  expect_gt(length(lits), 2L)                       # the scan must not go quiet
  said <- lits[grepl("[ ]", lits)]                  # the sentences, not the state names
  expect_gt(length(said), 1L)
  for (w in c("job", "slot", "pid", "process", "queue"))
    expect_identical(grep(w, said, value = TRUE, ignore.case = TRUE), character(0),
                     info = paste("the waiting message says", w))
})

test_that("a tab closed mid-conversion takes its process with it", {
  # An abandoned scan would otherwise hold a core for two more minutes producing
  # a result no browser is left to read -- and the cap would count it the while.
  blk <- .ui_block(.ui_src(), "session\\$onSessionEnded\\(function", 8L)
  expect_match(blk, "cv_slot\\$cancel\\(\\)")
  expect_match(blk, "adm_slot\\$cancel\\(\\)")
  # the jobs go BEFORE the scratch folder: a child is still writing into it
  i_job <- regexpr("cv_slot\\$cancel", blk)
  i_dir <- regexpr("unlink\\(d", blk)
  expect_true(i_job > 0 && i_dir > 0 && i_job < i_dir)
  # ...and the whole app taking a child with it, for a server that is restarted
  expect_match(paste(.ui_src(), collapse = "\n"), "onStop\\(function\\(\\) safe\\(job_reap_all\\(\\)\\)\\)")
})

test_that("a conversion that cannot even be started does not take the page down with it", {
  # job_start() writes the job's folder and its arguments to disk BEFORE anything
  # runs, so a full disk or a TEMP the service account cannot write makes it
  # throw. Thrown from inside this observer that ends the whole Shiny session --
  # the analyst's page greys out mid-click and takes the result she was reading
  # with it, and on a box with no room left it does that to everybody, every time.
  blk <- .ui_block(.ui_src(), "slot\\$start <- function", 22L)
  expect_match(blk, "tryCatch\\(do\\.call\\(job_start")
  expect_match(blk, 'inherits\\(h, "condition"\\)')
  # the cause goes to the maintainer's log...
  expect_match(blk, "errors\\.log")
  # ...and the analyst gets the sentence for a conversion that did not come back,
  # never one about her file
  expect_match(blk, "CONVERT_STOPPED")
  expect_false(grepl("FRIENDLY_READ_ERROR", blk, fixed = TRUE))
  # and the app carries on: no Progress bar left open, no handle registered
  expect_match(blk, "return\\(invisible\\(NULL\\)\\)")
})

test_that("the cap is a deployment setting, read once, and named in the example config", {
  expect_match(paste(.ui_src(), collapse = "\n"),
               "job_set_max_concurrent\\(CONFIG\\$app\\$max_concurrent_jobs\\)")
  ex <- file.path(engine_root(), "config", "config.example.yaml")
  skip_if_not(file.exists(ex))
  txt <- paste(readLines(ex, warn = FALSE), collapse = "\n")
  expect_match(txt, "max_concurrent_jobs", fixed = TRUE)
  # the example must say what the number BUYS and what it COSTS, or it is a
  # number nobody can set responsibly
  expect_match(txt, "AT THE SAME TIME", fixed = TRUE)   # what it buys
  expect_match(txt, "TRADE-OFF", fixed = TRUE)          # ...and what it costs
  expect_match(txt, "queue", fixed = TRUE)
})

test_that("a batch file is audited, captured and published exactly like a single one", {
  # The one thing a batch must never do is mean something different from
  # "convert one, thirty times".
  blk <- .ui_block(.ui_src(), "run_batch <- function", 70L)
  for (step in c("stamp_identity\\(", "record_upload\\(", "publish_result\\(res, TRUE\\)"))
    expect_match(blk, step)
  # ...and it is still the ONE feed writer (test-seams pins that too)
  expect_length(grep("safe\\(write_feed\\(", .ui_src()), 1L)
})

test_that("the first-visit empty state does not sit under a finished batch", {
  src <- .ui_src()
  i <- grep('uiOutput\\("cv_empty"\\)', src)
  expect_gt(length(i), 0L)
  blk <- paste(src[(min(i) - 2):min(i)], collapse = " ")
  expect_match(blk, "output.cv_has_batch != true", fixed = TRUE)
})

test_that("converting a single file clears a stale batch table", {
  # run_conversion reclaims the previous scratch folder, which is where every
  # batch file's workbook lives. A table whose downloads no longer resolve is
  # worse than no table.
  blk <- .ui_block(.ui_src(), "run_conversion <- function", 34L)
  expect_match(blk, "cv_batch\\(NULL\\)")
})

test_that("two uploads with the same name cannot overwrite each other's outputs", {
  # Outputs are named after the file they came from, so "statement.pdf" twice
  # would leave both rows pointing at one workbook - the second silently on top
  # of the first. The suffix is visible, so the clash is stated, not quiet.
  uniq <- .ui_fun(".unique_names")
  expect_identical(uniq(c("a.pdf", "b.pdf")), c("a.pdf", "b.pdf"))
  expect_identical(uniq(c("a.pdf", "a.pdf", "a.pdf")), c("a.pdf", "a (2).pdf", "a (3).pdf"))
  expect_identical(uniq(c("a.pdf", "a (2).pdf", "a.pdf")), c("a.pdf", "a (2).pdf", "a (3).pdf"))
  expect_identical(uniq("noext"), "noext")
})

# ---------------------------------------------------------------------------
test_that("the Convert sidebar no longer carries the uploads-retention line", {
  src <- .ui_src()
  i_side <- grep('fileInput\\("cv_file"', src)[1]
  i_main <- grep("mainPanel\\(", src)[1]
  sidebar <- paste(src[i_side:i_main], collapse = " ")
  expect_false(grepl("UPLOADS_NOTE", sidebar, fixed = TRUE))
  # ...and it is still shown where retention is actually managed
  expect_gt(length(grep("UPLOADS_NOTE", src, fixed = TRUE)), 0L)
})

# ---------------------------------------------------------------------------
# The About page is read by everyone who opens the tool. It must not describe a
# system none of them can see, use or act on.
test_that("the About page says nothing about the dashboards or the feed", {
  p <- file.path(engine_root(), "ui_content.R")
  skip_if_not(file.exists(p))
  txt <- tolower(paste(readLines(p, warn = FALSE), collapse = "\n"))
  for (w in c("dashboard", "qlik", "the feed", "feeds the"))
    expect_false(grepl(w, txt, fixed = TRUE), info = paste("About still mentions:", w))
  # the hub cards and lead on the About tab likewise
  src <- .ui_src()
  i0 <- grep('tabPanel\\("About", br\\(\\),', src)[1]
  i1 <- grep('"Convert",\\s*$', src)[1]
  expect_false(is.na(i0))
  about <- tolower(paste(src[i0:min(i1, length(src))], collapse = " "))
  for (w in c("dashboard", "qlik"))
    expect_false(grepl(w, about, fixed = TRUE), info = paste("About tab still mentions:", w))
})

# ---------------------------------------------------------------------------
# JSON is produced and nobody downloads it; a third equal button made two real
# choices look like three. Demoted, not removed - the person who wants it has no
# other route to the file.
test_that("JSON is a link, not a third download button, and still works", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  blk <- .ui_block(src, "dl_buttons <- function", 12L)
  expect_match(blk, 'labs <- c\\(xlsx = .*csv = ')
  expect_false(grepl("json =", blk))                       # not among the buttons
  expect_match(joined, 'downloadLink\\("dl_json"')         # still reachable
  expect_match(joined, 'output\\$dl_json <- mk_dl\\("json"\\)')   # still produced
  # the buttons bar is only asked for the two formats it now offers
  expect_match(joined, 'dl_buttons\\(res\\$outputs, c\\(xlsx = "dl_xlsx", csv = "dl_csv"\\)\\)')
})

# ---------------------------------------------------------------------------
# SEVERAL PERIODS IN ONE FILE. The engine reads them as ONE span, which is what
# lets the date and balance checks pass - but a span standing in for three
# quarters reads as one quarter unless the count is said beside it.
test_that("the number of statement periods is shown where the period is shown", {
  src <- .ui_src()
  # The card reads the ranges off .period_lines(), which is where the count now
  # lives too -- on the range it came FROM (the statement's printed period, which
  # is what the engine merged), not on whichever range happened to be shown.
  expect_match(.ui_block(src, "output\\$cv_summary <- renderUI", 12L),
               "\\.period_lines\\(")
  blk <- .ui_block(src, "\\.period_lines <- function", 40L)
  expect_match(blk, "n_periods", fixed = TRUE)
  expect_match(blk, "%d periods", fixed = TRUE)
  expect_match(blk, "isTRUE\\(np > 1L\\)")      # only when there really is more than one
  f <- .ui_fun(".period_lines")
  got <- f(as.Date(c("2025-01-05", "2025-03-20")), "2025-01-01", "2025-03-31", 3L)
  expect_true(any(grepl("3 periods", got, fixed = TRUE)))
  expect_true(any(grepl("^Statement period: ", got)))
  # one period says nothing extra
  expect_false(any(grepl("periods", f(as.Date("2025-01-05"), "2025-01-01", "2025-01-31", 1L),
                         fixed = TRUE)))
})

test_that("anything the engine had to say about a merged span is said on screen", {
  # A span with a hole in it looks exactly like a whole one.
  blk <- .ui_block(.ui_src(), "output\\$cv_summary <- renderUI", 45L)
  expect_match(blk, "period_note", fixed = TRUE)
  # the engine really does produce it, so this is wired to a fact
  expect_true(any(grepl("period_note",
    readLines(file.path(engine_root(), "R", "extract_metadata.R"), warn = FALSE),
    fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# THE TRANSACTIONS TABLE MUST SURVIVE A COLUMN THE LABEL MAP HAS NEVER MET.
# The preview relabels the stored column names for a human reader and falls back
# to a title-cased version of the raw name for anything unmapped. That fallback
# was written with `[[`, which on a NAMED CHARACTER VECTOR throws rather than
# returning NULL - so a template's `extras` column, or the statement_index an
# auto-split bundle stamps on every row, took the whole table down and the page's
# whole payoff rendered as nothing. Found by converting a bundle in the browser.
test_that("an unmapped column is titled, not fatal, in the transactions preview", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(engine_root(), "ui_labels.R"), envir = e)
  expect_identical(e$cv_friendly_cols(c("date", "amount")), c("Date", "Amount"))
  # the ones that actually blew up
  expect_identical(e$cv_friendly_cols("fx_amount"), "Fx Amount")
  expect_identical(e$cv_friendly_cols("conversion_charge"), "Conversion Charge")
  # a mixture, in order, is what the table really passes
  expect_identical(e$cv_friendly_cols(c("date", "made_up_column", "amount")),
                   c("Date", "Made Up Column", "Amount"))
  expect_identical(e$cv_friendly_cols(character(0)), character(0))
  # ...and the split marker now has a name of its own rather than a fallback
  expect_identical(e$cv_friendly_cols("statement_index"), "Statement #")
})

# ---------------------------------------------------------------------------
# N29 (the editor half) - AND THE THING THAT ACTUALLY BROKE IT.
#
# `uiOutput("g_more_toggle")` was written TWICE in the toolkit modal. A second
# output with an id already bound makes Shiny throw "Duplicate binding for ID"
# in the browser, and that exception ABORTS the whole bind pass for the inserted
# scope - so on a PDF, not one statically-drawn control in the toolkit reached
# the server: not the bank name, the date format, the amount style, the page
# number, Assign it or Remove it. Boxes drawn did nothing at all and the bands
# stayed at their drafted defaults, which is precisely the reported "the boxes I
# drew came back, and the credit column beside the debit one reads nothing".
# It is invisible from R - the app starts, the suite is green, the page renders -
# so the rule is held here, over every output the file draws.
test_that("no output id is drawn twice, because a duplicate unbinds the screen", {
  src <- .ui_src()
  pat <- paste0("(uiOutput|plotOutput|DTOutput|dataTableOutput|tableOutput|textOutput",
                "|verbatimTextOutput|imageOutput|downloadButton|downloadLink)\\(\\s*\"([A-Za-z0-9_]+)\"")
  ids <- unlist(regmatches(src, gregexpr(pat, src, perl = TRUE)))
  ids <- sub("\"$", "", sub("^[^\"]*\"", "", ids))
  expect_true(length(ids) > 50)                       # the scan really found them
  expect_identical(names(which(table(ids) > 1)), character(0))
})

test_that("no INPUT id is drawn twice either", {
  # The output test above was written for the duplicate that unbound the toolkit,
  # and it does not reach inputs -- where the failure is quieter but just as real.
  # Two actionButtons sharing an id keep INDEPENDENT click counters, so one of them
  # sends a value identical to the current one and its observeEvent never fires: a
  # dead button, invisible from R and green in the suite. Found live on
  # cv_teach_go (three elements, two of which render together on a tied-and-
  # unsupported result) and cv_rematch_go (two).
  #
  # Ids that are DELIBERATELY re-used across mutually exclusive screens would go in
  # `allow` with the reason. There are none today, and that is the point: the
  # exception has to be argued in writing rather than happen by accident.
  src <- .ui_src()
  allow <- character(0)
  # `.eff_picker` is in the list because it IS an input constructor: the validity
  # window's two date boxes are built through it, so leaving it out would take two
  # ids off the scan without anyone noticing.
  pat <- paste0("(actionButton|actionLink|textInput|textAreaInput|numericInput|dateInput",
                "|selectInput|selectizeInput|checkboxInput|checkboxGroupInput|radioButtons",
                "|sliderInput|fileInput|passwordInput|\\.eff_picker)\\(\\s*\"([A-Za-z0-9_]+)\"")
  ids <- unlist(regmatches(src, gregexpr(pat, src, perl = TRUE)))
  ids <- sub("\"$", "", sub("^[^\"]*\"", "", ids))
  ids <- setdiff(ids, allow)
  expect_true(length(ids) > 50)
  expect_identical(names(which(table(ids) > 1)), character(0))
})

# ---------------------------------------------------------------------------
# CLAIMS THE CODE CANNOT KEEP. Each of these was measured false by driving the
# app, and each is the kind that reads as reassurance rather than as a fact -
# which is exactly why nobody checked it.
test_that("the screen makes no promise the engine does not keep", {
  files <- c(file.path(engine_root(), c("app.R", "ui_labels.R", "ui_content.R")))
  lines <- unlist(lapply(files, function(f) {
    l <- readLines(f, warn = FALSE)
    l[!grepl("^\\s*#", l, useBytes = TRUE)]                 # comments record WHY
  }))
  banned <- c(
    # a template drafted from a real bank PDF can read 0 rows, with no path on
    "2-minute", "2 minutes", "two minutes", "couple of minutes",
    # convert_document only tries form templates after the statement path fails
    "it's detected automatically",
    # passing the checks clears the first feed gate, not the second
    "and it goes through",
    # reconciliation returns "na" on any statement with no balance anchor
    "Proof nothing's missing",
    # R/reconcile.R derives a missing closing from the running-balance column
    "closing balance the statement prints",
    # completeness_verified is FALSE for a missing OPENING balance too
    "This statement prints no closing balance",
    # R/convert.R picks, always; it holds the run for review afterwards
    "the tool won't pick for you")
  offenders <- unlist(lapply(banned, function(b)
    grep(b, lines, fixed = TRUE, value = TRUE, useBytes = TRUE)))
  expect_identical(as.character(offenders %||% character(0)), character(0),
                   info = paste("unkept promise on screen:", paste(offenders, collapse = " | ")))
})

test_that("no R name anywhere carries a non-ASCII character (invariant 17)", {
  files <- c(file.path(engine_root(), c("app.R", "ui_labels.R", "ui_content.R")),
             list.files(file.path(engine_root(), "R"), "[.]R$", full.names = TRUE))
  files <- files[file.exists(files)]
  expect_gt(length(files), 20L)                       # the scan must not go quiet
  offenders <- unlist(lapply(files, .nonascii_symbols))
  expect_identical(offenders, character(0),
                   info = paste("non-ASCII in an R name:", paste(offenders, collapse = " | ")))
})

test_that("the non-ASCII scan can tell a name from a string", {
  # A guard nobody has seen fail is a guard nobody knows works. Values pass...
  ok <- tempfile(fileext = ".R")
  writeLines('x <- c("\u2713 Correct" = "correct", "\u00b7 dot" = "d")', ok)
  expect_identical(.nonascii_symbols(ok), character(0))
  # ...and a NAME does not, in either locale: the C-locale parser refuses the
  # file, a UTF-8 one parses it and the token scan catches the symbol.
  bad <- tempfile(fileext = ".R")
  writeLines('caf\u00e9 <- 1', bad)
  expect_gt(length(.nonascii_symbols(bad)), 0L)
})

# The proof strip is the first quality signal on the page and had no key at all:
# a grey dash beside "Opening + transactions = closing balance" is either the best
# or the worst news on the screen, and nothing anywhere said which.
test_that("the proof strip has a key, in the glyphs it actually draws", {
  blk <- .ui_block(.ui_src(), "output\\$cv_proof <- renderUI", 46L)
  # the escapes, not the glyphs: the key must be drawn from the SAME three
  # sequences the chips are, so the two cannot drift apart
  for (g in c("\\\\u2713", "\\\\u2717", "\\\\u2013")) expect_match(blk, g)
  expect_match(blk, "could not be checked")
})

# Every other route into a conversion is blocked without a QID, because a run
# recorded against the server's own account identifies nobody. The sample button
# ran the whole flow -- result, checks, working downloads -- without one.
test_that("every way of starting a conversion goes through the same identity gate", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "\\.identity_ok <- function")
  expect_match(.ui_block(src, "\\.identity_ok <- function", 8L),
               "\\.identity_is_personal\\(detected_identity_info\\(\\)\\) \\|\\| !is\\.na\\(cv_qid\\(\\)\\)")
  for (h in c("observeEvent\\(input\\$cv_go", "observeEvent\\(input\\$cv_try_sample"))
    expect_match(.ui_block(src, h, 12L), "if \\(!\\.identity_ok\\(\\)\\) return\\(\\)")
})

test_that("the QID box says what shape a QID is, and still not why it is asked", {
  src <- paste(.ui_src(), collapse = "\n")
  expect_match(src, "Your six-character staff ID", fixed = TRUE)
  # the shape it states is the shape the validator enforces
  expect_match(src, 'QID_PATTERN <- "\\^\\[A-Za-z0-9\\]\\{6\\}\\$"')
})

# A pre-answered "Correct" means a submit-without-reading records a positive
# rating, and that rating drives Admin's flagged list, template usage and the
# suggestion ranking -- while "wrong" retracts rows from the dashboards.
test_that("the feedback question is not answered for the reviewer", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, 'radioButtons\\("cv_fb_verdict", NULL, inline = TRUE, selected = character\\(0\\),')
  # ...and submitting without choosing is refused with a reason, not defaulted
  expect_match(.ui_block(src, "observeEvent\\(input\\$cv_fb_submit", 12L),
               "!length\\(input\\$cv_fb_verdict")
})

test_that("a toast replaces the last one about the same thing", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "notify_once <- function\\(id, text")
  expect_match(.ui_block(src, "notify_once <- function", 3L), 'id = paste0\\("n_", id\\)')
  # and a message is withdrawn when it stops being true
  expect_match(joined, 'clear_notice\\("cv_qid"\\)')
})

test_that("an Admin download that cannot work is disabled and says why, never a 500", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "dl_when <- function\\(id, label, ready, why\\)")
  # the REAL control, disabled - not a lookalike <button>: Shiny only registers a
  # download while something is bound to it, so swapping the control out makes the
  # URL answer 404, which is another error page for anyone holding an old link
  expect_match(.ui_block(src, "dl_when <- function", 8L), 'class = "disabled"')
  expect_match(paste(readLines(file.path(engine_root(), "www", "app.css"), warn = FALSE),
                     collapse = "\n"), "a\\.btn\\.disabled")
  # WAS: "adm_audit_dl" was in this list. The "Single statement - safe summary"
  # picker it belonged to is gone (register 1b -- it was a third route to an export
  # the bulk picker above it and every saved upload already offer). WAS ALSO:
  # "adm_ba_csv", the bulk audit's "Converted report (.csv)" -- it only enabled on
  # the "Also convert & save" pass, and that tick went when this panel stopped
  # converting, so it was a download that could never be pressed beside a sentence
  # naming a control that no longer existed. The rule this test protects is
  # unchanged for the three that remain.
  for (id in c("adm_ba_report", "adm_up_audit", "adm_inbox_audit"))
    expect_match(joined, sprintf('dl_when\\("%s"', id))
  expect_false(grepl('dl_when("adm_ba_csv"', joined, fixed = TRUE))
  # req(FALSE) in a download handler is what Shiny answers as an HTTP 500 page.
  # Comments are stripped first: the fixes' own notes name the thing they removed,
  # and a raw substring test would read that record as a relapse.
  code <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl("req(FALSE)", code, fixed = TRUE)))
  # ...and the inbox summary cannot be named ".audit.md" any more
  expect_match(joined, '"no-file-selected.audit.md"', fixed = TRUE)
})

test_that("an empty Admin table looks empty instead of holding a placeholder row", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "dt_none_opts <- function\\(msg")
  expect_match(.ui_block(src, "dt_none_opts <- function", 2L), "emptyTable")
  # the four intake tables used to render NOTE | empty and be counted as a record
  expect_false(grepl('data.frame(note = "empty")', joined, fixed = TRUE))
  expect_false(grepl('data.frame(note = "no uploads yet")', joined, fixed = TRUE))
  expect_false(grepl('data.frame(message = "No feedback yet.")', joined, fixed = TRUE))
})

test_that("the gaps list holds only gaps, and a file that could not be opened says so", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  blk <- .ui_block(src, "output\\$adm_gaps <- renderDT", 22L)
  expect_match(blk, 'runs\\$status\\) %in% "unsupported"')
  expect_match(blk, "not recorded")                     # blanks are named, not left blank
  expect_match(joined, "output\\$adm_unreadable <- renderDT")
  expect_match(.ui_block(src, "output\\$adm_unreadable <- renderDT", 12L), '%in% "failed"')
})

test_that("Admin does not print instructions for a picker with no options", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$adm_sugg_help <- renderUI", 14L)
  expect_match(blk, "Nothing to teach it yet")
  expect_match(blk, "nrow\\(adm_suggestions\\(\\)\\$indicator_tokens")
})

test_that("every Admin action that changes something says what it changed", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # marking a format request done / dismissed writes to disk and said nothing
  expect_match(joined, "\\.req_set <- function")
  expect_match(.ui_block(src, "\\.req_set <- function", 16L), "adm_req_msg")
  expect_match(joined, 'uiOutput\\("adm_req_msg"\\)')
  # replacing the whole editor with the built-in defaults said nothing either
  expect_match(.ui_block(src, "observeEvent\\(input\\$adm_lex_defaults", 6L), "adm_lex_msg")
  # THE TWO "Reload from file" BUTTONS ARE GONE. They existed to answer "has
  # somebody else saved this underneath me", which the tool can answer itself, so
  # Save now compares the box against the file the way the template editor does.
  # The thing this test protects -- that nothing happens in silence -- is now
  # protected on the SAVE path instead: an unedited box is refreshed and said to
  # have been refreshed, an edited one is refused once and told why.
  expect_length(grep("input\\$adm_lex_reload", src), 0L)
  expect_length(grep("input\\$adm_dict_reload", src), 0L)
  expect_match(paste(.ui_block(src, "\\.vocab_stale <- function", 20L), collapse = " "),
               "Somebody else on this server saved this file")
  expect_match(.ui_block(src, "observeEvent\\(input\\$adm_dict_save", 14L), "\\.vocab_stale")
  expect_match(.ui_block(src, "observeEvent\\(input\\$adm_lex_save", 22L), "\\.vocab_stale")
})

# ---- the screen that grades a conversion cannot look clean while it is not ---
#
# Everything below was measured on a real statement in a browser before it was
# written; each one is a place the screen said something the tool itself already
# knew to be untrue.

# CARDINAL. The proof strip is the first quality signal on the page, and it was
# pinned to five NAMED checks. dates_within_period and transaction_count are
# verdict checks that CAN FAIL and were permanently off it -- so a statement drew
# three ticks and a dash under a key promising "a problem", while the Checks table
# two disclosures down read "All dates fall in the statement period | Problem |
# 34 date(s) outside period"; and a statement that printed nine transactions and
# gave up seven drew four chips, none red.
test_that("the proof strip cannot be all-clear while a check has failed", {
  pick <- .ui_fun(".proof_pick", also = c(".stmt_base", ".stmt_index"),
                  consts = ".STMT_TAG")
  listed <- c("balance_reconciliation", "no_unparsed_rows", "amount_direction",
              "running_balance_continuity", "dates_readable")
  # a failing check that is NOT on the list is added, and named last so the
  # confirmations a reviewer asked for keep their order
  got <- pick(name   = c("balance_reconciliation", "dates_readable", "dates_within_period"),
              status = c("pass", "pass", "fail"), listed = listed)
  expect_true("dates_within_period" %in% got)
  expect_identical(got[length(got)], "dates_within_period")
  # a CLEAN run's strip is exactly what it was: the listed checks, nothing added
  expect_identical(pick(c("balance_reconciliation", "dates_readable", "dates_within_period"),
                        c("pass", "pass", "pass"), listed),
                   c("balance_reconciliation", "dates_readable"))
  # a listed check that fails is not drawn twice
  expect_identical(pick(c("balance_reconciliation"), c("fail"), listed),
                   "balance_reconciliation")
  # an NA status is not a failure, and must not put an NA name on the strip
  expect_identical(pick(c("balance_reconciliation", "transaction_count"),
                        c("pass", NA), listed), "balance_reconciliation")
  # ...and the renderer really uses it
  expect_match(.ui_block(.ui_src(), "output\\$cv_proof <- renderUI", 8L),
               "\\.proof_pick\\(k\\$name, k\\$status, \\.PROOF_CHECKS\\)")
})

# CARDINAL, AND MEASURED ON SCREEN TWICE: an auto-split upload drew NO PROOF
# STRIP AT ALL. R/split.R tags every check of a bundle "<code> [statement N]";
# .proof_pick matched whole KPI names against the bare codes, so on a bundle
# nothing matched, and the renderer's `if (!length(pick)) return(NULL)` took the
# strip AND its key off the page -- anz_0382004_multi.pdf, 824 rows, "Sent to the
# dashboards", #cv_proof innerText zero characters. All three split files in the
# corpus did it. With a failing check it was worse by code: the failure branch
# compares names to names, so it still matched, and the bundle drew nothing but
# RED chips under a key whose first entry defines a tick.
test_that("the proof strip covers every statement of an auto-split bundle", {
  pick <- .ui_fun(".proof_pick", also = c(".stmt_base", ".stmt_index"),
                  consts = ".STMT_TAG")
  idx  <- .ui_fun(".stmt_index", consts = ".STMT_TAG")
  listed <- c("balance_reconciliation", "no_unparsed_rows", "amount_direction",
              "running_balance_continuity", "dates_readable")
  tag <- function(code, i) sprintf("%s [statement %d]", code, i)
  codes <- c("balance_reconciliation", "running_balance_continuity",
             "dates_within_period", "dates_readable", "no_unparsed_rows")
  nm <- unlist(lapply(1:3, function(i) tag(codes, i)))

  # a CLEAN bundle: every part is confirmed, and the parts do not interleave
  got <- pick(nm, rep("pass", length(nm)), listed)
  expect_true(length(got) > 0L)
  for (i in 1:3) expect_true(tag("balance_reconciliation", i) %in% got)
  expect_false(is.unsorted(idx(got)))

  # one part fails a check that is not on the list. It must appear, AND every
  # part's passing confirmations must still be there -- a bundle drawn all-red
  # over one bad check is the same lie as an all-clear strip over one.
  st <- rep("pass", length(nm)); st[nm == tag("dates_within_period", 2)] <- "fail"
  got2 <- pick(nm, st, listed)
  expect_true(tag("dates_within_period", 2) %in% got2)
  for (i in 1:3) expect_true(tag("balance_reconciliation", i) %in% got2)
  expect_false(is.unsorted(idx(got2)))
  # ...and it sits with its own statement, not orphaned at the end of the strip
  expect_identical(idx(got2[which(got2 == tag("dates_within_period", 2))]), 2L)

  # the tag readers themselves: an untagged run is one group numbered 0, which is
  # what lets the renderer group without special-casing a missing tag
  base <- .ui_fun(".stmt_base", consts = ".STMT_TAG")
  expect_identical(base(c("dates_readable [statement 7]", "dates_readable")),
                   c("dates_readable", "dates_readable"))
  expect_identical(idx(c("dates_readable [statement 7]", "dates_readable")), c(7L, 0L))

  # the renderer draws one LABELLED row per part, counted off the whole check
  # table rather than off the chips, so a part with nothing to draw still appears
  blk <- .ui_block(.ui_src(), "output\\$cv_proof <- renderUI", 46L)
  expect_match(blk, "\\.stmt_index\\(k\\$name\\)")
  expect_match(blk, "Statement %d of %d", fixed = TRUE)
  expect_match(blk, "No check ran on this part of the file", fixed = TRUE)
})

# One definition of the auto-split tag. Three readers each carried their own copy
# of the regex and the fourth -- the strip above -- did not know the tag existed.
test_that("only one place in app.R knows what the auto-split tag looks like", {
  src <- .ui_src()
  # the literal pattern, matched literally: it appears once, in the constant
  hits <- grep("[[:space:]]*\\\\[statement", src, fixed = TRUE)
  expect_length(hits, 1L)
  expect_match(src[hits], "^\\s*\\.STMT_TAG <- ")
  # ...and every reader goes through the pair of helpers built on it
  expect_gte(length(grep("\\.stmt_base\\(", src)), 3L)
})

# "Period:" was the min/max of the TRANSACTION dates whenever any row had one, so
# every transaction was inside the range the screen called the period BY
# CONSTRUCTION -- and "34 date(s) outside period" beside it could only ever read
# as a bug in the tool. The statement's own printed period appeared nowhere.
test_that("the transaction span and the statement's printed period are both named", {
  f <- .ui_fun(".period_lines")
  got <- f(as.Date(c("2025-08-15", "2025-09-13")), "13 Aug 25", "1 Sep 25")
  expect_length(got, 2L)
  expect_identical(got[1], "Transactions span: 15 Aug 2025 to 13 Sep 2025")
  # the header goes through the same parser the check uses, so the dates on screen
  # are the dates in the check's Expected column (2025-08-13..2025-09-01)
  expect_identical(got[2], "Statement period: 13 Aug 2025 to 01 Sep 2025")
  # neither range is ever printed under the other's name
  expect_false(any(grepl("^Period:", got)))
  # a header with no period says so by absence, not by borrowing the span
  expect_identical(f(as.Date("2025-08-15"), NA, NA), "Transactions span: 15 Aug 2025 to 15 Aug 2025")
  # a period bound that will not parse is shown in the statement's own words
  expect_match(f(as.Date(character(0)), "the 2024 tax year", NA)[1],
               "Statement period: the 2024 tax year")
  # nothing at all still leaves a labelled line, never a blank
  expect_identical(f(as.Date(character(0)), NA, NA), "Transactions span: -")
})

# ...AND THE SPLIT THAT MADE EACH HEADING SAY ITS OWN THING LEFT ONE OF THEM
# UNGRAMMATICAL. .why_empty frames every phrase as "..., so there is %s.", and
# the phrase kept its plural from the joined sentence it was cut out of: "so
# there is no fields to report." It shipped on BOTH routes, on every unsupported
# or failed run, and the test above pinned the broken string.
#
# So this pins the RULE instead of the strings: whatever phrases the screen
# passes to said(), each one has to finish the sentence .why_empty starts. A
# plural noun straight after "no" cannot.
test_that("every empty-table phrase finishes the sentence it is dropped into", {
  blk    <- .ui_block(.ui_src(), "output\\$cv_detail <- renderUI", 26L)
  phrase <- regmatches(blk, gregexpr('said\\("[^"]+"\\)', blk))[[1]]
  phrase <- sub('^said\\("', "", sub('"\\)$', "", phrase))
  expect_gt(length(phrase), 1L)
  why <- .ui_fun(".why_empty")
  for (p in phrase) {
    s <- why(list(status = "unsupported"), p)
    expect_match(s, "\\.$")
    # "there is no fields", "there is no rows" -- the failure this exists for.
    expect_false(grepl("there is no [a-z]+s\\b", s),
                 info = paste("plural after \"there is no\":", s))
  }
})

# ...AND THE THIRD TABLE WAS LEFT OUT OF THAT FIX. Two of the three headings in
# "Checks & detail" learned to say why they were empty; "Diagnostics - where /
# why / how to fix" did not, so it alone still sat over blank space on the screen
# with the least to go on. build_diagnostics() always returns at least a "no
# issues detected" row, so a conversion that RAN cannot leave it empty -- the way
# in is a conversion that never finished, where job_failed_result() builds a
# result out of a status and a sentence and nothing else.
test_that("the Diagnostics table says why it is empty, like the two beside it", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_detail <- renderUI", 26L)
  expect_match(blk, "has_diag <- is\\.data\\.frame\\(res\\$diagnostics\\) && nrow\\(res\\$diagnostics\\) > 0L")
  expect_match(blk, 'if \\(has_diag\\) DTOutput\\("cv_diag"\\) else')
  # ...in ITS OWN words. "Nothing was read from this file" (.why_empty) would name
  # the wrong cause: for a run that was killed, the file was never the problem.
  expect_match(blk, "WHY_NO_DIAG")
  say <- .ui_block(src, "^\\s*WHY_NO_DIAG <- ", 1L)
  expect_match(say, "did not get far enough")
  expect_false(grepl("Nothing was read from this file", say, fixed = TRUE))
})

# "Was this conversion correct?" appeared under "Could not read this file" -- a
# question about figures on a screen with no figures, whose one consequential
# answer withdraws rows from the dashboards that were never published. And the
# three answers were drawn with the SAME tick and cross the proof-strip key
# twenty lines above defines as "checked and passed" and "a problem".
test_that("feedback is asked only about a conversion, and never in the proof glyphs", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_feedback <- renderUI", 40L)
  expect_match(blk, 'res\\$status %\\|\\|% ""\\) %in% c\\("ok", "needs_review"\\)')
  expect_match(blk, 'choiceNames = list\\("Correct", "Minor issues", "Wrong"\\)')
  # the proof strip's three glyphs mean one thing each on this page
  for (g in c("\\u2713", "\\u2717")) expect_false(grepl(g, blk, fixed = TRUE))
  expect_match(blk, "choiceValues = list\\(\"correct\", \"minor_issues\", \"wrong\"\\)")
})

# ---------------------------------------------------------------------------
# 0c. A badly OCR'd page can come back "ok" -- and the caveat lived in a panel
# nobody opens, behind "Show me how it read this", while the verdict was green.
# ---------------------------------------------------------------------------

test_that("a scan says so beside the download, not in a panel", {
  note <- .ui_fun(".scan_note")
  # nothing machine-read: nothing said
  expect_null(note(NULL, NULL))
  expect_null(note(0L, 0L))
  # read from a scan, nothing doubtful: still said, because the figures came off
  # an image and not off a text layer
  expect_match(note(3L, 0L), "read from a scan")
  # ...and the count of doubtful figures when there is one, in plain words with
  # no code, no score and no fraction
  expect_match(note(3L, 4L), "4 figures came out too faint")
  expect_match(note(3L, 1L), "1 figure came out too faint")
  for (s in c(note(3L, 0L), note(3L, 4L))) {
    expect_false(grepl("ocr", s, ignore.case = TRUE))
    expect_false(grepl("confidence", s, ignore.case = TRUE))
    expect_lt(length(gregexpr("[.]", s)[[1]]), 2L)     # one sentence
  }
  # and it is rendered WITH the download bar, not in the evidence panel
  blk <- .ui_block(.ui_src(), "output\\$cv_downloads <- renderUI", 40L)
  expect_match(blk, "\\.scan_note\\(res\\$header\\$ocr_pages, n_low\\)")
  expect_match(blk, "ocr_low_conf")
  # the flag it counts is really the one the reader writes
  pp <- readLines(file.path(engine_root(), "R", "parse_pdf_table.R"), warn = FALSE)
  expect_true(any(grepl('"ocr_low_conf"', pp, fixed = TRUE)))
})

test_that("the number of controls on screen does not creep back up", {
  # Measured the way the register measures it, so the number in the register and
  # the number here can never disagree. It was 182 when the register was written
  # and 174 after this sweep. A rise is not forbidden by accident -- it is
  # forbidden so that adding a control means removing one, which is the only
  # discipline that has ever made a screen smaller.
  ids <- .ui_control_ids()
  expect_lte(length(ids), 174L)
})

test_that("the scan is warned about once, not three times above the fold", {
  note <- .ui_fun(".scan_note")
  # BOTH counts are in the one sentence beside the download...
  expect_match(note(4L, 2L), "4 page(s) were machine-read", fixed = TRUE)
  expect_match(note(4L, 2L), "2 figures came out too faint", fixed = TRUE)
  expect_match(note(4L, 0L), "4 page(s) were machine-read", fixed = TRUE)
  for (s in c(note(4L, 0L), note(4L, 2L))) expect_lt(length(gregexpr("[.]", s)[[1]]), 2L)
  # ...so the two chips that said the same two things on the verdict are gone
  src <- .ui_src()
  expect_false(any(grepl("page(s) machine-read (OCR) - double-check", src, fixed = TRUE)))
  chips <- .ui_block(src, "ROW_FLAG_CHIPS <- c\\(", 6L)
  expect_false(grepl("ocr_low_conf", chips, fixed = TRUE))
  # and nothing else was quietly dropped with them
  expect_match(chips, "date_year_inferred")
  expect_match(chips, "date_unresolved")
})

test_that("a forged identity header is refused, and the run is never called 'sso'", {
  # no proxy configured at all: the header is simply not read
  r <- .ident(list(identity_header = "", identity_shared_secret = ""),
              .hdrs(HTTP_X_FORWARDED_USER = "some.other.detective"))
  expect_identical(r$source, "os")
  expect_false(identical(r$who, "some.other.detective"))

  # proxy configured, but the request carries no secret
  r <- .ident(.IDENT_ON, .hdrs(HTTP_X_REMOTE_USER = "some.other.detective"))
  expect_identical(r$source, "os")

  # ...and a guessed secret is no better
  r <- .ident(.IDENT_ON, .hdrs(HTTP_X_REMOTE_USER = "some.other.detective",
                               HTTP_X_STATEMENT_STUDIO_SECRET = "guess"))
  expect_identical(r$source, "os")
})

test_that("no identity source reads the environment any more", {
  # There was a branch here reading an environment variable set by a per-container
  # deployment. That deployment has been deleted (locked-decisions.md, D7: no container
  # runtime on an air-gapped Windows box), and this holds the branch gone.
  #
  # It must stay gone for a reason that is specific to how this app runs: it is ONE R
  # process serving every analyst, so every session shares one environment. An
  # environment variable is therefore not a per-person authenticated identity here --
  # a leftover branch reading one would let anything able to set a variable in the
  # service account environment name itself in the download log.
  ln <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  expect_false(any(grepl("PROXY_USERNAME", ln, fixed = TRUE)))
  # ...and no environment read survives anywhere in the identity chain itself
  i <- grep("detected_identity_info <- function", ln, fixed = TRUE)[1]
  expect_false(is.na(i))
  expect_false(any(grepl("Sys.getenv", ln[i:(i + 40L)], fixed = TRUE)))
  # the real guarantee the deleted branch was tested for is kept: a forged header
  # cannot name itself without the shared secret
  r <- .ident(.IDENT_ON, .hdrs(HTTP_X_REMOTE_USER = "attacker"))
  expect_identical(r$source, "os")
})

test_that("the real proxy is still believed", {
  r <- .ident(.IDENT_ON, .hdrs(HTTP_X_REMOTE_USER = "real.detective",
                               HTTP_X_STATEMENT_STUDIO_SECRET = "s3cret-from-the-proxy"))
  expect_identical(r$source, "sso")
  expect_identical(r$who, "real.detective")
})

test_that("the two httpuv header tricks that defeat a careful proxy are refused", {
  # DUPLICATES ARE JOINED WITH A COMMA. httpuv's header map is case-insensitive,
  # so two copies of the name arrive as one value "attacker,real.detective" --
  # which a plain nzchar() test accepts as a username.
  r <- .ident(.IDENT_ON, .hdrs(HTTP_X_REMOTE_USER = "attacker,real.detective",
                               HTTP_X_STATEMENT_STUDIO_SECRET = "s3cret-from-the-proxy"))
  expect_identical(r$source, "os")

  # THE UNDERSCORE SPELLING IS A SEPARATE HEADER THAT WINS THE SAME ROOK KEY.
  # httpuv's map is underscore-SENSITIVE, so "X_Remote_User" is its own entry, but
  # it normalises to the same HTTP_X_REMOTE_USER and is written second. A proxy
  # that strips and sets only the hyphen spelling is bypassed by sending the
  # underscore one. (nginx drops underscore headers by default for this reason;
  # IIS and Apache do not.) So more than one spelling of the name is a refusal.
  smuggled <- list(
    HTTP_X_REMOTE_USER = "attacker",                    # the value that won
    HTTP_X_STATEMENT_STUDIO_SECRET = "s3cret-from-the-proxy",
    HEADERS = c("x-remote-user" = "real.detective",     # what the proxy set
                "x_remote_user" = "attacker",           # what the client smuggled
                "x-statement-studio-secret" = "s3cret-from-the-proxy"))
  r <- .ident(.IDENT_ON, smuggled)
  expect_identical(r$source, "os")
})

test_that("the shared-secret comparison cannot be timed one character at a time", {
  expect_true(.secret_ok("s3cret", "s3cret"))
  expect_false(.secret_ok("s3cret", "s3creT"))     # case matters
  expect_false(.secret_ok("s3cre", "s3cret"))      # a prefix is not a match
  expect_false(.secret_ok("s3cretx", "s3cret"))
  # absent, empty or unusable input is never proof of anything
  for (bad in list(NULL, NA, "", character(0)))
    expect_false(.secret_ok(bad, "s3cret"))
  for (bad in list(NULL, NA, ""))
    expect_false(.secret_ok("s3cret", bad))
  # every byte is compared: no early exit on the first difference
  src <- readLines(file.path(engine_root(), "R", "util.R"), warn = FALSE)
  i <- grep("^\\.secret_ok <- function", src)
  blk <- paste(src[i:(i + 12)], collapse = "\n")
  expect_match(blk, "xor")
  expect_false(grepl("identical(got, want)", blk, fixed = TRUE))
})

# ===========================================================================
# BANK-FIRST AUTOMATIC READING (spec section 7). Templates are retired: the
# Convert table carries each file's BANK and, once converted, its learned LAYOUT
# and its OUTCOME; Please check is where a statement that did not prove itself is
# set right; Admin -> Banks and Admin -> Automatic reading replace the template
# library. tools/ui/check.mjs drives all of it in a real browser; these hold the
# rules that browser drive cannot see.
# ===========================================================================

test_that("no screen offers a template any more", {
  src <- .ui_src()
  pd <- utils::getParseData(parse(file.path(engine_root(), "app.R"), keep.source = TRUE))
  lits <- pd$text[pd$terminal & pd$token == "STR_CONST"]
  expect_gt(length(lits), 300L)                          # the scan must not go quiet
  # The only strings left naming one are field and record names the engine keeps
  # (the feed's template_id column, the upload record's `template` field, a
  # metadata category), never words on a screen.
  said <- grep("template", lits, ignore.case = TRUE, value = TRUE)
  expect_setequal(gsub('"', "", said), c("template_id", "template_hints", "template"))
  for (gone in c("Add a template", "ts_file", "g_pdf_plot", "adm_tpl_overview", "adm_learned",
                 "cv_teach", "ix_plot", "tutorial_html"))
    expect_false(any(grepl(gone, src, fixed = TRUE)), info = gone)
  labs <- new.env(); sys.source(file.path(engine_root(), "ui_labels.R"), envir = labs)
  words <- unlist(Filter(is.character, mget(ls(labs), labs)))
  expect_false(any(grepl("template", words, ignore.case = TRUE)))
  about <- readLines(file.path(engine_root(), "ui_content.R"), warn = FALSE)
  about <- gsub("grid-template", "", about[!grepl("^\\s*#", about)], fixed = TRUE)   # a CSS property
  expect_false(any(grepl("template", about, ignore.case = TRUE)))
})

test_that("no retired engine function is called by the screens", {
  src <- paste(.ui_src(), collapse = "\n")
  retired <- c("load_template_set", "load_templates", "validate_template", "template_overview",
               "library_overview", "template_display_name", "template_yaml", "duplicate_template_groups",
               "save_user_template", "user_template_ids", "delete_user_template",
               "set_user_template_hidden", "detect_statement", "draft_template", "draft_preview",
               "header_phrases", "wd_amount_labels", "learned_load", "learned_record",
               "learned_forget", "template_choices", "recognition_summary", "fingerprint_phrases",
               "template_usage", "template_drift", "migrate_template_layout", "template_sha256",
               "\\.trust_ok", "inspect_pdf_layout")
  for (f in retired) expect_false(grepl(sprintf("\\b%s\\(", f), src, perl = TRUE), info = f)
  expect_false(grepl("CONFIG\\$paths\\$(templates|user_templates|learned_choices)", src))
  # ...and every engine call it does make is to a function the engine defines
  for (f in c("identify_file", "bank_choices", "layouts_load", "layout_display_name",
              "layout_confirm", "layout_retire", "layout_rename", "layouts_banks",
              "fixes_pending", "fix_accept", "fix_discard", "track_summary", "track_export",
              "spot_check_record", "convert_batch", "statement_audit", "batch_audit",
              "layout_usage", "layout_drift"))
    expect_true(exists(f, mode = "function"), info = f)
})

test_that("the Convert table carries each file's bank, and gives it only when changed", {
  eff <- .ui_fun("plan_effective")
  shown <- .ui_fun("plan_shown")
  p <- list(rows = data.frame(bank = c("bnz", NA, "anz", NA), stringsAsFactors = FALSE))
  picks <- c(NA, NA, "anz", "Smith Credit Union")
  # left alone, or set to what the statement named anyway: the statement decides
  # (rule 1); changed: exactly that bank (rule 2)
  expect_identical(eff(p, picks), c(NA, NA, NA, "Smith Credit Union"))
  expect_identical(shown(p, picks), c("bnz", NA, "anz", "Smith Credit Union"))
  src <- .ui_src()
  # one file per tick, from identify_file(), and the event loop gets the process back
  obs <- .src_block(src, "One file per tick", 36L)
  expect_match(obs, "identify_file\\(rows\\$datapath\\[i\\], rows\\$name\\[i\\]\\)")
  expect_match(obs, "invalidateLater\\(1, session\\)")
  # the dropdown: every bank, and a way to name one the list does not have
  sel <- .src_block(src, "\\.plan_select <- function", 14L)
  expect_match(sel, 'opt\\("__new__", "Another bank - type its name')
  expect_match(.src_block(src, "bank_list <- reactive", 8L), "bank_choices\\(LAYOUTS_DIR\\)")
  # a name typed for a bank is refused when it is really an account number
  prob <- .ui_fun(".bank_name_problem")
  expect_match(prob("01-0102-0123456-00"), "account number")
  expect_match(prob(""), "Type the bank")
  expect_null(prob("Smith Credit Union"))
  # Convert gives each file its own bank: a case through convert_batch's `banks`,
  # one file as convert_statement's `bank`
  go <- .src_block(src, "observeEvent\\(input\\$cv_go, \\{", 60L)
  expect_match(go, "if \\(nrow\\(f\\) > 1L\\) run_batch\\(f, eff, rows = again\\)")
  expect_match(go, "run_conversion\\(f\\$datapath\\[1\\], f\\$name\\[1\\], bank = if \\(is\\.na\\(eff\\[1\\]\\)\\) NULL else eff\\[1\\]\\)")
  expect_match(.src_block(src, "run_batch <- function", 60L), "args = c\\(a, list\\(banks = banks\\)\\)")
  expect_true("banks" %in% names(formals(convert_batch)))
})

test_that("the QID is asked once for the whole case, before it starts", {
  src <- .ui_src()
  blk <- src[grep("observeEvent\\(input\\$cv_go, \\{", src):length(src)][1:75]
  i_qid <- grep("\\.identity_ok\\(\\)", blk)[1]
  i_batch <- grep("run_batch\\(f, eff, rows = again\\)", blk)[1]
  expect_false(is.na(i_qid) || is.na(i_batch))
  expect_true(i_qid < i_batch)
  expect_match(.ui_block(src, "\\.identity_ok <- function", 8L), "is\\.na\\(cv_qid\\(\\)\\)")
  expect_length(grep("Enter your QID first", src, fixed = TRUE), 1L)
})

test_that("each file's outcome is said in the four phrases, and the reason goes with it", {
  L <- new.env(); sys.source(file.path(engine_root(), "ui_labels.R"), envir = L)
  po <- L$plain_outcome
  expect_identical(po("ok", "proven", "proven", "x")$word, "Proven")
  expect_identical(po("ok", "layout_match", "layout_match")$word, "Matches a learned layout")
  expect_identical(po("ok", "check", "person")$word, "Confirmed on Please check")
  expect_identical(po("ok", "proven", "person", fix = "boxes")$word, "Proven with the columns you drew")
  nr <- po("needs_review", "check", "none", "Two readings fit.")
  expect_identical(c(nr$word, nr$why, nr$cls), c("Please check", "Two readings fit.", "warn"))
  un <- po("unsupported", "unread", "none", "Nothing adds up.")
  expect_identical(c(un$word, un$why, un$cls), c("Couldn't read", "Nothing adds up.", "bad"))
  expect_identical(po("failed", NA, NA, "damaged")$word, "Couldn't read")
  # an automatic outcome carries no reason for the table: there is nothing to do
  expect_identical(po("ok", "proven", "proven", "every step adds up")$why, "")
  # ...and the table and both verdict cards say it with that one function
  src <- .ui_src()
  expect_match(.src_block(src, "output\\$cv_plan <- renderUI", 200L), "plain_outcome\\(res_i\\$status")
  expect_match(.ui_block(src, "output\\$cv_status <- renderUI", 30L), "plain_outcome\\(st,")
  expect_match(.ui_block(src, "output\\$cv_headline <- renderUI", 30L), "plain_outcome\\(\"ok\",")
  # the learned layout each file was read with, by the name people see
  lys <- .src_block(src, "\\.res_layouts <- function", 12L)
  expect_match(lys, "\\.layout_name\\(rd\\$matched_layout\\)")
  expect_match(lys, '"new"')
})

test_that("a row's Please check opens that file and takes the page to it", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "\\$\\(document\\)\\.on\\('click', 'a\\.plan-check'")
  op <- .src_block(src, "observeEvent\\(input\\$cv_plan_open, \\{", 10L)
  expect_match(op, "open_batch_row\\(i\\)")
  expect_match(op, 'session\\$sendCustomMessage\\("ss-scroll", "cv_check"\\)')
  expect_match(joined, "Shiny\\.addCustomMessageHandler\\('ss-scroll'")
  # offered only where a person has something to do AND there are columns to show
  blk <- .src_block(src, "output\\$cv_plan <- renderUI", 200L)
  expect_match(blk, "link <- if \\(o\\$cls != \"ok\" && has_cols\\)")
  # a click on the bank dropdown in a row is not a click on the row
  expect_match(joined, "closest\\('select,option,a,button,input,label'\\)\\.length\\) return;")
})

test_that("Please check draws the columns on the page and ticks each page's balance", {
  ticks <- .ui_fun(".page_ticks")
  # oldest first: 100 -> 150 -> 140 on page 1, 140 -> 200 on page 2
  r <- data.frame(page = c(1L, 1L, 1L, 2L), amount = c(0, 50, -10, 60),
                  balance = c(100, 150, 140, 200), derived = c(FALSE, FALSE, FALSE, TRUE))
  t <- ticks(r, 1:2)
  expect_identical(t$steps, c(2L, 1L)); expect_identical(t$held, c(2L, 1L))
  expect_identical(t$derived, c(0L, 1L))
  # newest first is the same statement read the other way round
  t2 <- ticks(r[4:1, ], 1:2)
  expect_identical(t2$held, t2$steps)
  # a step that does not add up is a cross on ITS page, and only there
  bad <- r; bad$balance[4] <- 205
  tb <- ticks(bad, 1:2)
  expect_identical(tb$held, c(2L, 0L)); expect_identical(tb$steps, c(2L, 1L))
  # a balance printed once a day still makes a step, just a longer one
  day <- data.frame(page = 1L, amount = c(0, 10, 20), balance = c(100, NA, 130), derived = FALSE)
  expect_identical(ticks(day, 1L)$held, 1L)
  # an amount that could not be read leaves its step unjudged, never "broken"
  na <- data.frame(page = 1L, amount = c(0, NA), balance = c(100, 110), derived = FALSE)
  expect_identical(c(ticks(na, 1L)$steps, ticks(na, 1L)$held), c(0L, 0L))
  # a page with no rows, and no rows at all
  expect_identical(ticks(r, 1:3)$rows, c(3L, 1L, 0L))
  expect_identical(ticks(NULL, 1:2)$rows, c(0L, 0L))
  word <- .ui_fun(".tick_word")
  expect_identical(word(t[1, ])$glyph, "\u2713")
  expect_identical(word(tb[2, ])$glyph, "\u2717")
  expect_match(word(tb[2, ])$say, "1 of 1 balance step do not add up")
  expect_identical(word(ticks(r, 1:3)[3, ])$say, "no transactions on this page")
})

test_that("a statement's page in a bundle is the file's page on Please check", {
  rows_of <- .ui_fun(".result_rows")
  d <- tempfile("rr_"); dir.create(d)
  js <- file.path(d, "x.json")
  jsonlite::write_json(list(
    transactions = data.frame(row_id = 1:3, amount = c(-1, 2, -3), balance = c(9, 11, 8),
                              flags = c("", "amount_from_balance", ""), statement_index = c(1L, 1L, 2L)),
    provenance = data.frame(row_id = 1:3, source_ref = c("pdf:p1", "pdf:p2", "pdf:p1"))),
    js, auto_unbox = TRUE)
  res <- list(outputs = c(json = js), reading = list(list(pages = 1:2), list(pages = 3L)))
  got <- rows_of(res)
  expect_identical(got$page, c(1L, 2L, 3L))            # statement 2's page 1 is the file's page 3
  expect_identical(got$statement, c(1L, 1L, 2L))
  expect_identical(got$derived, c(FALSE, TRUE, FALSE))
  expect_null(rows_of(list(outputs = character(0))))  # a run that wrote nothing
})

test_that("Re-read sends the roles as a fix, and the answer goes back where it came from", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  ov <- .src_block(src, "\\.ck_roles_overrides <- function", 12L)
  expect_match(ov, 'input\\[\\[paste0\\("cv_ck_role_", f\\)\\]\\]')
  expect_match(ov, "if \\(length\\(res\\$reading\\) > 1L\\) ov\\$statement <- s")
  # the dropdowns offer exactly the roles R/convert.R takes
  L <- new.env(); sys.source(file.path(engine_root(), "ui_labels.R"), envir = L)
  expect_setequal(names(L$ROLE_PLAIN), .FIGURE_ROLES)
  rr <- .src_block(src, "\\.reread <- function", 70L)
  expect_match(rr, "convert_args\\(bank = bk, bank_confirmed = bc, overrides = overrides, confirm = confirm\\)")
  # into the case's own folder, over the old outputs, never over the statement
  expect_match(rr, 'cv_slot\\$start\\("convert", src\\$path, isolate\\(cv_dir\\(\\)\\)')
  expect_match(.src_block(src, "run_conversion <- function", 20L), 'src <- file\\.path\\(sess, "in", name\\)')
  expect_match(.src_block(src, "run_batch <- function", 40L), 'paths <- file\\.path\\(sess, "in", nms\\)')
  # the changed figures reach the feed the same way a conversion's do, and the case row
  expect_match(rr, "publish_result\\(res, cv_recorded\\(\\)\\)\\s+gate <- isolate\\(cv_feed_gate\\(\\)\\)")
  expect_match(rr, "b\\$result\\[i\\] <- list\\(res\\)")
  expect_match(rr, "b\\$failing_check\\[i\\] <- \\.failing_check\\(res\\)")
  # the bank is read with [[ ]]: src$bank would partially match bank_confirmed
  expect_match(rr, 'bk <- bank %\\|\\|% src\\[\\["bank"\\]\\]')
  expect_false(grepl("src\\$bank", joined))
  # "This is right" vouches for the reading ON SCREEN, never a dropdown not yet re-read
  cf <- .src_block(src, "observeEvent\\(input\\$cv_ck_confirm, \\{", 16L)
  expect_match(cf, "press Re-read first")
  expect_match(cf, "\\.reread\\(cv_ov\\(\\), confirm = TRUE")
  # a confirm the engine refuses is said, in the engine's own words
  words <- .ui_fun(".reread_words", also = c("plain_messages", ".sentence"), consts = ".AUDIT_GAP_RX")
  expect_match(words(list(status = "needs_review", messages = c(
    "This reading cannot be confirmed: the statement's own arithmetic contradicts it (x) Set the columns' roles instead.",
    "needs_review: y")), TRUE), "^This reading cannot be confirmed")
  expect_match(words(list(status = "ok", feed_basis = "person", fix_held = "kiwibank_1", messages = "ok: 3 row(s)"), TRUE),
               "held for an admin")
  expect_match(words(list(status = "ok", outcome = "proven", feed_basis = "proven", messages = "ok: 3 row(s)",
                          learn = list(list(action = "corrected", why = "Layout x now reads this way."))), FALSE),
               "^Proven - .*Layout x now reads this way\\.$")
  expect_match(words(list(status = "needs_review", reason = "the balance breaks at row 2",
                          reading = list(list(transactions = data.frame(amount = 1:3))),
                          messages = "needs_review: z"), FALSE),
               "^Still not proven: The balance")
  # a change that leaves nothing readable says so, and where the way back is
  expect_match(words(list(status = "unsupported", reason = "The table reader could not read the rows.",
                          reading = list(list(transactions = NULL)), messages = "unsupported: z"), FALSE),
               "^Nothing could be read this way .*Undo your changes")
})

test_that("drawing the columns is the last resort, sends boxes, and is never learned", {
  src <- .ui_src()
  ed <- .src_block(src, 'observeEvent\\(input\\$cv_ck_editor, \\{', 40L)
  # it starts from the columns the reader found, page by page
  expect_match(ed, "boxes <- if \\(is\\.data\\.frame\\(cols\\) && nrow\\(cols\\)\\)")
  # the brush reports on release: a delay longer than any drag, across only
  expect_match(ed, 'brushOpts\\("ed_brush", direction = "x", delay = 1500,')
  sv <- .src_block(src, "observeEvent\\(input\\$ed_save, \\{", 14L)
  expect_match(sv, "ov <- list\\(columns = b\\)")
  expect_match(sv, '!\\("date" %in% b\\$field\\)')
  # reachable from Please check only, and only on a PDF
  expect_length(grep('actionLink\\("cv_ck_editor"', src), 1L)
  expect_match(.src_block(src, "output\\$cv_ck_side <- renderUI", 70L),
               'if \\(\\.ck_is_pdf\\(res\\)\\)\\s+p\\(style = "margin-top:10px;font-size:13px",\\s+"None of these fits\\? ", actionLink\\("cv_ck_editor"')
  # every field it offers is one the engine's box reader takes
  ids <- eval(parse(text = sub("^\\s*\\.ED_FIELDS <- ", "",
    paste(src[grep("^\\s*\\.ED_FIELDS <- c\\(", src) + 0:3], collapse = "\n")))[[1]])
  expect_true(all(ids %in% c(.BOX_CORE, "date2", "weekday")))
})

test_that("derived amounts are marked wherever the figures are", {
  src <- .ui_src()
  tx <- .src_block(src, "output\\$cv_txns <- renderDT", 60L)
  expect_match(tx, 'grepl\\("amount_from_balance", df\\$flags, fixed = TRUE\\)')
  expect_match(tx, 'formatStyle\\(dt, "\\.derived", target = "row"')
  expect_match(.src_block(src, "output\\$cv_ck_side <- renderUI", 70L), "res\\$derived")
  L <- new.env(); sys.source(file.path(engine_root(), "ui_labels.R"), envir = L)
  expect_match(L$FLAG_PLAIN[["amount_from_balance"]], "worked out from the balance")
  # the reader keeps a row whose amount was removed (R/auto_read.R): nobody added it
  expect_false(grepl("by hand", L$FLAG_PLAIN[["forced"]]))
  # the field coverage's notes speak of the statement, never of a template
  expect_false(any(grepl("template", L$COVERAGE_NOTE_PLAIN, ignore.case = TRUE)))
  expect_match(.src_block(src, "output\\$cv_coverage <- renderDT", 20L), "COVERAGE_NOTE_PLAIN\\[cov\\$verdict\\]")
})

test_that("the bank question is a plain sentence, and a stand-in is never a bank to pick", {
  q <- .ui_fun(".bank_question")
  expect_identical(q(paste("You picked ASB, but the statement looks like Westpac (medium confidence): Westpac: it",
                           "names Westpac's legal entity. Please confirm."), "ASB", "Westpac"),
                   "You picked ASB, but the statement looks like Westpac: it names Westpac's legal entity.")
  expect_identical(q("The statement points to two banks.", "ASB", NA_character_), "The statement points to two banks.")
  expect_match(.src_block(.ui_src(), "bank_list <- reactive", 8L), "ref\\$pseudo")
})

test_that("a bank the statement disagrees with is asked, and keeping it is a confirm", {
  src <- .ui_src()
  note <- .src_block(src, "output\\$cv_bank_note <- renderUI", 30L)
  expect_match(note, "isTRUE\\(bk\\$block_learning\\)")
  keep <- .src_block(src, "observeEvent\\(input\\$cv_bank_keep, \\{", 8L)
  expect_match(keep, "bank_confirmed = TRUE")
  use <- .src_block(src, "observeEvent\\(input\\$cv_bank_use, \\{", 8L)
  expect_match(use, "bank <- as\\.character\\(res\\$bank\\$institution\\)\\[1\\]")
  # ...and the table's row follows the answer
  expect_match(keep, "\\.set_row_bank\\(bank\\)"); expect_match(use, "\\.set_row_bank\\(bank\\)")
})

test_that("a spot check is asked only when picked, and recorded with no personal data", {
  src <- .ui_src()
  sp <- .src_block(src, "output\\$cv_spot <- renderUI", 20L)
  expect_match(sp, 'if \\(!isTRUE\\(res\\$spot_check\\) \\|\\| !identical\\(res\\$status, "ok"\\)\\) return\\(NULL\\)')
  expect_match(.src_block(src, "\\.spot <- function", 8L), "spot_check_record\\(res, v, TRACKING_DIR\\)")
  expect_true(all(c("right", "wrong", "cant_tell") %in% TRACK_SPOT_CHECKS))
  for (v in c('\\.spot\\("right"\\)', '\\.spot\\("wrong"\\)', '\\.spot\\("cant_tell"\\)'))
    expect_match(paste(src, collapse = "\n"), v)
})

test_that("Admin -> Banks changes a layout only through the engine, signed in, and says what changed", {
  src <- .ui_src()
  ch <- .src_block(src, "\\.layout_change_ui <- function", 14L)
  expect_match(ch, "req\\(admin_ok\\(\\)\\)")
  expect_match(ch, "layouts_bump\\(isolate\\(layouts_bump\\(\\)\\) \\+ 1L\\)")
  joined <- paste(src, collapse = "\n")
  for (f in c("layout_confirm\\(id, LAYOUTS_DIR", "layout_retire\\(id, LAYOUTS_DIR", "layout_rename\\(id, nm, LAYOUTS_DIR",
              "fix_accept\\(id, LAYOUTS_DIR", "fix_discard\\(id, LAYOUTS_DIR"))
    expect_match(joined, f)
  expect_match(.src_block(src, "\\.fix_act <- function", 6L), "req\\(admin_ok\\(\\)\\)")
  # a rename is a name, never a number that could be an account
  expect_match(.src_block(src, "observeEvent\\(input\\$adm_layout_rename, \\{", 12L), "\\[0-9\\]\\[0-9 -\\]\\{3,\\}\\[0-9\\]")
  # a retired layout stays on screen, so Confirm can bring it back
  expect_match(joined, "layouts_load\\(LAYOUTS_DIR, include_retired = TRUE\\)")
})

test_that("training a bank is the case machinery with the bank on every file, and feeds nothing", {
  src <- .ui_src()
  tr <- .src_block(src, "observeEvent\\(input\\$adm_train_go, \\{", 45L)
  expect_match(tr, "req\\(admin_ok\\(\\)\\)")
  expect_match(tr, 'train_slot\\$start\\("batch", paths, sess, overlay = FALSE')
  expect_match(tr, "banks = rep\\(bank, length\\(paths\\)\\)")
  expect_match(tr, "nrow\\(fs\\) > TRAIN_MAX_FILES")
  expect_false(grepl("publish_result|record_upload", tr))
  expect_match(paste(src, collapse = "\n"), "train_slot <- job_slot\\(\\)")
  # the report: N layouts from M statements, P proven, K need a look -- each with its reason
  rp <- .ui_fun(".train_report", also = ".bank_disputed")
  b <- data.frame(file = c("a", "b", "c"), stringsAsFactors = FALSE)
  b$result <- list(
    list(reading = list(list(outcome = "proven", why = "adds up", learned_layout = "anz_1@1"),
                        list(outcome = "check", why = "row 3 breaks", matched_layout = NULL))),
    list(status = "failed", reason = "damaged"),
    # another bank's statement proves itself and teaches nothing: it needs a look too
    list(bank = list(bank = "anz", institution = "westpac", identified_display = "Westpac",
                     block_learning = TRUE),
         reading = list(list(outcome = "proven", why = "adds up", learn = list(action = "none")))))
  got <- rp(list(b = b, names = c("a.pdf", "b.pdf", "c.pdf")))
  expect_identical(nrow(got$statements), 4L)
  expect_identical(sum(got$proven), 2L)
  expect_identical(got$layouts, "anz_1")
  expect_identical(got$statements$why[!got$proven], c("row 3 breaks", "damaged"))
  expect_identical(which(got$look), 2:4)
  expect_match(got$statements$why[4], "looks like a Westpac statement, so nothing was learned")
  expect_match(paste(src, collapse = "\n"),
               'sprintf\\("%s: %d layout%s from %d statement%s, %d proven, %d need%s a look\\."')
})

test_that("Admin -> Automatic reading: counts only, the target, and the spot-check rate", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "safe\\(track_summary\\(TRACKING_DIR\\), NULL\\)")
  expect_match(joined, "AR_TARGET <- 0\\.95")
  ex <- .src_block(src, "output\\$adm_ar_export <- downloadHandler", 10L)
  expect_match(ex, "req\\(admin_ok\\(\\)\\)"); expect_match(ex, "track_export\\(TRACKING_DIR, file\\)")
  expect_match(ex, "\\.dl_log\\(")
  # the rate is saved without disturbing the rest of the settings file, and a file
  # that does not parse is refused rather than overwritten
  save <- .ui_fun(".save_spot_rate")
  p <- tempfile(fileext = ".yaml")
  writeLines(c("app:", "  admin_password: keep-me", "auto_reading:", "  spot_check_rate: 0"), p)
  expect_true(save(0.05, p))
  y <- yaml::read_yaml(p)
  expect_identical(y$app$admin_password, "keep-me"); expect_equal(y$auto_reading$spot_check_rate, 0.05)
  writeLines(c("app: [", "  broken"), p); before <- readLines(p)
  expect_false(isTRUE(save(0.1, p)))
  expect_identical(readLines(p), before)
  # off by default, as the product owner decided
  expect_equal(.config_defaults()$auto_reading$spot_check_rate %||% 0, 0)
  expect_match(.src_block(src, "observeEvent\\(input\\$adm_spot_save, \\{", 8L), "req\\(admin_ok\\(\\)\\)")
})

test_that("every new Admin output is gated on the session, not on the tab being hidden", {
  src <- .ui_src()
  for (h in c("output\\$adm_banks <- renderDT", "output\\$adm_bank_head <- renderUI",
              "adm_bank_layouts <- reactive", "adm_fix_list <- reactive", "output\\$adm_train_status <- renderUI",
              "adm_ar <- reactive", "output\\$adm_ar_export_ui <- renderUI"))
    expect_match(.src_block(src, h, 4L), "req\\(admin_ok\\(\\)\\)", info = h)
})

test_that("the result page's state has exactly one definition", {
  src <- .ui_src()
  blk <- .ui_block(src, "show_result <- function", 16L)
  for (setter in c("cv_res\\(res\\)", "cv_src\\(src\\)", "cv_upload_id\\(upload_id\\)",
                   "cv_feed_gate\\(gate\\)", "cv_recorded\\(isTRUE\\(recorded\\)\\)",
                   "cv_fb_done\\(FALSE\\)", "cv_fb_rec\\(NULL\\)", "cv_spot_done\\(NA_character_\\)",
                   "cv_ov\\(NULL\\); cv_ck_note\\(NULL\\)"))
    expect_match(blk, setter)
  expect_match(.ui_block(src, "open_batch_row <- function", 12L), "show_result\\(b\\$result\\[\\[i\\]\\]")
  expect_match(.ui_block(src, "run_batch <- function", 65L), "show_result\\(\\)")
  wr <- function(nm) sum(grepl(sprintf("^\\s*%s\\(", nm), src))
  expect_equal(wr("cv_res"), 1L)
  expect_equal(wr("cv_src"), 1L)
  expect_equal(wr("cv_feed_gate"), 2L)    # show_result + publish_result
  expect_equal(wr("cv_recorded"), 2L)
  expect_length(grep('DTOutput\\("cv_txns"\\)', src), 1L)
  expect_length(grep('uiOutput\\("cv_downloads"\\)', src), 1L)
})

test_that("a case re-reads only the files whose bank changed, and hands back the rest", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  again <- .src_block(src, "plan_again <- function\\(\\)", 6L)
  expect_match(again, "if \\(length\\(ch\\)\\) ch else NULL")
  rb <- .src_block(src, "run_batch <- function\\(files, banks = NULL, rows = NULL\\)", 60L)
  expect_match(rb, "paths <- as\\.character\\(b_old\\$file\\[rows\\]\\)")
  expect_match(rb, "banks <- banks\\[rows\\]")
  expect_match(joined, "for \\(col in names\\(b\\)\\) bb\\[\\[col\\]\\]\\[rows\\] <- b\\[\\[col\\]\\]")
  btn <- .src_block(src, "output\\$cv_go_btn <- renderUI", 40L)
  expect_match(btn, 'sprintf\\("Convert %d changed file%s"')
  expect_match(btn, 'sprintf\\("Convert all %d again", n\\)')
  dl <- .ui_block(src, "output\\$cv_batch_dl <- downloadHandler", 30L)
  expect_match(dl, "\\.batch_outputs\\(cv_batch\\(\\)\\)")
  expect_match(dl, "could not be packed into one file")
  expect_match(.src_block(src, "output\\$cv_plan <- renderUI", 200L),
               'if \\(length\\(\\.batch_outputs\\(b\\)\\)\\)\\s+downloadButton\\("cv_batch_dl"')
})

test_that("a stopped case keeps nothing that no longer describes what is on disk", {
  src <- .ui_src()
  st <- .src_block(src, "observeEvent\\(input\\$cv_stop, \\{", 30L)
  expect_match(st, "cv_slot\\$cancel\\(\\); cv_run\\(NULL\\)")
  expect_match(st, 'b\\$status\\[i\\] <- "stopped"')
  expect_match(st, "r\\$outputs <- character\\(0\\)")
  expect_match(st, "ran\\$expected\\[run\\$rows\\] <- NA_character_")
  expect_match(st, "cv_plan_ran\\(NULL\\)")
  expect_match(.src_block(src, "output\\$cv_plan <- renderUI", 200L),
               'openable <- case_res && !running && !identical\\(as\\.character\\(b\\$status\\[i\\]\\), "stopped"\\)')
})

test_that("a scan's first pages are read off-process, and Convert stops the reading", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "plan_slot <- job_slot\\(\\)")
  st <- .src_block(src, "plan_scan_start <- function\\(\\)", 25L)
  expect_match(st, 'plan_slot\\$start\\("identify_scans"')
  expect_false(grepl("cv_slot", st, fixed = TRUE))
  go <- .src_block(src, "observeEvent\\(input\\$cv_go, \\{", 60L)
  expect_match(go, "plan_slot\\$cancel\\(\\); plan_scan_apply\\(final = TRUE\\)")
  expect_match(.src_block(src, "plan_start_check <- function\\(\\)", 6L), "plan_slot\\$cancel\\(\\)")
  # a scan whose pages were read fills its bank in like any other file
  expect_match(.src_block(src, "plan_scan_apply <- function", 25L), "\\.plan_bank_fields\\(rows, i, id\\)")
})

test_that("the purge asks first, and says what it will destroy", {
  src <- .ui_src()
  blk <- .ui_block(src, "observeEvent\\(input\\$adm_purge_uploads", 30L)
  expect_match(blk, "showModal\\(modalDialog")
  expect_match(blk, "permanently deletes")
  expect_match(.ui_block(src, "observeEvent\\(input\\$adm_purge_confirm", 4L), "req\\(admin_ok\\(\\)\\)")
  expect_match(paste(src, collapse = "\n"), "\\.uploads_due <- function")
})

test_that("a high-severity diagnosis nobody can fix on Please check takes the headline", {
  bd <- .ui_fun(".blocking_diag")
  mk <- function(cat, sev, own) data.frame(category = cat, severity = sev,
    detail = paste(cat, "happened"), how_to_fix = "do this", fix_owner = own,
    stringsAsFactors = FALSE)
  expect_equal(bd(list(diagnostics = mk("scanned_no_ocr", "high", "input")))$category, "scanned_no_ocr")
  expect_equal(bd(list(diagnostics = mk("sign_scan_unavailable", "high", "escalate")))$category,
               "sign_scan_unavailable")
  # a reading fault is mended on Please check, so it is not blocking
  expect_null(bd(list(diagnostics = mk("not_read", "high", "reading"))))
  expect_null(bd(list(diagnostics = mk("date_out_of_range", "medium", "input"))))
  expect_null(bd(list()))
  expect_identical(unname(.DIAG_FIX_OWNER[["not_read"]]), "reading")
  expect_match(.ui_block(.ui_src(), "output\\$cv_status <- renderUI", 30L),
               "if \\(!is\\.null\\(bdx\\)\\) headline <- \\.sentence\\(bdx\\$detail\\[1\\]\\)")
})

test_that("what to check leads with the highest-severity diagnostic, and carries the action", {
  top <- .ui_fun("top_diagnostics")
  d <- data.frame(where = c("upload", "dates"),
                  category = c("multiple_statements", "date_out_of_range"),
                  severity = c("high", "medium"),
                  detail = c("the balance block appears 2 times", "34 date(s) outside period"),
                  how_to_fix = c("Split it into one statement per file and re-run.", "..."),
                  stringsAsFactors = FALSE)
  got <- top(list(status = "needs_review", diagnostics = d))
  expect_identical(got$category, "multiple_statements")
  expect_identical(nrow(top(list(status = "unsupported", diagnostics = d))), 0L)
  blk <- .ui_block(.ui_src(), "failed_checks_ui <- function", 34L)
  expect_match(blk, "plain_diag\\(dg\\$category\\[i\\]\\)")
  expect_match(blk, "Do this first: ")
})

test_that("no raw code reaches the verdict card, and no sentence is said twice", {
  strip <- .ui_fun("plain_messages", consts = ".AUDIT_GAP_RX")
  expect_identical(strip("needs_review: 2 different readings of the columns all fit the arithmetic; check the reading, then confirm it or set the columns' roles"),
                   "2 different readings of the columns all fit the arithmetic")
  expect_identical(strip("unsupported: The balance does not add up at row 1; check the columns on Please check, or set the file aside"),
                   "The balance does not add up at row 1")
  expect_identical(strip("needs_review: parsed 3 row(s); 1 KPI(s) failed: amount_direction"), "parsed 3 row(s)")
  expect_identical(strip(NULL), character(0))
  vl <- .ui_fun(".verdict_lines", also = c("plain_messages", ".sentence"), consts = ".AUDIT_GAP_RX")
  res <- list(reason = "Two readings fit.", bank = list(why = "Please pick the bank."),
              messages = c("needs_review: Two readings fit; check the reading, then confirm it or set the columns' roles",
                           "Please pick the bank.", "Something else."))
  expect_identical(vl(res), "Something else.")
  expect_identical(vl(list(messages = "ok: 6 row(s); The running balance checks.")), "The running balance checks.")
})

test_that("a conversion with no audit record says so, on both cards, and is not green", {
  gap <- .ui_fun(".audit_gap", consts = ".AUDIT_GAP_RX")
  msg <- paste0("needs_review: this conversion was not recorded in the audit log; ",
                "tell whoever looks after this server before the file is relied on")
  expect_true(gap(list(status = "ok", messages = msg)))
  expect_false(gap(list(status = "ok", messages = "ok: 6 row(s)")))
  src <- .ui_src()
  hero <- .ui_block(src, "output\\$cv_headline <- renderUI", 40L)
  expect_match(hero, 'if \\(\\.audit_gap\\(res\\)\\) \\{ lvl <- "medium"; icon <- "!" \\}')
  expect_match(hero, "\\.audit_note\\(res\\)")
  expect_match(.ui_block(src, "output\\$cv_status <- renderUI", 30L), "\\.audit_note\\(res\\)")
  expect_match(.ui_block(src, "plain_messages <- function\\(m\\)", 24L), "m\\[!grepl\\(\\.AUDIT_GAP_RX, m\\)\\]")
})

test_that("an empty checks or coverage table says why it is empty", {
  why <- .ui_fun(".why_empty")
  expect_match(why(list(status = "failed"), "nothing to check"), "^Nothing was read from this file, so there is nothing to check\\.$")
  expect_match(why(list(status = "unsupported"), "no field coverage to report"),
               "^Nothing usable was read from this statement, so there is no field coverage to report\\.$")
})

test_that("Admin's uploads table is the whole log, with the layout each was read with", {
  src <- .ui_src()
  expect_match(.ui_block(src, 'h4\\("Uploads', 3L), "every document converted here")
  up <- .src_block(src, "output\\$adm_uploads <- renderDT", 30L)
  expect_match(up, '"Layout"'); expect_match(up, '"Nothing usable was read"')
  expect_match(up, "\\.layout_name\\(r\\)")
  # a saved upload, or a file in failed/, is read again on Convert -- where Please check is
  rr <- .src_block(src, "\\.reread_on_convert <- function", 8L)
  expect_match(rr, 'updateTabsetPanel\\(session, "main_tabs", selected = "Convert"\\)')
  expect_match(rr, "run_conversion\\(path, name, record = FALSE, upload_id = upload_id\\)")
  for (h in c("observeEvent\\(input\\$adm_up_reread, \\{", "observeEvent\\(input\\$adm_inbox_reread, \\{"))
    expect_match(.src_block(src, h, 4L), "req\\(admin_ok\\(\\)\\)", info = h)
})

test_that("Admin's bulk audit audits and never converts, saves or learns", {
  src <- .ui_src()
  expect_match(.src_block(src, 'adm_slot\\$start\\("audit"', 4L), "args = list\\(layouts_dir = LAYOUTS_DIR\\)")
  expect_true(identical(names(formals(batch_audit)), c("paths", "layouts_dir")))
  expect_match(.ui_block(src, 'h4\\("Check a pile of files at once"\\)', 2L),
               "Nothing is converted, saved or learned", fixed = TRUE)
})

test_that("the sample is a statement the reader proves on its own", {
  src <- paste(.ui_src(), collapse = "\n")
  expect_match(src, 'SAMPLE_STATEMENT <- file\\.path\\("samples", "raw", "tutorial", "sample_everyday_statement\\.pdf"\\)')
  p <- file.path(engine_root(), "samples", "raw", "tutorial", "sample_everyday_statement.pdf")
  skip_if_not(file.exists(p))
  d <- tempfile("smp_"); dir.create(d)
  r <- convert_statement(p, outdir = d, logdir = file.path(d, "logs"), layouts_dir = file.path(d, "ly"),
                         tracking_dir = NA, formats = "csv", requested_by = "tester")
  expect_identical(r$status, "ok")
  expect_identical(r$feed_basis, "proven")
})
