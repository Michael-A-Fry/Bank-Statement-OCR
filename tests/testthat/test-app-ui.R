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
.ui_block <- function(src, pat, n = 40L) {
  i <- grep(pat, src)
  testthat::expect_true(length(i) >= 1)
  paste(src[i[1]:min(i[1] + n, length(src))], collapse = " ")
}
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
                "details\\.adv-bank", "#ss-busy", "body\\.ss-run", "\\.split-table"))
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

# On an unsupported result the engine still carries a template id -- the CLOSEST
# MISS, kept for the logs. Printing it under "No template for this statement yet"
# read as though that template had read the file. It had not.
test_that("a template is only named when one actually read the statement", {
  src <- .ui_src()
  # 40, not 30: the block now also picks the headline for an ambiguous (tied)
  # result. The window only has to reach the end of cv_status, which is 34 lines.
  block <- .ui_block(src, "output\\$cv_status <- renderUI", 58L)
  expect_match(block, 'st %in% c\\("ok", "needs_review"\\)')
  expect_match(block, "friendly_tpl\\(tid\\)")          # a name, not an internal id
  # and the form panel no longer repeats the same id underneath it
  joined <- paste(src, collapse = "\n")
  expect_false(grepl("Read as a <b>form / labelled-value PDF</b>", joined, fixed = TRUE))
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
# The teaching journey. "No rows detected" in grey monospace is where a template
# gets abandoned, so it must say what to reach for -- and admit that the PDF
# preview only reads the first few pages, which is why a statement whose table
# starts later looks empty here.
test_that("the toolkit preview says what to do when it reads nothing", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, 'uiOutput\\("g_status"\\)')
  expect_false(grepl('verbatimTextOutput("g_status")', joined, fixed = TRUE))
  # 70, not 45: the "you removed the date column" branch now stands in front of
  # these sentences. The assertions are unchanged - the window reaches them again.
  block <- .ui_block(src, "output\\$g_status <- renderUI", 70L)
  expect_match(block, "No transaction rows read yet", fixed = TRUE)
  expect_match(block, "only the first few pages", fixed = TRUE)
  expect_match(block, "date format", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Page pickers. Typing a page the document doesn't have used to leave a blank
# panel with nothing to explain it -- on the X-ray, in the toolkit and in the
# form builder, each of which derived the page number for itself.
test_that("page numbers are clamped to the document, in one place", {
  joined <- paste(.ui_src(), collapse = "\n")
  expect_match(joined, "\\.clamp_page <- function\\(v, n\\)")
  # nobody re-derives a page number by hand any more
  expect_false(grepl("max\\(1L, as\\.integer\\(input\\$[A-Za-z_]*page", joined))
  # every page box says how many pages there are
  expect_match(joined, "Page \\(1 to %d\\)")
  # Three page boxes: the toolkit, the X-ray and the one builder for everything
  # that is not a statement. The count is the inventory - a new page box that does
  # not say how many pages there are is exactly what this catches.
  expect_equal(length(gregexpr("Page \\(1 to %d\\)", joined)[[1]]), 3L)
})

# ---------------------------------------------------------------------------
# The 'Other document' builder asked for the file a second time, so the picker at
# the top of the page did nothing at all on that path and nothing said so.
test_that("the builder uses the document already uploaded, and never asks twice", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "rb_doc <- reactive")
  expect_match(.ui_block(src, "rb_doc <- reactive", 8L), "input\\$ts_file")
  # There is no second file picker on this page at all any more. The builder used
  # to carry its own ("draw on a different PDF instead"), which meant the picker
  # at the top of the page did nothing on that path and nothing said so.
  expect_false(any(grepl('fileInput\\("(fb|rb)_sample"', src)))
  expect_match(.ui_block(src, "output\\$rb_has_doc <- reactive", 2L), "rb_doc\\(\\)")
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
# Prominence: on the page whose whole promise is "add a bank by pointing and
# clicking", the thing to click must come before the reading matter about it.
test_that("the in-app guide points at the tab the phrase box is actually on", {
  # The 2-minute guide is the ONLY how-to a user has (no access to docs/), so it
  # must not send her to the wrong tab. The identifying phrase moved to Simple.
  p <- file.path(engine_root(), "ui_content.R")
  skip_if_not(file.exists(p))
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_false(grepl("identifying phrases (Advanced tab)", txt, fixed = TRUE))
  expect_match(txt, "distinctive phrase \\(<b>Simple</b> tab\\)")
  # ...and it names the same first-few-pages limit the toolkit now shows inline
  expect_match(txt, "first few pages", fixed = TRUE)
})

test_that("Add a template leads with the action, not the guide", {
  src <- .ui_src()
  i_go   <- grep('actionButton\\("ts_go"', src)[1]
  i_help <- grep('actionLink\\("ts_help"', src)[1]
  expect_false(is.na(i_go) || is.na(i_help))
  expect_true(i_go < i_help)
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
# WHAT IS NEEDED EVERY TIME MUST NOT BE HIDDEN. The date format and the amount
# style are the two settings an analyst reports changing on almost every template
# she builds - the drafter guesses them and is wrong often enough to matter. They
# were briefly moved behind the "show the settings" disclosure with everything
# else, which put a click on the MOST common path rather than the rarest.
# Frequency beats how technical a control looks.
test_that("date format and amount style are in front, not behind the disclosure", {
  src <- .ui_src()
  i_date <- grep('selectInput\\("g_date"', src)
  i_sign <- grep('selectInput\\("g_sign"', src)
  i_more <- grep('uiOutput\\("g_more_toggle"\\)', src)
  expect_length(i_date, 1L); expect_length(i_sign, 1L)
  expect_gt(length(i_more), 0L)
  # both appear BEFORE every disclosure toggle on the toolkit panel
  expect_true(all(i_date < max(i_more)), info = "date format fell behind the disclosure")
  expect_true(all(i_sign < max(i_more)), info = "amount style fell behind the disclosure")
})

test_that("controls that only exist because of the amount style follow it out", {
  # Picking "a D/C column" or "unsigned" must not send the user hunting for where
  # to say what D means, or what a bare number is.
  src <- .ui_src()
  i_sign <- grep('selectInput\\("g_sign"', src)[1]
  i_more <- max(grep('uiOutput\\("g_more_toggle"\\)', src))
  for (id in c("g_unsigned_default", "g_type_debit", "g_type_credit")) {
    i <- grep(sprintf('"%s"', id), src, fixed = FALSE)
    expect_gt(length(i), 0L)
    expect_true(i[1] > i_sign && i[1] < i_more,
                info = paste(id, "is not beside the amount style it depends on"))
  }
})

test_that("the disclosure describes what it actually still holds", {
  joined <- paste(.ui_src(), collapse = "\n")
  expect_false(grepl("How the dates are written, how amounts are shown", joined, fixed = TRUE))
  expect_match(joined, "Identifying phrase, save name", fixed = TRUE)
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

# ---------------------------------------------------------------------------
# THE BANK PICKER IS IN FRONT. It sat inside "It picked the wrong bank?" on the
# assumption detection usually gets it right. On real statements it does not yet,
# which makes the override one of the most-used controls on the page - the same
# frequency argument as the date format. Auto-detect stays the default, so nobody
# is asked a question they do not have to answer; it is simply visible when they
# do. This test fails if it is ever hidden again.
test_that("the bank picker is on the page, not behind a disclosure", {
  src <- .ui_src()
  i_bank <- grep('selectInput\\("cv_bank_quick"', src)
  # WAS: tags$summary("It picked the wrong bank?"). Register L1 ("a form or report
  # template cannot be forced, anywhere in the product") made that panel serve both
  # routes, so its summary can no longer name a bank -- on the other route there is
  # not one. The RULE this test exists for is untouched: the bank picker is in
  # front of the disclosure, never inside it.
  i_adv  <- grep('tags\\$summary\\("It picked the wrong template\\?"\\)', src)
  expect_length(i_bank, 1L)
  expect_length(i_adv, 1L)
  expect_true(i_bank < i_adv, info = "the bank picker fell back behind the disclosure")
  # auto-detect is still the default: the tool answers it unless told otherwise
  expect_match(paste(src[i_bank:(i_bank + 2)], collapse = " "),
               '"Detect automatically" = ""', fixed = TRUE)
})

# REWRITTEN. This used to assert the exact line
#     bank_choice <- reactive(pick(input$cv_bank_quick) %||% pick(input$cv_bank))
# as proof that "the visible bank picker reaches the conversion". It did -- but
# that line was also a BUG, and the test was pinning it in place. There were TWO
# bank pickers: cv_bank_quick in front, and cv_bank inside "It picked the wrong
# bank?", with separate state and two spellings of "no bank". Because the front
# one was read first, a bank chosen inside the disclosure was silently discarded
# whenever the front one named a different bank -- the tool converted with a bank
# the user had stopped asking for and never said so. Confirmed in the browser
# before the fix: front=ANZ + disclosure=ASB left the exact-template list showing
# ANZ's four templates. The second picker is gone; the rule is now the stronger
# one it was always trying to express -- ONE control, read in ONE place.
test_that("there is exactly one bank picker, and it reaches the conversion", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_length(grep("selectInput\\(\"cv_bank", src), 1L)     # one, and it is cv_bank_quick
  expect_length(grep('selectInput\\("cv_bank_quick"', src), 1L)
  expect_match(joined, "bank_choice <- reactive\\(pick\\(input\\$cv_bank_quick\\)\\)")
  # the second picker and its list-builder are gone entirely, not merely unread
  expect_false(grepl("cv_bank_ui", joined, fixed = TRUE))
  expect_false(grepl('selectInput("cv_bank"', joined, fixed = TRUE))
  # ...and with one spelling of "no bank" left, pick() no longer needs the literal
  # "(auto-detect)" sentinel that only the deleted, unnamed-choices picker produced
  expect_match(joined, "pick <- function\\(v\\) if \\(is.null\\(v\\) \\|\\| !nzchar\\(v\\)\\) NULL else v")
  # every remaining control pick() reads uses a NAMED "" choice, so no control can
  # hand a label through as if it were a value
  for (id in c("cv_bank_quick", "cv_template")) {
    i <- grep(sprintf('selectInput\\("%s"', id), src)
    expect_match(paste(src[i:(i + 2)], collapse = " "), '= ""', fixed = TRUE, info = id)
  }
})

# ---------------------------------------------------------------------------
# THE EVIDENCE VIEW OPENS ITSELF WHEN SOMETHING IS FLAGGED. Reported as the most
# useful view the moment something has gone wrong - which is exactly when nobody
# should have to know a link exists. A clean run still opens lean.
test_that("a flagged result opens the evidence view without being asked", {
  joined <- paste(.ui_src(), collapse = "\n")
  expect_match(joined, "cv_detail_touched <- reactiveVal\\(FALSE\\)")
  expect_match(joined, "observeEvent\\(cv_res\\(\\), \\{")
  # ...but never against someone who closed it deliberately
  expect_match(joined, "if \\(isTRUE\\(cv_detail_touched\\(\\)\\)\\) return\\(\\)")
  # opens on a non-ok status OR any failing check
  expect_match(joined, 'any\\(k\\$status %in% "fail"\\)')
})

# ---------------------------------------------------------------------------
# A TIE THAT CONVERTED IS NOT A QUESTION. R/convert.R picks deterministically
# (tested over hand-built), reads the statement and holds the run at
# needs_review. The verdict card was replacing "Converted - please double-check
# it" with "More than one template fits - pick which one" on those runs -- on a
# screen with no picker anywhere on it (cv_tie_pick renders only when the status
# is `unsupported`) -- and suppressing the confidence grade the engine had already
# computed for real rows. The headline now follows the picker.
test_that("the tie headline only appears where there is a pick to make", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_status <- renderUI", 58L)
  expect_match(blk, 'ambig <- isTRUE\\(res\\$detect\\$ambiguous\\) && identical\\(st, "unsupported"\\)')
  # ...and the confidence grade is no longer withheld from an ambiguous run that
  # converted: `graded` turns on the status alone.
  expect_match(blk, 'graded <- st %in% c\\("ok", "needs_review"\\)')
  expect_false(grepl("graded <- !ambig", blk, fixed = TRUE))
  # the picker's own gate is the same condition, so the two cannot drift
  expect_match(.ui_block(src, "output\\$cv_teach <- renderUI", 45L),
               'identical\\(st, "unsupported"\\)')
})

# The engine's messages carry machine codes for the LOG. ui_labels.R's own note
# says a raw code on screen "is the moment a forensic reviewer stops trusting the
# screen", and the verdict card was printing "2 KPI(s) failed:
# balance_reconciliation, running_balance_continuity" -- the same checks
# failed_checks_ui() lists directly underneath in plain words with figures.
test_that("no raw check code reaches the verdict card", {
  src <- .ui_src()
  strip <- .ui_fun("plain_messages", consts = ".AUDIT_GAP_RX")
  expect_identical(strip("needs_review: parsed 22 row(s) but review needed; 2 KPI(s) failed: balance_reconciliation, running_balance_continuity"),
                   "parsed 22 row(s) but review needed")
  expect_identical(strip("ok: matched anz_everyday_csv, 7 row(s), trust medium"),
                   "matched anz_everyday_csv, 7 row(s), trust medium")
  expect_identical(strip("needs_review: parsed 3 row(s) but review needed; 1 KPI(s) not applicable: dates_within_period; all applicable checks passed"),
                   "parsed 3 row(s) but review needed; all applicable checks passed")
  expect_identical(strip(character(0)), character(0))
  expect_identical(strip(NULL), character(0))
  # a message that was ONLY a KPI clause disappears rather than leaving an empty <p>
  expect_identical(strip("needs_review: 1 KPI(s) failed: amount_direction"), character(0))
  # and the card really uses it
  expect_match(.ui_block(src, "output\\$cv_status <- renderUI", 58L),
               "plain_messages\\(res\\$messages\\)")
})

# ---------------------------------------------------------------------------
# "Untick to hide a layer" has to work at the boundary. Both the picture and the
# legend read `input$ix_layers %||% <all six>`, and an empty checkboxGroupInput
# sends NULL -- so unticking every box drew a byte-identical plot to ticking every
# box. The label promised the opposite of what happened.
test_that("unticking every X-ray layer really hides every layer", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "ix_layers_now <- reactive")
  # nobody falls back to "all six" on an empty selection any more
  expect_false(grepl('input$ix_layers %||% c("cols"', joined, fixed = TRUE))
  # both readers go through the one helper
  expect_equal(length(grep("layers <- ix_layers_now\\(\\)", src)), 2L)
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
  # the batch panel is rendered on Convert, above the result it opens
  i_batch <- grep('DTOutput\\("cv_batch"\\)', src)
  i_status <- grep('uiOutput\\("cv_status"\\)', src)
  expect_length(i_batch, 1L)
  expect_true(i_batch < min(i_status))
})

test_that("every file in a batch goes through the engine's own batch loop", {
  # There must be no second conversion pipeline in the UI: a batch answer and a
  # single-file answer for the same statement have to be the same code.
  #
  # REWRITTEN for the move off this process. The loop is unchanged and is still
  # R/batch.R's; what changed is WHICH PROCESS runs it (R/jobs.R, job_run_task),
  # so the assertion follows it there rather than going quietly vacuous scanning
  # an app.R that no longer holds the call.
  joined <- paste(.ui_src(), collapse = "\n")
  jobs <- paste(readLines(file.path(engine_root(), "R", "jobs.R"), warn = FALSE), collapse = "\n")
  expect_match(joined, 'cv_slot\\$start\\("batch", paths, sess')     # app.R asks for it
  expect_match(jobs, "convert_batch, c\\(list\\(paths\\), args,")   # ...and this runs it
  expect_true(exists("convert_batch"))
  expect_true(exists("batch_summary"))
  expect_true(exists("BATCH_STATUSES"))
  # ...and neither side calls the single-file front door in a loop of its own
  for (s in c(joined, jobs))
    expect_false(grepl("for \\([a-z]+ in seq_along\\(paths\\)\\)[^\n]*convert_document", s))
  # THE SEAM test-batch.R guards, asserted where the arguments now are: anything
  # convert_batch does not itself take goes on to convert_document(), which has no
  # `...`, so one stale argument fails EVERY file in the case.
  blk <- .ui_block(.ui_src(), 'cv_slot\\$start\\("batch", paths, sess', 12L)
  expect_false(grepl("keep_rows", blk, fixed = TRUE))
  a <- sub("(?s).*args = list\\(", "", blk, perl = TRUE)
  a <- sub("(?s)\\),\\s*finish =.*", "", a, perl = TRUE)
  sent <- unique(unlist(regmatches(a, gregexpr("[A-Za-z_][A-Za-z_0-9]*(?= =)", a, perl = TRUE))))
  expect_gt(length(sent), 4L)                       # the scan must not go quiet
  expect_identical(setdiff(sent, names(formals(convert_document))), character(0))
  expect_true("outdir" %in% names(formals(convert_document)))   # job_start adds this one
})

test_that("the QID is asked once for the whole batch, before it starts", {
  # REWRITTEN for the shared gate. The check used to be written out inside
  # cv_go, and this test read it there; it is now .identity_ok(), because the
  # sample button was a second way into a conversion that had no gate at all.
  # Same invariant, asserted where it now lives: the gate answers before the
  # batch starts, and it is the one place the question is asked.
  src <- .ui_src()
  i_go   <- grep("observeEvent\\(input\\$cv_go, \\{", src)
  expect_length(i_go, 1L)
  blk <- src[i_go:(i_go + 25)]
  i_qid   <- grep("\\.identity_ok\\(\\)", blk)[1]
  i_batch <- grep("run_batch\\(f\\)", blk)[1]
  expect_false(is.na(i_qid) || is.na(i_batch))
  expect_true(i_qid < i_batch, info = "the batch starts before who-ran-this is settled")
  # ...and the gate really is the QID question
  expect_match(.ui_block(src, "\\.identity_ok <- function", 8L), "is\\.na\\(cv_qid\\(\\)\\)")
  # asked once, not once per file: nothing inside run_batch asks again. Counted
  # over the LINES, because gregexpr on one joined string returns a length-1
  # vector holding -1 when the sentence is not there at all -- so the old count
  # passed just as happily if the wording had gone.
  hits <- grep("Enter your QID first", .ui_src(), fixed = TRUE)
  expect_length(hits, 1L)
})

# REWRITTEN, because the thing it was guarding got a name. It used to list the
# four reactives open_batch_row() had to set by hand and check they were all
# there -- a checklist kept in a test because the code had no single place to keep
# it. Three functions each set an overlapping subset of "the result page's state",
# and the failure mode is silent: one reactive left behind shows the PREVIOUS
# statement's feed verdict or feedback panel beside this statement's figures.
# There is now ONE definition, show_result(), and the checklist lives there. (It
# caught a real one: a finished batch left cv_feed_gate/cv_recorded holding the
# last file in the loop.)
test_that("the result page's state has exactly one definition", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  blk <- .ui_block(src, "show_result <- function", 14L)
  # every piece of it, in one function
  for (setter in c("cv_res\\(res\\)", "cv_src\\(src\\)", "cv_upload_id\\(upload_id\\)",
                   "cv_feed_gate\\(gate\\)", "cv_recorded\\(isTRUE\\(recorded\\)\\)",
                   "cv_fb_done\\(FALSE\\)", "cv_fb_rec\\(NULL\\)", "cv_forced\\(list\\(\\)\\)"))
    expect_match(blk, setter)
  # ...and all three routes onto the result page go through it: a single
  # conversion, a batch finishing with no row open, and a batch row being opened
  expect_gte(length(grep("^\\s*show_result\\(", src)), 3L)
  expect_match(.ui_block(src, "open_batch_row <- function", 12L), "show_result\\(b\\$result\\[\\[i\\]\\]")
  # 45 -> 60: run_conversion now LAUNCHES and hands the tail of the flow to a
  # continuation, so the same one line lives a little further down the function.
  expect_match(.ui_block(src, "run_conversion <- function", 60L), "show_result\\(res, list\\(path = src")
  # a batch clears the screen's copy of the LAST file's feed verdict
  expect_match(.ui_block(src, "run_batch <- function", 65L), "show_result\\(\\)")
  # Nothing sets the result page's reactives behind show_result's back. Counted
  # over WRITES only (a line that starts with the call; `res <- cv_res()` is a
  # read). show_result holds one of each; the only other writers are named, and
  # both are deliberate:
  #   * cv_feed_gate / cv_recorded, in publish_result -- the feed's verdict is not
  #     known until the write has happened, so it is the last word on those two;
  #   * cv_res, in the X-ray "add this row" re-run -- which must NOT go through
  #     show_result, because show_result clears cv_forced and the forced row it has
  #     just added is the entire point of that path.
  wr <- function(nm) sum(grepl(sprintf("^\\s*%s\\(", nm), src))
  expect_equal(wr("cv_res"), 2L)          # show_result + the X-ray re-run
  expect_equal(wr("cv_src"), 1L)          # show_result alone
  expect_equal(wr("cv_feed_gate"), 2L)    # show_result + publish_result
  expect_equal(wr("cv_recorded"), 2L)     # show_result + publish_result
  expect_match(.ui_block(src, "publish_result <- function", 6L), "cv_feed_gate\\(gate\\)")
  # and there is exactly ONE transactions table / downloads bar in the whole app
  expect_length(grep('DTOutput\\("cv_txns"\\)', src), 1L)
  expect_length(grep('uiOutput\\("cv_downloads"\\)', src), 1L)
  expect_false(grepl('DTOutput("cv_batch_txns")', joined, fixed = TRUE))
})

test_that("the batch table sorts by what went wrong, by meaning not by spelling", {
  # Sorting IS the feature: every file that failed the same way must gather
  # together so they can be fixed together. Alphabetical order on the verdict
  # would scatter them ("Could not read" before "No template"), so the verdict
  # column sorts off the engine's own worst-last status order instead.
  #
  # The hidden sort key used to be `length(BATCH_STATUSES) + 1L - match(...)`
  # sorted ASCENDING -- a subtraction whose only job was to turn a descending sort
  # into an ascending one. It is now match() sorted DESCENDING: the same order with
  # the arithmetic taken out. Asserted as an ORDER, not as a spelling, so the two
  # cannot be mistaken for each other again.
  #
  # The column POSITIONS are no longer written as literals. A Confidence column
  # was added in the middle of this frame, which shifted every index below it, and
  # a hard-coded `5` would have hidden the wrong column and sorted the table on the
  # wrong key with nothing on screen to notice it. They are addressed by name.
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_batch <- renderDT", 72L)
  expect_match(blk, "BATCH_STATUSES", fixed = TRUE)
  expect_match(blk, "orderData", fixed = TRUE)          # verdict sorts by severity
  expect_match(blk, "failing_check", fixed = TRUE)      # the failure kind is a column
  expect_match(blk, "severity <- match\\(b\\$status, BATCH_STATUSES")
  expect_match(blk, 'at <- function\\(nm\\) which\\(names\\(disp\\) == nm\\) - 1L')
  expect_match(blk, 'order = list\\(list\\(at\\("order"\\), "desc"\\), list\\(at\\("What to check"\\), "asc"\\)\\)')
  expect_match(blk, 'visible = FALSE, targets = at\\("order"\\)')
  expect_false(grepl("length(BATCH_STATUSES) + 1L -", blk, fixed = TRUE))
  # ...and no bare integer target survives, which is the whole point of at()
  expect_false(grepl("targets = 5", blk, fixed = TRUE))
  # the table is sortable and clickable at all
  # WAS: selection = "single". Register J7 ("thirty files, three failed: no way to
  # re-run just those three") names single-select as the fault - the deliverable
  # for a 30-file case was 30 row-clicks and 30 downloads. Clicking one row still
  # opens it (DT's _row_last_clicked); ticking several now means something too.
  expect_match(blk, 'selection = "multiple"', fixed = TRUE)
  # the engine's order really is worst-last, which is what descending re-reads
  expect_identical(BATCH_STATUSES, c("ok", "needs_review", "unsupported", "failed"))
  # ...and the key it builds really does put the worst first when sorted that way,
  # with an unrecognised verdict at the very top (nobody has words for it yet)
  key <- match(c("ok", "failed", "needs_review", "something_new", "unsupported"),
               BATCH_STATUSES, nomatch = length(BATCH_STATUSES) + 1L)
  expect_identical(c("ok", "failed", "needs_review", "something_new", "unsupported")[order(-key)],
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

test_that("no conversion runs in the app's own R process any more", {
  # THE WHOLE CHANGE, in one assertion. An engine call made on this thread freezes
  # every other analyst's browser for as long as it takes -- measured at 65
  # seconds of a dead page while one person converted one scanned statement.
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  # The PARSER's own call tokens, so a comment naming convert_batch() (there are
  # several, and they should stay) can never be mistaken for a call, and a call
  # hidden after a "#" inside a string can never be missed.
  pd <- utils::getParseData(parse(app, keep.source = TRUE))
  calls <- pd$text[pd$terminal & pd$token == "SYMBOL_FUNCTION_CALL"]
  expect_gt(length(calls), 100L)                    # the scan must not go quiet
  expect_identical(
    intersect(calls, c("convert_document", "convert_statement", "convert_batch",
                       "batch_audit", "convert_form")),
    character(0))
  # ...and every launch goes through the one slot object, which caps and queues
  expect_gt(length(grep("\\$start\\(\"(convert|batch|audit)\"", .ui_src())), 3L)
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
# THE TOOL MUST USE WHAT IT ALREADY KNOWS. Choosing a bank and then scrolling a
# hundred other banks' templates is the interface rule broken in the small: the
# answer is already on the screen, so the list should not ask again.
test_that("the exact-template list is narrowed by the bank already chosen", {
  src <- .ui_src()
  # the filter, the bank it filters on and the update all sit in ONE observer
  i <- grep("ov <- template_overview\\(ts\\)", src)
  expect_length(i, 1L)
  blk <- paste(src[max(1L, i - 3L):(i + 10L)], collapse = " ")
  expect_match(blk, "bank <- bank_choice\\(\\)")                      # the same choice
  expect_match(blk, "ov\\[ov\\$bank %in% bank, , drop = FALSE\\]")    # narrowed by it
  expect_match(blk, 'updateSelectInput\\(session, "cv_template"')
  # a selection that no longer belongs to the chosen bank is dropped, never left
  # hidden and still in force
  expect_match(blk, 'selected = if \\(keep %in% ch\\) keep else ""')
})

# ---------------------------------------------------------------------------
# AN ACTION MUST ACT ON THE THING THAT WAS PICKED. Admin's template picker is
# rebuilt whenever the template set changes (a save, a hide, a delete). It used
# to be rebuilt with no `selected`, so selectize fell back to the first option:
# the confirmation was wiped by the picker's own observer, and -- the half with
# teeth -- the NEXT click acted on whatever the picker had jumped to. Measured
# live: "Only USER templates can be hidden" about a shipped template the operator
# never chose. Deleting is the one case where the selection SHOULD fall away,
# and it does, because a deleted id is no longer among the choices.
test_that("rebuilding the Admin template picker keeps the template you picked", {
  src <- .ui_src()
  i <- grep('updateSelectInput\\(session, "adm_tpl_pick"', src)
  expect_gte(length(i), 1L)
  blk <- paste(src[max(1L, min(i) - 6L):(max(i) + 3L)], collapse = " ")
  expect_match(blk, "keep <- isolate\\(input\\$adm_tpl_pick\\)")
  expect_match(blk, "selected = if \\(!is\\.null\\(keep\\) && keep %in% ids\\) keep else NULL")
})

# Hiding decides whether a template is used to read statements AT ALL. It landed
# silently: the inline note was wiped by the rebuild above, and unlike Delete it
# had no toast to survive it. A state change nobody can confirm happened is the
# same class of defect as a silent success anywhere else in this app.
test_that("hiding a template says so somewhere that survives the redraw", {
  src <- .ui_src()
  i <- grep("input\\$adm_tpl_hide", src)
  expect_length(i, 1L)
  blk <- paste(src[i:(i + 34L)], collapse = " ")
  expect_match(blk, 'notify_once\\("adm_tpl_hide"')
  # By its NAME, not its id -- and by the name that is true for whichever kind of
  # template it is. template_display_name() appends "statement" to everything,
  # which is a lie about a report and about a form.
  expect_match(blk, "template_library_name")
  # ...and it hides in the folder THAT KIND lives in, or Hide is a control that
  # does nothing on two of the three kinds.
  expect_match(blk, "\\.adm_user_dir\\(\\.adm_kind\\(id\\)\\)")
})

test_that("the exact-template picker is called exactly 'Template (optional)'", {
  src <- .ui_src()
  i <- grep('selectInput\\("cv_template"', src)
  expect_length(i, 1L)
  expect_match(src[i], '"cv_template", "Template \\(optional\\)"')
})

test_that("the bank and the forced template are read in one place", {
  # Two readings of the same control is how a filter comes to offer a template
  # the conversion would not have used.
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  expect_length(grep("bank_choice <- reactive", src), 1L)
  expect_length(grep("tpl_choice <- reactive", src), 1L)
  # Both are read in convert_args(), which is the ONE place the front door's
  # arguments are assembled -- so the single conversion, a whole case folder and
  # the X-ray's re-run cannot honour different overrides. (This used to be
  # convert_now(), which assembled them AND ran the conversion; running it is now
  # a child process's job, and only the assembling is shared.)
  expect_match(joined, "bank = bank_choice\\(\\)")
  expect_match(joined, "force_template = force_tpl %\\|\\|% tpl_choice\\(\\)")
  expect_length(grep("^\\s*convert_args <- function", .ui_src()), 1L)
  # nothing reads the raw inputs a second time
  expect_length(grep("input\\$cv_bank_quick", src), 2L)   # the update + bank_choice
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
# WHEN A LAYOUT APPLIES. The same bank and product printed differently in 2020
# and 2024 is a real variant; the schema has always had effective_from /
# effective_to and nothing on screen ever showed them, so the only way to have
# both was two rival templates that tie on every statement forever.
# REWRITTEN for the control, not the machinery. The window used to be two free
# TEXT boxes, so "is this even a date?" was a question the screen had to ask,
# answer and refuse: six helpers, a tri-state .eff_date (NULL / NA / string) that
# only worked because length(NULL) == 0, an inline problems list, a read-back
# sentence and a save-time refusal, ~85 lines for two boxes the code itself calls
# rarely needed. They are DATE PICKERS now, which makes a non-date impossible to
# enter, so all of that goes and the FEATURE is untouched: a template can still
# carry effective_from / effective_to, and the one mistake a pair of pickers still
# allows -- an end before its start -- is still named inline and still refused at
# Save. The assertions below are the surviving rules, not the deleted plumbing.
test_that("the validity window is picked, not typed, and sits behind the disclosure", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  i_from <- grep('\\.eff_picker\\("g_eff_from"', src)
  i_to   <- grep('\\.eff_picker\\("g_eff_to"', src)
  i_more <- max(grep('uiOutput\\("g_more_toggle"\\)', src))
  expect_length(i_from, 1L); expect_length(i_to, 1L)
  # rare -> behind the one disclosure, with the rest of the rarely-touched settings
  expect_true(i_from > i_more && i_to > i_more)
  # said in plain words, not as a schema key
  expect_match(src[i_from], "This layout applies from", fixed = TRUE)
  # a date picker, so a non-date cannot be entered at all
  expect_false(any(grepl('textInput("g_eff_', src, fixed = TRUE)))
  expect_match(joined, "\\.eff_picker <- function\\(id, label, v\\)")
  expect_match(joined, "suppressWarnings\\(dateInput\\(id, label,")
})

# THE BOX HAS TO BE ABLE TO BE EMPTY, and shiny::dateInput cannot do that by
# itself: given no value its JS falls back to TODAY. Found by opening the toolkit
# in a browser, where both boxes came up showing today's date on a template that
# declares no window at all -- and a save would then have written a ONE-DAY
# validity window onto every template built here, which R/diagnose.R would use to
# caution against every statement not dated today. Empty is the usual answer, so
# it is the case that must be pinned.
test_that("an empty validity window is reachable, and is what 'always' looks like", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # value = "" is the ONE input shiny's date JS reads as "leave the box alone"
  expect_match(joined, 'value = tryCatch\\(\\.eff_date\\(v\\) %\\|\\|% ""')
  expect_match(joined, "back to TODAY", fixed = TRUE)
  # and putting a box BACK to empty (loading a windowless template over a windowed
  # one) is done explicitly, because updateDateInput drops a NULL value entirely
  expect_match(joined, "\\.eff_set <- function\\(id, v\\) session\\$sendInputMessage")
  expect_length(grep("\\.eff_set\\(\"g_eff_", src), 2L)
  # the two ways of saying "no window" both reach the same one place
  eff <- .ui_fun(".eff_date")
  expect_null(eff(""))            # what the picker is built with
  expect_null(eff(NA))            # what the picker hands back when emptied
  # ...and putting the app's FIRST date box on screen must not throw. Shiny renames
  # bootstrap-datepicker's plugin to bsDatepicker, but the library's own ready hook
  # still calls $(...).datepicker() -- an uncaught TypeError in the middle of a
  # modal full of dynamically inserted inputs, which is exactly where this app has
  # been bitten before by an exception aborting a bind pass. Seen in the browser
  # console the moment the toolkit was opened; the shim below makes the hook the
  # no-op it was meant to be.
  expect_match(joined, "\\$\\.fn\\.datepicker = \\$\\.fn\\.datepicker \\|\\| function")
  expect_match(joined, "\\$\\.fn\\.bsDatepicker\\.apply\\(this, arguments\\)")
})

test_that("the validity window reaches the saved template, and empty means always", {
  apply_eff <- .ui_fun("apply_overrides", also = ".eff_date")
  base <- list(id = "t", bank = "B", format = "delimited",
               columns = list(date = list(source = "Date", format = "%d/%m/%Y"),
                              description = list(source = "Desc"),
                              amount = list(source = "Amt")))
  # a picked range is written as the schema stores it, whether it arrives as the
  # Date a dateInput hands back or as the string the YAML holds
  t1 <- apply_eff(base, bank = NULL, datefmt = NULL, sign = NULL,
                  effective_from = as.Date("2020-01-01"), effective_to = "2024-12-31")
  expect_identical(t1$effective_from, "2020-01-01")
  expect_identical(t1$effective_to, "2024-12-31")
  # an EMPTY picker CLEARS it: a template that says "always" has no key at all,
  # exactly like one that never had one. Shiny hands back NA for an empty date box.
  t2 <- apply_eff(t1, bank = NULL, datefmt = NULL, sign = NULL,
                  effective_from = as.Date(NA), effective_to = "  ")
  expect_false("effective_from" %in% names(t2))
  expect_false("effective_to" %in% names(t2))
  # the control not being on screen at all leaves whatever is there untouched --
  # absence of a control is not an instruction to delete a rule
  t3 <- apply_eff(t1, bank = NULL, datefmt = NULL, sign = NULL)
  expect_identical(t3$effective_from, "2020-01-01")
  # and the engine really does consume these keys, so this is not a dead setting
  expect_true(any(grepl("effective_from",
    readLines(file.path(engine_root(), "R", "diagnose.R"), warn = FALSE), fixed = TRUE)))
})

test_that("a validity window that runs backwards is refused, never quietly saved", {
  back <- .ui_fun(".eff_backwards", also = ".eff_date")
  eff  <- .ui_fun(".eff_date")
  expect_false(back(NA, NA))                             # both empty is fine
  expect_false(back("2020-01-01", NA))
  expect_false(back("2020-01-01", "2024-12-31"))
  expect_false(back("2020-01-01", "2020-01-01"))         # one day wide is a window
  expect_true(back("2024-01-01", "2020-01-01"))
  expect_true(back(as.Date("2024-01-01"), as.Date("2020-01-01")))
  expect_null(eff(NA))                                   # an empty box = always
  expect_null(eff(NULL)); expect_null(eff(""))
  expect_identical(eff(" 2020-01-01 "), "2020-01-01")
  expect_identical(eff(as.Date("2020-01-01")), "2020-01-01")
  # ...and the save is blocked on it, rather than writing a template that applies
  # to no statement ever printed
  src <- .ui_src()
  blk <- .ui_block(src, "observeEvent\\(input\\$g_save", 18L)
  expect_match(blk, "\\.eff_backwards\\(input\\$g_eff_from, input\\$g_eff_to\\)")
  expect_match(blk, "g_more_open\\(TRUE\\)")             # opens where the fix is
  # the same words inline as it is picked, from ONE constant, so the message the
  # box shows and the message the save gives can never drift apart
  expect_match(.ui_block(src, "output\\$g_eff_msg <- renderUI", 24L),
               "\\.eff_backwards\\(input\\$g_eff_from, input\\$g_eff_to\\)")
  expect_length(grep("\\.EFF_BACKWARDS_MSG", src), 3L)   # declared once, used twice
  # The deleted machinery really is gone, not merely unreferenced. Matched on a
  # WORD BOUNDARY, not as a substring: `.eff_show` is a prefix of `.eff_shows`,
  # which is a different, live helper, and a substring test would report the
  # deleted one as still present forever. A guard that cries wolf gets deleted.
  joined <- paste(src, collapse = "\n")
  for (dead in c(".eff_bad", ".eff_txt", ".eff_show", ".eff_problems", ".eff_sentence"))
    expect_false(grepl(paste0("\\Q", dead, "\\E\\b"), joined, perl = TRUE), info = dead)
})

test_that("a validity window the pickers cannot show is refused, and never silently emptied", {
  # A date picker can only hold a date, so a window written any other way has to be
  # dealt with rather than quietly dropped. Two ways in, two answers:
  #   * the Advanced YAML box REFUSES to apply one, so it can never be saved here;
  #   * a template hand-edited on the server still OPENS -- it must, because this
  #     modal is the only place that YAML can be fixed, and a toolkit that will not
  #     open over a bad template locks the repair tool inside the thing it repairs --
  #     with the boxes empty and a line saying so.
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # .eff_shows too: the "can the pickers show this?" question is asked directly
  # now, instead of by catching .eff_date()'s exception - a guard that rested on
  # another function throwing was one tidy-up away from silently disappearing.
  ok <- .ui_fun(".eff_stored_ok", also = c(".eff_date", ".eff_shows"))
  expect_true(ok(list(effective_from = "2020-01-01", effective_to = NULL)))
  expect_true(ok(list()))                                   # no window is fine
  expect_false(ok(list(effective_from = "last year")))
  # ...and the four letters "NA" mean ALWAYS, not a broken window: yaml round-trips
  # an absent value through that string, and a template saying "always" must not
  # open under a red banner telling the user to fix something that is correct.
  expect_true(ok(list(effective_from = "NA")))
  expect_true(ok(list(effective_from = "", effective_to = "")))
  blk <- .ui_block(src, "observeEvent\\(input\\$g_adv_apply", 30L)
  expect_match(blk, "\\.eff_stored_ok\\(parsed\\)")
  expect_match(blk, "must be dates", fixed = TRUE)
  # opening never throws...
  expect_match(joined, "value = tryCatch\\(\\.eff_date\\(v\\) %\\|\\|% \"\", error = function\\(e\\) \"\"\\)")
  # ...and never pretends the template said "always"
  expect_match(.ui_block(src, "output\\$g_eff_msg <- renderUI", 24L),
               "!\\.eff_stored_ok\\(g\\$tmpl\\)")
  expect_match(joined, "saved validity window is not a date", fixed = TRUE)
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

# THE BAND FRAME (R/parse_pdf_table.R) is the one space stored bands live in.
# The editor draws on the page at its OWN size, so it must divide going out and
# multiply coming in. Drawn or stored raw, every page that is not the frame's
# size is displaced - and a displaced band cannot be told from an untouched one.
test_that("the band editor draws and stores in the template's band frame", {
  src <- .ui_src()
  expect_match(.ui_block(src, "g_band_scale <- function", 1L),
               "pdf_band_frame_scale\\(pdf_band_frame\\(")
  drawn <- .ui_block(src, "output\\$g_pdf_plot <- renderPlot", 34L)
  expect_match(drawn, "b\\$x_min / s\\[1\\]")         # divide to draw
  expect_false(grepl("rect(b$x_min, 0", drawn, fixed = TRUE))
  stored <- .ui_block(src, "observeEvent\\(input\\$g_pdf_assign", 30L)
  expect_match(stored, "br\\$xmin \\* s\\[1\\]")      # multiply to store
  expect_false(grepl("round(br$xmin)", stored, fixed = TRUE))
  # and the two engine functions really are exact inverses, for a page that is
  # not the frame's size (the case the whole contract exists for)
  fr <- pdf_band_frame(list(table = list(ref_width = 595.28, ref_height = 841.89)))
  s  <- pdf_band_frame_scale(fr, 1240, 1754)
  expect_lt(s[1], 1)                    # a page BIGGER than the frame scales into it
  expect_equal((100 / s[1]) * s[1], 100)   # draw out, store back: the same band
  # an A4 page on an A4 frame is left bit-for-bit alone
  expect_equal(pdf_band_frame_scale(fr, 595.28, 841.89), c(1, 1))
})

# ---------------------------------------------------------------------------
# N26. Shiny sends a brush on every mouse-move, so nudging a band fought a
# re-read the whole way. A long debounce swallows the mid-drag sends; the mouse
# release flushes immediately (shiny.js calls immediateCall on mouseup). A
# mis-drawn band is the commonest cause of a wrong amount column, so this is a
# correctness fix wearing a usability costume.
test_that("a drawn box commits on release, not on every mouse move", {
  lines <- .ui_src(); src <- paste(lines, collapse = " ")
  num_at <- function(pat) {
    i <- grep(pat, lines)[1]
    if (is.na(i)) NA_integer_ else as.integer(sub("^[^0-9]*([0-9]+).*$", "\\1", lines[i]))
  }
  for (id in c("g_pdf_brush", "rb_brush")) {
    blk <- regmatches(src, regexpr(sprintf('brushOpts\\("%s".{0,120}', id), src, perl = TRUE))
    # A debounce of ANY length is the promise -- one commit when the mouse stops,
    # not one per mouse-move.
    expect_match(blk, 'delayType = "debounce"')
  }
  # THE TOOLKIT waits long, because its drag is the whole gesture and there is
  # nothing else competing for it.
  tk <- as.integer(sub('.*brushOpts\\("g_pdf_brush", delay = ([0-9]+).*', "\\1", src))
  expect_gte(tk, 500L)
  # THE BUILDER CANNOT. Its plot also carries a click, and Shiny sends a plot
  # click on MOUSEDOWN - so the leading edge of every drag arrives as a click and
  # the click has to wait to find out whether a drag followed. The brush must
  # report FIRST, or the drag is read as a click and the gesture is lost, which is
  # exactly the fault that made the builder unusable. So the builder's delay is
  # the named constant, and the only rule is that it is the shorter of the two.
  expect_match(src, "brushOpts\\(\"rb_brush\", direction = \"xy\", delay = \\.RB_BRUSH_WAIT")
  bw <- num_at("^\\.RB_BRUSH_WAIT <- ")
  cw <- num_at("^\\.RB_CLICK_WAIT <- ")
  expect_false(is.na(bw)); expect_false(is.na(cw))
  expect_lt(bw, cw)
  # ...and still long enough to be a debounce rather than a stream of commits.
  expect_gte(bw, 150L)
})

# ---------------------------------------------------------------------------
# N27. The save name was fixed at draft time and built from the BANK alone, so
# every layout one bank issues drafted the same name and the second save
# overwrote the first. Templates that cannot be told apart by name are exactly
# the ones that tie in detection. Both answers are already on screen.
test_that("the save name is built from the bank and the kind of statement", {
  blk <- .ui_block(.ui_src(), "observeEvent\\(list\\(input\\$g_bank, input\\$g_type\\)", 14L)
  expect_match(blk, "\\.compose_id\\(input\\$g_bank, input\\$g_type")
  expect_match(blk, 'updateTextInput\\(session, "g_id"')
  # it stops following the moment she names it herself
  expect_match(blk, "g_id_auto\\(\\)")
  # the engine really composes both halves into the id
  expect_identical(.compose_id("ANZ", "credit card", "pdf", "somefile"), "anz_credit_card_pdf")
  expect_false(identical(.compose_id("ANZ", "everyday", "pdf", "f"),
                         .compose_id("ANZ", "credit card", "pdf", "f")))
})

# ---------------------------------------------------------------------------
# N28. "None of these fit? Tell our team" is the way out for somebody ALREADY
# stuck, and it sat inside the settings disclosure - so to a user it existed only
# on Advanced. Worse, picking "None of these" in a dropdown named a box that was
# not on screen. It belongs in front, on Simple, always.
test_that("the way out is on Simple and outside the settings disclosure", {
  src <- .ui_src()
  hatch <- grep("None of these fit\\? Tell our team", src)
  expect_length(hatch, 1L)
  # the nearest enclosing disclosure must be BEFORE the escape hatch's own hr,
  # so walk back: the last conditionalPanel on g_more_open has to be closed by
  # then. Simplest honest check: the request box is not inside that panel.
  panel <- grep('conditionalPanel\\("output\\.g_more_open == true"', src)
  panel <- panel[panel < hatch]
  expect_true(length(panel) >= 1)
  between <- paste(src[max(panel):hatch], collapse = "\n")
  # the disclosure's block ends (uiOutput("g_eff_msg")),) before the hatch begins
  expect_match(between, 'uiOutput\\("g_eff_msg"\\)\\)')
  # and the notification points at a box that is now genuinely below it
  expect_match(paste(src, collapse = " "), "'Tell our team' box below")
})

# ---------------------------------------------------------------------------
# N31. The form builder opened with the ABSTRACT step - a blank box headed "the
# values to pull out" - and put the one concrete thing a person can do, point at
# a number, last and marked optional. The page now leads with the document.
test_that("the builder leads with the document, not a blank list", {
  src <- .ui_src()
  step <- grep('uiOutput\\("rb_vstep"\\)', src)
  typed <- grep('textAreaInput\\("rb_val_typed"', src)
  expect_length(step, 1L); expect_length(typed, 1L)
  expect_true(step < typed)                        # the concrete step is first
  # and the typed list is behind the disclosure, not in front of it
  det <- grep("Know the wording already\\? Type them instead", src)
  expect_length(det, 1L)
  expect_true(det < typed && typed - det < 4L)
  # nothing is deleted: the shortcut still parses through the same helper
  expect_true(any(grepl("parse_fields_spec\\(input\\$rb_val_typed\\)", src)))
  expect_false(any(grepl("Value printed away from its wording", src, fixed = TRUE)))
})

# A PAIR IS TWO DRAGS, AND NEITHER OF THEM IS A GUESS.
#
# The version before this asked for the label and then GUESSED the value -- the
# next thing along the same line, else the line under it. A guess that is right
# nine times in ten is a habit of not checking, and the tenth is wrong in a file
# nobody re-reads. It also only ever looked right and down, so a value printed to
# the LEFT of its label, or above it, could not be expressed at all.
#
# So: two drags, both asked for out loud, and what is stored is the label's
# wording plus WHICH SIDE the value was on. On the next document the label is
# found by its wording and the search runs in that direction -- which survives a
# longer figure and a wider label, where a fixed offset does not.
test_that("a value is two explicit drags, and the side is stored, not guessed", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  brush <- .ui_block(src, "observeEvent\\(input\\$rb_brush", 190L)
  # the label drag stores the wording and then ASKS for the value
  lab <- .ui_block(src, 'if \\(m == "label"\\)', 26L)
  expect_match(lab, "label_text <- txt", fixed = TRUE)
  expect_match(lab, 'rb\\$mode <- "value"')
  # nothing hunts for the value beside the label any more
  expect_false(grepl("right <- w\\[w\\$cx > box\\$x_max", joined))
  expect_false(grepl("below <- w\\[w\\$cy > box\\$y_max", joined))
  # the value drag works out the side from the two boxes
  val <- .ui_block(src, 'if \\(m == "value"\\)', 16L)
  expect_match(val, "\\.doc_pair_rel\\(v\\$label, box\\)")
  # ...which is SHOWN, in words, before it is saved...
  card <- .ui_block(src, "output\\$rb_vstep <- renderUI", 20L)
  expect_match(card, "\\.doc_pair_where_text\\(rel\\)")
  expect_match(card, "not drawn yet")
  # ...and can be overridden, before AND after saving
  expect_match(joined, 'selectInput\\("rb_vwhere"')
  for (w in c('"right"', '"left"', '"below"', '"above"'))
    expect_match(joined, sprintf("= %s,?", w))
  expect_match(joined, 'observeEvent\\(input\\$rb_vwhere')
  expect_match(joined, 'paste0\\("rb_ved_", i\\), "Edit"')
  # saving refuses a half-drawn pair rather than storing one box
  sv <- .ui_block(src, "observeEvent\\(input\\$rb_vsave", 24L)
  expect_match(sv, "Drag a box round the label first")
  expect_match(sv, "Drag a box round the value first")
})

test_that("a value can be removed, and so can a table and a column", {
  # Nothing built by hand is worth building if it cannot be taken back.
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, 'paste0("rb_vrm_", i), "Remove"', fixed = TRUE)   # a value
  expect_match(joined, 'paste0("rb_rm_", i), "Remove"', fixed = TRUE)    # a table
  expect_match(joined, 'paste0("rb_cx_", j)', fixed = TRUE)              # a column
  for (id in c("rb_vrm_", "rb_rm_", "rb_cx_"))
    expect_match(joined, sprintf('observeEvent\\(input\\[\\[paste0\\("%s", [ij]\\)\\]\\]', id))
})

# ---- N34: the preview's row count is a PARTIAL read, and must say so ---------
# draft_preview stops at PREVIEW_PAGES so the toolkit stays quick on a 46-page
# statement. The verdict then announced that count as "N transaction rows read" -
# so the number the analyst checks her template against is not the number the
# conversion gives her, with nothing on screen saying why.
test_that("the toolkit preview states that it read only the first few pages", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # the page limit and the sentence quoting it come from ONE constant
  expect_match(joined, "PREVIEW_PAGES <- 3L", fixed = TRUE)
  expect_match(joined, "PREVIEW_ROWS  <- 12L", fixed = TRUE)
  expect_false(any(grepl("preview_pages = 3L", src, fixed = TRUE)))   # no stray literal
  expect_false(any(grepl("utils::head(tx, 12)", src, fixed = TRUE)))
  blk <- .ui_block(src, "output\\$g_status <- renderUI", 40L)
  # it is only called partial when the document really is longer than the preview
  expect_match(blk, "np > PREVIEW_PAGES")
  expect_match(blk, "read from the first %d of %d pages")
})

# ---- N35: the first question has to be answerable again ----------------------
# "A statement, or labelled values?" is asked once on the page BEHIND the toolkit
# modal, and not at all when the toolkit is opened from a Convert result. Cancel
# threw the work away without answering it.
test_that("the toolkit offers a way back to the other builder, carrying the file", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, 'actionLink("g_not_statement"', fixed = TRUE)
  blk <- .ui_block(src, "observeEvent\\(input\\$g_not_statement", 12L)
  expect_match(blk, "rb_handoff\\(g\\$path\\)")                 # the file goes with it
  expect_match(blk, "ts_doctype", fixed = TRUE)                # ...and the answer changes
  expect_match(blk, "removeModal\\(\\)")
  # the builder must actually READ the handoff, or the link is theatre
  expect_match(.ui_block(src, "rb_doc <- reactive", 8L), "rb_handoff\\(\\)")
})

test_that("no on-screen text points at a control that is off screen", {
  # The old wording told someone stuck inside the modal to "pick 'Something else'
  # above" - a control the modal is covering. Same mistake as N28.
  src <- .ui_src()
  expect_false(any(grepl("pick 'Something else' above", src, fixed = TRUE)))
  # ...and the draft-failed notification made it a third time: it names a control
  # and then RETURNS without ever showing the window that control lives in, so
  # "Not a transaction table?" (an actionLink built inside showModal) was quoted at
  # someone looking at the Convert page. The branch must not name it.
  #
  # Comments stripped before the assertion: the fix's own note quotes the old
  # wording so a future reader knows what went wrong, and a substring test over
  # the raw block would read that record as a relapse.
  i <- grep("# Fail loud AND specific", src)
  expect_length(i, 1L)
  blk <- src[i:min(i + 34L, length(src))]
  expect_match(paste(blk, collapse = " "), "return\\(invisible\\(FALSE\\)\\)")  # no modal is shown
  blk <- blk[!grepl("^\\s*#", blk)]
  expect_false(any(grepl("Not a transaction table?", blk, fixed = TRUE)))
  expect_false(any(grepl("top of this window", blk, fixed = TRUE)))
  # NOR A RADIO ON A TAB IT DOES NOT OFFER TO OPEN. This message used to name the
  # Add-a-template radio, which is an instruction nobody can follow from here.
  # Where somebody lands here is the override on the green card, so the message
  # names the button that is already in front of them.
  expect_false(any(grepl("Something else", blk, fixed = TRUE)))
  expect_true(any(grepl("Set it up as a report", blk, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# THE CARD MUST NOT GIVE THREE ANSWERS AT ONCE. R/diagnose.R is explicit: a
# template that matched the wording and read nothing means "'Add a template' is
# not the fix -- there IS one, its columns just sit in the wrong place. Send the
# analyst to the template that failed, not to a blank form." The card said "This
# layout is new", the engine's message directly above it named the template that
# matched, and the green button drafted a FRESH template from the file.
test_that("a template that matched and read nothing sends you to that template", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "\\.matched_but_empty <- function")
  expect_match(joined, 'd\\$category %in% "matched_but_empty"')
  blk <- .ui_block(src, "output\\$cv_teach <- renderUI", 80L)
  expect_match(blk, "\\.matched_but_empty\\(res\\)")
  expect_match(blk, "matched this statement but read no rows from it")
  expect_match(blk, "cv_teach_go_empty")
  # ...and that button seeds the toolkit with the template that failed, which is
  # the ONE unsupported result whose template id is a real match, not a near miss
  expect_match(joined, "observeEvent\\(input\\$cv_teach_go_empty, \\.teach_now\\(seed_matched = TRUE\\)\\)")
  expect_match(.ui_block(src, "\\.teach_now <- function", 22L),
               'isTRUE\\(seed_matched\\) \\|\\| \\(res\\$status %\\|\\|% ""\\) %in% c\\("ok", "needs_review"\\)')
  # the engine really raises that category, so this is wired to a fact
  expect_true("matched_but_empty" %in% names(.DIAG_FIX_OWNER))
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

# The bundled specimen is the ONE button offered to somebody with no statement to
# hand, and it must actually convert. It pointed at a tutorial PDF whose template
# carries `sample: true`, which load_template_set() deliberately drops from the
# detection set - so the button answered "No template for this statement yet"
# every time, and forcing the id would not have helped either (convert_document
# looks a forced id up in that same filtered set).
test_that("the sample button converts with a template that is actually loaded", {
  joined <- paste(.ui_src(), collapse = "\n")
  p <- regmatches(joined, regexpr('SAMPLE_STATEMENT <- file\\.path\\([^)]*\\)', joined))
  expect_length(p, 1L)
  f <- eval(parse(text = sub("^SAMPLE_STATEMENT <- ", "", p)))
  skip_if_not(file.exists(file.path(engine_root(), f)))
  tset <- load_template_set(templates_dir(),
                            user_templates_dir())
  det <- detect_statement(read_input(file.path(engine_root(), f)), tset)
  expect_true(isTRUE(det$matched),
              info = "the 'Try it on a sample' file is not recognised by any loaded template")
})


# ---- what the adversarial review found in this sweep -------------------------
test_that("the batch Result column gathers the FAILURES on the first click", {
  # orderData points column 1 at the hidden severity key, and DataTables' default
  # first click is ASCENDING - which on that key puts the CLEAN files on top, on
  # the very click meant to gather what went wrong. The initial order was right
  # and the old test only checked the initial order, so the inversion was
  # invisible. Pin the click direction, not just the opening state.
  blk <- .ui_block(.ui_src(), "output\\$cv_batch <- renderDT", 72L)
  expect_match(blk, 'orderSequence = list\\("desc", "asc"\\)')
  expect_match(blk, 'orderData = at\\("order"\\), targets = at\\("Result"\\)')
})

test_that("the stored-window banner stops claiming the boxes are empty once they are not", {
  # It returned EARLY, so it both stated something false the moment the user
  # picked a date, and hid the backwards warning for exactly the templates the
  # branch was added for - the one mistake a pair of date pickers still allows
  # went unnamed until Save.
  blk <- .ui_block(.ui_src(), "output\\$g_eff_msg <- renderUI", 24L)
  expect_match(blk, "picked <- ")
  expect_match(blk, "!\\.eff_stored_ok\\(g\\$tmpl\\) && !picked")
})

test_that("a template that says ALWAYS does not open looking broken", {
  # yaml round-trips an absent value through the four letters "NA". Without this
  # the red "not a date" banner appears on a template that is perfectly correct.
  expect_match(paste(.ui_src(), collapse = "\n"),
               'identical\\(toupper\\(s\\), "NA"\\)')
})

# ---- the screen sweep: what the page says about itself -----------------------

# INVARIANT 17, ENFORCED. The deployment box is an air-gapped Windows machine in a
# C (non-UTF-8) locale, where a non-ASCII character in an R NAME -- a variable, a
# function, an argument -- is not a style question: the parser cannot read the
# file at all ("invalid multibyte character in parser"), so the app does not
# start. A non-ASCII character in a string VALUE is fine, and the screen is full
# of legitimate ones (the tick and cross in the proof strip, the middot in the
# verdict line), so this reads the PARSE TOKENS and never the text.
#
# Until now the rule was guarded by one comment in app.R, beside the one control
# that had already been bitten by it.
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
  writeLines('x <- c("✓ Correct" = "correct", "· dot" = "d")', ok)
  expect_identical(.nonascii_symbols(ok), character(0))
  # ...and a NAME does not, in either locale: the C-locale parser refuses the
  # file, a UTF-8 one parses it and the token scan catches the symbol.
  bad <- tempfile(fileext = ".R")
  writeLines('café <- 1', bad)
  expect_gt(length(.nonascii_symbols(bad)), 0L)
})

# THE CONFIDENCE LEVEL IS NAMED IN FOUR PLACES THE ANALYST READS -- the About tab,
# both operational guides ("Confidence medium on a PDF" is a troubleshooting row)
# and the README -- and on a clean run it appeared on screen in NONE of them:
# cv_status, the only renderer that printed it, returns NULL the moment a
# statement converts cleanly. So the word existed only when something had gone
# wrong, and there was no way to tell a high run from a medium one.
test_that("the confidence level is on the hero card of every graded run", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_headline <- renderUI", 60L)
  expect_match(blk, "confidence: %s")
  expect_match(blk, "res\\$trust\\$level")
  # and the didn't-go-cleanly card still carries it too, so both halves agree
  expect_match(.ui_block(src, "output\\$cv_status <- renderUI", 58L), "confidence: %s")
})

test_that("a PDF that stops at medium says why medium is the ceiling", {
  src <- .ui_src()
  expect_match(paste(src, collapse = "\n"), "\\.medium_is_the_ceiling <- function")
  blk <- .ui_block(src, "\\.medium_is_the_ceiling <- function", 12L)
  # keyed on the CHECK that cannot run plus the format, not on a guess
  expect_match(blk, "no_unparsed_rows")
  expect_match(blk, 'fmt %in% c\\("pdf", "excel"\\)')
  # ...and the sentence says it is the normal ceiling, not a fault to chase
  expect_match(paste(src, collapse = "\n"), "CEILING_NOTE <- paste")
  expect_match(.ui_block(src, "CEILING_NOTE <- paste", 4L), "Medium is the ceiling")
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

# A clean result read by the WRONG template looks perfect. cv_edit is the only
# route back from that, and nothing rendered it -- while cv_teach stayed silent on
# a clean result precisely BECAUSE it believed that line was on screen.
#
# WAS: cv_rematch, everywhere in this block. Register B1 renames it and WIDENS it
# ("the always-available door is made by widening the existing re-match control
# rather than adding anything"), so the id and the output moved with it. The rule
# is unchanged and is asserted on the new name.
test_that("a clean result still offers a way to fix a wrong match", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, 'uiOutput\\("cv_edit"\\)')
  expect_match(joined, "output\\$cv_edit <- renderUI")
  blk <- .ui_block(src, "output\\$cv_edit <- renderUI", 30L)
  expect_match(blk, "cv_edit_go")
  # it renders for a CLEAN result, not only for needs_review
  expect_match(blk, 'st %in% c\\("ok", "needs_review"\\)')
})

# The charter's interface rule: no raw engine code, template id or internal metric
# on a customer-facing screen. The close-call panel printed both an id and a
# detection score, and two pickers offered raw ids as their options.
test_that("no template id or match score reaches the result page", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_candidates <- renderUI", 44L)
  expect_false(grepl("score %s", blk, fixed = TRUE))
  expect_match(blk, "friendly_tpl\\(res\\$template_id\\)")
  expect_match(blk, "tpl_choices\\(others\\)")
  expect_match(.ui_block(src, 'selectInput\\("cv_tie_pick"', 2L), "tpl_choices\\(tied\\)")
  # names for people, ids for the server -- and identical names stay distinguishable
  expect_match(.ui_block(src, "tpl_choices <- function", 12L), "option %d")
})

test_that("a template name does not end in 'statement statement'", {
  # A PDF drafted in the toolkit is saved with statement_type "statement"
  # (R/draft.R), and the label appended the word again.
  #
  # The suffix rule is now .tpl_label(), because TWO template sets feed it: the
  # transaction templates and the form (mode: fields) templates, which were
  # printing raw ids for want of exactly these four lines.
  f <- .ui_fun(".tpl_label")
  expect_identical(f("Sample Everyday Statement", ""), "Sample Everyday Statement")
  expect_identical(f("BNZ", "everyday"), "BNZ everyday statement")
  expect_identical(f("ANZ", "kiwisaver"), "ANZ kiwisaver statement")
  expect_true(is.na(f("", "")))                      # nothing to build a name from
  # ...and an ABSENT bank is absent, not the two letters "N" and "A" -- which is
  # the "Read as: NA NA statement" this helper's other caller was fixed for
  expect_true(is.na(f(NA, NA)))
  expect_identical(f(NA, "everyday"), "everyday statement")
  expect_true(is.function(.ui_fun("friendly_tpl")))
})

# A typed page the document does not have was clamped for the PICTURE and left as
# typed in the box, so the control and the picture disagreed about which page was
# on screen -- on the view a reviewer takes evidence from.
test_that("a page number the document does not have is corrected in the box", {
  src <- .ui_src()
  expect_match(paste(src, collapse = "\n"), "ix_page_settled <- debounce")
  blk <- .ui_block(src, "ix_page_settled <- debounce", 12L)
  expect_match(blk, 'updateNumericInput\\(session, "ix_page", value = p\\)')
  expect_match(blk, "showing page %d")
})

test_that("the X-ray key names only what is drawn on this page", {
  blk <- .ui_block(.ui_src(), "output\\$ix_legend <- renderUI", 40L)
  for (cond in c("n_kept > 0", "n_skip > 0", "has_red", "has_meta"))
    expect_true(grepl(cond, blk, fixed = TRUE), info = cond)
  # ...and the column names in the key are the reader's, matching the page itself
  expect_match(blk, "cv_friendly_cols\\(nm\\)")
})

test_that("a toast replaces the last one about the same thing", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "notify_once <- function\\(id, text")
  expect_match(.ui_block(src, "notify_once <- function", 3L), 'id = paste0\\("n_", id\\)')
  # and a message is withdrawn when it stops being true
  expect_match(joined, 'clear_notice\\("cv_qid"\\)')
})

# ---- Admin (maintainer-only, but it must still tell the truth) ---------------

test_that("the two irreversible actions ask first, and say what they will destroy", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  for (h in c("observeEvent\\(input\\$adm_purge_uploads", "observeEvent\\(input\\$adm_tpl_delete,")) {
    blk <- .ui_block(src, h, 30L)
    expect_match(blk, "showModal\\(modalDialog")
    expect_match(blk, "permanently deletes")
  }
  # the deed itself moved behind a separate confirm input, still admin-gated
  for (h in c("observeEvent\\(input\\$adm_purge_confirm", "observeEvent\\(input\\$adm_tpl_delete_confirm"))
    expect_match(.ui_block(src, h, 4L), "req\\(admin_ok\\(\\)\\)")
  # ...and the count is taken the way purge_uploads takes it, not guessed
  expect_match(joined, "\\.uploads_due <- function")
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

test_that("assigning or removing a box says so where the box was drawn", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, 'uiOutput\\("g_pdf_msg"\\)')
  expect_match(.ui_block(src, "\\.g_box_note <- function", 6L), "output\\$g_pdf_msg")
  # ...and removing the DATE column is named as what it is, not as a mis-drawn box
  expect_match(joined, "\\.has_date_col <- function")
  expect_match(.ui_block(src, "output\\$g_status <- renderUI", 60L),
               "There is no date column, so no rows can be read")
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

# The toolkit is where a template is DECIDED and SAVED, and its verdict branched
# on nothing but n > 0. Measured: asb.pdf drew a green tick over "1 transaction
# row read ... If they are right, click Save template" while row_coverage() on
# that same drafted template already knew page 2 kept nothing; d5_sample.pdf drew
# one over "19 transaction rows read", and the screen after saving said "11
# discontinuity(ies)".
test_that("the toolkit preview asks what the tool already knows before it ticks", {
  f <- .ui_fun("preview_doubts")
  clean <- data.frame(row_id = 1:3, amount = c(10, -5, -2), balance = c(10, 5, 3))
  cov_ok <- list(applicable = TRUE, actionable_skips_total = 0L, empty_pages = integer(0),
                 diagnosis = "Every candidate row was kept.")
  expect_identical(f(clean, cov_ok), character(0))       # a clean draft still ticks
  # balances that do not follow are what a dropped row looks like from here
  broken <- data.frame(row_id = 1:3, amount = c(10, -5, -2), balance = c(10, 5, -40))
  d <- f(broken, cov_ok)
  expect_length(d, 1L)
  expect_match(d, "balances do not follow in 1 place")
  # a page that carried words and kept nothing is the engine's own sentence
  cov_bad <- list(applicable = TRUE, actionable_skips_total = 9L, empty_pages = 2L,
                  diagnosis = "Page(s) 2 carry words but kept no rows -- either the column bands don't line up on this layout, or those pages hold no transactions.")
  expect_match(f(clean, cov_bad)[1], "Page(s) 2 carry words but kept no rows", fixed = TRUE)
  # ...but an empty page that lost NOTHING is not evidence of anything. Measured
  # on the bundled specimen: page 1 is the cover, keeps 0 rows and skips 0
  # candidates, and the first version of this said "but not all of the page" over
  # a perfect draft. A tool that cries wolf on the common case gets ignored.
  cov_cover <- list(applicable = TRUE, actionable_skips_total = 0L, empty_pages = 1L,
                    any_page_rescaled = FALSE,
                    diagnosis = "Page(s) 1 carry words but kept no rows -- either the column bands don't line up on this layout, or those pages hold no transactions.")
  expect_identical(f(clean, cov_cover), character(0))
  # ...unless the parser had to RESCALE that page, which loses rows without
  # leaving candidates behind to count
  cov_scaled <- utils::modifyList(cov_cover, list(any_page_rescaled = TRUE))
  expect_length(f(clean, cov_scaled), 1L)
  # both at once, and neither swallows the other
  expect_length(f(broken, cov_bad), 2L)
  # a non-PDF (no coverage to read) is judged on the balances alone, not refused
  expect_identical(f(clean, NULL), character(0))
  expect_length(f(broken, NULL), 1L)
  # ...and the tick itself is spent on the answer, not on n > 0
  blk <- .ui_block(.ui_src(), "output\\$g_status <- renderUI", 40L)
  expect_match(blk, "preview_doubts\\(tx, g_preview_cov\\(\\)\\)")
  expect_match(blk, 'if \\(ok\\) "verdict verdict-high" else "verdict verdict-medium"')
  expect_false(grepl('if (n > 0L) return(div(class = "verdict verdict-high"', blk, fixed = TRUE))
})

# The form result was the only table on Convert handed straight to datatable():
# a FIELD column of engine schema names beside a LABEL column that already said
# the same thing in the document's words, and three columns of `true` / `false`.
test_that("the form result table shows no schema name and no raw true/false", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_fields <- renderDT", 22L)
  expect_match(blk, "yes_no\\(f\\$matched\\)")
  expect_match(blk, "yes_no\\(f\\$required\\)")
  expect_match(blk, "What the document calls it")
  # the engine's own column names never reach the frame that is drawn
  expect_false(grepl('"field", "label", "value", "matched"', blk, fixed = TRUE))
  # a field whose label is blank keeps its identity in readable form
  expect_match(blk, "cv_friendly_cols\\(as\\.character\\(f\\$field")
  yn <- .ui_fun("yes_no")
  expect_identical(yn(c(TRUE, FALSE, NA)), c("yes", "no", "no"))
})

# "Read as: anz_kiwisaver_fields". friendly_tpl returned the RAW ID for anything
# not in all_templates(), and form templates are deliberately not in that set --
# so the fix for "Read as: NA NA statement" swapped one wrong answer for an engine
# code on a customer-facing screen, which the charter forbids outright.
test_that("a form template is named, not printed as its id", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, "all_field_templates <- reactive")
  expect_match(.ui_block(src, "all_field_templates <- reactive", 4L),
               "load_fields_templates\\(FIELDS_DIR, USER_FIELDS_DIR\\)")
  expect_match(.ui_block(src, "friendly_tpl <- function", 20L),
               "all_field_templates\\(\\)\\[\\[tid\\]\\]")
  # the shipped form template really does carry the two fields the name is built
  # from, so this is wired to a fact and not to a hope
  ft <- load_fields_templates(fields_templates_dir())
  t <- ft[["anz_kiwisaver_fields"]]
  expect_false(is.null(t))
  expect_identical(.ui_fun(".tpl_label")(t$bank, t$statement_type), "ANZ kiwisaver statement")
})

# One needs_review screen carried three alarm levels: a headline calling the
# failures "secondary and commonly flag", a What-to-check list holding only the
# dates item, a HIGH diagnostic saying "split it into one statement per file", and
# a prominent button offering the template toolkit instead.
test_that("what to check leads with the highest-severity diagnostic, and it carries the action", {
  top <- .ui_fun("top_diagnostics", also = c(".diagnostics_of", ".statement_route", ".is_txn_result"))
  d <- data.frame(where = c("upload", "dates"),
                  category = c("multiple_statements", "date_out_of_range"),
                  severity = c("high", "medium"),
                  detail = c("the balance block appears 2 times", "34 date(s) outside period"),
                  how_to_fix = c("Split it into one statement per file and re-run.", "..."),
                  stringsAsFactors = FALSE)
  got <- top(list(status = "needs_review", diagnostics = d))
  expect_identical(nrow(got), 1L)
  expect_identical(got$category[1], "multiple_statements")
  # a failed / unsupported run's top diagnostic IS its headline message, already on
  # the card, so it is not listed a second time
  expect_identical(nrow(top(list(status = "failed", diagnostics = d))), 0L)
  expect_identical(nrow(top(list(status = "unsupported", diagnostics = d))), 0L)
  # "none" is the explicit no-issues row, not a fault
  expect_identical(nrow(top(list(status = "ok", diagnostics = data.frame(
    category = "none", severity = "high", detail = "", how_to_fix = "",
    stringsAsFactors = FALSE)))), 0L)
  expect_identical(nrow(top(list(status = "ok"))), 0L)          # no diagnostics at all
  # the card lists it, and the remedy is the action line under it
  blk <- .ui_block(.ui_src(), "failed_checks_ui <- function", 34L)
  expect_match(blk, "dg <- top_diagnostics\\(res\\)")
  # never the code -- and in the words of the route this run took, not always the
  # statement route's (ui_labels.R: plain_diag / DIAG_PLAIN_OTHER)
  expect_match(blk, "plain_diag\\(dg\\$category\\[i\\], \\.statement_route\\(res\\)\\)")
  expect_match(blk, "Do this first: ")
  expect_match(blk, "dg\\$how_to_fix\\[1\\]")
})

# The template invitation was keyed on `needs_review` -- a verdict about the
# FIGURES -- so a run whose balance reconciled to the cent and whose 79 dates all
# read was told "worth checking it's the right match" with a warning-coloured
# button. The charter says the tool decides the template.
test_that("the match is only called into question when detection left a question", {
  thin <- .ui_fun(".match_is_thin")
  expect_false(thin(list(detect = list(thin = FALSE, ambiguous = FALSE, tied = character(0)))))
  expect_true(thin(list(detect = list(thin = TRUE))))
  expect_true(thin(list(detect = list(ambiguous = TRUE))))
  expect_true(thin(list(detect = list(tied = c("a", "b")))))
  expect_false(thin(list()))                       # nothing detected: claim nothing
  # the engine really does record those three, so this reads a fact
  det <- readLines(file.path(engine_root(), "R", "convert.R"), warn = FALSE)
  expect_true(any(grepl("thin", det, fixed = TRUE)))
  # WAS: output$cv_rematch. Renamed to cv_edit by register B1; the branch this
  # asserts on is the same one, unchanged.
  blk <- .ui_block(.ui_src(), "output\\$cv_edit <- renderUI", 30L)
  expect_match(blk, "\\.match_is_thin\\(res\\)")
  expect_false(grepl("worth checking it's the right match", blk, fixed = TRUE))
})

# A text file renamed .pdf: "no text could be read from this PDF - it is damaged,
# encrypted, or not a PDF at all", and directly under it an offer to open the
# template toolkit and "save an improved template". There is no page to draw on.
test_that("the template toolkit is not offered for a file that was never read", {
  # THE WINDOW HAS TO REACH THE END OF THE FUNCTION or the test passes by not
  # looking, and cv_teach keeps growing branches above these two -- a report
  # result, then both doors on an unrecognised one. Widened twice for that
  # reason. The ORDER is the promise, not the line number. Widened again for the
  # high-severity diagnostic that now takes the headline (register J8).
  blk <- .ui_block(.ui_src(), "output\\$cv_teach <- renderUI", 240L)
  i <- regexpr('if (identical(st, "failed")) return(NULL)', blk, fixed = TRUE)
  expect_gt(i, 0L)
  j <- regexpr('actionButton("cv_teach_go_fix"', blk, fixed = TRUE)
  expect_gt(j, 0L)
  expect_lt(i, j)                     # the guard is BEFORE the offer, so it works
})

# A "Checks" heading with no rows under it, on the one screen with the least to
# go on -- and the page promises "every check that exists for your statement is in
# this table". A blank table cannot be told from a rendering failure.
test_that("an empty checks or coverage table says why it is empty", {
  why <- .ui_fun(".why_empty")
  expect_match(why(list(status = "failed"), "nothing to check"), "Nothing was read from this file")
  expect_match(why(list(status = "unsupported"), "nothing to check"), "No template read this document")
  # EACH HEADING SAYS WHAT IS TRUE OF ITS OWN TABLE. One string used to name both,
  # so the identical sentence appeared twice on one screen, each time half about
  # the other table.
  expect_match(why(list(status = "unsupported"), "nothing to check"), "nothing to check\\.$")
  expect_match(why(list(status = "unsupported"), "no fields to report"), "no fields to report\\.$")
  blk <- .ui_block(.ui_src(), "output\\$cv_detail <- renderUI", 26L)
  expect_match(blk, 'if \\(has_kpis\\) DTOutput\\("cv_kpis"\\) else said\\("nothing to check"\\)')
  expect_match(blk, 'said\\("no fields to report"\\)')
  expect_match(blk, "if \\(has_cov\\) tagList")
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

# THE TOOLKIT SAVE TOAST PRINTED A RAW TEMPLATE ID, TWICE, on the screen a
# non-technical analyst uses to add a bank. Measured:
#   Saved "newbank_everyday_everyday_csv".
#   Next time, a statement like this one is recognised automatically.
#   We re-checked it against every template and it matched yours
#   ("newbank_everyday_everyday_csv") on its own, with nothing forced.
# The second copy is R/util.R's sentence; this holds app.R's half, and the other
# confirmation that named a template the same way.
test_that("the toasts that name a template name it the way the rest of the screen does", {
  src <- .ui_src()
  # ONE display name, built where the template itself is in hand, so the id can
  # never come back as friendly_tpl's fallback
  blk <- .ui_block(src, "saved_name <- friendly_tpl\\(saved_id\\)", 4L)
  expect_match(blk, "\\.tpl_label\\(tmpl\\$bank, tmpl\\$statement_type\\)")
  expect_match(blk, "your new template", fixed = TRUE)
  # ...and neither branch of the toast interpolates the id itself
  toast <- .ui_block(src, "<b>Saved ", 6L)
  expect_match(toast, "htmlEscape\\(saved_name\\)")
  expect_false(grepl("saved_id", toast, fixed = TRUE))
  again <- .ui_block(src, "Saved as your template", 3L)
  expect_false(grepl("saved_id", again, fixed = TRUE))
  # "Convert with this one instead" said the id too, beside a picker that offers
  # names (tpl_choices) and a "Read as:" chip that prints one
  conv <- .ui_block(src, "Converted with %s", 2L)
  expect_match(conv, "friendly_tpl\\(tid\\)")
  # ...and the SECOND copy of the id on that same toast came from R/util.R's
  # sentence, which can only name a template if it is handed the set the template
  # lives in. The caller's half is passing it; without it .saved_name falls back
  # to the id and the toast prints it again.
  expect_match(.ui_block(src, "recognition_summary\\(detect_statement", 2L),
               "templates = tset")
})

# THE HEADING CONTRADICTED THE CONTENTS. read_uploads() returns EVERY upload
# record, newest first, with no filter -- which is what the incident procedure
# sends a maintainer to it for. It was headed "Uploads - new formats to pick up"
# over help text reading "Statements the tool couldn't read, that nobody has set
# up yet", so at step 1 of that procedure, hunting a conversion that WORKED, the
# screen told her not to look in the only table that had it.
test_that("the Admin uploads table is described as the whole log it really is", {
  src <- .ui_src()
  blk <- .ui_block(src, 'h4\\("Uploads', 5L)
  expect_false(grepl("new formats to pick up", blk, fixed = TRUE))
  expect_false(grepl("Statements the tool couldn't read", blk, fixed = TRUE))
  # WAS: "every statement converted here". The uploads table has always held every
  # upload of every kind -- a report and a form are uploads too -- so the heading
  # says document, which is the word the rest of the product uses for both routes.
  expect_match(blk, "every document converted here")
  expect_match(blk, "needs_pickup")        # the pickup queue is still named, as a column
  # ...and the engine really does hand back rows the old heading denied: a
  # converted, template-matched upload that needs no pickup at all.
  d <- tempfile("tup_"); dir.create(d)
  f <- file.path(d, "seed.csv"); writeLines("date,amount", f)
  record_upload(f, name = "seed.csv", status = "ok", template = "westpac_everyday_pdf",
                trust = "medium", dir = d)
  u <- read_uploads(d)
  expect_equal(nrow(u), 1L)
  expect_identical(u$status[1], "ok")
  expect_false(u$needs_pickup[1])
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

# The batch row for a file said "Converted successfully" with "-" under What to
# check, while opening that same row said "confidence: medium / Read cleanly.
# Something could not be proven". On a thirty-file case there was no way to tell
# the clean files from the merely-uncomplaining ones without opening all thirty.
test_that("the batch table grades a file with the same word its own card uses", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_batch <- renderDT", 72L)
  expect_match(blk, "Confidence = dash\\(b\\$trust\\)")
  # the card prints res$trust$level, and convert_batch really carries it per file
  expect_match(.ui_block(src, "output\\$cv_headline <- renderUI", 45L),
               "res\\$trust\\$level")
  bsrc <- readLines(file.path(engine_root(), "R", "batch.R"), warn = FALSE)
  expect_true(any(grepl("^\\s*trust\\s+= rep\\(NA_character_", bsrc)))
  expect_true(any(grepl("out\\$trust\\[i\\]", bsrc)))
})

# app.R is the file a maintainer reads first, and two comments in it stated a
# MEASURED fact that a later fix had made false.
test_that("no comment in app.R still claims the drafter reads nothing", {
  joined <- paste(.ui_src(), collapse = "\n")
  expect_false(grepl("draft_template reads 0 rows", joined, fixed = TRUE))
  expect_false(grepl("the drafter reads 0 rows", joined, fixed = TRUE))
  # ...and what replaced them is a figure, which is what makes it re-checkable
  expect_match(joined, "311 rows over the file")
})

# An empty date box posts the literal string "NaN-NaN-NaN", and shiny's own
# shiny.date handler ran as.Date() over it, caught the error and re-raised it as a
# warning -- twice on every toolkit open, for a value the app handles correctly.
# suppressWarnings() at the widget cannot reach it: it fires a tick later, inside
# shiny's input decoding.
test_that("an empty date picker is decoded quietly, and a real date still arrives", {
  f <- .ui_fun(".decode_shiny_date")
  expect_warning(got <- f("NaN-NaN-NaN"), NA)
  expect_true(is.na(got))
  expect_s3_class(got, "Date")
  expect_identical(f("2024-03-01"), as.Date("2024-03-01"))
  expect_identical(f(list("2024-03-01", NULL)), as.Date(c("2024-03-01", NA)))
  # one empty box in a pair no longer blanks the other, which shiny's own
  # all-or-nothing coercion did
  expect_identical(f(list("2024-03-01", "NaN-NaN-NaN")), as.Date(c("2024-03-01", NA)))
  # and it is really registered, before anything can render a date box
  src <- .ui_src()
  i <- grep('registerInputHandler\\("shiny.date", force = TRUE, \\.decode_shiny_date\\)', src)
  expect_length(i, 1L)
  expect_true(i < grep("\\.eff_picker <- function", src)[1])
})

# ---------------------------------------------------------------------------
# B1. "For Other statements the likelihood that something will need to change on
# every convert - it shouldn't just auto process, it should process and open up
# the editor. For statements I want a threshold where it does and where it
# doesn't, but ensure there is an option even if it's confident."
#
# There was no such thing anywhere: opening an editor was only ever something a
# CLICK did, so the report route converted and stopped, and the statement route
# had no threshold and no way back from a confident result either.
# ---------------------------------------------------------------------------

test_that("the editor opens by itself on the route that always needs it, and on a doubtful statement", {
  needs <- .ui_fun(".needs_editor", also = ".is_txn_result")
  # A REPORT AND A FORM ALWAYS OPEN. Neither carries any trust at all -- there is
  # no reconciliation behind either -- so there is nothing to be confident about.
  for (k in c("tables", "form")) {
    expect_true(needs(list(kind = k, status = "ok"), "medium"))
    expect_true(needs(list(kind = k, status = "needs_review"), "medium"))
    # ...and the statement threshold cannot switch them off. The threshold was
    # asked for on statements only, and "any" means "a statement never opens it".
    expect_true(needs(list(kind = k, status = "ok"), "any"))
  }
  # A STATEMENT OPENS ON THE THRESHOLD. Default medium: only a `low` one.
  expect_false(needs(list(status = "ok", trust = list(level = "high")), "medium"))
  expect_false(needs(list(status = "ok", trust = list(level = "medium")), "medium"))
  expect_true(needs(list(status = "ok", trust = list(level = "low")), "medium"))
  expect_true(needs(list(status = "needs_review", trust = list(level = "low")), "medium"))
  # high: medium and low both open it. any: neither does.
  expect_true(needs(list(status = "ok", trust = list(level = "medium")), "high"))
  expect_false(needs(list(status = "ok", trust = list(level = "low")), "any"))
  # A statement with no trust recorded at all fails CLOSED -- it opens -- because
  # an unproven figure is the case this exists for.
  expect_true(needs(list(status = "ok"), "medium"))
  # AND NOTHING OPENS ON A RESULT THAT PRODUCED NOTHING. An `unsupported`
  # statement is trust `low`, and auto-opening the statement toolkit over it would
  # answer, for her, the very question its card is asking.
  expect_false(needs(list(status = "unsupported", trust = list(level = "low")), "medium"))
  expect_false(needs(list(status = "failed"), "medium"))
  expect_false(needs(list(kind = "tables", status = "unsupported"), "medium"))
})

test_that("the threshold is a deployment setting with a stated default, not a number in the code", {
  src <- .ui_src()
  i <- grep("^EDITOR_MIN_TRUST <- ", src)
  expect_length(i, 1L)
  blk <- paste(src[i:(i + 3)], collapse = " ")
  expect_match(blk, "CONFIG\\$convert\\$open_editor_below_trust")
  expect_match(blk, '%\\|\\|% "medium"')
  # an unrecognised value keeps the default rather than silently meaning "never"
  expect_match(paste(src[i:(i + 4)], collapse = " "),
               'EDITOR_MIN_TRUST %in% c\\("high", "medium", "any"\\)')
  # the same three words the feed gate already uses, so nobody learns a second
  # vocabulary -- and the SAME function evaluates them
  expect_match(paste(src[(i - 10):(i + 2)], collapse = " "), "feed\\.min_trust")
  fe <- readLines(file.path(engine_root(), "R", "feed.R"), warn = FALSE)
  expect_true(any(grepl("^\\.trust_ok <- function", fe)))
})

test_that("the auto-open fires on a real upload and on nothing else", {
  src <- .ui_src()
  blk <- .ui_block(src, "run_conversion <- function", 80L)
  expect_match(blk, "isTRUE\\(record\\) && \\.needs_editor\\(res, EDITOR_MIN_TRUST\\)")
  expect_match(blk, "\\.edit_now\\(\\)")
  # `record` is the gate BECAUSE it already separates a person handing the tool a
  # file from the three re-runs that must stay silent. Each of those really does
  # pass record = FALSE, or the sample would re-open the toolkit every time.
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "run_conversion\\(SAMPLE_STATEMENT, basename\\(SAMPLE_STATEMENT\\), record = FALSE\\)")
  expect_match(joined, "run_conversion\\(src\\$path, src\\$name, record = FALSE, force_tpl = tid")
  # ...and a case folder never auto-opens anything: it does not come through here
  expect_false(grepl(".needs_editor", .ui_block(src, "run_batch <- function", 80L), fixed = TRUE))
})

test_that("the door back is one control, on all three kinds, and it is not a new one", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # It renders for a report and a form too -- the guard that stopped it is gone.
  blk <- .ui_block(src, "output\\$cv_edit <- renderUI", 30L)
  expect_false(grepl("req(.is_txn_result(res))", blk, fixed = TRUE))
  # ...and it asks the question that route actually has
  expect_match(blk, "A table missing, or reading the wrong columns\\?")
  expect_match(blk, "A value missing, or reading the wrong thing\\?")
  expect_match(blk, "Not the right bank\\?")
  # THE TWO HALF-WORKING CONTROLS IT REPLACES ARE GONE, not left beside it.
  expect_false(grepl("cv_goto_report", joined, fixed = TRUE))
  expect_false(grepl("cv_goto_templates", joined, fixed = TRUE))
  # and it sits directly under the downloads, where the payoff is
  i_dl <- grep('uiOutput\\("cv_downloads"\\)', src)
  i_ed <- grep('uiOutput\\("cv_edit"\\)', src)
  expect_length(i_ed, 1L)
  expect_true(i_ed > i_dl && i_ed - i_dl < 30L)
})

test_that("the door carries the document and the template, which is what the deleted links did not", {
  src <- .ui_src()
  blk <- .ui_block(src, "\\.edit_now <- function", 40L)
  # a report opens the SAVED TEMPLATE on the document she just converted
  expect_match(blk, "rb_open_template\\(t, src\\$path\\)")
  expect_match(blk, "all_doc_templates\\(\\)\\[\\[tid\\]\\]")
  # ...and a template that is no longer in the library SAYS SO rather than
  # opening a blank screen with no explanation
  expect_match(blk, "no longer in the library")
  # a form has no editor of its own, and the notification says what will be built
  expect_match(blk, "makes a report template")
  # a statement seeds the toolkit with the template that matched
  expect_match(blk, "open_guided\\(src\\$path, src\\$name, seed_tmpl = seed")
  # the NON-admin set, so a parked template is never handed back on a result
  expect_match(.ui_block(src, "all_doc_templates <- reactive", 4L),
               "load_document_templates\\(DOC_DIR, USER_DOC_DIR\\)")
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

# ---------------------------------------------------------------------------
# J8. Driven with no OCR software and an image-only PDF: the card said "no
# template for this layout", the primary green button said "Set it up as a
# report", and the diagnostics table said correctly that this machine has no OCR
# software and a template will not help. The biggest button was the wrong one.
# ---------------------------------------------------------------------------

test_that("a high-severity diagnosis nobody can fix with a template takes the headline", {
  bd <- .ui_fun(".blocking_diag", also = c(".diagnostics_of", ".statement_route", ".is_txn_result"))
  mk <- function(cat, sev, own) data.frame(category = cat, severity = sev,
    detail = paste(cat, "happened"), how_to_fix = "do this", fix_owner = own,
    stringsAsFactors = FALSE)
  # the file itself, and an engine gap: neither is mended by drawing boxes
  expect_equal(bd(list(diagnostics = mk("scanned_no_ocr", "high", "input")))$category,
               "scanned_no_ocr")
  expect_equal(bd(list(diagnostics = mk("redaction_unverified", "high", "escalate")))$category,
               "redaction_unverified")
  # a TEMPLATE fault is deliberately NOT blocking -- a template is exactly the fix
  expect_null(bd(list(diagnostics = mk("matched_but_empty", "high", "template"))))
  # ...nor is anything below high, nor the explicit no-issues row
  expect_null(bd(list(diagnostics = mk("date_out_of_range", "medium", "input"))))
  expect_null(bd(list(diagnostics = mk("none", "info", "none"))))
  expect_null(bd(list()))
  # most severe first is the engine's own order, so the FIRST hit is the one
  d <- rbind(mk("scanned_no_ocr", "high", "input"), mk("oversized", "high", "input"))
  expect_equal(bd(list(diagnostics = d))$category, "scanned_no_ocr")
  # the engine really grades ownership this way
  dg <- readLines(file.path(engine_root(), "R", "diagnose.R"), warn = FALSE)
  expect_true(any(grepl("scanned_no_ocr\\s+= \"input\"", dg)))
  expect_true(any(grepl("^\\.diag_fix_owner <- function", dg)))
  # and the card really puts it FIRST, above every "set it up" branch
  blk <- .ui_block(.ui_src(), "output\\$cv_teach <- renderUI", 240L)
  i <- regexpr("bd <- .blocking_diag(res)", blk, fixed = TRUE)
  expect_gt(i, 0L)
  for (later in c('This looks like a report, not a bank statement.',
                  'The tool cannot tell what kind of document this is.'))
    expect_lt(i, regexpr(later, blk, fixed = TRUE))
  # the primary action is REPLACED, not merely joined: the setup route on that
  # branch is the quiet override, never a btn-primary
  card <- substr(blk, i, i + 700L)
  expect_match(card, 'other\\("Sure a template is what this needs\\?"')
  expect_false(grepl("btn-primary", substr(card, 1, regexpr("other(", card, fixed = TRUE)),
                     fixed = TRUE))
})

# ---------------------------------------------------------------------------
# J7. "Thirty files, three failed: no way to re-run just those three and no way
# to download the other twenty-seven."
# ---------------------------------------------------------------------------

test_that("a case folder can re-run the files that failed and hand back the rest", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # the table can be ticked, and one click still OPENS a row
  blk <- .ui_block(src, "output\\$cv_batch <- renderDT", 72L)
  expect_match(blk, 'selection = "multiple"', fixed = TRUE)
  expect_match(joined, "observeEvent\\(input\\$cv_batch_row_last_clicked")
  # THE RE-RUN IS THE SAME ENGINE CALL, on fewer files -- never a second pipeline
  re <- .ui_block(src, "observeEvent\\(input\\$cv_batch_again", 40L)
  expect_match(re, "run_batch\\(data\\.frame\\(name = basename\\(paths\\[keep\\]\\)")
  # it refuses with a reason rather than doing nothing when nothing is ticked
  expect_match(re, "Tick the files you want to convert again first")
  # ...and it is never silent about converting fewer files than were ticked
  expect_match(re, "no longer on this server, so only the rest")
  # the same QID gate every other conversion goes through
  expect_match(re, "\\.identity_ok\\(\\)")
  # ONE FILE FOR THE WHOLE CASE, built from the outputs already on disk
  dl <- .ui_block(src, "output\\$cv_batch_dl <- downloadHandler", 30L)
  expect_match(dl, "\\.batch_outputs\\(cv_batch\\(\\)\\)")
  expect_match(dl, "zip::zip")
  # a host that cannot pack a zip says so in a file that opens, never a broken one
  expect_match(dl, "could not be packed into one file")
  # and neither control renders with nothing to act on
  op <- .ui_block(src, "output\\$cv_batch_open <- renderUI", 30L)
  expect_match(op, "if \\(length\\(sel\\)\\)")
  expect_match(op, "if \\(length\\(outs\\)\\)")
})

# ---------------------------------------------------------------------------
# B2. "Ensure submit feedback is also there on other reports." A report has no
# reconciliation behind it, so a human's "this is wrong" is the ONLY signal there
# is -- it matters MORE there, not less.
# ---------------------------------------------------------------------------

test_that("feedback is asked on a report and a form, and says why it matters there", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_feedback <- renderUI", 40L)
  # NO KIND GUARD -- it may READ the kind to word one sentence, but it may never
  # refuse to render on it. The two shapes that would do that are named outright.
  expect_false(grepl("req(.is_txn_result(res))", blk, fixed = TRUE))
  expect_false(grepl('identical(res$kind, "form")', blk, fixed = TRUE))
  # the ONE return(NULL) pair it is allowed is about the RUN, not about the kind
  expect_equal(length(gregexpr("return(NULL)", blk, fixed = TRUE)[[1]]), 2L)
  # WHY IT MATTERS THERE IS SAID AT THE TOP OF THE RESULT, NOT AT THE BOTTOM.
  # This panel used to carry a third saying of it -- "your reading of it against
  # the page is the only check there is" -- under a route paragraph and a verdict
  # card that had both already said there is no running balance behind a report
  # and that she is the check. The sentence is not lost; it is two inches higher,
  # on both halves of the OTHER route, in one wording. (Words sweep, cut 29.)
  expect_false(grepl("the only check there is", blk, fixed = TRUE))
  for (out in c("cv_form", "cv_tables"))
    expect_match(.ui_block(src, sprintf("output\\$%s <- renderUI", out), 34L),
                 "so nothing reconciles", fixed = TRUE)
  # ...and this panel does NOT reuse the statement promise, which no report can keep
  expect_false(grepl("reconcile", blk, fixed = TRUE))
  # it is rendered outside the transaction-only panel, or the guard would be back
  # by the back door
  i_fb <- grep('uiOutput\\("cv_feedback"\\)', src)
  i_panel <- grep("output.cv_is_tables != true", src)
  expect_length(i_fb, 1L)
  expect_true(all(i_fb > i_panel))
  expect_match(paste(src[i_fb:(i_fb + 2)], collapse = " "), "^\\s*uiOutput")
})

# ---------------------------------------------------------------------------
# J9. "The OTHER route ships empty" -- and cv_empty only ever described
# statements, three inches under a radio button reading "Something else".
# ---------------------------------------------------------------------------

test_that("the empty state promises what the answer she gave can deliver", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_empty <- renderUI", 42L)
  expect_match(blk, 'other <- identical\\(kind_choice\\(\\), "other"\\)')
  expect_match(blk, "Convert a report, a form or a letter")
  # THE ROUTE STILL SPLITS -- the heading, what may be uploaded, and where an
  # unrecognised layout is set up all differ by the answer she gave...
  expect_match(blk, "Upload it on the left", fixed = TRUE)
  expect_match(blk, "Upload a statement on the left", fixed = TRUE)
  expect_match(blk, "Your bank is detected automatically", fixed = TRUE)
  expect_match(blk, "If no template fits this layout yet", fixed = TRUE)
  # ...but the screen no longer ADVERTISES the result page to somebody who has
  # already opened it. Every one of the six bullets is delivered and named on the
  # result itself -- the download bar, the proof strip, the table navigator.
  expect_false(grepl("You'll get back", blk, fixed = TRUE))
  expect_false(grepl("Whether it reconciles", blk, fixed = TRUE))
  expect_false(grepl("Where each one came from", blk, fixed = TRUE))
  # and it does not name a distinction nobody outside Admin makes
  expect_false(grepl("a form or report template", blk, fixed = TRUE))
  # THE SAMPLE IS A BANK STATEMENT, so it is not offered under "something else"
  expect_match(blk, "if \\(!other && file\\.exists\\(SAMPLE_STATEMENT\\)\\)")
  # one link element, not two literals sharing an id
  expect_length(grep('actionLink\\("cv_empty_to_tmpl"', src), 1L)
})

test_that("the verdict card's headline is the diagnosis, not the generic template line", {
  # On an `unsupported` run the engine writes ONE message whatever the cause --
  # "we don't have a template for this layout yet" -- so the card said that over
  # an image-only PDF on a machine with no OCR software, while its own diagnostics
  # said at severity high that there was nothing on the page to read.
  blk <- .ui_block(.ui_src(), "output\\$cv_status <- renderUI", 55L)
  expect_match(blk, 'bdx <- if \\(st %in% c\\("unsupported", "failed"\\)\\) \\.blocking_diag\\(res\\)')
  expect_match(blk, "headline <- \\.sentence\\(bdx\\$detail\\[1\\]\\)")
  # The body still renders every OTHER engine message...
  expect_match(blk, "plain_messages\\(res\\$messages\\)")
  # ...but not this one, because the card's own title already says it. The title
  # is STATUS_PLAIN["unsupported"], which is the fact, in bigger type.
  e <- new.env(parent = globalenv())
  sys.source(file.path(engine_root(), "ui_labels.R"), envir = e)
  expect_match(e$STATUS_PLAIN[["unsupported"]], "No template recognised")
  pm <- .ui_block(.ui_src(), "plain_messages <- function\\(m\\)", 14L)
  expect_match(pm, "m\\[!grepl\\(\"\\^we don't have a template for this layout yet\\$\", m\\)\\]")
  # the engine really does write that one sentence for every unsupported run,
  # which is what makes the headline swap necessary rather than cosmetic
  cv <- readLines(file.path(engine_root(), "R", "convert.R"), warn = FALSE)
  expect_true(any(grepl("we don't have a template for this layout yet", cv, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# THE SWEEP: fewer controls, one sentence each, and the same question asked the
# same way on both routes.
#
# "This needs to be as SIMPLE as possible. No fancy bullshit." / "There are LOTS
# of buttons, lots of things to interact with. It NEEDS to be even more simple."
# / "We DO NOT want the platform getting overly verbose."
#
# The register counted 182 interactive controls and observed that Beth's whole
# job is four of them. Nothing below adds a rule about WHICH controls exist; they
# hold the ones this sweep removed removed, and put a ceiling under the total so
# the next agent's additions have to pay for themselves.
# ---------------------------------------------------------------------------

.ui_control_ids <- function(src = .ui_src()) {
  pat <- paste0("(actionButton|actionLink|textInput|textAreaInput|numericInput|",
                "selectInput|selectizeInput|checkboxInput|checkboxGroupInput|",
                "radioButtons|fileInput|sliderInput|downloadButton|downloadLink)",
                "[(][\"][a-zA-Z0-9_]+")
  sub(".*[\"]", "", unlist(regmatches(src, gregexpr(pat, src))))
}

test_that("the number of controls on screen does not creep back up", {
  # Measured the way the register measures it, so the number in the register and
  # the number here can never disagree. It was 182 when the register was written
  # and 174 after this sweep. A rise is not forbidden by accident -- it is
  # forbidden so that adding a control means removing one, which is the only
  # discipline that has ever made a screen smaller.
  ids <- .ui_control_ids()
  expect_lte(length(ids), 174L)
})

test_that("one control per fact: the page number, and no buttons beside it", {
  src <- .ui_src()
  ids <- .ui_control_ids(src)
  # "Back a page" / "Next page" did what the box's own arrows do.
  expect_false("rb_prev_page" %in% ids)
  expect_false("rb_next_page" %in% ids)
  expect_true("rb_page" %in% ids)
  # ...and the box is bounded and labelled with how many pages there are, which
  # is what makes the buttons unnecessary rather than merely absent.
  expect_match(.ui_block(src, "updateNumericInput\\(session, \"rb_page\", max = n", 3L),
               "Page \\(1 to %d\\)")
})

test_that("the close-call panel has one action, not a second door to the toolkit", {
  src <- .ui_src()
  ids <- .ui_control_ids(src)
  # Converting with the other template and then pressing the one door under the
  # downloads reaches the toolkit seeded with exactly that template.
  expect_false("cv_cand_go" %in% ids)
  expect_true("cv_cand_convert" %in% ids)
  expect_length(grep("input\\$cv_cand_go", src), 0L)
  # the one door is still there, on every route
  expect_true("cv_edit_go" %in% ids)
})

test_that("choosing a fingerprint phrase adds it, with no second button to press", {
  src <- .ui_src()
  ids <- .ui_control_ids(src)
  expect_true("g_fp_pick" %in% ids)
  expect_false("g_fp_add" %in% ids)
  # the choice IS the action, and it clears itself so the same phrase can never
  # be added twice by a re-render
  blk <- .ui_block(src, "observeEvent\\(input\\$g_fp_pick", 12L)
  expect_match(blk, "updateTextAreaInput\\(session, \"g_fp\"")
  expect_match(blk, "updateSelectInput\\(session, \"g_fp_pick\", selected = \"\"\\)")
  expect_match(blk, "ignoreInit = TRUE")
})

test_that("Admin's bulk audit audits and never converts", {
  # Register 1b: five doors into one engine call, and this was one of them --
  # a tick-box on the tab about what is FAILING that wrote real outputs and real
  # log records. Convert's own picker takes thirty files and is the door.
  src <- .ui_src()
  expect_false("adm_ba_convert" %in% .ui_control_ids(src))
  expect_length(grep("input\\$adm_ba_convert", src), 0L)
  expect_match(.ui_block(src, "adm_slot\\$start\\(\"audit\"", 8L), "convert = FALSE", fixed = TRUE)
  # and the promise on the screen says so
  expect_match(.ui_block(src, "h4\\(\"Check a pile of files at once\"\\)", 2L),
               "Nothing is converted or saved", fixed = TRUE)
})

test_that("the builder's preview offers the workbook, not the same rows twice", {
  src <- .ui_src()
  ids <- .ui_control_ids(src)
  expect_true("rb_dl_xlsx" %in% ids)
  expect_true("rb_dl_values" %in% ids)   # the only file the workbook's sheet is not
  expect_false("rb_dl_csv" %in% ids)
  expect_length(grep("output\\$rb_dl_csv", src), 0L)
  # Convert still offers the long CSV once the template is saved, so nothing is
  # unreachable -- it is just not offered twice.
  expect_length(grep('c\\(xlsx = "dl_xlsx", csv = "dl_csv"\\)', src), 1L)
})

test_that("the drawn-column floor is engine tuning, not a number typed into a screen", {
  src <- .ui_src()
  # the value lives with the rest of the engine's geometry
  p <- file.path(engine_root(), "R", "params.R")
  params <- readLines(p, warn = FALSE)
  expect_length(grep("^PARAM_DOC_MIN_COL_PT <- ", params), 1L)
  expect_length(grep("^PARAM_DOC_SAME_PAGE_PT <- ", params), 1L)
  # and app.R only names it
  expect_match(.ui_block(src, "\\.RB_MIN_COL <- ", 1L), "PARAM_DOC_MIN_COL_PT")
  expect_length(grep("\\.RB_MIN_COL <- [0-9]", src), 0L)
  # the page-size slack too: no bare 2 deciding whether a document is a different
  # size from the one the template was drawn on
  expect_match(.ui_block(src, "output\\$rb_frame_note <- renderText", 10L),
               "PARAM_DOC_SAME_PAGE_PT")
})

test_that("both builders ask who produced the document, and ask for a phrase, the same way", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  # The report builder said "Issuer" -- the field's name in the file -- where the
  # statement toolkit asks a question. Two words for one question is the drift
  # the two routes are meant not to have.
  expect_match(joined, 'textInput\\("rb_bank", "Who produced this document\\?"')
  expect_match(joined, 'textInput\\("g_bank", "Which bank is this statement from\\?"')
  expect_length(grep('textInput\\("rb_bank", "Issuer"', src), 0L)
  # ...and the fingerprint is asked for in one sentence on both, saying the same
  # three things: one per line, all must appear, never a person's name. (Joined:
  # the statement side's sentence is wrapped over two source lines.)
  flat <- gsub('",\\s*\\n?\\s*"', "", joined)
  for (frag in c("One phrase per line - all must appear, and they must not be words every statement carries",
                 "One phrase per line - all must appear, and they must not be words every document carries"))
    expect_true(grepl(frag, flat, fixed = TRUE), info = frag)
})

test_that("the messages this sweep rewrote are one sentence and carry no id", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  # A second sentence pointing at a control that is on screen two inches away.
  expect_length(grep("Use \\\\u201cStart a new template\\\\u201d", src), 0L)
  expect_match(joined, "Read as another example of this template - only the page changed\\.")
  # A second sentence describing the panel the message is printed over: the
  # notification is now the count and the correction, and stops.
  expect_match(joined, 'sprintf\\("%d columns, ending on page %d\\.%s"')
  # Reassurance.
  expect_length(grep("Nothing else will change", src), 0L)
  # The save note named the template id, which the header bar above already names.
  expect_match(.ui_block(src, "output\\$rb_save_note <- renderText", 14L),
               "Replaces the saved template with %s\\.")
  # A file PATH in a message on screen.
  expect_length(grep("Reloaded %s into the editor", src), 0L)
})

# ---------------------------------------------------------------------------
# THE CONTROL SWEEP OF 2026-08-26. Every case below is one control that changed
# nothing the person chose, or that only meant something in one state and was
# always on screen. The rule each is held to: never remove the only route to a
# capability, and never leave a control that answers a question the tool has
# already answered.
# ---------------------------------------------------------------------------

test_that("the QID is read from the box, not confirmed with a second press", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # "Use this QID" was a button whose only job was to confirm the box above it --
  # and until it was pressed, a perfectly valid QID sat visible in the box with
  # Convert greyed out and a caption underneath reading "Enter your QID above".
  expect_length(grep("cv_qid_set", src), 0L)
  expect_match(joined, 'QID_PATTERN <- "\\^\\[A-Za-z0-9\\]\\{6\\}\\$"')
  blk <- .ui_block(src, "observeEvent\\(input\\$cv_qid,", 6L)
  expect_match(blk, "grepl\\(QID_PATTERN, v\\)")
  expect_match(blk, "cv_qid\\(toupper\\(v\\)\\)")
  # the refusal did not disappear with the button -- it moved to the box, and it
  # waits until the answer cannot come right rather than nagging mid-word
  why <- .ui_block(src, 'output\\$cv_qid_why <- renderUI', 9L)
  expect_match(why, "A QID is six letters or numbers")
  expect_match(why, "nchar\\(v\\) <= 6L")
  # ...and the reason the button is off is still said where the button is
  expect_match(joined, "Enter your QID above - it records who ran this conversion\\.")
})

test_that("Group by and Measure exist only on the view they act on", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # On "Balance over time" and "Running total" the data is drawn ungrouped, so
  # "Group by" silently meant "date label style" -- and "Measure = Count" printed
  # real account balances as bare integers with no dollar sign under a y-label
  # reading "Balance". A money figure rendered as a non-money figure, by a control
  # the person was invited to touch.
  for (id in c("an_group", "an_unit"))
    expect_match(joined, sprintf("conditionalPanel\\(\"input.an_view == 'inout'\", class = \"col-sm-4\",\\s*\\n\\s*(selectInput|radioButtons)\\(\"%s\"", id))
  # belt and braces in the plot itself: nothing they were last set to may leak
  plot <- .ui_block(src, "output\\$cv_trend <- renderPlot", 12L)
  expect_match(plot, 'if \\(view != "inout"\\) unit <- "amount"')
  # and the date axis works its own labels out instead of following "Group by"
  xd <- .ui_block(src, "xdate <- function\\(dates\\)", 6L)
  expect_false(grepl("grp", xd, fixed = TRUE))
  expect_match(xd, "sp > 120")
  # the three views themselves are untouched -- this is a defer, not a delete
  expect_match(joined, 'selectInput\\("an_view", "Show"')
})

test_that("the Advanced tab holds the live template without being asked", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # "Load current settings" was a button for a box that went stale the instant
  # anything on Simple was touched, with nothing on screen saying so -- so the way
  # it was used was: edit a stale template, Check & apply, and quietly revert your
  # own Simple choices.
  expect_length(grep("g_adv_load", src), 0L)
  blk <- .ui_block(src, "observeEvent\\(input\\$g_tabs,", 6L)
  expect_match(blk, 'identical\\(input\\$g_tabs, "Advanced"\\)')
  expect_match(blk, 'updateTextAreaInput\\(session, "g_yaml", value = template_yaml\\(guided_live\\(\\)\\)\\)')
  # the only way back from text to the live template stays
  expect_match(joined, 'actionButton\\("g_adv_apply", "Check & apply"')
})

test_that("uploading an example opens the builder on BOTH routes", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # The same act -- "I have an example, teach it" -- cost one press on the report
  # route and two on the statement route, and the extra press was on the route
  # Beth is likelier to be on.
  expect_match(joined, "\\.ts_open_toolkit <- function\\(\\)")
  blk <- .ui_block(src, "observeEvent\\(input\\$ts_file, \\{\\s*$", 4L)
  expect_match(paste(src, collapse = "\n"),
               "observeEvent\\(input\\$ts_file, \\{\\s*\\n\\s*if \\(!identical\\(input\\$ts_doctype")
  # the button is not deleted -- it is the way back in after Cancel, and it is not
  # on screen when there is nothing for it to open
  expect_match(joined, "conditionalPanel\\(\"input.ts_doctype == 'statement' && output.ts_have_file == true\",\\s*\\n\\s*actionButton\\(\"ts_go\"")
  expect_match(joined, 'output\\$ts_have_file <- reactive')
})

test_that("the same question is asked in the same words wherever it is asked", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  # Convert's "What is this?", Add-a-template's "What is this?" and the
  # cannot-tell card on the result all offer the same two answers. They had
  # drifted -- Something/Anything, and a "summary" on one screen only.
  n <- length(grep('"A bank or card statement"', src, fixed = TRUE))
  expect_gte(n, 3L)
  expect_gte(length(grep('"Something else - a report, a form, a letter"', src, fixed = TRUE)), 3L)
  expect_length(grep('A bank or card statement - a table of transactions', src, fixed = TRUE), 0L)
  expect_length(grep('Anything else - a report, a form, a summary, a letter', src, fixed = TRUE), 0L)
})

test_that("the column edit panel asks for a name and a kind, and nothing else", {
  src <- .ui_src()
  w <- paste(src, collapse = "\n")
  # "Done with this column" closed a panel that closes itself: Edit on another
  # column reselects, Save and Cancel clear the draft, and Cancel in the sticky
  # banner un-arms the drag from where the person is actually looking.
  expect_length(grep("rb_cdone", src), 0L)
  # the typed edges are NOT a duplicate of the drag (typed is exact, dragged is
  # approximate) so they stay -- behind the same disclosure their four siblings on
  # the start/end panel already sit behind
  i_sum <- regexpr("Type the exact edges instead", w, fixed = TRUE)
  expect_gt(i_sum, 0L)
  for (b in c("rb_cx0", "rb_cx1"))
    expect_gt(regexpr(sprintf('"%s"', b), w, fixed = TRUE), i_sum, label = b)
  for (b in c("rb_cname", "rb_ckind"))
    expect_lt(regexpr(sprintf('"%s"', b), w, fixed = TRUE), i_sum, label = b)
})

# ---------------------------------------------------------------------------
# THE WORDS SWEEP. "We DO NOT want the platform getting overly verbose." One
# sentence per thing; the second sentence is usually the screen explaining a
# control that is visible directly above it. And the two things the verifier
# caught, which were wrong rather than merely long.
# ---------------------------------------------------------------------------

# VERIFIER FINDING 2. R/forms.R writes the audit-log warning with
# status_message("needs_review", ...) and leaves res$status alone. So on a form
# or a report it landed in the BODY of a green card headed "Converted
# successfully" -- a sentence claiming a severity the card denied -- and on a
# clean STATEMENT it reached the screen at all: cv_status returns NULL on a clean
# transaction result and cv_headline never rendered res$messages. A workbook with
# no record of how it was produced, and nothing on screen saying so.
test_that("a conversion with no audit record says so, on both routes, and is not green", {
  gap <- .ui_fun(".audit_gap", consts = ".AUDIT_GAP_RX")
  line <- .ui_fun(".audit_line", consts = ".AUDIT_GAP_RX")
  msg <- paste0("needs_review: this conversion was not recorded in the audit log; ",
                "tell whoever looks after this server before the file is relied on")
  # the gap is seen whether the engine set the field or only wrote the sentence
  expect_true(gap(list(status = "ok", log_error = "the audit log folder could not be written to")))
  expect_true(gap(list(status = "ok", messages = msg)))
  expect_false(gap(list(status = "ok", messages = "ok: matched anz_everyday_pdf, 22 row(s)")))
  expect_false(gap(list(status = "ok")))
  # ...and the words are the ENGINE'S, read back off the result, never a second copy
  expect_match(line(list(messages = msg)), "^this conversion was not recorded in the audit log")
  expect_match(line(list(messages = msg)), "tell whoever looks after this server")

  src <- .ui_src()
  # BOTH verdicts carry it, in the same place, and neither stays green while it does
  stat <- .ui_block(src, "output\\$cv_status <- renderUI", 60L)
  expect_match(stat, 'if \\(identical\\(lvl, "high"\\) && \\.audit_gap\\(res\\)\\) lvl <- "medium"')
  expect_match(stat, "\\.audit_note\\(res\\)")
  hero <- .ui_block(src, "output\\$cv_headline <- renderUI", 55L)
  expect_match(hero, "\\.audit_gap\\(res\\)")
  expect_match(hero, "\\.audit_note\\(res\\)")
  # and it is said ONCE: plain_messages drops it, because .audit_note carries it
  pm <- .ui_block(src, "plain_messages <- function\\(m\\)", 24L)
  expect_match(pm, "m\\[!grepl\\(\\.AUDIT_GAP_RX, m\\)\\]")
})

# VERIFIER FINDING 1. build_diagnostics() serves all three routes and is written
# in statement vocabulary, so a report nothing recognised was told to open "the
# template toolkit", to read off "the closest match and the missing columns"
# (the report detector reports neither), and that its running balances may not be
# continuous across accounts. A report has no running balance; the same screen
# says so two inches higher.
test_that("the diagnostics table speaks the route the run actually took", {
  e <- new.env(parent = globalenv())
  sys.source(file.path(engine_root(), "ui_labels.R"), envir = e)
  d <- data.frame(
    category = c("unknown_format", "combined_statement", "oversized"),
    severity = c("high", "info", "medium"),
    detail = c("no template matched this file",
               "6 account numbers appear in one statement period",
               "140 pages in one file"),
    how_to_fix = c("Add a template for this layout in the template toolkit (Add a template tab: upload a sample and confirm what it detects). The closest match and the missing columns are in the detail.",
                   "Looks like a combined statement (several accounts/products, or transfer counterparties named in transactions). If transactions from more than one account are mixed, running balances won't be continuous across them - review per account.",
                   "Very long PDFs (>100 pages) may hit tool limits; split into smaller files if extraction stalls."),
    stringsAsFactors = FALSE)

  # THE STATEMENT-ONLY REMEDY IS WRONG ON EVERY ROUTE, so it is replaced on both
  for (stmt in c(TRUE, FALSE)) {
    got <- e$diag_for_route(d, statement = stmt)
    expect_false(grepl("template toolkit", got$how_to_fix[1], fixed = TRUE))
    expect_false(grepl("closest match", got$how_to_fix[1], fixed = TRUE))
    expect_match(got$how_to_fix[1], "Add a template tab", fixed = TRUE)
    # a category with nothing to correct keeps the engine's own words, verbatim
    expect_identical(got$how_to_fix[3], d$how_to_fix[3])
    expect_identical(got$detail[3], d$detail[3])
    # NO ROW IS ADDED, DROPPED OR RE-GRADED -- only the wording moves
    expect_identical(nrow(got), nrow(d))
    expect_identical(got$category, d$category)
    expect_identical(got$severity, d$severity)
  }

  # ON THE STATEMENT ROUTE the running-balance warning is a real warning: untouched
  onstmt <- e$diag_for_route(d, statement = TRUE)
  expect_identical(onstmt$how_to_fix[2], d$how_to_fix[2])
  expect_identical(onstmt$detail[2], d$detail[2])
  expect_match(e$plain_diag("combined_statement", TRUE), "in one statement")

  # ON THE OTHER ROUTE it stops talking about running balances and statements
  other <- e$diag_for_route(d, statement = FALSE)
  expect_false(grepl("running balance", other$how_to_fix[2], fixed = TRUE))
  expect_false(grepl("statement", other$how_to_fix[2], fixed = TRUE))
  expect_false(grepl("statement", other$detail[2], fixed = TRUE))
  expect_match(other$detail[2], "6 account numbers", fixed = TRUE)   # the count survives
  expect_match(e$plain_diag("combined_statement", FALSE), "in one document")
  expect_match(e$plain_diag("multiple_statements", FALSE), "several documents")
  # an empty or malformed frame is handed straight back
  expect_identical(e$diag_for_route(NULL, FALSE), NULL)
  expect_identical(nrow(e$diag_for_route(d[0, ], FALSE)), 0L)
})

test_that("every reader of the diagnostics goes through the one route-aware door", {
  src <- .ui_src()
  # .statement_route asks what she SAID, not what kind the engine got as far as
  # setting: on the other route R/forms.R hands back the statement attempt
  # unchanged, so .is_txn_result says TRUE about a document she called a report.
  rt <- .ui_block(src, "\\.statement_route <- function", 3L)
  expect_match(rt, "res\\$asked_kind")
  expect_match(rt, '"other"')
  # every reader of the frame's WORDING goes through .diagnostics_of
  for (blk in c(.ui_block(src, "\\.blocking_diag <- function", 12L),
                .ui_block(src, "top_diagnostics <- function", 14L),
                .ui_block(src, "output\\$cv_diag <- renderDT", 18L)))
    expect_match(blk, "\\.diagnostics_of\\(res\\)")
  # ...and the "What" column is mapped route-aware too
  expect_false(any(grepl("plain_label(dg$category[i], DIAG_PLAIN)", src, fixed = TRUE)))
  expect_true(any(grepl("plain_diag(dg$category[i], .statement_route(res))", src, fixed = TRUE)))
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

test_that("both halves of the OTHER route say the no-reconciliation fact the same way", {
  src <- .ui_src()
  form <- .ui_block(src, "output\\$cv_form <- renderUI", 12L)
  tabs <- .ui_block(src, "output\\$cv_tables <- renderUI", 34L)
  expect_match(form, "No running balance behind a form, so nothing reconciles", fixed = TRUE)
  expect_match(tabs, "No running balance behind a report, so nothing reconciles", fixed = TRUE)
  # and the second verdict card no longer repeats the engine's three counts, which
  # the card above already prints AND names the tables in
  expect_false(grepl("found by position rather than by a heading", tabs, fixed = TRUE))
  expect_match(tabs, "Every table was found by its own heading and every column filled", fixed = TRUE)
  # the third saying of it, at the bottom of the page beside the feedback box, is gone
  expect_false(any(grepl("adds up to a balance the tool can check", src, fixed = TRUE)))
})

test_that("one measurement has one wording wherever the screen says it", {
  src <- .ui_src()
  # It was spelled three ways: "claimed by no column" in the builder, "not claimed
  # by any column" on Convert, "not in any column" in the engine. A reviewer who
  # meets one measurement under three names has to work out it is one measurement.
  expect_false(any(grepl("claimed by no column", src, fixed = TRUE)))
  expect_false(any(grepl("not claimed by any column", src, fixed = TRUE)))
  expect_true(any(grepl("were not in any column", src, fixed = TRUE)))
  expect_false(any(grepl("found by position rather than by a heading", src, fixed = TRUE)))
  expect_true(any(grepl("were found by position, not by their heading", src, fixed = TRUE)))
})
