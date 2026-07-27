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
.ui_fun <- function(name, also = character(0)) {
  src <- .ui_src()
  env <- new.env(parent = globalenv())      # globalenv carries the engine (%||%, safe)
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
  block <- .ui_block(src, "output\\$cv_status <- renderUI", 30L)
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
  block <- .ui_block(src, "output\\$cv_status <- renderUI", 40L)
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
  block <- .ui_block(src, "output\\$g_status <- renderUI", 45L)
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
  expect_equal(length(gregexpr("Page \\(1 to %d\\)", joined)[[1]]), 3L)
})

# ---------------------------------------------------------------------------
# The 'Other document' builder asked for the file a second time, so the picker at
# the top of the page did nothing at all on that path and nothing said so.
test_that("the form builder uses the document already uploaded", {
  src <- .ui_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "fb_doc <- reactive")
  expect_match(.ui_block(src, "fb_doc <- reactive", 8L), "input\\$ts_file")
  # the render and the preview both go through it, not the second picker
  expect_false(grepl("read_input(input$fb_sample$datapath)", joined, fixed = TRUE))
  expect_false(grepl("render_page_view(input$fb_sample$datapath", joined, fixed = TRUE))
  expect_match(.ui_block(src, "output\\$fb_has_sample <- reactive", 2L), "fb_doc\\(\\)")
})

# ---------------------------------------------------------------------------
# Admin: the common case -- "this bank writes a word we've never met" -- is now a
# plain-English control, and it writes through the SAME engine function the
# Approve button uses, so there is one way a word gets into the vocabulary.
test_that("teaching the engine a word does not need YAML, and has one write path", {
  src <- .ui_src()
  block <- .ui_block(src, "observeEvent\\(input\\$adm_word_add", 18L)
  expect_match(block, "req\\(admin_ok\\(\\)\\)")            # privileged, like every other
  expect_match(block, "lexicon_append\\(kind, tolower\\(w\\), LEXICON_PATH\\)")
  expect_false(grepl("writeLines", block, fixed = TRUE))    # no second writer
  # the whole-file editor is kept, not deleted -- just no longer the front door
  joined <- paste(src, collapse = "\n")
  expect_match(joined, 'textAreaInput\\("adm_lex_edit"')
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
  i_adv  <- grep('tags\\$summary\\("It picked the wrong bank\\?"\\)', src)
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
  # ONE Convert button, on the Convert tab -- no separate batch tab or trigger
  expect_length(grep('actionButton\\("cv_go"', src), 1L)
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
  joined <- paste(.ui_src(), collapse = "\n")
  expect_match(joined, "convert_batch\\(paths,")
  expect_true(exists("convert_batch"))
  expect_true(exists("batch_summary"))
  expect_true(exists("BATCH_STATUSES"))
  # ...and app.R never calls the single-file front door in a loop of its own
  expect_false(grepl("for \\([a-z]+ in seq_along\\(paths\\)\\)[^\n]*convert_document", joined))
})

test_that("the QID is asked once for the whole batch, before it starts", {
  src <- .ui_src()
  i_go   <- grep("observeEvent\\(input\\$cv_go, \\{", src)
  expect_length(i_go, 1L)
  blk <- src[i_go:(i_go + 25)]
  i_qid   <- grep("is.na\\(cv_qid\\(\\)\\)", blk)[1]
  i_batch <- grep("run_batch\\(f\\)", blk)[1]
  expect_false(is.na(i_qid) || is.na(i_batch))
  expect_true(i_qid < i_batch, info = "the batch starts before who-ran-this is settled")
  # asked once, not once per file: nothing inside run_batch asks again
  joined <- paste(.ui_src(), collapse = "\n")
  expect_equal(length(gregexpr("Enter your QID first", joined)[[1]]), 1L)
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
  expect_match(.ui_block(src, "run_conversion <- function", 45L), "show_result\\(res, list\\(path = src")
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
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_batch <- renderDT", 34L)
  expect_match(blk, "BATCH_STATUSES", fixed = TRUE)
  expect_match(blk, "orderData", fixed = TRUE)          # verdict sorts by severity
  expect_match(blk, "failing_check", fixed = TRUE)      # the failure kind is a column
  expect_match(blk, "severity <- match\\(b\\$status, BATCH_STATUSES")
  expect_match(blk, 'order = list\\(list\\(5, "desc"\\), list\\(4, "asc"\\)\\)')
  expect_false(grepl("length(BATCH_STATUSES) + 1L -", blk, fixed = TRUE))
  # the table is sortable and clickable at all
  expect_match(blk, 'selection = "single"', fixed = TRUE)
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
  blk <- .ui_block(.ui_src(), "convert_batch\\(paths,", 16L)
  expect_match(blk, "progress = function\\(i, nn, f\\)")
  expect_match(blk, "setProgress", fixed = TRUE)
  expect_match(blk, "basename\\(f\\)")                 # says WHICH file, not just a bar
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
  expect_match(joined, "bank <- bank_choice\\(\\)")
  expect_match(joined, "forced_tpl <- force_tpl %\\|\\|% tpl_choice\\(\\)")
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
  expect_match(.ui_block(src, "output\\$g_eff_msg <- renderUI", 12L),
               "\\.eff_backwards\\(input\\$g_eff_from, input\\$g_eff_to\\)")
  expect_length(grep("\\.EFF_BACKWARDS_MSG", src), 3L)   # declared once, used twice
  # the deleted machinery really is gone, not merely unreferenced
  joined <- paste(src, collapse = "\n")
  for (dead in c(".eff_bad", ".eff_txt", ".eff_show", ".eff_problems", ".eff_sentence"))
    expect_false(grepl(dead, joined, fixed = TRUE), info = dead)
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
  ok <- .ui_fun(".eff_stored_ok", also = ".eff_date")
  expect_true(ok(list(effective_from = "2020-01-01", effective_to = NULL)))
  expect_true(ok(list()))                                   # no window is fine
  expect_false(ok(list(effective_from = "last year")))
  blk <- .ui_block(src, "observeEvent\\(input\\$g_adv_apply", 30L)
  expect_match(blk, "\\.eff_stored_ok\\(parsed\\)")
  expect_match(blk, "must be dates", fixed = TRUE)
  # opening never throws...
  expect_match(joined, "value = tryCatch\\(\\.eff_date\\(v\\) %\\|\\|% \"\", error = function\\(e\\) \"\"\\)")
  # ...and never pretends the template said "always"
  expect_match(.ui_block(src, "output\\$g_eff_msg <- renderUI", 10L),
               "!\\.eff_stored_ok\\(g\\$tmpl\\)")
  expect_match(joined, "saved validity window is not a date", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# SEVERAL PERIODS IN ONE FILE. The engine reads them as ONE span, which is what
# lets the date and balance checks pass - but a span standing in for three
# quarters reads as one quarter unless the count is said beside it.
test_that("the number of statement periods is shown where the period is shown", {
  src <- .ui_src()
  blk <- .ui_block(src, "output\\$cv_summary <- renderUI", 40L)
  expect_match(blk, "n_periods", fixed = TRUE)
  expect_match(blk, "%d periods", fixed = TRUE)
  # on the same line as the range it belongs to
  expect_match(blk, 'sprintf\\("Period: %s%s%s", drange')
  # and only when there really is more than one
  expect_match(blk, "isTRUE\\(np > 1L\\)")
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
  src <- paste(.ui_src(), collapse = " ")
  for (id in c("g_pdf_brush", "fb_brush")) {
    blk <- regmatches(src, regexpr(sprintf('brushOpts\\("%s".{0,120}', id), src, perl = TRUE))
    expect_match(blk, "delay = 1500")
    expect_match(blk, 'delayType = "debounce"')
  }
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
test_that("the form builder leads with the document, not a blank list", {
  src <- .ui_src()
  draw <- grep("1\\. Draw a box round a value you want", src)
  what <- grep("2\\. What is this\\?", src)
  save <- grep("5\\. Name it and save", src)
  typed <- grep('textAreaInput\\("fb_fields"', src)
  expect_length(draw, 1L); expect_length(what, 1L); expect_length(save, 1L)
  expect_true(draw < what && what < save)          # the concrete step is first
  expect_true(typed > draw)                        # the typed list is no longer the door
  # and it is behind the disclosure, not in front of it
  det <- grep("Know the wording\\? Type them instead", src)
  expect_length(det, 1L)
  expect_true(det < typed && typed - det < 4L)
  # nothing is deleted: the shortcut still parses to the same fields{} block
  expect_true(any(grepl("parse_fields_spec\\(input\\$fb_fields\\)", src)))
  # the old edge-case section is gone as a section
  expect_false(any(grepl("Value printed away from its wording", src, fixed = TRUE)))
})

# The builder answers "what is this?" itself: what the box contains, and the
# wording printed beside it. A box drawn just PAST its value used to be named
# after the value it missed, which could never match the next document.
test_that("the builder reads the box and the wording beside it", {
  f <- .ui_fun(".fb_read_box", also = ".fb_wording")
  w <- data.frame(
    text   = c("Opening", "balance", "$875.20", "Closing", "balance", "$2,477.80"),
    x      = c(145, 190, 355, 145, 190, 355),
    y      = c(100, 100, 100, 112, 112, 112),
    width  = c(40, 35, 45, 38, 35, 55),
    height = rep(8, 6), stringsAsFactors = FALSE)
  on_value <- f(w, list(x_min = 350, x_max = 405, y_min = 96, y_max = 110))
  expect_identical(on_value$value, "$875.20")
  expect_identical(on_value$label, "Opening balance")
  # a box drawn PAST the value: nothing in it, and the label is still the wording
  past <- f(w, list(x_min = 420, x_max = 500, y_min = 96, y_max = 110))
  expect_identical(past$value, "")
  expect_identical(past$label, "Opening balance")   # not "Opening balance $875.20"
  # the row below is a different value with its own wording
  below <- f(w, list(x_min = 350, x_max = 415, y_min = 108, y_max = 122))
  expect_identical(below$label, "Closing balance")
  expect_identical(f(NULL, list(x_min = 0, x_max = 1, y_min = 0, y_max = 1))$label, "")
})

# The kind of value is read, not asked - with the SAME matchers the extractor
# uses, so whatever is offered is what the extraction would really do.
test_that("the builder reads what kind of value it is", {
  k <- .ui_fun(".fb_kind")
  expect_identical(k("$875.20"), "money")
  expect_identical(k("-$577.80"), "money")
  expect_identical(k("31 Mar 2026"), "date")
  expect_identical(k("Sam Okafor"), "text")
  expect_identical(k(""), "text")
  expect_identical(k(NULL), "text")
})

# Reading by WORDING survives the value moving on the next document; reading from
# a box does not. So the wording is preferred - but only when it actually finds
# the value she boxed. That is a question the tool can answer, which is why
# "the value is printed away from its wording" is no longer a question it asks.
test_that("wording is only trusted when it reaches the value she boxed", {
  same <- .ui_fun(".fb_same")
  expect_true(same("$875.20", "875.20"))
  expect_true(same(" 875.20 ", "$875.20"))
  expect_false(same("875.20", "2477.80"))
  expect_false(same("", ""))          # nothing read proves nothing
  expect_false(same(NA, "875.20"))
  blk <- .ui_block(.ui_src(), "observeEvent\\(input\\$fb_rf_set", 22L)
  expect_match(blk, "byword <- \\.fb_same\\(fb_by_wording\\(")
  # and the template really emits one or the other
  tpl <- .ui_block(.ui_src(), "fb_template <- reactive", 22L)
  expect_match(tpl, "any_of = list\\(b\\$label\\)")
  expect_match(tpl, "region = list\\(page = b\\$page")
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
  expect_match(blk, "fb_handoff\\(g\\$path\\)")                 # the file goes with it
  expect_match(blk, "ts_doctype", fixed = TRUE)                # ...and the answer changes
  expect_match(blk, "removeModal\\(\\)")
  # the form builder must actually READ the handoff, or the link is theatre
  expect_match(.ui_block(src, "fb_doc <- reactive", 8L), "fb_handoff\\(\\)")
})

test_that("no on-screen text points at a control that is off screen", {
  # The old wording told someone stuck inside the modal to "pick 'Something else'
  # above" - a control the modal is covering. Same mistake as N28.
  src <- .ui_src()
  expect_false(any(grepl("pick 'Something else' above", src, fixed = TRUE)))
})
