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
  env <- new.env(parent = globalenv())      # globalenv carries the engine (%||%, safe)
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
  block <- .ui_block(src, "output\\$cv_status <- renderUI", 45L)
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
  blk <- .ui_block(src, "output\\$cv_status <- renderUI", 40L)
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
  strip <- .ui_fun("plain_messages")
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
  expect_match(.ui_block(src, "output\\$cv_status <- renderUI", 45L),
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
  blk <- .ui_block(src, "output\\$cv_batch <- renderDT", 60L)
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
  blk <- paste(src[i:(i + 26L)], collapse = " ")
  expect_match(blk, 'notify_once\\("adm_tpl_hide"')
  expect_match(blk, "template_display_name")      # by its name, not its id
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
  src <- paste(.ui_src(), collapse = " ")
  for (id in c("g_pdf_brush", "rb_brush")) {
    blk <- regmatches(src, regexpr(sprintf('brushOpts\\("%s".{0,120}', id), src, perl = TRUE))
    # A debounce of ANY length is the promise -- one commit when the mouse stops,
    # not one per mouse-move. The builder runs a shorter one than the toolkit
    # because a drag there is a step in a wizard and waiting a second and a half
    # to be told it worked is its own kind of broken.
    expect_match(blk, 'delayType = "debounce"')
    ms <- as.integer(sub(".*delay = ([0-9]+).*", "\\1", blk))
    expect_gte(ms, 500L)
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
  blk <- src[i:min(i + 22L, length(src))]
  expect_match(paste(blk, collapse = " "), "return\\(invisible\\(FALSE\\)\\)")  # no modal is shown
  blk <- blk[!grepl("^\\s*#", blk)]
  expect_false(any(grepl("Not a transaction table?", blk, fixed = TRUE)))
  expect_false(any(grepl("top of this window", blk, fixed = TRUE)))
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
  blk <- .ui_block(.ui_src(), "output\\$cv_batch <- renderDT", 60L)
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
  expect_match(.ui_block(src, "output\\$cv_status <- renderUI", 40L), "confidence: %s")
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
  expect_match(.ui_block(src, "CEILING_NOTE <- paste", 4L), "cannot go higher than medium")
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

# A clean result read by the WRONG template looks perfect. cv_rematch is the only
# route back from that, and nothing rendered it -- while cv_teach stayed silent on
# a clean result precisely BECAUSE it believed cv_rematch was on screen.
test_that("a clean result still offers a way to fix a wrong match", {
  src <- .ui_src(); joined <- paste(src, collapse = "\n")
  expect_match(joined, 'uiOutput\\("cv_rematch"\\)')
  expect_match(joined, "output\\$cv_rematch <- renderUI")
  blk <- .ui_block(src, "output\\$cv_rematch <- renderUI", 24L)
  expect_match(blk, "cv_rematch_go")
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
  for (id in c("adm_ba_report", "adm_ba_csv", "adm_audit_dl", "adm_up_audit", "adm_inbox_audit"))
    expect_match(joined, sprintf('dl_when\\("%s"', id))
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
  expect_match(.ui_block(src, "observeEvent\\(input\\$adm_lex_reload", 5L), "adm_lex_msg")
  expect_match(.ui_block(src, "observeEvent\\(input\\$adm_dict_reload", 5L), "adm_dict_msg")
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
  top <- .ui_fun("top_diagnostics")
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
  expect_match(blk, "plain_label\\(dg\\$category\\[i\\], DIAG_PLAIN\\)")   # never the code
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
  blk <- .ui_block(.ui_src(), "output\\$cv_rematch <- renderUI", 24L)
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
  # reason. The ORDER is the promise, not the line number.
  blk <- .ui_block(.ui_src(), "output\\$cv_teach <- renderUI", 160L)
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
  expect_match(why(list(status = "failed")), "Nothing was read from this file")
  expect_match(why(list(status = "unsupported")), "No template read this document")
  for (st in c("failed", "unsupported"))
    expect_match(why(list(status = st)), "nothing to check and no fields to report")
  blk <- .ui_block(.ui_src(), "output\\$cv_detail <- renderUI", 26L)
  expect_match(blk, "if \\(has_kpis\\) DTOutput\\(\"cv_kpis\"\\) else said")
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
  expect_match(blk, "every statement converted here")
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
  blk <- .ui_block(src, "output\\$cv_batch <- renderDT", 60L)
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
