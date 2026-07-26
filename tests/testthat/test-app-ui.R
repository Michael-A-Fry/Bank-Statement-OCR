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
.ui_block <- function(src, pat, n = 40L) {
  i <- grep(pat, src)
  testthat::expect_true(length(i) >= 1)
  paste(src[i[1]:min(i[1] + n, length(src))], collapse = " ")
}

# ---------------------------------------------------------------------------
# The design tokens. There used to be TWO :root blocks; the first was shadowed by
# the second, so a maintainer editing a colour there saw nothing change on screen.
test_that("the design tokens are declared in exactly one place", {
  joined <- paste(.ui_src(), collapse = "\n")
  expect_equal(length(gregexpr(":root\\{", joined)[[1]]), 1L)
  # ...and the surviving block still declares every token the CSS consumes, or a
  # surface loses its colour. --panel was the one only the deleted block had.
  root <- sub("(?s).*:root\\{(.*?)\\}.*", "\\1", joined, perl = TRUE)
  used <- unique(gsub(".*var\\((--[a-z0-9-]+)\\).*", "\\1",
                      unlist(regmatches(joined, gregexpr("var\\(--[a-z0-9-]+\\)", joined)))))
  expect_identical(sort(setdiff(used, unlist(regmatches(root, gregexpr("--[a-z0-9-]+", root))))),
                   character(0))
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
  block <- .ui_block(src, "output\\$g_status <- renderUI", 25L)
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

test_that("the visible bank picker actually reaches the conversion", {
  # A control that looks like it works and does not is worse than no control.
  joined <- paste(.ui_src(), collapse = "\n")
  expect_match(joined, "pick\\(input\\$cv_bank_quick\\) %\\|\\|% pick\\(input\\$cv_bank\\)")
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
