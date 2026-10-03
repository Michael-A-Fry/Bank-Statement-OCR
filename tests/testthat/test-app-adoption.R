# Static invariants over app.R.
#
# app.R is ~3,000 lines of Shiny and is NOT sourced by the suite (it needs a live
# session), so behaviour that only exists there is held to its word the same way
# test-admin_auth.R does it: read the file and assert the rule. These are the
# adoption blockers -- the ones where the app quietly did the opposite of what the
# screen said -- so a future edit that reintroduces them fails the build.

.app_src <- function() {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  readLines(app, warn = FALSE)
}
# the N lines starting at the first line matching `pat`, as one string
.app_block <- .src_block   # brace-balanced, in helper.R
# .app_fun(name) -- one of app.R's PURE helpers, lifted out and made callable, so
# a rule can be tested on BEHAVIOUR rather than on the spelling of its source.
# Found by name, read forward until it parses (which is exactly where the
# function ends), evaluated in an environment carrying the engine and the wording
# maps -- the only two things these helpers close over.
.app_fun <- function(name) {
  src <- .app_src()
  env <- new.env(parent = globalenv())     # globalenv carries the engine (%||%)
  lab <- file.path(engine_root(), "ui_labels.R")
  if (file.exists(lab)) sys.source(lab, envir = env)
  i <- grep(sprintf("^\\s*\\Q%s\\E <- function", name), src, perl = TRUE)
  testthat::expect_length(i, 1L)
  for (j in seq(i[1], min(i[1] + 250L, length(src)))) {
    f <- tryCatch(eval(parse(text = paste(src[i[1]:j], collapse = "\n"))[[1]], envir = env),
                  error = function(e) NULL)
    if (is.function(f)) return(f)
  }
  testthat::fail(paste("could not lift", name, "out of app.R"))
}

# ---------------------------------------------------------------------------
# #18 -- a template someone builds here must actually take part in detection.
# It once did not: nothing in the app ever ticked "include user-created
# templates", so a template Beth built was ignored unless she found a tick-box
# inside a collapsed panel. The tick-box itself is now gone -- whether a
# colleague's template counts is not a question to put to the person converting a
# statement -- so the invariant is stronger: they are ON, from config, with
# nothing to find.
test_that("templates built here take part in detection with nothing to switch on (#18)", {
  src <- paste(.app_src(), collapse = "\n")
  # one deployment setting, read once, defaulting ON
  expect_match(src, "USE_USER_TEMPLATES <- isTRUE\\(CONFIG\\$app\\$user_templates_default %\\|\\|% TRUE\\)")
  expect_match(src, "if \\(USE_USER_TEMPLATES\\) templates\\(\\) else proven_templates\\(\\)")
  # and NO per-conversion control anywhere -- that is the whole point
  expect_false(grepl("cv_user_templates", src, fixed = TRUE))
  expect_false(grepl("cv_use_user_tpl", src, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# #24 -- the save-then-reconvert used force_tpl = saved_id, and R/convert.R
# short-circuits a forced template past detection. The save must run a REAL
# detection pass and report it.
test_that("a successful save runs a real detection pass and reports it (#24)", {
  src <- .app_src()
  save_block <- .app_block(src, 'observeEvent\\(input\\$g_save', 80L)
  expect_match(save_block, "detect_statement\\(")        # detection actually runs
  expect_match(save_block, "recognition_summary\\(")     # ...and is turned into words
  # forcing is now the FALLBACK, not the default: it must be conditional on the
  # detection verdict, never an unconditional force that hides the answer.
  expect_false(grepl("force_tpl = saved_id[,)]", save_block))
  expect_match(save_block, "force_tpl = if \\(isTRUE\\(recog\\$ok\\)\\)")
})

test_that("the post-save conversion can see user templates before the tick lands (#24)", {
  src <- paste(.app_src(), collapse = "\n")
  # updateCheckboxInput only reaches the browser after the observer returns, so the
  # conversion that demonstrates the new template must include them explicitly --
  # otherwise it would convert without the template it just saved.
  expect_match(src, "include_user = TRUE", fixed = TRUE)
  # a deployment CAN switch user templates off; the conversion that demonstrates a
  # just-saved template must still see it, or the save appears not to have worked.
  expect_match(src, "USE_USER_TEMPLATES \\|\\| !is.null\\(force_tpl\\) \\|\\| isTRUE\\(include_user\\)")
})

# ---------------------------------------------------------------------------
# #25 -- a too-generic PDF fingerprint refuses the save, and the only place to fix
# it was the raw YAML box on the Advanced tab: a hard stop at the last step of the
# flow for a non-technical user.
test_that("the identifying phrase is editable in plain English on the Simple tab (#25)", {
  src <- .app_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, 'textAreaInput\\("g_fp"')
  expect_match(joined, "A distinctive phrase printed on this statement", fixed = TRUE)
  # it is on SIMPLE, not Advanced: the box appears before the Advanced tabPanel
  i_fp  <- grep('textAreaInput\\("g_fp"', src)[1]
  i_adv <- grep('"Advanced", br\\(\\)', src)[1]
  expect_true(!is.na(i_fp) && !is.na(i_adv) && i_fp < i_adv)
  # pre-filled from phrases actually found on her statement, not typed from memory
  expect_match(joined, "fp_candidates", fixed = TRUE)
  expect_match(joined, "header_phrases\\(read_input\\(path\\)")
  # and it feeds the template build + shows the same verdict the save would give
  expect_match(joined, "fingerprint_text = input\\$g_fp")
  expect_match(joined, 'output\\$g_fp_msg')
})

# ---------------------------------------------------------------------------
# #47 -- the UI claimed a sign-in it does not have. On the shipped one-server
# deployment the "detected" identity is the account the SERVER process runs as,
# which is identical for the whole department.
test_that("the app no longer claims a sign-in it does not have (#47)", {
  joined <- paste(.app_src(), collapse = "\n")
  expect_false(grepl("Detected as %s from your sign-in", joined, fixed = TRUE))
  # Only a host sign-in or an SSO header is a PERSON; the OS account is the
  # server's and identifies nobody. That distinction is the whole finding.
  expect_match(joined, "\\.identity_is_personal <- function\\(info\\) info\\$source %in% c\\(\"host\", \"sso\"\\)")
  # Who ran it is asked ONLY when the tool cannot work it out: a real per-person
  # sign-in renders nothing at all.
  expect_match(joined, "if \\(\\.identity_is_personal\\(detected_identity_info\\(\\)\\)\\) return\\(NULL\\)")
  # ...and where it cannot, a conversion is BLOCKED rather than recorded against
  # the server's own account. Warning-and-continue is not enough: the run would
  # still be written, naming nobody, and a defensible output cannot rest on that.
  #
  # The gate has a NAME now (.identity_ok), because writing it out inside cv_go
  # meant the other way into a conversion - "Try it on a sample statement" - had
  # no gate at all and produced a full result, with downloads, recorded against
  # the server account. So the rule asserted here is the stronger one: one gate,
  # and EVERY entry point goes through it.
  expect_match(joined, "\\.identity_ok <- function")
  expect_match(joined, "\\.identity_is_personal\\(detected_identity_info\\(\\)\\) \\|\\| !is\\.na\\(cv_qid\\(\\)\\)")
  src_go <- .app_src()
  for (h in c("observeEvent\\(input\\$cv_go", "observeEvent\\(input\\$cv_try_sample"))
    expect_match(.app_block(src_go, h, 25L), "if \\(!\\.identity_ok\\(\\)\\) return\\(\\)")
  # asked once a session, not once a statement
  expect_match(joined, "cv_qid <- reactiveVal", fixed = TRUE)
  expect_match(joined, "Recording as %s", fixed = TRUE)
})

test_that("every conversion stamps the detected identity and its source (#47)", {
  src <- .app_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "stamp_identity <- function", fixed = TRUE)
  # both facts: the QID a person gave, and what the machine could establish.
  expect_match(joined, "identity_fields\\(attested = cv_qid\\(\\), detected = info\\$who")
  # ...and it happens on the ONE shared conversion path, so no route can skip it
  expect_match(.app_block(src, "run_conversion <- function", 40L), "stamp_identity\\(")
  # a failure to complete the audit record is never silent
  expect_match(joined, "audit record could not be completed", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# #6/#32/#7 -- every converted statement is copied into uploads/ and was kept
# forever, the user was never told, and the per-session scratch folders were never
# reclaimed on a server that runs for months.
# REWRITTEN, deliberately. This used to require the retention line to sit under
# the Convert file picker. It has been taken OFF that page (owner's decision): it
# is a fact about how the server is configured, not a question the person
# converting a statement answers or a thing she can act on, and it was the first
# line on the page with nothing to do with converting a statement. Nothing about
# retention itself changed - the note is still generated from the ONE setting the
# purge uses, so what is said and what happens still cannot disagree; it is now
# said where retention is actually managed and acted on.
test_that("the retention note comes from the setting, and sits where it is acted on", {
  src <- .app_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "UPLOADS_NOTE <- uploads_retention_note\\(UPLOADS_KEEP_DAYS\\)")
  # no longer under the Convert file picker
  expect_false(grepl("UPLOADS_NOTE", .app_block(src, 'fileInput\\("cv_file"', 8L), fixed = TRUE))
  # ...but still directly above the button that does the deleting, so the sentence
  # and the button can only ever quote the same number
  # WAS: h4. Retention now sits under one "Housekeeping" heading on the Health tab
  # (register 1c), so it is a level down; the rule -- the sentence directly above
  # the button that does the deleting -- is what this pins and is unchanged.
  expect_match(.app_block(src, 'h5\\("Saved statements - retention"\\)', 3L),
               "UPLOADS_NOTE", fixed = TRUE)
})

test_that("uploads and scratch folders are actually reclaimed (#6/#7)", {
  src <- .app_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "purge_uploads\\(UPLOADS_DIR, keep_days = UPLOADS_KEEP_DAYS\\)")
  expect_match(joined, "sweep_temp_dirs\\(keep_hours = 24\\)")
  expect_match(joined, "session$onSessionEnded", fixed = TRUE)
  # an admin can also run the purge on demand -- and that handler is gated like
  # every other privileged one
  expect_match(.app_block(src, 'observeEvent\\(input\\$adm_purge_uploads', 3L),
               "req\\(admin_ok\\(\\)\\)")
  # the retention period is a setting, never a number typed into the code twice
  expect_match(joined, "CONFIG\\$retention\\$uploads_keep_days")
})

# A DESTRUCTIVE TIDY-UP RUNS ON THE SITE'S OWN NUMBER OR IT DOES NOT RUN.
#
# UPLOADS_KEEP_DAYS is read from config/config.yaml. When that file cannot be
# parsed the app starts on BUILT-IN DEFAULTS, so the number driving an
# irreversible delete is one this deployment never chose. REPRODUCED end to end:
# a site that had set `uploads_keep_days: 0` (keep indefinitely -- what a unit
# holding evidence sets, and what the example config offers in those words) took
# one stray tab in config.yaml, which is precisely what the docs warn about
# because they tell a non-technical analyst to edit it in Notepad. On the next
# restart the default 90 applied and every stored client statement older than 90
# days was deleted and stamped `purged`, while the console warning saying the
# settings had not loaded scrolled past above it.
test_that("the startup purge does not delete client statements on a settings file that did not load", {
  src <- .app_src()
  i <- grep("safe\\(purge_uploads\\(UPLOADS_DIR", src)
  expect_length(i, 1L)
  # the startup call is inside the "settings loaded" branch...
  guard <- paste(src[max(1L, i - 6L):i], collapse = " ")
  expect_match(guard, "if \\(is\\.null\\(CONFIG_ERROR\\)\\)")
  # ...and the operator is told that nothing was deleted, so a folder that did not
  # shrink is explained rather than mysterious
  after <- paste(src[i:min(i + 10L, length(src))], collapse = " ")
  expect_match(after, "Nothing has been deleted", fixed = TRUE)
  expect_match(after, "warning\\(")
  # the deliberate, asked-for purge on the Admin button is NOT gated on this: it
  # states its own number and is confirmed, and a maintainer who has just fixed
  # the settings file must still be able to run it
  expect_match(.app_block(src, 'observeEvent\\(input\\$adm_purge_confirm', 6L),
               "purge_uploads\\(UPLOADS_DIR, keep_days = UPLOADS_KEEP_DAYS\\)")
})

# ---------------------------------------------------------------------------
# #60 -- a settings file that does not parse must not silently weaken the install.
test_that("a broken settings file is announced at startup and on screen (#60)", {
  joined <- paste(.app_src(), collapse = "\n")
  expect_match(joined, "CONFIG_ERROR <- config_error\\(CONFIG\\)")
  expect_match(joined, "SETTINGS FILE NOT LOADED", fixed = TRUE)
  expect_match(joined, 'output\\$adm_cfg_banner')
  # the banner must be reachable without signing in -- a broken config is often
  # exactly why nobody can sign in
  expect_match(joined, 'outputOptions\\(output, "adm_cfg_banner", suspendWhenHidden = FALSE\\)')
})

test_that("the launcher script warns too - that is how the server is started (#60)", {
  p <- file.path(engine_root(), "scripts", "run_app.R")
  skip_if_not(file.exists(p))
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(txt, "SETTINGS FILE NOT LOADED", fixed = TRUE)
  expect_match(txt, "admin_password_is_default", fixed = TRUE)
})

# A template may declare several candidate date formats (templates/asb_everyday_csv.yaml
# does, so it can read both ASB exports). The Simple tab is one dropdown showing the
# first candidate, so merely opening the toolkit and pressing Save must NOT write that
# single value back and silently narrow the template again.
test_that("the guided editor cannot narrow a multi-format template on save", {
  app <- file.path(engine_root(), "app.R")
  skip_if_not(file.exists(app))
  src <- paste(readLines(app, warn = FALSE), collapse = "\n")
  expect_true(grepl("\\.datefmt_unchanged <- function", src))
  # both write sites (pdf table + delimited columns) must consult the guard
  writes <- length(gregexpr("!\\.datefmt_unchanged\\(tmpl, datefmt\\)", src)[[1]])
  expect_gte(writes, 2L)
  # and the dropdown must show a single value, never the whole vector
  expect_true(grepl("gv_datefmt <- function\\(tmpl\\) as\\.character\\(gv_datefmt_all\\(tmpl\\)\\)\\[1\\]", src))
})

# ---------------------------------------------------------------------------
# INVARIANT 17, THE OTHER HALF: the STRING LITERALS, not just the names.
#
# test-app-ui.R already refuses a non-ASCII character in an R NAME, because in a
# C locale the parser cannot read the file at all. That guard deliberately let
# VALUES through -- and app.R carried 40 of them, including the pound and euro
# signs in cur_symbol(). They are not decoration: a GBP statement now really does
# reach that switch, and the file is read on an air-gapped Windows box whose
# locale is not ours. A literal glyph also depends on the file staying UTF-8
# through every editor, mail client and zip it passes on the way to that box,
# while an escape is seven ASCII characters meaning the same thing everywhere.
#
# BYTES, not characters, and the whole file including comments: this has to fail
# for the same reason the deployment fails, which is a byte the parser cannot
# read where it stands.
.nonascii_bytes <- function(path) {
  ln <- readLines(path, warn = FALSE)
  bad <- grep("[^ -~\t]", ln, useBytes = TRUE)
  if (!length(bad)) return(character(0))
  sprintf("%s:%d %s", basename(path), bad, substr(ln[bad], 1, 90))
}

test_that("no non-ASCII byte survives in the three UI files, literals included", {
  files <- file.path(engine_root(), c("app.R", "ui_labels.R", "ui_content.R"))
  files <- files[file.exists(files)]
  expect_identical(length(files), 3L)               # the scan must not go quiet
  offenders <- unlist(lapply(files, .nonascii_bytes))
  expect_identical(offenders, character(0),
                   info = paste("non-ASCII byte:", paste(offenders, collapse = " | ")))
  # ...and the glyphs are still THERE, as escapes: a file that passed by losing
  # the tick and the currency symbols would be worse than the one it replaced.
  app <- paste(readLines(file.path(engine_root(), "app.R"), warn = FALSE), collapse = "\n")
  for (esc in c("\\\\u2713", "\\\\u00a3", "\\\\u20ac", "\\\\u00b7", "\\\\u2014"))
    expect_match(app, esc)
  # the currency switch really is one of them -- the literal that made this a live
  # risk rather than a style nit, now that a GBP statement reaches it
  expect_true(grepl('GBP = "\\u00a3", EUR = "\\u20ac"',
                    .app_block(.app_src(), "cur_symbol <- function", 3L), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# NO TWO FILES MAY DEFINE THE SAME FUNCTION NAME.
#
# THE FAILURE IS SILENT AND IT MOVES FIGURES. The app sources every file in R/ and
# then app.R, so when two files define one name the LAST one sourced wins, decided by
# nothing but alphabetical order. Nothing errors. Every call in both files now reaches
# one body -- and if the two took different arguments, or meant different things, the
# loser is answering questions it was never written for.
#
# It happened twice while R/column_fit.R was being written. `.col_kind` and
# `.looks_money` already existed in R/column_profile.R, where they INFER a column kind
# from a sample of data; the new ones read the kind a TEMPLATE DECLARES. Same names,
# opposite directions, and `column_fit.R` sorts after `column_profile.R`, so the new
# pair silently won everywhere -- including inside the drafting wizard, which had been
# working. The rename to `.cfit_*` then missed one call site, so that one resolved to
# the OTHER signature and quietly returned the wrong answer.
#
# A reviewer cannot catch this by reading a diff: both halves are correct on their own
# page, and the collision only exists in the union. Only a scan of the whole tree can.
#
# Top-level definitions only -- a helper nested inside another function is scoped to
# it and shadows nothing, which is why the pattern is anchored to column 1.
.top_level_defs <- function() {
  files <- c(list.files(file.path(engine_root(), "R"), "[.]R$", full.names = TRUE),
             file.path(engine_root(), c("app.R", "ui_labels.R", "ui_content.R", "run.R")))
  files <- files[file.exists(files)]
  out <- list()
  for (f in files) {
    ln <- readLines(f, warn = FALSE)
    m <- grep("^(`[^`]+`|[.A-Za-z][A-Za-z0-9._]*)[ ]*<-[ ]*function", ln, value = TRUE)
    nm <- gsub("`", "", trimws(sub("[ ]*<-[ ]*function.*$", "", m)))
    for (n in nm) out[[n]] <- c(out[[n]], basename(f))
  }
  out
}

test_that("no function name is defined in two files, and none twice in one file", {
  defs <- .top_level_defs()
  expect_gt(length(defs), 400L)            # the scan must not go quiet
  across <- names(defs)[vapply(defs, function(v) length(unique(v)) > 1L, logical(1))]
  expect_identical(across, character(0),
    info = paste("defined in two files, so load order decides which body answers:",
                 paste(vapply(across, function(n)
                   sprintf("%s (%s)", n, paste(unique(defs[[n]]), collapse = " + ")),
                   character(1)), collapse = "; ")))
  twice <- names(defs)[vapply(defs, function(v) length(v) > length(unique(v)), logical(1))]
  expect_identical(twice, character(0),
    info = paste("defined twice in one file, so the first body is dead code:",
                 paste(twice, collapse = ", ")))
})

# ---------------------------------------------------------------------------
# NO STATEMENT IN server() MAY THROW ITS VALUE AWAY.
#
# THE BUG THIS EXISTS FOR CRASHED EVERY SINGLE PAGE LOAD, and it had been doing so
# for at least the length of a long session before anyone saw it. One orphaned line
# sat at the top level of the server body:
#
#     length(rb$tables) > 0L || length(rb$pairs) > 0L
#
# Debris from a deleted function: `rb$tables` and `rb$pairs` appear nowhere else in
# the file. Reading a reactiveValues field outside a reactive consumer is a FATAL
# error, so the server body aborted there -- which also meant `bank_choice` (defined
# further down) never came into existence, and an observer above it then failed too.
# The websocket closed about 300ms after load and Shiny greyed the page out behind
# its disconnected overlay, with the "Working..." pill latched on because the last
# event anyone saw was `shiny:busy` and no `shiny:idle` ever followed.
#
# WHY NOTHING CAUGHT IT. Every app test in this suite reads app.R as TEXT. Booting
# the app and checking for HTTP 200 does not catch it either: the HTTP response is
# the static page, and the server function does not run until a WEBSOCKET session
# opens. `shiny::testServer()` does not catch it either -- it was tried, and
# MockShinySession evaluates the body inside a reactive context, so the error never
# fires. It took driving a real browser at the app to see it.
#
# This guard is the part that can run anywhere, every time: a statement at the top
# level of the server body whose head is an operator can only produce a value, and
# at statement level that value goes nowhere. Harmless code never looks like this;
# orphaned code does. Verified to catch the original line.
.server_body_statements <- function(path) {
  ex <- parse(path, keep.source = TRUE)
  srv <- NULL
  for (e in ex) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        identical(as.character(e[[2]]), "server") &&
        is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "function")) {
      srv <- e[[3]][[3]]; break
    }
  }
  srv
}

test_that("no statement in server() is an expression whose value is discarded", {
  srv <- .server_body_statements(file.path(engine_root(), "app.R"))
  expect_false(is.null(srv), info = "server <- function(...) not found in app.R")
  expect_true(is.call(srv) && identical(as.character(srv[[1]]), "{"))
  stmts <- as.list(srv)[-1]
  expect_gt(length(stmts), 50L)              # the scan must not go quiet
  inert <- c("||", "&&", "|", "&", "==", "!=", "<", ">", "<=", ">=",
             "+", "-", "*", "/", "^", "%%", "%in%", ":")
  dead <- character(0)
  for (s in stmts) {
    if (is.symbol(s)) { dead <- c(dead, paste0("bare symbol `", as.character(s), "`")); next }
    if (is.call(s)) {
      fn <- tryCatch(as.character(s[[1]])[1], error = function(e) "")
      if (fn %in% inert)
        dead <- c(dead, paste(utils::head(deparse(s), 1), collapse = " "))
    }
  }
  expect_identical(dead, character(0),
    info = paste("statement(s) in server() that can only discard their value --",
                 "orphaned code, and a reactive read among them is a FATAL that",
                 "closes the session on every page load:",
                 paste(dead, collapse = "; ")))
})

test_that("the loader really does source all of R/, which is what the guard assumes", {
  # The guard above is only worth having if its file list matches what actually runs.
  # app.R loads EVERY file in R/ in one line; if that ever becomes a hand-written list,
  # a module left off it would be outside the scan and a collision in it would stay
  # silent -- so this fails the moment the assumption stops holding.
  app <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  expect_true(any(grepl('for (.f in list.files("R", full.names = TRUE, pattern = "\\\\.R$")) source(.f)',
                        app, fixed = TRUE)),
    info = "app.R no longer sources all of R/ in one line -- re-check .top_level_defs")
  for (f in c("ui_labels.R", "ui_content.R"))
    expect_true(any(grepl(sprintf('source("%s")', f), app, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# ONE CLICK FROM THE VERDICT, which is what the page promises and what the
# charter promises ("every diagnostic still exists and is still reachable in one
# click"). The checks used to live at the bottom of the panel that "Show me how it
# read this" opens, inside a SECOND collapsible, so on a case folder they were
# three controls deep. They now sit outside that panel.
#
# The dashboards line was here too, held to the same rule. It is GONE now, and
# this test guards its absence instead: whether a conversion reaches the org's
# dashboards is decided by a machine gate the analyst has no part in and cannot
# change, so telling her was noise dressed as information. The one case that
# matters -- a feed write that FAILED -- is a server fault, and it is raised in
# Admin (adm_feed_health) where somebody can act on it.
test_that("the checks are not behind the evidence toggle, and the feed line is gone", {
  src <- .app_src()
  i_detail <- grep('^\\s*uiOutput\\("cv_detail"\\),', src)
  i_toggle <- grep('^\\s*uiOutput\\("cv_more_toggle"\\),', src)
  i_panel  <- grep('conditionalPanel\\("output\\.cv_detail_open == true"', src)
  expect_length(i_detail, 1L)
  expect_length(i_toggle, 1L); expect_length(i_panel, 1L)
  expect_length(grep('uiOutput\\("cv_feed"\\)', src), 0L)   # not on the page at all
  expect_true(i_detail < i_toggle)      # the checks: above the link, one click
  expect_true(i_toggle < i_panel)
  # the toggle's caption no longer claims the checks it does not open
  cap <- .app_block(src, "output\\$cv_more_toggle <- renderUI", 20L)
  expect_false(grepl("The page, the checks, and the template it used.", cap, fixed = TRUE))
  # ...and the panel is not offered at all where there is nothing behind it: on a
  # run with no transactions the X-ray, the charts and the candidates all render
  # nothing, so the link used to open an empty box.
  expect_true(i_toggle > grep('conditionalPanel\\("output\\.cv_has_txns == true",\\s*$', src)[1])
})

# A heading is a promise about what is under it. "See it on the page" sat outside
# the is-this-a-PDF test, so every CSV and Excel conversion printed it over a
# single sentence explaining that there is no page.
test_that("the page heading renders only where there is a page", {
  src <- .app_src()
  i_head <- grep('h4\\("See it on the page"\\)', src)
  i_pdf  <- grep('conditionalPanel\\("output\\.ix_is_pdf == true"', src)
  expect_length(i_head, 1L)
  expect_true(length(i_pdf) >= 1 && i_pdf[1] < i_head)     # inside the PDF branch
  # and the non-PDF branch says what IS true instead of pointing at a picture
  blk <- .app_block(src, 'conditionalPanel\\("output\\.ix_is_pdf != true"', 4L)
  expect_match(blk, "no page picture for a CSV or Excel export")
})

# The case-folder table graded a file the verdict card deliberately leaves
# ungraded: "No template for this statement yet | - | 0 | low", a confidence
# score for a conversion that never happened, in the column a reviewer skims to
# decide which of thirty files to open.
test_that("a file that converted nothing carries no confidence grade anywhere", {
  graded <- .app_fun(".is_graded")
  expect_true(all(graded(c("ok", "needs_review"))))
  expect_false(any(graded(c("unsupported", "failed", "", NA))))
  # the batch table blanks the grade with it, and the card's own rule is the same
  expect_match(.app_block(.app_src(), "output\\$cv_batch <- renderDT", 4L),
               "b\\$trust\\[!\\.is_graded\\(b\\$status\\)\\] <- NA_character_")
  expect_match(.app_block(.app_src(), "output\\$cv_status <- renderUI", 45L),
               'graded <- st %in% c\\("ok", "needs_review"\\)')
})
