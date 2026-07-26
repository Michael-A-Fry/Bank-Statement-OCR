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
.app_block <- function(src, pat, n = 60L) {
  i <- grep(pat, src)
  testthat::expect_true(length(i) >= 1)
  paste(src[i[1]:min(i[1] + n, length(src))], collapse = " ")
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
  save_block <- .app_block(src, 'observeEvent\\(input\\$g_save', 60L)
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
  expect_match(joined, "!\\.identity_is_personal\\(detected_identity_info\\(\\)\\) && is.na\\(cv_qid\\(\\)\\)")
  expect_match(.app_block(src_go <- .app_src(), "observeEvent\\(input\\$cv_go", 25L), "return\\(\\)")
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
test_that("the Convert page discloses that a copy of the upload is kept (#6)", {
  src <- .app_src()
  joined <- paste(src, collapse = "\n")
  expect_match(joined, "UPLOADS_NOTE <- uploads_retention_note\\(UPLOADS_KEEP_DAYS\\)")
  # the line sits with the file picker, not hidden in Admin
  expect_match(.app_block(src, 'fileInput\\("cv_file"', 8L), "UPLOADS_NOTE", fixed = TRUE)
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
