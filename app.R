# app.R -- interactive GUI for the statement conversion engine.
#
# Two jobs, both point-and-click for a non-engineer analyst:
#   1. Convert -- upload a statement, convert it, review the checks, download.
#   2. Add a template -- upload a sample and open the template toolkit: the tool
#      pre-fills what it can detect, you confirm against a live preview and SAVE
#      a new bank template (it writes the YAML for you).
#
# Run locally:  R -e 'shiny::runApp(".", launch.browser = TRUE)'
# (from the repo root, so R/ and templates/ resolve.)

# Force a UTF-8 locale FIRST. On a host whose default locale is C/ASCII
# (ANSI_X3.4-1968), R cannot represent the unicode symbols used throughout the
# UI and renders them as mojibake ("<80><94>"), which makes the whole app look
# broken. Try the common UTF-8 locale names and stop at the first that takes.
suppressWarnings(for (.loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"))
  if (nzchar(Sys.setlocale("LC_CTYPE", .loc))) break)

suppressMessages({
  library(shiny)
  library(DT)
})

# AN EMPTY DATE BOX, DECODED WITHOUT SHOUTING ABOUT IT.
#
# The only dateInputs in this app are the two template-validity pickers
# (.eff_picker), and both are deliberately EMPTY when a template has no validity
# window -- which is the usual case. An empty bootstrap-datepicker holds an
# Invalid Date, shiny's DateInputBinding formats that as the literal string
# "NaN-NaN-NaN" and posts it, and shiny's own `shiny.date` handler then calls
# as.Date() on it, catches the error and re-raises it as a warning. Measured in a
# browser: two warnings on the console every time the toolkit is opened, for a
# value the app handles correctly (it becomes NA, and .eff_date takes it from
# there). A console that cries wolf twice per open is a console nobody reads the
# real warnings in.
#
# Same contract as shiny's own handler -- a Date vector, NA where the browser sent
# nothing usable -- with the sentinel recognised instead of thrown at as.Date.
# Per element rather than all-or-nothing, so one empty box in a pair can no longer
# blank the other. Registered once, at load: an input handler is global, and the
# gate this closes is a decoding rule, not a session's business.
.decode_shiny_date <- function(val, ...) {
  v <- vapply(val, function(x) if (is.null(x)) NA_character_ else as.character(x)[1],
              character(1), USE.NAMES = FALSE)
  # shiny's JS sends ISO dates and nothing else; anything that is not one is an
  # empty picker, not a date this app should try to guess at.
  v[!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v)] <- NA_character_
  as.Date(v)
}
shiny::registerInputHandler("shiny.date", force = TRUE, .decode_shiny_date)

# Load the engine (all pure-R modules) into the session.
for (.f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(.f)

# EVERY TEMPLATE NOW LIVES UNDER templates\. A deployment updated from an older
# version still has the old folders at the root, holding work that exists nowhere
# else, so move them once -- here, before anything reads a template, and not by
# asking a person to do it on an air-gapped box. It moves files, never deletes
# them, never overwrites, and with nothing to move it says nothing.
#
# Here as well as in scripts/run_app.R because the server is not the only way in:
# a maintainer running the app from an R console gets the same migration, and
# running it twice is a no-op. R/config.R holds the rules.
for (.m in tryCatch(migrate_template_layout("."), error = function(e) character(0)))
  message("Statement Studio: ", .m)

# All deployment settings live in ONE place: config/config.yaml (copy it from
# config/config.example.yaml). Any absent key falls back to the built-in default,
# so with no config file the app behaves exactly as before.
CONFIG <- load_config()
# A config.yaml that does not parse used to revert to built-in defaults SILENTLY --
# including the admin password (back to the shipped placeholder) and the Qlik
# feed_dir. The docs tell a non-technical analyst to edit that file in Notepad, so
# one stray tab could quietly weaken the deployment. Say it loudly at startup, and
# again on a banner in Admin (output$adm_cfg_banner) for whoever is looking at the
# screen rather than the console.
CONFIG_ERROR <- config_error(CONFIG)
if (!is.null(CONFIG_ERROR)) {
  warning(sprintf(paste("SETTINGS FILE NOT LOADED: %s\n",
                        "Statement Studio has started on its BUILT-IN DEFAULTS, which include the",
                        "placeholder admin password and the default Qlik feed folder. Fix the file",
                        "and restart."), CONFIG_ERROR), call. = FALSE, immediate. = TRUE)
}
# Admin stays CLOSED while the password is still the shipped placeholder (or blank).
# Read once at startup: the gate must not depend on a per-session value.
ADMIN_PW_UNSET <- isTRUE(admin_password_is_default(CONFIG))
if (ADMIN_PW_UNSET)
  message("Statement Studio: Admin is CLOSED - no admin password is set. ",
          "Set app.admin_password in config/config.yaml or the BSO_ADMIN_PASSWORD ",
          "environment variable, then restart.")
# Shiny's built-in upload ceiling is 5 MB, which REJECTS the input this tool exists
# for: a 300-dpi scan of a year's statement is routinely 10-40 MB, and even the
# sample scan shipped with the repo is 4.4 MB. The file picker refuses it with a bare
# red strip and nothing reaches the engine, so there is no log line either. Raise it
# here (and in scripts/run_app.R, which is how the server actually starts) from one
# config key so a site can tune it without touching code.
MAX_UPLOAD_MB <- suppressWarnings(as.numeric(CONFIG$app$max_upload_mb %||% 200))
# Do templates built HERE take part in detection? A deployment decision, read once
# -- never a tick-box on the Convert page. Whether a colleague's template counts
# is not a question to put to the person converting a statement, and the tick-box
# only ever produced the puzzle "I built this template and it does not work".
# Governance is unaffected: what reaches the dashboards is gated separately, on
# template origin (feed.allowed_template_origins).
USE_USER_TEMPLATES <- isTRUE(CONFIG$app$user_templates_default %||% TRUE)
# WHEN THE EDITOR OPENS BY ITSELF. A deployment setting with a stated default,
# not a number buried in the code, and the SAME three words feed.min_trust uses so
# nobody has to learn a second vocabulary:
#   high   - a statement whose confidence is medium or low opens the toolkit
#   medium - only a `low` statement opens it (the default)
#   any    - a statement never opens it by itself
# A report or a form has no confidence to measure - nothing about either is
# proven, because neither reconciles - so the editor always opens on those, and
# this setting cannot switch that off. The door is on screen either way.
EDITOR_MIN_TRUST <- tolower(as.character(
  CONFIG$convert$open_editor_below_trust %||% "medium")[1])
if (!(EDITOR_MIN_TRUST %in% c("high", "medium", "any"))) EDITOR_MIN_TRUST <- "medium"
if (!is.finite(MAX_UPLOAD_MB) || MAX_UPLOAD_MB <= 0) MAX_UPLOAD_MB <- 200
options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)
# HOW MANY CONVERSIONS MAY RUN AT ONCE. Every conversion now runs in its own
# short-lived R process (R/jobs.R): R is single-threaded and this is ONE process
# for the whole team, so an engine call made here froze every other analyst's
# browser for as long as it took - measured at 65 seconds of a dead page while
# one person converted one scanned statement. Ten analysts must not be able to
# answer that with ten OCR processes on a four-core box, so the cap leaves a core
# for Shiny itself (the process every browser is talking to) and the queue behind
# it says out loud where you are in it. Raise it on a bigger server; lowering it
# to 1 turns the tool back into one-at-a-time, but a NAMED one-at-a-time.
job_set_max_concurrent(CONFIG$app$max_concurrent_jobs)
# How often a waiting page asks its conversion how it is getting on. Half a
# second: quick enough that a 2.9s CSV does not feel delayed, cheap enough that
# ten waiting browsers cost a handful of file checks a second between them.
JOB_POLL_MS <- 500
# Nothing may outlive the app. A child still OCR'ing when the server stops would
# go on burning a core for two minutes on a result nobody can collect.
onStop(function() safe(job_reap_all()))
TEMPLATES_DIR      <- CONFIG$paths$templates       # curated, team-maintained (proven) templates
USER_TEMPLATES_DIR <- CONFIG$paths$user_templates  # templates accountants create via guided setup
LOGDIR             <- CONFIG$paths$logs            # run log + feedback log live together, next to the app
UPLOADS_DIR        <- CONFIG$paths$uploads         # every uploaded statement + its lifecycle status (local-only)
REQUESTS_DIR       <- CONFIG$paths$requests        # "none of these fits -- tell our team" raises (local-only)
FIELDS_DIR         <- CONFIG$paths$fields          # curated mode:fields (IRD/form) templates
USER_FIELDS_DIR    <- CONFIG$paths$user_fields     # form templates built in the app
# mode:document templates -- a report carrying many tables of different shapes.
# %||% so a settings file written before this existed still starts: an absent path
# is a missing folder, which both loaders treat as "no templates", not an error.
DOC_DIR            <- CONFIG$paths$docs %||% file.path("templates", "documents")
USER_DOC_DIR       <- CONFIG$paths$user_docs %||% file.path("templates", "documents_user")
DICT_PATH          <- CONFIG$paths$dictionary      # the shared label dictionary
LEXICON_PATH       <- CONFIG$paths$lexicon %||% file.path("dictionaries", "lexicon.yaml")  # recognition vocabularies
# The bundled specimen statement (public, synthetic, ships with the app) that "Try
# it on a sample" converts, so a brand-new user sees a full result without a file.
#
# IT HAS TO BE A FILE A SHIPPED TEMPLATE ACTUALLY READS. This pointed at
# samples/raw/tutorial/sample_everyday_statement.pdf, whose template carries
# `sample: true` -- and load_template_set() deliberately drops those from the
# detection set (R/templates.R), so the one button offered to somebody with no
# statement to hand answered "No template for this statement yet" every single
# time. Forcing the id does not help either: convert_document looks the forced id
# up in that same filtered set. So the specimen is one the shipped templates
# really do read; verified end to end (anz_everyday_csv, status ok, 7 rows).
SAMPLE_STATEMENT <- file.path("samples", "raw", "anz", "anz_transaction_export_01.csv")

# How many days of run/feedback logs to keep before "Tidy up logs" archives them.
# One place, so the button label and both rollup calls can never disagree.
LOG_KEEP_DAYS <- 90L
# How long a COPY of an uploaded statement is kept under uploads/<id>/. Config-driven
# because it is a data-retention decision, not a code decision; the same number
# drives the purge, the Admin button label and the line shown under the file picker,
# so what the user is told and what happens can never disagree.
UPLOADS_KEEP_DAYS <- suppressWarnings(as.numeric(CONFIG$retention$uploads_keep_days %||% 90))
if (!is.finite(UPLOADS_KEEP_DAYS)) UPLOADS_KEEP_DAYS <- 90
UPLOADS_NOTE <- uploads_retention_note(UPLOADS_KEEP_DAYS)
# Startup tidy-up, once per process. Two things nothing else reclaimed:
#   * old copies of client statements under uploads/ (kept forever until now);
#   * per-session scratch folders under the temp dir. R only clears the temp dir
#     when the process exits, and this app is a service that runs for months, so
#     every conversion's outputs AND its copy of the source piled up for the life
#     of the server. Each session now unlinks its own (see run_conversion and
#     onSessionEnded); this sweep is the backstop for a browser that closed
#     abruptly, and it is re-run on each conversion (a fresh process's temp dir is
#     empty, so at startup it usually finds nothing -- that is fine, it is cheap).
#
# THE PURGE DOES NOT RUN ON A SETTINGS FILE THAT DID NOT LOAD, and this is the
# reason. UPLOADS_KEEP_DAYS comes out of config/config.yaml, and when that file
# cannot be parsed CONFIG is the BUILT-IN DEFAULTS -- so the number driving an
# irreversible delete is one this site never chose. REPRODUCED: a deployment that
# had set `uploads_keep_days: 0` (keep indefinitely -- the evidence-retention
# choice, and the example config offers it in those words) took one stray tab in
# config.yaml, which is exactly what the docs warn about because they tell a
# non-technical analyst to edit that file in Notepad. On the next restart the
# default 90 applied and every stored client statement older than 90 days was
# deleted, record.json stamped `purged`, before anybody had read the warning three
# lines above. Deleting evidence on a number nobody set is not a tidy-up.
# So: keep everything, say why, and leave it to the Admin button, which states the
# number and asks -- once the settings file is readable and the number is the
# site's own again.
if (is.null(CONFIG_ERROR)) {
  safe(purge_uploads(UPLOADS_DIR, keep_days = UPLOADS_KEEP_DAYS))
} else {
  warning(paste("Saved statements were NOT tidied up at startup: the settings file",
                "did not load, so the retention period is a built-in default and not",
                "this deployment's. Nothing has been deleted. Fix the settings file",
                "and restart."), call. = FALSE, immediate. = TRUE)
}
safe(sweep_temp_dirs(keep_hours = 24))
# read_file_text(p) -- a file's contents as one string ("" if absent). Used by the
# Admin YAML editors to load the dictionary / lexicon into their text boxes.
read_file_text <- function(p) if (file.exists(p)) paste(readLines(p, warn = FALSE), collapse = "\n") else ""

# Plain-English label maps + the preview-column helpers live in ui_labels.R (the
# wording a non-technical user sees). Sourced here -- after the engine, before the
# UI -- so rewording copy is one small, obvious file, never buried in app.R.
source("ui_labels.R")

# About-page + tutorial HTML content lives in ui_content.R (readability).
source("ui_content.R")

# The whole design system is www/app.css, served off disk by Shiny. If the folder
# did not travel with the install, every screen renders as bare Bootstrap: still
# usable, but it does not look like the tool anyone was shown, and a user's first
# thought is that something is broken with their data. Say it once, loudly, to
# whoever started the server -- the same rule as a settings file that would not
# parse. NB: whatever copies this app to the server (scripts/bundle-offline.R,
# or a hand copy) has to include www/.
# THE SCREEN'S COLOURS, ONCE. The stylesheet's :root block is the design system,
# but the X-ray draws its layers with base-R graphics (rect/text/border), which
# cannot read a CSS variable -- so those four values had been typed as raw hex in
# app.R, drifting into NEAR-MISSES of the tokens they meant: b00020 against
# --bad:#b3261e, 137333 against --ok:#0f7a37, c77700 against --warn:#b7791f.
# Nothing looked wrong, which is exactly why it would never have self-corrected.
# One list here, the same values in app.css, and test-app-ui.R fails if the two
# ever disagree -- so a colour can still only be changed in one place, even though
# two languages have to read it.
PALETTE <- list(ok = "#0f7a37", bad = "#b3261e", warn = "#b7791f", meta = "#a15c00")
# Base-R graphics take 8-digit hex for a translucent fill; CSS names the same
# thing with its own alpha. Kept as a helper so a fill is visibly the SAME colour
# as its outline rather than a second literal that has to be remembered.
pal_fill <- function(name, alpha) paste0(PALETTE[[name]], alpha)

# .is_txn_result(res) -- is this a TRANSACTION statement, as opposed to one of the
# two document kinds that carry no transactions at all (a form of labelled values,
# or a report of many tables)?
#
# THE WHOLE CONVERT PAGE HANGS OFF THIS ONE QUESTION. Money-in/money-out cards, the
# balance proof, the transactions table, the trust headline, "not the right bank?"
# -- every one of them reads a transaction frame that a form or a report simply
# does not have. Each of those outputs used to ask `!identical(res$kind, "form")`
# for itself, which was correct while "form" was the only exception; adding a
# second one that way would mean six places that each have to remember the list.
# Asking it in one place is the only version of this that stays right.
.is_txn_result <- function(res) !isTRUE((res$kind %||% "") %in% c("form", "tables"))

# .needs_editor(res, min_trust) -- should the editor open BY ITSELF on this
# result?
#
# "For Other statements the likelihood that something will need to change on
# every convert - it shouldn't just auto process, it should process and open up
# the editor. For statements I want a threshold where it does and where it
# doesn't, but ensure there is an option even if it's confident."
#
# ONE LINE COVERS BOTH ROUTES, and it is honest about why. A report or a form
# carries NO trust at all -- there is no reconciliation behind either, so nothing
# about the figures is proven -- and .trust_ok() coalesces an absent level to the
# lowest, so the OTHER half falls out of the same expression for free: it always
# opens. The !.is_txn_result clause stays EXPLICIT so that setting the threshold
# to "any" cannot switch the other route off; the threshold was asked for on
# statements only.
#
# The status clause is load-bearing. An `unsupported` statement is trust `low`
# and must NOT auto-open the toolkit: the card on that result is still asking
# "is this a statement or something else?", and opening the statement toolkit
# over it would answer, for her, the one question the card exists to put.
.needs_editor <- function(res, min_trust) {
  isTRUE((res$status %||% "") %in% c("ok", "needs_review")) &&
    (!.is_txn_result(res) || !.trust_ok(res$trust$level, min_trust))
}

# .blocking_diag(res) -- the diagnosis that OUTRANKS "no template for this
# layout", or NULL.
#
# Driven with no OCR software and an image-only PDF: the card said "No template
# for this layout yet", the green button said "Set it up as a report", and the
# diagnostics table further down said correctly that this machine has no OCR
# software installed and that building a template will NOT help until that is
# done. Three answers on one screen, and the biggest button was the wrong one.
#
# The engine already grades this: `severity` high with `fix_owner` naming who can
# actually fix it. "input" is the file itself (rescan it, re-export it, split it)
# and "escalate" is an engine or install gap -- neither is mended by drawing boxes
# on a page, so neither may be offered a template button as its primary action.
# "template" IS mended that way and is deliberately not in the list.
.blocking_diag <- function(res) {
  d <- res$diagnostics
  if (!is.data.frame(d) || !nrow(d)) return(NULL)
  if (!all(c("category", "severity", "detail", "how_to_fix", "fix_owner") %in% names(d)))
    return(NULL)
  hit <- d$severity %in% "high" & d$fix_owner %in% c("input", "escalate") &
    !(d$category %in% "none")
  if (!any(hit)) return(NULL)
  d[which(hit)[1], , drop = FALSE]
}

# .scan_note(ocr_pages, low_conf) -- ONE SENTENCE saying this came off a scan,
# for the line beside the download.
#
# "A badly OCR'd page can come back ok." The per-word confidence is carried the
# whole way through the reader and every doubtful figure already earns a row
# flag, but the caveat lived in a diagnostics panel behind "Show me how it read
# this" -- a panel most people never open -- while the verdict above it was
# green. For a unit where a large share of what arrives is a scan of a
# photocopy, that is the wrong way round: it belongs above the fold, next to the
# download, where the person is looking.
#
# NULL when nothing was machine-read, so an ordinary text PDF and every CSV are
# untouched.
.scan_note <- function(ocr_pages, low_conf) {
  p <- suppressWarnings(as.integer(ocr_pages %||% NA_integer_)[1])
  if (is.na(p) || p < 1L) return(NULL)
  n <- suppressWarnings(as.integer(low_conf %||% NA_integer_)[1])
  if (is.na(n)) n <- 0L
  if (n > 0L)
    sprintf("This was read from a scan, and %d figure%s came out too faint to be sure of - check %s against the page before you rely on %s.",
            n, if (n == 1L) "" else "s", if (n == 1L) "it" else "them",
            if (n == 1L) "it" else "them")
  else
    "This was read from a scan, so check the figures against the page before you rely on them."
}

# ---- ADMIN: THE ROUTE LABEL, AND THE TWO TABLES SPLIT BY IT ----------------
#
# "All templates are accessible, but broken down by statement and other... All
# templates should also be viewable in admin, but split by statement and other."
# "All feedback accessible in admin in same area, but ensure it can be traced to
# the statement and template."
#
# Both faults are one missing thing: Admin was organised by DATA SOURCE (the
# template library on one tab, the feedback log on another) and no single ROUTE
# label existed anywhere in the product. So the library was one un-partitioned
# list and the feedback log was one un-joined frame, and neither said which half
# of the app it belonged to.
#
# .adm_route(kind) is that label, derived once and used by both tables, so the
# two read as one screen split the same way.
# The third value exists so a rating whose route genuinely cannot be established
# is never folded into one it might not belong to. It is worded so that it also
# SORTS after the other two: the table is grouped by this column and DataTables
# orders it alphabetically, and "Not recorded" would have put the odd band
# between the two bands the split is actually about.
.ADM_ROUTES <- c("Bank statement", "Other", "Route not recorded")
.adm_route <- function(kind) {
  k <- as.character(kind %||% "")[1]
  if (identical(k, "statement")) return(.ADM_ROUTES[1])
  if (k %in% c("fields", "document", "form", "tables")) return(.ADM_ROUTES[2])
  .ADM_ROUTES[3]
}

# .adm_feedback_overview(feedback, runs, templates) -- ONE ROW PER RATING, with
# the document it was left on and the template that read it.
#
# A rating carries a run_id and a template_id and nothing else; the document name
# lives only in the run log, and which ROUTE the rating is about lives only on the
# template. Neither join was ever made, so "traced to the statement and template"
# was two ids on a screen in a different tab from the library.
#
# Three deliberate refusals, all of them the honest answer rather than a guess:
#   * the route comes from the TEMPLATE's kind; if that template has since been
#     deleted it falls back to the RUN's own kind; if neither is on the box it is
#     "Not recorded" and gets its own band -- never folded into a route it might
#     not belong to.
#   * the document is the run's own source_file, printed as "not recorded" when
#     the run record has been archived away, which is the rule the gaps table
#     already applies.
#   * `asked_kind` (what the person told Convert the file was) is deliberately not
#     used: it is what she said, not what read it.
#
# Pure, so it can be lifted out of app.R and called in a test. It belongs beside
# template_drift() in R/analytics.R and should move there whole.
.adm_feedback_overview <- function(feedback, runs = NULL, templates = list()) {
  cols <- c("route", "when", "document", "template", "verdict", "comment",
            "who", "template_id", "run_id")
  empty <- stats::setNames(
    data.frame(matrix(character(0), 0, length(cols)), stringsAsFactors = FALSE), cols)
  if (is.null(feedback) || !is.data.frame(feedback) || !nrow(feedback)) return(empty)
  col <- function(df, name) {
    if (is.null(df) || !is.data.frame(df) || !(name %in% names(df)))
      return(rep(NA_character_, if (is.data.frame(df)) nrow(df) else 0L))
    as.character(df[[name]])
  }
  said <- function(v) { v <- as.character(v); v[is.na(v) | !nzchar(trimws(v))] <- "not recorded"; v }
  fb_run <- col(feedback, "run_id"); fb_tpl <- col(feedback, "template_id")
  r_id <- col(runs, "run_id")
  i <- match(fb_run, r_id)
  r_file <- col(runs, "source_file")[i]
  r_kind <- col(runs, "kind")[i]
  # The template as it is NAMED on screen, never a bare id -- the id is kept as
  # its own column so a maintainer can still act on it.
  tk <- vapply(fb_tpl, function(id) {
    t <- if (!is.na(id) && nzchar(id)) templates[[id]] else NULL
    if (is.null(t)) NA_character_ else safe(template_kind(t), NA_character_)
  }, character(1), USE.NAMES = FALSE)
  tn <- vapply(fb_tpl, function(id) {
    t <- if (!is.na(id) && nzchar(id)) templates[[id]] else NULL
    if (is.null(t)) NA_character_ else as.character(safe(template_library_name(t), NA_character_))[1]
  }, character(1), USE.NAMES = FALSE)
  route <- vapply(seq_along(fb_run), function(j)
    .adm_route(if (!is.na(tk[j])) tk[j] else r_kind[j]), character(1))
  out <- data.frame(
    route    = route,
    when     = said(safe(local_time_text(col(feedback, "ts")), col(feedback, "ts"))),
    document = said(basename(ifelse(is.na(r_file), "", r_file))),
    template = ifelse(is.na(tn) | !nzchar(tn), said(fb_tpl), tn),
    verdict  = said(col(feedback, "verdict")),
    comment  = said(col(feedback, "comment")),
    who      = said(col(feedback, "requested_by")),
    template_id = said(fb_tpl),
    run_id      = said(fb_run),
    stringsAsFactors = FALSE)
  # Newest first, on the record's own stamp rather than the words shown.
  o <- order(col(feedback, "ts"), decreasing = TRUE)
  out <- out[o, , drop = FALSE]
  # Statement band, then Other, then anything whose route is genuinely unknown.
  out <- out[order(match(out$route, .ADM_ROUTES)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# .ADM_HISTORY_MAX / .adm_history(logdir, subdir, budget) -- the newest slice of
# a log folder, and how much of it was left behind.
#
# Admin used to parse EVERY line of every archive file, one record at a time,
# automatically, the moment the tab was opened. Measured: 20,000 archived rows
# took 10.5 seconds -- thirteen months at fifty conversions a day -- and it
# happens in the one Shiny process the whole team shares, so every other
# analyst's browser is frozen for it. Worse, it gets slower every week, so the
# tab a maintainer opens during an incident is the slowest thing on the box.
#
# The picture Admin draws does not need every record ever written; it needs the
# recent ones. So this reads the LIVE folder first (what has not been archived
# yet, and therefore the newest) and tops up from the yearly archive, newest year
# first, up to a budget -- and it counts what it did not read so the screen can
# say so. Nothing is deleted or hidden: the archive is intact on disk, and
# `Tidy up logs` is still what moves records into it.
.ADM_HISTORY_MAX <- 2500L
.adm_history <- function(logdir, subdir, budget = .ADM_HISTORY_MAX) {
  live_dir <- file.path(logdir, subdir)
  files <- if (dir.exists(live_dir))
    list.files(live_dir, pattern = "\\.json$", full.names = TRUE) else character(0)
  if (length(files) > 1L)
    files <- files[order(file.mtime(files), decreasing = TRUE)]
  arch <- sort(Sys.glob(file.path(logdir, "archive", paste0(subdir, "-*.jsonl"))),
               decreasing = TRUE)
  lines <- unlist(lapply(arch, function(a) rev(safe(safe_readlines(a), character(0)))),
                  use.names = FALSE)
  lines <- lines[nzchar(trimws(lines %||% character(0)))]
  total <- length(files) + length(lines)
  take_f <- utils::head(files, budget)
  take_l <- utils::head(lines, max(0L, budget - length(take_f)))
  recs <- c(lapply(take_f, function(f)
              safe(jsonlite::fromJSON(paste(safe_readlines(f), collapse = "\n")), NULL)),
            lapply(take_l, function(l) safe(jsonlite::fromJSON(l), NULL)))
  recs <- Filter(Negate(is.null), recs)
  out <- if (length(recs)) .rows_bind(recs) else data.frame()
  attr(out, "kept_of") <- c(read = nrow(out), total = total)
  out
}

APP_CSS <- file.path("www", "app.css")
if (!file.exists(APP_CSS))
  warning(paste("STYLESHEET NOT FOUND: www/app.css is missing, so Statement Studio will",
                "render unstyled. Copy the www/ folder next to app.R and restart."),
          call. = FALSE, immediate. = TRUE)

# ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$title("Statement Studio"),
    # THE DESIGN SYSTEM lives in www/app.css, which Shiny serves from disk. It is
    # still entirely ours and entirely local -- no CDN font, script or icon pack --
    # so air-gapping is untouched; it is simply not 265 lines of CSS wedged into the
    # R that builds the screen. The ?v= is the engine version, so an upgraded
    # install cannot serve a browser its cached copy of the old stylesheet.
    tags$link(rel = "stylesheet", type = "text/css",
              href = sprintf("app.css?v=%s", engine_version())),
    # A DATE BOX MUST NOT THROW ON ITS WAY IN. Shiny bundles bootstrap-datepicker
    # (the validity window on the template toolkit is the app's only date box) and
    # then renames the plugin to bsDatepicker via its own noConflict. The library's
    # OWN document-ready hook still calls $(...).datepicker(), which by then no
    # longer exists, so the first date box on a page throws a TypeError into the
    # console. noConflict puts back whatever $.fn.datepicker was BEFORE the library
    # loaded, which on Bootstrap 3 is nothing -- so give it something: a shim that
    # forwards to bsDatepicker. The hook's selector ([data-provide=datepicker-inline])
    # matches nothing Shiny renders, so this makes it the no-op it was always meant
    # to be. Left alone it is an uncaught exception in the middle of a modal full of
    # dynamically inserted inputs, which is the one place this app has already been
    # bitten by an exception aborting a bind pass.
    tags$script(HTML(
      "$.fn.datepicker = $.fn.datepicker || function(){
         return $.fn.bsDatepicker ? $.fn.bsDatepicker.apply(this, arguments) : this; };")),
    # Enter in the Admin password box = click Enter (no mouse trip). The
    # trigger('change') first flushes the debounced text value, so a fast
    # type-then-Enter never submits a stale password.
    tags$script(HTML(
      "$(document).on('keyup', '#adm_pw', function(e){
         if (e.key === 'Enter') { $(this).trigger('change'); $('#adm_login').click(); }
       });")),
    # Loading feedback: a real animation, not just the grey-out. A busy pill shows
    # whenever Shiny is working (convert, X-ray render, any recompute); recalculating
    # outputs dim and float a spinner so a slow plot/table clearly says "loading".
    # The pill, the dim and the centred CONVERTING overlay are styled in app.css
    # (part 2); this is only what turns them on.
    tags$script(HTML(
      "(function(){var t=null;
        function pill(){var p=document.getElementById('ss-busy');
          if(!p){p=document.createElement('div');p.id='ss-busy';
            p.innerHTML='<span class=\"ss-ring\"></span><span>Working\u2026</span>';
            document.body.appendChild(p);}return p;}
        $(document).on('shiny:busy',function(){clearTimeout(t);
          t=setTimeout(function(){pill().classList.add('on');},250);});
        $(document).on('shiny:idle',function(){clearTimeout(t);
          var p=document.getElementById('ss-busy');if(p)p.classList.remove('on');});
        // N32: a PROGRESS notification carries .progress-message; an ordinary
        // toast never does. So the centred overlay follows a progress panel only
        // -- withProgress, or the same panel held open across polls while a
        // conversion runs in its own process -- and warnings stay in the corner.
        // The rule is the DOM class, not which R call raised it, which is why
        // moving the conversion off this process left the overlay untouched.
        // MutationObserver rather than CSS :has(),
        // because the deployment browser may be older than :has() support.
        // $(function(){}) because this script runs BEFORE <body> exists: observing
        // document.body at head time throws a TypeError (it is null), the observer
        // never attaches, and the overlay silently never appears -- which is
        // exactly what happened, and what a prototype injected AFTER page load
        // cannot catch.
        $(function(){
          function ssRun(){var p=document.getElementById('shiny-notification-panel');
            document.body.classList.toggle('ss-run',
              !!(p && p.querySelector('.progress-message')));}
          new MutationObserver(ssRun).observe(document.body,{childList:true,subtree:true});
          ssRun();
        });
      })();")),
  ),
  div(class = "app-header",
    span(class = "app-mark"),
    span(class = "app-title", "Statement Studio"),
    span(class = "app-tagline", "Statements and documents in \u2014 clean, checked data out.")),
  tabsetPanel(
    id = "main_tabs", selected = "Convert",
    # ---- About: the "what is this and why can I rely on it" page - one promise,
    # then the two doors (convert / teach) and a quiet third, then the proof story.
    # NB the app OPENS on Convert (selected, below); this is the page you come back
    # to, not the one you land on.
    tabPanel("About", br(),
      div(class = "hub",
        div(class = "hub-lead",
          "Bank statements and financial documents \u2014 PDF, CSV or Excel \u2014 into clean,",
          " checked data. Every figure comes straight off your statement; anything",
          " that can't be verified is flagged with the reason."),
        div(class = "hub-cards",
          actionLink("ab_go_convert", class = "hub-card hub-card-primary", label = div(
            div(class = "hub-card-kicker", "Most days"),
            div(class = "hub-card-title", "Convert a statement"),
            div(class = "hub-card-body",
                "Upload, click Convert. The verdict, the analysis, every transaction, the download."),
            div(class = "hub-card-go", "Open Convert \u2192"))),
          actionLink("ab_go_template", class = "hub-card", label = div(
            div(class = "hub-card-kicker", "New bank or document"),
            div(class = "hub-card-title", "Teach it a new layout"),
            # No "about 2 minutes, no code". On a real bank PDF the drafter can
            # come back with 0 rows and no path to a working template at all, so
            # the number was a promise the toolkit could not keep.
            # The "pre-fills what it can, you confirm against a live preview" half
            # is the lead sentence of the page this card opens. Said once, there.
            div(class = "hub-card-body",
                "A new statement layout, or any other document - a form, a summary, a letter."),
            div(class = "hub-card-go", "Open Add a template \u2192"))),
          )),
      # NOTE: no Admin card here, and no Admin anywhere else a user can see.
      # Nobody who uses this app has the Admin password; advertising it is an
      # invitation to a locked door. Maintainer tasks are reached from the Admin
      # tab by the person who looks after the tool, and are never referred to in
      # the wording on Convert, Add a template or About.
      about_html()),
    # ---- Convert -------------------------------------------------------
    tabPanel(
      "Convert",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          # ONE PICKER, ONE FILE OR A WHOLE CASE. A folder of statements is not a
          # different MODE with its own tab, its own Convert button and its own
          # second answer to "who ran this" -- it is the same question asked of
          # more files. So it is the same control: pick one statement and you get
          # the result page; pick twelve and you get a row per file, each of which
          # OPENS that same result page. Nothing new to learn, nothing asked twice.
          fileInput("cv_file", "File(s) to convert (.pdf / .csv / .tsv / .xlsx)",
                    multiple = TRUE,
                    accept = c(".pdf", ".csv", ".tsv", ".tdv", ".xlsx")),
          helpText(class = "muted", "One document, or several for a whole case folder."),
          # ---------------------------------------------------------------
          # STATEMENT, OR NOT. Asked once, in front, and it decides everything
          # after it.
          #
          # Reported twice. "When I put a non-statement PDF in here and click
          # convert, it defaults to just the statement template wizard", and
          # "there NEEDS to be a decision at every point that needs STATEMENT OR
          # OTHER - this could be as simple as a toggle on the convert screen".
          # And the fault that made it urgent: "I put a phrase printed on it,
          # bang smack on front page, still used another template" -- a bank
          # template matched a document that was not a statement, and the report
          # pass is only reached when the statement pass fails, so the template
          # carrying that phrase was never consulted at all.
          #
          # WORK IT OUT FOR ME STAYS THE DEFAULT, because it is right most of the
          # time and a question asked on every single conversion is a tax on the
          # common case. But the override is in FRONT, one click, and named in
          # the same two words the rest of the app uses.
          radioButtons("cv_kind", "What is this?",
            c("Work it out for me" = "auto",
              "A bank or card statement" = "statement",
              "Something else - a report, a form, a letter" = "other"),
            selected = "auto"),
          # WHO RAN THIS. Asked only when the tool genuinely cannot work it out.
          # Where the server sits behind a real sign-in (host or SSO) this renders
          # nothing at all -- asking for what the environment already established
          # is the question a tool should answer for itself. Where there is no
          # sign-in, it is asked ONCE per session and then collapses to one line,
          # because the alternative is an audit trail that names the server's own
          # account for the whole department.
          uiOutput("cv_whoami"),
          # THE BANK, IN FRONT, AND ONLY HERE. It sat inside "It picked the wrong
          # bank?" on the assumption detection usually gets it right. On real
          # statements it does not yet, which makes the override one of the
          # most-used controls on the page - so hiding it put a click on a common
          # path. Auto-detect stays the default, so nobody has to answer it; it is
          # simply visible when they do.
          #
          # There used to be a SECOND bank picker inside the disclosure below, with
          # its own state and its own spelling of "no bank". bank_choice() read the
          # front one first, so choosing a bank in the disclosure while this one
          # named a different bank was silently discarded - the tool converted with
          # the bank the user had stopped asking for and said nothing. Verified in
          # the browser before it was removed: front=ANZ + disclosure=ASB left the
          # template list showing ANZ's templates only. One control, one answer.
          #
          # ...AND IT GOES AWAY WHEN SHE HAS SAID THIS IS NOT A STATEMENT. A bank
          # picker on a trustee report is a control that cannot do anything, sat
          # in front of somebody who has just told the tool this is not a bank
          # document. Reported as "the converted page picked the wrong bank (not
          # a bank)": every screen that names a bank has to stop naming one when
          # the answer is "something else".
          conditionalPanel("input.cv_kind != 'other'",
            selectInput("cv_bank_quick", "Bank",
                        choices = c("Detect automatically" = ""), width = "100%")),
          # ONE SENTENCE. The second half ("it is read by a form or report
          # template, or you set one up") named two kinds of template nobody
          # outside Admin distinguishes, and said what the empty state four
          # inches to the right already says in her own words.
          conditionalPanel("input.cv_kind == 'other'",
            div(class = "note", style = "margin:0 0 14px;font-size:12.5px",
              strong("No bank templates will be tried."))),
          # OFF UNTIL IT CAN WORK, with the reason under it. It was a full-width
          # green button from the moment the page loaded, and pressing it with no
          # QID typed produced a message that fades. So the most prominent
          # control on the screen did nothing, twice, and by the third press the
          # person is looking for what is broken rather than for the empty box
          # eight inches above it.
          uiOutput("cv_go_btn"),
          helpText(sprintf("Up to %g MB.", MAX_UPLOAD_MB)),
          # Everything most people never need is one obvious click away, so the
          # default view is simply: file, name, Convert.
          #
          # AND IT IS NOT A BANK-ONLY PANEL ANY MORE. It was hidden the moment
          # somebody said "something else", and its list only ever held bank
          # templates -- so a form or report template could not be forced from
          # anywhere in the product, on the one route where forcing one is most
          # often what is wanted (a report family whose fingerprint is a hair too
          # tight reads as "no template for this layout" and there was no way to
          # say "no, use that one"). The engine has taken a template of any kind
          # for a while (convert_document(template_id=), which carries the kind
          # with it); nothing ever passed one. The list below now follows the
          # answer to "What is this?" -- bank templates, or form and report
          # templates -- so the panel is one control that means one thing on both
          # routes, and the summary says so without naming a bank.
          tags$details(class = "adv-bank",
            tags$summary("It picked the wrong template?"),
            # NO "include templates built here" tick-box. Whether a colleague's
            # template counts is not a per-conversion decision an accountant should
            # be making -- a template someone here built for this bank either works
            # for the team or it does not, and it is reviewed before it is trusted
            # for the dashboards (that gate is feed.allowed_template_origins, and it
            # is untouched by this). The tick-box only ever produced the puzzle "I
            # built this template and it does not work". It is now a deployment
            # setting: app.user_templates_default in config/config.yaml.
            # The bank itself is the control in FRONT of this panel; what is left
            # in here is the one thing that is genuinely rarer - forcing an exact
            # audited template. STATIC, never a renderUI: its choices are rewritten
            # every time a bank is chosen, and a control that re-renders under the
            # hand choosing it loses the choice being made.
            # No caption: the Bank picker is directly above and this control's own
            # label says what it is. A sentence describing two visible controls is
            # the screen explaining itself.
            div(style = "padding-top:10px",
              selectInput("cv_template", "Template (optional)",
                          choices = c("(auto-detect)" = ""), width = "100%")))
        ),
        mainPanel(
          width = 8,
          # ---------------------------------------------------------------
          # THE INTERFACE RULE (charter), applied to the result page.
          #
          # ABOVE the fold: what a forensic accountant came here for -- did it
          # work, here is your download, here are your transactions. That is the
          # whole job, so it is the whole default view.
          #
          # BEHIND ONE CONTROL: evidence about how the tool did its job -- the
          # X-ray, the chart, the checks, the diagnostics, the template controls.
          # All of it still exists and none of it is more than one click away,
          # because some people want every bit of it. It simply stops being
          # pushed at the people who do not.
          #
          # That click is REMEMBERED for the session (cv_detail_open), so someone
          # technical opens it once and never sees the button again. Nobody is
          # ever asked "are you an advanced user?" -- a question the tool can
          # answer for itself by watching what they do.
          # ---------------------------------------------------------------
          # A CASE FOLDER: one row per file, above the result page it opens.
          #
          # The whole job here is triage - which of these thirty files is fine,
          # and which broke the SAME WAY so they can be fixed together. So the
          # table sorts, it starts worst-first grouped by what went wrong, and a
          # row opens THAT file's ordinary result page below: the same verdict,
          # the same proof strip, the same transactions, the same downloads. There
          # is deliberately no second, thinner result view to keep in step.
          conditionalPanel("output.cv_has_batch == true",
            uiOutput("cv_batch_summary"),
            DTOutput("cv_batch"),
            uiOutput("cv_batch_open")),
          uiOutput("cv_status"),
          uiOutput("cv_headline"),   # the verdict, in her words
          uiOutput("cv_downloads"),  # the payoff, right under it
          # ...and whether those same figures reached the org dashboards. ONE
          # LINE, AND NOT BEHIND THE TOGGLE. It used to render at the very bottom
          # of the evidence panel, under a link captioned "The page, the checks,
          # and the template it used" -- which it is none of. So the question
          # "did my conversion feed Qlik" cost a click on a single file and two
          # on a case folder (open the row, then open the panel), and the answer
          # was filed under a heading that does not cover it.
          # ...and the one question a CLEAN result still has to answer: is this
          # read the way I want it? A statement read end to end by the wrong
          # template is the exact failure this tool exists to prevent, and it
          # looks perfect on screen -- so the way to correct it belongs above the
          # fold, not behind the evidence toggle. (Nothing rendered this output at
          # all, while cv_teach stayed silent on a clean result BECAUSE of it:
          # between them there was no route back anywhere on the page.)
          #
          # ONE DOOR, ALL THREE KINDS. It was cv_rematch and it was guarded on
          # .is_txn_result, so a converted report or form had no route back to the
          # template that read it anywhere on this page -- while the two links
          # written for exactly that landed on an empty "Add a template" screen
          # and threw the document away. The guard is gone and the widened control
          # is the one door: for a report or a form it OPENS the template that
          # read it with the document already under it, and for a statement it
          # still offers the toolkit. Position unchanged, directly under the
          # downloads, so the door sits beside the payoff on every route.
          uiOutput("cv_edit"),
          # Form / labelled-value PDF result (renders only when kind == "form").
          uiOutput("cv_form"),
          # Report result -- many tables, no transactions (kind == "tables").
          uiOutput("cv_tables"),
          # Before any conversion, a clear empty state rather than bare headers.
          conditionalPanel("output.cv_has_result != true && output.cv_has_batch != true",
                           uiOutput("cv_empty")),
          # The transaction half of the page: figures, the balance proof, the rows.
          # A form and a report have none of those, and each is rendered by its own
          # output above -- so both are excluded here, not just the form.
          conditionalPanel(paste("output.cv_has_result == true &&",
                                 "output.cv_is_form != true &&",
                                 "output.cv_is_tables != true"),
            # Figures + transactions render only when the parse produced rows: an
            # unsupported or failed result must never show zero-money cards and an
            # empty graph under its honest verdict.
            conditionalPanel("output.cv_has_txns == true",
              uiOutput("cv_summary"),
              uiOutput("cv_proof"),    # did it add up - always, pass or fail
              uiOutput("cv_split"),    # a bundle: what each statement in it says
              h4("Your transactions"),
              DTOutput("cv_txns")),
            # cv_teach is the NEXT ACTION when a layout is new ("set it up once and
            # it converts every time"), not evidence about the run -- so it stays
            # above the fold. Burying the only thing left to do would be the same
            # mistake in the other direction.
            uiOutput("cv_teach"),
            # THE CHECKS, ONE CLICK FROM THE VERDICT AND LITERALLY BELOW IT.
            #
            # This used to render at the bottom of the panel below, so reaching it
            # on a clean run cost TWO clicks - open "Show me how it read this",
            # then open "Checks & detail" - while the proof strip a few inches up
            # said "could not be checked (why, in Checks below)" and the link's own
            # caption promised the checks. An accountant counted the clicks. It is
            # also the wrong audience for that panel: the checks are the
            # accountant's evidence, the X-ray and the chart are the maintainer's.
            # It still opens ITSELF the moment anything is flagged, so on the runs
            # that matter the count is zero.
            uiOutput("cv_detail"),
            # ONE control; everything technical sits behind it -- AND ONLY WHERE
            # THERE IS SOMETHING BEHIND IT. Everything in this panel needs
            # transactions: the X-ray, the charts, the template candidates. On an
            # unsupported or failed run there are none, so the link opened onto an
            # empty box under a caption promising three things. A control that
            # does nothing is worse than no control: the reader concludes the page
            # is broken, on the screen that already has the least to go on.
            conditionalPanel("output.cv_has_txns == true",
              uiOutput("cv_more_toggle"),
              conditionalPanel("output.cv_detail_open == true",
                # THE HEADING BELONGS TO THE PAGE, AND ONLY A PDF HAS ONE. "See it
                # on the page" sat OUTSIDE this conditional, so a CSV or Excel
                # conversion -- which has no page to draw and no X-ray -- printed
                # the heading over a single sentence pointing somewhere else
                # entirely. A heading that promises a picture and delivers a
                # signpost is the screen telling a small lie on every spreadsheet
                # export the tool converts, and this is the one screen whose whole
                # job is to be checkable against the document.
                conditionalPanel("output.ix_is_pdf == true",
                  h4("See it on the page"),
                  # No "green = kept, amber = skipped" line here: ix_legend says it
                  # under the picture, beside the colours it is naming.
                  fluidRow(
                    column(3, numericInput("ix_page", "Page", 1, min = 1, step = 1)),
                    column(9, br(),
                      # The layer is NAMED FOR WHAT IT DRAWS. "Skipped rows" drew
                      # only the ones that look like transactions and did not
                      # read, so ticking it and counting the boxes against the
                      # table below gave two different numbers.
                      checkboxGroupInput("ix_layers", "Show on the page",
                        choices = c("Columns" = "cols", "Kept transaction rows" = "kept",
                                    stats::setNames("skipped", UNREAD_ROW_PLAIN_LAYER),
                                    "Redactions" = "redact",
                                    "Balances / dates / account" = "meta",
                                    "Faint box on every word" = "words"),
                        selected = c("cols", "kept", "skipped", "redact", "meta", "words"),
                        inline = TRUE))),
                  plotOutput("ix_plot", height = "640px"),
                  uiOutput("ix_legend"),
                  h4("Rows skipped on this page - and why"),
                  helpText(HTML("A real transaction here usually means a template fix - most often the <b>date format</b>. A one-off: select it and add it, flagged <b>forced</b>.")),
                  DTOutput("ix_skipped"),
                  br(),
                  actionButton("ix_add_row", "This IS a transaction - add the selected row", class = "btn-warning"),
                  tags$hr(),
                  downloadButton("ix_coverage_dl", "Download shareable diagnostic (page sizes and counts only)")),
                conditionalPanel("output.ix_is_pdf != true",
                  p(class = "muted", style = "margin:8px 0 0",
                    "There is no page picture for a CSV or Excel export - it has no page. Which column fed each field is under Field coverage, in Checks & detail just above.")),
                h4("Analysis"),
            # Design-system tokens, not one-off greys: this panel is the last
            # surface on Convert that drew its own border.
            div(style = "border:1px solid var(--line);border-radius:var(--r);padding:10px 14px;margin:6px 0 14px",
              fluidRow(
                column(4, selectInput("an_view", "Show",
                  c("Money in vs out" = "inout", "Balance over time" = "balance",
                    "Running total of every transaction" = "cumnet"), width = "100%")),
                column(4, selectInput("an_group", "Group by",
                  c("Day" = "day", "Week" = "week", "Month" = "month"),
                  selected = "week", width = "100%")),
                column(4, radioButtons("an_unit", "Measure",
                  c("Dollars" = "amount", "Count" = "count"), inline = TRUE))),
              plotOutput("cv_trend", height = "270px"),
              uiOutput("cv_trend_note")),
              # "Wrong template?" -- a real question, but for someone who wants to
              # look under the bonnet, not for the person who just wanted a file.
              uiOutput("cv_candidates")))),
          uiOutput("cv_feedback")
        )
      )
    ),
    # ---- Add a template (the statement toolkit, and one builder for
    #      everything that is not a statement) ------------------------------
    tabPanel(
      "Add a template",
      br(),
      # ONCE THERE IS A DOCUMENT, THE UPLOAD IS NOT THE SCREEN ANY MORE.
      #
      # This panel is four inches of chrome -- a heading, a sentence, a file
      # picker, a pair of radio buttons and a yellow note -- and every one of them
      # is answered the moment the document is on screen. Left where it was it
      # pushed the builder below the fold, so the instruction telling you what the
      # next drag does started the session already scrolled off. Uploading now
      # SWAPS the screen: one line saying which document is open, and a way back.
      conditionalPanel("input.ts_doctype == 'other' && output.rb_has_doc == true && output.rb_up_open != true",
        div(style = paste("display:flex;align-items:center;gap:12px;flex-wrap:wrap;",
                          "padding:8px 14px;margin:0 0 10px;border-radius:8px;",
                          "background:#eef4ef;border:1px solid #cfe0d4"),
          div(style = "font-size:16px;line-height:1", "\U0001F4C4"),
          div(style = "flex:1;min-width:0;font-size:13px;color:#2f4f3a",
            # EDITING SOMETHING, OR MAKING SOMETHING. Save writes to the id in the
            # box, so re-saving an opened template REPLACES it -- which is exactly
            # right for an edit and alarming if you thought you were starting
            # fresh. The line says which of the two is happening. ONE output per
            # id: rb_docname appears once, and only the words around it change.
            uiOutput("rb_editing_note", inline = TRUE),
            textOutput("rb_docname", inline = TRUE),
            # A page of a different size from the one the boxes were drawn on is
            # read scaled into their space. Said here, because it is the reason
            # the boxes sit where they do. (Register H4.)
            div(style = "font-size:12px;color:#5a6b5f;margin-top:2px",
                textOutput("rb_frame_note", inline = TRUE))),
          # The kind-of-document radio folds away with the panel, so the way back
          # has to say so -- otherwise somebody who picked "anything else" by
          # mistake has no visible way to change their mind.
          actionButton("rb_change_doc", "Change the document, or what kind it is",
                       class = "btn-default btn-sm"),
          # THE ONLY WAY TO EMPTY THIS SCREEN, and it is one link because it is
          # the rarer of the two things a second upload can mean. A new document
          # on top of work in progress is another EXAMPLE of that template; this
          # says "no, a different template altogether". (Register H5.)
          actionLink("rb_fresh", "Start a new template", class = "muted"))),
      conditionalPanel("!(input.ts_doctype == 'other' && output.rb_has_doc == true) || output.rb_up_open == true",
      wellPanel(
        h4(style = "margin-top:0", "Teach the tool a new layout"),
        # No "about two minutes". Re-measured on the same two real bank PDFs the
        # old note was written from, after the drafter was fixed: anz_single.pdf
        # now drafts cleanly (311 rows over the file, 79 in the 3-page preview),
        # but asb.pdf still drafts a template that reads ONE row out of ten
        # candidate lines and keeps nothing at all on page 2. One statement in two
        # is not a promise of a couple of minutes, and the sentence below is true
        # of both.
        p(class = "muted", style = "max-width:820px",
          "Upload one example. The tool reads what it can; you confirm it against a live preview and save."),
        # THE KIND FIRST, THEN THE FILE. It was the other way round, and the file
        # picker said ".csv / .tsv / .tdv / .pdf / .xlsx" while the panel below it
        # said "It has to be a PDF" -- both true, of different halves, and read
        # together they are a contradiction on the first screen of the job.
        # Answering "what kind" first means the picker can then say which files
        # THIS job takes, and only that.
        radioButtons("ts_doctype", "What kind of document is this?",
          c("A bank or card statement - a table of transactions" = "statement",
            "Anything else - a report, a form, a summary, a letter" = "other"),
          selected = "statement"),
        fileInput("ts_file", "One example of the document",
                  accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
        conditionalPanel("input.ts_doctype == 'statement'",
          p(class = "muted", style = "margin:-10px 0 12px;font-size:12.5px",
            "A PDF, CSV, TSV or Excel file.")),
        conditionalPanel("input.ts_doctype == 'other'",
          p(class = "muted", style = "margin:-10px 0 12px;font-size:12.5px",
            "It has to be a PDF: you build this one by pointing at the page.")),
        conditionalPanel("input.ts_doctype == 'statement'",
          actionButton("ts_go", "Open the toolkit", class = "btn-primary btn-lg"),
          # The guide sits AFTER the action it explains, so the thing to click is
          # the most prominent thing on the page.
          p(class = "muted", style = "margin:12px 0 0",
            actionLink("ts_help", "The guide - the ways statements differ, and what each setting means"))),
        conditionalPanel("input.ts_doctype == 'other'",
          div(style = "padding:10px 12px;background:#fffbe9;border:1px solid #f0c36d;border-radius:8px;margin-top:4px",
            strong("Anything that is not a transaction statement is read the same way"),
            p(style = "margin:6px 0 0;color:#555",
              paste("There are only two kinds of thing to pull out: TABLES, which have",
                    "rows and columns, and VALUES, which are one figure or one word with",
                    "a label. A document can have both, and one template holds both. No",
                    "running balance sits behind any of it, so the completeness checks",
                    "do not apply - you check what comes out. The output is a file to",
                    "download; none of this reaches the dashboards.")))))),
      # ONE BUILDER. IT STARTS BLANK, AND NOTHING HAPPENS UNTIL YOU ASK FOR IT.
      #
      # The version before this one treated a drag as a request: drag anywhere and
      # a table appeared. Which meant a mis-drag made a table, a second look at
      # the page made a table, and correcting a table made another table. A
      # gesture that always means "make me a new thing" cannot also mean "fix the
      # thing I am looking at", and fixing is most of the work.
      #
      # So the screen has ONE armed intent at a time, and it is always written
      # across the top: "DRAG a box round the table's TITLE", "CLICK where it
      # ENDS", "DRAG a box over the new column". Nothing is armed until a button
      # is pressed, and a drag with nothing armed does nothing at all except say
      # so. Every button that arms something is a plain sentence, and every thing
      # the tool works out for itself -- the columns, the end, which side a value
      # sits on -- is shown on screen with a control beside it to change it.
      conditionalPanel("input.ts_doctype == 'other'",
      br(),
      # THE ONE THING TO DO NEXT, said once and where the eye is. It used to sit
      # in a note four inches under the panel that carries the file picker, so
      # the screen's answer to "what now?" was below the thing it was about.
      conditionalPanel("output.rb_has_doc != true",
        uiOutput("rb_need_doc")),
      conditionalPanel("output.rb_has_doc == true",
      # WHAT THE SCREEN IS WAITING FOR, in one sentence, at the top, always.
      #
      # "At the top" has to mean the top of the SCREEN, not the top of the page.
      # The document image is 840px tall and the panel beside it is longer, so any
      # real work is done scrolled down -- and the sentence saying what the next
      # drag will do was then somewhere above the window, which is the same as not
      # being written at all. It sticks to the top of the window instead, over the
      # page image, for as long as the builder is open.
      div(class = "rb-sticky", uiOutput("rb_arm")),
      fluidRow(
        column(7,
          # ONE CONTROL FOR ONE FACT. "Back a page" and "Next page" were two more
          # buttons doing what this box's own arrows already do, on the screen the
          # owner called "LOTS of buttons"; the box is labelled with how many pages
          # there are and is bounded to them, so it cannot be stepped off the end.
          div(style = "display:flex;align-items:flex-end;gap:8px;flex-wrap:wrap;margin:0 0 6px",
            numericInput("rb_page", "Page", 1, min = 1, step = 1, width = "150px")),
          # SHOW ME LESS. Everything the tool draws sits on top of what the page
          # says -- and the column names printed on the page are the very thing
          # being checked against the column names the tool worked out. So the
          # tool's own names float in a strip ABOVE the page rather than over the
          # header row, and every layer switches off on its own, the way the X-ray
          # does.
          div(style = "margin:0 0 4px",
            checkboxGroupInput("rb_layers", NULL, inline = TRUE,
              choices = c("Tables" = "tables", "Column lines" = "edges",
                          "Names" = "names", "Values" = "values",
                          "Start and end" = "ends"),
              selected = c("tables", "edges", "names", "values", "ends"))),
          plotOutput("rb_plot", height = "840px", click = "rb_click",
            # resetOnNew: SHINY KEEPS A BRUSH ACROSS A RE-RENDER unless told not
            # to, and this plot re-renders on every change to the draft. That is
            # the other half of "the blue box does not disappear even when the
            # next column is added" -- the server had already cleared the value,
            # and the client redrew the rectangle anyway.
            brush = brushOpts("rb_brush", direction = "xy", delay = 900,
                              delayType = "debounce", resetOnNew = TRUE)),
          uiOutput("rb_hint")),
        column(5,
          tabsetPanel(id = "rb_tab", type = "tabs",
            # ---- TABLES ------------------------------------------------------
            tabPanel("Tables", value = "tables",
              br(),
              conditionalPanel("output.rb_has_draft != true",
                div(class = "note", style = "margin:0 0 10px",
                  p(style = "margin:0", strong("A table is rows and columns.")),
                  p(class = "muted", style = "margin:6px 0 0;font-size:12px",
                    paste("Press the button, then drag a box round the table's title.",
                          "You will be asked for its column names next. Nothing is",
                          "created until you press it."))),
                actionButton("rb_addtable", "+ Add a table", class = "btn-primary")),
              # The table being worked on. These inputs are built ONCE and filled
              # in as the draft changes: an input rebuilt by a renderUI is
              # destroyed on every keystroke that changes what it edits, which is
              # what made naming a table feel like the box was eating the typing.
              conditionalPanel("output.rb_has_draft == true",
                uiOutput("rb_step"),
                textInput("rb_name", "Call this table", "", width = "100%"),
                uiOutput("rb_where"),
                # EVERY POSITION IS BOTH DRAGGABLE AND TYPEABLE. Pointing at the
                # page is the easy way and the one that suits the job; typing the
                # number is the exact way, and somebody correcting a boundary by
                # four points should not have to hit it with a mouse. Same rule
                # as the column edges, and the same static inputs so typing in
                # them never rebuilds the box being typed into.
                tags$details(style = "margin:-4px 0 10px",
                  tags$summary(class = "muted", style = "cursor:pointer;font-size:12px",
                               "Type the exact positions instead"),
                  fluidRow(style = "margin-top:4px",
                    column(3, numericInput("rb_sp", "Starts page", 1, min = 1, step = 1, width = "100%")),
                    column(3, numericInput("rb_sy", "at", 0, step = 1, width = "100%")),
                    column(3, numericInput("rb_ep", "Ends page", 1, min = 1, step = 1, width = "100%")),
                    column(3, numericInput("rb_ey", "at", 0, step = 1, width = "100%"))),
                  helpText(class = "muted", style = "margin-top:-8px",
                           "Measured in points from the top of the page.")),
                # HOW MANY ROWS ARE THE HEADING, not just whether there is one.
                # A real report prints "Revenue" over "Medical & Family" over
                # "Welfare" -- three lines, one heading -- and calling only the
                # first of them a heading puts the other two in the table as
                # rows of data. The count is taken from the box that was drawn
                # round the column names, and can be corrected here.
                # "IT'S THE TOP ROW" IS ANSWERED, NOT ASKED. Named by the owner:
                # "'It's top row' - does that need to be prominent? Or can it be
                # collapsed?" It can. The answer is worked out from the box drawn
                # round the column names and printed in the line above (rb_step),
                # so the two controls that CHANGE it are behind a disclosure -
                # there when the guess is wrong, out of the way when it is right.
                tags$details(style = "margin:2px 0 10px",
                  tags$summary(class = "muted", style = "cursor:pointer;font-size:12px",
                               "Change what the top rows are"),
                  div(style = paste("display:flex;align-items:flex-end;gap:10px;",
                                    "flex-wrap:wrap;margin-top:6px"),
                    div(style = "flex:1;min-width:220px",
                      radioButtons("rb_hdr", "Its top rows",
                        c("name the columns, and are not read as data" = "head",
                          "are data, like every other row" = "data"), selected = "head")),
                    conditionalPanel("input.rb_hdr == 'head'",
                      # "How many rows", sitting under a table, reads as "how many
                      # rows has this table" -- which is not a question anybody is
                      # ever asked here. The number of DATA rows is worked out by
                      # reading between the start and the end; this is only how
                      # tall the heading is.
                      div(style = "width:170px;padding-bottom:10px",
                        numericInput("rb_hdrn", "Heading rows", 1, min = 1, max = 8,
                                     step = 1, width = "100%")))),
                  helpText(class = "muted", style = "margin:-6px 0 8px",
                           paste("How many printed lines the heading takes up.",
                                 "You never say how many rows of DATA there are -",
                                 "that is read from the page, between the start and the end."))),
                div(style = paste("display:flex;justify-content:space-between;",
                                  "align-items:center;margin:12px 0 4px"),
                  strong(textOutput("rb_ncols", inline = TRUE)),
                  actionButton("rb_addcol", "+ Add a column", class = "btn-default btn-sm")),
                uiOutput("rb_cols"),
                # One column at a time, edited in a panel that never moves. The
                # alternative -- a name box on every row -- rebuilds the whole
                # list on every letter typed into any of them.
                conditionalPanel("output.rb_col_sel == true",
                  div(class = "note", style = "margin:8px 0",
                    uiOutput("rb_col_head"),
                    fluidRow(
                      column(7, textInput("rb_cname", "Its name", "", width = "100%")),
                      column(5, selectInput("rb_ckind", "What is in it",
                        c("work it out" = "auto", "text" = "text", "money" = "money",
                          "date" = "date", "number" = "number"),
                        selected = "auto", width = "100%", selectize = FALSE))),
                    fluidRow(
                      column(6, numericInput("rb_cx0", "Left edge", 0, step = 1, width = "100%")),
                      column(6, numericInput("rb_cx1", "Right edge", 0, step = 1, width = "100%"))),
                    # TWO CONTROLS FOR ONE JOB, TWICE OVER. "Drag its width on the
                    # page" armed the drag that pressing Edit on the column has
                    # already armed - the banner turns amber and names the column
                    # the moment Edit is pressed - and "Delete this column" did
                    # what the x on the column's own row does, a finger away. Both
                    # are gone. A second way to do a thing is a second thing to
                    # read before you can do it once.
                    div(style = "display:flex;gap:6px;flex-wrap:wrap",
                      actionButton("rb_cdone", "Done with this column",
                                   class = "btn-default btn-sm")))),
                div(style = "display:flex;gap:6px;flex-wrap:wrap;margin-top:12px",
                  actionButton("rb_savetab", "Save this table", class = "btn-primary"),
                  actionButton("rb_refit", "Work the columns out again", class = "btn-default"),
                  # CANCEL MEANS TWO DIFFERENT THINGS AND MUST SAY WHICH. On a new
                  # table it throws the drawing away; on a saved one it leaves the
                  # saved table exactly as it is. Rendered, so the words are the
                  # true ones. (Register H13.)
                  uiOutput("rb_cancel_btn", inline = TRUE))),
              tags$hr(style = "margin:14px 0 8px"),
              strong("Tables on this template"),
              uiOutput("rb_saved")),
            # ---- VALUES ------------------------------------------------------
            tabPanel("Values", value = "values",
              br(),
              conditionalPanel("output.rb_has_vdraft != true",
                div(class = "note", style = "margin:0 0 10px",
                  p(style = "margin:0", strong("A value is one figure or one word, with a label.")),
                  p(class = "muted", style = "margin:6px 0 0;font-size:12px",
                    paste("Two drags: the label, then the value. The tool works out",
                          "which side the value sits on and looks there next time --",
                          "and you can change that, and both boxes, whenever you like."))),
                actionButton("rb_addval", "+ Add a value", class = "btn-primary")),
              conditionalPanel("output.rb_has_vdraft == true",
                uiOutput("rb_vstep"),
                fluidRow(
                  # THE NAME IS A COLUMN HEADING IN A FILE, so it is lower case
                  # with underscores -- ird_number, not "IRD Number". Type it
                  # however you like; the line under the box shows the name it
                  # will actually carry, and the box is set to that on save.
                  # Normalising while somebody types would rewrite the box under
                  # their caret, which is the one thing this screen must not do.
                  column(7, textInput("rb_vname", "Call this value", "", width = "100%"),
                    div(style = "margin:-8px 0 8px;font-size:12px;color:#666666",
                        textOutput("rb_vname_key", inline = TRUE))),
                  column(5, selectInput("rb_vtype", "What it is",
                    c("text" = "text", "money" = "money", "date" = "date",
                      "date range" = "date_range"),
                    selected = "text", width = "100%", selectize = FALSE))),
                selectInput("rb_vwhere", "On the next document, look for the value",
                  c("to the right of the label" = "right",
                    "to the left of the label"  = "left",
                    "under the label"           = "below",
                    "above the label"           = "above"),
                  selected = "right", width = "100%", selectize = FALSE),
                div(style = "display:flex;gap:6px;flex-wrap:wrap",
                  actionButton("rb_vsave", "Save this value", class = "btn-primary"),
                  actionButton("rb_vlabel", "Re-drag the label", class = "btn-default btn-sm"),
                  actionButton("rb_vvalue", "Re-drag the value", class = "btn-default btn-sm"),
                  actionButton("rb_vcancel", "Throw it away", class = "btn-default btn-sm"))),
              tags$hr(style = "margin:14px 0 8px"),
              strong("Values on this template"),
              uiOutput("rb_vsaved"),
              tags$details(style = "margin-top:12px",
                tags$summary(class = "muted", style = "cursor:pointer",
                             "Know the wording already? Type them instead"),
                textAreaInput("rb_val_typed", NULL, rows = 4, width = "100%", value = ""),
                helpText(class = "muted", HTML(paste0(
                  "One per line: <b>name</b> = <b>the wording on the page</b> | <b>kind</b>. ",
                  "Extra wordings after a semicolon. Kinds: money, date, date_range, text.",
                  "<br><code>closing_balance = Closing balance; Balance at end | money</code>")))))
          ))),
      tags$hr(style = "margin:18px 0 12px"),
      fluidRow(
        column(7,
          h4(style = "margin-top:0", "Check what comes out"),
          # A GREEN BUTTON THAT REFUSES IS FRICTION. It looked ready from the
          # moment the page loaded and answered "nothing to read yet" -- so the
          # most prominent control on the lower half of the screen was, for the
          # whole of the work, a thing that does not work. Off until there is
          # something to read, with the reason beside it.
          div(style = "margin:6px 0", uiOutput("rb_preview_btn")),
          uiOutput("rb_prev_status")),
        column(5,
          h4(style = "margin-top:0", "Name it and save"),
          # THE SAVE NAME IS WORKED OUT, SO IT IS NOT A QUESTION. It is composed
          # from the issuer and the kind of document, both of which are filled in
          # for you, and it only ever has to be touched when two templates would
          # otherwise share it. Behind a disclosure, with the name it will use
          # printed above so nothing is hidden - only folded. (Register D2.)
          # THE SAME QUESTION AS THE TOOLKIT'S, IN THE SAME WORDS. The statement
          # side asks "Which bank is this statement from?"; this asked for an
          # "Issuer", which is the field's name in the file rather than anything
          # a person would say. One question, asked one way, on both routes.
          textInput("rb_bank", "Who produced this document?", "NewIssuer", width = "100%"),
          div(style = "margin:-8px 0 6px;font-size:12px;color:#666666",
              textOutput("rb_id_note", inline = TRUE)),
          tags$details(style = "margin:-6px 0 10px",
            tags$summary(class = "muted", style = "cursor:pointer;font-size:12px",
                         "Change the kind of document, or the name it saves under"),
            div(style = "margin-top:6px",
              textInput("rb_type", "Kind of document", "report", width = "100%"),
              textInput("rb_id", "Saves under this name", "new_report", width = "100%"))),
          textAreaInput("rb_fp", "A phrase printed on it (one per line)",
                        rows = 2, width = "100%", value = ""),
          # DRAG IT OFF THE PAGE, like everything else here.
          #
          # Reported: "I put a phrase printed on it, bang smack on front page,
          # still used another template. Maybe another drag?" Half of that is the
          # search order (Convert now asks what the document is). The other half
          # is this box: it is the ONE thing on the builder that had to be typed,
          # character for character, and a fingerprint that does not match the
          # page exactly matches nothing at all. Every other box on this screen is
          # filled by pointing at the page; so is this one now.
          div(style = "margin:-6px 0 8px",
            actionButton("rb_arm_phrase", "Drag it off the page instead",
                         class = "btn-default btn-sm")),
          helpText(class = "muted",
                   "One phrase per line - all must appear, and they must not be words every document carries or anything naming a person."),
          # SAVE IS THE THING TO DO NEXT AND IT HAS TO LOOK LIKE IT.
          #
          # "Save - this needs to be more prominent / draw in the user to know
          # what to do next." It was a standard-sized button the same colour and
          # size as the six others on the screen, at the bottom of the third
          # column, under a help sentence. It is now the widest, tallest thing in
          # its column and the only large button in the builder, and the line
          # under it says what saving DOES - which is the half of "what do I do
          # next" a button label cannot carry.
          actionButton("rb_save", "Save template",
                       class = "btn-primary btn-lg rb-save"),
          div(class = "muted", style = "margin:6px 0 0;font-size:12px",
              textOutput("rb_save_note", inline = TRUE)),
          uiOutput("rb_msg"))),
      # WHAT CAME OUT: the tables AND the label/value pairs. A template can be all
      # pairs and no table at all -- a form -- and a preview that shows only
      # tables tells that person nothing about the only thing they built.
      # One block per table, built inside rb_prev_head -- the DT outputs behind
      # them are declared once in the server, not here.
      uiOutput("rb_prev_head"),
      uiOutput("rb_prev_values_head"),
      tableOutput("rb_prev_values"),
      uiOutput("rb_prev_summary_head"),
      tableOutput("rb_prev_summary"),
      tags$details(style = "margin-top:14px",
        tags$summary(class = "muted", style = "cursor:pointer",
                     "The template file this will save"),
        div(class = "mono", verbatimTextOutput("rb_yaml"))))
    )
    ),
    # ---- Admin (insights + batch intake) ------------------------------
    tabPanel(
      "Admin",
      br(),
      # A broken settings file is announced to everyone who opens Admin, signed in
      # or not (without the parse detail, which only an admin sees) -- a silent
      # revert to built-in defaults is how the password and the feed folder change
      # behind the team's back.
      uiOutput("adm_cfg_banner"),
      # The sign-in box is rendered SERVER-side, because when no admin password has
      # been set there must be no box at all -- just the instructions for setting one.
      conditionalPanel("!output.admin_authed", uiOutput("adm_login_panel")),
      conditionalPanel("output.admin_authed",
      div(style = "text-align:right;margin-bottom:6px",
          actionButton("adm_signout", "Sign out of Admin", class = "btn-default btn-sm")),
      # ---- ADMIN IS TWO TABS, BECAUSE AN ADMIN HAS TWO QUESTIONS ---------------
      #
      # It was four - Insights, Templates, Data capture, Batch & audit - and the
      # questions behind them are only ever: WHAT IS IN THE LIBRARY AND IS IT
      # RIGHT, and WHAT IS FAILING. "Insights" and "Batch & audit" both answered
      # the second one, in two places, so the same gap was reported twice under
      # two headings; "Data capture" was a whole tab of on-box analytics settings
      # that nobody in a police unit comes here to change.
      #
      # Nothing was deleted. Every control that did something is still here, and
      # the two that only ever repeated something else are gone (see below). The
      # settings nobody changes are behind a disclosure, one click away, where a
      # setting nobody changes belongs.
      #
      # The two vocabularies sit together on Templates now, not one tab apart:
      # dictionaries\labels.yaml and dictionaries\lexicon.yaml both mean "words
      # the tool looks for", and they were on two different screens.
      tabsetPanel(
        tabPanel(
          "Templates",
          br(),
          # A TEMPLATE THAT FAILED VALIDATION USED TO JUST VANISH. All three
          # loaders collect the reason on attr(x, "load_errors") and the comments
          # beside them promise it is "never SILENT" -- and nothing in the app
          # read the attribute once, so a report template that failed the
          # fingerprint gate produced "unsupported" on every document with no hint
          # anywhere that the file existed. It is the first thing on this tab
          # because it is the first thing that explains a template that "stopped
          # working" after an update.
          uiOutput("adm_tpl_load_errors"),
          # The two bands are on the screen, headed; saying they are there is the
          # screen describing itself. What is left is the one thing that is not
          # obvious from looking - what the two origins mean, and that a row is
          # clickable.
          helpText(HTML(paste0(
            "<b>tested</b> = shipped and checked, <b>user</b> = built here. ",
            "Click a row to view and edit it."))),
          DTOutput("adm_tpl_overview"),
          br(),
          # FEEDBACK BESIDE THE TEMPLATES IT IS ABOUT, split the same way, so the
          # two tables read as one screen. Every rating names the document it came
          # from and the template that read it; clicking a row picks that template
          # below, because "traceable to the template" means a route to it, not a
          # printed id.
          h4("What the team said about these conversions"),
          helpText("Every rating left on a conversion, newest first - click a row to pick that template below."),
          DTOutput("adm_tpl_feedback"),
          br(),
          fluidRow(
            column(5,
              selectizeInput("adm_tpl_pick", "Preview / edit a template", choices = NULL,
                             options = list(placeholder = "Type to search, or click a row above")),
              uiOutput("adm_tpl_origin"),
              # THE WAY BACK TO THE BANDS. Editing a template here meant editing its
              # YAML as text -- fine for a threshold, useless for "the credit column
              # has moved 4pt left", which is what actually goes wrong. The visual
              # editor could only ever be reached by converting a statement first,
              # so there was no route from "this template is wrong" to the picture
              # of it. It needs a page to draw on, and the saved statements know
              # which template read them, so the page comes from there.
              # The label follows the KIND of template selected -- see
              # output$adm_tpl_bands_btn. A statement's columns are redrawn on a
              # page; a report is reopened in the builder it was drawn in; a form
              # has no picture at all.
              uiOutput("adm_tpl_bands_btn"),
              uiOutput("adm_tpl_bands_msg"),
              br(),
              # SAVE IS NOT HERE. It writes what is in the YAML box, so it lives
              # under the YAML box -- two green buttons in one column, one of them
              # about the picture and one about the text beside it, is the screen
              # asking which without saying so.
              actionButton("adm_tpl_dup", "Duplicate (new id)"),
              actionButton("adm_tpl_validate", "Check it's valid"),
              actionButton("adm_tpl_hide", "Hide / un-hide"),
              actionButton("adm_tpl_delete", "Delete", class = "btn-danger"),
              br(), br(), uiOutput("adm_tpl_msg"),
              # THE ONE THING THAT MAKES A FORTY-TABLE TEMPLATE MAINTAINABLE.
              # After adding table 41, the question is whether the other forty
              # still read - and there was no way to ask it. There is no folder
              # picker beside this button on purpose: the examples are the
              # documents this template has already read on this box, and the tool
              # knows which those are, so it does not ask.
              tags$hr(),
              uiOutput("adm_tpl_check_btn"),
              uiOutput("adm_tpl_check_msg"),
              tags$details(
                tags$summary(class = "muted", style = "cursor:pointer;font-size:12.5px", "How these actions work"),
                helpText("Duplicate copies this template with a new id into the editor - tweak it and Save. Rename by changing the id, saving, then deleting the old one. Hide parks a template out of detection without deleting it. Hide and Delete only work on templates built HERE; shipped 'tested' ones are read-only and win on an id clash, so a copy needs its own id. Save writes the text on the right into the folder for that kind of template - change its 'mode' and it moves."))),
            column(7,
              h4("Template YAML"),
              textAreaInput("adm_tpl_edit", NULL, value = "", width = "100%", height = "460px"),
              actionButton("adm_tpl_save", "Save this text as a user template",
                           class = "btn-primary"))
          ),
          uiOutput("adm_tpl_check_head"),
          DTOutput("adm_tpl_check_grid"),
          tags$hr(),
          h4("Near-duplicate bank statement templates - consolidate the pile"),
          helpText("Bank statement templates that read a statement identically (same format, amounts, dates and columns) but were drafted more than once. Keep the best one and Hide or Delete the rest - pick any id above to act on it. (Forms and reports are not compared this way: two of them can read the same document and still be two different jobs.)"),
          uiOutput("adm_tpl_dupes"),
          tags$hr(),
          # THE WORDS, BOTH SETS, IN ONE PLACE. The label dictionary (which value
          # a wording means) and the recognition vocabulary (which words mean
          # money out, money in, a heading, a brand) were on two different Admin
          # tabs. They are the same question asked twice.
          h4("Words the tool looks for"),
          helpText(HTML(paste0(
            "The usual reason a value comes up <b>blank</b> is that the statement prints a wording ",
            "the tool has not met - it says <i>\"Balance at start\"</i> where the tool knows ",
            "<i>\"Opening balance\"</i>. Add the exact wording and Save; it applies from the next ",
            "conversion."))),
          # The common case first, in plain English: pick the value, type the exact
          # wording, add it. It writes through dictionary_append() (R/labels.R),
          # which INSERTS one line and leaves the rest of the file -- comments and
          # all -- untouched. The whole-file editor is kept one click below for the
          # rare edit a wording cannot express (a pattern, a page rule, a new value).
          fluidRow(
            column(5,
              uiOutput("adm_dict_field_ui"),
              textInput("adm_dict_phrase", "The wording this statement prints",
                        placeholder = "balance at start"),
              actionButton("adm_dict_add", "Teach it this wording", class = "btn-primary"),
              br(), br(), uiOutput("adm_dict_add_msg")),
            column(7,
              helpText(HTML(paste0(
                "Type it as the statement prints it - case doesn't matter, and part of the ",
                "wording is enough (<i>\"balance at start\"</i> matches <i>\"Balance at start ",
                "of period\"</i>). It applies from the next conversion, everywhere the tool ",
                "looks for that value. A backup of the file is kept each time."))))),
          br(),
          # NB the WORDING is a navigation anchor: docs/, config.example.yaml and
          # dictionaries/lexicon.yaml all send the reader to "Words the tool knows
          # to look for". It moved tabs (this used to be under "Data capture");
          # the phrase they point at is deliberately unchanged.
          h5("Words the tool knows to look for"),
          helpText(HTML(paste0(
            "And when a statement writes something the tool has never met - <code>cow</code> for money ",
            "out, say - it cannot tell which way that money went. Teach it the word here and every ",
            "conversion from then on knows it, everywhere. Nothing is ever added automatically: ",
            "you decide."))),
          fluidRow(
            column(5, selectInput("adm_word_kind", "This word means\u2026",
              c("money OUT - a debit marker (D, DR, Paid...)"    = "debit_markers",
                "money IN - a credit marker (C, CR, Recd...)"    = "credit_markers",
                "the heading of a money-OUT column"            = "amount_style_debit_headers",
                "the heading of a money-IN column"             = "amount_style_credit_headers",
                "the balance is overdrawn"                     = "overdrawn_markers",
                "a value has been redacted / blacked out"      = "redaction_markers",
                "a word that appears in a table's heading row" = "header_keywords",
                "a bank or brand name, not a customer's name"  = "fingerprint_brand_words"))),
            column(4, textInput("adm_word_text", "The word, exactly as the statement prints it", "")),
            column(3, br(), actionButton("adm_word_add", "Teach it this word", class = "btn-primary"))),
          uiOutput("adm_word_msg"),
          br(),
          h5("Words your statements used that the tool didn't recognise"),
          # Rendered, not fixed: the instructions ("pick one, say which way the
          # money goes, and Approve") stood over an EMPTY table with nothing in the
          # picker - 249 conversion records scanned here, not one unrecognised
          # marker among them - which reads as a control that has stopped working.
          # It has not: a word only lands here when a statement uses a debit/credit
          # INDICATOR COLUMN whose value the vocabulary has never met, and no
          # template in use does. So when there is nothing, the line says that,
          # rather than instructing the reader to use a picker with no options.
          uiOutput("adm_sugg_help"),
          uiOutput("adm_sugg_scope"),
          fluidRow(
            column(6,
              tableOutput("adm_sugg_tokens"),
              fluidRow(
                column(5, selectInput("adm_sugg_tok", "Word", choices = character(0))),
                column(4, radioButtons("adm_sugg_dir", "Means", inline = TRUE,
                                       choices = c("money out (debit)" = "debit_markers",
                                                   "money in (credit)" = "credit_markers"))),
                column(3, br(), actionButton("adm_sugg_approve", "Approve", class = "btn-primary"))),
              uiOutput("adm_sugg_msg")),
            column(6,
              strong("Columns in your statements that no template uses"),
              tableOutput("adm_sugg_cols"),
              # Says where the odd entries come from. This list is harvested from
              # EVERY conversion, including the ones where nothing was recognised,
              # so a letter or a file that is not a statement contributes its first
              # line as if it were a column heading ("Dear Sir", "a", "b", "c").
              # Left in rather than filtered out - what a file offered as headings
              # is a fact about that file - but no longer presented as though every
              # row were a field somebody should go and map.
              helpText(HTML(paste0(
                "A column that keeps turning up unused is usually a field worth mapping in that bank's ",
                "template. Rows that read as prose came from files nothing could read - the first line ",
                "of a letter or a non-statement is offered here as if it were a heading."))))),
          br(),
          tags$details(
            tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
              "Edit the whole vocabulary file or the whole dictionary file - for a pattern, a page rule, or a value that isn't listed yet"),
            div(style = "padding-top:10px",
              helpText(HTML(paste0(
                "Each value in the label dictionary has an <code>any_of:</code> list of wordings; add a ",
                "line under it, indented the same as the lines already there, wording in quotes:<br>",
                "<span class='mono' style='font-size:12px'>opening_balance:<br>",
                "&nbsp;&nbsp;any_of:<br>",
                "&nbsp;&nbsp;&nbsp;&nbsp;- \"opening balance\"<br>",
                "&nbsp;&nbsp;&nbsp;&nbsp;- \"balance at start\"&nbsp;&nbsp; &lt;- your new line</span><br>",
                "Save refuses anything that isn't laid out properly, and keeps a backup, so it ",
                "is safe to try."))),
              fluidRow(
                column(5,
                  actionButton("adm_dict_reload", "Reload from file"),
                  actionButton("adm_dict_save", "Save dictionary", class = "btn-primary"),
                  br(), br(), uiOutput("adm_dict_msg")),
                column(7,
                  textAreaInput("adm_dict_edit", NULL, value = "", width = "100%", height = "360px"))),
              tags$hr(),
              helpText(HTML(paste0(
                "Beyond single words the recognition vocabulary also holds the money / date / account ",
                "<b>patterns</b> and the date formats to try. Word lists ADD to the built-ins; a pattern ",
                "REPLACES one (and is refused if it won't work). Leave a category out to keep its ",
                "default. <b>Show built-in defaults</b> prints a complete, valid starting point to ",
                "copy from."))),
              fluidRow(
                column(5,
                  actionButton("adm_lex_reload", "Reload from file"),
                  actionButton("adm_lex_defaults", "Show built-in defaults"),
                  actionButton("adm_lex_save", "Save vocabulary", class = "btn-primary"),
                  br(), br(), uiOutput("adm_lex_msg")),
                column(7,
                  textAreaInput("adm_lex_edit", NULL, value = "", width = "100%", height = "320px")))))
        ),
        tabPanel(
          "Health",
          br(),
          actionButton("adm_refresh", "Refresh from logs", class = "btn-primary"),
          helpText("A live picture from every conversion the team has run and every rating left."),
          # HOW MUCH OF THE HISTORY THIS PICTURE IS MADE OF. Reading every archived
          # conversion one record at a time took 10.5 seconds on 20,000 rows, in the
          # one process the whole team shares - so it reads the newest slice and
          # says which slice, rather than freezing everybody's browser to be
          # complete. Nothing is lost: the archive is still on disk in full.
          uiOutput("adm_history_note"),
          tags$hr(),
          fluidRow(
            column(5, h4("Conversions by status"), plotOutput("adm_status_plot", height = "210px"),
                   DTOutput("adm_overview")),
            column(7,
              h4("Templates that started failing recently"),
              # HEALTH IS DEFINED PER ROUTE, and this table used to render a row of
              # NAs for anything that was not a bank statement: it asked whether a
              # report RECONCILED, which a report never does. A statement is healthy
              # when it adds up; a report when every table was found and no word fell
              # outside a column; a form when nothing was disputed or missing.
              helpText("A layout can change slightly - a field moves, a heading gets renamed - and stop reading properly. Any template that is suddenly producing more conversions worth a check shows here, whichever kind it is. Empty is good."),
              DTOutput("adm_drift"))),
          h4("Layouts the tool can't read yet - the gaps to fill"),
          helpText("Each row is one layout no template recognises yet (identical layouts are grouped). The biggest count is the one to build a template for first - it unblocks the most documents."),
          DTOutput("adm_gaps"),
          # SPLIT OUT, because a template does not fix these and this list used to
          # mix them in: a run that never reached a template at all arrived in the
          # gaps table with a blank layout and a blank closest-template, and one
          # unreadable moment (a file that had gone by the time it was opened) put
          # a statement that converts cleanly every day on the "can't read yet"
          # list for good.
          h5("Files that could not be opened at all"),
          helpText("Not a missing layout: these runs never got as far as a template - a damaged or password-protected file, a file that had gone by the time it was read, or something that isn't a statement. Building a template will not change them."),
          DTOutput("adm_unreadable"),
          h4("Template usage"),
          DTOutput("adm_usage"),
          tags$hr(),
          # THE HEADING DESCRIBED THE PICKUP QUEUE; THE TABLE IS THE WHOLE LOG.
          # read_uploads() returns EVERY upload record, newest first, with no
          # filter -- which is exactly what the incident procedure sends a
          # maintainer here for ("you know the file, or roughly when it was
          # converted"). It was headed "new formats to pick up" over help text
          # reading "Statements the tool couldn't read, that nobody has set up
          # yet", so at step 1 of that procedure, hunting a SUCCESSFUL conversion,
          # the screen told the maintainer not to look in the one table that had
          # it. (Measured: two rows, one `ok / westpac_everyday_pdf /
          # needs_pickup false`, under a heading that says it cannot be there.)
          # The pickup queue is still here -- it is the `needs_pickup` column, and
          # the picker beside the table still offers only those -- so nothing is
          # lost by naming the table after what it holds.
          h4("Uploads - every document converted here, newest first"),
          helpText(paste("One row per upload, whatever became of it - the route in when you know the file",
                         "or roughly when it was converted. `needs_pickup` marks a new format the tool",
                         "couldn't read that nobody has set up yet; the picker on the right offers those:",
                         "download an upload's safe summary (no personal data) or open it in the toolkit.")),
          fluidRow(
            column(8, DTOutput("adm_uploads")),
            column(4,
              # selectize, not a plain select: on a busy server this is hundreds of
              # entries and the only way to find one is to type part of it.
              selectizeInput("adm_up_pick", "Pick a saved upload", choices = NULL,
                             options = list(placeholder = "Type to search, or click a row on the left")),
              # Rendered server-side (see dl_when): a download with nothing to send
              # used to answer an HTTP 500 error page instead of a file.
              uiOutput("adm_up_audit_ui"),
              br(), br(),
              actionButton("adm_up_wizard", "Set it up - open the toolkit",
                           class = "btn-warning"))),
          tags$hr(),
          h4("Format requests - raised by the team"),
          helpText("Layouts the team flagged as unsupported, in their own words (no personal data). Build the template, then mark it done."),
          fluidRow(
            column(9, DTOutput("adm_requests")),
            column(3,
              selectizeInput("adm_req_pick", "A request", choices = NULL,
                             options = list(placeholder = "Loading\u2026")),
              actionButton("adm_req_actioned", "Mark done", class = "btn-primary"),
              br(), br(),
              actionButton("adm_req_dismiss", "Dismiss"),
              # Both of these change a request's status ON DISK and said nothing at
              # all: the row left the picker and that was the only sign anything
              # had happened, which is indistinguishable from a control that did
              # nothing.
              br(), br(), uiOutput("adm_req_msg"))),
          tags$hr(),
          h4("Folder intake - inbox / processed / failed"),
          helpText("Statements dropped into the inbox/ folder land here. Anything in failed/ is worth a look."),
          uiOutput("adm_inbox_counts"),
          fluidRow(
            column(8, h5("Failed - needs attention"), DTOutput("adm_inbox_failed")),
            column(4,
              selectizeInput("adm_inbox_pick", "A failed file", choices = NULL,
                             options = list(placeholder = "Loading\u2026")),
              actionButton("adm_inbox_wizard", "Open in the toolkit", class = "btn-warning"),
              br(), br(),
              uiOutput("adm_inbox_audit_ui"))),
          fluidRow(
            column(4, h5("Waiting in inbox"), DTOutput("adm_inbox_waiting")),
            column(4, h5("Processed"), DTOutput("adm_inbox_processed")),
            column(4, h5("Output folders (outbox)"), DTOutput("adm_inbox_outbox"))),
          tags$hr(),
          # THE ANALYTICS FEED, WHERE THE PERSON WHO CAN FIX IT WILL SEE IT.
          # The Convert screen used to carry this, which told an analyst about a
          # pipeline she has no part in and left the one case that MATTERS -- a
          # feed write that failed -- announced only to whoever happened to run
          # that conversion. It is a server fault, so it belongs here.
          h4("Analytics feed"),
          uiOutput("adm_feed_health"),
          tags$hr(),
          # THE BULK AUDIT, which used to be a tab of its own answering the same
          # question as the gaps table above: what is not converting. It is a
          # SOURCE of that answer, not a second answer, so it sits under it.
          # IT AUDITS; IT DOES NOT CONVERT. A tick-box here also converted and
          # saved the whole pile -- which is what Convert's own picker does when
          # you hand it thirty files, on the screen an analyst already knows, with
          # a result page per file and one download for the lot. A second door
          # into the same engine call is a door nobody exercises weekly, and this
          # one wrote real outputs and log records from an Admin tab whose whole
          # subject is what is FAILING. (Register 1b.)
          h4("Check a pile of files at once"),
          helpText(HTML("Drop in a pile of statements and get one picture: what converts, the gap layouts <b>biggest-first</b>, and <b>ready-to-edit draft templates</b> for them. Nothing is converted or saved - only shapes and counts, so it is safe to share.")),
          fluidRow(
            column(4,
              fileInput("adm_ba_files", "Statements (.csv / .tsv / .pdf / .xlsx)", multiple = TRUE,
                        accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
              actionButton("adm_ba_run", "Run", class = "btn-primary"),
              br(), br(),
              uiOutput("adm_ba_report_ui"),
              br(),
              uiOutput("adm_ba_csv_ui"),
              br(),
              helpText("Also available headless: Rscript scripts/bulk-audit.R <folder>")),
            column(8,
              uiOutput("adm_ba_summary"),
              h5("Gaps in this pile - biggest first"), DTOutput("adm_ba_clusters"),
              h5("Per file - shapes only, no personal data"), DTOutput("adm_ba_files_tbl"))),
          h5("Recommended draft templates (editable - copy into the Templates tab to save)"),
          uiOutput("adm_ba_recs"),
          # (A SECOND file picker stood here, "Single statement - safe summary",
          # doing what the picker above already does with one file - and a saved
          # upload's summary is a third route to the same export. Every extra
          # route into one function is a route that rots because nobody exercises
          # it weekly, so it is gone rather than kept working.)
          tags$hr(),
          h4("Housekeeping"),
          actionButton("adm_rollup", sprintf("Tidy up logs (archive runs older than %d days)", LOG_KEEP_DAYS)),
          uiOutput("adm_rollup_msg"),
          # WHAT HAPPENS TO THE FEED FOLDER, from the setting that decides it, so
          # the promise on the screen and the rule on disk cannot drift apart.
          uiOutput("adm_feed_retention"),
          br(),
          # Retention of the SAVED STATEMENTS themselves -- real client data, and
          # until now nothing ever deleted it. Runs at startup too; this is the
          # "do it now" button, and it says what it will do before you press it.
          h5("Saved statements - retention"),
          helpText(UPLOADS_NOTE),
          actionButton("adm_purge_uploads",
                       if (UPLOADS_KEEP_DAYS > 0)
                         sprintf("Delete saved statements older than %d days now", as.integer(UPLOADS_KEEP_DAYS))
                       else "Delete old saved statements now (retention is set to 'keep indefinitely')",
                       class = "btn-danger"),
          uiOutput("adm_purge_msg"),
          br(),
          # NB "Data capture" was a tab; docs/, config.example.yaml and
          # dictionaries/lexicon.yaml send the reader to "Admin -> Data capture",
          # so the words stay as the summary of this disclosure.
          tags$details(
            tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
                         "Data capture - what this server records about its own conversions"),
            div(style = "padding:8px 2px",
              helpText(HTML(paste0(
                "Every conversion can save a rich, structured record of <b>how it went</b> ",
                "(the layout it matched, how cleanly it parsed, detection scores, ",
                "reconciliation outcomes, OCR / redaction signals). It is stored on <b>this ",
                "machine only</b> under <code>logs/metadata/</code>, kept forever, and ",
                "<b>never enters the Qlik feed</b>. <b>No statement content is stored</b> - ",
                "only structure, counts and quality signals; any account number is stored ",
                "only as a one-way hash."))),
              fluidRow(
                column(5,
                  # ONE QUESTION, NOT TWO. There used to be a nine-box category
                  # list beside this, and "Full - everything" already answers it:
                  # a switch that repeats what the setting above it just said is a
                  # control the tool could have worked out for itself. The level
                  # decides, every category is captured within it, and the line
                  # below says so before Save is pressed.
                  radioButtons("adm_meta_level", "How much to capture",
                    choices = c("Full - everything (recommended)" = "full",
                                "Standard - the essentials" = "standard",
                                "Off - capture nothing" = "off"),
                    selected = CONFIG$metadata$level %||% "full"),
                  actionButton("adm_meta_save", "Save capture settings", class = "btn-primary"),
                  br(), br(), uiOutput("adm_meta_msg")),
                column(7,
                  tags$div(class = "muted", style = "font-size:12px",
                    HTML(paste0(
                      "<b>What each level records (PII notes):</b><br>",
                      "<b>Off</b> - nothing beyond the normal run log.<br>",
                      "<b>Standard</b> - layout signature, format, detection score/match, ",
                      "row count, trust level, KPI pass/fail counts. No per-row detail.<br>",
                      "<b>Full</b> - adds flag histograms, per-field fill ratios, candidate ",
                      "scores, per-KPI outcomes, balance anchors and net amount, OCR / ",
                      "redaction detail, and timing.<br><br>",
                      "Balances and the statement period are financial metadata (not ",
                      "personal identifiers) and never leave this machine. Descriptions, ",
                      "payees and references are <b>never</b> stored. Account numbers are ",
                      "stored only as a hash, so the same account links across runs without ",
                      "the number being readable."))))))
          )
        )
      )
      )
    )
  )
)

# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Tutorial: the step-by-step "how to build a template" guide, reachable
  # from the Add-a-template tab and from inside the toolkit itself.
  show_tutorial <- function() showModal(modalDialog(
    title = "Building a template", size = "l", easyClose = TRUE,
    tutorial_html(), footer = modalButton("Close")))
  # (Only from the tab, not from inside the toolkit modal: Shiny shows one modal
  # at a time, so opening the guide there would close the toolkit mid-edit.)
  observeEvent(input$ts_help, show_tutorial())

  # .clamp_page(v, n) -- keep a typed page number inside the document. All three
  # screens with a page box (the X-ray, the statement toolkit, the builder)
  # share it: typing a page the document doesn't have used to leave a blank panel
  # with nothing to explain it. n = NA means "we couldn't count the pages", in
  # which case the number is left exactly as typed.
  .clamp_page <- function(v, n) {
    p <- suppressWarnings(as.integer(v %||% 1L)); if (!isTRUE(is.finite(p))) p <- 1L
    max(1L, if (is.na(n)) p else min(p, as.integer(n)))
  }

  # notify_once(id, ...) -- a toast that REPLACES the last one about the same
  # thing instead of stacking under it, and clear_notice(id) takes it down the
  # moment it stops being true. Shiny keeps every notification up for its full
  # duration, so three attempts at a QID left three copies of the same sentence
  # sitting beside "Recording as AB1234" -- a screen still asking for something it
  # had already been given. One id per topic, so a message can be corrected or
  # withdrawn rather than only added to.
  notify_once <- function(id, text, type = "warning", duration = 6)
    showNotification(text, id = paste0("n_", id), type = type, duration = duration)
  clear_notice <- function(id) removeNotification(paste0("n_", id))

  # .dl_note(file, msg) -- what a download hands back when it cannot hand back
  # what was asked for: the reason, in the file. Every alternative is worse -- an
  # aborted request is an HTTP 500 error page, and an empty file is silence.
  .dl_note <- function(file, msg) writeLines(c("Statement Studio", "", msg), file)

  # dt_none_opts(msg, ...) -- DataTables options that say "nothing here" in the
  # table's OWN empty slot.
  #
  # Every table in Admin said it by rendering a one-row data.frame instead --
  # NOTE | empty, message | No feedback yet -- which DataTables then counts:
  # "Showing 1 to 1 of 1 entries" underneath, directly below a counts line reading
  # 0, and a placeholder sitting in the rows where records go. On the intake tables
  # that is a fabricated record on the one screen whose whole job is telling a
  # maintainer what is really in the folders.
  dt_none_opts <- function(msg, ...)
    c(list(language = list(emptyTable = msg, zeroRecords = msg)), list(...))

  tpl_bump <- reactiveVal(0)   # bump to force a reload after a save
  # Active set: hidden user templates are excluded, so they take no part in
  # detection / conversion / the Convert picker.
  templates <- reactive({ tpl_bump(); load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR) })
  # Management set: EVERYTHING, including hidden, so Admin can preview and un-hide.
  all_templates <- reactive({ tpl_bump(); load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR, include_hidden = TRUE) })

  # ---- THE WHOLE LIBRARY, ALL THREE KINDS ------------------------------------
  #
  # Reported: "even in the admin, other templates and other things are not even
  # seen". They were not: Admin read all_templates() and nothing else, so a form
  # or a report template - the entire other half of the app - could not be found,
  # read, checked, hidden or deleted on the one screen that manages layouts. A
  # person who had just built one was told, by an empty-looking list, that it did
  # not exist.
  #
  # So there is one library, and everything in Admin reads it. Hidden ones are
  # included for the same reason they are in all_templates(): this is the view
  # that un-hides them.
  all_field_templates_admin <- reactive({
    tpl_bump()
    safe(load_fields_templates(FIELDS_DIR, USER_FIELDS_DIR, include_hidden = TRUE), list())
  })
  all_doc_templates_admin <- reactive({
    tpl_bump()
    safe(load_document_templates(DOC_DIR, USER_DOC_DIR, include_hidden = TRUE), list())
  })
  # adm_lib() -- every template of every kind, keyed by id. Statements are added
  # LAST so that on the (pathological) case of one id used by two kinds the
  # statement wins the key, which is the same precedence the converter itself
  # applies; adm_lib_dupe_ids() then names the clash on screen rather than letting
  # a template silently disappear from the list.
  adm_lib <- reactive({
    out <- c(all_doc_templates_admin(), all_field_templates_admin(), all_templates())
    out[!duplicated(names(out), fromLast = TRUE)]
  })
  adm_lib_dupe_ids <- reactive({
    n <- c(names(all_templates()), names(all_field_templates_admin()),
           names(all_doc_templates_admin()))
    unique(n[duplicated(n)])
  })
  # .adm_kind(id) -- "statement" / "fields" / "document", read off the template
  # itself. EVERY Admin action dispatches on this, because a form template saved
  # through save_user_template() is a statement template with no columns, and a
  # report template opened in the statement toolkit is the bug that started this.
  .adm_kind <- function(id) {
    t <- adm_lib()[[as.character(id %||% "")[1] %||% ""]]
    if (is.null(t)) NA_character_ else template_kind(t)
  }
  # .ADM_KIND_DIR -- where a template of each kind is saved, and which folder
  # holds the editable copies. One table, so the five actions below cannot drift
  # apart on where a report template lives.
  .adm_user_dir <- function(kind) switch(as.character(kind)[1],
    fields = USER_FIELDS_DIR, document = USER_DOC_DIR, USER_TEMPLATES_DIR)
  .adm_save <- function(kind, t) switch(as.character(kind)[1],
    fields   = save_fields_template(t, USER_FIELDS_DIR),
    document = save_document_template(t, USER_DOC_DIR),
    save_user_template(t, USER_TEMPLATES_DIR))
  .adm_validate <- function(kind, t) switch(as.character(kind)[1],
    fields = validate_fields_template(t), document = validate_document_template(t),
    validate_template(t))
  # WHAT KIND THE TEXT IN THE BOX IS, not what kind the picker had selected. Save
  # follows the YAML: change `mode: document` to `mode: fields` in the editor and
  # it saves as a form template, because that is what it now is.
  .adm_kind_of <- function(t) if (is.null(t)) NA_character_ else template_kind(t)

  # ---- Admin password gate. Hidden outputs are suspended, so no admin data is
  # computed or sent to the browser until the password is entered. Set it in
  # config/config.yaml (app.admin_password); the BSO_ADMIN_PASSWORD env var
  # overrides it if present.
  #
  # Three rules, all fail-closed:
  #  1. NO PASSWORD SET -> Admin is refused outright. The shipped placeholder is
  #     printed in the example config and the docs, so serving template deletion,
  #     the shared dictionary and the analytics-feed settings behind it is the same
  #     as serving them behind nothing. The screen says how to set one.
  #  2. WRONG PASSWORD -> counted, and after a few tries the box goes quiet for a
  #     spell, so the one short secret can't just be walked through from a script.
  #  3. There is a way OUT. Without a sign-out, the first person to log in on a
  #     shared machine leaves Admin open for whoever sits down next.
  admin_ok <- reactiveVal(FALSE)
  output$admin_authed <- reactive(isTRUE(admin_ok()))
  outputOptions(output, "admin_authed", suspendWhenHidden = FALSE)

  # THE ADMIN TAB IS NOT FOR ANYONE USING THIS APP. Nobody converting a statement
  # has the password, so a tab they cannot open is a reference to Admin on every
  # screen and an invitation to try. It is hidden unless the URL carries ?admin -
  # the maintainer bookmarks that link (see docs/operational/admin-and-maintenance.md).
  #
  # This is NOT the security boundary and must never be mistaken for one: the
  # password is. Taking the tab away only stops it being advertised; every admin
  # action is still checked server-side with req(admin_ok()), so a hand-typed
  # ?admin gets a login form, exactly as before.
  #
  # removeTab, not hideTab: hideTab only sets display:none, which leaves the tab in
  # the live page for anyone who edits the style back.
  #
  # BUT BE CLEAR ABOUT WHAT THIS DOES NOT DO, because a maintainer reading only the
  # word "remove" will believe more than is true. This runs in the SERVER, on the
  # session's first flush, so the page Shiny serves over plain HTTP still contains
  # the whole Admin tab -- its link, its sub-tabs and every control on them -- and
  # `curl` or "View source" still reads them out (checked: the first response is
  # ~70 KB and carries adm_login, adm_dict_save, adm_ba_run and the rest). It is
  # taken away a moment later, once the browser has connected, which is also why an
  # Admin tab is briefly visible to everyone on a slow first load.
  # None of that is a way in -- the controls are inert markup, every hidden output
  # is suspended so no admin DATA is ever computed or sent, and every admin action
  # re-checks req(admin_ok()) server-side. What it means is only this: the tab is
  # unadvertised, not absent, and nothing here should ever be described as absent.
  observe({
    q <- parseQueryString(session$clientData$url_search %||% "")
    if (!("admin" %in% names(q))) removeTab("main_tabs", "Admin")
  })
  adm_fails <- reactiveVal(0L)          # consecutive wrong passwords this session
  adm_locked_until <- reactiveVal(0)    # epoch seconds; 0 = not locked
  # Backoff: free for the first 3 tries, then 5s, 10s, 20s, 40s ... capped at 5
  # minutes. Deliberately simple and per-session -- enough to make guessing a short
  # password by hand or by script pointless, with no timer, store or dependency.
  .adm_lock_seconds <- function(fails) if (fails < 3L) 0 else min(300, 5 * 2^(fails - 3L))
  .adm_lock_left <- function() max(0, ceiling(adm_locked_until() - as.numeric(Sys.time())))
  .adm_login_error <- function(msg) output$adm_login_msg <- renderUI(
    div(style = "color:var(--bad);margin-top:6px", msg))
  observeEvent(input$adm_login, {
    if (ADMIN_PW_UNSET) {            # rule 1 -- no password set, no Admin, ever
      admin_ok(FALSE)
      .adm_login_error("Admin is closed on this install: no admin password has been set.")
      return()
    }
    wait <- .adm_lock_left()
    if (wait > 0) {                  # rule 2 -- still in the backoff window
      .adm_login_error(sprintf("Too many wrong passwords. Try again in %d second%s.",
                               wait, if (wait == 1) "" else "s"))
      return()
    }
    if (identical(input$adm_pw %||% "", CONFIG$app$admin_password %||% "")) {
      adm_fails(0L); adm_locked_until(0)
      admin_ok(TRUE); output$adm_login_msg <- renderUI(NULL)
    } else {
      n <- isolate(adm_fails()) + 1L
      adm_fails(n)
      secs <- .adm_lock_seconds(n)
      if (secs > 0) adm_locked_until(as.numeric(Sys.time()) + secs)
      .adm_login_error(if (secs > 0)
        sprintf("Wrong password (%d wrong tries). Try again in %d seconds.", n, as.integer(secs))
        else "Wrong password.")
    }
  })
  # Sign out: drop the session's admin flag AND the admin data it pulled, so
  # nothing privileged is left computed behind a hidden panel.
  observeEvent(input$adm_signout, {
    req(admin_ok())
    admin_ok(FALSE); adm_data(NULL)
    updateTextInput(session, "adm_pw", value = "")
    output$adm_login_msg <- renderUI(div(class = "muted", style = "margin-top:6px",
                                         "Signed out of Admin."))
  })
  # The sign-in panel -- or, when no password is set, the plain instructions for
  # setting one instead of a box that can never open.
  output$adm_login_panel <- renderUI({
    if (ADMIN_PW_UNSET) return(wellPanel(style = "max-width:640px",
      h4("Admin is closed - no admin password has been set"),
      p("Admin manages templates, the shared label dictionary and the analytics feed, so it stays shut until this install has its own password. The one in the example settings file is printed in the documentation, so it is not a password."),
      p(HTML(paste0("To open it, do <b>either</b> of these on the server and restart the app:",
        "<ul><li>put <code>admin_password: your-password</code> under <code>app:</code> in ",
        "<code>config/config.yaml</code> (copy <code>config/config.example.yaml</code> if it isn't there yet), or</li>",
        "<li>set the <code>BSO_ADMIN_PASSWORD</code> environment variable.</li></ul>"))),
      p(class = "muted", "Everything on the Convert and Add-a-template tabs works normally without this.")))
    wellPanel(style = "max-width:440px",
      h4("Admin - password required"),
      passwordInput("adm_pw", "Password"),
      actionButton("adm_login", "Enter", class = "btn-primary"),
      uiOutput("adm_login_msg"))
  })
  # Not suspended: it is a small static panel, and it must already be on screen the
  # moment the Admin tab is opened (it is the only thing that tells an operator the
  # tab is closed because no password has been set).
  outputOptions(output, "adm_login_panel", suspendWhenHidden = FALSE)
  # The settings-file banner. The parse detail can name a line of the file, so only
  # a signed-in admin sees it; everyone else is told plainly that something is wrong
  # and who to tell.
  output$adm_cfg_banner <- renderUI({
    if (is.null(CONFIG_ERROR)) return(NULL)
    div(style = "margin:0 0 10px;padding:10px 12px;border:1px solid var(--warn-line);background:var(--warn-bg);color:var(--warn-ink);border-radius:8px",
      strong("This install's settings file could not be read."),
      p(style = "margin:6px 0 0",
        "Statement Studio is running on built-in defaults, which include the placeholder admin password and the default analytics-feed folder. Anything set in config/config.yaml - the feed folder, the paths, the upload limit - is NOT in force. Tell whoever looks after the server."),
      if (isTRUE(admin_ok())) div(class = "mono", style = "margin-top:6px;font-size:12px", CONFIG_ERROR))
  })
  outputOptions(output, "adm_cfg_banner", suspendWhenHidden = FALSE)

  # The Convert picker offers PROVEN (curated) templates by default. Whether the
  # ones users built here join them is the deployment setting
  # `app.user_templates_default` (USE_USER_TEMPLATES), NOT a tick-box on the page --
  # see the note at the top of this file for why that question is not the
  # converting analyst's to answer. This comment used to describe the tick-box as
  # though it were still there, which is how a reader learns to distrust comments.
  proven_templates <- reactive({ tpl_bump()
    tryCatch(load_templates(TEMPLATES_DIR, strict = FALSE), error = function(e) list()) })
  # NO `user_template_ids` REACTIVE HERE. There was one -- a leftover from the
  # deleted "include templates built here" tick-box, unread by anything -- and it
  # SHADOWED the engine function of the same name (R/templates.R). The three Admin
  # call sites below pass it a directory, so every one of them raised
  # "unused argument (USER_TEMPLATES_DIR)": the origin line printed that error
  # instead of saying whether the selected template is shipped or yours, and Hide
  # and Delete did nothing at all, silently. The reactive also excluded HIDDEN
  # templates, which is the wrong set for a button whose whole job is un-hiding
  # one. Gone, so those three calls reach the engine function they were written for.
  cv_pick_templates <- reactive({
    if (USE_USER_TEMPLATES) templates() else proven_templates()
  })
  # Keep THE bank picker's list in step with the templates actually loaded. One
  # list-builder, because there is now one picker: this observer used to have a
  # twin immediately below it, building the same list of banks off the same
  # reactive for a second, hidden bank control.
  observe({
    ts <- cv_pick_templates()
    b <- sort(unique(vapply(ts, function(t) t$bank %||% "", character(1))))
    b <- b[nzchar(b)]
    updateSelectInput(session, "cv_bank_quick",
                      choices = c("Detect automatically" = "", stats::setNames(b, b)),
                      selected = isolate(input$cv_bank_quick) %||% "")
  })

  # THE EXACT-TEMPLATE LIST FOLLOWS THE BANK. Choosing a bank and then scrolling a
  # hundred other banks' templates is the tool refusing to use what it already
  # knows -- the same rule as "never ask a question the tool can answer", applied
  # to a list. The bank picker narrows it (through bank_choice(), the same reading
  # the conversion does), and "Detect automatically" shows everything.
  # A template already picked survives a bank change when it still belongs to that
  # bank; otherwise it clears, because leaving a hidden, out-of-scope template
  # selected is how a statement gets read by a template nobody chose on purpose.
  #
  # ...AND IT FOLLOWS "WHAT IS THIS?" FIRST. The list only ever held bank
  # templates, and the panel holding it was hidden the moment somebody answered
  # "something else" -- so a form or a report template could not be forced from
  # anywhere in the product, though both engine calls have taken one all along.
  # Say "something else" and the list becomes the form and report templates; say
  # anything else and it is the bank templates it always was.
  #
  # THIS IS ALSO WHERE THE HIDDEN CONTRADICTION DIES. Hiding a control does not
  # clear it: she opened the panel, picked a bank template, then ticked
  # "Something else", and the selection survived underneath and beat the answer
  # she could see. Rebuilding the list on the kind means the statement template
  # is no longer among the choices, so it cannot stay selected -- and the bank
  # picker is cleared beside it, for the same reason and in the same breath.
  observe({
    other <- identical(kind_choice(), "other")
    ch <- c("(auto-detect)" = "")
    if (other) {
      # Every template that reads something which is NOT a statement, named the
      # way the rest of the screen names one -- never a raw id, and never a
      # library the person has to know the name of.
      ts <- c(all_doc_templates(), all_field_templates())
      ids <- sort(names(ts))
      if (length(ids)) ch <- c(ch, tpl_choices(ids))
    } else {
      ts <- cv_pick_templates()
      bank <- bank_choice()
      ov <- template_overview(ts)
      if (!is.null(bank) && nrow(ov)) ov <- ov[ov$bank %in% bank, , drop = FALSE]
      # Labelled "Bank (middot) type - id" so you can force an EXACT audited
      # template, not just a bank, when you need to be specific.
      if (nrow(ov)) ch <- c(ch, stats::setNames(ov$id, sprintf("%s \u00b7 %s - %s", ov$bank, ov$type, ov$id)))
    }
    keep <- isolate(input$cv_template) %||% ""
    updateSelectInput(session, "cv_template", choices = ch,
                      selected = if (keep %in% ch) keep else "")
  })
  # A BANK PICKED, THEN "SOMETHING ELSE": the bank goes too. It is hidden at that
  # point, and a hidden control that still decides which templates are tried is
  # the same fault as the one above, one control along.
  observeEvent(kind_choice(), {
    if (identical(kind_choice(), "other"))
      updateSelectInput(session, "cv_bank_quick", selected = "")
  }, ignoreInit = TRUE)


  # ---- Admin: template overview / preview / edit ----
  # The management view shows ALL templates, hidden ones included, so a parked
  # draft can be found and un-hidden.
  # ...and ALL THREE KINDS, with `kind` in front. See adm_lib() for why.
  adm_ov <- reactive(library_overview(all_templates(), all_field_templates_admin(),
                                      all_doc_templates_admin()))

  # A REFUSED TEMPLATE IS NAMED, NOT LOST. All three loaders already gather the
  # reason each skipped file was skipped on attr(x, "load_errors") -- and the app
  # read it nowhere, so a template that stopped validating after an update simply
  # disappeared from the library and every document it used to read came back
  # "unsupported" with nothing on any screen saying the file was still there.
  # Same wording as scripts/health-check.R, deliberately, so the operator running
  # the check after an update and the admin reading this screen see one sentence.
  output$adm_tpl_load_errors <- renderUI({
    req(admin_ok())
    why <- c(as.character(attr(all_templates(), "load_errors") %||% character(0)),
             as.character(attr(all_field_templates_admin(), "load_errors") %||% character(0)),
             as.character(attr(all_doc_templates_admin(), "load_errors") %||% character(0)))
    why <- why[nzchar(trimws(why))]
    if (!length(why)) return(NULL)
    div(class = "note-bad", style = "margin-bottom:10px",
      p(strong(sprintf("%d template file%s could not be loaded, so %s not in use.",
                       length(why), if (length(why) == 1L) "" else "s",
                       if (length(why) == 1L) "it is" else "they are"))),
      tags$ul(lapply(why, function(w) tags$li(w))),
      p(class = "muted", style = "font-size:12px",
        "Fix the file in the templates folder, or open it here and save it again."))
  })

  # C1: TWO HEADED BANDS, "Bank statement" and "Other", both on screen at once.
  # The route is a real leading column, HIDDEN, with DT's RowGroup drawing it as
  # the band heading -- so the split costs no second table, no second observer and
  # no control, and (proven in a browser) rows_selected is still the data-frame
  # row index, which is what the row-click handler below reads.
  .adm_rowgroup <- function(df, page = 25L, none = "Nothing here yet.") {
    datatable(df, rownames = FALSE, selection = "single",
              extensions = "RowGroup",
              options = dt_none_opts(none,
                pageLength = page, dom = "tip", scrollX = TRUE,
                rowGroup = list(dataSrc = 0L),
                orderFixed = list(list(0L, "asc")),
                columnDefs = list(list(visible = FALSE, targets = 0L))))
  }
  output$adm_tpl_overview <- renderDT({
    ov <- adm_ov()
    ov <- cbind(route = vapply(ov$kind, function(k)
      if (identical(k, unname(.TEMPLATE_KIND_LABEL[["statement"]]))) .ADM_ROUTES[1]
      else .ADM_ROUTES[2], character(1), USE.NAMES = FALSE), ov)
    .adm_rowgroup(ov, page = 25L, none = "No templates are installed.")
  })

  # C2: THE FEEDBACK, BESIDE THE TEMPLATES, SPLIT THE SAME WAY. One row per
  # rating, carrying the document it was left on and the template that read it --
  # neither of which the feedback record holds, so both are joined on here (see
  # .adm_feedback_overview). It replaces the old "flagged as wrong" table, which
  # showed a subset of the ratings, none of their documents, and sat on a
  # different tab from the templates it was about.
  adm_fb_ov <- reactive({
    d <- adm_data()
    .adm_feedback_overview(d$fb, d$runs, adm_lib())
  })
  output$adm_tpl_feedback <- renderDT({
    req(adm_data())
    .adm_rowgroup(adm_fb_ov(), page = 8L,
                  none = "Nobody has rated a conversion yet.")
  })
  # TRACEABLE MEANS A ROUTE TO THE TEMPLATE, not a printed id: click the rating
  # and the template it is about is selected in the picker below it.
  observeEvent(input$adm_tpl_feedback_rows_selected, {
    req(admin_ok())
    fb <- adm_fb_ov(); i <- input$adm_tpl_feedback_rows_selected
    if (!length(i) || i > nrow(fb)) return()
    id <- as.character(fb$template_id[i])
    if (!id %in% names(adm_lib())) {
      output$adm_tpl_msg <- renderUI(span(class = "muted",
        "That conversion's template is no longer in the library, so there is nothing to open."))
      return()
    }
    updateSelectInput(session, "adm_tpl_pick", selected = id)
  })

  # Rebuilding the choices used to DROP the selection: selectize falls back to the
  # first option, the picker's own observer below then blanks adm_tpl_msg, and the
  # confirmation for whatever you just did went with it. Delete worked around that
  # with a toast; Hide and Save did not, so they completed in silence. The worse
  # half was not the missing message: the picker had silently moved, so a SECOND
  # click acted on a template nobody chose -- measured, "Only USER templates can be
  # hidden" about a shipped template the operator never picked. Keep the selection
  # whenever it still exists (a deleted one cannot, and falling back is right there).
  observe({
    req(admin_ok())      # reads an admin input now, so it re-verifies like the rest
    lib <- adm_lib()
    ids <- names(lib)
    # LABELLED BY KIND. Twelve bare ids in a dropdown do not say which of them is
    # the report template you just built, and "type to search" cannot help you
    # find a word that is not there. Grouped so the three kinds stay apart, and
    # each option reads "<kind> - <id>".
    kinds <- vapply(lib, template_kind, character(1))
    ord <- order(match(kinds, names(.TEMPLATE_KIND_LABEL)), ids)
    ids <- ids[ord]; kinds <- kinds[ord]
    ch <- stats::setNames(ids, sprintf("%s \u2013 %s",
                                       unname(.TEMPLATE_KIND_LABEL[kinds]), ids))
    keep <- isolate(input$adm_tpl_pick)
    updateSelectInput(session, "adm_tpl_pick", choices = ch,
                      selected = if (!is.null(keep) && keep %in% ids) keep else NULL)
  })

  # clicking a row selects it in the picker
  # Server-side admin gate. The Admin tab's controls are hidden until login, but
  # a crafted client message can still fire any input, so EVERY privileged handler
  # re-verifies the admin session here and fail-closes (silent no-op) without one.
  observeEvent(input$adm_tpl_overview_rows_selected, {
    req(admin_ok())
    ov <- adm_ov()
    i <- input$adm_tpl_overview_rows_selected
    if (length(i) && i <= nrow(ov)) updateSelectInput(session, "adm_tpl_pick", selected = ov$id[i])
  })

  observeEvent(input$adm_tpl_pick, {
    req(admin_ok())
    t <- adm_lib()[[input$adm_tpl_pick]]; req(t)
    updateTextAreaInput(session, "adm_tpl_edit", value = template_yaml(t))
    output$adm_tpl_msg <- renderUI(NULL)
    # WHAT THIS BOX WAS FILLED FROM. Save compares it against the folder before
    # overwriting, so two admins on one server cannot silently overwrite each
    # other -- see the H9 block above the Save handler.
    adm_tpl_opened(list(id = as.character(t$id %||% "")[1],
                        sha = safe(template_sha256(t), NA_character_)))
    adm_tpl_forced("")
  })

  # Show whether the selected template is a read-only shipped one or a deletable
  # user one, so the analyst knows what Delete will do.
  output$adm_tpl_origin <- renderUI({
    id <- input$adm_tpl_pick; if (is.null(id) || !nzchar(id)) return(NULL)
    t <- adm_lib()[[id]]; if (is.null(t)) return(NULL)
    kind <- template_kind(t)
    is_user <- id %in% user_template_ids(.adm_user_dir(kind))
    hidden <- isTRUE(t$hidden)
    tagList(
      # WHICH KIND, FIRST AND IN BOLD. Every button under this line behaves
      # differently depending on it, and the answer to "why did Redraw open the
      # bank statement editor" was that nothing on the screen ever said what had
      # been selected.
      div(style = "margin:2px 0 4px",
        strong(unname(.TEMPLATE_KIND_LABEL[[kind]])),
        span(class = "muted", sprintf(" \u00b7 %s", .template_reads(t)))),
      span(class = "muted",
        if (is_user) "This is a USER template (yours) - editable, hideable & deletable."
        else "This is a shipped 'tested' template - read-only (Save makes a user copy)."),
      if (hidden) tagList(br(), span(class = "bad",
        "Hidden - it is NOT used for detection. Un-hide to bring it back.")))
  })
  # Hide / un-hide a USER template: parks it out of detection without deleting.
  observeEvent(input$adm_tpl_hide, {
    req(admin_ok())
    id <- input$adm_tpl_pick
    if (is.null(id) || !nzchar(id)) return()
    # THE USER FOLDER FOR THIS KIND. set_user_template_hidden() only ever reads and
    # rewrites YAML files in the folder it is given, so it hides a form or a report
    # template exactly as it hides a statement one -- it just has to be pointed at
    # the right folder. Pointed at the statement folder (as it always was), Hide
    # answered "only USER templates can be hidden" about a template the person had
    # built here five minutes earlier.
    dir <- .adm_user_dir(.adm_kind(id))
    if (!(id %in% user_template_ids(dir))) {
      output$adm_tpl_msg <- .tpl_note("Only USER templates can be hidden; this one is shipped/read-only.", ok = FALSE)
      return()
    }
    now_hidden <- isTRUE(adm_lib()[[id]]$hidden)
    res <- safe(set_user_template_hidden(id, !now_hidden, dir), NULL)
    if (is.null(res)) { output$adm_tpl_msg <- .tpl_note("Couldn't change it.", ok = FALSE); return() }
    tpl_bump(isolate(tpl_bump()) + 1)
    nm <- template_library_name(adm_lib()[[id]]) %||% id
    output$adm_tpl_msg <- .tpl_note(if (isTRUE(res))
      sprintf("Hid <b>%s</b> - it won't be used for detection until you un-hide it.", id)
      else sprintf("Un-hid <b>%s</b> - it's active again.", id))
    # Also as a toast, for the same reason Delete has one: this changes whether a
    # template is used at all, and a state change that reports itself only in a
    # panel that a redraw can wipe is a state change nobody can be sure happened.
    notify_once("adm_tpl_hide",
                if (isTRUE(res))
                  sprintf("Hid %s. It stays on disk and will not be used to read statements until you un-hide it.", nm)
                else sprintf("Un-hid %s. It is back in use for reading statements.", nm),
                type = "message", duration = 8)
  })
  # Near-duplicate user templates, grouped by identical layout, so a heap of
  # variants can be consolidated (keep one, hide/delete the rest via the controls
  # above). Uses the management set so hidden variants show up too.
  output$adm_tpl_dupes <- renderUI({
    # ONE ID, TWO KINDS. Nothing stops a form template and a report template being
    # given the same id -- they are validated by different loaders reading
    # different folders -- and the library keys by id, so the second one would
    # simply not appear in the list above. Named here rather than left to be
    # discovered as "my template vanished".
    clash <- adm_lib_dupe_ids()
    clash_ui <- if (length(clash)) tags$div(
      style = "margin:6px 0;padding:6px 10px;border-left:3px solid var(--bad);background:var(--bad-bg)",
      strong("The same id is used by more than one kind of template: "),
      paste(clash, collapse = ", "),
      p(style = "margin:4px 0 0", "Only one of them can appear in the list above. Give one a different id."))
    else NULL
    groups <- duplicate_template_groups(all_templates())
    if (!length(groups))
      return(tagList(clash_ui,
        helpText("No duplicate user templates - nothing to consolidate.")))
    ov <- template_overview(all_templates())
    tagList(clash_ui, do.call(tagList, lapply(seq_along(groups), function(gi) {
      ids <- groups[[gi]]
      rows <- ov[ov$id %in% ids, , drop = FALSE]
      lab <- sprintf("%s \u00b7 %s", rows$bank[1] %||% "?", rows$format[1] %||% "?")
      tags$div(style = "margin:6px 0;padding:6px 10px;border-left:3px solid var(--warn);background:var(--warn-bg)",
        strong(sprintf("Same layout (%d): %s", length(ids), lab)),
        tags$ul(lapply(seq_len(nrow(rows)), function(i) tags$li(
          sprintf("%s%s", rows$id[i], if (nzchar(rows$hidden[i])) " (hidden)" else "")))))
    })))
  })
  # Delete a USER template (never a shipped one), then refresh the picker.
  #
  # ASKED FIRST. This and the uploads purge are the only irreversible actions in
  # the product, and both fired on ONE click of an enabled red button: no modal,
  # no typed confirmation, nothing to undo them with. The template is a file
  # somebody built by hand and there is no copy of it anywhere else, so the
  # question names the file it is about to remove and what stops working.
  observeEvent(input$adm_tpl_delete, {
    req(admin_ok())
    id <- input$adm_tpl_pick
    if (is.null(id) || !nzchar(id)) return()
    dir <- .adm_user_dir(.adm_kind(id))
    if (!(id %in% user_template_ids(dir))) {
      output$adm_tpl_msg <- .tpl_note("Only USER templates can be deleted; this one is shipped/read-only.", ok = FALSE)
      return()
    }
    files <- Filter(function(f) {
      t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
      identical(t$id %||% "", id) ||
        identical(tools::file_path_sans_ext(basename(f)), gsub("[^A-Za-z0-9_]+", "_", id))
    }, list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE))
    showModal(modalDialog(
      title = "Delete this template?", size = "m", easyClose = FALSE,
      p(sprintf("This permanently deletes %d file(s) from %s:",
                length(files), dir)),
      tags$ul(lapply(files, function(f) tags$li(tags$code(basename(f))))),
      p(strong("There is no undo and no backup."),
        " Statements this template was reading will stop being recognised by it from the next conversion; conversions already run are unaffected."),
      if (!length(files)) p(class = "bad", "No file matches that id - nothing would be deleted."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("adm_tpl_delete_confirm", sprintf("Delete %s", id), class = "btn-danger"))))
  })
  observeEvent(input$adm_tpl_delete_confirm, {
    req(admin_ok())
    removeModal()
    id <- input$adm_tpl_pick
    if (is.null(id) || !nzchar(id)) return()
    dir <- .adm_user_dir(.adm_kind(id))
    if (!(id %in% user_template_ids(dir))) {
      output$adm_tpl_msg <- .tpl_note("Only USER templates can be deleted; this one is shipped/read-only.", ok = FALSE)
      return()
    }
    ok <- safe(delete_user_template(id, dir), FALSE)
    if (isTRUE(ok)) {
      tpl_bump(isolate(tpl_bump()) + 1)
      output$adm_tpl_msg <- .tpl_note(sprintf("Deleted user template <b>%s</b>.", id))
      # ...and as a toast, because the line above does not survive its own
      # success: deleting rebuilds the template picker, and the picker's own
      # observer blanks adm_tpl_msg. So the only irreversible action in the tab
      # reported itself for a fraction of a second and then looked like nothing.
      notify_once("adm_tpl_delete", sprintf("Deleted the user template %s. It is gone from disk.", id),
                  type = "message", duration = 8)
    } else {
      output$adm_tpl_msg <- .tpl_note("Couldn't delete it.", ok = FALSE)
      notify_once("adm_tpl_delete",
                  sprintf("Could not delete %s - check folder permissions on %s.", id, dir),
                  type = "error", duration = 10)
    }
  })

  # Duplicate the selected template with a fresh id, into the editor to tweak+save.
  # The new id must be free across the WHOLE library, not just the statement half,
  # or a duplicated report template lands on an id a form template already holds.
  observeEvent(input$adm_tpl_dup, {
    req(admin_ok())
    t <- tryCatch(yaml::yaml.load(input$adm_tpl_edit %||% ""), error = function(e) NULL)
    if (is.null(t) || is.null(t$id)) t <- adm_lib()[[input$adm_tpl_pick]]
    req(t)
    ids <- names(adm_lib())
    new_id <- paste0(t$id, "_copy"); k <- 2L
    while (new_id %in% ids) { new_id <- paste0(t$id, "_copy", k); k <- k + 1L }
    t$id <- new_id; t$origin <- NULL
    updateTextAreaInput(session, "adm_tpl_edit", value = yaml::as.yaml(t))
    output$adm_tpl_msg <- .tpl_note(sprintf("Duplicated as <b>%s</b> - edit it and click Save.", new_id))
  })

  .tpl_from_editor <- function() tryCatch(yaml::yaml.load(input$adm_tpl_edit), error = function(e) NULL)
  .tpl_note <- function(html, ok = TRUE)
    renderUI(div(style = sprintf("color:%s;font-size:12px", if (ok) PALETTE$ok else PALETTE$bad), HTML(html)))

  # CHECKED AGAINST ITS OWN RULES. validate_template() is the STATEMENT rulebook:
  # run over a report template it reported a pile of missing columns and no date
  # format, none of which a report has -- so the one button that says whether a
  # template is right answered nonsense for two of the three kinds.
  observeEvent(input$adm_tpl_validate, {
    req(admin_ok())
    t <- .tpl_from_editor()
    if (is.null(t)) { output$adm_tpl_msg <- .tpl_note("That is not valid YAML.", FALSE); return() }
    kind <- .adm_kind_of(t)
    probs <- safe(.adm_validate(kind, t), "could not be checked")
    output$adm_tpl_msg <- if (!length(probs))
        .tpl_note(sprintf("Valid \u2713 <span class='muted'>(checked as a %s)</span>",
                          unname(.TEMPLATE_KIND_NOUN[[kind]])))
      else .tpl_note(paste("Problems:<br>", paste(probs, collapse = "<br>")), FALSE)
  })

  # ---- H9: TWO ADMINS, ONE SERVER. Last save used to win, blindly ------------
  #
  # There is no lock, no version compare and no author stamp anywhere in this
  # product, so two maintainers editing one template on one server meant whoever
  # pressed Save second silently threw the other's work away -- on a forty-table
  # template that is months of somebody's work gone with no message.
  #
  # A lock is the wrong answer here (it needs an identity the app does not have,
  # and a stale lock on a shared box blocks the very person who can clear it). The
  # right answer is the one the builder already uses for Remove: notice, SAY it,
  # and make the second press mean it. So Save compares what is on disk NOW with
  # what was loaded into the box, and if it has moved it refuses ONCE, names when
  # it changed, and lets the next press through. Nothing new on the screen.
  adm_tpl_opened <- reactiveVal(NULL)   # list(id, sha) -- what was put in the box
  adm_tpl_forced <- reactiveVal("")     # the id whose overwrite has been agreed
  # The template as the FOLDER holds it this second, not as the app cached it.
  .adm_on_disk <- function(kind, id) {
    id <- as.character(id %||% "")[1]
    if (!nzchar(id)) return(NULL)
    ts <- switch(as.character(kind)[1],
      fields   = safe(load_fields_templates(USER_FIELDS_DIR, NULL, include_hidden = TRUE), list()),
      document = safe(load_document_templates(USER_DOC_DIR, NULL, include_hidden = TRUE), list()),
      safe(load_templates(USER_TEMPLATES_DIR, origin = "user", strict = FALSE), list()))
    ts[[id]]
  }
  .adm_when_changed <- function(kind, id) {
    d <- .adm_user_dir(kind)
    fs <- list.files(d, pattern = "\\.ya?ml$", full.names = TRUE)
    hit <- fs[vapply(fs, function(f)
      identical(as.character(safe(yaml::read_yaml(f)$id, "")[1]), as.character(id)[1]),
      logical(1))]
    if (!length(hit)) return(NA_character_)
    safe(local_time_text(utc_stamp(file.mtime(hit[1]))), NA_character_)
  }

  # SAVED WHERE ITS OWN KIND LIVES. Everything here used to go through
  # save_user_template() into the statement folder -- so editing a report template
  # in Admin and pressing Save either refused it outright (it has no columns) or
  # wrote a report template into templates/statements_user/, where no loader would
  # ever look at it again. The kind is read off the YAML in the box, so changing
  # `mode:` in the editor moves the file to the right folder on the next save.
  observeEvent(input$adm_tpl_save, {
    req(admin_ok())
    t <- .tpl_from_editor()
    if (is.null(t)) { output$adm_tpl_msg <- .tpl_note("That is not valid YAML.", FALSE); return() }
    kind <- .adm_kind_of(t)
    tid <- as.character(t$id %||% "")[1]
    opened <- adm_tpl_opened()
    now <- safe(template_sha256(.adm_on_disk(kind, tid)), NA_character_)
    moved <- !is.null(opened) && identical(opened$id, tid) &&
      !is.na(now) && !is.na(opened$sha %||% NA_character_) && !identical(now, opened$sha)
    if (moved && !identical(adm_tpl_forced(), tid)) {
      adm_tpl_forced(tid)
      when <- .adm_when_changed(kind, tid)
      output$adm_tpl_msg <- .tpl_note(sprintf(
        "Somebody else on this server saved this template%s - open it again to see theirs, or press Save once more to replace it.",
        if (is.na(when)) "" else paste(" at", when)), FALSE)
      return()
    }
    adm_tpl_forced("")
    path <- tryCatch(.adm_save(kind, t), error = function(e) conditionMessage(e))
    if (is.character(path) && file.exists(path)) {
      tpl_bump(tpl_bump() + 1)
      # What is in the box IS what is on disk again, so the next Save has nothing
      # to warn about until somebody else moves it.
      adm_tpl_opened(list(id = tid, sha = safe(template_sha256(t), NA_character_)))
      # Shadowing is a statement-template rule (a shipped id wins over a user one);
      # the other two loaders take the first file they find in folder order, which
      # is the curated folder, so the same warning is true for them.
      shipped <- switch(kind,
        fields   = safe(load_fields_templates(FIELDS_DIR, NULL, include_hidden = TRUE), list()),
        document = safe(load_document_templates(DOC_DIR, NULL, include_hidden = TRUE), list()),
        safe(load_templates(TEMPLATES_DIR), list()))
      shadowed <- !is.null(shipped[[t$id %||% ""]])
      msg <- sprintf("Saved to %s <span class='muted'>(as a %s)</span>.", path,
                     unname(.TEMPLATE_KIND_NOUN[[kind]]))
      if (shadowed) msg <- paste0(msg, "<br><b>Note:</b> a shipped 'tested' template with id '",
        t$id, "' takes precedence - rename the id for your edit to apply.")
      output$adm_tpl_msg <- .tpl_note(msg, !shadowed)
    } else output$adm_tpl_msg <- .tpl_note(paste("Could not save:", path), FALSE)
  })

  # The two YAML editors (dictionary, vocabulary) refuse and fail in exactly the
  # same two ways, and each had written out its own copy of both sentences. One
  # copy, so a reword of either can never leave the other behind.
  YAML_BAD_MSG   <- "Not valid YAML - not saved."
  YAML_WRITE_MSG <- "Could not write the file - check folder permissions."

  # ---- Admin: label dictionary edit (the fix for "check shows NA") ----
  # Two controls, ONE write path each way round: the plain-English "teach it this
  # wording" goes through dictionary_append() (R/labels.R), the whole-file editor
  # writes the text the admin typed. Both back the file up first.
  .load_dict_text <- function() read_file_text(DICT_PATH)
  dict_bump <- reactiveVal(0)
  # The values the dictionary already knows, offered by name so an admin never has
  # to invent a key. Read from the FILE, so the list can never drift from it.
  output$adm_dict_field_ui <- renderUI({
    req(admin_ok()); dict_bump()
    keys <- sort(names(safe(load_label_dict(DICT_PATH), list())))
    if (!length(keys)) return(helpText("No label dictionary file was found on this install."))
    selectInput("adm_dict_field", "This wording means\u2026",
                stats::setNames(keys, tools::toTitleCase(gsub("_", " ", keys))))
  })
  observeEvent(input$adm_dict_add, {
    req(admin_ok())
    fld <- input$adm_dict_field %||% ""
    out <- dictionary_append(fld, input$adm_dict_phrase %||% "", path = DICT_PATH)
    if (isTRUE(out)) {
      updateTextAreaInput(session, "adm_dict_edit", value = .load_dict_text())
      dict_bump(isolate(dict_bump()) + 1)
      if (isTRUE(attr(out, "added"))) updateTextInput(session, "adm_dict_phrase", value = "")
    }
    output$adm_dict_add_msg <- renderUI(span(class = if (isTRUE(out)) "ok" else "bad",
      # The engine's own sentence, shown as it comes back -- so the screen can
      # never describe a refusal differently from the reason it was refused for.
      if (isTRUE(out) && isTRUE(attr(out, "added")))
        sprintf("Added. From the next conversion, \"%s\" is read as %s.",
                trimws(input$adm_dict_phrase %||% ""), gsub("_", " ", fld))
      else attr(out, "reason") %||% "Could not add that wording."))
  })
  observeEvent(admin_ok(), if (isTRUE(admin_ok()))
    updateTextAreaInput(session, "adm_dict_edit", value = .load_dict_text()))
  observeEvent(input$adm_dict_reload, { req(admin_ok())
    updateTextAreaInput(session, "adm_dict_edit", value = .load_dict_text())
    output$adm_dict_msg <- renderUI(span(class = "ok",
      "Reloaded from the file - any unsaved edits in the box are gone.")) })
  observeEvent(input$adm_dict_save, {
    req(admin_ok())
    txt <- input$adm_dict_edit %||% ""
    if (!isTRUE(tryCatch({ yaml::yaml.load(txt); TRUE }, error = function(e) FALSE))) {
      output$adm_dict_msg <- renderUI(div(style = "color:var(--bad)", YAML_BAD_MSG))
      return()
    }
    safe(file.copy(DICT_PATH, paste0(DICT_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({ writeLines(txt, DICT_PATH); TRUE }, error = function(e) FALSE))
    output$adm_dict_msg <- renderUI(div(style = sprintf("color:%s", if (okw) PALETTE$ok else PALETTE$bad),
      if (okw) "Saved, with a backup kept - new wordings apply from the next conversion."
      else YAML_WRITE_MSG))
  })

  # ---- Admin: local metadata-capture settings (level + category switches) ----
  # ONE QUESTION, NOT TWO. There used to be a nine-box "categories to capture"
  # list beside the level, and the level already answered it: "Full - everything"
  # and a tick-list of everything are the same fact said twice, and the pair could
  # be left contradicting each other (Full, with six categories switched off) with
  # nothing on screen resolving it. The level decides; every category is captured
  # within it, which is exactly what the built-in default has always been.
  .ADM_META_CATS <- c("layout", "parse_quality", "detection", "reconciliation",
                      "multi_statement", "novelty", "template_hints", "ocr", "redaction")
  observeEvent(input$adm_meta_save, {
    req(admin_ok())
    lvl <- input$adm_meta_level %||% "full"
    capture <- stats::setNames(as.list(rep(TRUE, length(.ADM_META_CATS))), .ADM_META_CATS)
    okw <- save_metadata_config(lvl, capture)
    if (okw) { CONFIG$metadata$level <<- lvl; CONFIG$metadata$capture <<- capture }
    # A refusal carries its REASON (e.g. "the file is there but doesn't parse, so
    # writing this toggle would wipe every other setting in it"). Show it -- a bare
    # "check folder permissions" would send the admin hunting the wrong problem.
    why <- attr(okw, "reason")
    output$adm_meta_msg <- renderUI(div(style = sprintf("color:%s", if (okw) PALETTE$ok else PALETTE$bad),
      if (okw) "Saved - it applies from the next conversion."
      else if (!is.null(why)) paste("Not saved:", why)
      else "Could not write config/config.yaml - check folder permissions."))
  })

  # ---- Admin: recognition-vocabulary (lexicon) editor ----
  .load_lex_text <- function() read_file_text(LEXICON_PATH)
  observeEvent(admin_ok(), if (isTRUE(admin_ok()))
    updateTextAreaInput(session, "adm_lex_edit", value = .load_lex_text()))
  # Both REPLACE the whole editor, and both did it in silence -- including the one
  # that overwrites unsaved work with a completely different document. What just
  # happened to the box, and whether anything was written to disk (nothing was),
  # is said out loud.
  observeEvent(input$adm_lex_reload, { req(admin_ok())
    updateTextAreaInput(session, "adm_lex_edit", value = .load_lex_text())
    output$adm_lex_msg <- renderUI(span(class = "ok",
      "Reloaded from the file - any unsaved edits in the box are gone.")) })
  observeEvent(input$adm_lex_defaults, { req(admin_ok())
    updateTextAreaInput(session, "adm_lex_edit", value = safe(lexicon_defaults_yaml(), ""))
    output$adm_lex_msg <- renderUI(span(class = "ok",
      "The box now holds the built-in defaults, not your file - saving it as it stands would replace your file with them.")) })
  observeEvent(input$adm_lex_save, {
    req(admin_ok())
    txt <- input$adm_lex_edit %||% ""
    parsed <- tryCatch(yaml::yaml.load(txt), error = function(e) e)
    if (inherits(parsed, "error")) {
      output$adm_lex_msg <- renderUI(div(style = "color:var(--bad)", YAML_BAD_MSG)); return()
    }
    probs <- validate_lexicon(parsed)
    if (length(probs)) {
      output$adm_lex_msg <- renderUI(div(style = "color:var(--bad)",
        HTML(paste("Not saved -", paste(probs, collapse = "; "))))); return()
    }
    safe(file.copy(LEXICON_PATH, paste0(LEXICON_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({
      dir.create(dirname(LEXICON_PATH), recursive = TRUE, showWarnings = FALSE)
      writeLines(txt, LEXICON_PATH); TRUE }, error = function(e) FALSE))
    if (okw) clear_lexicon_cache()   # the next conversion re-reads the vocabulary
    output$adm_lex_msg <- renderUI(div(style = sprintf("color:%s", if (okw) PALETTE$ok else PALETTE$bad),
      if (okw) "Saved, with a backup kept - it applies from the next conversion, everywhere."
      else YAML_WRITE_MSG))
  })

  # ---- Admin: teaching the engine a word -- typed in, or approved from what the
  # conversions actually met. Both write through the SAME engine function
  # (lexicon_append: union into the file, back it up, clear the cache), so there is
  # exactly one way a word gets into the vocabulary whichever control put it there.
  sugg_bump <- reactiveVal(0)
  # Teach it ONE word, in plain English. The common case ("this bank writes a word
  # we've never met") should not need YAML, and now it doesn't.
  observeEvent(input$adm_word_add, {
    req(admin_ok())
    w <- trimws(input$adm_word_text %||% "")
    if (!nzchar(w)) {
      output$adm_word_msg <- renderUI(span(class = "bad",
        "Type the word first - exactly as the statement prints it.")); return() }
    kind <- input$adm_word_kind %||% "debit_markers"
    out <- lexicon_append(kind, tolower(w), LEXICON_PATH)
    updateTextAreaInput(session, "adm_lex_edit", value = read_file_text(LEXICON_PATH))
    sugg_bump(isolate(sugg_bump()) + 1)
    ok <- isTRUE(out)
    if (ok) updateTextInput(session, "adm_word_text", value = "")
    output$adm_word_msg <- renderUI(span(class = if (ok) "ok" else "bad",
      # A refusal is reported in the engine's own words, so the screen and the
      # reason it was refused for can never say different things.
      if (ok) sprintf("Added \"%s\". Every conversion from now on knows it.", w)
      else attr(out, "reason") %||% "Could not write the vocabulary file - check folder permissions on the dictionaries folder."))
  })
  adm_suggestions <- reactive({ sugg_bump()
    safe(lexicon_suggestions(LOGDIR), list(indicator_tokens = data.frame(), unmapped_columns = data.frame())) })
  output$adm_sugg_tokens <- renderTable({ req(admin_ok()); adm_suggestions()$indicator_tokens })
  output$adm_sugg_cols   <- renderTable({ req(admin_ok()); adm_suggestions()$unmapped_columns })
  # Instructions only when there is something to act on. An empty list here is the
  # healthy state, not a fault: only a template whose amount style is a D/C
  # indicator column can produce an unrecognised marker at all, so on a site whose
  # statements are all signed-amount or debit/credit-column exports it stays empty
  # for good, and the old copy read as a control that had stopped working.
  output$adm_sugg_help <- renderUI({
    req(admin_ok())
    n <- nrow(adm_suggestions()$indicator_tokens %||% data.frame())
    if (isTRUE(n > 0))
      helpText("Taken from the conversions run here, most frequent first - so the word worth teaching it is at the top. Pick one, say which way the money goes, and Approve.")
    else
      helpText("Nothing to teach it yet - every money-in / money-out marker on the statements converted here was one it already knows. A word appears here by itself the first time a statement writes one it has never met.")
  })
  # The metadata corpus is kept forever and this read is BOUNDED (see
  # R/suggestions.R), so say which slice the ranking came from. A partial answer
  # that doesn't admit it is partial is the thing the charter forbids.
  output$adm_sugg_scope <- renderUI({
    req(admin_ok())
    s <- adm_suggestions()
    n <- s$scanned %||% NA_integer_; tot <- s$total %||% NA_integer_
    if (is.na(n) || is.na(tot)) return(NULL)
    p(class = "muted", style = "font-size:12px", if (n < tot)
      sprintf("Ranked from the %s newest conversion records of %s kept (the newest are what matter for 'what is it still missing?').",
              format(n, big.mark = ","), format(tot, big.mark = ","))
      else sprintf("Ranked from all %s conversion records kept.", format(tot, big.mark = ",")))
  })
  # ADMIN-ONLY, and it must be gated HERE. A bare observe() is never suspended by
  # visibility (unlike a render output), so without this it runs for every browser
  # that connects -- before any password -- and forces a full scan of the metadata
  # corpus on connect. conditionalPanel hides; it does not authorise.
  observe({
    req(admin_ok())
    toks <- adm_suggestions()$indicator_tokens$token %||% character(0)
    updateSelectInput(session, "adm_sugg_tok", choices = toks)
  })
  observeEvent(input$adm_sugg_approve, {
    req(admin_ok())
    tok <- input$adm_sugg_tok; cat_ <- input$adm_sugg_dir %||% "debit_markers"
    if (is.null(tok) || !nzchar(tok)) {
      output$adm_sugg_msg <- renderUI(div(class = "muted", "Pick a token first.")); return()
    }
    out <- lexicon_append(cat_, tolower(tok), LEXICON_PATH)
    updateTextAreaInput(session, "adm_lex_edit",
      value = read_file_text(LEXICON_PATH))
    sugg_bump(sugg_bump() + 1)
    ok <- isTRUE(out)
    output$adm_sugg_msg <- renderUI(div(style = sprintf("color:%s", if (ok) PALETTE$ok else PALETTE$bad),
      if (ok) sprintf("Added '%s' to %s. The engine will use it on the next conversion.",
                      tolower(tok), sub("_markers$", "", cat_))
      else attr(out, "reason") %||% "Could not write the vocabulary file."))
  })

  # ---- Admin: bulk audit & gaps ----
  adm_ba <- reactiveVal(NULL)
  adm_ba_conv <- reactiveVal(NULL)   # converted-report rows, when "convert & save" was ticked
  observeEvent(input$adm_ba_run, {
    req(admin_ok())
    if (is.null(input$adm_ba_files)) {
      showNotification("Upload some statements first, then click Run.",
                       type = "warning", duration = 6)
      return()
    }
    fs <- input$adm_ba_files
    sess <- tempfile("ba_"); dir.create(sess, showWarnings = FALSE)  # guaranteed-unique per session/process (no bleed)
    paths <- vapply(seq_len(nrow(fs)), function(i) {
      d <- file.path(sess, fs$name[i]); file.copy(fs$datapath[i], d, overwrite = TRUE); d }, character(1))
    adm_ba_conv(NULL)
    # OCR'ing a whole folder is the longest blocking call in the app - longer than
    # any single conversion, and started by the one person who can least afford to
    # take the tool away from everybody (the maintainer, mid-triage). So it runs in
    # its own process too, in the Admin slot, which is separate from the Convert
    # one: auditing a folder must not cancel the statement the same person is
    # converting on the other tab. The optional heavier "convert & log" pass moved
    # with it, unchanged (job_run_task, R/jobs.R).
    adm_slot$start("audit", paths, sess,
      message = "Auditing statements (scanned pages are OCR'd)",
      args = list(templates_dir = TEMPLATES_DIR, user_templates_dir = USER_TEMPLATES_DIR,
                  logdir = LOGDIR, convert = FALSE,
                  names = as.character(fs$name)),
      finish = function(res) {
        if (is.null(res$audit)) {
          showNotification(paste("The audit stopped before it finished, so there is nothing to show.",
                                 "Run it again on fewer files, or check the error log."),
                           type = "error", duration = 12)
          return(invisible(NULL))
        }
        adm_ba(res$audit)
        adm_ba_conv(res$converted)
        if (!is.null(res$converted)) load_admin()   # the batch just wrote logs; refresh Insights
      })
  })
  output$adm_ba_summary <- renderUI({
    b <- adm_ba(); if (is.null(b)) return(helpText("Upload statements and click Run."))
    g <- b$feature_gaps
    conv <- adm_ba_conv()
    none <- function(x) if (length(x)) paste(names(x), collapse = ", ") else "(none seen)"
    tagList(
      p(strong(sprintf("%d statements: ", g$total)),
        paste(sprintf("%s=%s", names(g$by_status), g$by_status), collapse = ", ")),
      p(sprintf("scanned %d \u00b7 with redactions %d \u00b7 multi-account %d \u00b7 multi-period %d \u00b7 unsupported %d across %d layouts",
        g$scanned, g$with_redactions, g$multi_account, g$multi_period, g$unsupported, g$distinct_gap_layouts)),
      p(class = "muted", sprintf("amount styles: %s | date formats: %s | banks: %s",
        none(g$amount_styles), none(g$date_formats), none(g$banks))),
      if (!is.null(conv)) div(style = "background:#eef;padding:6px 10px;border-radius:6px;margin-top:6px",
        sprintf("Converted & logged %d file(s): %d ok, %d need review, %d unsupported/failed - now in Insights.",
                nrow(conv), sum(conv$status == "ok"), sum(conv$status == "needs_review"),
                sum(conv$status %in% c("unsupported", "failed")))))
  })
  output$adm_ba_clusters <- renderDT({
    b <- adm_ba(); req(b)
    cols <- c("count", "kind", "layout_hint", "signature")
    if (!nrow(b$clusters) || !all(cols %in% names(b$clusters)))
      return(stats::setNames(data.frame(matrix(character(0), 0, length(cols))), cols))
    b$clusters[, cols]
  }, options = dt_none_opts("No gaps - every file in this batch was read by a template.",
                            pageLength = 10, dom = "tp"), rownames = FALSE)
  output$adm_ba_files_tbl <- renderDT({
    b <- adm_ba(); req(b)
    b$per_file[, c("idx", "kind", "status", "template", "bank", "n_rows", "redacted", "amount_style", "date_format", "trust")]
  }, options = list(pageLength = 15, dom = "tip"), rownames = FALSE)
  output$adm_ba_recs <- renderUI({
    b <- adm_ba(); if (is.null(b) || !length(b$recommendations))
      return(helpText("Run a bulk audit to see recommended draft templates."))
    do.call(tagList, lapply(b$recommendations, function(r) tagList(
      h5(sprintf("%d file(s), %s - draft id: %s", r$count, r$kind, r$draft_id %||% "?")),
      tags$pre(style = "font-size:11px;max-height:260px;overflow:auto;background:#f7f7f7;padding:8px", r$draft_yaml))))
  })
  # ---- Admin downloads that cannot work are not offered, and never 500 --------
  #
  # Three of these answered an HTTP 500 error page ("An error has occurred!")
  # whenever the thing they export did not exist yet, and a fourth handed back a
  # file called ".audit.md" holding one line of failure text. A browser error page
  # where a file was asked for reads as a broken tool, not as "run the audit
  # first"; req(FALSE) inside a download handler is exactly that, an aborted
  # request Shiny can only report as a server error.
  #
  # dl_when: enabled only when the export really exists, and otherwise DISABLED
  # AND STILL VISIBLE with the one line saying what to do first -- the maintainer
  # should see that the export exists and what it needs, not an empty space.
  #
  # It stays the REAL button. A lookalike <button> would be wrong in one specific
  # way: Shiny registers a download only while something on the page is bound to
  # it, so swapping the control out leaves the URL itself answering 404 -- another
  # error page, for anyone holding a link from a minute ago. Rendering the genuine
  # control keeps the handler registered, and every handler below hands back a
  # readable note instead of an error if it is reached anyway.
  dl_when <- function(id, label, ready, why) {
    if (isTRUE(ready)) return(downloadButton(id, label))
    tagList(
      downloadButton(id, label, class = "disabled", `aria-disabled` = "true"),
      div(class = "muted", style = "font-size:12px;margin:4px 0 0", why))
  }
  BA_REPORT_WHY <- "Upload statements above and click Run - there is no audit report yet."
  BA_CSV_WHY    <- "Tick \"Also convert & save\" before Run - the converted report is only produced on that pass."
  UP_AUDIT_WHY  <- "Pick a saved upload above first."
  INBOX_WHY     <- "Pick a failed file above first."

  output$adm_ba_report_ui <- renderUI({ req(admin_ok())
    dl_when("adm_ba_report", "Safe audit report (.md)", !is.null(adm_ba()), BA_REPORT_WHY) })
  output$adm_ba_csv_ui <- renderUI({ req(admin_ok())
    dl_when("adm_ba_csv", "Converted report (.csv)", !is.null(adm_ba_conv()), BA_CSV_WHY) })
  output$adm_up_audit_ui <- renderUI({ req(admin_ok())
    dl_when("adm_up_audit", "Download its safe summary (no personal data)",
            nzchar(input$adm_up_pick %||% ""), UP_AUDIT_WHY) })
  output$adm_inbox_audit_ui <- renderUI({ req(admin_ok())
    dl_when("adm_inbox_audit", "Download its safe summary (no personal data)",
            nzchar(input$adm_inbox_pick %||% ""), INBOX_WHY) })

  output$adm_ba_report <- downloadHandler(
    filename = function() "bulk-audit.md",
    content = function(file) {
      req(admin_ok())        # admin-only export
      b <- adm_ba()
      if (is.null(b)) { notify_once("adm_ba_report", BA_REPORT_WHY, duration = 6)
                        return(.dl_note(file, BA_REPORT_WHY)) }
      writeLines(format_batch_audit(b), file) })
  output$adm_ba_csv <- downloadHandler(
    filename = function() "batch_converted.csv",
    content = function(file) {
      req(admin_ok())        # admin-only export
      conv <- adm_ba_conv()
      if (is.null(conv)) { notify_once("adm_ba_csv", BA_CSV_WHY, duration = 8)
                           return(.dl_note(file, BA_CSV_WHY)) }
      utils::write.csv(conv, file, row.names = FALSE) })

  # adm_tpl_bands -- open the SELECTED template in the visual editor, on a real
  # statement it has actually read.
  #
  # The band editor needs two things: a template, and a page to draw it on. Admin
  # had the first and no way to get the second, so the only route to the picture
  # was to go and convert a statement first -- which is backwards when the reason
  # you are in Admin is that you already know a template is wrong. The saved
  # statements record WHICH template read them, so the page is already on disk:
  # take the most recent one that this template read and still has its file.
  #
  # Most recent, not oldest: a template that has drifted is wrong on the newest
  # statements first, and that is the one worth looking at.
  .tpl_sample <- function(tid) {
    u <- safe(read_uploads(UPLOADS_DIR), NULL)
    if (is.null(u) || !nrow(u) || !"template" %in% names(u)) return(NULL)
    ok <- !is.na(u$template) & u$template == tid & !u$purged
    if (!any(ok)) return(NULL)
    u <- u[ok, , drop = FALSE]          # read_uploads() is already newest-first
    for (i in seq_len(nrow(u))) {
      p <- safe(upload_file_path(u$id[i], UPLOADS_DIR), NA_character_)
      if (!is.na(p) && file.exists(p)) return(list(path = p, id = u$id[i], ts = u$ts[i]))
    }
    NULL
  }
  # THE EDITOR THAT MATCHES THE TEMPLATE.
  #
  # Reported: "the template I set up for other, open in Admin, open template,
  # opens bank statement template!" It did: this button called open_guided()
  # unconditionally, so a report template -- built by pointing at a page -- was
  # handed to the toolkit that edits transaction columns, bank name and all. The
  # kind decides which builder opens, and the button says which before it is
  # pressed (adm_tpl_bands_btn below).
  observeEvent(input$adm_tpl_bands, {
    req(admin_ok())
    tid <- input$adm_tpl_pick
    if (is.null(tid) || !nzchar(tid)) {
      output$adm_tpl_bands_msg <- .tpl_note("Pick a template first.", ok = FALSE); return()
    }
    t <- adm_lib()[[tid]]
    if (is.null(t)) {
      output$adm_tpl_bands_msg <- .tpl_note("That template could not be loaded.", ok = FALSE); return()
    }
    kind <- template_kind(t)
    # A fields (form) template finds its values by WORDING, not by coordinates --
    # there is no picture of it to open. Say so, and point at the box that IS its
    # editor, rather than opening a page with nothing drawn on it.
    if (identical(kind, "fields")) {
      output$adm_tpl_bands_msg <- .tpl_note(paste(
        "This is a form template: it finds its values by the wording printed beside",
        "them, not by where they sit on the page, so there is nothing to draw. Its",
        "fields are edited in the YAML box on the right."), ok = FALSE)
      return()
    }
    smp <- .tpl_sample(tid)
    if (is.null(smp)) {
      # The honest dead end, with the reason and the way out. Without a document
      # this template has read there is nothing to draw on, and no amount of
      # clicking will change that.
      output$adm_tpl_bands_msg <- .tpl_note(sprintf(paste(
        "No saved document was read with %s, so there is no page to draw its",
        "%s on. Convert one with it first - then this button opens it here.",
        "(Saved documents are deleted after the retention period, so an old",
        "template may have none left.)"), htmltools::htmlEscape(tid),
        if (identical(kind, "document")) "tables" else "columns"), ok = FALSE)
      return()
    }
    output$adm_tpl_bands_msg <- renderUI(NULL)
    if (identical(kind, "document")) rb_open_template(t, smp$path)
    else open_guided(smp$path, basename(smp$path), seed_tmpl = t, upload_id = smp$id)
  })
  # ...and the button names the editor it will open, so nobody has to press it to
  # find out. "Redraw its columns on a real statement" was a promise the button
  # could not keep for two of the three kinds.
  output$adm_tpl_bands_btn <- renderUI({
    id <- input$adm_tpl_pick
    t <- if (is.null(id) || !nzchar(id)) NULL else adm_lib()[[id]]
    kind <- if (is.null(t)) NA_character_ else template_kind(t)
    lab <- if (identical(kind, "document")) "Open it in the report builder"
           else if (identical(kind, "fields")) "Edit its fields (in the box on the right)"
           else "Redraw its columns on a real statement"
    actionButton("adm_tpl_bands", lab,
                 class = if (identical(kind, "fields")) "btn-default" else "btn-primary")
  })

  # ---- H7: "I have just added table 41 - do the other 40 still read?" --------
  #
  # A forty-table template grown over months is unmaintainable without this. Every
  # edit is a change whose blast radius nobody can see, because there was no way
  # to ask one template the one question that matters after an edit: does anything
  # that used to read stop reading?
  #
  # THERE IS NO FOLDER PICKER, ON PURPOSE. The examples are the documents this
  # template has ALREADY READ on this box -- the saved uploads record which
  # template read them -- so the tool knows which files the question is about and
  # does not ask. .tpl_sample() above takes the newest one for the band editor;
  # this takes the newest few for the check, and .ADM_CHECK_MAX bounds it because
  # every one of them is read (and OCR'd) in this process, which the whole team
  # shares.
  .ADM_CHECK_MAX <- 6L
  .tpl_examples <- function(tid, n = .ADM_CHECK_MAX) {
    u <- safe(read_uploads(UPLOADS_DIR), NULL)
    if (is.null(u) || !nrow(u) || !"template" %in% names(u)) return(character(0))
    ok <- !is.na(u$template) & u$template == tid & !u$purged
    if (!any(ok)) return(character(0))
    u <- u[ok, , drop = FALSE]          # read_uploads() is already newest-first
    ps <- vapply(u$id, function(i) safe(upload_file_path(i, UPLOADS_DIR), NA_character_),
                 character(1), USE.NAMES = FALSE)
    ps <- ps[!is.na(ps) & file.exists(ps)]
    utils::head(ps, n)
  }
  adm_check <- reactiveVal(NULL)     # list(id, grid, message, n)
  output$adm_tpl_check_btn <- renderUI({
    id <- input$adm_tpl_pick
    if (is.null(id) || !nzchar(id)) return(NULL)
    n <- length(.tpl_examples(id))
    tagList(
      actionButton("adm_tpl_check", "Check it still reads the examples on this box"),
      div(class = "muted", style = "font-size:12px;margin-top:4px",
          if (n == 0L)
            "There are no saved documents on this box that this template has read, so there is nothing to check it against yet."
          else sprintf("It will re-read the %d most recent document%s this template has read here.",
                       n, if (n == 1L) "" else "s")))
  })
  observeEvent(input$adm_tpl_check, {
    req(admin_ok())
    id <- input$adm_tpl_pick
    t <- if (is.null(id) || !nzchar(id)) NULL else adm_lib()[[id]]
    if (is.null(t)) {
      adm_check(list(id = "", grid = NULL, n = 0L, ok = FALSE,
                     note = "Pick a template first."))
      return()
    }
    paths <- .tpl_examples(id)
    # THIS BLOCKS THE PROCESS while it reads and OCRs each example, which is why it
    # is bounded to a handful and why the wait is SHOWN. It cannot go through the
    # job slot as things stand: job_run_task (R/jobs.R) has no task for it.
    r <- withProgress(message = "Re-reading this template's examples", value = 0.1,
      tryCatch(template_check(t, paths,
                              progress = function(i, n, f)
                                setProgress(value = i / max(1, n), detail = basename(f))),
               error = function(e) NULL))
    if (is.null(r)) {
      adm_check(list(id = id, grid = NULL, n = length(paths), ok = FALSE,
                     note = "The check stopped before it finished, so there is nothing to show."))
      return()
    }
    # ONE SENTENCE, in template_check's own words with the machine code taken off
    # it the way every other verdict on this app has, so the screen and the engine
    # can never say two different things about the same check.
    msg <- as.character(r$message)[1]
    said <- plain_messages(msg)
    adm_check(list(id = id, grid = r$grid, n = length(paths),
                   ok = grepl("^ok:", msg),
                   note = .sentence(if (length(said)) said[1] else msg)))
  })
  output$adm_tpl_check_msg <- renderUI({
    ch <- adm_check(); if (is.null(ch)) return(NULL)
    div(style = "margin-top:6px", span(class = if (isTRUE(ch$ok)) "ok" else "bad", ch$note))
  })
  # The grid is wide, so it sits under the two columns rather than inside one.
  output$adm_tpl_check_head <- renderUI({
    ch <- adm_check(); if (is.null(ch) || is.null(ch$grid) || !nrow(ch$grid)) return(NULL)
    tagList(tags$hr(),
      h4(sprintf("How this template read %d example%s", ch$n, if (ch$n == 1L) "" else "s")),
      helpText("One row per table (or value) per example. A count of 0 rows on every example is what an edit has broken; a word count above 0 means the column edges do not fit that document."))
  })
  output$adm_tpl_check_grid <- renderDT({
    ch <- adm_check(); req(ch); g <- ch$grid; req(g)
    if (nrow(g) && "file" %in% names(g)) g$file <- basename(as.character(g$file))
    datatable(g, rownames = FALSE,
              options = dt_none_opts("Nothing to show yet.", pageLength = 15,
                                     dom = "tip", scrollX = TRUE))
  })
  # A template that is edited is a different template, so the last check stops
  # standing for it -- a stale green grid over a template that has since changed
  # is exactly the wrong figure that looks right, one screen along.
  observeEvent(input$adm_tpl_pick, {
    req(admin_ok())
    ch <- adm_check()
    if (!is.null(ch) && !identical(ch$id, input$adm_tpl_pick)) adm_check(NULL)
  })

  # adm_feed_health -- did the analytics feed actually receive what it should have?
  #
  # Nobody was asking. write_feed() records every outcome under logs\feed\, and
  # the ONLY place a failure ever surfaced was the screen of whichever analyst
  # happened to run that conversion -- who cannot fix a read-only share and, since
  # the feed line came off Convert, is no longer told at all. The documented check
  # was `findstr /s /m "write_failed" logs\feed\*.json`, which nobody runs until
  # they already suspect something. A failed write is silent by nature: the
  # conversion succeeded, the download is complete, and only the dashboards are
  # short. So it is counted here, and it leads with the bad news or says plainly
  # that there is none.
  output$adm_feed_health <- renderUI({
    req(admin_ok())
    input$adm_refresh
    # read_log_records() returns a DATA FRAME, one row per conversion -- not a
    # list of records. Treating it as a list iterates the COLUMNS, and `$` on an
    # atomic vector printed "$ operator is invalid for atomic vectors" onto the
    # Admin page. Caught by opening the page; no grep would have found it.
    recs <- safe(read_log_records(LOGDIR, "feed"), NULL)
    if (is.null(recs) || !nrow(recs))
      return(p(class = "muted", "No conversions have been through the feed yet."))
    col <- function(nm, default = NA) if (nm %in% names(recs)) recs[[nm]] else rep(default, nrow(recs))
    gr    <- as.character(col("gate_result", ""))
    gr[is.na(gr)] <- ""
    wrote <- as.logical(col("feed_written", FALSE)); wrote[is.na(wrote)] <- FALSE
    # A suffix after the gate reason is the failure half: accepted:write_failed,
    # accepted:stale_row_kept. Those are the ones worth a maintainer's attention.
    bad  <- grepl(":(write_failed|stale_row_kept)$", gr)
    when <- function(i) {
      t <- as.character(col("ts", ""))[i]
      t <- t[!is.na(t) & nzchar(t)]
      if (!length(t)) "" else paste0(" (most recent ", max(t), ")")
    }
    tagList(
      p(class = if (any(bad)) "bad" else "ok", style = "font-weight:600;margin:0",
        if (any(bad))
          sprintf("%d of %d conversion(s) did not reach the dashboards as intended%s.",
                  sum(bad), nrow(recs), when(bad))
        else sprintf("All %d conversion(s) were handled as intended - %d published, %d held back by the gate.",
                     nrow(recs), sum(wrote), nrow(recs) - sum(wrote))),
      if (any(bad)) tags$ul(lapply(unique(gr[bad]), function(g) {
        n <- sum(gr == g)
        fb <- plain_feed(list(reason = sub(":[^:]*$", "", g), gate_result = g))
        tags$li(HTML(sprintf("<b>%d</b> &times; %s", n, fb$why %||% g)))
      })),
      p(class = "muted", style = "margin:4px 0 0",
        HTML(sprintf("Feed folder: <code>%s</code>. Full detail is one file per conversion under <code>%s</code>.",
                     CONFIG$feed$feed_dir %||% "feed", file.path(LOGDIR, "feed")))))
  })

  # .fill_pick(session, id, choices, empty) -- fill an Admin picker and let it say
  # what it is when there is nothing to pick.
  #
  # Every one of these controls used to be a plain selectInput fed a bare
  # character vector. Two things followed. You could not TYPE in them, so finding
  # one entry among hundreds meant scrolling; and an empty list rendered as an
  # empty box, which reads as a broken control rather than as an empty queue --
  # the report was "I click it and nothing comes up", for a picker that was
  # working exactly as written. The placeholder carries the answer either way.
  .fill_pick <- function(session, id, choices, empty, selected = NULL) {
    if (is.null(choices)) choices <- character(0)
    # NOT as.character(): that drops the names, and the names are the whole point
    # for the uploads picker, where the value is an opaque id and the label is the
    # date and status a person actually recognises.
    keep <- selected %||% isolate(input[[id]]) %||% ""
    if (!keep %in% unname(choices)) keep <- ""     # a stale pick must not survive
    updateSelectizeInput(session, id, choices = choices, selected = keep,
                         server = FALSE,
                         options = list(placeholder = if (length(choices)) "Type to search" else empty))
  }

  # ---- Admin: uploads & pickups ----
  # A stored timestamp is ISO ("2026-07-28T00:20:57Z"), which is precise and hard
  # to read down a list. The picker beside this table has to be scannable, so it
  # gets the same instant in the form a person reads at a glance; the table keeps
  # the exact value, because that is what you quote in an audit.
  .up_when <- function(ts) {
    p <- suppressWarnings(as.POSIXct(sub("Z$", "", ts), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    ifelse(is.na(p), ts %||% "(no date)", format(p, "%d %b %Y %H:%M"))
  }
  output$adm_uploads <- renderDT({
    cv_upload_id(); input$adm_refresh          # refresh after a convert or on demand
    u <- read_uploads(UPLOADS_DIR)
    cols <- c("ts", "file_ext", "status", "template", "trust", "needs_pickup", "purged", "run_id")
    if (!nrow(u) || !all(cols %in% names(u)))
      return(stats::setNames(data.frame(matrix(character(0), 0, length(cols))), cols))
    # `purged` = the saved copy has passed its retention period and been deleted.
    # Shown, because a pickup row whose file is gone must not look actionable.
    u[, cols]
  }, options = dt_none_opts("No statements have been converted here yet.",
                            pageLength = 8, dom = "tip"), rownames = FALSE,
     selection = "single")   # so clicking a row can fill the picker beside it
  observe({
    req(admin_ok())          # upload ids identify real client statements
    cv_upload_id(); input$adm_refresh
    u <- read_uploads(UPLOADS_DIR)
    # EVERY upload still on disk, not just the ones that failed. This used to
    # filter on `needs_pickup` -- unsupported-or-failed and not since taught --
    # under a label that says "Pick a saved upload" and beside a table listing
    # them all. On a server where conversions are working, that is the empty set,
    # so the control looked broken rather than selective, and the audit download
    # and "open in the toolkit" beside it were unreachable for every statement
    # that had actually converted. Both are useful on a GOOD conversion: auditing
    # one is how you check a template, and opening one in the toolkit is how you
    # improve it. A purged upload is still excluded -- its file is gone, so both
    # buttons would be dead ends.
    keep <- if (nrow(u)) !u$purged else logical(0)
    ids  <- if (any(keep)) u$id[keep] else character(0)
    # Labelled, because a raw id ("0fdca7c700-20260728002057-fa1f") tells nobody
    # anything. read_uploads() already returns newest first, so the order is the
    # one a person wants.
    lab <- if (length(ids)) sprintf("%s  -  %s%s", .up_when(u$ts[keep]),
                                    u$status[keep],
                                    ifelse(is.na(u$file_ext[keep]), "",
                                           paste0(" (.", u$file_ext[keep], ")"))) else character(0)
    .fill_pick(session, "adm_up_pick", stats::setNames(ids, lab),
               empty = "Nothing has been converted here yet")
  })
  # Clicking a row in the table fills the picker, which is what everybody tries
  # first and nothing was listening for.
  observeEvent(input$adm_uploads_rows_selected, {
    req(admin_ok())
    u <- read_uploads(UPLOADS_DIR)
    i <- input$adm_uploads_rows_selected
    if (nrow(u) && length(i) && i <= nrow(u))
      updateSelectizeInput(session, "adm_up_pick", selected = u$id[i])
  })
  output$adm_up_audit <- downloadHandler(
    filename = function() "upload.audit.md",
    content = function(file) {
      req(admin_ok())        # reads another analyst's saved statement off disk
      id <- input$adm_up_pick
      p <- if (!is.null(id) && nzchar(id)) upload_file_path(id, UPLOADS_DIR) else NA_character_
      if (is.na(p)) { notify_once("adm_up_audit", UP_AUDIT_WHY, duration = 6)
                      return(.dl_note(file, UP_AUDIT_WHY)) }
      a <- tryCatch(format_audit(statement_audit(need_file(p), templates = templates())),
                    error = function(e) NULL)
      if (is.null(a)) return(.dl_note(file, sprintf(
        "The saved statement for %s could not be read - it may have been deleted by the retention purge. Its upload record is still in Insights.", id)))
      writeLines(a, file)
    })

  # ---- Admin: format requests raised via the "tell our team" escape hatch ----
  req_bump <- reactiveVal(0)   # bump to refresh after a triage action
  output$adm_requests <- renderDT({
    req_bump(); input$adm_refresh
    q <- read_template_requests(REQUESTS_DIR)
    cols <- c("ts", "requested_by", "status", "detail", "context")
    if (!nrow(q) || !all(cols %in% names(q)))
      return(stats::setNames(data.frame(matrix(character(0), 0, length(cols))), cols))
    q[, cols]
  }, options = dt_none_opts("Nobody has raised a format request.",
                            pageLength = 6, dom = "tip"), rownames = FALSE)
  observe({
    # A bare observe() is NOT suspended when its tab is hidden, so without this
    # guard the template-request queue was read off disk and its ids pushed into
    # every session's select input, admin or not. Found by the invariant test once
    # it started reading whole observer bodies instead of a fixed nine lines.
    req(admin_ok())
    req_bump(); input$adm_refresh
    q <- read_template_requests(REQUESTS_DIR)
    open <- if (nrow(q)) q$id[q$status == "open"] else character(0)
    # An empty picker used to render as a blank box, which reads as broken rather
    # than as "there is nothing here". The placeholder says which it is, so the
    # control explains itself without needing a line of text beside it.
    .fill_pick(session, "adm_req_pick", open,
               empty = "No open requests - nothing to triage")
  })
  # Every action that changes something says what it changed -- including the one
  # that fails, which said nothing at all either.
  .req_note <- function(html, ok = TRUE)
    renderUI(span(class = if (ok) "ok" else "bad", HTML(html)))
  .req_set <- function(id, status, done) {
    if (is.null(id) || !nzchar(id)) {
      output$adm_req_msg <- .req_note("Pick a request first.", ok = FALSE); return()
    }
    if (isTRUE(set_request_status(id, status, dir = REQUESTS_DIR))) {
      req_bump(req_bump() + 1)
      output$adm_req_msg <- .req_note(sprintf("Request <b>%s</b> marked %s - it has left the open list.",
                                              id, done))
    } else {
      output$adm_req_msg <- .req_note(sprintf(
        "Could not update request %s - check folder permissions on %s.", id, REQUESTS_DIR), ok = FALSE)
    }
  }
  observeEvent(input$adm_req_actioned, {
    req(admin_ok())
    .req_set(input$adm_req_pick, "actioned", "done")
  })
  observeEvent(input$adm_req_dismiss, {
    req(admin_ok())
    .req_set(input$adm_req_pick, "dismissed", "dismissed")
  })

  # ---- Admin: folder-intake browser (inbox / processed / failed / outbox) ----
  inbox_state <- reactive({ input$adm_refresh; cv_upload_id(); inbox_status(".") })
  output$adm_inbox_counts <- renderUI({
    s <- inbox_state(); c <- s$counts
    p(class = "muted", HTML(sprintf(
      "Waiting: <b>%d</b> &nbsp;|&nbsp; Processed: <b>%d</b> &nbsp;|&nbsp; Failed: <b>%d</b> &nbsp;|&nbsp; Stuck: <b>%d</b> &nbsp;|&nbsp; Output folders: <b>%d</b>",
      c[["inbox"]], c[["processed"]], c[["failed"]], c[["stuck"]], c[["outbox"]])))
  })
  inbox_tbl <- function(which, none) renderDT({
    inbox_state()$folders[[which]]
  }, options = dt_none_opts(none, pageLength = 6, dom = "tip"), rownames = FALSE)
  output$adm_inbox_failed    <- inbox_tbl("failed",    "Nothing has failed - good.")
  output$adm_inbox_waiting   <- inbox_tbl("inbox",     "Nothing waiting in inbox/.")
  output$adm_inbox_processed <- inbox_tbl("processed", "Nothing processed yet.")
  output$adm_inbox_outbox    <- inbox_tbl("outbox",    "No output folders yet.")
  observe({
    # These are REAL filenames of failed client statements -- routinely a surname
    # and a case reference. They were being pushed to every connected browser on
    # connect, with no login and no user action.
    req(admin_ok())
    s <- inbox_state()
    .fill_pick(session, "adm_inbox_pick",
               if (nrow(s$folders$failed)) s$folders$failed$file else character(0),
               empty = "Nothing in failed/ - good")
  })
  observeEvent(input$adm_inbox_wizard, {
    req(admin_ok())
    nm <- input$adm_inbox_pick
    if (is.null(nm) || !nzchar(nm)) { showNotification("Pick a failed file first.", type = "warning"); return() }
    p <- failed_file_path(nm, ".")
    if (is.na(p)) { showNotification("That file is no longer in failed/.", type = "error"); return() }
    open_guided(p, nm)
  })
  output$adm_inbox_audit <- downloadHandler(
    # With nothing picked this built the filename ".audit.md" -- a dot-file, hidden
    # on the maintainer's own machine, holding a one-line failure message. `%||%`
    # never fired because an empty selectInput sends "", not NULL.
    filename = function() {
      nm <- basename(trimws(input$adm_inbox_pick %||% ""))
      if (nzchar(nm)) paste0(nm, ".audit.md") else "no-file-selected.audit.md"
    },
    content = function(file) {
      req(admin_ok())        # reads a failed client statement off disk
      nm <- input$adm_inbox_pick
      p <- if (!is.null(nm) && nzchar(nm)) failed_file_path(nm, ".") else NA_character_
      if (is.na(p)) { notify_once("adm_inbox_audit", INBOX_WHY, duration = 6)
                      return(.dl_note(file, INBOX_WHY)) }
      a <- tryCatch(format_audit(statement_audit(need_file(p), templates = templates())),
                    error = function(e) NULL)
      if (is.null(a)) return(.dl_note(file, sprintf(
        "%s could not be read at all, so there is nothing to summarise. That it cannot be read IS the finding: it is not a statement this tool can open, or the file is damaged.", nm)))
      writeLines(a, file)
    })

  # ---- Add a template: ONE builder, one table at a time ---------------------
  #
  # Two gestures build a table and nothing else is required:
  #   step 1  drag round its TITLE        -> the name
  #   step 2  drag round its COLUMN NAMES -> the columns, and where it starts
  #           ...the end is then read off the document and drawn
  #   step 3  check it, adjust, save. Next table.
  #
  # THE DRAG MEANS ONE THING AT A TIME. Which thing is on the card in front of
  # her, and it changes only when she moves on. There is no mode to set, no
  # second question after a box is drawn, and no list of thirty near-right tables
  # to work through -- "nearly right" is the most expensive state there is.

  # parse_fields_spec -- the friendly "name = Label; Label2 | money" lines. Typing
  # the wording is still the fastest way in for somebody who knows the field
  # names, and a value found by wording alone survives the layout moving.
  parse_fields_spec <- function(text) {
    lines <- trimws(strsplit(text %||% "", "\n")[[1]])
    lines <- lines[nzchar(lines) & grepl("=", lines)]
    fields <- list()
    for (ln in lines) {
      name <- trimws(sub("=.*$", "", ln))
      rhs  <- trimws(sub("^[^=]*=", "", ln))
      vtype <- NULL
      if (grepl("\\|", rhs)) { vtype <- trimws(sub("^.*\\|", "", rhs)); rhs <- trimws(sub("\\|.*$", "", rhs)) }
      labels <- trimws(strsplit(rhs, ";")[[1]]); labels <- labels[nzchar(labels)]
      if (!nzchar(name) || !length(labels)) next
      spec <- list(any_of = as.list(labels))
      if (!is.null(vtype) && vtype %in% c("money", "date", "date_range", "text")) spec$value <- vtype
      fields[[name]] <- spec
    }
    fields
  }

  # .rb_kind(v) -- money, date or text, read from the value with the same rule the
  # extractor uses, so what the picker offers is what the extraction would do.
  .rb_kind <- function(v) .doc_value_kind(v)

  # ONE ARMED INTENT AT A TIME.
  #
  # `mode` is the whole interaction model: it names the single thing the next
  # gesture will do, it is written across the top of the screen in a sentence,
  # and it is empty unless a button put something there. A drag with nothing
  # armed changes nothing and says so -- which is the difference between a tool
  # you can look around in and one where every mis-drag leaves a table behind.
  .RB_ASK <- list(
    title  = c("Drag a box round the table's TITLE.",
               "Its wording becomes the table's name. No title? Press Skip."),
    cols   = c("Drag a box round the ROW OF COLUMN NAMES.",
               "Only the header row - not the rows of data under it."),
    addcol = c("Drag a box over the NEW COLUMN.",
               "Anywhere down its width; the top and bottom of the box are ignored."),
    colpos = c("Drag a box over WHERE THIS COLUMN SHOULD BE.",
               "Only the left and right edges of the box are used."),
    start  = c("Click the page where the table STARTS.",
               "Click just under its column names. Turn the page first if you need to."),
    end    = c("Click the page where the table ENDS.",
               "Turn the page first if it carries on past this one."),
    # THE BOTTOM OF A CARRYING-ON PAGE IS A THIRD FACT, and it is the one nobody
    # was asked for. "Where it ends" is a place on ONE page -- the last one. On
    # every page in between, the table runs to whatever bottom the reader is
    # given, which is the bottom of the paper unless somebody says otherwise. A
    # report with a footnote, a source line or a page footer under the table then
    # reads those in as rows, on every page but the last, and the person who set
    # the end correctly has no way to see why.
    bottom = c("Click the lowest line the table reaches on a page it CARRIES ON past.",
               "The bottom edge for every page except the last. Below it is footer, not table."),
    label  = c("Drag a box round the LABEL.",
               "The words that name the value - not the value itself."),
    value  = c("Drag a box round the VALUE.",
               "The figure or the words that the label is naming."),
    # THE ONE BOX THAT HAD TO BE TYPED. A fingerprint is matched against the page
    # exactly, so a phrase retyped with a different dash, a stray space or the
    # wrong capital matches nothing -- and the person is left looking at a phrase
    # they can SEE on the page, wondering why another template was used.
    phrase = c("Drag a box round A PHRASE PRINTED ON THIS DOCUMENT.",
               "Its wording is how this document is recognised. A heading is ideal - not words every document carries."))

  rb <- reactiveValues(
    tables = list(), pairs = list(),
    draft = NULL,                        # the table being worked on
    vdraft = NULL,                       # the value being worked on
    mode = "",                           # what the next gesture means
    # TRUE while the screen is WALKING somebody through building a table --
    # title, columns, starts, ends, and the bottom edge if it runs over pages.
    # Off the moment they steer themselves (press "Move it", or Cancel), because
    # a guide that keeps arming the next step after you have taken the wheel is a
    # screen that will not stop asking questions.
    guide = FALSE,
    colsel = NA_integer_, colver = 0L,   # which column the edit panel holds
    # WHICH SAVED TABLE THE DRAFT IS A COPY OF, or NA for a new one.
    #
    # Editing used to CUT the table out of the template and put it in the draft,
    # so a mis-click on Edit followed by Cancel destroyed it - one click, no
    # confirmation, no undo, on a template that can hold forty tables built over
    # months. The draft is a COPY now and Save puts it back at this index.
    edit_idx = NA_integer_,
    # A remove asked for once. Removing a saved table is the only gesture here
    # that destroys work, so it is the only one that has to be meant twice.
    rm_armed = NULL,
    # THE PAGE SIZE THE TEMPLATE'S BOXES WERE DRAWN IN, when a saved template is
    # open. Every band is a number in that space; reading it back over a document
    # of a different size and re-stamping the size is how every box on the
    # template silently moves. NULL means "this document's own page size", which
    # is right for a template being drawn for the first time.
    frame = NULL,
    # Where and when a click last set a boundary. Shiny sends a plot click on
    # MOUSEDOWN, so a drag sends one too -- this is how the brush that follows is
    # recognised as the tail of that same gesture rather than a new instruction.
    click_at = NULL,
    preview = NULL, outputs = character(0))

  # A document handed over from the statement toolkit ("Not a transaction
  # table?"). Without it the answer to that question would be "upload it again".
  rb_handoff <- reactiveVal(NULL)
  rb_doc <- reactive({
    h <- rb_handoff(); if (!is.null(h) && file.exists(h)) return(h)
    f <- input$ts_file
    if (!is.null(f) && identical(tolower(tools::file_ext(f$name %||% "")), "pdf"))
      return(f$datapath)
    NULL
  })
  output$rb_has_doc <- reactive({ !is.null(rb_doc()) })
  outputOptions(output, "rb_has_doc", suspendWhenHidden = FALSE)

  # ---- THE WAY BACK INTO A SAVED REPORT TEMPLATE -----------------------------
  #
  # A report template could be built and then never opened again. There was no
  # route from a saved template back to the boxes it was drawn from, so the thing
  # that actually goes wrong -- "this column is a few points too far left" -- meant
  # editing coordinates as text, or drawing the whole template a second time. The
  # statement half has had this door since Admin grew "Redraw its columns"; this is
  # the same door for the other half, and it is what makes Admin's editor open the
  # RIGHT builder instead of the bank statement one.
  #
  # rb_editing() holds the id of the saved template on screen, or NA for a new one.
  # It is not decoration: while it is set, the three "we guessed this for you"
  # observers below (issuer, id, fingerprint) stand down. A suggestion that
  # overwrites what somebody deliberately saved is not a suggestion.
  rb_editing <- reactiveVal(NA_character_)
  output$rb_editing_note <- renderUI({
    e <- rb_editing()
    if (is.na(e) || !nzchar(e)) return(strong("Building a template from "))
    tagList(strong(sprintf("Editing the saved template %s", e)),
            span(class = "muted", " \u00b7 Save replaces it \u00b7 on "))
  })
  outputOptions(output, "rb_editing_note", suspendWhenHidden = FALSE)
  rb_open_template <- function(t, path = NULL) {
    prop <- document_proposal_from_template(t)
    rb$tables <- prop$tables
    rb$pairs  <- prop$pairs
    rb$draft <- NULL; rb$vdraft <- NULL; rb$mode <- ""; rb$colsel <- NA_integer_
    rb$guide <- FALSE; rb$preview <- NULL; rb$click_at <- NULL
    rb$edit_idx <- NA_integer_; rb$rm_armed <- NULL
    # THE FRAME COMES BACK WITH THE TEMPLATE. Its bands are points in the page
    # size it was drawn on; opening it over a US Letter copy and re-stamping the
    # size left every band in the old space with a new label on it -- measured at
    # 11pt across and 41pt up for one word. Restored here and never overwritten
    # while it is open. (Register H4.)
    fw <- suppressWarnings(as.numeric((t$ref_width  %||% NA_real_)[1]))
    fh <- suppressWarnings(as.numeric((t$ref_height %||% NA_real_)[1]))
    rb$frame <- if (is.finite(fw) && fw > 0 && is.finite(fh) && fh > 0)
      list(ref_width = fw, ref_height = fh) else NULL
    updateTextInput(session, "rb_bank", value = as.character(t$bank %||% "")[1])
    updateTextInput(session, "rb_type",
                    value = as.character(t$statement_type %||% "report")[1])
    updateTextInput(session, "rb_id", value = as.character(t$id %||% "")[1])
    updateTextAreaInput(session, "rb_fp", value = paste(
      trimws(as.character(unlist(t$fingerprint$page_contains_all %||% character(0)))),
      collapse = "\n"))
    rb_id_auto(NA_character_); rb_fp_auto(NA_character_)
    rb_editing(as.character(t$id %||% "")[1])
    if (!is.null(path) && file.exists(path)) rb_handoff(path)
    updateRadioButtons(session, "ts_doctype", selected = "other")
    updateTabsetPanel(session, "main_tabs", selected = "Add a template")
  }
  # ANOTHER EXAMPLE OF THE SAME TEMPLATE, OR A NEW ONE - AND THE TOOL WORKS OUT
  # WHICH.
  #
  # "A number of these reports will NOT have consistent page numbers... sometimes
  # there may only be 2 tables and others 40" - which is a template built from
  # several examples, and it was impossible. Uploading a second document cleared
  # the editing flag, which un-blocked the three seeding observers: the issuer was
  # overwritten from the new FILENAME and the identifying phrases wholesale. Half
  # a template, and the half you cannot see, gone.
  #
  # There is nothing to ask. If there is work on this screen - a saved template
  # open, or one table or value drawn - a second document is ANOTHER EXAMPLE of
  # it, and the only thing that changes is the page. Nothing else can be true:
  # the boxes are still on screen, so they are still the answer. If there is no
  # work, a new document is a new template, which is what it always was.
  # It is not silent, and the way back is one link. (Register H5.)
  .rb_has_work <- function() !is.na(rb_editing()) ||
    length(rb$tables) > 0L || length(rb$pairs) > 0L
  observeEvent(input$ts_file, {
    # THE FILE JUST CHOSEN IS THE DOCUMENT. rb_doc() prefers a handed-over path
    # when there is one, so without this the picker did nothing at all after a
    # template had been opened from Convert - which is the one route into "add
    # tables from a second example" that anybody would try first.
    rb_handoff(NULL)
    if (!.rb_has_work()) { rb_editing(NA_character_); rb$frame <- NULL; return() }
    rb$draft <- NULL; rb$vdraft <- NULL; rb$mode <- ""; rb$colsel <- NA_integer_
    rb$edit_idx <- NA_integer_; rb$rm_armed <- NULL; rb$preview <- NULL
    updateNumericInput(session, "rb_page", value = 1)
    # ONE SENTENCE. The second half told her to use "Start a new template" -- a
    # link sitting two inches above this message, on screen, saying exactly that.
    showNotification("Read as another example of this template - only the page changed.",
                     type = "message", duration = 9)
  }, ignoreInit = TRUE)
  # THE WAY BACK, and the only thing on screen that empties it.
  observeEvent(input$rb_fresh, {
    rb$tables <- list(); rb$pairs <- list()
    rb$draft <- NULL; rb$vdraft <- NULL; rb$mode <- ""; rb$colsel <- NA_integer_
    rb$edit_idx <- NA_integer_; rb$rm_armed <- NULL; rb$preview <- NULL
    rb$frame <- NULL
    rb_editing(NA_character_); rb_seeded(NA_character_)
    rb_id_auto(NA_character_); rb_fp_auto(NA_character_)
    updateTextInput(session, "rb_id", value = "new_report")
    updateTextAreaInput(session, "rb_fp", value = "")
    showNotification("Started again. Nothing was saved to the library.",
                     type = "message", duration = 6)
  })

  # WHAT TO DO NEXT, AND WHY NOTHING HAPPENED. Choosing "anything else" and
  # uploading a spreadsheet used to do nothing at all: the picker accepts one
  # because the STATEMENT half of this screen reads spreadsheets, the builder
  # needs a PDF because it works by pointing at the page, and the screen said
  # neither. A dead end with no message is the worst thing a first screen can do.
  output$rb_need_doc <- renderUI({
    f <- input$ts_file
    ext <- tolower(tools::file_ext(as.character(f$name %||% "")))
    if (!is.null(f) && nzchar(ext) && !identical(ext, "pdf"))
      return(div(class = "note", style = "max-width:820px;border-left:4px solid #b7791f",
        strong(sprintf("%s is a .%s file.", f$name, ext)),
        p(style = "margin:6px 0 0",
          paste0("Building a template for a report means pointing at the page, so ",
                 "this half needs a PDF. A .", ext, " file has no page to point at."),
          br(),
          "If it is a bank statement, choose ",
          strong("A bank or card statement"), " above and it will read this file.")))
    div(class = "note", style = "max-width:820px",
        strong("Upload the document above to start."),
        p(class = "muted", style = "margin:6px 0 0",
          "One example PDF. Nothing is created until you press a button on it."))
  })
  outputOptions(output, "rb_need_doc", suspendWhenHidden = FALSE)

  # THE UPLOAD PANEL FOLDS AWAY ONCE THERE IS A DOCUMENT, and comes back when
  # somebody asks for it. Not a preference and not remembered: a document is
  # loaded, so the picker has been used, so it is in the way. "Use a different
  # document" is the only way back and it puts the real picker on screen -- a
  # fileInput cannot be cleared from the server, so pretending otherwise (a
  # button that "resets" it) would be a control that lies.
  rb_up_open <- reactiveVal(FALSE)
  output$rb_up_open <- reactive({ isTRUE(rb_up_open()) })
  outputOptions(output, "rb_up_open", suspendWhenHidden = FALSE)
  observeEvent(input$rb_change_doc, { rb_up_open(TRUE) })
  # A new file answers the question the panel was reopened to ask.
  observeEvent(input$ts_file, { rb_up_open(FALSE) }, ignoreInit = TRUE)
  observeEvent(rb_handoff(), { rb_up_open(FALSE) }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Which document is open, said in the one line that replaces the panel. The
  # handoff path has no fileInput behind it, so fall back to the file's own name.
  output$rb_docname <- renderText({
    h <- rb_handoff()
    if (!is.null(h) && file.exists(h)) return(basename(h))
    as.character(input$ts_file$name %||% "the uploaded document")[1]
  })
  outputOptions(output, "rb_docname", suspendWhenHidden = FALSE)
  output$rb_has_draft <- reactive({ !is.null(rb$draft) })
  outputOptions(output, "rb_has_draft", suspendWhenHidden = FALSE)
  output$rb_has_vdraft <- reactive({ !is.null(rb$vdraft) })
  outputOptions(output, "rb_has_vdraft", suspendWhenHidden = FALSE)
  output$rb_col_sel <- reactive({ !is.na(rb$colsel) && !is.null(rb$draft) })
  outputOptions(output, "rb_col_sel", suspendWhenHidden = FALSE)

  rb_input <- reactive({
    p <- rb_doc(); req(p)
    tryCatch(read_input(p), error = function(e) NULL)
  })
  rb_n_pages <- reactive({
    i <- rb_input(); if (is.null(i)) return(NA_integer_)
    n <- .doc_npages(i)
    if (is.na(n) || n < 1L) NA_integer_ else n
  })
  # THE FRAME IS THE DOCUMENT'S OWN PAGE SIZE, not A4: the picture is drawn in the
  # page's point space and every box comes off that picture, so the number drawn
  # and the number saved are one number.
  # ...AND A SAVED TEMPLATE BRINGS ITS OWN. A template's bands are points in the
  # page size it was DRAWN on. Reading page 1 of whatever is open and stamping
  # that size onto the template left every band in the old space under a new
  # label, so opening an A4 template on a US Letter example to add table 41 moved
  # every box on the other forty by 11pt across and 41pt up - and nothing said so.
  # While a template is open its own frame is the frame, and the page is drawn
  # scaled into it, which is exactly what the reader does at conversion time.
  rb_frame <- reactive({
    f <- rb$frame
    if (is.list(f) && is.finite(.doc_num(f$ref_width, NA_real_)) &&
        is.finite(.doc_num(f$ref_height, NA_real_))) return(f)
    i <- rb_input()
    w <- suppressWarnings(as.numeric((i$page_width  %||% NA_real_)[1]))
    h <- suppressWarnings(as.numeric((i$page_height %||% NA_real_)[1]))
    list(ref_width  = if (is.na(w) || w <= 0) .A4_W else w,
         ref_height = if (is.na(h) || h <= 0) .A4_H else h)
  })
  # NOTHING SILENT. A document whose pages are a different size from the one the
  # template was drawn on is read scaled into the template's space - which is
  # right, and invisible, and exactly the thing somebody needs to be told before
  # they start nudging boxes that are not where they expect.
  output$rb_frame_note <- renderText({
    f <- rb$frame; if (!is.list(f)) return("")
    i <- rb_input(); if (is.null(i)) return("")
    w <- suppressWarnings(as.numeric((i$page_width  %||% NA_real_)[1]))
    h <- suppressWarnings(as.numeric((i$page_height %||% NA_real_)[1]))
    if (!is.finite(w) || !is.finite(h)) return("")
    if (abs(w - .doc_num(f$ref_width, w)) < PARAM_DOC_SAME_PAGE_PT &&
        abs(h - .doc_num(f$ref_height, h)) < PARAM_DOC_SAME_PAGE_PT) return("")
    "This document's pages are a different size from the one this template was drawn on, so the page is shown scaled to fit its boxes - the same way it will be read."
  })

  observe({
    n <- rb_n_pages(); if (is.na(n)) return()
    updateNumericInput(session, "rb_page", max = n,
      label = if (n == 1L) "Page (1 page)" else sprintf("Page (1 to %d)", n))
  })

  .rb_pg <- reactive(.clamp_page(input$rb_page, rb_n_pages()))
  .rb_box <- function(br, pg) list(page = as.integer(pg),
    x_min = round(br$xmin, 1), x_max = round(br$xmax, 1),
    y_min = round(max(br$ymin, 0), 1), y_max = round(max(br$ymax, 0), 1))
  .rb_read <- function(box) {
    w <- .doc_page_words(rb_input(), box$page, rb_frame())
    if (is.null(w)) return("")
    trimws(gsub("\\s+", " ", paste(.doc_box_words(w, box)$text, collapse = " ")))
  }
  .rb_col_at <- function(tab, x) {
    cols <- .doc_columns(tab)
    if (!length(cols) || !is.finite(x)) return(NA_integer_)
    hit <- which(vapply(cols, function(cc)
      x >= .doc_num(cc$x_min, -Inf) && x <= .doc_num(cc$x_max, Inf), logical(1)))
    if (length(hit)) as.integer(hit[1]) else NA_integer_
  }
  # ---- A COLUMN IN THE MIDDLE MOVES THE ONES BESIDE IT ----------------------
  #
  # "If I need to expand or contract a column in the middle of others, that
  # clashes where the auto boundaries are, the other columns NEED to adapt to it
  # rather than just do nothing. The auto column widths are sometimes too big and
  # or too small for the data, and quickly resizing the column that needs to be
  # bigger is SUPER hard - I've got to first collapse the larger one."
  #
  # That is exactly what it did: doc_set_column_band() clamps the moved column off
  # its neighbours and tells the person to "move the neighbour first", which on a
  # table of eight columns means seven moves to make one. A control that refuses
  # the obvious thing is worse than no control.
  #
  # THE RULE: the column goes where it was put, and everything in its way slides
  # along - recursively, keeping its width where there is room, down to a floor
  # where there is not, and never off the page (the wall). If the chain cannot
  # fit even at the floor, the request itself is trimmed to what the page can
  # hold. Nothing is ever silent: the count of columns that moved, the count that
  # had to be narrowed, and whether the request was trimmed all come back on the
  # answer.
  #
  # THE FLOOR ITSELF IS NOT A NUMBER IN THIS FILE. It decides where a figure
  # lands on a page, which makes it engine tuning, so the value lives in
  # R/params.R beside every other such decision and this is only the handle.
  .RB_MIN_COL <- PARAM_DOC_MIN_COL_PT
  #
  # THE ENGINE HALF IS NOT MINE. doc_set_column_band() in R/tables.R is still the
  # clamping setter and is still what every other caller uses; this is the same
  # decision made where the page width is known. See notes_for_next.
  .rb_move_band <- function(cols, j, x0, x1, page_w = NA_real_) {
    old <- if (is.list(cols) && !is.null(cols$columns)) .doc_columns(cols) else (cols %||% list())
    j <- .doc_int(j)
    n <- length(old)
    if (is.na(j) || j < 1L || j > n) return(old)
    W <- .doc_num(page_w, NA_real_)
    if (!is.finite(W) || W <= 0) W <- Inf
    lo <- suppressWarnings(min(x0, x1)); hi <- suppressWarnings(max(x0, x1))
    if (!isTRUE(is.finite(lo)) || !isTRUE(is.finite(hi))) return(old)
    lo <- max(0, lo); hi <- min(W, hi)
    # LEFT TO RIGHT, or "the one beside it" means nothing. Everything that builds
    # a column list sorts it, but a template written by hand need not have, and
    # pushing along an unsorted list would move the wrong neighbours.
    ord <- order(vapply(old, function(cc) .doc_num(cc$x_min, Inf), numeric(1)))
    if (!identical(as.integer(ord), seq_len(n))) {
      old <- old[ord]; j <- match(j, ord)
      if (is.na(j)) return(cols)
    }
    # Room the neighbours must keep on each side, at the floor.
    need_r <- (n - j) * .RB_MIN_COL
    need_l <- (j - 1L) * .RB_MIN_COL
    trimmed <- FALSE
    if (is.finite(W) && hi > W - need_r) { hi <- W - need_r; trimmed <- TRUE }
    if (lo < need_l) { lo <- need_l; trimmed <- TRUE }
    if (hi - lo < .RB_MIN_COL) return(old)
    b <- cbind(vapply(old, function(cc) .doc_num(cc$x_min, NA_real_), numeric(1)),
               vapply(old, function(cc) .doc_num(cc$x_max, NA_real_), numeric(1)))
    if (anyNA(b)) return(old)
    b[j, ] <- c(lo, hi)
    pushed <- 0L; narrowed <- 0L
    if (n > j) for (k in (j + 1L):n) {
      if (b[k, 1] >= b[k - 1L, 2] - 1e-9) break     # the chain stops at the first gap
      w <- b[k, 2] - b[k, 1]
      nlo <- b[k - 1L, 2]
      nhi <- min(nlo + w, if (is.finite(W)) W - (n - k) * .RB_MIN_COL else nlo + w)
      if (nhi - nlo < .RB_MIN_COL) nhi <- nlo + .RB_MIN_COL
      if (nhi - nlo < w - 1e-9) narrowed <- narrowed + 1L
      b[k, ] <- c(nlo, nhi); pushed <- pushed + 1L
    }
    if (j > 1L) for (k in (j - 1L):1L) {
      if (b[k, 2] <= b[k + 1L, 1] + 1e-9) break
      w <- b[k, 2] - b[k, 1]
      nhi <- b[k + 1L, 1]
      nlo <- max(nhi - w, (k - 1L) * .RB_MIN_COL)
      if (nhi - nlo < .RB_MIN_COL) nlo <- max(0, nhi - .RB_MIN_COL)
      if (nhi - nlo < w - 1e-9) narrowed <- narrowed + 1L
      b[k, ] <- c(nlo, nhi); pushed <- pushed + 1L
    }
    for (k in seq_len(n)) {
      old[[k]]$x_min <- round(b[k, 1], 2); old[[k]]$x_max <- round(b[k, 2], 2)
    }
    attr(old, "pushed") <- pushed
    attr(old, "narrowed") <- narrowed
    attr(old, "trimmed") <- trimmed
    old
  }
  # .rb_push_msg(cols) -- one sentence about what moving a column did to the ones
  # beside it, or "" when it touched nothing. Rule: if the tool moved something
  # nobody pointed at, it says so.
  .rb_push_msg <- function(cols) {
    p <- .doc_int(attr(cols, "pushed"), 0L)
    nw <- .doc_int(attr(cols, "narrowed"), 0L)
    if (isTRUE(attr(cols, "trimmed")))
      return(paste("Moved as far as the page allows - the columns beside it were",
                   "already down to their narrowest."))
    if (p < 1L) return("")
    sprintf("Moved. %d column%s beside it moved along%s.",
            p, if (p == 1L) "" else "s",
            if (nw > 0L) sprintf(", and %d had to be narrowed to fit", nw) else "")
  }

  # The name for ONE band, read from the table's own header row -- so a column
  # somebody adds by hand arrives called what the page calls it, the same way the
  # derived ones do. Two edges is a one-column table as far as the reader of the
  # header row is concerned, so this is doc_header_names() asked a narrow
  # question.
  .rb_name_for_band <- function(i, d, fr, x0, x1) {
    nm <- doc_header_names(i, d, fr, c(min(x0, x1), max(x0, x1)))
    if (length(nm) && nzchar(trimws(nm[1]))) trimws(nm[1]) else ""
  }

  # .rb_default_bottom(d) -- THE BOTTOM EDGE OF THE PAGES IN BETWEEN, ANSWERED
  # RATHER THAN ASKED.
  #
  # "The bottom edge on pages in between needs to default show up. Shouldn't need
  # to work out end for me." It did not show up: band$y_max was never written by
  # anything, so on every page but the last the reader ran the table to the
  # bottom of the paper and read the footer, the source line and the page number
  # in as rows. The person who set the END correctly had no way to see why.
  #
  # The tool already knows the answer and it does not need a new rule to find it:
  # open the table's own start page right to the bottom and ask the READER where
  # its last row is, with the same stop rule doc_auto_end uses between pages. So
  # the default is not a guess about this document, it is a measurement of it -
  # and it is drawn on the page and moved with one click, because a default that
  # cannot be seen is the same silent assumption in a different place.
  #
  # It only ever FILLS IN a blank. A bottom edge somebody has set is never
  # touched, and a one-page table has no pages in between to have one.
  # (Register D4.)
  .rb_default_bottom <- function(d) {
    if (is.null(d)) return(d)
    p0 <- .doc_int(d$start$page, 1L); p1 <- .doc_int(d$end$page, p0)
    if (is.na(p0) || is.na(p1) || p1 <= p0) return(d)
    b <- d$band %||% list()
    if (is.finite(.doc_num(b$y_max, NA_real_))) return(d)
    i <- rb_input(); if (is.null(i)) return(d)
    fr <- rb_frame()
    probe <- d
    probe$follow <- FALSE
    probe$end <- list(page = as.integer(p0), y = fr$ref_height)
    r <- tryCatch(doc_table_rows(i, probe, fr), error = function(e) NULL)
    y <- if (is.null(r)) NA_real_ else suppressWarnings(as.numeric(r$last_y))
    if (!isTRUE(is.finite(y)) || y <= .doc_num(d$start$y, 0)) return(d)
    b$y_max <- round(y + 2, 1)
    d$band <- b
    d
  }

  # .rb_next_step(done, d) -- the step after `done`, or "" to stop.
  #
  # WHERE IT STARTS AND WHERE IT STOPS ARE STEPS, NOT SETTINGS. They decide every
  # row that comes out, and they were worked out by the tool and shown in a panel
  # a person had to notice, then press "Move it" on. So a table with a footer or a
  # source line under it read those in as rows, and the screen never once asked
  # about it. Now the same sentence-at-a-time guide that gets the columns gets
  # these: columns -> starts -> ends -> (if it runs over pages) the bottom edge of
  # the pages in between. Each one skippable, because the tool's guess is usually
  # right and a guide you cannot leave is a wizard.
  .rb_next_step <- function(done, d) {
    if (!isTRUE(rb$guide)) return("")
    if (identical(done, "cols")) return("start")
    if (identical(done, "start")) return("end")
    if (identical(done, "end")) {
      p0 <- .doc_int(d$start$page, 1L); p1 <- .doc_int(d$end$page, p0)
      if (!is.na(p0) && !is.na(p1) && p1 > p0) return("bottom")
    }
    rb$guide <- FALSE
    ""
  }

  # .rb_subject() -- WHICH THING the armed gesture is about, in the words on the
  # screen. Pressing Edit on a column arms the drag that moves it, so the banner
  # has to say WHICH column, or "drag a box over where this column should be" is
  # a sentence about a column the reader has to guess at. The picture picks the
  # same one out in the same colour; this is the half you can read.
  .rb_subject <- function() {
    m <- as.character(rb$mode %||% "")
    if (m %in% c("colpos", "addcol")) {
      cols <- .doc_columns(rb$draft); j <- rb$colsel
      if (m == "colpos" && !is.na(j) && length(cols) && j <= length(cols))
        return(trimws(as.character(cols[[j]]$name %||% sprintf("column %d", j))[1]))
      return("")
    }
    if (m %in% c("label", "value")) {
      v <- rb$vdraft
      nm <- trimws(as.character(v$name %||% "")[1])
      if (nzchar(nm)) return(nm)
      return(trimws(as.character(v$label_text %||% "")[1]))
    }
    ""
  }

  # .rb_about() -- TABLE, COLUMN, VALUE or DOCUMENT: which of the four things on
  # this screen the armed gesture is about.
  #
  # "Clear distinction between when I'm editing columns, tables etc." Every
  # sentence in .RB_ASK names the thing in capitals, but they are nine different
  # sentences and the distinction has to be read out of the wording each time.
  # This is the same fact as one word, in the same place, every time. (Register D6.)
  .rb_about <- function(m) switch(as.character(m %||% ""),
    title = , cols = , start = , end = , bottom = "TABLE",
    addcol = , colpos = "COLUMN",
    label = , value = "VALUE",
    phrase = "DOCUMENT",
    "")

  # ---- The banner: the one thing the screen is waiting for -------------------
  output$rb_arm <- renderUI({
    a <- .RB_ASK[[rb$mode %||% ""]]
    if (is.null(a)) return(div(style = paste(
      "display:flex;align-items:center;gap:10px;padding:8px 12px;margin:0 0 10px;",
      "border-radius:6px;background:#f2f2f2;border-left:4px solid #cccccc"),
      div(style = "flex:1;font-size:13px;color:#555555",
        "Nothing is waiting for a drag. Nothing you drag on the page will change anything.")))
    m <- as.character(rb$mode %||% "")
    subj <- .rb_subject()
    div(style = paste(
      "display:flex;align-items:center;gap:12px;padding:10px 14px;margin:0 0 10px;",
      "border-radius:6px;background:#fff4d6;border-left:5px solid #b7791f"),
      div(style = "font-size:20px;line-height:1", "\u270e"),
      div(style = "flex:1;min-width:0",
        # WHICH OF THE FOUR THINGS THIS IS ABOUT, first, always.
        if (nzchar(.rb_about(m))) div(style = paste(
          "font-size:11px;font-weight:700;letter-spacing:1px;color:#8a6a2a;",
          "margin-bottom:2px"), .rb_about(m)),
        div(style = "font-size:16px;font-weight:700;color:#7a4f00", a[1]),
        div(style = "font-size:12px;color:#8a6a2a;margin-top:2px", a[2]),
        if (nzchar(subj)) div(style = "font-size:13px;color:#7a4f00;margin-top:4px",
          "You are editing ", strong(subj), " - it is outlined on the page.")),
      # A step you cannot leave is a wizard. Every step the guide arms by itself
      # has the answer the tool already worked out sitting behind one button.
      actionButton("rb_disarm", "Cancel", class = "btn-default btn-sm"),
      switch(m,
        title  = actionButton("rb_skip_title", "Skip - it has no title",
                              class = "btn-default btn-sm"),
        start  = actionButton("rb_skip_step", "Skip - it starts under the headings",
                              class = "btn-default btn-sm"),
        end    = actionButton("rb_skip_step", "Skip - use the end it worked out",
                              class = "btn-default btn-sm"),
        bottom = actionButton("rb_skip_step", "Skip - it runs to the bottom of the page",
                              class = "btn-default btn-sm"),
        # Editing a value: one drag re-draws the figure, the other the wording.
        # Both are one press away from each other, because which one is wrong is
        # not something the screen can know.
        value  = actionButton("rb_arm_label", "Re-draw the LABEL instead",
                              class = "btn-default btn-sm"),
        label  = actionButton("rb_arm_value", "Re-draw the VALUE instead",
                              class = "btn-default btn-sm"),
        NULL))
  })
  observeEvent(input$rb_arm_label, { if (!is.null(rb$vdraft)) rb$mode <- "label" })
  observeEvent(input$rb_arm_value, { if (!is.null(rb$vdraft)) rb$mode <- "value" })
  # THE FINGERPRINT, DRAGGED. It needs a page and nothing else -- it is not part
  # of a table or a value, so it interrupts neither and arms on its own.
  observeEvent(input$rb_arm_phrase, {
    if (is.null(rb_doc())) {
      showNotification("Upload the document at the top of this page first.",
                       type = "warning", duration = 6); return()
    }
    rb$guide <- FALSE
    rb$mode <- "phrase"
  })
  # ARMING OR CANCELLING CLEARS THE PAGE. A new question deserves a page with no
  # leftover answer drawn on it, and Cancel that leaves the rectangle behind is
  # a Cancel that visibly did not cancel.
  observeEvent(rb$mode, { session$resetBrush("rb_brush") }, ignoreInit = TRUE)
  # Cancel leaves the guide as well as the step: somebody who cancels is steering.
  observeEvent(input$rb_disarm, { rb$guide <- FALSE; rb$mode <- "" })
  # Skip keeps what the tool worked out and moves on to the next step.
  observeEvent(input$rb_skip_step, {
    m <- as.character(rb$mode %||% "")
    rb$mode <- .rb_next_step(m, rb$draft)
  })

  # ---- Loading a draft: the ONLY place the static inputs are filled in -------
  # Filling them from an observer on the draft would fight the person typing into
  # them; filling them here means they are set when a draft ARRIVES and left alone
  # while it is being edited.
  # .rb_pin_frame() -- THE SPACE THE BOXES LIVE IN, FIXED THE MOMENT THE FIRST
  # ONE IS DRAWN. A template that has been saved brings its frame back with it
  # (rb_open_template); one being drawn for the first time has to pin its own, or
  # loading a second example of a different page size moves every box already on
  # screen into a space it was never drawn in - H4 again, on the half of the job
  # that has not been saved yet. No-op once it is set.
  .rb_pin_frame <- function() {
    if (!is.null(rb$frame)) return(invisible(NULL))
    # rb_frame() reaches rb_input(), which req()s a document - and a req() inside
    # an observer aborts the whole observer, silently. Pinning the frame must
    # never be the reason a table failed to open.
    f <- tryCatch(rb_frame(), error = function(e) NULL)
    if (is.list(f)) rb$frame <- f
    invisible(NULL)
  }
  .rb_load_draft <- function(d) {
    .rb_pin_frame()
    rb$draft <- d
    rb$colsel <- NA_integer_
    # A DRAFT IS A NEW TABLE UNLESS THE CALLER SAYS OTHERWISE. Only the Edit
    # handler sets rb$edit_idx, and it sets it AFTER this call - so a draft that
    # arrives any other way can never overwrite a saved table on Save.
    rb$edit_idx <- NA_integer_
    updateTextInput(session, "rb_name", value = as.character(d$name %||% ""))
    updateRadioButtons(session, "rb_hdr",
      selected = if (.doc_int(d$header_rows, 1L) == 0L) "data" else "head")
    updateNumericInput(session, "rb_hdrn", value = max(1L, .doc_int(d$header_rows, 1L)))
    .rb_push_where(d)
    invisible(NULL)
  }
  # .rb_push_where(d) -- the four position boxes, filled from the draft. Called
  # when a draft arrives and after every gesture that moves a boundary, never on
  # a timer: the boxes are what a person types into.
  .rb_push_where <- function(d) {
    if (is.null(d)) return(invisible(NULL))
    updateNumericInput(session, "rb_sp", value = .doc_int(d$start$page, 1L))
    updateNumericInput(session, "rb_sy", value = round(.doc_num(d$start$y, 0), 1))
    updateNumericInput(session, "rb_ep", value = .doc_int(d$end$page, .doc_int(d$start$page, 1L)))
    updateNumericInput(session, "rb_ey", value = round(.doc_num(d$end$y, 0), 1))
    invisible(NULL)
  }
  .rb_load_vdraft <- function(v) {
    .rb_pin_frame()
    rb$vdraft <- v
    updateTextInput(session, "rb_vname", value = as.character(v$name %||% ""))
    updateSelectInput(session, "rb_vtype", selected = as.character(v$type %||% "text"))
    rel <- .doc_key(v, "where")
    wh <- if (is.list(rel)) as.character(rel$where %||% "right")[1]
          else if (is.character(rel) && nzchar(rel[1])) rel[1] else "right"
    updateSelectInput(session, "rb_vwhere", selected = wh)
    invisible(NULL)
  }

  observeEvent(input$rb_addtable, {
    if (is.null(rb_input())) return()
    rb$guide <- TRUE
    rb$mode <- "title"
  })
  observeEvent(input$rb_addval, {
    if (is.null(rb_input())) return()
    .rb_load_vdraft(list(name = "", label_text = "", label = NULL, value = NULL,
                         read = "", type = "text"))
    rb$mode <- "label"
  })
  observeEvent(input$rb_skip_title, {
    fr <- rb_frame(); pg <- .rb_pg()
    .rb_load_draft(list(name = sprintf("Table on page %d", pg), title = NULL,
                        start = list(page = as.integer(pg), y = 0),
                        end = list(page = as.integer(pg), y = fr$ref_height),
                        header_rows = 1L, follow = FALSE, min_fill = 0.5,
                        columns = list(),
                        anchor = list(header_text = list(), first_column = list())))
    rb$mode <- "cols"
  })

  # ---- The drag: whatever is armed, and nothing otherwise --------------------
  # THE BLUE BOX MUST ALWAYS GO AWAY, AND IT MUST GO AWAY FIRST.
  #
  # Reported: "click and drag just stopped working. Cancelled out, created the
  # table again, RELOADED THE PAGE, still no drag. The blue box appeared and does
  # not disappear even when the next column is added."
  #
  # observeEvent fires on CHANGE. session$resetBrush() is what sets the value back
  # to NULL so the next drag is a change -- and it used to sit BELOW three early
  # returns and two calls that can throw (.rb_pg, rb_frame, .rb_box). Any of those
  # leaves input$rb_brush holding the old rectangle, and then:
  #   * the rectangle stays drawn on the page, over everything drawn after it;
  #   * an identical drag sends an identical value, which is not a change, so
  #     THE OBSERVER NEVER FIRES AGAIN -- "drag stopped working";
  #   * and it survives a reload, because the stale value is on the server.
  # Which is exactly the sequence reported, in order.
  #
  # So it is the first statement in the observer, before anything that can return
  # or throw, and the rest of the body is wrapped so an error cannot leave the
  # screen stuck either. A drag that fails must fail as a message, never as a
  # rectangle nobody can get rid of.
  observeEvent(input$rb_brush, {
    br <- input$rb_brush
    session$resetBrush("rb_brush")
    if (is.null(br)) return()
    # ...the tail of a click. Any CORNER of the box within a few points of where
    # a boundary click just landed means this drag and that click were one
    # gesture; the click already did the job.
    ca <- rb$click_at
    if (is.list(ca) && is.finite(ca$t %||% NA) &&
        as.numeric(Sys.time()) - ca$t < 2.5) {
      near <- function(a, b) isTRUE(abs(a - b) <= 8)
      corners <- list(c(br$xmin, br$ymin), c(br$xmin, br$ymax),
                      c(br$xmax, br$ymin), c(br$xmax, br$ymax))
      if (any(vapply(corners, function(p)
              near(p[1], ca$x) && near(p[2], ca$y), logical(1)))) {
        rb$click_at <- NULL
        return()
      }
    }
    i <- rb_input(); if (is.null(i)) return()
    m <- as.character(rb$mode %||% "")
    pg <- .rb_pg(); fr <- rb_frame()
    box <- .rb_box(br, pg)

    # THE FIX FOR "every time I drag it just creates a new table". A drag is a
    # way of answering a question, and when nothing has asked one it is a way of
    # looking at the page.
    if (!nzchar(m)) {
      # SAY WHAT THEY WERE PROBABLY TRYING TO DO. "Nothing was waiting" is true
      # and useless to somebody who has just pressed Edit on a column and dragged
      # where they want it: the button they still have to press is named nowhere
      # in that sentence. Reported as "I click edit, and drag where I actually
      # want the column, and it says nothing was waiting".
      d <- rb$draft; j <- rb$colsel
      cols <- .doc_columns(d)
      sel <- if (!is.null(d) && !is.na(j) && j <= length(cols))
               as.character(cols[[j]]$name %||% sprintf("column %d", j))[1] else ""
      showNotification(
        if (nzchar(sel))
          sprintf(paste("Nothing was waiting for that drag, so nothing changed. To put",
                        "\u201c%s\u201d there, press Edit beside it in the column list",
                        "first - then drag."), sel)
        else paste("Nothing was waiting for that drag, so nothing changed.",
                   "Press \u201c+ Add a table\u201d or \u201c+ Add a value\u201d first."),
        type = "message", duration = 9)
      return()
    }
    if (box$x_max - box$x_min < 2 || box$y_max - box$y_min < 2) {
      showNotification("That box was too small to read anything from.",
                       type = "warning", duration = 5)
      return()
    }

    if (m == "title") {
      txt <- .rb_read(box)
      if (!nzchar(txt)) {
        showNotification("Nothing readable in that box - drag round the table's heading.",
                         type = "warning", duration = 6); return()
      }
      .rb_load_draft(list(name = txt, title = box,
                          start = list(page = as.integer(pg), y = box$y_max),
                          end = list(page = as.integer(pg), y = fr$ref_height),
                          header_rows = 1L, follow = FALSE, min_fill = 0.5,
                          columns = list(),
                          anchor = list(header_text = list(), first_column = list())))
      rb$mode <- "cols"
      return()
    }

    if (m == "cols") {
      d <- rb$draft; if (is.null(d)) { rb$mode <- ""; return() }
      cols <- tryCatch(doc_columns_from_box(i, box, fr), error = function(e) list())
      if (length(cols) < 1L) {
        showNotification("No column names readable in that box - drag round the row of headings.",
                         type = "warning", duration = 7); return()
      }
      d$columns <- cols
      d$start <- list(page = as.integer(pg), y = box$y_min)
      # THE HEADING IS AS TALL AS THE BOX THAT WAS DRAWN ROUND IT. A person
      # dragging round a three-line block of column names has said the block is
      # the heading; skipping only its first line puts the other two in the
      # table as data. (Seen on a real report: "Medical &" and "Public Health"
      # arrived as rows 1 and 2 of the figures.)
      #
      # COUNTED IN THE READER'S OWN LINE GROUPING, not the drag box's. The reader
      # learns its row spacing from the whole table, where the data rows are
      # further apart than the heading's baselines are -- so six printed lines of
      # heading can be three rows to it. A count measured any other way is a
      # count of something the reader never sees, and skipping six of ITS rows
      # eats the first three rows of figures.
      wpg <- .doc_page_words(i, pg, fr)
      nlines <- if (is.null(wpg)) 1L else {
        below <- wpg[wpg$cy >= box$y_min, , drop = FALSE]
        lns <- .doc_lines(below, d$row_tol)
        sum(vapply(lns, function(l)
          as.numeric(attr(l, "top")) <= box$y_max + 1, logical(1)))
      }
      d$header_rows <- max(1L, min(8L, as.integer(nlines)))
      d$anchor$header_text <- as.list(vapply(cols, function(cc) cc$name, character(1)))
      d$end <- tryCatch(doc_auto_end(i, d, fr), error = function(e) d$end)
      # THE DIVISIONS COME FROM ALL THE INK IN THE TABLE, NOT FROM THE BOX THAT
      # WAS DRAWN ROUND THE HEADINGS.
      #
      # Reported: "when I define columns, but one of the columns doesn't have a
      # first row of data, it pulls all info in the last column into the second
      # to last column". The drag box names the columns and says where the
      # heading is; a column that prints nothing inside that sample yields no
      # band, and because the surviving bands tile, its whole territory belongs
      # to a neighbour - silently, because a tiled column claims every word and
      # nothing is left unclaimed to complain about.
      #
      # Now the end is known, so the whole table is available to derive from.
      # doc_fit_columns can only ever ADD a column and keeps a drawn column with
      # no ink in it exactly as drawn (R/tables.R), which is what makes this safe
      # to run over a set somebody has just named. It runs ONCE, here, at the
      # moment the columns are first derived - never again, so a band that has
      # since been drawn or moved by hand is never re-derived. (Findings A3.)
      fit <- tryCatch(doc_fit_columns(i, d, fr), error = function(e) list())
      gained <- max(0L, length(fit) - length(cols))
      if (length(fit) >= length(cols)) {
        d$columns <- fit; cols <- fit
        d$anchor$header_text <- as.list(vapply(cols, function(cc)
          as.character(cc$name %||% "")[1], character(1)))
      }
      # The bottom edge of the pages in between, measured after the columns are
      # settled - the reader uses them to decide what is a row. (Register D4.)
      d <- .rb_default_bottom(d)
      rb$draft <- d
      .rb_push_where(d)
      rb$mode <- .rb_next_step("cols", d)
      updateRadioButtons(session, "rb_hdr", selected = "head")
      updateNumericInput(session, "rb_hdrn", value = .doc_int(d$header_rows, 1L))
      # ONE COLUMN IS ALMOST NEVER WHAT A DRAG ROUND "THE COLUMN NAMES" MEANT.
      #
      # A box with one unbroken run of words in it -- a sentence, the table's
      # title, a wrapped heading -- has one gutter-free band in it, so it makes
      # one column, and every row then arrives with the whole row in a single
      # cell. That is correct and it is also, nine times in ten, a box drawn
      # round the wrong thing. It cost five of fifteen documents in a run over
      # somebody else's PDFs, and nothing on screen said why.
      if (length(cols) == 1L) {
        showNotification(paste("That box holds one unbroken run of words, so this table has",
                               "ONE column and each row arrives whole in it. If you meant a",
                               "row of column headings, drag round the headings themselves.",
                               "If you did mean one column, carry on."),
                         type = "warning", duration = 12)
      } else {
        # NOTHING SILENT. Reading the whole table can find a column the heading
        # row alone did not show - one that prints nothing until page 2, or one
        # whose first row is blank. That is a correction to what was just drawn,
        # so it is said, once, here.
        # What it found, and the correction if it made one. The old third clause
        # ("Everything below can be changed - names, edges, where it starts and
        # stops") described the panel it is printed over.
        showNotification(sprintf("%d columns, ending on page %d.%s",
                                 length(cols), .doc_int(d$end$page, pg),
                                 if (gained > 0L)
                                   sprintf(paste(" %d of them %s found by reading the whole table,",
                                                 "not the heading row."),
                                           gained, if (gained == 1L) "was" else "were")
                                 else ""),
                         type = "message", duration = 8)
      }
      return()
    }

    if (m == "addcol") {
      d <- rb$draft; if (is.null(d)) { rb$mode <- ""; return() }
      before <- length(.doc_columns(d))
      # ONE NEW COLUMN, EXACTLY WHERE IT WAS DRAWN, and nothing else touched.
      # Columns may have whitespace between them; only an overlap is impossible.
      # R/tables.R carries the reasoning and the two things this used to do
      # instead.
      nm <- tryCatch(.rb_name_for_band(i, d, fr, box$x_min, box$x_max),
                     error = function(e2) "")
      cols <- doc_add_column(.doc_columns(d), box$x_min, box$x_max, names = nm)
      clamped <- isTRUE(attr(cols, "clamped"))
      if (length(cols) == before) {
        showNotification(paste("That box is inside a column that is already there, so there is",
                               "no room for a new one. Drag over open space, or press Edit on",
                               "that column and move it first."),
                         type = "warning", duration = 9)
        rb$mode <- ""; return()
      }
      d$columns <- cols
      rb$draft <- d
      rb$mode <- ""
      # Select what the drag just made, so naming it is the obvious next thing.
      rb$colsel <- .rb_col_at(d, (box$x_min + box$x_max) / 2)
      rb$colver <- rb$colver + 1L
      if (clamped)
        showNotification(paste("The new column is in. It was trimmed where it ran into the column",
                               "beside it - two columns cannot overlap, because anything in the",
                               "overlap is read into one and lost from the other."),
                         type = "message", duration = 9)
      return()
    }

    if (m == "colpos") {
      d <- rb$draft; j <- rb$colsel
      if (is.null(d) || is.na(j)) { rb$mode <- ""; return() }
      old <- .doc_columns(d)
      if (!length(old) || j > length(old)) { rb$mode <- ""; return() }
      # THIS COLUMN GOES WHERE IT WAS PUT, AND WHAT IS IN THE WAY MOVES ALONG.
      # WAS: doc_set_column_band(), which clamped it at its neighbour and said
      # "move the neighbour first" - the refusal register D5 is about. The name
      # is still the person's, not the header row's: moving a column is not a
      # request to rename it back to whatever is printed above its new position.
      keep_nm <- as.character(old[[j]]$name %||% "")[1]
      cols <- .rb_move_band(old, j, box$x_min, box$x_max, fr$ref_width)
      note <- .rb_push_msg(cols)
      d$columns <- cols
      rb$draft <- d
      jj <- .rb_col_at(list(columns = cols), (box$x_min + box$x_max) / 2)
      rb$colsel <- if (is.na(jj)) j else jj
      rb$colver <- rb$colver + 1L
      rb$mode <- ""
      if (nzchar(note)) showNotification(note, type = "message", duration = 8)
      return()
    }

    # THE FINGERPRINT PHRASE, TAKEN OFF THE PAGE WORD FOR WORD.
    #
    # Matching is exact, so the difference between this and typing it is the
    # difference between a template that recognises the document and one that
    # never fires. Added to what is already there rather than replacing it: a
    # fingerprint is a LIST, all of which must appear.
    if (m == "phrase") {
      txt <- .rb_read(box)
      if (!nzchar(txt)) {
        showNotification("Nothing readable in that box - drag round some printed words.",
                         type = "warning", duration = 6); return()
      }
      have <- trimws(strsplit(as.character(input$rb_fp %||% ""), "\n")[[1]])
      have <- have[nzchar(have)]
      if (txt %in% have) {
        showNotification(sprintf("\u201c%s\u201d is already one of the phrases.", txt),
                         type = "message", duration = 6)
        rb$mode <- ""; return()
      }
      updateTextAreaInput(session, "rb_fp", value = paste(c(have, txt), collapse = "\n"))
      # It is HERS now, so the two observers that suggest a fingerprint stop.
      rb_fp_auto(NA_character_)
      rb$mode <- ""
      # A phrase every document carries is the one fault that turns a correct
      # "no template read this" into a confident wrong read, and the loader
      # refuses it at save time. Saying so now costs nothing and saves the trip.
      n_words <- length(strsplit(txt, "\\s+")[[1]])
      showNotification(
        if (n_words >= 2L || nchar(txt) >= 10L)
          sprintf("Added \u201c%s\u201d.", txt)
        else
          sprintf(paste("Added \u201c%s\u201d - but one short word is printed on thousands of",
                        "documents. Drag a heading as well, or this template will match",
                        "things it has never seen."), txt),
        type = if (n_words >= 2L || nchar(txt) >= 10L) "message" else "warning",
        duration = if (n_words >= 2L || nchar(txt) >= 10L) 5 else 10)
      return()
    }

    if (m == "label") {
      txt <- .rb_read(box)
      if (!nzchar(txt)) {
        showNotification("Nothing readable in that box - drag round the label's words.",
                         type = "warning", duration = 6); return()
      }
      v <- rb$vdraft %||% list(name = "", type = "text", read = "")
      v$label <- box
      v$label_text <- txt
      if (!nzchar(trimws(as.character(v$name %||% "")))) v$name <- doc_suggest_name(txt)
      # NO GUESSING AT THE VALUE. A guess that is right nine times in ten is a
      # habit of not checking, and the tenth is wrong in a file nobody re-reads.
      # The second drag is asked for, out loud.
      if (is.list(v$value)) {
        v$where <- .doc_pair_rel(v$label, v$value)
        .rb_load_vdraft(v); rb$mode <- ""
      } else {
        .rb_load_vdraft(v); rb$mode <- "value"
      }
      return()
    }

    if (m == "value") {
      v <- rb$vdraft; if (is.null(v)) { rb$mode <- ""; return() }
      txt <- .rb_read(box)
      if (!nzchar(txt)) {
        showNotification("Nothing readable in that box - drag round the value itself.",
                         type = "warning", duration = 6); return()
      }
      v$value <- box
      v$read <- txt
      v$type <- .doc_value_kind(txt)
      if (is.list(v$label)) v$where <- .doc_pair_rel(v$label, box)
      .rb_load_vdraft(v)
      rb$mode <- ""
      return()
    }
  })

  # ---- The click: only ever moves the start or the end ----------------------
  observeEvent(input$rb_click, {
    cl <- input$rb_click; if (is.null(cl)) return()
    # A CLICK MEANS "I AM DOING SOMETHING ELSE NOW", so any rectangle still on the
    # page goes with it. Reported: the box persisted while trying to set the
    # bottom edge -- the percentage updated but the picture kept the old
    # selection, so the screen showed two different answers at once.
    session$resetBrush("rb_brush")
    m <- as.character(rb$mode %||% "")
    if (!(m %in% c("start", "end", "bottom"))) return()
    d <- rb$draft; if (is.null(d)) { rb$mode <- ""; return() }
    pg <- .rb_pg(); y <- round(max(as.numeric(cl$y), 0), 1)
    if (m == "start") {
      p1 <- .doc_int(d$end$page, pg)
      if (pg > p1 || (pg == p1 && y >= .doc_num(d$end$y, Inf))) {
        showNotification("That is at or past where the table ends - not applied.",
                         type = "warning", duration = 6); return()
      }
      d$start <- list(page = as.integer(pg), y = y)
    } else if (m == "end") {
      p0 <- .doc_int(d$start$page, pg)
      if (pg < p0 || (pg == p0 && y <= .doc_num(d$start$y, 0))) {
        showNotification("That is at or above where the table starts - not applied.",
                         type = "warning", duration = 6); return()
      }
      d$end <- list(page = as.integer(pg), y = y)
      # An end on a later page means there are now pages in between, and they
      # need a bottom edge. Filled in, never asked for. (Register D4.)
      d <- .rb_default_bottom(d)
    } else {
      # THE BOTTOM EDGE OF EVERY PAGE BUT THE LAST. Stored on the table's band,
      # which is what doc_locate_table() uses to close a continuation page's
      # window (R/tables.R). Refused above the top of the band, because a bottom
      # above the top is not a table.
      b <- d$band %||% list()
      top <- .doc_num(b$y_min, 0)
      if (y <= top + 2) {
        showNotification("That is at or above the top of the table - not applied.",
                         type = "warning", duration = 6); return()
      }
      b$y_max <- y; d$band <- b
    }
    rb$draft <- d
    .rb_push_where(d)
    # A DRAG THAT SHOULD HAVE BEEN A CLICK IS STILL A CLICK.
    #
    # Reported: "lots of the click-and-drags that should be clicks -- users will
    # do this, we MUST account for it -- persist and seem to block other
    # activity." They do, and the reason is in Shiny, not here: with no dblclick
    # id the click is sent on MOUSEDOWN (shiny.js createClickInfo), so a drag
    # sends the click at the press point first and the brush at mouseup second.
    # The click sets the boundary -- correctly, at the point they pressed -- and
    # then the brush arrives against the NEXT step and answers "that box was too
    # small", or worse, acts on it.
    #
    # So the click records itself, and a brush that starts at the same point
    # moments later is the tail of the same gesture and is swallowed in silence.
    # Nothing to explain, because from the person's side nothing went wrong: they
    # dragged where they wanted the line and the line is there.
    rb$click_at <- list(t = as.numeric(Sys.time()), x = as.numeric(cl$x),
                        y = as.numeric(cl$y))
    rb$mode <- .rb_next_step(m, d)
  })

  # ---- The table being worked on --------------------------------------------
  # WHAT THIS PANEL IS ABOUT, in one line, at the top of it: which table, whether
  # it is new or a copy of a saved one, and the two answers the tool worked out
  # for itself (how tall the heading is, and how many columns). The heading count
  # is here because the controls that change it are now folded away, and an
  # answer you cannot see is an answer you cannot check. (Register D2 and D6.)
  output$rb_step <- renderUI({
    d <- rb$draft; if (is.null(d)) return(NULL)
    p0 <- .doc_int(d$start$page, 1L); p1 <- .doc_int(d$end$page, p0)
    nc <- length(.doc_columns(d))
    hr <- .doc_int(d$header_rows, 1L)
    idx <- .doc_int(rb$edit_idx, NA_integer_)
    editing <- !is.na(idx) && idx >= 1L && idx <= length(rb$tables)
    div(style = "margin:0 0 8px;font-size:13px;color:#555555",
      div(style = "font-size:11px;letter-spacing:.6px;color:#7a4f00;font-weight:700",
          "THIS TABLE"),
      strong(if (p1 > p0) sprintf("Pages %d to %d", p0, p1) else sprintf("Page %d", p0)),
      sprintf(" \u00b7 %d column%s \u00b7 %s",
              nc, if (nc == 1L) "" else "s",
              if (hr < 1L) "its top row is data"
              else if (hr == 1L) "its top row names the columns"
              else sprintf("its top %d rows name the columns", hr)),
      div(class = "muted", style = "font-size:12px",
          if (editing)
            # Cancel's own button says what Cancel does, a few inches below and
            # rendered with the true words for the case in hand.
            sprintf("A copy of the saved table \u201c%s\u201d \u00b7 Save puts it back.",
                    as.character(rb$tables[[idx]]$name %||% "")[1])
          else "Not saved yet."))
  })
  output$rb_ncols <- renderText({
    n <- length(.doc_columns(rb$draft))
    if (n == 1L) "Columns (1)" else sprintf("Columns (%d)", n)
  })

  # WHERE IT STARTS AND WHERE IT STOPS, in words, with a button beside each.
  # These two numbers decide every row that comes out, and they used to be a
  # sentence in a hint and an undocumented click on the picture.
  output$rb_where <- renderUI({
    d <- rb$draft; if (is.null(d)) return(NULL)
    fr <- rb_frame()
    p0 <- .doc_int(d$start$page, 1L); p1 <- .doc_int(d$end$page, p0)
    down <- function(y) sprintf("%.0f%% down the page",
      100 * min(max(.doc_num(y, 0) / max(fr$ref_height, 1), 0), 1))
    row <- function(dot, colr, what, pg, y, setid, goid) div(
      style = "display:flex;align-items:center;gap:8px;margin:0 0 4px",
      span(style = sprintf("color:%s;font-size:15px;line-height:1", colr), dot),
      div(style = "flex:1;min-width:0;font-size:13px",
        strong(what), sprintf(" page %d, %s", pg, down(y))),
      actionButton(goid, "Show me", class = "btn-default btn-sm"),
      actionButton(setid, "Move it", class = "btn-primary btn-sm"))
    ymax <- .doc_num((d$band %||% list())$y_max, NA_real_)
    div(class = "note", style = "margin:0 0 10px;padding:8px 10px",
      row("\u25b2", PALETTE$ok, "Starts", p0, d$start$y, "rb_setstart", "rb_gostart"),
      row("\u25bc", PALETTE$bad, "Ends", p1, d$end$y, "rb_setend", "rb_goend"),
      # THE THIRD FACT, and only when it can matter. On a one-page table there
      # are no pages in between, so asking about them is noise.
      if (p1 > p0) div(
        style = "display:flex;align-items:center;gap:8px;margin:0 0 4px",
        span(style = sprintf("color:%s;font-size:15px;line-height:1;letter-spacing:-1px",
                             PALETTE$warn), "\u2013\u2013"),
        div(style = "flex:1;min-width:0;font-size:13px",
          strong("Bottom edge on the pages in between"),
          # SET, DRAWN, AND SAID. It is worked out by reading the table's own
          # first page, so the screen says that rather than presenting a number
          # out of the air. (Register D4.)
          if (is.finite(ymax)) sprintf(" %s, worked out by reading this document", down(ymax))
          else " the bottom of the page (so a footer under it would be read as rows)"),
        actionButton("rb_setbottom", "Move it", class = "btn-primary btn-sm")),
      # FOUR SHORTCUTS TO AN ANSWER THE TOOL HAS ALREADY GIVEN.
      #
      # The end is worked out the moment the columns are drawn and the bottom
      # edge is worked out with it, so these four are how to answer FASTER when
      # one of them is wrong - not what to do next. They were five buttons in a
      # row under the three that matter, on a screen the owner called "LOTS of
      # buttons". Folded, with the two direct corrections ("Show me", "Move it")
      # left on the lines they belong to. (Register D2.)
      tags$details(style = "margin-top:6px",
        tags$summary(class = "muted", style = "cursor:pointer;font-size:12px",
                     "Other ways to say where it ends"),
        div(style = "display:flex;gap:6px;flex-wrap:wrap;margin-top:6px",
          actionButton("rb_endauto", "Work the end out for me", class = "btn-default btn-sm"),
          actionButton("rb_endpage", "End at the bottom of its page", class = "btn-default btn-sm"),
          actionButton("rb_endlast", "End at the bottom of the last page",
                       class = "btn-default btn-sm"),
          if (p1 > p0 && is.finite(ymax))
            actionButton("rb_clearbottom", "Run to the bottom of every page",
                         class = "btn-default btn-sm"))))
  })
  observeEvent(input$rb_clearbottom, {
    d <- rb$draft; if (is.null(d)) return()
    b <- d$band %||% list(); b$y_max <- NULL; d$band <- b
    rb$draft <- d
  })
  # Pressing one of these by hand is somebody steering, so the guide stops
  # arming the next step behind them.
  observeEvent(input$rb_setstart, { if (!is.null(rb$draft)) { rb$guide <- FALSE; rb$mode <- "start" } })
  observeEvent(input$rb_setend,   { if (!is.null(rb$draft)) { rb$guide <- FALSE; rb$mode <- "end" } })
  observeEvent(input$rb_setbottom, { if (!is.null(rb$draft)) { rb$guide <- FALSE; rb$mode <- "bottom" } })
  observeEvent(input$rb_gostart, {
    d <- rb$draft; if (is.null(d)) return()
    updateNumericInput(session, "rb_page", value = .doc_int(d$start$page, 1L))
  })
  observeEvent(input$rb_goend, {
    d <- rb$draft; if (is.null(d)) return()
    updateNumericInput(session, "rb_page", value = .doc_int(d$end$page, 1L))
  })
  # EVERY WAY OF SETTING THE END GOES THROUGH THE SAME LINE, so a table that has
  # just become a multi-page table gets its bottom edge filled in whichever of
  # the four ways the end was set. (Register D4.)
  observeEvent(input$rb_endauto, {
    d <- rb$draft; if (is.null(d)) return()
    e <- tryCatch(doc_auto_end(rb_input(), d, rb_frame()), error = function(x) NULL)
    if (is.null(e)) { showNotification("Couldn't work out where it ends - move it by hand.",
                                       type = "warning", duration = 6); return() }
    d$end <- e; d <- .rb_default_bottom(d); rb$draft <- d; .rb_push_where(d)
  })
  observeEvent(input$rb_endpage, {
    d <- rb$draft; if (is.null(d)) return()
    d$end <- list(page = .doc_int(d$start$page, 1L), y = rb_frame()$ref_height)
    rb$draft <- d; .rb_push_where(d)
  })
  observeEvent(input$rb_endlast, {
    d <- rb$draft; if (is.null(d)) return()
    n <- rb_n_pages(); if (is.na(n)) return()
    d$end <- list(page = as.integer(n), y = rb_frame()$ref_height)
    d <- .rb_default_bottom(d)
    rb$draft <- d; .rb_push_where(d)
  })

  # CANCEL SAYS WHICH OF THE TWO THINGS IT DOES. On a new table it throws the
  # drawing away; on a copy of a saved one it leaves the saved table untouched -
  # and a button labelled "Throw it away" over a table that took an afternoon to
  # get right is a button nobody dares press. (Register H13.)
  output$rb_cancel_btn <- renderUI({
    editing <- !is.na(.doc_int(rb$edit_idx, NA_integer_))
    actionButton("rb_cancel",
                 if (editing) "Cancel - keep the saved table" else "Throw it away",
                 class = "btn-default")
  })
  observeEvent(input$rb_cancel, {
    idx <- .doc_int(rb$edit_idx, NA_integer_)
    rb$draft <- NULL; rb$colsel <- NA_integer_; rb$mode <- ""
    rb$edit_idx <- NA_integer_
    if (!is.na(idx) && idx >= 1L && idx <= length(rb$tables))
      showNotification(sprintf("Left \u201c%s\u201d as it was saved.",
                               as.character(rb$tables[[idx]]$name %||% "that table")[1]),
                       type = "message", duration = 5)
  })

  # Typing is debounced before it reaches the draft: the list beside the box is
  # drawn from the draft, and redrawing it on every letter is what "it updates
  # and glitches while I type" was.
  rb_name_d <- debounce(reactive(input$rb_name), 500)
  observeEvent(rb_name_d(), {
    d <- rb$draft; if (is.null(d)) return()
    nm <- trimws(rb_name_d() %||% "")
    if (nzchar(nm) && !identical(nm, d$name)) { d$name <- nm; rb$draft <- d }
  }, ignoreInit = TRUE)

  # The choice and the count are ONE setting, so they are read together: "data"
  # means zero heading rows, "head" means however many the box said.
  observeEvent(list(input$rb_hdr, input$rb_hdrn), {
    d <- rb$draft; if (is.null(d)) return()
    want <- if (identical(input$rb_hdr, "data")) 0L
            else max(1L, min(8L, .doc_int(input$rb_hdrn, 1L)))
    if (is.na(want)) return()
    if (!identical(.doc_int(d$header_rows, 1L), want)) { d$header_rows <- want; rb$draft <- d }
  }, ignoreInit = TRUE)

  # Typed positions, debounced, and refused where a click would be refused: an
  # end before its start is not a boundary, it is a table with no rows.
  rb_where_d <- debounce(reactive(c(input$rb_sp, input$rb_sy, input$rb_ep, input$rb_ey)), 700)
  observeEvent(rb_where_d(), {
    d <- rb$draft; if (is.null(d)) return()
    sp <- .doc_int(input$rb_sp); sy <- .doc_num(input$rb_sy)
    ep <- .doc_int(input$rb_ep); ey <- .doc_num(input$rb_ey)
    if (any(is.na(c(sp, sy, ep, ey))) || sp < 1L || ep < 1L || sy < 0 || ey < 0) return()
    if (ep < sp || (ep == sp && ey <= sy)) {
      showNotification("That would end the table at or before it starts - not applied.",
                       type = "warning", duration = 6); return()
    }
    if (identical(.doc_int(d$start$page, 1L), sp) && abs(.doc_num(d$start$y, 0) - sy) < 0.05 &&
        identical(.doc_int(d$end$page, 1L), ep) && abs(.doc_num(d$end$y, 0) - ey) < 0.05) return()
    d$start <- list(page = sp, y = round(sy, 1))
    d$end <- list(page = ep, y = round(ey, 1))
    d <- .rb_default_bottom(d)
    rb$draft <- d
  }, ignoreInit = TRUE)

  observeEvent(input$rb_refit, {
    d <- rb$draft; if (is.null(d)) return()
    fit <- tryCatch(doc_fit_columns(rb_input(), d, rb_frame()), error = function(e) list())
    if (!length(fit)) { showNotification("Nothing readable between the start and the end.",
                                         type = "warning", duration = 6); return() }
    d$columns <- fit; rb$draft <- d
    rb$colsel <- NA_integer_
  })
  observeEvent(input$rb_addcol, {
    if (is.null(rb$draft)) return()
    rb$mode <- "addcol"
  })

  # ---- The columns: a list you read, and one panel that edits ---------------
  .RB_MAX_COLS <- 24L
  output$rb_cols <- renderUI({
    d <- rb$draft; cols <- .doc_columns(d)
    if (!length(cols)) return(div(class = "note", style = "margin:6px 0",
      paste("No columns yet. Drag a box round the row of column names, or press",
            "\u201c+ Add a column\u201d and drag one at a time.")))
    nm <- .doc_column_names(d); sel <- rb$colsel
    lapply(seq_along(cols), function(j) {
      if (j > .RB_MAX_COLS) return(NULL)
      on <- !is.na(sel) && sel == j
      div(style = sprintf(paste("display:flex;align-items:center;gap:6px;padding:3px 6px;",
                                "margin:0 0 2px;border-radius:0 4px 4px 0;background:%s;",
                                "border-left:3px solid %s"),
                          if (on) "#fff4d6" else "#f7f7f7", if (on) "#b7791f" else "#dddddd"),
        span(class = "muted", style = "font-size:11px;width:14px", j),
        span(style = "flex:1;min-width:0;overflow:hidden;text-overflow:ellipsis;white-space:nowrap",
             strong(nm[j])),
        span(class = "muted", style = "font-size:11px;white-space:nowrap",
             sprintf("%.0f\u2013%.0f", .doc_num(cols[[j]]$x_min, 0), .doc_num(cols[[j]]$x_max, 0))),
        span(class = "muted", style = "font-size:11px",
             as.character(cols[[j]]$type %||% "auto")),
        actionButton(paste0("rb_cs_", j), "Edit", class = "btn-default btn-sm"),
        actionButton(paste0("rb_cx_", j), "\u00d7", class = "btn-default btn-sm",
                     title = "delete this column"))
    })
  })
  output$rb_col_head <- renderUI({
    d <- rb$draft; j <- rb$colsel
    cols <- .doc_columns(d)
    if (is.null(d) || is.na(j) || j > length(cols)) return(NULL)
    div(style = "font-size:13px;margin:0 0 6px",
      strong(sprintf("Column %d of %d", j, length(cols))),
      span(class = "muted", " \u00b7 everything about it can be changed"))
  })

  # One observer per possible column, created ONCE: a Shiny observer cannot attach
  # to a control that does not exist yet, and creating them inside the renderUI
  # would stack another on every column each time the list redrew.
  .rb_drop_col <- function(j) {
    d <- rb$draft; if (is.null(d)) return()
    cols <- .doc_columns(d)
    if (!length(cols) || j > length(cols)) return()
    # DELETING A COLUMN LEAVES ITS SPACE EMPTY, it does not hand it to a
    # neighbour. Columns may have whitespace between them, so there is nothing to
    # decide here any more -- and growing the column beside it was a change
    # nobody asked for, on a column they had already got right.
    d$columns <- cols[-j]
    rb$draft <- d
    rb$colsel <- NA_integer_
  }
  lapply(seq_len(.RB_MAX_COLS), function(j) {
    observeEvent(input[[paste0("rb_cx_", j)]], .rb_drop_col(j), ignoreInit = TRUE)
    observeEvent(input[[paste0("rb_cs_", j)]], {
      if (j > length(.doc_columns(rb$draft))) return()
      rb$colsel <- j
      rb$colver <- rb$colver + 1L
      # EDIT ARMS THE DRAG. Pressing Edit on a column is a person saying "this
      # one is wrong", and moving it is what they are nearly always about to do
      # -- so the next drag does it, instead of answering "nothing was waiting"
      # and naming a second button they have to find first. It is not a silent
      # arming: the banner turns amber, names the column, and says it is
      # outlined on the page, and Cancel is in the banner. The other four things
      # the panel offers (rename, retype, type the edges, delete) are all still
      # one click away and none of them is a drag.
      rb$guide <- FALSE
      rb$mode <- "colpos"
    }, ignoreInit = TRUE)
  })

  # Filling the edit panel: on selection only, never while it is being typed in.
  observeEvent(list(rb$colsel, rb$colver), {
    j <- rb$colsel; d <- rb$draft
    if (is.na(j) || is.null(d)) return()
    cols <- .doc_columns(d)
    if (j > length(cols)) { rb$colsel <- NA_integer_; return() }
    cc <- cols[[j]]
    updateTextInput(session, "rb_cname", value = .doc_column_names(d)[j])
    updateSelectInput(session, "rb_ckind", selected = as.character(cc$type %||% "auto"))
    updateNumericInput(session, "rb_cx0", value = round(.doc_num(cc$x_min, 0), 1))
    updateNumericInput(session, "rb_cx1", value = round(.doc_num(cc$x_max, 0), 1))
  }, ignoreInit = TRUE)

  observeEvent(input$rb_cdone, { rb$colsel <- NA_integer_; if (identical(rb$mode, "colpos")) rb$mode <- "" })

  rb_cname_d <- debounce(reactive(input$rb_cname), 500)
  observeEvent(rb_cname_d(), {
    d <- rb$draft; j <- rb$colsel
    if (is.null(d) || is.na(j)) return()
    cols <- .doc_columns(d); if (j > length(cols)) return()
    nm <- trimws(rb_cname_d() %||% "")
    if (!nzchar(nm) || identical(nm, as.character(cols[[j]]$name %||% ""))) return()
    cols[[j]]$name <- nm; d$columns <- cols; rb$draft <- d
  }, ignoreInit = TRUE)
  observeEvent(input$rb_ckind, {
    d <- rb$draft; j <- rb$colsel
    if (is.null(d) || is.na(j)) return()
    cols <- .doc_columns(d); if (j > length(cols)) return()
    v <- input$rb_ckind %||% "auto"
    if (identical(as.character(cols[[j]]$type %||% "auto"), v)) return()
    cols[[j]]$type <- v; d$columns <- cols; rb$draft <- d
  }, ignoreInit = TRUE)

  # TYPING THE EDGES IS THE SAME REQUEST AS DRAGGING THEM, so it goes through the
  # same function: this column goes where it is put and its neighbours move along.
  rb_cx_d <- debounce(reactive(c(input$rb_cx0, input$rb_cx1)), 700)
  observeEvent(rb_cx_d(), {
    d <- rb$draft; j <- rb$colsel
    if (is.null(d) || is.na(j)) return()
    cols <- .doc_columns(d); if (j > length(cols)) return()
    x0 <- .doc_num(input$rb_cx0); x1 <- .doc_num(input$rb_cx1)
    if (is.na(x0) || is.na(x1) || x1 - x0 < 1) return()
    if (abs(.doc_num(cols[[j]]$x_min, 0) - x0) < 0.5 &&
        abs(.doc_num(cols[[j]]$x_max, 0) - x1) < 0.5) return()
    cols2 <- .rb_move_band(cols, j, x0, x1, rb_frame()$ref_width)
    jj <- .rb_col_at(list(columns = cols2), (x0 + x1) / 2)
    d$columns <- cols2; rb$draft <- d
    # The same sentence a drag gets. Typing a number that shoves three columns
    # along must not be quieter than dragging one.
    nt <- .rb_push_msg(cols2)
    if (nzchar(nt)) showNotification(nt, type = "message", duration = 8)
    # DO NOT RE-SELECT THE COLUMN IT ALREADY IS.
    #
    # Assigning a reactiveValues field invalidates it even when the new value is
    # identical to the old one -- and the SELECTION is what pushes values back
    # into these two boxes. So a no-op assignment here rewrites the number under
    # the caret of the person typing it, which is the exact complaint this
    # screen was rebuilt to fix. Measured before this line was guarded: five
    # redraws of the page for a three-character number.
    #
    # Move the selection only when the band really landed on a different column.
    if (!is.na(jj) && !identical(as.integer(jj), as.integer(j))) rb$colsel <- jj
  }, ignoreInit = TRUE)

  observeEvent(input$rb_savetab, {
    d <- rb$draft; if (is.null(d)) return()
    if (length(.doc_columns(d)) < 1L) {
      showNotification("This table has no columns yet.", type = "warning"); return()
    }
    d$name <- trimws(as.character(input$rb_name %||% d$name %||% ""))
    if (!nzchar(d$name)) d$name <- sprintf("Table %d", length(rb$tables) + 1L)
    d$anchor$first_column <- list()
    # A DRAFT THAT CAME FROM A SAVED TABLE GOES BACK WHERE IT CAME FROM.
    # Editing copies rather than cuts (register H13), so saving is a REPLACE at
    # that index, not an append - otherwise one edit would leave two of it.
    idx <- .doc_int(rb$edit_idx, NA_integer_)
    if (!is.na(idx) && idx >= 1L && idx <= length(rb$tables)) {
      rb$tables[[idx]] <- d
      showNotification(sprintf("Replaced \u201c%s\u201d in this template.", d$name),
                       type = "message", duration = 5)
    } else {
      rb$tables <- c(rb$tables, list(d))
      showNotification(sprintf("Saved \u201c%s\u201d. It can be edited again from the list below.",
                               d$name), type = "message", duration = 5)
    }
    rb$draft <- NULL; rb$colsel <- NA_integer_; rb$mode <- ""
    rb$edit_idx <- NA_integer_
  })

  # .RB_MAX_ROWS -- how many saved tables (or values) carry a working Edit and
  # Remove. The observers are created ONCE, outside the renderUI, so there is a
  # fixed number of them; the list used to render a pair of buttons for every
  # table with no cap at all, so on a 61-table template the last button did
  # nothing when pressed and said nothing about why. The list stops where the
  # observers stop and says so. (Register H13.)
  .RB_MAX_ROWS <- 60L
  output$rb_saved <- renderUI({
    if (!length(rb$tables)) return(p(class = "muted", style = "margin:6px 0",
      "None yet - every table you save appears here."))
    shown <- seq_len(min(length(rb$tables), .RB_MAX_ROWS))
    arm <- rb$rm_armed
    tagList(
      lapply(shown, function(i) {
        tb <- rb$tables[[i]]
        p0 <- .doc_int(tb$start$page, 1L); p1 <- .doc_int(tb$end$page, p0)
        nm <- .doc_column_names(tb)
        on <- identical(.doc_int(rb$edit_idx, NA_integer_), as.integer(i))
        armed <- is.list(arm) && identical(.doc_int(arm$i, NA_integer_), as.integer(i))
        # A TABLE READ WHEREVER IT APPEARS DOES NOT HAVE A PAGE NUMBER, and
        # printing one for it would be the template saying something it does not
        # mean. `occurrence` and `.places` are set by the proposer when it folds
        # near-identical tables into one (R/tables_detect.R). (Register A2.)
        pp <- vapply(tb$.places %||% list(),
                     function(pl) .doc_int(.doc_key(.doc_key(pl, "start"), "page"), NA_integer_),
                     integer(1))
        pp <- pp[!is.na(pp)]
        where <- if (identical(as.character(tb$occurrence %||% "once")[1], "all"))
            sprintf("wherever it appears%s", if (length(pp))
              sprintf(" \u00b7 %d places, pages %s%s", length(pp),
                      paste(utils::head(pp, 6), collapse = ", "),
                      if (length(pp) > 6L) "..." else "") else "")
          else if (p1 > p0) sprintf("pages %d-%d", p0, p1) else sprintf("page %d", p0)
        div(style = sprintf(paste("padding:5px 7px;border-left:3px solid %s;margin-bottom:3px;",
                                  "background:%s;border-radius:0 4px 4px 0"),
                            if (on) "#b7791f" else "#0f7a37",
                            if (on) "#fff4d6" else "#f7f7f7"),
          div(style = "display:flex;justify-content:space-between;align-items:center;gap:8px",
            div(style = "min-width:0",
              strong(tb$name %||% sprintf("Table %d", i)),
              if (on) span(class = "muted", style = "font-size:12px",
                           " \u00b7 open above"),
              div(class = "muted", style = "font-size:12px",
                  sprintf("%s \u00b7 %s", where,
                          if (length(nm)) paste(nm, collapse = ", ") else "no columns"))),
            div(style = "white-space:nowrap",
              actionButton(paste0("rb_ed_", i), "Edit", class = "btn-default btn-sm"),
              actionButton(paste0("rb_rm_", i), "Remove",
                           class = if (armed) "btn-warning btn-sm" else "btn-default btn-sm"),
              if (armed) span(class = "muted", style = "font-size:11px;margin-left:4px",
                              "press again"))))
      }),
      if (length(rb$tables) > .RB_MAX_ROWS)
        p(class = "muted", style = "margin:6px 0 0",
          sprintf(paste("%d more table(s) are on this template and will be saved,",
                        "but cannot be edited here. Edit them one at a time by",
                        "removing tables above, or in the template file."),
                  length(rb$tables) - .RB_MAX_ROWS)))
  })

  lapply(seq_len(.RB_MAX_ROWS), function(i) {
    observeEvent(input[[paste0("rb_rm_", i)]], {
      if (i > length(rb$tables)) return()
      # REMOVING IS THE ONLY GESTURE HERE THAT DESTROYS WORK, so it is the only
      # one that has to be meant twice. It used to take the table out on the
      # first press, with no confirmation and no undo, beside a button labelled
      # "Throw it away" - one mis-click and table 27 was gone from a template
      # that took months. (Register H13.)
      nm <- as.character(rb$tables[[i]]$name %||% sprintf("Table %d", i))[1]
      a <- rb$rm_armed
      if (is.list(a) && identical(.doc_int(a$i, NA_integer_), as.integer(i)) &&
          identical(as.character(a$name %||% ""), nm)) {
        rb$tables <- rb$tables[-i]
        rb$rm_armed <- NULL
        # An index above the one removed now points at a different table.
        idx <- .doc_int(rb$edit_idx, NA_integer_)
        if (!is.na(idx)) rb$edit_idx <- if (idx == i) NA_integer_ else
          if (idx > i) idx - 1L else idx
        showNotification(sprintf("Removed \u201c%s\u201d from this template.", nm),
                         type = "message", duration = 6)
        return()
      }
      rb$rm_armed <- list(i = as.integer(i), name = nm)
      showNotification(sprintf("Press Remove again to take \u201c%s\u201d out of this template.", nm),
                       type = "warning", duration = 8)
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("rb_ed_", i)]], {
      if (i > length(rb$tables)) return()
      # EDITING COPIES. It used to CUT the table out of the list and into the
      # draft - so Cancel, or a second thought, destroyed it. The draft is a copy
      # and rb$edit_idx remembers where it goes back to, so Save replaces it and
      # Cancel leaves the saved table exactly as it was. (Register H13.)
      d <- rb$tables[[i]]
      rb$rm_armed <- NULL
      .rb_load_draft(d)
      rb$edit_idx <- as.integer(i)
      rb$mode <- ""
      updateNumericInput(session, "rb_page", value = .doc_int(d$start$page, 1L))
      updateTabsetPanel(session, "rb_tab", selected = "tables")
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("rb_vrm_", i)]], {
      if (i > length(rb$pairs)) return()
      rb$pairs <- rb$pairs[-i]
    }, ignoreInit = TRUE)
    observeEvent(input[[paste0("rb_ved_", i)]], {
      if (i > length(rb$pairs)) return()
      v <- rb$pairs[[i]]
      rb$pairs <- rb$pairs[-i]
      .rb_load_vdraft(v)
      lb <- .doc_key(v, "label"); vb <- .doc_key(v, "value")
      pg <- .doc_int(.doc_key(vb, "page") %||% .doc_key(lb, "page"), 1L)
      updateNumericInput(session, "rb_page", value = pg)
      updateTabsetPanel(session, "rb_tab", selected = "values")
      # Edit arms the drag here too. THE VALUE, not the label: a pair that reads
      # the wrong thing is nearly always the figure moving, not the wording, and
      # the wording is the half that is found again by matching rather than by
      # position. "Re-draw the LABEL instead" is one press, in the banner.
      rb$guide <- FALSE
      rb$mode <- "value"
    }, ignoreInit = TRUE)
  })

  # ---- The value being worked on --------------------------------------------
  output$rb_vstep <- renderUI({
    v <- rb$vdraft; if (is.null(v)) return(NULL)
    lb <- .doc_key(v, "label"); vb <- .doc_key(v, "value")
    line <- function(what, got, have, colr) div(
      style = "display:flex;align-items:baseline;gap:8px;margin:0 0 3px",
      span(style = sprintf("width:46px;font-size:12px;font-weight:700;color:%s", colr), what),
      div(style = "flex:1;min-width:0",
        if (have) strong(got) else span(class = "muted", "not drawn yet")))
    rel <- if (is.list(lb) && is.list(vb)) .doc_pair_rel(lb, vb) else NULL
    div(class = "note", style = "margin:0 0 10px;padding:8px 10px",
      line("Label", v$label_text %||% "", is.list(lb), PALETTE$bad),
      line("Value", v$read %||% "", is.list(vb), PALETTE$ok),
      if (!is.null(rel)) p(class = "muted", style = "margin:6px 0 0;font-size:12px",
        # "about 0 points away" is what a person reads when the two boxes touch,
        # and it says nothing. Under a few points, the honest word is "right".
        if (.doc_num(rel$gap, 0) < 3)
          sprintf("On this page the value sits immediately %s. That is what will be looked for next time.",
                  .doc_pair_where_text(rel))
        else sprintf("On this page the value sits %s, about %.0f points away. That is what will be looked for next time.",
                     .doc_pair_where_text(rel), .doc_num(rel$gap, 0)))
      else p(class = "muted", style = "margin:6px 0 0;font-size:12px",
             "Both boxes are needed: the label names it, the value is what comes out."))
  })

  observeEvent(input$rb_vlabel,  { if (!is.null(rb$vdraft)) rb$mode <- "label" })
  observeEvent(input$rb_vvalue,  { if (!is.null(rb$vdraft)) rb$mode <- "value" })
  observeEvent(input$rb_vcancel, { rb$vdraft <- NULL; rb$mode <- "" })

  rb_vname_d <- debounce(reactive(input$rb_vname), 500)
  # What the box will actually become, live under the box. Reads the DEBOUNCED
  # value, not the raw one, so it does not chase every keystroke.
  output$rb_vname_key <- renderText({
    typed <- trimws(as.character(rb_vname_d() %||% ""))
    if (!nzchar(typed)) "Lower case with underscores - ird_number, total_paid."
    else sprintf("Comes out as  %s", doc_suggest_name(typed))
  })
  # AN EMPTY BOX IS "NO NAME YET", NOT THE WORD "table".
  #
  # Reported: "Call this value still defaulting sometimes to the table name rather
  # than the label." doc_suggest_name() falls back to the literal string "table"
  # for anything it cannot make a key out of -- which is right for a TABLE and
  # wrong here. This observer fed it the empty box half a second after "+ Add a
  # value" was pressed, stored v$name = "table", and the label drag then refused
  # to overwrite it because the name was no longer empty. The value came out
  # called "table" and nothing on screen explained why.
  observeEvent(rb_vname_d(), {
    v <- rb$vdraft; if (is.null(v)) return()
    typed <- trimws(as.character(rb_vname_d() %||% ""))
    nm <- if (nzchar(typed)) doc_suggest_name(typed) else ""
    if (!identical(nm, as.character(v$name %||% ""))) { v$name <- nm; rb$vdraft <- v }
  }, ignoreInit = TRUE)
  observeEvent(input$rb_vtype, {
    v <- rb$vdraft; if (is.null(v)) return()
    ty <- input$rb_vtype %||% "text"
    if (!identical(as.character(v$type %||% "text"), ty)) { v$type <- ty; rb$vdraft <- v }
  }, ignoreInit = TRUE)
  # OVERRIDE THE SIDE. The tool works it out from the two boxes; a person who
  # knows the next copy prints the figure underneath rather than beside can say so.
  observeEvent(input$rb_vwhere, {
    v <- rb$vdraft; if (is.null(v)) return()
    w <- input$rb_vwhere %||% "right"
    rel <- .doc_key(v, "where")
    if (!is.list(rel)) rel <- if (is.list(v$label) && is.list(v$value))
      .doc_pair_rel(v$label, v$value) else list(where = w, gap = 0, cross = 0,
                                                width = 0, height = 0)
    if (identical(as.character(rel$where %||% ""), w)) return()
    rel$where <- w
    v$where <- rel; rb$vdraft <- v
  }, ignoreInit = TRUE)

  observeEvent(input$rb_vsave, {
    v <- rb$vdraft; if (is.null(v)) return()
    if (!is.list(.doc_key(v, "label"))) {
      showNotification("Drag a box round the label first.", type = "warning"); return()
    }
    if (!is.list(.doc_key(v, "value"))) {
      showNotification("Drag a box round the value first.", type = "warning"); return()
    }
    v$type <- input$rb_vtype %||% v$type %||% "text"
    v$name <- doc_suggest_name(input$rb_vname %||% v$name)
    if (!nzchar(v$name)) v$name <- sprintf("value_%d", length(rb$pairs) + 1L)
    # Show the person the name it really got. Safe here and nowhere else: the
    # draft is finished, so nobody is typing into the box this rewrites.
    updateTextInput(session, "rb_vname", value = v$name)
    rel <- .doc_key(v, "where")
    if (!is.list(rel)) rel <- .doc_pair_rel(v$label, v$value)
    rel$where <- input$rb_vwhere %||% rel$where %||% "right"
    v$where <- rel
    rb$pairs <- c(rb$pairs, list(v))
    rb$vdraft <- NULL; rb$mode <- ""
    showNotification(sprintf("Saved \u201c%s\u201d. It can be edited again from the list below.",
                             v$name), type = "message", duration = 5)
  })

  output$rb_vsaved <- renderUI({
    if (!length(rb$pairs)) return(p(class = "muted", style = "margin:6px 0",
      "None yet. Every value you save appears here, and every one can be edited again."))
    lapply(seq_along(rb$pairs), function(i) {
      pr <- rb$pairs[[i]]
      rel <- .doc_key(pr, "where")
      if (!is.list(rel) && is.list(.doc_key(pr, "label")) && is.list(.doc_key(pr, "value")))
        rel <- .doc_pair_rel(pr$label, pr$value)
      div(style = paste("padding:5px 7px;border-left:3px solid #b7791f;margin-bottom:3px;",
                        "background:#f7f7f7;border-radius:0 4px 4px 0"),
        div(style = "display:flex;justify-content:space-between;align-items:center;gap:8px",
          div(style = "min-width:0",
            strong(pr$name %||% sprintf("Value %d", i)),
            div(class = "muted", style = "font-size:12px",
                sprintf("\u201c%s\u201d \u2192 %s%s",
                        substr(pr$label_text %||% "", 1, 24),
                        substr(pr$read %||% "", 1, 20),
                        if (is.list(rel)) sprintf(" \u00b7 %s", .doc_pair_where_text(rel)) else ""))),
          div(style = "white-space:nowrap",
            actionButton(paste0("rb_ved_", i), "Edit", class = "btn-default btn-sm"),
            actionButton(paste0("rb_vrm_", i), "Remove", class = "btn-default btn-sm"))))
    })
  })

  # .rb_all_pairs() -- the drawn values PLUS the typed ones. A typed value has no
  # box at all: it is found by its wording anywhere on the document, which is what
  # the form templates have always done and is more portable than a box.
  .rb_all_pairs <- reactive({
    out <- rb$pairs
    used <- vapply(out, function(pp) as.character(pp$name %||% "")[1], character(1))
    typed <- parse_fields_spec(input$rb_val_typed)
    for (nm in names(typed)) {
      if (nm %in% used) next
      terms <- as.character(unlist(typed[[nm]]$any_of %||% list()))
      out[[length(out) + 1L]] <- list(name = nm, label_text = terms[1] %||% nm,
                                      type = as.character(typed[[nm]]$value %||% "text")[1],
                                      read = "(found by wording)")
      used <- c(used, nm)
    }
    out
  })

  # ---- What a gesture does, said under the picture --------------------------
  # ---- What a gesture does, repeated under the picture where the mouse is ----
  # UNDER THE PICTURE: the same sentence as the banner, but ONLY while something
  # is armed. The banner is pinned to the top of the window now, so when nothing
  # is waiting this line said, a second time and two inches lower, exactly what
  # the grey bar above already said. Repeating the instruction at the point of
  # the action is worth the words; repeating "nothing is happening" is not.
  output$rb_hint <- renderUI({
    a <- .RB_ASK[[rb$mode %||% ""]]
    if (is.null(a)) return(NULL)
    p(class = "muted", style = "margin:6px 0", a[1])
  })

  # ---- The picture -----------------------------------------------------------
  rb_render <- reactive({
    p <- rb_doc(); req(p)
    render_page_view(p, .rb_pg(), 110)
  })

  # A STRIP OF WHITE ABOVE THE PAGE for the tool's own labels.
  #
  # The column names the tool worked out have to be read against the column names
  # printed on the page, and until now they were drawn ON them -- so the one
  # comparison the screen exists to let you make was the one thing it covered up.
  # The plot's y range is extended above the paper and every label the tool writes
  # goes up there, joined to what it names by a hairline. Nothing the tool draws
  # sits over anything the document says.
  .RB_GUTTER <- 38

  # .rb_tag(x, y, s, col, above) -- a small filled label. Readable over anything,
  # because it brings its own background.
  .rb_tag <- function(x, y, s, col, above = TRUE, cex = 0.7) {
    if (!nzchar(s)) return(invisible(NULL))
    w <- graphics::strwidth(s, cex = cex, font = 2) + 6
    h <- graphics::strheight(s, cex = cex, font = 2) + 6
    yc <- if (above) y - h / 2 - 1 else y + h / 2 + 1
    rect(x, yc - h / 2, x + w, yc + h / 2, col = col, border = NA)
    text(x + w / 2, yc, s, col = "#ffffff", font = 2, cex = cex, adj = c(0.5, 0.5))
    invisible(NULL)
  }
  # .rb_fit(s, w, cex) -- the label trimmed until it fits the width it has.
  .rb_fit <- function(s, w, cex = 0.68) {
    s <- as.character(s %||% "")
    if (!nzchar(s) || graphics::strwidth(s, cex = cex, font = 2) <= w) return(s)
    while (nchar(s) > 2L &&
           graphics::strwidth(paste0(s, "\u2026"), cex = cex, font = 2) > w)
      s <- substr(s, 1, nchar(s) - 1L)
    paste0(s, "\u2026")
  }

  # .rb_draw_table -- one table on one page, layer by layer. Every layer is a
  # thing a person can switch off, because every layer is drawn over a document
  # they may need to read.
  .rb_draw_table <- function(tb, r, pg, key, lwd, lay) {
    p0 <- .doc_int(tb$start$page, NA_integer_)
    p1 <- .doc_int(tb$end$page, p0)
    if (is.na(p0) || is.na(p1) || pg < p0 || pg > p1) return(invisible(NULL))
    y0 <- if (pg == p0) .doc_num(tb$start$y, 0) else 0
    # THE BOTTOM EDGE OF A PAGE THE TABLE CARRIES ON PAST IS A REAL EDGE, so it
    # is drawn where it really is. It used to be drawn at the bottom of the paper
    # on every page but the last, because that is what the reader assumed - and
    # an assumption drawn as if it were an answer is the reason a footer got read
    # in as rows with nothing on screen to explain it. (Register D4.)
    ymax <- .doc_num((tb$band %||% list())$y_max, NA_real_)
    y1 <- if (pg == p1) .doc_num(tb$end$y, r$h)
          else if (is.finite(ymax)) min(ymax, r$h) else r$h
    # DRAW THE COLUMNS, NOT THE EDGE VIEW.
    #
    # This drew from doc_column_edges(), which is a LOSSY projection: it collects
    # every column's x_min and only the LAST column's x_max. That was harmless
    # while the columns tiled and became a liar the moment they were allowed not
    # to -- each band was drawn from its own left edge to the NEXT COLUMN'S left
    # edge, so every gap was painted as if it belonged to the column before it and
    # the tint ran straight over the column after. Reported as "it seems to just
    # place the new column x axis on top of the previous, overlapping x axis":
    # the model was right and the PICTURE was wrong, which is the worse of the
    # two, because the picture is what is being checked.
    #
    # Each column is now drawn from its own x_min to its own x_max, so a gap
    # between two columns is drawn as a gap -- which is the whole point of being
    # allowed to have one.
    cols <- .doc_columns(tb)
    col <- PALETTE[[key]]
    bands <- Filter(Negate(is.null), lapply(cols, function(cc) {
      lo <- .doc_num(cc$x_min, NA_real_); hi <- .doc_num(cc$x_max, NA_real_)
      if (!is.finite(lo) || !is.finite(hi) || hi <= lo) NULL else c(lo, hi)
    }))
    if ("tables" %in% lay) {
      if (length(bands)) {
        # EVERY COLUMN IS TINTED. It used to tint only the odd-numbered ones,
        # meaning to show where one ends and the next begins -- but on screen
        # that reads as "these three are selected and these three are not", and
        # the list beside it says six. A stripe is still there, it is just the
        # difference between two shades of the SAME thing rather than between a
        # thing and nothing. The one column being edited is the one with a
        # thick outline round it, and it is the only one.
        pale <- if (lwd > 1) "16" else "0d"
        dark <- if (lwd > 1) "2a" else "1a"
        for (j in seq_along(bands))
          rect(bands[[j]][1], y0, bands[[j]][2], y1, border = NA,
               col = pal_fill(key, if (j %% 2L == 1L) dark else pale))
      } else {
        rect(0, y0, r$w, y1, border = col, lty = 3, lwd = lwd)
      }
    }
    # Two rules per column, its own two edges -- not one rule per shared divider,
    # which is a different picture as soon as the columns do not touch.
    if ("edges" %in% lay)
      for (b in bands) for (e in b) lines(c(e, e), c(y0, y1), col = col, lwd = lwd)
    if ("ends" %in% lay) {
      # WHERE IT STARTS AND WHERE IT STOPS, named on the page in the same words
      # the panel beside it uses. Only on the page the boundary is actually on --
      # a table running over four pages has one start and one end, not four.
      if (pg == p0) {
        lines(c(0, r$w), c(y0, y0), col = PALETTE$ok, lwd = lwd + 0.5)
        # A CHIP, NOT A CAPTION. The table's full name is in the box beside the
        # picture; what the picture has to say is "the table starts here", and a
        # long title printed over the document's own title says it worse.
        .rb_tag(2, y0, paste0("START \u00b7 ", .rb_fit(tb$name %||% "", r$w * 0.22)),
                PALETTE$ok, above = TRUE)
      } else lines(c(0, r$w), c(y0, y0), col = col, lty = 3, lwd = lwd)
      if (pg == p1) {
        lines(c(0, r$w), c(y1, y1), col = PALETTE$bad, lwd = lwd + 0.5)
        .rb_tag(2, y1, "END", PALETTE$bad, above = FALSE)
      } else if (is.finite(ymax)) {
        lines(c(0, r$w), c(y1, y1), col = PALETTE$warn, lwd = lwd + 0.5, lty = 2)
        .rb_tag(2, y1, "BOTTOM", PALETTE$warn, above = FALSE)
      } else lines(c(0, r$w), c(y1, y1), col = col, lty = 3, lwd = lwd)
    }
    invisible(NULL)
  }

  # .rb_draw_names -- the column names, FLOATED into the strip above the page.
  # Two rows, staggered, so a narrow column beside a wide one still gets its name
  # in full; a dotted leader joins each name to the divider on its left.
  # .rb_name_items(tb, pg, key) -- what this table wants written in the strip on
  # this page: one item per column, with where it points and how wide it is.
  # PLACING them is a separate job (.rb_place_names), because it cannot be done
  # one table at a time.
  .rb_name_items <- function(tb, pg, key) {
    p0 <- .doc_int(tb$start$page, NA_integer_)
    p1 <- .doc_int(tb$end$page, p0)
    if (is.na(p0) || is.na(p1) || pg < p0 || pg > p1) return(list())
    cols <- .doc_columns(tb)
    if (!length(cols)) return(list())
    nm <- .doc_column_names(tb)
    y0 <- if (pg == p0) .doc_num(tb$start$y, 0) else 0
    Filter(Negate(is.null), lapply(seq_along(cols), function(j) {
      lo <- .doc_num(cols[[j]]$x_min, NA_real_); hi <- .doc_num(cols[[j]]$x_max, NA_real_)
      if (!is.finite(lo) || !is.finite(hi)) return(NULL)
      list(cx = (lo + hi) / 2, w = hi - lo, y0 = y0,
           text = if (j <= length(nm)) nm[j] else "", col = PALETTE[[key]])
    }))
  }

  # .rb_place_names(items, r) -- write every column name in the strip above the
  # page, on as many rows as it takes for none of them to touch.
  #
  # IT USED TO BE TWO ROWS, ODD COLUMNS ON ONE AND EVEN ON THE OTHER, PER TABLE.
  # That is fine for one table and wrong the moment there are two on a page:
  # every table started again at row one, so the second table's names were
  # written straight over the first's -- and a report with three tables side by
  # side is exactly the document this builder exists for. The names were moved
  # off the page to stop them covering the headings underneath, and then covered
  # each other instead.
  #
  # So placement is a PAGE-level decision. Every label from every table goes into
  # one list, sorted left to right, and each takes the first row where it clears
  # what is already there. A label is never dropped and never truncated to
  # nothing: the strip grows downwards instead, and the page raster starts below
  # it, so more tables means a taller margin rather than a worse one.
  .RB_NAME_ROW_H <- 15          # points per row of labels
  .rb_name_rows <- function(items) {
    if (!length(items)) return(1L)
    # the same greedy pass the drawing does, run for its answer only
    ends <- numeric(0)
    for (it in items[order(vapply(items, function(x) x$cx, numeric(1)))]) {
      half <- max(9, min(it$w * 0.95, 90)) / 2
      k <- which(ends <= it$cx - half - 3)[1]
      if (is.na(k)) { k <- length(ends) + 1L }
      ends[k] <- it$cx + half + 3
    }
    max(1L, length(ends))
  }
  .rb_place_names <- function(items, gutter) {
    if (!length(items)) return(invisible(NULL))
    ends <- numeric(0)
    for (it in items[order(vapply(items, function(x) x$cx, numeric(1)))]) {
      half <- max(9, min(it$w * 0.95, 90)) / 2
      k <- which(ends <= it$cx - half - 3)[1]
      if (is.na(k)) k <- length(ends) + 1L
      ends[k] <- it$cx + half + 3
      ty <- -gutter + 9 + (k - 1L) * .RB_NAME_ROW_H
      lines(c(it$cx, it$cx), c(ty + 5, it$y0), col = it$col, lwd = 0.7, lty = 3)
      text(it$cx, ty, .rb_fit(it$text, half * 2), col = it$col, font = 2,
           cex = 0.68, adj = c(0.5, 0.5))
    }
    invisible(NULL)
  }

  # .rb_draw_pair -- a label box and a value box, joined, and named in the strip.
  .rb_draw_pair <- function(pr, r, pg, lab_col, val_col, lwd, lay, nm = "") {
    lb <- .doc_key(pr, "label"); vb <- .doc_key(pr, "value")
    on_l <- is.list(lb) && identical(.doc_int(lb$page, 1L), pg)
    on_v <- is.list(vb) && identical(.doc_int(vb$page, 1L), pg)
    if (!on_l && !on_v) return(invisible(NULL))
    if (on_l) rect(lb$x_min, lb$y_max, lb$x_max, lb$y_min,
                   border = lab_col, lwd = lwd, lty = 2)
    if (on_v) rect(vb$x_min, vb$y_max, vb$x_max, vb$y_min, border = val_col, lwd = lwd)
    if (on_l && on_v) {
      # The arrow IS the relationship the template stores. Drawn, it is checkable.
      lines(c((lb$x_min + lb$x_max) / 2, (vb$x_min + vb$x_max) / 2),
            c((lb$y_min + lb$y_max) / 2, (vb$y_min + vb$y_max) / 2),
            col = val_col, lwd = 1, lty = 3)
    }
    if ("names" %in% lay && on_l && nzchar(nm))
      .rb_tag(lb$x_min, lb$y_min, .rb_fit(nm, r$w * 0.35, 0.7), lab_col, above = TRUE)
    invisible(NULL)
  }

  output$rb_plot <- renderPlot({
    r <- rb_render()
    if (is.null(r)) {
      plot.new()
      text(0.5, 0.5, "This page could not be drawn.\nThe tables can still be read from it.",
           cex = 1.1, col = "#666666")
      return(invisible(NULL))
    }
    lay <- input$rb_layers %||% c("tables", "edges", "names", "values", "ends")
    pg0 <- as.integer(r$pg)
    # THE STRIP IS AS TALL AS THE NAMES NEED, and no taller. Worked out before the
    # plot window is opened, because the window's top edge IS the strip.
    items <- if ("names" %in% lay)
      c(unlist(lapply(rb$tables, function(tb) .rb_name_items(tb, pg0, "ok")), recursive = FALSE),
        if (!is.null(rb$draft)) .rb_name_items(rb$draft, pg0, "meta") else list())
      else list()
    gutter <- max(.RB_GUTTER, 9 + .rb_name_rows(items) * .RB_NAME_ROW_H + 6)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, -gutter), xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    # a hairline where the paper really starts, so the strip above it reads as the
    # tool's margin and not as part of the document
    lines(c(0, r$w), c(0, 0), col = "#cccccc", lwd = 1)
    pg <- as.integer(r$pg)
    for (tb in rb$tables) .rb_draw_table(tb, r, pg, "ok", 1, lay)
    if (!is.null(rb$draft)) .rb_draw_table(rb$draft, r, pg, "meta", 2, lay)
    .rb_place_names(items, gutter)
    # THE COLUMN BEING EDITED, picked out from the rest of them.
    d <- rb$draft; j <- rb$colsel
    if (!is.null(d) && !is.na(j) && "edges" %in% lay) {
      cols <- .doc_columns(d)
      p0 <- .doc_int(d$start$page, 1L); p1 <- .doc_int(d$end$page, p0)
      if (j <= length(cols) && pg >= p0 && pg <= p1) {
        ym <- .doc_num((d$band %||% list())$y_max, NA_real_)
        y0 <- if (pg == p0) .doc_num(d$start$y, 0) else 0
        y1 <- if (pg == p1) .doc_num(d$end$y, r$h)
              else if (is.finite(ym)) min(ym, r$h) else r$h
        rect(.doc_num(cols[[j]]$x_min, 0), y0, .doc_num(cols[[j]]$x_max, 0), y1,
             border = PALETTE$warn, lwd = 3)
      }
    }
    if ("values" %in% lay) {
      for (i in seq_along(rb$pairs))
        .rb_draw_pair(rb$pairs[[i]], r, pg, PALETTE$warn, PALETTE$warn, 2, lay,
                      as.character(rb$pairs[[i]]$name %||% ""))
      if (!is.null(rb$vdraft))
        .rb_draw_pair(rb$vdraft, r, pg, PALETTE$bad, PALETTE$ok, 2.5, lay,
                      as.character(rb$vdraft$name %||% ""))
    }
  })

  # ---- THE ADMIN HALF IS FILLED IN FOR YOU, exactly as it is for a statement
  #
  # The statement toolkit guesses the bank from the filename, offers the phrases
  # it found printed on the page, and composes the save name from the two -- so
  # the three boxes at the bottom cost nothing. The document builder asked for all
  # three by hand, which made setting up a document look harder than setting up a
  # statement when the only genuine difference is "which table did you mean?".
  # Measured end to end on a real document: ten decisions became seven.
  rb_id_auto <- reactiveVal(NA_character_)
  rb_fp_auto <- reactiveVal(NA_character_)
  rb_seeded <- reactiveVal(NA_character_)
  observe({
    p <- rb_doc(); if (is.null(p)) return()
    # ...UNLESS A SAVED TEMPLATE IS OPEN. Then the issuer and the phrase on screen
    # are somebody's answers, not the tool's guesses, and overwriting them with a
    # guess drawn from the filename is how an edit turns into a second template.
    if (!is.na(rb_editing())) return()
    # ...OR ANY WORK AT ALL IS ON SCREEN. A second example loaded under tables
    # that are already drawn is another example of THAT template, so the issuer
    # and the phrases on screen are answers too, whether or not it has been saved
    # yet. Without this H5's other half stands: the issuer is overwritten from
    # the new filename and the fingerprint wholesale.
    if (length(rb$tables) || length(rb$pairs)) return()
    nm <- input$ts_file$name %||% basename(p)
    if (identical(rb_seeded(), nm)) return()
    rb_seeded(nm)
    guess <- trimws(tools::toTitleCase(gsub("[^A-Za-z]+", " ",
                                            tools::file_path_sans_ext(nm))))
    if (nzchar(guess)) updateTextInput(session, "rb_bank", value = guess)
    # The phrases the document prints about ITSELF, found the same way the
    # statement drafter finds them. They still have to be looked at -- a
    # fingerprint of words every document carries is the one fault that turns a
    # correct "unsupported" into a silently-wrong read -- but retyping one by
    # hand, character for character, is where that goes wrong.
    ph <- tryCatch(header_phrases(rb_input()), error = function(e) character(0))
    ph <- ph[!is.na(ph) & nzchar(ph)]
    if (length(ph)) {
      v <- paste(utils::head(ph, 2), collapse = "\n")
      rb_fp_auto(v)
      updateTextAreaInput(session, "rb_fp", value = v)
    }
  })

  # A TABLE'S TITLE IS A BETTER FINGERPRINT THAN ITS COLUMN NAMES, and the
  # builder has one the moment a table is saved. "Public Health Outlay 2012-13"
  # is printed on this document and on no other; "Date Description Amount" is
  # printed on thousands. Only replaces what was suggested, never what was typed.
  observeEvent(rb$tables, {
    if (!length(rb$tables)) return()
    if (!is.na(rb_editing())) return()     # see the seeding observer above
    nm <- trimws(as.character(rb$tables[[1]]$name %||% ""))
    if (!nzchar(nm) || length(strsplit(nm, "\\s+")[[1]]) < 2L) return()
    cur <- trimws(input$rb_fp %||% "")
    if (nzchar(cur) && !identical(cur, trimws(rb_fp_auto() %||% ""))) return()
    rb_fp_auto(nm)
    updateTextAreaInput(session, "rb_fp", value = nm)
  }, ignoreInit = TRUE)
  observeEvent(list(input$rb_bank, input$rb_type), {
    if (is.null(rb_doc())) return()
    cur <- trimws(input$rb_id %||% "")
    if (nzchar(cur) && !identical(cur, rb_id_auto()) &&
        !identical(cur, "new_report")) return()          # hers now
    new <- .compose_id(input$rb_bank, input$rb_type, "pdf",
                       tools::file_path_sans_ext(input$ts_file$name %||% ""))
    rb_id_auto(new)
    updateTextInput(session, "rb_id", value = new)
  }, ignoreInit = TRUE)

  # ---- The template, the preview and the save -------------------------------
  rb_template <- reactive({
    ph <- trimws(strsplit(input$rb_fp %||% "", "\n")[[1]]); ph <- ph[nzchar(ph)]
    fr <- rb_frame()
    t <- document_template_from_proposal(
      id = input$rb_id %||% "new_report", bank = input$rb_bank %||% "NewIssuer",
      statement_type = input$rb_type %||% "report", phrases = ph,
      tables = rb$tables, pairs = .rb_all_pairs(), doc_pages = rb_n_pages())
    t$ref_width <- fr$ref_width; t$ref_height <- fr$ref_height
    t
  })
  output$rb_yaml <- renderText({ t <- rb_template(); t$origin <- NULL; yaml::as.yaml(t) })

  # THE NAME IT WILL SAVE UNDER, printed where the boxes that compose it are - so
  # folding those two boxes away hides a control, not a fact. (Register D2.)
  output$rb_id_note <- renderText({
    id <- trimws(as.character(input$rb_id %||% "")[1])
    if (!nzchar(id)) return("")
    sprintf("Saves as %s", id)
  })
  # WHAT SAVING DOES, beside the button that does it. Half of "what do I do next"
  # is what happens when you do. (Register D3.)
  output$rb_save_note <- renderText({
    n_t <- length(rb$tables); n_v <- length(.rb_all_pairs())
    e <- rb_editing()
    if (!n_t && !n_v)
      return("Nothing to save yet - draw a table or a value first.")
    what <- sprintf("%d table%s and %d value%s",
                    n_t, if (n_t == 1L) "" else "s",
                    n_v, if (n_v == 1L) "" else "s")
    # NO ID HERE. The header bar above already names the template being edited;
    # printing its id again turns a fact into a second question, and an id is not
    # something a forensic accountant can check against anything.
    if (!is.na(e) && nzchar(e))
      sprintf("Replaces the saved template with %s.", what)
    else sprintf("Adds %s to the library, ready to be used on the next document like this one.",
                 what)
  })

  output$rb_preview_btn <- renderUI({
    ready <- length(rb$tables) > 0L || length(.rb_all_pairs()) > 0L
    if (ready) return(actionButton("rb_preview", "Read the whole document",
                                   class = "btn-primary"))
    div(style = "display:flex;align-items:center;gap:10px;flex-wrap:wrap",
      actionButton("rb_preview", "Read the whole document",
                   class = "btn-primary disabled", `aria-disabled` = "true"),
      span(class = "muted", style = "font-size:12.5px",
           "Save a table or a value first."))
  })

  observeEvent(input$rb_preview, {
    i <- rb_input()
    if (is.null(i)) { showNotification("Upload the document at the top of this page first.",
                                       type = "warning", duration = 6); return() }
    if (!length(rb$tables) && !length(.rb_all_pairs())) {
      showNotification("Nothing to read yet - save a table or a value first.",
                       type = "warning", duration = 6); return()
    }
    withProgress(message = "Reading the whole document\u2026", value = 0.3, {
      ext <- tryCatch(extract_document(i, rb_template()), error = function(e) NULL)
      incProgress(0.5)
      rb$preview <- ext
      rb$outputs <- if (is.null(ext)) character(0) else {
        outdir <- file.path(tempdir(), paste0("rbprev-", session$token))
        unlink(outdir, recursive = TRUE)
        base <- tools::file_path_sans_ext(basename(input$ts_file$name %||% "document"))
        tryCatch(write_document_outputs(ext, outdir, base), error = function(e) character(0))
      }
    })
    if (is.null(rb$preview))
      showNotification("Couldn't read the document with these tables.", type = "error")
  })

  output$rb_prev_status <- renderUI({
    ext <- rb$preview
    if (is.null(ext)) {
      if (!length(rb$tables) && !length(.rb_all_pairs()))
        return(p(class = "muted", "Save a table or a value first."))
      return(p(class = "muted", "Press \u201cRead the whole document\u201d to see what comes out."))
    }
    # EVERY COUNT IS FORCED TO A NUMBER. A template with values and no tables
    # gives a zero-ROW summary, and sum() over a column of the wrong type is an
    # error, not a zero -- which is how "Read the whole document" came back
    # "invalid 'type' (character) of argument" for anyone who had drawn only
    # label/value pairs. R/doc_extract.R now types the empty frame; this is the
    # second lock on the same door.
    s <- ext$summary
    n_tab <- NROW(s)
    num <- function(v) { x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; x }
    weak <- if (n_tab) sum(!(as.character(s$found_by) %in% "its heading")) else 0L
    thin <- sum(num(s$thin_columns)); lost <- sum(num(s$unclaimed_words))
    n_rows <- sum(num(s$rows))
    empty <- sum(num(s$rows) < 1L); one_col <- sum(num(s$columns) == 1L)
    n_val <- NROW(ext$pairs)
    blank_val <- if (n_val && !is.null(ext$pairs$value))
                   sum(!nzchar(trimws(as.character(ext$pairs$value)))) else 0L
    bad <- weak + thin + lost + empty + blank_val
    plural <- function(n) if (n == 1L) "" else "s"
    tagList(
      div(class = if (bad) "verdict verdict-medium" else "verdict verdict-high",
          style = "margin:2px 0 12px",
        div(class = "verdict-ico", if (bad) "!" else "\u2713"),
        div(style = "flex:1;min-width:0",
          div(class = "verdict-title",
              sprintf("%d table%s, %d row%s, %d value%s",
                      n_tab, plural(n_tab), n_rows, plural(n_rows),
                      n_val, plural(n_val))),
          p(class = "verdict-body", style = "margin:0", paste(c(
            if (empty) sprintf("%d table(s) came out empty.", empty),
            # A one-column table cannot have an unclaimed word or a thin column,
            # so it passes every check there is while telling you nothing. Said
            # out loud, because "every column filled" reads like a result.
            if (one_col) sprintf(paste("%d table(s) have a single column, so each",
                                       "row arrives whole in one cell."), one_col),
            if (weak) sprintf("%d found by position rather than by a heading.", weak),
            if (thin) sprintf("%d column(s) came out mostly empty - check the column edges.", thin),
            if (lost) sprintf("%d word(s) inside a table were claimed by no column.", lost),
            if (blank_val) sprintf(paste("%d value(s) came back empty - the label was found and",
                                         "nothing was beside it."), blank_val),
            if (!n_tab && n_val)
              "No tables on this template - it reads labelled values only, which is a whole template on a form.",
            if (!bad && n_tab && !one_col)
              "Every table was found by its own heading and every column filled."),
            collapse = " ")))),
      # THE WORKBOOK, AND THE VALUES WHEN THERE ARE ANY. "Download one long CSV"
      # stood beside these: the same rows the workbook already holds, in a second
      # file shape, on a screen whose job is teaching a layout rather than
      # producing a deliverable - and Convert offers that exact file the moment
      # the template is saved.
      if (length(rb$outputs)) div(style = "margin:0 0 12px;display:flex;gap:8px;flex-wrap:wrap",
        downloadButton("rb_dl_xlsx", "Download the workbook", class = "btn-default"),
        # Offered only when there is something in it. A download button that
        # hands back a file saying "nothing to download" is a button that lies
        # about what the template made.
        if (n_val) downloadButton("rb_dl_values", "Download the values CSV",
                                  class = "btn-default")))
  })

  .rb_out <- function(pattern) {
    p <- rb$outputs[grepl(pattern, rb$outputs)]
    if (length(p) && file.exists(p[1])) p[1] else NA_character_
  }
  output$rb_dl_xlsx <- downloadHandler(
    filename = function() { p <- .rb_out("\\.xlsx$")
      if (is.na(p)) "nothing-to-download.txt" else basename(p) },
    content = function(file) { p <- .rb_out("\\.xlsx$")
      if (is.na(p)) return(.dl_note(file, NOTHING_TO_DL))
      file.copy(p, file, overwrite = TRUE) })
  output$rb_dl_values <- downloadHandler(
    filename = function() { p <- .rb_out("\\.values\\.csv$")
      if (is.na(p)) "nothing-to-download.txt" else basename(p) },
    content = function(file) { p <- .rb_out("\\.values\\.csv$")
      if (is.na(p)) return(.dl_note(file, NOTHING_TO_DL))
      file.copy(p, file, overwrite = TRUE) })

  # EVERY TABLE, NOT THE FIRST ONE.
  #
  # This showed the rows of `names(ext$tables)[1]` and nothing else, under a
  # summary line that faithfully listed all six tables on the document -- so the
  # screen said "6 tables, 412 rows" and then showed you 38 of them, with no
  # control anywhere to see the rest. The workbook has a sheet per table; this is
  # that, on screen, in the same order.
  #
  # The outputs are declared ONCE, up to a fixed maximum, because a DT rendered
  # inside a renderUI has no server-side render behind it and comes up blank.
  .RB_MAX_PREV <- 12L
  .rb_prev_keys <- reactive({
    ext <- rb$preview; if (is.null(ext) || !length(ext$tables)) return(character(0))
    utils::head(names(ext$tables), .RB_MAX_PREV)
  })
  output$rb_prev_head <- renderUI({
    ks <- .rb_prev_keys(); if (!length(ks)) return(NULL)
    ext <- rb$preview
    n_all <- length(ext$tables)
    tagList(
      h5(style = "margin:16px 0 2px",
         sprintf("What came out (%d table%s)", n_all, if (n_all == 1L) "" else "s")),
      lapply(seq_along(ks), function(i) {
        t <- ext$tables[[ks[i]]]
        n <- NROW(t$rows)
        tagList(
          div(style = "margin:14px 0 2px;display:flex;align-items:baseline;gap:8px;flex-wrap:wrap",
            strong(style = "font-size:15px", t$name %||% ks[i]),
            span(class = "muted", style = "font-size:12.5px",
                 sprintf("%d row%s", n, if (n == 1L) "" else "s")),
            if (!n) span(class = "chip chip-warn", "came out empty")),
          if (nzchar(as.character(t$detail %||% "")))
            p(class = "muted", style = "margin:0 0 4px;font-size:12.5px", t$detail),
          DTOutput(paste0("rb_prev_tbl_", i)))
      }),
      # NO SILENT CAP. Twelve is a lot of tables and more than any report seen so
      # far, but a screen that quietly stops at twelve reads as "that is all of
      # them".
      if (n_all > .RB_MAX_PREV)
        p(class = "muted", style = "margin:10px 0 0",
          sprintf("%d more table(s) are in the workbook but not shown here.",
                  n_all - .RB_MAX_PREV)))
  })
  lapply(seq_len(.RB_MAX_PREV), function(i) {
    output[[paste0("rb_prev_tbl_", i)]] <- renderDT({
      ks <- .rb_prev_keys(); req(length(ks) >= i)
      d <- rb$preview$tables[[ks[i]]]$rows
      req(!is.null(d))
      d <- d[, !grepl("__value$", names(d)), drop = FALSE]
      datatable(d, rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE, lengthChange = FALSE))
    })
  })
  # The label/value pairs, shown next to the tables rather than only inside the
  # workbook. They are half of what this builder makes and were the half you
  # could not see without downloading a file.
  output$rb_prev_values_head <- renderUI({
    ext <- rb$preview; if (is.null(ext)) return(NULL)
    n <- NROW(ext$pairs)
    tagList(h5(style = "margin-top:16px",
               sprintf("Values (%d)", n)),
            if (!n) p(class = "muted", style = "margin:0 0 6px",
                      "None on this template yet. The Values tab draws them.")
            else p(class = "muted", style = "margin:0 0 6px",
                   "Each of these is one label and the thing printed beside it."))
  })
  output$rb_prev_values <- renderTable({
    ext <- rb$preview; if (is.null(ext) || !NROW(ext$pairs)) return(NULL)
    ext$pairs
  }, striped = TRUE, spacing = "xs", width = "100%", na = "")

  output$rb_prev_summary_head <- renderUI({
    if (is.null(rb$preview)) return(NULL)
    h5(style = "margin-top:16px", "Every table on this document")
  })
  output$rb_prev_summary <- renderTable({
    ext <- rb$preview; if (is.null(ext)) return(NULL)
    ext$summary[, c("name", "pages", "rows", "columns", "thin_columns",
                    "unclaimed_words", "found_by"), drop = FALSE]
  })

  observeEvent(input$rb_save, {
    t <- rb_template()
    probs <- validate_document_template(t)
    if (length(probs)) {
      output$rb_msg <- renderUI(span(class = "bad",
        paste("Not valid:", paste(probs, collapse = "; ")))); return()
    }
    # A SAVE THAT CANNOT TAKE EFFECT IS WORSE THAN A REFUSED ONE.
    #
    # load_document_templates() keeps the FIRST template it finds for an id and
    # reads the curated folder first, so saving a user copy under a curated id
    # writes a file that is never loaded, never used and never mentioned - the
    # correction is simply outvoted, silently, by the very template it corrects.
    # The statement toolkit has met this before and answers it the same way: give
    # the copy its own name and say so. (Register L5.)
    shadowed <- {
      have <- tryCatch(all_doc_templates(), error = function(e) list())
      cur <- have[[as.character(t$id %||% "")[1]]]
      isTRUE(identical(as.character(cur$origin %||% "")[1], "default"))
    }
    renamed <- NA_character_
    if (shadowed) {
      renamed <- paste0(t$id, "_custom")
      t$refines <- t$id
      t$id <- renamed
    }
    ok <- tryCatch({ save_document_template(t, USER_DOC_DIR); TRUE }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      # THE DOCUMENT THIS WAS TAUGHT FROM IS NOW TAUGHT. The statement toolkit
      # marks its upload the moment it saves, which is what takes the file off
      # Admin's "needs pickup" list; the report builder did none of it, so every
      # document ever taught as a report stayed on that list for ever and the
      # queue grew instead of shrinking. Same three lines, same status word.
      # (Register L5.)
      # ...and only for the upload this template was actually taught FROM. The
      # tracked upload is whatever was converted last, so marking it blind would
      # take an unrelated file off the queue - which is the same fault as leaving
      # this one on it, pointing the other way.
      taught <- {
        h <- rb_handoff(); sc <- cv_src()
        !is.na(cv_upload_id()) && !is.null(h) && !is.null(sc) &&
          identical(as.character(h)[1], as.character(sc$path)[1])
      }
      if (isTRUE(taught))
        safe(set_upload_status(cv_upload_id(), "wizard_saved",
                               template = t$id %||% NA_character_, dir = UPLOADS_DIR))
      tpl_bump(isolate(tpl_bump()) + 1)
      rb_editing(as.character(t$id %||% "")[1])
      if (!is.na(renamed)) updateTextInput(session, "rb_id", value = renamed)
    }
    output$rb_msg <- renderUI(if (isTRUE(ok))
      tagList(
        span(class = "ok", sprintf(paste("Saved '%s'. On Convert it is used when no",
                                         "statement template reads the document. What it",
                                         "produces is a download - it never goes to the",
                                         "dashboards."), t$id)),
        if (!is.na(renamed)) div(class = "muted", style = "margin-top:4px",
          sprintf(paste("It was saved under a new name because '%s' is one of the",
                        "templates that shipped with the tool, and a copy under that",
                        "name would never have been used."), t$refines)))
      else span(class = "bad",
                sprintf("Couldn't save - check folder permissions on %s.", USER_DOC_DIR)))
  })

  # ---- X-ray, shown inline on the Convert tab (no separate upload/section).
  # Derived from the conversion result: read the converted file with its matched
  # template and lay out exactly what the engine selected on the page.
  ix_state <- reactive({
    res <- cv_res(); src <- cv_src(); if (is.null(res) || is.null(src)) return(NULL)
    tid <- (res$template_id %||% NA_character_)[1]
    if (is.na(tid) || !nzchar(tid)) return(NULL)
    tmpl <- tryCatch(templates()[[tid]], error = function(e) NULL); if (is.null(tmpl)) return(NULL)
    inp <- tryCatch(read_input(src$path), error = function(e) NULL); if (is.null(inp)) return(NULL)
    if (!identical(inp$kind, "pdf")) return(list(is_pdf = FALSE))
    # force_rows: user-confirmed rows are painted kept, so the X-ray matches what
    # the reader now emits after a force-include.
    layout <- tryCatch(inspect_pdf_layout(inp, tmpl, force_rows = cv_forced()), error = function(e) NULL)
    # the balance/period/account values are ALREADY in the conversion result -- reuse
    # them instead of re-scanning the whole document with extract_metadata.
    meta <- cv_res()$metadata %||% tryCatch(extract_metadata(inp), error = function(e) NULL)
    meta_loc <- NULL
    if (!is.null(meta)) {
      targets <- list(opening_balance = meta$opening_balance, closing_balance = meta$closing_balance,
                      period_start = meta$period_start, period_end = meta$period_end)
      if (length(meta$accounts)) targets$account <- meta$accounts[1]
      wbp <- inp$words %||% list()
      meta_loc <- lapply(seq_along(wbp), function(p)
        tryCatch(locate_values_on_page(wbp[[p]], targets), error = function(e) NULL))
    }
    list(is_pdf = TRUE, path = src$path, layout = layout, meta_loc = meta_loc)
  })
  # Answer "is this a PDF?" CHEAPLY (read_input is content-cached, ~1ms) instead of
  # pulling ix_state -- which would run the whole ~1.6s X-ray layout on EVERY PDF
  # convert just to drive the conditionalPanel, even with the X-ray tab closed.
  output$ix_is_pdf <- reactive({
    res <- cv_res(); src <- cv_src(); if (is.null(res) || is.null(src)) return(FALSE)
    isTRUE(identical(tryCatch(read_input(src$path)$kind, error = function(e) NULL), "pdf"))
  })
  outputOptions(output, "ix_is_pdf", suspendWhenHidden = FALSE)

  # How many pages the converted statement has. read_input is content-cached (~1ms),
  # so this is cheap enough to drive a label.
  ix_n_pages <- reactive({
    src <- cv_src(); if (is.null(src)) return(NA_integer_)
    inp <- tryCatch(read_input(src$path), error = function(e) NULL)
    if (is.null(inp) || !identical(inp$kind, "pdf")) return(NA_integer_)
    n <- length(inp$pages %||% inp$words %||% list())
    if (n >= 1L) as.integer(n) else NA_integer_
  })
  # ONE page number, clamped, shared by the picture, the legend, the skipped-row
  # table and "add this row" -- they each used to re-derive it, so they could
  # disagree about which page you were looking at.
  ix_page_now <- reactive(.clamp_page(input$ix_page, ix_n_pages()))
  # Say how many pages there are, on the control itself.
  observe({
    n <- ix_n_pages(); if (is.na(n)) return()
    updateNumericInput(session, "ix_page", max = n,
      label = if (n == 1L) "Page (this statement is 1 page)" else sprintf("Page (1 to %d)", n))
  })
  # ...and put the number BACK when it is one the document does not have. The
  # clamp above only ever applied to the picture: type 99 on an 11-page statement
  # and the box kept saying 99 while page 11 was drawn under it, so the control and
  # the picture disagreed about which page you were looking at -- and a reader
  # taking evidence off that page had no way to notice. Debounced, because a number
  # box sends every keystroke and correcting mid-type ("1" of "10") would fight the
  # person typing. The toast says what it clamped to; it replaces itself rather
  # than stacking.
  ix_page_settled <- debounce(reactive(input$ix_page), 1200)
  observe({
    n <- ix_n_pages(); v <- ix_page_settled()
    if (is.na(n) || is.null(v) || !length(v) || is.na(v)) return()
    p <- .clamp_page(v, n)
    if (identical(as.integer(v), as.integer(p))) return()
    updateNumericInput(session, "ix_page", value = p)
    notify_once("ix_page", sprintf("This statement has %d page%s - showing page %d.",
                                   n, if (n == 1L) "" else "s", p),
                type = "message", duration = 5)
  })

  ix_pal <- function(bands) {
    if (!length(bands)) return(character(0))
    stats::setNames(grDevices::hcl(seq(5, 320, length.out = length(bands)), 75, 50), names(bands))
  }
  # WHICH LAYERS ARE TICKED -- and "none" has to mean none.
  #
  # The picture and the legend each read `input$ix_layers %||% <all six>`, and an
  # empty checkboxGroupInput sends NULL, so unticking every box drew exactly the
  # same plot as ticking every box (md5-identical, verified). The label promised
  # "untick to hide a layer" and at the boundary it did the opposite.
  # `already_bound` distinguishes "the browser has not sent this input yet"
  # (draw everything, as before) from "the user unticked them all" (draw nothing
  # but the page). checkboxGroupInput sends its selection on bind, so the first is
  # a single frame at most.
  ix_layers_bound <- reactiveVal(FALSE)
  observeEvent(input$ix_layers, ix_layers_bound(TRUE), once = TRUE)
  ix_layers_now <- reactive({
    v <- input$ix_layers
    if (is.null(v)) return(if (isTRUE(ix_layers_bound())) character(0)
                           else c("cols", "kept", "skipped", "redact", "meta", "words"))
    as.character(v)
  })
  # .ix_unread(rows) -- of a page's visual rows, which ones LOOK like transactions
  # and did not read. The one place the X-ray decides it, for the amber boxes, the
  # key that names them and the order of the table underneath.
  #
  # IT ASKS THE ENGINE INSTEAD OF READING ITS PROSE. All three used to test
  # grepl("didn't parse|no amount", reason) against the sentence the parser
  # writes -- and the HEADING sentence is "no date and no amount - treated as a
  # heading, note or wrapped line", which contains "no amount". So every heading
  # and note on the page was drawn amber and legended "skipped row that looks like
  # a transaction" while the table beside it called the same row a heading:
  # measured at 109 amber rows against the engine's 2 on samples/_private_staging/
  # anz_single.pdf, and 26 against 2 on page 1 of westpac.pdf. R/parse_pdf_table.R
  # exports pdf_reason_actionable() for exactly this -- it maps the sentence back
  # through the table that produced it, so a reword cannot break it and no
  # substring can be shared by accident.
  .ix_unread <- function(rows) {
    if (is.null(rows) || !nrow(rows) || is.null(rows$reason))
      return(rep(FALSE, if (is.null(rows)) 0L else nrow(rows)))
    !(rows$kept %in% TRUE) & pdf_reason_actionable(rows$reason %||% "")
  }
  # A SWATCH THAT LOOKS LIKE WHAT IT NAMES (sw, below). Two of this key's entries
  # are drawn in the same amber -- an unread row, dashed outline, and a
  # machine-read word the scan doubted, shaded -- and both used to print an
  # identical solid square, so the key showed one colour twice with two meanings
  # and matched neither picture. The OCR line then had to say "shaded amber = " in
  # words to make up for its own swatch, which is the tell. sw()'s three style
  # arguments are the three the plot draws with, so the key cannot describe a
  # picture the page is not.
  output$ix_legend <- renderUI({
    st <- ix_state(); req(st, st$is_pdf, !is.null(st$layout))
    pg <- as.character(ix_page_now())
    P <- st$layout$pages[[pg]]; req(P)
    pal <- ix_pal(P$bands)
    layers <- ix_layers_now()
    sw <- function(col, lab, dashed = FALSE, fill = "transparent")
      tags$div(style = "margin:2px 0",
        tags$span(style = sprintf(paste0("display:inline-block;width:12px;height:12px;",
                                         "border:2px %s %s;background:%s;",
                                         "margin-right:6px;vertical-align:middle"),
                                  if (dashed) "dashed" else "solid", col, fill)),
        tags$span(lab))
    has_ocr <- !is.null(P$words$ocr_conf) && any(!is.na(P$words$ocr_conf))
    # A KEY NAMES WHAT IS ON THE PICTURE. It listed every ticked layer whether or
    # not that layer drew anything, so a page with no redactions still carried
    # "redaction (not read)" in its key -- and the only way to find out that the
    # colour is not there is to search the page for it. Each entry below now has
    # to have painted something on THIS page, using the same conditions the plot
    # itself draws from (a legend derived from different rules than the picture is
    # how the two come to disagree).
    w <- P$words
    rows <- P$rows
    has_cols <- length(pal) > 0 && ("cols" %in% layers) &&
      (any(!is.na(w$column)) ||
       any(vapply(P$bands, function(b) !is.null(b$x_min) && !is.null(b$x_max), logical(1))))
    n_kept <- sum(rows$kept %in% TRUE)
    n_skip <- sum(.ix_unread(rows))
    has_red <- any(w$redacted %in% TRUE)
    ml <- if (!is.null(st$meta_loc)) st$meta_loc[[ix_page_now()]] else NULL
    has_meta <- (!is.null(ml) && any(ml$found %in% TRUE)) || length(P$meta_regions %||% list()) > 0
    tagList(strong("Legend"),
      # Friendly names, and the SAME ones the picture writes over each band: the
      # stored column names are the engine's ("other_party"), not the reader's.
      if (has_cols) lapply(names(pal), function(nm) sw(pal[[nm]], cv_friendly_cols(nm))),
      if ("kept" %in% layers && n_kept > 0) sw(PALETTE$ok, "transaction row (kept)"),
      if ("skipped" %in% layers && n_skip > 0) sw(PALETTE$warn, UNREAD_ROW_PLAIN_KEY, dashed = TRUE),
      if ("meta" %in% layers && has_meta) sw(PALETTE$meta, "balance / account details"),
      if ("redact" %in% layers && has_red) sw(PALETTE$bad, "redaction (not read)", fill = pal_fill("bad", "22")),
      if (has_ocr) sw(PALETTE$warn, "machine-read word the tool is unsure about - double-check it",
                      fill = pal_fill("warn", "40")))
  })
  ix_render <- reactive({
    st <- ix_state(); req(st, st$is_pdf)
    render_page_view(st$path, ix_page_now(), 100)
  })
  output$ix_plot <- renderPlot({
    st <- ix_state(); req(st, st$is_pdf); r <- ix_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    lay <- st$layout; if (is.null(lay)) return(invisible())
    P <- lay$pages[[as.character(r$pg)]]; if (is.null(P)) return(invisible())
    layers <- ix_layers_now()
    reg <- P$region; ytop <- reg$y_min %||% 0; ybot <- reg$y_max %||% r$h
    pal <- ix_pal(P$bands)
    # #666666, not #666 -- see the note on area_line() in cv_trend: base R rejects
    # three-digit hex. This branch only fires for a template whose table region
    # carries an x_min, so the X-ray survived on the shipped set and would have
    # thrown "invalid RGB specification" on any template the band editor resized.
    if (!is.null(reg$x_min)) rect(reg$x_min, ybot, reg$x_max %||% r$w, ytop,
                                  border = "#666666", lty = 2, lwd = 1.4)
    w <- P$words
    if ("words" %in% layers && nrow(w))
      rect(w$x, w$y, w$x + w$width, w$y + w$height, border = "#cfcfcf", lwd = 0.4)
    # Machine-read (OCR) words the engine itself is unsure about: shaded amber so
    # "double-check the numbers" points at exactly the doubtful words. Uses the SAME
    # floor as the engine's ocr_low_conf row flag (PARAM_OCR_CELL_MIN_CONF, params.R).
    if (!is.null(w$ocr_conf)) {
      lc <- w[!is.na(w$ocr_conf) & w$ocr_conf < PARAM_OCR_CELL_MIN_CONF, , drop = FALSE]
      if (nrow(lc)) rect(lc$x, lc$y, lc$x + lc$width, lc$y + lc$height,
                         border = PALETTE$warn, col = pal_fill("warn", "40"), lwd = 1.2)
    }
    if ("cols" %in% layers) {
      sel <- w[!is.na(w$column), , drop = FALSE]
      if (nrow(sel)) rect(sel$x, sel$y, sel$x + sel$width, sel$y + sel$height,
                          border = pal[sel$column], lwd = 1.3)
    }
    if ("redact" %in% layers) {
      red <- w[w$redacted %in% TRUE, , drop = FALSE]
      if (nrow(red)) rect(red$x, red$y, red$x + red$width, red$y + red$height,
                          border = PALETTE$bad, col = pal_fill("bad", "22"), lwd = 1)
    }
    if ("cols" %in% layers) for (nm in names(P$bands)) { b <- P$bands[[nm]]
      if (!is.null(b$x_min) && !is.null(b$x_max)) {
        rect(b$x_min, ybot, b$x_max, ytop, border = pal[[nm]], lwd = 2)
        # The reader's name for the column, not the stored one -- and the same
        # string the legend prints, so the page and its key cannot differ.
        text((b$x_min + b$x_max) / 2, ytop, cv_friendly_cols(nm), col = pal[[nm]],
             font = 2, cex = 0.9, pos = 3, offset = 0.2)
      } }
    if ("kept" %in% layers) {
      kr <- P$rows[P$rows$kept, , drop = FALSE]
      if (nrow(kr)) rect(kr$x0 - 1, kr$y0 - 1, kr$x1 + 1, kr$y1 + 1, border = PALETTE$ok, lwd = 1)
    }
    # Amber dashed: rows the engine skipped that LOOK like transactions (bad date
    # or missing amount) -- the "why aren't you seeing it" rows. Continuations,
    # summaries and headings are intentionally left unhighlighted (they're in the
    # table below with their reason) so the page isn't noisy.
    if ("skipped" %in% layers) {
      sk <- P$rows[.ix_unread(P$rows), , drop = FALSE]
      if (nrow(sk)) rect(sk$x0 - 1, sk$y0 - 1, sk$x1 + 1, sk$y1 + 1,
                         border = PALETTE$warn, lty = 2, lwd = 1.6)
    }
    if ("meta" %in% layers && !is.null(st$meta_loc)) {
      ml <- st$meta_loc[[r$pg]]
      if (!is.null(ml)) { f <- ml[ml$found %in% TRUE, , drop = FALSE]
        if (nrow(f)) { rect(f$x0 - 2, f$y0 - 2, f$x1 + 2, f$y1 + 2, border = PALETTE$meta, lwd = 2)
          text(f$x1 + 3, (f$y0 + f$y1) / 2, f$field, col = PALETTE$meta, font = 2, cex = 0.8, adj = c(0, 0.5)) } }
    }
    # pinned header-value boxes (metadata_regions) the template defines for this page
    if ("meta" %in% layers) {
      mr <- P$meta_regions %||% list()
      for (nm in names(mr)) { b <- mr[[nm]]
        if (is.null(b$x_min) || is.null(b$x_max)) next
        y0 <- b$y_min %||% 0; y1 <- b$y_max %||% r$h
        rect(b$x_min, y0, b$x_max, y1, border = "#7b1fa2", lwd = 2, lty = 3)
        text(b$x_min, y0, nm, col = "#7b1fa2", font = 2, cex = 0.8, pos = 3, offset = 0.2)
      }
    }
  })
  # "Why aren't you seeing it": every row the engine skipped on this page, with the
  # plain-English reason. Kept rows are excluded (they're the transactions). The
  # actionable skips (bad date / missing amount) sort to the top.
  # The skipped rows for the CURRENT page, ordered likely-missed-transactions
  # first. ONE source of truth so the table's row numbers and the "add" action
  # below refer to exactly the same rows.
  ix_skipped_rows <- reactive({
    st <- ix_state(); if (is.null(st) || !isTRUE(st$is_pdf)) return(NULL)
    lay <- st$layout; if (is.null(lay)) return(NULL)
    pg <- ix_page_now()
    P <- lay$pages[[as.character(pg)]]
    if (is.null(P) || is.null(P$rows) || !nrow(P$rows) || is.null(P$rows$reason)) return(NULL)
    sk <- P$rows[!P$rows$kept & nzchar(P$rows$reason %||% ""), , drop = FALSE]
    if (!nrow(sk)) return(sk)
    sk[order(!.ix_unread(sk)), , drop = FALSE]       # likely-missed transactions first
  })
  output$ix_skipped <- renderDT({
    sk <- ix_skipped_rows()
    cols <- c("what's on the row", "date cell", "why it was skipped")
    # "nothing was skipped" is not a row of the table -- see dt_none_opts. Here it
    # matters twice over: this table is where a reviewer looks for a MISSING
    # transaction, and a placeholder in the rows is the last thing that should
    # look like one.
    none <- if (is.null(sk)) "Nothing to show for this page yet."
            else "Every row on this page was either kept or is a heading / footer."
    trunc <- function(s, n = 90) ifelse(nchar(s) > n, paste0(substr(s, 1, n), "\u2026"), s)
    out <- if (is.null(sk) || !nrow(sk))
      stats::setNames(data.frame(matrix(character(0), 0, length(cols))), cols)
    else data.frame(
      `what's on the row` = trunc(sk$raw %||% ""),
      `date cell` = sk$date %||% NA_character_,
      `why it was skipped` = sk$reason,
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(out, rownames = FALSE, selection = "single",
              options = dt_none_opts(none, pageLength = 10, dom = "tp", scrollX = TRUE))
  })
  # Shareable, PII-safe row-coverage diagnostic for the current statement: page
  # sizes vs the template reference, kept/skipped counts and reasons -- the numbers
  # needed to see why rows go missing WITHOUT the statement contents.
  output$ix_coverage_dl <- downloadHandler(
    filename = function() "row-coverage-diagnostic.md",
    content = function(file) {
      st <- ix_state(); src <- cv_src(); res <- cv_res()
      tid <- (res$template_id %||% NA_character_)[1]
      tmpl <- if (!is.na(tid) && nzchar(tid)) tryCatch(templates()[[tid]], error = function(e) NULL) else NULL
      # Same rule as the Admin exports: a download that cannot be produced hands
      # back the reason in the file, never an HTTP 500 error page.
      if (is.null(src) || is.null(tmpl)) {
        notify_once("ix_cov", "Convert a PDF statement first - nothing to diagnose yet.", duration = 6)
        return(.dl_note(file, "Nothing to diagnose yet: convert a PDF statement, then download this from its result page.")) }
      inp <- tryCatch(read_input(src$path), error = function(e) NULL)
      if (is.null(inp)) {
        notify_once("ix_cov", "Couldn't re-read the file for the diagnostic.", type = "error", duration = 6)
        return(.dl_note(file, "The statement could not be re-read from disk, so no diagnostic could be built. The scratch copy may have been cleaned up - convert it again.")) }
      writeLines(format_row_coverage(row_coverage(inp, tmpl)), file)
    })
  # "This IS a transaction": select a skipped row and add it. We record its page +
  # y-band as a force_rows entry and re-run the conversion, so the row lands in the
  # output flagged `forced` (and malformed / date_unresolved if its amount or date
  # couldn't be read) -- captured, and honestly labelled as hand-added.
  observeEvent(input$ix_add_row, {
    sk <- ix_skipped_rows()
    if (is.null(sk) || !nrow(sk)) {
      showNotification("No skipped rows on this page to add.", type = "warning"); return() }
    sel <- input$ix_skipped_rows_selected
    if (is.null(sel) || !length(sel)) {
      showNotification("Click a row in the table below first, then add it.", type = "warning"); return() }
    row <- sk[sel[1], , drop = FALSE]
    band <- list(page = ix_page_now(), y_min = row$y0 - 1, y_max = row$y1 + 1)
    cur <- cv_forced(); cur[[length(cur) + 1L]] <- band; cv_forced(cur)
    src <- cv_src(); sess <- cv_dir()
    if (is.null(src) || is.null(sess)) {
      showNotification("Convert a statement first.", type = "warning"); return() }
    # Re-runs in its own process like every other conversion. It deliberately does
    # NOT go through show_result(): keeping the forced rows is the whole point of
    # this path, and show_result clears them.
    cv_slot$start("convert", src$path, sess, message = "Re-checking that statement\u2026",
      args = convert_args(forced_rows = cv_forced()),
      finish = function(res) {
        cv_res(res)
        # Re-publish. Adding a row changes the figures the workbook and CSV hold, and
        # the feed is keyed by the statement's content hash, so this OVERWRITES that
        # statement's published rows rather than adding a second copy. Without it the
        # dashboards kept the pre-correction rows while the screen and the download
        # showed the corrected ones -- and the feed line above would have gone on
        # reporting a verdict from the previous run.
        publish_result(res, cv_recorded())
        showNotification("Added that row as a transaction (flagged 'forced') and re-checked the statement.",
                         type = "message", duration = 6)
      })
  })
  # Remediate a stuck upload right here: load the saved file into the SAME guided
  # toolkit the Convert tab uses, so a failed/abandoned statement is a 2-second
  # pickup - identify it in the table (A), open it, teach the tool, save (B).
  observeEvent(input$adm_up_wizard, {
    req(admin_ok())
    id <- input$adm_up_pick
    if (is.null(id) || !nzchar(id)) {
      showNotification("Pick a saved upload first.", type = "warning"); return() }
    p <- upload_file_path(id, UPLOADS_DIR)
    if (is.na(p) || !file.exists(p)) {
      showNotification("That upload's file is no longer available.", type = "error"); return() }
    # Same setup surface as Convert; upload_id ties a successful Save back to this
    # pickup so it drops off the list.
    open_guided(p, basename(p), upload_id = id)
  })

  # bank_choice() -- WHICH BANK the user asked for: ONE control, read in ONE place.
  # Blank means auto-detect. The conversion and the exact-template list both read
  # it here, so the list can never offer templates the conversion would not have
  # used. (Until now there were two pickers and this line preferred the front one,
  # which meant a bank chosen in the disclosure was thrown away without a word.)
  pick <- function(v) if (is.null(v) || !nzchar(v)) NULL else v
  bank_choice <- reactive(pick(input$cv_bank_quick))
  # WHAT KIND OF DOCUMENT THIS IS, read in ONE place for the same reason the bank
  # is. Two controls that both mean "this is not a statement" is how one of them
  # gets silently discarded.
  kind_choice <- reactive({
    k <- as.character(input$cv_kind %||% "auto")[1]
    if (k %in% c("auto", "statement", "other")) k else "auto"
  })
  # ...and the exact template, if one was forced. Same one-place reading, so the
  # single conversion and a whole case folder cannot honour different overrides.
  tpl_choice <- reactive(pick(input$cv_template))

  cv_res <- reactiveVal(NULL)
  cv_dir <- reactiveVal(NULL)
  cv_src <- reactiveVal(NULL)      # the uploaded file (path + name), for guided setup
  cv_fb_done <- reactiveVal(FALSE)
  cv_fb_rec  <- reactiveVal(NULL)              # the feedback record, incl. any feed retraction
  cv_upload_id <- reactiveVal(NA_character_)   # the tracked upload for this conversion
  cv_forced <- reactiveVal(list())             # user-confirmed "this IS a transaction" bands
  cv_feed_gate <- reactiveVal(NULL)            # what the governed feed did with it
  cv_recorded  <- reactiveVal(FALSE)           # ...and whether this run feeds at all
  # ---- ONE CONVERSION, ONE PROCESS -------------------------------------------
  #
  # The engine call used to happen right here, inside the observer, in the app's
  # only R thread. It now happens in a child process (R/jobs.R) and this side
  # LAUNCHES and POLLS. Between polls the R process is free, which is the whole
  # point: while one analyst's scan runs, every other browser keeps being served.
  #
  # A session can have more than one thing in flight -- a maintainer may run a
  # bulk audit on Admin while a statement converts on Convert -- so a slot is a
  # small object rather than one set of session variables. Starting a second
  # conversion in the SAME slot supersedes the first, process and all.
  job_slot <- function() {
    slot <- new.env(parent = emptyenv())
    slot$handle <- reactiveVal(NULL)
    slot$ctx <- NULL
    slot$bar <- NULL
    slot$close_bar <- function() {
      if (!is.null(slot$bar)) { safe(slot$bar$close()); slot$bar <- NULL }
    }
    slot$cancel <- function() {
      h <- isolate(slot$handle())
      if (!is.null(h)) safe(job_reap(h))
      slot$close_bar(); slot$ctx <- NULL; slot$handle(NULL)
    }
    slot$start <- function(task, paths, outdir, message, finish, args = list()) {
      slot$cancel()
      # A CONVERSION THAT CANNOT EVEN BE STARTED IS A FAILED CONVERSION, not a
      # failed app. job_start() writes the job's folder and its arguments to disk
      # before anything runs, so a full disk or a TEMP the service account cannot
      # write makes it throw -- and an error thrown here, inside an observer, ends
      # the whole Shiny session: the analyst's page greys out mid-click and takes
      # the result she was reading with it, on a box where a full disk means it
      # will do that to everybody, every time. It ends like any other conversion
      # that did not come back: the maintainer gets the cause in the error log, she
      # gets the plain sentence, and the app is still there for the next attempt.
      h <- tryCatch(do.call(job_start, c(list(paths, outdir), args,
                                         list(task = task, root = getwd()))),
                    error = function(e) e)
      if (inherits(h, "condition")) {
        safe(cat(sprintf("[%s] %s job could not be started: %s\n", format(Sys.time()),
                         as.character(task)[1], conditionMessage(h)),
                 file = file.path(LOGDIR, "errors.log"), append = TRUE))
        if (is.function(finish)) finish(list(status = "failed", messages = CONVERT_STOPPED))
        return(invisible(NULL))
      }
      # THE SAME progress panel withProgress used to raise, just held open across
      # polls instead of for the length of one blocking call -- so the centred
      # "converting" overlay (www/app.css, body.ss-run, which follows
      # .progress-message) looks and behaves exactly as before.
      slot$bar <- Progress$new(session)
      slot$bar$set(message = message, value = 0.15, detail = "Starting\u2026")
      slot$ctx <- list(finish = finish)
      slot$handle(h)
      invisible(h)
    }
    # THE POLL. invalidateLater re-runs this every half second while something is
    # in flight, and does nothing at all when nothing is.
    observe({
      h <- slot$handle(); if (is.null(h)) return()
      st <- job_poll(h)
      if (st %in% c("queued", "running")) {
        job_say(slot$bar, h, st)
        invalidateLater(JOB_POLL_MS, session)
        return()
      }
      fin <- slot$ctx$finish
      # Read the result BEFORE reaping: reaping deletes the folder it is in.
      res <- if (identical(st, "done")) job_result(h) else NULL
      if (is.null(res)) res <- job_failed_result(h)
      slot$close_bar(); slot$ctx <- NULL; slot$handle(NULL)
      safe(job_reap(h))
      if (is.function(fin)) fin(res)
    })
    slot
  }
  cv_slot  <- job_slot()   # converting a statement, a case folder, or a re-check
  adm_slot <- job_slot()   # the maintainer's bulk audit (Admin), which is longer still

  # WHAT THE WAITING PAGE SAYS. A silent wait is the exact failure this change
  # exists to remove, so a conversion that has not started yet says so and says
  # how many are in front of it -- and the number falls as the queue drains.
  job_say <- function(bar, h, st) {
    if (is.null(bar)) return(invisible(NULL))
    if (identical(st, "queued")) {
      n <- job_queue_ahead(h)
      return(invisible(bar$set(value = 0.05, detail = if (n <= 0L)
        "Yours starts in a moment."
        else sprintf("%d conversion%s ahead of yours - yours starts as soon as one finishes.",
                     n, if (n == 1L) "" else "s"))))
    }
    p <- job_progress(h)     # a case folder reports which file it is on
    invisible(bar$set(
      value  = if (is.null(p)) 0.4 else min(0.95, max(0.05, (p$i - 1) / max(p$n, 1))),
      detail = if (is.null(p)) "Reading the file and running the checks\u2026"
               else sprintf("%d of %d - %s", p$i, p$n, p$file)))
  }

  # A conversion that did not come back. The engine's own read failure keeps its
  # existing wording -- that case IS the tryCatch this replaced, and the sentence
  # is the one users already know. A process that DIED is a different fact and
  # gets its own sentence. The child's own words go to the maintainer's error log
  # and nowhere near the screen.
  #
  # THE RULE, AND IT ONLY GOES ONE WAY. `error` is the ONLY kind that means the
  # engine looked at this statement and refused it, and it is the only kind that
  # may be answered with a sentence about her file. Every other kind -- broken,
  # stopped, timeout, nostart, and anything added later -- is this server's
  # failing, and saying "it may be password-protected" about a file that is
  # perfectly good would send her back to her bank for a re-download that cannot
  # help, while nothing at all said the server was in trouble. R/jobs.R is where
  # that distinction is drawn (.job_exit_reason); this is the only place it is
  # spent, so the `else` below must stay the safe half.
  job_failed_result <- function(h) {
    f <- job_failure(h) %||% list(kind = "unknown", detail = NA_character_)
    safe(cat(sprintf("[%s] convert job %s (%s): %s\n", format(Sys.time()),
                     h$id %||% "?", f$kind, f$detail %||% ""),
             file = file.path(LOGDIR, "errors.log"), append = TRUE))
    list(status = "failed",
         messages = if (identical(f$kind, "error")) FRIENDLY_READ_ERROR else CONVERT_STOPPED)
  }

  # convert_args(...) -- the arguments the front door is called with, in ONE
  # place, so the Convert button and the X-ray's "this IS a transaction" re-run
  # can never ask for different things. (This is what convert_now() shared; that
  # function also RAN the conversion, and running it is now somebody else's job.)
  # An explicit force, or include_user, brings in the user-created template set:
  # include_user is for the moment right after a template is saved, when the
  # caller already knows the new template must take part even where the
  # deployment has user-built templates switched off.
  convert_args <- function(forced_rows = NULL, force_tpl = NULL, include_user = FALSE) {
    use_user <- USE_USER_TEMPLATES || !is.null(force_tpl) || isTRUE(include_user)
    list(bank = bank_choice(),
         templates_dir = TEMPLATES_DIR,
         user_templates_dir = if (use_user) USER_TEMPLATES_DIR else NULL,
         fields_dir = FIELDS_DIR, user_fields_dir = USER_FIELDS_DIR,
         doc_dir = DOC_DIR, user_doc_dir = USER_DOC_DIR,
         requested_by = who_now(), logdir = LOGDIR,
         force_template = force_tpl %||% tpl_choice(), force_rows = forced_rows,
         # WHAT SHE SAID IT IS. "auto" is the default and the old behaviour; the
         # other two take a whole half of the search off the table, which is the
         # only reliable answer to "a phrase printed on page 1 and it still used
         # another template".
         kind = kind_choice())
  }

  # When the browser tab closes, take this session's scratch folder with it. The
  # folder holds a copy of the client's statement plus every output; the process
  # temp dir is only cleared when R exits, and this app is a long-running service.
  # The JOBS go first, and they have to: a child is still writing into that folder,
  # and an abandoned OCR run would otherwise hold a core for two more minutes
  # producing a result no browser is left to read.
  session$onSessionEnded(function() {
    safe(cv_slot$cancel()); safe(adm_slot$cancel())
    d <- isolate(cv_dir())
    if (!is.null(d) && nzchar(d) && dir.exists(d)) try(unlink(d, recursive = TRUE), silent = TRUE)
  })

  # detected_identity_info() -- who the environment can actually ESTABLISH, without
  # anyone typing anything, AND how it established it. Order of trust:
  #   "host" -- the Shiny host's authenticated user (session$user: Shiny Server Pro,
  #             Posit Connect, RStudio auth). A real, per-person sign-in.
  #   "sso"  -- an identity a reverse proxy / SSO gateway forwards in a request
  #             header (oauth2-proxy, nginx auth_request, IIS/Windows-auth,
  #             Cloudflare Access). Also per-person.
  #   "os"   -- the OS account the SERVER PROCESS runs as. In the shipped
  #             one-server-for-the-team model this is the SAME for everybody and
  #             identifies NOBODY. It is kept (it is still a fact worth logging)
  #             but it must never be presented as the user's sign-in.
  #   "none" -- nothing at all.
  # Header values are only ever stored as a string in the audit log -- never
  # evaluated -- so there is no injection surface.
  .SSO_HEADERS <- c("HTTP_X_FORWARDED_USER", "HTTP_X_AUTH_REQUEST_USER",
    "HTTP_X_AUTH_REQUEST_EMAIL", "HTTP_X_FORWARDED_EMAIL", "HTTP_REMOTE_USER",
    "HTTP_X_REMOTE_USER", "HTTP_X_FORWARDED_PREFERRED_USERNAME", "HTTP_CF_ACCESS_AUTHENTICATED_USER_EMAIL")
  detected_identity_info <- function() {
    su <- session$user
    if (!is.null(su) && nzchar(trimws(su))) return(list(who = trimws(su), source = "host"))
    req <- session$request
    if (!is.null(req)) for (h in .SSO_HEADERS) {
      v <- tryCatch(req[[h]], error = function(e) NULL)
      if (!is.null(v) && nzchar(trimws(v))) return(list(who = trimws(v), source = "sso"))
    }
    cu <- current_user()
    if (!is.null(cu) && nzchar(cu) && !identical(cu, "unknown"))
      return(list(who = cu, source = "os"))
    list(who = NA_character_, source = "none")
  }
  detected_identity <- function() detected_identity_info()$who
  # Is the detected identity actually THIS PERSON (rather than the server's own
  # account)? Only a host/SSO sign-in is; that is the only case where pre-filling
  # the name box or saying "signed in as" is true.
  .identity_is_personal <- function(info) info$source %in% c("host", "sso")
  # who_now() -- what goes in the run log's requested_by: the name typed on
  # Convert, else whatever the environment could establish. NOTE this is the
  # ATTESTED-or-fallback value; it never stands alone in the log any more -- the
  # typed claim and the machine-detected identity are ALSO written as separate
  # fields (see stamp_identity below), so a reader can tell one from the other.
  # WHO RAN THIS, asked at most once a session and only when it cannot be
  # established. NA until given; conversion is blocked until then (see cv_go), so
  # no run is ever recorded against nobody.
  # A QID is six letters or digits. Checked here rather than accepted as free
  # text, because the audit trail is the point of asking: a typo, a name, or an
  # empty string recorded against a conversion is a record nobody can follow back
  # to a person, which is the same as having no record. Stored uppercase so the
  # same person is one identity in the log however they typed it.
  QID_PATTERN <- "^[A-Za-z0-9]{6}$"
  cv_qid <- reactiveVal(NA_character_)
  observeEvent(input$cv_qid_set, {
    v <- trimws(input$cv_qid %||% "")
    if (!grepl(QID_PATTERN, v)) {
      notify_once("cv_qid", "A QID is six letters or numbers, e.g. AB1234.",
                  type = "warning", duration = 6)
    } else { cv_qid(toupper(v)); clear_notice("cv_qid") }
  })
  observeEvent(input$cv_qid_change, cv_qid(NA_character_))
  output$cv_whoami <- renderUI({
    # A real per-person sign-in answers this already: ask nothing.
    if (.identity_is_personal(detected_identity_info())) return(NULL)
    q <- cv_qid()
    if (!is.na(q))
      return(div(class = "muted", style = "margin:-2px 0 12px",
                 sprintf("Recording as %s", q), " \u00b7 ",
                 actionLink("cv_qid_change", "change")))
    tagList(
      textInput("cv_qid", "Your QID", value = ""),
      # Say what it is FOR. Never say why it is needed: the old wording announced
      # that this server has no sign-in, which tells every person who opens the
      # page - including anyone who should not be on it - exactly where the door
      # is unlocked. What the tool cannot do is nobody's business but the
      # maintainer's, and it is in the docs where it belongs.
      #
      # The SHAPE is not a secret, and withholding it was the reason people typed
      # their name into this box and were refused: the validator below already
      # knows it is six characters, so the box says so before the refusal does.
      helpText(style = "margin-top:-6px",
        "Your six-character staff ID. Recorded as who ran this conversion. Asked once per session."),
      actionButton("cv_qid_set", "Use this QID", class = "btn-default"),
      tags$hr(style = "margin:14px 0"))
  })
  who_now <- function() {
    q <- cv_qid()
    if (!is.na(q) && nzchar(q)) return(q)
    detected_identity() %||% (session$user %||% current_user())
  }
  # stamp_identity(run_id) -- write BOTH facts onto the run record: what was typed
  # (a claim) and what the machine could establish (with its source). The engine
  # only knows one string; the UI knows both, so it completes the record here.
  # Never silent: if the record can't be completed (missing, or an ambiguous
  # same-second id) the user is TOLD, because an audit trail that quietly loses the
  # "who" is worse than one that says it did.
  stamp_identity <- function(run_id) {
    rid <- as.character(run_id %||% NA_character_)[1]
    # No run_id means the engine never got far enough to write a record at all
    # (the file could not be read). That failure is already on screen -- there is
    # nothing to amend and nothing extra to warn about.
    if (is.na(rid) || !nzchar(rid)) return(invisible(FALSE))
    info <- detected_identity_info()
    ok <- safe(amend_log_record(LOGDIR, "runs", rid,
      identity_fields(attested = cv_qid(), detected = info$who, source = info$source)), FALSE)
    if (!isTRUE(ok))
      showNotification(paste("This conversion ran, but its audit record could not be completed with who ran it.",
                             "Tell whoever looks after the tool before relying on this run."),
                       type = "warning", duration = 12)
    invisible(ok)
  }
  # run_conversion -- the whole convert-a-file flow (session dir, convert, state,
  # upload capture), shared by the Convert button and "Try it on a sample".
  # record = FALSE skips the Admin uploads capture (the bundled sample is not a
  # team statement to pick up).
  run_conversion <- function(srcpath, name, record = TRUE, force_tpl = NULL,
                             include_user = FALSE, upload_id = NULL) {
    old <- isolate(cv_dir())
    sess <- tempfile("cv_")   # guaranteed-unique per session/process (no cross-user bleed)
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, name)
    file.copy(srcpath, src, overwrite = TRUE)
    # STOP the previous conversion, then reclaim its scratch folder. Both halves
    # matter and in this order: it is a separate process now and it is still
    # writing in there, so deleting underneath it burns a core finishing a result
    # nobody will read (and on Windows the delete fails outright while its files
    # are open). The folder holds the outputs AND a copy of the client's
    # statement, and nothing was ever removing it -- on a server that runs for
    # months every conversion by every user stayed on disk. AFTER the copy above,
    # because a re-convert is often handed the file that lives in it.
    cv_slot$cancel()
    if (!is.null(old) && nzchar(old) && !identical(old, sess) && dir.exists(old))
      safe(unlink(old, recursive = TRUE))
    # ...and anything this process left behind more than a day ago, which is what a
    # browser closed abruptly (no onSessionEnded) leaves lying about. The folder
    # this conversion is using, and any file open in the toolkit, are excluded.
    safe(sweep_temp_dirs(keep_hours = 24,
                         exclude = c(sess, dirname(isolate(guided())$path %||% "."))))
    # A single conversion ends any case folder on screen. It has to: the sweep
    # above has just reclaimed the batch's scratch folder, so every other file's
    # workbook is gone, and a table whose downloads no longer resolve is worse
    # than no table.
    cv_batch(NULL); cv_batch_row(NA_integer_)
    # ...and it ends the statement on screen too, now that the answer arrives
    # seconds later rather than on this line. The sweep above has just deleted the
    # folder the last result's downloads resolve into, so leaving its card up
    # would leave a Download button pointing at a file that is gone.
    show_result()
    cv_dir(sess)
    who <- who_now()
    cv_slot$start("convert", src, sess, message = "Converting statement\u2026",
      args = convert_args(force_tpl = force_tpl, include_user = include_user),
      finish = function(res) {
        # Complete the audit record with the attested vs detected identity split.
        stamp_identity(res$run_id %||% NA_character_)
        # Capture the upload + its outcome so a failed/abandoned new format is a
        # 2-second pickup in Admin -> Uploads (the file is saved for a safe re-audit).
        uid <- if (record) safe(record_upload(src, name = name, requested_by = who,
          status = res$status %||% "failed", run_id = res$run_id %||% NA_character_,
          template = res$template_id %||% NA_character_,
          trust = res$trust$level %||% NA_character_,
          detail = paste(res$messages, collapse = "; "), dir = UPLOADS_DIR), NA_character_)
        else NA_character_
        # The result page is now THIS statement's, in one line...
        # upload_id given = this is a RE-RUN of a statement already picked up, so keep
        # its id. Passing it through beats the call-then-repair this replaced: setting
        # the state and then patching it back afterwards is exactly the shape
        # show_result() exists to remove.
        show_result(res, list(path = src, name = name), upload_id %||% uid)
        # ...and this is what the governed feed did with it (the last word on
        # cv_recorded / cv_feed_gate, which show_result has just cleared).
        publish_result(res, record)
        # "For Other statements... it shouldn't just auto process, it should
        # process and open up the editor. For statements I want a threshold where
        # it does and where it doesn't."
        #
        # `record` IS THE GATE, and it is the one flag that already means "a person
        # just handed the tool a file". The three re-runs that must stay silent all
        # pass record = FALSE: the bundled sample, convert_with_template's forced
        # re-convert, and the re-convert after the toolkit saves a template --
        # without it, saving a template that still reads at low confidence would
        # re-open the toolkit she has just closed, forever. A case folder never
        # auto-opens anything either: run_batch does not come through here, and
        # opening a batch row is browsing, not converting.
        #
        # The report route is a tab switch, not a modal, so cv_res / cv_src /
        # res$outputs are untouched and clicking back to Convert restores this
        # result with its Download buttons live. The statement route is the only
        # one that puts a modal up, and by default only a `low` one does -- the
        # case where the download is the wrong thing to want.
        if (isTRUE(record) && .needs_editor(res, EDITOR_MIN_TRUST)) {
          .edit_now()
          showNotification(if (.is_txn_result(res))
              "This one needs checking, so the template toolkit is open - your download is still on Convert."
            else
              "Opened the template that read this, so you can adjust it - your download is still on Convert.",
            type = "message", duration = 9)
        }
      })
  }

  # show_result(res, src, upload_id, gate, recorded) -- THE RESULT PAGE'S STATE.
  # Everything below the Convert sidebar (the verdict, the proof strip, the
  # transactions, the downloads, the feedback panel, the feed line, the "teach it
  # this layout" route) reads these seven reactives and nothing else.
  #
  # It is the one place a RESULT is opened, and it is NOT the only writer of every
  # field in it. Saying "defined ONCE" would be tidier and false, and a lone
  # maintainer would believe it. Three sites write into this set deliberately:
  #   * publish_result() sets cv_recorded / cv_feed_gate, because the feed verdict
  #     is only known AFTER the write;
  #   * the X-ray forced-row re-run must NOT come through here - this clears
  #     cv_forced, and keeping those rows is the entire point of that path;
  #   * open_guided() sets cv_upload_id when the toolkit adopts an upload.
  #
  # Three functions used to set overlapping subsets of them by hand -- a single
  # conversion, a batch finishing, and a batch row being opened -- and the way that
  # goes wrong is silent: one reactive left behind puts the PREVIOUS statement's
  # feed verdict, feedback panel or download beside this statement's figures, and
  # every one of those reads as a fact about the statement on screen. (It really
  # did: a finished batch left cv_feed_gate / cv_recorded holding the last file in
  # the loop.) Called with no arguments it means "no statement is open" -- which is
  # exactly the state a batch table sits in until a row is clicked.
  show_result <- function(res = NULL, src = NULL, upload_id = NA_character_,
                          gate = NULL, recorded = FALSE) {
    cv_res(res)
    cv_src(src)                        # the file itself, for the toolkit / X-ray
    cv_upload_id(upload_id)            # the tracked upload this result belongs to
    cv_feed_gate(gate)                 # what the governed feed did with THIS run
    cv_recorded(isTRUE(recorded))      # ...and whether this run feeds at all
    cv_fb_done(FALSE); cv_fb_rec(NULL) # the rating is per statement
    cv_forced(list())                  # rows forced onto the last one are not this one's
    invisible(res)
  }

  # publish_result(res, record) -- write the governed feed for this conversion and
  # keep the gate's verdict for the screen. ONE place, so what reaches Qlik and
  # what the screen claims about Qlik always come from the same run.
  #
  # Feeding used to be a silent side-effect: the verdict went to the feed manifest
  # and the feed log, and the person who ran the conversion was never told. A clean
  # statement read by a template she built here is withheld ("not_proven") -- and
  # she had every reason to believe her figures were on the dashboard.
  # `record = FALSE` (the bundled sample, and the preview re-convert after saving a
  # template) neither feeds nor claims anything about the feed.
  # Returns the gate so a caller converting MANY files can keep one verdict per
  # file. The two reactives it sets are show_result()'s, written here because the
  # feed verdict is only known once the write has happened; a batch clears them
  # again when its loop ends and sets them from the row the user opens.
  publish_result <- function(res, record) {
    cv_recorded(isTRUE(record))
    gate <- if (isTRUE(record)) safe(write_feed(res, CONFIG), NULL) else NULL
    cv_feed_gate(gate)
    invisible(gate)
  }

  # ---- A WHOLE CASE FOLDER, through the same front door ----------------------
  #
  # convert_batch() (R/batch.R) runs each file through convert_document(), so a
  # batch answer and a single-file answer for the same statement are the same
  # code and can never disagree. Everything below is screen: copy the uploads in,
  # show progress, publish each result exactly as a single conversion does, and
  # keep one row per file for the table.
  cv_batch     <- reactiveVal(NULL)          # the frame convert_batch() returned
  cv_batch_row <- reactiveVal(NA_integer_)   # which file's result is open below it
  output$cv_has_batch <- reactive({ !is.null(cv_batch()) })
  outputOptions(output, "cv_has_batch", suspendWhenHidden = FALSE)

  # .unique_names(x) -- the uploaded names, made unique inside one scratch folder.
  # Outputs are named after the file they came from, so two files both called
  # "statement.pdf" would have the second silently overwrite the first's workbook
  # and both rows would offer the same download. A "(2)" suffix is visible in the
  # table and in the downloaded file name, so the clash is stated, never quiet.
  #
  # IT CANNOT BE REPLACED BY GIVING EACH FILE ITS OWN SUBFOLDER, which is the
  # obvious-looking cure. The clash is in the OUTPUT name, not the input path:
  # convert_document() writes to `outdir` under
  # tools::file_path_sans_ext(basename(path)) (R/convert.R), and convert_batch()
  # takes ONE outdir for the whole case (R/batch.R passes `...` straight through,
  # so it cannot vary per file). Two inputs at sess/1/statement.pdf and
  # sess/2/statement.pdf therefore still both write sess/statement.xlsx, and both
  # rows' Download buttons -- which resolve through res$outputs -- still point at
  # the same workbook. This function is the only thing standing between a 30-file
  # case and one statement's figures downloading as another's.
  .unique_names <- function(x) {
    seen <- character(0)
    vapply(as.character(x), function(nm) {
      if (nm %in% seen) {
        base <- tools::file_path_sans_ext(nm); ext <- tools::file_ext(nm)
        k <- 2L
        repeat {
          cand <- sprintf("%s (%d)%s", base, k, if (nzchar(ext)) paste0(".", ext) else "")
          if (!(cand %in% seen)) break
          k <- k + 1L
        }
        nm <- cand
      }
      seen <<- c(seen, nm)
      nm
    }, character(1), USE.NAMES = FALSE)
  }

  run_batch <- function(files) {
    old <- isolate(cv_dir())
    sess <- tempfile("cvb_")
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    nms <- .unique_names(files$name)
    paths <- file.path(sess, nms)
    file.copy(as.character(files$datapath), paths, overwrite = TRUE)
    # Stop whatever this session had running before its folder is reclaimed: it is
    # a separate process, and it is still writing in there.
    cv_slot$cancel()
    if (!is.null(old) && nzchar(old) && !identical(old, sess) && dir.exists(old))
      safe(unlink(old, recursive = TRUE))
    safe(sweep_temp_dirs(keep_hours = 24,
                         exclude = c(sess, dirname(isolate(guided())$path %||% "."))))
    who <- who_now(); n <- length(paths)
    # No file is open yet -- and nothing is converted yet either. show_result()
    # with nothing in it says exactly that, and clears every per-file piece of
    # state in one place rather than leaving any of it pointing at the statement
    # whose scratch folder the sweep above has just deleted.
    show_result(); cv_batch_row(NA_integer_); cv_batch(NULL)
    cv_dir(sess)
    # A 50-file case must not look frozen, and it must not freeze the eight other
    # analysts either - a case folder blocks for far longer than one statement, so
    # it is the same one process per job, once for the whole case. ONE job, not one
    # per file: thirty files would otherwise fill the cap on their own and put the
    # whole team behind a single case.
    #
    # convert_batch() hands back each file's WHOLE result, rows included, and says
    # so: trimming is the caller's job because only the caller knows when it has
    # finished with them. This one has not -- the governed feed is written from the
    # parsed rows -- so they are dropped below, per file, the moment that write is
    # done. Anything convert_batch does not itself take goes to convert_document(),
    # which has no `...`, so a stray argument here fails every file in the case.
    cv_slot$start("batch", paths, sess, message = sprintf("Converting %d files\u2026", n),
      args = list(templates_dir = TEMPLATES_DIR,
        user_templates_dir = if (USE_USER_TEMPLATES) USER_TEMPLATES_DIR else NULL,
        fields_dir = FIELDS_DIR, user_fields_dir = USER_FIELDS_DIR,
        doc_dir = DOC_DIR, user_doc_dir = USER_DOC_DIR,
        requested_by = who, logdir = LOGDIR,
        bank = bank_choice(), force_template = tpl_choice(),
        # A case folder answers "what is this?" once for the whole folder, the
        # same as it answers "which bank" once. Thirty reports dropped in together
        # must not each be searched for a bank template.
        kind = kind_choice()),
      finish = function(b) {
        # A case that never came back is not an empty case. Say so on the verdict
        # card rather than draw a table of nothing.
        if (!is.data.frame(b)) return(show_result(b, NULL, NA_character_))
        # Each file finishes exactly as a single conversion does: its audit record is
        # completed with who ran it, its upload is captured for pickup, and it goes
        # through the governed feed. A batch that quietly skipped any of the three
        # would make "convert thirty" mean something different from "convert one,
        # thirty times", which is the one thing a batch must never do.
        b$upload_id <- rep(NA_character_, n)
        b$feed_gate <- vector("list", n)
        for (i in seq_len(n)) {
          res <- b$result[[i]]
          stamp_identity(res$run_id %||% NA_character_)
          b$upload_id[i] <- safe(record_upload(paths[i], name = nms[i], requested_by = who,
            status = res$status %||% "failed", run_id = res$run_id %||% NA_character_,
            template = res$template_id %||% NA_character_,
            trust = res$trust$level %||% NA_character_,
            detail = paste(res$messages, collapse = "; "), dir = UPLOADS_DIR), NA_character_)
          # `[i] <- list(...)`, never `[[i]] <-`: the gate is NULL when the feed is
          # switched off, and assigning NULL with [[ DELETES the element instead of
          # storing it - the column would come up one short of the files and the whole
          # batch would fail at the last statement.
          b$feed_gate[i] <- list(publish_result(res, TRUE))
          # The rows are on disk in this file's workbook / CSV / JSON; holding fifty
          # more copies in one object buys nothing. Marked with the engine's own
          # name for it, so a reader can tell "dropped" from "there were none".
          if (!is.null(res$feed_rows)) {
            res$feed_rows <- NULL; res$dropped_feed_rows <- TRUE; b$result[[i]] <- res
          }
        }
        # (The loop's publish_result calls leave the LAST file's feed verdict on
        # the screen's copy; this is what takes it off again.)
        show_result()
        cv_batch_row(NA_integer_)
        cv_batch(b)
      })
  }

  # open_batch_row(i) -- put THAT file's result on the ordinary result page. It
  # goes through show_result(), the same one line a single conversion uses, so the
  # page below is not a copy of the result view, it IS the result view.
  open_batch_row <- function(i) {
    b <- cv_batch()
    if (is.null(b) || length(i) != 1L || is.na(i) || i < 1L || i > nrow(b)) return(invisible(FALSE))
    show_result(b$result[[i]],
                src = list(path = b$file[i], name = basename(b$file[i])),
                upload_id = b$upload_id[i],
                # This file really was fed, in run_batch's loop: its own verdict,
                # never the one belonging to whichever row was open before.
                gate = b$feed_gate[[i]], recorded = TRUE)
    cv_batch_row(as.integer(i))
    invisible(TRUE)
  }
  # WHICH ROW IS OPEN, AND WHICH ROWS ARE TICKED, ARE TWO DIFFERENT QUESTIONS.
  # The table is multi-select now (see cv_batch), so "the selection" is a set --
  # but opening a file's result is still one click on one row, and DT's own
  # _row_last_clicked is exactly that click. Reading the selection instead would
  # leave the first file of a tick-list open while the person ticks the tenth.
  observeEvent(input$cv_batch_row_last_clicked, {
    i <- suppressWarnings(as.integer(input$cv_batch_row_last_clicked)[1])
    if (!is.na(i)) open_batch_row(i)
  })

  # The one line above the table: what came out of the whole case. Counted from
  # batch_summary(), which lists every status even at zero, so "0 could not be
  # read" is a fact the tally states rather than one a reader has to infer.
  output$cv_batch_summary <- renderUI({
    b <- cv_batch(); req(b)
    s <- batch_summary(b); n <- stats::setNames(s$n, s$status)
    say <- function(k, word) if (isTRUE(n[[k]] > 0L)) sprintf("%d %s", n[[k]], word) else NULL
    bits <- Filter(Negate(is.null), list(
      say("ok", "converted"),
      say("needs_review", "need a check"),
      say("unsupported", "with no template yet"),
      say("failed", "could not be read")))
    if (!length(bits)) bits <- list("nothing to convert")
    div(style = "margin:2px 0 10px",
      h4(style = "margin:0 0 2px", sprintf("%d files", nrow(b))),
      # paste0, not a second argument: shiny's tag builder joins children with a
      # space, so the tally read "2 with no template yet ." with a gap before it.
      p(class = "muted", style = "margin:0",
        paste0(paste(unlist(bits), collapse = "  -  "), ".")))
  })

  # .is_graded(status) -- IS THERE ANY WORK TO BE CONFIDENT ABOUT? A run that
  # produced nothing has no confidence grade: "low" beside "No template for this
  # statement yet" reads as a warning about the file rather than a plain statement
  # of where we are. The verdict card has withheld it for a while (cv_status,
  # `graded`); the case-folder table did not, so on a three-file case the row for
  # a layout with no template read "No template for this statement yet | - | 0 |
  # low" -- a grade for a conversion that never happened, in the column a reviewer
  # skims to pick which of thirty files to open first.
  .is_graded <- function(status) as.character(status %||% "") %in% c("ok", "needs_review")

  # THE TABLE. Sorting is the whole point of it, so the two columns worth sorting
  # by sort by MEANING, not by spelling: "Result" orders worst-first off the
  # engine's own status order (BATCH_STATUSES), not alphabetically, and the table
  # opens on that order with the failure kind as the tie-break -- so the pile that
  # needs work is already at the top and already grouped.
  output$cv_batch <- renderDT({
    b <- cv_batch(); req(b)
    b$trust[!.is_graded(b$status)] <- NA_character_   # nothing converted, nothing to grade
    dash <- function(v) { v <- as.character(v); v[is.na(v) | !nzchar(v)] <- "-"; v }
    # BATCH_STATUSES is worst-LAST, so a status's position in it IS its severity:
    # ok 1 ... failed 4, and anything the engine has learned to say since 5, which
    # sorts to the very top because a verdict this screen has no wording for is the
    # one a person most needs to look at. Sorted DESCENDING below. (It used to be
    # this same match() subtracted from one-past-the-end and then sorted ascending
    # -- the same order written as a double negative, with the nomatch value
    # load-bearing by accident.)
    severity <- match(b$status, BATCH_STATUSES, nomatch = length(BATCH_STATUSES) + 1L)
    disp <- data.frame(
      File = basename(b$file),
      Result = vapply(b$status, plain_status, character(1), USE.NAMES = FALSE),
      Bank = dash(b$bank),
      Rows = suppressWarnings(as.integer(b$rows)),
      # THE SAME GRADE THE SINGLE-FILE CARD PRINTS. convert_batch() has carried
      # `trust` all along and this table threw it away, so the row for a file read
      # `Converted successfully` with `-` under What to check, while opening that
      # very row said "Converted - 11 transactions read - confidence: medium /
      # Read cleanly. Something could not be proven". Two gradings of one file,
      # the more confident one on the screen built for skimming - and on a
      # thirty-file case there was no way to tell the clean files from the
      # merely-uncomplaining ones without opening all thirty.
      #
      # The word is the card's word, not a second vocabulary: cv_headline prints
      # `res$trust$level` and so does this -- INCLUDING when the card prints none
      # (blanked above, .is_graded).
      Confidence = dash(b$trust),
      # plain_failing_check() lives in ui_labels.R with the other wording maps: the
      # engine carries its own CODE ("check:balance_reconciliation") so it never has
      # to read a UI file off disk, and the words are added here.
      `What to check` = dash(plain_failing_check(b$failing_check)),
      order = severity,
      check.names = FALSE, stringsAsFactors = FALSE)
    # DataTables counts columns from zero, and the two that matter are addressed
    # by NAME rather than by a literal: adding Confidence in the middle shifted
    # every index below it, and a hard-coded 5 would have hidden the wrong column
    # and sorted the table on the wrong key with nothing to notice it.
    at <- function(nm) which(names(disp) == nm) - 1L
    # MULTI-SELECT, because the job a thirty-file case ends in is "three of these
    # failed, fix the template, run those three again". Single-select made that
    # thirty row-clicks and thirty separate downloads; the re-run path itself has
    # existed all along (run_batch takes any list of files). Clicking one row still
    # opens it -- that is _row_last_clicked, above -- so nothing anybody already
    # does changes; ticking several now also means something.
    datatable(disp, rownames = FALSE, selection = "multiple",
      options = list(
        # No scrollX: it makes DataTables clone the header into the scroll body,
        # and the clone's sort arrows render as a stack of triangles over the last
        # column. Six short columns fit without it.
        pageLength = 15,
        order = list(list(at("order"), "desc"), list(at("What to check"), "asc")),
        columnDefs = list(list(visible = FALSE, targets = at("order")),
                          # Clicking "Result" sorts on the hidden severity key, and
                          # DataTables' default first click is ASCENDING - which on
                          # this key puts the CLEAN files on top, on the very click
                          # meant to gather the failures. The table exists to group
                          # what went wrong, so worst-first is the first click.
                          list(orderData = at("order"), targets = at("Result"),
                               orderSequence = list("desc", "asc"))))) |>
      formatStyle("Confidence", fontWeight = "bold",
                  color = styleEqual(c("high", "medium", "low"),
                                     c(PALETTE$ok, "#8a6d00", PALETTE$bad)))
  })

  # WHAT A CASE FOLDER IS ACTUALLY FOR, ONCE THE TABLE HAS BEEN READ.
  #
  # "Thirty files, three failed: no way to re-run just those three and no way to
  # download the other twenty-seven." The deliverable for a 30-file case was 30
  # row-clicks and 30 separate downloads, and after building the template the
  # three failures needed, the only way to use it was to upload the whole case
  # again. Both answers were already sitting there -- run_batch takes any list of
  # files, and every converted file's outputs are on disk in one folder -- so this
  # is two actions over what exists, not a new pipeline.
  #
  # THEY ONLY APPEAR WITH SOMETHING TO ACT ON: the re-run needs ticked rows and
  # says so where the ticking happens, and the download needs a file that was
  # actually written. A control that cannot do anything is worse than no control.
  .batch_outputs <- function(b) {
    if (is.null(b)) return(character(0))
    p <- unlist(lapply(b$result, function(r) as.character(r$outputs %||% character(0))))
    p <- unique(p[nzchar(p)])
    p[file.exists(p)]
  }
  output$cv_batch_open <- renderUI({
    b <- cv_batch(); req(b)
    sel <- as.integer(input$cv_batch_rows_selected %||% integer(0))
    sel <- sel[!is.na(sel) & sel >= 1L & sel <= nrow(b)]
    outs <- .batch_outputs(b)
    actions <- div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:10px;margin:10px 0 0",
      if (length(sel))
        actionButton("cv_batch_again",
                     sprintf("Convert %s again", if (length(sel) == 1L) "this file"
                             else sprintf("these %d files", length(sel))),
                     class = "btn-default")
      else span(class = "muted", style = "font-size:13px",
                "Tick the files you want to convert again."),
      if (length(outs))
        downloadButton("cv_batch_dl", "\u2b73 Download everything", class = "btn-primary"))
    i <- cv_batch_row()
    if (is.na(i))
      return(tagList(p(class = "muted", style = "margin:8px 0 0",
                       "Click a row for that file's full result."), actions))
    tagList(actions,
      div(style = "margin:16px 0 2px;padding-top:12px;border-top:1px solid var(--line)",
        h4(style = "margin:0", sprintf("Showing: %s", basename(b$file[i])))))
  })
  # THE SAME ENGINE CALL, ON FEWER FILES. run_batch copies the paths it is given
  # into a fresh scratch folder, so handing it the ticked rows' files is exactly
  # what dropping those files into the picker would have done -- one code path, and
  # a re-run and a first run of the same file can never disagree.
  observeEvent(input$cv_batch_again, {
    b <- cv_batch(); req(b)
    if (!.identity_ok()) return()
    sel <- as.integer(input$cv_batch_rows_selected %||% integer(0))
    sel <- sel[!is.na(sel) & sel >= 1L & sel <= nrow(b)]
    if (!length(sel)) {
      notify_once("cv_batch_again", "Tick the files you want to convert again first.",
                  type = "warning", duration = 6)
      return()
    }
    clear_notice("cv_batch_again")
    paths <- as.character(b$file[sel])
    keep <- file.exists(paths)
    if (!any(keep)) {
      showNotification("Those files are no longer on this server - upload them again.",
                       type = "error", duration = 10)
      return()
    }
    # SAID, NEVER SILENT: a case whose scratch folder has been reclaimed under it
    # would otherwise re-run fewer files than were ticked with nothing to show it.
    if (!all(keep))
      showNotification(sprintf("%d of those files are no longer on this server, so only the rest are being converted again.",
                               sum(!keep)), type = "warning", duration = 10)
    run_batch(data.frame(name = basename(paths[keep]), datapath = paths[keep],
                         stringsAsFactors = FALSE))
  })
  # ONE FILE HOLDING THE WHOLE CASE. The zip is built from the outputs already on
  # disk, so it carries exactly what the per-file buttons carry, and its name is
  # the case's own scratch handle rather than "download.zip".
  output$cv_batch_dl <- downloadHandler(
    filename = function() sprintf("converted-case-%s.zip", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      outs <- .batch_outputs(cv_batch())
      if (!length(outs)) {
        notify_once("dl", NOTHING_TO_DL, duration = 6)
        return(.dl_note(file, NOTHING_TO_DL))
      }
      # zip::zip is what openxlsx already brings, so it is on every box this runs
      # on and needs no external tool -- which matters on a machine with no
      # internet and no Rtools. utils::zip is the fallback and needs one, so a
      # host with neither says so in a file that opens rather than handing back a
      # zip that does not.
      root <- unique(dirname(outs))
      ok <- tryCatch({
        if (requireNamespace("zip", quietly = TRUE) && length(root) == 1L)
          zip::zip(file, files = basename(outs), root = root)
        else utils::zip(file, outs, flags = "-j9Xq")
        file.exists(file) && file.info(file)$size > 0
      }, error = function(e) FALSE)
      if (!isTRUE(ok)) {
        msg <- "The case could not be packed into one file - download the files you need one at a time."
        notify_once("dl", msg, duration = 10)
        .dl_note(file, msg)
      }
    })

  # No sign-in and no QID means the run would be recorded against the account the
  # SERVER process runs as -- identical for the whole department, identifying
  # nobody. For a tool whose output is meant to be defensible, that is worse than
  # stopping, so it stops. Once per session, not once per statement -- and once
  # per BATCH, not once per file in it.
  #
  # ONE gate, because there is more than one way to start a conversion. The sample
  # button ran the whole flow -- result card, checks, working Excel / CSV / JSON
  # downloads -- with no QID at all, and the run it wrote to the log carried the
  # server's own account. The same argument that stops the Convert button stops it.
  .identity_ok <- function() {
    if (.identity_is_personal(detected_identity_info()) || !is.na(cv_qid())) return(TRUE)
    notify_once("cv_qid",
                "Enter your QID first - it is what the audit trail records as who ran this conversion.",
                type = "warning", duration = 8)
    FALSE
  }
  # WHY THE CONVERT BUTTON IS OFF, said where the button is. The two reasons are
  # different jobs -- one is a file, one is a name -- so they are never merged
  # into "you cannot do this yet".
  output$cv_go_btn <- renderUI({
    who <- .identity_is_personal(detected_identity_info()) || !is.na(cv_qid())
    got <- !is.null(input$cv_file) && NROW(input$cv_file)
    if (who && got)
      return(actionButton("cv_go", "Convert", class = "btn-primary btn-lg btn-block"))
    tagList(
      actionButton("cv_go", "Convert",
                   class = "btn-primary btn-lg btn-block disabled",
                   `aria-disabled` = "true"),
      p(class = "muted", style = "margin:6px 0 0;font-size:12.5px",
        if (!got && !who) "Choose a file above, and enter your QID."
        else if (!got) "Choose a file above."
        else "Enter your QID above - it records who ran this conversion."))
  })
  outputOptions(output, "cv_go_btn", suspendWhenHidden = FALSE)

  observeEvent(input$cv_go, {
    f <- input$cv_file
    if (is.null(f) || !nrow(f)) {
      notify_once("cv_file", "Choose a statement file first.", type = "warning", duration = 6)
      return()
    }
    if (!.identity_ok()) return()
    if (nrow(f) > 1L) run_batch(f) else run_conversion(f$datapath[1], f$name[1])
  })

  # The button says what it is about to do. Twelve files selected and a button
  # marked "Convert" leaves the user to wonder whether it means all of them.
  observe({
    f <- input$cv_file
    n <- if (is.null(f)) 0L else nrow(f)
    updateActionButton(session, "cv_go",
                       label = if (n > 1L) sprintf("Convert %d files", n) else "Convert")
  })

  # "Try it on a sample": convert the bundled specimen statement, so the very
  # first visit can show the whole payoff (verdict, analysis, downloads) without
  # the user needing a statement at hand.
  observeEvent(input$cv_try_sample, {
    if (!file.exists(SAMPLE_STATEMENT)) {
      notify_once("cv_sample",
                  "The bundled sample statement isn't on this install (samples/ folder missing).",
                  type = "warning", duration = 6)
      return()
    }
    if (!.identity_ok()) return()
    run_conversion(SAMPLE_STATEMENT, basename(SAMPLE_STATEMENT), record = FALSE)
  })

  # A result exists once a conversion has run -- gates the whole result scaffold so
  # a first-time visitor never sees bare "Checks / Diagnostics" headers over empty
  # tables (which read as half-built).
  output$cv_has_result <- reactive({ !is.null(cv_res()) })
  outputOptions(output, "cv_has_result", suspendWhenHidden = FALSE)
  # A parse that produced rows: gates the analysis cards / graph / transactions,
  # so an unsupported or failed result shows its verdict + next step, not an
  # empty dashboard of zeros.
  output$cv_has_txns <- reactive({
    res <- cv_res()
    isTRUE((res$status %||% "") %in% c("ok", "needs_review")) &&
      .is_txn_result(res) && length(res$outputs %||% character(0)) > 0
  })
  outputOptions(output, "cv_has_txns", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------------
  # "Show me how it read this" -- the one control that separates the accountant's
  # view from the evidence view (charter: the interface rule).
  #
  # STICKY BY DESIGN. Once someone opens it, it stays open for every conversion
  # for the rest of their session. That is the whole answer to "some users are
  # more technical than others": we do not ask them which they are, and we do not
  # make them re-open it on every file. They tell us by clicking once, and the
  # tool remembers. Closing it again is equally sticky, so nobody is stuck with a
  # view they did not want.
  cv_detail_open <- reactiveVal(FALSE)
  # Has the person expressed a preference this session? Once they have, it is
  # theirs: auto-open must never fight someone who deliberately closed it.
  cv_detail_touched <- reactiveVal(FALSE)
  observeEvent(input$cv_more, {
    cv_detail_touched(TRUE)
    cv_detail_open(!isTRUE(cv_detail_open()))
  })
  # OPEN ITSELF WHEN SOMETHING IS FLAGGED. On a clean run the page stays lean; the
  # moment a check fails or the run needs review, the page that diagnoses it - the
  # statement with the bands drawn on it - is already open rather than behind a
  # link somebody has to know about. Reported as the single most useful view when
  # something has gone wrong, so it should not need finding at exactly that moment.
  observeEvent(cv_res(), {
    if (isTRUE(cv_detail_touched())) return()
    res <- cv_res(); if (is.null(res)) return()
    k <- res$kpis
    flagged <- !identical(res$status %||% "", "ok") ||
      (!is.null(k) && "status" %in% names(k) && any(k$status %in% "fail"))
    if (isTRUE(flagged)) cv_detail_open(TRUE)
  })
  output$cv_detail_open <- reactive({ isTRUE(cv_detail_open()) })
  outputOptions(output, "cv_detail_open", suspendWhenHidden = FALSE)

  output$cv_more_toggle <- renderUI({
    res <- cv_res(); req(res)
    if (!.is_txn_result(res)) return(NULL)
    open <- isTRUE(cv_detail_open())
    # Named for what is BEHIND it, not for the mechanism. "Advanced" would make an
    # accountant feel it is not for her; "how it read this" is a question she may
    # genuinely want answered, and it is the honest description of the contents.
    div(style = "margin:16px 0 6px",
      actionLink("cv_more", style = "font-weight:700;font-size:14.5px",
        label = if (open) "Hide how it read this" else "Show me how it read this"),
      # One caption, and it names only what is actually behind THIS link. It said
      # "the checks" while the checks were a second click further in; they now sit
      # above it, one click from the verdict, so the caption stops claiming them.
      div(class = "muted", style = "font-size:13px;margin-top:2px",
          "The statement with the columns drawn on it (PDF only), the charts, and the template it used."))
  })

  # Empty state: shown before the first conversion. Tells a brand-new user what
  # this page is for and exactly what they'll get back, so the screen is never a
  # mystery or a wall of empty headers.
  #
  # ...AND IT PROMISES WHAT THE ANSWER SHE GAVE CAN DELIVER. It described a bank
  # statement whatever "What is this?" said - every transaction read verbatim,
  # whether it reconciles - three inches under a radio button reading "Something
  # else - a report, a form, a letter". A report has no transactions and nothing
  # to reconcile, so every line of that was a promise the other route cannot
  # keep, made to the person who had just said which route she was on. The
  # sample offer goes with it: the bundled specimen IS a bank statement, so on
  # the other route it is not offered, because offering it would be the fourth
  # promise of the same kind.
  output$cv_empty <- renderUI({
    other <- identical(kind_choice(), "other")
    # ONE link element, written once and used by both, because two literal
    # actionLinks with one id is exactly the accident the "no id used twice" scan
    # exists to catch -- and it cannot tell a deliberate pair from a mistake.
    to_tmpl <- actionLink("cv_empty_to_tmpl", "Add a template")
    div(style = "max-width:560px;color:#444;line-height:1.6",
      h4(style = "margin-top:4px",
         if (other) "Convert a report, a form or a letter" else "Convert a bank statement"),
      if (other)
        p("Upload it on the left - a ", tags$b("PDF"), " - and click ", tags$b("Convert"), ".")
      else
        p("Upload a statement on the left - a ", tags$b("PDF"), ", ", tags$b("CSV"),
          " or ", tags$b("Excel"), " file - and click ", tags$b("Convert"), "."),
      p(class = "muted", style = "margin-bottom:6px", "You'll get back:"),
      if (other)
        tags$ul(style = "color:#444",
          tags$li(tags$b("The tables and figures a template points at"), ", read verbatim off the page."),
          # NOT "whether it reconciles". There is no reconciliation on this route
          # and there cannot be - nothing on a report adds up to anything the tool
          # can check - so promising one here would be the screen making a promise
          # the engine has no way to keep. What it does give is where every figure
          # came from, which is the honest half of the same assurance.
          tags$li(tags$b("Where each one came from"), " - the page it was on, and how it was found."),
          tags$li(tags$b("Your download"), " - Excel or CSV. These never reach the dashboards."))
      else
        tags$ul(style = "color:#444",
          tags$li(tags$b("Every transaction"), ", read verbatim - date, description, amount, balance."),
          # NOT "proof nothing's missing". On four of the six statements this was
          # measured against, balance_reconciliation could not run at all -- no
          # printed opening or closing and no running-balance column to derive one
          # from -- so the page promised a proof it then did not give. What it always
          # does give is the answer either way.
          tags$li(tags$b("Whether it reconciles"), " - the balance proof, or plainly why it couldn't run."),
          tags$li(tags$b("Your download"), " - Excel or CSV.")),
      if (other)
        p(class = "muted", "It is read by a form or report template. If none of them fits this layout yet, you set one up on ",
          to_tmpl, " by pointing at what you want.")
      else
        p(class = "muted", "Your bank is detected automatically. A layout the tool hasn't seen points you to ",
          to_tmpl, "."),
      # First visit, nothing to upload yet? One click shows the whole payoff on
      # a bundled specimen statement (public, synthetic - not anyone's real data).
      # NOT OFFERED ON THE OTHER ROUTE: the bundled specimen is a bank statement,
      # so a "try it on a sample" button under "Something else" would convert a
      # statement to demonstrate a route that does not read statements. Nothing
      # ships that this route can be shown on yet - see the register's J9 - and a
      # button that quietly answers a different question is worse than no button.
      if (!other && file.exists(SAMPLE_STATEMENT))
        div(style = "margin-top:14px;padding:12px 14px;background:#f8faf9;border:1px dashed #bfe0c8;border-radius:10px",
          actionButton("cv_try_sample", "Try it on a sample statement", class = "btn-default"),
          div(class = "muted", style = "margin-top:6px", "No file needed.")))
  })

  output$cv_status <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    # A successful transaction statement gets the plain hero headline (cv_headline
    # below); this card is kept for form results and for anything that did NOT
    # convert cleanly, so failures still explain themselves up top. It is the SAME
    # verdict card as the success headline -- one visual language for "how did it
    # go", rather than a second, hand-coloured one for bad news.
    if (isTRUE(res$status == "ok") && .is_txn_result(res)) return(NULL)
    st <- res$status %||% "failed"
    lvl <- switch(st, ok = "high", needs_review = , unsupported = "medium", "low")
    # THE TIE HEADLINE, ONLY WHERE THERE IS A PICK TO MAKE. An ordinary tie
    # CONVERTS: R/convert.R takes the best candidate (tested over hand-built, then
    # deterministic), reads the statement and holds it at needs_review with the tie
    # named in the message. This line was replacing "Converted - please double-check
    # it" on those runs -- demanding a pick on a screen with no picker anywhere on
    # it (cv_tie_pick renders only when the status is `unsupported`), and throwing
    # away the confidence grade the engine had already computed for 7 real rows.
    # So it is tied to the same condition the picker is: the tie whose winner read
    # nothing, which is the one case where there genuinely is a choice to make.
    ambig <- isTRUE(res$detect$ambiguous) && identical(st, "unsupported")
    headline <- if (ambig) STATUS_PLAIN_AMBIGUOUS else plain_status(st)
    # ...AND A HIGH-SEVERITY DIAGNOSIS NOBODY CAN FIX WITH A TEMPLATE OUTRANKS
    # BOTH. On an `unsupported` run the engine's message is always the same
    # sentence -- "we don't have a template for this layout yet" -- whatever the
    # real reason was, so an image-only PDF on a machine with no OCR software got
    # a headline about templates while the engine's own diagnostics, at severity
    # high, said there was no text on the page to read and that a template would
    # not help. R/diagnose.R calls that generic message "actively misleading" in
    # its own words. The diagnosis takes the title; the template sentence stays
    # underneath as the secondary fact it is, because nothing is dropped.
    bdx <- if (st %in% c("unsupported", "failed")) .blocking_diag(res) else NULL
    if (!is.null(bdx)) headline <- .sentence(bdx$detail[1])
    # No confidence grade on a run that produced nothing: there is no work to be
    # confident about, and "confidence: low" beside "no template yet" reads as a
    # warning about the file rather than a plain statement of where we are. The
    # case-folder table grades by the same rule (.is_graded, above cv_batch).
    graded <- st %in% c("ok", "needs_review")
    trust <- if (!is.null(res$trust) && graded) sprintf(" \u00b7 confidence: %s", res$trust$level) else ""
    # Name the template ONLY when one was actually used to read the statement. On
    # an unsupported result the engine still carries a template id -- the CLOSEST
    # MISS, kept for the logs -- and printing it under "No template for this
    # statement yet" read as though that template had read the file. It had not.
    tid <- if (st %in% c("ok", "needs_review")) (res$template_id %||% NA_character_)[1]
           else NA_character_
    div(class = paste0("verdict verdict-", lvl),
      div(class = "verdict-ico", if (identical(lvl, "high")) "\u2713" else "!"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", paste0(headline, trust)),
        lapply(plain_messages(res$messages), function(m) p(class = "verdict-body", .sentence(m))),
        failed_checks_ui(res),
        if (!is.na(tid) && nzchar(tid))
          div(span(class = "chip", paste("Read as:", friendly_tpl(tid))))))
  })

  # plain_messages(m) -- the engine's status messages with their MACHINE CODES
  # taken off, and empties dropped.
  #
  # Two kinds of code reached the screen. The leading status ("needs_review: ...")
  # was already stripped; the KPI clauses were not, so the verdict card read
  # "parsed 22 row(s) but review needed; 2 KPI(s) failed: balance_reconciliation,
  # running_balance_continuity". ui_labels.R's own note says a raw code on screen
  # "is the moment a forensic reviewer stops trusting the screen" -- and those very
  # checks are listed directly underneath by failed_checks_ui(), in the words the
  # Checks table uses and with the engine's figures beside them. So the clause was
  # the same fact twice, once in a language the reader cannot use.
  plain_messages <- function(m) {
    m <- sub("^(ok|needs_review|unsupported|failed):\\s*", "", as.character(m %||% character(0)))
    m <- gsub(";?\\s*[0-9]+ KPI\\(s\\) (failed|not applicable):[^;]*", "", m)
    m <- trimws(sub("^\\s*;\\s*", "", sub("\\s*;\\s*$", "", m)))
    m[nzchar(m)]
  }
  # .sentence(x) -- a capital where the machine code used to be. Every engine
  # message is written to follow "needs_review: ", so once plain_messages has
  # taken the prefix off, the verdict card printed "we don't have a template for
  # this layout yet" and "parsed 1 row(s) but review needed" -- lower-case
  # fragments in the largest type on the screen, which read as log output rather
  # than as the tool speaking. FIRST LETTER ONLY: everything after it is the
  # engine's words verbatim, and a message already starting with a capital, a
  # digit or a quote is untouched.
  .sentence <- function(x) sub("^([a-z])", "\\U\\1", as.character(x), perl = TRUE)

  # failed_checks_ui(res) -- WHICH check failed, in the words the Checks table
  # uses, with the engine's own figures beside it.
  #
  # The engine knows exactly which of the ten checks failed and by how much; the
  # card only ever showed its message, whose reason fragment names the check by
  # its internal code ("1 KPI(s) failed: balance_reconciliation") and gives no
  # figures. So the one thing a reviewer needs on a needs-review result -- what to
  # go and look at -- was a click away at the bottom of the page under "Checks &
  # detail". Same source of truth as that table (res$kpis) and the same wording
  # (CHECK_PLAIN), so the two can never say different things. NULL when nothing
  # failed, so a clean result is unchanged.
  #
  # ...AND THE DIAGNOSTIC THAT OUTRANKS THEM ALL, FIRST. build_diagnostics()
  # returns most-severe-first, and the top row on a real needs_review run read
  # "This upload looks like more than one statement bundled together, which
  # corrupts a single parse. Split it into one statement per file and re-run." --
  # severity HIGH, and nowhere on the card. "What to check" listed only the
  # secondary dates item, so the highest-severity thing on the page was the one
  # thing the list of what to check left out.
  #
  # ONLY where a conversion happened. On a `failed` or `unsupported` result the
  # engine's headline message IS the top diagnostic, already printed on this card
  # two lines up, so listing it again would say one sentence twice.
  #
  # ...AND THE SAME FAULT IS NOT LISTED TWICE UNDER TWO NAMES. The two halves of
  # this list are built from different frames, and the engine writes the SAME
  # sentence into both when they are the same fault. Measured on a real ASB
  # statement, "What to check" read:
  #     dates couldn't be read - no row dates could be read - the date column
  #       mapping or format is wrong
  #     Row dates could be read - no row dates could be read - the date column
  #       mapping or format is wrong
  # -- one fault, two vocabularies, and the second bullet's own name says the
  # OPPOSITE of what happened, because CHECK_PLAIN words a check as what it
  # PROVES for use beside a pass/fail column that this list does not have.
  # ui_labels.R solved that once for the batch table (plain_failing_check prefixes
  # "Failed: "); the verdict card never got it. Both are fixed below: the check
  # says it failed, and a check whose detail is BYTE-IDENTICAL to a listed
  # diagnostic's is dropped as the echo it is. Byte-identical, never fuzzy -- if
  # the engine ever words them differently they are two facts again and both
  # appear, which is the safe way round.
  top_diagnostics <- function(res) {
    d <- res$diagnostics
    empty <- data.frame(category = character(0), severity = character(0),
                        detail = character(0), how_to_fix = character(0),
                        stringsAsFactors = FALSE)
    if (!isTRUE((res$status %||% "") %in% c("ok", "needs_review"))) return(empty)
    if (!is.data.frame(d) || !nrow(d) ||
        !all(c("category", "severity", "detail", "how_to_fix") %in% names(d))) return(empty)
    # "info" is context, not a fault, and "none" is the explicit no-issues row --
    # the same two exclusions R/batch.R makes when it picks a file's headline
    # problem, so the card and the batch table can never disagree about what the
    # top issue is.
    d[d$severity %in% "high" & !(d$category %in% "none"),
      c("category", "severity", "detail", "how_to_fix"), drop = FALSE]
  }
  failed_checks_ui <- function(res) {
    k <- res$kpis
    f <- if (is.null(k) || !all(c("name", "status") %in% names(k))) NULL
         else k[k$status %in% "fail", , drop = FALSE]
    dg <- top_diagnostics(res)
    if (!is.null(f) && nrow(f) && nrow(dg))              # the echo, dropped
      f <- f[!((f$detail %||% "") %in% dg$detail), , drop = FALSE]
    if ((is.null(f) || !nrow(f)) && !nrow(dg)) return(NULL)
    tagList(
      div(class = "verdict-body", style = "margin-top:2px",
        tags$b("What to check:"),
        tags$ul(style = "margin:4px 0 0 18px;padding:0",
          # The diagnostic named in plain English (never its code) and what was
          # actually seen -- the same shape as a failing check, so the list reads
          # as one list. Its remedy is not repeated here; it is the action line
          # below, which is the only place on the card that tells anyone to DO
          # something.
          lapply(seq_len(nrow(dg)), function(i) tags$li(
            tags$b(plain_label(dg$category[i], DIAG_PLAIN)),
            if (nzchar(dg$detail[i] %||% "")) sprintf(" - %s", dg$detail[i]) else NULL)),
          lapply(seq_len(if (is.null(f)) 0L else nrow(f)), function(i) tags$li(
            tags$b(sprintf("Failed: %s", plain_check(f$name[i]))),
            if (nzchar(f$detail[i] %||% "")) sprintf(" - %s", f$detail[i]) else NULL)))),
      # THE REMEDY IS THE TOOL'S OWN, AND IT SITS WITH THE DIAGNOSIS. The
      # prominent action on this screen used to be "Set up the right template",
      # rendered further down the page and keyed on the verdict rather than on any
      # evidence -- so a run whose top diagnostic said "split the file" carried a
      # warning-coloured button pointing at the template toolkit instead. One
      # screen, two remedies, and the tool contradicting itself.
      if (nrow(dg) && nzchar(dg$how_to_fix[1] %||% ""))
        div(class = "verdict-body", style = "margin-top:6px",
            tags$b("Do this first: "), dg$how_to_fix[1]) else NULL)
  }

  # THE TAG AN AUTO-SPLIT RUN PUTS ON EVERY CHECK, read in ONE place.
  #
  # R/split.R names each segment's checks "<code> [statement 2]", so every reader
  # of a KPI name on this page has to know the tag. Three readers did, each
  # carrying its own copy of the regex, and the fourth -- the proof strip, the
  # first quality signal on the page -- did not, and matched nothing at all on a
  # bundle. One definition, so a fifth reader cannot be written that quietly
  # disagrees with the other four. ui_labels.R keeps its own copy on purpose: it
  # is loaded by the test suite without app.R, and the wording map is what the
  # suite holds to its word.
  .STMT_TAG <- "[[:space:]]*\\[statement ([0-9]+)\\]$"
  .stmt_base <- function(name) sub(.STMT_TAG, "", as.character(name))
  # Which statement of a bundle a check belongs to -- a NUMBER, and 0 for a file
  # that was not split, so a caller can group by it without special-casing an NA.
  .stmt_index <- function(name) {
    name <- as.character(name)
    n <- sub(paste0("^.*", .STMT_TAG), "\\1", name)
    out <- suppressWarnings(as.integer(n))
    out[n == name | is.na(out)] <- 0L        # no tag, or one that will not read
    out
  }

  # THE CHECKS THAT MATTER, ALWAYS ON SCREEN.
  #
  # Only FAILING checks were shown. That reads as "no news is good news", and for a
  # tool whose whole purpose is a defensible figure it is the wrong way round: the
  # reason to trust this output is that the opening balance plus every transaction
  # equals the closing balance the statement prints, and a passing proof said
  # nothing at all. It sat inside a panel nobody opens on a clean run.
  #
  # These four are the ones a forensic reviewer would ask about, in the order they
  # would ask. Everything else stays in the full checks table.
  #   - does it add up (the cardinal proof)
  #   - was every row read
  #   - does the running balance follow from row to row
  #   - could every date be read
  # A dash means the check could not run on this statement (no printed closing
  # balance, no running balance column) - which is a fact worth seeing, not a pass.
  # amount_direction is here because it is the check that catches what actually
  # goes wrong most: the amount column or the debit/credit mapping. It used to
  # surface only when it failed, which is the wrong way round for the most common
  # error - a reviewer wants to see that one confirmed, not merely not-complained-about.
  .PROOF_CHECKS <- c("balance_reconciliation", "no_unparsed_rows", "amount_direction",
                     "running_balance_continuity", "dates_readable")
  # .proof_pick(name, status, listed) -- which chips the strip draws, in order.
  #
  # THE FIVE ABOVE ARE THE ONES WORTH CONFIRMING ON A CLEAN RUN. They are not the
  # five that can go wrong: dates_within_period and transaction_count are verdict
  # checks that can FAIL, and they were permanently off this strip with no way
  # back on. So a real statement drew three ticks and a dash under a key promising
  # "x = a problem", while the Checks table two disclosures down read "All dates
  # fall in the statement period | Problem | 34 date(s) outside period" -- and a
  # statement that PRINTED nine transactions and gave up seven drew four chips,
  # none red, with transaction_count failed. The first quality signal on the page
  # said clean about the one thing this tool exists to catch.
  #
  # Appended, never substituted: nothing is hidden, a clean run's strip is exactly
  # what it was (there is nothing to append), and the strip is now INCAPABLE of
  # being all-clear while any check failed. `%in%` rather than `==` so an NA status
  # cannot slip an NA name into the list.
  #
  # SPLIT-AWARE, because it was matching whole KPI names against the bare check
  # codes. An auto-split upload is several statements in one file and R/split.R
  # tags every check "<code> [statement 2]", so on a bundle NOTHING matched: the
  # picker came back empty and the renderer's `if (!length(pick)) return(NULL)`
  # took the strip AND its key off the page altogether -- measured at zero
  # characters on an 824-row three-statement ANZ file that had converted cleanly.
  # Worse when a check had failed: the `setdiff` branch still matched (it compares
  # names to names), so the bundle drew nothing but RED chips under a key whose
  # first entry defines a tick. The picking is now per statement, because that is
  # how a bundle is reconciled -- the listed checks in the order a reviewer asks
  # them, then anything that failed and is not on the list, for each part in turn.
  .proof_pick <- function(name, status, listed) {
    name <- as.character(name)
    keep <- .stmt_base(name) %in% listed | status %in% "fail"
    if (!any(keep)) return(character(0))
    nm <- name[keep]
    rank <- match(.stmt_base(nm), listed)
    rank[is.na(rank)] <- length(listed) + 1L   # failed, and not one of the listed
    nm[order(.stmt_index(nm), rank)]
  }
  output$cv_proof <- renderUI({
    res <- cv_res(); req(res)
    k <- res$kpis
    if (is.null(k) || !all(c("name", "status") %in% names(k))) return(NULL)
    if (!nrow(k)) return(NULL)
    pick <- .proof_pick(k$name, k$status, .PROOF_CHECKS)
    if (!length(pick)) return(NULL)
    chip <- function(nm) {
      st <- k$status[k$name == nm][1]
      m <- switch(st %||% "na",
        pass = list("\u2713", PALETTE$ok, "#e9f5ec"),
        fail = list("\u2717", PALETTE$bad, "#fdecef"),
        list("\u2013", "#6b7780", "#f2f4f6"))     # na / anything else: could not run
      span(style = sprintf(paste0("display:inline-flex;align-items:center;gap:6px;",
                                  "margin:0 8px 6px 0;padding:5px 10px;border-radius:999px;",
                                  "background:%s;color:%s;font-size:13px;font-weight:600"),
                           m[[3]], m[[2]]),
           # The row already says which statement, so the chip does not repeat it.
           span(style = "font-size:14px", m[[1]]), plain_check(.stmt_base(nm)))
    }
    # A BUNDLE IS CHECKED PART BY PART, AND NOW SAYS SO. Each statement in a split
    # upload is reconciled on its own, and the reviewer's question is whether EVERY
    # part was proved -- so the strip is one labelled row per statement. The parts
    # are counted off the whole check table rather than off the chips, so a part
    # with nothing to draw still appears, and says so, instead of vanishing.
    part_of <- .stmt_index(k$name); parts <- sort(unique(part_of)); n_parts <- max(parts)
    div(style = "margin:2px 0 12px",
      lapply(parts, function(s) {
        got <- pick[.stmt_index(pick) == s]
        div(style = "margin-bottom:2px",
          if (s > 0L) div(class = "muted", style = "font-size:12.5px;font-weight:600",
                          sprintf("Statement %d of %d", s, n_parts)),
          if (length(got)) lapply(got, chip)
          else span(class = "bad", "No check ran on this part of the file."))
      }),
      # THE KEY, because this strip is the first quality signal on the page and it
      # had none: no legend, no title attribute, nothing in About or in either
      # guide. A grey dash beside "Opening + transactions = closing balance" above
      # the fold is either the best or the worst news on the screen, and there was
      # nothing anywhere to tell a reader which. The glyphs are the SAME escapes
      # the chips are drawn from, not literals retyped beside them: a key that can
      # drift from its picture is worse than no key at all.
      div(class = "muted", style = "font-size:12.5px;margin-top:2px",
          sprintf("%s = checked and passed \u00b7 %s = a problem \u00b7 %s = could not be checked (why, in Checks below)",
                  "\u2713", "\u2717", "\u2013")))
  })

  # tpl_choices(ids) -- ids for the server, NAMES for the person. A template id is
  # a maintainer's handle, and the charter's interface rule keeps it off a
  # customer-facing screen; two pickers on the result page were offering the raw
  # ids as their options.
  #
  # Identical names are numbered, never collapsed: several templates carrying one
  # name is exactly what a tie often IS, and a picker whose two options read the
  # same is a question nobody can answer.
  tpl_choices <- function(ids) {
    ids <- as.character(ids)
    lab <- vapply(ids, function(i) friendly_tpl(i), character(1), USE.NAMES = FALSE)
    blank <- is.na(lab) | !nzchar(lab); lab[blank] <- ids[blank]
    if (anyDuplicated(lab))
      lab <- stats::ave(lab, lab, FUN = function(x)
        if (length(x) == 1L) x else sprintf("%s (option %d)", x, seq_along(x)))
    stats::setNames(ids, lab)
  }
  # .tpl_label(bank, type) -- bank + statement type as a name Beth reads, or NA
  # when there is nothing to build one from. Lifted out because two template sets
  # feed it (transaction templates and form templates) and one of them was
  # printing raw ids for want of these four lines.
  #
  # "statement statement". A PDF drafted in the toolkit is saved with
  # statement_type "statement" (R/draft.R), so the word was appended to a label
  # that already ended in it -- "Read as: Sample Everyday Statement statement".
  # The word is a suffix, not a fact about the template, so it goes on only when
  # the name does not already say it.
  # NA is EMPTY here, not the two letters "N" and "A". `%||%` only replaces NULL,
  # so a template whose bank is missing used to paste the string "NA" into the
  # label -- which is exactly the "Read as: NA NA statement" this helper's other
  # caller was fixed for. Absent means absent, and a label built from nothing at
  # all comes back NA so the caller can fall back.
  .tpl_label <- function(bank, type) {
    one <- function(v) { s <- trimws(as.character(v %||% "")[1]); if (is.na(s)) "" else s }
    lab <- trimws(paste(one(bank), one(type)))
    if (!nzchar(lab)) return(NA_character_)
    if (grepl("statements?$", lab, ignore.case = TRUE)) lab else paste(lab, "statement")
  }
  # Form (mode: fields) templates, loaded the same way convert_document loads them.
  # They are deliberately NOT in all_templates() -- keeping them out of that set is
  # what stops them affecting transaction detection - which is exactly why
  # friendly_tpl had nothing to look up.
  all_field_templates <- reactive({
    tpl_bump()
    tryCatch(load_fields_templates(FIELDS_DIR, USER_FIELDS_DIR), error = function(e) list())
  })
  # Report (mode: document) templates, the same way. The only set that existed was
  # the ADMIN one (include_hidden = TRUE), which is the wrong set for anything on
  # the Convert page: a template somebody deliberately parked must not be handed
  # back on a result, and must not be offered as one to force. Same loader
  # convert_document uses, so what this lists and what a conversion could use are
  # the same templates.
  all_doc_templates <- reactive({
    tpl_bump()
    tryCatch(load_document_templates(DOC_DIR, USER_DOC_DIR), error = function(e) list())
  })
  # friendly_tpl -- turn a template id (e.g. "bnz_everyday_csv") into a name Beth
  # reads ("BNZ everyday statement"). Falls back to the id if we can't resolve it.
  friendly_tpl <- function(tid) {
    if (length(tid) != 1 || is.na(tid) || !nzchar(tid)) return(NA_character_)
    # A FORM template is not in the statement set, and `list[["missing"]]` on a
    # named list gives one NULL element named NA -- which template_overview() turns
    # into a row of NAs, so an IRD form that extracted all seven of its fields
    # correctly was labelled "Read as: NA NA statement".
    #
    # ...and then it fell back to the RAW ID, so the chip read "Read as:
    # anz_kiwisaver_fields" -- one wrong answer swapped for an engine code on a
    # customer-facing screen, which the charter's interface rule forbids outright.
    # The form template carries `bank: ANZ` and `statement_type: kiwisaver`, so the
    # name is derivable from the same two fields as any other template's; ask the
    # set it really lives in before giving up.
    #
    # ...AND A REPORT TEMPLATE IS IN NEITHER SET, so a converted report's chip read
    # its raw id too -- the same breach, on the route with the least else to go on.
    # It does NOT go through .tpl_label: that helper appends the word "statement"
    # to anything that does not already end in it, which would name a trustee
    # report "Acme quarterly report statement". A report is named by what it is.
    if (!(tid %in% names(all_templates()))) {
      ft <- tryCatch(all_field_templates()[[tid]], error = function(e) NULL)
      if (!is.null(ft)) {
        lab <- .tpl_label(ft$bank, ft$statement_type)
        return(if (is.na(lab)) tid else lab)
      }
      dt <- tryCatch(all_doc_templates()[[tid]], error = function(e) NULL)
      if (!is.null(dt)) {
        one <- function(v) { s <- trimws(as.character(v %||% "")[1]); if (is.na(s)) "" else s }
        lab <- trimws(paste(one(dt$bank), one(dt$statement_type %||% "report")))
        return(if (nzchar(lab)) lab else tid)
      }
      return(tid)
    }
    # Build the overview for JUST this template, not the whole set: friendly_tpl
    # runs on every successful convert and only needs this id's bank + type, and
    # the full-set build grows with every template the team adds. Same function,
    # one-element input -> identical row (missing id still falls back to `tid`).
    ov <- tryCatch(template_overview(all_templates()[tid]), error = function(e) NULL)
    if (is.null(ov) || !nrow(ov)) return(tid)
    r <- ov[ov$id == tid, , drop = FALSE]
    if (!nrow(r)) return(tid)
    lab <- .tpl_label(r$bank[1], r$type[1])
    if (is.na(lab)) tid else lab
  }

  # cv_headline -- the EASY, plain-English verdict for a transaction result: did it
  # work, how many transactions, and can I trust it, said in words rather than KPI
  # codes. This is what a non-technical reviewer reads first; the KPI tables stay
  # available under "Checks & detail".
  # THE LINE MUST NOT NAME A CAUSE IT DOES NOT KNOW. It has been wrong twice in
  # this spot, both times by asserting a fact about the file:
  #   * "usually because this statement has no running balance" -- it is just as
  #     often OCR, or a year the tool had to infer;
  #   * "This statement prints no closing balance" -- completeness_verified is
  #     FALSE whenever NEITHER balance check ran and no count was stated, which
  #     includes a statement that prints a closing balance but no opening one. The
  #     engine's own detail said the OPPOSITE ("no opening balance was found") two
  #     inches below.
  # So the medium branch names no cause at all: the checks underneath name it, per
  # check, from the engine's own words. One line where there were two.
  #
  # The high line likewise dropped "the closing balance the statement PRINTS":
  # R/reconcile.R derives a missing closing from the last running balance when the
  # opening is labelled, and reconciliation then passes at trust `high` -- so that
  # headline quoted a figure the statement never printed.
  plain_trust <- function(trust) {
    switch(trust$level %||% "",
      high   = list(cls = "ok",   icon = "\u2713",
                    line = "Every transaction adds up to the closing balance. Nothing is missing."),
      medium = list(cls = "warn", icon = "\u2713",
                    line = "Read cleanly. Something could not be proven - the checks below say which."),
      low    = list(cls = "bad",  icon = "!",
                    line = "Check these against the statement before you use them."),
      list(cls = "warn", icon = "\u2713",
           line = "Read cleanly. Check the number of rows against the statement."))
  }
  # Row FLAGS worth a chip: things the engine recorded per row that a clean-looking
  # result would otherwise never mention on screen. Each caps the trust level in
  # R/reconcile.R, and each was reaching only the workbook -- most importantly the
  # inferred year, which makes dates that LOOK proven merely likely. Counted off
  # the flags column of the produced table, so the chip and the file agree.
  ROW_FLAG_CHIPS <- c(
    date_year_inferred = "%d row(s) took their YEAR from a number on the page, not a statement period - confirm it",
    date_unresolved    = "%d row(s) have no year at all (none was printed) - day and month only",
    ocr_low_conf       = "%d row(s) have a machine-read date/amount the scan was unsure of - check those cells")
  # WHY A CLEAN PDF STOPS AT MEDIUM -- said where the level is said, or not at all.
  #
  # "No row failed to read" needs an independent count of the physical lines in the
  # file. R/parse.R and R/parse_pdf_table.R both leave source_line_count NA for a
  # PDF and for an Excel sheet, so that check comes back "could not be checked" and
  # the trust ladder is capped at medium however clean the statement was. PDFs are
  # most of what comes in, so without this line the ladder on About offers a rung
  # this format can never reach and a perfect conversion reads as a near miss
  # somebody should go and chase.
  .medium_is_the_ceiling <- function(res) {
    k <- res$kpis
    if (is.null(k) || !all(c("name", "status") %in% names(k))) return(FALSE)
    base <- .stmt_base(k$name)                                       # split-aware
    if (!any(k$status[base == "no_unparsed_rows"] %in% "na")) return(FALSE)
    fmt <- tryCatch(templates()[[(res$template_id %||% "")[1]]]$format,
                    error = function(e) NULL) %||% ""
    fmt %in% c("pdf", "excel")
  }
  CEILING_NOTE <- paste(
    "A PDF or Excel statement cannot go higher than medium: proving no row was",
    "missed needs a line count of the file itself, which only a CSV or TSV export",
    "has. That is the normal ceiling for this format, not something to chase.")
  output$cv_headline <- renderUI({
    res <- cv_res(); req(res); req(.is_txn_result(res))
    if (!isTRUE(res$status == "ok")) return(NULL)   # failures are shown by cv_status
    d <- cv_data(); n <- if (is.null(d)) NA_integer_ else nrow(d)   # reuse the shared read
    pt <- plain_trust(res$trust %||% list())
    lvl <- c(ok = "high", warn = "medium", bad = "low")[[pt$cls]]
    # Small honest-flags row: which template read it, and anything a reviewer
    # should know at a glance (OCR pages, honoured redactions, hand-added rows).
    # All of this already exists in the result - it was just buried in the tables.
    chip <- function(txt, warn = FALSE)
      span(class = if (warn) "chip chip-warn" else "chip", txt)
    chips <- list()
    tid <- (res$template_id %||% NA_character_)[1]
    if (!is.na(tid) && nzchar(tid)) chips <- c(chips, list(chip(paste("Read as:", friendly_tpl(tid)))))
    op <- suppressWarnings(as.integer(res$trust$ocr_pages %||% 0L))
    if (isTRUE(op > 0)) chips <- c(chips, list(chip(
      sprintf("%d page(s) machine-read (OCR) - double-check the numbers", op), warn = TRUE)))
    k <- res$kpis
    if (!is.null(k) && "name" %in% names(k)) {
      nred <- suppressWarnings(as.integer(k$actual[k$name == "redaction_summary"][1]))
      if (isTRUE(nred > 0)) chips <- c(chips, list(chip(
        sprintf("%d redacted row(s) honoured - hidden values stay hidden", nred))))
    }
    if (length(cv_forced())) chips <- c(chips, list(chip(
      sprintf("%d row(s) added by hand - flagged 'forced' in the output", length(cv_forced())),
      warn = TRUE)))
    fl <- if (!is.null(d) && "flags" %in% names(d)) as.character(d$flags) else character(0)
    for (f in names(ROW_FLAG_CHIPS)) {
      nf <- sum(grepl(f, fl, fixed = TRUE))
      if (nf > 0) chips <- c(chips, list(chip(sprintf(ROW_FLAG_CHIPS[[f]], nf), warn = TRUE)))
    }
    # THE CONFIDENCE LEVEL, ON THE LINE EVERYONE READS. It is named in the About
    # tab, in both operational guides and in the README, and on a clean run it
    # appeared on screen NOWHERE: the only renderer that printed it (cv_status)
    # returns NULL the moment a statement converts cleanly. So the one word that
    # tells a high run from a medium one was visible only when something had gone
    # wrong, and an analyst following the troubleshooting guide ("confidence medium
    # on a PDF") had nothing on screen to match it against.
    lev <- (res$trust$level %||% "")[1]
    div(class = paste0("verdict verdict-", lvl),
      div(class = "verdict-ico", pt$icon),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", sprintf("Converted%s%s",
          if (!is.na(n)) sprintf(" \u2014 %d transaction%s read", n, if (n == 1) "" else "s") else "",
          if (nzchar(lev)) sprintf(" \u00b7 confidence: %s", lev) else "")),
        p(class = "verdict-body", pt$line),
        if (identical(lev, "medium") && .medium_is_the_ceiling(res))
          p(class = "verdict-body", style = "margin-top:2px", CEILING_NOTE),
        if (length(chips)) div(chips)))
  })

  # --- Analysis: the useful numbers + graphs pulled from the conversion -------
  # The displayed transactions come from the produced CSV; read them once here as
  # a data frame for the summary cards and the trend graph. No new dependency -
  # base graphics, the same ones the X-ray uses.
  cv_data <- reactive({
    res <- cv_res(); if (is.null(res) || is.null(res$outputs)) return(NULL)
    csv <- res$outputs[grepl("\\.csv$", res$outputs)]
    if (length(csv) != 1 || !file.exists(csv)) return(NULL)
    d <- tryCatch(utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE),
                  error = function(e) NULL)
    if (is.null(d) || !nrow(d)) return(NULL)
    d$.date <- suppressWarnings(as.Date(d$date))
    d$.amt  <- suppressWarnings(as.numeric(d$amount))
    d$.bal  <- if ("balance" %in% names(d)) suppressWarnings(as.numeric(d$balance)) else NA_real_
    d
  })
  fmt_money <- function(x, cur = "") {
    if (length(x) != 1 || is.na(x)) return("-")
    sprintf("%s%s%s", if (x < 0) "-" else "", cur,
            formatC(abs(x), format = "f", digits = 2, big.mark = ","))
  }
  cur_symbol <- function(h) switch(h$currency %||% "", NZD = "$", AUD = "$", USD = "$",
                                   GBP = "\u00a3", EUR = "\u20ac", "")

  # .period_lines(txn_dates, period_start, period_end, n_periods) -- the date
  # ranges for the summary card, each labelled for WHICH FACT IT IS.
  #
  # ONE LINE USED TO PRINT ONE OF THEM UNDER THE OTHER'S NAME. "Period:" was the
  # min/max of the TRANSACTION dates whenever any row had one, with the
  # statement's own printed period only a fallback -- so the screen read "Period:
  # 15 Aug 2025 to 13 Sep 2025" over a statement whose header says 13 Aug to
  # 1 Sep, and the failing check's Expected (2025-08-13..2025-09-01) named a range
  # that appeared nowhere on the page.
  #
  # By construction every transaction sits inside the span, so "34 date(s) outside
  # period" beside it can only ever look like a bug in the tool -- which teaches a
  # reviewer to dismiss the one check that catches a mis-parsed year or a swapped
  # day/month. Both ranges now, each named, so the check's Expected has a referent
  # on the screen.
  #
  # The header's period goes through the SAME PARSER THE CHECK USES
  # (.tolerant_date, R/params.R), so the range here and the range in Expected are
  # the same two dates. A bound that will not parse is shown in the statement's own
  # words rather than dropped -- never invented, never silently gone.
  .period_lines <- function(txn_dates, period_start, period_end, n_periods = NA) {
    day <- function(v) {
      s <- as.character(v %||% NA_character_)[1]
      if (is.na(s) || !nzchar(trimws(s))) return(NA_character_)
      p <- suppressWarnings(.tolerant_date(s))
      if (is.na(p)) s else format(p, "%d %b %Y")
    }
    dts <- suppressWarnings(as.Date(txn_dates %||% as.Date(character(0))))
    span <- if (any(!is.na(dts)))
        sprintf("%s to %s", format(min(dts, na.rm = TRUE), "%d %b %Y"),
                format(max(dts, na.rm = TRUE), "%d %b %Y")) else NA_character_
    hs <- day(period_start); he <- day(period_end)
    hdr <- if (!is.na(hs)) sprintf("%s to %s", hs, he %||% "?") else NA_character_
    # SEVERAL PRINTED PERIODS, SAID WHERE THE PERIOD IS SAID. The engine reads a
    # multi-period file as ONE span (R/extract_metadata.R), which is what makes
    # "all dates fall in the statement period" and the balance reconciliation pass
    # on such a file - but a span silently standing in for three quarters reads as
    # one quarter. The count goes on the range it came FROM: the statement's
    # printed period, which is what was merged, falling back to the transaction
    # span only when the header carries no period at all.
    np <- suppressWarnings(as.integer(n_periods)[1])
    if (isTRUE(np > 1L)) {
      if (!is.na(hdr)) hdr <- sprintf("%s - %d periods", hdr, np)
      else if (!is.na(span)) span <- sprintf("%s - %d periods", span, np)
    }
    out <- c(if (!is.na(span)) sprintf("Transactions span: %s", span),
             if (!is.na(hdr))  sprintf("Statement period: %s", hdr))
    if (length(out)) out else "Transactions span: -"
  }

  output$cv_summary <- renderUI({
    res <- cv_res(); req(res); req(.is_txn_result(res))
    d <- cv_data(); h <- res$header %||% list(); cur <- cur_symbol(h)
    n   <- if (!is.null(d)) nrow(d) else (h$row_count %||% NA)
    amt <- if (!is.null(d)) d$.amt[!is.na(d$.amt)] else numeric(0)
    money_in <- sum(amt[amt > 0]); money_out <- sum(amt[amt < 0]); net <- sum(amt)
    ranges <- .period_lines(if (is.null(d)) as.Date(character(0)) else d$.date,
                            h$period_start, h$period_end,
                            res$metadata$n_periods %||% NA_integer_)
    card <- function(label, value, col = NULL)
      div(class = "stat",
          div(class = "stat-label", label),
          div(class = "stat-value", style = if (!is.null(col)) sprintf("color:%s", col) else NULL, value))
    has_close <- !is.na(suppressWarnings(as.numeric(h$closing_balance %||% NA)))
    tagList(
      div(class = "stat-grid",
        card("Transactions", if (is.na(n)) "-" else n),
        card("Money in",  fmt_money(money_in, cur),  PALETTE$ok),
        card("Money out", fmt_money(money_out, cur), PALETTE$bad),
        card("Net",       fmt_money(net, cur), if (isTRUE(net < 0)) PALETTE$bad else PALETTE$ok),
        if (has_close) card("Closing balance", fmt_money(as.numeric(h$closing_balance), cur))),
      p(class = "muted", style = "margin:0 0 4px", sprintf("%s%s%s",
        paste(ranges, collapse = "  \u00b7  "),
        if (!is.na(h$account_number %||% NA_character_)) sprintf("  \u00b7  Account: %s", h$account_number) else "",
        if (!is.na(h$bank %||% NA_character_)) sprintf("  \u00b7  %s", h$bank) else "")),
      # ...and anything the engine had to say about that merge - a window no
      # printed period covers, or balances it could not pair - said here rather
      # than only in the diagnostics table. A span with a hole in it looks exactly
      # like a whole one, which is the one thing this line exists to prevent.
      if (!is.na(res$metadata$period_note %||% NA_character_))
        p(class = "bad", style = "margin:0 0 6px;font-size:13px",
          res$metadata$period_note))
  })

  # cv_split -- an auto-split bundle, statement by statement.
  #
  # When a template opts into auto-split, the engine parses and reconciles each
  # statement in the file SEPARATELY and keeps every one's period, balances, row
  # count and confidence in result$metadata$split$statements. The screen said only
  # "auto-split into 5 statements" and then showed one set of summary cards for the
  # whole bundle -- so the combined period, and a single confidence level that is
  # really the WEAKEST statement's, read as if they described one statement. This
  # is the same table the feed stamps onto each row, shown to the person reviewing.
  output$cv_split <- renderUI({
    res <- cv_res(); req(res)
    sts <- res$metadata$split$statements
    if (is.null(sts) || !length(sts)) return(NULL)
    cell <- function(v) { v <- as.character(v %||% NA)[1]; if (is.na(v) || !nzchar(v)) "-" else v }
    hdr <- c("Statement", "Pages", "Period", "Opening", "Closing", "Rows", "Confidence")
    tagList(
      h4(sprintf("The %d statements in this file", length(sts))),
      p(class = "muted", style = "margin:0 0 6px",
        # "a statement_index column" was the engine's own column name, on the
        # customer's screen; the table and the download both head it "Statement #".
        paste("Each was read and reconciled on its own. The cards above cover the whole file, at the",
              "weakest statement's confidence; every row you download says which statement it came from.")),
      tags$table(class = "split-table",
        tags$thead(tags$tr(lapply(hdr, tags$th))),
        tags$tbody(lapply(sts, function(s) tags$tr(
          tags$td(cell(s$index)), tags$td(cell(s$pages)),
          tags$td(sprintf("%s to %s", cell(s$period_start), cell(s$period_end))),
          tags$td(cell(s$opening_balance)), tags$td(cell(s$closing_balance)),
          tags$td(cell(s$rows)), tags$td(cell(s$trust_level)))))))
  })

  output$cv_trend_note <- renderUI({
    req(cv_data())
    msg <- switch(input$an_view %||% "inout",
      inout   = "Green = money in, red = money out.",
      # No "(only if the statement shows a balance column)": when there is none the
      # chart itself says so, in the space the chart would have been.
      balance = "The running balance as it moves through the statement.",
      cumnet  = "Every transaction added up over time - where the account net sits at each point.")
    p(class = "muted", style = "margin:6px 0 0", msg)
  })

  output$cv_trend <- renderPlot({
    d <- cv_data(); req(d); d <- d[!is.na(d$.date), , drop = FALSE]; req(nrow(d) > 0)
    view <- input$an_view %||% "inout"; grp <- input$an_group %||% "week"; unit <- input$an_unit %||% "amount"
    # On-brand, low-chrome base-R chart: no plot box, light gridlines behind,
    # human date labels and a $k money axis, so it reads as product, not raw plot.
    # Green = money in, red = out (their meaning everywhere); brand blue for the
    # neutral balance / cumulative lines, softly area-filled so they read as product.
    GREEN <- "#0b7a34"; RED <- PALETTE$bad; BLUE <- "#00205b"; BLUE_FILL <- "#00205b1f"
    INK <- "#1f2a33"; GRID <- "#eceef1"; AXIS <- "#6b7280"
    fmt_k <- function(v) ifelse(abs(v) >= 1000,
      paste0(formatC(v / 1000, format = "f", digits = 1), "k"),
      formatC(v, format = "f", digits = 0, big.mark = ","))
    money_lab <- function(at) if (unit == "count") formatC(at, format = "d", big.mark = ",") else paste0("$", fmt_k(at))
    op <- par(mar = c(4, 4.8, 0.6, 1), mgp = c(3, 0.5, 0), tcl = -0.2, family = "sans",
              col.axis = AXIS, col.lab = INK, cex.axis = 0.9, cex.lab = 1, xpd = FALSE)
    on.exit(par(op))
    xdate <- function(dates) axis.Date(1, x = dates, format = if (grp == "month") "%b %Y" else "%d %b",
                                       col = NA, col.ticks = NA, col.axis = AXIS)
    area_line <- function(x, y, col, fill) {
      polygon(c(x[1], x, x[length(x)]), c(min(y, 0), y, min(y, 0)), col = fill, border = NA)
      lines(x, y, col = col, lwd = 2.6)
      # "#fff" is CSS, not R: grDevices wants #rrggbb or #rrggbbaa and raises
      # "invalid RGB specification" on three digits. Both line views ("Balance over
      # time" and "Running total of every transaction") drew nothing but that error
      # in the chart panel, on every statement that had the column to draw.
      points(x, y, pch = 21, cex = 0.65, col = "#ffffff", bg = col, lwd = 1)
    }
    if (view == "balance") {
      b <- d[!is.na(d$.bal), , drop = FALSE]
      if (!nrow(b)) { plot.new(); text(0.5, 0.5, "This statement has no running balance column.", col = AXIS); return(invisible()) }
      b <- b[order(b$.date), , drop = FALSE]; aty <- pretty(range(b$.bal, na.rm = TRUE))
      plot(b$.date, b$.bal, type = "n", axes = FALSE, xlab = "", ylab = "Balance", ylim = range(aty))
      abline(h = aty, col = GRID)
      area_line(b$.date, b$.bal, BLUE, BLUE_FILL)
      axis(2, at = aty, labels = money_lab(aty), col = NA, col.ticks = NA, las = 1); xdate(b$.date)
    } else if (view == "cumnet") {
      dd <- d[order(d$.date), , drop = FALSE]; cn <- cumsum(ifelse(is.na(dd$.amt), 0, dd$.amt))
      aty <- pretty(range(c(0, cn)))
      plot(dd$.date, cn, type = "n", axes = FALSE, xlab = "", ylab = "Running total", ylim = range(aty))
      abline(h = aty, col = GRID); abline(h = 0, col = "#c3c9d2", lwd = 1.2)
      area_line(dd$.date, cn, BLUE, BLUE_FILL)
      axis(2, at = aty, labels = money_lab(aty), col = NA, col.ticks = NA, las = 1); xdate(dd$.date)
    } else {
      key <- switch(grp, day = d$.date, week = as.Date(cut(d$.date, "week")), month = as.Date(cut(d$.date, "month")))
      lv  <- sort(unique(key)); pf <- factor(as.character(key), levels = as.character(lv))
      val_in  <- ifelse(d$.amt > 0, if (unit == "count") 1 else d$.amt, 0)
      val_out <- ifelse(d$.amt < 0, if (unit == "count") 1 else -d$.amt, 0)
      ins  <- tapply(val_in,  pf, sum); ins[is.na(ins)]  <- 0
      outs <- tapply(val_out, pf, sum); outs[is.na(outs)] <- 0
      m <- rbind(as.numeric(ins), as.numeric(outs))
      # ~18% headroom above the tallest bar so the top value label and the legend
      # both clear the bars instead of colliding with them.
      aty <- pretty(c(0, max(m, 1, na.rm = TRUE) * 1.18))
      bp <- barplot(m, beside = TRUE, col = c(GREEN, RED), border = NA, axes = FALSE,
                    names.arg = rep("", ncol(m)), ylim = range(aty), space = c(0.1, 0.8),
                    ylab = if (unit == "count") "Transactions" else "Dollars")
      abline(h = aty, col = GRID)   # gridlines behind, then re-draw bars on top
      barplot(m, beside = TRUE, col = c(GREEN, RED), border = NA, axes = FALSE, add = TRUE,
              names.arg = rep("", ncol(m)), space = c(0.1, 0.8))
      axis(2, at = aty, labels = money_lab(aty), col = NA, col.ticks = NA, las = 1)
      # Value labels above each bar when the period count is small enough to stay legible.
      if (ncol(m) <= 8) {
        lab_v <- function(v) ifelse(v <= 0, "", if (unit == "count") formatC(v, format = "d") else paste0("$", fmt_k(v)))
        text(as.numeric(bp), as.numeric(m), labels = lab_v(as.numeric(m)),
             pos = 3, offset = 0.25, cex = 0.68, col = AXIS, xpd = NA)
      }
      lab <- switch(grp, month = format(lv, "%b %Y"), format(lv, "%d %b"))
      axis(1, at = colMeans(bp), labels = lab, col = NA, col.ticks = NA,
           las = if (length(lv) > 8) 2 else 1, cex.axis = if (length(lv) > 8) 0.75 else 0.9)
      legend("topleft", legend = c(if (unit == "count") "In" else "Money in", if (unit == "count") "Out" else "Money out"),
             fill = c(GREEN, RED), border = NA, bty = "n", cex = 0.9, horiz = TRUE)
    }
  })
  # Is this result a form (labelled values) rather than a transaction statement?
  output$cv_is_form <- reactive({ isTRUE((cv_res()$kind %||% "") == "form") })
  outputOptions(output, "cv_is_form", suspendWhenHidden = FALSE)
  output$cv_form <- renderUI({
    res <- cv_res(); req(res); req(identical(res$kind, "form"))
    tagList(
      # The card above already says how it went and which template read it, so this
      # says the one thing that card cannot: WHY there are no completeness checks on
      # a form, and therefore what the reviewer has to do instead.
      p(class = "muted", style = "margin:8px 0 12px; max-width:760px",
        "A labelled-value document, not a transaction statement: no transaction table and no running balance, so the completeness checks don't apply. Check each value below against the document."),
      h4("Values found"), DTOutput("cv_fields"))
  })

  # ---- A REPORT: many tables, no transactions (kind == "tables") -------------
  #
  # WHAT THIS SCREEN HAS TO SAY THAT THE OTHERS DO NOT. There is no balance to
  # check a report against, so "it converted" is not on its own worth anything.
  # The two things a reviewer can actually act on are HOW each table was found
  # (by its heading, or by where it sat on the example -- the second is the one
  # that goes wrong when a document changes) and WHAT CAME OUT THIN (a column
  # mostly empty, or words inside a table that no column claimed). So those are
  # the headline, and the tables themselves come after them.
  output$cv_is_tables <- reactive({ isTRUE((cv_res()$kind %||% "") == "tables") })
  outputOptions(output, "cv_is_tables", suspendWhenHidden = FALSE)

  # Which table is being shown. The navigator down the right sets it; it survives
  # a re-render, and resets whenever a new document is converted.
  cv_tbl_pick <- reactiveVal(NULL)
  observeEvent(cv_res(), cv_tbl_pick(NULL))

  .cv_ext <- reactive({ res <- cv_res(); if (is.null(res)) NULL else res$extract })

  output$cv_tables <- renderUI({
    res <- cv_res(); req(res); req(identical(res$kind, "tables"))
    ext <- res$extract
    if (is.null(ext) || !length(ext$tables))
      return(p(class = "muted", "This report template found no tables on the document."))
    s <- ext$summary
    weak <- sum(!(s$found_by %in% "its heading"))
    thin <- sum(as.integer(s$thin_columns %||% 0L))
    lost <- sum(as.integer(s$unclaimed_words %||% 0L))
    tagList(
      p(class = "muted", style = "margin:8px 0 12px; max-width:820px",
        paste("A report, not a transaction statement: no running balance, so the",
              "completeness checks don't apply. What is checkable is below - how",
              "each table was found, and anything that came out thin.")),
      div(class = if (weak || thin || lost) "verdict verdict-medium" else "verdict verdict-high",
          style = "margin:2px 0 14px",
        div(class = "verdict-ico", if (weak || thin || lost) "!" else "\u2713"),
        div(style = "flex:1;min-width:0",
          div(class = "verdict-title", sprintf("%d table%s, %d row%s",
              nrow(s), if (nrow(s) == 1L) "" else "s",
              sum(s$rows), if (sum(s$rows) == 1L) "" else "s")),
          p(class = "verdict-body", style = "margin:0",
            paste(c(
              if (weak) sprintf("%d found by position rather than by a heading - check those against the document.", weak),
              if (thin) sprintf("%d column(s) came out mostly empty - check the column edges.", thin),
              if (lost) sprintf("%d word(s) inside a table were not claimed by any column.", lost),
              if (!weak && !thin && !lost) "Every table was found by its own heading and every column filled."),
              collapse = " ")))),
      fluidRow(
        column(8,
          uiOutput("cv_tbl_title"),
          DTOutput("cv_tbl_rows")),
        column(4,
          strong("What is on this report"),
          p(class = "muted", style = "margin:2px 0 8px", "Click one to see it."),
          uiOutput("cv_tbl_nav"))),
      tags$details(style = "margin-top:16px",
        tags$summary(class = "muted", style = "cursor:pointer",
                     "How each table was found, and how full it came out"),
        h5("Tables"), tableOutput("cv_tbl_summary"),
        h5("Columns"), tableOutput("cv_tbl_report")))
  })

  # The navigator: every table and every value, with the thing a reviewer needs to
  # see at a glance -- how it was found -- said beside each one rather than hidden
  # in a report nobody opens.
  output$cv_tbl_nav <- renderUI({
    ext <- .cv_ext(); req(!is.null(ext))
    s <- ext$summary
    cur <- cv_tbl_pick() %||% (if (nrow(s)) s$table[1] else NULL)
    items <- lapply(seq_len(nrow(s)), function(i) {
      k <- s$table[i]
      byhead <- identical(s$found_by[i], "its heading")
      div(style = paste0("padding:6px 8px;border-left:3px solid ",
                         if (byhead) PALETTE$ok else PALETTE$warn,
                         ";margin-bottom:4px;background:",
                         if (identical(k, cur)) "#eef4ff" else "transparent"),
        actionLink(paste0("cv_tblpick_", i), s$name[i], style = "font-weight:600"),
        div(class = "muted", style = "font-size:12px",
            sprintf("page %s \u00b7 %d row%s \u00b7 found by %s", s$pages[i], s$rows[i],
                    if (s$rows[i] == 1L) "" else "s", s$found_by[i])))
    })
    pr <- ext$pairs
    vals <- if (is.null(pr) || !nrow(pr)) NULL else tagList(
      tags$hr(style = "margin:10px 0"),
      strong("Values"),
      tags$ul(style = "margin:6px 0 0;padding-left:18px",
        lapply(seq_len(nrow(pr)), function(i) tags$li(
          HTML(sprintf("%s: <b>%s</b>", htmltools::htmlEscape(pr$label[i]),
                       htmltools::htmlEscape(if (nzchar(pr$value[i])) pr$value[i] else "not found"))),
          div(class = "muted", style = "font-size:12px",
              sprintf("page %d \u00b7 found by %s", pr$page[i], pr$found_by[i]))))))
    tagList(items, vals)
  })

  # One observer per navigator row. They are created once, for as many rows as any
  # report is ever likely to have, because a Shiny observer cannot be attached to a
  # control that does not exist yet -- and re-creating them on every render would
  # stack a new observer on each row every time the page redrew.
  lapply(seq_len(60L), function(i) observeEvent(input[[paste0("cv_tblpick_", i)]], {
    ext <- .cv_ext(); req(!is.null(ext))
    s <- ext$summary
    if (i <= nrow(s)) cv_tbl_pick(s$table[i])
  }, ignoreInit = TRUE))

  .cv_tbl_cur <- reactive({
    ext <- .cv_ext(); if (is.null(ext) || !length(ext$tables)) return(NULL)
    k <- cv_tbl_pick()
    if (is.null(k) || is.null(ext$tables[[k]])) k <- names(ext$tables)[1]
    ext$tables[[k]]
  })

  output$cv_tbl_title <- renderUI({
    t <- .cv_tbl_cur(); req(!is.null(t))
    tagList(h4(style = "margin-top:0", t$name %||% t$key),
            p(class = "muted", style = "margin:0 0 8px", t$detail %||% ""))
  })

  output$cv_tbl_rows <- renderDT({
    t <- .cv_tbl_cur(); req(!is.null(t))
    d <- t$rows
    # The parsed companions are for the file, not the screen: a reviewer checking
    # against the page wants the page's own words, once.
    d <- d[, !grepl("__value$", names(d)), drop = FALSE]
    datatable(d, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE))
  })

  output$cv_tbl_summary <- renderTable({
    ext <- .cv_ext(); req(!is.null(ext))
    ext$summary[, c("name", "pages", "rows", "columns", "thin_columns",
                    "unclaimed_words", "found_by"), drop = FALSE]
  })
  output$cv_tbl_report <- renderTable({
    ext <- .cv_ext(); req(!is.null(ext))
    r <- ext$report
    if (is.null(r) || !nrow(r)) return(NULL)
    r[, c("table", "column", "type", "filled", "rows", "fill_rate", "low_fill"),
      drop = FALSE]
  })
  # THE ONE TABLE ON CONVERT THAT WENT STRAIGHT TO datatable(). Every other table
  # on this page maps what it shows; this one handed the engine's frame over
  # untouched, so a forensic reviewer read a FIELD column of schema names
  # (opening_balance, government_contribution, investment_return) beside a LABEL
  # column that already said the same thing in the document's own words, and three
  # columns of `true` / `false`.
  #
  # The label is what the DOCUMENT prints, so it is the identity a reviewer can
  # check against the page; the schema name is the maintainer's handle and belongs
  # in the JSON. A field whose label is blank keeps its name in readable form
  # rather than losing its row.
  #
  # `flagged` and `conflict` are the engine's own columns, not re-derived here.
  #
  # NEEDS A LOOK MUST NAME EVERY ROW THE CARD COUNTS. It read `flagged` alone --
  # required-and-not-found -- so on a real KiwiSaver summary the card said "3
  # label(s) appear more than once with different values; the first of each was
  # taken - check them against the document" over a table whose NEEDS A LOOK cell
  # was empty on all seven rows. The tool knew which three (extract_fields sets
  # `conflict` per field, and convert_form counts exactly that column into the
  # sentence) and would not say. Telling a forensic reviewer that three of these
  # figures are contested and then refusing to say which makes all seven suspect,
  # which is the opposite of what the message is for.
  #
  # Both reasons come from the frame the card counted, so the two can never
  # disagree, and a row carrying both gets both sentences rather than the first
  # one that matched.
  # The two sentences live with the other wording (ui_labels.R, FIELD_LOOK_PLAIN).
  .field_needs_look <- function(f) {
    flags <- lapply(names(FIELD_LOOK_PLAIN), function(nm) {
      v <- f[[nm]] %||% rep(FALSE, nrow(f))
      ifelse(v %in% TRUE, unname(FIELD_LOOK_PLAIN[[nm]]), NA_character_)
    })
    vapply(seq_len(nrow(f)), function(i) {
      say <- Filter(nzchar, stats::na.omit(vapply(flags, `[`, character(1), i)))
      if (!length(say)) "" else paste0("yes - ", paste(say, collapse = "; "))
    }, character(1))
  }
  yes_no <- function(v) ifelse(v %in% TRUE, "yes", "no")
  output$cv_fields <- renderDT({
    res <- cv_res(); req(res, !is.null(res$fields))
    f <- res$fields
    lab <- trimws(as.character(f$label %||% rep(NA_character_, nrow(f))))
    fallback <- cv_friendly_cols(as.character(f$field %||% rep("", nrow(f))))
    lab[is.na(lab) | !nzchar(lab)] <- fallback[is.na(lab) | !nzchar(lab)]
    disp <- data.frame(
      `What the document calls it` = lab,
      Value = as.character(f$value %||% rep(NA_character_, nrow(f))),
      Found = yes_no(f$matched),
      Required = yes_no(f$required),
      `Needs a look` = .field_needs_look(f),
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(disp, rownames = FALSE, options = list(dom = "t", pageLength = 30)) |>
      formatStyle("Found", fontWeight = "bold",
                  color = styleEqual(c("yes", "no"), c(PALETTE$ok, PALETTE$bad))) |>
      # The rows the card is talking about, findable at a glance on a table of
      # thirty fields -- styleEqual("") leaves an unflagged row untouched.
      formatStyle("Needs a look", fontWeight = "bold",
                  color = styleEqual("", "inherit", PALETTE$bad))
  })

  # THE FIGURES THE CHECK WAS DECIDED ON, BESIDE THE VERDICT.
  #
  # The engine records `expected` and `actual` on every KPI and this table printed
  # neither, so a check could report OK while its own numbers said otherwise and
  # nothing on screen showed the gap. Real example: a statement whose date column
  # did not map gave "Row dates could be read | OK", because .kpi_dates_readable
  # passes on ONE readable date -- while the row it came from said expected 500,
  # actual 1. Same for "Row count", which degrades to n > 0 when no count is
  # printed. Two columns of numbers is not more words; it is what makes a
  # generous pass verifiable instead of silent, which is the whole charter.
  #
  # `informational` gets its own word. redaction_summary and ocr_confidence are
  # counts the engine read successfully, not checks that could not run, so
  # RESULT_PLAIN["na"] ("could not be checked") is wrong for them -- and the old
  # wording, "not on this statement", claimed a scanned statement had no scan.
  output$cv_kpis <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$kpis))
    k <- res$kpis
    info <- if ("informational" %in% names(k)) k$informational %in% TRUE
            else .stmt_base(k$name) %in% INFORMATIONAL_CHECKS
    dash <- function(v) { v <- as.character(v %||% rep(NA, nrow(k)))
                          v[is.na(v) | !nzchar(v)] <- "-"; v }
    disp <- data.frame(
      Check    = plain_check(k$name),      # split-aware: "... (statement 2)"
      Result   = ifelse(info, RESULT_PLAIN_INFO, plain_label(k$status, RESULT_PLAIN)),
      Expected = dash(k$expected),
      Read     = dash(k$actual),
      Detail   = if ("detail" %in% names(k)) k$detail else "",
      stringsAsFactors = FALSE)
    datatable(disp, rownames = FALSE,
              options = list(dom = "t", pageLength = 20, scrollX = TRUE)) |>
      formatStyle("Result", fontWeight = "bold",     # coloured off the same map
        color = styleEqual(unname(RESULT_PLAIN[c("pass", "fail")]), c(PALETTE$ok, PALETTE$bad)))
  })

  output$cv_diag <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$diagnostics))
    # Customer-facing: where / why / how-to-fix only. The fix-ownership triage
    # (template vs engine-gap vs escalate) is maintainer-only and lives on the
    # Admin tab, never here. Category codes render as plain words.
    d <- res$diagnostics[, intersect(c("where", "category", "severity", "detail", "how_to_fix"),
                                     names(res$diagnostics)), drop = FALSE]
    if ("category" %in% names(d)) d$category <- plain_label(d$category, DIAG_PLAIN)
    names(d) <- plain_label(names(d), c(where = "Where", category = "What",
                                        severity = "Severity", detail = "Detail",
                                        how_to_fix = "How to fix"))
    datatable(d, rownames = FALSE,
              options = list(dom = "t", pageLength = 20, scrollX = TRUE))
  })

  # "Checks & detail" is depth-as-an-option on a clean result -- and NOT optional
  # when the tool has just asked for a second pair of eyes. Anything other than a
  # clean pass (a non-ok status, or any check that FAILED) opens the section, so the
  # reasons sit in front of the reviewer instead of one collapsed click behind the
  # chart. A clean, fully-passing conversion still starts tidy.
  #
  # ONE CLICK, NOT TWO -- see the UI, where this output now sits. It used to be
  # rendered INSIDE the panel that "Show me how it read this" opens, so on a clean
  # run the checks were two clicks deep, under a link whose caption promised them
  # and under a proof strip that said "could not be checked - why, in Checks
  # below" when nothing of the sort was below. It is also the wrong audience: the
  # checks answer the accountant's question ("can I use this file?"), while the
  # X-ray, the chart and the template candidates answer the maintainer's.
  #
  # AN EMPTY TABLE CANNOT BE TOLD FROM A BROKEN ONE, and these two are empty on
  # exactly the screens with the least to go on. A failed or unsupported run has no
  # KPIs and no coverage frame, so both DTOutputs rendered nothing at all -- a
  # "Checks" heading over blank space, under a page that promises "every check that
  # exists for your statement is in this table with one of those four words beside
  # it". A reviewer has no way to tell that from a table that failed to draw.
  #
  # The reason is the same for both and is a fact the result already carries: no
  # transactions came out, so there was nothing to check and no field to report on.
  .why_empty <- function(res) {
    if (isTRUE((res$status %||% "") == "failed"))
      "Nothing was read from this file, so there is nothing to check and no fields to report."
    else
      "No template read this document, so no transactions were produced - there is nothing to check and no fields to report."
  }
  # ...AND THE THIRD TABLE WAS LEFT OUT OF THAT FIX, which is the whole of this
  # defect: two of the three headings learned to say why they were empty and
  # Diagnostics did not, so it alone still sat over blank space.
  #
  # It needs its OWN sentence, because its empty state has a different cause.
  # build_diagnostics() always returns at least a "no issues detected" row, so a
  # conversion that RAN cannot leave this table empty. The way to get here is a
  # conversion that never finished: a child process killed from outside leaves
  # job_failed_result() to build a result out of a status and a sentence and
  # nothing else (no checks, no coverage, no diagnostics). Borrowing "nothing was
  # read from this file" there would name the wrong cause -- the file was never
  # the problem.
  WHY_NO_DIAG <- "No diagnosis was recorded for this run - it did not get far enough to make one."
  output$cv_detail <- renderUI({
    res <- cv_res(); req(res)
    k <- res$kpis
    any_failed <- !is.null(k) && "status" %in% names(k) && any(k$status %in% "fail")
    open_it <- !isTRUE((res$status %||% "") == "ok") || isTRUE(any_failed)
    has_kpis <- is.data.frame(k) && nrow(k) > 0L
    has_cov  <- is.data.frame(res$coverage) && nrow(res$coverage) > 0L
    has_diag <- is.data.frame(res$diagnostics) && nrow(res$diagnostics) > 0L
    said <- p(class = "muted", style = "margin:0 0 8px", .why_empty(res))
    tags$details(style = "margin-top:14px", open = if (open_it) NA else NULL,
      tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
                   "Checks & detail (for review)"),
      div(style = "padding:8px 2px",
        h4("Checks"), if (has_kpis) DTOutput("cv_kpis") else said,
        h4("Diagnostics - where / why / how to fix"),
        if (has_diag) DTOutput("cv_diag") else p(class = "muted", style = "margin:0 0 8px", WHY_NO_DIAG),
        h4("Field coverage - what's present / empty / not read as its own column"),
        if (has_cov) tagList(uiOutput("cv_cov_summary"), DTOutput("cv_coverage")) else said))
  })

  output$cv_cov_summary <- renderUI({
    res <- cv_res(); req(res); req(!is.null(res$coverage))
    p(class = "muted", coverage_summary(res$coverage))
  })
  # EVERY FIELD, because the line above counts every field. This dropped `unmapped`
  # rows unless they happened to be balance / particulars / reference, so the
  # summary read "6 populated, 5 not on this statement" over a table showing 2 --
  # a reader can only conclude that three fields went somewhere unexplained. The
  # count and the rows now come from the same frame.
  output$cv_coverage <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$coverage))
    cov <- res$coverage
    disp <- data.frame(
      # The stored SCHEMA names ("other_party", "amount_raw") are the engine's, and
      # this table is the one place they still reached a customer-facing screen --
      # beside a "Field coverage" heading written for the person holding the
      # statement. Same map the transactions table and the toolkit preview use.
      Field   = cv_friendly_cols(cov$field),
      # 'unmapped' is a fact about the TEMPLATE, not the file -- see COVERAGE_PLAIN.
      Verdict = plain_label(cov$verdict, COVERAGE_PLAIN),
      Populated = cov$populated, Empty = cov$empty, Note = cov$note,
      stringsAsFactors = FALSE)
    datatable(disp, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatStyle("Verdict",
        backgroundColor = styleEqual(unname(COVERAGE_PLAIN),
                                     c("#e6f4ea","#fff8e6","#fde7e7","#f2f2f2")))
  })

  output$cv_txns <- renderDT({
    # Reuse the already-read output CSV (cv_data) rather than reading it from disk a
    # second time -- by the time this Preview tab renders, cv_headline / cv_summary /
    # cv_trend have all consumed cv_data(). Drop its three derived helper columns
    # (.date/.amt/.bal) so the display shape is exactly what read.csv gave before.
    d <- cv_data(); req(!is.null(d))
    df <- d[, setdiff(names(d), c(".date", ".amt", ".bal")), drop = FALSE]
    # The CSV is already the display shape (no verbatim *_raw; debit/credit when the
    # statement splits them). Trim columns this statement never fills so the table
    # shows only fields that were actually read.
    df <- df[, .cols_with_data(df), drop = FALSE]
    # Beth reads Date, Description, Amount, Balance first; the bank-technical
    # columns (in/out, particulars / code / reference / type / ...) follow. The
    # internal row id ("#") is dropped from the preview - the "Showing N" line
    # already counts, and the downloaded file keeps it.
    df <- df[, setdiff(names(df), "row_id"), drop = FALSE]
    lead <- intersect(c("date", "description", "amount", "debit", "credit", "balance",
                        "direction", "type", "reference", "particulars", "code", "other_party"),
                      names(df))
    df <- df[, c(lead, setdiff(names(df), lead)), drop = FALSE]
    # The HEADERS were mapped and the VALUES were not, so the Flags column read
    # `ocr_low_conf` and `date_year_inferred` in the cells of the table she is
    # checking figures in. Same map both tables use (ui_labels.R, FLAG_PLAIN).
    if ("flags" %in% names(df)) df$flags <- plain_flags(df$flags)
    datatable(df, rownames = FALSE, colnames = cv_friendly_cols(names(df)),
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # need_file(p) -- a download with nothing to give tells the user (a toast) and
  # STOPS, instead of handing the browser an empty "NA" file. `character(0)` is
  # NOT NULL, so an unsupported/failed result (outputs = character(0)) must be
  # length-checked, not null-checked.
  #
  # stop(), not req(FALSE): req(FALSE) inside a download handler aborts the
  # request and Shiny answers a bare HTTP 500 "An error has occurred!" page -- a
  # browser error where a file was asked for. Its callers catch this and write the
  # sentence into the file instead.
  NOTHING_TO_DL <- "Nothing to download - this conversion produced no output. Convert a statement first."
  need_file <- function(p) {
    if (length(p) != 1 || is.na(p) || !nzchar(p) || !file.exists(p)) {
      notify_once("dl", NOTHING_TO_DL, duration = 6)
      stop(NOTHING_TO_DL, call. = FALSE)
    }
    p
  }
  # dl_buttons(outputs, ids) -- a Download button ONLY for formats actually produced
  # (e.g. no Excel on a host without openxlsx), so no button promises a missing file.
  # Excel is the primary (btn-primary) since it's what most reviewers want.
  #
  # JSON is NOT one of these. Nobody downloads it -- the work is done in Excel or
  # CSV -- and a third equal button made the two real choices look like three, so
  # every reviewer paid a moment's thought to an option that was never theirs. It
  # is still produced and still one click away (dl_json_link below): demoted, not
  # removed, because the person who does want it has no other route to it.
  dl_buttons <- function(outputs, ids) {
    labs <- c(xlsx = "\u2b73 Excel", csv = "\u2b73 CSV")
    has <- function(ext) any(grepl(paste0("\\.", ext, "$"), outputs %||% character(0)))
    Filter(Negate(is.null), lapply(names(ids), function(ext)
      if (has(ext) && !is.na(labs[ext])) downloadButton(ids[[ext]], labs[[ext]],
        class = if (ext == "xlsx") "btn-primary" else NULL)))
  }
  output$cv_downloads <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    btns <- dl_buttons(res$outputs, c(xlsx = "dl_xlsx", csv = "dl_csv"))
    has_json <- any(grepl("\\.json$", res$outputs %||% character(0)))
    # THE VALUES ARE A SECOND CSV AND HAD NO BUTTON. On a report the CSV button
    # gives the TABLES stacked long -- page, row, column, value -- and a
    # label/value pair is none of those, so a document read for its labelled
    # figures downloaded as a file with none of them in it, and the only place
    # they appeared was a sheet inside the workbook. The file was always written
    # (R/doc_extract.R); nothing offered it.
    has_val <- any(grepl("\\.values\\.csv$", res$outputs %||% character(0)))
    if (has_val) btns <- c(btns, list(downloadButton("dl_values", "\u2b73 Values CSV")))
    if (!length(btns) && !has_json) return(NULL)
    # THIS CAME OFF A SCAN, SAID WHERE THE FILE IS COLLECTED.
    #
    # A large share of what arrives at a police unit is a scan of a photocopy, and
    # a page the OCR read badly can still come back green: the per-word confidence
    # is carried the whole way through the reader and every doubtful figure earns
    # a row flag, but the caveat lived in a diagnostics panel behind "Show me how
    # it read this", which most people never open. A misread 8 for a 3 in an
    # amount is the exact wrong-figure-that-looks-right this tool exists to stop,
    # and reconciliation only catches it if it happens to break the balance.
    # So it is said here, once, beside the button she came for.
    d <- cv_data()
    n_low <- if (is.null(d) || !("flags" %in% names(d))) NA_integer_
             else sum(grepl("ocr_low_conf", d$flags %||% ""), na.rm = TRUE)
    scan <- .scan_note(res$header$ocr_pages, n_low)
    # Prominent bar right under the verdict: the download is the point of the page,
    # so it's the most visible thing, not a quiet box tucked into the sidebar.
    tagList(
      div(class = "dl-hero", span(class = "dl-hero-label", "Download your converted data:"), btns,
        if (has_json)
          span(class = "muted", style = "font-size:12.5px;margin-left:2px",
               downloadLink("dl_json", "JSON", class = "muted"))),
      if (!is.null(scan))
        p(class = "muted", style = "margin:-6px 0 12px;font-size:13px", scan))
  })

  # THE FEED LINE IS NOT ON THIS SCREEN, DELIBERATELY.
  #
  # It used to say "Sent to the dashboards." / "Held back from the dashboards -
  # it needs checking first." right under the verdict. Whether a conversion
  # reaches the org's dashboards is an ORG question, decided by a machine gate on
  # template origin and trust, and there is nothing the analyst who ran it can do
  # about the answer: her download is complete either way, and the checks above
  # already tell her whether the figures are sound. Telling her about a Qlik
  # pipeline she has no part in was noise dressed as information.
  #
  # Nothing about the gate changed -- write_feed() still decides, still records
  # the verdict in the manifest, still logs it. What moved is WHO IS TOLD: a feed
  # write that FAILS is an operational fault for whoever runs the server, so it
  # is raised in Admin (adm_feed_health, below) where that person can act on it,
  # instead of on the screen of an analyst who can only be puzzled by it.
  # cv_edit -- THE ONE DOOR BACK INTO HOW THIS WAS READ, on all three routes.
  #
  # It began as cv_rematch, an escape hatch for a WRONG match on a statement, and
  # it was guarded on .is_txn_result -- so on the two routes that need it MOST it
  # did not render at all. A report has no reconciliation behind it: nothing
  # arithmetic can catch a wrong read, so "open the template that read this and
  # move the box" is the only correction there is, and there was no way to ask for
  # it. (The two links written for that job -- cv_teach's form and report branches
  # -- switched tab and threw the document away, landing on "Upload the document
  # above to start". They are gone; this is what replaces them, and it is one
  # control where there were three.)
  #
  # TWO BRANCHES, KEPT AS THEY WERE. The quiet line is the ordinary door and it
  # now opens the template that DID the reading, seeded, with the document under
  # it. The loud one still fires only where detection left a real doubt, and still
  # drafts fresh, because there the question is which template, not where a box
  # sits.
  #
  # ON SCREEN NOW, directly under the downloads (see the UI). It was rendered by
  # nothing at all, so its two buttons and every word in it were unreachable --
  # while cv_teach's `ok` branch stayed silent precisely because "the 'Wrong bank?'
  # line up top already offers a fix". Between them a clean-looking result read by
  # the wrong template had no route back anywhere on the page, which is the one
  # case the charter cares most about.
  # .match_is_thin(res) -- did DETECTION actually leave room for doubt? The engine
  # already answers this: detect_statement() records `thin` (won by a hair over a
  # near-duplicate), `ambiguous`, and `tied` (two or more fitting equally well).
  #
  # The invitation below was keyed on `needs_review` instead, which is a verdict
  # about the FIGURES, not about the match. Measured: a run where the balance
  # reconciles to the cent and all 79 dates read was told "worth checking it's the
  # right match" with a prominent button, while nothing whatsoever suggested the
  # match was wrong -- and the charter is explicit that the tool decides the
  # template. So the line is now shown on the evidence that would justify it.
  .match_is_thin <- function(res) {
    d <- res$detect
    if (is.null(d)) return(FALSE)
    isTRUE(d$thin) || isTRUE(d$ambiguous) || length(d$tied %||% character(0)) >= 2L
  }
  output$cv_edit <- renderUI({
    res <- cv_res(); req(res)
    st <- res$status %||% "failed"
    if (!(st %in% c("ok", "needs_review"))) return(NULL)   # unsupported/failed already prompt setup
    tid <- (res$template_id %||% NA_character_)[1]
    nice <- if (!is.na(tid) && nzchar(tid)) friendly_tpl(tid) else NA_character_
    # THE QUESTION IN FRONT OF THE LINK IS THE ONE THAT ROUTE ACTUALLY HAS. A
    # statement's doubt is which bank; a form's is a value that read the wrong
    # thing; a report's is a table that came out of the wrong columns. Same door,
    # same words for the door, and the question named for the document in hand.
    ask <- if (identical(res$kind, "tables")) "A table missing, or reading the wrong columns?"
           else if (identical(res$kind, "form")) "A value missing, or reading the wrong thing?"
           else "Not the right bank?"
    quiet <- div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:8px;margin:0 0 12px;color:var(--muted);font-size:13px",
      span(ask),
      actionLink("cv_edit_go", "Adjust how this was read"))
    # Happy path: one quiet line, and it does NOT re-state which template read
    # the statement -- the "Read as" chip on the verdict card two inches above
    # says that already, and saying it twice makes a question out of a fact.
    if (identical(st, "ok") || !.is_txn_result(res)) return(quiet)
    # needs_review. The route back is always here, but it only ANNOUNCES ITSELF as
    # a doubt about the match when detection left one. The remedy for whatever
    # actually went wrong is on the verdict card, beside the diagnosis it belongs
    # to (failed_checks_ui), not competing with it from further down the page.
    if (!.match_is_thin(res)) return(quiet)
    div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:8px;margin:0 0 12px;font-size:13px",
      span(if (!is.na(nice)) sprintf("Read as %s \u2014 more than one template nearly fitted, so it's worth checking.", nice)
           else "More than one template nearly fitted, so it's worth checking this is the right match."),
      actionButton("cv_rematch_go_rv", "Set up the right template", class = "btn-warning btn-sm"))
  })
  # Same action, several places to ask for it. Each element has its OWN input id
  # (two elements sharing one id keep separate click counters, which is how a
  # button ends up dead), and they all run this.
  .rematch_now <- function() {
    src <- cv_src(); req(src)
    # Fresh draft from the file itself, never seeded from the wrong match.
    open_guided(src$path, src$name, seed_tmpl = NULL, upload_id = cv_upload_id())
  }
  observeEvent(input$cv_rematch_go_rv, .rematch_now())
  # .edit_now() -- OPEN WHAT READ THIS DOCUMENT, with the document still under it.
  #
  # One function dispatching on what the result IS, which is the dispatch Admin's
  # editor already uses and has proven. It is also the whole of B1's automatic
  # half: run_conversion's finish callback calls this, so "process and open the
  # editor" and "the door is on screen even on a confident result" are one code
  # path and cannot drift apart.
  #
  #   report  -- the saved template, reopened on this document. Every box where it
  #              was drawn, the header already saying "Editing the saved template
  #              X - Save replaces it". If the id is not in the library (a curated
  #              template that has since been parked, say) it falls back to the
  #              document with no boxes, which is the honest answer rather than a
  #              silent nothing.
  #   form    -- there is no editor for a mode:fields template: it has no
  #              coordinates to draw. The builder's label/value pairs are the
  #              pointing-based successor, so the document opens there and the
  #              notification SAYS that is what will be built. Never silent about
  #              taking her somewhere other than where she asked.
  #   statement -- the toolkit, seeded from the template that matched, exactly as
  #              .teach_now() seeds it on an ok / needs_review result.
  .edit_now <- function() {
    res <- cv_res(); src <- cv_src()
    if (is.null(res) || is.null(src) || !file.exists(src$path %||% "")) return(invisible(FALSE))
    tid <- (res$template_id %||% NA_character_)[1]
    if (identical(res$kind, "tables")) {
      t <- if (!is.na(tid) && nzchar(tid)) tryCatch(all_doc_templates()[[tid]], error = function(e) NULL)
           else NULL
      if (!is.null(t)) rb_open_template(t, src$path)
      else {
        rb_handoff(src$path)
        updateRadioButtons(session, "ts_doctype", selected = "other")
        updateTabsetPanel(session, "main_tabs", selected = "Add a template")
        showNotification("The template that read this is no longer in the library, so this opens as a new one.",
                         type = "warning", duration = 8)
      }
      return(invisible(TRUE))
    }
    if (identical(res$kind, "form")) {
      rb_handoff(src$path)
      updateRadioButtons(session, "ts_doctype", selected = "other")
      updateTabsetPanel(session, "main_tabs", selected = "Add a template")
      # The tail ("...which is what replaces a form one") named a distinction only
      # a maintainer can act on; what she needs is what to do and what it does.
      showNotification("Point at the values you want and save - that makes a report template, which replaces the one that read this.",
                       type = "message", duration = 9)
      return(invisible(TRUE))
    }
    seed <- NULL
    if (!is.na(tid) && nzchar(tid)) {
      tset <- tryCatch(templates(), error = function(e) list())
      if (!is.null(tset[[tid]])) seed <- tset[[tid]]
    }
    open_guided(src$path, src$name, seed_tmpl = seed, upload_id = cv_upload_id())
    invisible(TRUE)
  }
  observeEvent(input$cv_edit_go, .edit_now())
  # The file is NAMED for what is in it. With no output to send, the old handler
  # aborted and the browser showed an HTTP 500 page; naming the download
  # "download.xlsx" and putting an explanation in it would be worse still -- a
  # workbook that will not open. So a download with nothing behind it comes back
  # as a .txt saying so, and the toast says it on screen at the same time.
  .out_path <- function(ext) {
    p <- cv_res()$outputs[grepl(paste0("\\.", ext, "$"), cv_res()$outputs)]
    if (length(p) && file.exists(p[1])) p[1] else NA_character_
  }
  mk_dl <- function(ext) downloadHandler(
    filename = function() {
      p <- .out_path(ext)
      if (is.na(p)) "nothing-to-download.txt" else basename(p)
    },
    content = function(file) {
      p <- .out_path(ext)
      if (is.na(p)) { notify_once("dl", NOTHING_TO_DL, duration = 6)
                      return(.dl_note(file, NOTHING_TO_DL)) }
      file.copy(p, file, overwrite = TRUE)
    })
  output$dl_xlsx <- mk_dl("xlsx"); output$dl_csv <- mk_dl("csv"); output$dl_json <- mk_dl("json")
  # "values.csv", not "csv": the extension alone would match the long CSV first.
  output$dl_values <- mk_dl("values\\.csv")

  # ---- Feedback (every conversion can be rated; one file per logs/feedback/) ----
  #
  # A CONVERSION THAT PRODUCED NOTHING IS NOT A CONVERSION TO RATE. "Was this
  # conversion correct?" appeared under a card reading "Could not read this file"
  # -- a question about figures on a screen with no figures, and the one answer
  # that does anything (Wrong) withdraws rows from the dashboards that were never
  # published. The run is still logged; there is simply nothing here for a reviewer
  # to have an opinion about.
  #
  # ...AND IT IS ASKED ON ALL THREE ROUTES, WHICH IS NOT A DETAIL. A statement has
  # reconciliation behind it: the arithmetic catches a wrong figure without anybody
  # saying so. A report and a form have NOTHING of the kind -- no total to prove,
  # no balance to carry forward -- so a human saying "this is wrong" is the only
  # quality signal those routes have, and it matters MORE there, not less. This
  # output is deliberately outside the transaction-only panel and carries no kind
  # guard; the one thing that changes with the route is the sentence saying why
  # the answer is worth giving, because the statement wording ("it reconciles")
  # would be a promise no report can keep.
  output$cv_feedback <- renderUI({
    res <- cv_res(); if (is.null(res) || is.null(res$run_id)) return(NULL)
    if (!isTRUE((res$status %||% "") %in% c("ok", "needs_review"))) return(NULL)
    if (isTRUE(cv_fb_done()))
      return(div(style = "margin-top:16px", span(class = "ok",
        "Thanks - your feedback was recorded."), cv_fb_note()))
    div(style = "margin-top:16px;padding:12px;border:1px solid #ddd;border-radius:6px",
        h4("Was this conversion correct?"),
        if (!.is_txn_result(res))
          p(class = "muted", style = "margin:0 0 8px",
            "Nothing here adds up to a balance the tool can check, so your reading of it against the page is the only check there is."),
        # NO TICK, NO CROSS. These three used to be drawn with the SAME glyphs the
        # proof-strip key twenty lines above defines as "checked and passed" and
        # "a problem" -- so one screen used one mark for two different things: what
        # the tool proved, and what the reader thinks. On a page whose whole job is
        # a defensible figure, that is the one collision that must not happen. The
        # three words carry the meaning on their own.
        # choiceNames/choiceValues (not named choices): a non-ASCII name in
        # c(name = value) becomes a SYMBOL at parse time, which on a C-locale
        # host mangles to '<U+2713>'. Lists of plain literals stay UTF-8, and the
        # form is kept for the day one of these needs a glyph of its own.
        # NOTHING PRE-ANSWERED. "Correct" was selected on arrival, so a click on
        # Submit without reading a figure recorded a positive rating -- and that
        # rating is not just an opinion: it is what Admin's "flagged as wrong"
        # list, the template-usage table and the suggestion ranking are all built
        # from, and marking a run WRONG retracts its rows from the dashboards. A
        # default answer to "was this correct?" is the tool answering for the
        # reviewer, on the one question only she can answer.
        radioButtons("cv_fb_verdict", NULL, inline = TRUE, selected = character(0),
          choiceNames = list("Correct", "Minor issues", "Wrong"),
          choiceValues = list("correct", "minor_issues", "wrong")),
        textAreaInput("cv_fb_comment", "Comment (optional - what was wrong?)",
                      width = "100%", rows = 2),
        actionButton("cv_fb_submit", "Submit feedback", class = "btn-primary"))
  })

  # Marking a conversion WRONG does more than log an opinion: submit_feedback()
  # withdraws that run's rows from the accepted feed (retract_feed, R/feed.R), so
  # figures a forensic accountant has called wrong stop reaching the dashboards.
  # The screen said only "Thanks - your feedback was recorded", which is the one
  # consequence she most needs confirmed. It now says what was withdrawn.
  cv_fb_note <- function() {
    rec <- cv_fb_rec(); if (is.null(rec)) return(NULL)
    n <- suppressWarnings(as.integer(rec$retracted_rows %||% NA))
    if (!identical(rec$verdict, "wrong")) return(NULL)
    # WHAT MARKING IT WRONG DID, in terms of the statement in front of her rather
    # than of the pipeline behind it. These lines used to name the org dashboards,
    # which is somewhere she has no part in and cannot check: what she needs to
    # know is that her verdict took effect and that fixing the template is what
    # puts things right. Where it went is the server's business, and Admin's.
    div(class = "muted", style = "margin-top:4px",
      if (is.na(n))
        "Recorded - but the figures could not be pulled back. Tell whoever looks after the server."
      else if (n > 0)
        sprintf("Recorded, and the %d row(s) this produced have been pulled back so nothing downstream uses them. Fix the template and convert again to replace them with corrected figures.", n)
      else
        "Recorded. Nothing had to be pulled back - these figures had not gone anywhere.")
  }
  observeEvent(input$cv_fb_submit, {
    res <- cv_res(); req(res, res$run_id)
    # Refused, not defaulted: with nothing chosen there is no rating to record,
    # and the reviewer is told which of the three to pick rather than left in
    # front of a button that did nothing.
    if (!length(input$cv_fb_verdict %||% character(0))) {
      notify_once("cv_fb", "Choose Correct, Minor issues or Wrong first - that is the rating being recorded.",
                  type = "warning", duration = 8)
      return()
    }
    clear_notice("cv_fb")
    rec <- tryCatch(
      submit_feedback(run_id = res$run_id, verdict = input$cv_fb_verdict,
                      comment = input$cv_fb_comment, requested_by = who_now(),
                      template_id = res$template_id, logdir = LOGDIR,
                      # The app's own settings, not a second read of the file: the
                      # retraction must target the SAME feed folder write_feed used.
                      config = CONFIG),
      error = function(e) NULL)
    cv_fb_rec(rec)
    cv_fb_done(!is.null(rec))
    if (is.null(rec))
      showNotification("Could not save feedback.", type = "error")
  })

  # ---- Guided setup: teach the tool from a statement it couldn't read ----
  guided <- reactiveVal(NULL)   # list(path, name, tmpl)

  # "__report__" is the escape hatch: picking it means "none of these fit" and
  # reveals the "tell our team" box. guided_live treats it as no-override.
  REPORT_OPT <- stats::setNames("__report__", "None of these - tell our team")
  guided_date_choices <- function(extra = NULL) {
    base <- setNames(vapply(wd_date_table(), `[[`, "", "fmt"),
                     vapply(wd_date_table(), `[[`, "", "label"))
    # Always include the working template's OWN date format, even if it isn't one
    # of the standard options - so an exotic format set on the Advanced tab stays
    # selectable and is never silently reverted to a list value by guided_live().
    if (!is.null(extra) && nzchar(extra) && !(extra %in% base))
      base <- c(base, stats::setNames(extra, sprintf("%s  (from Advanced)", extra)))
    c(base, REPORT_OPT)
  }
  guided_sign_choices <- function()
    c(setNames(names(wd_amount_labels()), unname(wd_amount_labels())), REPORT_OPT)

  # The current date-format / amount-sign of a template, wherever the format
  # stores them (PDF keeps them under `table`, delimited at the top / in columns).
  # A template may declare SEVERAL candidate date formats (the engine accepts one
  # only if it reads every value). The Simple tab is a single dropdown, so it shows
  # the first candidate -- and apply_overrides below refuses to write that single
  # value back unless the user actually PICKED a different one, otherwise merely
  # opening the toolkit on a multi-format template and pressing Save would silently
  # narrow it back to one format and re-break the statement it was widened for.
  gv_datefmt_all <- function(tmpl) if (identical(tmpl$format, "pdf")) (tmpl$table$date_format %||% "%d/%m/%Y")
                                   else (tmpl$columns$date$format %||% "%d/%m/%Y")
  gv_datefmt <- function(tmpl) as.character(gv_datefmt_all(tmpl))[1]
  gv_sign    <- function(tmpl) if (identical(tmpl$format, "pdf")) (tmpl$table$amount_sign %||% "signed")
                               else (tmpl$amount_sign %||% "signed")

  # apply_overrides -- fold the Basic-tab choices onto the working template. Only
  # the common fields live here; everything else is edited as YAML on Advanced.
  # .datefmt_unchanged -- did the user leave the date dropdown on what the template
  # already declares? True when the template lists SEVERAL formats and the shown
  # (first) one came back unchanged: writing it would drop the other candidates.
  .datefmt_unchanged <- function(tmpl, datefmt) {
    cur <- as.character(gv_datefmt_all(tmpl))
    length(cur) > 1L && identical(datefmt, cur[1])
  }

  # WHEN A LAYOUT APPLIES (schema keys effective_from / effective_to). The same
  # bank and product in 2020 and in 2024 is a genuinely different layout, and the
  # schema has always had a date range for saying so -- but nothing on screen ever
  # showed it, so the only way to have both was two rival templates that tie on
  # every statement forever. R/diagnose.R already reads the range and cautions
  # when a statement falls outside it.
  #
  # The two boxes are DATE PICKERS. That is the whole of the simplification: the
  # window used to be two free-text boxes, so "is this even a date?" was a question
  # the screen had to ask, answer and refuse in five helpers and eighty lines. A
  # date picker cannot hand back "last year", so the only thing left to check is
  # the one thing a pair of pickers still lets you say: an end before its start.
  #
  # .eff_date(x): a picker's value as the schema stores it ("yyyy-mm-dd"), or NULL
  # for an empty box -- which means ALWAYS, and is the normal answer.
  .eff_date <- function(x) {
    if (is.null(x) || !length(x) || is.na(x[1])) return(NULL)
    s <- trimws(as.character(x[1]))
    # The four letters "NA" mean ALWAYS, not a broken window. yaml round-trips an
    # absent value through that string, and without this a template saying "always"
    # opens with a red banner telling the user to fix something that is correct.
    if (!nzchar(s) || identical(toupper(s), "NA")) return(NULL)
    # NULL, never a throw. The docstring says "a string or NULL, full stop", but
    # as.Date("last year") ERRORS rather than returning NA -- so three call sites
    # wrapped this in tryCatch while set_eff() inside apply_overrides did not.
    # Unreachable today (its only caller feeds it dateInput values), which is
    # exactly the kind of gap that stops being unreachable when someone adds a
    # fourth caller. Honour the contract here instead of at each call site.
    d <- suppressWarnings(tryCatch(as.Date(s), error = function(e) as.Date(NA)))
    if (is.na(d)) return(NULL)
    format(d, "%Y-%m-%d")
  }
  # .eff_backwards(from, to) -- TRUE for the one window a date picker still lets a
  # person build by accident: one that applies to nothing at all.
  .eff_backwards <- function(from, to) {
    f <- .eff_date(from); t <- .eff_date(to)
    !is.null(f) && !is.null(t) && as.Date(t) < as.Date(f)
  }
  .EFF_BACKWARDS_MSG <- "The end date is before the start date, so this layout would apply to nothing at all."
  # .eff_picker(id, label, v) -- a date box that can be EMPTY, which is what a
  # template with no validity window says and is the usual answer.
  #
  # shiny::dateInput has no empty state of its own: given no value, its JS falls
  # back to TODAY (DateInputBinding.initialize in shiny.js). Left like that,
  # opening the toolkit on any ordinary template would show today's date in both
  # boxes and the save would stamp a one-day window on it -- a rule its author
  # never wrote, on a template that would then caution against every statement not
  # dated today. Caught by opening the toolkit in a browser. An EMPTY
  # data-initial-date is the one thing that JS reads as "leave the box alone", and
  # value = "" is how R produces it; shiny's date coercion warns on its way past a
  # non-date, which is the warning suppressed here.
  #
  # IT IS NOT THE ONLY ONE, and this comment said it was. An empty box leaves
  # bootstrap-datepicker holding an Invalid Date, and shiny's DateInputBinding
  # formats that into the literal string "NaN-NaN-NaN" and POSTS it -- once per
  # picker, so twice per toolkit open. The `shiny.date` input HANDLER then runs
  # as.Date() over it and re-signals the coercion error as a warning
  # ("character string is not in a standard unambiguous format", raised inside
  # the handler's own tryCatch, which is why the console blames value[[3L]]).
  # Verified in the browser: the binding's getValue() really does return
  # "NaN-NaN-NaN", and the log gained exactly two warnings per open.
  #
  # suppressWarnings() here cannot reach that one: it fires on a later tick, in
  # shiny's input decoding, long after this call returned. So the sentinel is
  # translated where it arrives -- see the shiny.date input handler registered at
  # the top of this file.
  # A stored window that is NOT a date (hand-edited YAML on the server; the
  # Advanced tab refuses to apply one) opens the box EMPTY rather than throwing.
  # It must not throw: this modal is the only place that YAML can be fixed, so a
  # toolkit that will not open over a bad template is a dead end with the repair
  # tool locked inside it. Never silent either -- g_eff_msg says so, and says that
  # saving replaces it.
  # .eff_stored_ok(tmpl) -- can the pickers actually SHOW this template's stored
  # window? It used to answer by catching .eff_date()'s error, which meant the
  # detection depended on that helper THROWING -- so the moment .eff_date was made
  # to honour its "a string or NULL, full stop" contract, a stored "last year"
  # silently became "always" and the red banner stopped appearing. A guard that
  # rests on another function's exception is a guard waiting to be deleted by
  # someone tidying up. It now asks the question directly.
  .eff_shows <- function(v) {
    if (is.null(v) || !length(v) || is.na(v[1])) return(TRUE)     # absent = always
    s <- trimws(as.character(v[1]))
    if (!nzchar(s) || identical(toupper(s), "NA")) return(TRUE)   # blank / "NA" = always
    !is.null(.eff_date(s))                                        # anything else must parse
  }
  .eff_stored_ok <- function(tmpl)
    .eff_shows(tmpl$effective_from) && .eff_shows(tmpl$effective_to)
  .eff_picker <- function(id, label, v)
    suppressWarnings(dateInput(id, label,
                               value = tryCatch(.eff_date(v) %||% "", error = function(e) ""),
                               format = "dd M yyyy", startview = "year",
                               autoclose = TRUE, width = "100%"))
  # .eff_set(id, v) -- put a schema date into a picker already on screen, or EMPTY
  # it again. Emptying is why this is not a plain updateDateInput: that call DROPS
  # a NULL value (leaving whatever is already in the box), so loading a template
  # that says "always" would keep the previous one's dates on screen and then save
  # them. NA is how R says JSON null, which is what clears a date picker.
  .eff_set <- function(id, v) session$sendInputMessage(id, list(value = .eff_date(v) %||% NA))

  apply_overrides <- function(tmpl, bank, datefmt, sign, decimal = NULL,
                              unsigned_default = NULL, desc_col = NULL,
                              ref_col = NULL, bal_col = NULL,
                              id = NULL, type = NULL, currency = NULL,
                              date_col = NULL, amount_col = NULL,
                              keep_dateless = NULL,
                              type_debit_value = NULL, type_credit_value = NULL,
                              fingerprint_text = NULL,
                              effective_from = NULL, effective_to = NULL) {
    # The validity window. An EMPTY picker clears the key outright, so a template
    # with no window looks exactly like one that never had one. NULL is different
    # and means "the control is not on screen at all" (it has not rendered yet):
    # that leaves whatever the template already carries alone, because absence of a
    # control is not an instruction to delete anything. That is Shiny's own
    # distinction between an input that does not exist and an empty one.
    set_eff <- function(t, key, v) {
      if (is.null(v)) return(t)
      t[[key]] <- .eff_date(v)      # NULL from an empty box DELETES the key
      t
    }
    tmpl <- set_eff(tmpl, "effective_from", effective_from)
    tmpl <- set_eff(tmpl, "effective_to",   effective_to)
    if (!is.null(id) && nzchar(trimws(id)))
      tmpl$id <- gsub("[^A-Za-z0-9_]+", "_", trimws(id))   # the name it saves under
    if (!is.null(type) && nzchar(trimws(type))) tmpl$statement_type <- trimws(type)
    if (!is.null(currency) && nzchar(trimws(currency))) tmpl$currency <- trimws(currency)
    if (!is.null(bank) && nzchar(bank)) tmpl$bank <- bank
    if (identical(tmpl$format, "pdf")) {
      # The Simple-tab identifying phrases (one per line). PDF only -- the box is
      # not rendered for delimited/excel, and a Shiny input keeps its last value, so
      # applying it outside this branch would stamp the previous PDF's phrases onto
      # a CSV template. Blank means "leave whatever is there" (never silently wipe
      # a fingerprint); min_score follows the count so every phrase must be present.
      if (!is.null(fingerprint_text)) {
        ph <- fingerprint_phrases(fingerprint_text)
        cur <- as.character(unlist(tmpl$fingerprint$page_contains_all %||% list()))
        # ONLY when she actually changed them. A curated template opened for
        # refinement may deliberately carry min_score BELOW its phrase count
        # ("any 2 of these 4"); touching min_score on every keystroke would rewrite
        # that rule behind her back. An edit does set min_score to the phrase count,
        # which is what the box says out loud: all of them must appear.
        # A BLANK box means "leave whatever is there", as it always has: emptying
        # it used to wipe the list and then refuse the save with an accusation of
        # genericness about an empty box. Only a non-empty edit changes anything.
        if (length(ph) && !identical(ph, cur)) {
          # Assign the list outright; do NOT modifyList it. page_contains_all is
          # UNNAMED and modifyList merges only by name, so it silently kept the
          # drafter's phrases while min_score still dropped to the count of what she
          # typed -- the box appeared to work and instead LOOSENED the fingerprint
          # (3-of-3 became 1-of-3), which is exactly the "matched another bank's
          # statement" failure the gate in R/templates.R exists to stop. Updating
          # the list in place keeps any other fingerprint keys.
          fp <- tmpl$fingerprint %||% list()
          fp$page_contains_all <- as.list(ph)
          tmpl$fingerprint <- fp
          tmpl$min_score <- max(1L, length(ph))
        }
      }
      if (!is.null(datefmt) && nzchar(datefmt) && !.datefmt_unchanged(tmpl, datefmt))
        tmpl$table$date_format <- datefmt
      if (!is.null(sign) && nzchar(sign)) tmpl$table$amount_sign <- sign
      # Shared-date (HSBC-style) opt-in: only stamp the key when ON, so normal
      # templates stay clean and unaffected.
      if (!is.null(keep_dateless))
        tmpl$table$keep_dateless_rows <- if (isTRUE(keep_dateless)) TRUE else NULL
    } else {
      if (!is.null(datefmt) && nzchar(datefmt) && !is.null(tmpl$columns$date) &&
          !.datefmt_unchanged(tmpl, datefmt))
        tmpl$columns$date$format <- datefmt
      if (!is.null(sign) && nzchar(sign)) tmpl$amount_sign <- sign
      # Basic column-pickers (delimited): "" means "(none)" -> drop the mapping;
      # a name sets .source while preserving any other keys the field carries.
      set_src <- function(cols, field, val) {
        if (is.null(val)) return(cols)
        if (nzchar(val)) cols[[field]] <- modifyList(cols[[field]] %||% list(), list(source = val))
        else cols[[field]] <- NULL
        cols
      }
      tmpl$columns <- set_src(tmpl$columns, "description", desc_col)
      tmpl$columns <- set_src(tmpl$columns, "reference",   ref_col)
      tmpl$columns <- set_src(tmpl$columns, "balance",     bal_col)
      # Date / Amount pickers: set_src preserves the date's format key, and ""
      # (the "(pick a column)" placeholder) only ever drops an already-absent
      # mapping, so an auto-detected column is never silently unmapped.
      tmpl$columns <- set_src(tmpl$columns, "date",   date_col)
      tmpl$columns <- set_src(tmpl$columns, "amount", amount_col)
    }
    # decimal_mark / unsigned_default are top-level keys the engine reads.
    if (!is.null(decimal) && nzchar(decimal))
      tmpl$decimal_mark <- if (identical(decimal, "auto")) NULL else decimal
    if (!is.null(unsigned_default) && nzchar(unsigned_default) &&
        identical(sign, "unsigned"))
      tmpl$unsigned_default <- unsigned_default
    # type_dc tokens: which indicator value means a debit (required for the sign),
    # and optionally which means a credit (declaring it makes an unrecognised
    # indicator fail closed instead of defaulting to credit). Only stamped for the
    # type_dc style, so switching away clears them.
    if (identical(sign, "type_dc")) {
      if (!is.null(type_debit_value) && nzchar(trimws(type_debit_value)))
        tmpl$type_debit_value <- trimws(type_debit_value)
      tmpl$type_credit_value <- if (!is.null(type_credit_value) && nzchar(trimws(type_credit_value)))
        trimws(type_credit_value) else NULL
    } else {
      tmpl$type_debit_value <- NULL; tmpl$type_credit_value <- NULL
    }
    tmpl
  }

  # "Show the settings for this statement" -- the one control that separates the
  # accountant's four steps from everything the tool worked out for itself
  # (charter: the interface rule). Same pattern, and the same reasoning, as
  # cv_detail_open on the Convert result page.
  #
  # STICKY BY DESIGN. Once someone opens it, it stays open for every template they
  # set up for the rest of their session, so a maintainer refining templates all
  # afternoon opens it once. Closing it again is equally sticky.
  g_more_open <- reactiveVal(FALSE)
  observeEvent(input$g_more, g_more_open(!isTRUE(g_more_open())))
  output$g_more_open <- reactive({ isTRUE(g_more_open()) })
  outputOptions(output, "g_more_open", suspendWhenHidden = FALSE)

  output$g_more_toggle <- renderUI({
    open <- isTRUE(g_more_open())
    # Named for what is behind it, not for the mechanism. "Advanced" would tell an
    # accountant it is not for her; these really are the settings for this one
    # statement, and the honest thing to say is that they are already filled in.
    div(style = "margin:16px 0 6px",
      actionLink("g_more", style = "font-weight:700;font-size:14.5px",
        label = if (open) "Hide the settings for this statement"
                else "Show the settings for this statement"),
      div(class = "muted", style = "font-size:13px;margin-top:2px",
          "Identifying phrase, save name, number punctuation, when this layout applies."))
  })

  # Statement template toolkit. Your statement is ALWAYS on the left (the PDF page,
  # or sample rows for a CSV) so you can see what you're answering; the controls
  # are on the right (Simple for the common case, Advanced for the full YAML). A
  # live preview underneath shows exactly what will be pulled out.
  show_guided_modal <- function() {
    g <- guided(); req(g); tmpl <- g$tmpl
    is_pdf   <- identical(tmpl$format, "pdf")
    cur_fmt  <- gv_datefmt(tmpl); cur_sign <- gv_sign(tmpl)
    cur_dec  <- tmpl$decimal_mark %||% "auto"
    cur_ud   <- tmpl$unsigned_default %||% "debit"

    # LEFT: the statement itself, always visible. ONE gesture, asked once: draw a
    # box, say what it is, Assign. It used to be two gestures with two dropdowns
    # and four buttons -- a column band and a pinned header value -- which read as
    # two separate features when it is the same box either way. The dropdown now
    # carries the difference (see .meta_field), so the page has one thing to do.
    left_panel <- if (is_pdf) tagList(
      strong("Your statement"),
      # "Drag a box across a column and say what it is" is step 1 of the strip four
      # inches above; what is left here is the part the strip does not say and the
      # page cannot show.
      p(class = "muted", HTML(
        "Only a box's <b>left-right</b> position matters - a column runs the full height of the page.")),
      fluidRow(
        column(3, numericInput("g_pdf_page",
          if (isTRUE(g$n_pages > 1L)) sprintf("Page (1 to %d)", g$n_pages) else "Page",
          1, min = 1, max = g$n_pages %||% NA, step = 1)),
        column(9, selectInput("g_pdf_field", "What did you draw a box around?",
                              list("(what is this?)" = "",
                                   "A column - read on every row" =
                                     c("date", "description", "amount", "balance", "particulars",
                                       "reference", "type", "debit", "credit", "other_party", "code"),
                                   "A one-off value - read from just that spot" =
                                     c("opening balance"          = "meta:opening_balance",
                                       "closing balance"          = "meta:closing_balance",
                                       "statement period - start" = "meta:period_start",
                                       "statement period - end"   = "meta:period_end",
                                       "account number"           = "meta:account_number",
                                       "account name"             = "meta:account_name")),
                              width = "100%"))),
      div(actionButton("g_pdf_assign", "Assign it", class = "btn-primary"),
          actionButton("g_pdf_remove", "Remove it")),
      # WHAT JUST HAPPENED, WHERE IT HAPPENED. Both buttons wrote their
      # confirmation into g_adv_msg -- which lives on the ADVANCED tab, behind a
      # click, out of sight of the person drawing boxes. So assigning a column and
      # removing one both looked like nothing at all, on the two controls the whole
      # toolkit is built around.
      uiOutput("g_pdf_msg"),
      # Behind the one disclosure: a column of your own naming, and the shared-date
      # opt-in. Neither belongs in setting up an ordinary statement.
      #
      # THE TOGGLE ITSELF IS NOT REPEATED HERE. It used to be, and a second
      # uiOutput with the same id threw "Duplicate binding for ID g_more_toggle"
      # in the browser, which ABORTS Shiny's whole bind pass for the modal: on a
      # PDF, not one control the toolkit draws statically - the bank name, the
      # date format, the amount style, the page number, Assign it, Remove it -
      # was ever wired to the server. Boxes drawn did nothing, the page number did
      # nothing, and the bands stayed at their drafted defaults, which is exactly
      # the "the boxes I drew came back, and the credit column reads nothing"
      # N29 was filed for. One id, one place. The toggle on the Simple tab is
      # always rendered and opens this panel too.
      conditionalPanel("output.g_more_open == true",
        tags$hr(style = "margin:8px 0"),
        textInput("g_pdf_custom", "\u2026or a column name of your own (overrides the list)",
                  "", width = "100%"),
        checkboxInput("g_keep_dateless",
          "Several rows share one date (e.g. HSBC) - keep the undated rows too (blank date, flagged)",
          value = isTRUE(tmpl$table$keep_dateless_rows))),
      # The box commits when the mouse is RELEASED, not while it is being dragged.
      # Shiny sends the brush on every mouse-move, and each send re-reads the
      # statement, so nudging a band a few points fought a re-parse the whole way.
      # A long debounce is how that is said: mid-drag sends are swallowed, and the
      # release flushes immediately. A mis-drawn band is the commonest cause of a
      # wrong amount column, so this is correctness, not comfort.
      plotOutput("g_pdf_plot", height = "560px",
                 brush = brushOpts("g_pdf_brush", delay = 1500, delayType = "debounce")))
    else tagList(
      strong("Your statement - the first rows"),
      div(class = "mono", style = "max-height:560px;overflow:auto;border:1px solid #eee;padding:8px;font-size:12px",
          verbatimTextOutput("g_raw_sample")))

    # RIGHT: the controls.
    #
    # THE INTERFACE RULE (charter), applied to template setup.
    #
    # DEFAULT: the only things an accountant genuinely has to do -- point at the
    # columns (on the page for a PDF, in the pickers below for a CSV), say which
    # bank it is, check the preview underneath, Save. She confirms the tool's
    # reading by LOOKING AT HER OWN TRANSACTIONS, which she can do, rather than by
    # reading a date-format string or judging a recognition phrase, which she
    # cannot. Everything she is asked here, she can answer.
    #
    # BEHIND ONE CONTROL: everything the tool already worked out for itself from
    # her file, and only needs touching when the preview looks wrong -- the date
    # format, the amount style, the number punctuation, the phrase that
    # identifies the bank, the name it saves under, the "tell our team" hatch.
    # Nothing is deleted; the whole template as text is still on Advanced.
    #
    # The click is REMEMBERED for the session (g_more_open), exactly as on the
    # Convert result page: a maintainer opens it once and never sees it closed
    # again, and nobody is ever asked whether they are an advanced user.
    right_panel <- tabsetPanel(
      id = "g_tabs",
      tabPanel(
        "Simple", br(),
        textInput("g_bank", "Which bank is this statement from?", value = tmpl$bank, width = "100%"),
        if (!is.null(g$cols) && length(g$cols)) tagList(
          p(class = "muted", style = "margin:2px 0 8px",
            "Leave as detected unless the preview looks wrong."),
          fluidRow(
            column(4, selectInput("g_col_date", "Date (required)",
                                  choices = c("(pick a column)" = "", g$cols),
                                  selected = tmpl$columns$date$source %||% "")),
            column(4, selectInput("g_col_amt", "Amount",
                                  choices = c("(pick a column)" = "", g$cols),
                                  selected = tmpl$columns$amount$source %||% "")),
            column(4, selectInput("g_col_desc", "Description (required)",
                                  choices = g$cols,
                                  selected = tmpl$columns$description$source %||% g$cols[1])))),
        # THE DATE FORMAT AND THE AMOUNT STYLE STAY IN FRONT. They were briefly moved
        # behind the disclosure with everything else, on the theory that the drafter
        # fills them in. It does - and it is wrong often enough that these are the two
        # settings an analyst reports changing on almost every template she builds.
        # Frequency beats how technical a thing looks: hiding what is needed nearly
        # every time puts a click on the MOST common path, not the rarest. The rule is
        # "don't ask a question the tool can answer" - the tool cannot reliably answer
        # these two, so it asks, with its best guess already selected.
        tags$hr(style = "margin:14px 0 10px"),
        p(class = "muted", style = "margin:0 0 8px",
          "Detected from your statement - worth a check against the preview."),
        fluidRow(
          column(6, selectInput("g_date", "How are the dates written?",
                                choices = guided_date_choices(cur_fmt), selected = cur_fmt)),
          column(6, selectInput("g_sign", "How are amounts shown?",
                                choices = guided_sign_choices(), selected = cur_sign))),
        # These follow the amount style OUT: they appear only when that style is
        # chosen, and when they appear they are required. Left behind the disclosure,
        # picking "a D/C column" showed nowhere to say what D means.
        fluidRow(column(6, conditionalPanel(
              "input.g_sign == 'unsigned'",
              selectInput("g_unsigned_default", "A plain number (no + / \u2212 / CR) is a\u2026",
                          choices = c("Charge - money out" = "debit",
                                      "Payment - money in" = "credit"),
                          selected = cur_ud)))),
        conditionalPanel(
            "input.g_sign == 'type_dc'",
            fluidRow(
              column(6, textInput("g_type_debit", "Which indicator value means money OUT (debit)?",
                                  value = tmpl$type_debit_value %||% "")),
              column(6, textInput("g_type_credit", "\u2026and money IN (credit)? (blank = anything else is a credit)",
                                  value = tmpl$type_credit_value %||% "")))),
        uiOutput("g_more_toggle"),   # what is left is genuinely rare
        conditionalPanel("output.g_more_open == true",
          fluidRow(column(6, selectInput("g_decimal", "How are numbers punctuated?",
                                  choices = c("Auto-detect (NZ / AU / UK / US)" = "auto",
                                              "1,234.56 - dot is the decimal point" = "dot",
                                              "1.234,56 - comma is the decimal (European)" = "comma"),
                                  selected = cur_dec))),
          # THE PHRASE THAT RECOGNISES THIS BANK. For PDFs this is the one setting
          # that can refuse a save, and it used to be editable only in the raw YAML
          # box on Advanced -- a hard stop at the last step of the flow for the exact
          # person the toolkit exists for. So it stays here, in plain words, offering
          # phrases actually found on her statement; a refused save opens this panel
          # for her (see g_save) rather than naming a place she has to go and find.
          if (is_pdf) tagList(
            tags$hr(),
            strong("A distinctive phrase printed on this statement"),
            # No "avoid single common words like Balance": the box below says so
            # live, by name, against the same rule the save uses (g_fp_msg).
            # THE SAME SENTENCE THE OTHER BUILDER USES. This was three sentences
            # saying what the report builder says in one, and the two screens ask
            # for the same thing - a phrase printed on the page that identifies
            # the layout. One question, one wording, both routes.
            p(class = "muted", style = "margin:4px 0 6px",
              HTML(paste0("One phrase per line - all must appear, and they must not be words every ",
                          "statement carries or anything naming a customer."))),
            if (length(g$fp_candidates))
              selectInput("g_fp_pick", "Phrases found on your statement",
                          choices = c("(choose one to add it below)" = "", g$fp_candidates),
                          width = "100%"),
            textAreaInput("g_fp", NULL, width = "100%", rows = 3,
                          value = paste(unlist(tmpl$fingerprint$page_contains_all %||% list()),
                                        collapse = "\n")),
            uiOutput("g_fp_msg")),
          # The optional columns: real fields, but nothing is missing from the
          # result if they are left alone, so they wait here with the rest.
          if (!is.null(g$cols) && length(g$cols)) tagList(
            tags$hr(),
            fluidRow(
              column(6, selectInput("g_col_ref", "Reference (optional)",
                                    choices = c("(none)" = "", g$cols),
                                    selected = tmpl$columns$reference$source %||% "")),
              column(6, selectInput("g_col_bal", "Balance (optional)",
                                    choices = c("(none)" = "", g$cols),
                                    selected = tmpl$columns$balance$source %||% "")))),
          tags$hr(),
          fluidRow(
            column(6, textInput("g_id", "Saves under this name", value = tmpl$id %||% ""),
                   # SAY IT BEFORE THE SAVE, NOT AFTER. Saving a TESTED template
                   # under its own name would be shadowed -- the curated set wins on
                   # an id clash -- so the save quietly stores it as "<id>_custom"
                   # with a `refines:` line, and detection then prefers the
                   # correction. That is right, and it was invisible: the box said
                   # one name and the toast afterwards said another, which reads as
                   # the tool ignoring what you typed. A template built here has no
                   # such problem and really is saved over, in place.
                   if ((tmpl$id %||% "") %in% (g$default_ids %||% character(0)))
                     helpText(HTML(sprintf(paste(
                       "<b>%s is a tested template</b>, so it is not edited in place.",
                       "Saving stores your version as <code>%s_custom</code>, which then",
                       "wins over the original whenever this layout is detected -",
                       "the original stays untouched as the thing you can fall back to."),
                       htmltools::htmlEscape(tmpl$id), htmltools::htmlEscape(tmpl$id))))
                   else helpText("Saving replaces this template, in place.")),
            column(6, textInput("g_currency", "Currency", value = tmpl$currency %||% "NZD"))),
          textInput("g_type", "Kind of statement", value = tmpl$statement_type %||% "everyday",
                    width = "100%"),
          # WHEN THIS LAYOUT APPLIES. Rare, so it is behind the disclosure with the
          # rest of the rarely-touched settings - but it is the only answer to a
          # real problem: the same bank and the same product printed differently in
          # 2020 and in 2024. Without it the two layouts have to be two templates
          # that fit equally well and tie on every statement forever.
          #
          # DATE PICKERS, not text boxes: the format is then the tool's problem
          # rather than the user's, and "that is not a date" stops being something
          # the screen has to detect, word and refuse. Empty means always, which is
          # the normal answer and what every shipped template says.
          tags$hr(),
          strong("When this layout applies"),
          p(class = "muted", style = "margin:4px 0 6px", "Leave both empty for always."),
          fluidRow(
            column(6, .eff_picker("g_eff_from", "This layout applies from",
                                  tmpl$effective_from)),
            column(6, .eff_picker("g_eff_to", "\u2026to (empty = no end)",
                                  tmpl$effective_to))),
          uiOutput("g_eff_msg")),
        # THE WAY OUT STAYS IN FRONT. This is for someone ALREADY stuck, so putting
        # it inside the settings disclosure meant it appeared only to people who had
        # worked out there was a disclosure - and the "none of these fit" dropdown
        # option pointed at a box that was not on screen. It is now the last thing on
        # Simple, outside the disclosure, on every statement.
        tags$hr(style = "margin:14px 0 10px"),
        div(style = "padding:10px 12px;border:1px dashed #c98a00;background:#fffbe9;border-radius:8px",
          strong("None of these fit? Tell our team"),
          p(class = "muted", style = "margin:4px 0 6px",
            "In plain words - no names or numbers."),
          textAreaInput("g_req_detail", NULL, width = "100%", rows = 2,
            placeholder = "e.g. Dates look like 2 Dez (German). Amounts end in 'H' for credit."),
          actionButton("g_req_send", "Send to our team", class = "btn-warning"),
          uiOutput("g_req_msg"))),
      tabPanel(
        "Advanced", br(),
        helpText(HTML("The <b>complete</b> template as text. Load your Simple choices in, edit, then Check &amp; apply.")),
        div(actionButton("g_adv_load", "Load current settings"),
            actionButton("g_adv_apply", "Check & apply", class = "btn-primary")),
        br(), uiOutput("g_adv_msg"),
        textAreaInput("g_yaml", NULL, value = template_yaml(tmpl), width = "100%", rows = 24)))

    showModal(modalDialog(
      title = "Statement template toolkit", size = "l", easyClose = FALSE,
      div(class = "note", style = "margin-bottom:8px",
        HTML(sprintf("Setting up: <b>%s</b> &nbsp;\u00b7&nbsp; %s &nbsp;\u00b7&nbsp; ",
             htmltools::htmlEscape(g$name %||% "your file"),
             if (is_pdf) "PDF" else if (identical(tmpl$format, "excel")) "Excel" else "CSV / delimited")),
        # THE WAY BACK. The first question -- statement, or labelled values? --
        # is asked once on the page BEHIND this modal, so from in here it could
        # not be revisited, and opened from Convert it was never asked at all.
        # Cancel threw the work away without answering it. This carries the same
        # file to the other builder. (The old wording pointed at a control
        # "above", which is not on screen once this modal is open - the same
        # mistake N28 was.)
        actionLink("g_not_statement", "Not a transaction table?")),
      # An always-visible mini-guide: the guide is a separate modal and Shiny shows
      # one modal at a time, so once the toolkit is open it can't be reopened. This
      # strip keeps the whole flow on screen the entire time.
      #
      # It ended with "Everything else is already filled in from your own file -
      # you're just confirming it", twice, once per branch. On the bundled specimen
      # that is true; on asb.pdf and anz_single.pdf draft_template() fills in
      # nothing that reads a single row, and the sentence then tells somebody
      # staring at an empty preview that there is nothing left to do. The steps say
      # what to do; the preview says whether it worked. Neither needs reassuring.
      div(style = "padding:8px 12px;background:#f6faf7;border:1px solid #cfe6d8;border-radius:6px;margin-bottom:10px;font-size:13px",
        HTML(if (is_pdf)
          paste0("<b>How this works:</b> &nbsp;1&nbsp;Drag a box over a column and say what it is &nbsp;\u00b7&nbsp; ",
                 "2&nbsp;click <b>Assign it</b> &nbsp;\u00b7&nbsp; 3&nbsp;name the bank on the right &nbsp;\u00b7&nbsp; ",
                 "4&nbsp;check the <b>Preview</b> below &nbsp;\u00b7&nbsp; 5&nbsp;<b>Save</b>.")
        else
          paste0("<b>How this works:</b> &nbsp;1&nbsp;check each column picker matches your file &nbsp;\u00b7&nbsp; ",
                 "2&nbsp;name the bank &nbsp;\u00b7&nbsp; 3&nbsp;check the <b>Preview</b> below &nbsp;\u00b7&nbsp; ",
                 "4&nbsp;<b>Save</b>."))),
      fluidRow(column(6, left_panel), column(6, right_panel)),
      tags$hr(),
      h4("Preview - what will be pulled out of your statement"),
      uiOutput("g_status"),
      DTOutput("g_preview"),
      footer = tagList(modalButton("Cancel"),
        actionButton("g_save", "Save template", class = "btn-primary"))))
  }

  # "Not a transaction table?" -- close the toolkit, put the SAME file in the
  # other builder and select it there. Nothing is re-uploaded and nothing is lost.
  observeEvent(input$g_not_statement, {
    g <- isolate(guided())
    removeModal()
    if (!is.null(g$path) && file.exists(g$path)) rb_handoff(g$path)
    updateRadioButtons(session, "ts_doctype", selected = "other")
    updateTabsetPanel(session, "main_tabs", selected = "Add a template")
    showNotification("Switched to the other builder - your document came with you.",
                     type = "message", duration = 6)
  })

  # open_guided -- the single entry into the setup modal, shared by every launch
  # point (Convert result, Admin pickup, Add-a-template). Drafts a template from
  # the file unless the caller already has one (e.g. the matched template).
  open_guided <- function(path, name, seed_tmpl = NULL, upload_id = NA_character_) {
    tmpl <- seed_tmpl
    if (is.null(tmpl)) {
      bankguess <- trimws(tools::toTitleCase(gsub("[^A-Za-z]+", " ", tools::file_path_sans_ext(name))))
      tmpl <- withProgress(message = "Opening the toolkit\u2026", value = 0.4,
        tryCatch(draft_template(path, bank = if (nzchar(bankguess)) bankguess else "New bank"),
                 error = function(e) NULL))
    }
    if (is.null(tmpl)) {
      # Fail loud AND specific. An Excel draft comes back NULL only when no sheet
      # held a recognisable transaction table (a date column + a money column).
      #
      # AND POINT AT A CONTROL THAT IS ON SCREEN. This branch RETURNS -- the toolkit
      # modal is never shown -- so "use 'Not a transaction table?' at the top of
      # this window" named a link inside a window that does not exist, from Convert
      # as well as from Add a template. That is the identical mistake the comment
      # on g_not_statement was written to record. "Something else" is a radio on
      # the Add-a-template tab, which is always reachable and always visible.
      if (tolower(tools::file_ext(name %||% "")) %in% c("xlsx", "xlsm", "xls")) {
        showNotification(paste("No transaction table in this workbook - the toolkit needs a sheet with a",
                               "date column and an amount column. If the table is unusual, save the sheet",
                               "as CSV and set that up instead."),
                         type = "warning", duration = 10)
      } else {
        # NAME THE BUTTON THAT IS ON THE SCREEN. This is where somebody lands who
        # overrode "this looks like a report" with "no, it is a statement" -- so
        # the answer is one click away, on the card they just used, and the
        # message should say which click. It used to name a radio called
        # "Something else", on a tab it did not offer to open, and the radio is
        # called "Anything else".
        showNotification(paste("Couldn't read this file as a transaction table - there is no column of",
                               "dates with a column of amounts beside it. If it is a report, a form or a",
                               "letter, press \u201cSet it up as a report\u201d on the green card. If it really",
                               "is a statement, a text PDF or a CSV export of it will read."),
                         type = "error", duration = 14)
      }
      return(invisible(FALSE))
    }
    # Ids of the curated (tested) templates: saving a customised copy under one of
    # these would be shadowed (defaults win), so g_save gives it a distinct id.
    default_ids <- tryCatch(names(load_templates(TEMPLATES_DIR, strict = FALSE)),
                            error = function(e) character(0))
    # For delimited statements, offer the file's actual columns in the Basic
    # field-pickers (PDF columns are bands, edited visually / in Advanced).
    cols <- if (identical(tmpl$format, "delimited"))
      tryCatch(names(read_delimited(read_input(path), tmpl)$table), error = function(e) NULL)
    else if (identical(tmpl$format, "excel"))
      tryCatch(names(read_input(path)$table), error = function(e) NULL)
    else NULL
    # Candidate identifying phrases the drafter actually FOUND on this page, so the
    # Simple tab can offer real choices ("pick a phrase that's printed on your
    # statement") instead of sending a non-technical user to the raw YAML box.
    fp_cands <- if (identical(tmpl$format, "pdf"))
      safe(header_phrases(read_input(path), n = 8), character(0)) else character(0)
    # How many pages the sample has, counted once here: the toolkit's page box says
    # so out loud and can't be walked past the end of the document.
    npages <- if (identical(tmpl$format, "pdf"))
      safe({ i <- read_input(path); as.integer(length(i$pages %||% i$words %||% list())) },
           NA_integer_) else NA_integer_
    if (!isTRUE(npages >= 1L)) npages <- NA_integer_
    cv_upload_id(upload_id)
    g_id_auto(tmpl$id %||% NA_character_)   # the drafted name is the tool's, not hers
    guided(list(path = path, name = name, tmpl = tmpl, default_ids = default_ids,
                cols = cols, n_pages = npages,
                fp_candidates = unique(trimws(as.character(fp_cands %||% character(0))))))
    show_guided_modal()
    invisible(TRUE)
  }

  # Launch the same setup modal from the Add-a-template tab (not tied to a Convert
  # upload, so a successful Save just adds the template).
  observeEvent(input$ts_go, {
    if (is.null(input$ts_file)) {
      showNotification("Upload the document first (the file picker above), then open the toolkit.",
                       type = "warning", duration = 6)
      return()
    }
    sess <- tempfile("ts_")   # guaranteed-unique per session/process (no cross-user bleed)
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, input$ts_file$name)
    file.copy(input$ts_file$datapath, src, overwrite = TRUE)
    open_guided(src, input$ts_file$name)
  })

  output$cv_teach <- renderUI({
    res <- cv_res(); req(res)
    if (is.null(cv_src())) return(NULL)
    st <- res$status %||% "failed"
    # NO FORM OR REPORT BRANCH HERE ANY MORE. Two links used to sit at the top of
    # this renderer offering "Set it up on Add a template" for those two kinds --
    # and they were DEAD CODE on the very route they were written for, because
    # this output is rendered inside a panel that requires the result NOT to be a
    # form and NOT to be a report. Worse, they only switched tab: the document and
    # the template that had just read it were both thrown away, so following one
    # landed on "Upload the document above to start". cv_edit is that door now,
    # it renders on all three kinds, and it carries both. Two half-working
    # controls removed, one working one kept.
    if (identical(st, "unsupported")) {
      # "Unsupported" covers two opposite situations. If two or more templates fit
      # this statement EQUALLY well, the tool refuses to guess which -- but telling
      # the analyst "this layout is new, build a template" would be flatly wrong
      # advice: we already have templates that fit, and a third would only tie too.
      # So name them and let one click convert with either.
      #
      # The old line here said "the tool won't pick for you". It does pick, always:
      # R/convert.R takes the best candidate and reads the statement with it. What
      # brought her to THIS panel is that the one it picked read no rows. Saying
      # otherwise contradicted the engine's own design note two files away, and
      # made a deliberate deterministic choice look like a refusal to choose.
      tied <- as.character(res$detect$tied %||% character(0))
      if (isTRUE(res$detect$ambiguous) && length(tied) >= 2) {
        return(div(style = "margin:12px 0;padding:14px;border:1px solid var(--warn-line);background:var(--warn-bg);border-radius:8px",
          strong(sprintf("%d templates fit this statement equally well - which one is it?", length(tied))),
          p(class = "muted", style = "margin:6px 0 10px",
            "The one the tool used read no rows. Pick another and it converts straight away - nothing is saved or changed."),
          div(style = "display:flex;flex-wrap:wrap;gap:8px;align-items:flex-end",
            div(style = "flex:1 1 340px",
              selectInput("cv_tie_pick", NULL, choices = tpl_choices(tied), width = "100%")),
            div(style = "margin-bottom:15px",
              actionButton("cv_tie_go", "Convert with this one", class = "btn-primary"))),
          div(class = "muted", style = "font-size:13px",
            "Sure it's neither? ",
            # Its own id: this link and the "Set up a template" button below render
            # TOGETHER on a tied-and-unsupported result, and two elements sharing an
            # input id keep independent click counters -- so one of them sends a
            # value identical to the current one and its observer never fires. A
            # dead button, invisible from R. Both call the same handler.
            actionLink("cv_teach_go_tie", "Set up a new template instead"),
            ".")))
      }
      # A TEMPLATE THAT MATCHED AND READ NOTHING IS NOT A NEW LAYOUT.
      #
      # R/diagnose.R states the rule outright: "'Add a template' is not the fix --
      # there IS one, its columns just sit in the wrong place. Send the analyst to
      # the template that failed, not to a blank form." The card did the opposite:
      # the headline said "This layout is new", the engine's own message directly
      # above it said "badbands_pdf matches the wording on this statement but read
      # no transactions from it", and the green button drafted a FRESH template
      # rather than opening the one that had just failed. Same card, three answers.
      if (.matched_but_empty(res)) {
        tid <- (res$template_id %||% NA_character_)[1]
        return(div(style = "margin:12px 0;padding:14px;border:1px solid var(--warn-line);background:var(--warn-bg);border-radius:8px",
          strong(sprintf("%s matched this statement but read no rows from it.",
                         if (!is.na(tid) && nzchar(tid)) friendly_tpl(tid) else "A template")),
          p(class = "muted", style = "margin:6px 0 10px",
            "Its columns sit in the wrong place. Open it and move them - a new template would read nothing either."),
          actionButton("cv_teach_go_empty", "Open that template and fix its columns",
                       class = "btn-primary btn-lg")))
      }
      # A new layout is a FORK, not a cliff. We WANT analysts setting up their own
      # templates, so the prominent GREEN action is "set it up yourself" (the tool
      # pre-fills what it can); handing it to the team is the small fallback.
      #
      # The headline says what happened, not what the file is. "This layout is new"
      # was printed verbatim over 4,000 bytes of /dev/urandom named .pdf and over a
      # 31-byte text file: neither is a layout, and neither is new. And the promise
      # underneath it -- "takes a couple of minutes, no data background needed" --
      # is not promised either. Re-measured after the drafter was fixed:
      # anz_single.pdf now drafts 311 rows over the file (79 in the toolkit's
      # 3-page preview), while asb.pdf still drafts a template that keeps one row
      # of ten and nothing on page 2. Half the measured cases are a couple of
      # minutes; the other half are not, so the button says what it does and
      # stops.
      # A CLEAR DECISION, OR A CLEAR WAY TO OVERRIDE IT. Never two big buttons.
      #
      # The first go at this offered both doors side by side with the likelier one
      # styled as primary. That is neither: it hands somebody a choice without
      # telling them they are making one, and the only difference between the two
      # is a shade of green. Beth reads two large buttons and asks which.
      #
      # So: when the tool can tell, it SAYS SO, does that thing under one button,
      # and puts the other answer underneath as an override that is unmistakably
      # the opposite answer rather than a second equal choice. When it cannot
      # tell, it says THAT, and asks -- two equal buttons are honest there and
      # nowhere else.
      #
      # doc_shape_hint() is the reading: table-shaped blocks, and lines that start
      # with a date and carry an amount, which is what a transaction row is
      # whatever bank printed it. The count is printed, so the decision is
      # checkable rather than asserted.
      #
      # The words for the two kinds are the words on Add a template -- "a bank or
      # card statement" and "anything else" -- because a person meeting the same
      # question twice should not have to learn it twice.
      hint <- cv_shape_hint()
      looks <- as.character(hint$looks %||% "neither")
      why <- p(class = "muted", style = "margin:6px 0 10px", hint$sentence)
      box <- function(...) div(
        style = "margin:12px 0;padding:14px;border:1px solid #b7e1b0;background:#eef8ec;border-radius:8px",
        ...,
        div(style = "margin-top:10px",
          span(class = "muted", "Would rather not set one up? "),
          actionLink("cv_unsup_raise", "Send it to the team instead")))
      # The override, spelled out as the OTHER ANSWER rather than as a button.
      other <- function(question, answer, id) div(
        style = "margin-top:10px;padding-top:10px;border-top:1px solid #cfe0d4",
        span(class = "muted", question, " "),
        actionLink(id, answer, style = "font-weight:700"))

      # THE DIAGNOSIS THAT OUTRANKS EVERY CARD BELOW, AND IT TAKES THE HEADLINE.
      #
      # Driven with no OCR software and an image-only PDF: this card said "This
      # looks like a report", the primary green button said "Set it up as a
      # report", and the engine's own diagnostics table further down the page
      # said, correctly and at severity HIGH, that this machine has no OCR
      # software installed and that building a template will NOT help until that
      # is done. Three answers on one screen, and the biggest button was the one
      # that cannot work -- so somebody spends twenty minutes drawing boxes on a
      # page the tool could not read a word of.
      #
      # The engine already grades who can fix a thing (.blocking_diag reads it).
      # Where the answer is not "a template", the diagnosis IS the headline, the
      # engine's own remedy is the action, and setting a template up drops to the
      # quiet override underneath -- the same shape every other branch of this
      # card uses for the answer it thinks is wrong.
      bd <- .blocking_diag(res)
      if (!is.null(bd))
        return(div(style = "margin:12px 0;padding:14px;border:1px solid var(--warn-line);background:var(--warn-bg);border-radius:8px",
          strong(style = "font-size:15px", .sentence(bd$detail[1])),
          if (nzchar(bd$how_to_fix[1] %||% ""))
            p(class = "muted", style = "margin:6px 0 0", bd$how_to_fix[1]),
          other("Sure a template is what this needs?", "Set it up anyway",
                "cv_teach_go_report")))

      # SHE ALREADY ANSWERED THIS. When "What is this?" on the left was set, the
      # kind is not in doubt and asking again is the tool ignoring what it was
      # told. The card goes straight to the door she chose, with the override
      # still underneath -- an answer can be changed, it just is not re-asked.
      asked <- as.character(res$asked_kind %||% "auto")[1]
      if (identical(asked, "other"))
        return(box(
          strong(style = "font-size:15px",
                 "No form or report template reads this yet."),
          p(class = "muted", style = "margin:6px 0 10px",
            "No bank statement template was tried - you said this is not one."),
          actionButton("cv_teach_go_report", "Set it up as a report \u2192",
                       class = "btn-primary btn-lg"),
          p(class = "muted", style = "margin:8px 0 0;font-size:12.5px",
            "You point at the tables and figures you want. It downloads, and never reaches the dashboards."),
          other("Is it a bank statement after all?",
                "Set it up as a bank statement", "cv_teach_go")))
      if (identical(asked, "statement"))
        return(box(
          strong(style = "font-size:15px", "No template reads this statement yet."),
          why,
          actionButton("cv_teach_go", "Set it up as a bank statement \u2192",
                       class = "btn-primary btn-lg"),
          p(class = "muted", style = "margin:8px 0 0;font-size:12.5px",
            "Set it up once and this layout converts every time, with its balance checked."),
          other("Not a statement after all?",
                "It is something else - a report, a form, a letter",
                "cv_teach_go_report")))

      if (identical(looks, "report"))
        return(box(
          strong(style = "font-size:15px",
                 "This looks like a report, not a bank statement."),
          why,
          actionButton("cv_teach_go_report", "Set it up as a report \u2192",
                       class = "btn-primary btn-lg"),
          p(class = "muted", style = "margin:8px 0 0;font-size:12.5px",
            "You point at the tables and figures you want. It downloads, and never reaches the dashboards."),
          other("Not a report?", "It is a bank or card statement", "cv_teach_go")))

      if (identical(looks, "statement"))
        return(box(
          strong(style = "font-size:15px",
                 "This looks like a bank statement, but no template reads it yet."),
          why,
          actionButton("cv_teach_go", "Set it up as a bank statement \u2192",
                       class = "btn-primary btn-lg"),
          p(class = "muted", style = "margin:8px 0 0;font-size:12.5px",
            "Set it up once and this layout converts every time, with its balance checked."),
          other("Not a statement?", "It is something else - a report, a form, a letter",
                "cv_teach_go_report")))

      # CANNOT TELL. Say that, and ask. Two equal buttons are honest here because
      # the tool genuinely has no answer -- and dressing a coin toss as a decision
      # is the thing this card exists to stop.
      box(
        strong(style = "font-size:15px", "The tool cannot tell what kind of document this is."),
        why,
        p(style = "margin:6px 0 8px", strong("Which is it?")),
        div(style = "display:flex;gap:8px;flex-wrap:wrap",
          actionButton("cv_teach_go", "A bank or card statement",
                       class = "btn-primary btn-lg"),
          actionButton("cv_teach_go_report", "Anything else - a report, a form, a letter",
                       class = "btn-primary btn-lg")),
        p(class = "muted", style = "margin:8px 0 0;font-size:12.5px",
          "A statement is a table of transactions with a running balance, and it reconciles. ",
          "Anything else is read by pointing at what you want."))
    } else {
      # Happy path stays quiet: the "Wrong bank?" line up top already offers a fix,
      # so we don't repeat a toolkit prompt here.
      if (identical(st, "ok")) return(NULL)
      # ...AND SO DOES A FILE THAT WAS NEVER READ. `failed` means no text came out
      # of it at all -- damaged, encrypted, or not the type it claims to be. The
      # toolkit's whole job is drawing boxes over columns on a page, and there is
      # no page: over a text file renamed .pdf the screen said "no text could be
      # read from this PDF" and then offered to "save an improved template" from
      # it. A template cannot be built out of nothing, so offering it is a wasted
      # trip. The card's own message already says what to do (check the file
      # opens, is the type it claims, is not password-protected), so nothing is
      # left unanswered by dropping this.
      if (identical(st, "failed")) return(NULL)
      div(style = "margin:12px 0;padding:10px 12px;border:1px solid #d9d9d9;background:#fafafa;border-radius:8px",
        span(class = "muted", "Fix how it's read and save an improved template. "),
        actionButton("cv_teach_go_fix", "Open the template toolkit", class = "btn-default"))
    }
  })
  # (The two "Set it up on Add a template" observers were here, one per other-route
  # kind. Both switched tab and nothing else, so the document and the template that
  # had just read it were dropped on the floor; cv_edit carries both and is the one
  # door. Deleted with the two links that drove them.)
  observeEvent(input$cv_empty_to_tmpl,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))
  observeEvent(input$ab_go_convert,
    updateTabsetPanel(session, "main_tabs", selected = "Convert"))
  observeEvent(input$ab_go_template,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))

  # .matched_but_empty(res) -- the engine matched a template's identifying wording
  # and that template then read zero transactions. The engine raises it as a
  # diagnostic category (R/diagnose.R), which is where this reads it from: it is
  # the one `unsupported` result whose template id is a REAL match rather than the
  # closest miss, so it is the one that may be seeded into the toolkit.
  .matched_but_empty <- function(res) {
    d <- res$diagnostics
    isTRUE(!is.null(d) && "category" %in% names(d) && any(d$category %in% "matched_but_empty"))
  }
  # WHAT THE DOCUMENT LOOKS LIKE, read once per conversion and only when asked.
  # It reads the first pages and runs the table proposer, so it is real work --
  # but it is only ever needed by the card that appears when nothing matched, and
  # a reactive computes it once and remembers it for that result.
  cv_shape_hint <- reactive({
    src <- cv_src()
    fallback <- list(tables = 0L, txn_lines = 0L, lines = 0L, looks = "neither",
                     sentence = "")
    if (is.null(src) || !file.exists(src$path %||% "")) return(fallback)
    inp <- tryCatch(read_input(src$path), error = function(e) NULL)
    if (is.null(inp)) return(fallback)
    tryCatch(doc_shape_hint(inp), error = function(e) fallback)
  })

  # THE REPORT DOOR, WITH THE FILE ALREADY THROUGH IT. Sending somebody to a tab
  # where they have to find and upload the same file again is the sort of small
  # tax that turns "try the other one" into "give up". rb_handoff is exactly the
  # hand-over the statement toolkit already uses.
  observeEvent(input$cv_teach_go_report, {
    src <- cv_src()
    if (!is.null(src) && !is.null(src$path) && file.exists(src$path)) rb_handoff(src$path)
    updateRadioButtons(session, "ts_doctype", selected = "other")
    updateTabsetPanel(session, "main_tabs", selected = "Add a template")
  })

  .teach_now <- function(seed_matched = FALSE) {
    src <- cv_src(); req(src)
    res <- cv_res()
    seed <- NULL
    # If the conversion MATCHED a template (ok / needs_review), open that template
    # so the user refines the real one. An unsupported result also carries a
    # template id - usually the CLOSEST MISS, for the logs - and seeding from that
    # would open the wrong bank's settings and save a fingerprint that can never
    # match this file. So unsupported drafts fresh from the file itself, EXCEPT on
    # matched_but_empty, where the wording really did match and the template that
    # failed is exactly the thing to open (seed_matched, from the card that names it).
    if (isTRUE(seed_matched) || (res$status %||% "") %in% c("ok", "needs_review")) {
      tid <- (res$template_id %||% NA_character_)[1]
      if (!is.na(tid) && nzchar(tid)) {
        tset <- tryCatch(templates(), error = function(e) list())
        if (!is.null(tset[[tid]])) seed <- tset[[tid]]
      }
    }
    open_guided(src$path, src$name, seed_tmpl = seed, upload_id = cv_upload_id())
  }
  observeEvent(input$cv_teach_go,       .teach_now())
  observeEvent(input$cv_teach_go_tie,   .teach_now())
  observeEvent(input$cv_teach_go_fix,   .teach_now())
  observeEvent(input$cv_teach_go_empty, .teach_now(seed_matched = TRUE))

  # Send an unsupported layout to the team (PII-safe: generic context only - a
  # file extension, the detected bank guess and the closest template - never file
  # contents or the file name).
  observeEvent(input$cv_unsup_raise, {
    res <- cv_res(); req(res)
    ctx <- list(
      file_ext = tolower(tools::file_ext(cv_src()$name %||% "")),
      format   = res$format %||% (res$header$format %||% "delimited"),
      bank     = res$header$bank %||% "",
      closest  = (res$template_id %||% "")[1])
    id <- tryCatch(record_template_request(
      "Unsupported statement layout - raised from Convert. Please set up a template.",
      ctx, requested_by = who_now(), dir = REQUESTS_DIR), error = function(e) NULL)
    if (is.null(id))
      showNotification("Couldn't send just now - please try again.", type = "error")
    else
      # What follows this is a promise about people ("you'll be able to convert it
      # once it's ready"), which no code here can keep or check. What IS checkable
      # is what left the machine, so that is what it says.
      showNotification("Sent to the team - the layout only, no statement contents.",
                       type = "message", duration = 7)
  })

  # "Matched but maybe wrong": when a near-duplicate template nearly matched too,
  # show the candidates + margin and let the analyst re-open the toolkit with a
  # different one. Only surfaces on a genuine CLOSE CALL - a confident match stays
  # clutter-free (Beth is never asked "is this the right template?" without cause).
  output$cv_candidates <- renderUI({
    res <- cv_res(); req(res); req(!is.null(res$candidates))
    cand <- res$candidates
    if (is.null(nrow(cand)) || nrow(cand) < 2) return(NULL)
    thin <- isTRUE(res$detect$thin)
    if (!thin) return(NULL)   # only on a close call; the happy path shows nothing here
    top <- utils::head(cand, 4L)
    # The candidate frame includes the matched winner; the "nearest others" line
    # and the picker must both EXCLUDE it (else it reads "matched X. Nearest
    # others: X ...").
    others_df <- top[top$id != res$template_id, , drop = FALSE]
    others <- others_df$id
    style <- if (thin) "border:1px solid #f0c36d;background:#fff8e6"
             else "border:1px solid #e3e3e3;background:#fafafa"
    tagList(div(style = sprintf("margin:12px 0;padding:10px 12px;border-radius:8px;%s", style),
      strong(if (thin) "Close call - please confirm this is the right template"
             else "Template match"),
      # NAMES, and no scores. This line printed raw template ids and the engine's
      # match score -- "Matched anz_everyday_pdf. Nearest others: asb_everyday_pdf
      # (score 3)" -- on the customer-facing result page. The id is a maintainer's
      # handle and the score is an internal metric; neither is something the person
      # holding the statement can check against the paper in front of her, and both
      # are exactly what the charter's interface rule keeps off this screen. Which
      # ones nearly fitted is the useful half, and it is said in their names.
      p(class = "muted", if (nrow(others_df))
        sprintf("Read as %s. Others that nearly fitted: %s.", friendly_tpl(res$template_id),
                paste(names(tpl_choices(others_df$id)), collapse = ", "))
        else sprintf("Read as %s.", friendly_tpl(res$template_id))),
      # "Try the other one" must BE one click, not a trip to the toolkit. Opening
      # the toolkit is a heavy action (a form full of settings, a template to name
      # and save) for what is really the question "did it pick the right variant?".
      #
      # AND IT IS THE ONLY ACTION HERE NOW. "or open the toolkit with it" sat
      # beside this button as a second door into the same room: converting with
      # the other template and then pressing "Adjust how this was read" under the
      # downloads reaches the toolkit seeded with exactly that template, which is
      # where the link went. One door, and it is the one on every route.
      if (length(others)) tagList(
        selectInput("cv_cand_pick", "Wrong one? Try a different template:",
                    choices = tpl_choices(others), width = "100%"),
        actionButton("cv_cand_convert", "Convert with this one instead", class = "btn-primary"))))
  })

  # convert_with_template(tid) -- re-run THIS statement against one exact template.
  # Nothing is saved and no template is edited: it is the same conversion the
  # analyst just ran, with the guess replaced by their choice. Reconciliation runs
  # in full, so a wrong choice still comes back as needs_review rather than being
  # trusted because a human picked it. Shared by the tie chooser and the close-call
  # panel so "try the other one" behaves identically wherever it is offered.
  convert_with_template <- function(tid) {
    src <- cv_src(); req(src, tid, nzchar(tid))
    # same statement, same pickup - not a new upload, so the id goes THROUGH
    run_conversion(src$path, src$name, record = FALSE, force_tpl = tid,
                   upload_id = cv_upload_id())
    res <- cv_res()
    # The pickup record has to learn that this statement DID convert, or Admin
    # keeps asking someone to build a template for a file that already has one.
    # `uid` was a bare name with no binding anywhere in the server -- run_conversion
    # has a local of that name, this function does not -- so every use of "Convert
    # with this one instead" (the tie chooser AND the close-call panel) threw
    # "object 'uid' not found" after the conversion had already run.
    uid <- cv_upload_id()
    if (!is.na(uid) && (res$status %||% "") %in% c("ok", "needs_review"))
      safe(set_upload_status(uid, res$status,
        run_id = res$run_id %||% NA_character_,
        template = res$template_id %||% NA_character_,
        trust = res$trust$level %||% NA_character_,
        detail = sprintf("converted with %s, chosen by hand from the matching templates", tid),
        dir = UPLOADS_DIR))
    # The NAME, not the id: the picker she chose from offers names (tpl_choices),
    # and the "Read as:" chip that appears next to the new figures says the name
    # too, so the id here was the one word on the exchange that named nothing she
    # had seen. `detail` above is the upload record on disk -- a maintainer's file,
    # where the id is the useful handle -- so that one keeps it.
    showNotification(sprintf("Converted with %s.", friendly_tpl(tid)),
                     type = "message", duration = 5)
  }
  observeEvent(input$cv_tie_go, convert_with_template(input$cv_tie_pick))
  observeEvent(input$cv_cand_convert, convert_with_template(input$cv_cand_pick))

  # gl_build -- the guided template with the Simple-tab overrides applied.
  # meta_live = FALSE ISOLATES the three pure-metadata fields (template name / bank
  # / statement-type) so editing them doesn't invalidate the caller. The live
  # preview uses this: those three never appear in and never affect the parsed
  # transaction rows, yet each keystroke on them was forcing a full 3-page PDF
  # re-parse (~378 ms measured) -- a spinner-stall while typing a bank name.
  # `currency` STAYS live because it stamps the previewed currency column. Save
  # and the YAML editor use guided_live() (meta_live = TRUE), so every field is
  # still written exactly as typed.
  gl_build <- function(meta_live = TRUE) {
    g <- guided(); req(g)
    # "__report__" (the "none of these fit" option) is a no-override sentinel.
    no_sentinel <- function(v) if (identical(v, "__report__")) "" else v
    idv   <- if (meta_live) input$g_id   else isolate(input$g_id)
    bankv <- if (meta_live) input$g_bank else isolate(input$g_bank)
    typev <- if (meta_live) input$g_type else isolate(input$g_type)
    apply_overrides(g$tmpl, bankv, no_sentinel(input$g_date), no_sentinel(input$g_sign),
                    input$g_decimal, input$g_unsigned_default,
                    input$g_col_desc, input$g_col_ref, input$g_col_bal,
                    idv, typev, input$g_currency,
                    date_col = input$g_col_date, amount_col = input$g_col_amt,
                    keep_dateless = input$g_keep_dateless,
                    type_debit_value = input$g_type_debit,
                    type_credit_value = input$g_type_credit,
                    fingerprint_text = input$g_fp,
                    effective_from = input$g_eff_from, effective_to = input$g_eff_to)
  }
  guided_live <- reactive(gl_build(meta_live = TRUE))
  # Is a date column mapped at all? Wherever this format keeps it. Rows are found
  # by their date in both readers, so this is the one mapping whose absence is not
  # a "check your settings" problem but a hard stop.
  .has_date_col <- function(tmpl) {
    if (identical(tmpl$format %||% "delimited", "pdf")) !is.null(tmpl$table$columns$date)
    else nzchar(trimws(as.character(tmpl$columns$date$source %||% "")))
  }

  # THE SAVE NAME follows the bank AND the kind of statement. It used to be fixed
  # at draft time, so every layout one bank issues drafted the same name and the
  # second save overwrote the first -- and templates that cannot be told apart by
  # name are exactly the ones that tie in detection. Both answers are already on
  # screen, so the tool composes the name itself instead of asking again.
  # It stops following the moment she types a name of her own: g_id_auto holds
  # what the tool last put there, so "still ours" is a fact, not a guess.
  g_id_auto <- reactiveVal(NA_character_)
  observeEvent(list(input$g_bank, input$g_type), {
    g <- guided(); req(g)
    cur <- trimws(input$g_id %||% "")
    if (nzchar(cur) && !identical(cur, g_id_auto())) return()   # hers now
    sfx <- switch(g$tmpl$format %||% "delimited", pdf = "pdf", excel = "xlsx", "csv")
    # Same fallback draft_template() uses, so a bank still called "New bank" keeps
    # the filename-derived name it was drafted with rather than gaining a suffix.
    new <- .compose_id(input$g_bank, input$g_type, sfx,
                       tools::file_path_sans_ext(g$name %||% ""))
    g_id_auto(new)
    updateTextInput(session, "g_id", value = new)
  }, ignoreInit = TRUE)

  # CHOOSING THE PHRASE ADDS IT. There was an "Add it" button beside this picker,
  # and a picker that changes nothing until a second control is pressed is two
  # things to do for one decision -- so the choice IS the action. (The box below
  # is still typed into and edited freely; this only saves retyping a phrase the
  # drafter already found, which is where an exact-match fingerprint goes wrong.)
  observeEvent(input$g_fp_pick, {
    pick <- trimws(input$g_fp_pick %||% "")
    if (!nzchar(pick)) return()
    cur <- fingerprint_phrases(input$g_fp)
    if (!(pick %in% cur))
      updateTextAreaInput(session, "g_fp", value = paste(c(cur, pick), collapse = "\n"))
    updateSelectInput(session, "g_fp_pick", selected = "")
  }, ignoreInit = TRUE)
  # Live verdict on the phrase(s), in the same place they are typed: the SAME rule
  # the save uses (validate_template), so "Save" can no longer be the first time a
  # too-generic fingerprint is mentioned.
  output$g_fp_msg <- renderUI({
    g <- guided(); req(g); req(identical(g$tmpl$format, "pdf"))
    # NA (not "no problems") when the check itself could not run -- an "all clear"
    # we did not actually earn is the one answer we must never give.
    probs <- safe(validate_template(guided_live()), NA_character_)
    if (length(probs) == 1L && is.na(probs))
      return(span(class = "muted", style = "font-size:12.5px",
                  "Couldn't check the phrase just now - Save will tell you for certain."))
    fpp <- probs[grepl("fingerprint", probs, fixed = TRUE)]
    if (!length(fpp))
      return(span(class = "ok", style = "font-size:12.5px",
                  "This phrase will do - it is specific enough to identify this layout."))
    span(class = "bad", style = "font-size:12.5px", paste(fpp, collapse = " "))
  })

  # A window that runs backwards is named the moment it is picked, not first heard
  # about at Save. The pickers read their own dates back ("01 Jan 2020"), so there
  # is nothing else here to say: this is the only mistake still available.
  output$g_eff_msg <- renderUI({
    g <- guided()
    # A window the boxes could not show is stated, not silently emptied: without
    # this the template would open looking like "always" and save that way.
    #
    # ...but ONLY while the boxes are still empty. The message says "the boxes
    # above could not show it", and the moment the user picks a date that is no
    # longer true - and because it returned early, it also HID the backwards
    # warning for exactly the templates this branch was added for, so the only
    # mistake a pair of date pickers still allows went unnamed until Save.
    picked <- !is.null(.eff_date(input$g_eff_from)) || !is.null(.eff_date(input$g_eff_to))
    if (!is.null(g) && !.eff_stored_ok(g$tmpl) && !picked)
      return(span(class = "bad", style = "font-size:12.5px",
        paste("This template's saved validity window is not a date, so the boxes above could not show it.",
              "Pick the dates again, or leave them empty for always - saving replaces it either way.")))
    if (!.eff_backwards(input$g_eff_from, input$g_eff_to)) return(NULL)
    span(class = "bad", style = "font-size:12.5px", .EFF_BACKWARDS_MSG)
  })

  # Nudge the user to the "tell our team" box when they pick "none of these".
  observeEvent(list(input$g_date, input$g_sign), {
    if (identical(input$g_date, "__report__") || identical(input$g_sign, "__report__"))
      showNotification("Use the 'Tell our team' box below.",
                       type = "message", duration = 6)
  }, ignoreInit = TRUE)

  # Raise a template request (PII-safe: free-text + generic context only).
  observeEvent(input$g_req_send, {
    g <- guided(); req(g)
    detail <- trimws(input$g_req_detail %||% "")
    if (!nzchar(detail)) {
      output$g_req_msg <- renderUI(span(class = "bad", "Please describe the format first.")); return() }
    ctx <- list(
      file_ext      = tolower(tools::file_ext(g$name %||% "")),
      format        = g$tmpl$format %||% "delimited",
      bank          = input$g_bank %||% (g$tmpl$bank %||% ""),
      date_choice   = input$g_date %||% "",
      amount_choice = input$g_sign %||% "")
    id <- tryCatch(record_template_request(detail, ctx, requested_by = who_now(), dir = REQUESTS_DIR),
                   error = function(e) NULL)
    if (is.null(id)) {
      output$g_req_msg <- renderUI(span(class = "bad", "Couldn't save - try again.")); return() }
    updateTextAreaInput(session, "g_req_detail", value = "")
    output$g_req_msg <- renderUI(span(class = "ok",
      "Raised for review, with no statement contents."))
  })

  # Advanced tab: pull the current Basic settings into the YAML editor on demand.
  observeEvent(input$g_adv_load, {
    req(guided())
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "muted", "Loaded current settings into the editor."))
  })

  # Advanced tab: validate the edited YAML and adopt it as the working template.
  # On success we re-seed the Basic controls so their live overrides match (never
  # clobbering an advanced-only change). Fail loud on bad YAML / invalid template.
  observeEvent(input$g_adv_apply, {
    g <- guided(); req(g)
    parsed <- tryCatch(yaml::yaml.load(input$g_yaml %||% ""), error = function(e) e)
    if (inherits(parsed, "error") || !is.list(parsed)) {
      output$g_adv_msg <- renderUI(span(class = "bad",
        paste("YAML error:", if (inherits(parsed, "error")) conditionMessage(parsed) else "not a template")))
      return()
    }
    probs <- tryCatch(validate_template(parsed), error = function(e) conditionMessage(e))
    if (length(probs)) {
      output$g_adv_msg <- renderUI(span(class = "bad",
        paste("Not a valid template:", paste(probs, collapse = "; "))))
      return()
    }
    # The validity window has to survive the trip into the two date pickers below,
    # and a picker can only hold a date. Refuse a window written any other way HERE
    # -- otherwise it would simply not appear on screen and would then be saved
    # away, which is the template quietly losing a rule its author wrote.
    if (!.eff_stored_ok(parsed)) {
      output$g_adv_msg <- renderUI(span(class = "bad",
        "effective_from / effective_to must be dates (yyyy-mm-dd), or left out for always."))
      return()
    }
    g$tmpl <- parsed; guided(g)
    # A name written by hand in the YAML is hers, so the bank/kind composer leaves
    # it alone from here (an id is how a template is found again).
    g_id_auto(NA_character_)
    updateTextInput(session, "g_id", value = parsed$id %||% "")
    updateTextInput(session, "g_type", value = parsed$statement_type %||% "")
    updateTextInput(session, "g_currency", value = parsed$currency %||% "NZD")
    updateTextInput(session, "g_bank", value = parsed$bank %||% "")
    # Re-offer the date list WITH the applied format included, so an exotic
    # Advanced date_format is selectable and survives (not reverted by guided_live).
    updateSelectInput(session, "g_date", choices = guided_date_choices(gv_datefmt(parsed)),
                      selected = gv_datefmt(parsed))
    updateSelectInput(session, "g_sign", selected = gv_sign(parsed))
    updateSelectInput(session, "g_decimal", selected = parsed$decimal_mark %||% "auto")
    updateSelectInput(session, "g_unsigned_default", selected = parsed$unsigned_default %||% "debit")
    updateTextInput(session, "g_type_debit",  value = parsed$type_debit_value %||% "")
    updateTextInput(session, "g_type_credit", value = parsed$type_credit_value %||% "")
    .eff_set("g_eff_from", parsed$effective_from)
    .eff_set("g_eff_to",   parsed$effective_to)
    updateSelectInput(session, "g_col_date", selected = parsed$columns$date$source %||% "")
    updateSelectInput(session, "g_col_amt",  selected = parsed$columns$amount$source %||% "")
    updateSelectInput(session, "g_col_desc", selected = parsed$columns$description$source %||% "")
    updateSelectInput(session, "g_col_ref",  selected = parsed$columns$reference$source %||% "")
    updateSelectInput(session, "g_col_bal",  selected = parsed$columns$balance$source %||% "")
    # Keep the Simple-tab phrase box in step with a fingerprint edited in the YAML,
    # or the next Simple-tab keystroke would silently put the old phrases back.
    if (identical(parsed$format, "pdf"))
      updateTextAreaInput(session, "g_fp",
        value = paste(unlist(parsed$fingerprint$page_contains_all %||% list()), collapse = "\n"))
    output$g_adv_msg <- renderUI(span(class = "ok", "Applied - preview updated below."))
  })

  # ---- Toolkit: visual PDF column editor --------------------------------------
  # Renders the chosen page and draws the working template's column bands on it;
  # a drawn box assigns/updates a column, keeping the YAML editor and preview in
  # sync so PDF setup is fully visual and in one place.
  # Sample rows of a delimited file, shown on the left of the toolkit so the user
  # can see the columns while answering bank / date / amount.
  output$g_raw_sample <- renderText({
    g <- guided(); req(g); req(!identical(g$tmpl$format, "pdf"))
    # Excel is binary - show the cleaned table (right sheet, preamble skipped),
    # not raw bytes. Delimited files show their first lines verbatim.
    if (identical(g$tmpl$format, "excel")) {
      t <- tryCatch(read_input(g$path)$table, error = function(e) NULL)
      if (is.null(t) || !nrow(t)) return("(couldn't read the workbook)")
      return(paste(utils::capture.output(print(utils::head(t, 25), row.names = FALSE)),
                   collapse = "\n"))
    }
    lines <- tryCatch(readLines(g$path, n = 40, warn = FALSE), error = function(e) character(0))
    if (!length(lines)) "(couldn't read the file)" else paste(lines, collapse = "\n")
  })

  g_pdf_render <- reactive({
    g <- guided(); req(g); req(identical(g$tmpl$format, "pdf"))
    # Only the file + page matter for the bitmap (bands are overlays drawn in the
    # plot), and the render is cached, so assigning a box no longer re-renders it.
    render_page_view(g$path, .clamp_page(input$g_pdf_page, g$n_pages %||% NA_integer_), 100)
  })
  # THE BAND FRAME (R/parse_pdf_table.R) is the one coordinate space every stored
  # band lives in: the size of the page it was drawn on, recorded as ref_width /
  # ref_height. This plot is the page at its OWN size. So DIVIDE a band to draw it
  # here, MULTIPLY a drawn box to store it -- the exact mirror of what the reader
  # does to the words, which is the only reason the box she draws is the box the
  # reader matches. Drawn or stored raw, every page that is not the frame's size
  # is displaced, and a displaced band is indistinguishable from an untouched
  # default one: "the boxes I drew came back", and the narrowest money column
  # (usually credit) reads nothing. A page the frame's size scales by exactly 1.
  g_band_scale <- function(r) pdf_band_frame_scale(pdf_band_frame(guided()$tmpl), r$w, r$h)
  output$g_pdf_plot <- renderPlot({
    r <- g_pdf_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    s <- g_band_scale(r)
    cols <- guided()$tmpl$table$columns %||% list()
    if (length(cols)) {
      pal <- grDevices::hcl(seq(0, 300, length.out = length(cols)), 70, 55)
      for (i in seq_along(cols)) {
        b <- cols[[i]]; if (is.null(b$x_min) || is.null(b$x_max)) next
        rect(b$x_min / s[1], 0, b$x_max / s[1], r$h, border = pal[i], lwd = 2)
        text(mean(c(b$x_min, b$x_max)) / s[1], 16, names(cols)[i], col = pal[i], font = 2)
      }
    }
    # pinned header-value boxes (metadata_regions) for the CURRENT page, in orange
    mr <- guided()$tmpl$table$metadata_regions %||% list()
    pg <- r$pg   # the page actually drawn, so the overlay can never sit on another
    for (nm in names(mr)) { b <- mr[[nm]]
      if (is.null(b$x_min) || is.null(b$x_max)) next
      if (!identical(as.integer(b$page %||% 1), pg)) next
      y0 <- (b$y_min %||% 0) / s[2]; y1 <- if (is.null(b$y_max)) r$h else b$y_max / s[2]
      rect(b$x_min / s[1], y0, b$x_max / s[1], y1, border = PALETTE$meta, lwd = 2)
      text(b$x_min / s[1], y0, nm, col = PALETTE$meta, font = 2, cex = 0.85, pos = 3, offset = 0.2)
    }
    # Live feedback: the box being drawn is shown as the FULL-HEIGHT column it will
    # become (a translucent band over the whole page height), so it is obvious the
    # box's top/bottom are ignored and only its left-right span defines the column.
    br <- input$g_pdf_brush
    if (!is.null(br) && is.finite(br$xmin) && is.finite(br$xmax)) {
      rect(br$xmin, 0, br$xmax, r$h, col = "#1a73e820", border = "#1a73e8", lty = 2, lwd = 2)
      text(mean(c(br$xmin, br$xmax)), r$h * 0.5, "this whole column",
           col = "#1a73e8", font = 2, cex = 0.95, srt = 90)
    }
  })
  .CANON_PDF_COLS <- c("date", "description", "amount", "balance", "debit", "credit",
                       "particulars", "code", "reference", "other_party", "type")
  # A custom name (typed) becomes an EXTRA column (output as x.<name>); a canonical
  # name is a normal table column. Which "slot" a field lives in for assign/remove.
  .pdf_field_ref <- function(f) if (f %in% .CANON_PDF_COLS) "columns" else "extras"
  .pdf_all_bands <- function(tbl) c(tbl$columns %||% list(), tbl$extras %||% list())
  .pdf_resize_region <- function(g) {
    xs <- unlist(lapply(.pdf_all_bands(g$tmpl$table), function(c) c(c$x_min, c$x_max)))
    reg <- g$tmpl$table$region %||% list()
    if (length(xs)) { reg$x_min <- min(xs) - 5; reg$x_max <- max(xs) + 5 }
    else { reg$x_min <- NULL; reg$x_max <- NULL }   # no bands left -> drop x-scope, keep y
    g$tmpl$table$region <- if (length(reg)) reg else NULL
    g
  }
  .pdf_chosen_field <- function() {
    # A hidden control must never override a visible one. The custom name lives
    # behind the disclosure; while that is closed the analyst cannot see it, so it
    # cannot be what she meant - the dropdown in front of her is.
    custom <- trimws(input$g_pdf_custom %||% "")
    if (isTRUE(g_more_open()) && nzchar(custom))
      gsub("[^A-Za-z0-9_]+", "_", custom) else input$g_pdf_field
  }
  # One dropdown answers both kinds of box, so the choice carries which kind it is:
  # "meta:" marks a one-off header value, anything else is a column. A typed custom
  # name can never collide -- .pdf_chosen_field strips the colon to an underscore.
  .meta_field <- function(f) if (grepl("^meta:", f %||% "")) sub("^meta:", "", f) else NA_character_
  # Say it BESIDE THE BUTTON, and keep saying it on Advanced. Both of these
  # confirmations only ever went to g_adv_msg, which is on the other tab.
  .g_box_note <- function(msg, ok = TRUE) {
    ui <- renderUI(span(class = if (ok) "ok" else "bad", msg))
    output$g_pdf_msg <- ui
    output$g_adv_msg <- ui
  }

  observeEvent(input$g_pdf_assign, {
    g <- guided(); req(g); br <- input$g_pdf_brush
    if (is.null(br)) { showNotification("Draw a box on the page first.", type = "warning"); return() }
    f <- .pdf_chosen_field()
    if (!nzchar(trimws(f %||% ""))) {
      showNotification("Say what you boxed first, then Assign it.", type = "warning"); return() }
    # A one-off header value: a specific box (x AND y) around ONE value that isn't
    # on every row -- a balance, the statement period, an account detail -- read
    # straight from that spot when the automatic reader can't label it. It never
    # touches the transaction region, which is why it is not a column band.
    # Into the band frame before it is stored (see g_band_scale): the brush reports
    # this page's own points, the template holds the frame's.
    r <- g_pdf_render(); s <- if (is.null(r)) c(1, 1) else g_band_scale(r)
    mf <- .meta_field(f)
    if (!is.na(mf)) {
      pg <- .clamp_page(input$g_pdf_page, g$n_pages %||% NA_integer_)
      g$tmpl$table$metadata_regions[[mf]] <- list(page = pg,
        x_min = round(br$xmin * s[1]), x_max = round(br$xmax * s[1]),
        y_min = round(br$ymin * s[2]), y_max = round(br$ymax * s[2]))
      guided(g)
      updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
      .g_box_note(sprintf("Pinned '%s' to the box you drew on page %d.", mf, pg))
      return()
    }
    slot <- .pdf_field_ref(f)
    g$tmpl$table[[slot]][[f]] <- list(x_min = round(br$xmin * s[1]), x_max = round(br$xmax * s[1]))
    # Mapping a money-in / money-out band means this is a separate debit/credit
    # statement: switch the amount style to match, so saving never demands a single
    # 'amount' column (the reported "amount is still required even when debit and
    # credit are present"). The dropdown is the source of truth guided_live() reads,
    # so update it too.
    switched <- FALSE
    if (f %in% c("debit", "credit") && !identical(g$tmpl$table$amount_sign, "debit_credit_cols")) {
      g$tmpl$table$amount_sign <- "debit_credit_cols"
      updateSelectInput(session, "g_sign", selected = "debit_credit_cols")
      switched <- TRUE
    }
    g <- .pdf_resize_region(g); guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    .g_box_note(sprintf("Set the '%s' column%s.%s Page and preview updated below.", f,
                        if (identical(slot, "extras")) " (custom / extra)" else "",
                        if (switched) " Amount style set to separate money-in / money-out." else ""))
    if (isTRUE(switched))
      showNotification(paste("Amount style set to separate money-in / money-out, because you drew",
                             "both a money-out and a money-in column."),
                       type = "message", duration = 7)
  })
  # Delete a column band the auto-setup got wrong (a column that isn't on this
  # statement), or unpin a header value. Recomputes the table region from whatever
  # bands remain.
  observeEvent(input$g_pdf_remove, {
    g <- guided(); req(g); f <- .pdf_chosen_field()
    if (!nzchar(trimws(f %||% ""))) {
      showNotification("Pick what to remove first.", type = "warning"); return() }
    mf <- .meta_field(f)
    if (!is.na(mf)) {
      if (is.null(g$tmpl$table$metadata_regions[[mf]])) {
        showNotification(sprintf("There's no pinned box for '%s' to remove.", mf), type = "warning"); return() }
      g$tmpl$table$metadata_regions[[mf]] <- NULL
      if (!length(g$tmpl$table$metadata_regions)) g$tmpl$table$metadata_regions <- NULL
      guided(g)
      updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
      .g_box_note(sprintf("Removed the pinned box for '%s'.", mf))
      return()
    }
    slot <- if (!is.null(g$tmpl$table$columns[[f]])) "columns"
            else if (!is.null(g$tmpl$table$extras[[f]])) "extras" else NA
    if (is.na(slot)) {
      showNotification(sprintf("There's no '%s' column to remove.", f), type = "warning"); return() }
    g$tmpl$table[[slot]][[f]] <- NULL
    g <- .pdf_resize_region(g); guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    .g_box_note(sprintf("Removed the '%s' column. Page and preview updated below.", f))
  })

  # ONE parse per change, shared by the preview table + the status line (each used
  # to call draft_preview independently, doubling the parse on every box assignment).
  # The live preview parses only the FIRST FEW PDF pages -- enough to confirm the
  # columns read correctly -- so a big statement previews in a fraction of the time
  # (the full convert on Save still parses every page).
  # How many pages the preview reads, and how many rows it shows. Deliberately NOT
  # the whole document: a 46-page statement would re-parse on every keystroke in
  # the toolkit. Both numbers are QUOTED ON SCREEN, so they live here once rather
  # than as a literal in one place and a sentence in another that drifts from it.
  PREVIEW_PAGES <- 3L
  PREVIEW_ROWS  <- 12L
  g_preview_tx <- reactive({ g <- guided(); req(g)
    # Isolated build: editing template name / bank / statement-type won't re-parse
    # the statement (they don't affect the rows); currency and every column /
    # date / amount setting stay live, so the preview still updates on those.
    draft_preview(g$path, gl_build(meta_live = FALSE), preview_pages = PREVIEW_PAGES) })
  output$g_preview <- renderDT({
    tx <- g_preview_tx(); req(!is.null(tx))
    tx <- utils::head(tx, PREVIEW_ROWS)
    # Show every field that was actually read -- including reference, and the
    # separate debit / credit columns when the statement splits them -- so the
    # user can confirm each mapped column, not just date/description/amount.
    show <- setdiff(.cols_with_data(tx), "row_id")
    lead <- intersect(c("date", "description", "amount", "debit", "credit", "direction",
                        "balance", "reference", "particulars", "code", "other_party", "type"), show)
    show <- c(lead, setdiff(show, lead))
    if (!length(show)) show <- names(tx)
    tx <- tx[, show, drop = FALSE]
    # Same as the Convert table: the header was mapped, the cells were not, so a
    # drafted template previewed nineteen rows of `date_year_inferred`.
    if ("flags" %in% names(tx)) tx$flags <- plain_flags(tx$flags)
    datatable(tx, rownames = FALSE, colnames = cv_friendly_cols(show),
              options = list(dom = "t", pageLength = PREVIEW_ROWS, scrollX = TRUE))
  })
  # g_preview_cov -- row coverage for EXACTLY the pages the preview parsed.
  #
  # row_coverage() reads the whole document; draft_preview() deliberately reads
  # only the first few pages so a 46-page statement does not re-parse on every
  # keystroke. Handing the two different page sets would put "19 rows read" beside
  # a skip count from pages the preview never looked at, so the input is trimmed
  # the same way draft_preview trims it (R/draft.R, .subinput_pages) and the two
  # numbers describe the same paper. read_input is content-cached, so this costs
  # the layout pass and not a re-read.
  g_preview_cov <- reactive({
    g <- guided(); req(g)
    t <- gl_build(meta_live = FALSE)
    if (!identical(t$format %||% "", "pdf")) return(NULL)
    inp <- tryCatch(read_input(g$path), error = function(e) NULL)
    if (is.null(inp)) return(NULL)
    np <- length(inp$pages %||% inp$words %||% list())
    if (np > PREVIEW_PAGES) inp <- tryCatch(.subinput_pages(inp, seq_len(PREVIEW_PAGES)),
                                            error = function(e) inp)
    tryCatch(row_coverage(inp, t), error = function(e) NULL)
  })
  # preview_doubts(tx, cov) -- WHAT THE TOOL ALREADY KNOWS THAT ARGUES AGAINST THE
  # TICK. This is the screen on which a template is decided and SAVED, and its
  # verdict branched on nothing but `n > 0`: asb.pdf drew a green tick over "1
  # transaction row read - ... If they are right, click Save template" while
  # row_coverage() on that same drafted template already knew page 2 kept nothing;
  # d5_sample.pdf drew one over "19 transaction rows read", and the very next
  # screen after saving said "11 discontinuity(ies)". The Convert page asks both
  # questions. The toolkit, which decides the template, did not.
  #
  # Both answers come from the engine, not from a rule invented here: the running
  # balance from the same .kpi_running_balance_continuity() the Checks table
  # prints, and the page evidence from row_coverage()'s own diagnosis. A tick now
  # means the page was read, not that a number was greater than zero.
  preview_doubts <- function(tx, cov) {
    out <- character(0)
    n <- if (is.null(tx)) 0L else nrow(tx)
    if (n >= 2L && "balance" %in% names(tx)) {
      k <- tryCatch(.kpi_running_balance_continuity(tx, n), error = function(e) NULL)
      bad <- suppressWarnings(as.integer((k$actual %||% NA)[1]))
      # A break between two printed balances is what a DROPPED ROW looks like from
      # here: the balances are the statement's own arithmetic, so a step the rows
      # do not account for is a row the template did not keep.
      if (!is.null(k) && identical((k$status %||% "")[1], "fail") && isTRUE(bad > 0L))
        out <- c(out, sprintf(paste("the balances do not follow in %d place(s):",
                                    "rows are probably being skipped"), bad))
    }
    if (isTRUE(cov$applicable)) {
      skipped <- suppressWarnings(as.integer(cov$actionable_skips_total %||% 0L))
      # A PAGE THAT KEPT NOTHING IS NOT BY ITSELF EVIDENCE OF ANYTHING, and the
      # first version of this fired on one. Measured on the bundled specimen: page
      # 1 kept 0 rows and skipped 0 candidates -- it is the cover page, nothing was
      # lost, and the screen said "but not all of the page" over a perfect draft.
      # A tool that cries wolf on the common case teaches people to ignore it,
      # which is the failure this whole item is about.
      #
      # What IS evidence: a line that LOOKED like a transaction and was not kept
      # (actionable_skips_total), or a page the parser had to rescale that then
      # kept nothing -- a page-scale mismatch loses rows without leaving candidates
      # behind to count.
      empty <- length(cov$empty_pages %||% integer(0)) > 0L
      if (isTRUE(skipped > 0L) || (empty && isTRUE(cov$any_page_rescaled)))
        # Its own words. row_coverage() writes one sentence per situation and it
        # is better than anything restated here.
        out <- c(out, trimws(as.character(cov$diagnosis %||% "")[1]))
    }
    out[nzchar(out)]
  }
  # The preview verdict. This is where a template gets abandoned: a grey line of
  # monospace saying "no rows detected" reads as a dead end, so it now says which
  # setting to reach for -- and, for a PDF, that the preview only reads the first
  # few pages, which is why a statement whose table starts later looks empty here.
  output$g_status <- renderUI({
    g <- guided(); req(g)
    tx <- g_preview_tx()
    n <- if (is.null(tx)) 0L else nrow(tx)
    is_pdf <- identical(g$tmpl$format, "pdf")
    # Is this a PARTIAL read? Only when the document has more pages than the
    # preview looks at. A 2-page statement is read whole, and "the first 3 pages"
    # there would be a caveat about nothing.
    np <- suppressWarnings(as.integer(g$n_pages %||% NA))
    partial <- is_pdf && !is.na(np) && np > PREVIEW_PAGES
    # The four branches below shared two sentences between them, written out in
    # full each time. One copy each: what to look at when there ARE rows, and where
    # the two settings live when there are none.
    check_them <- "Check the dates, descriptions and amounts below against your statement."
    two_settings <- "If so, open \"Show the settings for this statement\" and check the date format and the amount style."
    if (n > 0L) {
      doubts <- preview_doubts(tx, g_preview_cov())
      ok <- !length(doubts)
      return(div(class = if (ok) "verdict verdict-high" else "verdict verdict-medium",
                 style = "margin:2px 0 12px",
        div(class = "verdict-ico", if (ok) "\u2713" else "!"),
        div(style = "flex:1;min-width:0",
          div(class = "verdict-title",
              sprintf("%s%s", if (partial)
                sprintf("%d transaction row%s read from the first %d of %d pages",
                        n, if (n == 1L) "" else "s", PREVIEW_PAGES, np)
                else sprintf("%d transaction row%s read", n, if (n == 1L) "" else "s"),
                if (ok) "" else " - but not all of the page")),
          # The doubts FIRST: they are the reason not to press Save, and they went
          # above the "if they are right, click Save template" line that used to be
          # the only thing here.
          if (!ok) tags$ul(class = "verdict-body", style = "margin:4px 0 0 18px;padding:0",
                           lapply(doubts, tags$li)),
          p(class = "verdict-body", style = "margin:0",
            paste(check_them, if (partial)
              "Converting reads every page, so expect a bigger number then."
              else if (ok) "If they are right, click Save template."
              else "Fixing the column boxes or the date format above usually recovers the missing rows."),
          if (n > PREVIEW_ROWS)
            span(class = "muted", sprintf(" The table shows the first %d.", PREVIEW_ROWS))))))
    }
    # THE DATE COLUMN IS NOT OPTIONAL, and its absence is not a mis-drawn box.
    # Remove it and the panel below handed out advice about widening a band and
    # checking the date format - advice for a template that HAS a date column -
    # while the actual cause was one click old and unmentioned. Rows are found by
    # their date, so with no date column nothing can ever be read, whatever else
    # is set.
    if (!.has_date_col(gl_build(meta_live = FALSE)))
      return(div(class = "verdict verdict-low", style = "margin:2px 0 12px",
        div(class = "verdict-ico", "!"),
        div(style = "flex:1;min-width:0",
          div(class = "verdict-title", "There is no date column, so no rows can be read"),
          p(class = "verdict-body", style = "margin:0",
            if (is_pdf)
              "Draw a box over the dates on the page, choose \"date\" and click Assign it. Every row this tool reads is found by its date - nothing else will bring the rows back."
            else
              "Point the Date picker above at the column holding the dates. Every row this tool reads is found by its date - nothing else will bring the rows back."))))
    div(class = "verdict verdict-medium", style = "margin:2px 0 12px",
      div(class = "verdict-ico", "!"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", "No transaction rows read yet"),
        p(class = "verdict-body", style = "margin:0 0 4px",
          paste(if (is_pdf) "Rows are found by their date. Does your box cover the dates on the page?"
                else "Are the Date and Amount pickers above on the right columns?",
                two_settings)),
        if (is_pdf) p(class = "muted", style = "margin:0",
          "This preview reads only the first few pages - if the transactions start later, that is why nothing shows here.")))
  })
  observeEvent(input$g_save, {
    g <- guided(); req(g)
    # A validity window that runs backwards is refused BEFORE anything is written:
    # it would save a template that applies to no statement ever printed, and
    # nothing downstream would say so. The disclosure it lives behind is opened, so
    # the fix is on screen rather than somewhere to go and find.
    if (.eff_backwards(input$g_eff_from, input$g_eff_to)) {
      g_more_open(TRUE)
      showNotification(HTML(paste0("<b>Couldn't save.</b> ",
        htmltools::htmlEscape(.EFF_BACKWARDS_MSG),
        "<br>Opened on the right, under <b>When this layout applies</b>.")),
        type = "error", duration = 12)
      return()
    }
    tmpl <- guided_live()
    # If we opened a tested (default) template to refine it, saving under the same
    # id would be shadowed - curated defaults win on an id clash. Give the
    # customised copy a distinct id so the accountant's fix actually takes effect.
    # Record WHAT she was fixing, not just that the copy is different. Without this
    # her correction was outvoted by the very template she opened to correct: both
    # carry the same fingerprint, so they tie on every statement, and the tie-break
    # prefers the tested one. She then had no way to win except by making her
    # fingerprint MORE specific, which is not why she lost, and the tool told her
    # so. `refines` says "this exists to replace that one", and detection honours it.
    if (!is.null(g$default_ids) && (tmpl$id %||% "") %in% g$default_ids) {
      tmpl$refines <- tmpl$id
      tmpl$id <- paste0(tmpl$id, "_custom")
    }
    # Surface the ACTUAL reason a save fails (validation problems name the field,
    # never any statement content) instead of a dead-end generic toast.
    err <- tryCatch({ save_user_template(tmpl, USER_TEMPLATES_DIR); NULL },
                    error = function(e) conditionMessage(e))
    if (is.null(err)) {
      tpl_bump(isolate(tpl_bump()) + 1); removeModal()
      # mark this upload as taught, so it drops off the "needs pickup" list
      if (!is.na(cv_upload_id()))
        safe(set_upload_status(cv_upload_id(), "wizard_saved",
          template = tmpl$id %||% NA_character_, dir = UPLOADS_DIR))
      saved_id <- tmpl$id %||% NA_character_
      # THE NAME SHE GAVE IT, NEVER THE ID THE FILE IS SAVED UNDER. This toast read
      # `Saved "newbank_everyday_everyday_csv".` and then, in R/util.R's sentence
      # underneath, printed the same id a second time -- an engine handle twice on
      # one confirmation, on the screen a non-technical analyst uses to add a bank.
      # friendly_tpl is the app's single answer to what a template is CALLED, and
      # the bump above has already put the new template in the set it reads; it
      # falls back to the id when it cannot resolve a name, and here there is
      # always something better to say, because the template itself is in hand.
      saved_name <- friendly_tpl(saved_id)
      if (is.na(saved_name) || identical(saved_name, saved_id))
        saved_name <- .tpl_label(tmpl$bank, tmpl$statement_type)
      if (is.na(saved_name) || !nzchar(saved_name)) saved_name <- "your new template"
      gp <- g$path; gn <- g$name %||% (if (!is.null(gp)) basename(gp) else NA_character_)
      if (!is.null(gp) && file.exists(gp) && !is.na(saved_id) && nzchar(saved_id)) {
        updateTabsetPanel(session, "main_tabs", selected = "Convert")
        # PROVE IT. The old flow re-converted with the new template FORCED by id,
        # which convert.R short-circuits to "template chosen by the user" -- so the one
        # moment the app could have shown that the new template auto-detects was the
        # one moment it skipped detection entirely. That is how templates with
        # fingerprints that could never match shipped under a green suite. Run a
        # REAL detection pass over the whole template set first, say in plain words
        # what it means, and only fall back to forcing when detection did NOT pick
        # this template (so she still sees her template's output, correctly labelled).
        # THE SET IS PASSED IN, or the sentence falls back to the id. This is the
        # caller's half of the same fix as saved_name above: recognition_summary
        # names templates by their display name and can only do that if it is
        # handed the set they live in -- without it, `.saved_name` has nothing to
        # look the id up in and returns the id, which is what this toast printed
        # twice. The detection pass already loads exactly that set, so there is
        # one load and one answer, not two that can disagree.
        recog <- safe({
          tset <- load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR)
          recognition_summary(detect_statement(read_input(gp), tset), saved_id,
                              templates = tset)
        }, NULL)
        recog <- recog %||% recognition_summary(NULL, saved_id)
        run_conversion(gp, gn, record = FALSE,
                       force_tpl = if (isTRUE(recog$ok)) NULL else saved_id,
                       include_user = TRUE)
        showNotification(
          HTML(paste0("<b>Saved \"", htmltools::htmlEscape(saved_name), "\".</b><br>",
                      "<b>", htmltools::htmlEscape(recog$headline), "</b><br>",
                      htmltools::htmlEscape(recog$detail))),
          type = if (isTRUE(recog$ok)) "message" else "warning",
          duration = if (isTRUE(recog$ok)) 10 else NULL)
      } else {
        showNotification(sprintf("Saved as your template \"%s\". Click Convert again to run this statement with it.",
                                 saved_name),
                         type = "message", duration = 8)
      }
    } else {
      # A refused save is nearly always the identifying phrase, and its box now sits
      # behind the disclosure. OPEN it rather than naming a place she then has to go
      # and find -- the tool knows where the fix is, so it should not make her look.
      g_more_open(TRUE)
      showNotification(HTML(paste0("<b>Couldn't save.</b> ", htmltools::htmlEscape(err),
        "<br>Opened on the right - or use <b>Advanced</b> for the whole template as text.")),
        type = "error", duration = 12)
    }
  })

  # ---- Admin: the health picture, from the logs -----------------------------
  #
  # BOUNDED, AND IT SAYS SO. This used to be read_runs_all(), which parses every
  # line of every archive file one record at a time -- 10.5 seconds on 20,000
  # archived rows, measured, in the single Shiny process the whole team shares, so
  # opening this tab froze everybody's browser, and got slower every week. It also
  # read only the LIVE feedback folder, so every rating older than the rollup
  # window had already vanished from the one screen that is supposed to hold all
  # of them.
  #
  # .adm_history() fixes both: newest-first across the live folder AND the
  # archive, capped, with the count of what it did not read carried on the frame
  # so adm_history_note can say it out loud. Nothing is deleted or hidden.
  adm_data <- reactiveVal(NULL)
  load_admin <- function() adm_data(list(
    runs = tryCatch(.adm_history(LOGDIR, "runs"), error = function(e) data.frame()),
    fb   = tryCatch(.adm_history(LOGDIR, "feedback"), error = function(e) data.frame())))
  # Admin dashboard data (run logs, feedback) is loaded ONLY for an authenticated
  # admin session, so a non-admin client can never pull it by marking a hidden
  # output visible -- every admin output does req(adm_data()), which stays NULL
  # (and therefore blank) without a load.
  observeEvent(input$adm_refresh, { req(admin_ok()); load_admin() })
  observe({ req(admin_ok()); if (is.null(adm_data())) load_admin() })
  # A PARTIAL PICTURE THAT DOES NOT ADMIT IT IS PARTIAL IS THE THING THE CHARTER
  # FORBIDS. Silent when everything was read, which is the ordinary case.
  output$adm_history_note <- renderUI({
    d <- adm_data(); req(d)
    n <- function(x) { k <- attr(x, "kept_of"); if (is.null(k)) c(read = 0L, total = 0L) else k }
    r <- n(d$runs); f <- n(d$fb)
    short <- c(if (r[["total"]] > r[["read"]])
                 sprintf("the newest %s conversions of %s", format(r[["read"]], big.mark = ","),
                         format(r[["total"]], big.mark = ",")),
               if (f[["total"]] > f[["read"]])
                 sprintf("the newest %s ratings of %s", format(f[["read"]], big.mark = ","),
                         format(f[["total"]], big.mark = ",")))
    if (!length(short)) return(NULL)
    p(class = "muted", style = "font-size:12px",
      sprintf("Drawn from %s - the older records are all still kept in logs/archive/, they are just too slow to read on every visit.",
              paste(short, collapse = " and ")))
  })

  output$adm_overview <- renderDT({
    d <- adm_data(); req(d)
    datatable(runs_overview(d$runs), rownames = FALSE, options = list(dom = "t"))
  })
  output$adm_status_plot <- renderPlot({
    d <- adm_data(); req(d); ov <- runs_overview(d$runs); if (!nrow(ov)) return(NULL)
    cols <- c(ok = PALETTE$ok, needs_review = "#e3b341", unsupported = PALETTE$bad,
              failed = "#7d1a1a")[ov$status]
    cols[is.na(cols)] <- "#888888"   # three-digit hex throws in base R
    op <- par(mar = c(5, 4, 1, 1)); on.exit(par(op))
    barplot(setNames(ov$n, ov$status), col = cols, las = 2, ylab = "conversions")
  })
  # THE GAPS ARE THE `unsupported` RUNS ONLY. unsupported_clusters() takes the
  # FAILED ones too, and a failed run is not a gap: the file never reached a
  # template, so it lands here with no layout, no closest template and no reason --
  # three blank cells under a heading promising a layout to build. One such run
  # (an unreadable moment on a file that converts cleanly every day) left that file
  # on the "can't read yet" list permanently. They are shown as what they are, in
  # adm_unreadable below.
  .GAP_COLS <- c("count", "layout", "closest_template", "why", "last_seen", "example_file")
  output$adm_gaps <- renderDT({
    d <- adm_data(); req(d)
    runs <- d$runs
    if (nrow(runs) && "status" %in% names(runs))
      runs <- runs[as.character(runs$status) %in% "unsupported", , drop = FALSE]
    g <- unsupported_clusters(runs)
    # A blank cell reads as "the layout is empty" / "nothing was close". Both are
    # facts the log simply does not carry for these runs, so say that instead.
    said <- function(v) { v <- as.character(v); v[is.na(v) | !nzchar(trimws(v))] <- "not recorded"; v }
    g <- g[, .GAP_COLS, drop = FALSE]
    for (nm in c("layout", "closest_template", "why", "example_file")) g[[nm]] <- said(g[[nm]])
    datatable(g, rownames = FALSE,
              options = dt_none_opts("No statement has come back 'no template for this yet'.",
                                     pageLength = 10, scrollX = TRUE)) |>
      formatStyle("count", fontWeight = "bold")
  })
  output$adm_unreadable <- renderDT({
    d <- adm_data(); req(d)
    runs <- d$runs
    cols <- intersect(c("ts", "source_file", "message"), names(runs))
    if (!nrow(runs) || !("status" %in% names(runs)) || !length(cols))
      return(stats::setNames(data.frame(matrix(character(0), 0, 3)), c("ts", "source_file", "message")))
    f <- runs[as.character(runs$status) %in% "failed", cols, drop = FALSE]
    f[order(as.character(f$ts), decreasing = TRUE), , drop = FALSE]
  }, options = dt_none_opts("Every run reached a template - nothing failed to open.",
                            pageLength = 5, dom = "tip"), rownames = FALSE)
  output$adm_usage <- renderDT({
    d <- adm_data(); req(d)
    u <- template_usage(d$runs, d$fb)
    datatable(u, rownames = FALSE,
              options = dt_none_opts("No conversion has matched a template yet.",
                                     dom = "t", pageLength = 20))
  })
  # HEALTH IS DEFINED PER ROUTE NOW (run_healthy(), R/analytics.R), so this table
  # no longer renders a row of NAs for any template that is not a bank statement.
  # The screen's half of that is saying WHICH route each row is about: a drop from
  # 100% to 40% means "it stopped reconciling" on one row and "its tables stopped
  # being found" on the next, and those are different jobs for the maintainer.
  output$adm_drift <- renderDT({
    d <- adm_data(); req(d)
    dr <- template_drift(d$runs)
    if (nrow(dr)) {
      lib <- adm_lib()
      dr <- cbind(route = vapply(as.character(dr$template), function(id) {
        t <- lib[[id]]
        .adm_route(if (is.null(t)) NA_character_ else safe(template_kind(t), NA_character_))
      }, character(1), USE.NAMES = FALSE), dr)
    }
    tbl <- datatable(dr, rownames = FALSE,
                     options = dt_none_opts("No template has started failing - good.", dom = "t"))
    if (nrow(dr)) tbl <- formatStyle(tbl, "drop", fontWeight = "bold", color = PALETTE$bad)
    tbl
  })
  # ONE CALL TIDIES THE THREE FOLDERS THAT GROW. rollup_logs() defaults to every
  # one of them (LOG_ROLLUP_SUBDIRS) and trims logs\errors.log while it is there;
  # this used to name "runs" and "feedback" by hand, so logs\feed\ -- which gains a
  # file for EVERY conversion, at exactly the rate logs\runs\ does -- was never
  # archived at all. archive_feed() beside it MOVES feed rows older than the
  # stated period out of the folder Qlik's wildcard reads. Neither deletes
  # anything, and the message says everything that happened, including the trim.
  observeEvent(input$adm_rollup, {
    req(admin_ok())
    r <- tryCatch(rollup_logs(LOGDIR, keep_days = LOG_KEEP_DAYS), error = function(e) NULL)
    a <- tryCatch(archive_feed(CONFIG, keep_days = CONFIG$feed$keep_days), error = function(e) NULL)
    load_admin()
    output$adm_rollup_msg <- renderUI(span(class = "ok", paste(
      sprintf("Archived %d old log file(s); %d kept.", r$archived %||% 0, r$kept %||% 0),
      if (isTRUE(r$trimmed)) "The error log was trimmed back to its recent lines." else "",
      if (!is.null(a) && (a$archived %||% 0) > 0)
        sprintf("Moved %d old feed file(s) out of the folder the dashboards read.", a$archived) else "",
      "Nothing was deleted - it is all in logs/archive/.")))
  })
  # WHAT WILL BE DELETED, COUNTED BEFORE IT IS. purge_uploads() has no dry-run, so
  # the count is taken exactly the way it takes it: the saved statement's own
  # mtime, never record.json's (every status change rewrites that, which would
  # keep resetting the clock on a file nobody has touched).
  .uploads_due <- function(keep_days) {
    kd <- suppressWarnings(as.numeric(keep_days %||% NA)[1])
    if (!is.finite(kd) || kd <= 0) return(0L)
    cutoff <- as.numeric(Sys.time()) - kd * 86400
    recs <- Sys.glob(file.path(UPLOADS_DIR, "*", "record.json"))
    sum(vapply(recs, function(rp) {
      files <- setdiff(list.files(dirname(rp), full.names = TRUE), rp)
      if (!length(files)) return(FALSE)
      when <- suppressWarnings(max(as.numeric(file.info(files)$mtime), na.rm = TRUE))
      isTRUE(is.finite(when) && when < cutoff)
    }, logical(1)))
  }
  # These are real client bank statements, deleted for good on one click of an
  # enabled red button. Ask, and say how many and what survives.
  observeEvent(input$adm_purge_uploads, {
    req(admin_ok())
    if (UPLOADS_KEEP_DAYS <= 0) {
      output$adm_purge_msg <- renderUI(span(class = "bad",
        "Nothing deleted: retention is set to keep saved statements indefinitely. Set retention.uploads_keep_days in config/config.yaml and restart."))
      return()
    }
    n <- .uploads_due(UPLOADS_KEEP_DAYS)
    showModal(modalDialog(
      title = "Delete saved statements?", size = "m", easyClose = FALSE,
      p(sprintf("%d saved client statement file(s) in %s are older than %d days.",
                n, UPLOADS_DIR, as.integer(UPLOADS_KEEP_DAYS))),
      p(strong("This permanently deletes those files. There is no undo."),
        " The record of each upload is kept, so Insights and the audit trail are unchanged - only the statement itself goes."),
      if (n == 0L) p(class = "muted", "Nothing is old enough to delete, so this would do nothing."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("adm_purge_confirm", sprintf("Delete %d file(s)", n), class = "btn-danger"))))
  })
  observeEvent(input$adm_purge_confirm, {
    req(admin_ok())
    removeModal()
    p <- tryCatch(purge_uploads(UPLOADS_DIR, keep_days = UPLOADS_KEEP_DAYS), error = function(e) NULL)
    if (is.null(p)) {
      output$adm_purge_msg <- renderUI(span(class = "bad",
        "Could not tidy up the saved statements - check folder permissions on uploads/."))
      return()
    }
    output$adm_purge_msg <- renderUI(span(class = "ok", sprintf(
      "Deleted %d saved statement file(s); %d still within the %d-day period. The record of each upload is kept - only the statement itself is gone.",
      p$purged, p$kept, as.integer(UPLOADS_KEEP_DAYS))))
  })
  # (The old "Feedback flagged as wrong / minor issues" table stood here. It has
  # been REPLACED, not moved: adm_tpl_feedback on the Templates tab shows every
  # rating -- not just the flagged ones -- with the document it was left on and
  # the template that read it, beside the templates it is about, which is what C2
  # asked for. Two tables of the same log on two tabs was the fault, not the fix.)

  # WHAT HAPPENS TO THE FEED FOLDER, said in the same breath as what happens to
  # the saved statements. Generated from the setting itself (feed.keep_days) so
  # the promise on the screen and the rule on disk cannot drift apart.
  output$adm_feed_retention <- renderUI({
    req(admin_ok())
    note <- safe(feed_retention_note(CONFIG$feed$keep_days), NULL)
    if (is.null(note) || !nzchar(note)) return(NULL)
    helpText(note)
  })
}

shinyApp(ui, server)
