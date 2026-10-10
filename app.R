# app.R -- interactive GUI for the statement conversion engine.
#
# Two jobs for a non-engineer analyst, and one for whoever looks after the tool:
#   1. Convert -- drop in statements. Each file's bank is filled in from the
#      statement itself; each statement is read from its own content and proven
#      by its own arithmetic. One that proves needs nothing.
#   2. Please check -- one that does not prove is shown with the reason: the page
#      with the columns found drawn on it, what each column is, a Re-read and a
#      "This is right". Drawing the columns by hand is the last resort.
#   3. Admin -> Banks -- what the tool has learned for each bank, and training a
#      bank from a pile of its statements; Admin -> Automatic reading -- how it is
#      doing, with no personal data.
#
# Run locally:  R -e 'shiny::runApp(".", launch.browser = TRUE)'
# (from the repo root, so R/ and dictionaries/ resolve.)

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

# Load the engine (all pure-R modules) into the session.
for (.f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(.f)

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
# for: a 300-dpi scan of a year's statement is routinely 10-40 MB. The file picker
# refuses it with a bare red strip and nothing reaches the engine, so there is no
# log line either. Raise it here (and in scripts/run_app.R, which is how the server
# actually starts) from one config key so a site can tune it without touching code.
MAX_UPLOAD_MB <- suppressWarnings(as.numeric(CONFIG$app$max_upload_mb %||% 200))
if (!is.finite(MAX_UPLOAD_MB) || MAX_UPLOAD_MB <= 0) MAX_UPLOAD_MB <- 200
options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)
# HOW MANY FILES, not just how many megabytes. The size limit is per REQUEST, so a
# folder of five hundred small statements passed it and then converted one after
# another inside a single job, with no progress for the first one and no way to stop.
MAX_BATCH_FILES <- suppressWarnings(as.integer(CONFIG$app$max_batch_files %||% 50L))
# How long It's right and Set aside wait, with an Undo button, before they are done.
UNDO_SECONDS <- 10L
if (!is.finite(MAX_BATCH_FILES) || MAX_BATCH_FILES < 1L) MAX_BATCH_FILES <- 50L
# Training a bank is the one place a pile bigger than a case is expected ("upload
# ALL statements you have for a bank"). It runs as one background job, and a job
# is stopped after JOB_TIMEOUT_SECS (R/jobs.R) -- half an hour, which a few hundred
# text statements fit in and a pile of scans may not. More can be added any time.
TRAIN_MAX_FILES <- max(MAX_BATCH_FILES, 200L)
# HOW MANY CONVERSIONS MAY RUN AT ONCE. Every conversion runs in its own
# short-lived R process (R/jobs.R): R is single-threaded and this is ONE process
# for the whole team, so an engine call made here froze every other analyst's
# browser for as long as it took. The cap leaves a core for Shiny itself and the
# queue behind it says out loud where you are in it.
job_set_max_concurrent(CONFIG$app$max_concurrent_jobs)
# How often a waiting page asks its conversion how it is getting on. Half a
# second: quick enough that a 2.9s CSV does not feel delayed, cheap enough that
# ten waiting browsers cost a handful of file checks a second between them.
JOB_POLL_MS <- 500
# Nothing may outlive the app. A child still OCR'ing when the server stops would
# go on burning a core for two minutes on a result nobody can collect.
onStop(function() safe(job_reap_all()))
LOGDIR       <- CONFIG$paths$logs      # run log + feedback log live together, next to the app
UPLOADS_DIR  <- CONFIG$paths$uploads   # every uploaded statement + its lifecycle status (local-only)
REQUESTS_DIR <- CONFIG$paths$requests  # "none of these fits -- tell our team" raises (local-only)
DICT_PATH    <- CONFIG$paths$dictionary   # the shared label dictionary
LEXICON_PATH <- CONFIG$paths$lexicon %||% file.path("dictionaries", "lexicon.yaml")  # recognition vocabularies
# What the tool has learned, a folder per bank (R/layouts.R), and how it is doing
# (R/tracking.R). Read through the engine's own helpers so the app, the CLI and a
# conversion in a child process can never disagree about where either lives.
LAYOUTS_DIR  <- layouts_dir(CONFIG)
TRACKING_DIR <- tracking_dir(CONFIG)
# The bank remembered for an account (a salted mark, never the number) or a file
# name seen before (R/bank_memory.R). The salt is this install's own.
options(statement_studio.account_salt = tryCatch(bank_memory_salt(TRACKING_DIR), error = function(e) ""))
# The bundled specimen statement (public, synthetic, ships with the app) that "Try
# it on a sample" converts, so a brand-new user sees a full result without a file.
# It has to be one the reader PROVES on its own, with nothing learned: a sample
# that came back "Please check" would teach a first-time visitor the wrong thing.
# (Measured: samples/raw/tutorial/sample_everyday_statement.pdf, proven, 12 rows,
# no bank and nothing learned needed.) A PDF, so the sample shows the page too.
SAMPLE_STATEMENT <- file.path("samples", "raw", "tutorial", "sample_everyday_statement.pdf")

# How many days of run/feedback logs to keep before "Tidy up logs" archives them.
# One place, so the button label and both rollup calls can never disagree.
LOG_KEEP_DAYS <- 90L
# How long a COPY of an uploaded statement is kept under uploads/<id>/. Config-driven
# because it is a data-retention decision, not a code decision; the same number
# drives the purge, the Admin button label and the line shown in Admin, so what
# the maintainer is told and what happens can never disagree.
UPLOADS_KEEP_DAYS <- suppressWarnings(as.numeric(CONFIG$retention$uploads_keep_days %||% 90))
if (!is.finite(UPLOADS_KEEP_DAYS)) UPLOADS_KEEP_DAYS <- 90
UPLOADS_NOTE <- uploads_retention_note(UPLOADS_KEEP_DAYS)
# Startup tidy-up, once per process: old copies of client statements under
# uploads/, and per-session scratch folders a browser that closed abruptly left
# behind (each session unlinks its own; this is the backstop).
#
# THE PURGE DOES NOT RUN ON A SETTINGS FILE THAT DID NOT LOAD. UPLOADS_KEEP_DAYS
# comes out of config/config.yaml, and when that file cannot be parsed CONFIG is
# the BUILT-IN DEFAULTS -- so the number driving an irreversible delete would be
# one this site never chose. A site that set `uploads_keep_days: 0` (keep
# indefinitely) and then broke its settings file with one stray tab would lose
# every stored client statement older than 90 days on the next restart. Keep
# everything, say why, and leave it to the Admin button, which states the number.
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

# About-page content lives in ui_content.R (readability).
source("ui_content.R")

# THE SCREEN'S COLOURS, ONCE. The stylesheet's :root block is the design system,
# but the page pictures on Please check draw with base-R graphics, which cannot
# read a CSS variable -- so the four values are declared here as well, and
# test-app-ui.R fails if the two lists ever disagree.
PALETTE <- list(ok = "#0f7a37", bad = "#b3261e", warn = "#b7791f", meta = "#a15c00")
# .col_label(x, label, col) -- a column's name at the top of a page picture, on a
# white chip: printed straight onto the page it vanished into a bank's dark
# masthead, which is where many statements put their first lines.
.col_label <- function(x, label, col) {
  w <- abs(graphics::strwidth(label, cex = 0.85, font = 2)); h <- abs(graphics::strheight(label, cex = 0.85, font = 2))
  graphics::rect(x - w / 2 - 3, 14 - h, x + w / 2 + 3, 14 + h, col = "#ffffffe6", border = col, lwd = 1)
  graphics::text(x, 14, label, col = col, font = 2, cex = 0.85)
}

# .ck_col_colour(field, kind) -- the colour of a column on a page picture: what it
# IS, the same colours the money columns have everywhere (money in green, money
# out red). Shared by Please check and the recipe card.
.ck_col_colour <- function(field, kind) {
  if (identical(kind, "date")) return("#1d4ed8")
  if (!identical(kind, "money")) return("#68727d")
  switch(sub("[0-9]+$", "", field), debit = PALETTE$bad, credit = PALETTE$ok,
         balance = "#00205b", amount = PALETTE$warn, "#7c3aed")
}
# .col_short(field) -- one short word for a column: a band is too narrow for
# "Money going out".
.col_short <- function(field) {
  s <- unname(c(debit = "Out", credit = "In", amount = "In/Out", balance = "Balance", date = "Date",
                description = "Details")[as.character(field)])
  s[is.na(s)] <- "Other"
  s
}
# .money_cols(reading) -> a reading's columns of figures: field and heading.
.money_cols <- function(rd) {
  cols <- rd$columns
  if (!is.data.frame(cols) || !nrow(cols)) return(data.frame(field = character(0), heading = character(0)))
  m <- cols[cols$kind %in% "money", , drop = FALSE]
  m <- m[!duplicated(m$field), , drop = FALSE]
  data.frame(field = as.character(m$field), heading = as.character(m$heading %||% ""), stringsAsFactors = FALSE)
}
# .draw_page_columns(r, cols, labels) -- a page picture (render_page_view) with
# each column's band drawn, the ink read inside it shaded, and its label on top.
# Please check and the recipe card draw their pages with this one function.
.draw_page_columns <- function(r, cols, labels) {
  # NOT restored on exit: the caller may draw more over the page (Please check's
  # marked line), and restoring the margins would move its coordinates.
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i", xlab = "", ylab = "", axes = FALSE)
  graphics::rasterImage(r$ras, 0, r$h, r$w, 0)
  if (!is.data.frame(cols) || !nrow(cols)) return(invisible())
  for (j in seq_len(nrow(cols))) {
    cc <- .ck_col_colour(cols$field[j], cols$kind[j])
    graphics::rect(cols$x_min[j], 0, cols$x_max[j], r$h, border = cc, lwd = 1.6, lty = 2)
    if (all(is.finite(c(cols$ink_min[j], cols$ink_max[j]))))
      graphics::rect(cols$ink_min[j], 0, cols$ink_max[j], r$h, col = paste0(cc, "1f"), border = NA)
    .col_label((cols$x_min[j] + cols$x_max[j]) / 2, labels[j], cc)
  }
  invisible()
}
# .plot_height(width, r) -- a page picture's height at the width it is shown.
.plot_height <- function(width, r) {
  w <- width %||% 600
  if (is.null(r) || !is.finite(r$w) || r$w <= 0) 600 else max(300, round(w * r$h / r$w))
}

# .audit_gap(res) -- this conversion produced a workbook and NO audit record.
#
# The engine keeps the words (R/convert.R writes them into res$messages when the
# run-log record could not be written), and both verdict cards carry them in the
# same place and drop out of green while they do: a workbook with no record of how
# it was produced, on a forensic tool, is not a green result. .audit_line reads
# the engine's own sentence back off the result rather than writing a second copy.
.AUDIT_GAP_RX <- "^this conversion was not recorded in the audit log"
.audit_gap <- function(res) {
  if (nzchar(trimws(as.character(res$log_error %||% "")[1]))) return(TRUE)
  any(grepl(.AUDIT_GAP_RX,
            sub("^[a-z_]+:\\s*", "", as.character(res$messages %||% character(0)))))
}
.audit_line <- function(res) {
  m <- sub("^[a-z_]+:\\s*", "", as.character(res$messages %||% character(0)))
  m <- m[grepl(.AUDIT_GAP_RX, m)]
  if (!length(m)) "this conversion was not recorded in the audit log; tell whoever looks after this server before the file is relied on"
  else m[1]
}

# .blocking_diag(res) -- the diagnosis that OUTRANKS the reader's own reason on a
# file that read nothing, or NULL.
#
# Driven with no OCR software and an image-only PDF, the card's headline was the
# generic "nothing usable was read" while the diagnostics further down said,
# correctly, that this machine has no OCR software installed and that nothing on
# Please check would help until that was done. The engine already grades this:
# severity high with `fix_owner` naming who can fix it. "input" is the file itself
# (rescan it, re-export it, split it) and "escalate" an engine or install gap --
# neither is mended on Please check, so either takes the headline.
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
# whole way through the reader and every doubtful figure earns a row flag, so the
# caveat belongs above the fold, next to the download, where the person is
# looking. NULL when nothing was machine-read, so an ordinary text PDF and every
# CSV are untouched.
.scan_note <- function(ocr_pages, low_conf) {
  p <- suppressWarnings(as.integer(ocr_pages %||% NA_integer_)[1])
  if (is.na(p) || p < 1L) return(NULL)
  n <- suppressWarnings(as.integer(low_conf %||% NA_integer_)[1])
  if (is.na(n)) n <- 0L
  if (n > 0L)
    sprintf("%d page(s) were machine-read from a scan, and %d figure%s came out too faint to be sure of - check %s against the page.",
            p, n, if (n == 1L) "" else "s", if (n == 1L) "it" else "them")
  else
    sprintf("%d page(s) were machine-read from a scan - check the figures against the page.", p)
}

# ---- ADMIN: THE FEEDBACK LOG, JOINED TO THE LAYOUT THAT READ EACH FILE --------
#
# "All feedback accessible in admin in same area, but ensure it can be traced to
# the statement." The feedback record holds a run_id and the layout reference the
# result carried (template_id) and nothing else; the document name lives only in
# the run log, so it is joined on here.
#
# `layout_name` turns a layout reference into the name people see; a reference
# the store no longer holds is shown as itself. Pure, so it can be lifted out of
# app.R and called in a test.
.adm_feedback_overview <- function(feedback, runs = NULL, layout_name = function(ref) ref) {
  cols <- c("when", "document", "layout", "verdict", "comment", "who", "run_id")
  empty <- stats::setNames(
    data.frame(matrix(character(0), 0, length(cols)), stringsAsFactors = FALSE), cols)
  if (is.null(feedback) || !is.data.frame(feedback) || !nrow(feedback)) return(empty)
  col <- function(df, name) {
    if (is.null(df) || !is.data.frame(df) || !(name %in% names(df)))
      return(rep(NA_character_, if (is.data.frame(df)) nrow(df) else 0L))
    as.character(df[[name]])
  }
  said <- function(v) { v <- as.character(v); v[is.na(v) | !nzchar(trimws(v))] <- "not recorded"; v }
  fb_run <- col(feedback, "run_id"); fb_ly <- col(feedback, "template_id")
  r_file <- col(runs, "source_file")[match(fb_run, col(runs, "run_id"))]
  ly <- vapply(fb_ly, function(ref) {
    if (is.na(ref) || !nzchar(ref)) return(NA_character_)
    as.character(safe(layout_name(ref), ref))[1]
  }, character(1), USE.NAMES = FALSE)
  out <- data.frame(
    when     = said(safe(local_time_text(col(feedback, "ts")), col(feedback, "ts"))),
    document = said(basename(ifelse(is.na(r_file), "", r_file))),
    layout   = said(ly),
    verdict  = said(col(feedback, "verdict")),
    comment  = said(col(feedback, "comment")),
    who      = said(col(feedback, "requested_by")),
    run_id   = said(fb_run),
    stringsAsFactors = FALSE)
  # Newest first, on the record's own stamp rather than the words shown.
  out <- out[order(col(feedback, "ts"), decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# .ADM_HISTORY_MAX / .adm_history(logdir, subdir, budget) -- the newest slice of
# a log folder, and how much of it was left behind.
#
# Admin used to parse EVERY line of every archive file, one record at a time,
# automatically, the moment the tab was opened. Measured: 20,000 archived rows
# took 10.5 seconds, in the one Shiny process the whole team shares. The picture
# Admin draws needs the recent records, so this reads the LIVE folder first and
# tops up from the yearly archive, newest year first, up to a budget -- and counts
# what it did not read so the screen can say so. Nothing is deleted or hidden.
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

# ---- PLEASE CHECK: WHICH PAGES ADD UP ------------------------------------------
#
# .result_rows(res) -> data.frame(row_id, statement, page, amount, balance,
# derived), one row per transaction in the order the statement prints them, with
# `page` the FILE's page. The conversion ran in another process, so the rows the
# screen has are the ones it wrote: the JSON output holds every row with its
# statement and its provenance ("pdf:p3" -- the page within that statement), and
# res$reading[[s]]$pages maps a statement's pages to the file's. NULL when there is
# no JSON (a run that wrote no files) or it cannot be read.
.result_rows <- function(res) {
  js <- as.character(res$outputs %||% character(0))
  js <- js[grepl("\\.json$", js) & file.exists(js)]
  if (!length(js)) return(NULL)
  j <- safe(jsonlite::fromJSON(js[1], simplifyVector = TRUE), NULL)
  tx <- j$transactions
  if (!is.data.frame(tx) || !nrow(tx) || !all(c("row_id", "amount") %in% names(tx))) return(NULL)
  pv <- j$provenance
  ref <- if (is.data.frame(pv) && all(c("row_id", "source_ref") %in% names(pv)))
    as.character(pv$source_ref[match(tx$row_id, pv$row_id)]) else rep(NA_character_, nrow(tx))
  st <- if ("statement_index" %in% names(tx)) suppressWarnings(as.integer(tx$statement_index)) else rep(1L, nrow(tx))
  st[is.na(st)] <- 1L
  local <- suppressWarnings(as.integer(sub("^pdf:p", "", ref)))
  local[!grepl("^pdf:p[0-9]+$", ref %||% "")] <- NA_integer_
  page <- vapply(seq_len(nrow(tx)), function(i) {
    pg <- res$reading[[st[i]]]$pages %||% integer(0)
    if (is.na(local[i])) NA_integer_
    else if (local[i] <= length(pg)) as.integer(pg[local[i]]) else local[i]
  }, integer(1))
  num <- function(v) suppressWarnings(as.numeric(v))
  data.frame(row_id = tx$row_id, statement = st, page = page, amount = num(tx$amount),
             balance = if ("balance" %in% names(tx)) num(tx$balance) else NA_real_,
             derived = grepl("amount_from_balance", as.character(tx$flags %||% ""), fixed = TRUE),
             stringsAsFactors = FALSE)
}

# .page_ticks(rows, pages) -> data.frame(page, rows, steps, held, derived): for
# each page, how many of its balance steps add up. A step runs from one printed
# balance to the next and holds when the earlier balance plus every amount between
# equals the later one, to the cent -- a balance printed once a day still makes a
# step, just a longer one. A statement can print newest first, so both directions
# are walked and the one that holds more steps is the one the page is judged by;
# the step belongs to the page of the row that closes it. An amount that could not
# be read leaves its step unjudged rather than broken. `rows` is .result_rows() for
# ONE statement, in its printed order.
.page_ticks <- function(rows, pages) {
  pages <- sort(unique(as.integer(pages[!is.na(pages)])))
  out <- data.frame(page = pages, rows = 0L, steps = 0L, held = 0L, derived = 0L)
  if (!is.data.frame(rows) || !nrow(rows)) return(out)
  walk <- function(o) {
    res <- rep(NA, nrow(rows)); last <- NA_real_; acc <- 0; known <- TRUE
    for (i in o) {
      a <- rows$amount[i]; b <- rows$balance[i]
      if (is.na(a)) known <- FALSE else acc <- acc + a
      if (!is.na(b)) {
        if (!is.na(last) && known) res[i] <- abs(last + acc - b) < 0.005
        last <- b; acc <- 0; known <- TRUE
      }
    }
    res
  }
  fwd <- walk(seq_len(nrow(rows))); bwd <- walk(rev(seq_len(nrow(rows))))
  held <- if (sum(bwd %in% TRUE) > sum(fwd %in% TRUE)) bwd else fwd
  for (k in seq_along(pages)) {
    on <- rows$page %in% pages[k]
    out$rows[k] <- sum(on)
    out$steps[k] <- sum(on & !is.na(held))
    out$held[k] <- sum(on & held %in% TRUE)
    out$derived[k] <- sum(on & rows$derived %in% TRUE)
  }
  out
}

# .tick_word(t) -- one page's tick, as the strip under the picture says it.
.tick_word <- function(t) {
  if (t$rows == 0L) return(list(glyph = "\u2013", cls = "tick-none", say = "no transactions on this page"))
  if (t$steps == 0L) return(list(glyph = "\u2013", cls = "tick-none", say = "no running balance to check"))
  if (t$held == t$steps)
    return(list(glyph = "\u2713", cls = "tick-ok",
                say = sprintf("the balance adds up (%d step%s)", t$steps, if (t$steps == 1L) "" else "s")))
  list(glyph = "\u2717", cls = "tick-bad",
       say = sprintf("%d of %d balance step%s do not add up", t$steps - t$held, t$steps,
                     if (t$steps == 1L) "" else "s"))
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
    # entirely ours and entirely local -- no CDN font, script or icon pack -- so
    # air-gapping is untouched. The ?v= is the engine version, so an upgraded
    # install cannot serve a browser its cached copy of the old stylesheet.
    tags$link(rel = "stylesheet", type = "text/css",
              href = sprintf("app.css?v=%s", engine_version())),
    # The tab icon -- local like everything else. Without one every page load asked
    # for /favicon.ico, got a 404, and put an error in the browser console that
    # anyone checking the console for a real fault then had to read past.
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    # Enter in the Admin password box = click Enter (no mouse trip). The
    # trigger('change') first flushes the debounced text value, so a fast
    # type-then-Enter never submits a stale password.
    tags$script(HTML(
      "$(document).on('keyup', '#adm_pw', function(e){
         if (e.key === 'Enter') { $(this).trigger('change'); $('#adm_login').click(); }
       });")),
    # The Convert table (cv_plan). Its bank dropdowns are plain <select>s, not Shiny
    # inputs, so a change is sent as ONE event naming the upload and the row, and the
    # server records it (cv_plan_picks). A click on a row that has a result opens that
    # result -- but never a click that was really on the dropdown in it. Enter does
    # the same for somebody on the keyboard. "Please check" in a row opens that row
    # AND takes the page down to the check.
    tags$script(HTML(
      "$(document).on('change', 'select.plan-pick', function(){
         Shiny.setInputValue('cv_plan_pick', {gen: parseInt($(this).attr('data-gen'), 10),
           row: parseInt($(this).attr('data-row'), 10), value: $(this).val()}, {priority: 'event'});
       });
       $(document).on('click', 'tr.plan-openable', function(e){
         if ($(e.target).closest('select,option,a,button,input,label').length) return;
         Shiny.setInputValue('cv_plan_open', {gen: parseInt($(this).attr('data-gen'), 10),
           row: parseInt($(this).attr('data-row'), 10), check: false}, {priority: 'event'});
       });
       $(document).on('click', 'a.plan-check', function(e){
         e.preventDefault();
         Shiny.setInputValue('cv_plan_open', {gen: parseInt($(this).attr('data-gen'), 10),
           row: parseInt($(this).attr('data-row'), 10), check: true}, {priority: 'event'});
       });
       // the Undo bar counts down on the page; typing a note gives more time
       setInterval(function(){
         document.querySelectorAll('.undo-count[data-secs]').forEach(function(el){
           if (!el.dataset.end) el.dataset.end = Date.now() + 1000 * parseInt(el.dataset.secs, 10);
           var left = Math.max(0, Math.ceil((parseInt(el.dataset.end, 10) - Date.now()) / 1000));
           el.textContent = 'in ' + left + ' s';
         });
       }, 250);
       $(document).on('input', '#cv_ck_aside_note', function(){
         document.querySelectorAll('.undo-count[data-secs]').forEach(function(el){
           el.dataset.end = Date.now() + 1000 * parseInt(el.dataset.secs, 10); });
         Shiny.setInputValue('cv_ck_note_typing', Date.now(), {priority: 'event'});
       });
       $(document).on('keydown', 'tr.plan-openable', function(e){
         if (e.key === 'Enter' && e.target === this) $(this).trigger('click');
       });
       $(document).on('shiny:connected', function(){
         Shiny.addCustomMessageHandler('ss-scroll', function(id){
           setTimeout(function(){ var el = document.getElementById(id);
             if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'}); }, 400);
         });
       });")),
    # DROP FILES OR A FOLDER ANYWHERE ON THE PAGE. The dropped files (a folder's
    # files, walked) are handed to the ordinary picker, so they go through exactly
    # the same upload as Browse: same size limit, same file types, same table.
    tags$script(HTML(
      "(function(){
        var OK = /\\.(pdf|csv|tsv|tdv|xlsx|xls)$/i, depth = 0;
        function walk(entry, out){
          return new Promise(function(done){
            if (!entry) return done();
            if (entry.isFile) return entry.file(function(f){ if (OK.test(f.name)) out.push(f); done(); }, function(){ done(); });
            if (!entry.isDirectory) return done();
            var rd = entry.createReader(), all = [];
            (function more(){ rd.readEntries(function(es){
              if (!es.length) return Promise.all(all.map(function(e){ return walk(e, out); })).then(function(){ done(); });
              all = all.concat([].slice.call(es)); more(); }, function(){ done(); }); })();
          });
        }
        function hasFiles(e){ var t = e.originalEvent && e.originalEvent.dataTransfer;
          return t && [].indexOf.call(t.types || [], 'Files') >= 0; }
        $(document).on('dragenter', function(e){ if (!hasFiles(e)) return; depth++; document.body.classList.add('ss-drop'); });
        $(document).on('dragleave', function(e){ if (!hasFiles(e)) return; if (--depth <= 0){ depth = 0; document.body.classList.remove('ss-drop'); } });
        $(document).on('dragover', function(e){ if (hasFiles(e)) e.preventDefault(); });
        $(document).on('drop', function(e){
          if (!hasFiles(e)) return;
          e.preventDefault(); depth = 0; document.body.classList.remove('ss-drop');
          var dt = e.originalEvent.dataTransfer, items = [].slice.call(dt.items || []), out = [];
          var entries = items.map(function(it){ return it.webkitGetAsEntry ? it.webkitGetAsEntry() : null; });
          var p = entries.some(Boolean) ? Promise.all(entries.map(function(en){ return walk(en, out); }))
                                         : Promise.resolve([].slice.call(dt.files).forEach(function(f){ if (OK.test(f.name)) out.push(f); }));
          p.then(function(){
            if (!out.length) { Shiny.setInputValue('cv_drop_none', Date.now(), {priority: 'event'}); return; }
            var tab = document.querySelector('a[data-value=\"Convert\"]'); if (tab) $(tab).tab('show');
            var inp = document.getElementById('cv_file'); if (!inp) return;
            var box = new DataTransfer(); out.forEach(function(f){ box.items.add(f); });
            inp.files = box.files; $(inp).trigger('change');
          });
        });
      })();")),
    # Loading feedback: a real animation, not just the grey-out. A busy pill shows
    # whenever Shiny is working; recalculating outputs dim and float a spinner. The
    # pill, the dim and the centred CONVERTING overlay are styled in app.css
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
        // A PROGRESS notification carries .progress-message; an ordinary toast
        // never does. So the centred overlay follows a progress panel only, and
        // warnings stay where toasts go. MutationObserver rather than CSS :has(),
        // because the deployment browser may be older than :has() support, and
        // $(function(){}) because this script runs BEFORE <body> exists.
        // A <details> opened by hand: tell Shiny its outputs are now visible, so
        // a table inside one draws (Shiny only listens for Bootstrap's 'shown').
        $(document).on('change','.dropzone input[type=file]',function(){
          var z=$(this).closest('.dropzone');z.toggleClass('has-files',this.files&&this.files.length>0);});
        document.addEventListener('toggle',function(e){
          if(e.target&&e.target.tagName==='DETAILS'&&e.target.open&&window.jQuery)
            jQuery(e.target).trigger('shown');},true);
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
    span(class = "app-mark", `aria-hidden` = "true", HTML(
      '<svg viewBox="0 0 24 24" width="24" height="24"><rect x="1" y="1" width="22" height="22" rx="6" fill="currentColor"/><path d="M7 8.5h10M7 12h6" stroke="#ffffff" stroke-width="1.8" stroke-linecap="round" opacity=".7"/><path d="M12.5 16.2l2 2 4-4.4" stroke="#ffffff" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg>')),
    span(class = "app-title", "Statement Studio"),
    span(class = "app-header-end", uiOutput("hdr_user", inline = TRUE),
      conditionalPanel("output.admin_authed", style = "display:inline",
        actionLink("adm_signout", "Sign out of Admin", class = "hdr-signout")))),
  tabsetPanel(
    id = "main_tabs", selected = "Convert",
    # ---- About: the "what is this and why can I rely on it" page. The app OPENS
    # on Convert (selected, above); this is the page you come back to.
    tabPanel("About", br(),
      div(class = "hub",
        h1(class = "hub-h1", "Every figure comes straight off your statement, and the statement\u2019s own arithmetic has to prove it."),
        p(class = "hub-lead", "Bank statements in \u2014 clean, checked data out. PDF, scan, CSV or Excel; anything that can\u2019t be proven is shown to you with the reason."),
        actionLink("ab_go_convert", class = "btn btn-primary hub-go", label = "Convert statements \u2192"),
      # NOTE: no Admin card here, and no Admin anywhere else a user can see.
      # Nobody who uses this app has the Admin password; advertising it is an
      # invitation to a locked door.
      about_html())),
    # ---- Convert -------------------------------------------------------
    tabPanel(
      "Convert",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          # ONE PICKER, ONE FILE OR A WHOLE CASE. A folder of statements is the same
          # question asked of more files, so it is the same control: pick one and
          # you get its result page; pick twelve and you get a row per file, each
          # of which OPENS that same result page.
          # ONE DROP ZONE: a dashed box with an upload mark, the words, and what it
          # takes; once files are in, a compact count with Change files.
          div(class = "dropzone", id = "cv_drop",
            fileInput("cv_file", NULL, multiple = TRUE, width = "100%",
                      accept = c(".pdf", ".csv", ".tsv", ".tdv", ".xlsx", ".xls"),
                      buttonLabel = tagList(
                        HTML('<svg class="dz-ico" viewBox="0 0 24 24" width="28" height="28" aria-hidden="true"><path d="M12 16V5m0 0l-4.5 4.5M12 5l4.5 4.5M5 15v3a2 2 0 002 2h10a2 2 0 002-2v-3" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"/></svg>'),
                        span(class = "dz-say", "Drop statements here or ", span(class = "dz-link", "choose files")),
                        span(class = "dz-sub", sprintf("PDF, scans, CSV or Excel \u00b7 up to %d files at a time", MAX_BATCH_FILES)),
                        span(class = "dz-change", "Change files")),
                      placeholder = "")),
          uiOutput("cv_whoami"),
          # OFF UNTIL IT CAN WORK, with the reason under it: a full-width green button
          # that does nothing when pressed sends the person looking for what is broken.
          uiOutput("cv_go_btn"),
          helpText(class = "dz-limit", sprintf("Up to %g MB each.", MAX_UPLOAD_MB))
        ),
        mainPanel(
          width = 8,
          # ---------------------------------------------------------------
          # THE INTERFACE RULE (charter), applied to the result page.
          #
          # ABOVE the fold: what a forensic accountant came here for -- did it
          # work, here is your download, here are your transactions -- and, when
          # the statement did not prove itself, the one thing left to do (Please
          # check). BEHIND ONE CONTROL: the charts.
          #
          # THE TABLE FIRST: one row per file, its bank (pre-filled from the
          # statement, changeable), and once converted its layout and outcome. A
          # row opens THAT file's ordinary result page below: there is no second,
          # thinner result view to keep in step.
          uiOutput("cv_plan"),
          uiOutput("cv_status"),
          # ONE RESULT HEADER: the verdict, its downloads, then a one-line bank note
          div(class = "result-head",
            uiOutput("cv_headline"),   # the verdict, in her words
            uiOutput("cv_downloads")), # the payoff, right under it
          uiOutput("cv_bank_note"),  # the statement names another bank than the one used
          uiOutput("cv_spot"),       # picked for a spot check: is it right?
          # PLEASE CHECK. Open by itself when the statement did not prove; one
          # quiet link on a proven one, because a reviewer may still want to see
          # where the columns were found.
          uiOutput("cv_check"),
          # Before any conversion, a clear empty state rather than bare headers.
          conditionalPanel("output.cv_has_result != true && output.cv_has_batch != true",
                           uiOutput("cv_empty")),
          conditionalPanel("output.cv_has_result == true",
            # NO MORE THAN THE QVF (D16): the sentence above, then the table with its
            # Check column. Everything else -- the figures, did it add up, a bundle's
            # statements, the checks, the charts, the feedback form -- is behind ONE
            # closed "More detail" link. A file that read nothing shows no table.
            conditionalPanel("output.cv_has_txns == true",
              DTOutput("cv_txns")),
            uiOutput("cv_accept_bar"),   # a new design that adds up: accept it, under its rows
            uiOutput("cv_more_toggle"),
            conditionalPanel("output.cv_detail_open == true",
              conditionalPanel("output.cv_has_txns == true",
                uiOutput("cv_summary"),
                uiOutput("cv_proof"),    # did it add up - always, pass or fail
                uiOutput("cv_split")),   # a bundle: what each statement in it says
              uiOutput("cv_detail"),
              conditionalPanel("output.cv_has_txns == true",
                h4("Analysis"),
                div(style = "border:1px solid var(--line);border-radius:var(--r);padding:10px 14px;margin:6px 0 14px",
                  fluidRow(
                    column(4, selectInput("an_view", "Show",
                      c("Money in vs out" = "inout", "Balance over time" = "balance",
                        "Running total of every transaction" = "cumnet"), width = "100%")),
                    # THESE TWO ONLY EXIST WHEN THEY MEAN SOMETHING: on the two line
                    # views the transactions are drawn ungrouped, and "Count" printed
                    # real account balances as bare integers under a "Balance" label.
                    conditionalPanel("input.an_view == 'inout'", class = "col-sm-4",
                      selectInput("an_group", "Group by",
                        c("Day" = "day", "Week" = "week", "Month" = "month"),
                        selected = "week", width = "100%")),
                    conditionalPanel("input.an_view == 'inout'", class = "col-sm-4",
                      radioButtons("an_unit", "Measure",
                        c("Dollars" = "amount", "Count" = "count"), inline = TRUE))),
                  plotOutput("cv_trend", height = "270px"),
                  uiOutput("cv_trend_note"))),
              uiOutput("cv_feedback")))
        )
      )
    ),
    tabPanel(
      "Admin",
      br(),
      # A broken settings file is announced to everyone who opens Admin, signed in
      # or not (without the parse detail, which only an admin sees).
      uiOutput("adm_cfg_banner"),
      # The sign-in box is rendered SERVER-side, because when no admin password has
      # been set there must be no box at all -- just the instructions for setting one.
      conditionalPanel("!output.admin_authed", uiOutput("adm_login_panel")),
      conditionalPanel("output.admin_authed",
      # ---- FOUR TABS, BECAUSE AN ADMIN HAS FOUR QUESTIONS --------------------
      #   Needs attention   what is waiting for an admin, each with one button
      #   Recipes           how each design of statement is read: on/off, change, merge
      #   Words             the words it looks for
      #   Health            what is failing, how automatic reading is doing, the
      #                     uploads, the queues, training, the housekeeping
      tabsetPanel(
        id = "adm_tabs",
        tabPanel(
          "Needs attention",
          br(),
          # ONE CARD PER KIND OF THING TO DO, each with its count, and each item
          # with ONE button (D15): Fix, Accept / Retire, Merge.
          uiOutput("adm_na_cards"),
          uiOutput("adm_na_msg"),
          tags$hr(),
          h4("Fixes waiting for an admin"),
          p(class = "muted", style = "max-width:860px",
            "When someone fixes a statement, it only changes that one file. Accept a fix to use it for every statement like it."),
          DTOutput("adm_fixes"),
          uiOutput("adm_fix_btns"),
          uiOutput("adm_fix_msg")
        ),
        tabPanel(
          "Recipes",
          br(),
          # ONE ROW PER RECIPE (D15). A recipe is how the tool reads one design of
          # statement; every reading it makes must still add up. Nobody sees YAML.
          p(class = "muted", style = "max-width:860px",
            "Each recipe reads one design of statement. Every reading must still add up, so a recipe can never make a wrong conversion look right. Click a recipe to see it and change it."),
          uiOutput("adm_rc_list"),
          div(style = "margin:10px 0",
            actionButton("adm_rc_new_open", "New recipe from a statement", class = "btn-default")),
          uiOutput("adm_rc_new"),
          uiOutput("adm_rc_card")
        ),
        tabPanel(
          "Words",
          br(),
          # THE WORDS, BOTH SETS, IN ONE PLACE. The label dictionary (which value a
          # wording means) and the recognition vocabulary (which words mean money
          # out, money in, a heading, a brand) are the same question asked twice.
          h4("Words the tool looks for"),
          # NB the WORDING is a navigation anchor: dictionaries/lexicon.yaml sends
          # the reader to Admin -> Words -> "Words the tool looks for", so the
          # phrase stays on the page.
          p(class = "muted", style = "margin:-6px 0 10px;font-size:12.5px",
            "Type a word as the statement prints it, then pick what it means - each choice has an example. An admin can also teach one from the statement itself, on Please check."),
          # ONE FORM, NOT THREE. The WORD is typed once; "What it means" carries both
          # files' categories, and the answer decides which file is written, because
          # which file a word lives in is a fact about the word, not a question for
          # the person typing it. Both writers insert one line and leave the rest of
          # the file -- comments and all -- untouched.
          fluidRow(
            column(5,
              textInput("adm_word_text", "The word or wording, as the statement prints it",
                        "", placeholder = "balance at start"),
              uiOutput("adm_word_kind_ui"),
              actionButton("adm_word_add", "Teach it", class = "btn-primary"),
              br(), br(), uiOutput("adm_word_msg")),
            column(7,
              helpText(HTML(paste0(
                "Type it as the statement prints it - case doesn't matter, and part of the ",
                "wording is enough (<i>\"balance at start\"</i> matches <i>\"Balance at start ",
                "of period\"</i>). It applies from the next conversion, everywhere the tool ",
                "looks for that value. Nothing is ever added automatically, and a backup of ",
                "the file is kept each time."))))),
          br(),
          h5("Words your statements used that the tool didn't recognise"),
          uiOutput("adm_sugg_help"),
          uiOutput("adm_sugg_scope"),
          fluidRow(
            column(6,
              # THE LIST IS THE PICKER: clicking a row puts the word in the box above,
              # where the one question left - what does it mean - is answered once.
              DTOutput("adm_sugg_tokens"),
              uiOutput("adm_sugg_msg")),
            column(6,
              strong("Columns in your statements that nothing reads"),
              uiOutput("adm_sugg_cols"),
              helpText(HTML(paste0(
                "This list is harvested from every conversion, including the ones where nothing ",
                "was read - the first line of a letter or a non-statement is offered here as if it ",
                "were a heading."))))),
          br(),
          tags$details(
            class = "quiet-advanced",
            # (was "Edit the whole vocabulary file or the whole dictionary file"; now one quiet link)
            tags$summary("Advanced"),
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
              # NO "RELOAD FROM FILE" BUTTON. The box already holds the file; the only
              # question the button could answer is "has somebody else on this server
              # saved this underneath me", and Save answers it (see .vocab_stale).
              fluidRow(
                column(5,
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
                  actionButton("adm_lex_defaults", "Show built-in defaults"),
                  actionButton("adm_lex_save", "Save vocabulary", class = "btn-primary"),
                  br(), br(), uiOutput("adm_lex_msg")),
                column(7,
                  textAreaInput("adm_lex_edit", NULL, value = "", width = "100%", height = "320px")))))
        ),
        tabPanel(
          "Health",
          br(),
          # HOW AUTOMATIC READING IS DOING (it was its own tab; D15 keeps four).
          h3(class = "hl-title", "How reading is going"),
          div(style = "margin-bottom:8px",
            actionButton("adm_ar_refresh", "Refresh", class = "btn-default btn-sm"),
            uiOutput("adm_ar_export_ui", inline = TRUE)),
          uiOutput("adm_ar_head"),
          h4("By kind of file"),
          DTOutput("adm_ar_kinds"),
          fluidRow(
            column(6, h4("Checks that failed"),
              helpText("Which of the reader's checks stopped a statement being proven, most often first."),
              DTOutput("adm_ar_checks")),
            column(6, h4("What each reading was checked against, and what was learned"),
              DTOutput("adm_ar_proof"))),
          actionButton("adm_refresh", "Refresh from logs", class = "btn-default btn-sm"),
          helpText("A live picture from every conversion the team has run and every rating left."),
          # HOW MUCH OF THE HISTORY THIS PICTURE IS MADE OF: it reads the newest
          # slice and says which slice, rather than freezing everybody's browser to
          # be complete. Nothing is lost: the archive is still on disk in full.
          uiOutput("adm_history_note"),
          tags$details(class = "hl-sec", tags$summary("Spot checks"),
          h4("Spot checks"),
          uiOutput("adm_ar_spot"),
          fluidRow(
            column(5,
              numericInput("adm_spot_rate", "Spot-check rate (% of automatic conversions)",
                           value = 0, min = 0, max = 100, step = 0.5),
              actionButton("adm_spot_save", "Save the rate", class = "btn-primary"),
              uiOutput("adm_spot_msg")),
            column(7, helpText(paste(
              "Off (0) by default. A spot check asks the person who ran an automatic conversion to compare a few",
              "figures with the statement. Which statements are picked depends on the file itself, so the same",
              "statement is always picked, or never; one converted on a layout with no balance of its own is",
              "picked at twice the rate."))))),
          tags$details(class = "hl-sec", tags$summary("Train a bank"),
          h4("Train a bank"),
          p(class = "muted", style = "max-width:860px",
            sprintf("Pick or name the bank, add every statement you have for it (up to %d at a time), and Train. They are read in the background - the layouts are worked out from them, and each statement that does not prove itself is listed with the reason. More can be added any time.",
                    TRAIN_MAX_FILES)),
          fluidRow(
            column(4, selectizeInput("adm_train_bank", "Bank", choices = NULL,
                                     options = list(create = TRUE,
                                                    placeholder = "Pick a bank, or type a new one's name"))),
            column(5, fileInput("adm_train_files", "Its statements", multiple = TRUE,
                                accept = c(".pdf", ".csv", ".tsv", ".tdv", ".xlsx", ".xls"))),
            column(3, br(), actionButton("adm_train_go", "Train", class = "btn-primary"))),
          uiOutput("adm_train_status")),
          tags$details(class = "hl-sec", tags$summary("Recent conversions, and designs that stopped adding up"),
          fluidRow(
            column(5, h4("Conversions by status"), plotOutput("adm_status_plot", height = "210px"),
                   DTOutput("adm_overview")),
            column(7,
              h4("Layouts that started failing recently"),
              helpText("A bank can change its statement slightly - a column moves, a heading is renamed - and the arithmetic stops proving the readings. Any learned layout whose statements are suddenly going to a person more often shows here. Empty is good."),
              DTOutput("adm_drift"))),
          h4("Statements nothing could be read from"),
          helpText("Each row is one layout (identical layouts are grouped) whose statements read nothing usable. The biggest count is the one to look at first."),
          DTOutput("adm_gaps"),
          h5("Files that could not be opened at all"),
          helpText("These runs never got as far as reading: a damaged or password-protected file, a file that had gone by the time it was read, or something that isn't a statement."),
          DTOutput("adm_unreadable"),
          h4("Learned layouts in use"),
          DTOutput("adm_usage"),
          # A LAYOUT THAT MATCHED WRONGLY IS RETIRED HERE (the Banks tab is gone, D1;
          # recipes replace learned layouts, and until the last is retired this is
          # where one is taken out of use).
          div(style = "display:flex;gap:8px;align-items:flex-end;flex-wrap:wrap;margin-top:8px",
            div(style = "flex:0 1 320px;min-width:0", selectizeInput("adm_layout_pick", "Take a learned layout out of use", choices = NULL,
              width = "100%", options = list(placeholder = "Pick a layout"))),
            actionButton("adm_layout_retire", "Retire it", class = "btn-danger", style = "margin-bottom:15px")),
          uiOutput("adm_layout_msg"),
          h4("What the team said about these conversions"),
          helpText("Every rating left on a conversion, newest first, with the document it was left on and the layout that read it."),
          DTOutput("adm_feedback")),
          tags$details(class = "hl-sec", tags$summary("Every statement uploaded"),
          # THE TABLE IS THE WHOLE LOG: one row per upload, whatever became of it,
          # which is what the incident procedure sends a maintainer here for.
          h4("Uploads - every document converted here, newest first"),
          helpText(paste("One row per upload, whatever became of it. A row marked",
                         "'nothing usable was read' is a statement still to look at.")),
          fluidRow(
            column(8, DTOutput("adm_uploads")),
            column(4,
              selectizeInput("adm_up_pick", "Pick a saved upload", choices = NULL,
                             options = list(placeholder = "Type to search, or click a row on the left")),
              # Rendered server-side (see dl_when): a download with nothing to send
              # used to answer an HTTP 500 error page instead of a file.
              uiOutput("adm_up_audit_ui"),
              br(), br(),
              actionButton("adm_up_reread", "Read it again on Convert", class = "btn-warning")))),
          tags$details(class = "hl-sec", tags$summary("Requests from the team"),
          h4("Format requests - raised by the team"),
          helpText("Layouts the team flagged, in their own words (no personal data). Mark each done once it is dealt with."),
          fluidRow(
            column(9, DTOutput("adm_requests")),
            column(3,
              selectizeInput("adm_req_pick", "A request", choices = NULL,
                             options = list(placeholder = "Loading\u2026")),
              actionButton("adm_req_actioned", "Mark done", class = "btn-primary"),
              br(), br(),
              actionButton("adm_req_dismiss", "Dismiss"),
              br(), br(), uiOutput("adm_req_msg")))),
          tags$details(class = "hl-sec", tags$summary("Folder intake"),
          h4("Folder intake - inbox / processed / failed"),
          helpText("Statements dropped into the inbox/ folder land here. Anything in failed/ is worth a look."),
          uiOutput("adm_inbox_counts"),
          fluidRow(
            column(8, h5("Failed - needs attention"), DTOutput("adm_inbox_failed")),
            column(4,
              selectizeInput("adm_inbox_pick", "A failed file", choices = NULL,
                             options = list(placeholder = "Loading\u2026")),
              actionButton("adm_inbox_reread", "Read it again on Convert", class = "btn-warning"),
              br(), br(),
              uiOutput("adm_inbox_audit_ui"))),
          fluidRow(
            column(4, h5("Waiting in inbox"), DTOutput("adm_inbox_waiting")),
            column(4, h5("Processed"), DTOutput("adm_inbox_processed")),
            column(4, h5("Output folders (outbox)"), DTOutput("adm_inbox_outbox")))),
          tags$details(class = "hl-sec", tags$summary("Sending results to reports"),
          # THE ANALYTICS FEED, WHERE THE PERSON WHO CAN FIX IT WILL SEE IT. A feed
          # write that failed is a server fault, so it belongs here, not on Convert.
          uiOutput("adm_feed_health")),
          tags$details(class = "hl-sec", tags$summary("Check a pile of files at once"),
          # THE BULK AUDIT. IT AUDITS; IT DOES NOT CONVERT, AND IT DOES NOT LEARN:
          # training a bank is Banks -> Train, where what is learned is the point.
          h4("Check a pile of files at once"),
          helpText(HTML("Drop in a pile of statements and get one picture: what the reader proves on its own, and the statements it cannot read <b>grouped by layout, biggest first</b>. Nothing is converted, saved or learned - only shapes and counts, so it is safe to share.")),
          fluidRow(
            column(4,
              fileInput("adm_ba_files", "Statements (.csv / .tsv / .pdf / .xlsx / .xls)", multiple = TRUE,
                        accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx", ".xls")),
              actionButton("adm_ba_run", "Run", class = "btn-primary"),
              br(), br(),
              uiOutput("adm_ba_report_ui"),
              br(),
              helpText("Also available headless: Rscript scripts/bulk-audit.R <folder>")),
            column(8,
              uiOutput("adm_ba_summary"),
              h5("Not read - grouped by layout, biggest first"), DTOutput("adm_ba_clusters"),
              h5("Per file - shapes only, no personal data"), DTOutput("adm_ba_files_tbl")))),
          tags$details(class = "hl-sec", tags$summary("Housekeeping"),
          h4("Housekeeping"),
          actionButton("adm_rollup", sprintf("Tidy up logs (archive runs older than %d days)", LOG_KEEP_DAYS)),
          uiOutput("adm_rollup_msg"),
          # WHAT HAPPENS TO THE FEED FOLDER, from the setting that decides it, so the
          # promise on the screen and the rule on disk cannot drift apart.
          uiOutput("adm_feed_retention"),
          br(),
          # Retention of the SAVED STATEMENTS themselves -- real client data. Runs at
          # startup too; this is the "do it now" button, and it says what it will do
          # before you press it.
          h5("Saved statements - retention"),
          helpText(UPLOADS_NOTE),
          actionButton("adm_purge_uploads",
                       if (UPLOADS_KEEP_DAYS > 0)
                         sprintf("Delete saved statements older than %d days now", as.integer(UPLOADS_KEEP_DAYS))
                       else "Delete old saved statements now (retention is set to 'keep indefinitely')",
                       class = "btn-danger"),
          uiOutput("adm_purge_msg"),
          br(),
          # NB "Data capture" was a tab; config.example.yaml sends the reader to
          # Admin -> Health -> "Data capture", so the words stay as the summary of
          # this disclosure.
          tags$details(
            tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
                         "Data capture - what this server records about its own conversions"),
            div(style = "padding:8px 2px",
              helpText(HTML(paste0(
                "Every conversion can save a rich, structured record of <b>how it went</b> ",
                "(the layout it matched, how cleanly it parsed, reconciliation outcomes, OCR ",
                "signals). It is stored on <b>this machine only</b> under <code>logs/metadata/</code>, ",
                "kept forever, and <b>never enters the Qlik feed</b>. <b>No statement content is ",
                "stored</b> - only structure, counts and quality signals; any account number is ",
                "stored only as a one-way hash."))),
              fluidRow(
                column(5,
                  # ONE QUESTION, NOT TWO: the level decides, every category is
                  # captured within it.
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
                      "<b>Standard</b> - layout signature, format, row count, trust level, ",
                      "KPI pass/fail counts. No per-row detail.<br>",
                      "<b>Full</b> - adds flag histograms, per-field fill ratios, per-KPI ",
                      "outcomes, balance anchors and net amount, OCR and timing.")))))))
          )
        )
      )
      )
    )
  )
)

# ---------------------------------------------------------------------------
server <- function(input, output, session) {

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
  # .plain_tbl(df) -- an Admin table as a person reads it: plain column names
  # ("Conversions", not RUNS), status words a person says ("Done", "Needs a
  # check", "Couldn't read"), file names without their folder, and no run ids.
  PLAIN_COLS <- c(status = "How it went", n = "Conversions", runs = "Conversions", count = "Statements",
    layout = "Statement design", why = "Why", last_seen = "Last seen", example_file = "Example file",
    ts = "When", source_file = "File", message = "What happened", earlier_ok_pct = "Read on its own before (%)",
    recent_ok_pct = "Read on its own lately (%)", drop = "Drop", low_trust = "Low confidence",
    flagged_feedback = "Flagged by the team", requested_by = "Raised by", detail = "What they said",
    context = "About", size_kb = "Size (KB)", file = "File", name = "Name", modified = "Changed",
    token = "Word", column = "Column heading", pct = "Share (%)", share = "Share (%)", rating = "Rating",
    comment = "Comment", when = "When", proven = "Proven", statements = "Statements", last_used = "Last used",
    first_seen = "First seen", ok = "Done", needs_review = "Needs a check", unsupported = "Couldn't read")
  PLAIN_STATUS <- c(ok = "Done", needs_review = "Needs a check", unsupported = "Couldn't read",
                    failed = "Couldn't read", open = "Open", actioned = "Done", dismissed = "Dismissed")
  .plain_tbl <- function(df) {
    if (!is.data.frame(df)) return(df)
    df <- df[, !(tolower(names(df)) %in% c("run_id", "id", "run", "sha256", "path")), drop = FALSE]
    for (nm in names(df)) {
      v <- df[[nm]]
      if (is.character(v) || is.factor(v)) {
        v <- as.character(v)
        if (tolower(nm) %in% c("status", "outcome")) v <- ifelse(v %in% names(PLAIN_STATUS), unname(PLAIN_STATUS[v]), v)
        if (tolower(nm) %in% c("source_file", "file", "example_file")) v <- ifelse(is.na(v), v, basename(v))
        v <- gsub("(/[A-Za-z0-9._~-]+){2,}/?", "", v)            # no folder paths on screen
        v <- gsub("\\b[0-9a-f]{24,}\\b", "", v)                  # no run hashes
        v <- gsub("^failed: ", "", v)
        df[[nm]] <- v
      }
    }
    k <- tolower(names(df))
    names(df) <- ifelse(k %in% names(PLAIN_COLS), unname(PLAIN_COLS[k]),
                        { x <- gsub("_", " ", names(df)); paste0(toupper(substr(x, 1, 1)), substring(x, 2)) })
    df
  }
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
      slot$close_bar(); slot$ctx <- NULL; slot$handle(NULL); slot$live(NULL)
    }
    # LIVE STATE FOR A SCREEN THAT SHOWS ITS OWN PROGRESS. A case folder is not put
    # behind the full-screen overlay: the Convert table shows each file waiting,
    # converting, and then its verdict the moment it exists (job_done_rows), so the
    # page stays readable for the minutes a big case takes. `live` is what it reads:
    # list(state, ahead, i, n, file, done), NULL when nothing is in flight. Set only
    # when something CHANGED, so the table is not redrawn twice a second for nothing.
    slot$live <- reactiveVal(NULL)
    slot$live_update <- function(h, st) {
      cur <- isolate(slot$live()) %||% list()
      done <- cur$done
      if (identical(st, "queued")) {
        nw <- list(state = "queued", ahead = job_queue_ahead(h), i = 0L, n = cur$n,
                   file = NA_character_, done = done)
      } else {
        got <- safe(job_done_rows(h, have = slot$ctx$have %||% integer(0)), NULL)
        if (!is.null(got) && length(got$idx)) {
          slot$ctx$have <- c(slot$ctx$have, got$idx)
          done <- if (is.null(done)) got$rows else rbind(done, got$rows)
        }
        p <- job_progress(h)
        ok <- function(v) !is.null(v) && length(v) == 1L && !is.na(v)
        nw <- list(state = "running", ahead = 0L, i = if (ok(p$i)) p$i else 0L, n = if (ok(p$n)) p$n else cur$n,
                   file = p$file %||% NA_character_, says = p$says %||% NA_character_, done = done)
      }
      if (!identical(nw, cur)) slot$live(nw)
    }
    # `overlay = FALSE`: no progress panel (and so no full-screen overlay); the
    # caller's screen reads `live` instead.
    slot$start <- function(task, paths, outdir, message, finish, args = list(), overlay = TRUE) {
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
      if (isTRUE(overlay)) {
        slot$bar <- Progress$new(session)
        slot$bar$set(message = message, value = 0.15, detail = "Starting\u2026")
      } else {
        slot$live(list(state = "queued", ahead = NA_integer_, i = 0L, n = length(paths),
                       file = NA_character_, done = NULL))
      }
      slot$ctx <- list(finish = finish, have = integer(0))
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
        if (is.null(slot$bar) && !is.null(isolate(slot$live()))) slot$live_update(h, st)
        invalidateLater(JOB_POLL_MS, session)
        return()
      }
      fin <- slot$ctx$finish
      # Read the result BEFORE reaping: reaping deletes the folder it is in.
      res <- if (identical(st, "done")) job_result(h) else NULL
      if (is.null(res)) res <- job_failed_result(h)
      slot$close_bar(); slot$ctx <- NULL; slot$handle(NULL); slot$live(NULL)
      safe(job_reap(h))
      if (is.function(fin)) fin(res)
    })
    slot
  }
  cv_slot  <- job_slot()   # converting a statement, a case folder, or a re-read on Please check
  plan_slot <- job_slot()  # reading scans' first pages for the Convert table (identify_scans)
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
    p <- job_progress(h)     # a case folder reports which file it is on, and where in it
    ok <- function(v) !is.null(v) && length(v) == 1L && !is.na(v)
    # how far into the file: its page, or a fixed share for the steps without pages
    within <- if (is.null(p) || !ok(p$stage)) 0.3
              else if (p$stage %in% c("ocr", "table") && ok(p$done) && ok(p$total) && p$total > 0) 0.1 + 0.8 * p$done / p$total
              else switch(p$stage, reading = 0.05, files = 0.95, 0.3)
    batch <- !is.null(p) && ok(p$i) && ok(p$n)
    invisible(bar$set(
      value  = min(0.97, max(0.03, if (batch) (p$i - 1 + within) / max(p$n, 1) else within)),
      detail = {
        d <- paste(c(if (batch) sprintf("%d of %d - %s", p$i, p$n, p$file),
                     if (!is.null(p) && ok(p$says)) p$says), collapse = " \u00b7 ")
        if (nzchar(d)) d else "Reading the file and running the checks\u2026"
      }))
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
  # ---- Admin password gate. Hidden outputs are suspended, so no admin data is
  # computed or sent to the browser until the password is entered. Set it in
  # config/config.yaml (app.admin_password); the BSO_ADMIN_PASSWORD env var
  # overrides it if present.
  #
  # Three rules, all fail-closed:
  #  1. NO PASSWORD SET -> Admin is refused outright. The shipped placeholder is
  #     printed in the example config and the docs, so serving the learned layouts,
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
      p("Admin manages what the tool has learned for each bank, the shared label dictionary and the analytics feed, so it stays shut until this install has its own password. The one in the example settings file is printed in the documentation, so it is not a password."),
      p(HTML(paste0("To open it, do <b>either</b> of these on the server and restart the app:",
        "<ul><li>put <code>admin_password: your-password</code> under <code>app:</code> in ",
        "<code>config/config.yaml</code> (copy <code>config/config.example.yaml</code> if it isn't there yet), or</li>",
        "<li>set the <code>BSO_ADMIN_PASSWORD</code> environment variable.</li></ul>"))),
      p(class = "muted", "Everything on the Convert tab works normally without this.")))
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

  # ---- WHAT THE TOOL HAS LEARNED: one read of the layout store, shared -----------
  #
  # Every screen that names a layout -- the Convert table, the result page, Admin
  # -- reads it through here, so a layout renamed, confirmed or retired in Admin is
  # named the same way everywhere on the next redraw. layouts_bump() is pressed by
  # anything that changes the store from THIS process (Admin's buttons, training);
  # a conversion learns in its own process, so its finish presses it too.
  layouts_bump <- reactiveVal(0L)
  # Learning also happens in OTHER processes: a conversion's background job,
  # another person's session, a training run. So every open page also watches the
  # store itself, cheaply: a layout file is never edited (every change is a new
  # file), so its count and newest time say whether anything changed. The
  # tracking folder is watched the same way for the Banks table's counts.
  .store_stamp <- function(d) {
    f <- safe(list.files(d, recursive = TRUE, all.files = TRUE, full.names = TRUE), character(0))
    if (!length(f)) return("none")
    paste(length(f), format(max(file.mtime(f), na.rm = TRUE), "%Y%m%d%H%M%OS3"))
  }
  layouts_poll <- reactivePoll(5000, session,
    checkFunc = function() paste(.store_stamp(LAYOUTS_DIR), .store_stamp(TRACKING_DIR)),
    valueFunc = function() Sys.time())
  lay_all <- reactive({ layouts_bump(); layouts_poll(); safe(layouts_load(LAYOUTS_DIR, include_retired = TRUE), list()) })
  # .layout_name(ref) -- "bnz_1@3" (or "bnz_1") as the name people see. A layout the
  # store no longer holds is shown as its reference rather than as nothing: a
  # blank would hide that something read the statement.
  .layout_name <- function(ref) {
    ref <- as.character(ref %||% NA_character_)[1]
    if (is.na(ref) || !nzchar(ref)) return(NA_character_)
    ly <- lay_all()[[sub("@v?[0-9]+$", "", ref)]]
    if (is.null(ly)) ref else as.character(safe(layout_display_name(ly), ref))[1]
  }
  # .read_as_name(ref, how) -- a learned layout as the Convert table's "Read as"
  # says it: the bank and how it is known, never the engine's own name for it
  # ("BNZ layout 1: Date | ..."). Blank when the reference is not a layout.
  .read_as_name <- function(ref, how = "seen before") {
    ref <- as.character(ref %||% NA_character_)[1]
    if (is.na(ref) || !nzchar(ref)) return(NA_character_)
    ly <- lay_all()[[sub("@v?[0-9]+$", "", ref)]]
    if (is.null(ly)) return(NA_character_)
    bk <- as.character(ly$bank %||% ly$institution %||% "")[1]
    sprintf("%s statement, %s", .layout_bank_display(bk, if (nzchar(bk)) toupper(bk) else "A"), how)
  }
  # The banks a person can choose from: every NZ bank on the list, every bank the
  # store has layouts for, and any bank named in this session (cv_new_banks) --
  # by the same rule bank_choices() uses, sorted by name.
  cv_new_banks <- reactiveVal(character(0))
  # NOT the register's stand-ins ("a business that banks through ANZ"): they mark a
  # branch the clearing bank lends out, the identifier never answers with one, and
  # as the first three entries of every dropdown they read as banks to pick.
  bank_list <- reactive({
    layouts_bump(); layouts_poll()
    ch <- safe(bank_choices(LAYOUTS_DIR), character(0))
    ref <- safe(.bi_ref(), NULL)
    if (!is.null(ref)) ch <- ch[!(unname(ch) %in% names(ref$pseudo)[ref$pseudo %in% TRUE])]
    extra <- setdiff(cv_new_banks(), unname(ch))
    if (length(extra)) ch <- c(ch, stats::setNames(extra, extra))
    ch[order(tolower(names(ch)), method = "radix")]
  })
  # .bank_label(id) -- a bank id ("bnz") as its name ("BNZ"); a bank named by hand
  # is its own name.
  .bank_label <- function(id) {
    id <- as.character(id %||% NA_character_)[1]
    if (is.na(id) || !nzchar(id)) return(NA_character_)
    ch <- bank_list()
    if (id %in% ch) names(ch)[match(id, ch)] else id
  }
  # A bank typed by a person is a NAME. One holding a long run of digits is an
  # account number typed into the wrong box, and a bank's name becomes a folder,
  # layout ids and a line in the run log -- so it is refused at the door with the
  # reason (the engine refuses it too, R/convert.R).
  .bank_name_problem <- function(nm) {
    nm <- trimws(as.character(nm %||% "")[1])
    if (is.na(nm) || !nzchar(nm)) return("Type the bank's name.")
    if (grepl("[0-9][0-9 -]{3,}[0-9]", nm))
      return("That looks like an account number. Type the bank's name instead.")
    if (is.na(.layout_slug(nm))) return("A bank's name needs some letters in it.")
    NULL
  }

  # ---- Admin -> Needs attention and Recipes (D15; engine: R/recipes_admin.R) --------
  # Every change is a NEW recipe version in the server's own recipes folder; the
  # shipped recipes/ are never written. Nobody sees YAML: the card asks the same
  # plain questions Please check asks, with this recipe's answers.
  RC_DIRS <- list(shipped = if (nzchar(Sys.getenv("ENGINE_ROOT", ""))) file.path(Sys.getenv("ENGINE_ROOT"), "recipes") else "recipes",
                  server = safe(recipes_state_dir(LAYOUTS_DIR, CONFIG), NULL))
  rc_bump <- reactiveVal(0L)
  rc_poll <- reactivePoll(5000, session,
    checkFunc = function() paste(.store_stamp(RC_DIRS$server %||% tempdir()), .store_stamp(TRACKING_DIR),
                                 .store_stamp(UPLOADS_DIR)),
    valueFunc = function() Sys.time())
  .rc_changed <- function() { rc_bump(isolate(rc_bump()) + 1L); layouts_bump(isolate(layouts_bump()) + 1L) }
  rc_ov <- reactive({ req(admin_ok()); rc_bump(); rc_poll()
    safe(recipes_overview(RC_DIRS, TRACKING_DIR, 30, hidden = FALSE), NULL) })
  adm_na <- reactive({ req(admin_ok()); rc_bump(); rc_poll()
    safe(needs_attention(RC_DIRS, TRACKING_DIR, UPLOADS_DIR, 30), NULL) })
  # A button that tells the server which item it is for, in one event. Ids are
  # recipe ids and upload ids: plain slugs, refused otherwise.
  .act_btn <- function(input_id, value, label, cls = "btn-default btn-sm") {
    if (!grepl("^[A-Za-z0-9_.|:-]+$", value)) return(NULL)
    tags$button(type = "button", class = paste("btn", cls), `data-act` = value,
                onclick = sprintf("Shiny.setInputValue('%s','%s',{priority:'event'})", input_id, value), label)
  }
  .na_card <- function(title, n, empty, items) div(class = "na-card",
    div(class = "na-head", span(class = "na-count", n), span(title)),
    if (!n) p(class = "muted", style = "margin:4px 0 0", empty)
    else tags$ul(class = "na-items", items))
  .na_item <- function(words, ...) tags$li(span(class = "na-words", words), span(class = "na-btns", ...))
  output$adm_na_cards <- renderUI({
    req(admin_ok())
    na <- adm_na()
    if (is.null(na)) return(p(class = "bad", "What needs attention could not be worked out."))
    up <- na$set_aside; dr <- na$drafts; fl <- na$failing; mg <- na$merges
    cards <- list(
      if (nrow(up)) .na_card("Statements waiting for a look", nrow(up), "",
        lapply(seq_len(nrow(up)), function(i) .na_item(
          tagList(sprintf("Set aside %s (%s)", as.character(safe(local_time_text(up$ts[i]), up$ts[i]))[1], toupper(up$file_ext[i] %||% "")),
            if (!is.na(up$note[i] %||% NA) && nzchar(up$note[i])) div(class = "na-note", sprintf("\u201c%s\u201d", up$note[i]))),
          .act_btn("adm_na_act", paste0("fix|", up$id[i]), "Fix", "btn-primary btn-sm")))),
      if (nrow(dr)) .na_card("New statement designs waiting for you", nrow(dr), "",
        lapply(seq_len(nrow(dr)), function(i) .na_item(
          sprintf("%s %s (%d checked)", dr$bank[i], dr$name[i], as.integer(dr$proofs[i])),
          .act_btn("adm_na_act", paste0("accept|", dr$id[i]), "Accept", "btn-primary btn-sm"),
          .act_btn("adm_na_act", paste0("retire|", dr$id[i]), "Retire")))),
      if (nrow(fl)) .na_card("Designs that stopped adding up", nrow(fl), "",
        lapply(seq_len(nrow(fl)), function(i) .na_item(
          sprintf("%s %s: %d statement%s did not add up. The bank may have changed the design.", fl$bank[i], fl$name[i],
                  as.integer(fl$tried_not_proven[i]), if (fl$tried_not_proven[i] == 1L) "" else "s"),
          .act_btn("adm_na_act", paste0("open|", fl$id[i]), "Fix", "btn-primary btn-sm")))),
      if (nrow(mg)) .na_card("Designs that look like one", nrow(mg), "",
        lapply(seq_len(nrow(mg)), function(i) .na_item(
          sprintf("%s: %s and %s read the same design", mg$bank[i], .rc_title(mg$a[i]), .rc_title(mg$b[i])),
          .act_btn("adm_na_act", paste0("merge|", mg$a[i], ":", mg$b[i]), "Merge", "btn-default btn-sm")))))
    cards <- Filter(Negate(is.null), cards)
    tagList(
    p(class = "na-week", na$week %||% ""),
    if (!length(cards)) div(class = "na-calm", span(class = "na-calm-tick", "\u2713"), "Nothing waiting \u2014 all good")
    else div(class = "na-grid", cards))
  })
  .rc_title <- function(id) {
    ov <- rc_ov(); i <- if (is.data.frame(ov)) match(id, ov$id) else NA
    if (is.na(i)) id else ov$name[i]
  }
  adm_na_msg <- reactiveVal(NULL)
  output$adm_na_msg <- renderUI({ m <- adm_na_msg(); if (is.null(m)) return(NULL)
    div(class = if (isTRUE(m$ok)) "note" else "note-bad", style = "margin:8px 0", m$text) })
  .rc_say <- function(rv, r) rv(list(ok = isTRUE(r$ok), text = r$why %||% "It could not be done."))
  observeEvent(input$adm_na_act, {
    req(admin_ok())
    v <- as.character(input$adm_na_act %||% "")[1]
    kind <- sub("[|].*$", "", v); id <- sub("^[^|]*[|]", "", v)
    if (identical(kind, "fix")) {
      p <- upload_file_path(id, UPLOADS_DIR)
      if (is.na(p) || !file.exists(p)) { adm_na_msg(list(ok = FALSE, text = "That statement's file is no longer kept.")); return() }
      .reread_on_convert(p, basename(p), upload_id = id)
      return()
    }
    if (identical(kind, "open")) { .rc_open(id); updateTabsetPanel(session, "adm_tabs", selected = "Recipes"); return() }
    r <- switch(kind,
      accept = safe(recipe_accept(id, RC_DIRS), NULL),
      retire = safe(recipe_retire(id, RC_DIRS), NULL),
      merge = { ab <- strsplit(id, ":", fixed = TRUE)[[1]]; if (length(ab) == 2L) safe(recipe_merge(ab[1], ab[2], RC_DIRS), NULL) },
      NULL)
    .rc_say(adm_na_msg, r %||% list(ok = FALSE, why = "It could not be done."))
    .rc_changed()
  })

  # ---- the list: one row per recipe, grouped by bank ----
  # A quiet switch per row (no label, grey/green), a Status pill, numbers on the
  # right; drafts first under "Waiting for you"; each bank folds away with its
  # count; a search box finds a bank or a statement design. On a phone each row
  # stacks (data-label on each cell).
  .rc_status_pill <- function(enabled, status) {
    if (!enabled) span(class = "pill pill-muted", "Off")
    else if (identical(status, "draft")) span(class = "pill pill-warn", "Draft")
    else span(class = "pill pill-ok", "Proven")
  }
  .rc_switch <- function(id, on) {
    if (!grepl("^[A-Za-z0-9_.|:-]+$", id)) return(NULL)
    tags$button(type = "button", role = "switch", class = paste("rc-toggle", if (on) "rc-on" else "rc-off"),
                `aria-checked` = if (on) "true" else "false", `aria-label` = if (on) "Turn off" else "Turn on",
                title = if (on) "On \u2014 click to turn off" else "Off \u2014 click to turn on",
                onclick = sprintf("Shiny.setInputValue('adm_rc_toggle','%s',{priority:'event'})", id),
                span(class = "rc-knob"))
  }
  .rc_rows <- function(ov) lapply(seq_len(nrow(ov)), function(i) {
    id <- ov$id[i]
    tags$tr(class = paste(if (identical(rc_sel(), id)) "rc-picked", if (!ov$enabled[i]) "rc-row-off"), `data-recipe` = id,
            `data-find` = tolower(paste(ov$bank[i], ov$name[i], ov$title[i])),
      tags$td(`data-label` = "Bank", ov$bank[i]),
      tags$td(`data-label` = "Statement design", tags$a(href = "#", class = "rc-open",
                     onclick = sprintf("Shiny.setInputValue('adm_rc_open','%s',{priority:'event'});return false;", id),
                     ov$name[i])),
      tags$td(`data-label` = "Used", .rc_switch(id, ov$enabled[i])),
      tags$td(class = "num", `data-label` = "Read on its own", ov$read[i] - ov$needed_help[i]),
      tags$td(class = "num", `data-label` = "Needed a person (30 days)", ov$needed_help[i]),
      tags$td(`data-label` = "Status", .rc_status_pill(ov$enabled[i], ov$status[i])))
  })
  .rc_head <- function() tags$thead(tags$tr(tags$th("Bank"), tags$th("Statement design"), tags$th("Used"),
    tags$th(class = "num", "Read on its own"), tags$th(class = "num", "Needed a person (30 days)"), tags$th("Status")))
  output$adm_rc_list <- renderUI({
    req(admin_ok())
    ov <- rc_ov()
    if (!is.data.frame(ov)) return(p(class = "bad", "The recipes could not be read."))
    ov <- ov[!ov$hidden, , drop = FALSE]
    if (!nrow(ov)) return(p(class = "muted", "No statement designs yet."))
    ov <- ov[order(tolower(ov$bank), tolower(ov$name)), , drop = FALSE]
    wait <- ov[ov$enabled & ov$status == "draft", , drop = FALSE]
    rest <- ov[!(ov$id %in% wait$id), , drop = FALSE]
    banks <- unique(rest$bank)
    div(class = "rc-wrap",
      div(class = "rc-find", tags$input(type = "search", id = "adm_rc_find", class = "form-control",
        placeholder = "Find a bank or statement", `aria-label` = "Find a bank or statement",
        oninput = "ssRcFind(this.value)")),
      if (nrow(wait)) div(class = "rc-group rc-waiting",
        div(class = "rc-group-head", "Waiting for you", span(class = "rc-count", nrow(wait))),
        tags$table(class = "rc-table", .rc_head(), tags$tbody(.rc_rows(wait)))),
      lapply(banks, function(b) {
        g <- rest[rest$bank == b, , drop = FALSE]
        tags$details(class = "rc-group", open = NA,
          tags$summary(class = "rc-group-head", b, span(class = "rc-count", nrow(g))),
          tags$table(class = "rc-table", .rc_head(), tags$tbody(.rc_rows(g))))
      }),
      tags$script(HTML("window.ssRcFind=function(q){q=(q||'').toLowerCase().trim();document.querySelectorAll('#adm_rc_list tr[data-find]').forEach(function(r){r.style.display=!q||r.dataset.find.indexOf(q)>=0?'':'none';});document.querySelectorAll('#adm_rc_list .rc-group').forEach(function(g){var any=[].some.call(g.querySelectorAll('tr[data-find]'),function(r){return r.style.display!=='none';});g.style.display=any?'':'none';if(q&&g.tagName==='DETAILS')g.open=true;});};")))
  })
  observeEvent(input$adm_rc_toggle, {
    req(admin_ok())
    id <- as.character(input$adm_rc_toggle)[1]
    ov <- rc_ov(); i <- match(id, ov$id); req(!is.na(i))
    r <- safe(recipe_set_enabled(id, !ov$enabled[i], RC_DIRS), list(ok = FALSE, why = "It could not be switched."))
    .rc_say(rc_msg, r); if (!identical(rc_sel(), id)) rc_msg(NULL)
    adm_na_msg(NULL); .rc_changed()
    if (!isTRUE(r$ok)) showNotification(r$why, type = "error")
  })

  # ---- the recipe card ----
  rc_sel <- reactiveVal(NULL)
  rc_words <- reactiveVal(character(0))
  rc_sample <- reactiveVal(NULL)
  rc_msg <- reactiveVal(NULL)
  rc_test_msg <- reactiveVal(NULL)
  .rc_open <- function(id) {
    rc_sel(id); rc_msg(NULL); rc_test_msg(NULL); rc_sample(NULL)
    cd <- safe(recipe_card(id, RC_DIRS), NULL)
    rc_words(as.character(cd$recognise %||% character(0)))
    session$sendCustomMessage("ss-scroll", "adm_rc_card")
  }
  observeEvent(input$adm_rc_open, { req(admin_ok()); .rc_open(as.character(input$adm_rc_open)[1]) })
  rc_card <- reactive({ req(admin_ok()); rc_bump(); id <- rc_sel(); req(id)
    cd <- safe(recipe_card(id, RC_DIRS), NULL)
    if (is.null(cd) || !is.null(cd$error)) NULL else cd })
  # after a save or an undo, the words on the card are the saved ones
  observeEvent(rc_bump(), { cd <- isolate(tryCatch(rc_card(), error = function(e) NULL))
    if (!is.null(cd)) rc_words(as.character(cd$recognise)) }, ignoreInit = TRUE)
  RC_ROLE_CHOICES <- c("Date" = "date", "Description" = "description", "Money going out" = "money out",
                       "Money coming in" = "money in", "Money in and out, in one column" = "amount",
                       "Balance" = "balance", "Second date" = "second date", "Particulars" = "particulars",
                       "Code" = "code", "Reference" = "reference", "Other party" = "other party",
                       "Type" = "type", "Something else - ignore it" = "other")
  output$adm_rc_card <- renderUI({
    req(admin_ok())
    cd <- rc_card(); req(cd)
    ov <- rc_ov()
    others <- if (is.data.frame(ov)) ov[ov$id != cd$id & ov$enabled & !ov$hidden & ov$bank == cd$bank, , drop = FALSE] else NULL
    s <- rc_sample()
    has_page <- !is.null(s) && identical(s$id, cd$id) && length(s$pages)
    div(class = "rc-card", id = "adm_rc_card_box",
      # (a) the header: name, bank, status and the on/off switch
      div(class = "rc-card-head",
        div(h4(class = "rc-card-title", cd$name), div(class = "rc-card-bank", cd$bank)),
        div(class = "rc-card-state", .rc_status_pill(cd$enabled, cd$status), .rc_switch(cd$id, cd$enabled))),
      uiOutput("adm_rc_msg"),
      # (b) how it reads: the page beside the column questions
      tags$section(class = "rc-sec",
        h5(class = "rc-sec-title", "How it reads"),
        fluidRow(
          column(7,
            if (has_page) tagList(
              if (length(s$pages) > 1L) radioButtons("adm_rc_page", NULL, inline = TRUE, selected = s$page,
                                                      choiceValues = as.list(s$pages), choiceNames = as.list(sprintf("Page %d", s$pages))),
              plotOutput("adm_rc_plot", height = "auto"))
            else p(class = "muted", "Try a statement below to see its page here, with the columns numbered.")),
          column(5,
            textInput("adm_rc_title", "Name", cd$name, width = "100%"),
            if (!nrow(cd$columns)) p(class = "muted", "This design names no columns.")
            else lapply(seq_len(nrow(cd$columns)), function(j) selectInput(paste0("adm_rc_role_", j),
              sprintf("Column %d%s", j, if (nzchar(cd$columns$heading[j])) sprintf(" \u00b7 headed \u201c%s\u201d", substr(cd$columns$heading[j], 1, 40)) else ""),
              RC_ROLE_CHOICES, selected = cd$columns$role[j], width = "100%")),
            textInput("adm_rc_date", "How is a date printed? (an example)", cd$date, width = "100%"),
            radioButtons("adm_rc_money", "How is money shown?", inline = FALSE, selected = cd$money,
              choiceNames = list("Money out and money in, in columns of their own", "One amount column"),
              choiceValues = list("money out and money in", "one amount"))))),
      # (c) recognised by: the words
      tags$section(class = "rc-sec",
        h5(class = "rc-sec-title", "Recognised by these words"),
        div(class = "rc-chips", lapply(rc_words(), function(w) span(class = "rc-chip", w,
          tags$button(type = "button", class = "rc-chip-x", title = "Remove", `aria-label` = paste("Remove", w),
                      onclick = sprintf("Shiny.setInputValue('adm_rc_word_rm',%s,{priority:'event'})",
                                        jsonlite::toJSON(w, auto_unbox = TRUE)), "\u00d7")))),
        div(class = "rc-addword",
          div(style = "flex:1 1 160px", textInput("adm_rc_word_new", NULL, "", placeholder = "Add a word the statement prints", width = "100%")),
          actionButton("adm_rc_word_add", "Add", class = "btn-default"))),
      # (d) try it: one drop zone and Test
      tags$section(class = "rc-sec",
        h5(class = "rc-sec-title", "Try it"),
        div(class = "rc-test dropzone-wrap",
          fileInput("adm_rc_test_file", NULL, accept = c(".pdf", ".csv", ".xlsx", ".xls"), width = "100%",
                    buttonLabel = "Choose a statement", placeholder = "or drop one here"),
          actionButton("adm_rc_test", "Test", class = "btn-default"),
          uiOutput("adm_rc_test_msg"))),
      # (e) history and more, folded away
      tags$details(class = "rc-sec rc-more",
        tags$summary("History and more"),
        h5(class = "rc-sec-title", "Versions"),
        tags$table(class = "split-table rc-versions",
          tags$tbody(lapply(seq_len(min(6L, nrow(cd$versions))), function(i) tags$tr(
            tags$td(sprintf("Version %d", cd$versions$version[i])), tags$td(cd$versions$what[i]),
            tags$td(cd$versions$when[i]))))),
        div(class = "rc-more-actions",
          if (nrow(cd$versions) > 1L) actionButton("adm_rc_undo", "Undo the last change", class = "btn-default btn-sm"),
          actionButton("adm_rc_onoff", if (cd$enabled) "Turn off" else "Turn on", class = "btn-default btn-sm")),
        if (!is.null(others) && nrow(others)) div(class = "rc-addword",
          div(style = "flex:1 1 160px", selectInput("adm_rc_merge_with", "Merge with\u2026",
            stats::setNames(others$id, others$name), width = "100%")),
          actionButton("adm_rc_merge", "Merge", class = "btn-default", style = "margin-bottom:15px"))),
      uiOutput("adm_rc_savebar"))
  })
  # one sticky primary, only once something on the card has changed
  output$adm_rc_savebar <- renderUI({
    req(admin_ok()); cd <- rc_card(); req(cd)
    if (!length(.rc_changes())) return(NULL)
    div(class = "rc-savebar", span(class = "muted", "You have changes that are not saved yet."),
        actionButton("adm_rc_save", "Save changes", class = "btn-primary"))
  })
  output$adm_rc_msg <- renderUI({ m <- rc_msg(); if (is.null(m)) return(NULL)
    div(class = if (isTRUE(m$ok)) "note" else "note-bad", style = "margin:6px 0", m$text) })
  output$adm_rc_test_msg <- renderUI({ m <- rc_test_msg(); if (is.null(m)) return(NULL)
    div(class = if (isTRUE(m$ok)) "note" else "note-bad", style = "margin:6px 0", m$text) })
  observeEvent(input$adm_rc_word_rm, { req(admin_ok())
    w <- as.character(input$adm_rc_word_rm)[1]; rc_words(setdiff(rc_words(), w)) })
  observeEvent(input$adm_rc_word_add, { req(admin_ok())
    w <- trimws(input$adm_rc_word_new %||% "")
    if (!nzchar(w)) return()
    if (grepl("[0-9][0-9 -]{3,}[0-9]", w)) {
      rc_msg(list(ok = FALSE, text = "A word that recognises a design must not hold a long number - it could be an account.")); return() }
    if (!(tolower(w) %in% tolower(rc_words()))) rc_words(c(rc_words(), w))
    updateTextInput(session, "adm_rc_word_new", value = "")
  })
  # .rc_changes() -- what the card says now that the saved recipe does not, as
  # recipe_update takes it. Empty: nothing changed.
  .rc_changes <- function() {
    cd <- rc_card(); ch <- list()
    t <- trimws(input$adm_rc_title %||% cd$name)
    if (nzchar(t) && !identical(t, cd$name)) ch$title <- t
    if (nrow(cd$columns)) {
      roles <- vapply(seq_len(nrow(cd$columns)), function(j) as.character(input[[paste0("adm_rc_role_", j)]] %||% cd$columns$role[j])[1], "")
      if (!identical(roles, cd$columns$role)) ch$columns <- roles
    }
    d <- trimws(input$adm_rc_date %||% cd$date)
    if (nzchar(d) && !identical(d, cd$date)) ch$date_format <- d
    m <- input$adm_rc_money %||% cd$money
    if (!identical(m, cd$money)) ch$money_style <- m
    w <- rc_words()
    add <- w[!(tolower(w) %in% tolower(cd$recognise))]; rm <- cd$recognise[!(tolower(cd$recognise) %in% tolower(w))]
    if (length(add)) ch$recognise_add <- add
    if (length(rm)) ch$recognise_remove <- rm
    ch
  }
  observeEvent(input$adm_rc_save, {
    req(admin_ok()); cd <- rc_card(); req(cd)
    ch <- .rc_changes()
    if (!length(ch)) { rc_msg(list(ok = TRUE, text = "Nothing was changed, so nothing was saved.")); return() }
    r <- safe(recipe_update(cd$id, ch, RC_DIRS), list(ok = FALSE, why = "It could not be saved."))
    rc_msg(list(ok = isTRUE(r$ok), text = if (isTRUE(r$ok)) "Saved as a new version. Undo brings the last one back." else r$why))
    if (isTRUE(r$ok)) .rc_changed()
  })
  observeEvent(input$adm_rc_undo, {
    req(admin_ok()); cd <- rc_card(); req(cd)
    r <- safe(recipe_undo(cd$id, RC_DIRS), list(ok = FALSE, why = "It could not be undone."))
    .rc_say(rc_msg, r); if (isTRUE(r$ok)) .rc_changed()
  })
  observeEvent(input$adm_rc_onoff, {
    req(admin_ok()); cd <- rc_card(); req(cd)
    r <- safe(recipe_set_enabled(cd$id, !cd$enabled, RC_DIRS), list(ok = FALSE, why = "It could not be switched."))
    .rc_say(rc_msg, r); if (isTRUE(r$ok)) .rc_changed()
  })
  observeEvent(input$adm_rc_merge, {
    req(admin_ok()); cd <- rc_card(); req(cd)
    other <- as.character(input$adm_rc_merge_with %||% "")[1]; req(nzchar(other))
    r <- safe(recipe_merge(cd$id, other, RC_DIRS), list(ok = FALSE, why = "They could not be merged."))
    .rc_say(rc_msg, r)
    if (isTRUE(r$ok)) { .rc_changed(); if (!identical(r$kept, cd$id)) rc_sel(r$kept) }
  })
  observeEvent(input$adm_rc_test, {
    req(admin_ok()); cd <- rc_card(); req(cd)
    f <- input$adm_rc_test_file
    if (is.null(f) || !nrow(f)) { rc_test_msg(list(ok = FALSE, text = "Add a statement to test with first.")); return() }
    path <- f$datapath[1]
    r <- withProgress(message = "Testing\u2026", value = 0.5, {
      inp <- safe(read_input(path), NULL)
      if (is.null(inp)) list(outcome = "check", why = "That file could not be opened.")
      else safe(recipe_test(cd$id, inp, RC_DIRS, changes = .rc_changes()),
                list(outcome = "check", why = "It could not be tested."))
    })
    rc_test_msg(list(ok = identical(r$outcome, "proven"), text = r$why))
    cols <- r$reading$columns
    pg <- if (is.data.frame(cols) && nrow(cols)) sort(unique(as.integer(cols$page))) else integer(0)
    rc_sample(if (length(pg) && grepl("[.]pdf$", tolower(path))) list(id = cd$id, path = path, reading = r$reading, pages = pg, page = pg[1]) else NULL)
  })
  observeEvent(input$adm_rc_page, { req(admin_ok()); s <- rc_sample(); pg <- suppressWarnings(as.integer(input$adm_rc_page))
    if (!is.null(s) && !is.na(pg)) { s$page <- pg; rc_sample(s) } })
  output$adm_rc_plot <- renderPlot({
    s <- rc_sample(); cd <- rc_card(); req(s, cd, file.exists(s$path))
    r <- render_page_view(s$path, s$page, 100); req(r)
    cols <- s$reading$columns; cols <- cols[cols$page %in% s$page, , drop = FALSE]
    n <- match(cols$field, cd$columns$field)
    .draw_page_columns(r, cols, ifelse(is.na(n), plain_column(cols$field),
                                       sprintf("%d %s", n, .col_short(cols$field))))
  }, height = function() .plot_height(session$clientData$output_adm_rc_plot_width,
                                      tryCatch({ s <- rc_sample(); render_page_view(s$path, s$page, 100) }, error = function(e) NULL)))

  # ---- a new recipe from a statement: never a blank form ----
  rc_new_open <- reactiveVal(FALSE)
  rc_new <- reactiveVal(NULL)
  rc_new_msg <- reactiveVal(NULL)
  observeEvent(input$adm_rc_new_open, { req(admin_ok()); rc_new_open(!isTRUE(rc_new_open())); rc_new(NULL); rc_new_msg(NULL) })
  output$adm_rc_new <- renderUI({
    req(admin_ok()); if (!isTRUE(rc_new_open())) return(NULL)
    div(class = "rc-card",
      h4(style = "margin-top:0", "New recipe from a statement"),
      p(class = "muted", "Add one statement of the new design. The tool reads it and fills in the answers; check them, Test, then Save."),
      fluidRow(
        column(4, selectizeInput("adm_rc_new_bank", "Bank", choices = c("", bank_list()), width = "100%",
                                 options = list(create = TRUE, placeholder = "Pick a bank, or type a new one's name"))),
        column(4, textInput("adm_rc_new_name", "What kind of statement is it?", "", placeholder = "e.g. Everyday account", width = "100%")),
        column(4, fileInput("adm_rc_new_file", "The statement (a PDF)", accept = ".pdf", width = "100%"))),
      actionButton("adm_rc_new_read", "Read it", class = "btn-primary"),
      uiOutput("adm_rc_new_body"))
  })
  .rc_new_roles <- function() {
    n <- rc_new(); m <- .money_cols(n$reading)
    if (!nrow(m)) return(NULL)
    roles <- vapply(m$field, function(f) as.character(input[[paste0("adm_rc_new_role_", f)]] %||% .field_role(f))[1], "")
    if (identical(unname(roles), vapply(m$field, .field_role, "", USE.NAMES = FALSE))) return(NULL)
    stats::setNames(roles, m$field)
  }
  .rc_new_read <- function(roles = NULL) {
    f <- input$adm_rc_new_file; bank <- trimws(input$adm_rc_new_bank %||% "")
    if (!nzchar(bank)) { rc_new_msg(list(ok = FALSE, text = "Say which bank the statement is from first.")); return() }
    pb <- .bank_name_problem(bank); if (!is.null(pb)) { rc_new_msg(list(ok = FALSE, text = pb)); return() }
    if (is.null(f) || !nrow(f)) { rc_new_msg(list(ok = FALSE, text = "Add the statement first.")); return() }
    r <- withProgress(message = "Reading it\u2026", value = 0.5, {
      inp <- safe(read_input(f$datapath[1]), NULL)
      if (is.null(inp)) list(outcome = "check", why = "That file could not be opened.")
      else c(safe(recipe_preview(inp, bank, roles), list(outcome = "check", why = "It could not be read.")), list(input = inp))
    })
    rc_new(list(reading = r$reading, input = r$input, path = f$datapath[1], bank = bank, roles = roles,
                page = as.integer((r$reading$columns$page %||% 1L)[1])))
    rc_new_msg(list(ok = identical(r$outcome, "proven"), text = r$why))
  }
  observeEvent(input$adm_rc_new_read, { req(admin_ok()); .rc_new_read(NULL) })
  observeEvent(input$adm_rc_new_test, { req(admin_ok()); req(rc_new()); .rc_new_read(.rc_new_roles()) })
  observeEvent(input$adm_rc_new_save, {
    req(admin_ok()); n <- rc_new(); req(n, n$input)
    r <- withProgress(message = "Saving\u2026", value = 0.5,
      safe(recipe_from_statement(n$input, list(bank = n$bank, roles = n$roles, title = trimws(input$adm_rc_new_name %||% "")), RC_DIRS),
           list(ok = FALSE, why = "It could not be saved.")))
    rc_new_msg(list(ok = isTRUE(r$ok), text = r$why))
    if (isTRUE(r$ok)) {
      .rc_changed(); rc_new_open(FALSE); rc_new(NULL)
      .rc_open(r$id %||% sub("@v?[0-9]+$", "", r$ref)); rc_msg(list(ok = TRUE, text = r$why))
    }
  })
  output$adm_rc_new_body <- renderUI({
    m <- rc_new_msg(); n <- rc_new()
    tagList(
      if (!is.null(m)) div(class = if (isTRUE(m$ok)) "note" else "note-bad", style = "margin:8px 0", m$text),
      if (!is.null(n) && !is.null(n$reading)) {
        money <- .money_cols(n$reading)
        fluidRow(
          column(7, if (is.data.frame(n$reading$columns) && nrow(n$reading$columns)) plotOutput("adm_rc_new_plot", height = "auto")),
          column(5,
            h5("What is each column?"),
            if (!nrow(money)) p(class = "muted", "No column of figures was found on this statement.")
            else lapply(seq_len(nrow(money)), function(j) {
              f <- money$field[j]
              sel <- if (!is.null(n$roles) && f %in% names(n$roles)) n$roles[[f]] else .field_role(f)
              selectInput(paste0("adm_rc_new_role_", f), sprintf("Column %d of %d", j, nrow(money)),
                          stats::setNames(names(ROLE_PLAIN), unname(ROLE_PLAIN)), selected = sel, width = "100%")
            }),
            div(style = "display:flex;gap:8px;flex-wrap:wrap;margin:8px 0",
              if (nrow(money)) actionButton("adm_rc_new_test", "Test", class = "btn-default"),
              actionButton("adm_rc_new_save", "Save", class = "btn-primary"))))
      })
  })
  output$adm_rc_new_plot <- renderPlot({
    n <- rc_new(); req(n, file.exists(n$path %||% ""))
    r <- render_page_view(n$path, n$page, 100); req(r)
    cols <- n$reading$columns; cols <- cols[cols$page %in% n$page, , drop = FALSE]
    qn <- match(cols$field, .money_cols(n$reading)$field)
    .draw_page_columns(r, cols, ifelse(is.na(qn), plain_column(cols$field), sprintf("%d %s", qn, .col_short(cols$field))))
  }, height = function() .plot_height(session$clientData$output_adm_rc_new_plot_width,
                                      tryCatch({ n <- rc_new(); render_page_view(n$path, n$page, 100) }, error = function(e) NULL)))

  observe({
    req(admin_ok())
    .fill_pick(session, "adm_train_bank", bank_list(), empty = "Type the bank's name")
  })

  # ---- a learned layout, retired from Health ----
  observe({
    req(admin_ok())
    ls <- Filter(function(l) !identical(l$layout$status, "retired"), lay_all())
    ch <- vapply(ls, function(l) as.character(l$layout$id)[1], "")
    names(ch) <- vapply(ls, function(l) as.character(safe(layout_display_name(l), l$layout$id))[1], "")
    .fill_pick(session, "adm_layout_pick", ch, empty = "No learned layouts")
  })
  observeEvent(input$adm_layout_retire, {
    req(admin_ok())
    id <- as.character(input$adm_layout_pick %||% "")[1]
    if (!nzchar(id)) { output$adm_layout_msg <- renderUI(div(class = "bad", "Pick a layout first.")); return() }
    r <- safe(layout_retire(id, LAYOUTS_DIR, by = who_now()), list(ok = FALSE, why = "It could not be retired."))
    if (isTRUE(r$ok)) layouts_bump(isolate(layouts_bump()) + 1L)
    output$adm_layout_msg <- renderUI(div(class = if (isTRUE(r$ok)) "ok" else "bad",
      if (isTRUE(r$ok)) sprintf("%s is retired: it is no longer used to read statements. Conversions already issued are unchanged.", .layout_name(id) %||% id)
      else r$why %||% "It could not be retired."))
  })

  # ---- fixes held for an admin (R/fixes.R) ----
  adm_fix_bump <- reactiveVal(0L)
  adm_fix_list <- reactive({ req(admin_ok()); adm_fix_bump(); layouts_bump()
    safe(fixes_pending(LAYOUTS_DIR), data.frame()) })
  .FIX_KIND_PLAIN <- c(roles = "columns' roles set, but not proven", confirm = "confirmed as right")
  output$adm_fixes <- renderDT({
    f <- adm_fix_list()
    heads <- c("Bank", "What the person did", "Who", "When")
    if (!is.data.frame(f) || !nrow(f))
      return(datatable(stats::setNames(data.frame(matrix(character(0), 0, length(heads))), heads),
                       rownames = FALSE, selection = "none",
                       options = dt_none_opts("Nothing is waiting.", dom = "t")))
    d <- data.frame(vapply(f$bank, function(b) .bank_label(b) %||% b, ""),
                    plain_label(f$kind, .FIX_KIND_PLAIN), ifelse(is.na(f$by), "-", f$by),
                    as.character(safe(local_time_text(f$held), f$held)), stringsAsFactors = FALSE)
    names(d) <- heads
    datatable(d, rownames = FALSE, selection = "single", options = list(dom = "tip", pageLength = 10))
  })
  # the two buttons only when there is a fix to act on
  output$adm_fix_btns <- renderUI({
    f <- adm_fix_list(); if (!is.data.frame(f) || !nrow(f)) return(NULL)
    div(style = "margin-top:8px;display:flex;gap:8px",
      actionButton("adm_fix_accept", "Accept the selected fix", class = "btn-primary"),
      actionButton("adm_fix_discard", "Discard it", class = "btn-default"))
  })
  .fix_act <- function(fun, done) {
    req(admin_ok())
    f <- adm_fix_list(); i <- input$adm_fixes_rows_selected
    if (!is.data.frame(f) || !nrow(f) || !length(i) || i[1] > nrow(f)) {
      output$adm_fix_msg <- renderUI(div(class = "bad", "Click a fix in the table first.")); return()
    }
    r <- safe(fun(f$id[i[1]]), list(ok = FALSE, why = "It could not be done."))
    adm_fix_bump(isolate(adm_fix_bump()) + 1L); layouts_bump(isolate(layouts_bump()) + 1L)
    output$adm_fix_msg <- renderUI(div(class = if (isTRUE(r$ok)) "ok" else "bad",
      if (isTRUE(r$ok)) done(r) else r$why %||% "It could not be done."))
  }
  observeEvent(input$adm_fix_accept, .fix_act(
    function(id) fix_accept(id, LAYOUTS_DIR, by = who_now()),
    function(r) sprintf("%s statement design saved", .layout_bank_display(sub("_[0-9]+(@.*)?$", "", as.character(r$ref %||% "")[1]))))) 
  observeEvent(input$adm_fix_discard, .fix_act(
    function(id) fix_discard(id, LAYOUTS_DIR),
    function(r) "Discarded. Nothing was learned from it."))

  # ---- Train a bank: many statements, one background job --------------------------
  #
  # THE CASE-FOLDER MACHINERY, NOT A SECOND PIPELINE. Training is convert_batch()
  # with the admin's bank on every file: each statement is read, proven, and
  # learned from exactly as a conversion on Convert would be (only a proven reading
  # teaches; a statement that names another bank teaches nothing until someone
  # confirms it). It runs in its own slot, so it neither waits for nor cancels an
  # audit, and it writes only what a conversion writes -- the run log, tracking,
  # and the layouts. The workbooks it makes go with its scratch folder: nobody asked
  # for them, and nothing is fed to the dashboards.
  train_slot <- job_slot()
  adm_train <- reactiveVal(NULL)     # list(bank, n, b) once a run has finished
  observeEvent(input$adm_train_go, {
    req(admin_ok())
    bank <- trimws(input$adm_train_bank %||% "")
    fs <- input$adm_train_files
    why <- .bank_name_problem(bank)
    if (!is.null(why)) { notify_once("adm_train", paste("Pick the bank first.", why), duration = 8); return() }
    if (is.null(fs) || !nrow(fs)) { notify_once("adm_train", "Add the bank's statements first.", duration = 6); return() }
    if (nrow(fs) > TRAIN_MAX_FILES) {
      notify_once("adm_train", sprintf("%d files chosen - train on up to %d at a time, and add the rest after.",
                                       nrow(fs), TRAIN_MAX_FILES), duration = 10)
      return()
    }
    if (!is.null(isolate(train_slot$handle()))) {
      notify_once("adm_train", "A training run is already going - it finishes first.", type = "message"); return()
    }
    sess <- tempfile("ba_"); dir.create(file.path(sess, "in"), recursive = TRUE, showWarnings = FALSE)
    nms <- .unique_names(fs$name)
    paths <- file.path(sess, "in", nms)
    file.copy(as.character(fs$datapath), paths, overwrite = TRUE)
    adm_train(NULL)
    who <- who_now()
    train_slot$start("batch", paths, sess, overlay = FALSE, message = "",
      args = list(requested_by = who, logdir = LOGDIR, layouts_dir = LAYOUTS_DIR,
                  tracking_dir = TRACKING_DIR, formats = "csv",
                  banks = rep(bank, length(paths))),
      finish = function(b) {
        safe(unlink(sess, recursive = TRUE))
        layouts_bump(isolate(layouts_bump()) + 1L)
        adm_train(list(bank = bank, n = length(paths), names = nms, b = b))
      })
  })
  # .train_report(t) -- what a training run found, statement by statement: the
  # layouts its statements matched or started, how many proved themselves, and
  # each one that did not, with the reader's reason.
  .train_report <- function(t) {
    b <- t$b
    if (!is.data.frame(b)) return(list(error = paste(c(b$messages, CONVERT_STOPPED)[1])))
    sts <- do.call(rbind, lapply(seq_len(nrow(b)), function(i) {
      r <- b$result[[i]]
      rd <- r$reading %||% list()
      if (!length(rd))
        return(data.frame(file = t$names[i], statement = NA_integer_, outcome = "unread",
                          layout = NA_character_, held = FALSE,
                          why = sub("^[a-z_]+:\\s*", "", as.character(r$reason %||% r$messages %||% "")[1]),
                          stringsAsFactors = FALSE))
      # A STATEMENT OF ANOTHER BANK proves itself like any other and teaches this
      # bank nothing (spec section 5). Counted as proven and said nowhere, it read
      # as one more statement learned from: it needs a look like one that failed.
      seen <- .bank_disputed(r)
      do.call(rbind, lapply(seq_along(rd), function(s) {
        x <- rd[[s]]
        ref <- x$matched_layout %||% x$learned_layout %||% x$learn$ref %||% NA_character_
        data.frame(file = t$names[i], statement = if (length(rd) > 1L) s else NA_integer_,
                   outcome = as.character(x$outcome %||% "unread")[1],
                   layout = if (is.null(seen)) sub("@v?[0-9]+$", "", as.character(ref)[1]) else NA_character_,
                   held = !is.null(seen),
                   why = if (is.null(seen)) as.character(x$why %||% "")[1]
                         else sprintf("It looks like a %s statement, so nothing was learned from it. Convert it on the Convert tab and say which bank it is.", seen),
                   stringsAsFactors = FALSE)
      }))
    }))
    list(statements = sts, proven = sts$outcome %in% c("proven", "layout_match"),
         look = !(sts$outcome %in% c("proven", "layout_match")) | sts$held,
         layouts = unique(stats::na.omit(sts$layout)))
  }
  output$adm_train_status <- renderUI({
    req(admin_ok())
    lv <- train_slot$live()
    if (!is.null(lv)) {
      n <- lv$n %||% 0L; nd <- NROW(lv$done)
      say <- if (identical(lv$state, "queued")) "Waiting for a free slot\u2026"
             else sprintf("Reading %d of %d%s", min(n, max(1L, as.integer(lv$i %||% 1L))), n,
                          if (!is.na(lv$file %||% NA)) paste0(" - ", lv$file) else "")
      return(div(class = "plan plan-running", style = "margin-top:8px",
        p(class = "plan-head", say),
        div(class = "plan-bar", div(class = "plan-bar-fill",
          style = sprintf("width:%d%%", as.integer(round(100 * nd / max(1L, n))))))))
    }
    t <- adm_train(); if (is.null(t)) return(NULL)
    rep <- .train_report(t)
    if (!is.null(rep$error)) return(div(class = "note-bad", style = "margin-top:8px", rep$error))
    st <- rep$statements; k <- sum(rep$look)
    names_ly <- vapply(rep$layouts, function(id) .layout_name(id) %||% id, "")
    div(class = "plan", style = "margin-top:8px",
      p(class = "plan-head", sprintf("%s: %d layout%s from %d statement%s, %d proven, %d need%s a look.",
        .bank_label(t$bank) %||% t$bank, length(rep$layouts), if (length(rep$layouts) == 1L) "" else "s",
        nrow(st), if (nrow(st) == 1L) "" else "s", sum(rep$proven), k, if (k == 1L) "s" else "")),
      if (length(names_ly)) tags$ul(style = "margin:0 0 8px 18px;padding:0", lapply(names_ly, tags$li)),
      if (k) tagList(
        p(class = "muted", style = "margin:4px 0", "The ones that need a look, with the reason:"),
        tags$table(class = "split-table",
          tags$thead(tags$tr(tags$th("File"), tags$th("Statement"), tags$th("Reason"))),
          tags$tbody(lapply(which(rep$look), function(j) tags$tr(
            tags$td(st$file[j]), tags$td(if (is.na(st$statement[j])) "-" else st$statement[j]),
            tags$td(st$why[j])))))),
      p(class = "muted", style = "margin:6px 0 0;font-size:12.5px",
        "A statement that needs a look teaches nothing. Convert it on the Convert tab and use Please check to set it right - a fix that then proves is learned."))
  })

  # ---- Admin -> Automatic reading (R/tracking.R): counts only ---------------------
  adm_ar_bump <- reactiveVal(0L)
  observeEvent(input$adm_ar_refresh, { req(admin_ok()); adm_ar_bump(isolate(adm_ar_bump()) + 1L) })
  adm_ar <- reactive({ req(admin_ok()); adm_ar_bump()
    safe(track_summary(TRACKING_DIR), NULL) })
  .pct <- function(x) if (is.null(x) || is.na(x)) "-" else sprintf("%.1f%%", 100 * x)
  # THE GATE THE PRODUCT OWNER SET (spec section 9): at least 95% of statements read
  # automatically, per kind of file, and nothing automatic and wrong. Said beside the
  # figure it judges, never as a verdict on a number too small to judge.
  AR_TARGET <- 0.95
  output$adm_ar_head <- renderUI({
    s <- adm_ar()
    if (is.null(s)) return(div(class = "note-bad", "The tracking files could not be read."))
    tile <- function(label, value, col = NULL) div(class = "stat",
      div(class = "stat-label", label),
      div(class = "stat-value", style = if (!is.null(col)) sprintf("color:%s", col), value))
    oc <- s$outcomes
    rate <- s$automatic_rate
    tagList(
      div(class = "stat-grid",
        tile("Statements read", format(s$statements, big.mark = ",")),
        tile("Read on their own", .pct(rate),
             if (is.na(rate %||% NA)) NULL else if (rate >= AR_TARGET) PALETTE$ok else PALETTE$warn),
        tile("Please check", format(unname(oc["check"]), big.mark = ",")),
        tile("Couldn't read", format(unname(oc["unread"]), big.mark = ","))),
      p(class = "muted", style = "margin:0 0 4px",
        sprintf("Proven %s, matched a known design %s. The target is %.0f%% read on their own for each kind of file, with nothing automatic and wrong.",
                format(unname(oc["proven"]), big.mark = ","), format(unname(oc["layout_match"]), big.mark = ","),
                100 * AR_TARGET)),
      if (!is.na(s$first %||% NA))
        p(class = "muted", style = "margin:0 0 4px;font-size:12.5px",
          sprintf("From %s to %s.", safe(local_time_text(s$first), s$first), safe(local_time_text(s$last), s$last))),
      if (isTRUE(s$unreadable_lines > 0L))
        p(class = "bad", style = "font-size:12.5px",
          sprintf("%d line(s) of the tracking files could not be read and are not counted.", s$unreadable_lines)),
      if (length(s$notes)) p(class = "muted", paste(s$notes, collapse = " ")))
  })
  output$adm_ar_kinds <- renderDT({
    s <- adm_ar(); req(s)
    k <- s$by_kind
    kind_plain <- c(pdf = "PDF", scan = "Scanned PDF", delimited = "CSV / TSV", excel = "Excel", unknown = "Not recorded")
    d <- data.frame(Kind = plain_label(k$kind, kind_plain), Statements = k$statements,
                    Proven = k$proven, `Matched a layout` = k$layout_match, `Please check` = k$check,
                    `Couldn't read` = k$unread,
                    `Read automatically` = vapply(k$automatic_rate, .pct, ""),
                    check.names = FALSE, stringsAsFactors = FALSE)
    datatable(d, rownames = FALSE, selection = "none", options = list(dom = "t"))
  })
  output$adm_ar_checks <- renderDT({
    s <- adm_ar(); req(s)
    ck <- s$checks_failed
    d <- data.frame(Check = plain_reading_check(names(ck)), Statements = as.integer(ck),
                    stringsAsFactors = FALSE)
    datatable(d, rownames = FALSE, selection = "none",
              options = dt_none_opts("No check has failed.", dom = "tp", pageLength = 10))
  })
  output$adm_ar_proof <- renderDT({
    s <- adm_ar(); req(s)
    proof_plain <- c(chain = "the running balance, row by row", totals = "the opening, closing and printed totals",
                     layout = "a proven layout", person = "a person on Please check")
    learn_plain <- c(created = "New layouts started", evidence_added = "Evidence added to a layout",
                     promoted = "Layouts proven", corrected = "Corrected by a person",
                     confirmed = "Confirmed by an admin", retired = "Retired", renamed = "Renamed",
                     none = "Nothing learned")
    # CHECKED AGAINST, NOT "PROVEN BY": the tracker records the kind of proof every
    # reading was put to, held or not, so a statement whose balance broke counted
    # under "Proven by: running balance" -- an unproven reading shown as proven.
    # Whether it held is the Proven / Please check count above.
    pk <- s$proof_kinds; la <- s$learn_actions
    d <- data.frame(
      What = c(ifelse(names(pk) == "none", "Checked against: nothing on the statement",
                      paste("Checked against:", plain_label(names(pk), proof_plain))),
               plain_label(names(la), learn_plain), "Readings corrected on Please check",
               "Statements with an amount filled in from the balance"),
      Count = c(as.integer(pk), as.integer(la), s$corrections, s$with_derived),
      stringsAsFactors = FALSE)
    datatable(d, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 30))
  })
  output$adm_ar_spot <- renderUI({
    s <- adm_ar(); req(s)
    sc <- s$spot_checks
    tot <- unname(sc["total"] %||% 0L)
    tagList(
      p(sprintf("%d spot check%s answered: %d right, %d wrong, %d couldn't tell.", tot,
                if (tot == 1L) "" else "s", unname(sc["right"]), unname(sc["wrong"]), unname(sc["cant_tell"]))),
      if (isTRUE(unname(sc["wrong"]) > 0L))
        p(class = "bad", "A spot check found an automatic conversion that was wrong. Look at the recipe it was read with, on Recipes."),
      # WHAT A CLEAN RUN OF SPOT CHECKS CAN AND CANNOT SAY (spec section 9): zero
      # errors in n checks only shows the error rate is below about 3/n.
      p(class = "muted", style = "font-size:12.5px", if (tot >= 30L && !isTRUE(unname(sc["wrong"]) > 0L))
        sprintf("No error in %d checks shows the error rate is below about %.1f%%; about 300 clean checks are needed to say under 1%%.",
                tot, min(100, 300 / tot))
        else "About 300 clean spot checks are needed to say the error rate is under 1%."))
  })
  # The current rate, read from the settings file the conversions read -- never
  # this session's memory of it.
  observe({
    req(admin_ok()); adm_ar_bump()
    r <- suppressWarnings(as.numeric(load_config()$auto_reading$spot_check_rate %||% 0)[1])
    updateNumericInput(session, "adm_spot_rate", value = if (is.na(r)) 0 else round(100 * r, 2))
  })
  # .save_spot_rate(rate, path) -- persist ONLY auto_reading$spot_check_rate,
  # merged over whatever the settings file already holds, by the same rule as
  # save_metadata_config() (R/config.R): a file that does not parse is refused,
  # never treated as empty, or saving one number would wipe every other setting.
  .save_spot_rate <- function(rate, path = .config_path()) {
    existing <- list()
    if (file.exists(path)) {
      parsed <- tryCatch(yaml::read_yaml(path), error = function(e) e)
      if (inherits(parsed, "error"))
        return(structure(FALSE, reason = sprintf("%s could not be read, so it was left untouched - fix the file first.", path)))
      if (is.list(parsed)) existing <- parsed
    }
    existing$auto_reading$spot_check_rate <- rate
    ok <- isTRUE(tryCatch({
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      yaml::write_yaml(existing, path); TRUE }, error = function(e) FALSE))
    if (ok) structure(TRUE, reason = "saved")
    else structure(FALSE, reason = sprintf("%s could not be written - check folder permissions.", path))
  }
  observeEvent(input$adm_spot_save, {
    req(admin_ok())
    pct <- suppressWarnings(as.numeric(input$adm_spot_rate))
    if (length(pct) != 1L || is.na(pct) || pct < 0 || pct > 100) {
      output$adm_spot_msg <- renderUI(div(class = "bad", "The rate is a percentage from 0 to 100.")); return()
    }
    ok <- .save_spot_rate(round(pct / 100, 4))
    if (isTRUE(ok)) CONFIG$auto_reading$spot_check_rate <<- round(pct / 100, 4)
    output$adm_spot_msg <- renderUI(div(class = if (isTRUE(ok)) "ok" else "bad", style = "margin-top:6px",
      if (!isTRUE(ok)) paste("Not saved:", attr(ok, "reason"))
      else if (pct == 0) "Saved - spot checks are off."
      else sprintf("Saved - about %s of automatic conversions will be picked for a spot check, from the next conversion.",
                   if (pct >= 1) sprintf("%g%%", pct) else sprintf("1 in %d", as.integer(round(100 / pct))))))
  })
  output$adm_ar_export_ui <- renderUI({ req(admin_ok())
    downloadButton("adm_ar_export", "Download the carry-off summary (counts only)", class = "btn-sm") })
  output$adm_ar_export <- downloadHandler(
    filename = function() sprintf("automatic-reading-summary-%s.json", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      req(admin_ok())        # admin-only export
      ok <- safe(track_export(TRACKING_DIR, file), structure(FALSE, reason = "the summary could not be made"))
      if (!isTRUE(ok)) .dl_note(file, paste("No summary could be made:", attr(ok, "reason") %||% "unknown reason."))
      .dl_log(if (isTRUE(ok)) "tracking-summary" else "tracking-summary:failed", file = file)
    })
  # The two YAML editors (dictionary, vocabulary) refuse and fail in exactly the
  # same two ways, and each had written out its own copy of both sentences. One
  # copy, so a reword of either can never leave the other behind.
  YAML_BAD_MSG   <- "Not valid YAML - not saved."
  YAML_WRITE_MSG <- "Could not write the file - check folder permissions."

  # ---- H9 FOR THE TWO WORD FILES, which is what "Reload from file" was for ----
  #
  # Both editors used to carry a Reload button whose own message admitted what it
  # was ("Reloaded from the file - any unsaved edits in the box are gone."). The
  # box already holds the file: it is filled when Admin opens and refilled after
  # every save, so the only reason to press Reload was the suspicion that another
  # maintainer had saved underneath you. That is a question the tool can answer -
  # it knows what it put in the box and it can read what is on disk now - so the
  # button is gone and Save answers it. Nothing is silent: an UNEDITED box is refreshed and said to have been
  # refreshed; an EDITED one is refused once, told why, and a second press means
  # it (the other version survives as the .bak every save writes).
  vocab_seen   <- reactiveValues(dict = NULL, lex = NULL)
  vocab_forced <- reactiveValues(dict = FALSE, lex = FALSE)
  .vocab_stale <- function(key, path, box_id, txt) {
    on_disk <- safe(read_file_text(path), NA_character_)
    seen <- vocab_seen[[key]]
    if (is.null(seen) || length(on_disk) != 1L || is.na(on_disk) ||
        identical(on_disk, seen)) return(NULL)
    if (identical(trimws(txt), trimws(seen))) {
      updateTextAreaInput(session, box_id, value = on_disk)
      vocab_seen[[key]] <- on_disk
      return(paste("Somebody else on this server saved this file since you opened it.",
                   "The box now holds theirs - you had not changed yours, so nothing is lost.",
                   "Check it and press Save again."))
    }
    if (isTRUE(vocab_forced[[key]])) return(NULL)
    vocab_forced[[key]] <- TRUE
    paste("Somebody else on this server saved this file since you opened it.",
          "Press Save once more to replace theirs with what is in the box -",
          "a backup of theirs is kept beside the file.")
  }

  # ---- Admin: label dictionary edit (the fix for "check shows NA") ----
  # Two controls, ONE write path each way round: the plain-English "teach it this
  # wording" goes through dictionary_append() (R/labels.R), the whole-file editor
  # writes the text the admin typed. Both back the file up first.
  .load_dict_text <- function() read_file_text(DICT_PATH)
  dict_bump <- reactiveVal(0)
  # The values the dictionary already knows, offered by name so an admin never has
  # to invent a key. Read from the FILE, so the list can never drift from it.
  # WHAT IT MEANS -- one list, both files. The meanings are the engine's own
  # (word_meanings(), R/words.R): each in plain words with an example, and each one
  # the reader acts on. The answer carries which file it belongs to, so nothing
  # asks the person which of two YAML files a wording lives in - which is not a
  # question they can answer, and is one the tool has always known. The list used
  # to be read from the dictionary FILE's keys, and so offered "Total Credits",
  # "Total Debits" and "Account Name", which nothing had read since templates were
  # retired: a word taught to one changed nothing, and the screen said "Added".
  output$adm_word_kind_ui <- renderUI({
    req(admin_ok()); dict_bump()
    selectInput("adm_word_kind", "What it means\u2026", word_meaning_choices(blank = TRUE))
  })
  observeEvent(admin_ok(), if (isTRUE(admin_ok())) {
    t <- .load_dict_text()
    updateTextAreaInput(session, "adm_dict_edit", value = t)
    vocab_seen$dict <- t; vocab_forced$dict <- FALSE
  })
  observeEvent(input$adm_dict_save, {
    req(admin_ok())
    txt <- input$adm_dict_edit %||% ""
    if (!isTRUE(tryCatch({ yaml::yaml.load(txt); TRUE }, error = function(e) FALSE))) {
      output$adm_dict_msg <- renderUI(div(style = "color:var(--bad)", YAML_BAD_MSG))
      return()
    }
    moved <- .vocab_stale("dict", DICT_PATH, "adm_dict_edit", txt)
    if (!is.null(moved)) {
      output$adm_dict_msg <- renderUI(div(style = "color:var(--bad)", moved)); return()
    }
    safe(file.copy(DICT_PATH, paste0(DICT_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({ writeLines(txt, DICT_PATH); TRUE }, error = function(e) FALSE))
    if (okw) { vocab_seen$dict <- .load_dict_text(); vocab_forced$dict <- FALSE }
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
  .ADM_META_CATS <- c("layout", "parse_quality", "reconciliation",
                      "multi_statement", "novelty", "ocr")
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
  observeEvent(admin_ok(), if (isTRUE(admin_ok())) {
    t <- .load_lex_text()
    updateTextAreaInput(session, "adm_lex_edit", value = t)
    vocab_seen$lex <- t; vocab_forced$lex <- FALSE
  })
  # "Show built-in defaults" REPLACES the whole editor with a different document,
  # and it used to do it in silence. What just happened to the box, and whether
  # anything was written to disk (nothing was), is said out loud.
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
    moved <- .vocab_stale("lex", LEXICON_PATH, "adm_lex_edit", txt)
    if (!is.null(moved)) {
      output$adm_lex_msg <- renderUI(div(style = "color:var(--bad)", moved)); return()
    }
    safe(file.copy(LEXICON_PATH, paste0(LEXICON_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({
      dir.create(dirname(LEXICON_PATH), recursive = TRUE, showWarnings = FALSE)
      writeLines(txt, LEXICON_PATH); TRUE }, error = function(e) FALSE))
    if (okw) { clear_lexicon_cache()   # the next conversion re-reads the vocabulary
               vocab_seen$lex <- .load_lex_text(); vocab_forced$lex <- FALSE }
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
    # ONE WRITER, ONE SET OF CHECKS. teach_wording() (R/words.R) picks the file
    # from the meaning, refuses a wording that would clash with another meaning,
    # and inserts one line, leaving every comment in the file where it was. A
    # refusal is reported in the engine's own words, so the screen and the reason
    # it was refused for can never say different things.
    out <- teach_wording(input$adm_word_kind %||% "", w, dict_path = DICT_PATH, lex_path = LEXICON_PATH)
    .words_taught()
    ok <- isTRUE(out) && isTRUE(attr(out, "added"))
    if (ok) updateTextInput(session, "adm_word_text", value = "")
    output$adm_word_msg <- renderUI(span(class = if (isTRUE(out)) "ok" else "bad",
      attr(out, "reason") %||% "Could not add that wording."))
  })
  # .words_taught() -- after a word is taught, from any screen: both whole-file
  # editors show the file as it now is, and the lists that depend on it refresh.
  .words_taught <- function() {
    t <- .load_dict_text()
    updateTextAreaInput(session, "adm_dict_edit", value = t)
    vocab_seen$dict <- t; vocab_forced$dict <- FALSE
    l <- read_file_text(LEXICON_PATH)
    updateTextAreaInput(session, "adm_lex_edit", value = l)
    vocab_seen$lex <- l; vocab_forced$lex <- FALSE
    dict_bump(isolate(dict_bump()) + 1)
    sugg_bump(isolate(sugg_bump()) + 1)
  }
  adm_suggestions <- reactive({ sugg_bump()
    safe(lexicon_suggestions(LOGDIR), list(indicator_tokens = data.frame(), unmapped_columns = data.frame())) })
  # THE ROW IS THE CONTROL. Picking a word here fills the box in the form above;
  # the one question left -- what does it mean -- is answered there, once, in the
  # same place every other taught word is answered.
  .words_cols <- function(d, first) { if (!is.data.frame(d) || !ncol(d)) return(data.frame(x = character(0), n = integer(0), check.names = FALSE) |> stats::setNames(c(first, "Times seen")))
    names(d) <- c(first, "Times seen")[seq_len(ncol(d))]; d }
  output$adm_sugg_tokens <- renderDT({ req(admin_ok()); .words_cols(adm_suggestions()$indicator_tokens, "Word") },
    selection = "single", rownames = FALSE,
    options = dt_none_opts("Nothing unrecognised yet.", pageLength = 8, dom = "tp"))
  output$adm_sugg_cols   <- renderUI({ req(admin_ok())
    d <- adm_suggestions()$unmapped_columns
    if (!is.data.frame(d) || !nrow(d)) return(p(class = "muted", "None yet \u2014 every column met so far was read."))
    d <- .words_cols(d, "Column heading")
    tags$table(class = "rc-table", tags$thead(tags$tr(tags$th(names(d)[1]), tags$th(class = "num", "Times seen"))),
      tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(tags$td(d[[1]][i]), tags$td(class = "num", d[[2]][i])))))
  })
  # Instructions only when there is something to act on. An empty list here is the
  # healthy state, not a fault: only a statement whose amount style is a D/C
  # indicator column can produce an unrecognised marker at all, so on a site whose
  # statements are all signed-amount or debit/credit-column exports it stays empty
  # for good, and the old copy read as a control that had stopped working.
  output$adm_sugg_help <- renderUI({
    req(admin_ok())
    n <- nrow(adm_suggestions()$indicator_tokens %||% data.frame())
    if (isTRUE(n > 0))
      helpText("Taken from the conversions run here, most frequent first - so the word worth teaching it is at the top. Click one to put it in the box above.")
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
  #
  # A word picker, a money-direction radio and an "Approve" button used to stand
  # under this table, which is the form above spelled a second time over a list the
  # reader is already looking at -- and the direction radio made the same decision
  # the form's "What it means" makes, in different words. Clicking the row hands
  # the word to the one form; nothing is written until Teach it is pressed there,
  # so the write path is the same one every taught word goes through.
  observeEvent(input$adm_sugg_tokens_rows_selected, {
    req(admin_ok())
    i <- input$adm_sugg_tokens_rows_selected
    toks <- adm_suggestions()$indicator_tokens$token %||% character(0)
    if (!length(i) || is.na(i[1]) || i[1] > length(toks)) return()
    tok <- as.character(toks[i[1]])
    updateTextInput(session, "adm_word_text", value = tok)
    updateSelectInput(session, "adm_word_kind", selected = "")   # the person says what it means
    output$adm_sugg_msg <- renderUI(div(class = "muted", style = "margin-top:6px",
      sprintf("\u201c%s\u201d is in the box above - say what it means and press Teach it.", tok)))
  }, ignoreInit = TRUE)

  # ---- Admin: bulk audit & gaps ----
  adm_ba <- reactiveVal(NULL)
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
    # OCR'ing a whole folder is the longest blocking call in the app - longer than
    # any single conversion, and started by the one person who can least afford to
    # take the tool away from everybody (the maintainer, mid-triage). So it runs in
    # its own process too, in the Admin slot, which is separate from the Convert
    # one: auditing a folder must not cancel the statement the same person is
    # converting on the other tab. The optional heavier "convert & log" pass moved
    # with it, unchanged (job_run_task, R/jobs.R).
    adm_slot$start("audit", paths, sess,
      message = "Auditing statements (scanned pages are OCR'd)",
      args = list(layouts_dir = LAYOUTS_DIR),
      finish = function(res) {
        if (is.null(res$audit)) {
          showNotification(paste("The audit stopped before it finished, so there is nothing to show.",
                                 "Run it again on fewer files, or check the error log."),
                           type = "error", duration = 12)
          return(invisible(NULL))
        }
        adm_ba(res$audit)
      })
  })
  output$adm_ba_summary <- renderUI({
    b <- adm_ba(); if (is.null(b)) return(helpText("Upload statements and click Run."))
    g <- b$feature_gaps
    none <- function(x) if (length(x)) paste(names(x), collapse = ", ") else "(none seen)"
    tagList(
      p(strong(sprintf("%d statements: ", g$total)),
        paste(sprintf("%s=%s", names(g$by_outcome), g$by_outcome), collapse = ", ")),
      p(sprintf("scanned %d \u00b7 multi-account %d \u00b7 multi-period %d \u00b7 not read %d across %d layouts",
        g$scanned, g$multi_account, g$multi_period, g$unread, g$distinct_gap_layouts)),
      p(class = "muted", sprintf("amount styles: %s | date formats: %s | banks: %s",
        none(g$amount_styles), none(g$date_formats), none(g$banks))))
  })
  output$adm_ba_clusters <- renderDT({
    b <- adm_ba(); req(b)
    cols <- c("count", "kind", "layout_hint", "signature")
    if (!nrow(b$clusters) || !all(cols %in% names(b$clusters)))
      return(stats::setNames(data.frame(matrix(character(0), 0, length(cols))), cols))
    b$clusters[, cols]
  }, options = dt_none_opts("Nothing in this pile went unread.",
                            pageLength = 10, dom = "tp"), rownames = FALSE)
  output$adm_ba_files_tbl <- renderDT({
    b <- adm_ba(); req(b)
    b$per_file[, intersect(c("idx", "kind", "bank", "outcome", "layout", "checks_failed", "n_rows",
                             "amount_style", "date_format"), names(b$per_file))]
  }, options = list(pageLength = 15, dom = "tip", scrollX = TRUE), rownames = FALSE)
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
  UP_AUDIT_WHY  <- "Pick a saved upload above first."
  INBOX_WHY     <- "Pick a failed file above first."

  output$adm_ba_report_ui <- renderUI({ req(admin_ok())
    dl_when("adm_ba_report", "Safe audit report (.md)", !is.null(adm_ba()), BA_REPORT_WHY) })
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
                        .dl_note(file, BA_REPORT_WHY)
                        return(.dl_log("bulk-audit:nothing", file = file)) }
      writeLines(format_batch_audit(b), file)
      .dl_log("bulk-audit", file = file) })

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
      NULL)
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
    cols <- c("ts", "file_ext", "status", "template", "trust", "needs_pickup", "purged")
    # ...AND THE HEADERS SAY IT IN WORDS. `needs_pickup` is an engine code, in the
    # header of a table a maintainer reads to decide what to do next, and the cells
    # under it read TRUE / FALSE. One naming, used by the empty table too, so an
    # empty Admin does not head its columns differently from a full one.
    # (Words sweep, cut 33.)
    heads <- c("When", "Type", "How it went", "Statement design", "Confidence",
               "Nothing usable was read", "Saved copy deleted")
    if (!nrow(u) || !all(cols %in% names(u)))
      return(stats::setNames(data.frame(matrix(character(0), 0, length(cols))), heads))
    # `purged` = the saved copy has passed its retention period and been deleted.
    # Shown, because a pickup row whose file is gone must not look actionable.
    u <- u[, cols]
    u$needs_pickup <- ifelse(as.logical(u$needs_pickup) %in% TRUE, "yes", "")
    # The layout reference the run was read with, as the name people see.
    u$template <- vapply(u$template, function(r) { v <- .layout_name(r); if (is.na(v)) "" else v }, "")
    u$purged <- ifelse(as.logical(u$purged) %in% TRUE, "yes", "")
    u$status <- ifelse(u$status %in% names(PLAIN_STATUS), unname(PLAIN_STATUS[u$status]), u$status)
    names(u) <- heads
    u
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
    # and "read it again" beside it were unreachable for every statement that had
    # actually converted. Both are useful on a GOOD conversion: auditing one is how
    # you check a reading, and reading it again is how you look at it on Please
    # check. A purged upload is still excluded -- its file is gone, so both
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
                      .dl_note(file, UP_AUDIT_WHY)
                      return(.dl_log("upload-audit:nothing", id = id, file = file)) }
      a <- tryCatch(format_audit(statement_audit(need_file(p), layouts_dir = LAYOUTS_DIR)),
                    error = function(e) NULL)
      if (is.null(a)) {
        .dl_note(file, sprintf(
          "The saved statement for %s could not be read - it may have been deleted by the retention purge. Its upload record is still in Insights.", id))
        return(.dl_log("upload-audit:unreadable", id = id, file = file)) }
      writeLines(a, file)
      # THE ONE THAT MATTERS MOST. This reads a statement SOMEBODY ELSE uploaded,
      # and until there is case ownership to check against, the record of who read
      # it is the only control there is.
      .dl_log("upload-audit", id = id, file = file)
    })

  # ---- Admin: format requests raised via the "tell our team" escape hatch ----
  req_bump <- reactiveVal(0)   # bump to refresh after a triage action
  output$adm_requests <- renderDT({
    req_bump(); input$adm_refresh
    q <- read_template_requests(REQUESTS_DIR)
    cols <- c("ts", "requested_by", "status", "detail", "context")
    if (!nrow(q) || !all(cols %in% names(q)))
      return(.plain_tbl(stats::setNames(data.frame(matrix(character(0), 0, length(cols))), cols)))
    .plain_tbl(q[, cols])
  }, options = dt_none_opts("Nobody has raised a format request.",
                            pageLength = 6, dom = "tip"), rownames = FALSE)
  observe({
    # A bare observe() is NOT suspended when its tab is hidden, so without this
    # guard the format-request queue was read off disk and its ids pushed into
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
    .plain_tbl(inbox_state()$folders[[which]])
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
  observeEvent(input$adm_up_reread, {
    req(admin_ok())
    id <- input$adm_up_pick
    if (is.null(id) || !nzchar(id)) {
      showNotification("Pick a saved upload first.", type = "warning"); return() }
    p <- upload_file_path(id, UPLOADS_DIR)
    if (is.na(p) || !file.exists(p)) {
      showNotification("That upload's file is no longer available.", type = "error"); return() }
    .reread_on_convert(p, basename(p), upload_id = id)
  })
  observeEvent(input$adm_inbox_reread, {
    req(admin_ok())
    nm <- input$adm_inbox_pick
    if (is.null(nm) || !nzchar(nm)) { showNotification("Pick a failed file first.", type = "warning"); return() }
    p <- failed_file_path(nm, ".")
    if (is.na(p)) { showNotification("That file is no longer in failed/.", type = "error"); return() }
    .reread_on_convert(p, nm)
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
                      .dl_note(file, INBOX_WHY)
                      return(.dl_log("inbox-audit:nothing", id = nm, file = file)) }
      a <- tryCatch(format_audit(statement_audit(need_file(p), layouts_dir = LAYOUTS_DIR)),
                    error = function(e) NULL)
      if (is.null(a)) {
        .dl_note(file, sprintf(
          "%s could not be read at all, so there is nothing to summarise. That it cannot be read IS the finding: it is not a statement this tool can open, or the file is damaged.", nm))
        return(.dl_log("inbox-audit:unreadable", id = nm, file = file)) }
      writeLines(a, file)
      .dl_log("inbox-audit", id = nm, file = file)
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
    datatable(.plain_tbl(runs_overview(d$runs)), rownames = FALSE, options = list(dom = "t"))
  })
  # ONE STACKED BAR: done / needs a check / couldn't read, in the status colours
  output$adm_status_plot <- renderPlot({
    d <- adm_data(); req(d); ov <- runs_overview(d$runs); if (!nrow(ov)) return(NULL)
    grp <- c(ok = "Done", needs_review = "Needs a check", unsupported = "Couldn't read", failed = "Couldn't read")
    g <- unname(grp[as.character(ov$status)]); g[is.na(g)] <- "Couldn't read"
    tot <- tapply(ov$n, factor(g, levels = c("Done", "Needs a check", "Couldn't read")), sum)
    tot[is.na(tot)] <- 0
    cols <- c(PALETTE$ok, "#e3b341", PALETTE$bad)
    op <- par(mar = c(2.2, 0.5, 0.5, 0.5), family = "sans"); on.exit(par(op))
    barplot(matrix(tot, ncol = 1), horiz = TRUE, col = cols, border = NA, axes = FALSE, space = 0.15,
            xlim = c(0, max(1, sum(tot))))
    legend("bottom", inset = c(0, -0.32), xpd = TRUE, horiz = TRUE, bty = "n", fill = cols, border = NA,
           legend = sprintf("%s %d", names(tot), as.integer(tot)), cex = 0.95)
  })
  # THE GAPS ARE THE `unsupported` RUNS ONLY. unsupported_clusters() takes the
  # FAILED ones too, and a failed run is not a gap: the file never reached the
  # reader, so it would land here with no layout and no reason -- blank cells under
  # a heading promising a layout. They are shown as what they are, in
  # adm_unreadable below.
  .GAP_COLS <- c("count", "layout", "why", "last_seen", "example_file")
  output$adm_gaps <- renderDT({
    d <- adm_data(); req(d)
    runs <- d$runs
    if (nrow(runs) && "status" %in% names(runs))
      runs <- runs[as.character(runs$status) %in% "unsupported", , drop = FALSE]
    g <- unsupported_clusters(runs)
    # A blank cell reads as "the layout is empty". It is a fact the log simply does
    # not carry for these runs, so say that instead.
    said <- function(v) { v <- as.character(v); v[is.na(v) | !nzchar(trimws(v))] <- "not recorded"; v }
    g <- g[, .GAP_COLS, drop = FALSE]
    for (nm in c("layout", "why", "example_file")) g[[nm]] <- said(g[[nm]])
    datatable(.plain_tbl(g), rownames = FALSE,
              options = dt_none_opts("Every statement read here gave something usable.",
                                     pageLength = 10, scrollX = TRUE)) |>
      formatStyle("Statements", fontWeight = "bold")
  })
  output$adm_unreadable <- renderDT({
    d <- adm_data(); req(d)
    runs <- d$runs
    cols <- intersect(c("ts", "source_file", "message"), names(runs))
    if (!nrow(runs) || !("status" %in% names(runs)) || !length(cols))
      return(stats::setNames(data.frame(matrix(character(0), 0, 3)), c("ts", "source_file", "message")))
    f <- runs[as.character(runs$status) %in% "failed", cols, drop = FALSE]
    .plain_tbl(f[order(as.character(f$ts), decreasing = TRUE), , drop = FALSE])
  }, options = dt_none_opts("Every file opened - nothing failed to read.",
                            pageLength = 5, dom = "tip"), rownames = FALSE)
  # A layout reference in the logs is shown as the layout's name, the same one the
  # Banks tab and the Convert table use.
  .named_layouts <- function(df) {
    if (is.data.frame(df) && nrow(df) && "layout" %in% names(df))
      df$layout <- vapply(df$layout, function(r) .layout_name(r) %||% r, "")
    df
  }
  output$adm_usage <- renderDT({
    d <- adm_data(); req(d)
    u <- .named_layouts(layout_usage(d$runs, d$fb))
    datatable(.plain_tbl(u), rownames = FALSE,
              options = dt_none_opts("No conversion has been read with a learned design yet.",
                                     dom = "t", pageLength = 20))
  })
  # HEALTH MEANS PROVEN (run_healthy(), R/analytics.R): a layout whose statements
  # used to prove themselves and now go to a person is a layout the bank has
  # changed under it.
  output$adm_drift <- renderDT({
    d <- adm_data(); req(d)
    dr <- .named_layouts(layout_drift(d$runs))
    tbl <- datatable(.plain_tbl(dr), rownames = FALSE,
                     options = dt_none_opts("No design has started failing - good.", dom = "t"))
    if (nrow(dr) && "drop" %in% names(dr)) tbl <- formatStyle(tbl, "Drop", fontWeight = "bold", color = PALETTE$bad)
    tbl
  })
  # EVERY RATING, with the document it was left on and the layout that read it.
  output$adm_feedback <- renderDT({
    d <- adm_data(); req(d)
    datatable(.plain_tbl(.adm_feedback_overview(d$fb, d$runs, .layout_name)), rownames = FALSE, selection = "none",
              options = dt_none_opts("Nobody has rated a conversion yet.", pageLength = 8, dom = "tip",
                                     scrollX = TRUE))
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
  # WHAT HAPPENS TO THE FEED FOLDER, said in the same breath as what happens to
  # the saved statements. Generated from the setting itself (feed.keep_days) so
  # the promise on the screen and the rule on disk cannot drift apart.
  output$adm_feed_retention <- renderUI({
    req(admin_ok())
    note <- safe(feed_retention_note(CONFIG$feed$keep_days), NULL)
    if (is.null(note) || !nzchar(note)) return(NULL)
    helpText(note)
  })
  # When the browser tab closes, take this session's scratch folder with it. The
  # folder holds a copy of the client's statement plus every output; the process
  # temp dir is only cleared when R exits, and this app is a long-running service.
  # The JOBS go first, and they have to: a child is still writing into that folder,
  # and an abandoned OCR run would otherwise hold a core for two more minutes
  # producing a result no browser is left to read.
  session$onSessionEnded(function() {
    safe(cv_slot$cancel()); safe(plan_slot$cancel()); safe(adm_slot$cancel()); safe(train_slot$cancel())
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
  #
  # A FORWARDED HEADER IS NOT EVIDENCE OF ANYTHING ON ITS OWN, and treating it as
  # evidence was a live audit-integrity defect. This app trusted any of EIGHT
  # header names with no check that the request had come through a proxy at all,
  # while listening on every network card -- so
  #
  #     curl -H "X-Forwarded-User: some.other.detective" http://host:8100/
  #
  # made the run log record a conversion against a name the sender chose, in the
  # tier R/logging.R documents as "an identity forwarded by a proxy/gateway. Also
  # per-person". The record certified a claim it could not know, which is the one
  # thing an audit trail must never do.
  #
  # THE FIX IS TO REQUIRE PROOF OF PROVENANCE, not to drop the feature. The header
  # says WHO; a shared secret, present nowhere but the proxy's own configuration,
  # shows the claim came from something entitled to make it. No secret, no "sso" --
  # the person is asked to identify themselves exactly as if no proxy were there.
  # Downgrade, never refuse: a misconfigured secret must not take the tool away
  # from a whole office.
  #
  # ONE header name, from config. Eight names were eight forgery surfaces and
  # seven that no proxy in this deployment would ever set (one of them was a
  # Cloudflare header, on an air-gapped server).
  .ident_cfg <- function(k, d = "") {
    v <- CONFIG$app[[k]] %||% d
    if (length(v) != 1L || is.na(v)) d else trimws(as.character(v))
  }
  # .req_header_name(name) -- the Rook/CGI key httpuv builds for a header name.
  # httpuv upper-cases and turns "-" into "_", so "X-Remote-User" arrives as
  # HTTP_X_REMOTE_USER (httpuv src/webapplication.cpp, normalizeHeaderName).
  .req_header_name <- function(name)
    paste0("HTTP_", toupper(gsub("-", "_", name, fixed = TRUE)))
  #
  # TWO httpuv BEHAVIOURS THAT DEFEAT A CAREFUL PROXY, both read off its source:
  #
  #   * DUPLICATE HEADERS ARE JOINED WITH A COMMA. httpuv's header map is
  #     case-insensitive, so "X-Remote-User" and "x-remote-user" are one key and
  #     two copies arrive as "attacker,real.detective". A plain nzchar() test
  #     accepts that string as a username, so a comma is treated as an attack and
  #     the claim is thrown away rather than parsed.
  #   * THE UNDERSCORE SPELLING IS A DIFFERENT HEADER THAT OVERWRITES THE SAME
  #     KEY. The map is underscore-SENSITIVE, so "X_Remote_User" is a separate
  #     entry -- but it normalises to the same HTTP_X_REMOTE_USER, and the
  #     underscore form is written second and wins. A proxy that diligently
  #     strips and sets only the hyphen spelling is therefore bypassed by a client
  #     sending the underscore one. nginx drops underscore headers by default for
  #     exactly this reason; IIS and Apache do not. So the raw header list is
  #     checked too, and more than one spelling of the name is a refusal.
  .raw_header_spellings <- function(req, name) {
    hs <- tryCatch(req$HEADERS, error = function(e) NULL)
    if (is.null(hs) || !length(names(hs))) return(1L)   # cannot tell: don't block
    want <- toupper(gsub("[-_]", "", name))
    sum(toupper(gsub("[-_]", "", names(hs))) == want)
  }
  detected_identity_info <- function() {
    su <- session$user
    if (!is.null(su) && nzchar(trimws(su))) return(list(who = trimws(su), source = "host"))
    req <- session$request
    hdr <- .ident_cfg("identity_header")
    secret <- .ident_cfg("identity_shared_secret")
    if (!is.null(req) && nzchar(hdr) && nzchar(secret)) {
      got <- tryCatch(req[[.req_header_name(.ident_cfg("identity_secret_header",
                      "X-Statement-Studio-Secret"))]], error = function(e) NULL)
      # Constant-time: a byte-by-byte comparison leaks the secret one character at
      # a time to anyone willing to time the responses.
      if (.secret_ok(got, secret)) {
        v <- tryCatch(req[[.req_header_name(hdr)]], error = function(e) NULL)
        v <- if (is.null(v)) "" else trimws(as.character(v)[1])
        if (nzchar(v) && !grepl(",", v, fixed = TRUE) &&
            .raw_header_spellings(req, hdr) <= 1L)
          return(list(who = v, source = "sso"))
      }
    }
    cu <- current_user()
    if (!is.null(cu) && nzchar(cu) && !identical(cu, "unknown"))
      return(list(who = cu, source = "os"))
    list(who = NA_character_, source = "none")
  }
  detected_identity <- function() detected_identity_info()$who
  # .dl_log(what, id, file, run_id) -- record that somebody took a copy.
  #
  # EVERY downloadHandler calls this, from inside `content`, AFTER the bytes are in
  # place -- so the hash is of what was actually handed over, not of what was meant
  # to be. A download that produced only an explanation (.dl_note) is still a
  # download and is still recorded: "she asked for the workbook and got a note
  # saying why there wasn't one" is a fact a reviewer may need.
  #
  # It never throws and never blocks the download (log_download, R/logging.R). A
  # download that works but is not logged is better than one that fails because the
  # logging did -- the opposite trade would turn a full disk into an outage.
  .dl_log <- function(what, id = NA_character_, file = NA_character_,
                      run_id = NA_character_) {
    info <- safe(detected_identity_info(), list(who = NA_character_, source = "none"))
    safe(log_download(LOGDIR, what = what, id = id, path = file,
                      who = info$who, source = info$source, run_id = run_id), NULL)
    invisible(NULL)
  }
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
  # NO SEPARATE "Use this QID" BUTTON. There was one, and it was the whole of the
  # fault it was meant to prevent: a perfectly good six-character QID sitting in
  # the box with Convert still greyed out and a caption underneath saying "Enter
  # your QID above". The tool knows what a QID looks like -- QID_PATTERN is right
  # here -- so it reads the box itself the moment the box is right. One decision,
  # one action.
  observeEvent(input$cv_qid, {
    v <- trimws(input$cv_qid %||% "")
    if (grepl(QID_PATTERN, v)) { cv_qid(toupper(v)); clear_notice("cv_qid") }
    else cv_qid(NA_character_)
  }, ignoreInit = TRUE)
  # The refusal moved to the box, where the person is looking, and it waits until
  # the answer cannot come right: five alphanumerics is somebody still typing, a
  # space or a seventh character is somebody typing a name.
  output$cv_qid_why <- renderUI({
    v <- trimws(input$cv_qid %||% "")
    if (!nzchar(v) || grepl(QID_PATTERN, v)) return(NULL)
    if (nchar(v) <= 6L && !grepl("[^A-Za-z0-9]", v)) return(NULL)
    p(class = "muted", style = "margin:-4px 0 8px;color:#8a6d3b",
      "A QID is six letters or numbers, e.g. AB1234.")
  })
  observeEvent(input$cv_qid_change, cv_qid(NA_character_))
  output$hdr_user <- renderUI({
    if (.identity_is_personal(detected_identity_info())) return(NULL)
    q <- cv_qid(); if (is.na(q)) return(NULL)
    actionLink("cv_qid_change", class = "user-chip", title = sprintf("Recording as %s \u2014 click to change", q),
               label = tagList(span(class = "user-dot"), q))
  })
  output$cv_whoami <- renderUI({
    # A real per-person sign-in answers this already: ask nothing.
    if (.identity_is_personal(detected_identity_info())) return(NULL)
    q <- cv_qid()
    if (!is.na(q)) return(NULL)   # who is recorded sits in the header, as a small chip
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
      uiOutput("cv_qid_why"),
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
  # .case_converting() -- TRUE, and says so, while a case folder is converting.
  # The page is not behind an overlay while a case runs (its table shows the
  # progress), so another way into a conversion -- a re-read on Please check, a
  # sample -- is reachable mid-case. Starting one would supersede the case in the
  # same slot AND reclaim the scratch folder it is writing into. So it waits.
  .case_converting <- function() {
    if (is.null(isolate(cv_slot$live()))) return(FALSE)
    notify_once("cv_case_busy", paste("A case is converting - its results are filling in on Convert.",
      "Try again when it has finished."), type = "warning", duration = 8)
    TRUE
  }

  # ---- THE CONVERT TABLE -------------------------------------------------------
  #
  # "I want it to pre fill a table with the upload, its type, and ... easy dropdown
  # to change it. Same thing for single statement." The moment files are chosen,
  # each is identified (R/identify.R) and gets a row: the file, what kind of file it
  # is, and its BANK -- pre-filled from the statement itself (the holder's account
  # number in the bank branch register, then the bank's legal name, website and
  # brand words), in a plain dropdown that can be changed, with a way to name a
  # bank the list does not have. Convert reads the table.
  #
  # ONE TABLE, BEFORE AND AFTER. Once converted, the SAME rows carry the result --
  # the learned layout each was read with and its outcome in plain words -- worst
  # first, and a click on a row opens that file's full result below it.
  #
  # THE TWO RULES THAT MAKE IT SAFE.
  #  1. A row LEFT ON ITS BANK is converted with no bank given: the conversion
  #     identifies the bank from the WHOLE statement (this table reads only the text
  #     layer), and a statement that then names another bank teaches nothing until
  #     a person confirms which is right (R/convert.R).
  #  2. A row the analyst CHANGED is converted as that bank, file by file. A case
  #     folder holds several banks; one bank for all of it could only ever be right
  #     for some of them.
  #
  # THE CHOICES LIVE HERE, ON THE SERVER (cv_plan_picks), not in the dropdowns. A
  # dropdown is a plain <select>, not a Shiny input: a change arrives as one event
  # (cv_plan_pick) and is recorded against its row, so a redraw can never lose a
  # choice and a dropdown left over from the last upload can never be read as this
  # one's. Per row: NA = untouched, else the bank (an id, or a name typed here).
  #
  # CONVERT AGAIN RE-READS WHAT WOULD COME OUT DIFFERENTLY. After a run, a row whose
  # bank has changed is converted again and the rest keep their results; with
  # nothing changed, Convert runs every file again.
  #
  # NEVER HOLDS THE SERVER. One file is identified per tick, then the event loop
  # gets the process back (invalidateLater) before the next. A text PDF identifies
  # from its text layer alone, and a scan is SAID to be a scan: its first pages are
  # read in a background job and its row fills in when they are.
  plan_env <- new.env(parent = emptyenv())
  plan_env$gen <- 0L; plan_env$rows <- NULL; plan_env$i <- 0L
  cv_plan       <- reactiveVal(NULL)          # list(gen, rows, too_many) once every file is checked
  cv_plan_busy  <- reactiveVal(NULL)          # list(gen, n) while files are being checked
  cv_plan_done  <- reactiveVal(0L)            # how many of them so far (for the screen only)
  cv_plan_picks <- reactiveVal(character(0))  # per row: NA / a bank
  cv_plan_ran   <- reactiveVal(NULL)          # list(gen, expected): each row's bank when last converted
  cv_plan_redraw <- reactiveVal(0L)           # puts a dropdown back after "Another bank" is cancelled
  cv_run        <- reactiveVal(NULL)          # list(gen, rows): the case converting now, and its rows

  plan_start_check <- function() {
    plan_slot$cancel(); plan_env$scan <- NULL   # a re-check reads the scans again
    plan_env$i <- 0L
    cv_plan_done(0L)
    cv_plan_busy(list(gen = plan_env$gen, n = nrow(plan_env$rows)))
  }
  # plan_reset() -- forget the table: a file put on the result page from somewhere
  # else (a sample, a saved upload) is not one of the files the table describes.
  plan_reset <- function() {
    plan_env$gen <- plan_env$gen + 1L; plan_env$i <- 0L; plan_env$rows <- NULL
    plan_slot$cancel(); plan_env$scan <- NULL
    cv_plan(NULL); cv_plan_busy(NULL); cv_plan_done(0L); cv_plan_ran(NULL)
    cv_plan_picks(character(0))
  }

  observeEvent(input$cv_file, {
    f <- input$cv_file
    plan_reset()
    # New files replace what the page is about: the last case's rows, and the result
    # open under them, belong to files that are no longer chosen.
    if (!is.null(cv_batch()) || !is.null(cv_res())) {
      show_result(); cv_batch_row(NA_integer_); cv_batch(NULL)
    }
    if (is.null(f) || !NROW(f)) return()
    # Too many is said HERE, before anything is checked -- the Convert button refuses
    # the same number for the same reason.
    if (nrow(f) > MAX_BATCH_FILES) {
      cv_plan(list(gen = plan_env$gen, rows = NULL, too_many = nrow(f)))
      return()
    }
    plan_env$rows <- data.frame(name = as.character(f$name),
      datapath = as.character(f$datapath), kind = NA_character_,
      format = NA_character_, pages = NA_integer_, state = "checking",
      bank = NA_character_, bank_display = NA_character_, confidence = "unknown",
      ask = TRUE, detail = NA_character_, mark = NA_character_, remembered = NA_character_,
      stringsAsFactors = FALSE)
    cv_plan_picks(rep(NA_character_, nrow(f)))
    plan_start_check()
  }, ignoreNULL = FALSE)

  # One file per tick. Reads cv_plan_busy and nothing it writes on the way, so it is
  # re-run by the timer and not straight away inside the same flush.
  observe({
    b <- cv_plan_busy(); if (is.null(b)) return()
    isolate({
      rows <- plan_env$rows
      i <- plan_env$i + 1L
      if (!is.null(rows) && i <= nrow(rows)) {
        # quietly: a workbook with blank headings has readxl print "New names:" to
        # the server console for every file checked
        id <- safe(suppressMessages(identify_file(rows$datapath[i], rows$name[i])), NULL) %||%
          list(state = "unreadable", kind = toupper(tools::file_ext(rows$name[i])))
        rows$kind[i]   <- as.character(id$kind %||% NA_character_)[1]
        rows$format[i] <- as.character(id$format %||% NA_character_)[1]
        rows$pages[i]  <- as.integer(id$pages %||% NA_integer_)[1]
        rows$state[i]  <- as.character(id$state %||% "unreadable")[1]
        rows <- .plan_bank_fields(rows, i, id)
        plan_env$rows <- rows; plan_env$i <- i
        cv_plan_done(i)
      }
      if (is.null(rows) || plan_env$i >= nrow(rows)) {
        .plan_recall()
        cv_plan(list(gen = b$gen, rows = plan_env$rows, too_many = 0L))
        cv_plan_busy(NULL)
        plan_scan_start()          # scans: their first pages, read in the background
      }
    })
    if (!is.null(isolate(cv_plan_busy()))) invalidateLater(1, session)
  })
  # .plan_bank_fields(rows, i, id) -- a row's bank, from identify_file() or
  # identify_scan(): the institution, its name, how sure, and why (the hover).
  .plan_bank_fields <- function(rows, i, id) {
    rows$bank[i]         <- as.character(id$bank %||% NA_character_)[1]
    rows$bank_display[i] <- as.character(id$bank_display %||% NA_character_)[1]
    rows$confidence[i]   <- as.character(id$confidence %||% "unknown")[1]
    rows$ask[i]          <- isTRUE(id$ask %||% is.na(rows$bank[i]))
    rows$detail[i]       <- as.character(id$detail %||% NA_character_)[1]
    rows$mark[i]         <- as.character(id$account_mark %||% NA_character_)[1]
    rows
  }
  # .plan_recall() -- a row whose statement does not settle its bank takes the bank
  # remembered for its account or its file name, as a choice the person can change.
  .plan_recall <- function() {
    rows <- plan_env$rows; if (is.null(rows) || !nrow(rows)) return(invisible(NULL))
    mem <- safe(bank_memory_load(TRACKING_DIR), NULL); if (is.null(mem)) return(invisible(NULL))
    picks <- isolate(cv_plan_picks()); length(picks) <- nrow(rows)
    for (i in seq_len(nrow(rows))) {
      if (!(rows$state[i] %in% c("ready", "scan_ready", "scanned")) || !is.na(picks[i])) next
      if (!is.na(rows$bank[i]) && !isTRUE(rows$ask[i])) next
      r <- safe(bank_memory_recall(TRACKING_DIR, rows$name[i], rows$mark[i], memory = mem), NULL)
      if (is.null(r) || is.na(r$bank) || identical(r$bank, rows$bank[i])) next
      picks[i] <- r$bank; rows$remembered[i] <- r$why
    }
    plan_env$rows <- rows; cv_plan_picks(picks)
    invisible(NULL)
  }
  # .remember_bank(i, name, res, bank) -- after a file has converted and its
  # balance adds up, remember the bank it was read as, for its account and its name.
  .remember_bank <- function(i, name, res, bank = NULL) {
    if (!identical(as.character(res$status %||% "")[1], "ok")) return(invisible(NULL))
    bk <- as.character(bank %||% res$run_log$institution %||% NA_character_)[1]
    rows <- plan_env$rows
    mk <- if (!is.null(rows) && !is.na(i) && i <= nrow(rows) && identical(rows$name[i], name)) rows$mark[i] else NA_character_
    safe(bank_memory_note(TRACKING_DIR, name, mk, bk))
    invisible(NULL)
  }

  # ---- SCANS: their bank from their first pages ------------------------------------
  # A scan's text only exists once it is read as a picture (seconds a page), so the
  # quick check above says "Scanned" and moves on. Then, in a background job (never
  # this process), each scan's first two pages are read and its bank identified
  # (identify_scan); each row fills in as its pages are read. Convert does not wait
  # for it: a press stops the reading, and an unread scan's bank is found while it
  # converts -- so no answer can arrive AFTER the file it was for was converted.
  plan_scan_start <- function() {
    rows <- plan_env$rows
    i <- which(rows$state == "scanned")
    if (!length(i) || !isTRUE(safe(ocr_available(), FALSE))) return(invisible(NULL))
    rows$state[i] <- "scanning"; plan_env$rows <- rows
    plan_env$scan <- list(gen = plan_env$gen, rows = i)
    cv_plan(list(gen = plan_env$gen, rows = rows, too_many = 0L))
    od <- tempfile("scan_"); dir.create(od, showWarnings = FALSE)
    plan_slot$start("identify_scans", rows$datapath[i], od, overlay = FALSE, message = "",
      args = list(names = rows$name[i]),
      finish = function(res) {
        # a list per scan when it worked; the failed-job result (a named list) when not
        plan_scan_apply(if (is.list(res) && is.null(names(res))) res else NULL, final = TRUE)
      })
  }
  # plan_scan_apply(res, final, done) -- put what the scan reading found into the rows
  # still waiting for it. `done` is the verdicts streamed so far, `res` the job's whole
  # answer; `final` returns any row still "reading" to plain "Scanned".
  plan_scan_apply <- function(res = NULL, final = FALSE, done = NULL) {
    sc <- plan_env$scan
    if (is.null(sc) || !identical(sc$gen, plan_env$gen) || is.null(plan_env$rows)) return(invisible(NULL))
    rows <- plan_env$rows
    upd <- function(k, id) {
      i <- sc$rows[k]
      if (is.na(i) || i > nrow(rows) || !identical(rows$state[i], "scanning")) return(invisible(NULL))
      rows$state[i] <<- as.character(id$state %||% "scanned")[1]
      if (identical(rows$state[i], "scan_ready")) rows <<- .plan_bank_fields(rows, i, id)
      else rows$detail[i] <<- as.character(id$detail %||% NA_character_)[1]
    }
    if (!is.null(done)) for (j in seq_len(nrow(done)))
      upd(done$k[j], c(as.list(done[j, , drop = FALSE]),
                       list(ask = !(done$confidence[j] %in% c("high", "medium")))))
    if (!is.null(res)) for (k in seq_along(res)) upd(k, res[[k]])
    if (final) { rows$state[rows$state == "scanning"] <- "scanned"; plan_env$scan <- NULL }
    plan_env$rows <- rows
    cv_plan(list(gen = plan_env$gen, rows = rows, too_many = 0L))
  }
  observe({
    lv <- plan_slot$live()
    if (is.null(lv) || is.null(lv$done) || !NROW(lv$done)) return()
    isolate(plan_scan_apply(done = lv$done))
  })

  # A dropdown changed. Recorded against its row of THIS upload, and nothing else.
  # "Another bank" asks for the name first; the row keeps what it had until a name
  # arrives, so a cancelled question changes nothing.
  observeEvent(input$cv_plan_pick, {
    v <- input$cv_plan_pick; p <- cv_plan()
    i <- suppressWarnings(as.integer(v$row %||% NA)[1])
    if (is.null(p) || is.null(p$rows) || !identical(suppressWarnings(as.integer(v$gen %||% NA)[1]), p$gen) ||
        is.na(i) || i < 1L || i > nrow(p$rows)) return()
    val <- as.character(v$value %||% "")[1]
    if (identical(val, "__new__")) {
      plan_env$new_row <- list(gen = p$gen, row = i)
      cv_plan_redraw(isolate(cv_plan_redraw()) + 1L)
      showModal(modalDialog(
        title = "Another bank", size = "s", easyClose = TRUE,
        textInput("cv_new_bank", sprintf("The bank that issued %s", p$rows$name[i]), "",
                  placeholder = "e.g. Smith Credit Union", width = "100%"),
        helpText("As people know it. What the tool learns from this statement is kept under this name."),
        uiOutput("cv_new_bank_msg"),
        footer = tagList(modalButton("Cancel"),
                         actionButton("cv_new_bank_ok", "Use this bank", class = "btn-primary"))))
      return()
    }
    pk <- cv_plan_picks(); length(pk) <- nrow(p$rows)
    pk[i] <- if (nzchar(val)) val else NA_character_
    # a remembered bank changed by hand is the person's own choice now
    if (!is.na(p$rows$remembered[i] %||% NA)) {
      p$rows$remembered[i] <- NA_character_; cv_plan(p)
      if (!is.null(plan_env$rows) && nrow(plan_env$rows) >= i) plan_env$rows$remembered[i] <- NA_character_
    }
    cv_plan_picks(pk)
  })
  observeEvent(input$cv_new_bank_ok, {
    nr <- plan_env$new_row; p <- cv_plan()
    nm <- trimws(input$cv_new_bank %||% "")
    why <- .bank_name_problem(nm)
    if (!is.null(why)) { output$cv_new_bank_msg <- renderUI(div(class = "bad", why)); return() }
    removeModal()
    if (is.null(nr) || is.null(p) || !identical(nr$gen, p$gen) || nr$row > NROW(p$rows)) return()
    # A name that IS one of the banks on the list is that bank, not a second one.
    ch <- bank_list()
    hit <- which(tolower(names(ch)) == tolower(nm) | tolower(unname(ch)) == tolower(nm))
    val <- if (length(hit)) unname(ch[hit[1]]) else nm
    if (!length(hit)) cv_new_banks(unique(c(cv_new_banks(), nm)))
    pk <- cv_plan_picks(); length(pk) <- nrow(p$rows)
    pk[nr$row] <- val
    cv_plan_picks(pk)
  })
  # STOP. A case can run for many minutes, and closing the tab was the only way out.
  # A first run stopped leaves the table as it was before Convert. A Convert-again
  # stopped is the careful case: the rows it was re-reading may already have had
  # their files written over, so their old verdicts no longer describe what is on
  # disk -- they are taken out (no verdict, nothing in Download everything) and
  # marked to be converted again. Nothing from a stopped run is recorded or fed.
  observeEvent(input$cv_stop, {
    run <- cv_run()
    if (is.null(cv_slot$live()) || is.null(run)) return()
    cv_slot$cancel(); cv_run(NULL)
    p <- cv_plan(); b <- cv_batch()
    if (!is.null(b) && !is.null(p$rows) && nrow(b) == nrow(p$rows)) {
      for (i in run$rows) {
        b$status[i] <- "stopped"; b$rows[i] <- NA_integer_; b$trust[i] <- NA_character_
        b$failing_check[i] <- NA_character_
        r <- b$result[[i]]
        if (is.list(r)) { r$outputs <- character(0); b$result[i] <- list(r) }
      }
      cv_batch(b)
      ran <- cv_plan_ran()
      if (!is.null(ran)) { ran$expected[run$rows] <- NA_character_; cv_plan_ran(ran) }
    } else {
      cv_plan_ran(NULL)        # nothing was converted: back to before Convert
    }
    notify_once("cv_stopped", "Stopped. Nothing from that run was kept - press Convert to start again.",
                type = "message", duration = 6)
  })

  # A row clicked: that file's full result, below the table. "Please check" in the
  # row does the same and then takes the page down to the check.
  observeEvent(input$cv_plan_open, {
    v <- input$cv_plan_open; p <- cv_plan()
    i <- suppressWarnings(as.integer(v$row %||% NA)[1])
    if (is.null(p) || !identical(suppressWarnings(as.integer(v$gen %||% NA)[1]), p$gen) || is.na(i)) return()
    go <- function() {
      if (NROW(p$rows) > 1L) open_batch_row(i)
      if (isTRUE(v$check)) { cv_ck_open(TRUE); session$sendCustomMessage("ss-scroll", "cv_check") }
    }
    if (!is.null(ck_pending())) .ck_commit(then = go) else go()
  })

  # plan_effective(p, picks) -> per row, the bank to GIVE the conversion: NA = take
  # it from the statement (rule 1), else the bank chosen (rule 2). A pick that is the
  # bank the statement named anyway is rule 1: nothing was changed.
  plan_effective <- function(p, picks) {
    n <- NROW(p$rows); length(picks) <- n
    vapply(seq_len(n), function(i) {
      v <- picks[i]
      if (is.na(v) || !nzchar(v) || identical(v, p$rows$bank[i])) NA_character_ else v
    }, character(1))
  }
  # plan_shown(p, picks) -> per row, the bank the row's dropdown shows.
  plan_shown <- function(p, picks) {
    n <- NROW(p$rows); length(picks) <- n
    ifelse(is.na(picks), p$rows$bank, picks)
  }
  # plan_changed() -> the rows whose bank has changed since they were converted
  # (none before the first run).
  plan_changed <- function() {
    p <- cv_plan(); ran <- cv_plan_ran()
    if (is.null(p) || is.null(p$rows) || is.null(ran) || !identical(ran$gen, p$gen))
      return(integer(0))
    now <- plan_effective(p, cv_plan_picks()); now[is.na(now)] <- ""
    which(is.na(ran$expected) | now != ran$expected)
  }
  # plan_again() -> the rows Convert will run now on a case folder: the changed ones
  # when this case has results to keep, otherwise NULL (= every file).
  plan_again <- function() {
    n <- NROW(input$cv_file); b <- cv_batch()
    if (n <= 1L || is.null(b) || nrow(b) != n) return(NULL)
    ch <- plan_changed()
    if (length(ch)) ch else NULL
  }

  # What each row's state says, in her words, with the reason as the hover. The
  # bank is the tool's SUGGESTION to check, never a verdict.
  .plan_chip <- function(r) {
    st <- as.character(r$state)[1]
    if (!is.na(r$remembered %||% NA)) return(c("plan-ok", "Same bank as last time"))
    if (st %in% c("ready", "scan_ready")) {
      if (is.na(r$bank)) return(c("plan-warn", "Please choose the bank"))
      if (isTRUE(r$ask)) return(c("plan-warn", "Please check the bank"))
      return(c("plan-ok", if (identical(st, "scan_ready")) "From the scan" else "From the statement"))
    }
    switch(st,
      scanned        = c("plan-info", "Scanned"),
      scanning       = c("plan-info", "Reading the scan\u2026"),
      scanned_no_ocr = c("plan-bad", "Scanned - can't be read here"),
      unsupported_type = c("plan-bad", "Not a file type this reads"),
      checking       = c("plan-info", "Checking\u2026"),
      c("plan-bad", "Can't be read"))
  }
  # The hover on a row's chip: the identifier's own sentence where there is one,
  # and for a scan, why its bank is not named yet.
  .plan_hover <- function(r) {
    if (!is.na(r$remembered %||% NA)) return(r$remembered)
    if (identical(r$state, "scanned_no_ocr"))
      return(paste("This file is a picture of a statement, and this server has no OCR software to",
                   "read it. Ask whoever looks after the tool, or get a text PDF, CSV or Excel export",
                   "from the bank."))
    if (identical(r$state, "scanning"))
      return("Its first pages are being read as pictures to find its bank - a few seconds a page.")
    if (identical(r$state, "scanned") && is.na(r$detail))
      return(paste("This file is a picture of a statement. Its text only exists once it",
                   "has been read as a picture, so its bank is found while it converts."))
    # the engine's own reasons can start lower-case; a hover is a sentence
    if (!is.na(r$detail)) paste0(toupper(substr(r$detail, 1, 1)), substring(r$detail, 2)) else NULL
  }

  # .plan_select(gen, i, choices, selected, name, locked) -- the row's bank dropdown:
  # a plain <select> (see "THE CHOICES LIVE HERE"), every bank, and the way to name
  # one the list does not have.
  .plan_select <- function(gen, i, choices, selected, name, locked = FALSE) {
    sel <- if (is.na(selected) || !nzchar(selected)) "" else selected
    if (nzchar(sel) && !(sel %in% choices)) choices <- c(stats::setNames(sel, sel), choices)
    opt <- function(v, lab) tags$option(value = v,
      selected = if (identical(v, sel)) NA else NULL, lab)
    tags$select(class = "plan-pick", `data-gen` = gen, `data-row` = i,
      `aria-label` = sprintf("Bank for %s", name),
      disabled = if (isTRUE(locked)) NA else NULL,
      if (!nzchar(sel)) opt("", "Choose the bank\u2026"),
      lapply(seq_along(choices), function(k) opt(unname(choices[k]), names(choices)[k])),
      opt("__new__", "Another bank - type its name\u2026"))
  }

  # .res_layouts(res) -> the learned layouts a result was read with, as names, one
  # per statement that had one; a layout the reading STARTED says so.
  .res_layouts <- function(res) {
    out <- unlist(lapply(res$reading %||% list(), function(rd) {
      # a recipe names the design it read: "ANZ Business Premium Call Account"
      if (!is.null(rd$matched_recipe) && !isTRUE(rd$draft))
        return(sprintf("%s %s", .layout_bank_display(rd$recipe_bank %||% "", rd$recipe_bank %||% ""), rd$recipe_title %||% rd$matched_recipe))
      if (!is.null(rd$learned_recipe) || isTRUE(rd$draft)) return("New design (being learned)")
      if (!is.null(rd$matched_layout)) return(.read_as_name(rd$matched_layout))
      if (!is.null(rd$learned_layout))
        return(.read_as_name(rd$learned_layout, if (identical(rd$learn$action, "created")) "new" else "learned now"))
      # proved on its own content, then found to be a layout already proven: that
      # layout is what read it, though nothing new was learned
      if (identical(rd$learn$action, "none") && !is.null(rd$learn$ref)) return(.read_as_name(rd$learn$ref))
      NULL
    }))
    unique(out[!is.na(out)])
  }
  # .bank_disputed(res) -> the bank the statement itself names, when it is not the
  # bank it was read as and so nothing was learned from it; NULL otherwise.
  .bank_disputed <- function(res) {
    bk <- res$bank
    if (is.null(bk) || !isTRUE(bk$block_learning)) return(NULL)
    used <- as.character(bk$bank %||% NA_character_)[1]
    seen <- as.character(bk$institution %||% NA_character_)[1]
    if (is.na(used) || is.na(seen) || identical(used, seen)) return(NULL)
    as.character(bk$identified_display %||% .bank_label(seen) %||% seen)[1]
  }
  # .res_layout_ref(res) -- the one layout reference a result is filed under (the
  # upload record, the feedback): the first statement's matched or learned layout.
  .res_layout_ref <- function(res) {
    for (rd in res$reading %||% list()) {
      ref <- rd$matched_layout %||% rd$learned_layout
      if (!is.null(ref)) return(as.character(ref)[1])
    }
    NA_character_
  }

  # .plan_outcome(o, n_rows, check_link) -- a file's Outcome cell: the phrase, the
  # reason a person acts on, and the way to Please check. One builder for a
  # finished case and for each file's verdict as it arrives mid-run, so the two can
  # never say it differently.
  # .check_words(res) -- what a row's check link opens, said on the link itself:
  # "Check 4 rows" for a reading with rows, "Check the columns" for one without.
  .check_words <- function(res) {
    n <- suppressWarnings(as.integer(.rows_of(res))[1])
    if (is.na(n) || n < 1L) "Check the columns"
    else sprintf("Check %s row%s", format(n, big.mark = ","), if (n == 1L) "" else "s")
  }
  .plan_outcome <- function(o, n_rows = NA, link = NULL, again = FALSE, nxt = NULL) {
    # Two lines at most: a status pill with the reason (or, done, the row count in
    # muted text) beside it; then the one next step. Colour lives in the pill only.
    n_rows <- suppressWarnings(as.integer(n_rows)[1])
    why <- short_reason(o$why %||% "")
    pill <- span(class = paste("pill plan-verdict", paste0("o-", o$cls),
                               c(ok = "pill-ok", warn = "pill-warn", bad = "pill-bad")[[o$cls]] %||% ""),
                 title = OUTCOME_HELP[[o$cls]], o$word)
    rows_txt <- if (!is.na(n_rows) && o$cls == "ok")
      span(class = "plan-sub", sprintf("%s row%s", format(n_rows, big.mark = ","), if (identical(n_rows, 1L)) "" else "s"))
    tags$td(class = "plan-res",
      div(class = "plan-line1", pill, if (o$cls == "ok") rows_txt, link),
      if (o$cls == "warn" && nzchar(why)) div(class = "plan-why", title = why, why),
      if (o$cls == "bad" && length(nxt) && nzchar(nxt)) div(class = "plan-next", title = why, nxt),
      if (again) span(class = "plan-chip plan-mine", "Bank changed"))
  }

  # The result for row i of THIS upload, once it has one: the case's frame, or for a
  # single file the result on the page.
  .row_result <- function(i, p) {
    b <- cv_batch()
    if (!is.null(b) && nrow(b) == NROW(p$rows)) return(b$result[[i]])
    ran <- cv_plan_ran()
    if (NROW(p$rows) == 1L && !is.null(ran) && identical(ran$gen, p$gen)) return(cv_res())
    NULL
  }

  # .case_order(b) -- the rows of a case in the order the table shows them: the
  # files that need a person first, grouped by what went wrong.
  .case_order <- function(b) {
    sev <- match(b$status, BATCH_STATUSES, nomatch = length(BATCH_STATUSES) + 1L)
    order(-sev, as.character(b$failing_check), seq_len(nrow(b)))
  }
  # .case_addsup(b) -- the rows "Accept all that add up" accepts: held ONLY because
  # their design is new (always ask once), every statement in them adding up.
  .case_addsup <- function(b) {
    if (is.null(b) || !nrow(b)) return(integer(0))
    which(vapply(seq_len(nrow(b)), function(i) {
      r <- b$result[[i]]
      is.list(r) && identical(as.character(r$status %||% "")[1], "needs_review") && isTRUE(.new_design(r))
    }, logical(1)))
  }
  # .case_now(cls, n_add) -- the one line at the top of a case: what happens now.
  .case_now <- function(cls, n_add = 0L) {
    w <- sum(cls == "warn"); bad <- sum(cls == "bad")
    if (w > 0L)
      return(paste0(sprintf("%d file%s need%s you. ", w, if (w == 1L) "" else "s", if (w == 1L) "s" else ""),
        if (n_add > 0L) sprintf("%d of them add up - accept them in one go, or ", n_add) else "",
        if (n_add > 0L) "press Next file to check." else "Press Next file to check, or click any file to see it."))
    if (bad > 0L)
      return(sprintf("Everything that could be read is done. %d couldn't be read - each says what to try.", bad))
    "All done. Download everything, or click a file to see it."
  }
  observeEvent(input$cv_drop_none, notify_once("cv_drop",
    "Nothing to add there - drop PDF, CSV or Excel statements, or a folder of them.", type = "warning", duration = 6))
  observeEvent(input$cv_next_check, {
    if (!is.null(ck_pending())) return(.ck_commit(then = .next_check))
    .next_check()
  })
  .next_check <- function() {
    b <- isolate(cv_batch()); if (is.null(b) || !nrow(b)) return()
    ord <- .case_order(b)
    need <- ord[vapply(ord, function(i) identical(plain_outcome(b$result[[i]]$status, NA, b$result[[i]]$feed_basis)$cls, "warn"), NA)]
    if (!length(need)) { notify_once("cv_next", "Nothing left to check.", duration = 4); return() }
    cur <- isolate(cv_batch_row())
    pos <- if (is.na(cur) || !(cur %in% ord)) 0L else match(cur, ord)
    after <- need[match(need, ord) > pos]
    i <- if (length(after)) after[1] else need[1]
    open_batch_row(i)
    cv_ck_open(TRUE); session$sendCustomMessage("ss-scroll", "cv_check")
  }
  observeEvent(input$cv_accept_all, {
    b <- cv_batch(); f <- input$cv_file
    rows <- .case_addsup(b)
    if (!length(rows) || is.null(f) || nrow(f) != nrow(b)) return()
    if (!.identity_ok()) return()
    notify_once("cv_accept_all", sprintf("Accepting %d file%s that add up - each is recorded as checked by you.",
                length(rows), if (length(rows) == 1L) "" else "s"), duration = 6)
    run_batch(f, as.character(b$chosen %||% rep(NA_character_, nrow(b))), rows = rows, confirm = TRUE)
  })

  output$cv_plan <- renderUI({
    cv_plan_redraw()
    busy <- cv_plan_busy()
    if (!is.null(busy))
      return(div(class = "plan plan-busy",
        sprintf("Checking %d of %d file%s\u2026", min(busy$n, cv_plan_done() + 1L), busy$n,
                if (busy$n == 1L) "" else "s")))
    p <- cv_plan(); if (is.null(p)) return(NULL)
    if (isTRUE(p$too_many > 0L))
      return(div(class = "plan note-bad", sprintf(paste(
        "%d files chosen, and this tool takes %d at a time. Choose up to %d and",
        "convert the rest after."), p$too_many, MAX_BATCH_FILES, MAX_BATCH_FILES)))
    rows <- p$rows; if (is.null(rows) || !nrow(rows)) return(NULL)
    picks <- cv_plan_picks(); length(picks) <- nrow(rows)
    shown <- plan_shown(p, picks); eff <- plan_effective(p, picks)
    banks <- bank_list()
    one <- nrow(rows) == 1L
    ran <- cv_plan_ran(); ran_here <- !is.null(ran) && identical(ran$gen, p$gen)
    changed <- plan_changed()
    b <- cv_batch()
    case_res <- !one && !is.null(b) && nrow(b) == nrow(rows)
    single_res <- one && ran_here && !is.null(cv_res())
    has_res <- case_res || single_res
    # ...and a case converting right now, for THIS upload: its rows fill in as each
    # file finishes (cv_slot$live, R/jobs.R job_done_rows)
    live <- cv_slot$live(); run <- cv_run()
    running <- !is.null(live) && !is.null(run) && identical(run$gen, p$gen)
    cols <- has_res || running
    open <- if (running) NA_integer_ else if (one) 1L else cv_batch_row()
    # worst first once there are results -- the files that need a person at the top,
    # grouped by what went wrong (BATCH_STATUSES is worst-LAST). A first run keeps
    # the upload order while it runs, so rows do not jump about.
    ord <- seq_len(nrow(rows))
    if (case_res) ord <- .case_order(b)
    trs <- lapply(ord, function(i) {
      r <- rows[i, ]
      chip <- .plan_chip(r)
      kind <- tagList(r$kind, if (!is.na(r$pages)) div(class = "plan-sub",
        sprintf("%d page%s", r$pages, if (r$pages == 1L) "" else "s")))
      pickable <- !(r$state %in% c("unreadable", "unsupported_type"))
      mine <- !is.na(eff[i])
      # locked while a case converts: a choice made mid-run would apply to nothing
      ctl <- if (pickable) .plan_select(p$gen, i, banks, shown[i], r$name, locked = running)
             else span(class = "muted", "\u2014")
      in_run <- running && i %in% run$rows
      res_i <- if (has_res && !in_run) .row_result(i, p) else NULL
      # A statement that names another bank than the one it was read as is said in
      # its own row: in a case, the note with the two buttons is only on its result.
      seen <- .bank_disputed(res_i)
      bank_cell <- tags$td(class = "plan-tpl", ctl,
        if (!is.null(seen)) div(class = "plan-note plan-note-warn",
                                sprintf("Which bank? The statement looks like %s", seen))
        else if (!cols && (!mine || !is.na(r$remembered %||% NA)))
          span(class = paste("plan-chip", chip[1]), title = .plan_hover(r), chip[2]))
      tail <- if (in_run) {
        k <- match(i, run$rows)
        d <- if (!is.null(live$done)) live$done[live$done$k == k, , drop = FALSE] else NULL
        if (!is.null(d) && nrow(d)) {
          o <- plain_outcome(d$status[1], d$outcome[1], basis = d$outcome[1],
                             reason = plain_failing_check(d$failing_check[1]))
          list(tags$td(class = "plan-layout", { ra <- .read_as_name(d$layout[1]); if (is.na(ra)) "" else ra }),
               .plan_outcome(o, d$rows[1]))
        } else if (identical(live$state, "running") && identical(as.integer(live$i), as.integer(k)))
          list(tags$td(""), tags$td(class = "plan-res", div(class = "plan-converting",
            if (!is.na(live$says %||% NA)) paste0(live$says, "\u2026") else "Converting\u2026")))
        else list(tags$td(""), tags$td(class = "plan-res", span(class = "muted", "Waiting")))
      } else if (case_res && identical(as.character(b$status[i]), "stopped")) {
        list(tags$td(""), tags$td(class = "plan-res", span(class = "muted", "Stopped - press Convert to convert it")))
      } else if (!is.null(res_i)) {
        # the table's reason is the short phrase (D16): one word, then a few words
        o <- plain_outcome(res_i$status, res_i$outcome, res_i$feed_basis,
                           plain_failing_check(.failing_check(res_i)), res_i$person$fix)
        lys <- .res_layouts(res_i)
        # Please check has something to show only where columns were found
        has_cols <- any(vapply(res_i$reading %||% list(), function(rd) NROW(rd$columns) > 0L, logical(1)))
        link <- if (o$cls != "ok" && has_cols)
          tags$a(class = "plan-check", href = "#", `data-gen` = p$gen, `data-row` = i,
                 title = sprintf("%s: opens this file below, with its page and the question to answer", .check_words(res_i)),
                 `aria-label` = .check_words(res_i), "Check \u2192")
        nxt <- if (o$cls == "bad") unread_next(res_i$stamp$kind %||% NA, c(res_i$reason, res_i$messages), has_cols)
        list(tags$td(class = "plan-layout", if (length(lys)) lapply(lys, div) else NULL),
             .plan_outcome(o, .rows_of(res_i), link, again = i %in% changed, nxt = nxt))
      } else if (cols) list(tags$td(""), tags$td("")) else NULL
      openable <- case_res && !running && !identical(as.character(b$status[i]), "stopped")
      tags$tr(class = paste(c("plan-row", chip[1], if (mine) "plan-chosen", if (openable) "plan-openable",
                              if (has_res && !running && identical(open, i)) "plan-open",
                              if (in_run) "plan-in-run"), collapse = " "),
        `data-gen` = p$gen, `data-row` = i,
        tabindex = if (openable) "0" else NULL,
        title = if (openable) "Click for this file's full result" else NULL,
        tags$td(class = "plan-file", title = r$name, r$name),
        tags$td(class = "plan-kind", kind),
        bank_cell, tail)
    })
    head_cells <- if (cols) list(tags$th("Read as"), tags$th("Outcome")) else NULL
    tbl <- tags$table(class = paste("plan-table", if (cols) "plan-has-res"),
      tags$thead(tags$tr(tags$th("File"), tags$th("Type"), tags$th("Bank"), head_cells)),
      tags$tbody(trs))
    top <- if (running) {
      n <- length(run$rows); nd <- NROW(live$done)
      a <- suppressWarnings(as.integer(live$ahead))
      say <- if (identical(live$state, "queued")) {
        if (is.na(a) || a <= 0L) "Starting\u2026"
        else sprintf("Waiting for a free slot - %d conversion%s ahead of yours. Yours starts as soon as one finishes.",
                     a, if (a == 1L) "" else "s")
      } else sprintf("Converting %d of %d%s", min(n, max(1L, as.integer(live$i))), n,
                     if (!is.na(live$file)) paste0(" - ", live$file) else "")
      div(class = "plan-top plan-running",
        div(class = "plan-progress",
          p(class = "plan-head", say),
          div(class = "plan-bar", div(class = "plan-bar-fill",
            style = sprintf("width:%d%%", as.integer(round(100 * nd / max(1L, n))))))),
        actionButton("cv_stop", "Stop", class = "btn-default"))
    } else if (case_res) {
      cls <- vapply(seq_len(nrow(b)), function(i) {
        r <- b$result[[i]]
        if (identical(as.character(b$status[i]), "stopped")) "stopped"
        else plain_outcome(r$status, r$outcome, r$feed_basis, r$reason)$cls
      }, "")
      say <- function(x, word, k) { v <- sum(cls == x); if (v > 0L) span(class = paste("count-chip", k), sprintf("%d %s", v, word)) else NULL }
      bits <- Filter(Negate(is.null), list(say("ok", "done", "cc-ok"), say("warn", "to check", "cc-warn"),
        say("bad", "couldn't read", "cc-bad"), say("stopped", "stopped", "")))
      n_warn <- sum(cls == "warn"); n_ok_add <- length(.case_addsup(b))
      title <- if (n_warn > 0L) sprintf("%d file%s need%s a quick check", n_warn, if (n_warn == 1L) "" else "s", if (n_warn == 1L) "s" else "")
               else if (sum(cls == "bad") > 0L) "Everything that could be read is done"
               else sprintf("All %d files are done", nrow(rows))
      div(class = "plan-top plan-top-case",
        div(class = "plan-top-say",
            h2(class = "plan-head plan-h2", title),
            div(class = "plan-tally", bits),
            p(class = "plan-now", .case_now(cls, n_ok_add))),
        div(class = "plan-actions",
          if (n_ok_add > 0L)
            actionButton("cv_accept_all", sprintf("Accept all that add up (%d)", n_ok_add), class = "btn-default",
                         title = "Accepts every new-design file whose balance adds up, each recorded as checked by you"),
          if (n_warn > 0L)
            actionButton("cv_next_check", "Next file to check \u2192", class = "btn-primary",
                         title = "Opens the next file that needs you, with its question"),
          if (length(.batch_outputs(b)))
            downloadButton("cv_batch_dl", "Download everything", class = if (n_warn > 0L) "btn-default" else "btn-primary")))
    } else {
      p(class = "plan-head",
        if (single_res) "Not the right bank? Choose another and press Convert again."
        else if (one) "Check the bank, then press Convert."
        else sprintf("Check the bank for each of these %d files, then press Convert.", nrow(rows)))
    }
    foot <- if (running) {
      p(class = "muted plan-foot",
        "Each file's result appears here as soon as it is done. You can keep reading this page while it works.")
    } else if (case_res) {
      p(class = "muted plan-foot",
        if (length(changed))
          sprintf("%d changed - press Convert to read %s again. The rest keep their results.",
                  length(changed), if (length(changed) == 1L) "it" else "them")
        else NULL)
    }
    div(class = paste("plan", if (running) "plan-is-running"), top, div(class = "plan-scroll", tbl), foot)
  })

  cv_res <- reactiveVal(NULL)
  cv_dir <- reactiveVal(NULL)
  cv_src <- reactiveVal(NULL)      # the file on the page: list(path, name, bank, bank_confirmed)
  cv_fb_done <- reactiveVal(FALSE)
  cv_fb_rec  <- reactiveVal(NULL)              # the feedback record, incl. any feed retraction
  cv_upload_id <- reactiveVal(NA_character_)   # the tracked upload for this conversion
  cv_feed_gate <- reactiveVal(NULL)            # what the governed feed did with it
  cv_recorded  <- reactiveVal(FALSE)           # ...and whether this run feeds at all
  cv_spot_done <- reactiveVal(NA_character_)   # the spot-check answer given for this result
  cv_ck_note   <- reactiveVal(NULL)            # what the last re-read found: list(run_id, ok, text)
  cv_ov        <- reactiveVal(NULL)            # the fix the result on the page was read with

  # convert_args(...) -- the arguments the front door is called with, in ONE place,
  # so Convert, a case, a re-read and a confirm can never ask for different things.
  # `bank` only when the person chose it (rule 2 above).
  convert_args <- function(bank = NULL, bank_confirmed = FALSE, overrides = NULL, confirm = FALSE) {
    list(requested_by = who_now(), logdir = LOGDIR, layouts_dir = LAYOUTS_DIR,
         tracking_dir = TRACKING_DIR, bank = bank, bank_confirmed = isTRUE(bank_confirmed),
         overrides = overrides, confirm = isTRUE(confirm))
  }

  # run_conversion -- the whole convert-a-file flow (session dir, convert, state,
  # upload capture), shared by the Convert button, "Try it on a sample" and Admin's
  # "Read it again on Convert". record = FALSE skips the uploads capture and the
  # feed (the bundled sample is not a team statement; a saved upload is already
  # recorded).
  run_conversion <- function(srcpath, name, record = TRUE, bank = NULL, upload_id = NULL) {
    if (.case_converting()) return(invisible(NULL))
    old <- isolate(cv_dir())
    sess <- tempfile("cv_")   # guaranteed-unique per session/process (no cross-user bleed)
    # THE STATEMENT IN in/, ITS OUTPUTS BESIDE IT. Outputs are named after the file
    # they came from, so a CSV statement and the CSV it converts to share a name: in
    # one folder the conversion wrote over the statement, and anything reading it
    # again (Please check) read its own output instead.
    dir.create(file.path(sess, "in"), showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, "in", name)
    file.copy(srcpath, src, overwrite = TRUE)
    # STOP the previous conversion, then reclaim its scratch folder -- in this
    # order: it is a separate process and it is still writing in there. The folder
    # holds the outputs AND a copy of the client's statement. AFTER the copy above,
    # because a re-convert is often handed the file that lives in it.
    cv_slot$cancel()
    if (!is.null(old) && nzchar(old) && !identical(old, sess) && dir.exists(old))
      safe(unlink(old, recursive = TRUE))
    # ...and anything this process left behind more than a day ago, which is what a
    # browser closed abruptly (no onSessionEnded) leaves lying about.
    safe(sweep_temp_dirs(keep_hours = 24, exclude = sess))
    # A single conversion ends any case folder on screen: the sweep above has just
    # reclaimed the batch's scratch folder, so every other file's workbook is gone,
    # and a table whose downloads no longer resolve is worse than no table. It ends
    # the statement on screen too, for the same reason.
    cv_batch(NULL); cv_batch_row(NA_integer_)
    show_result()
    cv_dir(sess)
    who <- who_now()
    # HOW LONG, UP FRONT. A digital page costs 0.17s and a SCANNED page 9.3s --
    # measured, 55x apart -- and nineteen minutes of silence is indistinguishable
    # from a hung tool. The probe costs 0.05s on a 400-page file.
    est <- safe(conversion_estimate(src), NULL)
    cv_slot$start("convert", src, sess,
      message = paste(c("Converting statement\u2026", est$note %||% ""), collapse = " "),
      args = convert_args(bank = bank),
      finish = function(res) {
        # Complete the audit record with the attested vs detected identity split.
        stamp_identity(res$run_id %||% NA_character_)
        layouts_bump(isolate(layouts_bump()) + 1L)     # it may have learned
        # Capture the upload + its outcome so a statement that read nothing is a
        # 2-second pickup in Admin -> Uploads (the file is saved for a safe re-audit).
        uid <- if (record) safe(record_upload(src, name = name, requested_by = who,
          status = res$status %||% "failed", run_id = res$run_id %||% NA_character_,
          template = .res_layout_ref(res),
          trust = res$trust$level %||% NA_character_,
          detail = paste(res$messages, collapse = "; "), dir = UPLOADS_DIR), NA_character_)
        else NA_character_
        show_result(res, list(path = src, name = name, bank = bank), upload_id %||% uid)
        if (record) .remember_bank(1L, name, res, bank)
        # ...and this is what the governed feed did with it (the last word on
        # cv_recorded / cv_feed_gate, which show_result has just cleared).
        publish_result(res, record)
      })
  }

  # show_result(res, src, upload_id, gate, recorded) -- THE RESULT PAGE'S STATE.
  # Everything below the Convert table (the verdict, the downloads, Please check,
  # the transactions, the feedback panel) reads these reactives and nothing else.
  #
  # It is the one place a RESULT is opened, and it is NOT the only writer of every
  # field in it: publish_result() sets cv_recorded / cv_feed_gate, because the feed
  # verdict is only known AFTER the write. Called with no arguments it means "no
  # statement is open" -- which is exactly the state a case table sits in until a
  # row is clicked. One reactive left behind would put the PREVIOUS statement's
  # feed verdict, feedback panel or download beside this statement's figures.
  show_result <- function(res = NULL, src = NULL, upload_id = NA_character_,
                          gate = NULL, recorded = FALSE) {
    cv_res(res)
    cv_src(src)                        # the file itself, for Please check
    cv_upload_id(upload_id)            # the tracked upload this result belongs to
    cv_feed_gate(gate)                 # what the governed feed did with THIS run
    cv_recorded(isTRUE(recorded))      # ...and whether this run feeds at all
    cv_fb_done(FALSE); cv_fb_rec(NULL) # the rating is per statement
    cv_spot_done(NA_character_)        # so is the spot check
    cv_ov(NULL); cv_ck_note(NULL)      # and the fix being worked on
    cv_ck_open(FALSE)
    invisible(res)
  }

  # publish_result(res, record) -- write the governed feed for this conversion and
  # keep the gate's verdict for Admin. ONE place, so what reaches Qlik and what any
  # screen claims about Qlik always come from the same run. `record = FALSE` (the
  # bundled sample) neither feeds nor claims anything about the feed. Returns the
  # gate so a caller converting MANY files can keep one verdict per file.
  publish_result <- function(res, record) {
    cv_recorded(isTRUE(record))
    gate <- if (isTRUE(record)) safe(write_feed(res, CONFIG), NULL) else NULL
    cv_feed_gate(gate)
    invisible(gate)
  }

  # ---- A WHOLE CASE FOLDER, through the same front door ----------------------
  #
  # convert_batch() (R/batch.R) runs each file through convert_statement(), so a
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
  # convert_statement() writes to `outdir` under
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

  # `banks`: one entry per file from the Convert table (plan_effective) -- NA takes
  # that file's bank from the statement, a bank reads it as exactly that bank.
  # `rows`: CONVERT AGAIN -- only these files of the case already on screen (the
  # rows whose bank changed, plan_again). Their copies are already in this case's
  # scratch folder and their outputs are written over in place; every other row
  # keeps its result and its files, and the new results are merged into the table.
  run_batch <- function(files, banks = NULL, rows = NULL, confirm = FALSE) {
    if (.case_converting()) return(invisible(NULL))
    banks <- as.character(banks %||% rep(NA_character_, NROW(files)))
    b_old <- isolate(cv_batch())
    again <- !is.null(rows) && length(rows) && !is.null(b_old) && nrow(b_old) == NROW(files) &&
             all(file.exists(as.character(b_old$file[rows])))
    gen <- plan_env$gen
    if (again) {
      sess <- isolate(cv_dir())
      paths <- as.character(b_old$file[rows]); nms <- basename(paths)
      banks <- banks[rows]
      cv_slot$cancel()
      # the file open below may be one being re-read; its old result must not sit
      # under the table while the new one is made
      show_result(); cv_batch_row(NA_integer_)
    } else {
      rows <- seq_len(NROW(files))
      old <- isolate(cv_dir())
      sess <- tempfile("cvb_")
      # in/ for the statements, the case folder for their outputs (see run_conversion)
      dir.create(file.path(sess, "in"), showWarnings = FALSE, recursive = TRUE)
      nms <- .unique_names(files$name)
      paths <- file.path(sess, "in", nms)
      file.copy(as.character(files$datapath), paths, overwrite = TRUE)
      # Stop whatever this session had running before its folder is reclaimed: it is
      # a separate process, and it is still writing in there.
      cv_slot$cancel()
      if (!is.null(old) && nzchar(old) && !identical(old, sess) && dir.exists(old))
        safe(unlink(old, recursive = TRUE))
      safe(sweep_temp_dirs(keep_hours = 24, exclude = sess))
      show_result(); cv_batch_row(NA_integer_); cv_batch(NULL)
      cv_dir(sess)
    }
    who <- who_now(); n <- length(paths)
    cv_run(list(gen = gen, rows = rows))
    # A 50-file case must not look frozen, and it must not freeze the other analysts
    # either: ONE job for the whole case, not one per file -- thirty files would
    # otherwise fill the cap on their own and put the whole team behind one case.
    #
    # convert_batch() hands back each file's WHOLE result, rows included; the
    # governed feed is written from those rows, so they are dropped below, per file,
    # the moment that write is done. Anything convert_batch does not itself take
    # goes to convert_statement(), which has no `...`, so a stray argument here
    # fails every file in the case.
    # overlay = FALSE: the Convert table shows this case's progress row by row
    a <- convert_args(); a$bank <- NULL; a$overrides <- NULL; a$confirm <- NULL; a$bank_confirmed <- NULL
    # "Accept all that add up": the same confirm a person gives one file with It's
    # right, given to each of these rows -- the engine still refuses any that do not
    # add up, and records each one as checked by this person.
    if (isTRUE(confirm)) a$confirm <- TRUE
    cv_slot$start("batch", paths, sess, overlay = FALSE,
      message = sprintf("Converting %d file%s\u2026", n, if (n == 1L) "" else "s"),
      args = c(a, list(banks = banks)),
      finish = function(b) {
        cv_run(NULL)
        layouts_bump(isolate(layouts_bump()) + 1L)     # it may have learned
        # A case that never came back is not an empty case. Say so on the verdict
        # card rather than draw a table of nothing.
        if (!is.data.frame(b)) return(show_result(b, NULL, NA_character_))
        # Each file finishes exactly as a single conversion does: its audit record is
        # completed with who ran it, its upload is captured for pickup, and it goes
        # through the governed feed. "Convert thirty" must mean "convert one, thirty
        # times".
        b$upload_id <- rep(NA_character_, n)
        b$feed_gate <- vector("list", n)
        for (i in seq_len(n)) {
          res <- b$result[[i]]
          stamp_identity(res$run_id %||% NA_character_)
          b$upload_id[i] <- safe(record_upload(paths[i], name = nms[i], requested_by = who,
            status = res$status %||% "failed", run_id = res$run_id %||% NA_character_,
            template = .res_layout_ref(res),
            trust = res$trust$level %||% NA_character_,
            detail = paste(res$messages, collapse = "; "), dir = UPLOADS_DIR), NA_character_)
          # `[i] <- list(...)`, never `[[i]] <-`: the gate is NULL when the feed is
          # switched off, and assigning NULL with [[ DELETES the element instead of
          # storing it - the column would come up one short of the files.
          b$feed_gate[i] <- list(publish_result(res, TRUE))
          .remember_bank(rows[i], nms[i], res, .chosen_bank(b, i))
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
        # Recorded above whatever happens; DRAWN only while these are still the files
        # chosen.
        if (!identical(plan_env$gen, gen)) return(invisible(NULL))
        # Convert again: the new results take their rows' places, column by column
        # (`[<-` on a list column keeps a NULL feed verdict as an element).
        if (again) {
          bb <- isolate(cv_batch())
          if (!is.null(bb) && nrow(bb) >= max(rows) && identical(names(bb), names(b))) {
            for (col in names(b)) bb[[col]][rows] <- b[[col]]
            b <- bb
          }
        }
        cv_batch(b)
      })
  }

  # open_batch_row(i) -- put THAT file's result on the ordinary result page. It
  # goes through show_result(), the same one line a single conversion uses, so the
  # page below is not a copy of the result view, it IS the result view.
  # .chosen_bank(b, i) -- the bank chosen for file i of a case, or NULL when it was
  # taken from the statement. A re-read of that file must be read the same way.
  .chosen_bank <- function(b, i) {
    v <- as.character(b$chosen %||% character(0))[i]
    if (length(v) != 1L || is.na(v) || !nzchar(v)) NULL else v
  }
  open_batch_row <- function(i) {
    b <- cv_batch()
    if (is.null(b) || length(i) != 1L || is.na(i) || i < 1L || i > nrow(b)) return(invisible(FALSE))
    show_result(b$result[[i]],
                src = list(path = b$file[i], name = basename(b$file[i]), bank = .chosen_bank(b, i), row = i),
                upload_id = b$upload_id[i],
                # This file really was fed, in run_batch's loop: its own verdict,
                # never the one belonging to whichever row was open before.
                gate = b$feed_gate[[i]], recorded = TRUE)
    cv_batch_row(as.integer(i))
    invisible(TRUE)
  }

  # WHAT A CASE FOLDER IS ACTUALLY FOR, ONCE THE TABLE HAS BEEN READ.
  #
  # "Thirty files, three failed: no way to re-run just those three and no way to
  # download the other twenty-seven." Both answers are in the Convert table itself:
  # set a row right on Please check (or change its bank and press Convert, which
  # re-reads exactly the rows whose bank changed), and "Download everything" sits
  # above the table.
  #
  # THE DOWNLOAD ONLY APPEARS WITH SOMETHING IN IT: a control that cannot do
  # anything is worse than no control.
  .batch_outputs <- function(b) {
    if (is.null(b)) return(character(0))
    p <- unlist(lapply(b$result, function(r) as.character(r$outputs %||% character(0))))
    p <- unique(p[nzchar(p)])
    p[file.exists(p)]
  }
  # ONE FILE HOLDING THE WHOLE CASE. The zip is built from the outputs already on
  # disk, so it carries exactly what the per-file buttons carry, and its name is
  # the case's own scratch handle rather than "download.zip".
  output$cv_batch_dl <- downloadHandler(
    filename = function() sprintf("converted-case-%s.zip", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      outs <- .batch_outputs(cv_batch())
      if (!length(outs)) {
        notify_once("dl", NOTHING_TO_DL, duration = 6)
        .dl_note(file, NOTHING_TO_DL)
        return(.dl_log("case-zip:nothing", file = file))
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
    n <- NROW(input$cv_file); got <- n > 0L
    # WAITS FOR THE TABLE. Converting before the files are checked would skip the
    # one look this table exists to give -- it is a second or two, and it says so.
    busy <- !is.null(cv_plan_busy())
    conv <- !is.null(cv_slot$live())     # a case converting: its table shows the progress
    # The button says what it is about to do. Twelve files selected and a button
    # marked "Convert" leaves the user to wonder whether it means all of them.
    lab <- if (n > 1L) sprintf("Convert %d files", n) else "Convert"
    # ...and after a run, what pressing it again will do: only the files whose
    # bank has changed, or (nothing changed) every one of them again.
    p <- cv_plan(); ran <- cv_plan_ran()
    if (!busy && !is.null(p) && !is.null(ran) && identical(ran$gen, p$gen)) {
      ch <- plan_again()
      lab <- if (n <= 1L) "Convert again"
             else if (length(ch)) sprintf("Convert %d changed file%s", length(ch),
                                          if (length(ch) == 1L) "" else "s")
             else "Convert again"
    }
    if (conv) lab <- "Converting\u2026"
    if (who && got && !busy && !conv)
      return(actionButton("cv_go", lab, class = "btn-primary btn-lg btn-block"))
    tagList(
      actionButton("cv_go", lab,
                   class = "btn-primary btn-lg btn-block disabled",
                   `aria-disabled` = "true"),
      p(class = "muted", style = "margin:6px 0 0;font-size:12.5px",
        if (!got && !who) "Choose a file above, and enter your QID."
        else if (!got) "Choose a file above."
        else if (!who) "Enter your QID above - it records who ran this conversion."
        else if (conv) "Each file's result fills in on the right as it finishes."
        else "Checking your files - a moment."))
  })
  outputOptions(output, "cv_go_btn", suspendWhenHidden = FALSE)

  observeEvent(input$cv_go, {
    f <- input$cv_file
    if (is.null(f) || !nrow(f)) {
      notify_once("cv_file", "Choose a statement file first.", type = "warning", duration = 6)
      return()
    }
    if (!.identity_ok()) return()
    # the button is greyed while the table is filled in; this is the same rule for a
    # press that arrives anyway (keyboard, a double click)
    if (!is.null(isolate(cv_plan_busy()))) {
      notify_once("cv_checking", "Still checking your files - Convert in a moment.",
                  type = "message", duration = 4)
      return()
    }
    if (.case_converting()) return()
    # scans still being read: stop, and leave their bank to be found while they convert
    if (!is.null(isolate(plan_slot$handle()))) {
      plan_slot$cancel(); plan_scan_apply(final = TRUE)
    }
    # TOO MANY FILES IS REFUSED BEFORE ANY WORK STARTS, and says the number.
    if (nrow(f) > MAX_BATCH_FILES) {
      notify_once("cv_toomany", sprintf(
        paste("%d files selected, and this tool takes %d at a time. Convert them in",
              "batches of %d or fewer - every file still gets its own result and its",
              "own checks, so splitting the folder changes nothing about the answers."),
        nrow(f), MAX_BATCH_FILES, MAX_BATCH_FILES), type = "warning", duration = 12)
      return()
    }
    # Each file with ITS row of the Convert table: the statement's own bank where the
    # row was left alone, exactly the chosen bank where it was changed. A table that
    # is not THIS upload's (the names do not line up) gives no bank at all.
    p <- isolate(cv_plan()); picks <- isolate(cv_plan_picks())
    mine <- !is.null(p) && !is.null(p$rows) && identical(p$rows$name, as.character(f$name))
    eff <- if (mine) plan_effective(p, picks) else rep(NA_character_, nrow(f))
    # On a case that has already run, only the rows whose bank has changed.
    again <- isolate(plan_again())
    rows <- again %||% seq_len(nrow(f))
    if (mine) {
      ran <- isolate(cv_plan_ran())
      e <- if (!is.null(ran) && identical(ran$gen, p$gen)) ran$expected else rep(NA_character_, nrow(f))
      now <- eff; now[is.na(now)] <- ""
      e[rows] <- now[rows]
      cv_plan_ran(list(gen = p$gen, expected = e))
    }
    if (nrow(f) > 1L) run_batch(f, eff, rows = again)
    else run_conversion(f$datapath[1], f$name[1], bank = if (is.na(eff[1])) NULL else eff[1])
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
  # .reread_on_convert(path, name, upload_id) -- Admin's "Read it again on Convert":
  # the saved statement converted afresh on the ordinary result page, where Please
  # check is. Not recorded again and not fed: the upload is already on record, and
  # it is the maintainer looking, not a case being worked.
  .reread_on_convert <- function(path, name, upload_id = NULL) {
    if (.case_converting()) return(invisible(NULL))
    updateTabsetPanel(session, "main_tabs", selected = "Convert")
    plan_reset()
    run_conversion(path, name, record = FALSE, upload_id = upload_id)
  }
  observeEvent(input$ab_go_convert, updateTabsetPanel(session, "main_tabs", selected = "Convert"))

  # A result exists once a conversion has run -- gates the whole result scaffold so
  # a first-time visitor never sees bare "Checks / Diagnostics" headers over empty
  # tables (which read as half-built).
  output$cv_has_result <- reactive({ !is.null(cv_res()) })
  outputOptions(output, "cv_has_result", suspendWhenHidden = FALSE)
  # A reading that produced rows: gates the analysis cards / graph / transactions,
  # so a file that read nothing shows its verdict + next step, not an empty
  # dashboard of zeros.
  output$cv_has_txns <- reactive({
    res <- cv_res()
    isTRUE((res$status %||% "") %in% c("ok", "needs_review")) &&
      length(res$outputs %||% character(0)) > 0
  })
  outputOptions(output, "cv_has_txns", suspendWhenHidden = FALSE)

  # ---------------------------------------------------------------------------
  # The charts, behind one control. STICKY BY DESIGN: once someone opens them they
  # stay open for every conversion for the rest of their session, and closing is
  # equally sticky. Nobody is asked whether they are "advanced"; they tell the tool
  # by clicking once.
  cv_detail_open <- reactiveVal(FALSE)
  observeEvent(input$cv_more, cv_detail_open(!isTRUE(cv_detail_open())))
  output$cv_detail_open <- reactive({ isTRUE(cv_detail_open()) })
  outputOptions(output, "cv_detail_open", suspendWhenHidden = FALSE)
  output$cv_more_toggle <- renderUI({
    res <- cv_res(); req(res)
    div(style = "margin:16px 0 6px",
      actionLink("cv_more", style = "font-weight:700;font-size:14.5px",
        label = if (isTRUE(cv_detail_open())) "Show less detail" else "Show more detail"))
  })

  # Empty state: shown before the first conversion. Tells a brand-new user what
  # this page is for, so the screen is never a mystery or a wall of empty headers.
  output$cv_empty <- renderUI({
    # Once files are chosen the Convert table above says what to do next, and a
    # sample statement is no use to somebody holding real ones.
    if (!is.null(cv_plan_busy()) || !is.null(cv_plan()$rows)) return(NULL)
    div(style = "max-width:560px;color:#444;line-height:1.6",
      h4(style = "margin-top:4px", "Convert a bank statement"),
      p("Upload a statement on the left - a ", tags$b("PDF"), ", ", tags$b("CSV"),
        " or ", tags$b("Excel"), " file - and click ", tags$b("Convert"), "."),
      p(class = "muted", "Each file's bank is filled in from the statement itself, so you can check it",
        "before anything converts. A statement whose own arithmetic proves the reading needs",
        "nothing more; one that does not is shown on Please check, with the reason."),
      # First visit, nothing to upload yet? One click shows the whole payoff on
      # a bundled specimen statement (public, synthetic - not anyone's real data).
      if (file.exists(SAMPLE_STATEMENT))
        div(style = "margin-top:14px;padding:12px 14px;background:#f8faf9;border:1px dashed #bfe0c8;border-radius:10px",
          actionButton("cv_try_sample", "Try it on a sample statement", class = "btn-default"),
          div(class = "muted", style = "margin-top:6px", "No file needed.")))
  })

  # .verdict_lines(res) -- the engine's messages for the verdict card, without the
  # row count the title already says, and without the bank sentence the bank note
  # under the card says with its buttons.
  .verdict_lines <- function(res) {
    m <- plain_messages(res$messages)
    m <- sub("^[0-9,]+ row\\(s\\);\\s*", "", m)
    said <- sub("[.]$", "", c(res$bank$why, res$reason))
    m <- m[!(sub("[.]$", "", m) %in% said)]
    m[nzchar(m)]
  }
  # .layout_chips(res) -- which learned layout read it, as a chip each.
  .layout_chips <- function(res) {
    ly <- .res_layouts(res)
    if (!length(ly)) return(NULL)
    div(lapply(ly, function(x) span(class = "chip", paste("Layout:", x))))
  }

  output$cv_status <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    # A converted statement gets the hero headline (cv_headline below); this card
    # is for anything that did NOT convert, so the reason is said up top. It is the
    # SAME verdict card -- one visual language for "how did it go".
    if (isTRUE(res$status == "ok")) return(NULL)
    st <- res$status %||% "failed"
    o <- plain_outcome(st, res$outcome, res$feed_basis, res$reason)
    lvl <- if (identical(st, "needs_review")) "medium" else "low"
    headline <- if (identical(st, "failed")) plain_status(st) else o$word
    # ...AND A HIGH-SEVERITY DIAGNOSIS NOBODY CAN FIX ON PLEASE CHECK OUTRANKS IT:
    # an image-only PDF on a machine with no OCR software is not a reading to check.
    bdx <- if (st %in% c("unsupported", "failed")) .blocking_diag(res) else NULL
    if (!is.null(bdx)) headline <- .sentence(bdx$detail[1])
    # ONE SENTENCE (D16). A new design that adds up says only that; a reading that
    # stops adding up says WHERE, from the table's own Check column; anything else
    # gives the reader's first reason. The checks list is under More detail.
    body <- if (identical(st, "needs_review") && .new_design(res))
      "It adds up. Look over the rows below; if they match the statement, press \u201cIt\u2019s right \u2014 accept it\u201d."
    else if (identical(st, "needs_review") && !is.null(first_x <- .first_cross(cv_data())))
      sprintf("The balance stops adding up at %s (row %d).", first_x$date, first_x$row)
    else if (nzchar(o$why) && is.null(bdx)) .first_sentence(o$why) else NULL
    if (identical(st, "needs_review") && .new_design(res)) headline <- "New design \u2014 check it once"
    div(class = paste0("verdict verdict-", lvl),
      div(class = "verdict-ico", "!"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", headline),
        if (!is.null(body)) p(class = "verdict-body", body),
        .audit_note(res)))
  })
  # .new_design(res) -- held only because no recipe knows its design (always ask once)
  .new_design <- function(res) {
    rd <- res$reading %||% list()
    length(rd) && all(vapply(rd, function(r) !is.null(r$new_design) || r$outcome %in% c("proven", "layout_match"), NA)) &&
      any(vapply(rd, function(r) !is.null(r$new_design), NA))
  }
  # .first_cross(d) -- the first row whose Check is a cross: list(row, date), or NULL
  .first_cross <- function(d) {
    if (is.null(d) || !("check" %in% names(d))) return(NULL)
    i <- which(d$check %in% "\u2717")[1]
    if (is.na(i)) NULL else list(row = i, date = format(as.Date(d$date[i]), "%d %b %Y"))
  }
  .first_sentence <- function(x) {
    x <- .sentence(x); m <- regexpr("^.*?[.](\\s|$)", x, perl = TRUE)
    if (m > 0) trimws(regmatches(x, m)) else x
  }

  # Row FLAGS worth a chip: things the engine recorded per row that a clean-looking
  # result would otherwise never mention on screen -- most importantly the
  # inferred year, which makes dates that LOOK proven merely likely. Counted off
  # the flags column of the produced table, so the chip and the file agree.
  ROW_FLAG_CHIPS <- c(
    date_year_inferred = "%d row(s) took their YEAR from a number on the page, not a statement period - confirm it",
    date_unresolved    = "%d row(s) have no year at all (none was printed) - day and month only")
  # cv_headline -- the plain-English verdict for a converted statement: which of
  # the outcomes it is, how many transactions, and the reader's own sentence for
  # why it can be trusted -- never a claim the reader did not make.
  output$cv_headline <- renderUI({
    res <- cv_res(); req(res)
    if (!isTRUE(res$status == "ok")) return(NULL)   # anything else is cv_status
    d <- cv_data(); n <- if (is.null(d)) .rows_of(res) else nrow(d)
    o <- plain_outcome("ok", res$outcome, res$feed_basis, res$reason, res$person$fix)
    lvl <- "high"; icon <- "\u2713"
    # A workbook with no audit record is not a green tick. (Verifier finding 2.)
    if (.audit_gap(res)) { lvl <- "medium"; icon <- "!" }
    # NOR IS A READING THE ARITHMETIC NEVER PROVED. A person vouched for it, which is
    # what lets it convert; the card says so rather than wearing the proven green.
    vouched <- identical(as.character(res$feed_basis %||% "")[1], "person") &&
      !identical(as.character(res$person$fix %||% "")[1], "boxes")
    if (vouched) lvl <- "medium"
    chip <- function(txt) span(class = "chip chip-warn", txt)
    fl <- if (!is.null(d) && "flags" %in% names(d)) as.character(d$flags) else character(0)
    chips <- Filter(Negate(is.null), lapply(names(ROW_FLAG_CHIPS), function(f) {
      nf <- sum(grepl(f, fl, fixed = TRUE))
      if (nf > 0) chip(sprintf(ROW_FLAG_CHIPS[[f]], nf))
    }))
    # ONE SENTENCE (D16): how it went, how many rows, and what it rests on. The
    # reader's own reasons, the checks and the design that read it are under More
    # detail; a row-level warning (a year taken from elsewhere) stays in sight.
    tail <- switch(as.character(res$feed_basis %||% "")[1],
                   proven = "the balance adds up", layout_match = "read as a design it knows",
                   person = if (identical(as.character(res$person$fix %||% "")[1], "boxes")) "read with the columns you drew"
                            else "you checked it",
                   "converted")
    div(class = paste0("verdict verdict-", lvl),
      div(class = "verdict-ico", icon),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", sprintf("Done \u2014 %s transaction%s, %s.",
          format(n, big.mark = ","), if (identical(as.integer(n), 1L)) "" else "s", tail)),
        .audit_note(res),
        if (length(chips)) div(chips)))
  })

  # cv_bank_note -- THE BANK, WHEN THE STATEMENT DOES NOT SETTLE IT. A statement
  # that names another bank than the one it was read as teaches nothing until a
  # person says which is right (spec section 5); this is where she says it. With no
  # bank at all it says so plainly: nothing is learned from a statement whose bank
  # nobody knows.
  output$cv_bank_note <- renderUI({
    res <- cv_res(); req(res)
    bk <- res$bank
    if (is.null(bk) || !(res$status %||% "") %in% c("ok", "needs_review", "unsupported")) return(NULL)
    used <- as.character(bk$bank %||% NA_character_)[1]
    seen <- as.character(bk$institution %||% NA_character_)[1]
    # No bank: asked only where the table's Bank picker is on screen to answer it.
    if (is.na(used)) {
      if (!NROW(cv_plan()$rows)) return(NULL)
      return(p(class = "bank-line",
        strong("Which bank is this? "), "Pick it in the table above so we remember this design."))
    }
    if (!isTRUE(bk$block_learning)) return(NULL)
    used_lab <- as.character(bk$display %||% .bank_label(used) %||% used)[1]
    seen_lab <- as.character(bk$identified_display %||% .bank_label(seen) %||% seen)[1]
    div(class = "bank-ask",
      p(strong("Which bank? "), .bank_question(bk$why, used_lab, seen_lab)),
      p(class = "muted", style = "font-size:12.5px",
        "Nothing is learned from this statement until you say. The figures are not affected."),
      div(style = "display:flex;gap:8px;flex-wrap:wrap",
        actionButton("cv_bank_keep", sprintf("It is %s", used_lab), class = "btn-default btn-sm"),
        if (!is.na(seen) && !identical(seen, used))
          actionButton("cv_bank_use", sprintf("It is %s", seen_lab), class = "btn-default btn-sm")))
  })
  # .bank_question(why, used, seen) -- the identifier's sentence, as a person reads
  # it. It reads "You picked ASB, but the statement looks like Westpac (medium
  # confidence): Westpac: it names ...": a grade nobody can act on, and the bank's
  # name twice. The evidence after it is kept word for word; the buttons under the
  # note are the "please confirm", so it is not said again.
  .bank_question <- function(why, used, seen) {
    why <- as.character(why %||% "")[1]
    rx <- "^You picked .*? but the statement looks like .*?(?: \\([a-z]+ confidence\\))?: "
    if (is.na(why) || !grepl(rx, why, perl = TRUE) || is.na(seen)) return(why)
    ev <- sub(rx, "", why, perl = TRUE)
    if (startsWith(ev, paste0(seen, ": "))) ev <- substring(ev, nchar(seen) + 3L)
    ev <- sub("\\s*(Please confirm|Nothing will be learned until you confirm)\\.$", "", ev)
    sprintf("You picked %s, but the statement looks like %s: %s", used, seen, ev)
  }
  # .set_row_bank(bank) -- the Convert table's row for the file on the page follows
  # a bank chosen on its result, so the two can never show different banks.
  .set_row_bank <- function(bank) {
    p <- isolate(cv_plan()); src <- isolate(cv_src())
    i <- if (NROW(p$rows) == 1L) 1L else src[["row"]] %||% NA_integer_
    if (is.null(p) || is.na(i) || i > NROW(p$rows)) return(invisible(NULL))
    pk <- isolate(cv_plan_picks()); length(pk) <- nrow(p$rows)
    pk[i] <- bank
    cv_plan_picks(pk)
    ran <- isolate(cv_plan_ran())
    if (!is.null(ran) && identical(ran$gen, p$gen)) {
      e <- plan_effective(p, pk)[i]; ran$expected[i] <- if (is.na(e)) "" else e; cv_plan_ran(ran)
    }
  }
  observeEvent(input$cv_bank_keep, {
    res <- cv_res(); req(res)
    bank <- as.character(res$bank$bank)[1]
    .set_row_bank(bank)
    .reread(isolate(cv_ov()), bank = bank, bank_confirmed = TRUE,
            what = sprintf("Reading it again as %s\u2026", .bank_label(bank) %||% bank))
  })
  observeEvent(input$cv_bank_use, {
    res <- cv_res(); req(res)
    bank <- as.character(res$bank$institution)[1]
    .set_row_bank(bank)
    .reread(isolate(cv_ov()), bank = bank, bank_confirmed = FALSE,
            what = sprintf("Reading it again as %s\u2026", .bank_label(bank) %||% bank))
  })

  # cv_spot -- A SPOT CHECK, WHEN THIS CONVERSION WAS PICKED FOR ONE (spec section
  # 2: built, off by default, an admin sets the rate). Only an automatic conversion
  # is picked: it is the person's eyeball on what the arithmetic already proved.
  output$cv_spot <- renderUI({
    res <- cv_res(); req(res)
    if (!isTRUE(res$spot_check) || !identical(res$status, "ok")) return(NULL)
    done <- cv_spot_done()
    if (!is.na(done))
      return(div(class = "note", style = "margin:0 0 12px",
        if (identical(done, "wrong"))
          "Thank you - recorded as wrong. Set it right on Please check below, and mark it Wrong at the bottom of the page so its figures are pulled back."
        else "Thank you - your spot check was recorded."))
    div(class = "note spot-check", style = "margin:0 0 12px",
      p(strong("Spot check. "), "This conversion was picked for a person to look at. Compare a few dates and amounts in the table below with the statement - are they right?"),
      div(style = "display:flex;gap:8px;flex-wrap:wrap",
        actionButton("cv_spot_right", "They're right", class = "btn-default btn-sm"),
        actionButton("cv_spot_wrong", "Something is wrong", class = "btn-danger btn-sm"),
        actionButton("cv_spot_cant", "I can't tell", class = "btn-default btn-sm")))
  })
  .spot <- function(v) {
    res <- cv_res(); req(res, isTRUE(res$spot_check))
    ok <- safe(spot_check_record(res, v, TRACKING_DIR), FALSE)
    if (!isTRUE(ok)) notify_once("cv_spot", "The spot check could not be recorded - tell whoever looks after the tool.",
                                 type = "error", duration = 8)
    cv_spot_done(v)
    if (identical(v, "wrong")) cv_ck_open(TRUE)
  }
  observeEvent(input$cv_spot_right, .spot("right"))
  observeEvent(input$cv_spot_wrong, .spot("wrong"))
  observeEvent(input$cv_spot_cant, .spot("cant_tell"))

  # ---- PLEASE CHECK ----------------------------------------------------------------
  #
  # Spec section 7: the page with the columns found drawn on it and a balance tick
  # per page; what each column of figures is, as a dropdown; Re-read, which reads
  # the statement again with those roles and says at once whether the arithmetic
  # now proves it (a fix that proves is learned for the bank); and "This is right"
  # for a reading the arithmetic could not prove but a person can vouch for --
  # never for one it CONTRADICTS (R/convert.R refuses those, and the refusal is
  # said here). Drawing the columns by hand is reachable from here only, last.
  cv_ck_open <- reactiveVal(FALSE)
  observeEvent(input$cv_ck_toggle, cv_ck_open(!isTRUE(cv_ck_open())))
  .ck_needed <- function(res) (res$status %||% "") %in% c("needs_review", "unsupported")
  .ck_is_pdf <- function(res) (res$stamp$kind %||% "") %in% c("pdf", "scan")
  # Which statement of a bundle, and which page of it, is on screen. Reset when a
  # result opens: to the first statement that did not prove, and its first page.
  ck_stmt <- reactiveVal(1L)
  ck_page <- reactiveVal(1L)
  ck_mark <- reactiveVal(NULL)   # the clicked transaction's line (below)
  observeEvent(cv_res(), {
    res <- cv_res(); if (is.null(res)) return()
    oc <- vapply(res$reading %||% list(), function(r) as.character(r$outcome %||% "unread")[1], "")
    s <- which(!(oc %in% c("proven", "layout_match")))[1]
    if (is.na(s)) s <- 1L
    ck_stmt(as.integer(s))
    ck_page(as.integer((res$reading[[s]]$pages %||% 1L)[1]))
    ck_mark(NULL)
  }, ignoreNULL = FALSE)
  observeEvent(input$cv_ck_stmt, {
    res <- cv_res(); s <- suppressWarnings(as.integer(input$cv_ck_stmt))
    if (is.null(res) || is.na(s) || s < 1L || s > length(res$reading)) return()
    ck_stmt(s); ck_page(as.integer((res$reading[[s]]$pages %||% 1L)[1]))
  })
  observeEvent(input$cv_ck_page, {
    pg <- suppressWarnings(as.integer(input$cv_ck_page)); if (!is.na(pg)) ck_page(pg)
  })
  ck_rows <- reactive({ res <- cv_res(); if (is.null(res)) NULL else .result_rows(res) })
  # A ROW OF THE TRANSACTIONS TABLE, CLICKED (D16): Please check opens at that
  # row's page (its provenance, pdf:pN) with the line marked. The mark is the
  # row's amount found among the page's words; none found, the page alone.
  observeEvent(input$cv_txns_rows_selected, {
    i <- suppressWarnings(as.integer(input$cv_txns_rows_selected)[1])
    res <- cv_res(); rows <- ck_rows()
    if (is.na(i) || is.null(res) || !.ck_is_pdf(res) || is.null(rows) || i > nrow(rows)) return()
    pg <- rows$page[i]; if (is.na(pg)) return()
    ck_stmt(as.integer(rows$statement[i])); ck_page(as.integer(pg))
    ck_mark(list(page = as.integer(pg), amount = abs(rows$amount[i])))
    cv_ck_open(TRUE)
    updateRadioButtons(session, "cv_ck_page", selected = pg)
    session$sendCustomMessage("ss-scroll", "cv_check")
  })
  # .ck_mark_y(path, page, amount) -> the y (PDF points) of the amount's word, or NA
  .ck_mark_y <- function(path, page, amount) {
    if (!is.finite(amount %||% NA)) return(NA_real_)
    w <- tryCatch(suppressMessages(pdftools::pdf_data(path)[[page]]), error = function(e) NULL)
    if (!is.data.frame(w) || !nrow(w)) return(NA_real_)
    v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", w$text)))
    j <- which(!is.na(v) & abs(v - amount) < 0.005 & grepl("[.]", w$text))[1]
    if (is.na(j)) NA_real_ else w$y[j] + w$height[j] / 2
  }
  # The ticks for the statement on screen, one per page.
  ck_ticks <- reactive({
    res <- cv_res(); req(res); s <- ck_stmt()
    rd <- res$reading[[s]]; req(rd)
    rows <- ck_rows()
    rows <- if (is.null(rows)) NULL else rows[rows$statement == s, , drop = FALSE]
    .page_ticks(rows, rd$pages %||% integer(0))
  })

  output$cv_accept_bar <- renderUI({
    res <- cv_res()
    if (is.null(res) || !length(res$reading %||% list())) return(NULL)
    need <- .ck_needed(res)
    if (!(need && .new_design(res)) || isTRUE(cv_ck_open())) return(NULL)
    div(class = "accept-bar",
      div(class = "accept-bar-btns",
        actionButton("cv_ck_confirm", "It\u2019s right \u2014 accept it", class = "btn-primary"),
        actionButton("cv_ck_aside", "Set aside", class = "btn-default")),
      uiOutput("cv_ck_msg_short"),
      p(class = "muted accept-bar-alt", "Something not right? ",
        actionLink("cv_ck_toggle", "See how it was read, and change a column")))
  })
  output$cv_check <- renderUI({
    res <- cv_res()
    if (is.null(res) || !length(res$reading %||% list())) return(NULL)
    need <- .ck_needed(res)
    # A new design that adds up: nothing to ask about its columns (D16), so the
    # page and the questions stay folded behind one link; two buttons decide.
    new_only <- need && .new_design(res)
    # ...drawn UNDER the rows (cv_accept_bar), so she sees what she accepts first.
    if (new_only && !isTRUE(cv_ck_open())) return(NULL)
    if (!need && !isTRUE(cv_ck_open()))
      return(div(style = "margin:0 0 12px;font-size:13px;color:var(--muted)",
        "Want to see where the columns were found? ",
        actionLink("cv_ck_toggle", "See how it was read")))
    rd <- res$reading; k <- length(rd)
    is_pdf <- .ck_is_pdf(res)
    s <- isolate(ck_stmt())
    div(class = paste("check-panel", if (!need) "check-panel-done"),
      div(class = "check-head",
        h4(style = "margin:0", if (need) "Please check" else "How it was read"),
        if (!need || new_only) actionLink("cv_ck_toggle", "Close", class = "ck-close")),
      if (k > 1L) radioButtons("cv_ck_stmt", "This file holds several statements - which one:",
        inline = TRUE, selected = s, choiceValues = as.list(seq_len(k)),
        choiceNames = lapply(seq_len(k), function(i) {
          pg <- rd[[i]]$pages %||% integer(0)
          o <- rd[[i]]$outcome %||% "unread"
          sprintf("%d (page%s %s) %s", i, if (length(pg) == 1L) "" else "s",
                  if (length(pg)) paste(unique(range(pg)), collapse = "-") else "?",
                  if (o %in% c("proven", "layout_match")) "\u2713" else "\u2717")
        })),
      fluidRow(
        column(7,
          if (is_pdf) tagList(
            uiOutput("cv_ck_pages"),
            plotOutput("cv_ck_plot", height = "auto"),
            uiOutput("cv_ck_tick_line"))
          else uiOutput("cv_ck_table")),
        column(5, uiOutput("cv_ck_side"))))
  })

  # The page chooser IS the tick strip: each page, with whether its balance adds up.
  output$cv_ck_pages <- renderUI({
    t <- ck_ticks(); req(nrow(t) > 0L)
    radioButtons("cv_ck_page", NULL, inline = TRUE, selected = isolate(ck_page()),
      choiceValues = as.list(t$page),
      choiceNames = lapply(seq_len(nrow(t)), function(j) {
        w <- .tick_word(t[j, ])
        span(class = paste("tick", w$cls), title = paste0(.sentence(w$say), " ", if (identical(w$cls, "tick-ok")) TICK_HELP else if (identical(w$cls, "tick-bad")) CROSS_HELP else ""), sprintf("Page %d %s", t$page[j], w$glyph))
      }))
  })
  output$cv_ck_tick_line <- renderUI({
    t <- ck_ticks(); pg <- ck_page()
    j <- match(pg, t$page); req(!is.na(j))
    w <- .tick_word(t[j, ])
    p(class = paste("tick-line", w$cls),
      sprintf("Page %d: %s.%s", pg, w$say,
              if (t$derived[j] > 0L) sprintf(" %d amount%s on it %s filled in from the balance.", t$derived[j],
                                             if (t$derived[j] == 1L) "" else "s",
                                             if (t$derived[j] == 1L) "was" else "were") else ""))
  })
  ck_render <- reactive({
    src <- cv_src(); req(src, file.exists(src$path %||% ""))
    render_page_view(src$path, ck_page(), 100)
  })
  output$cv_ck_plot <- renderPlot({
    r <- ck_render(); req(r)
    res <- cv_res(); s <- ck_stmt()
    cols <- res$reading[[s]]$columns
    if (is.data.frame(cols)) cols <- cols[cols$page %in% r$pg, , drop = FALSE]
    # Each column of figures carries the number of its question beside the page,
    # and one short word: a band is too narrow for "Money going out".
    qn <- if (is.data.frame(cols)) match(cols$field, .ck_money(res, s)$field) else integer(0)
    labels <- vapply(seq_along(qn), function(j)
      if (is.na(qn[j])) plain_column(cols$field[j]) else sprintf("%d %s", qn[j], .col_short(cols$field[j])), "")
    .draw_page_columns(r, cols, labels)
    mk <- ck_mark()
    if (!is.null(mk) && identical(mk$page, as.integer(r$pg))) {
      y <- .ck_mark_y(cv_src()$path, r$pg, mk$amount)
      if (is.finite(y)) rect(0, y - 7, r$w, y + 7, col = "#f59e0b40", border = "#d97706", lwd = 2)
    }
    if (is.data.frame(res$reading[[s]]$columns) && nrow(res$reading[[s]]$columns) && !NROW(cols))
      text(r$w / 2, 30, "No columns were found on this page.", col = PALETTE$bad, font = 2)
  }, height = function() .plot_height(session$clientData$output_cv_ck_plot_width,
                                      tryCatch(ck_render(), error = function(e) NULL)))
  # A CSV or workbook has no page to draw: its columns are named by their headings.
  output$cv_ck_table <- renderUI({
    res <- cv_res(); s <- ck_stmt()
    cols <- res$reading[[s]]$columns
    if (!is.data.frame(cols) || !nrow(cols)) return(p(class = "muted", "No columns were found."))
    tagList(
      p(class = "muted", "A CSV or Excel file has no page to draw: these are its columns, by their headings, and what each was read as. The rows read are in the transactions table below."),
      tags$table(class = "split-table",
        tags$thead(tags$tr(tags$th("Heading in the file"), tags$th("Read as"))),
        tags$tbody(lapply(seq_len(nrow(cols)), function(j) tags$tr(
          tags$td(as.character(cols$heading[j] %||% "")), tags$td(plain_column(cols$field[j])))))))
  })

  # .ck_money(res, s) -> the statement's columns of figures: field and heading.
  .ck_money <- function(res, s) {
    cols <- res$reading[[s]]$columns
    if (!is.data.frame(cols) || !nrow(cols)) return(data.frame(field = character(0), heading = character(0)))
    m <- cols[cols$kind %in% "money", , drop = FALSE]
    m <- m[!duplicated(m$field), , drop = FALSE]
    data.frame(field = as.character(m$field), heading = as.character(m$heading %||% ""),
               stringsAsFactors = FALSE)
  }
  .field_role <- function(f) if (grepl("^other[0-9]*$", f)) "other" else f
  # .ck_ask(f, j, n, heading, ex, sel) -- one column's question: its number and
  # colour as drawn on the page, two of its own lines, and the plain answers.
  # Folded to one line ("Column 1 \u00b7 Withdrawals \u2192 Money going out \u2713
  # change") when the reading adds up; open where the tool is unsure.
  .ck_ask <- function(f, j, n, heading, ex, sel, open = TRUE) {
    cc <- .ck_col_colour(f, "money")
    hd <- trimws(as.character(heading %||% ""))
    rows <- if (is.data.frame(ex) && nrow(ex)) ex[ex$field == f, , drop = FALSE] else NULL
    ans <- ROLE_ASK[[sel %||% ""]][1] %||% ""
    tags$details(class = "ck-ask", open = if (isTRUE(open)) NA else NULL,
      tags$summary(class = "ck-ask-head",
        span(class = "ck-swatch", style = sprintf("background:%s", cc)),
        sprintf("Column %d", j),
        if (nzchar(hd)) span(class = "ck-ask-hd", sprintf("\u00b7 %s", substr(hd, 1, 40))),
        if (nzchar(ans)) span(class = "ck-ask-ans", sprintf("\u2192 %s", ans), if (!isTRUE(open)) span(class = "ck-ask-ok", " \u2713")),
        span(class = "ck-ask-change", "change")),
      if (!is.null(rows) && nrow(rows)) tagList(
        div(class = "ck-ask-label", "Lines from this column:"),
        tags$table(class = "ck-ask-lines", tags$tbody(lapply(seq_len(nrow(rows)), function(i) tags$tr(
          tags$td(rows$date[i]), tags$td(rows$description[i]), tags$td(class = "ck-ask-fig", rows$figure[i])))))),
      radioButtons(paste0("cv_ck_role_", f), "What is this column?", selected = sel, width = "100%",
        choiceValues = names(ROLE_ASK),
        choiceNames = unname(lapply(ROLE_ASK, function(a) tagList(tags$b(a[1]), tags$br(), span(class = "muted", a[2]))))))
  }
  # .ck_choice_ui(ch, need) -- TWO READINGS THAT BOTH ADD UP (two recipes for one
  # design that give different figures). The statement cannot say which is right,
  # so both are shown side by side in plain figures and the person picks one; the
  # pick is read again with that recipe alone and counts as a proof for it.
  .ck_money_txt <- function(x) if (is.na(x)) "-" else formatC(x, format = "f", digits = 2, big.mark = ",")
  # The message slot is drawn by whichever of the two side panels is showing (never
  # both at once), so it is made in one place: one id, one binding.
  .ck_msg_slot <- function() uiOutput("cv_ck_msg")
  .ck_choice_ui <- function(ch, need) {
    ch <- ch[seq_len(min(3L, length(ch)))]
    one <- function(o, j) div(class = "ck-choice", style = "flex:1 1 220px;border:1px solid var(--line,#ddd);border-radius:8px;padding:10px",
      tags$b(sprintf("Reading %d", j)), span(class = "muted", sprintf(" - %s", o$title)),
      tags$table(class = "ck-ask-lines", style = "margin:6px 0", tags$tbody(
        tags$tr(tags$td("Rows"), tags$td(class = "ck-ask-fig", o$rows)),
        tags$tr(tags$td("Money in"), tags$td(class = "ck-ask-fig", .ck_money_txt(o$money_in))),
        tags$tr(tags$td("Money out"), tags$td(class = "ck-ask-fig", .ck_money_txt(o$money_out))),
        tags$tr(tags$td("Closing balance"), tags$td(class = "ck-ask-fig", .ck_money_txt(o$closing))))),
      if (is.data.frame(o$sample) && nrow(o$sample)) tags$table(class = "ck-ask-lines", tags$tbody(lapply(seq_len(nrow(o$sample)), function(i)
        tags$tr(tags$td(o$sample$date[i]), tags$td(o$sample$description[i]), tags$td(class = "ck-ask-fig", .ck_money_txt(o$sample$amount[i])))))),
      if (need) actionButton(paste0("cv_ck_pick_", j), "This one is right", class = "btn-primary", style = "margin-top:6px"))
    tagList(
      h5(style = "margin-top:0", "Two readings both add up - which one is right?"),
      p(class = "muted", style = "font-size:12.5px;margin:-4px 0 8px",
        "The statement can be read two ways and its figures add up both times, so the tool cannot tell which is right. Compare them with the page and pick one."),
      div(style = "display:flex;gap:10px;flex-wrap:wrap", lapply(seq_along(ch), function(j) one(ch[[j]], j))),
      if (need) div(style = "margin-top:8px", actionButton("cv_ck_aside", "Neither - set aside", class = "btn-default")),
      .ck_msg_slot())
  }
  lapply(1:3, function(j) observeEvent(input[[paste0("cv_ck_pick_", j)]], {
    res <- cv_res(); req(res); s <- ck_stmt()
    ch <- res$reading[[s]]$recipe_choice %||% list()
    req(length(ch) >= j)
    ov <- list(recipe = ch[[j]]$ref)
    if (length(res$reading) > 1L) ov$statement <- s
    .reread(ov, what = "Reading it the way you picked\u2026")
  }, ignoreInit = TRUE))
  # .ck_fixed(res) -- was the result on the page read with a person's fix?
  .ck_fixed <- function(res) !is.null(cv_ov()) || !is.na(as.character(res$person$fix %||% NA_character_)[1])
  output$cv_ck_side <- renderUI({
    res <- cv_res(); req(res); s <- ck_stmt()
    rd <- res$reading[[s]]; req(rd)
    need <- .ck_needed(res)
    new_only <- need && .new_design(res)   # adds up, a design not yet taught (D16)
    money <- .ck_money(res, s)
    others <- unique(as.character(rd$columns$field[!(rd$columns$kind %in% "money")]))
    ov <- cv_ov()$roles
    ex <- rd$examples
    ck <- rd$checks
    bad <- if (is.data.frame(ck)) ck[ck$ok %in% FALSE, , drop = FALSE] else NULL
    ch <- rd$recipe_choice %||% list()
    if (length(ch) > 1L) return(.ck_choice_ui(ch, need))
    tagList(
      # ONE QUESTION PER COLUMN, ASKED WITH THE COLUMN'S OWN LINES. A dropdown
      # labelled "Money out" holding "Money out" asked nothing anyone could answer
      # with confidence. Each column of figures is numbered on the page, and here
      # it is shown by two of its own lines ("3 Feb  EFTPOS RIVERSIDE DAIRY  12.40")
      # with the question in plain words. The tool's guess is ticked already.
      h5(style = "margin-top:0", "What is each column?"),
      if (nrow(money)) p(class = "muted", style = "font-size:12.5px;margin:-4px 0 8px",
        "The columns of figures are numbered on the page. The tool's guess is ticked - change any that is wrong, then press Read it again."),
      if (!nrow(money)) p(class = "muted", "No column of figures was found on this statement.")
      else lapply(seq_len(nrow(money)), function(j) {
        f <- money$field[j]
        sel <- if (!is.null(ov) && f %in% names(ov)) as.character(ov[[f]]) else .field_role(f)
        .ck_ask(f, j, nrow(money), money$heading[j], ex, sel, open = need && !new_only)
      }),
      if (length(others)) p(class = "muted ck-also",
        sprintf("Also read: %s.", paste(plain_column(others), collapse = ", "))),
      # AT MOST THREE BUTTONS (D16)
      div(style = "display:flex;gap:8px;flex-wrap:wrap;margin:6px 0",
        if (nrow(money)) actionButton("cv_ck_reread", "Read it again", class = if (new_only) "btn-default" else "btn-primary"),
        if (need && !identical(res$status, "unsupported"))
          actionButton("cv_ck_confirm", "It\u2019s right \u2014 accept it", class = if (new_only) "btn-primary" else "btn-default"),
        if (need) actionButton("cv_ck_aside", "Set aside", class = "btn-default")),
      .ck_msg_slot(),
      uiOutput("cv_ck_recipe"),
      # THE WAY BACK. A role set wrong can leave nothing readable -- no columns, so
      # no dropdowns and no Re-read -- and the only other way out was converting
      # the whole case again. Offered on any reading a person's fix produced, so it
      # is still there after another file was opened and this one opened again.
      if (.ck_fixed(res))
        p(style = "margin:4px 0;font-size:13px",
          actionLink("cv_ck_undo", "Undo my changes"),
          span(class = "muted", " - read it again as the tool first found it.")),
      if (isTRUE((res$derived %||% 0L) > 0L))
        p(class = "chip-warn", style = "padding:6px 10px;border-radius:8px;font-size:13px;margin:8px 0",
          sprintf("%d amount%s could not be read and %s filled in from the running balance. %s shaded in the transactions table below and marked in its Flags column.",
                  res$derived, if (res$derived == 1L) "" else "s", if (res$derived == 1L) "was" else "were",
                  if (res$derived == 1L) "It is" else "They are")),
      # MORE DETAIL, CLOSED (D16): the checks that did not hold and the last resort.
      # ...SHOWN WITH THE ONE "Show more detail" LINK under the table, not a second
      # disclosure of its own.
      if ((!is.null(bad) && nrow(bad)) || .ck_is_pdf(res)) conditionalPanel("output.cv_detail_open", class = "ck-more",
        if (!is.null(bad) && nrow(bad)) tagList(
          p(style = "margin:6px 0 2px;font-size:13px", sprintf("%d check%s did not hold:", nrow(bad), if (nrow(bad) == 1L) "" else "s")),
          tags$ul(style = "margin:4px 0 0 18px;padding:0;font-size:13px",
            lapply(seq_len(nrow(bad)), function(j) tags$li(
              tags$b(plain_reading_check(bad$check[j])), sprintf(" - %s", bad$why[j]))))),
        if (.ck_is_pdf(res))
          p(style = "margin-top:10px;font-size:13px",
            "None of these fits? ", actionLink("cv_ck_editor", "Draw the columns yourself"),
            span(class = "muted", " - the last resort, for this file only."))),
      # TEACH IT FROM THE PAGE. A wording the tool did not know (the opening balance
      # printed as "Kickoff kitty") is taught here, with the statement beside it,
      # instead of on a separate Admin screen. Words apply to every statement, so
      # only a signed-in admin sees this; everyone else is never offered a control
      # they cannot use.
      if (isTRUE(admin_ok())) tags$details(style = "margin:10px 0",
        tags$summary(style = "font-weight:600;cursor:pointer", "Teach it a wording from this statement"),
        uiOutput("cv_ck_teach")))
  })
  # The wordings this statement prints in front of a figure or a date that the
  # tool does not read as anything yet (statement_wordings, R/words.R). Taken from
  # the page text in this session only; nothing is kept.
  ck_wordings <- reactive({
    res <- cv_res(); src <- cv_src(); s <- ck_stmt()
    req(res, src, file.exists(src$path %||% ""), .ck_is_pdf(res))
    txt <- safe(pdftools::pdf_text(src$path), character(0))
    pg <- as.integer(res$reading[[s]]$pages %||% seq_along(txt))
    pg <- pg[pg >= 1L & pg <= length(txt)]
    safe(statement_wordings(txt[pg], safe(load_label_dict(DICT_PATH), list()), LEXICON_PATH), character(0))
  })
  output$cv_ck_teach <- renderUI({
    req(admin_ok()); dict_bump()
    cands <- tryCatch(ck_wordings(), error = function(e) character(0))
    tagList(
      selectizeInput("cv_ck_teach_word", "The wording, as the statement prints it",
        choices = c("", cands), width = "100%",
        options = list(create = TRUE, placeholder = if (length(cands))
          "Pick one from this statement, or type it" else "Type it")),
      selectInput("cv_ck_teach_kind", "What it means", word_meaning_choices(blank = TRUE), width = "100%"),
      actionButton("cv_ck_teach_go", "Teach it and read again", class = "btn-default"),
      uiOutput("cv_ck_teach_msg"),
      p(class = "muted", style = "font-size:12px;margin-top:6px",
        "It applies to every statement from now on. A wording that would clash with another meaning is refused."))
  })
  observeEvent(input$cv_ck_teach_go, {
    req(admin_ok(), cv_res())
    w <- trimws(input$cv_ck_teach_word %||% "")
    out <- teach_wording(input$cv_ck_teach_kind %||% "", w, dict_path = DICT_PATH, lex_path = LEXICON_PATH)
    output$cv_ck_teach_msg <- renderUI(span(class = if (isTRUE(out)) "ok" else "bad", attr(out, "reason")))
    if (!isTRUE(out)) return()
    .words_taught()
    # read the statement again at once, with the same column roles, so the person
    # sees straight away what the new wording changed
    if (isTRUE(attr(out, "added")))
      .reread(cv_ov(), what = "Reading it again with the new wording\u2026",
              said = paste(attr(out, "reason"), "This statement has been read again with it."))
  })
  # The line under the buttons: the Undo bar while It's right / Set aside can still
  # be taken back, otherwise what the last action found.
  .ck_msg_ui <- function() {
    res <- cv_res(); pd <- ck_pending()
    if (!is.null(pd) && !is.null(res) && identical(pd$run_id, res$run_id))
      return(div(class = "undo-bar", role = "status",
        tags$b(if (identical(pd$kind, "confirm")) "Accepting it as right" else "Setting it aside"),
        span(class = "undo-count", `data-secs` = UNDO_SECONDS, sprintf("in %d s", UNDO_SECONDS)),
        actionButton("cv_ck_undo_pending", "Undo", class = "btn-default btn-sm"),
        if (identical(pd$kind, "aside"))
          tags$input(id = "cv_ck_aside_note", type = "text", class = "form-control undo-note", maxlength = "200",
                     placeholder = "A note for the admin (optional)", `aria-label` = "A note for the admin (optional)")))
    n <- cv_ck_note()
    if (is.null(n) || is.null(res) || !identical(n$run_id, res$run_id) || !length(n$text) || !nzchar(n$text[1])) return(NULL)
    div(class = if (isTRUE(n$ok)) "note" else "note-bad", style = "margin:6px 0", n$text)
  }
  output$cv_ck_msg_short <- renderUI(.ck_msg_ui())
  output$cv_ck_msg <- renderUI(.ck_msg_ui())

  # .ck_roles_overrides() -- the roles the dropdowns say, as R/convert.R takes them,
  # for the statement on screen. NULL when the statement has no column of figures.
  .ck_roles_overrides <- function() {
    res <- cv_res(); s <- ck_stmt()
    money <- .ck_money(res, s)
    if (!nrow(money)) return(NULL)
    roles <- vapply(money$field, function(f) as.character(input[[paste0("cv_ck_role_", f)]] %||% .field_role(f))[1], "")
    ov <- list(roles = stats::setNames(roles, money$field))
    if (length(res$reading) > 1L) ov$statement <- s
    ov
  }
  # .reread_words(res, confirm) -- what a re-read found, in one sentence or two:
  # whether the arithmetic now proves it, and what (if anything) was learned.
  .reread_words <- function(res, confirm) {
    m <- plain_messages(res$messages)
    learn <- unlist(lapply(res$learn %||% list(), function(l)
      if ((l$action %||% "none") %in% c("corrected", "created", "evidence_added", "promoted")) l$why))
    held <- length(res$fix_held %||% character(0)) > 0L
    if (isTRUE(confirm)) {
      if (identical(res$status, "ok"))
        return(paste("Confirmed. This file is converted as read and its download is ready.",
                     if (held) "It is held for an admin before anything is learned from it." else ""))
      return(.sentence(m[1] %||% "It could not be confirmed."))
    }
    fix_err <- grep("^The fix was not applied", m, value = TRUE)
    if (length(fix_err)) return(fix_err[1])
    if (identical(res$status, "ok")) {
      o <- plain_outcome("ok", res$outcome, res$feed_basis, res$reason, res$person$fix)
      return(paste0(o$word, " - the statement's own arithmetic now adds up.",
                    if (length(learn)) paste0(" ", learn[1]) else "",
                    if (identical(res$person$fix, "boxes")) " The columns you drew apply to this file only." else ""))
    }
    # A fix that leaves NOTHING readable says so, and where the way back is: "Still
    # not proven: The table reader could not read the rows" read as the tool's fault.
    if (!any(vapply(res$reading %||% list(), function(rd) NROW(rd$transactions) > 0L, logical(1))))
      return(sprintf("Nothing could be read this way (%s). Undo your changes, or set the columns another way.",
                     sub("[.]$", "", .sentence(res$reason %||% m[1] %||% "no rows were found"))))
    # Adds up, only new (always ask once): the page already asks; nothing to add.
    if (exists(".new_design", mode = "function") && isTRUE(.new_design(res)) && identical(as.character(res$status %||% "")[1], "needs_review")) return(NULL)
    paste0("Still not proven: ", .sentence(res$reason %||% m[1] %||% ""),
           if (held) " Your roles apply to this file only; they are held for an admin." else "")
  }

  # .reread(overrides, confirm, bank, bank_confirmed, what, said) -- read the file on
  # the page again, in its own process like every conversion, and put the answer in
  # its place: on the result page and, for a case, in its row. `said` replaces the
  # usual line under the buttons. Its outputs are written
  # over the old ones in the same folder, so Download hands over the new reading.
  .reread <- function(overrides = NULL, confirm = FALSE, bank = NULL, bank_confirmed = NULL,
                      what = "Re-reading\u2026", said = NULL, then = NULL) {
    if (.case_converting()) return(invisible(NULL))
    res0 <- isolate(cv_res()); src <- isolate(cv_src())
    if (is.null(res0) || is.null(src) || !file.exists(src$path %||% "") || is.null(isolate(cv_dir()))) {
      notify_once("cv_reread", "This file is no longer here - convert it again.", type = "warning", duration = 8)
      return(invisible(NULL))
    }
    # [[ ]], never $: with no `bank` in the list, $ would PARTIALLY match
    # `bank_confirmed` and read its FALSE as a bank (measured: a layout filed under a
    # bank called "FALSE").
    bk <- bank %||% src[["bank"]]
    bc <- if (is.null(bank_confirmed)) isTRUE(src[["bank_confirmed"]]) else isTRUE(bank_confirmed)
    gen <- plan_env$gen
    cv_slot$start("convert", src$path, isolate(cv_dir()), message = what,
      args = convert_args(bank = bk, bank_confirmed = bc, overrides = overrides, confirm = confirm),
      finish = function(res) {
        if (is.null(res$run_id)) {      # the job itself did not come back
          notify_once("cv_reread", paste(res$messages %||% CONVERT_STOPPED, collapse = " "),
                      type = "error", duration = 10)
          return(invisible(NULL))
        }
        stamp_identity(res$run_id)
        layouts_bump(isolate(layouts_bump()) + 1L)     # a fix that proves is learned
        uid <- isolate(cv_upload_id())
        if (!is.na(uid))
          safe(set_upload_status(uid, res$status %||% "failed", run_id = res$run_id,
                                 template = .res_layout_ref(res), trust = res$trust$level %||% NA_character_,
                                 dir = UPLOADS_DIR))
        rec <- isolate(cv_recorded())
        src2 <- src; src2["bank"] <- list(bk); src2[["bank_confirmed"]] <- bc
        show_result(res, src2, uid, recorded = rec)
        # Re-publish. A re-read changes the figures the workbook and CSV hold, and
        # the feed is keyed by the statement's content hash, so this OVERWRITES that
        # statement's published rows rather than adding a second copy.
        publish_result(res, cv_recorded())
        gate <- isolate(cv_feed_gate())
        b <- isolate(cv_batch()); i <- src[["row"]] %||% NA_integer_
        if (!is.null(b) && !is.na(i) && i <= nrow(b) && identical(plan_env$gen, gen)) {
          b$status[i] <- as.character(res$status %||% "failed")[1]
          b$outcome[i] <- as.character(res$run_log$outcome %||% NA_character_)[1]
          b$bank[i] <- as.character(res$run_log$institution %||% NA_character_)[1]
          b$chosen[i] <- as.character(bk %||% NA_character_)[1]
          b$layout[i] <- as.character(res$run_log$layout %||% NA_character_)[1]
          b$rows[i] <- .rows_of(res)
          b$trust[i] <- as.character(res$trust$level %||% NA_character_)[1]
          b$failing_check[i] <- .failing_check(res)
          b$message[i] <- paste(as.character(res$messages %||% character(0)), collapse = " | ")
          b$feed_gate[i] <- list(gate)
          if (!is.null(res$feed_rows)) { res$feed_rows <- NULL; res$dropped_feed_rows <- TRUE }
          b$result[i] <- list(res)
          cv_batch(b); cv_batch_row(as.integer(i))
        }
        cv_ov(overrides)
        cv_ck_note(list(run_id = res$run_id, ok = identical(res$status, "ok"),
                        text = said %||% .reread_words(res, confirm)))
        cv_ck_open(TRUE)
        if (is.function(then)) then()
      })
  }
  observeEvent(input$cv_ck_reread, {
    req(cv_res())
    ov <- .ck_roles_overrides()
    if (is.null(ov)) { notify_once("cv_reread", "This reading found no column of figures to set.", duration = 6); return() }
    .reread(ov, what = "Re-reading with these columns\u2026")
  })
  observeEvent(input$cv_ck_undo, {
    req(.ck_fixed(cv_res()))
    .reread(NULL, what = "Reading it as it was first found\u2026",
            said = "Your changes are undone: it is read as the tool first found it.")
  })
  observeEvent(input$cv_ck_confirm, {
    res <- cv_res(); req(res)
    # THE READING ON SCREEN is what is confirmed. A role changed in a dropdown and
    # not yet re-read is not on screen, so it cannot be vouched for.
    ov <- .ck_roles_overrides(); now <- cv_ov()$roles
    shown <- if (is.null(now)) vapply(names(ov$roles), .field_role, "") else unlist(now)[names(ov$roles)]
    if (!is.null(ov) && !identical(unname(as.character(ov$roles)), unname(as.character(shown)))) {
      notify_once("cv_reread", "You changed what a column is - press Read it again first, then confirm what it reads.",
                  duration = 8)
      return()
    }
    .ck_hold("confirm", res, ov = cv_ov())
  })
  # TEN SECONDS TO TAKE IT BACK. It's right and Set aside wait UNDO_SECONDS before
  # anything is done, with an Undo button; nothing is recorded, learned or marked
  # until then. Opening another file (or Next file to check) does it at once and
  # then goes on. ck_pending is WHAT is waiting (drawn once); ck_deadline is WHEN
  # (typing a note pushes it back without redrawing the note box).
  ck_pending  <- reactiveVal(NULL)
  ck_deadline <- reactiveVal(NULL)
  .ck_hold <- function(kind, res, ov = NULL) {
    ck_pending(list(kind = kind, run_id = res$run_id, ov = ov))
    ck_deadline(Sys.time() + UNDO_SECONDS)
  }
  .ck_aside_now <- function(res, note = "") {
    note <- substr(gsub("[0-9][0-9 -]{4,}[0-9]", "[number]", trimws(as.character(note %||% "")[1])), 1, 200)
    if (is.na(note)) note <- ""
    id <- cv_upload_id()
    ok <- !is.na(id) && isTRUE(safe(set_upload_status(id, "set_aside", run_id = res$run_id,
      detail = paste0("Set aside on Please check for an admin to look at",
                      if (nzchar(note)) paste0(". Note: ", note) else ""))))
    cv_ck_note(list(run_id = res$run_id, ok = ok,
      text = if (ok) "Set aside. It is not converted; an admin will see it under Needs attention."
             else "Set aside for now. (It could not be marked for an admin, so tell one.)"))
  }
  # .ck_commit(then) -- do what is waiting now; `then` runs once it is done.
  .ck_commit <- function(then = NULL) {
    pd <- isolate(ck_pending()); ck_pending(NULL); ck_deadline(NULL)
    res <- isolate(cv_res())
    if (is.null(pd) || is.null(res) || !identical(pd$run_id, res$run_id)) {
      if (is.function(then)) then()
      return(invisible(NULL))
    }
    if (identical(pd$kind, "confirm")) .reread(pd$ov, confirm = TRUE, what = "Confirming\u2026", then = then)
    else { .ck_aside_now(res, isolate(input$cv_ck_aside_note)); if (is.function(then)) then() }
  }
  observe({
    at <- ck_deadline(); if (is.null(at)) return()
    left <- as.numeric(difftime(at, Sys.time(), units = "secs"))
    if (left > 0.05) invalidateLater(ceiling(left * 1000)) else isolate(.ck_commit())
  })
  observeEvent(input$cv_ck_undo_pending, {
    pd <- ck_pending(); req(pd)
    ck_pending(NULL); ck_deadline(NULL)
    cv_ck_note(list(run_id = pd$run_id, ok = TRUE, text = "Undone - nothing was changed."))
  })
  observeEvent(input$cv_ck_note_typing, {
    if (!is.null(ck_pending())) ck_deadline(Sys.time() + UNDO_SECONDS)
  })
  # SET ASIDE (D16): not converted now; the upload is marked for an admin, who sees
  # it under Needs attention with its page.
  # SAVE AS A RECIPE (D15): a person's answers that made the statement add up can
  # become a draft recipe for its design, named in their words. A draft is never
  # trusted alone: statements like it come back filled in until it is proven.
  ck_rc_done <- reactiveVal(NULL)
  .ck_recipe_offer <- function() {
    res <- cv_res(); src <- cv_src()
    if (is.null(res) || is.null(src) || !identical(res$status, "ok") || is.null(cv_ov())) return(NULL)
    if (!identical(res$stamp$kind %||% "", "pdf") || length(res$reading %||% list()) != 1L) return(NULL)
    rd <- res$reading[[1]]
    if (!is.null(rd$matched_recipe) || !is.null(rd$learned_recipe)) return(NULL)
    bank <- as.character(src[["bank"]] %||% NA_character_)[1]
    if (is.na(bank) || !nzchar(bank)) return(NULL)
    list(bank = bank, label = .bank_label(bank) %||% bank, run = res$run_id)
  }
  output$cv_ck_recipe <- renderUI({
    o <- .ck_recipe_offer(); if (is.null(o)) return(NULL)
    d <- ck_rc_done()
    if (!is.null(d) && identical(d$run, o$run))
      return(div(class = if (isTRUE(d$ok)) "note" else "note-bad", style = "margin:6px 0", d$text))
    div(class = "ck-recipe",
      textInput("cv_ck_rc_name", sprintf("Which %s statement is it?", o$label), "", placeholder = "e.g. Everyday account", width = "100%"),
      actionButton("cv_ck_rc_save", sprintf("Save as a recipe for %s statements like this?", o$label), class = "btn-default"))
  })
  observeEvent(input$cv_ck_rc_save, {
    o <- .ck_recipe_offer(); req(o)
    nm <- trimws(input$cv_ck_rc_name %||% "")
    if (grepl("[0-9][0-9 -]{3,}[0-9]", nm)) {
      ck_rc_done(list(run = o$run, ok = FALSE, text = "A recipe's name must not hold a long number - it could be an account.")); return() }
    inp <- safe(read_input(cv_src()$path), NULL)
    r <- if (is.null(inp)) list(ok = FALSE, why = "The statement could not be opened again.")
         else safe(recipe_from_statement(inp, list(bank = o$bank, roles = cv_ov()$roles, title = nm), RC_DIRS),
                   list(ok = FALSE, why = "It could not be saved as a recipe."))
    ck_rc_done(list(run = o$run, ok = isTRUE(r$ok),
                    text = if (isTRUE(r$ok)) "Saved as a draft recipe. An admin accepts it under Needs attention; until then statements like it come back filled in." else r$why))
    if (isTRUE(r$ok)) .rc_changed()
  })
  observeEvent(input$cv_ck_aside, {
    res <- cv_res(); req(res)
    .ck_hold("aside", res)
  })

  # ---- the last resort: drawing the columns by hand -------------------------------
  #
  # The drag-the-boxes editor, kept for the statement no setting of the roles reads
  # right (spec section 2). It starts from the columns the reader found, page by
  # page; a box is a column's left and right edges in the page's own points, the
  # same frame the reader's columns are in, so what is drawn is what is read. Saving
  # sends the boxes as a fix (overrides$columns) and the statement is read again:
  # it still has to prove itself, and boxes are never learned -- a layout does not
  # remember positions.
  #
  # THE BRUSH REPORTS ON RELEASE. Shiny debounces a brush while the mouse moves, so a
  # short delay fires mid-drag the moment somebody pauses, and the rectangle is
  # wiped with the mouse still down. A delay longer than any drag means the ONE
  # brush that arrives is the finished box.
  .ED_FIELDS <- c("Date" = "date", "Description" = "description", "Money going out" = "debit",
                  "Money coming in" = "credit", "Both, in one column (+ in, - out)" = "amount", "Balance" = "balance",
                  "Particulars" = "particulars", "Code" = "code", "Reference" = "reference",
                  "Other party" = "other_party", "Type" = "type", "Second date" = "date2")
  ed <- reactiveVal(NULL)   # list(stmt, pages, boxes = data.frame(page, field, x_min, x_max))
  observeEvent(input$cv_ck_editor, {
    res <- cv_res(); src <- cv_src(); req(res, src)
    s <- ck_stmt(); rd <- res$reading[[s]]
    cols <- rd$columns
    boxes <- if (is.data.frame(cols) && nrow(cols))
      data.frame(page = as.integer(cols$page), field = as.character(cols$field),
                 x_min = as.numeric(cols$x_min), x_max = as.numeric(cols$x_max), stringsAsFactors = FALSE)
      else data.frame(page = integer(0), field = character(0), x_min = numeric(0), x_max = numeric(0))
    pages <- as.integer(rd$pages %||% 1L)
    ed(list(stmt = s, pages = pages, boxes = boxes))
    extra <- setdiff(unique(boxes$field), .ED_FIELDS)
    showModal(modalDialog(
      title = "Draw the columns yourself", size = "l", easyClose = FALSE, class = "ed-modal",
      p(class = "muted", "Drag across a column on the page - only its left and right edges matter - say what it is, and Set it. Every page starts with the columns the tool found. The columns you draw apply to this file only."),
      fluidRow(
        column(3, selectInput("ed_page", "Page", choices = pages, selected = isolate(ck_page()))),
        column(5, selectInput("ed_field", "What is in the box you drew?",
                              choices = c(.ED_FIELDS, stats::setNames(extra, plain_column(extra))))),
        column(4, div(style = "margin-top:25px", checkboxInput("ed_copy", "Use this page's columns on every page", FALSE)))),
      uiOutput("ed_msg"),
      plotOutput("ed_plot", height = "auto",
                 brush = brushOpts("ed_brush", direction = "x", delay = 1500,
                                   delayType = "debounce", resetOnNew = TRUE)),
      # the actions stay in sight in the footer while the page scrolls inside the box
      footer = div(class = "ed-footer",
        div(class = "ed-footer-left",
          actionButton("ed_set", "Set it", class = "btn-default"),
          actionButton("ed_remove", "Remove it", class = "btn-default")),
        div(class = "ed-footer-right", modalButton("Cancel"),
          actionButton("ed_save", "Re-read with these columns", class = "btn-primary")))))
  })
  ed_page_now <- reactive({ e <- ed(); req(e)
    pg <- suppressWarnings(as.integer(input$ed_page)); if (is.na(pg) || !(pg %in% e$pages)) e$pages[1] else pg })
  ed_render <- reactive({ src <- cv_src(); req(src); render_page_view(src$path, ed_page_now(), 100) })
  output$ed_plot <- renderPlot({
    r <- ed_render(); req(r); e <- ed(); req(e)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i", xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    b <- e$boxes[e$boxes$page %in% r$pg, , drop = FALSE]
    for (j in seq_len(nrow(b))) {
      kind <- if (b$field[j] %in% c("date", "date2")) "date"
              else if (b$field[j] %in% c("debit", "credit", "amount", "balance") || grepl("^other", b$field[j])) "money"
              else "text"
      cc <- .ck_col_colour(b$field[j], kind)
      rect(b$x_min[j], 0, b$x_max[j], r$h, border = cc, lwd = 2, col = paste0(cc, "14"))
      .col_label((b$x_min[j] + b$x_max[j]) / 2, plain_column(b$field[j]), cc)
    }
  }, height = function() {
    w <- session$clientData$output_ed_plot_width %||% 800
    r <- tryCatch(ed_render(), error = function(e) NULL)
    if (is.null(r) || !is.finite(r$w) || r$w <= 0) 700 else max(300, round(w * r$h / r$w))
  })
  .ed_note <- function(msg, ok = TRUE) output$ed_msg <- renderUI(div(class = if (ok) "ok" else "bad", msg))
  observeEvent(input$ed_set, {
    e <- ed(); req(e); br <- input$ed_brush
    if (is.null(br) || !is.finite(br$xmin) || !is.finite(br$xmax)) {
      .ed_note("Drag across the column on the page first.", FALSE); return() }
    f <- as.character(input$ed_field %||% "")[1]; pg <- ed_page_now()
    b <- e$boxes[!(e$boxes$page == pg & e$boxes$field == f), , drop = FALSE]
    b <- rbind(b, data.frame(page = pg, field = f, x_min = round(br$xmin, 1), x_max = round(br$xmax, 1),
                             stringsAsFactors = FALSE))
    e$boxes <- b[order(b$page, b$x_min), , drop = FALSE]; ed(e)
    .ed_note(sprintf("%s set on page %d.", plain_column(f), pg))
  })
  observeEvent(input$ed_remove, {
    e <- ed(); req(e); f <- as.character(input$ed_field %||% "")[1]; pg <- ed_page_now()
    hit <- e$boxes$page == pg & e$boxes$field == f
    if (!any(hit)) { .ed_note(sprintf("There is no %s column on page %d to remove.", plain_column(f), pg), FALSE); return() }
    e$boxes <- e$boxes[!hit, , drop = FALSE]; ed(e)
    .ed_note(sprintf("%s removed from page %d.", plain_column(f), pg))
  })
  observeEvent(input$ed_copy, {
    req(isTRUE(input$ed_copy))
    e <- ed(); req(e); pg <- ed_page_now()
    here <- e$boxes[e$boxes$page == pg, , drop = FALSE]
    if (!nrow(here)) { .ed_note("This page has no columns to copy.", FALSE); return() }
    e$boxes <- do.call(rbind, lapply(e$pages, function(p) transform(here, page = p))); ed(e)
    .ed_note(sprintf("Page %d's columns are now on all %d pages.", pg, length(e$pages)))
  })
  observeEvent(input$ed_save, {
    e <- ed(); req(e); res <- cv_res(); req(res)
    b <- e$boxes
    # Said here, before a conversion is spent on it: the reader needs a date and at
    # least one column of money to read a row at all (R/convert.R refuses the rest).
    if (!("date" %in% b$field) || !any(c("amount", "debit", "credit") %in% b$field)) {
      .ed_note("The columns need a date, and money out, money in or an amount.", FALSE); return() }
    removeModal()
    ov <- list(columns = b)
    if (length(res$reading) > 1L) ov$statement <- e$stmt
    .reread(ov, what = "Re-reading with the columns you drew\u2026")
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
    # THE VERDICT SAID TWICE. The card's title is the outcome ("Please check") and
    # its first line is the reader's own reason; the engine's message carries the
    # same reason again with what to do, and the card's buttons ARE what to do.
    m <- sub(";\\s*(check the reading, then confirm it or set the columns' roles|check the columns on Please check, or set the file aside)$", "", m)
    # The engine's sentence names the Flags column (R/convert.R); on screen the
    # rows are also shaded, so say where to look first.
    m <- sub("each one is marked in the Flags column",
             "each one is shaded in the transactions table and marked in its Flags column", m, fixed = TRUE)
    # The audit-log gap is rendered by .audit_note() instead, in the same place on
    # both routes and with the card demoted out of green to match it. Left here it
    # was a "needs_review" sentence in the body of a card headed "Converted
    # successfully". (Verifier finding 2.)
    m <- m[!grepl(.AUDIT_GAP_RX, m)]
    m <- trimws(m)
    m[nzchar(m)]
  }
  # The one carrier for the audit-log gap, used by both verdicts.
  .audit_note <- function(res)
    if (.audit_gap(res))
      p(class = "verdict-body", style = "margin-top:2px;font-weight:600",
        .sentence(.audit_line(res))) else NULL
  # .sentence(x) -- a capital where the machine code used to be. Every engine
  # message is written to follow "needs_review: ", so once plain_messages has
  # taken the prefix off, the verdict card printed lower-case fragments
  # in the largest type on the screen, which read as log output rather
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
  # ui_labels.R words each check as its PROBLEM (CHECK_PROBLEM_PLAIN) for the
  # batch table and this card alike. Both are fixed below: the check says what is
  # wrong, and a check whose detail is BYTE-IDENTICAL to a listed
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
            tags$b(plain_diag(dg$category[i])),
            if (nzchar(dg$detail[i] %||% "")) sprintf(" - %s", dg$detail[i]) else NULL)),
          lapply(seq_len(if (is.null(f)) 0L else nrow(f)), function(i) tags$li(
            tags$b(plain_check_problem(f$name[i])),
            if (nzchar(f$detail[i] %||% "")) sprintf(" - %s", f$detail[i]) else NULL)))),
      # THE REMEDY IS THE TOOL'S OWN, AND IT SITS WITH THE DIAGNOSIS: a run whose
      # top diagnostic says "split the file" must not carry a button somewhere else
      # pointing at a different cure.
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
           title = switch(st %||% "na", pass = TICK_HELP, fail = CROSS_HELP, "This could not be checked."),
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
  # --- Analysis: the useful numbers + graphs pulled from the conversion -------
  # The displayed transactions come from the produced CSV; read them once here as
  # a data frame for the summary cards and the trend graph. No new dependency -
  # base graphics, the same ones the Please check page uses.
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
    res <- cv_res(); req(res)
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
  # When a file holds several statements, the engine reads and proves each
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
    # Result, not the 1.x trust level: a proven statement read "medium" beside a
    # Proven headline. The reader's own outcome is what a person acts on.
    word <- function(o) { o <- as.character(o %||% "unread")[1]
      unname(OUTCOME_PLAIN[if (o %in% names(OUTCOME_PLAIN)) o else "unread"]) }
    hdr <- c("Statement", "Pages", "Period", "Opening", "Closing", "Rows", "Result")
    tagList(
      h4(sprintf("The %d statements in this file", length(sts))),
      p(class = "muted", style = "margin:0 0 6px",
        # "a statement_index column" was the engine's own column name, on the
        # customer's screen; the table and the download both head it "Statement #".
        paste("Each was read and proven on its own. The cards above cover the whole file;",
              "every row you download says which statement it came from.")),
      tags$table(class = "split-table",
        tags$thead(tags$tr(lapply(hdr, tags$th))),
        tags$tbody(lapply(sts, function(s) tags$tr(
          tags$td(cell(s$index)), tags$td(cell(s$pages)),
          tags$td(sprintf("%s to %s", cell(s$period_start), cell(s$period_end))),
          tags$td(cell(s$opening_balance)), tags$td(cell(s$closing_balance)),
          tags$td(cell(s$rows)), tags$td(word(s$outcome)))))))
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
    # Grouping and the dollars/count switch belong to "Money in vs out" alone, and
    # are not on screen anywhere else - so nothing they were last set to is
    # allowed to leak into a view that cannot honour it.
    if (view != "inout") unit <- "amount"
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
    # The date axis works its own labels out from how long the statement runs -
    # months over a long period, days over a short one. It used to follow "Group
    # by", which grouped nothing on either of the two views that call this.
    xdate <- function(dates) {
      sp <- suppressWarnings(as.numeric(diff(range(dates, na.rm = TRUE))))
      axis.Date(1, x = dates, format = if (is.finite(sp) && sp > 120) "%b %Y" else "%d %b",
                col = NA, col.ticks = NA, col.axis = AXIS)
    }
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
    # (reading vs input vs escalate) is maintainer-only, never here. Category codes
    # render as plain words.
    dd <- res$diagnostics
    d <- dd[, intersect(c("where", "category", "severity", "detail", "how_to_fix"),
                        names(dd)), drop = FALSE]
    if ("category" %in% names(d)) d$category <- plain_diag(d$category)
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
  # charts answer a different one.
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
  #
  # ONE SENTENCE EACH, NOT ONE SENTENCE TWICE. The two headings shared a string
  # that named BOTH of them, so the identical line appeared under "Checks" and
  # again under "Field coverage", each time half about the other table. Same
  # cause, said once per table, about that table. (Words sweep.)
  .why_empty <- function(res, what) {
    cause <- if (isTRUE((res$status %||% "") == "failed"))
      "Nothing was read from this file" else "Nothing usable was read from this statement"
    sprintf("%s, so there is %s.", cause, what)
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
    open_it <- FALSE   # behind More detail now (D16); opened by whoever wants it
    has_kpis <- is.data.frame(k) && nrow(k) > 0L
    has_cov  <- is.data.frame(res$coverage) && nrow(res$coverage) > 0L
    has_diag <- is.data.frame(res$diagnostics) && nrow(res$diagnostics) > 0L
    said <- function(what) p(class = "muted", style = "margin:0 0 8px", .why_empty(res, what))
    tags$details(style = "margin-top:14px", open = if (open_it) NA else NULL,
      tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
                   "Checks & detail (for review)"),
      div(style = "padding:8px 2px",
        h4("Checks"), if (has_kpis) DTOutput("cv_kpis") else said("nothing to check"),
        h4("Diagnostics - where / why / how to fix"),
        if (has_diag) DTOutput("cv_diag") else p(class = "muted", style = "margin:0 0 8px", WHY_NO_DIAG),
        h4("Field coverage - what's present / empty / not read as its own column"),
        # "no field coverage", not "no fields": .why_empty frames both headings as
        # "... so there is %s." and "so there is no fields to report" is not a
        # sentence. It read correctly before the two headings were split apart to
        # each say what is true of its own table, and the split kept the plural
        # from the joined version. This is the heading's own subject, singular.
        if (has_cov) tagList(uiOutput("cv_cov_summary"), DTOutput("cv_coverage"))
        else said("no field coverage to report")))
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
      # statement. Same map the transactions table uses.
      Field   = cv_friendly_cols(cov$field),
      # 'unmapped' is a fact about the READING, not the file -- see COVERAGE_PLAIN.
      Verdict = plain_label(cov$verdict, COVERAGE_PLAIN),
      Populated = cov$populated, Empty = cov$empty,
      Note = ifelse(cov$verdict %in% names(COVERAGE_NOTE_PLAIN),
                    unname(COVERAGE_NOTE_PLAIN[cov$verdict]), cov$note),
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
    # THE QVF'S TABLE, NO MORE (D16): Date | Description | Money out | Money in |
    # Balance | Check. Everything else the statement printed is in the downloaded
    # files; the screen is for checking the rows against the paper. Check is the
    # file's own column (R/outputs.R balance_check): the balance before, plus this
    # row's money, gives this row's balance -- a tick, or a cross where it does not.
    amt <- d$.amt
    # Dates as NZ people write them ("3 Feb 2026"); a date that is not a date stays as read.
    dd <- suppressWarnings(as.Date(as.character(d$date)))
    nz_date <- ifelse(is.na(dd), as.character(d$date), trimws(format(dd, "%e %b %Y")))
    df <- data.frame(date = nz_date, description = as.character(d$description),
                     money_out = ifelse(!is.na(amt) & amt < 0, -amt, NA_real_),
                     money_in = ifelse(!is.na(amt) & amt > 0, amt, NA_real_), stringsAsFactors = FALSE)
    if (any(!is.na(d$.bal))) df$balance <- d$.bal
    if ("check" %in% names(d)) df$check <- ifelse(is.na(d$check), "", as.character(d$check))
    # A row's own warnings (its year taken from elsewhere, a figure read from a
    # faint scan) in words, as a Note -- only when some row has one. A DERIVED
    # amount, filled in from the running balance, is shaded too (spec section 2).
    df$flags <- if ("flags" %in% names(d)) ifelse(is.na(d$flags), "", as.character(d$flags)) else ""
    df$.derived <- grepl("amount_from_balance", df$flags, fixed = TRUE)
    if (any(nzchar(df$flags))) df$flags <- plain_flags(df$flags) else df$flags <- NULL
    bad <- if ("check" %in% names(df)) which(df$check == "\u2717") else integer(0)
    vis <- setdiff(names(df), ".derived")
    labs <- c(date = "Date", description = "Description", money_out = "Money out", money_in = "Money in",
              balance = "Balance", check = "Check", flags = "Note")[vis]
    # MONEY IS SHOWN TO THE CENT, ALWAYS (display only: the files keep the number).
    # The table opens at the first cross: that is where to look.
    # Paging only for a long statement (over 50 rows); a search box with an icon;
    # on a phone each row is a small card (data-label on each cell, app.css).
    many <- nrow(df) > 50L
    dt <- datatable(df, rownames = FALSE, colnames = c(unname(labs), ".derived"), selection = "single",
                    class = "display ss-txns",
                    options = list(pageLength = if (many) 50L else max(1L, nrow(df)),
                                   dom = if (many) "ftip" else "ft",
                                   language = list(search = "", searchPlaceholder = "Search rows"),
                                   createdRow = DT::JS("function(row){var api=this.api();$('td',row).each(function(i){var ix=api.column.index('fromVisible',i);$(this).attr('data-label',$(api.column(ix).header()).text());});}"),
                                   displayStart = if (length(bad) && many) 50L * ((bad[1] - 1L) %/% 50L) else 0L,
                                   columnDefs = list(list(visible = FALSE, targets = length(vis)),
                                                     list(className = "dt-center", targets = which(vis == "check") - 1L),
                                                     list(className = "dt-right num", targets = which(vis %in% c("money_out", "money_in", "balance")) - 1L))))
    if (any(df$.derived)) dt <- formatStyle(dt, ".derived", target = "row", backgroundColor = styleEqual(TRUE, "#fff3d6"))
    if ("check" %in% vis) dt <- formatStyle(dt, "check", target = "row",
                                            backgroundColor = styleEqual("\u2717", "#fde2e1"))
    money <- intersect(c("money_out", "money_in", "balance"), vis)
    if (length(money)) dt <- DT::formatCurrency(dt, columns = money, currency = "", digits = 2)
    dt
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
    labs <- c(xlsx = "Excel", csv = "CSV")
    has <- function(ext) any(grepl(paste0("\\.", ext, "$"), outputs %||% character(0)))
    Filter(Negate(is.null), lapply(names(ids), function(ext)
      if (has(ext) && !is.na(labs[ext])) downloadButton(ids[[ext]], labs[[ext]],
        class = if (ext == "xlsx") "btn-primary" else "btn-default")))
  }
  output$cv_downloads <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    # Downloads once it is done or accepted (D16): a reading that needs a person is
    # not handed out before they have looked.
    if (!identical(res$status, "ok")) return(NULL)
    btns <- dl_buttons(res$outputs, c(xlsx = "dl_xlsx", csv = "dl_csv"))
    has_json <- any(grepl("\\.json$", res$outputs %||% character(0)))
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
      div(class = "dl-hero", span(class = "dl-hero-label", "Download"), div(class = "btn-group-dl", btns),
        if (has_json)
          tags$details(class = "dl-more", tags$summary("More"),
            div(class = "dl-more-menu", downloadLink("dl_json", "JSON")))),
      if (!is.null(scan))
        p(class = "muted", style = "margin:-6px 0 12px;font-size:13px", scan))
  })
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
      rid <- safe((cv_res()$run_id %||% NA_character_)[1], NA_character_)
      if (is.na(p)) { notify_once("dl", NOTHING_TO_DL, duration = 6)
                      .dl_note(file, NOTHING_TO_DL)
                      return(.dl_log(paste0(ext, ":nothing"), run_id = rid, file = file)) }
      file.copy(p, file, overwrite = TRUE)
      .dl_log(ext, id = safe(basename(dirname(p)), NA_character_), file = file, run_id = rid)
    })
  output$dl_xlsx <- mk_dl("xlsx"); output$dl_csv <- mk_dl("csv"); output$dl_json <- mk_dl("json")

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
        # THE SAME SENTENCE A THIRD TIME, at the bottom of the page. The route
        # paragraph at the top of this result already says there is no running
        # balance and that she is the check -- on both halves of the OTHER route,
        # in the same words since cuts 8 and 9. (Words sweep, cut 29.)
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
        # list, the layout-usage table and the suggestion ranking are all built
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
    # know is that her verdict took effect and that setting the reading right is what
    # puts things right. Where it went is the server's business, and Admin's.
    div(class = "muted", style = "margin-top:4px",
      if (is.na(n))
        "Recorded - but the figures could not be pulled back. Tell whoever looks after the server."
      else if (n > 0)
        sprintf("Recorded, and the %d row(s) this produced have been pulled back so nothing downstream uses them. Set the reading right on Please check to replace them with corrected figures.", n)
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
}

shinyApp(ui, server)
