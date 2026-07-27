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
if (!is.finite(MAX_UPLOAD_MB) || MAX_UPLOAD_MB <= 0) MAX_UPLOAD_MB <- 200
options(shiny.maxRequestSize = MAX_UPLOAD_MB * 1024^2)
TEMPLATES_DIR      <- CONFIG$paths$templates       # curated, team-maintained (proven) templates
USER_TEMPLATES_DIR <- CONFIG$paths$user_templates  # templates accountants create via guided setup
LOGDIR             <- CONFIG$paths$logs            # run log + feedback log live together, next to the app
UPLOADS_DIR        <- CONFIG$paths$uploads         # every uploaded statement + its lifecycle status (local-only)
REQUESTS_DIR       <- CONFIG$paths$requests        # "none of these fits -- tell our team" raises (local-only)
FIELDS_DIR         <- CONFIG$paths$fields          # curated mode:fields (IRD/form) templates
USER_FIELDS_DIR    <- CONFIG$paths$user_fields     # form templates built in the app
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
safe(purge_uploads(UPLOADS_DIR, keep_days = UPLOADS_KEEP_DAYS))
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
            p.innerHTML='<span class=\"ss-ring\"></span><span>Working…</span>';
            document.body.appendChild(p);}return p;}
        $(document).on('shiny:busy',function(){clearTimeout(t);
          t=setTimeout(function(){pill().classList.add('on');},250);});
        $(document).on('shiny:idle',function(){clearTimeout(t);
          var p=document.getElementById('ss-busy');if(p)p.classList.remove('on');});
        // N32: a PROGRESS notification carries .progress-message; an ordinary
        // toast never does. So the centred overlay follows withProgress only, and
        // warnings stay in the corner. MutationObserver rather than CSS :has(),
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
    span(class = "app-tagline", "Statements and documents in — clean, checked data out.")),
  tabsetPanel(
    id = "main_tabs", selected = "Convert",
    # ---- About: the "what is this and why can I rely on it" page - one promise,
    # then the two doors (convert / teach) and a quiet third, then the proof story.
    # NB the app OPENS on Convert (selected, below); this is the page you come back
    # to, not the one you land on.
    tabPanel("About", br(),
      div(class = "hub",
        div(class = "hub-lead",
          "Bank statements and financial documents — PDF, CSV or Excel — into clean,",
          " checked data. Every figure comes straight off your statement; anything",
          " that can't be verified is flagged with the reason."),
        div(class = "hub-cards",
          actionLink("ab_go_convert", class = "hub-card hub-card-primary", label = div(
            div(class = "hub-card-kicker", "Most days"),
            div(class = "hub-card-title", "Convert a statement"),
            div(class = "hub-card-body",
                "Upload, click Convert. The verdict, the analysis, every transaction, the download."),
            div(class = "hub-card-go", "Open Convert →"))),
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
            div(class = "hub-card-go", "Open Add a template →"))),
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
          fileInput("cv_file", "Statement file(s) (.pdf / .csv / .tsv / .xlsx)",
                    multiple = TRUE,
                    accept = c(".pdf", ".csv", ".tsv", ".tdv", ".xlsx")),
          helpText(class = "muted", "One statement, or several for a whole case folder."),
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
          selectInput("cv_bank_quick", "Bank",
                      choices = c("Detect automatically" = ""), width = "100%"),
          actionButton("cv_go", "Convert", class = "btn-primary btn-lg btn-block"),
          helpText(sprintf("Up to %g MB.", MAX_UPLOAD_MB)),
          # Everything most people never need is one obvious click away, so the
          # default view is simply: file, name, Convert.
          tags$details(class = "adv-bank",
            tags$summary("It picked the wrong bank?"),
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
          # ...and the one question a CLEAN result still has to answer: is this
          # the right bank? A statement read end to end by the wrong template is
          # the exact failure this tool exists to prevent, and it looks perfect on
          # screen -- so the way to correct it belongs above the fold, not behind
          # the evidence toggle. (Nothing rendered this output at all, while
          # cv_teach stayed silent on a clean result BECAUSE of it: between them
          # there was no route back anywhere on the page.)
          uiOutput("cv_rematch"),
          # Form / labelled-value PDF result (renders only when kind == "form").
          uiOutput("cv_form"),
          # Before any conversion, a clear empty state rather than bare headers.
          conditionalPanel("output.cv_has_result != true && output.cv_has_batch != true",
                           uiOutput("cv_empty")),
          conditionalPanel("output.cv_has_result == true && output.cv_is_form != true",
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
            # ONE control; everything technical sits behind it.
            uiOutput("cv_more_toggle"),
            conditionalPanel("output.cv_detail_open == true",
              conditionalPanel("output.cv_has_txns == true",
                h4("See it on the page"),
                div(
                conditionalPanel("output.ix_is_pdf == true",
                  # No "green = kept, amber = skipped" line here: ix_legend says it
                  # under the picture, beside the colours it is naming.
                  fluidRow(
                    column(3, numericInput("ix_page", "Page", 1, min = 1, step = 1)),
                    column(9, br(),
                      checkboxGroupInput("ix_layers", "Show on the page",
                        choices = c("Columns" = "cols", "Kept transaction rows" = "kept",
                                    "Skipped rows" = "skipped", "Redactions" = "redact",
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
                  helpText("For CSV / Excel, the field coverage below shows which column feeds each field."))),
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
              uiOutput("cv_trend_note"))
              ),
              # "Wrong template?" -- a real question, but for someone who wants to
              # look under the bonnet, not for the person who just wanted a file.
              uiOutput("cv_candidates"),
              uiOutput("cv_detail"),
              uiOutput("cv_feed"))),   # ...and whether it reached the dashboards
          uiOutput("cv_feedback")
        )
      )
    ),
    # ---- Add a template (one toolkit for statements + a form builder) --
    tabPanel(
      "Add a template",
      br(),
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
        fileInput("ts_file", "One example of the document (.csv / .tsv / .tdv / .pdf / .xlsx)",
                  accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
        radioButtons("ts_doctype", "What kind of document is this?",
          c("A bank or card statement - a table of transactions" = "statement",
            "Something else - labelled values (an IRD form, an account summary, a letter)" = "other"),
          selected = "statement"),
        conditionalPanel("input.ts_doctype == 'statement'",
          actionButton("ts_go", "Open the toolkit", class = "btn-primary btn-lg"),
          # The guide sits AFTER the action it explains, so the thing to click is
          # the most prominent thing on the page.
          p(class = "muted", style = "margin:12px 0 0",
            actionLink("ts_help", "The guide - the ways statements differ, and what each setting means"))),
        conditionalPanel("input.ts_doctype == 'other'",
          div(style = "padding:10px 12px;background:#fffbe9;border:1px solid #f0c36d;border-radius:8px;margin-top:4px",
            strong("An 'Other' document is read differently"),
            p(style = "margin:6px 0 0;color:#555",
              "No transaction table, no running balance - so the completeness checks don't apply. You point at the values you want and check each one yourself.")))),
      # One flow: the toolkit above is THE way to add a statement template (its
      # Advanced tab covers field-by-field / YAML editing, so there is no separate
      # "build by hand" path). 'Other' documents (labelled values) are set up with
      # this builder, shown right here when "Something else" is picked above.
      # THE DOCUMENT LEADS. This page used to open with a blank box headed "the
      # values to pull out", and put the one concrete thing a person can do -
      # point at a number - last, marked optional. Somebody opening this has the
      # document in front of her and no idea what a value is called, so the order
      # is now: draw a box, say what it is, repeat, check, save. The typed list is
      # what it should always have been - a shortcut for someone who already knows
      # the field names, behind the disclosure.
      conditionalPanel("input.ts_doctype == 'other'",
      br(),
      fluidRow(
        column(7,
          strong("1. Draw a box round a value you want"),
          p(class = "muted", style = "margin:4px 0 8px",
            "A balance, a date, a total - anything."),
          conditionalPanel("output.fb_has_sample != true",
            p(class = "muted", "Upload the document at the top first.")),
          conditionalPanel("output.fb_has_sample == true",
            numericInput("fb_rf_page", "Page", 1, min = 1, step = 1),
            # Same reason as the band editor: commit on release, not on every
            # mouse-move, so a small correction is not fighting a re-read.
            plotOutput("fb_plot", height = "540px",
                       brush = brushOpts("fb_brush", direction = "xy",
                                         delay = 1500, delayType = "debounce")))),
        column(5,
          strong("2. What is this?"),
          # The tool reads the box and the wording beside it and offers both, so
          # the question it asks is one she can actually answer.
          uiOutput("fb_box_read"),
          fluidRow(
            column(7, textInput("fb_rf_field", "Call it", "", width = "100%")),
            column(5, selectInput("fb_rf_type", "Kind of value",
                                  c("money", "date", "date_range", "text")))),
          actionButton("fb_rf_set", "Add this value", class = "btn-primary"),
          tags$hr(style = "margin:14px 0 10px"),
          strong("3. Repeat for the ones that matter"),
          uiOutput("fb_values_note"),
          tableOutput("fb_regions_tbl"),
          conditionalPanel("output.fb_has_values == true",
            actionButton("fb_rf_clear", "Clear them and start again", class = "btn-default btn-sm")),
          tags$hr(style = "margin:14px 0 10px"),
          strong("4. Check what comes out"),
          div(style = "margin:6px 0", actionButton("fb_preview", "Preview on the document")),
          tags$hr(style = "margin:14px 0 10px"),
          strong("5. Name it and save"),
          fluidRow(
            column(6, textInput("fb_bank", "Bank or issuer", "NewIssuer")),
            column(6, textInput("fb_type", "Kind of document", "summary"))),
          textInput("fb_id", "Saves under this name", "newpdf_fields", width = "100%"),
          textAreaInput("fb_fp", "A phrase printed on it (one per line)",
                        rows = 2, width = "100%", value = "KiwiSaver\nOpening balance"),
          helpText(class = "muted", "Recognised by these - all must appear."),
          actionButton("fb_save", "Save template", class = "btn-primary"),
          uiOutput("fb_msg"),
          tags$details(style = "margin-top:14px",
            tags$summary(class = "muted", style = "cursor:pointer",
                         "Know the wording? Type them instead"),
            textAreaInput("fb_fields", NULL, rows = 5, width = "100%", value = ""),
            helpText(class = "muted", HTML(
              "One per line: <b>name</b> = <b>the wording on the page</b> | <b>kind</b>. Extra wordings after a semicolon. Kinds: money, date, date_range, text.<br><code>closing_balance = Closing balance; Balance at end | money</code>")),
            fileInput("fb_sample", "Draw on a different PDF instead", accept = ".pdf")))),
      h4("What will be pulled out"), uiOutput("fb_prev_status"), DTOutput("fb_prev_tbl"),
      # The template file itself is the data analyst's view, not Beth's: it is
      # kept, one click away, instead of ending her flow with a wall of YAML.
      tags$details(style = "margin-top:14px",
        tags$summary(class = "muted", style = "cursor:pointer",
                     "The template file this will save"),
        div(class = "mono", verbatimTextOutput("fb_yaml")))
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
      tabsetPanel(
        tabPanel(
          "Insights",
          br(),
          actionButton("adm_refresh", "Refresh from logs", class = "btn-primary"),
          helpText("A live picture from every conversion the team has run and every rating left."),
          h4("Uploads - new formats to pick up"),
          helpText("Statements the tool couldn't read, that nobody has set up yet. Pick one up: download its safe summary (no personal data) or open it in the toolkit."),
          fluidRow(
            column(8, DTOutput("adm_uploads")),
            column(4,
              selectInput("adm_up_pick", "Pick a saved upload", choices = NULL),
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
              selectInput("adm_req_pick", "A request", choices = NULL),
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
              selectInput("adm_inbox_pick", "A failed file", choices = NULL),
              actionButton("adm_inbox_wizard", "Open in the toolkit", class = "btn-warning"),
              br(), br(),
              uiOutput("adm_inbox_audit_ui"))),
          fluidRow(
            column(4, h5("Waiting in inbox"), DTOutput("adm_inbox_waiting")),
            column(4, h5("Processed"), DTOutput("adm_inbox_processed")),
            column(4, h5("Output folders (outbox)"), DTOutput("adm_inbox_outbox"))),
          tags$hr(),
          fluidRow(
            column(5, h4("Conversions by status"), plotOutput("adm_status_plot", height = "210px"),
                   DTOutput("adm_overview")),
            column(7, h4("Feedback flagged as wrong / minor issues"), DTOutput("adm_feedback"))),
          h4("Statements the tool can't read yet - the gaps to fill"),
          helpText("Each row is one layout no template recognises yet (identical layouts are grouped). The biggest count is the one to build a template for first - it unblocks the most statements."),
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
          h4("Templates that started failing recently"),
          helpText("A statement's layout can change slightly - a field moves or gets renamed - and stop adding up. When that happens the tool flags the conversion for a check, and any template that's suddenly getting more of those shows here. Empty is good."),
          DTOutput("adm_drift"),
          h4("Template usage"),
          DTOutput("adm_usage"),
          br(),
          actionButton("adm_rollup", sprintf("Tidy up logs (archive runs older than %d days)", LOG_KEEP_DAYS)),
          uiOutput("adm_rollup_msg"),
          br(),
          # Retention of the SAVED STATEMENTS themselves -- real client data, and
          # until now nothing ever deleted it. Runs at startup too; this is the
          # "do it now" button, and it says what it will do before you press it.
          h4("Saved statements - retention"),
          helpText(UPLOADS_NOTE),
          actionButton("adm_purge_uploads",
                       if (UPLOADS_KEEP_DAYS > 0)
                         sprintf("Delete saved statements older than %d days now", as.integer(UPLOADS_KEEP_DAYS))
                       else "Delete old saved statements now (retention is set to 'keep indefinitely')",
                       class = "btn-danger"),
          uiOutput("adm_purge_msg")
        ),
        tabPanel(
          "Templates",
          br(),
          helpText(HTML("Every layout the tool can read: <b>tested</b> = shipped and checked, <b>user</b> = built here. Click a row to view and edit it.")),
          DTOutput("adm_tpl_overview"),
          br(),
          fluidRow(
            column(5,
              selectInput("adm_tpl_pick", "Preview / edit a template", choices = NULL),
              uiOutput("adm_tpl_origin"),
              actionButton("adm_tpl_dup", "Duplicate (new id)"),
              actionButton("adm_tpl_validate", "Check it's valid"),
              actionButton("adm_tpl_save", "Save as user template", class = "btn-primary"),
              actionButton("adm_tpl_hide", "Hide / un-hide (user template)"),
              actionButton("adm_tpl_delete", "Delete (user template)", class = "btn-danger"),
              br(), br(), uiOutput("adm_tpl_msg"),
              tags$details(
                tags$summary(class = "muted", style = "cursor:pointer;font-size:12.5px", "How these actions work"),
                helpText("Duplicate copies this template with a new id into the editor - tweak it and Save. Rename by changing the id, saving, then deleting the old one. Hide parks a user template out of detection without deleting it. Delete only removes USER templates; shipped 'tested' ones are read-only and win on an id clash, so a copy needs its own id."))),
            column(7,
              h4("Template YAML"),
              textAreaInput("adm_tpl_edit", NULL, value = "", width = "100%", height = "460px"))
          ),
          tags$hr(),
          h4("Near-duplicate user templates - consolidate the pile"),
          helpText("Templates that read a statement identically (same format, amounts, dates and columns) but were drafted more than once. Keep the best one and Hide or Delete the rest - pick any id above to act on it."),
          uiOutput("adm_tpl_dupes"),
          tags$hr(),
          h4("Label dictionary - the wordings the tool looks for"),
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
          tags$details(style = "margin-top:6px",
            tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
              "Edit the whole dictionary file - for a pattern, a page rule, or a value that isn't listed yet"),
            div(style = "padding-top:10px",
              helpText(HTML(paste0(
                "Each value has an <code>any_of:</code> list of wordings; add a line under it, ",
                "indented the same as the lines already there, wording in quotes:<br>",
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
                  textAreaInput("adm_dict_edit", NULL, value = "", width = "100%", height = "360px")))))
        ),
        tabPanel(
          # NB the tab NAME is a navigation anchor: docs/, config.example.yaml and
          # dictionaries/lexicon.yaml all send the reader to "Admin -> Data capture".
          # Rename it only together with those.
          "Data capture",
          br(),
          h4("Local metadata capture - the raw material for on-box analysis"),
          helpText(HTML(paste0(
            "Every conversion can save a rich, structured record of <b>how it went</b> ",
            "(the layout it matched, how cleanly it parsed, detection scores, ",
            "reconciliation outcomes, OCR / redaction signals). It is stored on <b>this ",
            "machine only</b> under <code>logs/metadata/</code>, kept forever, and ",
            "<b>never enters the Qlik feed</b>. <b>No statement content is stored</b> - ",
            "only structure, counts and quality signals; any account number is stored ",
            "only as a one-way hash. Turn the detail up or down, or switch off ",
            "categories you don't want captured."))),
          fluidRow(
            column(5,
              radioButtons("adm_meta_level", "How much to capture",
                choices = c("Full - everything (recommended)" = "full",
                            "Standard - the essentials" = "standard",
                            "Off - capture nothing" = "off"),
                selected = CONFIG$metadata$level %||% "full"),
              checkboxGroupInput("adm_meta_cats", "Categories to capture",
                choices = c("Layout (shape / signature)" = "layout",
                            "Parse quality (rows / flags / misses / shapes)" = "parse_quality",
                            "Detection (scores / candidates)" = "detection",
                            "Reconciliation (KPIs / balances)" = "reconciliation",
                            "Multi-statement (# statements / periods / accounts)" = "multi_statement",
                            "Novelty (unmapped columns / unrecognised tokens)" = "novelty",
                            "Template hints (column profiles / suggested mapping)" = "template_hints",
                            "OCR (pages / confidence)" = "ocr",
                            "Redaction (counts / coverage)" = "redaction"),
                selected = names(Filter(isTRUE, CONFIG$metadata$capture %||% list()))),
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
                  "the number being readable."))))
          ),
          tags$hr(),
          # The recognition vocabulary, fronted by the thing an admin actually comes
          # here to do: teach it one word. The whole-file editor is kept, one click
          # away, for the rare edit a word list cannot express (a pattern, a list of
          # date formats). Everything below writes through the SAME engine function,
          # so there is one way a word gets in.
          h4("Words the tool knows to look for"),
          helpText(HTML(paste0(
            "When a statement writes something the tool has never met - <code>cow</code> for money ",
            "out, say - it cannot tell which way that money went. Teach it the word here and every ",
            "conversion from then on knows it, everywhere. Nothing is ever added automatically: ",
            "you decide."))),
          fluidRow(
            column(5, selectInput("adm_word_kind", "This word means…",
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
          h4("Words your statements used that the tool didn't recognise"),
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
                         "Edit the whole vocabulary file - for a pattern or a list of date formats"),
            div(style = "padding:8px 2px",
              helpText(HTML(paste0(
                "Beyond single words this file also holds the money / date / account <b>patterns</b> ",
                "and the date formats to try. Word lists ADD to the built-ins; a pattern REPLACES ",
                "one (and is refused if it won't work). Leave a category out to keep its default. ",
                "<b>Show built-in defaults</b> prints a complete, valid starting point to copy from."))),
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
          "Batch & audit",
          br(),
          helpText(HTML("Drop in a pile of statements and get one picture: what converts, the gap layouts <b>biggest-first</b>, and <b>ready-to-edit draft templates</b> for them. Safe to share - only shapes and counts, never contents. Tick <b>convert &amp; save</b> to also produce real outputs and feed Insights.")),
          fluidRow(
            column(4,
              fileInput("adm_ba_files", "Statements (.csv / .tsv / .pdf / .xlsx)", multiple = TRUE,
                        accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
              checkboxInput("adm_ba_convert",
                            "Also convert & save outputs (writes files, logs runs, feeds Insights)",
                            value = FALSE),
              actionButton("adm_ba_run", "Run", class = "btn-primary"),
              br(), br(),
              uiOutput("adm_ba_report_ui"),
              br(),
              uiOutput("adm_ba_csv_ui"),
              br(),
              helpText("Also available headless: Rscript scripts/bulk-audit.R <folder>")),
            column(8,
              uiOutput("adm_ba_summary"),
              h4("Gaps - layouts the tool can't read yet, biggest first"), DTOutput("adm_ba_clusters"),
              h4("Per file - shapes only, no personal data"), DTOutput("adm_ba_files_tbl"))),
          h4("Recommended draft templates (editable - copy into the Templates tab to save)"),
          uiOutput("adm_ba_recs"),
          tags$hr(),
          h4("Single statement - safe summary"),
          helpText("Upload one statement to download its shapes-only summary (no personal data) for sharing."),
          fileInput("adm_audit_one", "Statement", multiple = FALSE,
                    accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
          uiOutput("adm_audit_dl_ui")
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
  # screens with a page box (the X-ray, the template toolkit, the form builder)
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
  # removeTab, not hideTab. hideTab only sets display:none, so the whole Admin tab
  # -- its link, its four sub-tabs and every control on them -- stayed in the page
  # sent to every visitor, where "View source" reads it out. The claim above was
  # that it is not there; now it is not there.
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
    div(style = "color:#b00020;margin-top:6px", msg))
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

  # The Convert picker offers PROVEN (curated) templates by default. Ticking
  # "Include user-created templates" adds the ones users built (with a not-tested
  # warning) -- so you can always reach your own templates, but the vetted set is
  # what's shown unless you ask for more.
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
  observe({
    ts <- cv_pick_templates()
    bank <- bank_choice()
    ov <- template_overview(ts)
    if (!is.null(bank) && nrow(ov)) ov <- ov[ov$bank %in% bank, , drop = FALSE]
    # Labelled "Bank · type - id" so you can force an EXACT audited template, not
    # just a bank, when you need to be specific.
    ch <- c("(auto-detect)" = "")
    if (nrow(ov)) ch <- c(ch, stats::setNames(ov$id, sprintf("%s · %s - %s", ov$bank, ov$type, ov$id)))
    keep <- isolate(input$cv_template) %||% ""
    updateSelectInput(session, "cv_template", choices = ch,
                      selected = if (keep %in% ch) keep else "")
  })


  # ---- Admin: template overview / preview / edit ----
  # The management view shows ALL templates, hidden ones included, so a parked
  # draft can be found and un-hidden.
  output$adm_tpl_overview <- renderDT(
    template_overview(all_templates()),
    options = list(pageLength = 25, dom = "tip"), rownames = FALSE, selection = "single")

  observe(updateSelectInput(session, "adm_tpl_pick", choices = sort(names(all_templates()))))

  # clicking a row selects it in the picker
  # Server-side admin gate. The Admin tab's controls are hidden until login, but
  # a crafted client message can still fire any input, so EVERY privileged handler
  # re-verifies the admin session here and fail-closes (silent no-op) without one.
  observeEvent(input$adm_tpl_overview_rows_selected, {
    req(admin_ok())
    ov <- template_overview(all_templates())
    i <- input$adm_tpl_overview_rows_selected
    if (length(i) && i <= nrow(ov)) updateSelectInput(session, "adm_tpl_pick", selected = ov$id[i])
  })

  observeEvent(input$adm_tpl_pick, {
    req(admin_ok())
    t <- all_templates()[[input$adm_tpl_pick]]; req(t)
    updateTextAreaInput(session, "adm_tpl_edit", value = template_yaml(t))
    output$adm_tpl_msg <- renderUI(NULL)
  })

  # Show whether the selected template is a read-only shipped one or a deletable
  # user one, so the analyst knows what Delete will do.
  output$adm_tpl_origin <- renderUI({
    id <- input$adm_tpl_pick; if (is.null(id) || !nzchar(id)) return(NULL)
    is_user <- id %in% user_template_ids(USER_TEMPLATES_DIR)
    hidden <- isTRUE(all_templates()[[id]]$hidden)
    tagList(
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
    if (!(id %in% user_template_ids(USER_TEMPLATES_DIR))) {
      output$adm_tpl_msg <- .tpl_note("Only USER templates can be hidden; this one is shipped/read-only.", ok = FALSE)
      return()
    }
    now_hidden <- isTRUE(all_templates()[[id]]$hidden)
    res <- safe(set_user_template_hidden(id, !now_hidden, USER_TEMPLATES_DIR), NULL)
    if (is.null(res)) { output$adm_tpl_msg <- .tpl_note("Couldn't change it.", ok = FALSE); return() }
    tpl_bump(isolate(tpl_bump()) + 1)
    output$adm_tpl_msg <- .tpl_note(if (isTRUE(res))
      sprintf("Hid <b>%s</b> - it won't be used for detection until you un-hide it.", id)
      else sprintf("Un-hid <b>%s</b> - it's active again.", id))
  })
  # Near-duplicate user templates, grouped by identical layout, so a heap of
  # variants can be consolidated (keep one, hide/delete the rest via the controls
  # above). Uses the management set so hidden variants show up too.
  output$adm_tpl_dupes <- renderUI({
    groups <- duplicate_template_groups(all_templates())
    if (!length(groups))
      return(helpText("No duplicate user templates - nothing to consolidate."))
    ov <- template_overview(all_templates())
    do.call(tagList, lapply(seq_along(groups), function(gi) {
      ids <- groups[[gi]]
      rows <- ov[ov$id %in% ids, , drop = FALSE]
      lab <- sprintf("%s · %s", rows$bank[1] %||% "?", rows$format[1] %||% "?")
      tags$div(style = "margin:6px 0;padding:6px 10px;border-left:3px solid #c77700;background:#fff8ef",
        strong(sprintf("Same layout (%d): %s", length(ids), lab)),
        tags$ul(lapply(seq_len(nrow(rows)), function(i) tags$li(
          sprintf("%s%s", rows$id[i], if (nzchar(rows$hidden[i])) " (hidden)" else "")))))
    }))
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
    if (!(id %in% user_template_ids(USER_TEMPLATES_DIR))) {
      output$adm_tpl_msg <- .tpl_note("Only USER templates can be deleted; this one is shipped/read-only.", ok = FALSE)
      return()
    }
    files <- Filter(function(f) {
      t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
      identical(t$id %||% "", id) ||
        identical(tools::file_path_sans_ext(basename(f)), gsub("[^A-Za-z0-9_]+", "_", id))
    }, list.files(USER_TEMPLATES_DIR, pattern = "\\.ya?ml$", full.names = TRUE))
    showModal(modalDialog(
      title = "Delete this template?", size = "m", easyClose = FALSE,
      p(sprintf("This permanently deletes %d file(s) from %s:",
                length(files), USER_TEMPLATES_DIR)),
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
    if (!(id %in% user_template_ids(USER_TEMPLATES_DIR))) {
      output$adm_tpl_msg <- .tpl_note("Only USER templates can be deleted; this one is shipped/read-only.", ok = FALSE)
      return()
    }
    ok <- safe(delete_user_template(id, USER_TEMPLATES_DIR), FALSE)
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
                  sprintf("Could not delete %s - check folder permissions on %s.", id, USER_TEMPLATES_DIR),
                  type = "error", duration = 10)
    }
  })

  # Duplicate the selected template with a fresh id, into the editor to tweak+save.
  observeEvent(input$adm_tpl_dup, {
    req(admin_ok())
    t <- tryCatch(yaml::yaml.load(input$adm_tpl_edit %||% ""), error = function(e) NULL)
    if (is.null(t) || is.null(t$id)) t <- templates()[[input$adm_tpl_pick]]
    req(t)
    ids <- names(templates())
    new_id <- paste0(t$id, "_copy"); k <- 2L
    while (new_id %in% ids) { new_id <- paste0(t$id, "_copy", k); k <- k + 1L }
    t$id <- new_id; t$origin <- NULL
    updateTextAreaInput(session, "adm_tpl_edit", value = yaml::as.yaml(t))
    output$adm_tpl_msg <- .tpl_note(sprintf("Duplicated as <b>%s</b> - edit it and click Save.", new_id))
  })

  .tpl_from_editor <- function() tryCatch(yaml::yaml.load(input$adm_tpl_edit), error = function(e) NULL)
  .tpl_note <- function(html, ok = TRUE)
    renderUI(div(style = sprintf("color:%s;font-size:12px", if (ok) "#137333" else "#b00020"), HTML(html)))

  observeEvent(input$adm_tpl_validate, {
    req(admin_ok())
    t <- .tpl_from_editor()
    if (is.null(t)) { output$adm_tpl_msg <- .tpl_note("That is not valid YAML.", FALSE); return() }
    probs <- validate_template(t)
    output$adm_tpl_msg <- if (!length(probs)) .tpl_note("Valid ✓")
      else .tpl_note(paste("Problems:<br>", paste(probs, collapse = "<br>")), FALSE)
  })

  observeEvent(input$adm_tpl_save, {
    req(admin_ok())
    t <- .tpl_from_editor()
    if (is.null(t)) { output$adm_tpl_msg <- .tpl_note("That is not valid YAML.", FALSE); return() }
    path <- tryCatch(save_user_template(t, USER_TEMPLATES_DIR), error = function(e) conditionMessage(e))
    if (is.character(path) && file.exists(path)) {
      tpl_bump(tpl_bump() + 1)
      shadowed <- !is.null(safe(load_templates(TEMPLATES_DIR), list())[[t$id %||% ""]])
      msg <- sprintf("Saved to %s.", path)
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
    selectInput("adm_dict_field", "This wording means…",
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
    output$adm_dict_msg <- renderUI(span(class = "ok", sprintf(
      "Reloaded %s into the editor. Any unsaved edits in the box are gone; the file itself is unchanged.",
      DICT_PATH))) })
  observeEvent(input$adm_dict_save, {
    req(admin_ok())
    txt <- input$adm_dict_edit %||% ""
    if (!isTRUE(tryCatch({ yaml::yaml.load(txt); TRUE }, error = function(e) FALSE))) {
      output$adm_dict_msg <- renderUI(div(style = "color:#b00020", YAML_BAD_MSG))
      return()
    }
    safe(file.copy(DICT_PATH, paste0(DICT_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({ writeLines(txt, DICT_PATH); TRUE }, error = function(e) FALSE))
    output$adm_dict_msg <- renderUI(div(style = sprintf("color:%s", if (okw) "#137333" else "#b00020"),
      if (okw) "Saved (backup at labels.yaml.bak). New wordings apply to the next conversion."
      else YAML_WRITE_MSG))
  })

  # ---- Admin: local metadata-capture settings (level + category switches) ----
  observeEvent(input$adm_meta_save, {
    req(admin_ok())
    lvl <- input$adm_meta_level %||% "full"
    cats <- input$adm_meta_cats %||% character(0)
    all_cats <- c("layout", "parse_quality", "detection", "reconciliation",
                  "multi_statement", "novelty", "template_hints", "ocr", "redaction")
    capture <- stats::setNames(as.list(all_cats %in% cats), all_cats)
    okw <- save_metadata_config(lvl, capture)
    if (okw) { CONFIG$metadata$level <<- lvl; CONFIG$metadata$capture <<- capture }
    # A refusal carries its REASON (e.g. "the file is there but doesn't parse, so
    # writing this toggle would wipe every other setting in it"). Show it -- a bare
    # "check folder permissions" would send the admin hunting the wrong problem.
    why <- attr(okw, "reason")
    output$adm_meta_msg <- renderUI(div(style = sprintf("color:%s", if (okw) "#137333" else "#b00020"),
      if (okw) "Saved. Applies to the next conversion. Capture is local only and never enters the Qlik feed."
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
    output$adm_lex_msg <- renderUI(span(class = "ok", sprintf(
      "Reloaded %s into the editor. Any unsaved edits in the box are gone; the file itself is unchanged.",
      LEXICON_PATH))) })
  observeEvent(input$adm_lex_defaults, { req(admin_ok())
    updateTextAreaInput(session, "adm_lex_edit", value = safe(lexicon_defaults_yaml(), ""))
    output$adm_lex_msg <- renderUI(span(class = "ok", paste(
      "The editor now holds the BUILT-IN defaults, not your file - copy what you need.",
      "Nothing is saved until you click Save vocabulary, and saving this as it stands",
      "would replace your file with the defaults."))) })
  observeEvent(input$adm_lex_save, {
    req(admin_ok())
    txt <- input$adm_lex_edit %||% ""
    parsed <- tryCatch(yaml::yaml.load(txt), error = function(e) e)
    if (inherits(parsed, "error")) {
      output$adm_lex_msg <- renderUI(div(style = "color:#b00020", YAML_BAD_MSG)); return()
    }
    probs <- validate_lexicon(parsed)
    if (length(probs)) {
      output$adm_lex_msg <- renderUI(div(style = "color:#b00020",
        HTML(paste("Not saved -", paste(probs, collapse = "; "))))); return()
    }
    safe(file.copy(LEXICON_PATH, paste0(LEXICON_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({
      dir.create(dirname(LEXICON_PATH), recursive = TRUE, showWarnings = FALSE)
      writeLines(txt, LEXICON_PATH); TRUE }, error = function(e) FALSE))
    if (okw) clear_lexicon_cache()   # the next conversion re-reads the vocabulary
    output$adm_lex_msg <- renderUI(div(style = sprintf("color:%s", if (okw) "#137333" else "#b00020"),
      if (okw) "Saved (backup at lexicon.yaml.bak). Applies to the next conversion, everywhere."
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
    output$adm_sugg_msg <- renderUI(div(style = sprintf("color:%s", if (ok) "#137333" else "#b00020"),
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
    withProgress(message = "Auditing statements (scanned pages are OCR'd)", value = NULL,
                 adm_ba(batch_audit(paths, templates = templates())))
    # Optional heavier pass: actually convert & log each file so the runs feed
    # Insights (this is what the old separate "Batch intake" tab did).
    if (isTRUE(input$adm_ba_convert)) {
      rows <- vector("list", length(paths))
      withProgress(message = "Converting & logging", value = 0, {
        for (i in seq_along(paths)) {
          incProgress(1 / length(paths), detail = fs$name[i])
          r <- tryCatch(convert_statement(paths[i], outdir = sess, templates_dir = TEMPLATES_DIR,
            user_templates_dir = USER_TEMPLATES_DIR, logdir = LOGDIR, requested_by = "batch"),
            error = function(e) NULL)
          csv <- if (!is.null(r)) r$outputs[grepl("\\.csv$", r$outputs)] else character(0)
          nrw <- if (length(csv) && file.exists(csv[1]))
            tryCatch(nrow(utils::read.csv(csv[1], check.names = FALSE)), error = function(e) NA_integer_) else NA_integer_
          rows[[i]] <- data.frame(file = fs$name[i], status = r$status %||% "failed",
            template = r$template_id %||% NA_character_, trust = r$trust$level %||% NA_character_,
            n_rows = nrw, stringsAsFactors = FALSE)
        }
      })
      adm_ba_conv(do.call(rbind, rows))
      load_admin()   # the batch just wrote logs; refresh Insights
    }
  })
  output$adm_ba_summary <- renderUI({
    b <- adm_ba(); if (is.null(b)) return(helpText("Upload statements and click Run."))
    g <- b$feature_gaps
    conv <- adm_ba_conv()
    none <- function(x) if (length(x)) paste(names(x), collapse = ", ") else "(none seen)"
    tagList(
      p(strong(sprintf("%d statements: ", g$total)),
        paste(sprintf("%s=%s", names(g$by_status), g$by_status), collapse = ", ")),
      p(sprintf("scanned %d · with redactions %d · multi-account %d · multi-period %d · unsupported %d across %d layouts",
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
  AUDIT_ONE_WHY <- "Upload a statement above first."
  UP_AUDIT_WHY  <- "Pick a saved upload above first."
  INBOX_WHY     <- "Pick a failed file above first."

  output$adm_ba_report_ui <- renderUI({ req(admin_ok())
    dl_when("adm_ba_report", "Safe audit report (.md)", !is.null(adm_ba()), BA_REPORT_WHY) })
  output$adm_ba_csv_ui <- renderUI({ req(admin_ok())
    dl_when("adm_ba_csv", "Converted report (.csv)", !is.null(adm_ba_conv()), BA_CSV_WHY) })
  output$adm_audit_dl_ui <- renderUI({ req(admin_ok())
    dl_when("adm_audit_dl", "Download safe audit (.md)", !is.null(input$adm_audit_one), AUDIT_ONE_WHY) })
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
  output$adm_audit_dl <- downloadHandler(
    filename = function() "statement.audit.md",
    content = function(file) {
      req(admin_ok())        # admin-only export
      if (is.null(input$adm_audit_one)) { notify_once("adm_audit_dl", AUDIT_ONE_WHY, duration = 6)
                                          return(.dl_note(file, AUDIT_ONE_WHY)) }
      a <- tryCatch(format_audit(statement_audit(input$adm_audit_one$datapath, templates = templates())),
                    error = function(e) NULL)
      if (is.null(a)) return(.dl_note(file, "That file could not be read, so there is no summary to give. The file itself is the problem, not the audit."))
      writeLines(a, file) })

  # ---- Admin: uploads & pickups ----
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
                            pageLength = 8, dom = "tip"), rownames = FALSE)
  observe({
    req(admin_ok())          # upload ids identify real client statements
    cv_upload_id(); input$adm_refresh
    u <- read_uploads(UPLOADS_DIR)
    # Only offer uploads whose file is still on disk -- a purged one cannot be
    # audited or opened in the toolkit, so offering it would be a dead end.
    pick <- if (nrow(u)) u$id[u$needs_pickup & !u$purged] else character(0)
    updateSelectInput(session, "adm_up_pick", choices = pick)
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
    updateSelectInput(session, "adm_req_pick", choices = open)
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
    updateSelectInput(session, "adm_inbox_pick",
      choices = if (nrow(s$folders$failed)) s$folders$failed$file else character(0))
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

  # ---- Add a template: build a PDF-form template from labels + placed boxes --
  # (Extraction/running of form PDFs now happens on the Convert tab - one door.)
  # parse_fields_spec -- turn the friendly "name = Label; Label2 | money" lines
  # into a fields{} block. Value type after "|" is optional (default text).
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
  # The values drawn on the document: field -> list(page, x_min..y_max, value =
  # the kind, label = the wording found beside it or "", read = what was in the box).
  fb_regions <- reactiveVal(list())

  # .fb_wording(toks) -- the last few words that are WORDING, never the value.
  # Trailing tokens that are themselves a figure are dropped, or a box drawn just
  # PAST its value gets named after the very value it missed ("Opening balance
  # $875.20") - a name that could never match anything on the next document.
  .fb_wording <- function(toks) {
    while (length(toks) && grepl("^[$(]?[-+]?[0-9][0-9,.:/-]*[%)]?$", toks[length(toks)]))
      toks <- toks[-length(toks)]
    utils::tail(toks, 4)
  }
  # .fb_read_box(w, box) -- what is INSIDE a drawn box, and the wording printed
  # beside it: the words on the same line to its left, else the line directly
  # above. This is how "what is this?" stops being a question only somebody who
  # has already done the task can answer.
  .fb_read_box <- function(w, box) {
    out <- list(value = "", label = "")
    if (is.null(w) || !nrow(w)) return(out)
    w <- as.data.frame(w, stringsAsFactors = FALSE)
    cx <- w$x + w$width / 2; cy <- w$y + w$height / 2
    ord <- function(d) d[order(round(d$y / 3), d$x), , drop = FALSE]
    inbox <- cx >= box$x_min & cx <= box$x_max & cy >= box$y_min & cy <= box$y_max
    if (any(inbox)) out$value <- paste(ord(w[inbox, , drop = FALSE])$text, collapse = " ")
    beside <- cy >= box$y_min & cy <= box$y_max & (w$x + w$width) <= box$x_min
    lab <- if (any(beside)) ord(w[beside, , drop = FALSE])$text else {
      # nothing beside it -- a column heading directly above is the other place a
      # form prints the wording for a value.
      above <- cy < box$y_min & cy >= (box$y_min - 26) & cx >= box$x_min & cx <= box$x_max
      if (any(above)) ord(w[above, , drop = FALSE])$text else character(0)
    }
    out$label <- sub("[[:punct:][:space:]]+$", "", trimws(paste(.fb_wording(lab), collapse = " ")))
    out
  }
  # .fb_kind(v) -- money, date or text, decided by reading the value rather than
  # asking. The same two matchers the extractor itself uses.
  .fb_kind <- function(v) {
    v <- trimws(as.character(v %||% ""))
    if (!nzchar(v)) return("text")
    if (!is.na(.value_from_line(v, "money"))) return("money")
    if (!is.na(.value_from_line(v, "date")))  return("date")
    "text"
  }
  # .fb_same(a, b) -- the same printed value, ignoring punctuation and case. Used
  # to decide whether reading by WORDING lands on the value she actually boxed.
  .fb_same <- function(a, b) {
    n <- function(x) gsub("[^a-z0-9]+", "", tolower(trimws(as.character(x %||% ""))))
    nzchar(n(a)) && identical(n(a), n(b))
  }
  # What the box she has drawn contains, and what is printed beside it.
  fb_box <- reactive({
    br <- input$fb_brush; p <- fb_doc()
    if (is.null(br) || is.null(p)) return(NULL)
    i <- tryCatch(read_input(p), error = function(e) NULL); if (is.null(i)) return(NULL)
    pg <- .clamp_page(input$fb_rf_page, fb_n_pages())
    wl <- i$words %||% list(); if (pg > length(wl)) return(NULL)
    box <- list(x_min = br$xmin, x_max = br$xmax, y_min = br$ymin, y_max = br$ymax)
    c(.fb_read_box(wl[[pg]], box), list(page = pg, box = box))
  })
  # Reading by its WORDING travels to the next copy of this document, wherever the
  # value has moved to; reading from the box does not. So prefer the wording -- but
  # only when the wording actually finds the value she boxed. That is a question
  # the tool can answer, and answering it is what makes "the value is printed away
  # from its wording" an observation rather than a section of the form.
  fb_by_wording <- function(label, vtype) {
    p <- fb_doc(); if (is.null(p) || !nzchar(label %||% "")) return(NA_character_)
    i <- tryCatch(read_input(p), error = function(e) NULL); if (is.null(i)) return(NA_character_)
    r <- tryCatch(match_label(list(any_of = list(label), value = vtype), i$pages %||% character(0)),
                  error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$matched)) NA_character_ else trimws(r$value %||% "")
  }
  # Say what was read, before anything is named or saved.
  output$fb_box_read <- renderUI({
    b <- fb_box()
    if (is.null(b)) return(p(class = "muted", style = "margin:4px 0 8px",
                             "Draw a box; what it says appears here."))
    div(class = "note", style = "margin:4px 0 8px",
      div(HTML(sprintf("In the box: <b>%s</b>",
        htmltools::htmlEscape(if (nzchar(b$value)) b$value else "(nothing readable)")))),
      div(class = "muted", if (nzchar(b$label))
            sprintf("Printed beside it: \"%s\"", b$label)
          else "Nothing beside it - read from this spot."))
  })
  # Offer the wording as the name (she can overwrite it) and the kind the value
  # reads as. Only fills a name box she has left empty.
  observeEvent(fb_box(), {
    b <- fb_box(); req(b)
    if (!nzchar(trimws(input$fb_rf_field %||% "")) && nzchar(b$label))
      updateTextInput(session, "fb_rf_field", value = b$label)
    updateSelectInput(session, "fb_rf_type", selected = .fb_kind(b$value))
  })
  output$fb_values_note <- renderUI({
    if (length(fb_regions())) return(NULL)
    p(class = "muted", style = "margin:4px 0", "Nothing added yet.")
  })
  output$fb_has_values <- reactive({ length(fb_regions()) > 0L })
  outputOptions(output, "fb_has_values", suspendWhenHidden = FALSE)
  # ONE upload for this whole flow. The page asks for the document at the top;
  # asking again inside the builder ("sample PDF to draw on") meant the first
  # picker did nothing at all on this path, and nothing said so. The document
  # uploaded at the top is used by default; the builder's own picker stays for the
  # case where a different PDF is handier to draw on.
  # A document handed over from the toolkit ("Not a transaction table?"). Without
  # it, the answer to that question would be "upload it again" -- and opened from
  # the Convert page the file was never in ts_file to begin with.
  fb_handoff <- reactiveVal(NULL)
  fb_doc <- reactive({
    if (!is.null(input$fb_sample)) return(input$fb_sample$datapath)   # an explicit choice wins
    h <- fb_handoff(); if (!is.null(h) && file.exists(h)) return(h)
    f <- input$ts_file
    if (!is.null(f) && identical(tolower(tools::file_ext(f$name %||% "")), "pdf"))
      return(f$datapath)
    NULL
  })
  fb_n_pages <- reactive({
    p <- fb_doc(); if (is.null(p)) return(NA_integer_)
    i <- tryCatch(read_input(p), error = function(e) NULL); if (is.null(i)) return(NA_integer_)
    n <- length(i$pages %||% i$words %||% list())
    if (n >= 1L) as.integer(n) else NA_integer_
  })
  observe({
    n <- fb_n_pages(); if (is.na(n)) return()
    updateNumericInput(session, "fb_rf_page", max = n,
      label = if (n == 1L) "Page (this document is 1 page)" else sprintf("Page (1 to %d)", n))
  })
  output$fb_has_sample <- reactive({ !is.null(fb_doc()) })
  outputOptions(output, "fb_has_sample", suspendWhenHidden = FALSE)
  observeEvent(input$fb_rf_set, {
    # Lower snake, like every other field name in the tool - and the only spelling
    # the base dictionary's synonyms are keyed under, so "Opening balance" typed
    # here still inherits "balance brought forward".
    nm <- tolower(gsub("^_+|_+$", "", gsub("[^A-Za-z0-9_]+", "_", trimws(input$fb_rf_field %||% ""))))
    b <- fb_box()
    if (is.null(b)) { showNotification("Draw a box round the value first.", type = "warning"); return() }
    if (!nzchar(nm)) { showNotification("Say what this value is first.", type = "warning"); return() }
    vt <- input$fb_rf_type %||% "text"
    # Compare like with like: the box text coerced the same way the extractor
    # would coerce it ("text" has no coercion, so the box text stands as read).
    boxval <- .value_from_line(b$value, vt)
    if (is.na(boxval) || !nzchar(boxval)) boxval <- b$value
    byword <- .fb_same(fb_by_wording(b$label, vt), boxval)
    r <- fb_regions()
    r[[nm]] <- list(page = b$page,
                    x_min = round(b$box$x_min), x_max = round(b$box$x_max),
                    y_min = round(b$box$y_min), y_max = round(b$box$y_max),
                    value = vt, label = if (byword) b$label else "",
                    read = trimws(b$value))
    fb_regions(r)
    updateTextInput(session, "fb_rf_field", value = "")
    session$resetBrush("fb_brush")
    showNotification(sprintf("Added '%s' - %s.", nm,
      if (byword) sprintf("read by its wording \"%s\"", b$label) else "read from this spot on the page"),
      type = "message", duration = 5)
  })
  observeEvent(input$fb_rf_clear, fb_regions(list()))
  # The list so far, said as how each value will be FOUND next time -- which is
  # the thing that decides whether the template still works on the next document.
  output$fb_regions_tbl <- renderTable({
    r <- fb_regions(); if (!length(r)) return(NULL)
    do.call(rbind, lapply(names(r), function(nm) { b <- r[[nm]]
      data.frame(value = nm, reads = b$read %||% "",
        `found by` = if (nzchar(b$label %||% "")) sprintf("its wording \"%s\"", b$label)
                     else sprintf("its place on page %d", b$page),
        check.names = FALSE, stringsAsFactors = FALSE) }))
  })
  fb_render <- reactive({
    p <- fb_doc(); req(p)
    render_page_view(p, .clamp_page(input$fb_rf_page, fb_n_pages()), 100)
  })
  output$fb_plot <- renderPlot({
    r <- fb_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i", xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    for (nm in names(fb_regions())) { b <- fb_regions()[[nm]]
      if (isTRUE(b$page == r$pg)) {
        rect(b$x_min, b$y_max, b$x_max, b$y_min, border = "#a15c00", lwd = 2)
        text(b$x_min, b$y_min, nm, col = "#a15c00", font = 2, cex = 0.9, adj = c(0, 1))
      } }
  })
  fb_template <- reactive({
    phrases <- trimws(strsplit(input$fb_fp %||% "", "\n")[[1]]); phrases <- phrases[nzchar(phrases)]
    flds <- parse_fields_spec(input$fb_fields)
    # Merge the drawn values. Read by WORDING when the wording was found to reach
    # the value she boxed (it survives the value moving on the next document);
    # otherwise pin the box, which is the "printed away from its wording" case,
    # decided by the tool instead of asked as a question.
    for (nm in names(fb_regions())) { b <- fb_regions()[[nm]]
      flds[[nm]] <- if (nzchar(b$label %||% ""))
        list(any_of = list(b$label), value = b$value)
      else list(region = list(page = b$page, x_min = b$x_min, x_max = b$x_max,
                              y_min = b$y_min, y_max = b$y_max), value = b$value)
    }
    list(id = gsub("[^A-Za-z0-9_]+", "_", input$fb_id %||% "newpdf_fields"),
         bank = input$fb_bank %||% "NewIssuer", statement_type = input$fb_type %||% "summary",
         format = "pdf", mode = "fields", version = 1,
         fingerprint = list(page_contains_all = as.list(phrases)),
         fields = flds, currency = "NZD")
  })
  output$fb_yaml <- renderText({ t <- fb_template(); t$origin <- NULL; yaml::as.yaml(t) })

  fb_preview <- reactiveVal(NULL)
  observeEvent(input$fb_preview, {
    p <- fb_doc()
    if (is.null(p)) { showNotification("Upload the document at the top of this page first.",
                                       type = "warning", duration = 6); return() }
    if (!length(fb_template()$fields)) {
      showNotification("Nothing to pull out yet - draw a box.",
                       type = "warning", duration = 6); return() }
    inp <- tryCatch(read_input(p), error = function(e) NULL)
    if (is.null(inp)) { showNotification("Couldn't read that file.", type = "error"); return() }
    f <- tryCatch(extract_fields(inp, fb_template()), error = function(e) NULL)
    fb_preview(f)
  })
  # Same verdict card as everywhere else, and it names the ones that came back
  # empty: a value that did not match is the whole reason to look at this preview.
  output$fb_prev_status <- renderUI({
    f <- fb_preview()
    if (is.null(f)) return(p(class = "muted", "Nothing previewed yet."))
    got <- sum(f$matched %in% TRUE); miss <- nrow(f) - got
    missing <- if (miss > 0) paste(f$field[!(f$matched %in% TRUE)], collapse = ", ") else ""
    div(class = if (miss > 0) "verdict verdict-medium" else "verdict verdict-high",
        style = "margin:2px 0 12px",
      div(class = "verdict-ico", if (miss > 0) "!" else "✓"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", sprintf("%d of %d value%s found", got, nrow(f),
                                             if (nrow(f) == 1L) "" else "s")),
        # No "Draw a box round it." -- reaching this line means a box HAS been
        # drawn and read nothing, so the advice was to repeat what had just failed.
        p(class = "verdict-body", style = "margin:0", if (miss > 0)
            sprintf("Not found: %s.", missing)
          else "Check each value, then Save template.")))
  })
  output$fb_prev_tbl <- renderDT({
    f <- fb_preview(); req(!is.null(f))
    datatable(f[, intersect(c("field", "label", "value", "matched"), names(f))],
              rownames = FALSE, options = list(dom = "t", pageLength = 25))
  })
  observeEvent(input$fb_save, {
    t <- fb_template()
    probs <- validate_fields_template(t)
    if (length(probs)) {
      output$fb_msg <- renderUI(span(class = "bad", paste("Not valid:", paste(probs, collapse = "; ")))); return() }
    ok <- tryCatch({ save_fields_template(t, USER_FIELDS_DIR); TRUE }, error = function(e) FALSE)
    # NOT "it's detected automatically". convert_document() only reaches the form
    # templates at all once the STATEMENT path has returned `unsupported`
    # (R/forms.R): the same PDF saved as a form here still came back "Read as: ASB
    # everyday statement" on Convert, because a statement template matched it first.
    output$fb_msg <- renderUI(if (isTRUE(ok))
      span(class = "ok", sprintf("Saved '%s'. On Convert it is used when no statement template reads the document.", t$id))
      else span(class = "bad", "Couldn't save - check the fields."))
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
  output$ix_legend <- renderUI({
    st <- ix_state(); req(st, st$is_pdf, !is.null(st$layout))
    pg <- as.character(ix_page_now())
    P <- st$layout$pages[[pg]]; req(P)
    pal <- ix_pal(P$bands)
    layers <- ix_layers_now()
    sw <- function(col, lab) tags$div(style = "margin:2px 0",
      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;border:2px solid %s;margin-right:6px;vertical-align:middle", col)),
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
    n_skip <- if (is.null(rows$reason)) 0L
              else sum(!(rows$kept %in% TRUE) & grepl("didn't parse|no amount", rows$reason %||% ""))
    has_red <- any(w$redacted %in% TRUE)
    ml <- if (!is.null(st$meta_loc)) st$meta_loc[[ix_page_now()]] else NULL
    has_meta <- (!is.null(ml) && any(ml$found %in% TRUE)) || length(P$meta_regions %||% list()) > 0
    tagList(strong("Legend"),
      # Friendly names, and the SAME ones the picture writes over each band: the
      # stored column names are the engine's ("other_party"), not the reader's.
      if (has_cols) lapply(names(pal), function(nm) sw(pal[[nm]], cv_friendly_cols(nm))),
      if ("kept" %in% layers && n_kept > 0) sw("#137333", "transaction row (kept)"),
      if ("skipped" %in% layers && n_skip > 0) sw("#c77700", "skipped row that looks like a transaction"),
      if ("meta" %in% layers && has_meta) sw("#a15c00", "balance / account details"),
      if ("redact" %in% layers && has_red) sw("#b00020", "redaction (not read)"),
      if (has_ocr) sw("#c77700", "shaded amber = machine-read word the tool is unsure about - double-check it"))
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
                         border = "#c77700", col = "#c7770040", lwd = 1.2)
    }
    if ("cols" %in% layers) {
      sel <- w[!is.na(w$column), , drop = FALSE]
      if (nrow(sel)) rect(sel$x, sel$y, sel$x + sel$width, sel$y + sel$height,
                          border = pal[sel$column], lwd = 1.3)
    }
    if ("redact" %in% layers) {
      red <- w[w$redacted %in% TRUE, , drop = FALSE]
      if (nrow(red)) rect(red$x, red$y, red$x + red$width, red$y + red$height,
                          border = "#b00020", col = "#b0002022", lwd = 1)
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
      if (nrow(kr)) rect(kr$x0 - 1, kr$y0 - 1, kr$x1 + 1, kr$y1 + 1, border = "#137333", lwd = 1)
    }
    # Amber dashed: rows the engine skipped that LOOK like transactions (bad date
    # or missing amount) -- the "why aren't you seeing it" rows. Continuations,
    # summaries and headings are intentionally left unhighlighted (they're in the
    # table below with their reason) so the page isn't noisy.
    if ("skipped" %in% layers && !is.null(P$rows$reason)) {
      sk <- P$rows[!P$rows$kept &
        grepl("didn't parse|no amount", P$rows$reason %||% ""), , drop = FALSE]
      if (nrow(sk)) rect(sk$x0 - 1, sk$y0 - 1, sk$x1 + 1, sk$y1 + 1,
                         border = "#c77700", lty = 2, lwd = 1.6)
    }
    if ("meta" %in% layers && !is.null(st$meta_loc)) {
      ml <- st$meta_loc[[r$pg]]
      if (!is.null(ml)) { f <- ml[ml$found %in% TRUE, , drop = FALSE]
        if (nrow(f)) { rect(f$x0 - 2, f$y0 - 2, f$x1 + 2, f$y1 + 2, border = "#a15c00", lwd = 2)
          text(f$x1 + 3, (f$y0 + f$y1) / 2, f$field, col = "#a15c00", font = 2, cex = 0.8, adj = c(0, 0.5)) } }
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
    actionable <- grepl("didn't parse|no amount", sk$reason)
    sk[order(!actionable), , drop = FALSE]           # likely-missed transactions first
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
    trunc <- function(s, n = 90) ifelse(nchar(s) > n, paste0(substr(s, 1, n), "…"), s)
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
    res <- convert_now(src$path, sess, forced_rows = cv_forced())
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
  # When the browser tab closes, take this session's scratch folder with it. The
  # folder holds a copy of the client's statement plus every output; the process
  # temp dir is only cleared when R exits, and this app is a long-running service.
  session$onSessionEnded(function() {
    d <- isolate(cv_dir())
    if (!is.null(d) && nzchar(d) && dir.exists(d)) try(unlink(d, recursive = TRUE), silent = TRUE)
  })

  # convert_now(src, sess, forced_rows) -- run the one front-door conversion with a
  # progress bar, reading bank / exact-template from the current inputs. Shared by
  # the Convert button and the X-ray "add this row" action so both paths behave
  # identically (the second just adds force_rows and reuses the same session dir).
  convert_now <- function(src, sess, forced_rows = NULL, force_tpl = NULL,
                          include_user = FALSE) {
    bank <- bank_choice()
    forced_tpl <- force_tpl %||% tpl_choice()
    # The tick-box, an explicit force, or an explicit include_user brings in the
    # user-created set. include_user exists because updateCheckboxInput only
    # reaches the browser AFTER this observer finishes: right after a save we know
    # this is for the moment right after a template is saved, when the caller
    # already knows the new template must take part even if the deployment has
    # user-built templates switched off.
    use_user <- USE_USER_TEMPLATES || !is.null(force_tpl) || isTRUE(include_user)
    who <- who_now()
    withProgress(message = "Converting statement…", value = 0.2, {
      incProgress(0.2, detail = "Reading the file and detecting its format…")
      out <- tryCatch(
        convert_document(src, bank = bank, outdir = sess,
                         templates_dir = TEMPLATES_DIR,
                         user_templates_dir = if (use_user) USER_TEMPLATES_DIR else NULL,
                         fields_dir = FIELDS_DIR, user_fields_dir = USER_FIELDS_DIR,
                         requested_by = who, logdir = LOGDIR,
                         force_template = forced_tpl, force_rows = forced_rows),
        error = function(e) {
          safe(cat(sprintf("[%s] convert error (%s): %s\n", format(Sys.time()),
                           basename(src), conditionMessage(e)),
                   file = file.path(LOGDIR, "errors.log"), append = TRUE))
          list(status = "failed", messages = FRIENDLY_READ_ERROR)
        })
      incProgress(0.5, detail = "Running checks and writing outputs…")
      out
    })
  }

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
    # Reclaim the PREVIOUS conversion's scratch folder, AFTER the copy above (a
    # re-convert is often handed the file that lives in it). It holds the outputs
    # AND a copy of the client's statement, and nothing was ever removing it: on a
    # server that runs for months, every conversion by every user stayed on disk for
    # the life of the process. Once a new conversion starts the old outputs are
    # unreachable from the UI anyway.
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
    who <- who_now()
    res <- convert_now(src, sess, forced_rows = NULL, force_tpl = force_tpl,
                       include_user = include_user)
    cv_dir(sess)
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
    if (!is.null(old) && nzchar(old) && !identical(old, sess) && dir.exists(old))
      safe(unlink(old, recursive = TRUE))
    safe(sweep_temp_dirs(keep_hours = 24,
                         exclude = c(sess, dirname(isolate(guided())$path %||% "."))))
    who <- who_now(); n <- length(paths)
    # A 50-file case must not look frozen, and "converting…" for four minutes is
    # the same as frozen. The callback names the file being read RIGHT NOW, so the
    # bar answers "is it stuck?" and "how much longer?" at the same time.
    # convert_batch() hands back each file's WHOLE result, rows included, and says
    # so: trimming is the caller's job because only the caller knows when it has
    # finished with them. This one has not -- the governed feed is written from the
    # parsed rows -- so they are dropped below, per file, the moment that write is
    # done. Anything convert_batch does not itself take goes to convert_document(),
    # which has no `...`, so a stray argument here fails every file in the case.
    b <- withProgress(message = sprintf("Converting %d files…", n), value = 0,
      convert_batch(paths,
        outdir = sess, templates_dir = TEMPLATES_DIR,
        user_templates_dir = if (USE_USER_TEMPLATES) USER_TEMPLATES_DIR else NULL,
        fields_dir = FIELDS_DIR, user_fields_dir = USER_FIELDS_DIR,
        requested_by = who, logdir = LOGDIR,
        bank = bank_choice(), force_template = tpl_choice(),
        progress = function(i, nn, f)
          setProgress(value = (i - 1) / nn,
                      detail = sprintf("%d of %d - %s", i, nn, basename(f)))))
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
    cv_dir(sess)
    # No file is open yet -- the table is. show_result() with nothing in it says
    # exactly that, and clears every per-file piece of state in one place rather
    # than leaving any of it pointing at whatever the loop above converted last.
    # (The loop's publish_result calls leave the LAST file's feed verdict behind;
    # this is what takes it off the screen's copy.)
    show_result()
    cv_batch_row(NA_integer_)
    cv_batch(b)
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
  observeEvent(input$cv_batch_rows_selected, {
    sel <- input$cv_batch_rows_selected
    if (length(sel)) open_batch_row(as.integer(sel[1]))
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

  # THE TABLE. Sorting is the whole point of it, so the two columns worth sorting
  # by sort by MEANING, not by spelling: "Result" orders worst-first off the
  # engine's own status order (BATCH_STATUSES), not alphabetically, and the table
  # opens on that order with the failure kind as the tie-break -- so the pile that
  # needs work is already at the top and already grouped.
  output$cv_batch <- renderDT({
    b <- cv_batch(); req(b)
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
      # `res$trust$level` and so does this.
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
    datatable(disp, rownames = FALSE, selection = "single",
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
                                     c("#137333", "#8a6d00", "#b00020")))
  })

  output$cv_batch_open <- renderUI({
    b <- cv_batch(); req(b)
    i <- cv_batch_row()
    if (is.na(i))
      return(p(class = "muted", style = "margin:8px 0 0",
               "Click a row for that file's full result."))
    div(style = "margin:16px 0 2px;padding-top:12px;border-top:1px solid var(--line)",
      h4(style = "margin:0", sprintf("Showing: %s", basename(b$file[i]))))
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
      !identical(res$kind, "form") && length(res$outputs %||% character(0)) > 0
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
    if (identical(res$kind, "form")) return(NULL)
    open <- isTRUE(cv_detail_open())
    # Named for what is BEHIND it, not for the mechanism. "Advanced" would make an
    # accountant feel it is not for her; "how it read this" is a question she may
    # genuinely want answered, and it is the honest description of the contents.
    div(style = "margin:16px 0 6px",
      actionLink("cv_more", style = "font-weight:700;font-size:14.5px",
        label = if (open) "Hide how it read this" else "Show me how it read this"),
      # One caption. It used to be an if/else whose two branches were the same
      # string, byte for byte -- a dead conditional pretending to be a decision.
      div(class = "muted", style = "font-size:13px;margin-top:2px",
          "The page, the checks, and the template it used."))
  })

  # Empty state: shown before the first conversion. Tells a brand-new user what
  # this page is for and exactly what they'll get back, so the screen is never a
  # mystery or a wall of empty headers.
  output$cv_empty <- renderUI({
    div(style = "max-width:560px;color:#444;line-height:1.6",
      h4(style = "margin-top:4px", "Convert a bank statement"),
      p("Upload a statement on the left - a ", tags$b("PDF"), ", ", tags$b("CSV"),
        " or ", tags$b("Excel"), " file - and click ", tags$b("Convert"), "."),
      p(class = "muted", style = "margin-bottom:6px", "You'll get back:"),
      tags$ul(style = "color:#444",
        tags$li(tags$b("Every transaction"), ", read verbatim - date, description, amount, balance."),
        # NOT "proof nothing's missing". On four of the six statements this was
        # measured against, balance_reconciliation could not run at all -- no
        # printed opening or closing and no running-balance column to derive one
        # from -- so the page promised a proof it then did not give. What it always
        # does give is the answer either way.
        tags$li(tags$b("Whether it reconciles"), " - the balance proof, or plainly why it couldn't run."),
        tags$li(tags$b("Your download"), " - Excel or CSV.")),
      p(class = "muted", "Your bank is detected automatically. A layout the tool hasn't seen points you to ",
        actionLink("cv_empty_to_tmpl", "Add a template"), "."),
      # First visit, nothing to upload yet? One click shows the whole payoff on
      # a bundled specimen statement (public, synthetic - not anyone's real data).
      if (file.exists(SAMPLE_STATEMENT))
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
    if (isTRUE(res$status == "ok") && !identical(res$kind, "form")) return(NULL)
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
    # No confidence grade on a run that produced nothing: there is no work to be
    # confident about, and "confidence: low" beside "no template yet" reads as a
    # warning about the file rather than a plain statement of where we are.
    graded <- st %in% c("ok", "needs_review")
    trust <- if (!is.null(res$trust) && graded) sprintf(" · confidence: %s", res$trust$level) else ""
    # Name the template ONLY when one was actually used to read the statement. On
    # an unsupported result the engine still carries a template id -- the CLOSEST
    # MISS, kept for the logs -- and printing it under "No template for this
    # statement yet" read as though that template had read the file. It had not.
    tid <- if (st %in% c("ok", "needs_review")) (res$template_id %||% NA_character_)[1]
           else NA_character_
    div(class = paste0("verdict verdict-", lvl),
      div(class = "verdict-ico", if (identical(lvl, "high")) "✓" else "!"),
      div(style = "flex:1;min-width:0",
        div(class = "verdict-title", paste0(headline, trust)),
        lapply(plain_messages(res$messages), function(m) p(class = "verdict-body", m)),
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
            tags$b(plain_check(f$name[i])),
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
  .proof_pick <- function(name, status, listed) {
    pick <- listed[listed %in% name]
    c(pick, setdiff(name[status %in% "fail"], pick))
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
        pass = list("\u2713", "#137333", "#e9f5ec"),
        fail = list("\u2717", "#b00020", "#fdecef"),
        list("\u2013", "#6b7780", "#f2f4f6"))     # na / anything else: could not run
      span(style = sprintf(paste0("display:inline-flex;align-items:center;gap:6px;",
                                  "margin:0 8px 6px 0;padding:5px 10px;border-radius:999px;",
                                  "background:%s;color:%s;font-size:13px;font-weight:600"),
                           m[[3]], m[[2]]),
           span(style = "font-size:14px", m[[1]]), plain_check(nm))
    }
    # THE KEY, because this strip is the first quality signal on the page and it
    # had none: no legend, no title attribute, nothing in About or in either
    # guide. A grey dash beside "Opening + transactions = closing balance" above
    # the fold is either the best or the worst news on the screen, and there was
    # nothing anywhere to tell a reader which. Same three glyphs the chips draw.
    div(style = "margin:2px 0 12px",
      lapply(pick, chip),
      # The glyphs are the SAME escapes the chips are drawn from, not literals
      # retyped beside them: a key that can drift from its picture is worse than
      # no key at all.
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
    if (!(tid %in% names(all_templates()))) {
      ft <- tryCatch(all_field_templates()[[tid]], error = function(e) NULL)
      lab <- if (is.null(ft)) NA_character_ else .tpl_label(ft$bank, ft$statement_type)
      return(if (is.na(lab)) tid else lab)
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
      high   = list(cls = "ok",   icon = "✓",
                    line = "Every transaction adds up to the closing balance. Nothing is missing."),
      medium = list(cls = "warn", icon = "✓",
                    line = "Read cleanly. Something could not be proven - the checks below say which."),
      low    = list(cls = "bad",  icon = "!",
                    line = "Check these against the statement before you use them."),
      list(cls = "warn", icon = "✓",
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
    base <- sub("[[:space:]]*\\[statement [0-9]+\\]$", "", k$name)   # split-aware
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
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
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
          if (!is.na(n)) sprintf(" — %d transaction%s read", n, if (n == 1) "" else "s") else "",
          if (nzchar(lev)) sprintf(" · confidence: %s", lev) else "")),
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
                                   GBP = "£", EUR = "€", "")

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
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
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
        card("Money in",  fmt_money(money_in, cur),  "#137333"),
        card("Money out", fmt_money(money_out, cur), "#b00020"),
        card("Net",       fmt_money(net, cur), if (isTRUE(net < 0)) "#b00020" else "#137333"),
        if (has_close) card("Closing balance", fmt_money(as.numeric(h$closing_balance), cur))),
      p(class = "muted", style = "margin:0 0 4px", sprintf("%s%s%s",
        paste(ranges, collapse = "  \u00b7  "),
        if (!is.na(h$account_number %||% NA_character_)) sprintf("  ·  Account: %s", h$account_number) else "",
        if (!is.na(h$bank %||% NA_character_)) sprintf("  ·  %s", h$bank) else "")),
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
    GREEN <- "#0b7a34"; RED <- "#b00020"; BLUE <- "#00205b"; BLUE_FILL <- "#00205b1f"
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
  # `flagged` is the engine's own column, not re-derived here: it means required
  # and not found, and it is the only row on the table that needs an action.
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
      `Needs a look` = ifelse(f$flagged %in% TRUE, "yes - required and not found", ""),
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(disp, rownames = FALSE, options = list(dom = "t", pageLength = 30)) |>
      formatStyle("Found", fontWeight = "bold",
                  color = styleEqual(c("yes", "no"), c("#137333", "#b00020")))
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
            else sub("[[:space:]]*\\[statement [0-9]+\\]$", "", k$name) %in% INFORMATIONAL_CHECKS
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
        color = styleEqual(unname(RESULT_PLAIN[c("pass", "fail")]), c("#137333", "#b00020")))
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
      "No template read this statement, so no transactions were produced - there is nothing to check and no fields to report."
  }
  output$cv_detail <- renderUI({
    res <- cv_res(); req(res)
    k <- res$kpis
    any_failed <- !is.null(k) && "status" %in% names(k) && any(k$status %in% "fail")
    open_it <- !isTRUE((res$status %||% "") == "ok") || isTRUE(any_failed)
    has_kpis <- is.data.frame(k) && nrow(k) > 0L
    has_cov  <- is.data.frame(res$coverage) && nrow(res$coverage) > 0L
    said <- p(class = "muted", style = "margin:0 0 8px", .why_empty(res))
    tags$details(style = "margin-top:14px", open = if (open_it) NA else NULL,
      tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
                   "Checks & detail (for review)"),
      div(style = "padding:8px 2px",
        h4("Checks"), if (has_kpis) DTOutput("cv_kpis") else said,
        h4("Diagnostics - where / why / how to fix"), DTOutput("cv_diag"),
        h4("Field coverage - what's present / empty / not on this statement"),
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
      Verdict = plain_label(cov$verdict, COVERAGE_PLAIN),   # 'unmapped' -> 'not on this statement'
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
    labs <- c(xlsx = "⭳ Excel", csv = "⭳ CSV")
    has <- function(ext) any(grepl(paste0("\\.", ext, "$"), outputs %||% character(0)))
    Filter(Negate(is.null), lapply(names(ids), function(ext)
      if (has(ext) && !is.na(labs[ext])) downloadButton(ids[[ext]], labs[[ext]],
        class = if (ext == "xlsx") "btn-primary" else NULL)))
  }
  output$cv_downloads <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    btns <- dl_buttons(res$outputs, c(xlsx = "dl_xlsx", csv = "dl_csv"))
    has_json <- any(grepl("\\.json$", res$outputs %||% character(0)))
    if (!length(btns) && !has_json) return(NULL)
    # Prominent bar right under the verdict: the download is the point of the page,
    # so it's the most visible thing, not a quiet box tucked into the sidebar.
    div(class = "dl-hero", span(class = "dl-hero-label", "Download your converted data:"), btns,
      if (has_json)
        span(class = "muted", style = "font-size:12.5px;margin-left:2px",
             downloadLink("dl_json", "JSON", class = "muted")))
  })

  # cv_feed -- did this conversion reach the org dashboards, and if not, why not?
  #
  # write_feed() already decides that, records it in the feed manifest and logs it;
  # this is the ONE place the person who ran the conversion is told. It matters
  # most on the quiet case: a perfectly good conversion read by a template built
  # here is withheld as "not proven", which is correct governance and was
  # completely invisible. Wording lives in FEED_PLAIN (ui_labels.R), keyed by the
  # engine's own gate reason, so the screen and the manifest cannot drift apart.
  output$cv_feed <- renderUI({
    res <- cv_res(); req(res)
    # Only where there were figures to publish. On an unsupported / failed result
    # the card above already says nothing was converted, and repeating it here
    # would push the "set this layout up" action down the page for no new fact.
    if (!isTRUE((res$status %||% "") %in% c("ok", "needs_review"))) return(NULL)
    fb <- plain_feed(cv_feed_gate())
    if (is.null(fb)) return(NULL)          # feed switched off, or a form result
    # ...UNLESS the reviewer has since marked this conversion WRONG, which
    # retracts its rows (submit_feedback -> retract_feed). The gate reactive holds
    # the verdict from the WRITE and nothing refreshes it, so "Sent to the
    # dashboards. …now available to the Qlik dashboards." sat on screen at the same
    # time as "7 row(s) have been withdrawn from the dashboards" a few inches
    # below. Read the retraction here rather than write the gate a third time: the
    # feed manifest is the authority on what was published, and this is the screen.
    rec <- cv_fb_rec()
    if (isTRUE(identical(rec$verdict, "wrong")) && isTRUE(fb$ok))
      fb <- list(ok = FALSE, line = "Withdrawn from the dashboards.", why = "")
    div(style = paste("display:flex;gap:8px;align-items:baseline;flex-wrap:wrap;",
                      "margin:0 0 12px;font-size:13px"),
      span(class = if (isTRUE(fb$ok)) "ok" else "bad", style = "font-weight:600", fb$line),
      if (nzchar(fb$why %||% "")) span(class = "muted", fb$why))
  })
  # cv_rematch -- an escape hatch for a WRONG match: a bank that matched the wrong
  # template (or the wrong bank) needs a one-click "no, set up the right template
  # for this" without hunting. Drafts a fresh template from THIS file (never seeded
  # from the wrong match).
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
  output$cv_rematch <- renderUI({
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
    st <- res$status %||% "failed"
    if (!(st %in% c("ok", "needs_review"))) return(NULL)   # unsupported/failed already prompt setup
    tid <- (res$template_id %||% NA_character_)[1]
    nice <- if (!is.na(tid) && nzchar(tid)) friendly_tpl(tid) else NA_character_
    quiet <- div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:8px;margin:0 0 12px;color:var(--muted);font-size:13px",
      span("Not the right bank?"),
      actionLink("cv_rematch_go", "Set up the right template for this statement"))
    # Happy path: one quiet line, and it does NOT re-state which template read
    # the statement -- the "Read as" chip on the verdict card two inches above
    # says that already, and saying it twice makes a question out of a fact.
    if (identical(st, "ok")) return(quiet)
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
  observeEvent(input$cv_rematch_go,    .rematch_now())
  observeEvent(input$cv_rematch_go_rv, .rematch_now())
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

  # ---- Feedback (every conversion can be rated; one file per logs/feedback/) ----
  #
  # A CONVERSION THAT PRODUCED NOTHING IS NOT A CONVERSION TO RATE. "Was this
  # conversion correct?" appeared under a card reading "Could not read this file"
  # -- a question about figures on a screen with no figures, and the one answer
  # that does anything (Wrong) withdraws rows from the dashboards that were never
  # published. The run is still logged; there is simply nothing here for a reviewer
  # to have an opinion about.
  output$cv_feedback <- renderUI({
    res <- cv_res(); if (is.null(res) || is.null(res$run_id)) return(NULL)
    if (!isTRUE((res$status %||% "") %in% c("ok", "needs_review"))) return(NULL)
    if (isTRUE(cv_fb_done()))
      return(div(style = "margin-top:16px", span(class = "ok",
        "Thanks - your feedback was recorded."), cv_fb_note()))
    div(style = "margin-top:16px;padding:12px;border:1px solid #ddd;border-radius:6px",
        h4("Was this conversion correct?"),
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
    div(class = "muted", style = "margin-top:4px",
      if (is.na(n))
        "The tool could not reach the feed folder to withdraw its rows - tell whoever looks after the server."
      else if (n > 0)
        sprintf("%d row(s) withdrawn from the dashboards and moved to the held-back feed. Fixing the template and converting again puts the corrected figures back.", n)
      else
        "Nothing to withdraw - this conversion had not been published to the dashboards.")
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
        textInput("g_pdf_custom", "…or a column name of your own (overrides the list)",
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
              selectInput("g_unsigned_default", "A plain number (no + / − / CR) is a…",
                          choices = c("Charge - money out" = "debit",
                                      "Payment - money in" = "credit"),
                          selected = cur_ud)))),
        conditionalPanel(
            "input.g_sign == 'type_dc'",
            fluidRow(
              column(6, textInput("g_type_debit", "Which indicator value means money OUT (debit)?",
                                  value = tmpl$type_debit_value %||% "")),
              column(6, textInput("g_type_credit", "…and money IN (credit)? (blank = anything else is a credit)",
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
            p(class = "muted", style = "margin:4px 0 6px",
              HTML(paste0("How the tool recognises this bank next time. Something printed on the page that other banks do <b>not</b> print - the statement heading, or the bank's own name; never a customer's name. ",
                          "One phrase per line; all must appear."))),
            if (length(g$fp_candidates)) fluidRow(
              column(9, selectInput("g_fp_pick", "Phrases found on your statement",
                                    choices = c("(choose one to add)" = "", g$fp_candidates),
                                    width = "100%")),
              column(3, br(), actionButton("g_fp_add", "Add it"))),
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
            column(6, textInput("g_id", "Saves under this name", value = tmpl$id %||% "")),
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
            column(6, .eff_picker("g_eff_to", "…to (empty = no end)",
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
        HTML(sprintf("Setting up: <b>%s</b> &nbsp;·&nbsp; %s &nbsp;·&nbsp; ",
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
          paste0("<b>How this works:</b> &nbsp;1&nbsp;Drag a box over a column and say what it is &nbsp;·&nbsp; ",
                 "2&nbsp;click <b>Assign it</b> &nbsp;·&nbsp; 3&nbsp;name the bank on the right &nbsp;·&nbsp; ",
                 "4&nbsp;check the <b>Preview</b> below &nbsp;·&nbsp; 5&nbsp;<b>Save</b>.")
        else
          paste0("<b>How this works:</b> &nbsp;1&nbsp;check each column picker matches your file &nbsp;·&nbsp; ",
                 "2&nbsp;name the bank &nbsp;·&nbsp; 3&nbsp;check the <b>Preview</b> below &nbsp;·&nbsp; ",
                 "4&nbsp;<b>Save</b>."))),
      fluidRow(column(6, left_panel), column(6, right_panel)),
      tags$hr(),
      h4("Preview - what will be pulled out of your statement"),
      uiOutput("g_status"),
      DTOutput("g_preview"),
      footer = tagList(modalButton("Cancel"),
        actionButton("g_save", "Save template", class = "btn-primary"))))
  }

  # "Not a transaction table?" -- close the toolkit, put the SAME file in the form
  # builder and select it there. Nothing is re-uploaded and nothing is lost.
  observeEvent(input$g_not_statement, {
    g <- isolate(guided())
    removeModal()
    if (!is.null(g$path) && file.exists(g$path)) fb_handoff(g$path)
    updateRadioButtons(session, "ts_doctype", selected = "other")
    updateTabsetPanel(session, "main_tabs", selected = "Add a template")
    showNotification("Switched to labelled values - your document came with you.",
                     type = "message", duration = 6)
  })

  # open_guided -- the single entry into the setup modal, shared by every launch
  # point (Convert result, Admin pickup, Add-a-template). Drafts a template from
  # the file unless the caller already has one (e.g. the matched template).
  open_guided <- function(path, name, seed_tmpl = NULL, upload_id = NA_character_) {
    tmpl <- seed_tmpl
    if (is.null(tmpl)) {
      bankguess <- trimws(tools::toTitleCase(gsub("[^A-Za-z]+", " ", tools::file_path_sans_ext(name))))
      tmpl <- withProgress(message = "Opening the toolkit…", value = 0.4,
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
        showNotification(paste("Couldn't read this file as a transaction table. Try a text PDF or a CSV export.",
                               "If it is a form or a summary, set it up on Add a template as \u201cSomething else\u201d."),
                         type = "error", duration = 10)
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
    if (identical(res$kind, "form")) {
      # A form result is set up in the PDF form builder, not the statement toolkit.
      return(div(style = "margin:12px 0;padding:10px 12px;border:1px solid #d9d9d9;background:#fafafa;border-radius:8px",
        span(class = "muted", "Change or add values? "),
        actionLink("cv_goto_templates", "Open the PDF form builder →")))
    }
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
      div(style = "margin:12px 0;padding:14px;border:1px solid #b7e1b0;background:#eef8ec;border-radius:8px",
        strong("No template read this statement — set one up and it converts every time."),
        p(class = "muted", style = "margin:6px 0 10px",
          "The tool pre-fills what it can detect from your statement; you confirm against a live preview and save."),
        actionButton("cv_teach_go", "Set up a template for this statement", class = "btn-primary btn-lg"),
        div(style = "margin-top:8px",
          span(class = "muted", "Would rather not? "),
          actionLink("cv_unsup_raise", "Send it to the team instead")))
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
  # Tick the box AND re-run in one click. include_user = TRUE because the tick has
  # not reached the browser yet when this conversion starts.
  observeEvent(input$cv_goto_templates,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))
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
      # So converting with the other template is the primary button and the toolkit
      # is the secondary link, for when the answer is "neither of these is right".
      if (length(others)) tagList(
        selectInput("cv_cand_pick", "Wrong one? Try a different template:",
                    choices = tpl_choices(others), width = "100%"),
        actionButton("cv_cand_convert", "Convert with this one instead", class = "btn-primary"),
        span(style = "margin-left:10px", class = "muted", "or "),
        actionLink("cv_cand_go", "open the toolkit with it"))))
  })
  observeEvent(input$cv_cand_go, {
    src <- cv_src(); req(src); tid <- input$cv_cand_pick; req(tid, nzchar(tid))
    tset <- tryCatch(templates(), error = function(e) list())
    seed <- tset[[tid]]
    if (is.null(seed)) { showNotification("That template isn't available.", type = "error"); return() }
    open_guided(src$path, src$name, seed_tmpl = seed, upload_id = cv_upload_id())
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
    showNotification(sprintf("Converted with %s.", tid), type = "message", duration = 5)
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

  # "Add it" -- append a phrase the drafter found on the page to the box, rather
  # than making the user retype it exactly (a fingerprint must match the page text
  # character for character, so retyping is where this goes wrong).
  observeEvent(input$g_fp_add, {
    pick <- trimws(input$g_fp_pick %||% "")
    if (!nzchar(pick)) {
      showNotification("Choose one of the phrases found on your statement first.", type = "warning"); return() }
    cur <- fingerprint_phrases(input$g_fp)
    if (pick %in% cur) {
      showNotification("That phrase is already in the box.", type = "message"); return() }
    updateTextAreaInput(session, "g_fp", value = paste(c(cur, pick), collapse = "\n"))
  })
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
      rect(b$x_min / s[1], y0, b$x_max / s[1], y1, border = "#a15c00", lwd = 2)
      text(b$x_min / s[1], y0, nm, col = "#a15c00", font = 2, cex = 0.85, pos = 3, offset = 0.2)
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
        recog <- safe({
          tset <- load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR)
          recognition_summary(detect_statement(read_input(gp), tset), saved_id)
        }, NULL)
        recog <- recog %||% recognition_summary(NULL, saved_id)
        run_conversion(gp, gn, record = FALSE,
                       force_tpl = if (isTRUE(recog$ok)) NULL else saved_id,
                       include_user = TRUE)
        showNotification(
          HTML(paste0("<b>Saved \"", htmltools::htmlEscape(saved_id), "\".</b><br>",
                      "<b>", htmltools::htmlEscape(recog$headline), "</b><br>",
                      htmltools::htmlEscape(recog$detail))),
          type = if (isTRUE(recog$ok)) "message" else "warning",
          duration = if (isTRUE(recog$ok)) 10 else NULL)
      } else {
        showNotification(sprintf("Saved as your template \"%s\". Click Convert again to run this statement with it.",
                                 saved_id %||% "template"),
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

  # ---- Admin: insights from the logs -------------------------------
  adm_data <- reactiveVal(NULL)
  load_admin <- function() adm_data(list(
    runs = tryCatch(read_runs_all(LOGDIR), error = function(e) data.frame()),  # live + archived
    fb   = tryCatch(read_feedback(LOGDIR), error = function(e) data.frame())))
  # Admin dashboard data (run logs, feedback) is loaded ONLY for an authenticated
  # admin session, so a non-admin client can never pull it by marking a hidden
  # output visible -- every admin output does req(adm_data()), which stays NULL
  # (and therefore blank) without a load.
  observeEvent(input$adm_refresh, { req(admin_ok()); load_admin() })
  observe({ req(admin_ok()); if (is.null(adm_data())) load_admin() })

  output$adm_overview <- renderDT({
    d <- adm_data(); req(d)
    datatable(runs_overview(d$runs), rownames = FALSE, options = list(dom = "t"))
  })
  output$adm_status_plot <- renderPlot({
    d <- adm_data(); req(d); ov <- runs_overview(d$runs); if (!nrow(ov)) return(NULL)
    cols <- c(ok = "#137333", needs_review = "#e3b341", unsupported = "#b00020",
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
  output$adm_drift <- renderDT({
    d <- adm_data(); req(d)
    dr <- template_drift(d$runs)
    tbl <- datatable(dr, rownames = FALSE,
                     options = dt_none_opts("No template has started failing - good.", dom = "t"))
    if (nrow(dr)) tbl <- formatStyle(tbl, "drop", fontWeight = "bold", color = "#b00020")
    tbl
  })
  observeEvent(input$adm_rollup, {
    req(admin_ok())
    r <- tryCatch(rollup_logs(LOGDIR, "runs", keep_days = LOG_KEEP_DAYS), error = function(e) NULL)
    r2 <- tryCatch(rollup_logs(LOGDIR, "feedback", keep_days = LOG_KEEP_DAYS), error = function(e) NULL)
    load_admin()
    output$adm_rollup_msg <- renderUI(span(class = "ok",
      sprintf("Archived %d old run file(s); %d kept. History is preserved in logs/archive/.",
              (r$archived %||% 0) + (r2$archived %||% 0), (r$kept %||% 0))))
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
  output$adm_feedback <- renderDT({
    d <- adm_data(); req(d); fb <- d$fb
    cols <- c("ts", "verdict", "comment", "template_id", "run_id")
    none <- if (is.null(fb) || !nrow(fb)) "Nobody has rated a conversion yet."
            else "Nothing has been rated wrong or minor-issues."
    if (is.null(fb) || !nrow(fb) || !("flagged" %in% names(fb)))
      fl <- stats::setNames(data.frame(matrix(character(0), 0, length(cols))), cols)
    else fl <- fb[as.logical(fb$flagged) %in% TRUE, intersect(cols, names(fb)), drop = FALSE]
    datatable(fl, rownames = FALSE,
              options = dt_none_opts(none, pageLength = 8, scrollX = TRUE))
  })
}

shinyApp(ui, server)
