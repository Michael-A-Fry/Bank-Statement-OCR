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

# Load the engine (all pure-R modules) into the session.
for (.f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(.f)

TEMPLATES_DIR <- "templates"            # curated, team-maintained (default) templates
USER_TEMPLATES_DIR <- "templates_user"  # templates accountants create via guided setup
LOGDIR <- "logs"   # run log + feedback log live together, next to the app
UPLOADS_DIR <- "uploads"  # every uploaded statement + its lifecycle status (git-ignored)
REQUESTS_DIR <- "requests"  # "none of these fits -- tell our team" raises (git-ignored)
FIELDS_DIR <- "fields_templates"            # curated mode:fields (IRD/form) templates
USER_FIELDS_DIR <- "fields_templates_user"  # form templates built in the app
DICT_PATH <- file.path("dictionaries", "labels.yaml")  # the shared label dictionary
# The bundled specimen statement (public sample, ships with the app) that "Try it
# on a sample" converts, so a brand-new user sees a full result without a file.
SAMPLE_STATEMENT <- file.path("samples", "raw", "tutorial", "sample_everyday_statement.pdf")

# Plain-English labels for the everyday screen. The engine's internal codes
# (needs_review, balance_reconciliation, ...) stay in the logs; a non-technical
# user only ever sees these sentences.
STATUS_PLAIN <- c(
  ok           = "Converted successfully",
  needs_review = "Converted - please double-check it",
  unsupported  = "No template for this statement yet",
  failed       = "Could not read this file")
CHECK_PLAIN <- c(
  balance_reconciliation     = "Opening + transactions = closing balance",
  running_balance_continuity = "Each running balance follows from the last",
  transaction_count          = "Row count matches the statement",
  dates_within_period        = "All dates fall in the statement period",
  dates_readable             = "Row dates could be read",
  no_unparsed_rows           = "Every row was read",
  redaction_summary          = "Redactions found and honoured",
  ocr_confidence             = "Scan / OCR read quality")
COVERAGE_PLAIN <- c(populated = "present", partial = "some rows empty",
                    empty = "empty (check the mapping)", unmapped = "not on this statement")
# Diagnostics 'category' codes -> plain words for the customer-facing table
# (the codes themselves stay in the logs / workbook Diagnostics sheet).
DIAG_PLAIN <- c(
  unknown_format          = "layout not recognised",
  unreadable              = "file could not be read",
  multiple_statements     = "several statements in one file",
  combined_statement      = "several accounts in one statement",
  mixed_currency          = "more than one currency",
  oversized               = "unusually large file",
  oversized_page          = "unusually large page",
  reconciliation_mismatch = "balance doesn't reconcile",
  balance_break           = "running balance jumps",
  row_count               = "row count doesn't match",
  date_out_of_range       = "date outside the period",
  date_format_mismatch    = "dates in a different style than expected",
  row_parse               = "rows didn't parse",
  date_parse              = "dates couldn't be read",
  amount_parse            = "amounts couldn't be read",
  completeness_unverified = "completeness not auto-verified",
  low_ocr_confidence      = "scan read with low confidence",
  ocr                     = "page(s) machine-read (OCR)",
  ocr_confidence_unknown  = "scan quality unknown")
plain_status <- function(s) { s <- s %||% "?"; v <- STATUS_PLAIN[s]; if (is.na(v)) toupper(s) else unname(v) }
plain_label  <- function(x, map) { out <- unname(map[x]); ifelse(is.na(out), x, out) }
# Human-readable HEADERS for the transactions preview. The stored core schema uses
# machine names that read as an internal tool to a forensic reviewer, so relabel
# for DISPLAY. The verbatim *_raw cells no longer surface here (they live in the
# JSON + Provenance); debit/credit appear when a statement splits money in / out.
CV_COL_LABELS <- c(
  row_id = "#", date = "Date", date_raw = "Date (as shown)",
  description = "Description", amount = "Amount", amount_raw = "Amount (as shown)",
  debit = "Debit (money out)", credit = "Credit (money in)",
  direction = "In / out", balance = "Balance", balance_raw = "Balance (as shown)",
  particulars = "Particulars", code = "Code", reference = "Reference",
  type = "Type", other_party = "Other party", currency = "Currency", flags = "Flags")
cv_friendly_cols <- function(cols) vapply(cols, function(cn) {
  lab <- CV_COL_LABELS[[cn]]
  if (is.null(lab)) tools::toTitleCase(gsub("_", " ", cn)) else lab
}, character(1), USE.NAMES = FALSE)
# .cols_with_data(df) -- names of columns carrying at least one non-blank value.
# Used to trim always-empty columns (a field this statement doesn't have) from the
# previews so the reviewer sees only what was actually read. row_id is always kept.
.cols_with_data <- function(df, always = "row_id") {
  keep <- vapply(df, function(c) any(!is.na(c) & nzchar(trimws(as.character(c)))), logical(1))
  union(intersect(always, names(df)), names(df)[keep])
}
# The friendly line shown when a file simply can't be read (technical detail -> log).
FRIENDLY_READ_ERROR <- paste(
  "We couldn't read this file. It may be password-protected, an image-only scan we can't open,",
  "or not a bank statement. Try re-saving it as a PDF or CSV, or open the template toolkit to set it up.")

# About-page + tutorial HTML content lives in ui_content.R (readability).
source("ui_content.R")

# ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$title("Statement Studio"),
    # One design system, all inline (the app must work offline / air-gapped:
    # no CDN fonts, scripts or icon packs). The brand accent is NZ-Police blue;
    # green stays reserved for its meaning -- \"checked / money in / OK\".
    tags$style(HTML("
     :root{
       --brand:#00205b; --brand-dark:#001640; --brand-tint:#eaf0f8; --brand-line:#b6c8e0;
       --ink:#1f2a33; --muted:#66727d; --line:#e3e7e5; --panel:#f8faf9;
       --ok:#137333; --bad:#b00020; --warn-ink:#8a5b00; --warn-bg:#fff8e6; --warn-line:#f0c36d;
     }
     body{font-family:'Segoe UI',system-ui,-apple-system,Roboto,'Helvetica Neue',Arial,sans-serif;
          color:var(--ink);font-size:14px}
     h1,h2,h3,h4,h5{font-weight:600;color:var(--ink)}
     h4{font-size:16.5px;margin-top:20px}
     hr{border-top-color:var(--line)}
     .ok{color:var(--ok);font-weight:600}.bad{color:var(--bad);font-weight:600}
     .muted{color:var(--muted)}
     .mono{font-family:Consolas,'Courier New',monospace;white-space:pre-wrap}
     .modal-lg{width:95%;max-width:1240px}
     .modal-content{border-radius:12px}
     /* header */
     .app-header{display:flex;align-items:baseline;flex-wrap:wrap;padding:14px 0 10px}
     .app-header>*{margin-right:12px}
     .app-mark{display:inline-block;width:13px;height:13px;border-radius:4px;background:var(--brand);align-self:center}
     .app-title{font-size:21px;font-weight:700;letter-spacing:.2px}
     .app-tagline{font-size:13px;color:var(--muted)}
     /* tabs: quiet underline style, active = product green */
     .nav-tabs{border-bottom:2px solid var(--line);margin-bottom:4px}
     .nav-tabs>li>a{border:none;border-bottom:3px solid transparent;border-radius:0;
       color:var(--muted);font-weight:600;padding:9px 14px;margin-right:2px}
     .nav-tabs>li>a:hover{background:var(--brand-tint);border:none;
       border-bottom:3px solid var(--brand-line);color:var(--brand)}
     .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus{
       border:none;border-bottom:3px solid var(--brand);color:var(--brand);background:transparent}
     /* buttons. NB: Shiny action/download buttons always carry btn-default even
        when another btn-* class is added, so the btn-default skin must exclude
        those or it beats them on focus (a clicked Convert would turn pale). */
     .btn{border-radius:7px;font-weight:600}
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger){
       border-color:#cfd6d2;color:var(--ink)}
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger):hover,
     .btn-default:not(.btn-primary):not(.btn-warning):not(.btn-danger):focus{
       background:var(--panel);border-color:#b9c2bd;color:var(--ink)}
     .btn-primary{background:var(--brand);border-color:var(--brand-dark);color:#fff}
     .btn-primary:hover,.btn-primary:focus,.btn-primary:active,.btn-primary:active:focus{
       background:var(--brand-dark);border-color:var(--brand-dark);color:#fff}
     .btn-warning{background:#f7c948;border-color:#dfa92e;color:#4a3200}
     .btn-warning:hover,.btn-warning:focus{background:#eeb63c;border-color:#c99518;color:#402b00}
     .btn-danger{background:#fff;border-color:#e3a4ae;color:var(--bad)}
     .btn-danger:hover,.btn-danger:focus{background:#fdecec;border-color:var(--bad);color:var(--bad)}
     /* panels + forms */
     .well{background:var(--panel);border:1px solid var(--line);border-radius:10px;box-shadow:none}
     .form-control{border-radius:7px;border-color:#cfd6d2;box-shadow:none}
     .form-control:focus{border-color:var(--brand);box-shadow:0 0 0 3px rgba(0,32,91,.15)}
     .help-block{color:var(--muted);font-size:12.5px}
     .progress-bar{background-color:var(--brand)}
     /* tables (DT) */
     table.dataTable{font-size:13px}
     table.dataTable thead th,table.dataTable thead td{
       background-color:var(--panel)!important;border-bottom:2px solid var(--line)!important;font-size:12.5px}
     table.dataTable tbody tr:hover{background:#f3f8f4}
     /* collapsed sections */
     details>summary{cursor:pointer}
     /* downloads box (appears in the sidebar once a conversion produced files) */
     .dl-box{background:var(--brand-tint);border:1px solid var(--brand-line);border-radius:10px;
       padding:10px 12px;margin:10px 0}
     .dl-box .btn{margin:3px 6px 3px 0}
     /* small status chips on the result headline */
     .chip{display:inline-block;padding:2px 10px;border-radius:999px;font-size:12px;font-weight:600;
       margin:2px 6px 0 0;background:#f2f4f3;border:1px solid var(--line);color:#4a555f}
     .chip-warn{background:var(--warn-bg);border-color:var(--warn-line);color:var(--warn-ink)}
     .shiny-notification{border-radius:10px;border:1px solid var(--line);
       box-shadow:0 6px 24px rgba(0,0,0,.14);font-size:13.5px}
     /* About hub: the journey entry - two doors and a quiet third */
     .hub{max-width:1020px}
     .hub-lead{font-size:15.5px;color:#3a4652;max-width:780px;line-height:1.55;margin:4px 0 18px}
     .hub-cards{display:flex;flex-wrap:wrap}
     a.hub-card{flex:1 1 260px;max-width:330px;margin:0 14px 14px 0;padding:16px 18px;
       border:1px solid var(--line);border-radius:12px;background:#fff;display:block;
       color:var(--ink);text-decoration:none}
     a.hub-card:hover,a.hub-card:focus{border-color:var(--brand-line);
       box-shadow:0 4px 14px rgba(0,32,91,.12);text-decoration:none;color:var(--ink)}
     a.hub-card-primary{background:var(--brand-tint);border-color:var(--brand-line)}
     a.hub-card-quiet{background:var(--panel)}
     .hub-card-kicker{font-size:11.5px;font-weight:700;letter-spacing:.6px;
       text-transform:uppercase;color:var(--brand);margin-bottom:4px}
     .hub-card-title{font-size:17px;font-weight:700;margin-bottom:6px}
     .hub-card-body{font-size:13px;color:var(--muted);line-height:1.5;margin-bottom:10px}
     .hub-card-go{font-size:13px;font-weight:700;color:var(--brand)}
    ")),
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
    tags$style(HTML("
     @keyframes ss-spin{to{transform:rotate(360deg)}}
     @keyframes ss-fade{from{opacity:0}to{opacity:1}}
     #ss-busy{position:fixed;right:18px;bottom:18px;z-index:2000;display:none;
       align-items:center;gap:9px;background:#fff;border:1px solid var(--line);
       border-left:4px solid var(--brand);border-radius:10px;padding:9px 14px;
       box-shadow:0 6px 22px rgba(0,0,0,.14);font-size:13px;font-weight:600;color:var(--ink)}
     #ss-busy.on{display:flex;animation:ss-fade .15s ease}
     .ss-ring{width:16px;height:16px;border-radius:50%;border:2.5px solid var(--brand-line);
       border-top-color:var(--brand);animation:ss-spin .7s linear infinite}
     /* dim + spinner over any output that is recalculating */
     .shiny-bound-output.recalculating{opacity:.45;transition:opacity .1s}
     .shiny-plot-output.recalculating{position:relative}
     .shiny-plot-output.recalculating::after{content:'';position:absolute;top:50%;left:50%;
       width:34px;height:34px;margin:-17px 0 0 -17px;border-radius:50%;
       border:3px solid var(--brand-line);border-top-color:var(--brand);
       animation:ss-spin .7s linear infinite}
     /* prominent download bar at the top of a result */
     .dl-hero{display:flex;align-items:center;flex-wrap:wrap;gap:10px;
       background:var(--brand-tint);border:1px solid var(--brand-line);border-radius:10px;
       padding:12px 16px;margin:2px 0 14px}
     .dl-hero .dl-hero-label{font-weight:700;color:var(--brand);margin-right:4px}
     .dl-hero .btn{font-size:15px;padding:8px 18px}
    ")),
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
      })();"))
  ),
  div(class = "app-header",
    span(class = "app-mark"),
    span(class = "app-title", "Statement Studio"),
    span(class = "app-tagline", "Statements and documents in - clean, checked data out.")),
  tabsetPanel(
    id = "main_tabs",
    # ---- About (landing): the journey hub. Everything starts here - one
    # promise, then the two doors (convert / teach), then the proof story.
    tabPanel("About", br(),
      div(class = "hub",
        div(class = "hub-lead",
          "Turn any bank statement or financial document - PDF, CSV or Excel -",
          " into clean, checked data. Deterministic: nothing is guessed,",
          " and anything uncertain is flagged with the reason."),
        div(class = "hub-cards",
          actionLink("ab_go_convert", class = "hub-card hub-card-primary", label = div(
            div(class = "hub-card-kicker", "Most days"),
            div(class = "hub-card-title", "Convert a statement"),
            div(class = "hub-card-body",
                "Upload your bank's export, click Convert. You get the verdict, the analysis, every transaction, and the download."),
            div(class = "hub-card-go", "Open Convert →"))),
          actionLink("ab_go_template", class = "hub-card", label = div(
            div(class = "hub-card-kicker", "New bank or document"),
            div(class = "hub-card-title", "Teach it a template"),
            div(class = "hub-card-body",
                "A new statement layout, or any other document - a form, a summary, a letter. The toolkit pre-fills what it can detect; you confirm against a live preview and save. About 2 minutes, no code."),
            div(class = "hub-card-go", "Open Add a template →"))),
          actionLink("ab_go_admin", class = "hub-card hub-card-quiet", label = div(
            div(class = "hub-card-kicker", "Looking after the tool"),
            div(class = "hub-card-title", "Admin"),
            div(class = "hub-card-body",
                "Team insights from every run, template management, batch audits and folder intake."),
            div(class = "hub-card-go", "Open Admin →"))))),
      about_html()),
    # ---- Convert -------------------------------------------------------
    tabPanel(
      "Convert",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          fileInput("cv_file", "Statement file (.pdf / .csv / .tsv / .xlsx)",
                    accept = c(".pdf", ".csv", ".tsv", ".tdv", ".xlsx")),
          textInput("cv_by", "Your name / initials (for the audit trail)", value = ""),
          uiOutput("cv_who_hint"),
          uiOutput("cv_bank_ui"),
          actionButton("cv_go", "Convert", class = "btn-primary btn-lg btn-block"),
          br(),
          helpText("Detection is automatic; pick a bank only to force one."),
          tags$hr(),
          uiOutput("cv_templates")
        ),
        mainPanel(
          width = 8,
          uiOutput("cv_status"),
          uiOutput("cv_headline"),   # plain verdict first (self-gates to an OK statement)
          uiOutput("cv_downloads"),  # prominent download bar, right under the verdict
          uiOutput("cv_rematch"),    # "wrong bank / template? make the right one" - up top, obvious
          # Form / labelled-value PDF result (renders only when kind == "form").
          uiOutput("cv_form"),
          # Before any conversion, show a clear empty state (what this page does /
          # what you'll get) instead of bare section headers over empty tables.
          conditionalPanel("output.cv_has_result != true", uiOutput("cv_empty")),
          # Transaction-statement result: EASY first. Lead with a plain-English
          # verdict (did it work? how many transactions? can I trust it?), then the
          # transactions themselves, then the DOWNLOAD. The technical checks,
          # diagnostics and field-coverage tables live inside a collapsed "Checks &
          # detail" section for whoever wants to dig in - depth as an option, not a
          # wall in front of a first-time user.
          conditionalPanel("output.cv_has_result == true && output.cv_is_form != true",
            # Analysis + transactions render only when the parse actually produced
            # rows (ok / needs_review): an unsupported or failed result must never
            # show zero-money cards and an empty graph under its honest verdict.
            conditionalPanel("output.cv_has_txns == true",
            uiOutput("cv_summary"),
            div(style = "border:1px solid #e3e3e3;border-radius:8px;padding:10px 14px;margin:6px 0 14px",
              fluidRow(
                column(4, selectInput("an_view", "Show",
                  c("Money in vs out" = "inout", "Balance over time" = "balance",
                    "Cumulative net" = "cumnet"), width = "100%")),
                column(4, selectInput("an_group", "Group by",
                  c("Day" = "day", "Week" = "week", "Month" = "month"),
                  selected = "week", width = "100%")),
                column(4, radioButtons("an_unit", "Measure",
                  c("Dollars" = "amount", "Count" = "count"), inline = TRUE))),
              plotOutput("cv_trend", height = "270px"),
              uiOutput("cv_trend_note")),
            h4("Your transactions"),
            tabsetPanel(
              tabPanel("Preview", br(), DTOutput("cv_txns")),
              tabPanel("See it on the page (X-ray)", br(),
                conditionalPanel("output.ix_is_pdf == true",
                  p(class = "muted", "Your statement page, with everything the tool read drawn on it. Green = kept transaction rows; amber dashed = skipped rows that look like transactions; the legend below names the rest."),
                  fluidRow(
                    column(3, numericInput("ix_page", "Page", 1, min = 1, step = 1)),
                    column(9, br(),
                      checkboxGroupInput("ix_layers", "Show on the page (untick to hide a layer):",
                        choices = c("Column bands" = "cols", "Kept transaction rows" = "kept",
                                    "Skipped rows" = "skipped", "Redactions" = "redact",
                                    "Balances / dates / account" = "meta",
                                    "Faint box on every word" = "words"),
                        selected = c("cols", "kept", "skipped", "redact", "meta", "words"),
                        inline = TRUE))),
                  plotOutput("ix_plot", height = "640px"),
                  uiOutput("ix_legend"),
                  h4("Rows skipped on this page - and why"),
                  helpText(HTML("A real transaction in here usually means a one-line template fix (most often the <b>date format</b> or an amount band) - that brings back every row like it. A genuine one-off? Select it and add it by hand; it's kept, flagged <b>forced</b>.")),
                  DTOutput("ix_skipped"),
                  br(),
                  actionButton("ix_add_row", "This IS a transaction - add the selected row", class = "btn-warning"),
                  tags$hr(),
                  helpText("Still stuck and can't share the statement? The diagnostic below uses only page sizes and counts - no dates, names or amounts leave this machine."),
                  downloadButton("ix_coverage_dl", "Download shareable diagnostic (no statement contents)")),
                conditionalPanel("output.ix_is_pdf != true",
                  helpText("The X-ray view is for PDF statements. For CSV / Excel, the field coverage inside 'Checks & detail' below shows which column feeds each field."))))),
            # Detection / "wrong template?" and the tweak-in-toolkit prompt: useful,
            # but AFTER the data, not before the verdict.
            uiOutput("cv_teach"),
            uiOutput("cv_candidates"),
            tags$details(style = "margin-top:14px",
              tags$summary(style = "cursor:pointer;font-weight:600;color:var(--brand)",
                           "Checks & detail (for review)"),
              div(style = "padding:8px 2px",
                h4("Checks"), DTOutput("cv_kpis"),
                h4("Diagnostics - where / why / how to fix"), DTOutput("cv_diag"),
                h4("Field coverage - what's present / empty / not on this statement"),
                uiOutput("cv_cov_summary"), DTOutput("cv_coverage")))),
          uiOutput("cv_feedback")
        )
      )
    ),
    # ---- Add a template (one toolkit for statements + a form builder) --
    tabPanel(
      "Add a template",
      br(),
      wellPanel(
        strong("Add a template"),
        p(class = "muted", "Upload the document and set it up with it on screen the whole time. Simple covers the common case; Advanced (full field-by-field / YAML) is one click away inside."),
        p(actionLink("ts_help", "New here? Read the 2-minute guide - the ways statements differ and what each setting means")),
        fileInput("ts_file", "Document file (.csv / .tsv / .tdv / .pdf / .xlsx)",
                  accept = c(".csv", ".tsv", ".tdv", ".pdf", ".xlsx")),
        radioButtons("ts_doctype", "What kind of document is this?",
          c("A bank or card statement - a table of transactions" = "statement",
            "Something else - labelled values (an IRD form, an account summary, a letter)" = "other"),
          selected = "statement"),
        conditionalPanel("input.ts_doctype == 'statement'",
          actionButton("ts_go", "Open the toolkit", class = "btn-primary")),
        conditionalPanel("input.ts_doctype == 'other'",
          div(style = "padding:10px 12px;background:#fffbe9;border:1px solid #f0c36d;border-radius:8px;margin-top:4px",
            strong("Heads up - an 'Other' document is read differently"),
            p(style = "margin:6px 0 0;color:#555",
              "It has no transaction table or running balance, so the completeness checks don't apply: the tool pulls the labelled values you name, and you eyeball each one. Set it up with the builder below.")))),
      # One flow: the toolkit above is THE way to add a statement template (its
      # Advanced tab covers field-by-field / YAML editing, so there is no separate
      # "build by hand" path). 'Other' documents (labelled values) are set up with
      # this builder, shown right here when "Something else" is picked above.
      conditionalPanel("input.ts_doctype == 'other'",
      br(),
      helpText("Teach the tool which labelled values to pull from a non-statement PDF (an IRD form, a summary, a letter). When a value sits far from its label, draw a box to say exactly where."),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          textInput("fb_id", "Template id", "newpdf_fields"),
          textInput("fb_bank", "Bank / issuer", "NewIssuer"),
          textInput("fb_type", "Document type", "summary"),
          textAreaInput("fb_fp", "Identifying phrases (one per line - text that appears on this PDF)",
                        rows = 3, value = "KiwiSaver\nOpening balance"),
          textAreaInput("fb_fields",
                        "Values found NEAR their label - one per line:  field_name = Label; Other label | money",
                        rows = 6,
                        value = paste("opening_balance = Opening balance; Balance brought forward | money",
                                      "closing_balance = Closing balance | money", sep = "\n")),
          tags$hr(),
          strong("Value in a different place than its label?"),
          helpText("Upload a sample, draw a box, name the field, click Set - the value reads from that box."),
          fileInput("fb_sample", "Sample PDF to test / draw on (.pdf)", accept = ".pdf"),
          fluidRow(
            column(6, textInput("fb_rf_field", "Field name", "")),
            column(6, selectInput("fb_rf_type", "Value type",
                                  c("money", "date", "date_range", "text")))),
          fluidRow(
            column(4, numericInput("fb_rf_page", "Page", 1, min = 1, step = 1)),
            column(8, br(),
                   actionButton("fb_rf_set", "Set value box", class = "btn-primary"),
                   actionButton("fb_rf_clear", "Clear boxes"))),
          tags$hr(),
          actionButton("fb_preview", "Preview on the sample"),
          actionButton("fb_save", "Save template", class = "btn-primary"),
          br(), br(), uiOutput("fb_msg")),
        mainPanel(
          width = 8,
          helpText("Label value types: money, date, date_range, text (default). A field whose name matches the shared dictionary inherits its synonyms automatically."),
          conditionalPanel("output.fb_has_sample == true",
            h4("Draw a box to place a value (optional)"),
            plotOutput("fb_plot", brush = brushOpts("fb_brush", direction = "xy"), height = "540px"),
            tableOutput("fb_regions_tbl")),
          h4("Live preview (needs a sample)"), verbatimTextOutput("fb_prev_status"), DTOutput("fb_prev_tbl"),
          h4("Generated template (YAML)"), div(class = "mono", verbatimTextOutput("fb_yaml"))))
    )
    ),
    # ---- Admin (insights + batch intake) ------------------------------
    tabPanel(
      "Admin",
      br(),
      conditionalPanel("!output.admin_authed",
        wellPanel(style = "max-width:440px",
          h4("Admin - password required"),
          passwordInput("adm_pw", "Password"),
          actionButton("adm_login", "Enter", class = "btn-primary"),
          uiOutput("adm_login_msg"))),
      conditionalPanel("output.admin_authed",
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
              downloadButton("adm_up_audit", "Download its safe summary (no personal data)"),
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
              actionButton("adm_req_dismiss", "Dismiss"))),
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
              downloadButton("adm_inbox_audit", "Download its safe summary (no personal data)"))),
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
          helpText("Each row is one layout the tool doesn't recognise yet (identical layouts are grouped). The biggest count is the one to build a template for first - it unblocks the most statements."),
          DTOutput("adm_gaps"),
          h4("Templates that started failing recently"),
          helpText("A statement's layout can change slightly - a field moves or gets renamed - and stop adding up. When that happens the tool flags the conversion for a check, and any template that's suddenly getting more of those shows here. Empty is good."),
          DTOutput("adm_drift"),
          h4("Template usage"),
          DTOutput("adm_usage"),
          br(),
          actionButton("adm_rollup", "Tidy up logs (archive runs older than 90 days)"),
          uiOutput("adm_rollup_msg")
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
          helpText(HTML("The usual reason a value comes up <b>blank</b>: the statement uses a wording the tool hasn't seen. Add the exact phrases your statements use and Save - it applies straight away.")),
          fluidRow(
            column(5,
              actionButton("adm_dict_reload", "Reload from file"),
              actionButton("adm_dict_save", "Save dictionary", class = "btn-primary"),
              br(), br(), uiOutput("adm_dict_msg")),
            column(7,
              textAreaInput("adm_dict_edit", NULL, value = "", width = "100%", height = "360px")))
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
              downloadButton("adm_ba_report", "Safe audit report (.md)"),
              br(), br(),
              downloadButton("adm_ba_csv", "Converted report (.csv)"),
              br(), br(),
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
          downloadButton("adm_audit_dl", "Download safe audit (.md)")
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
    title = "Building a template - the 2-minute guide", size = "l", easyClose = TRUE,
    tutorial_html(), footer = modalButton("Close")))
  # (Only from the tab, not from inside the toolkit modal: Shiny shows one modal
  # at a time, so opening the guide there would close the toolkit mid-edit.)
  observeEvent(input$ts_help, show_tutorial())

  tpl_bump <- reactiveVal(0)   # bump to force a reload after a save
  # Active set: hidden user templates are excluded, so they take no part in
  # detection / conversion / the Convert picker.
  templates <- reactive({ tpl_bump(); load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR) })
  # Management set: EVERYTHING, including hidden, so Admin can preview and un-hide.
  all_templates <- reactive({ tpl_bump(); load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR, include_hidden = TRUE) })

  # ---- Admin password gate. Hidden outputs are suspended, so no admin data is
  # computed or sent to the browser until the password is entered. Set it with
  # the BSO_ADMIN_PASSWORD env var (a default is used only for local dev).
  admin_ok <- reactiveVal(FALSE)
  output$admin_authed <- reactive(isTRUE(admin_ok()))
  outputOptions(output, "admin_authed", suspendWhenHidden = FALSE)
  observeEvent(input$adm_login, {
    pw <- Sys.getenv("BSO_ADMIN_PASSWORD", "changeme")
    if (identical(input$adm_pw %||% "", pw)) {
      admin_ok(TRUE); output$adm_login_msg <- renderUI(NULL)
    } else output$adm_login_msg <- renderUI(
      div(style = "color:#b00020;margin-top:6px", "Wrong password."))
  })

  output$cv_bank_ui <- renderUI({
    ts <- templates()
    banks <- sort(unique(vapply(ts, function(t) t$bank %||% "", character(1))))
    # Template picker: labelled "Bank · type - id" so you can force an EXACT
    # template, not just a bank, when you need to be specific.
    ov <- template_overview(ts)
    tpl_choices <- c("(auto-detect)" = "")
    if (nrow(ov)) tpl_choices <- c(tpl_choices,
      stats::setNames(ov$id, sprintf("%s · %s - %s", ov$bank, ov$type, ov$id)))
    tagList(
      selectInput("cv_bank", "Bank (optional)", c("(auto-detect)", banks)),
      selectInput("cv_template", "…or force an exact template (optional)", choices = tpl_choices))
  })

  # ---- Which templates are available (Convert visibility) ----
  output$cv_templates <- renderUI({
    ov <- template_overview(templates())
    nt <- sum(ov$origin == "tested"); nu <- sum(ov$origin == "user")
    lines <- sprintf("%s &middot; %s &middot; <i>%s</i> (%s)", ov$bank, ov$type, ov$format, ov$origin)
    tags$details(
      tags$summary(sprintf("Templates available: %d  (%d tested, %d user)", nrow(ov), nt, nu)),
      tags$div(style = "font-size:11px;color:#888;margin-top:4px",
               HTML("<b>tested</b> = shipped with the tool; <b>user</b> = set up on this machine.")),
      tags$div(style = "font-size:12px;color:#555;margin-top:6px;max-height:220px;overflow:auto",
               HTML(paste(lines, collapse = "<br>"))),
      tags$div(style = "font-size:11px;color:#888;margin-top:6px",
               "Full detail and editing in Admin → Templates."))
  })

  # ---- Admin: template overview / preview / edit ----
  # The management view shows ALL templates, hidden ones included, so a parked
  # draft can be found and un-hidden.
  output$adm_tpl_overview <- renderDT(
    template_overview(all_templates()),
    options = list(pageLength = 25, dom = "tip"), rownames = FALSE, selection = "single")

  observe(updateSelectInput(session, "adm_tpl_pick", choices = sort(names(all_templates()))))

  # clicking a row selects it in the picker
  observeEvent(input$adm_tpl_overview_rows_selected, {
    ov <- template_overview(all_templates())
    i <- input$adm_tpl_overview_rows_selected
    if (length(i) && i <= nrow(ov)) updateSelectInput(session, "adm_tpl_pick", selected = ov$id[i])
  })

  observeEvent(input$adm_tpl_pick, {
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
  observeEvent(input$adm_tpl_delete, {
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
    } else output$adm_tpl_msg <- .tpl_note("Couldn't delete it.", ok = FALSE)
  })

  # Duplicate the selected template with a fresh id, into the editor to tweak+save.
  observeEvent(input$adm_tpl_dup, {
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
    t <- .tpl_from_editor()
    if (is.null(t)) { output$adm_tpl_msg <- .tpl_note("That is not valid YAML.", FALSE); return() }
    probs <- validate_template(t)
    output$adm_tpl_msg <- if (!length(probs)) .tpl_note("Valid ✓")
      else .tpl_note(paste("Problems:<br>", paste(probs, collapse = "<br>")), FALSE)
  })

  observeEvent(input$adm_tpl_save, {
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

  # ---- Admin: label dictionary edit (the fix for "check shows NA") ----
  .load_dict_text <- function()
    if (file.exists(DICT_PATH)) paste(readLines(DICT_PATH, warn = FALSE), collapse = "\n") else ""
  observeEvent(admin_ok(), if (isTRUE(admin_ok()))
    updateTextAreaInput(session, "adm_dict_edit", value = .load_dict_text()))
  observeEvent(input$adm_dict_reload,
    updateTextAreaInput(session, "adm_dict_edit", value = .load_dict_text()))
  observeEvent(input$adm_dict_save, {
    txt <- input$adm_dict_edit %||% ""
    if (!isTRUE(tryCatch({ yaml::yaml.load(txt); TRUE }, error = function(e) FALSE))) {
      output$adm_dict_msg <- renderUI(div(style = "color:#b00020", "Not valid YAML - not saved."))
      return()
    }
    safe(file.copy(DICT_PATH, paste0(DICT_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({ writeLines(txt, DICT_PATH); TRUE }, error = function(e) FALSE))
    output$adm_dict_msg <- renderUI(div(style = sprintf("color:%s", if (okw) "#137333" else "#b00020"),
      if (okw) "Saved (backup at labels.yaml.bak). New wordings apply to the next conversion."
      else "Could not write the file - check folder permissions."))
  })

  # ---- Admin: bulk audit & gaps ----
  adm_ba <- reactiveVal(NULL)
  adm_ba_conv <- reactiveVal(NULL)   # converted-report rows, when "convert & save" was ticked
  observeEvent(input$adm_ba_run, {
    if (is.null(input$adm_ba_files)) {
      showNotification("Upload some statements first, then click Run.",
                       type = "warning", duration = 6)
      return()
    }
    fs <- input$adm_ba_files
    sess <- file.path(tempdir(), paste0("ba_", as.integer(runif(1, 1, 1e9)))); dir.create(sess, showWarnings = FALSE)
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
    b <- adm_ba(); req(b); if (!nrow(b$clusters)) return(data.frame(note = "no gaps - everything parsed"))
    b$clusters[, c("count", "kind", "layout_hint", "signature")]
  }, options = list(pageLength = 10, dom = "tp"), rownames = FALSE)
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
  output$adm_ba_report <- downloadHandler(
    filename = function() "bulk-audit.md",
    content = function(file) {
      b <- adm_ba()
      if (is.null(b)) { showNotification("Run a bulk audit first - nothing to download yet.",
                                         type = "warning", duration = 6); req(FALSE) }
      writeLines(format_batch_audit(b), file) })
  output$adm_ba_csv <- downloadHandler(
    filename = function() "batch_converted.csv",
    content = function(file) {
      conv <- adm_ba_conv()
      if (is.null(conv)) { showNotification("Tick 'Also convert & save' and run first - no converted report yet.",
                                            type = "warning", duration = 6); req(FALSE) }
      utils::write.csv(conv, file, row.names = FALSE) })
  output$adm_audit_dl <- downloadHandler(
    filename = function() "statement.audit.md",
    content = function(file) {
      if (is.null(input$adm_audit_one)) { showNotification("Upload a statement to audit first.",
                                          type = "warning", duration = 6); req(FALSE) }
      writeLines(format_audit(statement_audit(input$adm_audit_one$datapath, templates = templates())), file) })

  # ---- Admin: uploads & pickups ----
  output$adm_uploads <- renderDT({
    cv_upload_id(); input$adm_refresh          # refresh after a convert or on demand
    u <- read_uploads(UPLOADS_DIR)
    if (!nrow(u)) return(data.frame(note = "no uploads yet"))
    u[, c("ts", "file_ext", "status", "template", "trust", "needs_pickup", "run_id")]
  }, options = list(pageLength = 8, dom = "tip"), rownames = FALSE)
  observe({
    cv_upload_id(); input$adm_refresh
    u <- read_uploads(UPLOADS_DIR)
    updateSelectInput(session, "adm_up_pick",
      choices = if (nrow(u)) u$id[u$needs_pickup] else character(0))
  })
  output$adm_up_audit <- downloadHandler(
    filename = function() "upload.audit.md",
    content = function(file) {
      id <- input$adm_up_pick
      p <- if (!is.null(id) && nzchar(id)) upload_file_path(id, UPLOADS_DIR) else NA_character_
      writeLines(format_audit(statement_audit(need_file(p), templates = templates())), file)
    })

  # ---- Admin: format requests raised via the "tell our team" escape hatch ----
  req_bump <- reactiveVal(0)   # bump to refresh after a triage action
  output$adm_requests <- renderDT({
    req_bump(); input$adm_refresh
    q <- read_template_requests(REQUESTS_DIR)
    if (!nrow(q)) return(data.frame(note = "no format requests raised yet"))
    q[, c("ts", "requested_by", "status", "detail", "context")]
  }, options = list(pageLength = 6, dom = "tip"), rownames = FALSE)
  observe({
    req_bump(); input$adm_refresh
    q <- read_template_requests(REQUESTS_DIR)
    open <- if (nrow(q)) q$id[q$status == "open"] else character(0)
    updateSelectInput(session, "adm_req_pick", choices = open)
  })
  observeEvent(input$adm_req_actioned, {
    id <- input$adm_req_pick; req(id, nzchar(id))
    if (isTRUE(set_request_status(id, "actioned", dir = REQUESTS_DIR))) req_bump(req_bump() + 1)
  })
  observeEvent(input$adm_req_dismiss, {
    id <- input$adm_req_pick; req(id, nzchar(id))
    if (isTRUE(set_request_status(id, "dismissed", dir = REQUESTS_DIR))) req_bump(req_bump() + 1)
  })

  # ---- Admin: folder-intake browser (inbox / processed / failed / outbox) ----
  inbox_state <- reactive({ input$adm_refresh; cv_upload_id(); inbox_status(".") })
  output$adm_inbox_counts <- renderUI({
    s <- inbox_state(); c <- s$counts
    p(class = "muted", HTML(sprintf(
      "Waiting: <b>%d</b> &nbsp;|&nbsp; Processed: <b>%d</b> &nbsp;|&nbsp; Failed: <b>%d</b> &nbsp;|&nbsp; Stuck: <b>%d</b> &nbsp;|&nbsp; Output folders: <b>%d</b>",
      c[["inbox"]], c[["processed"]], c[["failed"]], c[["stuck"]], c[["outbox"]])))
  })
  inbox_tbl <- function(which) renderDT({
    d <- inbox_state()$folders[[which]]
    if (!nrow(d)) return(data.frame(note = "empty"))
    d
  }, options = list(pageLength = 6, dom = "tip"), rownames = FALSE)
  output$adm_inbox_failed    <- inbox_tbl("failed")
  output$adm_inbox_waiting   <- inbox_tbl("inbox")
  output$adm_inbox_processed <- inbox_tbl("processed")
  output$adm_inbox_outbox    <- inbox_tbl("outbox")
  observe({
    s <- inbox_state()
    updateSelectInput(session, "adm_inbox_pick",
      choices = if (nrow(s$folders$failed)) s$folders$failed$file else character(0))
  })
  observeEvent(input$adm_inbox_wizard, {
    nm <- input$adm_inbox_pick
    if (is.null(nm) || !nzchar(nm)) { showNotification("Pick a failed file first.", type = "warning"); return() }
    p <- failed_file_path(nm, ".")
    if (is.na(p)) { showNotification("That file is no longer in failed/.", type = "error"); return() }
    open_guided(p, nm)
  })
  output$adm_inbox_audit <- downloadHandler(
    filename = function() paste0(input$adm_inbox_pick %||% "file", ".audit.md"),
    content = function(file) {
      nm <- input$adm_inbox_pick
      p <- if (!is.null(nm) && nzchar(nm)) failed_file_path(nm, ".") else NA_character_
      writeLines(format_audit(statement_audit(need_file(p), templates = templates())), file)
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
  # Positional value boxes drawn on the sample: field -> list(page,x_min..y_max,value).
  fb_regions <- reactiveVal(list())
  output$fb_has_sample <- reactive({ !is.null(input$fb_sample) })
  outputOptions(output, "fb_has_sample", suspendWhenHidden = FALSE)
  observeEvent(input$fb_rf_set, {
    nm <- gsub("[^A-Za-z0-9_]+", "_", trimws(input$fb_rf_field %||% ""))
    br <- input$fb_brush
    if (!nzchar(nm)) { showNotification("Name the field first.", type = "warning"); return() }
    if (is.null(br)) { showNotification("Draw a box on the page around the value first.", type = "warning"); return() }
    r <- fb_regions()
    r[[nm]] <- list(page = max(1L, as.integer(input$fb_rf_page %||% 1)),
                    x_min = round(br$xmin), x_max = round(br$xmax),
                    y_min = round(br$ymin), y_max = round(br$ymax),
                    value = input$fb_rf_type %||% "text")
    fb_regions(r)
    showNotification(sprintf("Placed the value box for '%s'.", nm), type = "message")
  })
  observeEvent(input$fb_rf_clear, fb_regions(list()))
  output$fb_regions_tbl <- renderTable({
    r <- fb_regions(); if (!length(r)) return(NULL)
    do.call(rbind, lapply(names(r), function(nm) data.frame(field = nm, page = r[[nm]]$page,
      box = sprintf("x %d-%d, y %d-%d", r[[nm]]$x_min, r[[nm]]$x_max, r[[nm]]$y_min, r[[nm]]$y_max),
      value = r[[nm]]$value, stringsAsFactors = FALSE)))
  })
  fb_render <- reactive({
    req(input$fb_sample); pg <- max(1L, as.integer(input$fb_rf_page %||% 1))
    sz <- tryCatch(pdftools::pdf_pagesize(input$fb_sample$datapath), error = function(e) NULL)
    if (is.null(sz) || pg > nrow(sz)) return(NULL)
    ras <- tryCatch(as.raster(magick::image_read(
      pdftools::pdf_render_page(input$fb_sample$datapath, page = pg, dpi = 100))), error = function(e) NULL)
    if (is.null(ras)) return(NULL)
    list(ras = ras, w = sz$width[pg], h = sz$height[pg], pg = pg)
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
    # Merge the drawn value boxes: a positional field reads its value from the box,
    # regardless of where (or whether) a label appears.
    for (nm in names(fb_regions())) { b <- fb_regions()[[nm]]
      flds[[nm]] <- list(region = list(page = b$page, x_min = b$x_min, x_max = b$x_max,
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
    if (is.null(input$fb_sample)) { showNotification("Upload a sample PDF to preview on.", type = "warning"); return() }
    inp <- tryCatch(read_input(input$fb_sample$datapath), error = function(e) NULL)
    if (is.null(inp)) { showNotification("Couldn't read that file.", type = "error"); return() }
    f <- tryCatch(extract_fields(inp, fb_template()), error = function(e) NULL)
    fb_preview(f)
  })
  output$fb_prev_status <- renderText({
    f <- fb_preview()
    if (is.null(f)) return("Upload a sample and click Preview to see what the labels pull out.")
    sprintf("%d field(s); %d matched.", nrow(f), sum(f$matched))
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
    output$fb_msg <- renderUI(if (isTRUE(ok))
      span(class = "ok", sprintf("Saved '%s'. Now upload the document on the Convert tab - it's detected automatically.", t$id))
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
    meta <- tryCatch(extract_metadata(inp), error = function(e) NULL)
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
  output$ix_is_pdf <- reactive({ st <- ix_state(); isTRUE(st$is_pdf) })
  outputOptions(output, "ix_is_pdf", suspendWhenHidden = FALSE)

  ix_pal <- function(bands) {
    if (!length(bands)) return(character(0))
    stats::setNames(grDevices::hcl(seq(5, 320, length.out = length(bands)), 75, 50), names(bands))
  }
  output$ix_legend <- renderUI({
    st <- ix_state(); req(st, st$is_pdf, !is.null(st$layout))
    pg <- as.character(max(1L, as.integer(input$ix_page %||% 1)))
    P <- st$layout$pages[[pg]]; req(P)
    pal <- ix_pal(P$bands)
    layers <- input$ix_layers %||% c("cols", "kept", "skipped", "redact", "meta", "words")
    sw <- function(col, lab) tags$div(style = "margin:2px 0",
      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;border:2px solid %s;margin-right:6px;vertical-align:middle", col)),
      tags$span(lab))
    has_ocr <- !is.null(P$words$ocr_conf) && any(!is.na(P$words$ocr_conf))
    # Only name the layers currently shown, so the legend tracks the tick-boxes.
    tagList(strong("Legend"),
      if ("cols" %in% layers) lapply(names(pal), function(nm) sw(pal[[nm]], nm)),
      if ("kept" %in% layers) sw("#137333", "transaction row (kept)"),
      if ("skipped" %in% layers) sw("#c77700", "skipped row that looks like a transaction"),
      if ("meta" %in% layers) sw("#a15c00", "balance / account details"),
      if ("redact" %in% layers) sw("#b00020", "redaction (not read)"),
      if (has_ocr) sw("#c77700", "shaded amber = machine-read word the tool is unsure about - double-check it"))
  })
  ix_render <- reactive({
    st <- ix_state(); req(st, st$is_pdf)
    pg <- max(1L, as.integer(input$ix_page %||% 1))
    sz <- tryCatch(pdftools::pdf_pagesize(st$path), error = function(e) NULL)
    if (is.null(sz) || pg > nrow(sz)) return(NULL)
    ras <- tryCatch(as.raster(magick::image_read(
      pdftools::pdf_render_page(st$path, page = pg, dpi = 100))), error = function(e) NULL)
    if (is.null(ras)) return(NULL)
    list(ras = ras, w = sz$width[pg], h = sz$height[pg], pg = pg)
  })
  output$ix_plot <- renderPlot({
    st <- ix_state(); req(st, st$is_pdf); r <- ix_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    lay <- st$layout; if (is.null(lay)) return(invisible())
    P <- lay$pages[[as.character(r$pg)]]; if (is.null(P)) return(invisible())
    layers <- input$ix_layers %||% c("cols", "kept", "skipped", "redact", "meta", "words")
    reg <- P$region; ytop <- reg$y_min %||% 0; ybot <- reg$y_max %||% r$h
    pal <- ix_pal(P$bands)
    if (!is.null(reg$x_min)) rect(reg$x_min, ybot, reg$x_max %||% r$w, ytop,
                                  border = "#666", lty = 2, lwd = 1.4)
    w <- P$words
    if ("words" %in% layers && nrow(w))
      rect(w$x, w$y, w$x + w$width, w$y + w$height, border = "#cfcfcf", lwd = 0.4)
    # Machine-read (OCR) words the engine itself is unsure about: shaded amber so
    # "double-check the numbers" points at exactly the doubtful words. Same floor
    # (60) as the engine's ocr_low_conf row flag.
    if (!is.null(w$ocr_conf)) {
      lc <- w[!is.na(w$ocr_conf) & w$ocr_conf < 60, , drop = FALSE]
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
        text((b$x_min + b$x_max) / 2, ytop, nm, col = pal[[nm]], font = 2, cex = 0.9, pos = 3, offset = 0.2)
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
    pg <- max(1L, as.integer(input$ix_page %||% 1))
    P <- lay$pages[[as.character(pg)]]
    if (is.null(P) || is.null(P$rows) || !nrow(P$rows) || is.null(P$rows$reason)) return(NULL)
    sk <- P$rows[!P$rows$kept & nzchar(P$rows$reason %||% ""), , drop = FALSE]
    if (!nrow(sk)) return(sk)
    actionable <- grepl("didn't parse|no amount", sk$reason)
    sk[order(!actionable), , drop = FALSE]           # likely-missed transactions first
  })
  output$ix_skipped <- renderDT({
    sk <- ix_skipped_rows()
    if (is.null(sk)) return(datatable(data.frame(message = "Nothing to show for this page yet."),
                                      rownames = FALSE, options = list(dom = "t")))
    if (!nrow(sk)) return(datatable(
      data.frame(message = "Every row on this page was either kept or is a heading / footer."),
      rownames = FALSE, options = list(dom = "t")))
    trunc <- function(s, n = 90) ifelse(nchar(s) > n, paste0(substr(s, 1, n), "…"), s)
    out <- data.frame(
      `what's on the row` = trunc(sk$raw %||% ""),
      `date cell` = sk$date %||% NA_character_,
      `why it was skipped` = sk$reason,
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(out, rownames = FALSE, selection = "single",
              options = list(pageLength = 10, dom = "tp", scrollX = TRUE))
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
      if (is.null(src) || is.null(tmpl)) {
        showNotification("Convert a PDF statement first - nothing to diagnose yet.", type = "warning", duration = 6); req(FALSE) }
      inp <- tryCatch(read_input(src$path), error = function(e) NULL)
      if (is.null(inp)) { showNotification("Couldn't re-read the file for the diagnostic.", type = "error"); req(FALSE) }
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
    pg <- max(1L, as.integer(input$ix_page %||% 1))
    band <- list(page = pg, y_min = row$y0 - 1, y_max = row$y1 + 1)
    cur <- cv_forced(); cur[[length(cur) + 1L]] <- band; cv_forced(cur)
    src <- cv_src(); sess <- cv_dir()
    if (is.null(src) || is.null(sess)) {
      showNotification("Convert a statement first.", type = "warning"); return() }
    cv_res(convert_now(src$path, sess, forced_rows = cv_forced()))
    showNotification("Added that row as a transaction (flagged 'forced') and re-checked the statement.",
                     type = "message", duration = 6)
  })
  # Remediate a stuck upload right here: load the saved file into the SAME guided
  # toolkit the Convert tab uses, so a failed/abandoned statement is a 2-second
  # pickup - identify it in the table (A), open it, teach the tool, save (B).
  observeEvent(input$adm_up_wizard, {
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

  cv_res <- reactiveVal(NULL)
  cv_dir <- reactiveVal(NULL)
  cv_src <- reactiveVal(NULL)      # the uploaded file (path + name), for guided setup
  cv_fb_done <- reactiveVal(FALSE)
  cv_upload_id <- reactiveVal(NA_character_)   # the tracked upload for this conversion
  cv_forced <- reactiveVal(list())             # user-confirmed "this IS a transaction" bands

  # convert_now(src, sess, forced_rows) -- run the one front-door conversion with a
  # progress bar, reading bank / exact-template from the current inputs. Shared by
  # the Convert button and the X-ray "add this row" action so both paths behave
  # identically (the second just adds force_rows and reuses the same session dir).
  convert_now <- function(src, sess, forced_rows = NULL, force_tpl = NULL) {
    bank <- if (is.null(input$cv_bank) || input$cv_bank == "(auto-detect)") NULL else input$cv_bank
    forced_tpl <- force_tpl %||%
      (if (!is.null(input$cv_template) && nzchar(input$cv_template)) input$cv_template else NULL)
    who <- who_now()
    withProgress(message = "Converting statement…", value = 0.2, {
      incProgress(0.2, detail = "Reading the file and detecting its format…")
      out <- tryCatch(
        convert_document(src, bank = bank, outdir = sess,
                         templates_dir = TEMPLATES_DIR, user_templates_dir = USER_TEMPLATES_DIR,
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

  # detected_identity() -- who the environment says is accessing, without anyone
  # typing anything. Order of trust: the Shiny host's authenticated user
  # (session$user -- set by Shiny Server Pro / Posit Connect / RStudio auth), then
  # the identity a reverse proxy / SSO gateway forwards in a request header
  # (oauth2-proxy, nginx auth_request, IIS/Windows-auth, Cloudflare Access all use
  # one of these), then the OS login of the host process. Header values are only
  # ever stored as a string in the audit log -- never evaluated -- so there is no
  # injection surface. Returns NA when nothing identifies the user.
  .SSO_HEADERS <- c("HTTP_X_FORWARDED_USER", "HTTP_X_AUTH_REQUEST_USER",
    "HTTP_X_AUTH_REQUEST_EMAIL", "HTTP_X_FORWARDED_EMAIL", "HTTP_REMOTE_USER",
    "HTTP_X_REMOTE_USER", "HTTP_X_FORWARDED_PREFERRED_USERNAME", "HTTP_CF_ACCESS_AUTHENTICATED_USER_EMAIL")
  detected_identity <- function() {
    su <- session$user
    if (!is.null(su) && nzchar(trimws(su))) return(trimws(su))
    req <- session$request
    if (!is.null(req)) for (h in .SSO_HEADERS) {
      v <- tryCatch(req[[h]], error = function(e) NULL)
      if (!is.null(v) && nzchar(trimws(v))) return(trimws(v))
    }
    cu <- current_user()
    if (!is.null(cu) && nzchar(cu) && !identical(cu, "unknown")) return(cu)
    NA_character_
  }
  # who_now() -- the single source of truth for WHO is doing this, so the audit
  # trail records a real person, never a placeholder. Preference: the name typed
  # on Convert, then whoever the environment already identifies (SSO / host / OS).
  who_now <- function() {
    if (!is.null(input$cv_by) && nzchar(trimws(input$cv_by))) return(trimws(input$cv_by))
    detected_identity() %||% (session$user %||% current_user())
  }
  # On connect, pre-fill the audit-trail name with whoever the environment already
  # identifies, so it's right by default; the analyst can still override it.
  observe({
    who <- isolate(detected_identity())
    cur <- isolate(input$cv_by)
    if (!is.na(who) && nzchar(who) && (is.null(cur) || !nzchar(trimws(cur))))
      updateTextInput(session, "cv_by", value = who)
  })
  output$cv_who_hint <- renderUI({
    who <- detected_identity()
    if (is.na(who) || !nzchar(who)) return(NULL)
    helpText(sprintf("Detected as %s from your sign-in - change it above if you're recording this for someone else.", who))
  })

  # run_conversion -- the whole convert-a-file flow (session dir, convert, state,
  # upload capture), shared by the Convert button and "Try it on a sample".
  # record = FALSE skips the Admin uploads capture (the bundled sample is not a
  # team statement to pick up).
  run_conversion <- function(srcpath, name, record = TRUE, force_tpl = NULL) {
    sess <- file.path(tempdir(), paste0("cv_", as.integer(runif(1, 1, 1e9))))
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, name)
    file.copy(srcpath, src, overwrite = TRUE)
    cv_forced(list())   # a new file -> forget any force-included rows from the last one
    who <- who_now()
    res <- convert_now(src, sess, forced_rows = NULL, force_tpl = force_tpl)
    cv_res(res); cv_dir(sess); cv_src(list(path = src, name = name))
    cv_fb_done(FALSE)   # reset the feedback panel for the new conversion
    # Capture the upload + its outcome so a failed/abandoned new format is a
    # 2-second pickup in Admin -> Uploads (the file is saved for a safe re-audit).
    uid <- if (record) safe(record_upload(src, name = name, requested_by = who,
      status = res$status %||% "failed", run_id = res$run_id %||% NA_character_,
      template = res$template_id %||% NA_character_,
      trust = res$trust$level %||% NA_character_,
      detail = paste(res$messages, collapse = "; "), dir = UPLOADS_DIR), NA_character_)
    else NA_character_
    cv_upload_id(uid)
  }

  observeEvent(input$cv_go, {
    if (is.null(input$cv_file)) {
      showNotification("Choose a statement file first - a PDF, CSV or Excel export from your bank.",
                       type = "warning", duration = 6)
      return()
    }
    run_conversion(input$cv_file$datapath, input$cv_file$name)
  })

  # "Try it on a sample": convert the bundled specimen statement, so the very
  # first visit can show the whole payoff (verdict, analysis, downloads) without
  # the user needing a statement at hand.
  observeEvent(input$cv_try_sample, {
    if (!file.exists(SAMPLE_STATEMENT)) {
      showNotification("The bundled sample statement isn't on this install (samples/ folder missing).",
                       type = "warning", duration = 6)
      return()
    }
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

  # Empty state: shown before the first conversion. Tells a brand-new user what
  # this page is for and exactly what they'll get back, so the screen is never a
  # mystery or a wall of empty headers.
  output$cv_empty <- renderUI({
    div(style = "max-width:560px;color:#444;line-height:1.6",
      h4(style = "margin-top:4px", "Convert a bank statement"),
      p("Upload a statement on the left - a ", tags$b("PDF"), ", ", tags$b("CSV"),
        " or ", tags$b("Excel"), " file - and click ", tags$b("Convert"), "."),
      p(class = "muted", style = "margin-bottom:6px", "You'll get back, right here:"),
      tags$ul(style = "color:#444",
        tags$li(tags$b("Every transaction"), ", read verbatim - date, description, amount, balance."),
        tags$li(tags$b("Proof nothing's missing"), " - the balance reconciles, with a plain confidence level."),
        tags$li(tags$b("Your download"), " - Excel, CSV or JSON.")),
      p(class = "muted", "Your bank is detected automatically. A layout the tool hasn't seen points you to ",
        actionLink("cv_empty_to_tmpl", "Add a template"), " - a 2-minute, no-code setup."),
      # First visit, nothing to upload yet? One click shows the whole payoff on
      # a bundled specimen statement (public sample - not anyone's real data).
      if (file.exists(SAMPLE_STATEMENT))
        div(style = "margin-top:14px;padding:12px 14px;background:#f8faf9;border:1px dashed #bfe0c8;border-radius:10px",
          actionButton("cv_try_sample", "Try it on a sample statement", class = "btn-default"),
          div(class = "muted", style = "margin-top:6px",
              "No file needed - converts a bundled specimen so you can see a full result in seconds.")))
  })

  output$cv_status <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    # A successful transaction statement gets the plain hero headline (cv_headline
    # below); this status card is kept for form results and for anything that did
    # NOT convert cleanly (so failures still explain themselves up top) - same
    # card shape as the success headline, coloured by how it went.
    if (isTRUE(res$status == "ok") && !identical(res$kind, "form")) return(NULL)
    st <- res$status %||% "failed"
    pal <- switch(st,
      ok           = c(bg = "#eef8f0", bd = "#bfe0c8", ink = "#137333"),
      needs_review = ,
      unsupported  = c(bg = "#fff8e6", bd = "#f0c36d", ink = "#8a5b00"),
      c(bg = "#fdecec", bd = "#f2b8b8", ink = "#b00020"))
    trust <- if (!is.null(res$trust)) sprintf(" · confidence: %s", res$trust$level) else ""
    tid <- (res$template_id %||% NA_character_)[1]
    div(style = sprintf("background:%s;border:1px solid %s;border-radius:8px;padding:12px 16px;margin:4px 0 12px",
                        pal[["bg"]], pal[["bd"]]),
      h4(style = sprintf("margin:0 0 6px;color:%s", pal[["ink"]]),
         paste0(plain_status(st), trust)),
      # Engine messages carry a leading machine code ("needs_review: ...") for the
      # logs; the card headline already says it in words, so drop the code here.
      lapply(sub("^(ok|needs_review|unsupported|failed):\\s*", "",
                 as.character(res$messages %||% character(0))),
             function(m) p(style = "margin:0 0 4px;color:#333", m)),
      if (!is.na(tid) && nzchar(tid))
        p(class = "muted", style = "margin:0", paste("Template:", tid)))
  })

  # cv_headline -- the EASY, plain-English verdict for a transaction result: did it
  # work, how many transactions, and can I trust it, said in words rather than KPI
  # codes. This is what a non-technical reviewer reads first; the KPI tables stay
  # available under "Checks & detail".
  plain_trust <- function(level) switch(level %||% "",
    high   = list(cls = "ok",   icon = "✓", line = "High confidence: the opening balance plus every transaction equals the closing balance the statement prints, so nothing is missing."),
    medium = list(cls = "warn", icon = "✓", line = "Medium confidence: it read cleanly, but a full completeness check could not run (usually because this statement has no running balance to check against). Worth a quick eyeball of the count against the statement."),
    low    = list(cls = "bad",  icon = "!",      line = "Low confidence: please check these transactions against the statement before relying on them."),
    list(cls = "warn", icon = "✓", line = "Read cleanly, but completeness could not be auto-verified. Check the transaction count against the statement."))
  output$cv_headline <- renderUI({
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
    if (!isTRUE(res$status == "ok")) return(NULL)   # failures are shown by cv_status
    n <- tryCatch({
      csv <- res$outputs[grepl("\\.csv$", res$outputs)]
      if (length(csv) == 1 && file.exists(csv))
        nrow(utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE)) else NA_integer_
    }, error = function(e) NA_integer_)
    pt <- plain_trust(res$trust$level)
    bg <- c(ok = "#eef8f0", warn = "#fff8e6", bad = "#fdecec")[[pt$cls]]
    bd <- c(ok = "#bfe0c8", warn = "#f0c36d", bad = "#f2b8b8")[[pt$cls]]
    # Small honest-flags row: which template read it, and anything a reviewer
    # should know at a glance (OCR pages, honoured redactions, hand-added rows).
    # All of this already exists in the result - it was just buried in the tables.
    chip <- function(txt, warn = FALSE)
      span(class = if (warn) "chip chip-warn" else "chip", txt)
    chips <- list()
    tid <- (res$template_id %||% NA_character_)[1]
    if (!is.na(tid) && nzchar(tid)) chips <- c(chips, list(chip(paste("Template:", tid))))
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
    div(style = sprintf("background:%s;border:1px solid %s;border-radius:8px;padding:12px 16px;margin:4px 0 12px", bg, bd),
      h3(style = "margin:0 0 4px", sprintf("%s Converted%s", pt$icon,
        if (!is.na(n)) sprintf(" - %d transaction%s read", n, if (n == 1) "" else "s") else "")),
      p(style = "margin:0 0 6px;color:#333", pt$line),
      p(class = "muted", style = "margin:0 0 6px",
        "Full detail is under 'Checks & detail' below."),
      if (length(chips)) div(chips))
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

  output$cv_summary <- renderUI({
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
    d <- cv_data(); h <- res$header %||% list(); cur <- cur_symbol(h)
    n   <- if (!is.null(d)) nrow(d) else (h$row_count %||% NA)
    amt <- if (!is.null(d)) d$.amt[!is.na(d$.amt)] else numeric(0)
    money_in <- sum(amt[amt > 0]); money_out <- sum(amt[amt < 0]); net <- sum(amt)
    drange <- if (!is.null(d) && any(!is.na(d$.date)))
        sprintf("%s to %s", format(min(d$.date, na.rm = TRUE), "%d %b %Y"),
                format(max(d$.date, na.rm = TRUE), "%d %b %Y"))
      else if (!is.na(h$period_start %||% NA_character_))
        sprintf("%s to %s", h$period_start, h$period_end %||% "?") else "-"
    card <- function(label, value, col = "#333")
      div(style = "flex:1 1 130px;min-width:120px;background:#fafafa;border:1px solid #e6e6e6;border-radius:8px;padding:8px 12px",
          div(style = "font-size:12px;color:#888", label),
          div(style = sprintf("font-size:19px;font-weight:600;color:%s", col), value))
    has_close <- !is.na(suppressWarnings(as.numeric(h$closing_balance %||% NA)))
    tagList(
      div(style = "display:flex;flex-wrap:wrap;gap:8px;margin:2px 0 10px",
        card("Transactions", if (is.na(n)) "-" else n),
        card("Money in",  fmt_money(money_in, cur),  "#137333"),
        card("Money out", fmt_money(money_out, cur), "#b00020"),
        card("Net",       fmt_money(net, cur), if (isTRUE(net < 0)) "#b00020" else "#137333"),
        if (has_close) card("Closing balance", fmt_money(as.numeric(h$closing_balance), cur))),
      p(class = "muted", style = "margin:0 0 4px", sprintf("Period: %s%s%s", drange,
        if (!is.na(h$account_number %||% NA_character_)) sprintf("  ·  Account: %s", h$account_number) else "",
        if (!is.na(h$bank %||% NA_character_)) sprintf("  ·  %s", h$bank) else "")))
  })

  output$cv_trend_note <- renderUI({
    req(cv_data())
    msg <- switch(input$an_view %||% "inout",
      inout   = "Green = money in, red = money out, per period. Switch 'Measure' to count transactions instead of dollars.",
      balance = "The running balance as it moves through the statement (only if the statement shows a balance column).",
      cumnet  = "The running total of every transaction added up over time - where the account net sits at each point.")
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
      points(x, y, pch = 21, cex = 0.65, col = "#fff", bg = col, lwd = 1)
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
      plot(dd$.date, cn, type = "n", axes = FALSE, xlab = "", ylab = "Cumulative net", ylim = range(aty))
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
      div(style = "margin:8px 0;padding:8px 12px;background:#eef4ff;border-radius:6px",
        HTML(sprintf("Read as a <b>form / labelled-value PDF</b> (not a transaction statement). Template: <b>%s</b>. Download it above.",
                     htmltools::htmlEscape(res$template_id %||% "?")))),
      h4("Values found"), DTOutput("cv_fields"))
  })
  output$cv_fields <- renderDT({
    res <- cv_res(); req(res, !is.null(res$fields))
    d <- res$fields[, intersect(c("field", "label", "value", "matched", "required", "flagged"),
                                names(res$fields)), drop = FALSE]
    datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 30))
  })

  output$cv_kpis <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$kpis))
    k <- res$kpis
    # Show a plain-English check name + a plain result word, not snake_case codes.
    res_word <- c(pass = "OK", fail = "Problem", na = "not on this statement")
    disp <- data.frame(
      Check  = plain_label(k$name, CHECK_PLAIN),
      Result = plain_label(k$status, res_word),
      Detail = if ("detail" %in% names(k)) k$detail else "",
      stringsAsFactors = FALSE)
    datatable(disp, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatStyle("Result", fontWeight = "bold",
        color = styleEqual(c("OK", "Problem"), c("#137333", "#b00020")))
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

  output$cv_cov_summary <- renderUI({
    res <- cv_res(); req(res); req(!is.null(res$coverage))
    p(class = "muted", coverage_summary(res$coverage))
  })
  output$cv_coverage <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$coverage))
    cov <- res$coverage[res$coverage$verdict != "unmapped" | res$coverage$field %in% c("balance","particulars","reference"), ]
    disp <- data.frame(
      field   = cov$field,
      verdict = plain_label(cov$verdict, COVERAGE_PLAIN),   # 'unmapped' -> 'not on this statement'
      populated = cov$populated, empty = cov$empty, note = cov$note,
      stringsAsFactors = FALSE)
    datatable(disp, rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatStyle("verdict",
        backgroundColor = styleEqual(unname(COVERAGE_PLAIN),
                                     c("#e6f4ea","#fff8e6","#fde7e7","#f2f2f2")))
  })

  output$cv_txns <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$outputs))
    csv <- res$outputs[grepl("\\.csv$", res$outputs)]
    req(length(csv) == 1, file.exists(csv))
    df <- utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE)
    # The CSV is already the display shape (no verbatim *_raw; debit/credit when the
    # statement splits them). Trim columns this statement never fills so the table
    # shows only fields that were actually read.
    df <- df[, .cols_with_data(df), drop = FALSE]
    datatable(df, rownames = FALSE, colnames = cv_friendly_cols(names(df)),
              options = list(pageLength = 10, scrollX = TRUE))
  })

  # need_file(p) -- a download with nothing to give tells the user (a toast) and
  # aborts, instead of handing the browser an empty "NA" file. `character(0)` is
  # NOT NULL, so an unsupported/failed result (outputs = character(0)) must be
  # length-checked, not null-checked.
  need_file <- function(p) {
    if (length(p) != 1 || is.na(p) || !nzchar(p) || !file.exists(p)) {
      showNotification("Nothing to download - this produced no output (convert/run it first).",
                       type = "warning", duration = 6)
      req(FALSE)
    }
    p
  }
  # dl_buttons(outputs, ids) -- a Download button ONLY for formats actually produced
  # (e.g. no Excel on a host without openxlsx), so no button promises a missing file.
  # Excel is the primary (btn-primary) since it's what most reviewers want.
  dl_buttons <- function(outputs, ids) {
    labs <- c(xlsx = "⭳ Excel", csv = "⭳ CSV", json = "⭳ JSON")
    has <- function(ext) any(grepl(paste0("\\.", ext, "$"), outputs %||% character(0)))
    Filter(Negate(is.null), lapply(names(ids), function(ext)
      if (has(ext)) downloadButton(ids[[ext]], labs[[ext]],
        class = if (ext == "xlsx") "btn-primary" else NULL)))
  }
  output$cv_downloads <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    btns <- dl_buttons(res$outputs, c(xlsx = "dl_xlsx", csv = "dl_csv", json = "dl_json"))
    if (!length(btns)) return(NULL)
    # Prominent bar right under the verdict: the download is the point of the page,
    # so it's the most visible thing, not a quiet box tucked into the sidebar.
    div(class = "dl-hero", span(class = "dl-hero-label", "Download your converted data:"), btns)
  })
  # cv_rematch -- an obvious, always-there escape hatch for a WRONG match: a bank
  # that matched the wrong template (or the wrong bank) needs a one-click "no, set
  # up the right template for this" without hunting. Drafts a fresh template from
  # THIS file (never seeded from the wrong match).
  output$cv_rematch <- renderUI({
    res <- cv_res(); req(res); req(!identical(res$kind, "form"))
    st <- res$status %||% "failed"
    if (!(st %in% c("ok", "needs_review"))) return(NULL)   # unsupported/failed already prompt setup
    tid <- (res$template_id %||% NA_character_)[1]
    div(style = "display:flex;align-items:center;flex-wrap:wrap;gap:8px;margin:0 0 12px;color:#555;font-size:13px",
      span(if (!is.na(tid) && nzchar(tid))
             sprintf("Matched “%s”. Not the right one?", tid)
           else "Matched the wrong template?"),
      actionButton("cv_rematch_go", "No - set up the right template for this",
                   class = "btn-warning btn-sm"))
  })
  observeEvent(input$cv_rematch_go, {
    src <- cv_src(); req(src)
    # Fresh draft from the file itself, never seeded from the wrong match.
    open_guided(src$path, src$name, seed_tmpl = NULL, upload_id = cv_upload_id())
  })
  mk_dl <- function(ext) downloadHandler(
    filename = function() {
      p <- cv_res()$outputs[grepl(paste0("\\.", ext, "$"), cv_res()$outputs)]
      if (length(p)) basename(p[1]) else paste0("download.", ext)
    },
    content = function(file) {
      p <- cv_res()$outputs[grepl(paste0("\\.", ext, "$"), cv_res()$outputs)]
      file.copy(need_file(if (length(p)) p[1] else NA_character_), file, overwrite = TRUE)
    })
  output$dl_xlsx <- mk_dl("xlsx"); output$dl_csv <- mk_dl("csv"); output$dl_json <- mk_dl("json")

  # ---- Feedback (every conversion can be rated; one file per logs/feedback/) ----
  output$cv_feedback <- renderUI({
    res <- cv_res(); if (is.null(res) || is.null(res$run_id)) return(NULL)
    if (isTRUE(cv_fb_done()))
      return(div(style = "margin-top:16px", span(class = "ok",
        "Thanks - your feedback was recorded.")))
    div(style = "margin-top:16px;padding:12px;border:1px solid #ddd;border-radius:6px",
        h4("Was this conversion correct?"),
        p(class = "muted", sprintf("run %s", res$run_id)),
        # choiceNames/choiceValues (not named choices): a non-ASCII name in
        # c(name = value) becomes a SYMBOL at parse time, which on a C-locale
        # host mangles to '<U+2713>'. Lists of plain literals stay UTF-8.
        radioButtons("cv_fb_verdict", NULL, inline = TRUE,
          choiceNames = list("✓ Correct", "△ Minor issues", "✗ Wrong"),
          choiceValues = list("correct", "minor_issues", "wrong")),
        textAreaInput("cv_fb_comment", "Comment (optional - what was wrong?)",
                      width = "100%", rows = 2),
        actionButton("cv_fb_submit", "Submit feedback", class = "btn-primary"))
  })

  observeEvent(input$cv_fb_submit, {
    res <- cv_res(); req(res, res$run_id)
    ok <- tryCatch({
      submit_feedback(run_id = res$run_id, verdict = input$cv_fb_verdict,
                      comment = input$cv_fb_comment, requested_by = who_now(),
                      template_id = res$template_id, logdir = LOGDIR)
      TRUE
    }, error = function(e) FALSE)
    cv_fb_done(isTRUE(ok))
    if (!isTRUE(ok))
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
  gv_datefmt <- function(tmpl) if (identical(tmpl$format, "pdf")) (tmpl$table$date_format %||% "%d/%m/%Y")
                               else (tmpl$columns$date$format %||% "%d/%m/%Y")
  gv_sign    <- function(tmpl) if (identical(tmpl$format, "pdf")) (tmpl$table$amount_sign %||% "signed")
                               else (tmpl$amount_sign %||% "signed")

  # apply_overrides -- fold the Basic-tab choices onto the working template. Only
  # the common fields live here; everything else is edited as YAML on Advanced.
  apply_overrides <- function(tmpl, bank, datefmt, sign, decimal = NULL,
                              unsigned_default = NULL, desc_col = NULL,
                              ref_col = NULL, bal_col = NULL,
                              id = NULL, type = NULL, currency = NULL,
                              date_col = NULL, amount_col = NULL,
                              keep_dateless = NULL) {
    if (!is.null(id) && nzchar(trimws(id)))
      tmpl$id <- gsub("[^A-Za-z0-9_]+", "_", trimws(id))   # the name it saves under
    if (!is.null(type) && nzchar(trimws(type))) tmpl$statement_type <- trimws(type)
    if (!is.null(currency) && nzchar(trimws(currency))) tmpl$currency <- trimws(currency)
    if (!is.null(bank) && nzchar(bank)) tmpl$bank <- bank
    if (identical(tmpl$format, "pdf")) {
      if (!is.null(datefmt) && nzchar(datefmt)) tmpl$table$date_format <- datefmt
      if (!is.null(sign) && nzchar(sign)) tmpl$table$amount_sign <- sign
      # Shared-date (HSBC-style) opt-in: only stamp the key when ON, so normal
      # templates stay clean and unaffected.
      if (!is.null(keep_dateless))
        tmpl$table$keep_dateless_rows <- if (isTRUE(keep_dateless)) TRUE else NULL
    } else {
      if (!is.null(datefmt) && nzchar(datefmt) && !is.null(tmpl$columns$date))
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
    tmpl
  }

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

    # LEFT: the statement itself, always visible.
    left_panel <- if (is_pdf) tagList(
      strong("Your statement"),
      p(class = "muted", HTML(paste0(
        "A <b>column</b> runs the <b>full height of the page</b> - only the <b>left-right</b> ",
        "position of your box matters, its height is ignored. Drag across a column ",
        "(top-to-bottom doesn't matter), pick what it is, and click <b>Assign</b>. ",
        "Rows are found by the date, not by your box."))),
      fluidRow(
        column(3, numericInput("g_pdf_page", "Page", 1, min = 1, step = 1)),
        column(5, selectInput("g_pdf_field", "The box marks this column…",
                              c("date", "description", "amount", "balance", "particulars",
                                "reference", "type", "debit", "credit", "other_party", "code"))),
        column(4, textInput("g_pdf_custom", "…or a custom name", ""))),
      div(actionButton("g_pdf_assign", "Assign box → column (full height)", class = "btn-primary"),
          actionButton("g_pdf_remove", "Remove this column")),
      tags$hr(style = "margin:8px 0"),
      p(class = "muted", "A one-off value (opening / closing balance, period, account)? Draw a box around it and pin it here."),
      fluidRow(
        column(8, selectInput("g_meta_field", "Pin the box as this header value",
                              c("(choose)" = "",
                                "opening balance" = "opening_balance",
                                "closing balance" = "closing_balance",
                                "statement period - start" = "period_start",
                                "statement period - end" = "period_end",
                                "account number" = "account_number",
                                "account name" = "account_name"))),
        column(4, br(), actionButton("g_meta_assign", "Pin box → value"),
               actionButton("g_meta_remove", "Remove"))),
      checkboxInput("g_keep_dateless",
        "Several rows share one date (e.g. HSBC) - keep the undated rows too (blank date, flagged)",
        value = isTRUE(tmpl$table$keep_dateless_rows)),
      plotOutput("g_pdf_plot", brush = brushOpts("g_pdf_brush"), height = "560px"))
    else tagList(
      strong("Your statement - sample rows"),
      p(class = "muted", "The first rows of your file, so you can see the columns while you set things up."),
      div(class = "mono", style = "max-height:560px;overflow:auto;border:1px solid #eee;padding:8px;font-size:12px",
          verbatimTextOutput("g_raw_sample")))

    # RIGHT: the controls.
    right_panel <- tabsetPanel(
      id = "g_tabs",
      tabPanel(
        "Simple", br(),
        fluidRow(
          column(6, textInput("g_id", "Template name (saves as this)", value = tmpl$id %||% "")),
          column(6, textInput("g_bank", "Which bank?", value = tmpl$bank))),
        fluidRow(
          column(6, textInput("g_type", "Kind of statement", value = tmpl$statement_type %||% "everyday")),
          column(6, textInput("g_currency", "Currency", value = tmpl$currency %||% "NZD"))),
        fluidRow(
          column(6, selectInput("g_date", "How are the dates written?",
                                choices = guided_date_choices(cur_fmt), selected = cur_fmt)),
          column(6, selectInput("g_sign", "How are amounts shown?",
                                choices = guided_sign_choices(), selected = cur_sign))),
        fluidRow(
          column(6, selectInput("g_decimal", "Number format (thousands / decimal)",
                                choices = c("Auto-detect (NZ / AU / UK / US)" = "auto",
                                            "1,234.56 - dot is the decimal point" = "dot",
                                            "1.234,56 - comma is the decimal (European)" = "comma"),
                                selected = cur_dec)),
          column(6, conditionalPanel(
            "input.g_sign == 'unsigned'",
            selectInput("g_unsigned_default", "A plain number (no + / − / CR) is a…",
                        choices = c("Charge - money out" = "debit",
                                    "Payment - money in" = "credit"),
                        selected = cur_ud)))),
        if (!is.null(g$cols) && length(g$cols)) tagList(
          tags$hr(),
          p(class = "muted", "Which column holds each field? Leave as detected unless the preview looks wrong."),
          fluidRow(
            column(4, selectInput("g_col_date", "Date (required)",
                                  choices = c("(pick a column)" = "", g$cols),
                                  selected = tmpl$columns$date$source %||% "")),
            column(4, selectInput("g_col_amt", "Amount",
                                  choices = c("(pick a column)" = "", g$cols),
                                  selected = tmpl$columns$amount$source %||% "")),
            column(4, selectInput("g_col_desc", "Description (required)",
                                  choices = g$cols,
                                  selected = tmpl$columns$description$source %||% g$cols[1]))),
          fluidRow(
            column(4, selectInput("g_col_ref", "Reference (optional)",
                                  choices = c("(none)" = "", g$cols),
                                  selected = tmpl$columns$reference$source %||% "")),
            column(4, selectInput("g_col_bal", "Balance (optional)",
                                  choices = c("(none)" = "", g$cols),
                                  selected = tmpl$columns$balance$source %||% "")))),
        tags$hr(),
        div(style = "padding:10px 12px;border:1px dashed #c98a00;background:#fffbe9;border-radius:8px",
          strong("None of these fit? Tell our team"),
          p(class = "muted", "Describe the FORMAT in plain words (no names / account numbers / statement details) and we'll build a template."),
          textAreaInput("g_req_detail", NULL, width = "100%", rows = 2,
            placeholder = "e.g. Dates look like 2 Dez (German). Amounts end in 'H' for Haben (credit)."),
          actionButton("g_req_send", "Send to our team", class = "btn-warning"),
          uiOutput("g_req_msg"))),
      tabPanel(
        "Advanced", br(),
        helpText(HTML("The <b>complete</b> template as text - edit anything (identifiers, column mapping, label synonyms, region bounds, row tolerance, metadata labels). Load your Simple choices in, edit, then Check &amp; apply.")),
        div(actionButton("g_adv_load", "Load current settings"),
            actionButton("g_adv_apply", "Check & apply", class = "btn-primary")),
        br(), uiOutput("g_adv_msg"),
        textAreaInput("g_yaml", NULL, value = template_yaml(tmpl), width = "100%", rows = 24)))

    showModal(modalDialog(
      title = "Statement template toolkit", size = "l", easyClose = FALSE,
      div(style = "padding:8px 12px;background:#eef4ff;border:1px solid #d6e2ff;border-radius:6px;margin-bottom:8px",
        HTML(sprintf("Setting up: <b>%s</b> &nbsp;·&nbsp; %s",
             htmltools::htmlEscape(g$name %||% "your file"),
             if (is_pdf) "PDF" else if (identical(tmpl$format, "excel")) "Excel" else "CSV / delimited"))),
      fluidRow(column(6, left_panel), column(6, right_panel)),
      tags$hr(),
      h4("Preview - what we'll pull out"),
      verbatimTextOutput("g_status"),
      DTOutput("g_preview"),
      footer = tagList(modalButton("Cancel"),
        actionButton("g_save", "Save template", class = "btn-primary"))))
  }

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
      if (tolower(tools::file_ext(name %||% "")) %in% c("xlsx", "xlsm", "xls")) {
        showNotification(paste("No transaction table found in this workbook - the toolkit needs a sheet",
                               "with a date column and an amount column. If the table is unusual,",
                               "save the sheet as CSV (File > Save As in Excel) and set that up instead."),
                         type = "warning", duration = 10)
      } else {
        showNotification(paste("Couldn't read this file automatically. If it's a scanned/image PDF give it a moment,",
                               "or try a text PDF / CSV export. If it isn't a transaction table, pick 'Something else' above."),
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
    cv_upload_id(upload_id)
    guided(list(path = path, name = name, tmpl = tmpl, default_ids = default_ids, cols = cols))
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
    sess <- file.path(tempdir(), paste0("ts_", as.integer(runif(1, 1, 1e9))))
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
        span(class = "muted", "Want to change which values are pulled, or add more (including a value in a different place than its label)? "),
        actionLink("cv_goto_templates", "Open the PDF form builder →")))
    }
    if (identical(st, "unsupported")) {
      div(style = "margin:12px 0;padding:12px;border:1px solid #f0c36d;background:#fff8e6;border-radius:8px",
        strong("This statement doesn't match any template yet."),
        p(class = "muted", "Teach the tool to read it - we've already worked out most of it. You just check it looks right and Save. Takes about a minute."),
        actionButton("cv_teach_go", "Set up a template for this", class = "btn-warning"), " ",
        actionLink("cv_goto_templates", "or build one from scratch →"))
    } else {
      # ANY result - ok, needs_review, or failed - links into template setup so a
      # clean conversion can be refined and saved as a better version of THIS
      # template. (Making a fresh/right template for a wrong match is the prominent
      # action up top, so it isn't repeated here.)
      label <- if (identical(st, "ok"))
        "Looks good. Want to tweak how it's read and save a refined version of this template?"
      else
        "Open this statement in setup to fix how it's read and save an improved template."
      div(style = "margin:12px 0;padding:10px 12px;border:1px solid #d9d9d9;background:#fafafa;border-radius:8px",
        span(class = "muted", label), " ",
        actionButton("cv_teach_go", "Open the template toolkit", class = "btn-default"))
    }
  })
  observeEvent(input$cv_goto_templates,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))
  observeEvent(input$cv_empty_to_tmpl,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))
  observeEvent(input$ab_go_convert,
    updateTabsetPanel(session, "main_tabs", selected = "Convert"))
  observeEvent(input$ab_go_template,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))
  observeEvent(input$ab_go_admin,
    updateTabsetPanel(session, "main_tabs", selected = "Admin"))

  observeEvent(input$cv_teach_go, {
    src <- cv_src(); req(src)
    res <- cv_res()
    seed <- NULL
    # If the conversion MATCHED a template (ok / needs_review), open that template
    # so the user refines the real one. An unsupported result also carries a
    # template id - the CLOSEST MISS, for the logs - and seeding from that would
    # open the wrong bank's settings and save a fingerprint that can never match
    # this file. Unsupported/failed always drafts fresh from the file itself.
    if ((res$status %||% "") %in% c("ok", "needs_review")) {
      tid <- (res$template_id %||% NA_character_)[1]
      if (!is.na(tid) && nzchar(tid)) {
        tset <- tryCatch(templates(), error = function(e) list())
        if (!is.null(tset[[tid]])) seed <- tset[[tid]]
      }
    }
    open_guided(src$path, src$name, seed_tmpl = seed, upload_id = cv_upload_id())
  })

  # "Matched but maybe wrong": when a near-duplicate template nearly matched too,
  # show the candidates + margin and let the analyst re-open the toolkit with a
  # different one. Only appears when there's a genuine runner-up, so an
  # unambiguous match stays clutter-free.
  output$cv_candidates <- renderUI({
    res <- cv_res(); req(res); req(!is.null(res$candidates))
    cand <- res$candidates
    if (is.null(nrow(cand)) || nrow(cand) < 2) return(NULL)
    thin <- isTRUE(res$detect$thin)
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
      p(class = "muted", if (nrow(others_df))
        sprintf("Matched %s. Nearest others: %s.", res$template_id,
                paste(sprintf("%s (score %s)", others_df$id, others_df$score), collapse = ", "))
        else sprintf("Matched %s.", res$template_id)),
      if (length(others)) tagList(
        selectInput("cv_cand_pick", "Wrong one? Open the toolkit with a different template:",
                    choices = others, width = "100%"),
        actionButton("cv_cand_go", "Open the toolkit with this template", class = "btn-default"))))
  })
  observeEvent(input$cv_cand_go, {
    src <- cv_src(); req(src); tid <- input$cv_cand_pick; req(tid, nzchar(tid))
    tset <- tryCatch(templates(), error = function(e) list())
    seed <- tset[[tid]]
    if (is.null(seed)) { showNotification("That template isn't available.", type = "error"); return() }
    open_guided(src$path, src$name, seed_tmpl = seed, upload_id = cv_upload_id())
  })

  guided_live <- reactive({ g <- guided(); req(g)
    # "__report__" (the "none of these fit" option) is a no-override sentinel.
    no_sentinel <- function(v) if (identical(v, "__report__")) "" else v
    apply_overrides(g$tmpl, input$g_bank, no_sentinel(input$g_date), no_sentinel(input$g_sign),
                    input$g_decimal, input$g_unsigned_default,
                    input$g_col_desc, input$g_col_ref, input$g_col_bal,
                    input$g_id, input$g_type, input$g_currency,
                    date_col = input$g_col_date, amount_col = input$g_col_amt,
                    keep_dateless = input$g_keep_dateless) })

  # Nudge the user to the "tell our team" box when they pick "none of these".
  observeEvent(list(input$g_date, input$g_sign), {
    if (identical(input$g_date, "__report__") || identical(input$g_sign, "__report__"))
      showNotification("None of the options fit? Use the 'Tell our team' box below to describe it.",
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
      "Thanks - raised for review. Our team will build a template for this format."))
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
    g$tmpl <- parsed; guided(g)
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
    updateSelectInput(session, "g_col_date", selected = parsed$columns$date$source %||% "")
    updateSelectInput(session, "g_col_amt",  selected = parsed$columns$amount$source %||% "")
    updateSelectInput(session, "g_col_desc", selected = parsed$columns$description$source %||% "")
    updateSelectInput(session, "g_col_ref",  selected = parsed$columns$reference$source %||% "")
    updateSelectInput(session, "g_col_bal",  selected = parsed$columns$balance$source %||% "")
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
    pg <- max(1L, as.integer(input$g_pdf_page %||% 1))
    sz <- tryCatch(pdftools::pdf_pagesize(g$path), error = function(e) NULL)
    if (is.null(sz) || pg > nrow(sz)) return(NULL)
    ras <- tryCatch(as.raster(magick::image_read(
      pdftools::pdf_render_page(g$path, page = pg, dpi = 100))), error = function(e) NULL)
    if (is.null(ras)) return(NULL)
    list(ras = ras, w = sz$width[pg], h = sz$height[pg])
  })
  output$g_pdf_plot <- renderPlot({
    r <- g_pdf_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    cols <- guided()$tmpl$table$columns %||% list()
    if (length(cols)) {
      pal <- grDevices::hcl(seq(0, 300, length.out = length(cols)), 70, 55)
      for (i in seq_along(cols)) {
        b <- cols[[i]]; if (is.null(b$x_min) || is.null(b$x_max)) next
        rect(b$x_min, 0, b$x_max, r$h, border = pal[i], lwd = 2)
        text(mean(c(b$x_min, b$x_max)), 16, names(cols)[i], col = pal[i], font = 2)
      }
    }
    # pinned header-value boxes (metadata_regions) for the CURRENT page, in orange
    mr <- guided()$tmpl$table$metadata_regions %||% list()
    pg <- max(1L, as.integer(input$g_pdf_page %||% 1))
    for (nm in names(mr)) { b <- mr[[nm]]
      if (is.null(b$x_min) || is.null(b$x_max)) next
      if (!identical(as.integer(b$page %||% 1), pg)) next
      y0 <- b$y_min %||% 0; y1 <- b$y_max %||% r$h
      rect(b$x_min, y0, b$x_max, y1, border = "#a15c00", lwd = 2)
      text(b$x_min, y0, nm, col = "#a15c00", font = 2, cex = 0.85, pos = 3, offset = 0.2)
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
  .pdf_chosen_field <- function()
    if (nzchar(trimws(input$g_pdf_custom %||% "")))
      gsub("[^A-Za-z0-9_]+", "_", trimws(input$g_pdf_custom)) else input$g_pdf_field

  observeEvent(input$g_pdf_assign, {
    g <- guided(); req(g); br <- input$g_pdf_brush
    if (is.null(br)) { showNotification("Draw a box across the column first.", type = "warning"); return() }
    f <- .pdf_chosen_field(); slot <- .pdf_field_ref(f)
    g$tmpl$table[[slot]][[f]] <- list(x_min = round(br$xmin), x_max = round(br$xmax))
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
    output$g_adv_msg <- renderUI(span(class = "ok",
      sprintf("Set the '%s' column%s.%s Page and preview updated.", f,
              if (identical(slot, "extras")) " (custom / extra)" else "",
              if (switched) " Amount style set to separate money-in / money-out." else "")))
  })
  # Delete a column band the auto-setup got wrong (a column that isn't on this
  # statement). Recomputes the table region from whatever bands remain.
  observeEvent(input$g_pdf_remove, {
    g <- guided(); req(g); f <- .pdf_chosen_field()
    slot <- if (!is.null(g$tmpl$table$columns[[f]])) "columns"
            else if (!is.null(g$tmpl$table$extras[[f]])) "extras" else NA
    if (is.na(slot)) {
      showNotification(sprintf("There's no '%s' column to remove.", f), type = "warning"); return() }
    g$tmpl$table[[slot]][[f]] <- NULL
    g <- .pdf_resize_region(g); guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "ok",
      sprintf("Removed the '%s' column. Page and preview updated.", f)))
  })

  # Pin a drawn box to a header value (metadata_regions). Unlike a column, this is a
  # specific box (x AND y) around ONE value that isn't on every row -- a balance,
  # the statement period, an account detail -- read straight from that spot when the
  # automatic reader can't label it. It never touches the transaction region.
  observeEvent(input$g_meta_assign, {
    g <- guided(); req(g); br <- input$g_pdf_brush; f <- input$g_meta_field
    if (is.null(f) || !nzchar(f)) {
      showNotification("Choose which header value first.", type = "warning"); return() }
    if (is.null(br)) {
      showNotification("Draw a box around just that value on the page first.", type = "warning"); return() }
    pg <- max(1L, as.integer(input$g_pdf_page %||% 1))
    g$tmpl$table$metadata_regions[[f]] <- list(page = pg,
      x_min = round(br$xmin), x_max = round(br$xmax),
      y_min = round(br$ymin), y_max = round(br$ymax))
    guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "ok",
      sprintf("Pinned '%s' to the box you drew on page %d.", f, pg)))
  })
  observeEvent(input$g_meta_remove, {
    g <- guided(); req(g); f <- input$g_meta_field
    if (is.null(f) || !nzchar(f) || is.null(g$tmpl$table$metadata_regions[[f]])) {
      showNotification("No pinned box for that value to remove.", type = "warning"); return() }
    g$tmpl$table$metadata_regions[[f]] <- NULL
    if (!length(g$tmpl$table$metadata_regions)) g$tmpl$table$metadata_regions <- NULL
    guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "ok", sprintf("Removed the pinned box for '%s'.", f)))
  })

  output$g_preview <- renderDT({
    g <- guided(); req(g); tx <- draft_preview(g$path, guided_live()); req(!is.null(tx))
    tx <- utils::head(tx, 12)
    # Show every field that was actually read -- including reference, and the
    # separate debit / credit columns when the statement splits them -- so the
    # user can confirm each mapped column, not just date/description/amount.
    show <- setdiff(.cols_with_data(tx), "row_id")
    lead <- intersect(c("date", "description", "amount", "debit", "credit", "direction",
                        "balance", "reference", "particulars", "code", "other_party", "type"), show)
    show <- c(lead, setdiff(show, lead))
    if (!length(show)) show <- names(tx)
    datatable(tx[, show, drop = FALSE], rownames = FALSE, colnames = cv_friendly_cols(show),
              options = list(dom = "t", pageLength = 12, scrollX = TRUE))
  })
  output$g_status <- renderText({
    g <- guided(); req(g); tx <- draft_preview(g$path, guided_live())
    if (is.null(tx) || !nrow(tx)) "No rows detected yet - check the Date and Amount column pickers, or try a different date / amount setting."
    else sprintf("%d transaction row(s) detected. If these look right, click Save.", nrow(tx))
  })
  observeEvent(input$g_save, {
    g <- guided(); req(g)
    tmpl <- guided_live()
    # If we opened a tested (default) template to refine it, saving under the same
    # id would be shadowed - curated defaults win on an id clash. Give the
    # customised copy a distinct id so the accountant's fix actually takes effect.
    if (!is.null(g$default_ids) && (tmpl$id %||% "") %in% g$default_ids)
      tmpl$id <- paste0(tmpl$id, "_custom")
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
      # This template was built FOR the statement on screen, so re-run it now with
      # the just-saved template (forced by id) and show the result on Convert -- the
      # user shouldn't have to re-upload and re-convert by hand.
      saved_id <- tmpl$id %||% NA_character_
      gp <- g$path; gn <- g$name %||% (if (!is.null(gp)) basename(gp) else NA_character_)
      if (!is.null(gp) && file.exists(gp) && !is.na(saved_id) && nzchar(saved_id)) {
        updateTabsetPanel(session, "main_tabs", selected = "Convert")
        run_conversion(gp, gn, record = FALSE, force_tpl = saved_id)
        showNotification(sprintf("Saved \"%s\" and re-converted this statement with it.", saved_id),
                         type = "message", duration = 8)
      } else {
        showNotification(sprintf("Saved as your template \"%s\". Click Convert again to run this statement with it.",
                                 saved_id %||% "template"),
                         type = "message", duration = 8)
      }
    } else {
      # Show the specific problem + point at the Advanced tab where it's fixable.
      showNotification(HTML(paste0("<b>Couldn't save.</b> ", htmltools::htmlEscape(err),
        "<br>Open the <b>Advanced</b> tab to fix it, or adjust the fields above.")),
        type = "error", duration = 12)
    }
  })

  # ---- Admin: insights from the logs -------------------------------
  adm_data <- reactiveVal(NULL)
  load_admin <- function() adm_data(list(
    runs = tryCatch(read_runs_all(LOGDIR), error = function(e) data.frame()),  # live + archived
    fb   = tryCatch(read_feedback(LOGDIR), error = function(e) data.frame())))
  observeEvent(input$adm_refresh, load_admin())
  observe({ if (is.null(adm_data())) load_admin() })

  output$adm_overview <- renderDT({
    d <- adm_data(); req(d)
    datatable(runs_overview(d$runs), rownames = FALSE, options = list(dom = "t"))
  })
  output$adm_status_plot <- renderPlot({
    d <- adm_data(); req(d); ov <- runs_overview(d$runs); if (!nrow(ov)) return(NULL)
    cols <- c(ok = "#137333", needs_review = "#e3b341", unsupported = "#b00020",
              failed = "#7d1a1a")[ov$status]
    cols[is.na(cols)] <- "#888"
    op <- par(mar = c(5, 4, 1, 1)); on.exit(par(op))
    barplot(setNames(ov$n, ov$status), col = cols, las = 2, ylab = "conversions")
  })
  output$adm_gaps <- renderDT({
    d <- adm_data(); req(d)
    g <- unsupported_clusters(d$runs)
    if (!nrow(g)) return(datatable(data.frame(message = "No unsupported statements logged yet."),
                                   rownames = FALSE, options = list(dom = "t")))
    datatable(g[, c("count", "layout", "closest_template", "why", "last_seen", "example_file")],
              rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
      formatStyle("count", fontWeight = "bold")
  })
  output$adm_usage <- renderDT({
    d <- adm_data(); req(d)
    u <- template_usage(d$runs, d$fb)
    if (!nrow(u)) return(datatable(data.frame(message = "No matched conversions yet."),
                                   rownames = FALSE, options = list(dom = "t")))
    datatable(u, rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })
  output$adm_drift <- renderDT({
    d <- adm_data(); req(d)
    dr <- template_drift(d$runs)
    if (!nrow(dr)) return(datatable(data.frame(message = "No drift detected."),
                                    rownames = FALSE, options = list(dom = "t")))
    datatable(dr, rownames = FALSE, options = list(dom = "t")) |>
      formatStyle("drop", fontWeight = "bold", color = "#b00020")
  })
  observeEvent(input$adm_rollup, {
    r <- tryCatch(rollup_logs(LOGDIR, "runs", keep_days = 90), error = function(e) NULL)
    r2 <- tryCatch(rollup_logs(LOGDIR, "feedback", keep_days = 90), error = function(e) NULL)
    load_admin()
    output$adm_rollup_msg <- renderUI(span(class = "ok",
      sprintf("Archived %d old run file(s); %d kept. History is preserved in logs/archive/.",
              (r$archived %||% 0) + (r2$archived %||% 0), (r$kept %||% 0))))
  })
  output$adm_feedback <- renderDT({
    d <- adm_data(); req(d); fb <- d$fb
    if (is.null(fb) || !nrow(fb) || !("flagged" %in% names(fb)))
      return(datatable(data.frame(message = "No feedback yet."), rownames = FALSE, options = list(dom = "t")))
    fl <- fb[isTRUE(TRUE) & as.logical(fb$flagged) %in% TRUE, , drop = FALSE]
    if (!nrow(fl)) return(datatable(data.frame(message = "No flagged feedback."),
                                    rownames = FALSE, options = list(dom = "t")))
    datatable(fl[, intersect(c("ts", "verdict", "comment", "template_id", "run_id"), names(fl))],
              rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })
}

shinyApp(ui, server)
