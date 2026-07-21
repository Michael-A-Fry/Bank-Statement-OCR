# app.R -- interactive GUI for the statement conversion engine.
#
# Two jobs, both point-and-click for a non-engineer analyst:
#   1. Convert  -- upload a statement, convert it, review the checks, download.
#   2. Template wizard -- upload a sample, map columns by dropdown, preview the
#      parse live, and SAVE a new bank template (writes the YAML for you).
#
# Run locally:  R -e 'shiny::runApp(".", launch.browser = TRUE)'
# (from the repo root, so R/ and templates/ resolve.)

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
CANON_FIELDS <- c("date", "amount", "description", "particulars",
                  "code", "reference", "type", "other_party", "balance")

# Plain-English labels for the everyday screen. The engine's internal codes
# (needs_review, balance_reconciliation, ...) stay in the logs; a non-technical
# user only ever sees these sentences.
STATUS_PLAIN <- c(
  ok           = "Converted successfully",
  needs_review = "Converted — please double-check it",
  unsupported  = "No template for this statement yet",
  failed       = "Could not read this file")
CHECK_PLAIN <- c(
  balance_reconciliation     = "Opening + transactions = closing balance",
  running_balance_continuity = "Each running balance follows from the last",
  transaction_count          = "Row count matches the statement",
  dates_within_period        = "All dates fall in the statement period",
  no_unparsed_rows           = "Every row was read",
  redaction_summary          = "Redactions found and honoured",
  ocr_confidence             = "Scan / OCR read quality")
COVERAGE_PLAIN <- c(populated = "present", partial = "some rows empty",
                    empty = "empty (check the mapping)", unmapped = "not on this statement")
plain_status <- function(s) { s <- s %||% "?"; v <- STATUS_PLAIN[s]; if (is.na(v)) toupper(s) else unname(v) }
plain_label  <- function(x, map) { out <- unname(map[x]); ifelse(is.na(out), x, out) }
# The friendly line shown when a file simply can't be read (technical detail -> log).
FRIENDLY_READ_ERROR <- paste(
  "We couldn't read this file. It may be password-protected, an image-only scan we can't open,",
  "or not a bank statement. Try re-saving it as a PDF or CSV, or use Guided setup to teach the format.")

# Read the header row of a delimited sample -> character vector of column names.
read_headers <- function(path, delim = ",") {
  line <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) "")
  if (!length(line) || !nzchar(line)) return(character(0))
  parts <- strsplit(line, delim, fixed = TRUE)[[1]]
  trimws(gsub('^"|"$', "", parts))
}

# Best-guess a default source column for a canonical field.
guess_col <- function(headers, patterns) {
  for (p in patterns) {
    hit <- grep(p, headers, ignore.case = TRUE)
    if (length(hit)) return(headers[hit[1]])
  }
  "(none)"
}

# YAML-quote a scalar only when it contains non-word characters.
yq <- function(s) if (grepl("[^A-Za-z0-9_]", s)) sprintf('"%s"', s) else s

# Build a template YAML string in the exact known-good layout.
build_tpl_yaml <- function(cfg) {
  col <- function(src, fmt = NULL) {
    if (is.null(src) || !nzchar(src) || src == "(none)") return("null")
    if (!is.null(fmt) && nzchar(fmt))
      sprintf('{source: %s, format: "%s"}', yq(src), fmt)
    else sprintf('{source: %s}', yq(src))
  }
  fp <- paste(vapply(cfg$fingerprint, yq, character(1)), collapse = ", ")
  paste0(
    "id: ", cfg$id, "\n",
    "bank: ", cfg$bank, "\n",
    "statement_type: ", cfg$statement_type, "\n",
    "format: delimited\n",
    "version: 1\n",
    "effective_from: null\n",
    "effective_to: null\n",
    "min_score: ", cfg$min_score, "\n",
    "fingerprint:\n",
    "  header_contains_all: [", fp, "]\n",
    "  filename_regex: null\n",
    "delimiter: \"", cfg$delimiter, "\"\n",
    "preamble: null\n",
    "columns:\n",
    "  date:        ", col(cfg$date_source, cfg$date_format), "\n",
    "  amount:      ", col(cfg$amount_source), "\n",
    "  description: ", col(cfg$description_source), "\n",
    "  particulars: ", col(cfg$particulars_source), "\n",
    "  code:        ", col(cfg$code_source), "\n",
    "  reference:   ", col(cfg$reference_source), "\n",
    "  type:        ", col(cfg$type_source), "\n",
    "  other_party: ", col(cfg$other_party_source), "\n",
    "  balance:     ", col(cfg$balance_source), "\n",
    "amount_sign: ", cfg$amount_sign, "\n",
    "currency: ", cfg$currency, "\n"
  )
}

# About-page + tutorial HTML content lives in ui_content.R (readability).
source("ui_content.R")

# ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".ok{color:#137333;font-weight:600}.bad{color:#b00020;font-weight:600}
     .muted{color:#666}.mono{font-family:monospace;white-space:pre-wrap}"))),
  titlePanel("Bank statement conversion"),
  tabsetPanel(
    id = "main_tabs",
    # ---- About (landing) ----------------------------------------------
    tabPanel("About", br(), about_html()),
    # ---- Convert -------------------------------------------------------
    tabPanel(
      "Convert",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          fileInput("cv_file", "Statement file (.csv / .tsv / .tdv / .pdf)"),
          textInput("cv_by", "Your name / initials (for the audit trail)", value = ""),
          uiOutput("cv_bank_ui"),
          actionButton("cv_go", "Convert", class = "btn-primary"),
          br(), br(),
          uiOutput("cv_downloads"),
          helpText("Detection is automatic; pick a bank only to force one."),
          tags$hr(),
          uiOutput("cv_templates")
        ),
        mainPanel(
          width = 8,
          uiOutput("cv_status"),
          uiOutput("cv_teach"),
          uiOutput("cv_candidates"),
          # Form / labelled-value PDF result (renders only when kind == "form").
          uiOutput("cv_form"),
          # Transaction-statement result panels (hidden for a form result).
          conditionalPanel("output.cv_is_form != true",
            h4("Checks"), DTOutput("cv_kpis"),
            h4("Diagnostics — where / why / how to fix"), DTOutput("cv_diag"),
            h4("Field coverage — is it set up right? what's present / empty / not on this statement"),
            uiOutput("cv_cov_summary"), DTOutput("cv_coverage"),
            tabsetPanel(
              tabPanel("Transactions (preview)", br(), DTOutput("cv_txns")),
              tabPanel("X-ray — see it on the page", br(),
                conditionalPanel("output.ix_is_pdf == true",
                  p(class = "muted", "Coloured = a column (see legend) · green = a transaction row the tool kept · orange = a balance / date / account detail · red = a redaction (never read)."),
                  fluidRow(
                    column(3, numericInput("ix_page", "Page", 1, min = 1, step = 1)),
                    column(4, br(), checkboxInput("ix_show_words", "Faint box on every word", TRUE)),
                    column(5, br(), checkboxInput("ix_show_meta", "Box balances, dates & account info", TRUE))),
                  plotOutput("ix_plot", height = "780px"),
                  uiOutput("ix_legend")),
                conditionalPanel("output.ix_is_pdf != true",
                  helpText("The X-ray view is for PDF statements. For CSV / Excel, the Field coverage table above shows which column feeds each field."))))),
          uiOutput("cv_feedback")
        )
      )
    ),
    # ---- Add a template (spreadsheet + PDF wizards, consolidated) ------
    tabPanel(
      "Add a template",
      br(),
      wellPanel(
        strong("🪄 Guided setup — one place, Basic + Advanced (recommended)"),
        p(class = "muted", "Upload any statement and we set up the common cases for you; open the Advanced tab for full control over wildly different formats. This is the same wizard the Convert tab uses."),
        fluidRow(
          column(7, fileInput("ts_file", "Statement file (.csv / .tsv / .tdv / .pdf)")),
          column(5, br(), actionButton("ts_go", "🪄 Open guided setup", class = "btn-warning")))),
      helpText(HTML("<b>Advanced — build a template by hand.</b> Most people should use Guided setup above; these manual wizards are for building a template field by field. Click <b>ⓘ</b> for the full step-by-step.")),
      tabsetPanel(
    tabPanel(
      "Spreadsheet (CSV / Excel)",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          actionButton("wz_help", "ⓘ How to build a template (step-by-step)",
                       class = "btn-info", style = "margin-bottom:8px;width:100%"),
          fileInput("wz_file", "Sample statement (delimited)"),
          textInput("wz_delim", "How columns are separated (comma, tab…)", value = ","),
          textInput("wz_id", "A short name for this layout", value = "newbank_everyday_csv"),
          textInput("wz_bank", "Bank", value = "NewBank"),
          textInput("wz_type", "Kind of statement", value = "everyday"),
          selectInput("wz_amount_sign", "How are amounts shown?",
                      choices = setNames(names(wd_amount_labels()),
                                         unname(wd_amount_labels()))),
          selectInput("wz_datefmt", "How are dates written?",
                      choices = setNames(vapply(wd_date_table(), `[[`, "", "fmt"),
                                         vapply(wd_date_table(), `[[`, "", "label"))),
          helpText("Auto-detected from your sample — change only if wrong."),
          textInput("wz_currency", "Currency", value = "NZD"),
          actionButton("wz_preview", "Preview parse", class = "btn-primary"),
          actionButton("wz_save", "Save template"),
          br(), br(), uiOutput("wz_msg")
        ),
        mainPanel(
          width = 8,
          uiOutput("wz_detected"),
          h4("Check each field points at the right column"),
          uiOutput("wz_maps"),
          h4("Which column headings prove it's this bank (must all be present to match)"),
          uiOutput("wz_fingerprint"),
          h4("Live preview"), verbatimTextOutput("wz_preview_status"),
          DTOutput("wz_preview_tbl"),
          h4("Generated template (templates/<id>.yaml)"),
          div(class = "mono", verbatimTextOutput("wz_yaml"))
        )
      )
    ),
    # ---- PDF wizard (nested under Add a template) ----------------------
    tabPanel(
      "PDF",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          actionButton("wp_help", "ⓘ How to build a template (step-by-step)",
                       class = "btn-info", style = "margin-bottom:8px;width:100%"),
          fileInput("wp_file", "Sample PDF statement (.pdf)"),
          numericInput("wp_page", "Page", 1, min = 1, step = 1),
          selectInput("wp_field", "Field for the next box you draw",
                      c("date", "description", "amount", "balance", "particulars",
                        "code", "reference", "other_party", "type")),
          actionButton("wp_assign", "Assign drawn box → field", class = "btn-primary"),
          actionButton("wp_remove", "🗑 Remove this field"),
          actionButton("wp_clear", "Clear all boxes"),
          tags$hr(),
          textInput("wp_id", "Template id", "newbank_pdf"),
          textInput("wp_bank", "Bank", "NewBank"),
          textInput("wp_fingerprint", "A phrase unique to this statement (for matching)", ""),
          selectInput("wp_datefmt", "How are dates written?",
                      choices = setNames(vapply(wd_date_table(), `[[`, "", "fmt"),
                                         vapply(wd_date_table(), `[[`, "", "label"))),
          selectInput("wp_sign", "How are amounts shown?",
                      choices = setNames(names(wd_amount_labels()), unname(wd_amount_labels()))),
          actionButton("wp_preview", "Preview parse", class = "btn-primary"),
          actionButton("wp_save", "Save PDF template"),
          br(), br(), uiOutput("wp_msg")
        ),
        mainPanel(
          width = 8,
          helpText("Draw a box across a column on the page, pick which field it is, and click Assign. Rows are kept only where the date reads as a real date — so headings, notes and gaps are ignored automatically."),
          plotOutput("wp_plot", brush = brushOpts("wp_brush", direction = "x"), height = "760px"),
          h4("Columns you've assigned"), tableOutput("wp_bands"),
          h4("Live preview"), verbatimTextOutput("wp_prev_status"), DTOutput("wp_prev_tbl"),
          h4("Generated PDF template"), div(class = "mono", verbatimTextOutput("wp_yaml"))
        )
      )
    ),
    # ---- PDF form / labelled values (nested under Add a template) ------
    tabPanel(
      "PDF form (labelled values)",
      br(),
      helpText("For a PDF that ISN'T a transaction table — an IRD / KiwiSaver / account summary, a letter, a form. Teach the tool which labelled values to pull; when a value sits far from its label, draw a box to say exactly where it is. (To just READ one, upload it on Convert — it's detected automatically.)"),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          textInput("fb_id", "Template id", "newpdf_fields"),
          textInput("fb_bank", "Bank / issuer", "NewIssuer"),
          textInput("fb_type", "Document type", "summary"),
          textAreaInput("fb_fp", "Identifying phrases (one per line — text that appears on this PDF)",
                        rows = 3, value = "KiwiSaver\nOpening balance"),
          textAreaInput("fb_fields",
                        "Values found NEAR their label — one per line:  field_name = Label; Other label | money",
                        rows = 6,
                        value = paste("opening_balance = Opening balance; Balance brought forward | money",
                                      "closing_balance = Closing balance | money", sep = "\n")),
          tags$hr(),
          strong("Value in a different place than its label?"),
          helpText("Upload a sample, draw a box on the page, name the field and pick its type, then Set — the value is read from that box, wherever the label is."),
          fileInput("fb_sample", "Sample PDF to test / draw on (.pdf)"),
          fluidRow(
            column(6, textInput("fb_rf_field", "Field name", "")),
            column(6, selectInput("fb_rf_type", "Value type",
                                  c("money", "date", "date_range", "text")))),
          fluidRow(
            column(4, numericInput("fb_rf_page", "Page", 1, min = 1, step = 1)),
            column(8, br(),
                   actionButton("fb_rf_set", "📍 Set value box", class = "btn-primary"),
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
      )
    ),
    # ---- Admin (insights + batch intake) ------------------------------
    tabPanel(
      "Admin",
      br(),
      conditionalPanel("!output.admin_authed",
        wellPanel(style = "max-width:440px",
          h4("Admin — password required"),
          passwordInput("adm_pw", "Password"),
          actionButton("adm_login", "Enter", class = "btn-primary"),
          uiOutput("adm_login_msg"),
          helpText("Set the password with the BSO_ADMIN_PASSWORD environment variable before deploying."))),
      conditionalPanel("output.admin_authed",
      tabsetPanel(
        tabPanel(
          "Insights",
          br(),
          actionButton("adm_refresh", "↻ Refresh from logs", class = "btn-primary"),
          helpText(HTML(paste0("Built from the real run + feedback logs in <code>",
            LOGDIR, "/</code> — every conversion the team runs (including batches below)."))),
          h4("Uploads — new formats to pick up"),
          helpText("Every statement anyone converts is saved with its outcome. A row that stayed unsupported/failed and was never taught is a pickup: grab its safe audit (no PII) and build a template."),
          fluidRow(
            column(8, DTOutput("adm_uploads")),
            column(4,
              selectInput("adm_up_pick", "Pickup — a saved upload", choices = NULL),
              downloadButton("adm_up_audit", "Download its safe audit (no PII)"),
              br(), br(),
              actionButton("adm_up_wizard", "🪄 Remediate — open in wizard",
                           class = "btn-warning"))),
          tags$hr(),
          h4("🚩 Format requests — raised by the team"),
          helpText("When a statement matched nothing, the user described the format (no PII) and raised it here. Build a template, then mark it actioned."),
          fluidRow(
            column(9, DTOutput("adm_requests")),
            column(3,
              selectInput("adm_req_pick", "A request", choices = NULL),
              actionButton("adm_req_actioned", "Mark actioned", class = "btn-primary"),
              br(), br(),
              actionButton("adm_req_dismiss", "Dismiss"))),
          tags$hr(),
          h4("📂 Folder intake — inbox / processed / failed"),
          helpText("If the team drops statements into the inbox/ folder (the no-web option), this is where they land. A file in failed/ is actionable: open it in the wizard or grab its safe audit."),
          uiOutput("adm_inbox_counts"),
          fluidRow(
            column(8, h5("Failed — needs attention"), DTOutput("adm_inbox_failed")),
            column(4,
              selectInput("adm_inbox_pick", "A failed file", choices = NULL),
              actionButton("adm_inbox_wizard", "🪄 Open in wizard", class = "btn-warning"),
              br(), br(),
              downloadButton("adm_inbox_audit", "Download its safe audit (no PII)"))),
          fluidRow(
            column(4, h5("Waiting in inbox"), DTOutput("adm_inbox_waiting")),
            column(4, h5("Processed"), DTOutput("adm_inbox_processed")),
            column(4, h5("Output folders (outbox)"), DTOutput("adm_inbox_outbox"))),
          tags$hr(),
          fluidRow(
            column(5, h4("Conversions by status"), plotOutput("adm_status_plot", height = "210px"),
                   DTOutput("adm_overview")),
            column(7, h4("Feedback flagged as wrong / minor issues"), DTOutput("adm_feedback"))),
          h4("Where the gaps are — unsupported statements to fix"),
          helpText("Each row is one unknown layout (same format collapses together). Highest count = build that template first to unblock the most statements."),
          DTOutput("adm_gaps"),
          h4("⚠ Drift — templates that used to work and recently got worse"),
          helpText("A statement that subtly changed (a moved/renamed field) breaks the balance check → the run is logged as needs-review → a template whose recent health dropped shows here. Empty is good."),
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
          helpText(HTML("Every statement layout the tool can read. <b>tested</b> = shipped and covered by golden-file tests; <b>user</b> = built on this machine. Click a row or pick below to preview and edit its YAML.")),
          DTOutput("adm_tpl_overview"),
          br(),
          fluidRow(
            column(5,
              selectInput("adm_tpl_pick", "Preview / edit a template", choices = NULL),
              actionButton("adm_tpl_validate", "Check it's valid"),
              actionButton("adm_tpl_save", "Save as user template", class = "btn-primary"),
              br(), br(), uiOutput("adm_tpl_msg"),
              helpText("Editing a 'tested' template and saving keeps a user copy; give it a new id to make your version take effect (shipped templates win on an id clash).")),
            column(7,
              h4("Template YAML"),
              textAreaInput("adm_tpl_edit", NULL, value = "", width = "100%", height = "460px"))
          ),
          tags$hr(),
          h4("Label dictionary — the wordings the engine recognises"),
          helpText(HTML("This is usually why a check shows <b>NA</b> — the statement labels its opening/closing balance, period or totals with wording the engine hasn't seen. Add the exact phrases your statements use (case-insensitive) and Save. Applies to every statement immediately.")),
          fluidRow(
            column(5,
              actionButton("adm_dict_reload", "Reload from file"),
              actionButton("adm_dict_save", "Save dictionary", class = "btn-primary"),
              br(), br(), uiOutput("adm_dict_msg")),
            column(7,
              textAreaInput("adm_dict_edit", NULL, value = "", width = "100%", height = "360px")))
        ),
        tabPanel(
          "Bulk audit & gaps",
          br(),
          helpText(HTML("Drop a whole pile of statements (any bank, any variant, selectable or scanned). Get a <b>safe-to-share</b> picture — nothing but shapes, counts and layout hashes leave the machine — of what parses, the unsupported layouts <b>clustered biggest-gap-first</b>, and <b>editable draft templates</b> for those gaps. Paste a draft into the Templates tab to save it.")),
          fluidRow(
            column(4,
              fileInput("adm_ba_files", "Statements to audit", multiple = TRUE),
              actionButton("adm_ba_run", "Run bulk audit", class = "btn-primary"),
              br(), br(),
              downloadButton("adm_ba_report", "Download safe report (.md)"),
              br(), br(),
              helpText("Also available headless: Rscript scripts/bulk-audit.R <folder>")),
            column(8,
              uiOutput("adm_ba_summary"),
              h4("Gaps — unsupported layouts, biggest first"), DTOutput("adm_ba_clusters"),
              h4("Per-file (shapes only, no PII)"), DTOutput("adm_ba_files_tbl"))),
          h4("Recommended draft templates (editable — copy into the Templates tab to save)"),
          uiOutput("adm_ba_recs"),
          tags$hr(),
          h4("Single statement — safe audit"),
          helpText("Upload one statement to download its shapes-only audit (no PII) for sharing."),
          fileInput("adm_audit_one", "Statement", multiple = FALSE),
          downloadButton("adm_audit_dl", "Download safe audit (.md)")
        ),
        tabPanel(
          "Batch intake",
          br(),
          sidebarLayout(
            sidebarPanel(
              width = 4,
              fileInput("adm_batch", "Drop many statements at once (.csv / .tsv / .pdf / .xlsx)",
                        multiple = TRUE),
              actionButton("adm_run", "Run batch", class = "btn-primary"),
              br(), br(),
              downloadButton("adm_dl_report", "Download report (CSV)"),
              helpText("Every file is converted and logged, so the results also feed the Insights tab. Unsupported files are clustered, and the biggest gap gets a starting template drafted for you.")
            ),
            mainPanel(
              width = 8,
              uiOutput("adm_batch_summary"),
              h4("Per-file results"), DTOutput("adm_batch_tbl"),
              h4("Unsupported in this batch — clustered"), DTOutput("adm_batch_clusters"),
              h4("Auto-draft: a starting template for the biggest gap"),
              helpText("A best-effort draft from the file's own structure. Confirm/adjust it in the Template or PDF wizard, then Save."),
              verbatimTextOutput("adm_draft_status"),
              tableOutput("adm_draft_cols"),
              div(class = "mono", verbatimTextOutput("adm_draft"))
            )
          )
        )
      )
      )
    )
  )
)

# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Tutorial: step-by-step "how to build a template" (either wizard) ----
  show_tutorial <- function() showModal(modalDialog(
    title = "Building a template from scratch", size = "l", easyClose = TRUE,
    tutorial_html(), footer = modalButton("Close")))
  observeEvent(input$wp_help, show_tutorial())
  observeEvent(input$wz_help, show_tutorial())

  tpl_bump <- reactiveVal(0)   # bump to force a reload after a save
  templates <- reactive({ tpl_bump(); load_template_set(TEMPLATES_DIR, USER_TEMPLATES_DIR) })

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
    banks <- sort(unique(vapply(templates(), function(t) t$bank %||% "", character(1))))
    selectInput("cv_bank", "Bank (optional)", c("(auto-detect)", banks))
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
  output$adm_tpl_overview <- renderDT(
    template_overview(templates()),
    options = list(pageLength = 25, dom = "tip"), rownames = FALSE, selection = "single")

  observe(updateSelectInput(session, "adm_tpl_pick", choices = sort(names(templates()))))

  # clicking a row selects it in the picker
  observeEvent(input$adm_tpl_overview_rows_selected, {
    ov <- template_overview(templates())
    i <- input$adm_tpl_overview_rows_selected
    if (length(i) && i <= nrow(ov)) updateSelectInput(session, "adm_tpl_pick", selected = ov$id[i])
  })

  observeEvent(input$adm_tpl_pick, {
    t <- templates()[[input$adm_tpl_pick]]; req(t)
    updateTextAreaInput(session, "adm_tpl_edit", value = template_yaml(t))
    output$adm_tpl_msg <- renderUI(NULL)
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
        t$id, "' takes precedence — rename the id for your edit to apply.")
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
      output$adm_dict_msg <- renderUI(div(style = "color:#b00020", "Not valid YAML — not saved."))
      return()
    }
    safe(file.copy(DICT_PATH, paste0(DICT_PATH, ".bak"), overwrite = TRUE))
    okw <- isTRUE(tryCatch({ writeLines(txt, DICT_PATH); TRUE }, error = function(e) FALSE))
    output$adm_dict_msg <- renderUI(div(style = sprintf("color:%s", if (okw) "#137333" else "#b00020"),
      if (okw) "Saved (backup at labels.yaml.bak). New wordings apply to the next conversion."
      else "Could not write the file — check folder permissions."))
  })

  # ---- Admin: bulk audit & gaps ----
  adm_ba <- reactiveVal(NULL)
  observeEvent(input$adm_ba_run, {
    req(input$adm_ba_files)
    fs <- input$adm_ba_files
    sess <- file.path(tempdir(), paste0("ba_", as.integer(runif(1, 1, 1e9)))); dir.create(sess, showWarnings = FALSE)
    paths <- vapply(seq_len(nrow(fs)), function(i) {
      d <- file.path(sess, fs$name[i]); file.copy(fs$datapath[i], d, overwrite = TRUE); d }, character(1))
    withProgress(message = "Auditing statements (scanned pages are OCR'd)", value = NULL,
                 adm_ba(batch_audit(paths, templates = templates())))
  })
  output$adm_ba_summary <- renderUI({
    b <- adm_ba(); if (is.null(b)) return(helpText("Upload statements and click Run bulk audit."))
    g <- b$feature_gaps
    tagList(
      p(strong(sprintf("%d statements: ", g$total)),
        paste(sprintf("%s=%s", names(g$by_status), g$by_status), collapse = ", ")),
      p(sprintf("scanned %d · with redactions %d · multi-account %d · multi-period %d · unsupported %d across %d layouts",
        g$scanned, g$with_redactions, g$multi_account, g$multi_period, g$unsupported, g$distinct_gap_layouts)),
      p(class = "muted", sprintf("amount styles: %s | date formats: %s",
        paste(names(g$amount_styles), collapse = ", "), paste(names(g$date_formats), collapse = ", "))))
  })
  output$adm_ba_clusters <- renderDT({
    b <- adm_ba(); req(b); if (!nrow(b$clusters)) return(data.frame(note = "no gaps — everything parsed"))
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
      h5(sprintf("%d file(s), %s — draft id: %s", r$count, r$kind, r$draft_id %||% "?")),
      tags$pre(style = "font-size:11px;max-height:260px;overflow:auto;background:#f7f7f7;padding:8px", r$draft_yaml))))
  })
  output$adm_ba_report <- downloadHandler(
    filename = function() "bulk-audit.md",
    content = function(file) {
      b <- adm_ba()
      if (is.null(b)) { showNotification("Run a bulk audit first — nothing to download yet.",
                                         type = "warning", duration = 6); req(FALSE) }
      writeLines(format_batch_audit(b), file) })
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
  # (Extraction/running of form PDFs now happens on the Convert tab — one door.)
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
      box = sprintf("x %d–%d, y %d–%d", r[[nm]]$x_min, r[[nm]]$x_max, r[[nm]]$y_min, r[[nm]]$y_max),
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
      span(class = "ok", sprintf("Saved '%s'. Upload it on 'Extract from a form' to use it.", t$id))
      else span(class = "bad", "Couldn't save — check the fields."))
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
    layout <- tryCatch(inspect_pdf_layout(inp, tmpl), error = function(e) NULL)
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
    sw <- function(col, lab) tags$div(style = "margin:2px 0",
      tags$span(style = sprintf("display:inline-block;width:12px;height:12px;border:2px solid %s;margin-right:6px;vertical-align:middle", col)),
      tags$span(lab))
    tagList(strong("Legend"),
      lapply(names(pal), function(nm) sw(pal[[nm]], nm)),
      sw("#137333", "transaction row (kept)"),
      sw("#a15c00", "balance / account details"),
      sw("#b00020", "redaction (not read)"))
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
    reg <- P$region; ytop <- reg$y_min %||% 0; ybot <- reg$y_max %||% r$h
    pal <- ix_pal(P$bands)
    if (!is.null(reg$x_min)) rect(reg$x_min, ybot, reg$x_max %||% r$w, ytop,
                                  border = "#666", lty = 2, lwd = 1.4)
    w <- P$words
    if (isTRUE(input$ix_show_words) && nrow(w))
      rect(w$x, w$y, w$x + w$width, w$y + w$height, border = "#cfcfcf", lwd = 0.4)
    sel <- w[!is.na(w$column), , drop = FALSE]
    if (nrow(sel)) rect(sel$x, sel$y, sel$x + sel$width, sel$y + sel$height,
                        border = pal[sel$column], lwd = 1.3)
    red <- w[w$redacted %in% TRUE, , drop = FALSE]
    if (nrow(red)) rect(red$x, red$y, red$x + red$width, red$y + red$height,
                        border = "#b00020", col = "#b0002022", lwd = 1)
    for (nm in names(P$bands)) { b <- P$bands[[nm]]
      if (!is.null(b$x_min) && !is.null(b$x_max)) {
        rect(b$x_min, ybot, b$x_max, ytop, border = pal[[nm]], lwd = 2)
        text((b$x_min + b$x_max) / 2, ytop, nm, col = pal[[nm]], font = 2, cex = 0.9, pos = 3, offset = 0.2)
      } }
    kr <- P$rows[P$rows$kept, , drop = FALSE]
    if (nrow(kr)) rect(kr$x0 - 1, kr$y0 - 1, kr$x1 + 1, kr$y1 + 1, border = "#137333", lwd = 1)
    if (isTRUE(input$ix_show_meta) && !is.null(st$meta_loc)) {
      ml <- st$meta_loc[[r$pg]]
      if (!is.null(ml)) { f <- ml[ml$found %in% TRUE, , drop = FALSE]
        if (nrow(f)) { rect(f$x0 - 2, f$y0 - 2, f$x1 + 2, f$y1 + 2, border = "#a15c00", lwd = 2)
          text(f$x1 + 3, (f$y0 + f$y1) / 2, f$field, col = "#a15c00", font = 2, cex = 0.8, adj = c(0, 0.5)) } }
    }
  })
  # Remediate a stuck upload right here: load the saved file into the SAME guided
  # wizard the Convert tab uses, so a failed/abandoned statement is a 2-second
  # pickup — identify it in the table (A), open it, teach the tool, save (B).
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

  # who_now() -- the single source of truth for WHO is doing this, so the audit
  # trail records a real person, never a placeholder. Preference: the name typed
  # on Convert, then the Shiny session user, then the OS login.
  who_now <- function() {
    if (!is.null(input$cv_by) && nzchar(trimws(input$cv_by))) return(trimws(input$cv_by))
    session$user %||% current_user()
  }

  observeEvent(input$cv_go, {
    req(input$cv_file)
    sess <- file.path(tempdir(), paste0("cv_", as.integer(runif(1, 1, 1e9))))
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, input$cv_file$name)
    file.copy(input$cv_file$datapath, src, overwrite = TRUE)
    bank <- if (is.null(input$cv_bank) || input$cv_bank == "(auto-detect)") NULL else input$cv_bank
    who <- who_now()
    # Wrap the convert in a progress bar. Scanned PDFs go through OCR (poppler +
    # tesseract) and can take many seconds, during which the button used to look
    # frozen. A visible bar tells the user it IS working, not stuck.
    res <- withProgress(message = "Converting statement…", value = 0.2, {
      incProgress(0.2, detail = "Reading the file and detecting its format…")
      out <- tryCatch(
        convert_document(src, bank = bank, outdir = sess,
                          templates_dir = TEMPLATES_DIR, user_templates_dir = USER_TEMPLATES_DIR,
                          fields_dir = FIELDS_DIR, user_fields_dir = USER_FIELDS_DIR,
                          requested_by = who, logdir = LOGDIR),
        error = function(e) {
          # Log the technical detail for Admin; show the user a plain sentence.
          safe(cat(sprintf("[%s] convert error (%s): %s\n", format(Sys.time()),
                           input$cv_file$name, conditionMessage(e)),
                   file = file.path(LOGDIR, "errors.log"), append = TRUE))
          list(status = "failed", messages = FRIENDLY_READ_ERROR)
        })
      incProgress(0.5, detail = "Running checks and writing outputs…")
      out
    })
    cv_res(res); cv_dir(sess); cv_src(list(path = src, name = input$cv_file$name))
    cv_fb_done(FALSE)   # reset the feedback panel for the new conversion
    # Capture the upload + its outcome so a failed/abandoned new format is a
    # 2-second pickup in Admin -> Uploads (the file is saved for a safe re-audit).
    uid <- safe(record_upload(src, name = input$cv_file$name, requested_by = who,
      status = res$status %||% "failed", run_id = res$run_id %||% NA_character_,
      template = res$template_id %||% NA_character_,
      trust = res$trust$level %||% NA_character_,
      detail = paste(res$messages, collapse = "; "), dir = UPLOADS_DIR), NA_character_)
    cv_upload_id(uid)
  })

  output$cv_status <- renderUI({
    res <- cv_res(); if (is.null(res)) return(helpText("Upload a statement or any PDF and click Convert."))
    cls <- if (isTRUE(res$status == "ok")) "ok" else "bad"
    # Plain English headline + a word (not a raw number) for confidence.
    trust <- if (!is.null(res$trust)) sprintf(" · confidence: %s", res$trust$level) else ""
    tagList(
      h4(HTML(sprintf('<span class="%s">%s</span>%s', cls, plain_status(res$status), trust))),
      p(class = "muted", res$messages %||% ""),
      if (!is.null(res$template_id)) p(class = "muted", paste("template:", res$template_id))
    )
  })
  # Is this result a form (labelled values) rather than a transaction statement?
  output$cv_is_form <- reactive({ isTRUE((cv_res()$kind %||% "") == "form") })
  outputOptions(output, "cv_is_form", suspendWhenHidden = FALSE)
  output$cv_form <- renderUI({
    res <- cv_res(); req(res); req(identical(res$kind, "form"))
    tagList(
      div(style = "margin:8px 0;padding:8px 12px;background:#eef4ff;border-radius:6px",
        HTML(sprintf("Read as a <b>form / labelled-value PDF</b> (not a transaction statement). Template: <b>%s</b>. Download it from the sidebar on the left.",
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
    # Admin tab, never here.
    d <- res$diagnostics[, intersect(c("where", "category", "severity", "detail", "how_to_fix"),
                                     names(res$diagnostics)), drop = FALSE]
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
    datatable(utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE),
              rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  # need_file(p) -- a download with nothing to give tells the user (a toast) and
  # aborts, instead of handing the browser an empty "NA" file. `character(0)` is
  # NOT NULL, so an unsupported/failed result (outputs = character(0)) must be
  # length-checked, not null-checked.
  need_file <- function(p) {
    if (length(p) != 1 || is.na(p) || !nzchar(p) || !file.exists(p)) {
      showNotification("Nothing to download — this produced no output (convert/run it first).",
                       type = "warning", duration = 6)
      req(FALSE)
    }
    p
  }
  # dl_buttons(outputs, ids) -- render a Download button ONLY for formats that were
  # actually produced (e.g. no Excel on a host without openxlsx), so no button ever
  # promises a file that can't be delivered.
  dl_buttons <- function(outputs, ids) {
    labs <- c(xlsx = "Excel", csv = "CSV", json = "JSON")
    has <- function(ext) any(grepl(paste0("\\.", ext, "$"), outputs %||% character(0)))
    btns <- Filter(Negate(is.null), lapply(names(ids), function(ext)
      if (has(ext)) downloadButton(ids[[ext]], labs[[ext]])))
    if (!length(btns)) return(NULL)
    tagList(strong("Download:"), br(), btns)
  }
  output$cv_downloads <- renderUI({
    res <- cv_res(); if (is.null(res)) return(NULL)
    dl_buttons(res$outputs, c(xlsx = "dl_xlsx", csv = "dl_csv", json = "dl_json"))
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
        "Thanks — your feedback was recorded.")))
    div(style = "margin-top:16px;padding:12px;border:1px solid #ddd;border-radius:6px",
        h4("Was this conversion correct?"),
        p(class = "muted", sprintf("run %s", res$run_id)),
        radioButtons("cv_fb_verdict", NULL, inline = TRUE,
          choices = c("Correct" = "correct", "Minor issues" = "minor_issues",
                      "Wrong" = "wrong")),
        textAreaInput("cv_fb_comment", "Comment (optional — what was wrong?)",
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
  REPORT_OPT <- c("🚩 None of these — tell our team" = "__report__")
  guided_date_choices <- function(extra = NULL) {
    base <- setNames(vapply(wd_date_table(), `[[`, "", "fmt"),
                     vapply(wd_date_table(), `[[`, "", "label"))
    # Always include the working template's OWN date format, even if it isn't one
    # of the standard options — so an exotic format set on the Advanced tab stays
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
                              id = NULL, type = NULL, currency = NULL) {
    if (!is.null(id) && nzchar(trimws(id)))
      tmpl$id <- gsub("[^A-Za-z0-9_]+", "_", trimws(id))   # the name it saves under
    if (!is.null(type) && nzchar(trimws(type))) tmpl$statement_type <- trimws(type)
    if (!is.null(currency) && nzchar(trimws(currency))) tmpl$currency <- trimws(currency)
    if (!is.null(bank) && nzchar(bank)) tmpl$bank <- bank
    if (identical(tmpl$format, "pdf")) {
      if (!is.null(datefmt) && nzchar(datefmt)) tmpl$table$date_format <- datefmt
      if (!is.null(sign) && nzchar(sign)) tmpl$table$amount_sign <- sign
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
    }
    # decimal_mark / unsigned_default are top-level keys the engine reads.
    if (!is.null(decimal) && nzchar(decimal))
      tmpl$decimal_mark <- if (identical(decimal, "auto")) NULL else decimal
    if (!is.null(unsigned_default) && nzchar(unsigned_default) &&
        identical(sign, "unsigned"))
      tmpl$unsigned_default <- unsigned_default
    tmpl
  }

  # ONE cohesive setup surface. Basic = friendly dropdowns for the common case;
  # Advanced = the COMPLETE template as YAML for wildly different statements
  # (fingerprints, column mapping, label synonyms, region bounds, row tolerance,
  # metadata labels). A live preview under both tabs shows what will be pulled out.
  show_guided_modal <- function() {
    g <- guided(); req(g); tmpl <- g$tmpl
    cur_fmt  <- gv_datefmt(tmpl); cur_sign <- gv_sign(tmpl)
    cur_dec  <- tmpl$decimal_mark %||% "auto"
    cur_ud   <- tmpl$unsigned_default %||% "debit"
    showModal(modalDialog(
      title = "Guided setup — teach the tool to read this statement", size = "l", easyClose = FALSE,
      div(style = "padding:8px 12px;background:#eef4ff;border:1px solid #d6e2ff;border-radius:6px;margin-bottom:8px",
        HTML(sprintf("Setting up: <b>%s</b> &nbsp;·&nbsp; %s",
             htmltools::htmlEscape(g$name %||% "your file"),
             if (identical(tmpl$format, "pdf")) "PDF"
             else if (identical(tmpl$format, "excel")) "Excel" else "CSV / delimited"))),
      p(class = "muted", "We filled this in from your file. Change anything that looks wrong — the preview at the bottom updates as you go. Basic covers most statements; open Advanced for full control."),
      tabsetPanel(
        id = "g_tabs",
        tabPanel(
          "Basic", br(),
          fluidRow(
            column(6, textInput("g_id", "Template name (this is what it saves as)", value = tmpl$id %||% "")),
            column(6, textInput("g_bank", "Which bank is this?", value = tmpl$bank))),
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
                                              "1,234.56 — dot is the decimal point" = "dot",
                                              "1.234,56 — comma is the decimal (European)" = "comma"),
                                  selected = cur_dec)),
            column(6, conditionalPanel(
              "input.g_sign == 'unsigned'",
              selectInput("g_unsigned_default", "When an amount has no + / − and no CR, treat it as a…",
                          choices = c("Charge — money out" = "debit",
                                      "Payment — money in" = "credit"),
                          selected = cur_ud)))),
          if (!is.null(g$cols) && length(g$cols)) tagList(
            tags$hr(),
            p(class = "muted", "Which column holds each field? Leave as detected unless the preview looks wrong."),
            fluidRow(
              column(4, selectInput("g_col_desc", "Description / particulars (required)",
                                    choices = g$cols,
                                    selected = tmpl$columns$description$source %||% g$cols[1])),
              column(4, selectInput("g_col_ref", "Reference (optional)",
                                    choices = c("(none)" = "", g$cols),
                                    selected = tmpl$columns$reference$source %||% "")),
              column(4, selectInput("g_col_bal", "Running balance (optional)",
                                    choices = c("(none)" = "", g$cols),
                                    selected = tmpl$columns$balance$source %||% "")))),
          tags$hr(),
          # Escape hatch: when nothing in the lists fits, raise it for review.
          div(style = "padding:10px 12px;border:1px dashed #c98a00;background:#fffbe9;border-radius:8px",
            strong("🚩 None of these fit? Tell our team"),
            p(class = "muted", "If your dates or amounts aren't in the lists — or anything else won't match — describe the format in plain words and we'll build a template. Describe the FORMAT only; please do NOT paste names, account numbers or any statement details."),
            textAreaInput("g_req_detail", NULL, width = "100%", rows = 3,
              placeholder = "e.g. Dates look like 2 Dez (German). Amounts have a comma decimal and a trailing 'H' for Haben (credit)."),
            actionButton("g_req_send", "Send to our team for review", class = "btn-warning"),
            uiOutput("g_req_msg"))),
        tabPanel(
          "Advanced (full template)", br(),
          helpText(HTML("This is the <b>complete</b> template. Edit anything — identifiers/fingerprints, column mapping, label synonyms, region bounds, row tolerance, metadata labels — to read even wildly different statements. Load your Basic choices in, edit, then Check &amp; apply.")),
          if (identical(tmpl$format, "pdf")) tagList(
            strong("Visual column editor"),
            p(class = "muted", "Draw a box across a column on the page, choose which field it is, then Assign. The bands you set are drawn on the page and drive the preview — no need to type coordinates."),
            fluidRow(
              column(3, numericInput("g_pdf_page", "Page", 1, min = 1, step = 1)),
              column(4, selectInput("g_pdf_field", "The box I draw is the…",
                                    c("date", "description", "amount", "balance", "particulars",
                                      "reference", "type", "debit", "credit", "other_party", "code"))),
              column(5, br(),
                actionButton("g_pdf_assign", "Assign box → column", class = "btn-primary"),
                actionButton("g_pdf_remove", "🗑 Remove this column"))),
            helpText("Auto-setup can add a column that isn't really on this statement. Pick it above and click Remove to delete its box."),
            plotOutput("g_pdf_plot", brush = brushOpts("g_pdf_brush", direction = "x"), height = "520px"),
            tags$hr()),
          strong("Full template (YAML)"),
          div(actionButton("g_adv_load", "↻ Load current settings into the editor"),
              actionButton("g_adv_apply", "✓ Check & apply my edits", class = "btn-primary")),
          br(), uiOutput("g_adv_msg"),
          textAreaInput("g_yaml", NULL, value = template_yaml(tmpl), width = "100%", rows = 18))),
      tags$hr(),
      h4("Preview — what we'll pull out"),
      verbatimTextOutput("g_status"),
      DTOutput("g_preview"),
      footer = tagList(modalButton("Cancel"),
        actionButton("g_save", "Save — teach the tool", class = "btn-primary"))))
  }

  # open_guided -- the single entry into the setup modal, shared by every launch
  # point (Convert result, Admin pickup, Add-a-template). Drafts a template from
  # the file unless the caller already has one (e.g. the matched template).
  open_guided <- function(path, name, seed_tmpl = NULL, upload_id = NA_character_) {
    tmpl <- seed_tmpl
    if (is.null(tmpl)) {
      bankguess <- trimws(tools::toTitleCase(gsub("[^A-Za-z]+", " ", tools::file_path_sans_ext(name))))
      tmpl <- withProgress(message = "Opening in wizard…", value = 0.4,
        tryCatch(draft_template(path, bank = if (nzchar(bankguess)) bankguess else "New bank"),
                 error = function(e) NULL))
    }
    if (is.null(tmpl)) {
      showNotification("Couldn't auto-detect this file type — use the Template/PDF wizard.", type = "error")
      return(invisible(FALSE))
    }
    # Ids of the curated (tested) templates: saving a customised copy under one of
    # these would be shadowed (defaults win), so g_save gives it a distinct id.
    default_ids <- tryCatch(names(load_templates(TEMPLATES_DIR, strict = FALSE)),
                            error = function(e) character(0))
    # For delimited statements, offer the file's actual columns in the Basic
    # field-pickers (PDF columns are bands, edited visually / in Advanced).
    cols <- if (identical(tmpl$format, "delimited"))
      tryCatch(names(read_delimited(read_input(path), tmpl)$table), error = function(e) NULL) else NULL
    cv_upload_id(upload_id)
    guided(list(path = path, name = name, tmpl = tmpl, default_ids = default_ids, cols = cols))
    show_guided_modal()
    invisible(TRUE)
  }

  # Launch the same setup modal from the Add-a-template tab (not tied to a Convert
  # upload, so a successful Save just adds the template).
  observeEvent(input$ts_go, {
    req(input$ts_file)
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
      # A form result is set up in the PDF form builder, not the table wizard.
      return(div(style = "margin:12px 0;padding:10px 12px;border:1px solid #d9d9d9;background:#fafafa;border-radius:8px",
        span(class = "muted", "Want to change which values are pulled, or add more (including a value in a different place than its label)? "),
        actionLink("cv_goto_templates", "Open the PDF form builder →")))
    }
    if (identical(st, "unsupported")) {
      div(style = "margin:12px 0;padding:12px;border:1px solid #f0c36d;background:#fff8e6;border-radius:8px",
        strong("This statement doesn't match any template yet."),
        p(class = "muted", "Teach the tool to read it — we've already worked out most of it. You just check it looks right and Save. Takes about a minute."),
        actionButton("cv_teach_go", "🪄 Set up this statement (guided)", class = "btn-warning"), " ",
        actionLink("cv_goto_templates", "or build one from scratch →"))
    } else {
      # ANY result — ok, needs_review, or failed — links into template setup, so
      # even a clean conversion can be refined or saved as a reusable template.
      label <- if (identical(st, "ok"))
        "Looks good. Want to tweak how it's read, or save a refined version of this template?"
      else
        "Open this statement in setup to fix how it's read and save an improved template."
      div(style = "margin:12px 0;padding:10px 12px;border:1px solid #d9d9d9;background:#fafafa;border-radius:8px",
        span(class = "muted", label), " ",
        actionButton("cv_teach_go", "🪄 Open in setup / edit template", class = "btn-default"), " ",
        actionLink("cv_goto_templates", "or build one from scratch →"))
    }
  })
  observeEvent(input$cv_goto_templates,
    updateTabsetPanel(session, "main_tabs", selected = "Add a template"))

  observeEvent(input$cv_teach_go, {
    src <- cv_src(); req(src)
    res <- cv_res()
    seed <- NULL
    # If the conversion matched a template, open THAT template so the user refines
    # the real one instead of starting from scratch; otherwise draft from the file.
    tid <- (res$template_id %||% NA_character_)[1]
    if (!is.na(tid) && nzchar(tid)) {
      tset <- tryCatch(templates(), error = function(e) list())
      if (!is.null(tset[[tid]])) seed <- tset[[tid]]
    }
    open_guided(src$path, src$name, seed_tmpl = seed, upload_id = cv_upload_id())
  })

  # "Matched but maybe wrong": when a near-duplicate template nearly matched too,
  # show the candidates + margin and let the analyst re-open the wizard with a
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
      strong(if (thin) "⚠ Close call — please confirm this is the right template"
             else "Template match"),
      p(class = "muted", if (nrow(others_df))
        sprintf("Matched %s. Nearest others: %s.", res$template_id,
                paste(sprintf("%s (score %s)", others_df$id, others_df$score), collapse = ", "))
        else sprintf("Matched %s.", res$template_id)),
      if (length(others)) tagList(
        selectInput("cv_cand_pick", "Wrong one? Open the wizard with a different template:",
                    choices = others, width = "100%"),
        actionButton("cv_cand_go", "🪄 Open in wizard with this template", class = "btn-default"))))
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
                    input$g_id, input$g_type, input$g_currency) })

  # Nudge the user to the "tell our team" box when they pick "none of these".
  observeEvent(list(input$g_date, input$g_sign), {
    if (identical(input$g_date, "__report__") || identical(input$g_sign, "__report__"))
      showNotification("None of the options fit? Use the '🚩 Tell our team' box below to describe it.",
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
      output$g_req_msg <- renderUI(span(class = "bad", "Couldn't save — try again.")); return() }
    updateTextAreaInput(session, "g_req_detail", value = "")
    output$g_req_msg <- renderUI(span(class = "ok",
      "Thanks — raised for review. Our team will build a template for this format."))
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
    updateSelectInput(session, "g_col_desc", selected = parsed$columns$description$source %||% "")
    updateSelectInput(session, "g_col_ref",  selected = parsed$columns$reference$source %||% "")
    updateSelectInput(session, "g_col_bal",  selected = parsed$columns$balance$source %||% "")
    output$g_adv_msg <- renderUI(span(class = "ok", "Applied — preview updated below."))
  })

  # ---- Advanced tab: visual PDF column editor (folded in, same as the wp_ tab) --
  # Renders the chosen page and draws the working template's column bands on it;
  # a drawn box assigns/updates a column, keeping the YAML editor and preview in
  # sync so PDF setup is fully visual and in one place.
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
  })
  observeEvent(input$g_pdf_assign, {
    g <- guided(); req(g); br <- input$g_pdf_brush
    if (is.null(br)) { showNotification("Draw a box across the column first.", type = "warning"); return() }
    f <- input$g_pdf_field
    g$tmpl$table$columns[[f]] <- list(x_min = round(br$xmin), x_max = round(br$xmax))
    # Widen the region's x-bounds to include every band, but PRESERVE any y-bounds
    # the template set (don't silently un-scope the table vertically).
    xs <- unlist(lapply(g$tmpl$table$columns, function(c) c(c$x_min, c$x_max)))
    if (length(xs)) {
      reg <- g$tmpl$table$region %||% list()
      reg$x_min <- min(xs) - 5; reg$x_max <- max(xs) + 5
      g$tmpl$table$region <- reg
    }
    guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "ok",
      sprintf("Set the '%s' column. Page and preview updated.", f)))
  })
  # Delete a column band the auto-setup got wrong (a column that isn't on this
  # statement). Recomputes the table region from whatever bands remain.
  observeEvent(input$g_pdf_remove, {
    g <- guided(); req(g); f <- input$g_pdf_field
    if (is.null(g$tmpl$table$columns[[f]])) {
      showNotification(sprintf("There's no '%s' column to remove.", f), type = "warning"); return() }
    g$tmpl$table$columns[[f]] <- NULL
    xs <- unlist(lapply(g$tmpl$table$columns, function(c) c(c$x_min, c$x_max)))
    reg <- g$tmpl$table$region %||% list()
    if (length(xs)) { reg$x_min <- min(xs) - 5; reg$x_max <- max(xs) + 5 }
    else { reg$x_min <- NULL; reg$x_max <- NULL }   # no bands left -> drop x-scope, keep y
    g$tmpl$table$region <- if (length(reg)) reg else NULL
    guided(g)
    updateTextAreaInput(session, "g_yaml", value = template_yaml(guided_live()))
    output$g_adv_msg <- renderUI(span(class = "ok",
      sprintf("Removed the '%s' column. Page and preview updated.", f)))
  })

  output$g_preview <- renderDT({
    g <- guided(); req(g); tx <- draft_preview(g$path, guided_live()); req(!is.null(tx))
    datatable(utils::head(tx, 12)[, intersect(c("date", "description", "amount", "direction", "balance"), names(tx))],
              rownames = FALSE, options = list(dom = "t", pageLength = 12, scrollX = TRUE))
  })
  output$g_status <- renderText({
    g <- guided(); req(g); tx <- draft_preview(g$path, guided_live())
    if (is.null(tx) || !nrow(tx)) "No rows detected yet — try a different date or amount setting."
    else sprintf("%d transaction row(s) detected. If these look right, click Save.", nrow(tx))
  })
  observeEvent(input$g_save, {
    g <- guided(); req(g)
    tmpl <- guided_live()
    # If we opened a tested (default) template to refine it, saving under the same
    # id would be shadowed — curated defaults win on an id clash. Give the
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
      showNotification(sprintf("Saved as your template \"%s\". Click Convert again to run this statement with it.",
                               tmpl$id %||% "template"),
                       type = "message", duration = 8)
    } else {
      # Show the specific problem + point at the Advanced tab where it's fixable.
      showNotification(HTML(paste0("<b>Couldn't save.</b> ", htmltools::htmlEscape(err),
        "<br>Open the <b>Advanced</b> tab to fix it, or adjust the fields above.")),
        type = "error", duration = 12)
    }
  })

  # ---- PDF wizard ---------------------------------------------------
  wp_bands <- reactiveVal(list())

  wp_render <- reactive({
    req(input$wp_file)
    pg <- max(1L, as.integer(input$wp_page %||% 1))
    sz <- tryCatch(pdftools::pdf_pagesize(input$wp_file$datapath), error = function(e) NULL)
    if (is.null(sz) || pg > nrow(sz)) return(NULL)
    ras <- tryCatch(as.raster(magick::image_read(
      pdftools::pdf_render_page(input$wp_file$datapath, page = pg, dpi = 100))),
      error = function(e) NULL)
    if (is.null(ras)) return(NULL)
    list(ras = ras, w = sz$width[pg], h = sz$height[pg])
  })

  output$wp_plot <- renderPlot({
    r <- wp_render(); req(r)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
    plot(NA, xlim = c(0, r$w), ylim = c(r$h, 0), xaxs = "i", yaxs = "i",
         xlab = "", ylab = "", axes = FALSE)
    rasterImage(r$ras, 0, r$h, r$w, 0)
    b <- wp_bands()
    if (length(b)) {
      cols <- grDevices::hcl(seq(0, 300, length.out = length(b)), 70, 55)
      for (i in seq_along(b)) {
        rect(b[[i]][1], 0, b[[i]][2], r$h, border = cols[i], lwd = 2)
        text(mean(b[[i]]), 16, names(b)[i], col = cols[i], font = 2)
      }
    }
  })

  observeEvent(input$wp_assign, {
    br <- input$wp_brush; req(br)
    b <- wp_bands(); b[[input$wp_field]] <- c(round(br$xmin), round(br$xmax)); wp_bands(b)
  })
  observeEvent(input$wp_remove, {
    b <- wp_bands()
    if (is.null(b[[input$wp_field]])) {
      showNotification(sprintf("There's no '%s' box to remove.", input$wp_field), type = "warning"); return() }
    b[[input$wp_field]] <- NULL; wp_bands(b)
  })
  observeEvent(input$wp_clear, wp_bands(list()))

  output$wp_bands <- renderTable({
    b <- wp_bands()
    if (!length(b)) return(data.frame(field = character(0), x_min = numeric(0), x_max = numeric(0)))
    data.frame(field = names(b), x_min = vapply(b, `[`, 0, 1),
               x_max = vapply(b, `[`, 0, 2), row.names = NULL)
  })

  wp_template <- reactive({
    b <- wp_bands()
    cols <- list()
    for (f in names(b)) cols[[f]] <- list(x_min = b[[f]][1], x_max = b[[f]][2])
    xs <- unlist(b)
    fp <- if (nzchar(input$wp_fingerprint %||% "")) list(input$wp_fingerprint) else list()
    list(id = input$wp_id, bank = input$wp_bank, statement_type = "statement",
         format = "pdf", version = 1, min_score = if (length(fp)) 1 else 0,
         fingerprint = list(page_contains_all = fp),
         table = list(
           region = list(x_min = if (length(xs)) min(xs) - 5 else 0,
                         x_max = if (length(xs)) max(xs) + 5 else 9999),
           row_tol = 3, date_format = input$wp_datefmt, amount_sign = input$wp_sign,
           columns = cols),
         currency = "NZD")
  })

  wp_preview <- reactiveVal(NULL)
  observeEvent(input$wp_preview, {
    req(input$wp_file); b <- wp_bands()
    if (is.null(b$date) || is.null(b$amount)) {
      wp_preview(list(status = "Draw and assign at least the date and amount columns first.", tbl = NULL)); return()
    }
    out <- tryCatch({
      parsed <- parse_pdf_table(read_input(input$wp_file$datapath), wp_template())
      list(status = sprintf("%d transaction row(s) extracted", nrow(parsed$transactions)),
           tbl = parsed$transactions)
    }, error = function(e) list(status = paste("error:", conditionMessage(e)), tbl = NULL))
    wp_preview(out)
  })
  output$wp_prev_status <- renderText({ p <- wp_preview(); if (is.null(p)) "Draw the columns, then Preview." else p$status })
  output$wp_prev_tbl <- renderDT({ p <- wp_preview(); req(!is.null(p$tbl))
    datatable(p$tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$wp_yaml <- renderText({ req(input$wp_file); yaml::as.yaml(wp_template()) })

  observeEvent(input$wp_save, {
    req(input$wp_file); b <- wp_bands()
    if (is.null(b$date) || is.null(b$amount)) {
      output$wp_msg <- renderUI(span(class = "bad", "Assign at least the date and amount columns first.")); return()
    }
    id <- gsub("[^A-Za-z0-9_]+", "_", input$wp_id)
    path <- file.path(TEMPLATES_DIR, paste0(id, ".yaml"))
    ok <- tryCatch({ yaml::write_yaml(wp_template(), path); load_templates(TEMPLATES_DIR)
                     tpl_bump(isolate(tpl_bump()) + 1); TRUE }, error = function(e) FALSE)
    output$wp_msg <- renderUI(
      if (isTRUE(ok)) span(class = "ok", paste("Saved", path, "- add a golden test next (see Help)."))
      else span(class = "bad", "Save failed - check the boxes and settings."))
  })

  # ---- Wizard -------------------------------------------------------
  wz_headers <- reactive({
    req(input$wz_file)
    read_headers(input$wz_file$datapath, input$wz_delim %||% ",")
  })

  # Auto-detect everything from the sample so the analyst only has to confirm.
  observeEvent(input$wz_file, {
    delim <- detect_delimiter(input$wz_file$datapath)
    updateTextInput(session, "wz_delim", value = delim)
    df <- tryCatch(utils::read.csv(
      input$wz_file$datapath, sep = if (identical(delim, "\t")) "\t" else delim,
      stringsAsFactors = FALSE, colClasses = "character", nrows = 50L,
      check.names = FALSE, header = TRUE), error = function(e) NULL)
    h <- if (!is.null(df)) names(df) else read_headers(input$wz_file$datapath, delim)
    dcol <- guess_mapping(h, "date")
    if (!is.null(df) && dcol %in% names(df)) {
      fmt <- detect_date_format(df[[dcol]])
      if (nzchar(fmt)) updateSelectInput(session, "wz_datefmt", selected = fmt)
    }
    updateSelectInput(session, "wz_amount_sign", selected = detect_amount_style(h, df))
    base <- tools::file_path_sans_ext(input$wz_file$name)
    updateTextInput(session, "wz_id",
                    value = paste0(gsub("[^A-Za-z0-9]+", "_", tolower(base)), "_csv"))
  })

  output$wz_detected <- renderUI({
    req(input$wz_file)
    dl <- if (identical(input$wz_delim, "\t")) "tab" else (input$wz_delim %||% ",")
    div(style = "background:#eef7ee;border:1px solid #cfe8cf;padding:8px 12px;border-radius:6px;margin-bottom:8px",
        strong("Auto-detected: "),
        sprintf("columns split by '%s'  |  dates look like %s  |  %s",
                dl, date_format_label(input$wz_datefmt %||% "%d/%m/%Y"),
                wd_amount_labels()[[input$wz_amount_sign %||% "signed"]]))
  })

  output$wz_maps <- renderUI({
    h <- wz_headers(); if (!length(h)) return(helpText("Upload a sample to map columns."))
    opts <- c("(none)", h)
    defaults <- list(
      date = guess_col(h, c("date")), amount = guess_col(h, c("amount")),
      description = guess_col(h, c("payee","description","details","memo","narrative")),
      particulars = guess_col(h, c("particulars")), code = guess_col(h, c("^code$","analysis")),
      reference = guess_col(h, c("reference","unique")), type = guess_col(h, c("type")),
      other_party = guess_col(h, c("other party","counterparty")), balance = guess_col(h, c("balance"))
    )
    tagList(lapply(CANON_FIELDS, function(f)
      selectInput(paste0("map_", f), f, opts, selected = defaults[[f]])))
  })

  output$wz_fingerprint <- renderUI({
    h <- wz_headers(); if (!length(h)) return(NULL)
    checkboxGroupInput("wz_fp", NULL, choices = h, selected = h, inline = TRUE)
  })

  wz_cfg <- reactive({
    list(
      id = input$wz_id, bank = input$wz_bank, statement_type = input$wz_type,
      delimiter = input$wz_delim %||% ",", currency = input$wz_currency,
      amount_sign = input$wz_amount_sign, date_format = input$wz_datefmt,
      min_score = max(1, length(input$wz_fp)), fingerprint = input$wz_fp,
      date_source = input$map_date, amount_source = input$map_amount,
      description_source = input$map_description, particulars_source = input$map_particulars,
      code_source = input$map_code, reference_source = input$map_reference,
      type_source = input$map_type, other_party_source = input$map_other_party,
      balance_source = input$map_balance
    )
  })

  output$wz_yaml <- renderText({ req(input$wz_file); build_tpl_yaml(wz_cfg()) })

  wz_preview <- reactiveVal(NULL)
  observeEvent(input$wz_preview, {
    req(input$wz_file)
    tmp_tpls <- file.path(tempdir(), paste0("wz_", as.integer(runif(1, 1, 1e9))))
    dir.create(tmp_tpls, showWarnings = FALSE, recursive = TRUE)
    writeLines(build_tpl_yaml(wz_cfg()), file.path(tmp_tpls, paste0(input$wz_id, ".yaml")))
    out <- tryCatch({
      tpls  <- load_templates(tmp_tpls)
      input_obj <- read_input(input$wz_file$datapath)
      det   <- detect_statement(input_obj, tpls, hint_bank = input$wz_bank)
      if (!isTRUE(det$matched))
        list(status = paste("NO MATCH:", det$detail), tbl = NULL)
      else {
        parsed <- parse_statement(input_obj, tpls[[det$template_id]])
        list(status = sprintf("matched %s (score %s) | %d row(s)",
                              det$template_id, det$score, nrow(parsed$transactions)),
             tbl = parsed$transactions)
      }
    }, error = function(e) list(status = paste("error:", conditionMessage(e)), tbl = NULL))
    wz_preview(out)
  })

  output$wz_preview_status <- renderText({ p <- wz_preview(); if (is.null(p)) "Click 'Preview parse'." else p$status })
  output$wz_preview_tbl <- renderDT({
    p <- wz_preview(); req(!is.null(p$tbl))
    datatable(p$tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  observeEvent(input$wz_save, {
    req(input$wz_file)
    id <- gsub("[^A-Za-z0-9_]+", "_", input$wz_id)
    path <- file.path(TEMPLATES_DIR, paste0(id, ".yaml"))
    ok <- tryCatch({
      writeLines(build_tpl_yaml(wz_cfg()), path)
      load_templates(TEMPLATES_DIR)  # validates the whole set still loads
      tpl_bump(isolate(tpl_bump()) + 1)
      TRUE
    }, error = function(e) { attr(ok, "err") <<- conditionMessage(e); FALSE })
    output$wz_msg <- renderUI(
      if (isTRUE(ok)) span(class = "ok", paste("Saved", path, "- add a golden test next (see Help)."))
      else span(class = "bad", "Save failed - check the mappings."))
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
    if (!nrow(dr)) return(datatable(data.frame(message = "No drift detected 🎉"),
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

  # ---- Admin: batch intake + auto-draft ----------------------------
  adm_batch <- reactiveVal(NULL)

  # Reuses the same draft_template() the guided flow uses -> one source of truth.
  adm_draft_for <- function(row) {
    tmpl <- tryCatch(draft_template(row$path, bank = "NewBank"), error = function(e) NULL)
    if (is.null(tmpl)) return(list(kind = row$kind %||% "other",
      yaml = "(could not auto-draft this file — open the wizard)", cols = NULL))
    cols <- NULL
    if (identical(tmpl$format, "pdf")) {
      cd <- tmpl$table$columns
      cols <- data.frame(field = names(cd),
        x_min = vapply(cd, function(z) z$x_min, numeric(1)),
        x_max = vapply(cd, function(z) z$x_max, numeric(1)), row.names = NULL)
    }
    list(kind = tmpl$format, yaml = yaml::as.yaml(tmpl), cols = cols)
  }

  observeEvent(input$adm_run, {
    req(input$adm_batch)
    files <- input$adm_batch
    sess <- file.path(tempdir(), paste0("batch_", as.integer(runif(1, 1, 1e9))))
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    rows <- vector("list", nrow(files))
    withProgress(message = "Converting batch", value = 0, {
      for (i in seq_len(nrow(files))) {
        incProgress(1 / nrow(files), detail = files$name[i])
        src <- file.path(sess, files$name[i]); file.copy(files$datapath[i], src, overwrite = TRUE)
        r <- tryCatch(convert_statement(src, outdir = sess, templates_dir = TEMPLATES_DIR,
          user_templates_dir = USER_TEMPLATES_DIR, logdir = LOGDIR, requested_by = "batch"),
          error = function(e) NULL)
        inp <- tryCatch(read_input(src), error = function(e) NULL)
        lsig <- if (!is.null(inp)) layout_signature(inp) else list(signature = NA_character_, hint = "")
        csv <- if (!is.null(r)) r$outputs[grepl("\\.csv$", r$outputs)] else character(0)
        nrw <- if (length(csv) && file.exists(csv[1]))
          tryCatch(nrow(utils::read.csv(csv[1], check.names = FALSE)), error = function(e) NA_integer_) else NA_integer_
        rows[[i]] <- data.frame(file = files$name[i], status = r$status %||% "failed",
          template = r$template_id %||% NA_character_, trust = r$trust$level %||% NA_character_,
          n_rows = nrw, layout = lsig$hint, signature = lsig$signature %||% NA_character_,
          kind = inp$kind %||% NA_character_, path = src, stringsAsFactors = FALSE)
      }
    })
    adm_batch(do.call(rbind, rows))
    load_admin()   # the batch just wrote logs; refresh insights
  })

  output$adm_batch_summary <- renderUI({
    df <- adm_batch(); if (is.null(df)) return(helpText("Upload statements and click Run batch."))
    n <- nrow(df); ok <- sum(df$status == "ok"); rev <- sum(df$status == "needs_review")
    uns <- sum(df$status %in% c("unsupported", "failed"))
    div(style = "background:#eef;padding:8px 12px;border-radius:6px",
      sprintf("%d file(s): %d ok, %d need review, %d unsupported/failed.", n, ok, rev, uns))
  })
  output$adm_batch_tbl <- renderDT({
    df <- adm_batch(); req(df)
    datatable(df[, c("file", "status", "template", "trust", "n_rows", "kind", "layout")],
              rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  output$adm_batch_clusters <- renderDT({
    df <- adm_batch(); req(df)
    uns <- df[df$status %in% c("unsupported", "failed"), , drop = FALSE]
    if (!nrow(uns)) return(datatable(data.frame(message = "Nothing unsupported in this batch 🎉"),
                                     rownames = FALSE, options = list(dom = "t")))
    cl <- as.data.frame(table(signature = uns$signature), stringsAsFactors = FALSE)
    hint <- vapply(cl$signature, function(s) uns$layout[uns$signature == s][1], character(1))
    kind <- vapply(cl$signature, function(s) uns$kind[uns$signature == s][1], character(1))
    out <- data.frame(count = cl$Freq, layout = hint, kind = kind, stringsAsFactors = FALSE)
    datatable(out[order(-out$count), ], rownames = FALSE, options = list(dom = "t"))
  })

  adm_draft <- reactive({
    df <- adm_batch(); req(df)
    uns <- df[df$status %in% c("unsupported", "failed") & !is.na(df$signature), , drop = FALSE]
    if (!nrow(uns)) return(NULL)
    top_sig <- names(sort(table(uns$signature), decreasing = TRUE))[1]
    adm_draft_for(uns[uns$signature == top_sig, , drop = FALSE][1, ])
  })
  output$adm_draft_status <- renderText({
    df <- adm_batch(); if (is.null(df)) return("Run a batch to draft a template for the biggest gap.")
    d <- adm_draft(); if (is.null(d)) return("No unsupported files in this batch — nothing to draft.")
    sprintf("Drafted a %s template. Confirm it in the %s wizard, then Save.", d$kind,
            if (identical(d$kind, "pdf")) "PDF" else "Template")
  })
  output$adm_draft_cols <- renderTable({
    d <- adm_draft(); if (is.null(d) || is.null(d$cols) || !nrow(d$cols)) return(NULL)
    d$cols
  })
  output$adm_draft <- renderText({ d <- adm_draft(); if (is.null(d)) "" else d$yaml })

  output$adm_dl_report <- downloadHandler(
    filename = function() "batch_report.csv",
    content = function(file) {
      df <- adm_batch()
      if (is.null(df)) { showNotification("Run a batch first — no report to download yet.",
                                          type = "warning", duration = 6); req(FALSE) }
      utils::write.csv(df[, setdiff(names(df), "path"), drop = FALSE], file, row.names = FALSE)
    })
}

shinyApp(ui, server)
