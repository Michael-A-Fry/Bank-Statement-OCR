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
CANON_FIELDS <- c("date", "amount", "description", "particulars",
                  "code", "reference", "type", "other_party", "balance")

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

# tutorial_html() -- the step-by-step "how to build a template" walkthrough shown
# in a modal from either wizard. Mirrors docs/wizard-tutorial.md (the canonical,
# fuller version). Teaches the WAYS statements differ so nothing is a surprise.
tutorial_html <- function() HTML('
<style>
 .tut h4{margin:16px 0 6px;color:#137333} .tut table{border-collapse:collapse;width:100%;margin:6px 0}
 .tut th,.tut td{border:1px solid #ddd;padding:5px 8px;text-align:left;vertical-align:top;font-size:13px}
 .tut th{background:#f2f6f2} .tut code{background:#eef;padding:0 3px;border-radius:3px}
 .tut ol,.tut ul{margin:4px 0 4px 18px} .tut .lead{color:#555}
</style>
<div class="tut">
<p class="lead">The engine has <b>zero</b> banks built in. A template just says: the date is in
<i>this</i> column, the amount in <i>that</i> one, amounts are shown <i>this</i> way, and here is a
phrase that proves it is this bank. Add a bank = add one template. Follow along with the worked
example <code>samples/raw/tutorial/sample_everyday_statement.pdf</code>.</p>

<h4>Step 0 &mdash; read the statement&#39;s shape (9 questions)</h4>
<table><tr><th>#</th><th>Question</th><th>What it sets</th></tr>
<tr><td>1</td><td>File type: CSV/TSV, Excel, PDF-with-text, or scanned PDF?</td><td>Which wizard. Scanned PDFs are OCR&#39;d automatically &mdash; check the numbers.</td></tr>
<tr><td>2</td><td>How are amounts shown?</td><td>The amount style &mdash; see below. The #1 setting.</td></tr>
<tr><td>3</td><td>How are dates written?</td><td>The date format &mdash; see below.</td></tr>
<tr><td>4</td><td>Which columns exist?</td><td>Map what&#39;s there; leave the rest blank (e.g. no balance column).</td></tr>
<tr><td>5</td><td>A preamble before the table?</td><td>Header/junk lines above the real header row.</td></tr>
<tr><td>6</td><td>Multi-line rows?</td><td>Nothing to do &mdash; the 2nd line has no date, so it&#39;s ignored.</td></tr>
<tr><td>7</td><td>Redactions (black boxes)?</td><td>Nothing to do &mdash; never read under a redaction; marked <code>[REDACTED]</code>.</td></tr>
<tr><td>8</td><td>One account or several?</td><td>Combined statements parse but flag &mdash; balances aren&#39;t continuous across accounts.</td></tr>
<tr><td>9</td><td>How many statements in the file?</td><td>Merged bundles are flagged up front &mdash; split into one statement per file.</td></tr></table>

<h4>The ways AMOUNTS differ (pick one)</h4>
<ul>
<li><b>One signed column</b> (<code>signed</code>): <code>-45.00</code> out, <code>45.00</code> in.</li>
<li><b>Two columns: Withdrawals &amp; Deposits</b> (<code>debit_credit_cols</code>): map <b>both</b>. <i>(worked example)</i></li>
<li><b>DR/CR suffix</b> (<code>dr_cr_suffix</code>): <code>123.45 DR</code> / <code>123.45 CR</code> &mdash; common on cards.</li>
<li><b>A Type column of D/C</b> (<code>type_dc</code>): a column says D or C; map it too.</li>
</ul>
<p class="lead">Balance going the wrong way? Wrong amount style. Change it, Preview again.</p>

<h4>The ways DATES differ</h4>
<table><tr><th>On the statement</th><th>Setting</th></tr>
<tr><td>21/04/2026</td><td>day/month/year (NZ/UK)</td></tr>
<tr><td>04/21/2026</td><td>month/day/year (US)</td></tr>
<tr><td>2026-04-21</td><td>year/month/day (ISO)</td></tr>
<tr><td>1 April 2025</td><td>day month-name year</td></tr>
<tr><td><b>21 Apr</b> (no year)</td><td><b>day month-name, no year</b> &mdash; year taken from the statement period automatically <i>(worked example)</i></td></tr>
<tr><td>21/04/26</td><td>day/month/2-digit-year</td></tr></table>

<h4>PDF wizard &mdash; draw the boxes</h4>
<ol>
<li>Upload the PDF, pick the page with the table (often page 2).</li>
<li>For each column: choose the <b>field</b>, drag a box across that column, click <b>Assign</b>. Do date, description, the amount column(s), balance. For two-column amounts, draw <b>debit</b> and <b>credit</b> boxes.</li>
<li>Set the <b>date format</b> and <b>amount style</b>.</li>
<li>Type a <b>fingerprint phrase</b> that&#39;s on this statement and few others (e.g. <code>Transaction details</code>).</li>
<li><b>Preview</b> &mdash; only rows whose date box reads as a real date are kept, so headings/notes/gaps drop out by themselves.</li>
<li><b>Save</b>. Boxes are x-position only (full height) &mdash; you&#39;re defining columns; rows are found by the date.</li>
</ol>

<h4>Delimited (CSV/Excel) wizard</h4>
<ol><li>Upload the sample &mdash; delimiter, dates and amount style auto-detect.</li>
<li>Check each field&#39;s dropdown points at the right column (set <code>(none)</code> if absent).</li>
<li>Tick fingerprint columns, <b>Preview</b>, <b>Save</b>.</li></ol>

<h4>Trust it when it reconciles</h4>
<p class="lead">In <b>Checks</b>: <code>running_balance_continuity</code> pass = columns mapped right;
<code>balance_reconciliation</code> pass = opening + all transactions = closing (provably correct).
If a check fails, <b>Diagnostics</b> says where/why/how to fix &mdash; usually wrong amount style or date format.</p>

<h4>Troubleshooting</h4>
<table><tr><th>Symptom</th><th>Fix</th></tr>
<tr><td>No template matched</td><td>Loosen/repick the fingerprint; check you&#39;re on the table page</td></tr>
<tr><td>Deposits look like withdrawals</td><td>Wrong amount style &mdash; switch it</td></tr>
<tr><td>Dates blank/wrong</td><td>Wrong date format; for year-less dates confirm the period is detected</td></tr>
<tr><td>Column empty / description cut off</td><td>Redraw / widen the box (stop before the amount column)</td></tr>
<tr><td>Rows missing</td><td>Their date box didn&#39;t read as a date &mdash; widen/move it</td></tr></table>
<p class="lead" style="margin-top:12px">Full version with more detail: <code>docs/wizard-tutorial.md</code>.</p>
</div>')

# ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".ok{color:#137333;font-weight:600}.bad{color:#b00020;font-weight:600}
     .muted{color:#666}.mono{font-family:monospace;white-space:pre-wrap}"))),
  titlePanel("Bank statement conversion"),
  tabsetPanel(
    # ---- Convert -------------------------------------------------------
    tabPanel(
      "Convert",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          fileInput("cv_file", "Statement file (.csv / .tsv / .tdv / .pdf)"),
          uiOutput("cv_bank_ui"),
          actionButton("cv_go", "Convert", class = "btn-primary"),
          br(), br(),
          uiOutput("cv_downloads"),
          helpText("Detection is automatic; pick a bank only to force one.")
        ),
        mainPanel(
          width = 8,
          uiOutput("cv_status"),
          uiOutput("cv_teach"),
          h4("Checks"), DTOutput("cv_kpis"),
          h4("Diagnostics — where / why / how to fix"), DTOutput("cv_diag"),
          h4("Field coverage — is it set up right? what's present / empty / not on this statement"),
          uiOutput("cv_cov_summary"), DTOutput("cv_coverage"),
          h4("Transactions (preview)"), DTOutput("cv_txns"),
          uiOutput("cv_feedback")
        )
      )
    ),
    # ---- Template wizard ----------------------------------------------
    tabPanel(
      "Template wizard",
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          actionButton("wz_help", "ⓘ How to build a template (step-by-step)",
                       class = "btn-info", style = "margin-bottom:8px;width:100%"),
          fileInput("wz_file", "Sample statement (delimited)"),
          textInput("wz_delim", "Delimiter", value = ","),
          textInput("wz_id", "Template id", value = "newbank_everyday_csv"),
          textInput("wz_bank", "Bank", value = "NewBank"),
          textInput("wz_type", "Statement type", value = "everyday"),
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
          h4("Fingerprint columns (must all be present to match)"),
          uiOutput("wz_fingerprint"),
          h4("Live preview"), verbatimTextOutput("wz_preview_status"),
          DTOutput("wz_preview_tbl"),
          h4("Generated template (templates/<id>.yaml)"),
          div(class = "mono", verbatimTextOutput("wz_yaml"))
        )
      )
    ),
    # ---- Help ----------------------------------------------------------
    # ---- PDF wizard ----------------------------------------------------
    tabPanel(
      "PDF wizard",
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
          actionButton("wp_clear", "Clear boxes"),
          tags$hr(),
          textInput("wp_id", "Template id", "newbank_pdf"),
          textInput("wp_bank", "Bank", "NewBank"),
          textInput("wp_fingerprint", "A phrase unique to this statement (for matching)", ""),
          selectInput("wp_datefmt", "How are dates written?",
                      choices = setNames(vapply(wd_date_table(), `[[`, "", "fmt"),
                                         vapply(wd_date_table(), `[[`, "", "label"))),
          selectInput("wp_sign", "How are amounts shown?",
                      choices = c("One amount column, a minus sign means money out" = "signed",
                                  "Amounts ending in DR / CR" = "dr_cr_suffix")),
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
    # ---- Admin (insights + batch intake) ------------------------------
    tabPanel(
      "Admin",
      br(),
      tabsetPanel(
        tabPanel(
          "Insights",
          br(),
          actionButton("adm_refresh", "↻ Refresh from logs", class = "btn-primary"),
          helpText(HTML(paste0("Built from the real run + feedback logs in <code>",
            LOGDIR, "/</code> — every conversion the team runs (including batches below)."))),
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
    ),
    tabPanel(
      "Help",
      br(),
      div(style = "max-width:800px",
        h4("Testing the engine without the GUI"),
        tags$pre(
"# one statement -> outputs/ (xlsx + csv + json):
Rscript run.R samples/raw/bnz/bnz_transaction_export_01.csv BNZ outputs

# run the whole test suite:
Rscript tests/run_tests.R"),
        h4("Adding a bank"),
        tags$ol(
          tags$li(HTML("Click <b>ⓘ How to build a template (step-by-step)</b> at the top of either wizard for the full walkthrough — it explains every way statements differ (amount styles, date formats, redactions, combined/merged statements).")),
          tags$li("Template wizard tab: upload a sample, map the columns, Preview, Save."),
          tags$li("PDF wizard tab: draw a box over each column, Preview, Save."),
          tags$li(HTML("A worked example lives at <code>samples/raw/tutorial/sample_everyday_statement.pdf</code> — open it in the PDF wizard and follow along.")),
          tags$li(HTML("Full written guide: <code>docs/wizard-tutorial.md</code>. Golden tests: <code>tests/HOWTO-add-template-test.md</code>."))
        ),
        p(class = "muted",
          "The wizard handles single-header delimited exports. Statements with a ",
          "preamble (e.g. ASB) or PDF tables are hand-edited from an existing ",
          "template for now.")
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

  output$cv_bank_ui <- renderUI({
    banks <- sort(unique(vapply(templates(), function(t) t$bank %||% "", character(1))))
    selectInput("cv_bank", "Bank (optional)", c("(auto-detect)", banks))
  })

  cv_res <- reactiveVal(NULL)
  cv_dir <- reactiveVal(NULL)
  cv_src <- reactiveVal(NULL)      # the uploaded file (path + name), for guided setup
  cv_fb_done <- reactiveVal(FALSE)

  observeEvent(input$cv_go, {
    req(input$cv_file)
    sess <- file.path(tempdir(), paste0("cv_", as.integer(runif(1, 1, 1e9))))
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, input$cv_file$name)
    file.copy(input$cv_file$datapath, src, overwrite = TRUE)
    bank <- if (is.null(input$cv_bank) || input$cv_bank == "(auto-detect)") NULL else input$cv_bank
    res <- tryCatch(
      convert_statement(src, bank = bank, outdir = sess,
                        templates_dir = TEMPLATES_DIR, user_templates_dir = USER_TEMPLATES_DIR,
                        requested_by = "shiny", logdir = LOGDIR),
      error = function(e) list(status = "failed",
                               messages = paste("error:", conditionMessage(e))))
    cv_res(res); cv_dir(sess); cv_src(list(path = src, name = input$cv_file$name))
    cv_fb_done(FALSE)   # reset the feedback panel for the new conversion
  })

  output$cv_status <- renderUI({
    res <- cv_res(); if (is.null(res)) return(helpText("Upload a statement and click Convert."))
    cls <- if (isTRUE(res$status == "ok")) "ok" else "bad"
    trust <- if (!is.null(res$trust)) sprintf(" | trust: %s (%s)", res$trust$level, res$trust$score) else ""
    tagList(
      h4(HTML(sprintf('<span class="%s">%s</span>%s', cls, toupper(res$status %||% "?"), trust))),
      p(class = "muted", res$messages %||% ""),
      if (!is.null(res$template_id)) p(class = "muted", paste("template:", res$template_id))
    )
  })

  output$cv_kpis <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$kpis))
    datatable(res$kpis[, intersect(c("name","status","detail"), names(res$kpis)), drop = FALSE],
              rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })

  output$cv_diag <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$diagnostics))
    datatable(res$diagnostics, rownames = FALSE,
              options = list(dom = "t", pageLength = 20, scrollX = TRUE))
  })

  output$cv_cov_summary <- renderUI({
    res <- cv_res(); req(res); req(!is.null(res$coverage))
    p(class = "muted", coverage_summary(res$coverage))
  })
  output$cv_coverage <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$coverage))
    cov <- res$coverage[res$coverage$verdict != "unmapped" | res$coverage$field %in% c("balance","particulars","reference"), ]
    datatable(cov[, c("field", "verdict", "populated", "empty", "note")],
              rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatStyle("verdict",
        backgroundColor = styleEqual(c("populated","partial","empty","unmapped"),
                                     c("#e6f4ea","#fff8e6","#fde7e7","#f2f2f2")))
  })

  output$cv_txns <- renderDT({
    res <- cv_res(); req(res); req(!is.null(res$outputs))
    csv <- res$outputs[grepl("\\.csv$", res$outputs)]
    req(length(csv) == 1, file.exists(csv))
    datatable(utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE),
              rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$cv_downloads <- renderUI({
    res <- cv_res(); if (is.null(res) || is.null(res$outputs)) return(NULL)
    tagList(
      strong("Download:"), br(),
      downloadButton("dl_xlsx", "Excel"),
      downloadButton("dl_csv", "CSV"),
      downloadButton("dl_json", "JSON")
    )
  })
  mk_dl <- function(ext) downloadHandler(
    filename = function() {
      p <- cv_res()$outputs[grepl(paste0("\\.", ext, "$"), cv_res()$outputs)][1]
      basename(p)
    },
    content = function(file) {
      p <- cv_res()$outputs[grepl(paste0("\\.", ext, "$"), cv_res()$outputs)][1]
      file.copy(p, file, overwrite = TRUE)
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
                      comment = input$cv_fb_comment, requested_by = "shiny",
                      template_id = res$template_id, logdir = LOGDIR)
      TRUE
    }, error = function(e) FALSE)
    cv_fb_done(isTRUE(ok))
    if (!isTRUE(ok))
      showNotification("Could not save feedback.", type = "error")
  })

  # ---- Guided setup: teach the tool from a statement it couldn't read ----
  guided <- reactiveVal(NULL)   # list(path, name, tmpl)

  guided_date_choices <- function()
    c(setNames(vapply(wd_date_table(), `[[`, "", "fmt"),
               vapply(wd_date_table(), `[[`, "", "label")),
      "31 Dec  (day month-name, no year)" = "%d %b")

  apply_overrides <- function(tmpl, bank, datefmt, sign) {
    if (!is.null(bank) && nzchar(bank)) tmpl$bank <- bank
    if (identical(tmpl$format, "pdf")) {
      if (!is.null(datefmt) && nzchar(datefmt)) tmpl$table$date_format <- datefmt
      if (!is.null(sign) && nzchar(sign)) tmpl$table$amount_sign <- sign
    } else {
      if (!is.null(datefmt) && nzchar(datefmt) && !is.null(tmpl$columns$date))
        tmpl$columns$date$format <- datefmt
      if (!is.null(sign) && nzchar(sign)) tmpl$amount_sign <- sign
    }
    tmpl
  }

  show_guided_modal <- function() {
    g <- guided(); req(g); tmpl <- g$tmpl
    cur_fmt  <- if (identical(tmpl$format, "pdf")) tmpl$table$date_format else (tmpl$columns$date$format %||% "%d/%m/%Y")
    cur_sign <- if (identical(tmpl$format, "pdf")) tmpl$table$amount_sign else tmpl$amount_sign
    showModal(modalDialog(
      title = "Guided setup — teach the tool to read this statement", size = "l", easyClose = FALSE,
      p(class = "muted", "We filled this in from your file. Only change something if the preview below looks wrong. When it looks right, click Save."),
      textInput("g_bank", "Which bank is this?", value = tmpl$bank),
      fluidRow(
        column(6, selectInput("g_date", "How are the dates written?",
                              choices = guided_date_choices(), selected = cur_fmt)),
        column(6, selectInput("g_sign", "How are amounts shown?",
                              choices = setNames(names(wd_amount_labels()), unname(wd_amount_labels())),
                              selected = cur_sign))),
      h4("Preview — what we'll pull out"),
      verbatimTextOutput("g_status"),
      DTOutput("g_preview"),
      footer = tagList(modalButton("Cancel"),
        actionButton("g_save", "Save — teach the tool", class = "btn-primary"))))
  }

  output$cv_teach <- renderUI({
    res <- cv_res(); req(res)
    if (!identical(res$status, "unsupported") || is.null(cv_src())) return(NULL)
    div(style = "margin:12px 0;padding:12px;border:1px solid #f0c36d;background:#fff8e6;border-radius:8px",
      strong("This statement doesn't match any template yet."),
      p(class = "muted", "Teach the tool to read it — we've already worked out most of it. You just check it looks right and Save. Takes about a minute."),
      actionButton("cv_teach_go", "🪄 Set up this statement (guided)", class = "btn-warning"))
  })

  observeEvent(input$cv_teach_go, {
    src <- cv_src(); req(src)
    bankguess <- trimws(tools::toTitleCase(gsub("[^A-Za-z]+", " ", tools::file_path_sans_ext(src$name))))
    tmpl <- tryCatch(draft_template(src$path, bank = if (nzchar(bankguess)) bankguess else "New bank"),
                     error = function(e) NULL)
    if (is.null(tmpl)) { showNotification("Couldn't auto-detect this file type — use the Template/PDF wizard.", type = "error"); return() }
    guided(list(path = src$path, name = src$name, tmpl = tmpl))
    show_guided_modal()
  })

  guided_live <- reactive({ g <- guided(); req(g)
    apply_overrides(g$tmpl, input$g_bank, input$g_date, input$g_sign) })
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
    ok <- tryCatch({ save_user_template(guided_live(), USER_TEMPLATES_DIR); TRUE }, error = function(e) FALSE)
    if (isTRUE(ok)) {
      tpl_bump(isolate(tpl_bump()) + 1); removeModal()
      showNotification("Saved as your template. Click Convert again to run this statement with it.",
                       type = "message", duration = 8)
    } else showNotification("Couldn't save — adjust the settings and try again.", type = "error")
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
      if (is.null(df)) df <- data.frame(message = "no batch run")
      utils::write.csv(df[, setdiff(names(df), "path"), drop = FALSE], file, row.names = FALSE)
    })
}

shinyApp(ui, server)
