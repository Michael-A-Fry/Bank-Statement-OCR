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

TEMPLATES_DIR <- "templates"
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
          h4("Checks"), DTOutput("cv_kpis"),
          h4("Transactions (preview)"), DTOutput("cv_txns")
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
          fileInput("wz_file", "Sample statement (delimited)"),
          textInput("wz_delim", "Delimiter", value = ","),
          textInput("wz_id", "Template id", value = "newbank_everyday_csv"),
          textInput("wz_bank", "Bank", value = "NewBank"),
          textInput("wz_type", "Statement type", value = "everyday"),
          selectInput("wz_amount_sign", "Amount style",
                      c("signed", "debit_credit_cols", "dr_cr_suffix", "type_dc")),
          textInput("wz_datefmt", "Date format (strptime)", value = "%d/%m/%Y"),
          textInput("wz_currency", "Currency", value = "NZD"),
          actionButton("wz_preview", "Preview parse", class = "btn-primary"),
          actionButton("wz_save", "Save template"),
          br(), br(), uiOutput("wz_msg")
        ),
        mainPanel(
          width = 8,
          h4("Map each field to a source column"),
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
          tags$li("Use the Template wizard tab: upload a sample, map the columns, Preview, Save."),
          tags$li("Or copy a templates/*.yaml file and edit the columns map."),
          tags$li("See tests/HOWTO-add-template-test.md to add a golden test.")
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

  templates <- reactive({ load_templates(TEMPLATES_DIR) })

  output$cv_bank_ui <- renderUI({
    banks <- sort(unique(vapply(templates(), function(t) t$bank %||% "", character(1))))
    selectInput("cv_bank", "Bank (optional)", c("(auto-detect)", banks))
  })

  cv_res <- reactiveVal(NULL)
  cv_dir <- reactiveVal(NULL)

  observeEvent(input$cv_go, {
    req(input$cv_file)
    sess <- file.path(tempdir(), paste0("cv_", as.integer(runif(1, 1, 1e9))))
    dir.create(sess, showWarnings = FALSE, recursive = TRUE)
    src <- file.path(sess, input$cv_file$name)
    file.copy(input$cv_file$datapath, src, overwrite = TRUE)
    bank <- if (is.null(input$cv_bank) || input$cv_bank == "(auto-detect)") NULL else input$cv_bank
    res <- tryCatch(
      convert_statement(src, bank = bank, outdir = sess,
                        templates_dir = TEMPLATES_DIR, requested_by = "shiny"),
      error = function(e) list(status = "failed",
                               messages = paste("error:", conditionMessage(e))))
    cv_res(res); cv_dir(sess)
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

  # ---- Wizard -------------------------------------------------------
  wz_headers <- reactive({
    req(input$wz_file)
    read_headers(input$wz_file$datapath, input$wz_delim %||% ",")
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
      TRUE
    }, error = function(e) { attr(ok, "err") <<- conditionMessage(e); FALSE })
    output$wz_msg <- renderUI(
      if (isTRUE(ok)) span(class = "ok", paste("Saved", path, "- add a golden test next (see Help)."))
      else span(class = "bad", "Save failed - check the mappings."))
  })
}

shinyApp(ui, server)
