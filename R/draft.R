# draft.R -- "generate as much of the template as possible" from a single file,
# so an accountant who hits an unsupported statement only has to CONFIRM, not
# build. draft_template() reads the file, auto-detects everything it can, and
# returns a ready template list (origin "user"). The guided UI shows a live
# preview from it and lets the user fix at most one or two things, then Save.

.draft_id <- function(path) gsub("[^a-z0-9]+", "_", tolower(tools::file_path_sans_ext(basename(path))))

# .guess_pdf_date_format(input, band) -- sniff the date column's style. Handles
# the common year-less "02 May" case (year comes from the period) plus the usual
# full-date shapes.
.guess_pdf_date_format <- function(input, band) {
  wl <- input$words %||% list(); if (!length(wl)) return("%d/%m/%Y")
  toks <- character(0)
  for (w in wl) {
    if (is.null(w) || !nrow(w)) next
    w <- as.data.frame(w, stringsAsFactors = FALSE)
    cx <- w$x + w$width / 2
    toks <- c(toks, w$text[cx >= band$x_min & cx <= band$x_max])
  }
  toks <- trimws(toks[nzchar(toks)])
  if (!length(toks)) return("%d/%m/%Y")
  # group day + month-name tokens ("02","May") -> treat as "%d %b"
  joined <- paste(utils::head(toks, 30), collapse = " ")
  if (grepl("[0-9]{1,2}\\s+[A-Za-z]{3,9}", joined) && !grepl("[0-9]{1,2}[/.-][0-9]", joined))
    return("%d %b")
  f <- detect_date_format(toks)
  if (nzchar(f)) f else "%d/%m/%Y"
}

.draft_delimited <- function(path, id, bank) {
  delim <- detect_delimiter(path)
  df <- tryCatch(utils::read.csv(path, sep = if (identical(delim, "\t")) "\t" else delim,
    colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE, nrows = 50L),
    error = function(e) NULL)
  h <- if (!is.null(df)) names(df) else character(0)
  mapcol <- function(field) { c <- guess_mapping(h, field); if (identical(c, "(none)")) NULL else c }
  cols <- list()
  dcol <- mapcol("date")
  if (!is.null(dcol)) {
    fmt <- if (!is.null(df)) detect_date_format(df[[dcol]]) else ""
    cols$date <- list(source = dcol, format = if (nzchar(fmt)) fmt else "%d/%m/%Y")
  }
  for (f in c("amount", "description", "particulars", "code", "reference", "type", "other_party", "balance")) {
    cc <- mapcol(f); if (!is.null(cc)) cols[[f]] <- list(source = cc)
  }
  style <- detect_amount_style(h, df)
  if (identical(style, "debit_credit_cols")) {
    dc <- h[grepl("debit|withdrawal|money out|paid out", tolower(h))][1]
    cc <- h[grepl("credit|deposit|money in|paid in", tolower(h))][1]
    if (!is.na(dc)) cols$debit <- list(source = dc)
    if (!is.na(cc)) cols$credit <- list(source = cc)
  }
  list(id = paste0(id, "_csv"), bank = bank, statement_type = "everyday", format = "delimited",
    version = 1, min_score = max(1L, length(h)),
    fingerprint = list(header_contains_all = as.list(h)), delimiter = delim,
    columns = cols, amount_sign = style, currency = "NZD", origin = "user")
}

.draft_pdf <- function(input, id, bank) {
  sug <- suggest_pdf_columns(input)
  if (!nrow(sug)) return(NULL)
  # two money columns (besides balance) => Withdrawals/Deposits, not one signed amount
  amt_rows <- which(sug$field == "amount")
  style <- "signed"
  if (length(amt_rows) == 2) {
    sug$field[amt_rows[1]] <- "debit"; sug$field[amt_rows[2]] <- "credit"
    style <- "debit_credit_cols"
  }
  cols <- list()
  for (i in seq_len(nrow(sug))) cols[[sug$field[i]]] <- list(x_min = sug$x_min[i], x_max = sug$x_max[i])
  date_band <- if (!is.null(cols$date)) cols$date else list(x_min = 0, x_max = 90)
  fp <- header_phrases(input); if (!length(fp)) fp <- "Balance"
  list(id = paste0(id, "_pdf"), bank = bank, statement_type = "statement", format = "pdf",
    version = 1, min_score = max(1L, length(fp)),
    fingerprint = list(page_contains_all = as.list(fp)),
    table = list(row_tol = 3, date_format = .guess_pdf_date_format(input, date_band),
      amount_sign = style, columns = cols), currency = "NZD", origin = "user")
}

# draft_template(path, bank) -> a template list (or NULL if unsupported kind).
draft_template <- function(path, bank = "New bank") {
  input <- tryCatch(read_input(path), error = function(e) NULL)
  if (is.null(input)) return(NULL)
  id <- .draft_id(path)
  if (identical(input$kind, "delimited")) return(.draft_delimited(path, id, bank))
  if (identical(input$kind, "pdf"))       return(.draft_pdf(input, id, bank))
  NULL   # excel: use the Template wizard (needs sheet-aware mapping)
}

# draft_preview(path, template) -> the parsed transactions from a draft, or NULL.
# Lets the guided UI show "here's what we'll pull out -- does this look right?"
draft_preview <- function(path, template) {
  tryCatch({
    parsed <- parse_statement(read_input(path), template)
    parsed$transactions
  }, error = function(e) NULL)
}
