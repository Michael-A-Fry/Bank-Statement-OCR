# draft.R -- "generate as much of the template as possible" from a single file,
# so an accountant who hits an unsupported statement only has to CONFIRM, not
# build. draft_template() reads the file, auto-detects everything it can, and
# returns a ready template list (origin "user"). The guided UI shows a live
# preview from it and lets the user fix at most one or two things, then Save.

.draft_id <- function(path) gsub("[^a-z0-9]+", "_", tolower(tools::file_path_sans_ext(basename(path))))

# .slug(s) -- a safe lower-snake token for building template ids.
.slug <- function(s) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(s %||% ""))))

# .compose_id(bank, type, suffix, fallback) -- an id that SAYS what the template is
# ("anz_everyday_pdf") instead of a raw filename. Falls back to the filename slug
# when the bank is still the generic placeholder, so an id is always produced.
.compose_id <- function(bank, type, suffix, fallback) {
  parts <- Filter(nzchar, c(.slug(bank), .slug(type)))
  if (!length(parts) || identical(.slug(bank), "new_bank"))
    parts <- Filter(nzchar, .slug(fallback))
  out <- paste(c(parts, suffix), collapse = "_")
  if (!nzchar(.slug(out %||% ""))) out <- paste0("statement_", suffix)
  out
}

# .sniff_bank(input, fallback) -- a friendlier default NAME for a new template,
# recognised from the document's own text when it names a common NZ bank. This is
# a DISPLAY + naming hint only (pre-fills the "Which bank?" box, which the user
# confirms/edits); detection still matches on the template's fingerprint phrases,
# never on this. Falls back to the caller's filename-derived guess.
.KNOWN_BANKS <- c(anz = "ANZ", asb = "ASB", bnz = "BNZ", kiwibank = "Kiwibank",
  westpac = "Westpac", tsb = "TSB", sbs = "SBS", cooperative = "Co-operative Bank",
  "co-operative" = "Co-operative Bank", heartland = "Heartland", rabobank = "Rabobank",
  "the co-operative" = "Co-operative Bank")
.sniff_bank <- function(input, fallback = "New bank") {
  txt <- tolower(paste(unlist(c(input$text, input$pages,
    if (!is.null(input$table)) names(input$table))), collapse = " "))
  # Keywords are plain words (the hyphen in "co-operative" is literal outside a
  # character class), so a word-boundary match needs no metacharacter escaping.
  if (nzchar(txt)) for (kw in names(.KNOWN_BANKS)) {
    if (grepl(paste0("\\b", kw, "\\b"), txt)) return(.KNOWN_BANKS[[kw]])
  }
  fallback
}

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

# .draft_type_dc(style, headers, df, cols) -- when the amount style is a D/C
# indicator column, pin BOTH the indicator column and the token that means a
# debit, so a drafted template never falls back to the blind "D" default (which
# silently mis-signs a bank that writes "d" / "DR" / "Debit"). Returns the
# (possibly-augmented) columns plus the type_debit_value / type_credit_value keys
# to fold onto the template. A no-op for every other style.
.draft_type_dc <- function(style, headers, df, cols) {
  if (!identical(style, "type_dc")) return(list(cols = cols, keys = list()))
  tv <- detect_type_dc_values(headers, df)
  # Map the indicator column when the generic name-match missed it (a header like
  # "debit_credit" doesn't contain "type").
  if (!is.null(tv$column) && is.null(cols$type)) cols$type <- list(source = tv$column)
  keys <- list()
  if (!is.null(tv$debit))  keys$type_debit_value  <- tv$debit
  if (!is.null(tv$credit)) keys$type_credit_value <- tv$credit
  list(cols = cols, keys = keys)
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
  tdc <- .draft_type_dc(style, h, df, cols)
  cols <- tdc$cols
  out <- list(id = .compose_id(bank, "everyday", "csv", id), bank = bank, statement_type = "everyday", format = "delimited",
    version = 1, min_score = max(1L, length(h)),
    fingerprint = list(header_contains_all = as.list(h)), delimiter = delim,
    columns = cols, amount_sign = style, currency = "NZD", origin = "user")
  utils::modifyList(out, tdc$keys)
}

.draft_pdf <- function(input, id, bank) {
  sug <- safe(suggest_pdf_columns(input), data.frame())
  style <- "signed"
  cols <- list()
  if (!is.null(sug) && nrow(sug)) {
    # two money columns (besides balance) => Withdrawals/Deposits, not one signed amount
    amt_rows <- which(sug$field == "amount")
    if (length(amt_rows) == 2) {
      sug$field[amt_rows[1]] <- "debit"; sug$field[amt_rows[2]] <- "credit"
      style <- "debit_credit_cols"
    }
    for (i in seq_len(nrow(sug))) cols[[sug$field[i]]] <- list(x_min = sug$x_min[i], x_max = sug$x_max[i])
  }
  # Skeleton fallback: even when the auto-sniffer recognises nothing (an unusual
  # layout, or a page whose money/date shapes it didn't match), STILL open the
  # toolkit with sensible starter bands the user can drag into place -- a readable
  # PDF must never dead-end with "couldn't read this file". Only missing essentials
  # are filled, so a good auto-detection is left untouched.
  page_w <- suppressWarnings(as.numeric((input$page_width %||% NA)[1]))
  if (is.na(page_w) || page_w <= 0) page_w <- 595.28
  if (is.null(cols$date))        cols$date        <- list(x_min = 0, x_max = round(page_w * 0.16))
  if (is.null(cols$description)) cols$description <- list(x_min = round(page_w * 0.17), x_max = round(page_w * 0.55))
  if (style != "debit_credit_cols" && is.null(cols$amount) &&
      is.null(cols$debit) && is.null(cols$credit))
    cols$amount <- list(x_min = round(page_w * 0.72), x_max = round(page_w * 0.95))
  date_band <- cols$date
  # Fingerprint: prefer the distinctive multi-word phrases header_phrases now
  # returns. NEVER fall back to a bare generic word like "Balance" (it sits on
  # essentially every statement, so the template would match unseen PDFs and turn
  # a correct "unsupported" verdict into a silently-wrong parse). When nothing
  # distinctive is found, leave whatever was found (even empty) -- validate_template
  # then makes the analyst add a specific phrase before the template can save.
  fp <- safe(header_phrases(input), character(0))
  # Record the page size the bands were drawn in, so a differently-sized copy of
  # the statement (a rescan, another export) is normalised to this space at parse
  # time instead of dropping rows. Falls back to A4 when the size is unknown.
  ref_w <- suppressWarnings(as.numeric((input$page_width  %||% NA)[1]))
  ref_h <- suppressWarnings(as.numeric((input$page_height %||% NA)[1]))
  tbl <- list(row_tol = 3,
      date_format = safe(.guess_pdf_date_format(input, date_band), "%d/%m/%Y") %||% "%d/%m/%Y",
      amount_sign = style, columns = cols)
  if (!is.na(ref_w) && ref_w > 0) tbl$ref_width  <- round(ref_w, 2)
  if (!is.na(ref_h) && ref_h > 0) tbl$ref_height <- round(ref_h, 2)
  list(id = .compose_id(bank, "statement", "pdf", id), bank = bank, statement_type = "statement", format = "pdf",
    version = 1, min_score = max(1L, length(fp)),
    fingerprint = list(page_contains_all = as.list(fp)),
    table = tbl, currency = "NZD", origin = "user")
}

# .draft_excel -- the sheet-aware Excel draft. read_input has already picked the
# sheet, skipped any preamble and fixed serial dates, so drafting mirrors the
# delimited path: map the headers, sniff the date format and amount style.
# Returns NULL (honest "can't draft") when there's no date or no money column.
.draft_excel <- function(input, id, bank) {
  df <- input$table
  if (is.null(df) || !nrow(df) || !ncol(df)) return(NULL)
  h <- names(df)
  mapcol <- function(field) { c <- guess_mapping(h, field); if (identical(c, "(none)")) NULL else c }
  cols <- list()
  dcol <- mapcol("date")
  if (is.null(dcol)) return(NULL)   # no date column -> not a transaction table
  fmt <- detect_date_format(df[[dcol]])
  cols$date <- list(source = dcol, format = if (nzchar(fmt)) fmt else "%Y-%m-%d")
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
  tdc <- .draft_type_dc(style, h, df, cols)
  cols <- tdc$cols
  if (is.null(cols$amount) && is.null(cols$debit)) return(NULL)   # nothing to read money from
  out <- list(id = .compose_id(bank, "everyday", "xlsx", id), bank = bank, statement_type = "everyday", format = "excel",
    version = 1, min_score = max(1L, length(h)),
    fingerprint = list(header_contains_all = as.list(h)),
    columns = cols, amount_sign = style, currency = "NZD", origin = "user")
  utils::modifyList(out, tdc$keys)
}

# draft_template(path, bank) -> a template list (or NULL if unsupported kind).
draft_template <- function(path, bank = "New bank") {
  input <- tryCatch(read_input(path), error = function(e) NULL)
  if (is.null(input)) return(NULL)
  id <- .draft_id(path)
  # Recognise the bank from the document itself for a friendlier default name/id
  # (naming hint only -- see .sniff_bank). Keeps the caller's guess otherwise.
  bank <- .sniff_bank(input, fallback = bank)
  if (identical(input$kind, "delimited")) return(.draft_delimited(path, id, bank))
  if (identical(input$kind, "pdf"))       return(.draft_pdf(input, id, bank))
  if (identical(input$kind, "excel"))     return(.draft_excel(input, id, bank))
  NULL
}

# draft_preview(path, template) -> the human-facing transactions from a draft, or
# NULL. Lets the guided UI show "here's what we'll pull out -- does this look
# right?". Uses the same display shaping as the outputs (no verbatim *_raw noise,
# debit/credit surfaced when the statement splits them) so the toolkit preview
# matches exactly what the workbook / CSV will contain.
draft_preview <- function(path, template) {
  tryCatch({
    parsed <- parse_statement(read_input(path), template)
    display_transactions(parsed$transactions, parsed$extras)
  }, error = function(e) NULL)
}
