# parse.R -- map a read table through a template into the canonical schema.
# Guarantees: verbatim descriptions, redactions honoured, NO silent drops
# (every non-empty data row becomes a transaction; malformed rows are flagged).

# .col_source(template, field) -- the source column name for a canonical field,
# or NULL when the template maps it to null / omits it.
.col_source <- function(template, field) {
  spec <- template$columns[[field]]
  if (is.null(spec)) return(NULL)
  if (is.list(spec)) return(spec$source)
  as.character(spec)
}

# .pick(tbl, name) -- fetch a column by name as character, or NULL if absent.
.pick <- function(tbl, name) {
  if (is.null(name)) return(NULL)
  if (!(name %in% names(tbl))) return(NULL)
  as.character(tbl[[name]])
}

REDACTION_TOKEN <- "[REDACTED]"

.is_redacted <- function(x) {
  x <- as.character(x)
  !is.na(x) & grepl("REDACTED", x, ignore.case = TRUE)
}

# parse_statement(input, template) -> list(transactions, extras, header, provenance)
parse_statement <- function(input, template) {
  reader <- switch(template$format %||% "delimited",
    delimited = read_delimited(input, template),
    excel     = list(table = input$table, source_lines = integer(0),
                     source_spans = list(), raw = character(0),
                     field_counts = integer(0), expected_fields = NA_integer_,
                     n_data_lines = NA_integer_),
    # PDF text is extracted (pages/word boxes/sections via read_pdf) but
    # per-bank transaction-table parsing is future work: degrade to an empty
    # table so reconciliation reports needs_review, never crash.
    pdf       = list(table = NULL, source_lines = integer(0),
                     source_spans = list(), raw = character(0),
                     field_counts = integer(0), expected_fields = NA_integer_,
                     n_data_lines = NA_integer_),
    stop(sprintf("parse_statement: unsupported format '%s'", template$format))
  )
  tbl <- reader$table
  if (is.null(tbl)) tbl <- data.frame()
  n <- nrow(tbl)

  # ---- date ----
  date_src <- .col_source(template, "date")
  date_col <- .pick(tbl, date_src)
  date_fmt <- template$columns$date$format %||% "%Y-%m-%d"
  if (is.null(date_col)) {
    date_iso <- rep(NA_character_, n); date_raw <- rep(NA_character_, n)
  } else {
    d <- parse_date(date_col, date_fmt)
    date_iso <- d$iso; date_raw <- d$raw
  }

  # ---- amount (sign handling per template$amount_sign) ----
  style <- template$amount_sign %||% "signed"
  amt_opts <- list()
  if (style == "signed") {
    amt_col <- .pick(tbl, .col_source(template, "amount"))
  } else if (style == "debit_credit_cols") {
    amt_col <- rep(NA_character_, n)
    amt_opts$debit  <- .pick(tbl, .col_source(template, "debit"))
    amt_opts$credit <- .pick(tbl, .col_source(template, "credit"))
  } else if (style == "dr_cr_suffix") {
    amt_col <- .pick(tbl, .col_source(template, "amount"))
  } else if (style == "type_dc") {
    amt_col <- .pick(tbl, .col_source(template, "amount"))
    amt_opts$type <- .pick(tbl, .col_source(template, "type"))
    amt_opts$type_debit_value <- template$type_debit_value %||% "D"
  } else {
    amt_col <- .pick(tbl, .col_source(template, "amount"))
  }
  if (is.null(amt_col)) amt_col <- rep(NA_character_, n)
  a <- parse_amount(amt_col, style, amt_opts)

  # ---- description (verbatim) ----
  desc_col <- .pick(tbl, .col_source(template, "description"))
  description <- if (is.null(desc_col)) rep(NA_character_, n) else clean_description(desc_col)

  # ---- NZ / verbatim fields (blank -> NA) ----
  get_verbatim <- function(field) {
    v <- .pick(tbl, .col_source(template, field))
    if (is.null(v)) rep(NA_character_, n) else blank_to_na(v)
  }
  particulars <- get_verbatim("particulars")
  code        <- get_verbatim("code")
  reference   <- get_verbatim("reference")
  other_party <- get_verbatim("other_party")
  type        <- get_verbatim("type")

  # ---- balance (optional) ----
  bal_src <- .col_source(template, "balance")
  bal_col <- .pick(tbl, bal_src)
  if (is.null(bal_col)) {
    balance <- rep(NA_real_, n); balance_raw <- rep(NA_character_, n)
  } else {
    b <- parse_amount(bal_col, "signed")
    balance <- b$value; balance_raw <- as.character(bal_col)
  }

  currency <- rep(template$currency %||% "NZD", n)

  # ---- extras (template-declared per-bank columns, keyed by row_id) ----
  extras <- .build_extras(template, tbl, n)
  # foreign-currency amount (if mapped) drives the `fx` flag.
  fx_present <- if (!is.null(extras[["fx_amount"]]))
    !is.na(extras[["fx_amount"]]) & nzchar(trimws(as.character(extras[["fx_amount"]])))
  else rep(FALSE, n)

  # ---- flags ----
  flags <- vapply(seq_len(n), function(i) {
    f <- character(0)
    amt_red <- .is_redacted(a$raw[i]) || .is_redacted(description[i])
    if (amt_red) f <- c(f, "redacted")
    fc <- if (i <= length(reader$field_counts)) reader$field_counts[i] else NA_integer_
    exp <- reader$expected_fields
    malformed <- (!is.na(fc) && !is.na(exp) && fc != exp) ||
                 (is.na(a$value[i]) && !amt_red && !is.blank_amount(amt_col[i]))
    if (malformed) f <- c(f, "malformed")
    if (isTRUE(fx_present[i])) f <- c(f, "fx")
    paste(f, collapse = ",")
  }, character(1))
  # redacted amounts must not carry a derived value.
  a$value[.is_redacted(a$raw)] <- NA_real_

  core <- data.frame(
    row_id = seq_len(n), date = date_iso, date_raw = date_raw,
    description = description, amount = a$value, amount_raw = as.character(amt_col),
    direction = a$direction, balance = balance, balance_raw = balance_raw,
    particulars = particulars, code = code, reference = reference,
    other_party = other_party, type = type, currency = currency, flags = flags,
    stringsAsFactors = FALSE
  )
  core <- coerce_core(core)

  page_count <- if (identical(input$kind, "pdf")) (input$meta$page_count %||% NA_integer_) else NA_integer_
  header <- list(
    bank = template$bank %||% NA_character_,
    statement_type = template$statement_type %||% NA_character_,
    template_id = template$id %||% NA_character_,
    template_version = template$version %||% NA,
    account_number = NA_character_, account_name = NA_character_,
    period_start = NA_character_, period_end = NA_character_,
    opening_balance = NA_real_, closing_balance = NA_real_,
    currency = template$currency %||% "NZD",
    source_file = basename(input$path), source_sha256 = input$sha256,
    page_count = page_count, row_count = n
  )

  # Provenance is per PARSED ROW: a multi-line quoted record maps to one row and
  # records its full physical line span, so the audit trail survives embedded
  # newlines instead of being blanked for the whole statement.
  spans <- reader$source_spans %||% list()
  raw <- reader$raw %||% rep(NA_character_, n)
  source_ref <- if (length(spans) == n) {
    vapply(spans, .format_line_span, character(1))
  } else if (length(reader$source_lines %||% integer(0)) == n) {
    sprintf("csv:line=%d", reader$source_lines)
  } else rep(NA_character_, n)
  provenance <- data.frame(
    row_id = seq_len(n),
    source_ref = source_ref,
    raw = if (length(raw) == n) raw else rep(NA_character_, n),
    stringsAsFactors = FALSE
  )

  list(transactions = core, extras = extras, header = header,
       provenance = provenance,
       # completeness accounting for reconcile: how many non-empty physical data
       # lines the source held (vs how many rows we parsed).
       source_line_count = reader$n_data_lines %||% NA_integer_)
}

# .format_line_span(v) -- "csv:line=5" for a single physical line, "csv:line=5-6"
# for a contiguous span, "csv:line=5,8" otherwise.
.format_line_span <- function(v) {
  v <- as.integer(v)
  if (length(v) == 0 || all(is.na(v))) return(NA_character_)
  if (length(v) == 1) return(sprintf("csv:line=%d", v))
  if (identical(v, seq.int(v[1], v[length(v)])))
    return(sprintf("csv:line=%d-%d", v[1], v[length(v)]))
  sprintf("csv:line=%s", paste(v, collapse = ","))
}

# .build_extras(template, tbl, n) -- honour a template `extras:` block, building
# a row_id-keyed data.frame from the mapped source columns. Missing source
# columns become NA (never silently omitted). Returns the empty row_id frame when
# no extras are declared.
.build_extras <- function(template, tbl, n) {
  spec <- template$extras
  if (is.null(spec) || length(spec) == 0) {
    return(data.frame(row_id = integer(0), stringsAsFactors = FALSE))
  }
  out <- list(row_id = seq_len(n))
  for (field in names(spec)) {
    fs <- spec[[field]]
    colname <- if (is.list(fs)) fs$source else as.character(fs)
    v <- .pick(tbl, colname)
    out[[field]] <- if (is.null(v)) rep(NA_character_, n) else blank_to_na(v)
  }
  data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

# is.blank_amount(x) -- TRUE when an amount cell is truly empty (so a NA value
# there is expected, not malformed).
is.blank_amount <- function(x) {
  is.na(x) || !nzchar(trimws(as.character(x)))
}
