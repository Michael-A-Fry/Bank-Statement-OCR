# parse.R -- map a read table through a template into the canonical schema. Guarantees: verbatim
# descriptions, redactions honoured, NO silent drops (malformed rows are flagged, never dropped).

# The source column name for a canonical field, or NULL when unmapped.
.col_source <- function(template, field) {
  spec <- template$columns[[field]]
  if (is.null(spec)) return(NULL)
  if (is.list(spec)) return(spec$source)
  as.character(spec)
}

# Fetch a column by name as character, or NULL. Exact match first, then a UNIQUE case-insensitive
# match, so a bank flipping its header casing does not silently blank the column.
.pick <- function(tbl, name) {
  if (is.null(name)) return(NULL)
  if (name %in% names(tbl)) return(as.character(tbl[[name]]))
  hit <- which(tolower(names(tbl)) == tolower(name))
  if (length(hit) == 1) return(as.character(tbl[[hit]]))
  NULL
}

# Statement-level metadata for a delimited or excel file, mined from ANY preamble ABOVE the table.
# Reads only the preamble, never the rows, so a stray date pair or money value in the data can never
# be misread as a period or balance. NULL when there is no preamble.
.delimited_meta <- function(input, reader) {
  pre <- switch(input$kind %||% "",
    delimited = {
      hl <- reader$header_line_no %||% NA_integer_
      if (is.na(hl) || hl <= 1L) character(0) else (input$lines %||% character(0))[seq_len(hl - 1L)]
    },
    excel = input$meta$preamble %||% character(0),
    character(0))
  pre <- pre[!is.na(pre) & nzchar(trimws(pre))]
  if (!length(pre)) return(NULL)
  # A minimal PDF-free shim: extract_metadata treats `pages` as the text to mine.
  shim <- list(kind = input$kind %||% "delimited", pages = pre, meta = list(), path = input$path)
  safe(extract_metadata(shim), NULL)
}

REDACTION_TOKEN <- "[REDACTED]"

.is_redacted <- function(x) {
  x <- as.character(x)
  !is.na(x) & grepl("REDACTED", x, ignore.case = TRUE)
}

# parse_statement(input, template) -> list(transactions, extras, header, provenance)
parse_statement <- function(input, template, force_rows = NULL, meta = NULL) {
  # PDF templates parse straight from positioned word boxes. `meta` lets a caller that already ran
  # extract_metadata(input) hand it in so the PDF parser does not repeat the identical scan.
  if (identical(template$format %||% "delimited", "pdf"))
    return(parse_pdf_table(input, template, force_rows = force_rows, meta = meta))
  reader <- switch(template$format %||% "delimited",
    delimited = read_delimited(input, template),
    excel     = list(table = input$table, source_lines = integer(0),
                     source_spans = list(), raw = character(0),
                     field_counts = integer(0), expected_fields = NA_integer_,
                     n_data_lines = NA_integer_),
    # PDF text is extracted but per-bank table parsing on this branch is future work: degrade to an
    # empty table so reconciliation reports needs_review, never crash.
    pdf       = list(table = NULL, source_lines = integer(0),
                     source_spans = list(), raw = character(0),
                     field_counts = integer(0), expected_fields = NA_integer_,
                     n_data_lines = NA_integer_),
    stop(sprintf("parse_statement: unsupported format '%s'", template$format))
  )
  tbl <- reader$table
  if (is.null(tbl)) tbl <- data.frame()
  n <- nrow(tbl)

  # A template MAY declare SEVERAL candidate date formats. resolve_date_format picks the ONE that
  # reads EVERY non-empty cell - all-or-nothing, never a per-row mixture, because "03/04" is readable
  # as both 3 April and 4 March. With no candidate every date is NA and dates_readable fails loudly.
  date_src <- .col_source(template, "date")
  date_col <- .pick(tbl, date_src)
  date_fmt <- resolve_date_format(date_col, template$columns$date$format %||% "%Y-%m-%d")
  if (is.null(date_col) || is.na(date_fmt)) {
    date_iso <- rep(NA_character_, n)
    date_raw <- if (is.null(date_col)) rep(NA_character_, n) else as.character(date_col)
  } else {
    d <- parse_date(date_col, date_fmt)
    date_iso <- d$iso; date_raw <- d$raw
  }

  # ---- amount (sign handling per template$amount_sign) ----
  style <- template$amount_sign %||% "signed"
  # decimal_mark: dot | comma | auto, so a European template can declare its locale.
  dec <- template$decimal_mark %||% "auto"
  amt_opts <- list(decimal = dec, unsigned_default = template$unsigned_default %||% "debit")
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
    amt_opts$type_debit_value  <- template$type_debit_value %||% "D"
    # Optional credit token: when declared, parse_amount fails closed on any indicator that is
    # neither debit nor credit.
    amt_opts$type_credit_value <- template$type_credit_value
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
    b <- parse_amount(bal_col, "signed", list(decimal = dec))
    balance <- b$value; balance_raw <- as.character(bal_col)
  }

  currency <- rep(template$currency %||% "NZD", n)

  # ---- extras (template-declared per-bank columns, keyed by row_id) ----
  extras <- .build_extras(template, tbl, n)
  # Separate money-in / money-out statements keep the verbatim debit and credit cells beside the
  # collapsed signed amount, so the original split stays visible. Mirrors the PDF path.
  if (identical(style, "debit_credit_cols") && n > 0) {
    db <- blank_to_na(as.character(amt_opts$debit  %||% rep(NA_character_, n)))
    cr <- blank_to_na(as.character(amt_opts$credit %||% rep(NA_character_, n)))
    if (is.null(extras) || nrow(extras) != n)
      extras <- data.frame(row_id = seq_len(n), stringsAsFactors = FALSE)
    if (is.null(extras[["debit"]]))  extras[["debit"]]  <- db
    if (is.null(extras[["credit"]])) extras[["credit"]] <- cr
  }
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
    # An amount that did not come out as a number is malformed WHETHER the cell held unreadable text
    # or nothing at all: exempting a blank amount cell left such a row with no flag while the PDF
    # path flags it. A REDACTED amount is still not malformed - it was deliberately withheld.
    malformed <- (!is.na(fc) && !is.na(exp) && fc != exp) ||
                 (is.na(a$value[i]) && !amt_red)
    if (malformed) f <- c(f, "malformed")
    if (isTRUE(fx_present[i])) f <- c(f, "fx")
    paste(f, collapse = ",")
  }, character(1))
  # Redacted amounts must not carry a derived value.
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
  # Statement-level metadata from the preamble. NULL when there is none, so a bare transaction table
  # keeps its all-NA header and reconciliation leans on running_balance_continuity.
  md <- .delimited_meta(input, reader)
  header <- list(
    bank = template$bank %||% NA_character_,
    statement_type = template$statement_type %||% NA_character_,
    template_id = template$id %||% NA_character_,
    template_version = template$version %||% NA,
    account_number = md$account_number %||% NA_character_,
    account_name   = md$account_name %||% NA_character_,
    period_start = md$period_start %||% NA_character_,
    period_end   = md$period_end %||% NA_character_,
    opening_balance = suppressWarnings(as.numeric(md$opening_balance %||% NA)),
    closing_balance = suppressWarnings(as.numeric(md$closing_balance %||% NA)),
    currency = template$currency %||% "NZD",
    source_file = basename(input$path), source_sha256 = input$sha256,
    page_count = page_count, row_count = n,
    stated_count = md$stated_count %||% NA_integer_
  )

  # Provenance is per PARSED ROW: a multi-line quoted record maps to one row and records its full
  # physical line span, so the audit trail survives embedded newlines.
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

  # Physical data lines legitimately consumed by MULTI-LINE records. The completeness KPI subtracts
  # these so a valid multi-line statement does not look like it lost lines.
  multiline_extra <- if (length(spans) == n && n > 0)
    as.integer(sum(lengths(spans)) - length(spans)) else 0L

  list(transactions = core, extras = extras, header = header,
       provenance = provenance,
       # Completeness accounting: non-empty physical data lines the source held, against rows parsed.
       source_line_count = reader$n_data_lines %||% NA_integer_,
       # Spreadsheet padding lines not turned into transactions. Carried so the omission can be
       # STATED, and excluded from source_line_count so the completeness proof compares like with like.
       padding_line_count = reader$n_padding_lines %||% 0L,
       multiline_extra = multiline_extra)
}

# "csv:line=5", "csv:line=5-6" for a contiguous span, "csv:line=5,8" otherwise.
.format_line_span <- function(v) {
  v <- as.integer(v)
  if (length(v) == 0 || all(is.na(v))) return(NA_character_)
  if (length(v) == 1) return(sprintf("csv:line=%d", v))
  if (identical(v, seq.int(v[1], v[length(v)])))
    return(sprintf("csv:line=%d-%d", v[1], v[length(v)]))
  sprintf("csv:line=%s", paste(v, collapse = ","))
}

# Honour a template `extras:` block. Missing source columns become NA, never silently omitted.
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
