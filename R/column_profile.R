# column_profile.R -- "everything you'd need to DRAFT a template", captured
# PII-safely. When the engine can't match a statement, the single most useful
# thing to record is a STRUCTURAL description of its source columns plus the
# engine's own best-guess mapping, so a human -- or an AI assistant -- can turn it
# into a template WITHOUT ever seeing statement content.
#
# PII posture (identical to the rest of metadata capture): no raw values leave
# here. A column yields its NAME, inferred KIND, fill rate, cardinality, and a
# MASKED example shape (each digit -> 9, each letter -> A, punctuation kept, so
# "31/12/2025" -> "99/99/9999" and "$1,234.56" -> "$9,999.99"). The only literal
# tokens emitted are the distinct values of a genuinely low-cardinality, short
# INDICATOR column (e.g. {D, C} or {Paid, Recd}) -- structural markers, not
# content, exactly the posture of novelty.unrecognised_type_values.

# .mask_value(s) -- collapse a value to a PII-safe shape. Digit -> 9, letter -> A,
# everything else (/,.-() space $ £ €) kept, so the RESULT reveals structure
# (widths, separators, currency) but never a readable character of content.
.mask_value <- function(s) {
  s <- as.character(s)
  s <- gsub("[0-9]", "9", s)
  gsub("[A-Za-z]", "A", s)
}

# .modal_shape(v) -- the most common masked shape across a sample (capped in
# length so a long free-text field can't bloat the record).
.modal_shape <- function(v) {
  v <- v[nzchar(v)]
  if (!length(v)) return(NA_character_)
  shapes <- vapply(utils::head(v, 100L), .mask_value, character(1), USE.NAMES = FALSE)
  ms <- names(sort(table(shapes), decreasing = TRUE))[1]
  if (is.na(ms)) return(NA_character_)
  if (nchar(ms) > 48L) paste0(substr(ms, 1L, 45L), "...") else ms
}

# A character is "currency-ish" if it is NOT a digit, letter, standard number
# punctuation or space. This ASCII-only negation matches $ / £ / € (and any other
# currency glyph) WITHOUT embedding a multibyte character in the pattern -- raw
# £/€ in a regex warn and can mis-match in a non-UTF-8 locale (the air-gapped
# Windows box), so every money test here stays ASCII.
.CURRENCY_RX <- "[^0-9A-Za-z.,'()+[:space:]-]"

# .looks_money(v) -- vectorised: does each value look like a money figure?
# Strip a DR/CR/OD suffix and every currency/letter/symbol, then ask whether the
# remaining core is a well-formed number (optional sign/parens, digits, grouping
# and decimal separators). Loose on purpose -- kind inference thresholds it.
.looks_money <- function(v) {
  up <- toupper(trimws(as.character(v)))
  core <- gsub("(DR|CR|OD)[[:space:]]*$", "", up)   # drop a balance-sign suffix
  core <- gsub("[^0-9.,()+ -]", "", core)           # drop currency glyphs / letters
  core <- gsub("[[:space:]]", "", core)
  grepl("[0-9]", up) & nzchar(core) &
    grepl("^[(+-]?[0-9][0-9.,]*[0-9]?[)]?$", core)
}

# .col_kind(v) -- infer a column's KIND from a nonempty sample:
#   date | money | integer | indicator | text | empty
# Order matters: a decisive date/money test wins before the low-cardinality
# indicator test, so a two-value date column is still "date", not "indicator".
.col_kind <- function(v) {
  if (!length(v)) return("empty")
  if (nzchar(detect_date_format(v))) return("date")
  m <- .looks_money(v)
  if (mean(m) >= 0.8) {
    if (all(grepl("^[0-9]+$", v))) return("integer")   # bare ints, no money markers
    return("money")
  }
  if (all(grepl("^[0-9]+$", v))) return("integer")
  distinct <- unique(v)
  if (length(distinct) <= 8L && all(nchar(distinct) <= 12L)) return("indicator")
  "text"
}

# .money_shape(v) -- the STYLE facts a template needs for a money column: which
# separators, sign convention and suffix appear. Maps straight onto amount_sign /
# decimal_mark. Counts and booleans only, never a figure.
.money_shape <- function(v) {
  up <- toupper(trimws(as.character(v)))
  last_sep <- function(s) {
    dp <- max(gregexpr("\\.", s)[[1]]); cp <- max(gregexpr(",", s)[[1]])
    if (dp < 0 && cp < 0) return(NA_character_)
    if (dp > cp) "dot" else "comma"
  }
  seps <- vapply(v[grepl("[.,]", v)], last_sep, character(1), USE.NAMES = FALSE)
  seps <- seps[!is.na(seps)]
  list(
    currency_symbol = any(grepl(.CURRENCY_RX, v)),
    thousands_sep   = any(grepl("[0-9][,' ][0-9]{3}", v)),
    decimal_mark    = if (length(seps)) names(sort(table(seps), decreasing = TRUE))[1] else "unknown",
    parens_negative = any(grepl("\\([0-9., ]+\\)", v)),
    minus_negative  = any(grepl("^-|-$", trimws(v))),
    dr_cr_suffix    = any(grepl("(DR|CR|OD)[[:space:]]*$", up)))
}

# .col_profile(name, values) -- one PII-safe column profile (see file header).
.col_profile <- function(name, values) {
  raw <- as.character(values)
  n   <- length(raw)
  ne  <- trimws(raw[!is.na(raw) & nzchar(trimws(raw))])
  samp <- utils::head(ne, 300L)
  kind <- .col_kind(samp)
  prof <- list(
    name          = as.character(name),
    kind          = kind,
    fill_rate     = if (n > 0) round(length(ne) / n, 3) else 0,
    distinct      = length(unique(ne)),
    example_shape = .modal_shape(samp))
  if (identical(kind, "date")) {
    f <- detect_date_format(samp); if (nzchar(f)) prof$date_format <- f
  } else if (identical(kind, "money")) {
    prof$money <- .money_shape(samp)
  } else if (identical(kind, "indicator")) {
    toks <- sort(unique(toupper(ne)))
    if (length(toks) <= 12L && all(nchar(toks) <= 8L)) prof$tokens <- as.list(toks)
    else prof$distinct_tokens <- length(toks)
  } else if (identical(kind, "text")) {
    prof$length <- .len_stats(ne)
  }
  prof
}

# column_profiles(df) -- profile every column of a source table (capped so a
# pathologically wide sheet can't blow up the record).
column_profiles <- function(df) {
  if (is.null(df) || !ncol(df)) return(list())
  lapply(seq_len(min(ncol(df), 60L)), function(j) .col_profile(names(df)[j], df[[j]]))
}

# .suggest_mapping(df) -- the engine's OWN best guess at the template, using the
# same detectors the drafter and wizard use (guess_mapping / detect_amount_style /
# detect_date_format / detect_type_dc_values). Header names + formats only -- a
# ready-to-refine skeleton for whoever builds the template. No filename, no values.
.suggest_mapping <- function(df) {
  if (is.null(df) || !ncol(df)) return(NULL)
  h  <- names(df)
  mp <- function(field) { c <- guess_mapping(h, field); if (identical(c, "(none)")) NULL else c }
  out <- list()
  dcol <- mp("date")
  if (!is.null(dcol)) {
    out$date <- dcol
    f <- detect_date_format(df[[dcol]]); if (nzchar(f)) out$date_format <- f
  }
  fields <- list()
  for (f in c("amount", "description", "particulars", "code", "reference",
              "type", "other_party", "balance")) {
    cc <- mp(f); if (!is.null(cc)) fields[[f]] <- cc
  }
  if (length(fields)) out$fields <- fields
  style <- safe(detect_amount_style(h, df), "signed")
  out$amount_style <- style
  if (identical(style, "type_dc")) {
    tv <- safe(detect_type_dc_values(h, df), NULL)
    if (!is.null(tv$column)) out$type_column       <- tv$column
    if (!is.null(tv$debit))  out$type_debit_value  <- tv$debit
    if (!is.null(tv$credit)) out$type_credit_value <- tv$credit
  }
  out
}

# .delim_of(input) -- sniff a delimited file's separator from its first non-empty
# line (no file re-read; reuses the wizard's candidate set).
.delim_of <- function(input) {
  ln <- input$lines %||% character(0)
  ln <- ln[nzchar(trimws(ln))]
  if (!length(ln)) return(",")
  ln <- ln[1]
  counts <- vapply(wd_delims(), function(d) {
    m <- gregexpr(d, ln, fixed = TRUE)[[1]]; sum(m > 0)
  }, integer(1))
  if (max(counts) == 0) return(",")
  wd_delims()[which.max(counts)]
}

# .pdf_shapes(input) -- for a PDF, the money/date STYLE facts scannable from the
# text tokens (no columns exist until a template defines bands). Structural only.
.pdf_shapes <- function(input) {
  toks <- unlist(lapply(input$words %||% list(), function(w)
    if (!is.null(w) && nrow(w)) as.character(w$text) else character(0)))
  if (!length(toks)) toks <- unlist(strsplit(paste(input$pages %||% character(0),
                                                    collapse = " "), "[[:space:]]+"))
  toks <- trimws(toks); toks <- toks[nzchar(toks)]
  out <- list()
  moneyish <- toks[.looks_money(toks) &
                   (grepl("[.,][0-9]{2}$", toks) | grepl("[0-9],[0-9]{3}", toks) |
                    grepl(.CURRENCY_RX, toks))]
  if (length(moneyish)) out$money <- .money_shape(utils::head(moneyish, 300L))
  dateish <- toks[grepl("^[0-9]{1,2}[/.-][0-9]{1,2}([/.-][0-9]{2,4})?$", toks)]
  if (length(dateish)) {
    f <- detect_date_format(utils::head(dateish, 50L)); if (nzchar(f)) out$date_format_guess <- f
  }
  out
}

# template_hints(input, template, matched) -> the full "how to draft a template"
# bundle for one input, or NULL. Delimited/Excel: per-column profiles + the
# engine's suggested mapping. PDF: suggested column bands (only worth it when NO
# template matched -- otherwise the bands are already known), text style shapes and
# candidate fingerprint phrases. Never throws (caller wraps in safe() too).
template_hints <- function(input, template = NULL, matched = FALSE) {
  kind <- input$kind %||% ""
  if (identical(kind, "excel") || identical(kind, "delimited")) {
    df <- if (identical(kind, "excel")) input$table
          else safe(read_delimited(input, list(delimiter = .delim_of(input)))$table, NULL)
    if (is.null(df) || !ncol(df) || !nrow(df)) return(NULL)
    out <- list(kind = kind,
                row_sample = min(nrow(df), 300L),
                columns = column_profiles(df),
                suggested_mapping = .suggest_mapping(df))
    if (identical(kind, "delimited")) out$delimiter <- .delim_of(input)
    return(out)
  }
  if (identical(kind, "pdf")) {
    out <- list(kind = kind)
    if (!isTRUE(matched)) {
      sug <- safe(suggest_pdf_columns(input), NULL)
      if (!is.null(sug) && nrow(sug))
        out$pdf_bands <- lapply(seq_len(nrow(sug)), function(i)
          list(field = as.character(sug$field[i]),
               x_min = round(as.numeric(sug$x_min[i])),
               x_max = round(as.numeric(sug$x_max[i]))))
    }
    sh <- safe(.pdf_shapes(input), list())
    if (length(sh)) out$shapes <- sh
    fp <- safe(header_phrases(input), character(0))
    if (length(fp)) out$fingerprint_candidates <- as.list(utils::head(fp, 8L))
    if (length(out) <= 1L) return(NULL)   # nothing useful beyond `kind`
    return(out)
  }
  NULL
}
