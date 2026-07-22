# column_profile.R -- "everything you'd need to DRAFT a template", captured
# PII-safely. When the engine can't match a statement, the single most useful
# thing to record is a STRUCTURAL description of its source columns plus the
# engine's own best-guess mapping, so a human -- or an AI assistant -- can turn it
# into a template WITHOUT ever seeing statement content.
#
# PII posture (identical to the rest of metadata capture): no raw values leave
# here. A column yields its NAME, inferred KIND, fill rate, cardinality, and a
# MASKED example shape (each digit -> 9, EVERY letter -- ASCII or not -> A, only the
# ASCII separators a shape needs are kept, so "31/12/2025" -> "99/99/9999" and
# "TAMAKI PMT" -> "AAAAAA AAA"). Literal tokens are emitted ONLY for a genuine
# debit/credit-style INDICATOR column (values in the known D/C domain, or a header
# that names itself an indicator) -- NEVER for a free-text / reference / payee
# column, whose few short values would be real content.

# .enc_safe(x) -- coerce to valid UTF-8, dropping bytes invalid in the source
# encoding, so nchar()/gsub()/substr() can never throw on a hostile multibyte value
# in a non-UTF-8 locale (the air-gapped Windows box). Sanitise once, up front.
.enc_safe <- function(x) iconv(as.character(x), from = "", to = "UTF-8", sub = "")

# .mask_value(s) -- collapse a value to a PII-safe shape. Digit -> 9; ASCII letter
# -> A; then ANY remaining non-structural character (a te reo macron, an accent,
# CJK / Greek text, a non-$ currency glyph) is content too and is masked to A. Only
# the ASCII separators a shape needs are kept, so no readable character survives.
.mask_value <- function(s) {
  s <- as.character(s)
  s <- gsub("[0-9]", "9", s)                    # digit -> 9
  s <- gsub("[A-Za-z]", "A", s)                 # ASCII letter -> A
  gsub("[^0-9A-Za-z ./,:()$+-]", "A", s)        # any other char (incl. non-ASCII) -> A
}

# .modal_shape(v) -- the most common masked shape across a sample (capped in
# length so a long free-text field can't bloat the record). Ties are broken by the
# shape string so the choice is deterministic across machines/locales (the shapes
# are pure ASCII after masking).
.modal_shape <- function(v) {
  v <- v[nzchar(v)]
  if (!length(v)) return(NA_character_)
  shapes <- vapply(utils::head(v, 100L), .mask_value, character(1), USE.NAMES = FALSE)
  tb <- table(shapes)
  ms <- names(tb)[order(-as.integer(tb), names(tb))][1]
  if (is.na(ms)) return(NA_character_)
  if (nchar(ms) > 48L) paste0(substr(ms, 1L, 45L), "...") else ms
}

# Column-name patterns that decide whether a low-cardinality column's LITERAL
# values may be emitted. A CONTENT header (description / reference / payee / name /
# particulars ...) never leaks its values, even when short and few. An INDICATOR
# header (type / dr-cr / debit-credit / direction ...) is one whose few short codes
# ARE the structural signal a template needs.
.CONTENT_COL_RX   <- "desc|payee|paye|detail|memo|narrat|particular|referen|other.?part|counterpart|\\bname\\b|address|payer|remitter|merchant"
.INDICATOR_COL_RX <- "type|dr.?cr|cr.?dr|d/?c|debit.?credit|credit.?debit|\\bsign\\b|indicat|in/?out|money.?in|money.?out|direction"

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

# .col_kind(v, dfmt) -- infer a column's KIND from a nonempty sample:
#   date | money | integer | indicator | text | empty
# Order matters: a decisive date/money test wins before the low-cardinality
# indicator test, so a two-value date column is still "date", not "indicator".
# `dfmt` is the pre-computed detect_date_format(v) so the caller doesn't run it twice.
.col_kind <- function(v, dfmt = detect_date_format(v)) {
  if (!length(v)) return("empty")
  if (nzchar(dfmt)) return("date")
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
  raw <- .enc_safe(values)               # valid UTF-8 up front: nchar/gsub never throw
  n   <- length(raw)
  ne  <- trimws(raw[!is.na(raw) & nzchar(trimws(raw))])
  samp <- utils::head(ne, 300L)
  dfmt <- detect_date_format(samp)         # computed ONCE, reused for kind + date_format
  kind <- .col_kind(samp, dfmt)
  prof <- list(
    name          = as.character(name),
    kind          = kind,
    fill_rate     = if (n > 0) round(length(ne) / n, 3) else 0,
    distinct      = length(unique(ne)),
    example_shape = .modal_shape(samp))
  if (identical(kind, "date")) {
    if (nzchar(dfmt)) prof$date_format <- dfmt
  } else if (identical(kind, "money")) {
    prof$money <- .money_shape(samp)
  } else if (identical(kind, "indicator")) {
    toks <- sort(unique(toupper(ne)), method = "radix")     # deterministic ordering
    nm <- tolower(as.character(name))
    # Emit the LITERAL values ONLY when this is genuinely a debit/credit-style
    # indicator -- its values are a recognised D/C domain, OR the header names it an
    # indicator column -- and never for a content/reference/payee column (whose few
    # short values would be real content). Everything else: count only.
    # type_dc_domain() already upper-cases, so no toupper() needed.
    is_content   <- grepl(.CONTENT_COL_RX, nm)
    is_indicator <- all(toks %in% type_dc_domain()) || grepl(.INDICATOR_COL_RX, nm)
    if (!is_content && is_indicator && length(toks) <= 12L && all(nchar(toks) <= 8L))
      prof$tokens <- as.list(toks)
    else
      prof$distinct_tokens <- length(toks)
  } else if (identical(kind, "text")) {
    prof$length <- .len_stats(ne)
  }
  prof
}

# column_profiles(df) -- profile every column of a source table (capped so a
# pathologically wide sheet can't blow up the record). Each column is profiled
# defensively so one hostile column degrades to nothing, not the whole bundle.
column_profiles <- function(df) {
  if (is.null(df) || !ncol(df)) return(list())
  profs <- lapply(seq_len(min(ncol(df), 60L)),
                  function(j) safe(.col_profile(names(df)[j], df[[j]]), NULL))
  Filter(Negate(is.null), profs)
}

# .suggest_mapping(df) -- the engine's OWN best guess at the template. This is
# LITERALLY what the drafter would produce: it calls the shared .derive_mapping()
# brain (draft.R) and flattens it to a header-names-only skeleton, so the hint can
# never diverge from Draft-a-template. No filename, no values.
.suggest_mapping <- function(df) {
  if (is.null(df) || !ncol(df)) return(NULL)
  dm <- safe(.derive_mapping(names(df), df, "%d/%m/%Y"), NULL)
  if (is.null(dm)) return(NULL)
  src <- function(k) if (!is.null(dm$cols[[k]])) dm$cols[[k]]$source else NULL
  out <- list()
  if (!is.null(dm$cols$date)) {
    out$date <- dm$cols$date$source
    if (!is.null(dm$date_format) && nzchar(dm$date_format)) out$date_format <- dm$date_format
  }
  fields <- list()
  for (f in c("amount", "description", "particulars", "code", "reference",
              "type", "other_party", "balance", "debit", "credit")) {
    s <- src(f); if (!is.null(s)) fields[[f]] <- s     # incl. debit/credit sources
  }
  if (length(fields)) out$fields <- fields
  out$amount_style <- dm$amount_sign
  if (identical(dm$amount_sign, "type_dc")) {
    if (!is.null(src("type")))            out$type_column       <- src("type")
    if (!is.null(dm$keys$type_debit_value))  out$type_debit_value  <- dm$keys$type_debit_value
    if (!is.null(dm$keys$type_credit_value)) out$type_credit_value <- dm$keys$type_credit_value
  }
  out
}

# .delim_of(input) -- sniff a delimited file's separator from its first non-empty
# line (no file re-read; shares the choice logic with detect_delimiter).
.delim_of <- function(input) {
  ln <- input$lines %||% character(0)
  ln <- ln[nzchar(trimws(ln))]
  .delim_of_line(if (length(ln)) ln[1] else "")
}

# .pdf_shapes(input) -- for a PDF, the money/date STYLE facts scannable from the
# text tokens (no columns exist until a template defines bands). Structural only.
.pdf_shapes <- function(input) {
  toks <- unlist(lapply(input$words %||% list(), function(w)
    if (!is.null(w) && nrow(w)) as.character(w$text) else character(0)))
  if (!length(toks)) toks <- unlist(strsplit(paste(input$pages %||% character(0),
                                                    collapse = " "), "[[:space:]]+"))
  toks <- trimws(.enc_safe(toks)); toks <- toks[nzchar(toks)]
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
# template matched -- otherwise the bands are already known) and text style shapes.
# Never throws (caller wraps in safe() too). Structural only -- see the PII note in
# the file header.
template_hints <- function(input, template = NULL, matched = FALSE) {
  kind <- input$kind %||% ""
  if (identical(kind, "excel") || identical(kind, "delimited")) {
    # The delimiter for a delimited file: the MATCHED template's (so its
    # preamble.header_regex + delimiter locate the real header, not a preamble line),
    # else sniffed once from the source.
    delim <- if (identical(kind, "delimited")) (template$delimiter %||% .delim_of(input)) else NULL
    df <- if (identical(kind, "excel")) input$table
          else {
            tmpl_read <- if (!is.null(template) && identical(template$format %||% "", "delimited"))
                           template else list(delimiter = delim)
            safe(read_delimited(input, tmpl_read)$table, NULL)
          }
    if (is.null(df) || !ncol(df) || !nrow(df)) return(NULL)
    out <- list(kind = kind,
                row_sample = min(nrow(df), 300L),
                columns = column_profiles(df),
                suggested_mapping = .suggest_mapping(df))
    if (identical(kind, "delimited")) out$delimiter <- delim
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
    # NOTE: raw header/fingerprint PHRASES are deliberately NOT persisted -- a
    # statement's title line can be an account-holder or company name (PII). The
    # person drafting a PDF template reads the fingerprint phrases off the statement
    # itself (and the visual band editor supplies the columns).
    if (length(out) <= 1L) return(NULL)   # nothing useful beyond `kind`
    return(out)
  }
  NULL
}
