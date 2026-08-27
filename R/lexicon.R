# lexicon.R -- the engine's externalised recognition VOCABULARIES: the SUPERSET of synonyms, markers
# and shapes the engine tries when it auto-detects, drafts and parses GENERICALLY. Hardcoded across
# seven files, teaching the tool a bank that writes "cow"/"horse" for debit/credit meant a code change
# in several places. Precedence: template > lexicon > built-in. LISTS union, a REGEX replaces
# (validated), a date-format TABLE appends, a field-pattern MAP overrides per field.

.LEXICON_CACHE <- new.env(parent = emptyenv())

# Category to merge semantics. "list" unions with the built-in; "regex" replaces (validated);
# "table" appends format specs; "map" overrides per key.
.lexicon_spec <- function() c(
  header_keywords = "list", layout_stopwords = "list",
  # Bank and brand words the fingerprint drafter treats as a masthead rather than a customer's name.
  # Registered here or lex() rejects the category and the drafter silently falls back to code.
  fingerprint_brand_words = "list",
  debit_markers = "list", credit_markers = "list",
  amount_style_debit_headers = "list", amount_style_credit_headers = "list",
  dr_cr_suffix_debit = "list", dr_cr_suffix_credit = "list", overdrawn_markers = "list",
  # Whole-label wordings that mark a PDF line as a SUMMARY rather than a transaction. Registered here or
  # validate_lexicon() rejects the category and it is missing from the Admin editor, so the "teach it
  # in YAML" promise would fail at the moment an analyst tried to use it.
  summary_line_labels = "list",
  period_connectives = "list", redaction_markers = "list", redaction_block_glyphs = "list",
  money_regex = "regex", date_regex = "regex", account_regex = "regex", card_regex = "regex",
  date_formats = "table", field_name_patterns = "map")

# Built-in defaults. These MUST equal the values the engine shipped with, so an absent lexicon file
# is a no-op.
.lexicon_defaults <- function() list(
  header_keywords  = .HDR_KEYS,
  fingerprint_brand_words = .FP_BRAND_DEFAULT,   # R/wizard_auto.R
  layout_stopwords = .LAYOUT_STOP,
  debit_markers    = c("D", "DR", "DEBIT", "W", "WD", "WITHDRAWAL", "OUT"),
  credit_markers   = c("C", "CR", "CREDIT", "DEP", "DEPOSIT", "IN"),
  amount_style_debit_headers  = c("debit", "withdrawal", "money out", "paid out"),
  amount_style_credit_headers = c("credit", "deposit", "money in", "paid in"),
  dr_cr_suffix_debit  = c("DR"),   # debit suffix on an AMOUNT (dr_cr_suffix style)
  dr_cr_suffix_credit = c("CR"),   # credit suffix on an amount
  overdrawn_markers   = c("OD"),   # extra debit marker on a BALANCE (.num_one)
  summary_line_labels = .PDF_SUMMARY_LABELS,     # R/parse_pdf_table.R
  period_connectives  = c("to", "through", "thru", "until"),
  redaction_markers   = c("\\[REDACTED\\]", "\\bREDACTED\\b", "X{6,}", "#{6,}"),
  redaction_block_glyphs = .PDF_BLOCK_GLYPHS,
  money_regex   = .MONEY_RX,
  date_regex    = .DATE_RX,
  account_regex = .ACCT_RX,
  card_regex    = .CARD_RX,
  date_formats  = wd_date_table(),
  field_name_patterns = wd_field_patterns())

# BSO_LEXICON wins, else config paths$lexicon, else dictionaries/lexicon.yaml.
.lexicon_path <- function() {
  p <- Sys.getenv("BSO_LEXICON", "")
  if (nzchar(p)) return(p)
  cfg <- tryCatch(load_config(), error = function(e) NULL)
  (cfg$paths$lexicon %||% NULL) %||% file.path("dictionaries", "lexicon.yaml")
}

# Read and cache the raw lexicon file; clear_lexicon_cache() after a save.
load_lexicon <- function(path = .lexicon_path(), refresh = FALSE) {
  key <- path %||% "<none>"
  if (!refresh && exists(key, envir = .LEXICON_CACHE, inherits = FALSE))
    return(get(key, envir = .LEXICON_CACHE, inherits = FALSE))
  raw <- if (!is.null(path) && file.exists(path)) safe(yaml::read_yaml(path), list()) else list()
  if (!is.list(raw)) raw <- list()
  assign(key, raw, envir = .LEXICON_CACHE)
  raw
}

# Drop the cache so the next lex() re-reads the file.
clear_lexicon_cache <- function() rm(list = ls(.LEXICON_CACHE), envir = .LEXICON_CACHE)

# Does this string compile as a perl regex? Rejects a bad admin-entered or model-proposed pattern
# before it can break parsing.
.regex_ok <- function(rx) {
  rx <- as.character(rx)
  length(rx) == 1 && !is.na(rx) && nzchar(rx) &&
    isTRUE(tryCatch({ grepl(rx, "probe", perl = TRUE); TRUE },
                    error = function(e) FALSE, warning = function(w) FALSE))
}

# The resolved vocabulary for `category`: the built-in merged with the file per the category's
# semantics. Fail-safe - an unknown category or an invalid override returns the built-in. Memoised,
# because lex() is called dozens of times per conversion.
lex <- function(category, path = .lexicon_path()) {
  ckey <- paste0("lex::", category, "::", path %||% "<none>")
  if (exists(ckey, envir = .LEXICON_CACHE, inherits = FALSE))
    return(get(ckey, envir = .LEXICON_CACHE, inherits = FALSE))
  d <- .lexicon_defaults()[[category]]
  spec <- .lexicon_spec()[[category]]
  out <- if (is.null(spec)) d else {
    f <- load_lexicon(path)[[category]]
    if (is.null(f)) d else switch(spec,
      list  = unique(c(as.character(d), trimws(as.character(unlist(f))))),
      regex = { rx <- as.character(unlist(f))[1]; if (.regex_ok(rx)) rx else d },
      table = {
        extra <- Filter(function(e) is.list(e) && !is.null(e$fmt) && !is.null(e$rx) &&
                          .regex_ok(e$rx), f)
        c(d, extra)
      },
      map = {
        o <- d
        for (k in names(f)) { rx <- as.character(f[[k]])[1]; if (.regex_ok(rx)) o[[k]] <- rx }
        o
      },
      d)
  }
  assign(ckey, out, envir = .LEXICON_CACHE)
  out
}

# The values that mark a column as a debit/credit INDICATOR. Deliberately the UNION of the debit and
# credit markers, so teaching a new pair both RECOGNISES the column and CLASSIFIES the tokens.
type_dc_domain <- function() unique(toupper(c(lex("debit_markers"), lex("credit_markers"))))

# The editable category names and their merge type, for the Admin editor and validation.
lexicon_categories <- function() .lexicon_spec()

# The full built-in defaults as YAML text, so the Admin editor's reset can show a valid start.
lexicon_defaults_yaml <- function() yaml::as.yaml(.lexicon_defaults())

# Returns problems, length 0 if clean. Used at save time: an invalid entry is REJECTED, never
# silently applied and then falling back.
validate_lexicon <- function(raw) {
  problems <- character(0)
  if (is.null(raw)) return(problems)
  if (!is.list(raw)) return("lexicon must be a mapping of categories")
  spec <- .lexicon_spec()
  for (cat in names(raw)) {
    # `spec` is a named CHARACTER VECTOR, so spec[[cat]] for an absent name ERRORS before is.null()
    # could catch it - an admin typo crashed the validator whose job is to report "unknown category".
    if (!(cat %in% names(spec))) { problems <- c(problems, sprintf("unknown category '%s'", cat)); next }
    v <- raw[[cat]]
    if (identical(spec[[cat]], "regex") && !.regex_ok(as.character(unlist(v))[1]))
      problems <- c(problems, sprintf("%s is not a valid regex", cat))
    if (identical(spec[[cat]], "table")) {
      bad <- Filter(function(e) !(is.list(e) && !is.null(e$fmt) && !is.null(e$rx) && .regex_ok(e$rx)), v)
      if (length(bad)) problems <- c(problems, sprintf("%s has %d malformed date-format entr(y/ies) (need fmt + valid rx)", cat, length(bad)))
    }
    if (identical(spec[[cat]], "map")) for (k in names(v)) {
      if (!.regex_ok(as.character(v[[k]])[1]))
        problems <- c(problems, sprintf("%s.%s is not a valid regex", cat, k))
    }
  }
  problems
}
