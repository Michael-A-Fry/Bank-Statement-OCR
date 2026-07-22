# lexicon.R -- the engine's externalised recognition VOCABULARIES.
#
# WHY. Templates hold per-bank FACTS ("this bank's debit marker is 'cow'"). The
# label dictionary holds label WORDINGS. But the engine also carries a third kind
# of vocabulary: the SUPERSET of synonyms / markers / shapes it tries when it
# auto-detects, drafts and parses GENERICALLY -- debit/credit indicator words, the
# DR/CR/OD suffix markers, redaction markers, header keywords, amount-style hints,
# and the money / date / account regexes. Those used to be hardcoded and scattered
# across ~7 files, so teaching the tool a new one (a bank that writes "cow"/"horse"
# for debit/credit) meant a code change in several places. This module externalises
# them all into ONE admin-editable file so a single edit plumbs everywhere.
#
# PRECEDENCE: template > lexicon > built-in default.
#   * The BUILT-IN default is the value shipped in code (.lexicon_defaults). If the
#     lexicon file is absent or a category is omitted, behaviour is IDENTICAL to
#     before this module existed -- zero regression.
#   * The LEXICON FILE (dictionaries/lexicon.yaml) EXTENDS or OVERRIDES per category
#     type (see .lexicon_spec): word LISTS are UNIONED with the built-in (so adding
#     "cow" keeps "D"/"DR"/...); a REGEX replaces (validated -- an un-compilable
#     regex falls back to the built-in, never breaking parsing); a date-format TABLE
#     appends; a field-pattern MAP overrides per field.
#   * A TEMPLATE always wins for its own statements (e.g. type_debit_value).
#
# This is the deterministic half of the learning loop: a local model may PROPOSE a
# new entry from the captured metadata, but a human approves it into this file; the
# engine only ever reads approved, deterministic vocabulary.

.LEXICON_CACHE <- new.env(parent = emptyenv())

# Category -> merge semantics. "list" = union with built-in; "regex" = replace
# (validated); "table" = append format specs; "map" = per-key regex override.
.lexicon_spec <- function() c(
  header_keywords = "list", layout_stopwords = "list",
  debit_markers = "list", credit_markers = "list",
  amount_style_debit_headers = "list", amount_style_credit_headers = "list",
  dr_cr_suffix_debit = "list", dr_cr_suffix_credit = "list", overdrawn_markers = "list",
  period_connectives = "list", redaction_markers = "list", redaction_block_glyphs = "list",
  money_regex = "regex", date_regex = "regex", account_regex = "regex", card_regex = "regex",
  date_formats = "table", field_name_patterns = "map")

# Built-in defaults. These MUST equal the values the engine shipped with, so an
# absent lexicon file is a no-op. The named constants (.HDR_KEYS, .MONEY_RX, ...)
# live in their own modules and resolve at call time.
.lexicon_defaults <- function() list(
  header_keywords  = .HDR_KEYS,
  layout_stopwords = .LAYOUT_STOP,
  debit_markers    = c("D", "DR", "DEBIT", "W", "WD", "WITHDRAWAL", "OUT"),
  credit_markers   = c("C", "CR", "CREDIT", "DEP", "DEPOSIT", "IN"),
  amount_style_debit_headers  = c("debit", "withdrawal", "money out", "paid out"),
  amount_style_credit_headers = c("credit", "deposit", "money in", "paid in"),
  dr_cr_suffix_debit  = c("DR"),   # debit suffix on an AMOUNT (dr_cr_suffix style)
  dr_cr_suffix_credit = c("CR"),   # credit suffix on an amount
  overdrawn_markers   = c("OD"),   # extra debit marker on a BALANCE (.num_one)
  period_connectives  = c("to", "through", "thru", "until"),
  redaction_markers   = c("\\[REDACTED\\]", "\\bREDACTED\\b", "X{6,}", "#{6,}"),
  redaction_block_glyphs = .PDF_BLOCK_GLYPHS,
  money_regex   = .MONEY_RX,
  date_regex    = .DATE_RX,
  account_regex = .ACCT_RX,
  card_regex    = .CARD_RX,
  date_formats  = wd_date_table(),
  field_name_patterns = wd_field_patterns())

# .lexicon_path() -- BSO_LEXICON env wins (deployment / tests); else config
# paths$lexicon; else dictionaries/lexicon.yaml.
.lexicon_path <- function() {
  p <- Sys.getenv("BSO_LEXICON", "")
  if (nzchar(p)) return(p)
  cfg <- tryCatch(load_config(), error = function(e) NULL)
  (cfg$paths$lexicon %||% NULL) %||% file.path("dictionaries", "lexicon.yaml")
}

# load_lexicon(path, refresh) -- read + cache the raw lexicon file (a named list of
# overrides). Cached so it isn't re-read per call; clear_lexicon_cache() after a save.
load_lexicon <- function(path = .lexicon_path(), refresh = FALSE) {
  key <- path %||% "<none>"
  if (!refresh && exists(key, envir = .LEXICON_CACHE, inherits = FALSE))
    return(get(key, envir = .LEXICON_CACHE, inherits = FALSE))
  raw <- if (!is.null(path) && file.exists(path)) safe(yaml::read_yaml(path), list()) else list()
  if (!is.list(raw)) raw <- list()
  assign(key, raw, envir = .LEXICON_CACHE)
  raw
}

# clear_lexicon_cache() -- drop the cache so the next lex() re-reads the file.
clear_lexicon_cache <- function() rm(list = ls(.LEXICON_CACHE), envir = .LEXICON_CACHE)

# .regex_ok(rx) -- does this string compile as a (perl) regex? Used to reject a bad
# admin-entered / model-proposed pattern before it can break parsing.
.regex_ok <- function(rx) {
  rx <- as.character(rx)
  length(rx) == 1 && !is.na(rx) && nzchar(rx) &&
    isTRUE(tryCatch({ grepl(rx, "probe", perl = TRUE); TRUE },
                    error = function(e) FALSE, warning = function(w) FALSE))
}

# lex(category, path) -- the resolved vocabulary for `category`: the built-in
# default merged with the lexicon file per the category's semantics. Fail-safe:
# an unknown category, or an invalid override, returns the built-in default.
# The RESOLVED result is memoised in .LEXICON_CACHE (under a "lex::" key), because
# lex() is called dozens of times per conversion and the merge (unions, regex
# compiles) isn't free. It rides the lexicon's existing invalidation:
# clear_lexicon_cache() wipes these entries too, so an admin vocabulary edit takes
# effect on the next conversion exactly as before.
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

# type_dc_domain() -- the set of values that mark a column as a debit/credit
# INDICATOR. Deliberately the UNION of the debit + credit markers, so teaching the
# engine a new pair (cow / horse) both makes it RECOGNISE the column and CLASSIFY
# the tokens, from the one lexicon edit.
type_dc_domain <- function() unique(toupper(c(lex("debit_markers"), lex("credit_markers"))))

# lexicon_categories() -- the editable category names + their merge type, for the
# Admin editor and validation.
lexicon_categories <- function() .lexicon_spec()

# lexicon_defaults_yaml() -- the full built-in defaults as YAML text, so the Admin
# editor / reset button can show a complete, valid starting point.
lexicon_defaults_yaml <- function() yaml::as.yaml(.lexicon_defaults())

# validate_lexicon(raw) -> character vector of problems (length 0 if clean). Used at
# save time: an invalid entry is REJECTED (never silently applied then falling back).
validate_lexicon <- function(raw) {
  problems <- character(0)
  if (is.null(raw)) return(problems)
  if (!is.list(raw)) return("lexicon must be a mapping of categories")
  spec <- .lexicon_spec()
  for (cat in names(raw)) {
    if (is.null(spec[[cat]])) { problems <- c(problems, sprintf("unknown category '%s'", cat)); next }
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
