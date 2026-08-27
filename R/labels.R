# labels.R -- declarative label dictionary and matcher for SINGLE LABELLED VALUES. Transaction TABLES do
# NOT use this - they map by column header or x-band and never match on wording. A spec understands
# any_of, dict, value (money | date | date_range | text | regex:<pattern>), occurrence, where, required
# and on_conflict; the vocabulary lives in dictionaries/labels.yaml, never in code.

# A money token, SIGN-AWARE: leading currency/sign/paren, thousands grouped with , . or space, a 1-2
# place decimal in either mark so European "1.234,56" is captured whole, and an OPTIONAL trailing
# DR/CR/OD marker so an overdrawn "1,234.56 DR" keeps its sign. The no-cents branch requires the $
# because a bare "1,234" is ambiguous. A NEGATIVE MUST SURVIVE: the minus used to sit only inside the
# currency symbol, so "-$577.80" was read with the wrong sign, and it is not always an ASCII hyphen -
# PDF typesetting emits U+2212 and U+2013. An ALTERNATION not a bracket expression, matched with
# useBytes: in a C locale a multibyte character inside [...] is read a byte at a time.
.MINUS <- "(?:[-]|\u2212|\u2013)"
# The separators between a label and its value. An ALTERNATION matched as bytes, not a character class:
# in a C locale a multibyte character inside [...] is read a byte at a time, so the old class ate the
# leading byte of any non-ASCII VALUE and the result was no longer verbatim.
.LABEL_SEP_RX <- "^(?:[ :-]|\u2013|\u2212)+"
.MONEY_RX <- paste0(
  "\\(?", .MINUS, "?[$]?", .MINUS, "?[0-9][0-9,.]*(?:\\.[0-9]{2}|,[0-9]{2})(?![0-9])\\)?", .MINUS, "?(?:\\s?(?:DR|CR|OD))?",
  "|",
  "\\(?", .MINUS, "?[$]", .MINUS, "?[0-9][0-9,]*(?![0-9.,])\\)?", .MINUS, "?(?:\\s?(?:DR|CR|OD))?")
.DATE_RX  <- "[0-9]{1,2}[/ .-][A-Za-z0-9]{2,9}[/ .-][0-9]{2,4}"

# The list of label phrases a spec matches on.
.spec_terms <- function(spec) as.character(spec$any_of %||% spec$label %||% character(0))

# The value kind to pull once a label line is found.
.spec_value_type <- function(spec) {
  if (!is.null(spec$pattern) && nzchar(spec$pattern)) return(paste0("regex:", spec$pattern))
  spec$value %||% "money"
}

# Restrict the search to the requested pages.
.pages_in_scope <- function(pages, where) {
  where <- where %||% "any"
  n <- length(pages)
  if (n == 0) return(character(0))
  if (is.numeric(where)) { k <- as.integer(where); return(if (k >= 1 && k <= n) pages[k] else character(0)) }
  switch(as.character(where),
         page1     = pages[1],
         last_page = pages[n],
         pages)               # "any" / anything else
}

# Extract the typed value from one line, or NA.
.value_from_line <- function(line, vtype) {
  if (grepl("^regex:", vtype)) {
    pat <- sub("^regex:", "", vtype)
    m <- regmatches(line, regexpr(pat, line, perl = TRUE))
    return(if (length(m) && nzchar(m)) m[1] else NA_character_)
  }
  money_rx <- lex("money_regex"); date_rx <- lex("date_regex")   # lexicon (admin-editable)
  # useBytes on every one: the money pattern carries two non-ASCII minus signs and a statement line
  # carries whatever currency symbol the bank printed.
  switch(vtype,
    money = { m <- regmatches(line, gregexpr(money_rx, line, perl = TRUE,
                                             ignore.case = TRUE, useBytes = TRUE))[[1]]
              if (length(m)) m[length(m)] else NA_character_ },   # value sits to the right
    date  = { m <- regmatches(line, regexpr(date_rx, line, useBytes = TRUE))
              if (length(m)) m[1] else NA_character_ },
    date_range = { m <- regmatches(line, gregexpr(date_rx, line, useBytes = TRUE))[[1]]
                   if (length(m) >= 2) paste(m[1], "|", m[2]) else NA_character_ },
    NA_character_)   # "text" handled by the caller (needs the label position)
}

# The trimmed remainder of a line after the label, for `value: text`.
.text_after <- function(line, term) {
  p <- regexpr(tolower(term), tolower(line), fixed = TRUE)
  if (p < 0) return(NA_character_)
  rest <- substr(line, p + attr(p, "match.length"), nchar(line))
  rest <- trimws(sub(.LABEL_SEP_RX, "", rest, perl = TRUE, useBytes = TRUE))
  if (nzchar(rest)) rest else NA_character_
}

# The raw remainder of a line to the RIGHT of the label, so extraction is anchored after its own label.
# Without it, two labels sharing one physical line both pull the FIRST date.
.after_label <- function(line, term) {
  p <- regexpr(tolower(term), tolower(line), fixed = TRUE)
  if (p < 0) return(line)
  substr(line, p + attr(p, "match.length"), nchar(line))
}

# Returns list(value, values, raw, matched, n, conflict, term). The generic single-value extractor.
match_label <- function(spec, pages, dict = NULL) {
  if (is.character(spec)) spec <- list(any_of = spec)
  terms <- .spec_terms(spec)
  if (!is.null(spec$dict) && !is.null(dict) && !is.null(dict[[spec$dict]]))
    terms <- unique(c(terms, .spec_terms(dict[[spec$dict]])))
  vtype <- .spec_value_type(spec)
  occ   <- spec$occurrence %||% "first"
  scope <- .pages_in_scope(pages %||% character(0), spec$where)
  lines <- trimws(unlist(strsplit(paste(scope, collapse = "\n"), "\n", fixed = TRUE)))

  empty <- list(value = NA_character_, values = character(0), raw = NA_character_,
                matched = FALSE, n = 0L, conflict = FALSE,
                term = if (length(terms)) terms[1] else NA_character_)
  if (!length(terms) || !length(lines)) return(empty)

  # Candidate lines: contain ANY synonym, case-insensitive substring, in order.
  lc <- tolower(lines); hits <- integer(0); hit_terms <- character(0)
  for (t in terms) {
    idx <- which(grepl(tolower(t), lc, fixed = TRUE))
    hits <- c(hits, idx); hit_terms <- c(hit_terms, rep(t, length(idx)))
  }
  if (!length(hits)) return(empty)
  ord <- order(hits); hits <- hits[ord]; hit_terms <- hit_terms[ord]

  # Prefer a value ON the label line; only if none carry one, look at the next line.
  collect <- function(use_next) {
    vv <- character(0); rr <- character(0); tt <- character(0)
    for (i in seq_along(hits)) {
      li <- hits[i]; term <- hit_terms[i]; line <- lines[li]
      v <- if (identical(vtype, "text")) {
        .text_after(line, term)
      } else {
        # Prefer a value to the RIGHT of the label; if none, fall back to the WHOLE label line (a value
        # sitting to the LEFT) BEFORE ever reading the next line, or a right-empty label would wrongly
        # grab the following line's number.
        vr <- .value_from_line(.after_label(line, term), vtype)
        if (is.na(vr) || !nzchar(vr)) vr <- .value_from_line(line, vtype)
        vr
      }
      if ((is.na(v) || !nzchar(v)) && use_next && !identical(vtype, "text") && li < length(lines))
        v <- .value_from_line(lines[li + 1L], vtype)
      if (!is.na(v) && nzchar(v)) { vv <- c(vv, v); rr <- c(rr, line); tt <- c(tt, term) }
    }
    list(vals = vv, raws = rr, tms = tt)
  }
  got <- collect(FALSE)
  if (!length(got$vals)) got <- collect(TRUE)
  if (!length(got$vals)) return(modifyList(empty, list(matched = TRUE, n = length(hits))))

  uval <- unique(got$vals)
  conflict <- length(uval) > 1
  # occurrence is the primary selector; on_conflict only breaks ties when the caller left it at default.
  if (identical(occ, "all")) {
    value <- paste(uval, collapse = "; "); idx <- 1L
  } else if (identical(occ, "last")) {
    idx <- length(got$vals); value <- got$vals[idx]
  } else {
    idx <- 1L
    if (conflict && identical(spec$on_conflict %||% "flag", "last"))
      idx <- length(got$vals)
    value <- got$vals[idx]
  }
  list(value = value, values = got$vals, raw = got$raws[idx], matched = TRUE,
       n = length(got$vals), conflict = conflict, term = got$tms[idx])
}

# Teach the dictionary one more WORDING: a label the tool has never met is the commonest cause of a
# blank value. Writes through yaml_append_phrase() so the file keeps its comments.
dictionary_append <- function(field, phrase, path = NULL) {
  if (is.null(path)) path <- .dictionary_path()
  out <- yaml_append_phrase(path, field, phrase, under = "any_of")
  invisible(out)
}

# The label dictionary the ENGINE reads, resolved exactly as default_label_dict() does, so a write lands
# in the file the next conversion loads.
.DICTIONARY_CANDIDATES <- function() c(
  if (nzchar(Sys.getenv("BSO_DICTIONARY"))) Sys.getenv("BSO_DICTIONARY"),
  if (nzchar(Sys.getenv("ENGINE_ROOT")))
    file.path(Sys.getenv("ENGINE_ROOT"), "dictionaries", "labels.yaml"),
  tryCatch(load_config()$paths$dictionary, error = function(e) NULL) %||% NULL,
  file.path("dictionaries", "labels.yaml"))
.dictionary_path <- function() {
  cands <- .DICTIONARY_CANDIDATES()
  hit <- cands[file.exists(cands)]
  if (length(hit)) hit[1] else utils::tail(cands, 1)
}

# Named list of specs, empty list on any error.
load_label_dict <- function(path) {
  d <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (is.null(d)) list() else d
}

# The shipped base dictionary, resolved whether called from the repo root or under ENGINE_ROOT.
default_label_dict <- function() {
  # Resolve the dictionary the SAME way the lexicon does, so the engine reads exactly the file the Admin
  # editor writes to: BSO_DICTIONARY, the tests' ENGINE_ROOT copy, the configured path, then the shipped
  # default - the same list the WRITE side uses.
  cands <- .DICTIONARY_CANDIDATES()
  hit <- cands[file.exists(cands)]
  if (length(hit)) load_label_dict(hit[1]) else list()
}
