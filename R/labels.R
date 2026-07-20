# labels.R -- declarative label dictionary + matcher. This is how the engine
# stays generic to the "hundreds of different wordings" problem for SINGLE
# LABELLED VALUES: "opening balance" vs "balance brought forward" vs "starting
# balance" vs "opening:", statement dates, account names, IRD form fields.
#
# IMPORTANT: transaction TABLES do NOT use this -- they map by column header
# (delimited/excel) or x-band (pdf) and never match on wording. This module is
# only for labelled scalars (metadata + fields mode). Nothing here is hardcoded
# to a bank: the vocabulary lives in dictionaries/labels.yaml and in templates,
# never in code.
#
# A matcher spec (from YAML or an R list) understands:
#   any_of      : character vector of label phrases (SYNONYMS). `label:` (a
#                 single string) is accepted as a back-compat alias.
#   dict        : a key in the base dictionary whose synonyms are merged in.
#   value       : "money" (default) | "date" | "date_range" | "text" |
#                 "regex:<pattern>". Back-compat: a `pattern:` key => regex.
#   occurrence  : "first" (default) | "last" | "all"    -> handles REPEATS.
#   where       : "any" (default) | "page1" | "last_page" | <integer page>.
#   required    : logical (default FALSE)               -> EXIST / NOT EXIST.
#   on_conflict : "flag" (default) | "first" | "last"   -> matches disagree.

# A money token, SIGN-AWARE. Matches a leading currency/sign/paren, thousands
# grouped with , or . (or space), a 1-2 place decimal (either . or , so European
# "1.234,56" is captured whole), and an OPTIONAL trailing DR/CR/OD balance marker
# -- so an overdrawn "1,234.56 DR" is captured with its sign intact for .num to
# read (the old pattern stopped at the digits and silently dropped the sign).
# `(?![0-9])` after the cents stops "1,234" being mis-read as "1,23". Consumed
# with perl = TRUE + ignore.case in .value_from_line (lookahead needs perl).
.MONEY_RX <- "\\(?[$]?-?[0-9][0-9,.]*(?:\\.[0-9]{2}|,[0-9]{2})(?![0-9])\\)?(?:\\s?(?:DR|CR|OD))?"
.DATE_RX  <- "[0-9]{1,2}[/ .-][A-Za-z0-9]{2,9}[/ .-][0-9]{2,4}"

# .spec_terms(spec) -- the list of label phrases a spec matches on.
.spec_terms <- function(spec) as.character(spec$any_of %||% spec$label %||% character(0))

# .spec_value_type(spec) -- the value kind to pull once a label line is found.
.spec_value_type <- function(spec) {
  if (!is.null(spec$pattern) && nzchar(spec$pattern)) return(paste0("regex:", spec$pattern))
  spec$value %||% "money"
}

# .pages_in_scope(pages, where) -- restrict the search to the requested pages.
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

# .value_from_line(line, vtype) -- extract the typed value from one line, or NA.
.value_from_line <- function(line, vtype) {
  if (grepl("^regex:", vtype)) {
    pat <- sub("^regex:", "", vtype)
    m <- regmatches(line, regexpr(pat, line, perl = TRUE))
    return(if (length(m) && nzchar(m)) m[1] else NA_character_)
  }
  switch(vtype,
    money = { m <- regmatches(line, gregexpr(.MONEY_RX, line, perl = TRUE, ignore.case = TRUE))[[1]]
              if (length(m)) m[length(m)] else NA_character_ },   # value sits to the right
    date  = { m <- regmatches(line, regexpr(.DATE_RX, line))
              if (length(m)) m[1] else NA_character_ },
    date_range = { m <- regmatches(line, gregexpr(.DATE_RX, line))[[1]]
                   if (length(m) >= 2) paste(m[1], "|", m[2]) else NA_character_ },
    NA_character_)   # "text" handled by the caller (needs the label position)
}

# .text_after(line, term) -- the trimmed remainder of a line after the label
# (for `value: text`, e.g. "Account name: O'Connor & Sons" -> "O'Connor & Sons").
.text_after <- function(line, term) {
  p <- regexpr(tolower(term), tolower(line), fixed = TRUE)
  if (p < 0) return(NA_character_)
  rest <- substr(line, p + attr(p, "match.length"), nchar(line))
  rest <- trimws(sub("^[ :–-]+", "", rest))
  if (nzchar(rest)) rest else NA_character_
}

# .after_label(line, term) -- the raw remainder of a line to the RIGHT of the
# label, so a value extraction is anchored after its own label. This matters when
# two labels share one physical line ("Opening date 1 Jan 26  Closing date 31 Jan
# 26"): without it, both labels would pull the FIRST date on the line and the
# period would collapse to a single date. Falls back to the whole line if the term
# somehow isn't found (it always is -- this is only called on matched lines).
.after_label <- function(line, term) {
  p <- regexpr(tolower(term), tolower(line), fixed = TRUE)
  if (p < 0) return(line)
  substr(line, p + attr(p, "match.length"), nchar(line))
}

# match_label(spec, pages, dict) -> list(value, values, raw, matched, n,
# conflict, term). The generic single-value extractor everything routes through.
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

  # candidate lines: contain ANY synonym (case-insensitive substring), in order.
  lc <- tolower(lines); hits <- integer(0); hit_terms <- character(0)
  for (t in terms) {
    idx <- which(grepl(tolower(t), lc, fixed = TRUE))
    hits <- c(hits, idx); hit_terms <- c(hit_terms, rep(t, length(idx)))
  }
  if (!length(hits)) return(empty)
  ord <- order(hits); hits <- hits[ord]; hit_terms <- hit_terms[ord]

  # Prefer a value ON the label line; only if none carry one, look at the next
  # line (label above value). This skips heading/annotation lines cleanly.
  collect <- function(use_next) {
    vv <- character(0); rr <- character(0); tt <- character(0)
    for (i in seq_along(hits)) {
      li <- hits[i]; term <- hit_terms[i]; line <- lines[li]
      v <- if (identical(vtype, "text")) {
        .text_after(line, term)
      } else {
        # Prefer a value to the RIGHT of the label (handles two labels on one
        # line); if none, fall back to the WHOLE label line (handles a value that
        # sits to the LEFT, e.g. "1,234.56 Closing balance") BEFORE ever reading
        # the next line -- otherwise a right-empty label would wrongly grab the
        # following line's number.
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
  # occurrence is the primary selector. on_conflict only breaks ties when the
  # caller left occurrence at its default ("first") and the matches disagree.
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

# load_label_dict(path) -> named list of specs (empty list on any error).
load_label_dict <- function(path) {
  d <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (is.null(d)) list() else d
}

# default_label_dict() -- the shipped base dictionary, resolved robustly whether
# called from the repo root (app/CLI) or under ENGINE_ROOT (tests).
default_label_dict <- function() {
  cands <- c(
    if (nzchar(Sys.getenv("ENGINE_ROOT")))
      file.path(Sys.getenv("ENGINE_ROOT"), "dictionaries", "labels.yaml"),
    file.path("dictionaries", "labels.yaml"))
  hit <- cands[file.exists(cands)]
  if (length(hit)) load_label_dict(hit[1]) else list()
}
