# suggestions.R -- the DETERMINISTIC half of the learning loop.
#
# The metadata capture records what the engine did NOT recognise: indicator tokens
# that matched neither the debit nor credit value (a bank writing "cow"/"horse"),
# and source columns no template mapped. This module aggregates those across the
# whole logs/metadata corpus into a ranked list of SUGGESTIONS -- "COW seen 40x as
# an unrecognised debit/credit indicator; add it to the vocabulary?".
#
# It only ever PROPOSES; a human approves a suggestion into the lexicon (or a
# template), and only then does the deterministic engine act on it. No model is
# involved yet -- the ranking is plain frequency. A local model can later slot in
# to rank / classify better, but the approval gate (and the determinism) stays.

# .read_metadata_raw(logdir, max_records) -- the NEWEST metadata records as parsed
# lists (nested structure preserved, unlike the flattened run-log reader). A
# half-written file is skipped.
#
# BOUNDED ON PURPOSE. logs/metadata keeps one JSON per conversion FOREVER (it is
# exempt from log rollup), and this is called from a Shiny observer: reading the
# whole corpus took ~6 s at 15k records, about a year of normal volume, and grows
# without limit. Suggestions are a frequency ranking of what the engine keeps
# missing -- the newest few thousand records answer that as well as all of them --
# so we read the newest `max_records` by file time and TELL the caller how many of
# how many were read, rather than quietly ranking a subset.
.read_metadata_raw <- function(logdir = "logs", max_records = 2000L) {
  dir <- file.path(logdir, "metadata")
  files <- if (dir.exists(dir)) list.files(dir, pattern = "\\.json$", full.names = TRUE) else character(0)
  total <- length(files)
  n <- suppressWarnings(as.integer(max_records %||% NA)[1])
  if (!is.na(n) && n > 0L && total > n) {
    mt <- suppressWarnings(as.numeric(file.mtime(files)))
    mt[is.na(mt)] <- -Inf
    # Newest first; the filename breaks ties so the choice is deterministic (two
    # records written in the same second must not depend on directory order).
    files <- files[order(mt, files, decreasing = TRUE)][seq_len(n)]
  }
  recs <- lapply(files, function(f)
    safe(jsonlite::fromJSON(paste(safe_readlines(f), collapse = "\n"), simplifyVector = FALSE), NULL))
  recs <- Filter(Negate(is.null), recs)
  attr(recs, "total") <- total
  attr(recs, "scanned") <- length(files)
  recs
}

# lexicon_suggestions(logdir, min_count, max_records) ->
#   list(indicator_tokens, unmapped_columns, scanned, total)
# The two frames are frequency-ranked; `min_count` hides one-offs. `scanned` /
# `total` let the UI say plainly which slice of the corpus the ranking came from
# (a bounded read that does not admit it is a silent half-answer).
lexicon_suggestions <- function(logdir = "logs", min_count = 1L, max_records = 2000L) {
  recs <- .read_metadata_raw(logdir, max_records = max_records)
  scanned <- attr(recs, "scanned") %||% length(recs)
  total   <- attr(recs, "total") %||% length(recs)
  toks <- character(0); cols <- character(0)
  for (r in recs) {
    nv <- r$novelty
    if (!is.null(nv$unrecognised_type_values))
      toks <- c(toks, toupper(trimws(as.character(unlist(nv$unrecognised_type_values)))))
    if (!is.null(nv$unmapped_columns))
      cols <- c(cols, trimws(as.character(unlist(nv$unmapped_columns))))
  }
  .rank <- function(x, name) {
    x <- x[nzchar(x)]
    if (!length(x)) return(data.frame(setNames(list(character(0)), name),
                                      count = integer(0), stringsAsFactors = FALSE))
    tab <- sort(table(x), decreasing = TRUE)
    tab <- tab[tab >= min_count]
    data.frame(setNames(list(names(tab)), name), count = as.integer(tab),
               stringsAsFactors = FALSE, row.names = NULL)
  }
  list(indicator_tokens = .rank(toks, "token"),
       unmapped_columns = .rank(cols, "column"),
       scanned = as.integer(scanned), total = as.integer(total))
}

# lexicon_append(category, values, path) -- APPROVE a suggestion: add `values` to
# a LIST category of the lexicon file (backup first, then clear the cache so the
# next conversion sees it). Only list categories are appendable this way; a
# regex/table/map is edited in the Admin vocabulary editor. Returns TRUE on
# success, with attr "reason" -- one plain sentence the screen can show as is.
#
# The write goes through yaml_append_phrase() (R/util.R), which INSERTS the line
# rather than dumping the parsed object back over the file. That matters here:
# dictionaries/lexicon.yaml opens with the explanation of how each category merges
# and a worked cow/horse example, and a whole-file rewrite deleted all of it the
# first time anyone approved a word -- taking the file's own documentation with it.
lexicon_append <- function(category, values, path = .lexicon_path()) {
  if (!identical(.lexicon_spec()[[category]], "list"))
    return(invisible(structure(FALSE,
      reason = "that kind of vocabulary is edited in the whole-file editor, not one word at a time")))
  values <- trimws(as.character(values)); values <- values[nzchar(values)]
  if (!length(values))
    return(invisible(structure(FALSE, reason = "type the word first")))
  res <- lapply(values, function(v) yaml_append_phrase(path, category, v))
  ok <- all(vapply(res, isTRUE, logical(1)))
  if (ok) clear_lexicon_cache()
  invisible(structure(ok, reason = attr(res[[length(res)]], "reason")))
}
