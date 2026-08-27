# suggestions.R -- the DETERMINISTIC half of the learning loop: rank what the metadata capture says the
# engine did NOT recognise. It only ever PROPOSES; a human approves one into the lexicon or a template.

# The NEWEST metadata records. BOUNDED ON PURPOSE: logs/metadata keeps one JSON per conversion for ever
# and this runs in a Shiny observer, where the whole corpus took 6 seconds at 15k records. The caller is
# TOLD how many of how many were read rather than quietly ranking a subset.
.read_metadata_raw <- function(logdir = "logs", max_records = 2000L) {
  dir <- file.path(logdir, "metadata")
  files <- if (dir.exists(dir)) list.files(dir, pattern = "\\.json$", full.names = TRUE) else character(0)
  total <- length(files)
  n <- suppressWarnings(as.integer(max_records %||% NA)[1])
  if (!is.na(n) && n > 0L && total > n) {
    mt <- suppressWarnings(as.numeric(file.mtime(files)))
    mt[is.na(mt)] <- -Inf
    # Newest first; the filename breaks ties so the choice is deterministic in the same second.
    files <- files[order(mt, files, decreasing = TRUE)][seq_len(n)]
  }
  recs <- lapply(files, function(f)
    safe(jsonlite::fromJSON(paste(safe_readlines(f), collapse = "\n"), simplifyVector = FALSE), NULL))
  recs <- Filter(Negate(is.null), recs)
  attr(recs, "total") <- total
  attr(recs, "scanned") <- length(files)
  recs
}

# Returns list(indicator_tokens, unmapped_columns, scanned, total), frequency-ranked; `min_count` hides
# one-offs, and scanned/total let the UI say which slice the ranking came from.
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

# APPROVE a suggestion: add `values` to a LIST category of the lexicon, backing up first and clearing the
# cache. yaml_append_phrase() INSERTS the line rather than dumping the parsed object back - lexicon.yaml
# opens with an explanation that a whole-file rewrite deleted.
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
