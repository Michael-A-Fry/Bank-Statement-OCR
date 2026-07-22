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

# .read_metadata_raw(logdir) -- every metadata record as a parsed list (nested
# structure preserved, unlike the flattened run-log reader). A half-written file
# is skipped.
.read_metadata_raw <- function(logdir = "logs") {
  dir <- file.path(logdir, "metadata")
  files <- if (dir.exists(dir)) list.files(dir, pattern = "\\.json$", full.names = TRUE) else character(0)
  recs <- lapply(files, function(f)
    safe(jsonlite::fromJSON(paste(safe_readlines(f), collapse = "\n"), simplifyVector = FALSE), NULL))
  Filter(Negate(is.null), recs)
}

# lexicon_suggestions(logdir, min_count) -> list(indicator_tokens, unmapped_columns),
# each a frequency-ranked data.frame. `min_count` hides one-offs.
lexicon_suggestions <- function(logdir = "logs", min_count = 1L) {
  recs <- .read_metadata_raw(logdir)
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
       unmapped_columns = .rank(cols, "column"))
}

# lexicon_append(category, values, path) -- APPROVE a suggestion: union `values`
# into a LIST category of the lexicon file (backup first, then clear the cache so
# the next conversion sees it). Only list categories are appendable this way; a
# regex/table/map is edited in the Admin vocabulary editor. Returns TRUE on success.
lexicon_append <- function(category, values, path = .lexicon_path()) {
  if (!identical(.lexicon_spec()[[category]], "list")) return(invisible(FALSE))
  values <- trimws(as.character(values)); values <- values[nzchar(values)]
  if (!length(values)) return(invisible(FALSE))
  raw <- if (!is.null(path) && file.exists(path)) safe(yaml::read_yaml(path), list()) else list()
  if (!is.list(raw)) raw <- list()
  cur <- as.character(unlist(raw[[category]] %||% character(0)))
  raw[[category]] <- as.list(unique(c(cur, values)))
  if (!is.null(path)) safe(file.copy(path, paste0(path, ".bak"), overwrite = TRUE))
  ok <- isTRUE(tryCatch({
    if (!is.null(path)) dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    yaml::write_yaml(raw, path); TRUE }, error = function(e) FALSE))
  if (ok) clear_lexicon_cache()
  invisible(ok)
}
