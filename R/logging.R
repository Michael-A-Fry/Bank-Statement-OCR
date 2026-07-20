# logging.R -- append-only run log (one JSON line per run, no raw content).

# log_event(logdir, record) -- append `record` as a single JSON line to
# <logdir>/runs.jsonl. Never writes raw statement content.
log_event <- function(logdir, record) {
  if (!dir.exists(logdir)) dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(logdir, "runs.jsonl")
  line <- jsonlite::toJSON(record, auto_unbox = TRUE, na = "null")
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(line, con)
  invisible(path)
}
