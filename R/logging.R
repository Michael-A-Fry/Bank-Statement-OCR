# logging.R -- append-only run log (one JSON line per run, no raw content).

# log_event(logdir, record, file) -- append `record` as a single JSON line to
# <logdir>/<file> (default runs.jsonl). Never writes raw statement content.
# One-line appends are atomic on POSIX for lines < PIPE_BUF (see the concurrency
# note in docs/architecture/deployment-integration-plan.md); high-concurrency
# deployments should move this to a database or add file locking.
log_event <- function(logdir, record, file = "runs.jsonl") {
  if (!dir.exists(logdir)) dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(logdir, file)
  line <- jsonlite::toJSON(record, auto_unbox = TRUE, na = "null")
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(line, con)
  invisible(path)
}
