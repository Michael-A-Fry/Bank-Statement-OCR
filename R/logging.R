# logging.R -- run + feedback logs. Concurrency is handled by the SIMPLEST thing
# that cannot break: ONE FILE PER EVENT, never a shared append.
#
# When ten people convert statements at the same moment (even off the same
# network share), each conversion writes its OWN file -- logs/runs/<run_id>.json
# -- so records can never interleave, corrupt, or need a lock. There is no
# database, no server, no API, nothing to tune. To read the log you list a
# folder; to open one record you double-click a .json in Notepad. That is the
# entire concurrency story (see docs/architecture/deployment-integration-plan.md).
# No raw statement content is ever written -- only metadata about the run.

# .safe_name(x) -- make a string safe to use as a filename.
.safe_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", as.character(x))

# write_log_record(logdir, subdir, id, record) -- write one JSON file at
# <logdir>/<subdir>/<id>.json. Returns the path (invisibly).
write_log_record <- function(logdir, subdir, id, record) {
  dir <- file.path(logdir, subdir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(.safe_name(id), ".json"))
  txt <- jsonlite::toJSON(record, auto_unbox = TRUE, na = "null", pretty = TRUE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(txt, con)
  invisible(path)
}

# .rows_bind(records) -- bind a list of named-list records into one data.frame,
# aligning on the union of fields and coalescing NULL/absent to NA.
.rows_bind <- function(records) {
  cols <- unique(unlist(lapply(records, names)))
  rows <- lapply(records, function(r) {
    vals <- lapply(cols, function(c) { v <- r[[c]]; if (is.null(v) || length(v) == 0) NA else v[[1]] })
    names(vals) <- cols
    as.data.frame(vals, stringsAsFactors = FALSE)
  })
  do.call(rbind, c(rows, list(stringsAsFactors = FALSE)))
}

# read_log_records(logdir, subdir) -- read every JSON file in the folder into a
# single data.frame (empty frame if none). A half-written file is skipped.
read_log_records <- function(logdir, subdir) {
  dir <- file.path(logdir, subdir)
  files <- if (dir.exists(dir)) list.files(dir, pattern = "\\.json$", full.names = TRUE) else character(0)
  recs <- lapply(files, function(f)
    safe(jsonlite::fromJSON(paste(safe_readlines(f), collapse = "\n")), NULL))
  recs <- Filter(Negate(is.null), recs)
  if (!length(recs)) return(data.frame())
  .rows_bind(recs)
}
