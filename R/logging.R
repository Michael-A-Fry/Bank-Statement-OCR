# logging.R -- run + feedback logs. Concurrency is handled by the SIMPLEST thing
# that cannot break: ONE FILE PER EVENT, never a shared append.
#
# When ten people convert statements at the same moment (even off the same
# network share), each conversion writes its OWN file -- logs/runs/<run_id>.json
# -- so records can never interleave, corrupt, or need a lock. There is no
# database, no server, no API, nothing to tune. To read the log you list a
# folder; to open one record you double-click a .json in Notepad. That is the
# entire concurrency story (see docs/context/architecture/deployment-integration-plan.md).
# No raw statement content is ever written -- only metadata about the run.

# .safe_name(x) -- make a string safe to use as a filename.
.safe_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", as.character(x))

# write_log_record(logdir, subdir, id, record) -- write one JSON file at
# <logdir>/<subdir>/<id>.json. Returns the path (invisibly).
#
# NEVER OVERWRITES. run_id is (content hash + whole second), so converting the
# same statement twice inside one second produces the same id -- and opening the
# file "w" meant the second conversion silently ERASED the first one's audit
# record. In a forensic tool a destroyed audit record is the cardinal failure, so
# a clashing id gets a "~2", "~3" ... suffix instead: two conversions, two
# records, and the clash is visible in the filename rather than invisible.
write_log_record <- function(logdir, subdir, id, record) {
  dir <- file.path(logdir, subdir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  base <- .safe_name(id)
  path <- file.path(dir, paste0(base, ".json"))
  n <- 1L
  while (file.exists(path) && n < 1000L) {
    n <- n + 1L
    path <- file.path(dir, sprintf("%s~%d.json", base, n))
  }
  txt <- jsonlite::toJSON(record, auto_unbox = TRUE, na = "null", pretty = TRUE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(txt, con)
  invisible(path)
}

# identity_fields(attested, detected, source) -- the WHO of a conversion, as two
# SEPARATE facts that must never be collapsed into one.
#
#   attested_by       what the person TYPED on Convert. A claim. May be blank.
#   detected_identity what the machine could actually establish. Never overwritten
#                     by the claim.
#   identity_source   how detected_identity was established, so a reader can judge
#                     it: "host" (the Shiny host's authenticated user), "sso" (an
#                     identity forwarded by a proxy/gateway), "os" (the account the
#                     SERVER process runs as -- in the shipped one-server model
#                     that is the SAME for everybody and identifies nobody), or
#                     "none".
# Recording only the typed name made the audit trail self-declared; recording only
# the detected one stamped every conversion in the department identically. Both,
# labelled, is the only honest answer.
.IDENTITY_SOURCES <- c("host", "sso", "os", "none")
identity_fields <- function(attested = NA_character_, detected = NA_character_,
                            source = "none") {
  clean <- function(x) {
    x <- trimws(as.character(x %||% NA_character_)[1])
    if (is.na(x) || !nzchar(x)) NA_character_ else x
  }
  src <- as.character(source %||% "none")[1]
  if (is.na(src) || !(src %in% .IDENTITY_SOURCES)) src <- "none"
  list(attested_by = clean(attested), detected_identity = clean(detected),
       identity_source = src)
}

# amend_log_record(logdir, subdir, id, fields) -> TRUE when the record was amended.
#
# Adds fields to an ALREADY-WRITTEN record (the UI knows things the engine does
# not -- who attested the run, and what the machine could actually establish about
# them). Refuses, returning FALSE, when the record is missing OR when the id is
# ambiguous (a same-second clash left a "~2" sibling): stamping one person's name
# onto another person's conversion is precisely the silently-wrong outcome the
# charter forbids, so it fails closed and the caller says so out loud.
amend_log_record <- function(logdir, subdir, id, fields) {
  if (!length(fields) || !is.list(fields)) return(invisible(FALSE))
  dir <- file.path(logdir, subdir)
  base <- .safe_name(id)
  path <- file.path(dir, paste0(base, ".json"))
  if (!file.exists(path)) return(invisible(FALSE))
  if (length(Sys.glob(file.path(dir, paste0(base, "~*.json"))))) return(invisible(FALSE))
  rec <- safe(jsonlite::fromJSON(paste(safe_readlines(path), collapse = "\n"),
                                 simplifyVector = FALSE), NULL)
  if (!is.list(rec)) return(invisible(FALSE))
  for (k in names(fields)) rec[[k]] <- fields[[k]]
  txt <- jsonlite::toJSON(rec, auto_unbox = TRUE, na = "null", pretty = TRUE)
  ok <- isTRUE(tryCatch({
    con <- file(path, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(txt, con); TRUE
  }, error = function(e) FALSE))
  invisible(ok)
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
