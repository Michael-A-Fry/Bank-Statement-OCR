# logging.R -- run and feedback logs. ONE FILE PER EVENT, never a shared append, so records cannot
# interleave, corrupt or need a lock. The concurrency story: docs/context/how-it-fits-together.md.

# Make a string safe to use as a filename.
.safe_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", as.character(x))

# Write one JSON file at <logdir>/<subdir>/<id>.json. NEVER OVERWRITES: run_id is content hash plus whole
# second, so the same statement converted twice in one second reused the id and opening "w" ERASED the
# first conversion's audit record. A clash gets a "~2" suffix, so it is visible in the filename.
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

# The WHO of a conversion, as two facts that must never be collapsed: attested_by is what the person
# TYPED (a claim), detected_identity is what the machine established and is never overwritten by the
# claim. Only the typed name made the audit self-declared; only the detected one stamped every run alike.
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

# Add fields to an ALREADY-WRITTEN record, since the UI knows things the engine does not. Refuses when
# the record is missing or the id is ambiguous (a same-second clash left a "~2" sibling): stamping one
# person's name onto another's conversion is precisely the silently-wrong outcome forbidden.
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

# Bind a list of named-list records into one frame, aligning on the union of fields.
.rows_bind <- function(records) {
  cols <- unique(unlist(lapply(records, names)))
  rows <- lapply(records, function(r) {
    vals <- lapply(cols, function(c) { v <- r[[c]]; if (is.null(v) || length(v) == 0) NA else v[[1]] })
    names(vals) <- cols
    as.data.frame(vals, stringsAsFactors = FALSE)
  })
  do.call(rbind, c(rows, list(stringsAsFactors = FALSE)))
}

# Read every JSON file in the folder into a single frame. A half-written file is skipped.
read_log_records <- function(logdir, subdir) {
  dir <- file.path(logdir, subdir)
  files <- if (dir.exists(dir)) list.files(dir, pattern = "\\.json$", full.names = TRUE) else character(0)
  recs <- lapply(files, function(f)
    safe(jsonlite::fromJSON(paste(safe_readlines(f), collapse = "\n")), NULL))
  recs <- Filter(Negate(is.null), recs)
  if (!length(recs)) return(data.frame())
  .rows_bind(recs)
}
