# uploads.R -- capture every uploaded statement and track its lifecycle, so a new format that fails is
# a two-second pickup. The files are real statements: local-only, and Admin never shows their content.

.uploads_dir <- function(dir = NULL) dir %||% file.path(Sys.getenv("BSO_ROOT", "."), "uploads")

# The stored copy's filename, forced to ONE path segment inside uploads/<id>/. `name` is what the
# BROWSER sent - Shiny never sanitises it - and it was pasted into file.path() unchecked: a name of
# "../../config/config.yaml" copied the upload over that path, outside uploads/ and so outside
# purge_uploads(). BOTH separators are stripped: basename() on Linux hands "..\\..\\x" back whole.
.upload_leaf <- function(name) {
  nm <- as.character(name %||% "")[1]
  nm <- if (is.na(nm)) "" else basename(gsub("\\\\", "/", nm))
  if (!nzchar(nm) || nm %in% c(".", "..")) nm <- "statement"
  nm
}

# Copies the file into uploads/<id>/ and writes record.json.
record_upload <- function(path, name = basename(path), requested_by = NULL,
                          status = "uploaded", run_id = NA_character_,
                          template = NA_character_, trust = NA_character_,
                          detail = "", dir = NULL) {
  dir <- .uploads_dir(dir)
  sha <- safe(file_sha256(path), NA_character_)
  # The id is content hash plus whole second, so the same statement uploaded twice in one second shared
  # one record.json and lost the first one's history; a clash gets a "~2" suffix. UTC, because an upload
  # id and its run_id stamped in different zones read as different DAYS.
  base <- paste0(substr(if (is.na(sha)) "na" else sha, 1, 10), "-", utc_id())
  id <- base; n <- 1L
  while (dir.exists(file.path(dir, id)) && n < 1000L) { n <- n + 1L; id <- sprintf("%s~%d", base, n) }
  ud <- file.path(dir, id); dir.create(ud, recursive = TRUE, showWarnings = FALSE)
  leaf <- .upload_leaf(name)
  safe(file.copy(path, file.path(ud, leaf), overwrite = TRUE))
  ts <- utc_stamp()
  # `file` stays the name as SENT - the record is evidence about what arrived; the COPY's name is safe.
  rec <- list(id = id, ts = ts, file = name, file_ext = tolower(tools::file_ext(leaf)),
              sha256 = sha, requested_by = requested_by %||% "unknown",
              status = status, run_id = run_id, template = template, trust = trust,
              detail = detail, history = list(list(ts = ts, status = status)))
  safe(writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE),
                  file.path(ud, "record.json")))
  id
}

# Append a lifecycle transition and update the current status.
set_upload_status <- function(id, status, run_id = NULL, template = NULL,
                              trust = NULL, detail = NULL, dir = NULL) {
  dir <- .uploads_dir(dir); f <- file.path(dir, id, "record.json")
  if (!file.exists(f)) return(invisible(FALSE))
  rec <- safe(jsonlite::fromJSON(f, simplifyVector = FALSE), NULL); if (is.null(rec)) return(invisible(FALSE))
  ts <- utc_stamp()
  rec$status <- status
  if (!is.null(run_id))   rec$run_id <- run_id
  if (!is.null(template)) rec$template <- template
  if (!is.null(trust))    rec$trust <- trust
  if (!is.null(detail))   rec$detail <- detail
  rec$history <- c(rec$history, list(list(ts = ts, status = status)))
  safe(writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE), f))
  invisible(TRUE)
}

# All upload records, newest first. `abandoned` is inferred: unsupported or failed and never taught.
# TWO COLUMNS FOR ONE INSTANT: `ts` is local with the zone named, because a bare Z stamp on a New
# Zealand screen reads as the previous evening; `ts_utc` is the record's own stamp, verbatim.
read_uploads <- function(dir = NULL) {
  dir <- .uploads_dir(dir)
  recs <- Sys.glob(file.path(dir, "*", "record.json"))
  if (!length(recs)) return(data.frame(
    id = character(0), ts = character(0), ts_utc = character(0),
    file_ext = character(0), status = character(0),
    template = character(0), trust = character(0), run_id = character(0),
    needs_pickup = logical(0), purged = logical(0), stringsAsFactors = FALSE))
  rows <- lapply(recs, function(f) {
    r <- safe(jsonlite::fromJSON(f, simplifyVector = FALSE), NULL); if (is.null(r)) return(NULL)
    hist_status <- vapply(r$history %||% list(), function(h) h$status %||% "", character(1))
    taught <- "wizard_saved" %in% hist_status
    data.frame(id = r$id %||% NA_character_,
      ts = local_time_text(r$ts %||% NA_character_),
      ts_utc = as.character(r$ts %||% NA_character_),
      file_ext = r$file_ext %||% NA_character_, status = r$status %||% NA_character_,
      template = as.character(r$template %||% NA_character_),
      trust = as.character(r$trust %||% NA_character_),
      run_id = as.character(r$run_id %||% NA_character_),
      needs_pickup = (r$status %in% c("unsupported", "failed")) && !taught,
      # Deleted by the retention purge. Shown so "open it in the toolkit" never looks available.
      purged = isTRUE(r$purged),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) return(data.frame())
  # Order on the INSTANT, not on the text of it: a plain string sort mixes records written before this
  # tool settled on UTC with the ones since, and reverses the hour the clocks go back. Unreadable
  # stamps sort last rather than to the top of a page headed "newest first".
  when <- .parse_stamp(out$ts_utc)
  when[is.na(when)] <- as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC")
  out[order(when, decreasing = TRUE), , drop = FALSE]
}

# The saved statement path for re-audit, or NA.
upload_file_path <- function(id, dir = NULL) {
  # `id` arrives from the browser: a "/", "\\" or ".." would walk out of uploads/ and hand back any file
  # the server can read, so anything but a plain single path segment is refused rather than sanitised.
  if (length(id) != 1L || is.na(id) || !nzchar(id) ||
      grepl("[/\\\\]", id) || id %in% c(".", "..") || grepl("(^|[/\\\\])\\.\\.($|[/\\\\])", id))
    return(NA_character_)
  ud <- file.path(.uploads_dir(dir), id)
  fs <- setdiff(list.files(ud, full.names = TRUE), file.path(ud, "record.json"))
  if (length(fs)) fs[1] else NA_character_
}
