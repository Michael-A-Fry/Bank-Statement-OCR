# uploads.R -- capture every uploaded statement and track its lifecycle, so a new
# format that fails or is abandoned is a 2-second pickup: the file is saved, its
# status (converted / needs-review / unsupported / failed / wizard-started /
# wizard-saved) is recorded with the run_id and template, and a safe audit can be
# regenerated on demand. The saved files hold real statements, so the uploads
# folder is local-only; the Admin view never shows their content, only status.
# They are NOT kept forever: purge_uploads() (R/retention.R) deletes the copied
# statement after retention.uploads_keep_days and marks the record `purged`, and
# the Convert page says so under the file picker before anyone uploads.

.uploads_dir <- function(dir = NULL) dir %||% file.path(Sys.getenv("BSO_ROOT", "."), "uploads")

# record_upload(path, name, ...) -> upload_id. Copies the file into
# uploads/<id>/ and writes record.json. `status` is the initial lifecycle state.
record_upload <- function(path, name = basename(path), requested_by = NULL,
                          status = "uploaded", run_id = NA_character_,
                          template = NA_character_, trust = NA_character_,
                          detail = "", dir = NULL) {
  dir <- .uploads_dir(dir)
  sha <- safe(file_sha256(path), NA_character_)
  # The id is (content hash + whole second), so uploading the SAME statement twice
  # inside one second produced the same id -- and both uploads then shared one
  # folder and one record.json, silently losing the first one's lifecycle history.
  # A clash gets a "~2", "~3" ... suffix: two uploads, two records.
  base <- paste0(substr(if (is.na(sha)) "na" else sha, 1, 10), "-",
                 format(Sys.time(), "%Y%m%d%H%M%S"))
  id <- base; n <- 1L
  while (dir.exists(file.path(dir, id)) && n < 1000L) { n <- n + 1L; id <- sprintf("%s~%d", base, n) }
  ud <- file.path(dir, id); dir.create(ud, recursive = TRUE, showWarnings = FALSE)
  safe(file.copy(path, file.path(ud, name), overwrite = TRUE))
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  rec <- list(id = id, ts = ts, file = name, file_ext = tolower(tools::file_ext(name)),
              sha256 = sha, requested_by = requested_by %||% "unknown",
              status = status, run_id = run_id, template = template, trust = trust,
              detail = detail, history = list(list(ts = ts, status = status)))
  safe(writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE),
                  file.path(ud, "record.json")))
  id
}

# set_upload_status(id, status, ...) -- append a lifecycle transition and update
# the current status (+ optionally the run_id/template/trust/detail).
set_upload_status <- function(id, status, run_id = NULL, template = NULL,
                              trust = NULL, detail = NULL, dir = NULL) {
  dir <- .uploads_dir(dir); f <- file.path(dir, id, "record.json")
  if (!file.exists(f)) return(invisible(FALSE))
  rec <- safe(jsonlite::fromJSON(f, simplifyVector = FALSE), NULL); if (is.null(rec)) return(invisible(FALSE))
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  rec$status <- status
  if (!is.null(run_id))   rec$run_id <- run_id
  if (!is.null(template)) rec$template <- template
  if (!is.null(trust))    rec$trust <- trust
  if (!is.null(detail))   rec$detail <- detail
  rec$history <- c(rec$history, list(list(ts = ts, status = status)))
  safe(writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE), f))
  invisible(TRUE)
}

# read_uploads(dir) -> data.frame of all upload records, newest first. For the
# Admin "Uploads & pickups" view. `abandoned` is inferred: unsupported/failed and
# never taught (no wizard_saved).
read_uploads <- function(dir = NULL) {
  dir <- .uploads_dir(dir)
  recs <- Sys.glob(file.path(dir, "*", "record.json"))
  if (!length(recs)) return(data.frame(
    id = character(0), ts = character(0), file_ext = character(0), status = character(0),
    template = character(0), trust = character(0), run_id = character(0),
    needs_pickup = logical(0), purged = logical(0), stringsAsFactors = FALSE))
  rows <- lapply(recs, function(f) {
    r <- safe(jsonlite::fromJSON(f, simplifyVector = FALSE), NULL); if (is.null(r)) return(NULL)
    hist_status <- vapply(r$history %||% list(), function(h) h$status %||% "", character(1))
    taught <- "wizard_saved" %in% hist_status
    data.frame(id = r$id %||% NA_character_, ts = r$ts %||% NA_character_,
      file_ext = r$file_ext %||% NA_character_, status = r$status %||% NA_character_,
      template = as.character(r$template %||% NA_character_),
      trust = as.character(r$trust %||% NA_character_),
      run_id = as.character(r$run_id %||% NA_character_),
      needs_pickup = (r$status %in% c("unsupported", "failed")) && !taught,
      # The saved statement was deleted by the retention purge. Shown in Admin so
      # "open it in the toolkit" never looks available for a file that is gone --
      # the record survives, the client's statement does not.
      purged = isTRUE(r$purged),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) return(data.frame())
  out[order(out$ts, decreasing = TRUE), , drop = FALSE]
}

# upload_file_path(id, dir) -> the saved statement path (for re-audit), or NA.
upload_file_path <- function(id, dir = NULL) {
  # `id` arrives from the browser. A "/", "\\" or ".." in it would walk straight out
  # of uploads/ and hand back any file the server process can read, so an id that is
  # not a plain single path segment is refused outright rather than sanitised into
  # something that merely looks safe.
  if (length(id) != 1L || is.na(id) || !nzchar(id) ||
      grepl("[/\\\\]", id) || id %in% c(".", "..") || grepl("(^|[/\\\\])\\.\\.($|[/\\\\])", id))
    return(NA_character_)
  ud <- file.path(.uploads_dir(dir), id)
  fs <- setdiff(list.files(ud, full.names = TRUE), file.path(ud, "record.json"))
  if (length(fs)) fs[1] else NA_character_
}
