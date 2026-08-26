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

# .upload_leaf(name) -- the stored copy's filename, forced to ONE path segment
# inside uploads/<id>/.
#
# `name` is the filename the BROWSER sent (Shiny hands input$file$name straight
# through; it never sanitises it), so it is attacker-controlled, and it was being
# pasted into file.path() unchecked. A name of "../../config/config.yaml" copied
# the uploaded file over that path instead -- and a client's bank statement written
# outside uploads/ is also outside purge_uploads(), so it would never be deleted by
# the retention that the Convert page promises. upload_file_path() already refuses
# a traversing id on the way OUT; this is the same guard on the way IN.
#
# Both separators are stripped, not just the host's: the deployment is Windows,
# where "..\\..\\x" traverses, and basename() on Linux would hand that back whole.
.upload_leaf <- function(name) {
  nm <- as.character(name %||% "")[1]
  nm <- if (is.na(nm)) "" else basename(gsub("\\\\", "/", nm))
  if (!nzchar(nm) || nm %in% c(".", "..")) nm <- "statement"
  nm
}

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
  #
  # UTC, from the one helper in R/util.R -- not the server's local clock. The id
  # of an upload and the run_id of the conversion that read it are the two handles
  # a reviewer ties together, and they were stamped in different zones: measured in
  # Pacific/Auckland from the same second, this folder read 20260826193118 and the
  # run_id 20260826073118, a different DAY, with nothing on either saying which was
  # which. Now they agree by construction.
  base <- paste0(substr(if (is.na(sha)) "na" else sha, 1, 10), "-", utc_id())
  id <- base; n <- 1L
  while (dir.exists(file.path(dir, id)) && n < 1000L) { n <- n + 1L; id <- sprintf("%s~%d", base, n) }
  ud <- file.path(dir, id); dir.create(ud, recursive = TRUE, showWarnings = FALSE)
  leaf <- .upload_leaf(name)
  safe(file.copy(path, file.path(ud, leaf), overwrite = TRUE))
  ts <- utc_stamp()
  # `file` stays the name as SENT -- the record is evidence about what arrived, and
  # the stored copy's own name is the thing that has to be safe, not the record of it.
  rec <- list(id = id, ts = ts, file = name, file_ext = tolower(tools::file_ext(leaf)),
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

# read_uploads(dir) -> data.frame of all upload records, newest first. For the
# Admin "Uploads & pickups" view. `abandoned` is inferred: unsupported/failed and
# never taught (no wizard_saved).
#
# TWO COLUMNS FOR THE ONE INSTANT, because they answer different questions.
# `ts` is the SHOWN form -- local, with the zone named ("26 Aug 2026 19:31 NZST")
# -- because this frame exists to be put in front of a person, and a bare
# 2026-08-26T07:31:18Z on a New Zealand screen reads as the previous evening.
# `ts_utc` is the record's own stamp, verbatim, for anything comparing or
# exporting. See R/util.R for the one rule both sides come from.
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
      # The saved statement was deleted by the retention purge. Shown in Admin so
      # "open it in the toolkit" never looks available for a file that is gone --
      # the record survives, the client's statement does not.
      purged = isTRUE(r$purged),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) return(data.frame())
  # Order on the INSTANT, not on the text of it. A plain string sort mixes the
  # records written before this tool settled on UTC (".. +1200") with the ones
  # written since (".. Z"), which are the same moment written two ways, and it
  # reverses the hour of the year the clocks go back. Anything unreadable sorts
  # last rather than jumping to the top of a page headed "newest first".
  when <- .parse_stamp(out$ts_utc)
  when[is.na(when)] <- as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC")
  out[order(when, decreasing = TRUE), , drop = FALSE]
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
