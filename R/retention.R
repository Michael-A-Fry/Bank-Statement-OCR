# retention.R -- everything this tool leaves on disk, and when it goes away. Four jobs, all safe to run
# repeatedly: rollup_logs() archives the per-event log folders into yearly JSONL; purge_uploads()
# deletes the copied statement and keeps its record.json; sweep_temp_dirs() clears stale scratch
# folders; trim_log_file() keeps the recent tail and says so. Nothing is lost.

# Every folder under logs\\ that gains one file per event. There are three and the rollup knew two:
# logs\\feed\\ grew at the same rate as logs\\runs\\ with nothing archiving it.
LOG_ROLLUP_SUBDIRS <- c("runs", "feedback", "feed")

# Files older than keep_days move to <logdir>/archive/<subdir>-<year>.jsonl. `subdir` defaults to every
# folder that grows, so tidying the logs cannot be given a short list by accident.
rollup_logs <- function(logdir, subdir = LOG_ROLLUP_SUBDIRS, keep_days = 90,
                        now = as.numeric(Sys.time())) {
  subdir <- as.character(subdir)
  if (length(subdir) != 1L) {
    out <- list(archived = 0L, kept = 0L)
    for (s in subdir) {
      r <- rollup_logs(logdir, s, keep_days = keep_days, now = now)
      out$archived <- out$archived + r$archived; out$kept <- out$kept + r$kept
    }
    # The error log is not one-file-per-event, so it is trimmed rather than archived - but it grows in
    # the same folder for the same reason, and this is the one call that tidies that folder.
    out$trimmed <- isTRUE(trim_log_file(file.path(logdir, "errors.log")))
    return(out)
  }
  dir <- file.path(logdir, subdir)
  if (!dir.exists(dir)) return(list(archived = 0L, kept = 0L))
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) return(list(archived = 0L, kept = 0L))
  cutoff <- now - keep_days * 86400
  archdir <- file.path(logdir, "archive")
  if (!dir.exists(archdir)) dir.create(archdir, recursive = TRUE, showWarnings = FALSE)
  archived <- 0L; kept <- 0L
  for (f in files) {
    mt <- as.numeric(file.info(f)$mtime)
    if (is.na(mt) || mt >= cutoff) { kept <- kept + 1L; next }
    rec <- safe(jsonlite::fromJSON(paste(safe_readlines(f), collapse = "\n")), NULL)
    if (is.null(rec)) { kept <- kept + 1L; next }
    yr <- substr(as.character(rec$ts %||% ""), 1, 4)
    # UTC, so which archive a record lands in does not depend on which side of midnight the clock was.
    if (!nzchar(yr)) yr <- format(as.POSIXct(mt, origin = "1970-01-01", tz = "UTC"), "%Y")
    apath <- file.path(archdir, sprintf("%s-%s.jsonl", subdir, yr))
    con <- file(apath, open = "a", encoding = "UTF-8")
    writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, na = "null"), con)
    close(con)
    if (unlink(f) == 0) archived <- archived + 1L else kept <- kept + 1L
  }
  list(archived = archived, kept = kept)
}

# logs\\errors.log is appended to and never rotated. The line saying earlier lines were trimmed is not
# decoration - without it an operator counting a fault counts the survivors and thinks it started today.
trim_log_file <- function(path, max_bytes = 512000, keep_lines = 500L) {
  if (is.null(path) || !nzchar(path %||% "") || !file.exists(path)) return(invisible(FALSE))
  sz <- suppressWarnings(as.numeric(file.info(path)$size))
  if (!is.finite(sz) || sz <= max_bytes) return(invisible(FALSE))
  lines <- safe(safe_readlines(path), character(0))
  if (length(lines) <= keep_lines) return(invisible(FALSE))
  # tryCatch, not safe(): writeLines returns NULL on SUCCESS, so a NULL-means-failed test would report
  # every trim that worked as one that did not.
  invisible(isTRUE(tryCatch({
    writeLines(c("... earlier lines trimmed ...", utils::tail(lines, keep_lines)), path)
    TRUE }, error = function(e) FALSE)))
}

# Every Convert copies the uploaded statement byte-for-byte into uploads/<id>/, and nothing was deleting
# those REAL client statements. This deletes the copied FILE only: record.json stays, stamped `purged`,
# so the audit trail survives the client data. keep_days <= 0 means KEEP INDEFINITELY, and it says so.
purge_uploads <- function(uploads_dir, keep_days = 90, now = as.numeric(Sys.time())) {
  out <- list(purged = 0L, kept = 0L)
  if (is.null(uploads_dir) || !nzchar(uploads_dir %||% "") || !dir.exists(uploads_dir)) return(out)
  recs <- Sys.glob(file.path(uploads_dir, "*", "record.json"))
  if (!length(recs)) return(out)
  kd <- suppressWarnings(as.numeric(keep_days %||% NA)[1])
  if (!is.finite(kd) || kd <= 0) { out$kept <- length(recs); return(out) }
  cutoff <- now - kd * 86400
  for (rp in recs) {
    ud <- dirname(rp)
    files <- setdiff(list.files(ud, full.names = TRUE), rp)
    if (!length(files)) next            # already purged -- neither purged nor kept
    # Age from the STATEMENT COPY's mtime, not the record's: set_upload_status rewrites record.json on
    # every lifecycle change, which would keep resetting the clock.
    when <- suppressWarnings(max(as.numeric(file.info(files)$mtime), na.rm = TRUE))
    if (!is.finite(when) || when >= cutoff) { out$kept <- out$kept + 1L; next }
    if (!all(unlink(files) == 0)) { out$kept <- out$kept + 1L; next }
    ts <- utc_stamp()          # written, so UTC with a Z (R/util.R)
    rec <- safe(jsonlite::fromJSON(rp, simplifyVector = FALSE), NULL)
    if (!is.null(rec)) {
      rec$purged <- TRUE
      rec$purged_ts <- ts
      rec$history <- c(rec$history, list(list(ts = ts, status = "purged")))
      safe(writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE), rp))
    }
    out$purged <- out$purged + 1L
  }
  out
}

# The ONE honest line under the file picker, generated from the SAME setting the purge uses.
uploads_retention_note <- function(keep_days) {
  kd <- suppressWarnings(as.numeric(keep_days %||% NA)[1])
  if (!is.finite(kd) || kd <= 0)
    return("Uploads are kept indefinitely so a statement that failed can be picked up and fixed.")
  sprintf(paste("Uploads are kept %d day%s so a statement that failed can be",
                "picked up and fixed, then deleted."),
          as.integer(kd), if (as.integer(kd) == 1L) "" else "s")
}
# Every kind of scratch folder this app makes, in ONE place. There are four and the sweep knew two:
# `cvb_` is the CASE FOLDER, the biggest, and `ba_` is Admin's bulk audit - `cvb_*` is not caught by
# `cv_*` because the glob needs the underscore, so client statements piled up in %TEMP% for years.
TEMP_DIR_PREFIXES <- c("cv_", "cvb_", "ts_", "ba_")

# Each conversion, case folder, toolkit upload and bulk audit gets its own scratch folder holding the
# outputs AND a copy of the source statement, and R only clears the temp dir when the process EXITS.
# `exclude` protects folders a live session still uses. It also sweeps the SIBLING temp dirs, since
# every R process gets its own Rtmp<random> that only it cleans. Two guards, because this deletes files
# it did not create: only folders under R's own "Rtmp" stem are entered, and the sibling itself stays.
sweep_temp_dirs <- function(dir = tempdir(), prefixes = TEMP_DIR_PREFIXES,
                            keep_hours = 24, now = as.numeric(Sys.time()),
                            exclude = character(0)) {
  if (is.null(dir) || !dir.exists(dir)) return(0L)
  kh <- suppressWarnings(as.numeric(keep_hours %||% NA)[1])
  if (!is.finite(kh) || kh < 0) return(0L)
  cutoff <- now - kh * 3600
  keep <- if (length(exclude)) normalizePath(exclude, mustWork = FALSE) else character(0)
  removed <- 0L
  for (base in .temp_dir_family(dir)) {
    for (p in prefixes) {
      for (d in Sys.glob(file.path(base, paste0(p, "*")))) {
        if (!dir.exists(d)) next
        if (length(keep) && normalizePath(d, mustWork = FALSE) %in% keep) next
        mt <- suppressWarnings(as.numeric(file.info(d)$mtime))
        if (!is.finite(mt) || mt >= cutoff) next
        if (unlink(d, recursive = TRUE) == 0) removed <- removed + 1L
      }
    }
  }
  removed
}

# What R names a session temp dir, so this one word tells an abandoned session's folder from any other.
.R_TEMP_DIR_STEM <- "Rtmp"

# `dir` plus the abandoned R session temp dirs beside it. When `dir` is not one, ONLY it is swept:
# guessing at the neighbours of an unknown folder is not worth the disk.
.temp_dir_family <- function(dir) {
  self <- normalizePath(dir, mustWork = FALSE)
  if (!startsWith(basename(self), .R_TEMP_DIR_STEM)) return(self)
  sibs <- Sys.glob(file.path(dirname(self), paste0(.R_TEMP_DIR_STEM, "*")))
  sibs <- sibs[dir.exists(sibs)]
  unique(c(self, normalizePath(sibs, mustWork = FALSE)))
}

# Live runs plus archived runs, so reports still see history after a rollup.
read_runs_all <- function(logdir = "logs") {
  live <- read_log_records(logdir, "runs")
  arch <- character(0)
  adir <- file.path(logdir, "archive")
  if (dir.exists(adir)) arch <- list.files(adir, pattern = "^runs-.*\\.jsonl$", full.names = TRUE)
  if (!length(arch)) return(live)
  lines <- unlist(lapply(arch, safe_readlines))
  lines <- lines[nzchar(trimws(lines))]
  recs <- Filter(Negate(is.null), lapply(lines, function(l) safe(jsonlite::fromJSON(l), NULL)))
  if (!length(recs)) return(live)
  archdf <- .rows_bind(recs)
  if (!nrow(live)) return(archdf)
  # Union columns.
  allc <- union(names(live), names(archdf))
  for (c in setdiff(allc, names(live))) live[[c]] <- NA
  for (c in setdiff(allc, names(archdf))) archdf[[c]] <- NA
  rbind(live[allc], archdf[allc])
}
