# retention.R -- everything this tool leaves on disk, and when it goes away.
# Four jobs, all safe to run repeatedly (a nightly scheduled Rscript, a button in
# Admin, or at startup):
#   rollup_logs()     logs/runs + logs/feedback + logs/feed
#                                                 -> yearly archive JSONL, nothing lost
#                                                    (and logs/errors.log trimmed)
#   purge_uploads()   uploads/<id>/<statement>    -> deleted, the record.json kept
#   sweep_temp_dirs() every scratch folder, this process's and abandoned ones
#                                                 -> deleted once stale
#   trim_log_file()   logs/errors.log             -> the recent tail kept, and said
# Old per-event JSON files are ROLLED UP into a single yearly archive JSONL and
# the originals removed. Nothing is lost (the archive keeps every line); the live
# folders just stay small and fast to list.

# LOG_ROLLUP_SUBDIRS -- every folder under logs\ that gains one file per event.
#
# There are three and the rollup knew about two. logs\feed\ gets a record for
# EVERY conversion (accepted or withheld -- R/feed.R writes one so a feed outage
# can never be silent), so it grows at exactly the rate logs\runs\ does, and
# nothing ever archived it: at 50 conversions a day that is 18,000 loose JSON
# files after a year, in the folder Admin/Insights reads one file at a time.
LOG_ROLLUP_SUBDIRS <- c("runs", "feedback", "feed")

# rollup_logs(logdir, subdir, keep_days, now) -> list(archived, kept).
# Files older than keep_days move to <logdir>/archive/<subdir>-<year>.jsonl.
# `now` (epoch seconds) is injectable so the behaviour is testable/deterministic.
#
# `subdir` takes a VECTOR, and defaults to every folder that grows, so tidying the
# logs is one call that cannot be given a short list by accident. Naming one
# folder still does only that folder, so every existing caller is unchanged.
rollup_logs <- function(logdir, subdir = LOG_ROLLUP_SUBDIRS, keep_days = 90,
                        now = as.numeric(Sys.time())) {
  subdir <- as.character(subdir)
  if (length(subdir) != 1L) {
    out <- list(archived = 0L, kept = 0L)
    for (s in subdir) {
      r <- rollup_logs(logdir, s, keep_days = keep_days, now = now)
      out$archived <- out$archived + r$archived; out$kept <- out$kept + r$kept
    }
    # The error log is not one-file-per-event, so it is trimmed rather than
    # archived -- but it is in the same folder, growing for the same reason, and
    # this is the one call that tidies that folder. See trim_log_file().
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
    # UTC, because the stamp inside the record is UTC (R/util.R) and the archive
    # a record lands in must not depend on which side of midnight the server's
    # own clock happened to be on.
    if (!nzchar(yr)) yr <- format(as.POSIXct(mt, origin = "1970-01-01", tz = "UTC"), "%Y")
    apath <- file.path(archdir, sprintf("%s-%s.jsonl", subdir, yr))
    con <- file(apath, open = "a", encoding = "UTF-8")
    writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, na = "null"), con)
    close(con)
    if (unlink(f) == 0) archived <- archived + 1L else kept <- kept + 1L
  }
  list(archived = archived, kept = kept)
}

# trim_log_file(path, max_bytes, keep_lines) -> TRUE when it trimmed.
#
# logs\errors.log is APPENDED to and never rotated: every job that could not be
# started and every conversion that fell over writes a line, so a server with a
# recurring fault (a share that keeps dropping, a template that keeps throwing)
# writes to it all day for years. scripts\run_app.R has trimmed logs\startup.log
# this way since it was written -- keep the tail, say that the earlier lines are
# gone -- and the error log, which grows faster, had nothing.
#
# The tail is what matters: a maintainer reading this file is reading the most
# recent failures. The line saying earlier lines were trimmed is not decoration --
# without it an operator counting occurrences of a fault would count the survivors
# and believe it started this morning.
trim_log_file <- function(path, max_bytes = 512000, keep_lines = 500L) {
  if (is.null(path) || !nzchar(path %||% "") || !file.exists(path)) return(invisible(FALSE))
  sz <- suppressWarnings(as.numeric(file.info(path)$size))
  if (!is.finite(sz) || sz <= max_bytes) return(invisible(FALSE))
  lines <- safe(safe_readlines(path), character(0))
  if (length(lines) <= keep_lines) return(invisible(FALSE))
  # tryCatch, not safe(): writeLines returns NULL on SUCCESS, so a NULL-means-it-
  # failed test would report every trim that worked as one that did not.
  invisible(isTRUE(tryCatch({
    writeLines(c("... earlier lines trimmed ...", utils::tail(lines, keep_lines)), path)
    TRUE }, error = function(e) FALSE)))
}

# purge_uploads(uploads_dir, keep_days, now) -> list(purged, kept).
#
# Every real Convert copies the uploaded statement byte-for-byte into
# uploads/<id>/ so a format that failed can be picked up and fixed. Those copies
# are REAL client statements and nothing was ever deleting them, so the folder
# grew forever with no setting, no button and nothing said to the person who
# uploaded. This is the tidy-up, deliberately mirroring rollup_logs (same
# injectable `now`, same "safe to run repeatedly" contract).
#
# WHAT IT DELETES: the copied statement FILE only. record.json STAYS, stamped
# with a `purged` history entry, so the audit trail ("this was uploaded, this is
# what happened to it, its copy was deleted on this date") survives the client
# data. Nothing about the conversion itself is lost -- the run log is separate.
#
# keep_days <= 0 (or not a number) means KEEP INDEFINITELY: nothing is deleted.
# That is a real choice some sites make, so it is honoured -- and the Convert page
# says so out loud rather than implying a retention period that does not exist.
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
    # Age from the STATEMENT COPY's own mtime (the moment it was written), not the
    # record's: set_upload_status rewrites record.json on every lifecycle change,
    # which would keep resetting the clock on a file nobody has touched.
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

# uploads_retention_note(keep_days) -- the ONE honest line shown under the file
# picker. The person handing the tool a client's bank statement is entitled to
# know a copy is kept, for how long and why, before they upload it -- so this is
# generated from the SAME setting the purge uses and can never drift from it.
uploads_retention_note <- function(keep_days) {
  kd <- suppressWarnings(as.numeric(keep_days %||% NA)[1])
  if (!is.finite(kd) || kd <= 0)
    return("Uploads are kept indefinitely so a statement that failed can be picked up and fixed.")
  sprintf(paste("Uploads are kept %d day%s so a statement that failed can be",
                "picked up and fixed, then deleted."),
          as.integer(kd), if (as.integer(kd) == 1L) "" else "s")
}

# TEMP_DIR_PREFIXES -- every kind of scratch folder this app makes, in ONE place.
#
# There are four, and the sweep below knew about two. `cvb_` is the CASE FOLDER
# (up to 50 statements at once -- by far the biggest), and `ba_` is Admin's bulk
# audit; neither matched, and `cvb_*` is not caught by `cv_*` because the glob
# needs the underscore. Proven by seeding all four aged 72 hours: two were removed
# and two survived, so real client bank statements accumulated in %TEMP% for the
# life of the server, which on this deployment is months.
#
# It is a constant, and exported, because the bug was two lists of the same thing:
# tempfile("cvb_") in app.R and c("cv_", "ts_") here. app.R's four tempfile()
# calls should name their prefix from this vector so a fifth kind of scratch
# folder cannot be invented without the sweep learning about it.
TEMP_DIR_PREFIXES <- c("cv_", "cvb_", "ts_", "ba_")

# sweep_temp_dirs(dir, prefixes, keep_hours, now, exclude) -> number removed.
#
# Each conversion, case folder, toolkit upload and bulk audit gets its own scratch
# folder under the process temp dir, holding the outputs AND a copy of the source
# statement. R only clears the temp dir when the process EXITS -- and this app is
# a service that runs for months -- so on a shared server those copies piled up
# for the life of the process. Swept at startup and re-run cheaply; `now` is
# injectable so the behaviour is testable, and `exclude` protects folders still in
# use by a live session.
#
# AND IT SWEEPS THE SIBLING TEMP DIRS TOO, which is the other half of the same
# fault. Every R process gets its OWN temp dir (Rtmp<random>) inside the system
# temp folder, and only that process ever cleans it. So every conversion child
# process (R/jobs.R), every crash and every restart of the service left a whole
# Rtmp folder full of client statements that NOTHING would ever reclaim -- the old
# sweep could only see the folders of the process it was running in. It now walks
# its own parent for the R temp dirs beside it and sweeps the scratch folders in
# those as well.
#
# TWO GUARDS, because this deletes files it did not create:
#   * only folders whose name starts with the SAME prefix R gives its temp dirs
#     ("Rtmp") are entered, so no other program's folder is ever looked in;
#   * the sibling folder itself is never removed -- only the app's own scratch
#     folders inside it. A long-running process's temp dir is old by definition,
#     and deleting a live one would take the app's own working files with it.
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

# .R_TEMP_DIR_STEM -- what R names a session temp dir. Every R process on every
# platform makes "Rtmp" + a few random characters inside the system temp folder,
# so this one word is what tells an abandoned R session's folder apart from every
# other folder in %TEMP%.
.R_TEMP_DIR_STEM <- "Rtmp"

# .temp_dir_family(dir) -- `dir` plus the abandoned R session temp dirs sitting
# beside it. When `dir` is not an R session temp dir at all (a site pointing
# TMPDIR straight at a folder of its own, or a caller passing a folder to sweep),
# ONLY that folder is swept: guessing at the neighbours of an unknown folder is
# not a risk worth taking to reclaim disk.
.temp_dir_family <- function(dir) {
  self <- normalizePath(dir, mustWork = FALSE)
  if (!startsWith(basename(self), .R_TEMP_DIR_STEM)) return(self)
  sibs <- Sys.glob(file.path(dirname(self), paste0(.R_TEMP_DIR_STEM, "*")))
  sibs <- sibs[dir.exists(sibs)]
  unique(c(self, normalizePath(sibs, mustWork = FALSE)))
}

# read_runs_all(logdir) -> live runs + archived runs, so reports still see
# history after a rollup. (read_runs alone reads only the live folder.)
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
  # union columns
  allc <- union(names(live), names(archdf))
  for (c in setdiff(allc, names(live))) live[[c]] <- NA
  for (c in setdiff(allc, names(archdf))) archdf[[c]] <- NA
  rbind(live[allc], archdf[allc])
}
