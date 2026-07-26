# retention.R -- everything this tool leaves on disk, and when it goes away.
# Three jobs, all safe to run repeatedly (a nightly scheduled Rscript, a button in
# Admin, or at startup):
#   rollup_logs()     logs/runs + logs/feedback   -> yearly archive JSONL, nothing lost
#   purge_uploads()   uploads/<id>/<statement>    -> deleted, the record.json kept
#   sweep_temp_dirs() per-session scratch folders -> deleted once stale
# Old per-event JSON files are ROLLED UP into a single yearly archive JSONL and
# the originals removed. Nothing is lost (the archive keeps every line); the live
# folders just stay small and fast to list.

# rollup_logs(logdir, subdir, keep_days, now) -> list(archived, kept).
# Files older than keep_days move to <logdir>/archive/<subdir>-<year>.jsonl.
# `now` (epoch seconds) is injectable so the behaviour is testable/deterministic.
rollup_logs <- function(logdir, subdir = "runs", keep_days = 90,
                        now = as.numeric(Sys.time())) {
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
    if (!nzchar(yr)) yr <- format(as.POSIXct(mt, origin = "1970-01-01"), "%Y")
    apath <- file.path(archdir, sprintf("%s-%s.jsonl", subdir, yr))
    con <- file(apath, open = "a", encoding = "UTF-8")
    writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, na = "null"), con)
    close(con)
    if (unlink(f) == 0) archived <- archived + 1L else kept <- kept + 1L
  }
  list(archived = archived, kept = kept)
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
    ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
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
    return(paste("A copy of the file you upload is kept on this server indefinitely,",
                 "so a statement the tool couldn't read can be picked up and fixed.",
                 "Ask whoever looks after the tool if copies should be deleted automatically."))
  sprintf(paste("A copy of the file you upload is kept on this server for %d day%s,",
                "so a statement the tool couldn't read can be picked up and fixed.",
                "After that the copy is deleted automatically."),
          as.integer(kd), if (as.integer(kd) == 1L) "" else "s")
}

# sweep_temp_dirs(dir, prefixes, keep_hours, now, exclude) -> number removed.
#
# Each conversion and each toolkit upload gets its own scratch folder under the
# process temp dir (cv_*/ts_*), holding the outputs AND a copy of the source
# statement. R only clears the temp dir when the process EXITS -- and this app is
# a service that runs for months -- so on a shared server those copies piled up
# for the life of the process. Swept at startup and re-run cheaply; `now` is
# injectable so the behaviour is testable, and `exclude` protects folders still in
# use by a live session.
sweep_temp_dirs <- function(dir = tempdir(), prefixes = c("cv_", "ts_"),
                            keep_hours = 24, now = as.numeric(Sys.time()),
                            exclude = character(0)) {
  if (is.null(dir) || !dir.exists(dir)) return(0L)
  kh <- suppressWarnings(as.numeric(keep_hours %||% NA)[1])
  if (!is.finite(kh) || kh < 0) return(0L)
  cutoff <- now - kh * 3600
  removed <- 0L
  for (p in prefixes) {
    for (d in Sys.glob(file.path(dir, paste0(p, "*")))) {
      if (!dir.exists(d)) next
      if (length(exclude) && normalizePath(d, mustWork = FALSE) %in%
          normalizePath(exclude, mustWork = FALSE)) next
      mt <- suppressWarnings(as.numeric(file.info(d)$mtime))
      if (!is.finite(mt) || mt >= cutoff) next
      if (unlink(d, recursive = TRUE) == 0) removed <- removed + 1L
    }
  }
  removed
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
