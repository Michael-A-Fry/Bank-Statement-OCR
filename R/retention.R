# retention.R -- keep logs/runs and logs/feedback tidy over years without a
# database. Old per-event JSON files are ROLLED UP into a single yearly archive
# JSONL and the originals removed. Nothing is lost (the archive keeps every line);
# the live folders just stay small and fast to list. Safe to run repeatedly (a
# nightly scheduled Rscript, or a button in Admin).

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
