# feedback.R -- capture a human verdict on every conversion. Forensic users are
# the ground truth: if a parse is wrong, they say so, and that signal is kept.
# Feedback is appended to <logdir>/feedback.jsonl (one JSON line per submission,
# no raw statement content) and cross-references the run via run_id. Anything
# other than "correct" is flagged so maintenance can triage it.

VALID_VERDICTS <- c("correct", "minor_issues", "wrong")

# submit_feedback(run_id, verdict, ...) -> the appended record (invisibly).
# verdict must be one of VALID_VERDICTS. `flagged` is TRUE for anything that
# isn't a clean "correct", so the maintainer can filter feedback[flagged].
submit_feedback <- function(run_id, verdict, comment = NULL, requested_by = NULL,
                            template_id = NULL, logdir = "logs") {
  verdict <- tolower(trimws(as.character(verdict %||% "")))
  if (!verdict %in% VALID_VERDICTS)
    stop(sprintf("verdict must be one of: %s", paste(VALID_VERDICTS, collapse = ", ")))
  rec <- list(
    ts           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    run_id       = run_id %||% NA_character_,
    verdict      = verdict,
    flagged      = !identical(verdict, "correct"),
    comment      = blank_to_na(comment %||% NA_character_),
    requested_by = requested_by %||% NA_character_,
    template_id  = template_id %||% NA_character_
  )
  log_event(logdir, rec, file = "feedback.jsonl")
  invisible(rec)
}

# read_feedback(logdir) -> data.frame of all feedback (empty frame if none).
# Tolerant of partially-written lines (a concurrent append mid-read is skipped).
read_feedback <- function(logdir = "logs") {
  path <- file.path(logdir, "feedback.jsonl")
  empty <- data.frame(ts = character(0), run_id = character(0), verdict = character(0),
                      flagged = logical(0), comment = character(0),
                      requested_by = character(0), template_id = character(0),
                      stringsAsFactors = FALSE)
  if (!file.exists(path)) return(empty)
  lines <- safe_readlines(path)
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(empty)
  fields <- names(empty)
  # Parse each line to a one-row frame with the fixed column set. JSON `null`
  # comes back as NULL, so every field is coalesced to NA and forced scalar --
  # this is what lets rows with different present/absent fields rbind cleanly.
  rows <- lapply(lines, function(ln) {
    rec <- safe(jsonlite::fromJSON(ln), NULL)
    if (is.null(rec)) return(NULL)
    vals <- lapply(fields, function(f) {
      v <- rec[[f]]
      if (is.null(v) || length(v) == 0) NA else v[[1]]
    })
    names(vals) <- fields
    as.data.frame(vals, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty)
  out <- do.call(rbind, c(rows, list(stringsAsFactors = FALSE)))
  out$flagged <- as.logical(out$flagged)
  out
}
