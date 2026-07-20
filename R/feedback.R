# feedback.R -- capture a human verdict on every conversion. Forensic users are
# the ground truth: if a parse is wrong, they say so, and that signal is kept.
# Each submission is written as its OWN file under <logdir>/feedback/ (never a
# shared append), so any number of people can give feedback at the same time
# with no locking and no corruption -- same dead-simple concurrency model as the
# run log. Anything other than "correct" is flagged so maintenance can triage it.

VALID_VERDICTS <- c("correct", "minor_issues", "wrong")

# submit_feedback(run_id, verdict, ...) -> the written record (invisibly).
# verdict must be one of VALID_VERDICTS. `flagged` is TRUE for anything that
# isn't a clean "correct". requested_by defaults to the OS-authenticated user.
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
    requested_by = requested_by %||% current_user(),
    template_id  = template_id %||% NA_character_
  )
  # unique filename per submission: run_id + a millisecond stamp, so repeated
  # feedback on the same run never collides.
  stamp <- gsub("\\.", "", format(Sys.time(), "%Y%m%d%H%M%OS3"))
  write_log_record(logdir, "feedback", paste0(run_id %||% "na", "__", stamp), rec)
  invisible(rec)
}

# read_feedback(logdir) -> data.frame of all feedback (empty frame if none).
read_feedback <- function(logdir = "logs") {
  cols <- c("ts", "run_id", "verdict", "flagged", "comment", "requested_by", "template_id")
  empty <- data.frame(ts = character(0), run_id = character(0), verdict = character(0),
                      flagged = logical(0), comment = character(0),
                      requested_by = character(0), template_id = character(0),
                      stringsAsFactors = FALSE)
  out <- read_log_records(logdir, "feedback")
  if (!nrow(out)) return(empty)
  if ("flagged" %in% names(out)) out$flagged <- as.logical(out$flagged)
  out
}
