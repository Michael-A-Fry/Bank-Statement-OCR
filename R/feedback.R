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
#
# A verdict of "wrong" ALSO retracts that run's rows from the accepted analytics
# feed (retract_feed() in R/feed.R). The forensic accountant is the ground truth;
# leaving figures she has explicitly called wrong feeding the dashboards is the
# cardinal failure. The retraction is deterministic, recorded in this very record
# (retracted_files / retracted_rows) and reversible -- rows move to feed/review,
# they are not destroyed. `config` is taken from load_config() unless supplied
# (tests and callers with their own feed folder pass it in).
submit_feedback <- function(run_id, verdict, comment = NULL, requested_by = NULL,
                            template_id = NULL, logdir = "logs", config = NULL) {
  verdict <- tolower(trimws(as.character(verdict %||% "")))
  if (!verdict %in% VALID_VERDICTS)
    stop(sprintf("verdict must be one of: %s", paste(VALID_VERDICTS, collapse = ", ")))
  # safe(): a feed folder that is unreachable must never stop the verdict itself
  # being captured -- but the record then says the retraction did not happen,
  # rather than implying it did.
  retraction <- if (identical(verdict, "wrong"))
    safe(retract_feed(run_id, config = config %||% load_config(), logdir = logdir), NULL)
    else NULL
  rec <- list(
    ts           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    run_id       = run_id %||% NA_character_,
    verdict      = verdict,
    flagged      = !identical(verdict, "correct"),
    comment      = blank_to_na(comment %||% NA_character_),
    requested_by = requested_by %||% current_user(),
    template_id  = template_id %||% NA_character_,
    # NA (not 0) when no retraction was attempted or the feed was unreachable:
    # "we didn't look" and "we looked and there was nothing" are different facts.
    retracted_files = if (is.null(retraction)) NA_integer_ else as.integer(retraction$files),
    retracted_rows  = if (is.null(retraction)) NA_integer_ else as.integer(retraction$rows)
  )
  # unique filename per submission: run_id + a millisecond stamp, so repeated
  # feedback on the same run never collides.
  stamp <- gsub("\\.", "", format(Sys.time(), "%Y%m%d%H%M%OS3"))
  write_log_record(logdir, "feedback", paste0(run_id %||% "na", "__", stamp), rec)
  invisible(rec)
}

# read_feedback(logdir) -> data.frame of all feedback (empty frame if none).
read_feedback <- function(logdir = "logs") {
  empty <- data.frame(ts = character(0), run_id = character(0), verdict = character(0),
                      flagged = logical(0), comment = character(0),
                      requested_by = character(0), template_id = character(0),
                      retracted_files = integer(0), retracted_rows = integer(0),
                      stringsAsFactors = FALSE)
  out <- read_log_records(logdir, "feedback")
  if (!nrow(out)) return(empty)
  if ("flagged" %in% names(out)) out$flagged <- as.logical(out$flagged)
  out
}
