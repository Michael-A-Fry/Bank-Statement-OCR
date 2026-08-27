# feedback.R -- a human verdict on every conversion, each its own file so any number can arrive at once.

VALID_VERDICTS <- c("correct", "minor_issues", "wrong")

# verdict must be one of VALID_VERDICTS; `flagged` is TRUE for anything not a clean "correct". A verdict
# of "wrong" ALSO retracts that run's rows from the accepted feed - leaving figures a forensic accountant
# has called wrong feeding the dashboards is the cardinal failure. Rows MOVE to review, not destroyed.
submit_feedback <- function(run_id, verdict, comment = NULL, requested_by = NULL,
                            template_id = NULL, logdir = "logs", config = NULL) {
  verdict <- tolower(trimws(as.character(verdict %||% "")))
  if (!verdict %in% VALID_VERDICTS)
    stop(sprintf("verdict must be one of: %s", paste(VALID_VERDICTS, collapse = ", ")))
  # safe(): an unreachable feed folder must never stop the verdict being captured - but the record then
  # says the retraction did not happen, rather than implying it did.
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
    # NA, not 0: "we didn't look" and "we looked and there was nothing" are different facts.
    retracted_files = if (is.null(retraction)) NA_integer_ else as.integer(retraction$files),
    retracted_rows  = if (is.null(retraction)) NA_integer_ else as.integer(retraction$rows)
  )
  # Unique filename per submission, so repeated feedback on the same run never collides.
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
