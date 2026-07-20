# analytics.R -- turn the run + feedback logs into insight. Pure functions over
# the plain JSON logs (logs/runs/*.json, logs/feedback/*.json): no state, no
# database, easy to test, easy to show. This is what powers the Admin panel:
# where conversions succeed, where they DON'T, and which template to build next
# to unblock the most statements.

# .col(df, name, default) -- a column if present, else a default-filled vector.
.col <- function(df, name, default) {
  if (name %in% names(df)) df[[name]] else rep(default, nrow(df))
}
# .mode(x) -- most frequent non-NA value (ties -> first seen).
.mode <- function(x) {
  x <- x[!is.na(x) & nzchar(as.character(x))]
  if (!length(x)) return(NA_character_)
  t <- sort(table(x), decreasing = TRUE)
  names(t)[1]
}

# read_runs(logdir) / read_feedback already in feedback.R.
read_runs <- function(logdir = "logs") read_log_records(logdir, "runs")

# runs_overview(runs) -- count + share by status.
runs_overview <- function(runs) {
  empty <- data.frame(status = character(0), n = integer(0), pct = numeric(0))
  if (is.null(runs) || !nrow(runs)) return(empty)
  st <- as.character(.col(runs, "status", "unknown")); st[is.na(st)] <- "unknown"
  t <- as.data.frame(table(status = st), stringsAsFactors = FALSE)
  names(t) <- c("status", "n")
  t$pct <- round(100 * t$n / sum(t$n), 1)
  t[order(-t$n), , drop = FALSE]
}

# unsupported_clusters(runs) -- the headline report. Group every unsupported /
# failed run by its layout signature so the SAME unknown format collapses to one
# row: how many, what it looks like, the closest existing template, why it
# didn't match, and when it was last seen. Ranked by count = "build these next".
unsupported_clusters <- function(runs) {
  empty <- data.frame(layout = character(0), count = integer(0),
    closest_template = character(0), why = character(0),
    last_seen = character(0), example_file = character(0),
    signature = character(0), stringsAsFactors = FALSE)
  if (is.null(runs) || !nrow(runs)) return(empty)
  st <- as.character(.col(runs, "status", ""))
  u <- runs[st %in% c("unsupported", "failed"), , drop = FALSE]
  if (!nrow(u)) return(empty)
  sig <- as.character(.col(u, "layout_signature", "(unknown)"))
  sig[is.na(sig)] <- "(unknown)"
  parts <- lapply(split(seq_len(nrow(u)), sig), function(idx) {
    d <- u[idx, , drop = FALSE]
    data.frame(
      layout           = .col(d, "layout_hint", "")[1] %||% "",
      count            = length(idx),
      closest_template = .mode(.col(d, "closest_template", NA)),
      why              = .mode(.col(d, "detect_detail", NA)),
      last_seen        = max(as.character(.col(d, "ts", "")), na.rm = TRUE),
      example_file     = .col(d, "source_file", "")[1] %||% "",
      signature        = as.character(.col(d, "layout_signature", "")[1]),
      stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, parts)
  res[order(-res$count, res$last_seen), , drop = FALSE]
}

# template_usage(runs, feedback) -- per template that DID match: volume, review
# rate, trust mix, and how often people flagged its output as wrong.
template_usage <- function(runs, feedback = NULL) {
  empty <- data.frame(template = character(0), n = integer(0), ok = integer(0),
    needs_review = integer(0), low_trust = integer(0), flagged_feedback = integer(0),
    stringsAsFactors = FALSE)
  if (is.null(runs) || !nrow(runs)) return(empty)
  tmpl <- as.character(.col(runs, "detected_template", NA))
  keep <- !is.na(tmpl) & nzchar(tmpl)
  if (!any(keep)) return(empty)
  runs <- runs[keep, , drop = FALSE]; tmpl <- tmpl[keep]
  st <- as.character(.col(runs, "status", ""))
  tr <- as.character(.col(runs, "trust_level", ""))
  flagged_by_tmpl <- list()
  if (!is.null(feedback) && nrow(feedback) && "template_id" %in% names(feedback)) {
    flg <- as.logical(.col(feedback, "flagged", FALSE))
    fl <- feedback[!is.na(flg) & flg, , drop = FALSE]
    if (nrow(fl)) flagged_by_tmpl <- as.list(table(as.character(fl$template_id)))
  }
  parts <- lapply(split(seq_along(tmpl), tmpl), function(idx) {
    id <- tmpl[idx[1]]
    data.frame(template = id, n = length(idx),
      ok = sum(st[idx] == "ok"), needs_review = sum(st[idx] == "needs_review"),
      low_trust = sum(tr[idx] == "low"),
      flagged_feedback = as.integer(flagged_by_tmpl[[id]] %||% 0L),
      stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, parts)
  res[order(-res$n), , drop = FALSE]
}
