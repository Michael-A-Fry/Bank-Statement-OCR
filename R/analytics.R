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

# read_feed_log(logdir) -- the per-conversion FEED outcome records written by
# write_feed(): did the governed feed actually receive this conversion?
read_feed_log <- function(logdir = "logs") read_log_records(logdir, "feed")

# feed_health(feed_log) -- the answer to "is the feed still working?".
#
# WHY this exists: the documented deployment writes the feed to a UNC share. If
# that share goes read-only, every conversion still succeeds, the analyst sees
# nothing wrong, and the dashboards simply stop gaining data -- for ever. The feed
# log records each write outcome; this rolls it up by verdict so a single Admin
# card can show "N accepted, N withheld, N FAILED TO WRITE" and name the last one.
# Rows whose gate_result carries ':write_failed' are the alarm.
feed_health <- function(feed_log) {
  empty <- data.frame(gate_result = character(0), n = integer(0),
                      written = integer(0), last_seen = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(feed_log) || !nrow(feed_log)) return(empty)
  gr <- as.character(.col(feed_log, "gate_result", "unknown")); gr[is.na(gr)] <- "unknown"
  wr <- as.logical(.col(feed_log, "feed_written", FALSE)); wr[is.na(wr)] <- FALSE
  ts <- as.character(.col(feed_log, "ts", "")); ts[is.na(ts)] <- ""
  parts <- lapply(split(seq_along(gr), gr), function(idx)
    data.frame(gate_result = gr[idx[1]], n = length(idx),
               written = sum(wr[idx]), last_seen = max(ts[idx]),
               stringsAsFactors = FALSE))
  res <- do.call(rbind, parts)
  res[order(-res$n, res$gate_result), , drop = FALSE]
}

# feed_write_failures(feed_log) -- just the conversions the gate ACCEPTED and the
# feed did not receive. Empty is the healthy answer; anything here needs a human.
feed_write_failures <- function(feed_log) {
  empty <- data.frame(ts = character(0), run_id = character(0),
                      source_file = character(0), gate_result = character(0),
                      feed_dir = character(0), stringsAsFactors = FALSE)
  if (is.null(feed_log) || !nrow(feed_log)) return(empty)
  gr <- as.character(.col(feed_log, "gate_result", "")); gr[is.na(gr)] <- ""
  hit <- grepl(":write_failed$", gr)
  if (!any(hit)) return(empty)
  data.frame(ts = as.character(.col(feed_log, "ts", ""))[hit],
             run_id = as.character(.col(feed_log, "run_id", NA))[hit],
             source_file = as.character(.col(feed_log, "source_file", NA))[hit],
             gate_result = gr[hit],
             feed_dir = as.character(.col(feed_log, "feed_dir", NA))[hit],
             stringsAsFactors = FALSE)
}

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
      # %in%, not ==: a form or a report records NO trust level, because neither
      # has any reconciliation to be confident about -- and `NA == "low"` is NA,
      # so one report run turned this whole count into NA and the row Admin
      # printed for that template said nothing at all. %in% counts the runs that
      # were actually graded low, which is what the column claims to be.
      low_trust = sum(tr[idx] %in% "low"),
      flagged_feedback = as.integer(flagged_by_tmpl[[id]] %||% 0L),
      stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, parts)
  res[order(-res$n), , drop = FALSE]
}

# run_healthy(runs) -- was each run a GOOD one? TRUE / FALSE, one per row, never
# NA.
#
# HEALTH IS DEFINED PER KIND, because the three routes prove different things and
# only one of them has any arithmetic behind it. This used to be a single
# statement-shaped test -- `status ok AND no failed check AND trust is not low` --
# and a report carries no trust level at all, so `trust != "low"` was NA, the
# whole vector was NA, both percentages were NA, and the row Admin rendered for
# any other-route template with six or more runs was six NAs across. A screen
# that says NOTHING about a template is worse than one that says it is fine: the
# admin reads it as "nothing to see" and it means "never measured".
#
#   statement  RECONCILIATION. The balances proved the read, no check failed, and
#              the confidence grade is not `low`. Unchanged, deliberately: this
#              is the only route with arithmetic behind it and its bar must not
#              move because two other routes arrived. (A statement with NO trust
#              level recorded is now judged on its status and its checks instead
#              of turning the whole template's figures to NA. It cannot arise in
#              practice -- a statement with no trust is one that did not convert,
#              so its status is not `ok` -- but a screen must not be able to go
#              blank on an edge case nobody has met.)
#   report     EVERY TABLE FOUND BY ITS HEADING, NOTHING SPILLED. convert_tables
#              already refuses `ok` to a report with a table found by position, a
#              table that came out empty or thin, or a typed column that parsed
#              nothing -- so `ok` carries all of that. What it does NOT carry is
#              unclaimed words: it tolerates one, and a word printed inside a
#              table that no column claimed is the sharpest sign the bands no
#              longer fit. Here that is not health.
#   form       NOTHING DISPUTED, NOTHING REQUIRED MISSING. A form has no
#              reconciliation either; a label printed twice with two different
#              values is the whole of its quality signal.
#
# Every field is read through .col() with a default, so a record written by an
# older engine -- or by a route this function has never heard of -- degrades to
# the status test rather than to NA.
run_healthy <- function(runs) {
  if (is.null(runs) || !is.data.frame(runs) || !nrow(runs)) return(logical(0))
  kd <- as.character(.col(runs, "kind", "statement"))
  kd[is.na(kd) | !nzchar(kd)] <- "statement"
  st <- as.character(.col(runs, "status", "")); st[is.na(st)] <- ""
  ok <- st == "ok"
  num <- function(name) {
    v <- suppressWarnings(as.integer(.col(runs, name, 0L))); v[is.na(v)] <- 0L; v
  }
  tr <- as.character(.col(runs, "trust_level", "")); tr[is.na(tr)] <- ""
  out <- ok & num("kpi_fail_count") == 0L & tr != "low"     # statement
  isrep <- kd == "tables"
  out[isrep] <- (ok & num("unclaimed_words") == 0L & num("weak_tables") == 0L)[isrep]
  isform <- kd == "form"
  out[isform] <- (ok & num("n_conflicts") == 0L & num("required_missing") == 0L)[isform]
  out
}

# template_drift(runs, recent_frac, min_runs) -- catch a template that USED to
# work and is now producing review/low-trust/failed reconciliations. This is how
# statement DRIFT (a bank subtly changes a field) is surfaced: the field change
# breaks the balance check -> the run is logged needs_review -> a template whose
# recent health drops below its earlier health is flagged here. Deterministic,
# from the logs; no thresholds to tune beyond the obvious ones.
#
# A REPORT TEMPLATE DRIFTS TOO, and the same way: the issuer renames a heading,
# the table stops being found by it, the reader falls back to where it sat on the
# example -- and every figure under that heading is read out of whatever ink now
# sits there. run_healthy() above is what makes this table say so.
template_drift <- function(runs, recent_frac = 0.4, min_runs = 6) {
  empty <- data.frame(template = character(0), runs = integer(0),
    earlier_ok_pct = numeric(0), recent_ok_pct = numeric(0),
    drop = numeric(0), last_seen = character(0), stringsAsFactors = FALSE)
  if (is.null(runs) || !nrow(runs)) return(empty)
  tmpl <- as.character(.col(runs, "detected_template", NA))
  keep <- !is.na(tmpl) & nzchar(tmpl)
  if (!any(keep)) return(empty)
  runs <- runs[keep, , drop = FALSE]; tmpl <- tmpl[keep]
  ts <- as.character(.col(runs, "ts", ""))
  healthy <- run_healthy(runs)
  parts <- lapply(split(seq_along(tmpl), tmpl), function(idx) {
    o <- idx[order(ts[idx])]; k <- length(o)
    if (k < min_runs) return(NULL)
    nrec <- max(1L, round(k * recent_frac))
    recent <- utils::tail(o, nrec); earlier <- utils::head(o, k - nrec)
    if (!length(earlier)) return(NULL)
    e_ok <- mean(healthy[earlier]) * 100; r_ok <- mean(healthy[recent]) * 100
    data.frame(template = tmpl[o[1]], runs = k,
      earlier_ok_pct = round(e_ok, 0), recent_ok_pct = round(r_ok, 0),
      drop = round(e_ok - r_ok, 0), last_seen = max(ts[o]), stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(empty)
  res <- do.call(rbind, parts)
  res <- res[res$drop >= 25, , drop = FALSE]   # a real, sustained drop
  res[order(-res$drop), , drop = FALSE]
}
