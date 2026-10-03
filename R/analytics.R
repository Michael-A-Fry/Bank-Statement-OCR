# analytics.R -- turn the run + feedback logs into insight. Pure functions over
# the plain JSON logs (logs/runs/*.json, logs/feedback/*.json): no state, no
# database, easy to test, easy to show. This is what powers the Admin panel:
# where conversions succeed, where they DON'T, which learned layouts carry the
# load, and which layouts have started to drift.

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
# failed run by its layout signature so the SAME unreadable format collapses to
# one row: how many, what it looks like, the reader's commonest reason, and when it
# was last seen. Ranked by count = "look at these next".
unsupported_clusters <- function(runs) {
  empty <- data.frame(layout = character(0), count = integer(0), why = character(0),
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
      layout       = .col(d, "layout_hint", "")[1] %||% "",
      count        = length(idx),
      why          = .mode(.col(d, "reason", NA)),
      last_seen    = max(as.character(.col(d, "ts", "")), na.rm = TRUE),
      example_file = .col(d, "source_file", "")[1] %||% "",
      signature    = as.character(.col(d, "layout_signature", "")[1]),
      stringsAsFactors = FALSE)
  })
  res <- do.call(rbind, parts)
  res[order(-res$count, res$last_seen), , drop = FALSE]
}

# .run_layout(runs) -- the learned layout each run was read with, by id (a
# layout's versions are one layout here: a new version is the same design with
# more evidence), or NA. Runs logged before automatic reading carry no layout.
.run_layout <- function(runs) {
  ly <- as.character(.col(runs, "layout", NA))
  ly <- sub("@v?[0-9]+$", "", ly)
  ly[!is.na(ly) & !nzchar(ly)] <- NA_character_
  ly
}

# layout_usage(runs, feedback) -- per learned layout: volume, how many converted
# on their own, how many went to a person, and how often people flagged the
# output as wrong (feedback is filed under the result's template_id, which is the
# layout reference).
layout_usage <- function(runs, feedback = NULL) {
  empty <- data.frame(layout = character(0), n = integer(0), ok = integer(0),
    needs_review = integer(0), low_trust = integer(0), flagged_feedback = integer(0),
    stringsAsFactors = FALSE)
  if (is.null(runs) || !nrow(runs)) return(empty)
  ly <- .run_layout(runs)
  keep <- !is.na(ly)
  if (!any(keep)) return(empty)
  runs <- runs[keep, , drop = FALSE]; ly <- ly[keep]
  st <- as.character(.col(runs, "status", ""))
  tr <- as.character(.col(runs, "trust_level", ""))
  flagged <- list()
  if (!is.null(feedback) && nrow(feedback) && "template_id" %in% names(feedback)) {
    flg <- as.logical(.col(feedback, "flagged", FALSE))
    fl <- feedback[!is.na(flg) & flg, , drop = FALSE]
    if (nrow(fl)) flagged <- as.list(table(sub("@v?[0-9]+$", "", as.character(fl$template_id))))
  }
  parts <- lapply(split(seq_along(ly), ly), function(idx) {
    id <- ly[idx[1]]
    data.frame(layout = id, n = length(idx),
      ok = sum(st[idx] == "ok"), needs_review = sum(st[idx] == "needs_review"),
      # %in%, not ==: a run with no trust level recorded must not turn the count NA.
      low_trust = sum(tr[idx] %in% "low"),
      flagged_feedback = as.integer(flagged[[id]] %||% 0L),
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
# any other-route row with six or more runs was six NAs across. A screen that
# says NOTHING is worse than one that says it is fine: the admin reads it as
# "nothing to see" and it means "never measured".
#
#   statement  PROVEN. The reader's own arithmetic proved the reading (outcome
#              proven or layout_match). A run logged before automatic reading has
#              no outcome and is judged as it was then: ok, no failed check, and
#              a confidence grade that is not `low`.
#   report     EVERY TABLE FOUND BY ITS HEADING, NOTHING SPILLED. the reader
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
  out <- ok & num("kpi_fail_count") == 0L & tr != "low"     # statement, before auto reading
  oc <- as.character(.col(runs, "outcome", NA))
  auto <- !is.na(oc) & kd == "statement"
  out[auto] <- (ok & oc %in% c("proven", "layout_match"))[auto]
  isrep <- kd == "tables"
  out[isrep] <- (ok & num("unclaimed_words") == 0L & num("weak_tables") == 0L)[isrep]
  isform <- kd == "form"
  out[isform] <- (ok & num("n_conflicts") == 0L & num("required_missing") == 0L)[isform]
  out
}

# layout_drift(runs, recent_frac, min_runs) -- catch a learned layout that USED to
# read well and now sends its statements to a person. This is how statement DRIFT
# (a bank subtly changes its print) is surfaced: the change stops the arithmetic
# proving the reading -> the run is logged needs_review -> a layout whose recent
# health drops below its earlier health is flagged here. Deterministic, from the
# logs; no thresholds to tune beyond the obvious ones.
layout_drift <- function(runs, recent_frac = 0.4, min_runs = 6) {
  empty <- data.frame(layout = character(0), runs = integer(0),
    earlier_ok_pct = numeric(0), recent_ok_pct = numeric(0),
    drop = numeric(0), last_seen = character(0), stringsAsFactors = FALSE)
  if (is.null(runs) || !nrow(runs)) return(empty)
  tmpl <- .run_layout(runs)
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
    data.frame(layout = tmpl[o[1]], runs = k,
      earlier_ok_pct = round(e_ok, 0), recent_ok_pct = round(r_ok, 0),
      drop = round(e_ok - r_ok, 0), last_seen = max(ts[o]), stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(empty)
  res <- do.call(rbind, parts)
  res <- res[res$drop >= 25, , drop = FALSE]   # a real, sustained drop
  res[order(-res$drop), , drop = FALSE]
}
