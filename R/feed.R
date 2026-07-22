# feed.R -- the analytics feed Qlik loads for dashboards.
#
# Accountants convert in the Shiny app; write_feed() is called as a side-effect of
# each conversion and, IF the result clears the governance gate (reconciled +
# proven/curated template), writes a flat, fully-stamped transactions CSV that a
# Qlik folder connection loads. A one-row manifest is written for EVERY conversion
# (accepted or withheld) so coverage is never silent. Concurrency-safe: one file per
# statement (content-hash keyed) / per run, never a shared append.
#
# It NEVER throws (callers still wrap in safe()); a feed problem must not affect the
# user's conversion.

# .feed_key(sha) -- the per-statement key: first 16 of the content hash, so a
# re-convert overwrites (idempotent) and two different files never collide.
.feed_key <- function(sha) if (is.null(sha) || is.na(sha) || !nzchar(sha)) NA_character_ else substr(sha, 1, 16)

# .trust_ok(level, min_trust) -- does the trust level meet the floor?
.trust_ok <- function(level, min_trust) {
  level <- tolower(level %||% ""); min_trust <- tolower(min_trust %||% "high")
  switch(min_trust,
    any    = TRUE,
    medium = level %in% c("high", "medium"),
    level == "high")                       # default: high only
}

# .feed_gate(result, cfg, proven) -- decide accept/withhold + the reason.
.feed_gate <- function(result, cfg, proven) {
  status <- result$status %||% "failed"
  origin <- if (isTRUE(proven)) "default" else "user"
  allowed <- unlist(cfg$feed$allowed_template_origins %||% list("default"))
  allowlist <- unlist(cfg$feed$template_allowlist %||% list())
  tid <- (result$template_id %||% NA_character_)[1]
  if (isTRUE(cfg$feed$require_status_ok) && !identical(status, "ok"))
    return(list(accept = FALSE, reason = paste0("withheld:", status), origin = origin))
  if (!.trust_ok(result$trust$level, cfg$feed$min_trust))
    return(list(accept = FALSE, reason = "withheld:low_trust", origin = origin))
  if (!(origin %in% allowed))
    return(list(accept = FALSE, reason = "withheld:not_proven", origin = origin))
  if (length(allowlist) && !(tid %in% allowlist))
    return(list(accept = FALSE, reason = "withheld:not_in_allowlist", origin = origin))
  list(accept = TRUE, reason = "accepted", origin = origin)
}

# write_feed(result, config, ts) -> the gate result (invisibly). `result` is the
# convert_document() return. `ts` is the ISO timestamp to stamp (pass it in so the
# call stays deterministic in tests; defaults to now).
write_feed <- function(result, config = load_config(), ts = NULL,
                       proven_ids = NULL) {
  if (!isTRUE(config$feed$enabled)) return(invisible(NULL))
  if (identical(result$kind, "form")) return(invisible(NULL))     # statements only
  h <- result$header %||% list()
  sha <- h$source_sha256 %||% NA_character_
  key <- .feed_key(sha)
  if (is.na(key)) return(invisible(NULL))                          # nothing to key on
  if (is.null(ts)) ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  # "Proven" = the template is one of the curated/tested set (paths$templates).
  if (is.null(proven_ids))
    proven_ids <- tryCatch(names(load_templates(config$paths$templates, strict = FALSE)),
                           error = function(e) character(0))
  tid <- (result$template_id %||% NA_character_)[1]
  proven <- !is.na(tid) && tid %in% proven_ids
  gate <- .feed_gate(result, config, proven)

  fdir <- config$feed$feed_dir %||% "feed"
  tx_dir  <- file.path(fdir, "transactions"); runs_dir <- file.path(fdir, "runs")
  rev_dir <- file.path(fdir, "review")
  for (d in c(tx_dir, runs_dir, if (isTRUE(config$feed$include_review_feed)) rev_dir))
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  run_id <- result$run_id %||% NA_character_
  row_count <- suppressWarnings(as.integer(h$row_count %||% NA_integer_))

  # A statement's key maps to exactly ONE feed file. A re-convert can FLIP the
  # gate decision (accepted <-> withheld) or now yield no rows, so remove any
  # prior feed row for this key from BOTH data folders FIRST. Without this a
  # stale "accepted" row keeps feeding Qlik after the statement was withheld
  # (and a stale "review" row lingers after it was accepted) -- the feed would
  # show data the current conversion no longer stands behind. The fresh row (if
  # any) is written below, leaving exactly one file for this statement.
  for (d in c(tx_dir, rev_dir)) {
    old <- file.path(d, paste0(key, ".csv"))
    if (file.exists(old)) safe(unlink(old))
  }

  # --- the flat, stamped transactions (accepted -> dashboard; withheld -> review) --
  csv <- result$outputs[grepl("\\.csv$", result$outputs %||% character(0))]
  tx_written <- NA_character_
  if (length(csv) == 1 && file.exists(csv)) {
    df <- tryCatch(utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE),
                   error = function(e) NULL)
    if (!is.null(df) && nrow(df)) {
      ctx <- data.frame(
        run_id = run_id, converted_ts = ts,
        source_file = h$source_file %||% NA_character_, source_sha256 = sha,
        bank = h$bank %||% NA_character_, statement_type = h$statement_type %||% NA_character_,
        template_id = tid, template_version = as.character(h$template_version %||% NA),
        template_origin = gate$origin, trust_level = result$trust$level %||% NA_character_,
        period_start = h$period_start %||% NA_character_, period_end = h$period_end %||% NA_character_,
        account_number = h$account_number %||% NA_character_,
        stringsAsFactors = FALSE)
      stamped <- cbind(ctx[rep(1L, nrow(df)), , drop = FALSE], df)
      dest_dir <- if (gate$accept) tx_dir else if (isTRUE(config$feed$include_review_feed)) rev_dir else NULL
      if (!is.null(dest_dir)) {
        f <- file.path(dest_dir, paste0(key, ".csv"))
        safe(utils::write.csv(stamped, f, row.names = FALSE, na = ""))
        tx_written <- f
      }
    }
  }

  # --- the manifest: one row per run (accepted AND withheld) -> the Qlik QA table --
  manifest <- data.frame(
    run_id = run_id, converted_ts = ts,
    source_file = h$source_file %||% NA_character_, source_sha256 = sha,
    bank = h$bank %||% NA_character_, template_id = tid,
    template_origin = gate$origin, status = result$status %||% "failed",
    trust_level = result$trust$level %||% NA_character_, row_count = row_count,
    period_start = h$period_start %||% NA_character_, period_end = h$period_end %||% NA_character_,
    gate_result = gate$reason,
    feed_file = if (gate$accept && !is.na(tx_written)) basename(tx_written) else NA_character_,
    stringsAsFactors = FALSE)
  if (!is.na(run_id))
    safe(utils::write.csv(manifest, file.path(runs_dir, paste0(run_id, ".csv")),
                          row.names = FALSE, na = ""))

  invisible(gate)
}
