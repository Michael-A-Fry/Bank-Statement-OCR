# metadata_capture.R -- the LOCAL-ONLY "ML goldmine" capture.
#
# Every conversion can emit a rich, structured metadata record describing HOW it
# went -- the layout it matched, how cleanly it parsed, what the detector saw, how
# it reconciled, and any OCR / redaction signals. This is the raw material for
# future on-box analysis and a possible local ML assist (recommend template edits,
# spot drift, cluster unseen layouts). It is written per-run to
# logs/metadata/<run_id>.json (one file per run -- the same concurrency-safe story
# as the run log) and is KEPT FOREVER.
#
# TWO HARD RULES:
#   1. LOCAL ONLY. This never leaves the box and NEVER enters the governed Qlik
#      feed (feed.R). It lives under logs/, which no feed connection reads.
#   2. NO RAW CONTENT / PII-CONSCIOUS. Descriptions, payees, references and raw
#      amounts are NEVER stored -- only structure, counts, ratios and quality
#      signals. An account number is stored ONLY as a salted-free SHA-256 hash
#      (so the same account links across runs without the number being readable).
#      Balance anchors and the statement period ARE stored: they are financial
#      metadata, not personal identifiers, and never leave the machine.
#
# Detail is controlled by config$metadata: a `level` (off | standard | full) and
# per-category `capture` switches. `standard` keeps the essentials; `full`
# (default) adds the detailed histograms/coverage/anchors. See
# docs/context/metadata-capture.md for the per-level PII documentation.

metadata_levels <- function() c("off", "standard", "full")

# .meta_at_least(level, floor) -- is the configured level at or above `floor`?
.meta_at_least <- function(level, floor) {
  lv <- match(tolower(level %||% "full"), metadata_levels())
  fl <- match(tolower(floor), metadata_levels())
  if (is.na(lv)) lv <- match("full", metadata_levels())
  !is.na(lv) && !is.na(fl) && lv >= fl
}

# .meta_on(config, category) -- is a capture category switched on? Absent -> on.
.meta_on <- function(config, category) {
  cap <- config$metadata$capture
  is.null(cap) || is.null(cap[[category]]) || isTRUE(cap[[category]])
}

# .meta_hash(x) -- PII-safe one-way hash of an identifier (account number), or NA
# when none is present (so it serialises to null, never a readable value).
.meta_hash <- function(x) {
  x <- as.character(x %||% NA); x <- x[!is.na(x) & nzchar(trimws(x))]
  if (!length(x)) return(NA_character_)
  substr(.str_hash(paste(sort(unique(trimws(x))), collapse = "|")), 1, 16)
}

# .flag_histogram(flags) -- count each flag token across the rows (no content).
.flag_histogram <- function(flags) {
  toks <- unlist(strsplit(as.character(flags %||% character(0)), ",", fixed = TRUE))
  toks <- trimws(toks); toks <- toks[nzchar(toks)]
  if (!length(toks)) return(NULL)
  as.list(table(toks))
}

# capture_metadata(ctx, config) -> a named-list metadata record, or NULL when the
# level is "off". `ctx` bundles the conversion's artifacts:
#   run_id, ts, requested_by, sha, input, parsed, recon, det, meta, template,
#   status, elapsed_ms
# It NEVER throws (caller wraps in safe() regardless).
capture_metadata <- function(ctx, config = load_config()) {
  level <- tolower(config$metadata$level %||% "full")
  if (identical(level, "off")) return(NULL)

  tx <- ctx$parsed$transactions
  n  <- if (is.null(tx)) 0L else nrow(tx)
  h  <- ctx$parsed$header %||% list()
  meta <- ctx$meta %||% list()
  tmpl <- ctx$template

  rec <- list(
    schema        = 1L,
    run_id        = ctx$run_id %||% NA_character_,
    ts            = ctx$ts %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    level         = level,
    requested_by  = ctx$requested_by %||% NA_character_,
    source_sha256 = ctx$sha %||% NA_character_,
    source_ext    = tolower(tools::file_ext(ctx$input$path %||% "")),
    status        = ctx$status %||% NA_character_,
    template_id   = if (!is.null(tmpl)) tmpl$id %||% NA_character_ else NA_character_,
    template_origin  = tmpl$origin %||% NA_character_,
    template_version = tmpl$version %||% NA,
    period_start  = h$period_start %||% meta$period_start %||% NA_character_,
    period_end    = h$period_end   %||% meta$period_end   %||% NA_character_,
    account_hash  = .meta_hash(c(h$account_number, meta$accounts))   # PII-safe linkage key
  )

  # ---- layout ----
  if (.meta_on(config, "layout")) {
    sig <- safe(layout_signature(ctx$input), list(signature = NA, hint = ""))
    rec$layout <- list(
      signature = sig$signature %||% NA_character_,
      format    = tmpl$format %||% (ctx$input$kind %||% NA_character_),
      kind      = ctx$input$kind %||% NA_character_,
      n_pages   = suppressWarnings(as.integer(meta$pages_actual %||% h$page_count %||% NA)),
      n_columns = if (identical(ctx$input$kind, "excel")) ncol(ctx$input$table %||% data.frame())
                  else if (identical(ctx$input$kind, "delimited")) length(strsplit(
                    (ctx$input$lines %||% "")[1], "[,\t;|]")[[1]]) else NA_integer_)
    if (.meta_at_least(level, "full")) rec$layout$hint <- sig$hint %||% NA_character_
  }

  # ---- detection ----
  if (.meta_on(config, "detection") && !is.null(ctx$det)) {
    d <- ctx$det
    rec$detection <- list(
      matched   = isTRUE(d$matched),
      score     = d$score %||% NA_real_,
      margin    = if (is.null(d$margin) || is.infinite(d$margin %||% Inf)) NA_real_ else d$margin,
      runner_up = d$runner_up %||% NA_character_)
    if (.meta_at_least(level, "full") && !is.null(d$candidates) && nrow(d$candidates)) {
      cand <- utils::head(d$candidates[order(-d$candidates$score), , drop = FALSE], 5L)
      rec$detection$n_candidates    <- nrow(d$candidates)
      rec$detection$candidate_scores <- stats::setNames(as.list(cand$score), cand$id)
    }
  }

  # ---- parse quality ----
  if (.meta_on(config, "parse_quality") && n >= 0) {
    flags <- tx$flags %||% character(0)
    rec$parse_quality <- list(
      row_count      = n,
      malformed_rows = sum(grepl("malformed", flags)),
      redacted_rows  = sum(grepl("redacted", flags)),
      amount_sign    = tmpl$amount_sign %||% tmpl$table$amount_sign %||% NA_character_,
      date_format    = tmpl$columns$date$format %||% tmpl$table$date_format %||% NA_character_)
    if (.meta_at_least(level, "full")) {
      rec$parse_quality$flag_histogram <- .flag_histogram(flags)
      rec$parse_quality$source_line_count <- suppressWarnings(as.integer(ctx$parsed$source_line_count %||% NA))
      rec$parse_quality$multiline_extra   <- suppressWarnings(as.integer(ctx$parsed$multiline_extra %||% 0L))
      cov <- safe(field_coverage(ctx$parsed, tmpl), NULL)
      if (!is.null(cov) && nrow(cov))
        rec$parse_quality$field_fill <- stats::setNames(
          as.list(ifelse(cov$n > 0, round(cov$populated / cov$n, 3), NA_real_)), cov$field)
    }
  }

  # ---- reconciliation ----
  if (.meta_on(config, "reconciliation") && !is.null(ctx$recon)) {
    r <- ctx$recon
    rec$reconciliation <- list(
      trust_level = r$trust$level %||% NA_character_,
      trust_score = r$trust$score %||% NA_real_)
    if (!is.null(r$kpis) && nrow(r$kpis)) {
      km <- stats::setNames(as.list(r$kpis$status), r$kpis$name)
      if (.meta_at_least(level, "full")) rec$reconciliation$kpis <- km
      else rec$reconciliation$kpi_fail_count <- sum(r$kpis$status == "fail")
    }
    if (.meta_at_least(level, "full")) {
      rec$reconciliation$opening_balance <- suppressWarnings(as.numeric(h$opening_balance %||% NA))
      rec$reconciliation$closing_balance <- suppressWarnings(as.numeric(h$closing_balance %||% NA))
      rec$reconciliation$stated_count    <- suppressWarnings(as.integer(h$stated_count %||% NA))
      rec$reconciliation$net_amount      <- if (n > 0 && !all(is.na(tx$amount)))
        round(sum(tx$amount, na.rm = TRUE), 2) else NA_real_
    }
  }

  # ---- ocr ----
  if (.meta_on(config, "ocr")) {
    op <- suppressWarnings(as.integer(h$ocr_pages %||% meta$ocr_pages %||% 0L))
    if (!is.na(op) && op > 0) rec$ocr <- list(
      pages          = op,
      min_confidence = suppressWarnings(as.numeric(h$ocr_min_confidence %||% NA)),
      low_conf_cells = if (n > 0) sum(grepl("ocr_low_conf", tx$flags %||% "")) else 0L)
  }

  # ---- redaction ----
  if (.meta_on(config, "redaction")) {
    rr <- if (n > 0) sum(grepl("redacted", tx$flags %||% "")) else 0L
    si <- suppressWarnings(as.integer(h$redaction_scan_incomplete %||% 0L))
    if (rr > 0 || (!is.na(si) && si > 0)) rec$redaction <- list(
      redacted_rows   = rr,
      scan_incomplete = if (is.na(si)) 0L else si)
  }

  if (.meta_at_least(level, "full") && !is.null(ctx$elapsed_ms))
    rec$elapsed_ms <- round(as.numeric(ctx$elapsed_ms))

  rec
}

# write_metadata_record(logdir, run_id, record) -- persist one metadata record to
# logs/metadata/<run_id>.json. No-op for a NULL record (level = off). Never throws.
write_metadata_record <- function(logdir, run_id, record) {
  if (is.null(record) || is.null(run_id) || is.na(run_id)) return(invisible(NULL))
  safe(write_log_record(logdir, "metadata", run_id, record))
}
