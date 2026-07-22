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

# .amount_buckets(x) -- magnitude DISTRIBUTION of the amounts (structure, not
# values): how many fall in each order-of-magnitude band. Feeds anomaly/shape
# models without ever storing a single amount.
.amount_buckets <- function(x) {
  v <- suppressWarnings(abs(as.numeric(x))); v <- v[!is.na(v) & v > 0]
  if (!length(v)) return(NULL)
  br <- c(0, 1, 10, 100, 1e3, 1e4, 1e5, 1e6, Inf)
  labs <- c("<1", "1-10", "10-100", "100-1k", "1k-10k", "10k-100k", "100k-1M", ">1M")
  as.list(table(cut(v, breaks = br, labels = labs, right = FALSE)))
}

# .len_stats(x) -- min / median / max character length of a text column (a shape
# signal for descriptions), never the text itself.
.len_stats <- function(x) {
  n <- nchar(as.character(x %||% character(0)))
  n <- n[!is.na(n) & n > 0]
  if (!length(n)) return(NULL)
  list(min = as.integer(min(n)), median = as.integer(stats::median(n)),
       max = as.integer(max(n)), mean = round(mean(n), 1))
}

# .source_headers(input, template) -- the source COLUMN NAMES a delimited/excel
# file carried (structure, not content), so a model sees the raw header inventory
# and the drafter's mapping can be scored against it.
.source_headers <- function(input, template) {
  kind <- input$kind %||% ""
  hdr <- if (identical(kind, "excel")) names(input$table %||% list())
    else if (identical(kind, "delimited"))
      safe(.header_fields(input$lines %||% character(0), template), character(0))
    else character(0)
  trimws(as.character(hdr[nzchar(trimws(as.character(hdr)))]))
}

# .mapped_sources(template) -- every source column the template maps (canonical +
# extras), so header - mapped = the columns we did NOT use (the "we missed it" set).
.mapped_sources <- function(template) {
  if (is.null(template)) return(character(0))
  one_src <- function(c) {
    v <- if (is.list(c)) c$source else c
    v <- as.character(v %||% NA_character_)
    if (length(v) != 1) NA_character_ else v
  }
  specs <- c(template$columns %||% list(), template$extras %||% list())
  srcs <- vapply(specs, one_src, character(1))
  srcs <- tolower(trimws(srcs))
  srcs[!is.na(srcs) & nzchar(srcs)]
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

  # ---- parse quality (incl. the "things we missed") ----
  if (.meta_on(config, "parse_quality") && n >= 0) {
    flags <- tx$flags %||% character(0)
    # rows the engine could NOT fully read -- the sharpest training signal.
    unparsed_dates <- if (n > 0) sum(is.na(tx$date) & !is.na(tx$date_raw) &
                                       nzchar(trimws(as.character(tx$date_raw)))) else 0L
    unparsed_amounts <- if (n > 0) sum(is.na(tx$amount) & !grepl("redacted", flags)) else 0L
    rec$parse_quality <- list(
      row_count       = n,
      malformed_rows  = sum(grepl("malformed", flags)),
      redacted_rows   = sum(grepl("redacted", flags)),
      unparsed_dates  = unparsed_dates,     # date cell present but unreadable
      unparsed_amounts = unparsed_amounts,  # amount cell present but unreadable
      amount_sign     = tmpl$amount_sign %||% tmpl$table$amount_sign %||% NA_character_,
      date_format     = tmpl$columns$date$format %||% tmpl$table$date_format %||% NA_character_)
    if (.meta_at_least(level, "full")) {
      rec$parse_quality$flag_histogram    <- .flag_histogram(flags)   # EVERY flag, counted
      rec$parse_quality$source_line_count <- suppressWarnings(as.integer(ctx$parsed$source_line_count %||% NA))
      rec$parse_quality$multiline_extra   <- suppressWarnings(as.integer(ctx$parsed$multiline_extra %||% 0L))
      if (n > 0 && !is.null(tx$direction))
        rec$parse_quality$direction_dist <- as.list(table(factor(tx$direction,
          levels = c("debit", "credit"), exclude = NULL)))
      rec$parse_quality$amount_buckets <- if (n > 0) .amount_buckets(tx$amount) else NULL
      rec$parse_quality$desc_len       <- if (n > 0) .len_stats(tx$description) else NULL
      cov <- safe(field_coverage(ctx$parsed, tmpl), NULL)
      if (!is.null(cov) && nrow(cov))
        rec$parse_quality$field_fill <- stats::setNames(
          as.list(ifelse(cov$n > 0, round(cov$populated / cov$n, 3), NA_real_)), cov$field)
    }
  }

  # ---- multi-statement / periods / accounts (was this really ONE statement?) ----
  if (.meta_on(config, "multi_statement")) {
    m <- ctx$multi %||% list()
    rec$multi_statement <- list(
      likely_multiple  = isTRUE(m$likely_multiple),
      n_periods        = suppressWarnings(as.integer(meta$n_periods %||% NA)),
      n_accounts       = suppressWarnings(as.integer(meta$n_accounts %||% NA)),
      page1_markers    = suppressWarnings(as.integer(meta$page1_markers %||% NA)),
      pages_stated     = suppressWarnings(as.integer(meta$pages_stated %||% NA)),
      combined_accounts = isTRUE(m$combined_accounts))
    if (.meta_at_least(level, "full")) {
      rec$multi_statement$n_opening_labels <- suppressWarnings(as.integer(meta$n_opening_labels %||% NA))
      rec$multi_statement$n_closing_labels <- suppressWarnings(as.integer(meta$n_closing_labels %||% NA))
      rec$multi_statement$boundary_reasons <- as.list(m$reasons %||% character(0))
    }
  }

  # ---- novelty / gaps: what we did NOT recognise (the ML-feedback signal) ----
  # These are the "new or unique" and "we missed it" bits: source columns the
  # template never mapped, and indicator tokens (e.g. a bank writing "cow"/"horse"
  # for debit/credit) that matched NEITHER declared value. Short structural tokens,
  # never statement content -- exactly what a future model would learn new
  # vocabulary from.
  if (.meta_on(config, "novelty") && .meta_at_least(level, "standard")) {
    nov <- list()
    hdrs <- .source_headers(ctx$input, tmpl)
    if (length(hdrs)) {
      mapped <- .mapped_sources(tmpl)
      unmapped <- hdrs[!tolower(hdrs) %in% mapped]
      nov$source_header_count <- length(hdrs)
      if (.meta_at_least(level, "full")) {
        nov$source_headers   <- as.list(hdrs)             # column NAMES only, no values
        nov$unmapped_columns <- as.list(unmapped)         # headers we never used
      } else {
        nov$unmapped_column_count <- length(unmapped)
      }
    }
    # type_dc indicator tokens that match neither the debit nor the credit value.
    style <- tmpl$amount_sign %||% tmpl$table$amount_sign
    if (identical(style, "type_dc") && n > 0 && !is.null(tx$type)) {
      seen <- unique(toupper(trimws(as.character(tx$type))))
      seen <- seen[!is.na(seen) & nzchar(seen)]
      known <- toupper(trimws(c(tmpl$type_debit_value, tmpl$type_credit_value)))
      known <- known[!is.na(known) & nzchar(known)]
      unknown <- setdiff(seen, known)
      if (length(unknown)) nov$unrecognised_type_values <- as.list(unknown)  # e.g. "COW","HORSE"
    }
    if (length(nov)) rec$novelty <- nov
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
