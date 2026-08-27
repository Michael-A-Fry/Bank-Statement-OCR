# metadata_capture.R -- the LOCAL-ONLY capture describing HOW a conversion went, written per-run to
# logs/metadata/<run_id>.json and KEPT FOREVER. Two hard rules: (1) LOCAL ONLY - it never leaves the
# box and NEVER enters the governed Qlik feed; (2) NO RAW CONTENT - only structure, counts, ratios and
# quality signals, with an account number stored as a hash. Balance anchors and the period ARE stored:
# financial metadata, not personal identifiers. Per-level PII detail: docs/context/metadata-capture.md.

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

# A PII-safe one-way hash of an identifier, or NA when none is present, so it serialises to null.
.meta_hash <- function(x) {
  x <- as.character(x %||% NA); x <- x[!is.na(x) & nzchar(trimws(x))]
  if (!length(x)) return(NA_character_)
  substr(.str_hash(paste(sort(unique(trimws(x))), collapse = "|")), 1, 16)
}

# The layout hint with anything that is not a structural word removed. layout_signature() falls back to
# the commonest long words on the page, which on one page were two surnames - written into a folder
# kept FOREVER whose rule 2 says names are never stored. A hint exists to CLUSTER layouts and a surname
# clusters nothing. What actually clusters is the SIGNATURE, which is not touched here.
.layout_hint_safe <- function(hint, kind = "pdf") {
  h <- as.character(hint %||% "")[1]
  if (is.na(h) || !nzchar(h)) return("")
  if (!identical(as.character(kind %||% "")[1], "pdf")) return(h)
  toks <- trimws(unlist(strsplit(h, "|", fixed = TRUE)))
  keys <- tolower(as.character(safe(lex("header_keywords"), character(0))))
  keep <- toks[nzchar(toks) & tolower(toks) %in% keys]
  paste(keep, collapse = " | ")
}

# .flag_histogram(flags) -- count each flag token across the rows (no content).
.flag_histogram <- function(flags) {
  toks <- unlist(strsplit(as.character(flags %||% character(0)), ",", fixed = TRUE))
  toks <- trimws(toks); toks <- toks[nzchar(toks)]
  if (!length(toks)) return(NULL)
  as.list(table(toks))
}

# Magnitude DISTRIBUTION of the amounts. Feeds anomaly models without storing a single amount.
.amount_buckets <- function(x) {
  v <- suppressWarnings(abs(as.numeric(x))); v <- v[!is.na(v) & v > 0]
  if (!length(v)) return(NULL)
  br <- c(0, 1, 10, 100, 1e3, 1e4, 1e5, 1e6, Inf)
  labs <- c("<1", "1-10", "10-100", "100-1k", "1k-10k", "10k-100k", "100k-1M", ">1M")
  as.list(table(cut(v, breaks = br, labels = labs, right = FALSE)))
}

# min / median / max character length of a text column, never the text. Sanitised to valid UTF-8
# first so nchar() cannot throw on a hostile multibyte value in a non-UTF-8 locale.
.len_stats <- function(x) {
  n <- nchar(.enc_safe(x %||% character(0)))
  n <- n[!is.na(n) & n > 0]
  if (!length(n)) return(NULL)
  list(min = as.integer(min(n)), median = as.integer(stats::median(n)),
       max = as.integer(max(n)), mean = round(mean(n), 1))
}

# The source COLUMN NAMES a delimited or excel file carried, so the drafter's mapping can be scored.
.source_headers <- function(input, template) {
  kind <- input$kind %||% ""
  hdr <- if (identical(kind, "excel")) names(input$table %||% list())
    else if (identical(kind, "delimited"))
      safe(.header_fields(input$lines %||% character(0), template), character(0))
    else character(0)
  trimws(as.character(hdr[nzchar(trimws(as.character(hdr)))]))
}

# Every source column the template maps, so header minus mapped is the set we did NOT use.
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

# Returns a named-list metadata record, or NULL when the level is "off". Never throws.
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
    # WHICH ROUTE THIS RECORD DESCRIBES. Every record used to describe the STATEMENT attempt and say
    # nothing about being one, so a report that converted perfectly was filed as unsupported with a
    # null template_id - in the folder R/suggestions.R mines for "build these next".
    kind          = as.character(ctx$kind %||% "statement")[1],
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
    # Reuse the signature the caller already computed when supplied.
    sig <- ctx$layout_sig %||% safe(layout_signature(ctx$input), list(signature = NA, hint = ""))
    rec$layout <- list(
      signature = sig$signature %||% NA_character_,
      format    = tmpl$format %||% (ctx$input$kind %||% NA_character_),
      kind      = ctx$input$kind %||% NA_character_,
      n_pages   = suppressWarnings(as.integer(meta$pages_actual %||% h$page_count %||% NA)),
      n_columns = if (identical(ctx$input$kind, "excel")) ncol(ctx$input$table %||% data.frame())
                  else if (identical(ctx$input$kind, "delimited")) length(strsplit(
                    (ctx$input$lines %||% "")[1], "[,\t;|]")[[1]]) else NA_integer_)
    # The hint, and only the part of it this file is allowed to keep forever.
    if (.meta_at_least(level, "full"))
      rec$layout$hint <- .layout_hint_safe(sig$hint, ctx$input$kind)
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
    # Rows the engine could NOT fully read - the sharpest training signal.
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
      # Joined: a template may declare several candidate formats in one scalar field.
      date_format     = paste(tmpl$columns$date$format %||% tmpl$table$date_format %||% NA_character_,
                              collapse = " | "))
    if (.meta_at_least(level, "full")) {
      rec$parse_quality$flag_histogram    <- .flag_histogram(flags)   # EVERY flag, counted
      rec$parse_quality$source_line_count <- suppressWarnings(as.integer(ctx$parsed$source_line_count %||% NA))
      rec$parse_quality$multiline_extra   <- suppressWarnings(as.integer(ctx$parsed$multiline_extra %||% 0L))
      if (n > 0 && !is.null(tx$direction))
        rec$parse_quality$direction_dist <- as.list(table(factor(tx$direction,
          levels = c("debit", "credit"), exclude = NULL)))
      rec$parse_quality$amount_buckets <- if (n > 0) .amount_buckets(tx$amount) else NULL
      rec$parse_quality$desc_len       <- if (n > 0) .len_stats(tx$description) else NULL
      cov <- ctx$coverage %||% safe(field_coverage(ctx$parsed, tmpl), NULL)   # reuse if supplied
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

  # Novelty: what we did NOT recognise. Source columns the template never mapped, and indicator
  # tokens that matched neither declared value. Short structural tokens, never statement content.
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

  # The richest single signal for a statement the engine could not match: PII-safe per-column
  # profiles plus the engine's best-guess mapping, so a template can be built without seeing content.
  if (.meta_on(config, "template_hints") && .meta_at_least(level, "full")) {
    th <- safe(template_hints(ctx$input, tmpl, matched = isTRUE(ctx$det$matched)), NULL)
    if (!is.null(th) && length(th)) rec$template_hints <- th
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

# Persist one record to logs/metadata/<run_id>.json. No-op for a NULL record. Never throws.
write_metadata_record <- function(logdir, run_id, record) {
  if (is.null(record) || is.null(run_id) || is.na(run_id)) return(invisible(NULL))
  safe(write_log_record(logdir, "metadata", run_id, record))
}

# When a form or a report wins, the record has to say so. convert_document() tries the statement
# pipeline first and the record is written by that pass, so a report that converted perfectly left a
# record describing the abandoned attempt - kept forever and mined by R/suggestions.R, so the corpus
# would recommend building what already exists. The statement attempt is moved, not deleted.

# The fields a form or report result corrects on the statement attempt's record, plus that route's
# own structure. Numbers and counts only: no label text, no table names, no values.
route_metadata <- function(res) {
  k <- as.character(res$kind %||% "statement")[1]
  if (!k %in% c("form", "tables")) return(list())
  int <- function(x) { v <- suppressWarnings(as.integer(x %||% NA)[1]); v }
  n <- function(x) length(as.character(x %||% character(0)))
  out <- list(
    kind             = k,
    status           = as.character(res$status %||% NA_character_)[1],
    template_id      = as.character(res$template_id %||% NA_character_)[1],
    template_origin  = if (identical(k, "form")) "fields" else "document",
    template_version = res$template_version %||% NA,
    template_sha256  = as.character(res$template_sha256 %||% NA_character_)[1],
    forced_template  = isTRUE(res$forced_template),
    ambiguous        = isTRUE(res$ambiguous))
  if (identical(k, "form"))
    out$form <- list(n_fields = int(res$n_fields), n_values = int(res$n_values),
                     n_conflicts = int(res$n_conflicts),
                     required_missing = int(res$required_missing))
  else
    out$report <- list(n_tables = int(res$n_tables), n_table_rows = int(res$n_rows),
                       unclaimed_words = int(res$unclaimed_words),
                       empty_tables = n(res$empty_tables),
                       thin_tables = n(res$thin_tables),
                       weak_tables = n(res$weak_tables),
                       absent_tables = n(res$absent_tables),
                       parse_fail_tables = n(res$parse_fail_tables))
  out
}

# The file the statement pass just wrote for THIS run. write_log_record() never overwrites, so a
# clashing run id is filed as "<id>~2.json" and this run's record is the most recently written of
# that set - getting it wrong would correct one run's record with another run's answer.
.metadata_record_path <- function(logdir, run_id) {
  id <- as.character(run_id %||% NA_character_)[1]
  if (is.na(id) || !nzchar(id)) return(NA_character_)
  dir <- file.path(logdir, "metadata")
  if (!dir.exists(dir)) return(NA_character_)
  base <- safe(.safe_name(id), id)
  # .safe_name() has already reduced the id to [A-Za-z0-9_.-], so the only character left that a
  # regular expression would read as anything but itself is the dot.
  rx <- paste0("^", gsub(".", "\\.", base, fixed = TRUE), "(~[0-9]+)?\\.json$")
  hits <- list.files(dir, pattern = rx, full.names = TRUE)
  if (!length(hits)) return(NA_character_)
  hits[order(file.mtime(hits), hits, decreasing = TRUE)][1]
}

# Correct this run's metadata record once the route is known. In place, on the file this run wrote:
# a second file would leave the forever corpus holding two records for one conversion.
amend_metadata_record <- function(logdir, res) {
  delta <- safe(route_metadata(res), list())
  if (!length(delta)) return(invisible(NULL))
  path <- safe(.metadata_record_path(logdir, res$run_id %||% res$run_log$run_id), NA_character_)
  if (is.na(path) || !nzchar(path)) return(invisible(NULL))
  old <- safe(jsonlite::fromJSON(paste(safe_readlines(path), collapse = "\n"),
                                 simplifyVector = FALSE), NULL)
  if (is.null(old)) return(invisible(NULL))
  keep <- c("status", "template_id", "template_origin", "template_version")
  # NULL -> NA, because a NULL element of a list serialises as `{}` - an empty OBJECT where the
  # record said null, which reads as a value that is there and is empty.
  old$statement_attempt <- lapply(old[intersect(keep, names(old))],
                                  function(v) if (is.null(v)) NA else v)
  rec <- utils::modifyList(old, delta)
  safe({
    txt <- jsonlite::toJSON(rec, auto_unbox = TRUE, na = "null", pretty = TRUE)
    con <- file(path, open = "w", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(txt, con)
  })
  invisible(path)
}
