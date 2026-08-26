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

# .layout_hint_safe(hint, kind) -- the layout hint with anything that is not a
# structural word removed.
#
# G9. layout_signature() (R/layout.R) keys a PDF's hint off the line carrying the
# most transaction-header keywords -- and when NO line carries two of them it
# falls back to THE TWELVE COMMONEST LONG WORDS ON THE PAGE. Measured on a page
# whose commonest words were two surnames, the hint came back as
# "ambrose | whitcombe", and that string is written into logs/runs and into
# logs/metadata, which is kept FOREVER and whose rule 2 at the top of this file
# says descriptions, payees, references and names are NEVER stored. It also
# reaches the bulk audit's report, which is headed "safe to share - no PII".
#
# IT BITES THE OTHER ROUTES SPECIFICALLY. A bank statement has a transaction
# header, so it takes the keyword branch and yields "amount | balance | date |
# description". A document that reaches the form or the report route is BY
# DEFINITION one with no transaction header -- so it is exactly the class that
# takes the fallback.
#
# The hint exists to CLUSTER layouts, and a surname clusters nothing: it belongs
# to one person, so it can only ever split a cluster that should have been one.
# So a PDF's hint is kept only where it is made of the structural words it is
# supposed to be made of. A delimited or excel file's hint is its HEADER ROW --
# column names, structure by construction, and there is no frequency fallback on
# that branch at all -- so it is passed through untouched rather than filtered
# down to nothing.
#
# Nothing else moves. What actually clusters is the SIGNATURE, a hash of the
# whole token set, and it is not touched here: every cluster this tool has ever
# formed still forms, with a hint that names nobody.
#
# R/layout.R HAS SINCE FIXED ITS OWN HALF -- its fallback now keeps only header
# keywords too, so on today's engine this filter changes nothing. It stays
# anyway, and only here, at the two places these files WRITE a hint down: this
# module's records are kept forever and its own rule 2 above promises no names
# are in them, and a promise a module makes about its own files should not
# depend on a fallback in a different module staying as it is. It is a lock on a
# door, not a second way to do something.
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
# signal for descriptions), never the text itself. Input is sanitised to valid
# UTF-8 first so nchar() can't throw on a hostile multibyte value in a non-UTF-8
# locale.
.len_stats <- function(x) {
  n <- nchar(.enc_safe(x %||% character(0)))
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
    # WHICH ROUTE THIS RECORD DESCRIBES. Every record used to describe the
    # STATEMENT attempt and say nothing about being one, so a report that
    # converted perfectly was filed here as `status: unsupported,
    # template_id: null` -- and this folder is kept forever and is what
    # R/suggestions.R mines for "build these next". A genuinely unsupported
    # layout and a report that read 102 rows out of 6 tables were the same
    # record. This field is what separates them; amend_metadata_record() below
    # is what corrects the rest once the route is known.
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
    # reuse the signature the caller already computed (convert.R) when supplied.
    sig <- ctx$layout_sig %||% safe(layout_signature(ctx$input), list(signature = NA, hint = ""))
    rec$layout <- list(
      signature = sig$signature %||% NA_character_,
      format    = tmpl$format %||% (ctx$input$kind %||% NA_character_),
      kind      = ctx$input$kind %||% NA_character_,
      n_pages   = suppressWarnings(as.integer(meta$pages_actual %||% h$page_count %||% NA)),
      n_columns = if (identical(ctx$input$kind, "excel")) ncol(ctx$input$table %||% data.frame())
                  else if (identical(ctx$input$kind, "delimited")) length(strsplit(
                    (ctx$input$lines %||% "")[1], "[,\t;|]")[[1]]) else NA_integer_)
    # G9: the hint, and only the part of it this file is allowed to keep forever.
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
      # joined: a template may declare several candidate formats (scalar field)
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

  # ---- template hints: everything needed to DRAFT a template ----
  # The richest single signal for a statement the engine could NOT match (but
  # captured for every run at `full`): PII-safe per-source-column profiles
  # (kind / format / fill / masked shape) plus the engine's own best-guess
  # mapping, so a human or an AI assistant has ALL the structural detail to build
  # a template without seeing statement content. See column_profile.R.
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

# write_metadata_record(logdir, run_id, record) -- persist one metadata record to
# logs/metadata/<run_id>.json. No-op for a NULL record (level = off). Never throws.
write_metadata_record <- function(logdir, run_id, record) {
  if (is.null(record) || is.null(run_id) || is.na(run_id)) return(invisible(NULL))
  safe(write_log_record(logdir, "metadata", run_id, record))
}

# ---------------------------------------------------------------------------
# WHEN A FORM OR A REPORT WINS, THE RECORD HAS TO SAY SO
# ---------------------------------------------------------------------------
#
# L7. convert_document() tries the statement pipeline first and only then the
# form and the report ones -- and the metadata record is written by the statement
# pass, before either of the others has run. So a report that converted perfectly
# left a record describing the abandoned statement attempt: `status:
# "unsupported"`, `template_id: null`, no kind. Measured against the seven-page
# fixture, where the run log for the SAME run correctly said kind tables, status
# ok, 6 tables, 102 rows.
#
# That record is kept FOREVER and is exempt from the rollup, and R/suggestions.R
# mines exactly this folder for "build these next". So the corpus could not tell
# a genuinely unsupported layout -- a real gap, worth a template -- from a report
# that read 102 rows. It would recommend building what already exists, which is
# the same fault L4 fixed in the bulk audit, one folder along.
#
# THE STATEMENT ATTEMPT IS NOT DELETED. It is moved under its own name. The
# statement pipeline really did find no match, and that is a signal worth keeping
# (it is what a future model would learn "this is not a statement" from); what it
# may not do is masquerade as the verdict on the run. So the top of the record
# tells the truth about the run and `statement_attempt` holds what the first pass
# thought, labelled.

# route_metadata(res) -- the fields a form or report result corrects on the
# statement attempt's record, plus that route's own structure. Numbers and
# counts only, exactly as rule 2 at the top of this file requires: no label
# text, no table names, no values.
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

# .metadata_record_path(logdir, run_id) -- the file the statement pass just wrote
# for THIS run. write_log_record() never overwrites, so a run id that clashed
# with an earlier one is filed as "<id>~2.json"; the record for this run is the
# most recently written of that set, never simply "<id>.json". Getting that wrong
# would correct one run's record with another run's answer, which is worse than
# the fault being fixed.
.metadata_record_path <- function(logdir, run_id) {
  id <- as.character(run_id %||% NA_character_)[1]
  if (is.na(id) || !nzchar(id)) return(NA_character_)
  dir <- file.path(logdir, "metadata")
  if (!dir.exists(dir)) return(NA_character_)
  base <- safe(.safe_name(id), id)
  # .safe_name() has already reduced the id to [A-Za-z0-9_.-], so the only
  # character left that a regular expression would read as anything but itself
  # is the dot.
  rx <- paste0("^", gsub(".", "\\.", base, fixed = TRUE), "(~[0-9]+)?\\.json$")
  hits <- list.files(dir, pattern = rx, full.names = TRUE)
  if (!length(hits)) return(NA_character_)
  hits[order(file.mtime(hits), hits, decreasing = TRUE)][1]
}

# amend_metadata_record(logdir, res) -- correct this run's metadata record once
# the route is known. No-op when there is no record (capture level `off`, or a
# statement run, which the record already describes correctly). Never throws.
#
# In place, on the file this run wrote: a second file would leave the forever
# corpus holding two records for one conversion, one of them the wrong answer --
# which is the fault this fixes, doubled.
amend_metadata_record <- function(logdir, res) {
  delta <- safe(route_metadata(res), list())
  if (!length(delta)) return(invisible(NULL))
  path <- safe(.metadata_record_path(logdir, res$run_id %||% res$run_log$run_id), NA_character_)
  if (is.na(path) || !nzchar(path)) return(invisible(NULL))
  old <- safe(jsonlite::fromJSON(paste(safe_readlines(path), collapse = "\n"),
                                 simplifyVector = FALSE), NULL)
  if (is.null(old)) return(invisible(NULL))
  keep <- c("status", "template_id", "template_origin", "template_version")
  # NULL -> NA, because a NULL element of a list serialises as `{}` -- an empty
  # OBJECT where the record said null. "template_id": {} is a worse answer than
  # the one being corrected: it reads as a value that is there and is empty.
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
