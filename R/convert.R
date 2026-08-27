
# Build provenance makes determinism PROVABLE. template_sha256 hashes the template AS LOADED, so an
# edited YAML is visibly different at the same declared `version:`. Read ONCE per session.
.VERSION_CACHE <- new.env(parent = emptyenv())
engine_version <- function() {
  if (!is.null(.VERSION_CACHE$v)) return(.VERSION_CACHE$v)
  cand <- unique(c(file.path(Sys.getenv("ENGINE_ROOT", "."), "VERSION"), "VERSION"))
  v <- NA_character_
  for (p in cand) {
    if (!file.exists(p)) next
    ln <- trimws(safe(safe_readlines(p), character(0)))
    ln <- ln[!is.na(ln) & nzchar(ln)]
    if (length(ln)) { v <- ln[1]; break }
  }
  .VERSION_CACHE$v <- if (is.na(v)) "unknown" else v
  .VERSION_CACHE$v
}

.text_sha256 <- function(x) {
  x <- paste(as.character(x), collapse = "\n")
  if (requireNamespace("openssl", quietly = TRUE))
    return(paste0(openssl::sha256(charToRaw(x))))
  if (requireNamespace("digest", quietly = TRUE))
    return(digest::digest(x, algo = "sha256", serialize = FALSE))
  NA_character_
}

template_sha256 <- function(template) {
  if (is.null(template)) return(NA_character_)
  safe(.text_sha256(template_yaml(template)), NA_character_)
}

# Exactly one file per run, holding the FINAL outcome. Separate from convert_statement because
# convert_document may fall back to FORM extraction: written inline, a converted IRD form was recorded
# as an unsupported statement in Admin's "build these next" queue.
log_run <- function(logdir, result) {
  rec <- result$run_log
  if (is.null(rec) || !length(rec)) return(invisible(NULL))
  safe(write_log_record(logdir, "runs", rec$run_id %||% result$run_id %||% "unknown", rec))
}

# One sentence when the READER got nothing usable out of the file, NULL when it did - the line between
# "could not be read" and "no template yet", which need opposite answers, so each test is a FACT.

# validate_template requires date + description + a money column, so three is the fewest fields a
# convertible header row carries. Used ONLY on a file with a single record.
.MIN_TABLE_FIELDS <- 3L
.TABULAR_SCAN_LINES <- 200L

# A separator CHARACTER is not evidence: one comma in "Use the transaction export, not the PDF." made
# that sentence a statement layout. A table is the same split giving the SAME SHAPE line after line,
# so `lines` MUST keep its blank lines - the evidence is ADJACENCY.
.delimited_tabular <- function(lines, sep) {
  recs <- .split_records(lines, seq_along(lines))
  n <- vapply(recs, function(r) length(.record_fields(r$text, sep)), integer(1))
  if (!length(n) || max(n) < 2L) return(FALSE)
  # Two or more CONSECUTIVE records splitting the same way. Runs, not totals: an email whose
  # greeting and sign-off both end in a comma has two matching lines scattered through prose.
  r <- rle(n)
  if (any(r$lengths[r$values >= 2L] >= 2L)) return(TRUE)
  # ONE record has nothing to repeat, and a header-only export is indistinguishable from a sentence
  # with a comma, so fall back to the narrowest a statement table can be.
  sum(n > 0L) == 1L && max(n) >= .MIN_TABLE_FIELDS
}

.unreadable_reason <- function(input, templates = NULL) {
  kind <- input$kind %||% NA_character_
  if (identical(kind, "pdf")) {
    if (is.null(input$pages) || !length(input$pages))
      return(paste("no text could be read from this PDF - it is damaged, encrypted,",
                   "or not a PDF at all"))
    return(NULL)
  }
  if (identical(kind, "delimited")) {
    raw <- input$lines %||% character(0)
    raw[is.na(raw)] <- ""
    content <- which(nzchar(trimws(raw)))
    if (!length(content)) return("this file is empty - there is nothing in it to read")
    lines <- raw[content]
    seps <- unlist(lapply(templates %||% list(),
                          function(t) as.character(unlist(t$delimiter %||% character(0)))))
    seps <- unique(c(seps[!is.na(seps) & nzchar(seps)], ",", "\t", ";", "|"))
    # The shape test gets the file's real line structure, blanks included, because it decides on
    # ADJACENCY. The window is the first .TABULAR_SCAN_LINES lines WITH CONTENT plus the blanks
    # between, so a blank preamble cannot push the table out of view.
    head_lines <- raw[seq_len(content[min(length(content), .TABULAR_SCAN_LINES)])]
    for (s in seps) if (.delimited_tabular(head_lines, s)) return(NULL)
    # A declared header_regex is a template saying "MY table starts at this line" - direct evidence
    # even when the shape test cannot see it, since a preamble with no transactions has no run.
    hdr <- unlist(lapply(templates %||% list(),
                         function(t) as.character(t$preamble$header_regex %||% character(0))))
    for (h in hdr[!is.na(hdr) & nzchar(hdr)])
      if (any(safe(grepl(h, lines, perl = TRUE), FALSE))) return(NULL)
    return(paste("no rows of separated values were found in this file - it holds",
                 "text, but not a table"))
  }
  if (identical(kind, "excel")) {
    tbl <- input$table
    if (is.null(tbl) || !ncol(tbl))
      return("no worksheet with any data could be read from this workbook")
    return(NULL)
  }
  NULL
}

convert_statement <- function(path, bank = NULL, statement_type = NULL,
                              outdir = "out", templates_dir = "templates/statements",
                              user_templates_dir = "templates/statements_user",
                              requested_by = NULL,
                              formats = c("xlsx", "csv", "json"),
                              logdir = "logs", redaction_rects = NULL,
                              force_template = NULL, force_rows = NULL,
                              log = TRUE, use_statement_templates = TRUE) {
  # Nothing that touches `path` happens outside the funnel: docs/design.md and docs/overview.md both
  # state "never throws at the front door", and basename(path) once sat above the tryCatch, so
  # convert_document(1L) threw before a single guard ran.
  base <- "input"
  cfg <- safe(load_config(), .config_defaults())      # for the metadata-capture level
  t0 <- Sys.time()                                    # elapsed timing (metadata only)
  sha <- safe(file_sha256(path), NA_character_)
  run_id <- paste0(substr(if (is.na(sha)) "na" else sha, 1, 10), "-",
                   format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"), "-",
                   paste0(sample(c(0:9, letters[1:6]), 4, replace = TRUE), collapse = ""))
  result <- new_result(status = "failed", template_id = NA_character_,
                       messages = character(0))
  detected_template <- NA_character_
  template_version <- NA
  row_count <- 0L
  kpi_fail_count <- 0L
  trust_level <- NA_character_
  closest_template <- NA_character_
  detect_detail <- NA_character_
  layout_sig <- NA_character_
  layout_hint <- NA_character_
  template_origin <- NA_character_
  tmpl_sha <- NA_character_        # build provenance: WHICH template content ran

  outcome <- tryCatch({
    base <- tools::file_path_sans_ext(basename(path %||% "input"))
    # When the person has said this is NOT a bank statement, no statement template gets a vote: the
    # form and report passes are only reached when the statement pass comes back unsupported, so a
    # statement template matching a non-statement hid the report template.
    templates <- if (isTRUE(use_statement_templates))
      load_template_set(templates_dir, user_templates_dir) else list()
    input <- read_input(path, redaction_rects = redaction_rects)
    # THE FILE ITSELF COULD NOT BE READ - not the same answer as "we have never seen this layout".
    # A corrupt PDF and a note-to-self .txt both came back as "set one up".
    if (!is.null(why <- .unreadable_reason(input, templates))) stop(why, call. = FALSE)
    meta <- extract_metadata(input)
    multi <- detect_multiple_statements(input, meta)
    lsig <- layout_signature(input)
    layout_sig <- lsig$signature
    layout_hint <- lsig$hint

    # The user picked an exact template, so skip detection. Still runs full reconciliation, so a
    # wrong forced pick surfaces as needs_review rather than being silently trusted.
    if (!is.null(force_template) && nzchar(force_template) && !is.null(templates[[force_template]])) {
      det <- list(template_id = force_template, matched = TRUE, score = NA_real_,
                  margin = Inf, runner_up = NA_character_,
                  candidates = data.frame(id = force_template, score = NA_real_,
                                          stringsAsFactors = FALSE),
                  eligible_ids = force_template, tied = character(0),
                  detail = "template chosen by the user")
    } else {
      det <- detect_statement(input, templates, hint_bank = bank,
                              hint_type = statement_type)
    }
    closest_template <- det$template_id
    detect_detail <- det$detail
    parsed <- recon <- template <- NULL   # set in the matched branch; NULL otherwise

    # A TIE IS NOT A DEAD END. Take the best candidate, convert, and HOLD THE RUN FOR REVIEW with the
    # tie named - every figure still goes through reconciliation, so a wrong variant fails a check.
    ambiguous <- !isTRUE(det$matched) && length(det$tied) >= 2
    if (!isTRUE(det$matched) && !ambiguous) {
      result$status <- "unsupported"
      result$template_id <- if (is.na(det$template_id)) NA_character_ else det$template_id
      # det$detail names the closest template, its score and the phrases it wanted - kept behind "Show
      # me how it read this", but not on the verdict card: a template id and a fraction are not
      # something the person holding the statement can act on.
      result$messages <- status_message("unsupported",
        "we don't have a template for this layout yet")
      result$candidates <- det$candidates
      result$detect <- list(margin = NA_real_, runner_up = det$runner_up,
                            thin = FALSE, ambiguous = FALSE, tied = character(0))
      result$trust <- list(level = "low", score = 0, reasons = det$detail)
      result$metadata <- c(meta, list(multiple = multi))
      result$diagnostics <- build_diagnostics("unsupported", det = det,
        metadata = list(multi = multi, pages = meta$pages_actual, max_page_pt = meta$max_page_pt,
                        scanned_no_ocr = input$meta$scanned_no_ocr %||% 0L,
                        ocr_tools = input$meta$ocr_tools_available %||% TRUE))
    } else {
      # Read the statement with ONE template, all the way to a reconciled result. Auto-split returns
      # NULL unless the boundaries are independently confirmed, so anything else falls through.
      .read_with <- function(tid) {
        tpl <- templates[[tid]]
        sb <- if (isTRUE(multi$likely_multiple)) safe(split_bundle(input, tpl, meta), NULL) else NULL
        if (!is.null(sb))
          return(list(template = tpl, parsed = sb$parsed, recon = sb$recon,
                      sb = sb, did_split = TRUE))
        p <- parse_statement(input, tpl, force_rows = force_rows, meta = meta)
        list(template = tpl, parsed = p, recon = reconcile(p, tpl),
             sb = NULL, did_split = FALSE)
      }

      # WHICH template actually reads it. On a tie, det$template_id is the top scorer over ALL
      # templates while det$tied is those meeting their OWN min_score: reading with an ineligible top
      # scorer stamped its bank on the workbook while the screen named the other two.
      used_id <- if (ambiguous) det$tied[1] else det$template_id
      att <- .read_with(used_id)
      template <- att$template
      parsed <- att$parsed; recon <- att$recon; sb <- att$sb; did_split <- att$did_split
      detected_template <- template$id
      template_version <- template$version %||% NA
      template_origin <- template$origin %||% "default"
      tmpl_sha <- template_sha256(template)
      row_count <- nrow(parsed$transactions)
      kpi_fail_count <- sum(recon$kpis$status == "fail")
      trust_level <- recon$trust$level

      # A thin margin means a near-duplicate template nearly matched too, so it routes to
      # needs_review even when every KPI passes.
      thin_match <- is.finite(det$margin) && det$margin <= 1 && !is.na(det$runner_up)
      # A CONFIRMED auto-split has parsed and reconciled every statement on its own, so being
      # multiple is no longer a reason to force review. Raw `multi` still records it WAS a bundle.
      multi_resolved <- if (did_split) utils::modifyList(multi, list(likely_multiple = FALSE)) else multi
      # No transactions is not "review this" - it is the same outcome as having no template.
      # A badly OCR'd page cannot come back "ok": the status gate looked only at rows, KPIs, trust,
      # bundles and ties, so a page scoring 40 came back ok with a clean download. `ocr_pages > 0`
      # gates rather than ocr_min_conf alone - a page the reader could not measure proves nothing.
      ocr_pages <- suppressWarnings(as.integer(input$meta$ocr_pages %||% 0L))
      if (is.na(ocr_pages)) ocr_pages <- 0L
      ocr_conf  <- suppressWarnings(as.numeric(input$meta$ocr_min_conf %||% NA_real_))
      ocr_poor  <- ocr_pages > 0L &&
        (!is.finite(ocr_conf) || ocr_conf < PARAM_OCR_PAGE_MIN_CONF)

      status <- if (row_count == 0L) {
        "unsupported"
      } else if (kpi_fail_count > 0 || identical(recon$trust$level, "low") ||
                 isTRUE(multi_resolved$likely_multiple) || thin_match || ambiguous ||
                 ocr_poor) {
        "needs_review"
      } else {
        "ok"
      }
      diag <- build_diagnostics(status, parsed = parsed, recon = recon,
        metadata = list(multi = multi_resolved, pages = meta$pages_actual, max_page_pt = meta$max_page_pt,
                        template = template, pdf_doc = input$meta$pdf_doc,
                        matched_empty = if (row_count == 0L) template$id else NULL,
                        tied = if (ambiguous) det$tied else NULL,
                        tied_used = if (ambiguous) template$id else NULL))
      outputs <- write_outputs(parsed, recon, outdir, base, formats,
        diagnostics = diag, metadata = meta,
        build = list(engine_version = engine_version(), template_id = template$id,
                     template_version = as.character(template_version),
                     template_sha256 = tmpl_sha))

      # Every status message below lands on the VERDICT CARD, so they say the template in WORDS. A
      # template id is a maintainer's handle; it is still on every run-log record and diagnostic.
      used_name <- template_display_name(template)
      msg <- if (status == "ok") {
        status_message("ok", sprintf("matched %s, %d row(s), trust %s",
                                     used_name, row_count, recon$trust$level))
      } else if (row_count == 0L) {
        status_message("unsupported",
          sprintf("%s matches the wording on this statement but read no transactions from it",
                  used_name),
          "the columns are most likely in the wrong place - open this statement in the template toolkit and check where they sit")
      } else {
        status_message("needs_review",
          sprintf("parsed %d row(s) but review needed", row_count),
          paste(recon$trust$reasons, collapse = "; "))
      }
      # Named, not hidden, but stated as a fact about the TOOL: listing the tie without saying
      # which produced these figures leaves the reader unable to check the answer.
      if (ambiguous) {
        # A tie is most often the SAME layout set up twice, so the alternatives can share the used
        # template's display name; when nothing distinguishes them, the count is the whole fact.
        others <- setdiff(unique(vapply(det$tied,
          function(id) template_display_name(templates[[id]]), character(1))), used_name)
        msg <- c(status_message("needs_review",
          sprintf("%s templates fit this statement equally well; it was read as %s",
                  length(det$tied), used_name),
          if (length(others))
            sprintf("worth a check against the alternative%s: %s",
                    if (length(others) == 1L) "" else "s", paste(others, collapse = ", "))
          else "the same layout is set up more than once - one of them read this file"), msg)
      }

      result$status <- status
      result$template_id <- template$id
      result$trust <- recon$trust
      result$kpis <- recon$kpis
      result$header <- parsed$header
      result$outputs <- outputs
      # The governed feed's ONLY source of transaction values. Re-reading the output CSV let read.csv
      # mangle it - "000001" became 1 - and the Excel apostrophe guard survived into the feed. This is
      # the workbook's table, taken BEFORE that display-only guard.
      result$feed_rows <- display_transactions(parsed$transactions, parsed$extras)
      result$diagnostics <- diag
      result$coverage <- field_coverage(parsed, template)
      result$metadata <- c(meta, list(multiple = multi,
        split = if (did_split) list(n_statements = sb$n_statements, on = sb$on,
                                    statements = sb$statements) else NULL))
      result$messages <- msg
      result$candidates <- det$candidates
      result$detect <- list(margin = det$margin, runner_up = det$runner_up,
                            thin = thin_match, ambiguous = ambiguous,
                            tied = if (ambiguous) det$tied else character(0))
    }
    result$metadata_capture <- safe(capture_metadata(list(
      run_id = run_id, ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      requested_by = requested_by %||% current_user(), sha = sha,
      input = input, parsed = parsed, recon = recon, det = det, meta = meta,
      multi = multi, template = template, status = result$status,
      layout_sig = lsig, coverage = result$coverage,
      elapsed_ms = as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000), cfg), NULL)
    result
  }, error = function(e) {
    r <- new_result(status = "failed")
    # Not "...and matches a template": everything landing here failed BEFORE any template was in
    # play, so pointing at templates sends the reader to build one for a file that would not open.
    r$messages <- status_message("failed", conditionMessage(e),
                                 "check the file opens, is the type it claims to be, and is not password-protected or damaged")
    r$diagnostics <- build_diagnostics("failed", messages = r$messages)
    r
  })

  result <- outcome
  result$run_id <- run_id

  # One file per run, no shared append. requested_by defaults to the OS-authenticated user, so every
  # conversion is attributed without a login prompt. BUILT here, WRITTEN by log_run().
  result$run_log <- list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    run_id = run_id,
    kind = "statement",
    requested_by = requested_by %||% current_user(),
    source_file = safe(basename(path %||% NA_character_), NA_character_),
    source_sha256 = sha,
    bank_hint = bank %||% NA_character_,
    detected_template = if (result$status %in% c("ok", "needs_review")) result$template_id else NA_character_,
    template_origin = template_origin,
    closest_template = closest_template,
    detect_detail = detect_detail,
    layout_signature = layout_sig,
    layout_hint = layout_hint,
    template_version = template_version,
    engine_version = engine_version(),
    template_sha256 = tmpl_sha,
    status = result$status,
    trust_level = result$trust$level %||% NA_character_,
    row_count = row_count,
    n_fields = NA_integer_,          # forms only (see convert_document)
    kpi_fail_count = kpi_fail_count,
    pages = result$metadata$pages_actual %||% NA_integer_,
    period_start = result$metadata$period_start %||% NA_character_,
    period_end = result$metadata$period_end %||% NA_character_,
    n_accounts = result$metadata$n_accounts %||% NA_integer_,
    multiple_statements = isTRUE(result$metadata$multiple$likely_multiple),
    message = paste(result$messages, collapse = " | ")
  )
  if (isTRUE(log)) log_run(logdir, result)

  # Local only, kept forever, never fed. It describes the STATEMENT ATTEMPT and stays per-attempt
  # even when convert_document later succeeds with a form template.
  safe(write_metadata_record(logdir, run_id, result$metadata_capture))
  result$metadata_capture <- NULL   # transient -- not part of the returned result

  result
}
