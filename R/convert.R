# convert.R -- orchestrator. NEVER throws: every failure becomes a `failed`
# result with an actionable message. Logs exactly one line per run.

# convert_statement(...) -> result (build-contract sections 6, 7).
convert_statement <- function(path, bank = NULL, statement_type = NULL,
                              outdir = "out", templates_dir = "templates",
                              user_templates_dir = "templates_user",
                              requested_by = NULL,
                              formats = c("xlsx", "csv", "json"),
                              logdir = "logs", redaction_rects = NULL,
                              force_template = NULL, force_rows = NULL) {
  base <- tools::file_path_sans_ext(basename(path %||% "input"))
  # run_id: a stable, human-readable handle for this conversion. Content hash
  # (first 10 chars) + timestamp, so feedback and logs can point back to it.
  sha <- safe(file_sha256(path), NA_character_)
  run_id <- paste0(substr(if (is.na(sha)) "na" else sha, 1, 10), "-",
                   format(Sys.time(), "%Y%m%d%H%M%S"))
  result <- new_result(status = "failed", template_id = NA_character_,
                       messages = character(0))
  detected_template <- NA_character_
  template_version <- NA
  row_count <- 0L
  kpi_fail_count <- 0L
  trust_level <- NA_character_
  # layout/detection signals for the admin reports (clustering unsupported files)
  closest_template <- NA_character_
  detect_detail <- NA_character_
  layout_sig <- NA_character_
  layout_hint <- NA_character_
  template_origin <- NA_character_

  outcome <- tryCatch({
    templates <- load_template_set(templates_dir, user_templates_dir)
    input <- read_input(path, redaction_rects = redaction_rects)
    meta <- extract_metadata(input)
    multi <- detect_multiple_statements(input, meta)
    lsig <- layout_signature(input)
    layout_sig <- lsig$signature
    layout_hint <- lsig$hint

    # force_template: the user picked an exact template on Convert -> skip
    # detection and use it directly (still runs full reconciliation, so a wrong
    # forced pick still surfaces as needs_review, never silently trusted).
    if (!is.null(force_template) && nzchar(force_template) && !is.null(templates[[force_template]])) {
      det <- list(template_id = force_template, matched = TRUE, score = NA_real_,
                  margin = Inf, runner_up = NA_character_,
                  candidates = data.frame(id = force_template, score = NA_real_,
                                          stringsAsFactors = FALSE),
                  detail = "template chosen by the user")
    } else {
      det <- detect_statement(input, templates, hint_bank = bank,
                              hint_type = statement_type)
    }
    closest_template <- det$template_id
    detect_detail <- det$detail

    if (!isTRUE(det$matched)) {
      result$status <- "unsupported"
      result$template_id <- if (is.na(det$template_id)) NA_character_ else det$template_id
      result$messages <- status_message("unsupported", "no template matched", det$detail)
      result$trust <- list(level = "low", score = 0, reasons = det$detail)
      result$metadata <- c(meta, list(multiple = multi))
      result$diagnostics <- build_diagnostics("unsupported", det = det,
        metadata = list(multi = multi, pages = meta$pages_actual, max_page_pt = meta$max_page_pt))
    } else {
      template <- templates[[det$template_id]]
      detected_template <- template$id
      template_version <- template$version %||% NA
      template_origin <- template$origin %||% "default"

      parsed <- parse_statement(input, template, force_rows = force_rows)
      recon <- reconcile(parsed, template)
      row_count <- nrow(parsed$transactions)
      kpi_fail_count <- sum(recon$kpis$status == "fail")
      trust_level <- recon$trust$level

      # A THIN detection margin (won over the runner-up by only 1 fingerprint
      # phrase) means a near-duplicate template nearly matched too -- exactly the
      # "matched but maybe the wrong variant" case. Treat it as needs_review so the
      # analyst confirms the template, even when every KPI passes.
      thin_match <- is.finite(det$margin) && det$margin <= 1 && !is.na(det$runner_up)
      status <- if (kpi_fail_count > 0 || identical(recon$trust$level, "low") ||
                    isTRUE(multi$likely_multiple) || thin_match) {
        "needs_review"
      } else {
        "ok"
      }
      diag <- build_diagnostics(status, parsed = parsed, recon = recon,
        metadata = list(multi = multi, pages = meta$pages_actual, max_page_pt = meta$max_page_pt))
      outputs <- write_outputs(parsed, recon, outdir, base, formats,
        diagnostics = diag, metadata = meta)

      msg <- if (status == "ok") {
        status_message("ok", sprintf("matched %s, %d row(s), trust %s",
                                     template$id, row_count, recon$trust$level))
      } else {
        status_message("needs_review",
          sprintf("parsed %d row(s) but review needed", row_count),
          paste(recon$trust$reasons, collapse = "; "))
      }
      # OCR caveat is in the trust reasons already (so it shows on needs_review);
      # add it to the ok message too, so a clean scanned statement still warns the
      # reviewer that machine-read text is not guaranteed accurate.
      if (isTRUE(recon$trust$ocr_pages > 0) && status == "ok") {
        msg <- c(msg, status_message("ok", sprintf(
          "%d page(s) were read by OCR%s — verify amounts and descriptions against the source PDF",
          recon$trust$ocr_pages,
          if (is.na(recon$trust$ocr_min_confidence)) ""
          else sprintf(" (min page confidence %.0f%%)", recon$trust$ocr_min_confidence))))
      }
      if (thin_match) {
        msg <- c(msg, status_message("needs_review",
          sprintf("this matched %s by only %s over %s", template$id, det$margin, det$runner_up),
          "confirm it's the right template — see the candidate templates below"))
      }

      result$status <- status
      result$template_id <- template$id
      result$trust <- recon$trust
      result$kpis <- recon$kpis
      result$header <- parsed$header
      result$outputs <- outputs
      result$diagnostics <- diag
      result$coverage <- field_coverage(parsed, template)
      result$metadata <- c(meta, list(multiple = multi))
      result$messages <- msg
      # Candidate templates + margin, for the "matched but maybe wrong" panel.
      result$candidates <- det$candidates
      result$detect <- list(margin = det$margin, runner_up = det$runner_up,
                            thin = thin_match)
    }
    result
  }, error = function(e) {
    r <- new_result(status = "failed")
    r$messages <- status_message("failed", conditionMessage(e),
                                 "check the file is readable and matches a template")
    r$diagnostics <- build_diagnostics("failed", messages = r$messages)
    r
  })

  result <- outcome
  result$run_id <- run_id

  # ---- run log: one file per run (concurrency-safe, no shared append) ----
  # requested_by defaults to the OS-authenticated user, so every conversion is
  # attributed to a real person without any login prompt.
  safe(write_log_record(logdir, "runs", run_id, list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    run_id = run_id,
    requested_by = requested_by %||% current_user(),
    source_file = basename(path %||% NA_character_),
    source_sha256 = sha,
    bank_hint = bank %||% NA_character_,
    detected_template = if (result$status %in% c("ok", "needs_review")) result$template_id else NA_character_,
    template_origin = template_origin,
    closest_template = closest_template,
    detect_detail = detect_detail,
    layout_signature = layout_sig,
    layout_hint = layout_hint,
    template_version = template_version,
    status = result$status,
    trust_level = result$trust$level %||% NA_character_,
    row_count = row_count,
    kpi_fail_count = kpi_fail_count,
    pages = result$metadata$pages_actual %||% NA_integer_,
    period_start = result$metadata$period_start %||% NA_character_,
    period_end = result$metadata$period_end %||% NA_character_,
    n_accounts = result$metadata$n_accounts %||% NA_integer_,
    multiple_statements = isTRUE(result$metadata$multiple$likely_multiple),
    message = paste(result$messages, collapse = " | ")
  )))

  result
}
