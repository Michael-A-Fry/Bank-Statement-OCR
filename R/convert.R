# convert.R -- orchestrator. NEVER throws: every failure becomes a `failed`
# result with an actionable message. Logs exactly one line per run.

# convert_statement(...) -> result (build-contract sections 6, 7).
convert_statement <- function(path, bank = NULL, statement_type = NULL,
                              outdir = "out", templates_dir = "templates",
                              requested_by = NULL,
                              formats = c("xlsx", "csv", "json"),
                              logdir = "logs") {
  base <- tools::file_path_sans_ext(basename(path %||% "input"))
  result <- new_result(status = "failed", template_id = NA_character_,
                       messages = character(0))
  detected_template <- NA_character_
  template_version <- NA
  row_count <- 0L
  kpi_fail_count <- 0L
  trust_level <- NA_character_

  outcome <- tryCatch({
    templates <- load_templates(templates_dir)
    input <- read_input(path)

    det <- detect_statement(input, templates, hint_bank = bank,
                            hint_type = statement_type)

    if (!isTRUE(det$matched)) {
      result$status <- "unsupported"
      result$template_id <- if (is.na(det$template_id)) NA_character_ else det$template_id
      result$messages <- status_message("unsupported", "no template matched", det$detail)
      result$trust <- list(level = "low", score = 0, reasons = det$detail)
    } else {
      template <- templates[[det$template_id]]
      detected_template <- template$id
      template_version <- template$version %||% NA

      parsed <- parse_statement(input, template)
      recon <- reconcile(parsed, template)
      row_count <- nrow(parsed$transactions)
      kpi_fail_count <- sum(recon$kpis$status == "fail")
      trust_level <- recon$trust$level

      outputs <- write_outputs(parsed, recon, outdir, base, formats)

      status <- if (kpi_fail_count > 0 || identical(recon$trust$level, "low")) {
        "needs_review"
      } else {
        "ok"
      }
      msg <- if (status == "ok") {
        status_message("ok", sprintf("matched %s, %d row(s), trust %s",
                                     template$id, row_count, recon$trust$level))
      } else {
        status_message("needs_review",
          sprintf("parsed %d row(s) but review needed", row_count),
          paste(recon$trust$reasons, collapse = "; "))
      }

      result$status <- status
      result$template_id <- template$id
      result$trust <- recon$trust
      result$kpis <- recon$kpis
      result$header <- parsed$header
      result$outputs <- outputs
      result$messages <- msg
    }
    result
  }, error = function(e) {
    r <- new_result(status = "failed")
    r$messages <- status_message("failed", conditionMessage(e),
                                 "check the file is readable and matches a template")
    r
  })

  result <- outcome

  # ---- run log (no raw statement content) ----
  safe(log_event(logdir, list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    requested_by = requested_by %||% NA_character_,
    source_file = basename(path %||% NA_character_),
    source_sha256 = safe(file_sha256(path), NA_character_),
    bank_hint = bank %||% NA_character_,
    detected_template = result$template_id,
    template_version = template_version,
    status = result$status,
    trust_level = result$trust$level %||% NA_character_,
    row_count = row_count,
    kpi_fail_count = kpi_fail_count,
    message = paste(result$messages, collapse = " | ")
  )))

  result
}
