# qlik_convert.R -- the Qlik-facing conversion entrypoint.
#
# Qlik is a LOCKED-DOWN front-end: it converts using PROVEN templates only (never
# analyst drafts), and on a bank it has no proven template for it does NOT try to
# build one -- it points the user at the full Shiny app (where template-building
# lives) and files the statement into the team's pickup queue so "reach out to us"
# happens automatically. The Shiny app itself is unchanged; ONLY this path is
# restricted. Everything is config-driven (see R/config.R).

# convert_for_qlik(path, outdir, config) -> a small status list, ALSO written to
# outdir/status.json, that the Qlik app reads to decide what to show:
#   status == "ok"/"needs_review"  -> ODAG LOADs outdir/<base>.csv (the `csv` field)
#   needs_template == TRUE         -> show the message + a button opening `shiny_url`
convert_for_qlik <- function(path, outdir, config = load_config(),
                             requested_by = "qlik") {
  proven_dir <- config$qlik$proven_templates_dir %||% config$paths$templates
  res <- tryCatch(
    convert_document(path, outdir = outdir,
      templates_dir      = proven_dir,
      user_templates_dir = NULL,           # PROVEN ONLY -- never analyst drafts
      fields_dir         = config$paths$fields,
      user_fields_dir    = NULL,           # proven form templates only, too
      requested_by       = requested_by %||% "qlik",
      logdir             = config$paths$logs),
    error = function(e) list(status = "failed",
      messages = paste("Could not read this file:", conditionMessage(e))))

  status <- res$status %||% "failed"
  csv <- res$outputs[grepl("\\.csv$", res$outputs %||% character(0))]
  csv <- if (length(csv)) csv[1] else NA_character_
  needs_template <- identical(status, "unsupported")
  row_count <- if (!is.na(csv) && file.exists(csv))
    tryCatch(nrow(utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE)),
             error = function(e) NA_integer_) else NA_integer_

  # On a miss: queue the statement for the Shiny team (the existing Admin pickup
  # list), so a new bank is picked up and templated without anyone re-uploading.
  if (needs_template && isTRUE(config$qlik$queue_unsupported))
    safe(record_upload(path, name = basename(path),
      requested_by = requested_by %||% "qlik", status = "unsupported",
      run_id = res$run_id %||% NA_character_, template = NA_character_,
      trust = NA_character_, detail = "via Qlik: no proven template",
      dir = config$paths$uploads))

  out <- list(
    status         = status,
    needs_template = needs_template,
    run_id         = res$run_id %||% NA_character_,
    template_id    = res$template_id %||% NA_character_,
    trust_level    = res$trust$level %||% NA_character_,
    row_count      = row_count,
    csv            = csv,
    message        = if (needs_template)
        "No proven template for this bank yet. Set it up in the full app, then Qlik will convert it."
      else paste(res$messages %||% "", collapse = " | "),
    shiny_url      = config$app$shiny_url)

  safe({
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, na = "null", pretty = TRUE),
               file.path(outdir, "status.json"))
  })
  out
}

# convert_statement_sse(path, outdir, config) -- Rserve/SSE-friendly wrapper. It
# converts (proven only), writes the audit artifacts to disk as always, and RETURNS
# a data frame so a Qlik SSE `LOAD ... EXTENSION R.ScriptEval(...)` receives the
# transactions table directly. On a no-proven-template miss it returns a one-row
# frame carrying the status + app link, so the Qlik sheet can branch on it.
convert_statement_sse <- function(path, outdir = NULL, config = load_config()) {
  if (is.null(outdir))
    outdir <- file.path(config$feed$feed_dir %||% "feed", "qlik",
                        tools::file_path_sans_ext(basename(path)))
  st <- convert_for_qlik(path, outdir, config)
  if (!is.na(st$csv) && file.exists(st$csv))
    return(utils::read.csv(st$csv, stringsAsFactors = FALSE, check.names = FALSE))
  data.frame(status = st$status, needs_template = st$needs_template,
             message = st$message, shiny_url = st$shiny_url,
             stringsAsFactors = FALSE)
}
