# forms.R -- orchestrator for the "mode: fields" paradigm (IRD summaries,
# KiwiSaver/account summaries): documents whose useful data is LABELLED VALUES,
# not a transaction table. This wires the standalone extract_fields() primitive
# into a full pipeline (load templates -> detect by fingerprint -> extract ->
# write outputs), kept deliberately separate from the transaction pipeline so the
# core stays unchanged. A fields template is a normal YAML with `mode: fields`
# and a `fields:` block; they live in fields_templates/ (curated) and a user dir.

# is_fields_template(t) -- TRUE for a mode:fields template.
is_fields_template <- function(t) identical(t$mode %||% "", "fields") && !is.null(t$fields)

# load_fields_templates(dir, user_dir) -> named list<template>, keyed by id. Only
# mode:fields templates are returned. Lenient (a bad one is skipped, not fatal) so
# one malformed form template can never break the others.
load_fields_templates <- function(dir = "fields_templates", user_dir = NULL) {
  out <- list()
  dirs <- c(dir, user_dir)
  for (d in dirs) {
    if (is.null(d) || !dir.exists(d)) next
    for (f in list.files(d, pattern = "\\.ya?ml$", full.names = TRUE)) {
      t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
      if (is.null(t) || !is_fields_template(t) || is.null(t$id)) next
      if (is.null(out[[t$id]])) out[[t$id]] <- t
    }
  }
  out
}

# validate_fields_template(t) -> character() of problems (empty = valid).
validate_fields_template <- function(t) {
  p <- character(0)
  if (!is.list(t)) return("template is not a mapping")
  if (is.null(t$id) || !nzchar(as.character(t$id))) p <- c(p, "missing 'id'")
  if (!identical(t$mode %||% "", "fields")) p <- c(p, "mode must be 'fields'")
  if (is.null(t$fields) || !length(t$fields)) p <- c(p, "at least one field is required")
  p
}

# detect_form(input, ftemplates) -> list(template_id, matched, score, detail).
# Matched when a template's every fingerprint phrase is present on the page text
# and it is the unique best. No fingerprint (need == 0) never auto-matches.
detect_form <- function(input, ftemplates) {
  hay <- paste(input$pages %||% character(0), collapse = "\n")
  if (!length(ftemplates))
    return(list(template_id = NA_character_, matched = FALSE, score = 0,
                detail = "no form templates are installed"))
  # A template matches only if ALL its identifying phrases hit; the unique best is
  # the one with the most phrases (most specific). No phrases -> never auto-match.
  full <- vapply(ftemplates, function(t) {
    need <- as.character(t$fingerprint$page_contains_all %||% character(0))
    length(need) > 0 && all(vapply(need, function(ph) grepl(ph, hay, fixed = TRUE), logical(1)))
  }, logical(1))
  if (!any(full))
    return(list(template_id = NA_character_, matched = FALSE, score = 0,
                detail = "no form template's identifying phrases were all found"))
  need_len <- vapply(ftemplates, function(t) length(t$fingerprint$page_contains_all %||% character(0)), integer(1))
  cand <- which(full)
  best <- cand[which.max(need_len[cand])]
  # unique best (strictly more identifying phrases than any other match)
  matched <- sum(need_len[cand] == need_len[best]) == 1
  list(template_id = names(ftemplates)[best], matched = TRUE,
       score = need_len[best],
       detail = if (matched) "matched by identifying phrases"
                else "several form templates matched equally; pick a bank")
}

# write_form_outputs(fields, outdir, basename, formats) -> named path vector.
write_form_outputs <- function(fields, outdir, basename,
                               formats = c("xlsx", "csv", "json")) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0)
  tidy <- fields[, intersect(c("field", "label", "value", "raw", "matched",
                               "required", "flagged"), names(fields)), drop = FALSE]
  if ("csv" %in% formats) {
    p <- file.path(outdir, paste0(basename, ".fields.csv"))
    utils::write.csv(tidy, p, row.names = FALSE, na = "")
    paths <- c(paths, p)
  }
  if ("xlsx" %in% formats && requireNamespace("openxlsx", quietly = TRUE)) {
    p <- file.path(outdir, paste0(basename, ".fields.xlsx"))
    wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Fields")
    openxlsx::writeData(wb, "Fields", tidy)
    tryCatch({ openxlsx::saveWorkbook(wb, p, overwrite = TRUE); paths <- c(paths, p) },
             error = function(e) NULL)
  }
  if ("json" %in% formats) {
    p <- file.path(outdir, paste0(basename, ".fields.json"))
    kv <- stats::setNames(as.list(fields$value), fields$field)
    writeLines(jsonlite::toJSON(list(fields = kv, detail = tidy),
               auto_unbox = TRUE, null = "null", pretty = TRUE), p)
    paths <- c(paths, p)
  }
  paths
}

# convert_form(path, ...) -> result list. Never throws: any failure is a `failed`
# result with an actionable message, mirroring convert_statement's contract.
convert_form <- function(path, fields_dir = "fields_templates",
                         user_fields_dir = NULL, outdir = "out",
                         formats = c("xlsx", "csv", "json"),
                         dict = NULL, template_id = NULL) {
  base <- tools::file_path_sans_ext(basename(path %||% "input"))
  res <- list(status = "failed", template_id = NA_character_, fields = NULL,
              outputs = character(0), messages = character(0),
              required_missing = 0L, n_fields = 0L)
  tryCatch({
    ftpls <- load_fields_templates(fields_dir, user_fields_dir)
    input <- read_input(path)
    dict <- dict %||% (if (exists("default_label_dict", mode = "function")) default_label_dict() else list())

    tmpl <- NULL
    if (!is.null(template_id) && !is.null(ftpls[[template_id]])) {
      tmpl <- ftpls[[template_id]]
    } else {
      det <- detect_form(input, ftpls)
      if (!isTRUE(det$matched)) {
        res$status <- "unsupported"
        res$messages <- status_message("unsupported", "no form template matched", det$detail)
        return(res)
      }
      tmpl <- ftpls[[det$template_id]]
    }

    fields <- extract_fields(input, tmpl, dict)
    res$template_id <- tmpl$id %||% NA_character_
    res$fields <- fields
    res$n_fields <- nrow(fields)
    res$required_missing <- sum(isTRUE(fields$flagged) | fields$flagged, na.rm = TRUE)
    res$outputs <- write_form_outputs(fields, outdir, base, formats)
    res$status <- if (res$required_missing > 0) "needs_review" else "ok"
    res$messages <- if (res$required_missing > 0)
      status_message(res$status, sprintf("%d required field(s) not found", res$required_missing),
                     "check the document or the template labels")
      else status_message("ok", sprintf("%d field(s) extracted", res$n_fields))
    res
  }, error = function(e) {
    res$messages <- paste("error:", conditionMessage(e)); res
  })
}

# save_fields_template(tmpl, dir) -> path. Validates then writes <dir>/<id>.yaml.
save_fields_template <- function(tmpl, dir = "fields_templates_user") {
  probs <- validate_fields_template(tmpl)
  if (length(probs)) stop("fields template is not valid: ", paste(probs, collapse = "; "))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  id <- gsub("[^A-Za-z0-9_]+", "_", tmpl$id)
  path <- file.path(dir, paste0(id, ".yaml"))
  yaml::write_yaml(tmpl, path)
  invisible(path)
}
