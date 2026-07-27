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
# VALID mode:fields templates are returned. Lenient (a bad one is skipped, not
# fatal) so one malformed form template can never break the others -- but never
# SILENT: the skip reasons come back on attr(x, "load_errors"), exactly as
# load_templates() does for statement templates, so the app can say which form
# template vanished and why.
load_fields_templates <- function(dir = "fields_templates", user_dir = NULL) {
  out <- list()
  errors <- character(0)
  dirs <- c(dir, user_dir)
  for (d in dirs) {
    if (is.null(d) || !dir.exists(d)) next
    for (f in list.files(d, pattern = "\\.ya?ml$", full.names = TRUE)) {
      t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
      if (is.null(t) || !is_fields_template(t) || is.null(t$id)) next
      probs <- validate_fields_template(t)
      if (length(probs)) {
        errors <- c(errors, sprintf("%s: %s", t$id, paste(probs, collapse = "; ")))
        next
      }
      if (is.null(out[[t$id]])) out[[t$id]] <- t
    }
  }
  attr(out, "load_errors") <- errors
  out
}

# .fields_fingerprint_problems(ph) -- the SAME generic-fingerprint gate the
# statement templates pass, applied to a form template.
#
# WHY a form template needs it MORE, not less. detect_form() matches when every
# phrase is found anywhere in the page text, and it is reached for every statement
# no transaction template recognised. There is no reconciliation behind it and no
# balance check to catch a wrong match -- just an extraction and a download button.
# So a form template whose fingerprint is made of words that sit on every bank
# statement would turn a correct "unsupported" verdict into a confident, unchecked
# "form" result. detect_form requires ALL phrases, so the effective min_score is
# the phrase count; that is what is handed to the shared gate.
.fields_fingerprint_problems <- function(ph) {
  ph <- trimws(as.character(unlist(ph %||% character(0))))
  ph <- ph[!is.na(ph) & nzchar(ph)]
  if (!length(ph)) return(paste0(
    "fingerprint.page_contains_all is required: without identifying phrases a form ",
    "template can never be matched, and a template that matches nothing is a trap"))
  # The shared implementation lives in R/templates.R and is the single source of
  # truth for "is this fingerprint distinctive / free of PII". Fall back to its
  # distinctiveness rule only if that file is not loaded (never weaker).
  if (exists(".fp_fingerprint_problems", mode = "function"))
    return(.fp_fingerprint_problems(ph, min_score = length(ph), fmt = "pdf"))
  specific <- (lengths(strsplit(ph, "\\s+")) >= 2) | (nchar(ph) >= 10)
  if (!any(specific)) return(paste0(
    "fingerprint.page_contains_all is too generic: add a distinctive phrase (a ",
    "document heading or a multi-word label, not single words like 'Balance')"))
  character(0)
}

# validate_fields_template(t) -> character() of problems (empty = valid).
validate_fields_template <- function(t) {
  p <- character(0)
  if (!is.list(t)) return("template is not a mapping")
  if (is.null(t$id) || !nzchar(as.character(t$id))) p <- c(p, "missing 'id'")
  if (!identical(t$mode %||% "", "fields")) p <- c(p, "mode must be 'fields'")
  if (is.null(t$fields) || !length(t$fields)) p <- c(p, "at least one field is required")
  p <- c(p, .fields_fingerprint_problems(t$fingerprint$page_contains_all))
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
  # unique best (strictly more identifying phrases than any other match). On an
  # equal-specificity TIE, matched is FALSE so convert_form asks the user to pick,
  # rather than silently extracting with an arbitrarily-chosen form template
  # (mirrors detect_statement's unique-best rule).
  matched <- sum(need_len[cand] == need_len[best]) == 1
  list(template_id = names(ftemplates)[best], matched = matched,
       score = need_len[best],
       detail = if (matched) "matched by identifying phrases"
                # NOT "pick a bank": no form screen has a bank control, so that was
                # an instruction nobody could follow. State the fact and stop --
                # separating two equally-specific form templates is a maintainer's
                # job, not the job of whoever uploaded the document.
                else "several form templates are equally specific here, so none was chosen")
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
    res$template_version <- tmpl$version %||% NA
    res$fields <- fields
    res$n_fields <- nrow(fields)
    res$required_missing <- sum(isTRUE(fields$flagged) | fields$flagged, na.rm = TRUE)
    res$outputs <- write_form_outputs(fields, outdir, base, formats)
    # HOW MANY VALUES DID WE ACTUALLY GET? n_fields counts the fields the template
    # DECLARES, not the ones found - so a document where nothing was read still
    # reported "4 field(s) extracted", status ok. A form has no reconciliation to
    # catch that: a labelled value is trusted because it was read, so an empty or
    # contradictory read must say so itself. This is the same gate the statement
    # path got (zero rows is not "ok"), which the form path never had.
    res$n_values <- sum(!is.na(fields$value) & nzchar(fields$value %||% ""))
    # A label found more than once with DIFFERENT values. The tool takes the first
    # and marks it; it must never be reported as a clean read, because which one is
    # right is a question only someone holding the document can answer.
    res$n_conflicts <- sum(isTRUE(fields$conflict) | fields$conflict, na.rm = TRUE)
    res$status <- if (res$n_values == 0L) "unsupported"
                  else if (res$n_conflicts > 0 || res$required_missing > 0) "needs_review"
                  else "ok"
    res$messages <- if (res$n_values == 0L)
      status_message("unsupported",
        sprintf("%s matches this document but found none of its %d values",
                res$template_id, res$n_fields),
        "the labels are probably printed differently here, or the values sit in a table rather than beside their label")
      else if (res$n_conflicts > 0)
      status_message("needs_review",
        sprintf("%d label(s) appear more than once with different values", res$n_conflicts),
        "the first of each was taken - check them against the document")
      else if (res$required_missing > 0)
      status_message("needs_review", sprintf("%d required field(s) not found", res$required_missing),
                     "check the document or the template labels")
      else status_message("ok", sprintf("%d of %d value(s) found", res$n_values, res$n_fields))
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

# convert_document(path, ...) -> result. The single front door: try the normal
# transaction pipeline first; if nothing matched, fall back to form (mode:fields)
# extraction, so ONE upload handles bank statements AND labelled-value PDFs (IRD
# summaries, KiwiSaver, letters). The result carries `kind` = "statement" or
# "form" so the UI knows which way to render it. Never throws.
#
# THE RUN LOG IS WRITTEN HERE, ONCE. convert_statement() used to write it before
# returning, so a form that converted perfectly was already on record as an
# `unsupported` statement under the same run_id -- reported as a failure in Admin
# and counted in the "build these next" queue, which reads exactly that field. The
# statement pass now only BUILDS the record (res$run_log); this function writes it
# after the final outcome is known. Nothing is ever rewritten.
convert_document <- function(path, bank = NULL, statement_type = NULL, outdir = "out",
                             templates_dir = "templates", user_templates_dir = "templates_user",
                             fields_dir = "fields_templates", user_fields_dir = NULL,
                             requested_by = NULL, formats = c("xlsx", "csv", "json"),
                             logdir = "logs", force_template = NULL, force_rows = NULL) {
  res <- convert_statement(path, bank = bank, statement_type = statement_type, outdir = outdir,
                           templates_dir = templates_dir, user_templates_dir = user_templates_dir,
                           requested_by = requested_by, formats = formats, logdir = logdir,
                           force_template = force_template, force_rows = force_rows,
                           log = FALSE)
  res$kind <- "statement"
  # Fall back to form extraction only when nothing matched AND the user didn't
  # force a transaction template.
  if (is.null(force_template) && identical(res$status, "unsupported")) {
    fr <- tryCatch(convert_form(path, fields_dir = fields_dir, user_fields_dir = user_fields_dir,
                                outdir = outdir, formats = formats), error = function(e) NULL)
    # A form template that MATCHED this document but read none of its values is a
    # different answer from "we have no template for this layout", and needs the
    # opposite advice: the template exists, its labels do not match how this
    # document prints them (or the values sit in a table rather than beside their
    # label). Without this the specific message was thrown away and the analyst was
    # sent to build a template that already exists - the same mistake the statement
    # path made before matched_but_empty.
    if (!is.null(fr) && identical(fr$status, "unsupported") &&
        !is.na(fr$template_id %||% NA) && (fr$n_fields %||% 0L) > 0L) {
      res$messages <- fr$messages
      res$form_matched_empty <- fr$template_id
    }
    if (!is.null(fr) && (fr$status %in% c("ok", "needs_review"))) {
      fr$kind <- "form"
      fr$run_id <- res$run_id      # keep the run id so logging/feedback still line up
      # Reuse the statement pass's record (who/when/source/layout signature all
      # still true) and overwrite what the FORM decided, so the one record on
      # disk describes what actually happened. The detection fields
      # (closest_template / detect_detail / layout_signature) are kept on purpose:
      # they still say which transaction templates nearly fitted.
      fr$run_log <- utils::modifyList(res$run_log %||% list(), list(
        kind              = "form",
        status            = fr$status,
        detected_template = fr$template_id %||% NA_character_,
        template_origin   = "fields",
        template_version  = fr$template_version %||% NA,
        template_sha256   = NA_character_,
        trust_level       = NA_character_,
        row_count         = 0L,                 # a form has fields, not rows
        n_fields          = as.integer(fr$n_fields %||% NA_integer_),
        kpi_fail_count    = 0L,
        message           = paste(fr$messages, collapse = " | ")))
      res <- fr
    }
  }
  safe(log_run(logdir, res))
  res
}
