# forms.R -- orchestrator for "mode: fields": documents whose useful data is LABELLED VALUES, not
# a transaction table. Templates live in templates/fields/ and templates/fields_user/.

is_fields_template <- function(t) identical(t$mode %||% "", "fields") && !is.null(t$fields)

# Only VALID mode:fields templates. Lenient - a bad one is skipped, not fatal - but never silent:
# skip reasons come back on attr(x, "load_errors"). include_hidden parks one out of detection.
load_fields_templates <- function(dir = "templates/fields", user_dir = NULL,
                                  include_hidden = FALSE) {
  out <- list()
  errors <- character(0)
  dirs <- c(dir, user_dir)
  origins <- c(rep("default", length(dir)), rep("user", length(user_dir)))
  for (k in seq_along(dirs)) {
    d <- dirs[[k]]
    if (is.null(d) || !dir.exists(d)) next
    for (f in list.files(d, pattern = "\\.ya?ml$", full.names = TRUE)) {
      t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
      if (is.null(t) || !is_fields_template(t) || is.null(t$id)) next
      if (isTRUE(t$hidden) && !include_hidden) next
      probs <- validate_fields_template(t)
      if (length(probs)) {
        errors <- c(errors, sprintf("%s: %s", t$id, paste(probs, collapse = "; ")))
        next
      }
      t$origin <- origins[[k]]
      if (is.null(out[[t$id]])) out[[t$id]] <- t
    }
  }
  attr(out, "load_errors") <- errors
  out
}

# The same generic-fingerprint gate statement templates pass, and a form template needs it MORE:
# detect_form() matches when every phrase is found anywhere in the page text, with no reconciliation
# behind it, so a generic fingerprint turns a correct "unsupported" into an unchecked "form" result.
.fields_fingerprint_problems <- function(ph) {
  ph <- trimws(as.character(unlist(ph %||% character(0))))
  ph <- ph[!is.na(ph) & nzchar(ph)]
  if (!length(ph)) return(paste0(
    "fingerprint.page_contains_all is required: without identifying phrases a form ",
    "template can never be matched, and a template that matches nothing is a trap"))
  if (exists(".fp_fingerprint_problems", mode = "function"))
    return(.fp_fingerprint_problems(ph, min_score = length(ph), fmt = "pdf"))
  specific <- (lengths(strsplit(ph, "\\s+")) >= 2) | (nchar(ph) >= 10)
  if (!any(specific)) return(paste0(
    "fingerprint.page_contains_all is too generic: add a distinctive phrase (a ",
    "document heading or a multi-word label, not single words like 'Balance')"))
  character(0)
}

validate_fields_template <- function(t) {
  p <- character(0)
  if (!is.list(t)) return("template is not a mapping")
  if (is.null(t$id) || !nzchar(as.character(t$id))) p <- c(p, "missing 'id'")
  if (!identical(t$mode %||% "", "fields")) p <- c(p, "mode must be 'fields'")
  if (is.null(t$fields) || !length(t$fields)) p <- c(p, "at least one field is required")
  p <- c(p, .fields_fingerprint_problems(t$fingerprint$page_contains_all))
  p <- c(p, .fields_label_problems(t$fields))
  p
}

# The same PII gate the fingerprint passes, applied to the label wordings a field matches on. Labels
# are captured VERBATIM into templates/fields_user/, which gets copied off the box: "Prepared for Mr
# John Smith" was refused as a fingerprint phrase and accepted as a label.
.fields_label_problems <- function(fields) {
  if (!length(fields) || !exists(".fp_has_pii", mode = "function")) return(character(0))
  bad <- character(0)
  for (i in seq_along(fields)) {
    spec <- fields[[i]]
    if (is.character(spec)) spec <- list(any_of = spec)
    if (!is.list(spec)) next
    ph <- trimws(as.character(unlist(spec$any_of %||% spec$label %||% character(0))))
    ph <- ph[!is.na(ph) & nzchar(ph)]
    if (!length(ph)) next
    hit <- safe(.fp_has_pii(ph), rep(FALSE, length(ph)))
    if (any(hit)) bad <- c(bad, ph[hit])
  }
  if (!length(bad)) return(character(0))
  sprintf(paste0(
    "label %s names a PERSON -- a template must describe the LAYOUT, never a ",
    "customer. Use the wording printed beside the value instead."),
    paste(sprintf("'%s'", unique(bad)), collapse = ", "))
}

# Case, spacing and punctuation taken out of both sides: a phrase typed "Anz Kiwisaver Scheme"
# against a page printing "ANZ KiwiSaver Scheme" never matched, and nothing said why.
.form_fp_norm <- function(x) {
  x <- as.character(x %||% "")
  safe({
    x <- gsub("\u00a0", " ", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("([A-Z])", "\\L\\1", x, perl = TRUE, useBytes = TRUE)
    x <- gsub("[^a-z0-9\n]+", " ", x, useBytes = TRUE)
    x <- gsub("[ \t]+", " ", x, useBytes = TRUE)
    x <- gsub(" *\n *", "\n", x, useBytes = TRUE)
    gsub("^[ \n]+|[ \n]+$", "", x, useBytes = TRUE)
  }, x)
}

.form_tpl_name <- function(t) {
  lab <- trimws(paste(trimws(as.character(t$bank %||% "")),
                      trimws(as.character(t$statement_type %||% ""))))
  if (nzchar(lab)) lab else as.character(t$id %||% "this template")[1]
}

# Same return shape as detect_statement(). The rule is .detect_by_fingerprint() in R/templates.R;
# this route owns only how its text is folded and how a form template is named.
detect_form <- function(input, ftemplates) {
  .detect_by_fingerprint(input, ftemplates, "form", .form_fp_norm, .form_tpl_name)
}

# THE COLUMN LIST IS THE WHOLE OF A FORM'S EVIDENCE. It was a fixed seven, and `conflict` - the one
# quality signal a form has, and the one that DECIDES the run's status - was not among them.
write_form_outputs <- function(fields, outdir, basename,
                               formats = c("xlsx", "csv", "json")) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0)
  tidy <- fields[, intersect(c("field", "label", "value", "other_values", "raw",
                               "page", "found_by", "matched", "required",
                               "flagged", "conflict", "n"), names(fields)),
                 drop = FALSE]
  # A cell reading "=1+1" must not reach the file live: Excel evaluates it on open. Nothing is
  # stripped - a leading quote is added - and the JSON stays verbatim.
  safe_tidy <- .doc_csv_safe(tidy)
  if ("csv" %in% formats) {
    p <- file.path(outdir, paste0(basename, ".fields.csv"))
    utils::write.csv(safe_tidy, p, row.names = FALSE, na = "")
    paths <- c(paths, p)
  }
  if ("xlsx" %in% formats && requireNamespace("openxlsx", quietly = TRUE)) {
    p <- file.path(outdir, paste0(basename, ".fields.xlsx"))
    wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Fields")
    openxlsx::writeData(wb, "Fields", safe_tidy)
    # The same file twice must be the same file: openxlsx stamps the wall clock into
    # docProps/core.xml and the ZIP's mod-times.
    tryCatch({
      wb$core <- .deterministic_core(wb$core)
      openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
      safe(.normalize_zip_timestamps(p))
      paths <- c(paths, p)
    }, error = function(e) NULL)
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

convert_form <- function(path, fields_dir = "templates/fields",
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

    tmpl <- NULL; det <- NULL; ambiguous <- FALSE
    forced <- !is.null(template_id) && nzchar(as.character(template_id)[1])
    if (forced) {
      tmpl <- ftpls[[as.character(template_id)[1]]]
      if (is.null(tmpl)) {
        res$status <- "unsupported"
        res$messages <- status_message("unsupported",
          "the template you asked for is not in the library",
          "pick another one, or check it has not been removed")
        return(res)
      }
    } else {
      det <- detect_form(input, ftpls)
      ambiguous <- !isTRUE(det$matched) && length(det$tied) >= 2
      if (!isTRUE(det$matched) && !ambiguous) {
        res$status <- "unsupported"
        res$detect <- det
        res$messages <- status_message("unsupported", "no form template matched",
                                       det$detail_plain %||% det$detail)
        return(res)
      }
      tmpl <- ftpls[[det$template_id]]
    }

    fields <- extract_fields(input, tmpl, dict)
    res$template_id <- tmpl$id %||% NA_character_
    res$template_version <- tmpl$version %||% NA
    res$template_sha256 <- template_sha256(tmpl)
    res$forced_template <- forced
    res$detect <- det
    res$ambiguous <- ambiguous
    res$fields <- fields
    res$n_fields <- nrow(fields)
    res$required_missing <- sum(isTRUE(fields$flagged) | fields$flagged, na.rm = TRUE)
    res$outputs <- write_form_outputs(fields, outdir, base, formats)
    # n_fields counts the fields the template DECLARES, not the ones found, so a document where
    # nothing was read still reported "4 field(s) extracted", status ok.
    res$n_values <- sum(!is.na(fields$value) & nzchar(fields$value %||% ""))
    res$n_conflicts <- sum(isTRUE(fields$conflict) | fields$conflict, na.rm = TRUE)
    res$status <- if (res$n_values == 0L) "unsupported"
                  else if (res$n_conflicts > 0 || res$required_missing > 0 ||
                           ambiguous) "needs_review"
                  else "ok"
    me_why <- sprintf(
      "the %s fits this document but found none of its %d values",
      .form_tpl_name(tmpl), res$n_fields)
    me_fix <- paste("the labels are probably printed differently here, or the values sit in a",
                    "table rather than beside their label")
    if (res$n_values == 0L) res$matched_empty <- list(why = me_why, fix = me_fix)
    res$messages <- if (res$n_values == 0L)
      status_message("unsupported", me_why, me_fix)
      else if (ambiguous)
      status_message("needs_review",
        sprintf("%d templates fit this document equally well", length(det$tied)),
        "it was read with one of them - check the figures, and retire whichever template is the duplicate")
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

# Through the one saver all three kinds share, so this route cannot overwrite one form template
# with another whose id merely sanitises to the same filename.
save_fields_template <- function(tmpl, dir = "templates/fields_user") {
  .save_template_yaml(tmpl, dir, validate_fields_template, "fields template")
}

# One id, and the libraries say what kind of thing it is. Statement, then form, then report - the
# same order the front door tries them in.
.forced_template_kind <- function(id, templates_dir, user_templates_dir,
                                  fields_dir, user_fields_dir, doc_dir, user_doc_dir) {
  id <- as.character(id %||% "")[1]
  if (is.na(id) || !nzchar(id)) return(NA_character_)
  st <- safe(load_template_set(templates_dir, user_templates_dir), list())
  if (!is.null(st[[id]])) return("statement")
  fm <- safe(load_fields_templates(fields_dir, user_fields_dir), list())
  if (!is.null(fm[[id]])) return("form")
  dc <- safe(load_document_templates(doc_dir, user_doc_dir), list())
  if (!is.null(dc[[id]])) return("tables")
  NA_character_
}

# The part of the run record that changes when the ROUTE changes. Three rules, all broken before: a
# measure that does not apply is NA, not zero (literal zeros made Admin count every form and report
# run as a clean zero-failure conversion); the detection fields belong to the statement attempt and
# are CLEARED (the layout SIGNATURE stays - it names nobody); and the files are named.
.route_record <- function(kind, r, origin) {
  list(kind              = kind,
       status            = r$status,
       detected_template = r$template_id %||% NA_character_,
       template_origin   = origin,
       template_version  = r$template_version %||% NA,
       # Which template content ran. Hardcoded NA on both other routes until now: the difference
       # between a right figure and a defensible one.
       template_sha256   = r$template_sha256 %||% NA_character_,
       trust_level       = NA_character_,
       row_count         = NA_integer_,
       kpi_fail_count    = NA_integer_,
       closest_template  = NA_character_,
       detect_detail     = NA_character_,
       period_start      = NA_character_,
       period_end        = NA_character_,
       layout_hint       = NA_character_,
       output_files      = paste(basename(as.character(r$outputs %||% character(0))),
                                 collapse = "; "),
       message           = paste(r$messages, collapse = " | "))
}

# The LEAST confident table decides whether the workbook can be trusted. R/tables.R owns
# confidence, so this must degrade to NA rather than error.
.route_min_conf <- function(tr) {
  cf <- suppressWarnings(as.numeric(vapply(tr$tables %||% list(),
    function(x) as.numeric(x$confidence %||% NA_real_)[1], numeric(1))))
  cf <- cf[is.finite(cf)]
  if (!length(cf)) NA_real_ else min(cf)
}

# Write the run record and SAY SO IF IT COULD NOT BE WRITTEN: log_run() wrapped the write in safe() and
# both callers wrapped it again, so on a full disk every conversion still succeeded and left no audit
# record with nobody told. The FACT tested is whether there is a file on disk.
.record_and_return <- function(logdir, res, notice = character(0)) {
  if (length(notice)) res$messages <- c(notice, res$messages)
  # capture_metadata() runs inside convert_statement(), which on this route is the attempt that FAILED,
  # so the one record exempt from every rollup said "unsupported" for a conversion that produced six
  # tables. Best effort: a record that cannot be amended must not cost somebody their conversion.
  safe(amend_metadata_record(logdir, res))
  out <- tryCatch(log_run(logdir, res), error = function(e) conditionMessage(e))
  p <- if (is.list(out)) as.character(out$path %||% NA_character_)[1]
       else if (is.character(out) && length(out)) as.character(out)[1]
       else NA_character_
  reason <- if (is.list(out)) as.character(out$reason %||% NA_character_)[1] else NA_character_
  if (!is.na(p) && nzchar(p) && file.exists(p)) {
    res$log_path <- p
    return(res)
  }
  rec <- res$run_log
  if (is.null(rec) || !length(rec)) {
    res$log_error <- "no record of this conversion was built"
  } else {
    id <- as.character(rec$run_id %||% res$run_id %||% "unknown")[1]
    expect <- file.path(logdir, "runs", paste0(safe(.safe_name(id), id), ".json"))
    if (file.exists(expect)) { res$log_path <- expect; return(res) }
    again <- tryCatch(write_log_record(logdir, "runs", id, rec),
                      error = function(e) conditionMessage(e))
    q <- if (is.character(again) && length(again)) as.character(again)[1] else NA_character_
    if (!is.na(q) && nzchar(q) && file.exists(q)) {
      res$log_path <- q
      return(res)
    }
    res$log_error <- if (!is.na(reason) && nzchar(reason)) reason
                     else if (!is.na(q) && nzchar(q)) q
                     else "the audit log folder could not be written to"
  }
  res$messages <- c(status_message("needs_review",
    "this conversion was not recorded in the audit log",
    "tell whoever looks after this server before the file is relied on"),
    res$messages)
  res
}

# The single front door: transaction pipeline, then form extraction, then report, so ONE upload handles
# statements, labelled-value PDFs and multi-table reports. Never throws. THE RUN LOG IS WRITTEN HERE,
# ONCE: convert_statement() used to write it before returning, so a form that converted perfectly was
# on record as an `unsupported` statement. `kind` is the person's own answer and outranks detection;
# `template_id` CARRIES ITS KIND, looked up in all three libraries.
convert_document <- function(path, bank = NULL, statement_type = NULL, outdir = "out",
                             templates_dir = "templates/statements",
                             user_templates_dir = "templates/statements_user",
                             fields_dir = "templates/fields", user_fields_dir = NULL,
                             doc_dir = "templates/documents", user_doc_dir = NULL,
                             requested_by = NULL, formats = c("xlsx", "csv", "json"),
                             logdir = "logs", force_template = NULL, force_rows = NULL,
                             kind = c("auto", "statement", "other"),
                             template_id = NULL) {
  kind <- match.arg(kind)
  want <- as.character(template_id %||% force_template %||% "")[1]
  if (is.na(want)) want <- ""
  forced_kind <- NA_character_
  unknown_template <- FALSE
  contradiction <- FALSE
  if (nzchar(want)) {
    forced_kind <- .forced_template_kind(want, templates_dir, user_templates_dir,
                                         fields_dir, user_fields_dir,
                                         doc_dir, user_doc_dir)
    if (is.na(forced_kind)) { unknown_template <- TRUE; want <- "" }
    # A template of the wrong kind and an explicit answer are a contradiction, and the answer wins: the
    # "wrong bank?" panel keeps its selection when hidden, so ticking "Something else" left a bank
    # template chosen underneath, beating her on a control she could no longer see.
    if (!is.na(forced_kind) &&
        ((identical(kind, "other") && identical(forced_kind, "statement")) ||
         (identical(kind, "statement") && forced_kind %in% c("form", "tables")))) {
      contradiction <- TRUE; forced_kind <- NA_character_; want <- ""
    }
  }
  if (identical(forced_kind, "statement")) kind <- "statement"
  else if (forced_kind %in% c("form", "tables")) kind <- "other"
  force_stmt  <- if (identical(forced_kind, "statement") && nzchar(want)) want else NULL
  want_form   <- identical(forced_kind, "form")
  want_tables <- identical(forced_kind, "tables")
  res <- convert_statement(path, bank = bank, statement_type = statement_type, outdir = outdir,
                           templates_dir = templates_dir, user_templates_dir = user_templates_dir,
                           requested_by = requested_by, formats = formats, logdir = logdir,
                           force_template = force_stmt, force_rows = force_rows,
                           log = FALSE,
                           use_statement_templates = !identical(kind, "other"))
  res$kind <- "statement"
  res$asked_kind <- kind
  if (!is.null(res$run_log)) res$run_log$asked_kind <- kind
  notice <- character(0)
  if (unknown_template) notice <- status_message("needs_review",
    "the template you asked for is not in the library, so this was read the usual way",
    "check it has not been removed or renamed")
  if (contradiction) notice <- status_message("needs_review",
    "the template that was chosen is for a different kind of document, so what you said this is was used instead",
    "if that is wrong, say what the document is again and convert it once more")
  # When the person said it IS a statement, that is the whole search: a report template reading a
  # bank statement it happened to fingerprint is the same wrong answer in the other direction.
  if (identical(kind, "statement")) return(.record_and_return(logdir, res, notice))
  if (is.null(force_stmt) && !want_tables &&
      (want_form || identical(res$status, "unsupported"))) {
    fr <- tryCatch(convert_form(path, fields_dir = fields_dir, user_fields_dir = user_fields_dir,
                                outdir = outdir, formats = formats,
                                template_id = if (want_form) want else NULL),
                   error = function(e) NULL)
    # A form template that MATCHED but read none of its values needs the opposite advice from "we have
    # no template": the template exists and its labels do not match how this document prints them.
    # Without this the analyst was sent to build a template that already exists.
    if (!is.null(fr) && identical(fr$status, "unsupported") &&
        !is.na(fr$template_id %||% NA) && (fr$n_fields %||% 0L) > 0L) {
      res$messages <- fr$messages
      res$form_matched_empty <- fr$template_id
      res$diagnostics <- diagnostics_matched_empty(
        res$diagnostics, fr$matched_empty$why, fr$matched_empty$fix)
    }
    if (!is.null(fr) && (fr$status %in% c("ok", "needs_review"))) {
      fr$kind <- "form"
      fr$run_id <- res$run_id      # keep the run id so logging/feedback still line up
      # The person's own answer survives the handover: the run record kept asked_kind and the result
      # did not, so the screen could not say "you told us this is not a statement" back to her.
      fr$asked_kind <- kind
      fr$run_log <- utils::modifyList(res$run_log %||% list(), c(
        .route_record("form", fr, "fields"),
        list(n_fields         = as.integer(fr$n_fields %||% NA_integer_),
             # The numbers that decide a form's status, by name. All three were computed and thrown
             # away, so the only trace of "three labels disagreed" was prose inside a message.
             n_values         = as.integer(fr$n_values %||% NA_integer_),
             n_conflicts      = as.integer(fr$n_conflicts %||% NA_integer_),
             required_missing = as.integer(fr$required_missing %||% NA_integer_))))
      res <- fr
    }
  }
  # Third and last: a REPORT carrying many tables, reached only when neither a transaction nor a
  # form template read the document. Last because it is the least checkable of the three.
  if (is.null(force_stmt) && !want_form &&
      (want_tables || identical(res$status, "unsupported"))) {
    tr <- tryCatch(convert_tables(path, doc_dir = doc_dir, user_doc_dir = user_doc_dir,
                                  outdir = outdir,
                                  formats = intersect(formats, c("xlsx", "csv")),
                                  template_id = if (want_tables) want else NULL),
                   error = function(e) NULL)
    if (!is.null(tr) && (tr$status %in% c("ok", "needs_review"))) {
      tr$kind <- "tables"
      tr$run_id <- res$run_id
      tr$asked_kind <- kind
      tr$run_log <- utils::modifyList(res$run_log %||% list(), c(
        .route_record("tables", tr, "document"),
        list(n_tables     = as.integer(tr$n_tables %||% NA_integer_),
             n_table_rows = as.integer(tr$n_rows %||% NA_integer_),
             min_confidence  = .route_min_conf(tr),
             empty_tables    = length(tr$empty_tables %||% character(0)),
             thin_tables     = length(tr$thin_tables %||% character(0)),
             weak_tables     = length(tr$weak_tables %||% character(0)),
             absent_tables   = length(tr$absent_tables %||% character(0)),
             unclaimed_words = as.integer(tr$unclaimed_words %||% NA_integer_))))
      res <- tr
    } else if (!is.null(tr) && identical(tr$status, "unsupported") &&
               !is.na(tr$template_id %||% NA) && (tr$n_tables %||% 0L) > 0L &&
               # ...unless the FORM pass already claimed this document: two templates that both matched
               # and both read nothing is one message, and the earlier, more checkable paradigm keeps it.
               is.null(res$form_matched_empty)) {
      res$messages <- tr$messages
      res$doc_matched_empty <- tr$template_id
      res$diagnostics <- diagnostics_matched_empty(
        res$diagnostics, tr$matched_empty$why, tr$matched_empty$fix)
    }
  }
  .record_and_return(logdir, res, notice)
}
