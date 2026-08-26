# forms.R -- orchestrator for the "mode: fields" paradigm (IRD summaries,
# KiwiSaver/account summaries): documents whose useful data is LABELLED VALUES,
# not a transaction table. This wires the standalone extract_fields() primitive
# into a full pipeline (load templates -> detect by fingerprint -> extract ->
# write outputs), kept deliberately separate from the transaction pipeline so the
# core stays unchanged. A fields template is a normal YAML with `mode: fields`
# and a `fields:` block; they live in templates/fields/ (curated) and
# templates/fields_user/ (built on the box).

# is_fields_template(t) -- TRUE for a mode:fields template.
is_fields_template <- function(t) identical(t$mode %||% "", "fields") && !is.null(t$fields)

# load_fields_templates(dir, user_dir) -> named list<template>, keyed by id. Only
# VALID mode:fields templates are returned. Lenient (a bad one is skipped, not
# fatal) so one malformed form template can never break the others -- but never
# SILENT: the skip reasons come back on attr(x, "load_errors"), exactly as
# load_templates() does for statement templates, so the app can say which form
# template vanished and why.
#
# include_hidden mirrors load_template_set(): a template flagged `hidden: true` is
# parked out of detection without being deleted, and the Admin management view
# asks for them back so it can un-hide one. Hiding used to work for statement
# templates only, which made Admin's Hide button a control that did nothing on two
# of the three kinds -- worse than no control.
load_fields_templates <- function(dir = "templates/fields", user_dir = NULL,
                                  include_hidden = FALSE) {
  out <- list()
  errors <- character(0)
  dirs <- c(dir, user_dir)
  # Which FOLDER a template came out of is the only thing that says whether it is
  # shipped-and-tested or built on this box, and Admin has to say which before it
  # offers to delete one. Stamped here, the same marker load_templates() puts on a
  # statement template, so one overview can read all three kinds the same way.
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
  p <- c(p, .fields_label_problems(t$fields))
  p
}

# .fields_label_problems(fields) -- THE SAME PII GATE THE FINGERPRINT PASSES,
# applied to the label wordings a field matches on.
#
# WHY. The gate guarded one field. A form template's labels are captured
# VERBATIM off the page -- typed from it, or dragged off it -- and the saved file
# goes into templates/fields_user/, which is what gets copied off the box when
# somebody backs it up or sends it on. "Prepared for Mr John Smith" was refused
# as a fingerprint phrase and accepted, unchanged, as a label to look for. Same
# rule, same wording, so there is one thing to learn and not two.
#
# It is a rule about NAMING A PERSON, not about wording: a label is meant to be
# the words printed beside the value ("Opening balance"), and no printed label
# names the account holder.
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

# .form_fp_norm(x) -- how a fingerprint phrase is compared to the page: case,
# spacing and punctuation taken out of it, exactly as the report route's
# .doc_fp_norm and the table header matcher's .doc_norm already do.
#
# WHY IT IS A SEPARATE COPY. The three routes must agree on this rule and must
# not depend on each other's files to get it; the shape is what is shared, and a
# test pins the two to the same answers. Measured before this existed: a
# template whose phrase was typed "Anz Kiwisaver Scheme" against a page printing
# "ANZ KiwiSaver Scheme" never matched, and nothing on screen said why -- the
# exact complaint "I put a phrase printed on it, bang smack on front page, still
# used another template".
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

# .form_tpl_name(t) -- a form template said in WORDS. template_display_name()
# appends "statement", which is exactly what a form is not.
.form_tpl_name <- function(t) {
  lab <- trimws(paste(trimws(as.character(t$bank %||% "")),
                      trimws(as.character(t$statement_type %||% ""))))
  if (nzchar(lab)) lab else as.character(t$id %||% "this template")[1]
}

# detect_form(input, ftemplates) -> THE SAME RETURN SHAPE detect_statement() and
# detect_document_template() have: template_id, matched, score, candidates,
# eligible_ids, tied, margin, runner_up, detail, detail_plain.
#
# Every phrase still has to appear -- that stays the eligibility gate -- but the
# three faults the report detector had were here word for word, and cost the same
# work:
#
#   * A TIE REFUSED. Two good one-phrase form templates that both fit meant
#     "unsupported" on a document the library can read perfectly, and it named
#     neither of them. The statement route has never done that: it reports the
#     tie, reads with the best of them and marks the run for review. Choosing by
#     a PRINCIPLED order (most phrases, then shipped before hand-built, then the
#     id) is not choosing arbitrarily, and it beats refusing to read a document
#     nobody can then convert.
#   * A NEAR MISS SAID NOTHING. "no form template's identifying phrases were all
#     found" cannot be acted on. Scoring fractionally costs nothing and lets this
#     route say which template came closest and which wording was not on the page.
#   * THE COMPARISON WAS CASE AND SPACING SENSITIVE while every other text
#     comparison in the product normalises. See .form_fp_norm.
detect_form <- function(input, ftemplates) {
  none <- function(detail, plain = NULL)
    list(template_id = NA_character_, matched = FALSE, score = 0,
         candidates = data.frame(id = character(0), score = numeric(0),
                                 need = numeric(0), stringsAsFactors = FALSE),
         eligible_ids = character(0), tied = character(0),
         margin = NA_real_, runner_up = NA_character_,
         detail = detail, detail_plain = plain)
  if (!length(ftemplates)) return(none("no form templates are installed"))

  hay <- .form_fp_norm(paste(input$pages %||% character(0), collapse = "\n"))
  ids <- names(ftemplates)
  # A hand-assembled unnamed list still has to come back with an answer rather
  # than an error: the id is the key it was filed under, or its position.
  if (is.null(ids)) ids <- as.character(seq_along(ftemplates))
  ids[!nzchar(ids)] <- as.character(seq_along(ftemplates))[!nzchar(ids)]
  names(ftemplates) <- ids
  sc <- lapply(ids, function(i) {
    need <- as.character(unlist(ftemplates[[i]]$fingerprint$page_contains_all %||%
                                  character(0)))
    hit <- if (!length(need)) logical(0) else
      vapply(need, function(ph) {
        k <- .form_fp_norm(ph)
        nzchar(k) && grepl(k, hay, fixed = TRUE, useBytes = TRUE)
      }, logical(1))
    list(score = sum(hit), need = length(need), missing = need[!hit])
  })
  scores <- vapply(sc, function(s) as.numeric(s$score), numeric(1))
  needs  <- vapply(sc, function(s) as.numeric(s$need), numeric(1))
  # A template with NO phrases can never be matched (validation refuses one), so
  # it is never eligible however the page reads.
  eligible <- needs > 0 & scores >= needs

  # THE ORDER, and every step of it is principled. Most phrases first (the most
  # specific template that fits wins), then a shipped template ahead of one built
  # here (a shipped one has a test behind it), then the id so the answer is fully
  # deterministic and never depends on the order a folder happened to list in.
  shipped <- vapply(ids, function(i)
    as.numeric(!identical(ftemplates[[i]]$origin %||% "default", "user")), numeric(1))
  ord <- order(scores, needs, shipped, ids,
               decreasing = c(TRUE, TRUE, TRUE, FALSE), method = "radix")
  ids <- ids[ord]; scores <- scores[ord]; needs <- needs[ord]
  shipped <- shipped[ord]; eligible <- eligible[ord]; sc <- sc[ord]
  cand_df <- data.frame(id = ids, score = scores, need = needs,
                        stringsAsFactors = FALSE)

  if (!any(eligible)) {
    best <- ids[1]; miss <- sc[[1]]$missing
    return(list(template_id = NA_character_, matched = FALSE, score = 0,
      candidates = cand_df, eligible_ids = character(0), tied = character(0),
      margin = NA_real_, runner_up = if (length(ids) >= 2) ids[2] else NA_character_,
      detail = sprintf("closest %s score %g/%g%s", best, scores[1], needs[1],
        if (length(miss)) sprintf(" (missing %s)",
          paste(sprintf("'%s'", miss), collapse = ", ")) else ""),
      # The same fact for the person holding the document: no id, no fraction.
      detail_plain = sprintf("The closest we have is the %s, but this file doesn't print %s.",
        .form_tpl_name(ftemplates[[best]]),
        if (length(miss)) paste(sprintf("\"%s\"", miss), collapse = " or ")
        else "the wording it looks for")))
  }

  e_ids <- ids[eligible]; e_needs <- needs[eligible]; e_ship <- shipped[eligible]
  win <- e_ids[1]
  second_need <- if (length(e_needs) >= 2) e_needs[2] else -Inf
  second_ship <- if (length(e_ship) >= 2) e_ship[2] else -Inf
  # Unambiguous when the winner is strictly more specific, or ties on specificity
  # and something principled separates them (a shipped template over a hand-built
  # one). A shipped template drawing level with a hand-built one is not a real
  # question, and stopping to ask it helps nobody.
  matched <- (e_needs[1] > second_need) ||
             (e_needs[1] == second_need && e_ship[1] > second_ship)
  tied <- if (sum(eligible) >= 2) e_ids[e_needs == e_needs[1] & e_ship == e_ship[1]]
          else character(0)
  if (length(tied) < 2L) tied <- character(0)
  list(template_id = win, matched = matched, score = e_needs[1],
       candidates = cand_df, eligible_ids = e_ids, tied = tied,
       margin = if (is.finite(second_need)) e_needs[1] - second_need else Inf,
       runner_up = if (length(e_ids) >= 2) e_ids[2] else NA_character_,
       detail = if (matched) "matched by identifying phrases"
                else sprintf("%d form templates are equally specific here (%s)",
                             length(tied), paste(tied, collapse = ", ")),
       detail_plain = if (matched) NULL else
         sprintf("%d templates fit this document equally well, so it was read with the %s.",
                 length(tied), .form_tpl_name(ftemplates[[win]])))
}

# write_form_outputs(fields, outdir, basename, formats) -> named path vector.
#
# THE COLUMN LIST IS THE WHOLE OF A FORM'S EVIDENCE. It used to be a fixed seven,
# and `conflict` -- the one quality signal a form has, and the signal that already
# DECIDES the run's status -- was not among them. Measured on the shipped sample:
# three values were labels printed more than once with different figures, the tool
# took the first of each and said so on the screen, and the downloaded CSV showed
# all three as matched TRUE, flagged FALSE with no conflict column at all. The
# screen gets closed; the file is what goes in the bundle. So the file carries
# what the screen carried: the conflict, how many times the label was read
# (`n`), the readings that were NOT taken (`other_values`), and -- the same
# evidence the report route's pairs have always carried -- the page it was read
# from and how it was found.
write_form_outputs <- function(fields, outdir, basename,
                               formats = c("xlsx", "csv", "json")) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0)
  tidy <- fields[, intersect(c("field", "label", "value", "other_values", "raw",
                               "page", "found_by", "matched", "required",
                               "flagged", "conflict", "n"), names(fields)),
                 drop = FALSE]
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
    # FORCING A TEMPLATE, ALL THE WAY THROUGH. The argument has always existed and
    # nothing ever passed it; worse, an id that is not in the library fell through
    # to detection in silence, so "read it with THIS one" could quietly be
    # answered by a different template altogether.
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
      # A TIE IS NOT AN ANSWER OF "NO". Two templates that both fit used to mean
      # "unsupported" on a document the library reads perfectly, and it named
      # neither. Read it with the best of them, say so, and send it to review.
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
    # WHICH TEMPLATE CONTENT ran, not just which id. A report converts in March
    # and in September the defence asks how a figure was derived; a declared
    # `version:` that nobody bumps cannot answer that and a content hash can.
    res$template_sha256 <- template_sha256(tmpl)
    res$forced_template <- forced
    res$detect <- det
    res$ambiguous <- ambiguous
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
                  else if (res$n_conflicts > 0 || res$required_missing > 0 ||
                           ambiguous) "needs_review"
                  else "ok"
    res$messages <- if (res$n_values == 0L)
      status_message("unsupported",
        # THE TEMPLATE IN WORDS, not its id. An id is a maintainer's handle: the
        # person holding the document cannot check anything against one, and the
        # id is still on the run record where it is useful.
        sprintf("the %s fits this document but found none of its %d values",
                .form_tpl_name(tmpl), res$n_fields),
        "the labels are probably printed differently here, or the values sit in a table rather than beside their label")
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

# save_fields_template(tmpl, dir) -> path. Validates then writes <dir>/<id>.yaml.
save_fields_template <- function(tmpl, dir = "templates/fields_user") {
  probs <- validate_fields_template(tmpl)
  if (length(probs)) stop("fields template is not valid: ", paste(probs, collapse = "; "))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  id <- gsub("[^A-Za-z0-9_]+", "_", tmpl$id)
  path <- file.path(dir, paste0(id, ".yaml"))
  yaml::write_yaml(tmpl, path)
  invisible(path)
}

# .forced_template_kind(id, ...) -> "statement" | "form" | "tables" | NA.
#
# ONE id, and the libraries say what kind of thing it is. The alternative -- the
# caller telling us the kind alongside the id -- is a second thing to get right
# and a second thing to get wrong, and the answer is already written down.
# Statement first, then form, then report: the same order the front door tries
# them in, so an id that somehow existed in two libraries resolves the way the
# document would have been read anyway.
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

# .route_record(kind, r, origin) -- the part of the run record that changes when
# the ROUTE changes. Three rules, and all three were broken:
#
#   1. A MEASURE THAT DOES NOT APPLY IS NA, NOT ZERO. `row_count` and
#      `kpi_fail_count` were literal zeros on every form and report run, so Admin
#      counted each of them as a clean zero-failure conversion alongside
#      statements that genuinely reconciled. Zero failures and no failures to
#      count are not the same fact and a spreadsheet cannot tell them apart.
#   2. THE DETECTION FIELDS BELONG TO THE STATEMENT ATTEMPT AND ARE CLEARED.
#      closest_template, detect_detail, period_start, period_end and layout_hint
#      were carried over from the pass that failed -- measured, a form run
#      reported a period of "1 April 2025" scraped off the form, and layout_hint
#      carried page words that on a non-statement are as likely to be somebody's
#      surname as a column name. The layout SIGNATURE stays: it is a hash of the
#      page shape, it names nobody, and it is what clusters layouts.
#   3. THE FILES ARE NAMED. Without them a record and the workbook it produced
#      can only be tied together by guessing at a filename.
.route_record <- function(kind, r, origin) {
  list(kind              = kind,
       status            = r$status,
       detected_template = r$template_id %||% NA_character_,
       template_origin   = origin,
       template_version  = r$template_version %||% NA,
       # WHICH TEMPLATE CONTENT ran. Hardcoded NA on both other routes until now,
       # while template_sha256() has always worked on any template: the
       # difference between a right figure and a defensible one.
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

# .route_min_conf(tr) -- the LEAST confident table in a report, which is the one
# that decides whether the workbook can be trusted. Read defensively: R/tables.R
# owns confidence and this must degrade to NA rather than error if it moves.
.route_min_conf <- function(tr) {
  cf <- suppressWarnings(as.numeric(vapply(tr$tables %||% list(),
    function(x) as.numeric(x$confidence %||% NA_real_)[1], numeric(1))))
  cf <- cf[is.finite(cf)]
  if (!length(cf)) NA_real_ else min(cf)
}

# .record_and_return(logdir, res) -- write the run record and SAY SO IF IT COULD
# NOT BE WRITTEN.
#
# K9: log_run() wrapped the write in safe() and both callers here wrapped it
# again, so a run log that could not be written was swallowed twice. Proven with
# an uncreatable log folder: no file, nothing propagated. On a full disk or a
# vanished network path every conversion still succeeded, produced a complete
# workbook, and left no audit record at all -- and nobody was told. In a forensic
# deployment the record is half the deliverable.
#
# Written defensively against log_run()'s own contract: it may hand back the path
# it wrote, nothing at all, or (if R/convert.R gains the richer return K9 asks
# for) a list of path and reason. Whatever comes back, the FACT tested here is
# the one that matters -- is there a file on disk. When there is not, the writer
# is asked once more, unwrapped, purely so the operator gets the real reason
# instead of a shrug.
.record_and_return <- function(logdir, res, notice = character(0)) {
  if (length(notice)) res$messages <- c(notice, res$messages)
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
    # The file may be there even when nothing was handed back. Look before
    # writing again: a second record of one run is its own kind of wrong.
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
  # One sentence, first, where the person is looking: she has a workbook and no
  # record of how she got it, and only an operator can fix that.
  res$messages <- c(status_message("needs_review",
    "this conversion was not recorded in the audit log",
    "tell whoever looks after this server before the file is relied on"),
    res$messages)
  res
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
#
# `kind` IS THE PERSON'S OWN ANSWER, and it outranks detection.
#   "auto"      work it out: statement, then form, then report (the default, and
#               what every caller did before this existed).
#   "statement" it is a bank or card statement. Only statement templates are
#               tried; "no template for this yet" stays that answer instead of
#               being quietly re-read as a report.
#   "other"     it is NOT a bank statement. No statement template gets a vote, so
#               a form or report template is reached even when some bank's
#               fingerprint happens to fit.
#
# Reported: "I put a phrase printed on it, bang smack on front page, still used
# another template." It did, because the report pass is only reached when the
# statement pass fails, and a statement template had matched. No scoring change
# fixes that in general -- but nobody has to guess when the person holding the
# document already knows.
#
# `template_id` IS THE PERSON'S ANSWER TAKEN ONE STEP FURTHER: not just what kind
# of document it is, but WHICH template reads it. It travels the whole way and it
# CARRIES ITS KIND -- the id is looked up in all three libraries and the route
# follows from where it was found. Before this, convert_form() and
# convert_tables() each took a template_id and nothing ever passed one, while the
# front door's only override turned into kind "statement" outright: forcing a
# form template produced an unsupported STATEMENT. `force_template` is the old
# name for the same argument and is still accepted, so nothing that calls it
# breaks; an id that is in no library is said out loud and the document is read
# the usual way rather than refused.
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
    # A TEMPLATE OF THE WRONG KIND AND AN EXPLICIT ANSWER ARE A CONTRADICTION,
    # and the answer wins. Reported, and browser-proven: the "it picked the wrong
    # bank?" panel keeps its selection when it is hidden, so ticking "Something
    # else" left a bank template still chosen underneath -- and it beat her, in
    # silence, on a control she could no longer see. An answer she can see must
    # never lose to one she cannot.
    if (!is.na(forced_kind) &&
        ((identical(kind, "other") && identical(forced_kind, "statement")) ||
         (identical(kind, "statement") && forced_kind %in% c("form", "tables")))) {
      contradiction <- TRUE; forced_kind <- NA_character_; want <- ""
    }
  }
  # Forcing an exact statement template IS saying "this is a statement", so the
  # two can never disagree. Forcing a form or a report template says the opposite
  # just as plainly, and now says it just as effectively.
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
  # ASKED FOR A TEMPLATE THAT IS NOT THERE. Held, not written on to `res` here,
  # because a form or report result REPLACES `res` further down and would carry
  # the notice away with it -- and the one thing a notice must not do is go
  # missing on the route it was about.
  notice <- character(0)
  if (unknown_template) notice <- status_message("needs_review",
    "the template you asked for is not in the library, so this was read the usual way",
    "check it has not been removed or renamed")
  if (contradiction) notice <- status_message("needs_review",
    "the template that was chosen is for a different kind of document, so what you said this is was used instead",
    "if that is wrong, say what the document is again and convert it once more")
  # ...and when the person said it IS a statement, that is the whole search. A
  # report template reading a bank statement it happened to fingerprint is the
  # same wrong answer in the other direction, and it would arrive with no
  # reconciliation behind it to catch it.
  if (identical(kind, "statement")) return(.record_and_return(logdir, res, notice))
  # Fall back to form extraction only when nothing matched AND the user didn't
  # force a transaction template -- or go straight there when the form template
  # to use was named.
  if (is.null(force_stmt) && !want_tables &&
      (want_form || identical(res$status, "unsupported"))) {
    fr <- tryCatch(convert_form(path, fields_dir = fields_dir, user_fields_dir = user_fields_dir,
                                outdir = outdir, formats = formats,
                                template_id = if (want_form) want else NULL),
                   error = function(e) NULL)
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
      # ...and the person's OWN answer survives the handover. It was dropped here:
      # the run record kept asked_kind and the result did not, so the screen could
      # not say "you told us this is not a statement" back to her.
      fr$asked_kind <- kind
      # Reuse the statement pass's record (who/when/source/hash all still true)
      # and overwrite what the FORM decided, so the one record on disk describes
      # what actually happened -- see .route_record() for the three rules.
      fr$run_log <- utils::modifyList(res$run_log %||% list(), c(
        .route_record("form", fr, "fields"),
        list(n_fields         = as.integer(fr$n_fields %||% NA_integer_),
             # THE NUMBERS THAT DECIDE A FORM'S STATUS, BY NAME. All three were
             # computed and thrown away, so the only trace of "three labels
             # disagreed" was prose inside a truncated message string -- which
             # nothing can count, sort or chart.
             n_values         = as.integer(fr$n_values %||% NA_integer_),
             n_conflicts      = as.integer(fr$n_conflicts %||% NA_integer_),
             required_missing = as.integer(fr$required_missing %||% NA_integer_))))
      res <- fr
    }
  }
  # THIRD AND LAST: a REPORT carrying many tables (mode:document, R/doc_extract.R).
  # Reached only when neither a transaction template nor a form template read the
  # document, so it can never take work away from either. It stays last because it
  # is the least checkable of the three -- no reconciliation, no labelled values,
  # just tables read off the page -- and the honest place for the least checkable
  # answer is after every better one has been tried and failed.
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
             # THE NUMBERS THAT DECIDE A REPORT'S STATUS, BY NAME -- the same
             # ones convert_tables() weighs. A table found by position at
             # confidence 0.40 used to reach the record only as prose.
             min_confidence  = .route_min_conf(tr),
             empty_tables    = length(tr$empty_tables %||% character(0)),
             thin_tables     = length(tr$thin_tables %||% character(0)),
             weak_tables     = length(tr$weak_tables %||% character(0)),
             absent_tables   = length(tr$absent_tables %||% character(0)),
             unclaimed_words = as.integer(tr$unclaimed_words %||% NA_integer_))))
      res <- tr
    } else if (!is.null(tr) && identical(tr$status, "unsupported") &&
               !is.na(tr$template_id %||% NA) && (tr$n_tables %||% 0L) > 0L &&
               # ...unless the FORM pass already claimed this document. Two
               # templates that both matched and both read nothing is one
               # message, and the earlier, more checkable paradigm keeps it
               # rather than being silently overwritten by the later one.
               is.null(res$form_matched_empty)) {
      # Matched this report but read nothing from it -- the same distinction the
      # statement and form paths make, and the same reason: "the template exists
      # and did not fit" needs the opposite advice from "there is no template".
      res$messages <- tr$messages
      res$doc_matched_empty <- tr$template_id
    }
  }
  .record_and_return(logdir, res, notice)
}
