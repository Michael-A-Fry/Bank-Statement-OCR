# batch_audit.R -- review MANY statements at once and produce a SINGLE, PII-safe picture. Only
# shapes, counts, institution names and layout hashes.

# A coarse, safe descriptor of the file kind.
.kind_of <- function(input) {
  if (identical(input$kind, "pdf"))
    return(if ((input$meta$ocr_pages %||% 0L) > 0L) "pdf-scanned" else "pdf-text")
  input$kind %||% "?"
}

# Returns list(per_file, clusters, recommendations, feature_gaps). Safe to share. FORM and REPORT
# documents count as COVERED - asking only the transaction half inflated the gap count and spent
# draft recommendations. Read-only primitives, not convert_document(): a 250-file folder would be
# 250 spurious conversions polluting the "build these next" queue.
batch_audit <- function(paths, templates = NULL, max_recommendations = 8L,
                        fields_templates = NULL, doc_templates = NULL) {
  root <- Sys.getenv("ENGINE_ROOT", ".")
  if (is.null(templates))
    templates <- safe(load_template_set(file.path(root, "templates", "statements"),
                                        file.path(root, "templates", "statements_user")), list())
  if (is.null(fields_templates))
    fields_templates <- safe(load_fields_templates(file.path(root, "templates", "fields")), list())
  if (is.null(doc_templates))
    doc_templates <- safe(load_document_templates(file.path(root, "templates", "documents"),
                                                  file.path(root, "templates", "documents_user")), list())
  dict <- safe(default_label_dict(), list())
  paths <- as.character(paths)
  rows <- vector("list", length(paths))
  rep_path <- list()   # signature -> a representative path (for drafting)

  for (i in seq_along(paths)) {
    p <- paths[i]
    input <- safe(read_input(p), NULL)
    if (is.null(input)) {
      rows[[i]] <- data.frame(idx = i, file_type = tolower(tools::file_ext(p)), kind = "unreadable",
        pages = NA_integer_, detected = FALSE, template = NA_character_, bank = NA_character_,
        status = "unreadable", n_rows = 0L, n_fields = 0L, n_tables = 0L,
        n_periods = NA, n_accounts = NA,
        redacted = 0L, amount_style = NA_character_, date_format = NA_character_,
        trust = NA_character_, signature = NA_character_, layout_hint = "",
        stringsAsFactors = FALSE)
      next
    }
    meta <- safe(extract_metadata(input), list())
    det  <- safe(detect_statement(input, templates), list(matched = FALSE))
    lsig <- safe(layout_signature(input), list(signature = NA_character_, hint = ""))
    tmpl <- if (isTRUE(det$matched)) templates[[det$template_id]] else NULL
    parsed <- if (!is.null(tmpl)) safe(parse_statement(input, tmpl), NULL) else NULL
    recon  <- if (!is.null(parsed)) safe(reconcile(parsed, tmpl), NULL) else NULL
    # FORM fallback, in the same order convert_document() uses it: only when no transaction template
    # matched.
    ftmpl <- NULL; fields <- NULL
    if (is.null(tmpl) && length(fields_templates)) {
      fdet <- safe(detect_form(input, fields_templates), list(matched = FALSE))
      if (isTRUE(fdet$matched)) {
        ftmpl  <- fields_templates[[fdet$template_id]]
        fields <- safe(extract_fields(input, ftmpl, dict), NULL)
      }
    }
    # REPORT fallback, third and last, so it can never take work away from either of the others.
    dtmpl <- NULL; dext <- NULL
    if (is.null(tmpl) && is.null(ftmpl) && length(doc_templates)) {
      ddet <- safe(detect_document_template(input, doc_templates), list(matched = FALSE))
      if (isTRUE(ddet$matched)) {
        dtmpl <- doc_templates[[ddet$template_id]]
        dext  <- safe(extract_document(input, dtmpl), NULL)
      }
    }
    status <- if (!is.null(ftmpl)) "form"       # covered by the form pipeline, not a gap
      else if (!is.null(dtmpl)) "report"        # covered by the report pipeline, not a gap
      else if (is.null(tmpl)) "unsupported"
      else if (is.null(parsed)) "failed"
      else if (!is.null(recon) && identical(recon$trust$level, "low")) "needs_review"
      else "ok"
    sig <- lsig$signature %||% NA_character_
    if (!is.null(sig) && !is.na(sig) && is.null(rep_path[[sig]])) rep_path[[sig]] <- p
    # Amount style, date format and bank come from the matched template when there is one, otherwise
    # are DETECTED from the file, so a mostly-unsupported batch still shows what is present.
    if (!is.null(tmpl)) {
      amt_style <- tmpl$table$amount_sign %||% tmpl$amount_sign %||% NA_character_
      # A template may declare SEVERAL candidate date formats; this frame needs ONE scalar per file.
      dt_fmt    <- .one_date_format(tmpl$table$date_format %||% tmpl$columns$date$format)
      bank_v    <- tmpl$bank %||% NA_character_
    } else if (!is.null(ftmpl) || !is.null(dtmpl)) {
      # A form has labelled values and a report its own tables, not a transaction table: no amount
      # style and no date format, but the institution IS known from the template.
      amt_style <- NA_character_; dt_fmt <- NA_character_
      bank_v    <- (ftmpl %||% dtmpl)$bank %||% NA_character_
    } else {
      dr <- safe(draft_template(p), NULL)
      amt_style <- if (is.null(dr)) NA_character_ else (dr$table$amount_sign %||% dr$amount_sign %||% NA_character_)
      dt_fmt    <- if (is.null(dr)) NA_character_ else .one_date_format(dr$table$date_format %||% dr$columns$date$format)
      bank_v    <- NA_character_    # bank can't be inferred reliably from an unmatched file
    }
    rows[[i]] <- data.frame(idx = i, file_type = tolower(tools::file_ext(p)),
      kind = .kind_of(input), pages = input$meta$page_count %||% NA_integer_,
      detected = isTRUE(det$matched) || !is.null(ftmpl) || !is.null(dtmpl),
      template = (if (!is.null(ftmpl)) ftmpl$id
                  else if (!is.null(dtmpl)) dtmpl$id
                  else det$template_id) %||% NA_character_,
      bank = bank_v, status = status,
      # Rows mean what the route says they mean, and `status` says which route. A report is counted
      # in the rows of its own tables - printed as a flat 0 for every report this audit has seen.
      n_rows = if (!is.null(parsed)) nrow(parsed$transactions)
               else if (!is.null(dext)) sum(vapply(dext$tables,
                 function(r) as.integer(r$n_rows %||% 0L), integer(1)))
               else 0L,
      # A form is measured in FIELDS, in its own column so a covered form is never mistaken for an
      # empty statement.
      n_fields = if (!is.null(fields)) nrow(fields) else 0L,
      # ...and a report in TABLES: "6 tables, 102 rows" is a covered report and "0 tables, 0 rows"
      # is a template that no longer fits.
      n_tables = if (!is.null(dext)) length(dext$tables) else 0L,
      n_periods = meta$n_periods %||% NA, n_accounts = meta$n_accounts %||% NA,
      redacted = sum(input$meta$redactions$redacted_words %||% 0L),
      amount_style = amt_style, date_format = dt_fmt,
      trust = if (!is.null(recon)) recon$trust$level else NA_character_,
      signature = sig,
      # The hint goes into a report headed "safe to share - no PII", so it is filtered to structural
      # words. The SIGNATURE, which is what actually clusters, is untouched.
      layout_hint = .layout_hint_safe(lsig$hint, input$kind), stringsAsFactors = FALSE)
  }
  per <- do.call(rbind, rows)

  # Cluster the unsupported and failed by layout signature, biggest gaps first. "form" and "report"
  # are deliberately not in this set: both are covered work, not gaps.
  uns <- per[per$status %in% c("unsupported", "failed", "unreadable"), , drop = FALSE]
  clusters <- data.frame(); recs <- list()
  if (nrow(uns)) {
    sig_ok <- uns[!is.na(uns$signature), , drop = FALSE]
    if (nrow(sig_ok)) {
      tab <- sort(table(sig_ok$signature), decreasing = TRUE)
      clusters <- do.call(rbind, lapply(names(tab), function(s) {
        ex <- sig_ok[sig_ok$signature == s, , drop = FALSE][1, ]
        data.frame(signature = s, count = as.integer(tab[[s]]), example_idx = ex$idx,
                   kind = ex$kind, layout_hint = ex$layout_hint, stringsAsFactors = FALSE)
      }))
      # Recommend a draft template for each of the top clusters.
      top <- utils::head(clusters, max_recommendations)
      for (k in seq_len(nrow(top))) {
        p <- rep_path[[top$signature[k]]]
        t <- safe(draft_template(p, bank = "NewBank"), NULL)
        recs[[length(recs) + 1L]] <- list(
          signature = top$signature[k], count = top$count[k], kind = top$kind[k],
          draft_id = t$id %||% NA_character_,
          draft_yaml = if (is.null(t)) "(could not auto-draft - build in the wizard)"
                       else template_yaml(t))
      }
    }
  }

  tallyNA <- function(x) { x <- x[!is.na(x) & nzchar(as.character(x))]; if (!length(x)) list() else as.list(sort(table(x), decreasing = TRUE)) }
  feature_gaps <- list(
    total = nrow(per),
    by_status = as.list(table(per$status)),
    by_kind = as.list(table(per$kind)),
    amount_styles = tallyNA(per$amount_style),
    date_formats = tallyNA(per$date_format),
    banks = tallyNA(per$bank),
    scanned = sum(grepl("scanned", per$kind)),
    with_redactions = sum(per$redacted > 0, na.rm = TRUE),
    multi_account = sum(per$n_accounts > 1, na.rm = TRUE),
    multi_period = sum(per$n_periods > 1, na.rm = TRUE),
    # Forms and reports are reported as their OWN kinds of coverage - neither a statement nor a gap
    # - so "unsupported" means what it says.
    forms = sum(per$status == "form"),
    reports = sum(per$status == "report"),
    report_tables = sum(per$n_tables, na.rm = TRUE),
    unsupported = nrow(uns), distinct_gap_layouts = nrow(clusters))

  list(per_file = per, clusters = clusters, recommendations = recs, feature_gaps = feature_gaps)
}

# format_batch_audit(b) -> a safe-to-share markdown report of the whole batch.
format_batch_audit <- function(b) {
  L <- c(); add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  g <- b$feature_gaps
  add("# Bulk statement audit (safe to share - no PII)\n")
  add(sprintf("**%d statements.** Status: %s.", g$total,
      paste(sprintf("%s=%s", names(g$by_status), g$by_status), collapse = ", ")))
  add(sprintf("Kinds: %s.", paste(sprintf("%s=%s", names(g$by_kind), g$by_kind), collapse = ", ")))
  add(sprintf("Scanned (OCR): %d &middot; with redactions: %d &middot; multi-account: %d &middot; multi-period: %d",
      g$scanned, g$with_redactions, g$multi_account, g$multi_period))
  add("\n## What's already covered")
  add(sprintf("- amount styles seen: %s", paste(sprintf("%s(%s)", names(g$amount_styles), g$amount_styles), collapse = ", ")))
  add(sprintf("- date formats seen: %s", paste(sprintf("%s(%s)", names(g$date_formats), g$date_formats), collapse = ", ")))
  add(sprintf("- banks matched: %s", paste(sprintf("%s(%s)", names(g$banks), g$banks), collapse = ", ")))
  # Say the form count out loud: silence here made an already-covered IRD document read as a gap.
  add(sprintf("- form (labelled-value) documents covered by a fields template: %d", g$forms %||% 0L))
  # Say the report count too - a report counted as a gap sent an analyst off to build a bank template.
  add(sprintf("- report (many-table) documents covered by a document template: %d, holding %d table(s)",
      g$reports %||% 0L, g$report_tables %||% 0L))
  add(sprintf("\n## The gaps - %d unsupported/failed across %d distinct layouts (forms excluded - they are covered, and so are reports)",
      g$unsupported, g$distinct_gap_layouts))
  if (nrow(b$clusters)) {
    add("```")
    add(paste(capture.output(print(b$clusters[, c("count", "kind", "layout_hint", "signature")], row.names = FALSE)), collapse = "\n"))
    add("```")
  }
  if (length(b$recommendations)) {
    add(sprintf("\n## Recommended templates (%d) - editable drafts for the biggest gaps", length(b$recommendations)))
    for (r in b$recommendations) {
      add(sprintf("\n### %d file(s), %s layout - draft id `%s`", r$count, r$kind, r$draft_id %||% "?"))
      add("```yaml"); add(r$draft_yaml); add("```")
    }
  }
  paste(unlist(L), collapse = "\n")
}

# ONE TEMPLATE, MANY EXAMPLES: does the rest of it still read? batch_audit() asks which template fits
# each file; this asks the opposite, so the template is FORCED and whether the fingerprint would also
# have picked it is its own column rather than deciding anything. READ-ONLY. A table the copy does not
# carry is its own row, rows 0: absent and empty are different facts, and where a measure does not
# apply it is NA, never 0. progress(i, n, file) matches convert_batch()'s callback exactly.

# The empty grid, with the column types a full one has: a shape that changes with the row count is
# not a shape.
.tc_grid_proto <- function() data.frame(
  file = character(0), detected = logical(0), item = character(0),
  name = character(0), pages = character(0), rows = integer(0),
  found_by = character(0), confidence = numeric(0),
  unclaimed_words = integer(0), note = character(0), stringsAsFactors = FALSE)

# One grid row, every column present.
.tc_row <- function(file, detected, item, name, pages, rows, found_by,
                    confidence, unclaimed_words, note) {
  data.frame(file = as.character(file)[1], detected = isTRUE(detected),
             item = as.character(item)[1], name = as.character(name)[1],
             pages = as.character(pages)[1], rows = as.integer(rows)[1],
             found_by = as.character(found_by)[1],
             confidence = as.numeric(confidence)[1],
             unclaimed_words = as.integer(unclaimed_words)[1],
             note = as.character(note)[1], stringsAsFactors = FALSE)
}

# The grid rows one example produces for a mode:document template. document_summary() already
# answers the whole question; this only puts the columns in the grid's order.
.tc_document <- function(file, detected, input, tmpl) {
  ext <- safe(extract_document(input, tmpl), NULL)
  if (is.null(ext))
    return(.tc_row(file, detected, NA_character_, NA_character_, "", 0L,
                   "this file could not be read", NA_real_, NA_integer_, ""))
  s <- ext$summary
  if (is.null(s) || !nrow(s))
    return(.tc_row(file, detected, NA_character_, NA_character_, "", 0L,
                   "this template has no tables on it", NA_real_, NA_integer_, ""))
  data.frame(file = rep(as.character(file)[1], nrow(s)),
             detected = rep(isTRUE(detected), nrow(s)),
             item = as.character(s$table), name = as.character(s$name),
             pages = as.character(s$pages), rows = as.integer(s$rows),
             found_by = as.character(s$found_by),
             confidence = as.numeric(s$confidence),
             unclaimed_words = as.integer(s$unclaimed_words),
             note = as.character(s$note), stringsAsFactors = FALSE)
}

# The same grid for a mode:fields template. A form has no columns for a word to fall outside of and
# nothing to be confident about, so those two measures are NA rather than 0.
.tc_fields <- function(file, detected, input, tmpl, dict) {
  f <- safe(extract_fields(input, tmpl, dict), NULL)
  if (is.null(f) || !nrow(f))
    return(.tc_row(file, detected, NA_character_, NA_character_, "", 0L,
                   "this file could not be read", NA_real_, NA_integer_, ""))
  got <- !is.na(f$value) & nzchar(as.character(f$value))
  note <- ifelse(f$conflict %in% TRUE,
                 "this label appears more than once with different values",
                 ifelse(f$flagged %in% TRUE, "this one is required and was not found", ""))
  data.frame(file = rep(as.character(file)[1], nrow(f)),
             detected = rep(isTRUE(detected), nrow(f)),
             item = as.character(f$field), name = as.character(f$label),
             pages = as.character(f$page), rows = as.integer(got),
             found_by = ifelse(got, as.character(f$found_by), "not on this document"),
             confidence = rep(NA_real_, nrow(f)),
             unclaimed_words = rep(NA_integer_, nrow(f)),
             note = note, stringsAsFactors = FALSE)
}

template_check <- function(tmpl, paths, progress = NULL, dict = NULL) {
  paths <- as.character(paths %||% character(0))
  kind <- safe(template_kind(tmpl), "statement")
  empty <- .tc_grid_proto()
  # A bank statement is checked by its own arithmetic, which is a different tool. Refused out loud
  # rather than answered with an empty grid.
  if (!kind %in% c("document", "fields"))
    return(list(grid = empty, message = status_message("needs_review",
      "this is a bank statement template, and a folder of statements is checked by converting them",
      "run the folder through a case folder instead - the balances are what prove a statement read")))
  if (!length(paths))
    return(list(grid = empty, message = status_message("needs_review",
      "no example documents were given", "point this at the folder the examples are kept in")))
  dict <- dict %||% safe(default_label_dict(), list())
  n <- length(paths)
  parts <- vector("list", n)
  for (i in seq_len(n)) {
    if (is.function(progress)) safe(progress(i, n, paths[i]))
    input <- safe(read_input(paths[i]), NULL)
    if (is.null(input)) {
      parts[[i]] <- .tc_row(paths[i], FALSE, NA_character_, NA_character_, "", 0L,
                            "this file could not be read", NA_real_, NA_integer_, "")
      next
    }
    det <- if (identical(kind, "document"))
      safe(detect_document_template(input, stats::setNames(list(tmpl), tmpl$id %||% "t")),
           list(matched = FALSE))
    else safe(detect_form(input, stats::setNames(list(tmpl), tmpl$id %||% "t")),
              list(matched = FALSE))
    parts[[i]] <- if (identical(kind, "document"))
      .tc_document(paths[i], isTRUE(det$matched), input, tmpl)
    else .tc_fields(paths[i], isTRUE(det$matched), input, tmpl, dict)
  }
  grid <- do.call(rbind, parts); rownames(grid) <- NULL

  # What an admin wants after editing a forty-table template is whether anything STOPPED reading, so
  # the sentence names the count of tables that came out with nothing on every single example.
  n_files <- length(unique(grid$file))
  items <- unique(grid$item[!is.na(grid$item)])
  dead <- items[vapply(items, function(k)
    all(grid$rows[grid$item %in% k] < 1L), logical(1))]
  spill <- sum(grid$unclaimed_words, na.rm = TRUE)
  missed <- sum(!grid$detected[!duplicated(grid$file)])
  thing <- if (identical(kind, "document")) "table" else "value"
  msg <- if (length(dead))
    status_message("needs_review",
      sprintf("%d of the %d %ss on this template read nothing on any of the %d examples: %s",
              length(dead), length(items), thing, n_files,
              paste(unique(grid$name[grid$item %in% dead]), collapse = ", ")),
      "check those against a document that does carry them - the rest are unaffected")
  else if (spill > 0)
    status_message("needs_review",
      sprintf("every %s read on all %d examples, but %d word(s) fell outside every column",
              thing, n_files, spill),
      "the column edges do not fit one of these documents - the grid says which")
  else if (missed > 0)
    status_message("needs_review",
      sprintf("every %s read on all %d examples, but this template's identifying phrases were not all found on %d of them",
              thing, n_files, missed),
      "those documents read only because you named the template - shorten the identifying phrases so it is picked by itself")
  else status_message("ok", sprintf("every %s on this template read on all %d examples",
                                    thing, n_files))
  list(grid = grid, message = msg)
}
