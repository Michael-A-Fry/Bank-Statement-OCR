# batch_audit.R -- review MANY statements at once (a whole folder of 250+ PDFs of
# every bank/variant) and produce a SINGLE, PII-safe picture: what parses, what
# doesn't, the unsupported layouts CLUSTERED, N recommended DRAFT templates
# (editable) for the biggest gaps, and a feature-gap summary telling you what the
# wizard/engine would need to cover the rest. No PII: only shapes, counts,
# institution names (from a matched template), and layout hashes.

# .kind_of(input) -- a coarse, safe descriptor of the file kind.
.kind_of <- function(input) {
  if (identical(input$kind, "pdf"))
    return(if ((input$meta$ocr_pages %||% 0L) > 0L) "pdf-scanned" else "pdf-text")
  input$kind %||% "?"
}

# batch_audit(paths, templates) -> list(per_file, clusters, recommendations,
# feature_gaps). Safe to share.
batch_audit <- function(paths, templates = NULL, max_recommendations = 8L) {
  root <- Sys.getenv("ENGINE_ROOT", ".")
  if (is.null(templates))
    templates <- safe(load_template_set(file.path(root, "templates"),
                                        file.path(root, "templates_user")), list())
  paths <- as.character(paths)
  rows <- vector("list", length(paths))
  rep_path <- list()   # signature -> a representative path (for drafting)

  for (i in seq_along(paths)) {
    p <- paths[i]
    input <- safe(read_input(p), NULL)
    if (is.null(input)) {
      rows[[i]] <- data.frame(idx = i, file_type = tolower(tools::file_ext(p)), kind = "unreadable",
        pages = NA_integer_, detected = FALSE, template = NA_character_, bank = NA_character_,
        status = "unreadable", n_rows = 0L, n_periods = NA, n_accounts = NA, redacted = 0L,
        amount_style = NA_character_, date_format = NA_character_, trust = NA_character_,
        signature = NA_character_, layout_hint = "", stringsAsFactors = FALSE)
      next
    }
    meta <- safe(extract_metadata(input), list())
    det  <- safe(detect_statement(input, templates), list(matched = FALSE))
    lsig <- safe(layout_signature(input), list(signature = NA_character_, hint = ""))
    tmpl <- if (isTRUE(det$matched)) templates[[det$template_id]] else NULL
    parsed <- if (!is.null(tmpl)) safe(parse_statement(input, tmpl), NULL) else NULL
    recon  <- if (!is.null(parsed)) safe(reconcile(parsed, tmpl), NULL) else NULL
    status <- if (is.null(tmpl)) "unsupported"
      else if (is.null(parsed)) "failed"
      else if (!is.null(recon) && identical(recon$trust$level, "low")) "needs_review"
      else "ok"
    sig <- lsig$signature %||% NA_character_
    if (!is.null(sig) && !is.na(sig) && is.null(rep_path[[sig]])) rep_path[[sig]] <- p
    # Amount style / date format / bank: from the matched template when there IS
    # one, otherwise DETECT them from the file itself (a draft) -- so a batch of
    # mostly-unsupported statements still shows what styles/formats are present,
    # which is the whole point of the audit (they were blank before).
    if (!is.null(tmpl)) {
      amt_style <- tmpl$table$amount_sign %||% tmpl$amount_sign %||% NA_character_
      dt_fmt    <- tmpl$table$date_format %||% tmpl$columns$date$format %||% NA_character_
      bank_v    <- tmpl$bank %||% NA_character_
    } else {
      dr <- safe(draft_template(p), NULL)
      amt_style <- if (is.null(dr)) NA_character_ else (dr$table$amount_sign %||% dr$amount_sign %||% NA_character_)
      dt_fmt    <- if (is.null(dr)) NA_character_ else (dr$table$date_format %||% dr$columns$date$format %||% NA_character_)
      bank_v    <- NA_character_    # bank can't be inferred reliably from an unmatched file
    }
    rows[[i]] <- data.frame(idx = i, file_type = tolower(tools::file_ext(p)),
      kind = .kind_of(input), pages = input$meta$page_count %||% NA_integer_,
      detected = isTRUE(det$matched), template = det$template_id %||% NA_character_,
      bank = bank_v, status = status,
      n_rows = if (!is.null(parsed)) nrow(parsed$transactions) else 0L,
      n_periods = meta$n_periods %||% NA, n_accounts = meta$n_accounts %||% NA,
      redacted = sum(input$meta$redactions$redacted_words %||% 0L),
      amount_style = amt_style, date_format = dt_fmt,
      trust = if (!is.null(recon)) recon$trust$level else NA_character_,
      signature = sig, layout_hint = lsig$hint %||% "", stringsAsFactors = FALSE)
  }
  per <- do.call(rbind, rows)

  # cluster the unsupported/failed by layout signature -> the biggest gaps first
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
      # recommend a draft template for each of the top clusters
      top <- utils::head(clusters, max_recommendations)
      for (k in seq_len(nrow(top))) {
        p <- rep_path[[top$signature[k]]]
        t <- safe(draft_template(p, bank = "NewBank"), NULL)
        recs[[length(recs) + 1L]] <- list(
          signature = top$signature[k], count = top$count[k], kind = top$kind[k],
          draft_id = t$id %||% NA_character_,
          draft_yaml = if (is.null(t)) "(could not auto-draft — build in the wizard)"
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
    unsupported = nrow(uns), distinct_gap_layouts = nrow(clusters))

  list(per_file = per, clusters = clusters, recommendations = recs, feature_gaps = feature_gaps)
}

# format_batch_audit(b) -> a safe-to-share markdown report of the whole batch.
format_batch_audit <- function(b) {
  L <- c(); add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  g <- b$feature_gaps
  add("# Bulk statement audit (safe to share — no PII)\n")
  add(sprintf("**%d statements.** Status: %s.", g$total,
      paste(sprintf("%s=%s", names(g$by_status), g$by_status), collapse = ", ")))
  add(sprintf("Kinds: %s.", paste(sprintf("%s=%s", names(g$by_kind), g$by_kind), collapse = ", ")))
  add(sprintf("Scanned (OCR): %d &middot; with redactions: %d &middot; multi-account: %d &middot; multi-period: %d",
      g$scanned, g$with_redactions, g$multi_account, g$multi_period))
  add("\n## What's already covered")
  add(sprintf("- amount styles seen: %s", paste(sprintf("%s(%s)", names(g$amount_styles), g$amount_styles), collapse = ", ")))
  add(sprintf("- date formats seen: %s", paste(sprintf("%s(%s)", names(g$date_formats), g$date_formats), collapse = ", ")))
  add(sprintf("- banks matched: %s", paste(sprintf("%s(%s)", names(g$banks), g$banks), collapse = ", ")))
  add(sprintf("\n## The gaps — %d unsupported/failed across %d distinct layouts", g$unsupported, g$distinct_gap_layouts))
  if (nrow(b$clusters)) {
    add("```")
    add(paste(capture.output(print(b$clusters[, c("count", "kind", "layout_hint", "signature")], row.names = FALSE)), collapse = "\n"))
    add("```")
  }
  if (length(b$recommendations)) {
    add(sprintf("\n## Recommended templates (%d) — editable drafts for the biggest gaps", length(b$recommendations)))
    for (r in b$recommendations) {
      add(sprintf("\n### %d file(s), %s layout — draft id `%s`", r$count, r$kind, r$draft_id %||% "?"))
      add("```yaml"); add(r$draft_yaml); add("```")
    }
  }
  paste(unlist(L), collapse = "\n")
}
