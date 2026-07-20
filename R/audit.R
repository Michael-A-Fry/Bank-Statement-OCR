# audit.R -- a SAFE-TO-SHARE structural audit of a statement, for improving
# templates and the engine without ever leaking PII. Every piece of real text is
# MASKED to its shape only: letters -> x/X (by case), digits -> 9, punctuation and
# spaces kept. So "Coffee Shop 12" -> "Xxxxxx Xxxx 99", "1,234.56" -> "9,999.99",
# "17 Sep" -> "99 Xxx". No merchant names, no amounts, no account numbers, no dates
# survive -- only the LAYOUT, FORMATS and POSITIONS a template needs.

# mask_text(x) -- shape-only mask. Unicode-aware (accented letters are masked
# too, via \p{}), so NOTHING real survives. Preserves the [REDACTED] token.
mask_text <- function(x) {
  x <- as.character(x)
  red <- !is.na(x) & grepl("REDACT", toupper(x))
  out <- gsub("\\p{Ll}", "x", x, perl = TRUE)              # lowercase letter -> x
  out <- gsub("\\p{Lu}", "X", out, perl = TRUE)            # uppercase letter -> X
  out <- gsub("\\p{Lt}|\\p{Lo}|\\p{Lm}", "X", out, perl = TRUE)  # any other letter -> X
  out <- gsub("\\p{N}", "9", out, perl = TRUE)             # any number -> 9
  out[red] <- "[REDACTED]"
  out[is.na(x)] <- NA_character_
  out
}

# .audit_rows(input, tmpl, max_rows) -- every visual row group in a PDF table,
# with its date/amount/description cells MASKED, so a reviewer can see the shape
# of each row (e.g. a "[REDACTED]" date, a two-date cell "99 Xxx 99 Xxx", a blank
# amount) and why rows near the top might drop. Independent of the parser on
# purpose -- a cross-check view.
.audit_rows <- function(input, tmpl, max_rows = 40L) {
  t <- tmpl$table %||% list(); cols <- t$columns %||% list()
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% 3)); if (is.na(row_tol)) row_tol <- 3
  cell <- function(rw, cspec) {
    if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_character_)
    cx <- rw$x + rw$width / 2; sel <- rw[cx >= cspec$x_min & cx <= cspec$x_max, , drop = FALSE]
    if (!nrow(sel)) NA_character_ else paste(sel$text[order(sel$x)], collapse = " ")
  }
  rows <- list()
  for (p in seq_along(input$words %||% list())) {
    w <- input$words[[p]]; if (is.null(w) || !nrow(w)) next
    w <- as.data.frame(w, stringsAsFactors = FALSE); w <- w[order(w$y, w$x), , drop = FALSE]
    grp <- cumsum(c(TRUE, diff(w$y) > row_tol))
    for (g in unique(grp)) {
      rw <- w[grp == g, , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(page = p, y = round(min(rw$y)),
        date = mask_text(cell(rw, cols$date)),
        amount = mask_text(cell(rw, cols$amount %||% cols$debit)),
        credit = mask_text(cell(rw, cols$credit)),
        balance = mask_text(cell(rw, cols$balance)),
        description = substr(mask_text(cell(rw, cols$description)), 1, 40),
        stringsAsFactors = FALSE)
      if (length(rows) >= max_rows) break
    }
    if (length(rows) >= max_rows) break
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

# statement_audit(path, templates) -> a structured, PII-free audit list.
statement_audit <- function(path, templates = NULL, redaction_rects = NULL) {
  root <- Sys.getenv("ENGINE_ROOT", ".")
  if (is.null(templates))
    templates <- safe(load_template_set(file.path(root, "templates"),
                                        file.path(root, "templates_user")), list())
  input <- safe(read_input(path, redaction_rects = redaction_rects), NULL)
  if (is.null(input)) return(list(error = "could not read the file"))
  meta <- safe(extract_metadata(input), list())
  det  <- safe(detect_statement(input, templates), list(matched = FALSE))
  tmpl <- if (isTRUE(det$matched)) templates[[det$template_id]] else NULL
  parsed <- if (!is.null(tmpl)) safe(parse_statement(input, tmpl), NULL) else NULL
  recon  <- if (!is.null(parsed)) safe(reconcile(parsed, tmpl), NULL) else NULL

  # redaction map: counts + positions only (no text)
  red <- input$meta$redactions
  red_total <- if (!is.null(red)) sum(red$redacted_words) else 0L

  # word layout sample (page 1), masked -- for building a template from scratch
  wl <- NULL
  wbp <- input$words %||% list()
  w1 <- if (length(wbp)) wbp[[1]] else NULL
  if (!is.null(w1) && nrow(w1)) {
    w1 <- as.data.frame(w1, stringsAsFactors = FALSE)
    w1 <- w1[order(w1$y, w1$x), , drop = FALSE]
    k <- min(nrow(w1), 150L)
    wl <- data.frame(x = round(w1$x[seq_len(k)]), y = round(w1$y[seq_len(k)]),
                     w = round(w1$width[seq_len(k)]), text = mask_text(w1$text[seq_len(k)]),
                     stringsAsFactors = FALSE)
  }

  list(
    file_type   = tolower(tools::file_ext(path)),
    sha256_10   = substr(safe(file_sha256(path), NA_character_), 1, 10),
    format      = tmpl$format %||% (if (identical(input$kind, "pdf")) "pdf" else input$meta$ext %||% "?"),
    pages       = input$meta$page_count %||% NA_integer_,
    max_page_pt = round(meta$max_page_pt %||% NA_real_),
    ocr_pages   = input$meta$ocr_pages %||% 0L,
    ocr_min_confidence = input$meta$ocr_min_conf %||% NA_real_,
    detected    = list(matched = isTRUE(det$matched),
                       template = det$template_id %||% NA_character_,
                       score = det$score %||% NA, detail = det$detail %||% NA_character_,
                       n_periods = meta$n_periods %||% NA, n_accounts = meta$n_accounts %||% NA),
    period_shape = list(start = mask_text(meta$period_start), end = mask_text(meta$period_end)),
    date_format  = tmpl$table$date_format %||% tmpl$columns$date$format %||% NA_character_,
    amount_sign  = tmpl$table$amount_sign %||% tmpl$amount_sign %||% NA_character_,
    redactions   = list(total_words = red_total,
                        per_page = if (!is.null(red)) red$redacted_words else integer(0)),
    row_count    = if (!is.null(parsed)) nrow(parsed$transactions) else 0L,
    flags_summary = if (!is.null(parsed)) {
      fl <- unlist(strsplit(paste(parsed$transactions$flags, collapse = ","), ","))
      fl <- fl[nzchar(fl)]; if (length(fl)) as.list(table(fl)) else list()
    } else list(),
    kpis         = if (!is.null(recon)) recon$kpis[, c("name", "status")] else NULL,
    trust        = if (!is.null(recon)) list(level = recon$trust$level,
                      reasons = recon$trust$reasons) else NULL,
    rows_masked  = if (!is.null(tmpl) && identical(tmpl$format, "pdf")) .audit_rows(input, tmpl) else NULL,
    words_masked = wl
  )
}

# format_audit(a) -> a readable, safe-to-share markdown report.
format_audit <- function(a) {
  if (!is.null(a$error)) return(paste("Audit failed:", a$error))
  L <- c()
  add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  add("# Statement audit (safe to share — no PII, shapes only)\n")
  add("_Every value is masked to its shape: letters -> x/X, digits -> 9. No merchant names, amounts, account numbers or dates are included._\n")
  add(sprintf("- file: %s (sha %s)", a$file_type, a$sha256_10 %||% "?"))
  add(sprintf("- format: %s, pages: %s, max page: %s pt", a$format, a$pages, a$max_page_pt))
  add(sprintf("- OCR: %s page(s), min confidence %s", a$ocr_pages,
              if (is.na(a$ocr_min_confidence)) "n/a" else sprintf("%.0f%%", a$ocr_min_confidence)))
  add(sprintf("- detected template: %s (matched=%s, score=%s)",
              a$detected$template, a$detected$matched, a$detected$score))
  add(sprintf("- periods seen: %s, accounts seen: %s", a$detected$n_periods, a$detected$n_accounts))
  add(sprintf("- period shape: %s .. %s", a$period_shape$start %||% "NA", a$period_shape$end %||% "NA"))
  add(sprintf("- date format: %s, amount style: %s", a$date_format %||% "NA", a$amount_sign %||% "NA"))
  add(sprintf("- redacted words: %d total (per page: %s)", a$redactions$total_words,
              paste(a$redactions$per_page, collapse = ", ")))
  add(sprintf("- rows parsed: %d", a$row_count))
  if (length(a$flags_summary))
    add(sprintf("- flags: %s", paste(sprintf("%s=%s", names(a$flags_summary), a$flags_summary), collapse = ", ")))
  if (!is.null(a$kpis)) {
    add("\n## KPI statuses (no values)")
    for (i in seq_len(nrow(a$kpis))) add(sprintf("- %s: %s", a$kpis$name[i], a$kpis$status[i]))
  }
  if (!is.null(a$trust)) add(sprintf("\n- trust: %s\n  - %s", a$trust$level,
      paste(a$trust$reasons, collapse = "\n  - ")))
  if (!is.null(a$rows_masked) && nrow(a$rows_masked)) {
    add("\n## Row shapes (first rows; masked) — spot dropped/odd rows here")
    add("```")
    add(paste(capture.output(print(a$rows_masked, row.names = FALSE)), collapse = "\n"))
    add("```")
  }
  if (!is.null(a$words_masked) && nrow(a$words_masked)) {
    add("\n## Page-1 word layout (masked) — for building a template")
    add("```")
    add(paste(capture.output(print(a$words_masked, row.names = FALSE)), collapse = "\n"))
    add("```")
  }
  paste(unlist(L), collapse = "\n")
}

# write_statement_audit(path, out) -> writes the markdown audit; returns the path.
write_statement_audit <- function(path, out = NULL, templates = NULL) {
  a <- statement_audit(path, templates = templates)
  if (is.null(out)) out <- paste0(tools::file_path_sans_ext(basename(path)), ".audit.md")
  writeLines(format_audit(a), out)
  invisible(out)
}
