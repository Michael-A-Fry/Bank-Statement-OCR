# audit.R -- a SAFE-TO-SHARE structural audit of a statement, for improving the
# reader without ever leaking PII. Every piece of real text is
# MASKED to its shape only: letters -> x/X (by case), digits -> 9, punctuation and
# spaces kept. So "Coffee Shop 12" -> "Xxxxxx Xxxx 99", "1,234.56" -> "9,999.99",
# "17 Sep" -> "99 Xxx". No merchant names, no amounts, no account numbers, no dates
# survive -- only the LAYOUT, FORMATS and POSITIONS the reader worked from, and
# what it made of them (its outcome and the NAMES of the checks that failed --
# never its sentences, which can quote a line of the statement).

# mask_text(x) -- shape-only mask. Unicode-aware (accented letters are masked too,
# via \p{}), so NOTHING real survives.
mask_text <- function(x) {
  x <- as.character(x)
  out <- gsub("\\p{Ll}", "x", x, perl = TRUE)              # lowercase letter -> x
  out <- gsub("\\p{Lu}", "X", out, perl = TRUE)            # uppercase letter -> X
  out <- gsub("\\p{Lt}|\\p{Lo}|\\p{Lm}", "X", out, perl = TRUE)  # any other letter -> X
  out <- gsub("\\p{N}", "9", out, perl = TRUE)             # any number -> 9
  out[is.na(x)] <- NA_character_
  out
}

# .audit_rows(input, tmpl, max_rows) -- every visual row group in a PDF table,
# with its date/amount/description cells MASKED, so a reviewer can see the shape
# of each row (e.g. a "[REDACTED]" date, a two-date cell "99 Xxx 99 Xxx", a blank
# amount) and why rows near the top might drop.
#
# It reads the page the way the READER does -- the shared .group_rows() and
# .pdf_cell() from R/parse_pdf_table.R -- and stops there: it shows every visual
# row, applying none of the reader's keep/stitch/continuation decisions. That is
# the cross-check (what was on the page vs what came out), and it only works if
# both agree on where the lines ARE. This used to carry its own copies of both:
# the grouper was the pairwise-gap version (cumsum(diff(y) > tol)) that
# .group_rows replaced precisely because it merges a block of tightly-set lines
# into one giant row -- so on the dense statements a reviewer opens this report to
# understand, it showed three rows for two hundred and quietly blamed the reader.
.audit_rows <- function(input, tmpl, max_rows = 40L) {
  t <- tmpl$table %||% list(); cols <- t$columns %||% list()
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% PARAM_PDF_ROW_TOL)); if (is.na(row_tol)) row_tol <- PARAM_PDF_ROW_TOL
  rows <- list()
  for (p in seq_along(input$words %||% list())) {
    w <- input$words[[p]]; if (is.null(w) || !nrow(w)) next
    w <- as.data.frame(w, stringsAsFactors = FALSE); w <- w[order(w$y, w$x), , drop = FALSE]
    grp <- .group_rows(w$y, row_tol)
    for (g in unique(grp)) {
      rw <- w[grp == g, , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(page = p, y = round(min(rw$y)),
        date = mask_text(.pdf_cell(rw, cols$date)),
        amount = mask_text(.pdf_cell(rw, cols$amount %||% cols$debit)),
        credit = mask_text(.pdf_cell(rw, cols$credit)),
        balance = mask_text(.pdf_cell(rw, cols$balance)),
        description = substr(mask_text(.pdf_cell(rw, cols$description)), 1, 40),
        stringsAsFactors = FALSE)
      if (length(rows) >= max_rows) break
    }
    if (length(rows) >= max_rows) break
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

# statement_audit(path, layouts_dir) -> list: the statement read exactly as a
# conversion reads it (bank_identify, that bank's learned layouts, auto_read) but
# with no side effects -- no outputs, no run log, nothing learned -- and every
# value masked to its shape.
statement_audit <- function(path, layouts_dir = NULL) {
  input <- safe(read_input(path), NULL)
  if (is.null(input)) return(list(error = "could not read the file"))
  meta <- safe(extract_metadata(input), list())
  ident <- bank_identify(input)
  bank <- bank_pick(ident, NULL)$bank
  layouts <- if (!is.null(layouts_dir) && !is.na(bank)) layouts_load(layouts_dir, bank) else list()
  rd <- auto_read(input, layouts, bank)
  tmpl <- rd$template
  parsed <- rd$parsed; recon <- rd$recon

  # Page-1 word layout, MASKED: positions + text shapes for the first words, so a
  # reviewer can see where the reader looked without seeing what was printed.
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
  ck <- rd$checks
  sig <- tmpl$signature %||% list()

  list(
    file_type   = tolower(tools::file_ext(path)),
    sha256_10   = substr(safe(file_sha256(path), NA_character_), 1, 10),
    format      = input$kind %||% "?",
    pages       = input$meta$page_count %||% NA_integer_,
    max_page_pt = round(meta$max_page_pt %||% NA_real_),
    ocr_pages   = input$meta$ocr_pages %||% 0L,
    ocr_min_confidence = input$meta$ocr_min_conf %||% NA_real_,
    bank        = list(institution = ident$institution %||% NA_character_,
                       confidence = ident$confidence %||% "unknown",
                       n_periods = meta$n_periods %||% NA, n_accounts = meta$n_accounts %||% NA),
    reading     = list(outcome = rd$outcome %||% "unread", proof = rd$proof$kind %||% "none",
                       layout = rd$matched_layout %||% NA_character_,
                       roles = paste(unlist(sig$roles), collapse = " | "),
                       checks_failed = if (is.data.frame(ck)) ck$check[ck$ok %in% FALSE] else character(0),
                       candidates = if (is.data.frame(rd$candidates)) rd$candidates$source else character(0)),
    period_shape = list(start = mask_text(meta$period_start), end = mask_text(meta$period_end)),
    date_format  = sig$date_format %||% NA_character_,
    amount_sign  = sig$money_style %||% NA_character_,
    row_count    = if (!is.null(parsed)) nrow(parsed$transactions) else 0L,
    flags_summary = if (!is.null(parsed)) {
      fl <- unlist(strsplit(paste(parsed$transactions$flags, collapse = ","), ","))
      fl <- fl[nzchar(fl)]; if (length(fl)) as.list(table(fl)) else list()
    } else list(),
    kpis         = if (!is.null(recon)) recon$kpis[, c("name", "status")] else NULL,
    trust        = if (!is.null(recon)) list(level = recon$trust$level) else NULL,
    rows_masked  = if (!is.null(tmpl) && identical(tmpl$format, "pdf")) .audit_rows(input, tmpl) else NULL,
    words_masked = wl
  )
}

# format_audit(a) -> a readable, safe-to-share markdown report.
format_audit <- function(a) {
  if (!is.null(a$error)) return(paste("Audit failed:", a$error))
  L <- c()
  add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  add("# Statement audit (safe to share - no PII, shapes only)\n")
  add("_Every value is masked to its shape: letters -> x/X, digits -> 9. No merchant names, amounts, account numbers or dates are included._\n")
  add(sprintf("- file: %s (sha %s)", a$file_type, a$sha256_10 %||% "?"))
  add(sprintf("- format: %s, pages: %s, max page: %s pt", a$format, a$pages, a$max_page_pt))
  add(sprintf("- OCR: %s page(s), min confidence %s", a$ocr_pages,
              if (is.na(a$ocr_min_confidence)) "n/a" else sprintf("%.0f%%", a$ocr_min_confidence)))
  add(sprintf("- bank: %s (%s confidence)", a$bank$institution %||% "not identified", a$bank$confidence))
  add(sprintf("- periods seen: %s, accounts seen: %s", a$bank$n_periods, a$bank$n_accounts))
  add(sprintf("- reading: %s (proof: %s; layout: %s)", a$reading$outcome, a$reading$proof, a$reading$layout))
  add(sprintf("- column roles: %s", if (nzchar(a$reading$roles)) a$reading$roles else "none found"))
  if (length(a$reading$checks_failed))
    add(sprintf("- checks failed: %s", paste(a$reading$checks_failed, collapse = ", ")))
  add(sprintf("- period shape: %s .. %s", a$period_shape$start %||% "NA", a$period_shape$end %||% "NA"))
  add(sprintf("- date format: %s, amount style: %s", a$date_format %||% "NA", a$amount_sign %||% "NA"))
  add(sprintf("- rows parsed: %d", a$row_count))
  if (length(a$flags_summary))
    add(sprintf("- flags: %s", paste(sprintf("%s=%s", names(a$flags_summary), a$flags_summary), collapse = ", ")))
  if (!is.null(a$kpis)) {
    add("\n## KPI statuses (no values)")
    for (i in seq_len(nrow(a$kpis))) add(sprintf("- %s: %s", a$kpis$name[i], a$kpis$status[i]))
  }
  if (!is.null(a$trust)) add(sprintf("\n- trust: %s", a$trust$level))
  if (!is.null(a$rows_masked) && nrow(a$rows_masked)) {
    add("\n## Row shapes (first rows; masked) - spot dropped/odd rows here")
    add("```")
    add(paste(capture.output(print(a$rows_masked, row.names = FALSE)), collapse = "\n"))
    add("```")
  }
  if (!is.null(a$words_masked) && nrow(a$words_masked)) {
    add("\n## Page-1 word layout (masked)")
    add("```")
    add(paste(capture.output(print(a$words_masked, row.names = FALSE)), collapse = "\n"))
    add("```")
  }
  paste(unlist(L), collapse = "\n")
}

# There is no write_statement_audit(). There was one, and nothing ever called it --
# not the app, not scripts/audit-statement.R, not a test. Both real callers compose
# the two halves themselves, format_audit(statement_audit(path)), because both hand
# the text somewhere other than a file on the server: Admin streams it through a
# download handler, and the script writes where the maintainer said. A wrapper that
# picks a filename for you is only useful to a caller that does not exist.
