# row_coverage.R -- a PII-SAFE diagnostic that explains why a PDF statement lost
# ROWS, WITHOUT anyone having to see the statement. It reports only shapes and
# counts: each page's size vs the template's reference size (the page-scale that
# used to silently drop rows), how many visual rows were kept, and how many were
# skipped bucketed by REASON (unreadable date, missing amount, summary line,
# continuation, heading). No dates, descriptions or amounts leave the machine.

# .rowcov_bucket(reason) -- collapse a per-row skip reason into a safe category.
.rowcov_bucket <- function(reason) {
  if (is.null(reason) || !nzchar(reason)) return("kept")
  if (grepl("didn't parse", reason)) return("date_unreadable")
  if (grepl("no amount", reason))    return("amount_missing")
  if (grepl("summary", reason))      return("summary_line")
  if (grepl("continuation", reason)) return("continuation")
  "heading_or_note"
}

.ROWCOV_LEVELS <- c("kept", "date_unreadable", "amount_missing", "summary_line",
                    "continuation", "heading_or_note")

# row_coverage(input, template) -> list. Safe to share.
row_coverage <- function(input, template) {
  if (!identical(template$format %||% "delimited", "pdf"))
    return(list(applicable = FALSE, reason = "row coverage is for PDF templates"))
  t <- template$table %||% list()
  # Share the parser's page-geometry constants (R/parse_pdf_table.R) so this
  # diagnostic can never report a different page-scale verdict than the reader used.
  ref_w <- suppressWarnings(as.numeric(t$ref_width  %||% .A4_W)); if (is.na(ref_w) || ref_w <= 0) ref_w <- .A4_W
  ref_h <- suppressWarnings(as.numeric(t$ref_height %||% .A4_H)); if (is.na(ref_h) || ref_h <= 0) ref_h <- .A4_H
  wbp <- input$words %||% list()
  pw  <- input$page_width  %||% rep(NA_real_, length(wbp))
  ph  <- input$page_height %||% rep(NA_real_, length(wbp))
  ocr <- input$page_ocr    %||% rep(NA, length(wbp))
  lay <- tryCatch(inspect_pdf_layout(input, template), error = function(e) NULL)
  if (is.null(lay)) return(list(applicable = FALSE, reason = "could not read the page geometry"))

  pages <- lapply(seq_along(lay$pages), function(i) {
    P <- lay$pages[[i]]; rows <- P$rows
    buckets <- if (is.null(rows) || !nrow(rows)) character(0)
               else vapply(rows$reason %||% rep("", nrow(rows)), .rowcov_bucket, character(1))
    tab <- table(factor(buckets, levels = .ROWCOV_LEVELS))
    sx <- if (is.finite(pw[i]) && pw[i] > 0) pw[i] / ref_w else 1
    sy <- if (is.finite(ph[i]) && ph[i] > 0) ph[i] / ref_h else 1
    kept <- as.integer(tab[["kept"]])
    actionable <- as.integer(tab[["date_unreadable"]] + tab[["amount_missing"]])
    denom <- kept + actionable
    list(page = i,
         width = if (is.finite(pw[i])) round(pw[i]) else NA_integer_,
         height = if (is.finite(ph[i])) round(ph[i]) else NA_integer_,
         scaled = isTRUE(abs(sx - 1) >= .PAGE_SCALE_SNAP || abs(sy - 1) >= .PAGE_SCALE_SNAP),
         scale_x = round(sx, 3), scale_y = round(sy, 3),
         ocr = isTRUE(ocr[i]), n_words = nrow(P$words),
         kept = kept, actionable_skips = actionable,
         yield = if (denom > 0) round(kept / denom, 3) else NA_real_,
         by_reason = stats::setNames(as.integer(tab), .ROWCOV_LEVELS))
  })

  kept_tot <- sum(vapply(pages, function(p) p$kept, integer(1)))
  act_tot  <- sum(vapply(pages, function(p) p$actionable_skips, integer(1)))
  any_scaled <- any(vapply(pages, function(p) isTRUE(p$scaled), logical(1)))
  any_ocr <- any(vapply(pages, function(p) isTRUE(p$ocr), logical(1)))
  empty_pages <- which(vapply(pages, function(p) p$kept == 0 && p$n_words > 30, logical(1)))

  diag <- if (length(empty_pages) && any_scaled)
      sprintf("Page(s) %s are a different physical size than the template and kept no rows -- a page-scale mismatch. The parser normalises this automatically; if it persists the template's reference page size is likely wrong.",
              paste(empty_pages, collapse = ", "))
    else if (length(empty_pages))
      sprintf("Page(s) %s carry words but kept no rows -- either the column bands don't line up on this layout, or those pages hold no transactions.",
              paste(empty_pages, collapse = ", "))
    else if (act_tot > 0)
      sprintf("%d row(s) were skipped for an unreadable date or a missing amount%s -- usually OCR quality on a scan, or a band that is slightly off.",
              act_tot, if (any_ocr) " (this document was machine-read / OCR'd)" else "")
    else "Every candidate row was kept."

  list(applicable = TRUE, ref_width = round(ref_w, 2), ref_height = round(ref_h, 2),
       page_count = length(pages), kept_total = kept_tot, actionable_skips_total = act_tot,
       any_page_rescaled = any_scaled, any_ocr = any_ocr, empty_pages = empty_pages,
       diagnosis = diag, pages = pages)
}

# format_row_coverage(cov) -> markdown, safe to share (no PII).
format_row_coverage <- function(cov) {
  if (!isTRUE(cov$applicable)) return(paste0("Row coverage not available: ", cov$reason %||% "n/a"))
  L <- c(); add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  add("# Statement row-coverage diagnostic (safe to share - no statement contents)\n")
  add(sprintf("Template reference page: **%g x %g pt**. Pages: **%d**. Rows kept: **%d**. Rows skipped as unreadable-date / missing-amount: **%d**.",
      cov$ref_width, cov$ref_height, cov$page_count, cov$kept_total, cov$actionable_skips_total))
  add(sprintf("\n**Diagnosis:** %s\n", cov$diagnosis))
  add("| page | size (pt) | rescaled? | OCR? | words | kept | skipped(date/amt) | yield |")
  add("|---|---|---|---|---|---|---|---|")
  for (p in cov$pages)
    add(sprintf("| %d | %sx%s | %s | %s | %d | %d | %d | %s |",
        p$page, p$width, p$height,
        if (p$scaled) sprintf("yes (%.2fx / %.2fy)", p$scale_x, p$scale_y) else "no",
        if (p$ocr) "yes" else "no", p$n_words, p$kept, p$actionable_skips,
        if (is.na(p$yield)) "-" else sprintf("%.0f%%", 100 * p$yield)))
  add("\n_yield = kept / (kept + rows that looked like transactions but were skipped). A low yield on a page, or a page that rescaled and kept nothing, is where rows go missing._")
  paste(unlist(L), collapse = "\n")
}
