# row_coverage.R -- a PII-SAFE diagnostic that explains why a PDF statement lost
# ROWS, WITHOUT anyone having to see the statement. It reports only shapes and
# counts: each page's size vs the template's reference size (the page-scale that
# used to silently drop rows), how many visual rows were kept, and how many were
# skipped bucketed by REASON (unreadable date, missing amount, summary line,
# continuation, heading). No dates, descriptions or amounts leave the machine.

# .rowcov_bucket(reason) -- collapse a per-row skip reason into a safe category.
# ORDER MATTERS. The heading reason ("no date and no amount - treated as a heading,
# note or wrapped line") CONTAINS the substring "no amount", so testing "no amount"
# first swallowed it into amount_missing -- every heading and note on the page then
# counted as an ACTIONABLE skip, and the diagnosis told the analyst rows had been
# lost to a missing amount when nothing was lost at all. Match the whole-reason
# cases first, from the front of the string, before the loose substring tests.
.rowcov_bucket <- function(reason) {
  if (is.null(reason) || !nzchar(reason)) return("kept")
  # A split row re-joined above is captured, not lost -- decided by the caller
  # (it needs the neighbouring row), so it is still matched on text here.
  if (grepl("^split row", reason)) return("continuation")
  if (grepl("continuation", reason)) return("continuation")
  # Everything else is one of the engine's own codes, mapped back from the
  # sentence it produced. .PDF_ROW_REASON_TEXT is the single source of both.
  code <- names(.PDF_ROW_REASON_TEXT)[match(reason, .PDF_ROW_REASON_TEXT)]
  if (is.na(code)) return("heading_or_note")
  if (identical(code, "date_unparsed")) "date_unreadable" else code
}

.ROWCOV_LEVELS <- c("kept", "date_unreadable", "amount_missing", "summary_line",
                    "continuation", "heading_or_note")

# row_coverage(input, template) -> list. Safe to share.
row_coverage <- function(input, template) {
  if (!identical(template$format %||% "delimited", "pdf"))
    return(list(applicable = FALSE, reason = "row coverage is for PDF templates"))
  t <- template$table %||% list()
  # THE BAND FRAME, resolved by the parser's own function (R/parse_pdf_table.R) so
  # this diagnostic can never report a different page-scale verdict than the reader
  # used -- the two used to keep private copies of the rule.
  frame <- pdf_band_frame(template)
  # Every band the template maps. A band that claims no words is the measurement
  # behind "the credit column has a box drawn over it but reads nothing".
  band_names <- names(Filter(function(b) !is.null(b$x_min) && !is.null(b$x_max),
                             t$columns %||% list()))
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
    # The rescaled VERDICT comes from the parser's frame maths, not a local copy.
    # The ratio reported below is page-over-frame because that is the direction a
    # person reads ("this page is 2.08x the size the bands were drawn on").
    s <- pdf_band_frame_scale(frame, pw[i], ph[i])
    sx <- if (is.finite(pw[i]) && pw[i] > 0) pw[i] / frame$width  else 1
    sy <- if (is.finite(ph[i]) && ph[i] > 0) ph[i] / frame$height else 1
    kept <- as.integer(tab[["kept"]])
    actionable <- as.integer(tab[["date_unreadable"]] + tab[["amount_missing"]])
    denom <- kept + actionable
    cn <- P$words$column
    list(page = i,
         width = if (is.finite(pw[i])) round(pw[i]) else NA_integer_,
         height = if (is.finite(ph[i])) round(ph[i]) else NA_integer_,
         scaled = !(s[1] == 1 && s[2] == 1),
         scale_x = round(sx, 3), scale_y = round(sy, 3),
         ocr = isTRUE(ocr[i]), n_words = nrow(P$words),
         kept = kept, actionable_skips = actionable,
         yield = if (denom > 0) round(kept / denom, 3) else NA_real_,
         # Words each band claimed on this page, and words inside the table region
         # that NO band claimed. Counts only, so safe to share. The pair is what
         # tells a misplaced band from a genuinely empty column: a band reading
         # nothing while nothing is unclaimed just means that column is blank on
         # this statement; a band reading nothing while words sit unclaimed means
         # the band is not where the printing is.
         band_words = stats::setNames(vapply(band_names,
           function(k) sum(!is.na(cn) & cn == k), integer(1)), band_names),
         unbanded_words = as.integer(sum(P$words$in_region & is.na(cn))),
         by_reason = stats::setNames(as.integer(tab), .ROWCOV_LEVELS))
  })

  kept_tot <- sum(vapply(pages, function(p) p$kept, integer(1)))
  act_tot  <- sum(vapply(pages, function(p) p$actionable_skips, integer(1)))
  any_scaled <- any(vapply(pages, function(p) isTRUE(p$scaled), logical(1)))
  any_ocr <- any(vapply(pages, function(p) isTRUE(p$ocr), logical(1)))
  empty_pages <- which(vapply(pages, function(p) p$kept == 0 && p$n_words > 30, logical(1)))
  # A mapped band that claimed NOTHING anywhere in the document, WHILE words inside
  # the table region went unclaimed by any band. That pair is the exact shape of the
  # reported "a box is drawn over the credit column but it reads nothing while the
  # debit beside it reads fine": the printing is there, the band is not over it.
  # Both halves are required. A band that reads nothing on a statement with nothing
  # unclaimed is simply a column with no entries this period -- reporting that would
  # send the analyst to redraw a band that is fine, which is the mistake this file
  # already had to fix once for headings.
  band_totals <- if (!length(band_names)) integer(0) else
    stats::setNames(vapply(band_names, function(k)
      sum(vapply(pages, function(p) as.integer(p$band_words[[k]]), integer(1))), integer(1)), band_names)
  unbanded_tot <- sum(vapply(pages, function(p) p$unbanded_words, integer(1)))
  empty_bands <- if (unbanded_tot > 0 && length(band_totals))
    names(band_totals)[band_totals == 0L] else character(0)

  diag <- if (length(empty_bands))
      sprintf("The %s band read no words anywhere, yet %d word(s) inside the table are in no band at all -- so the printing is there and the band is not over it. Redraw that band, or check the band frame (the page size the bands were drawn on) matches this file.",
              paste(empty_bands, collapse = " and "), unbanded_tot)
    else if (length(empty_pages) && any_scaled)
      sprintf("Page(s) %s are a different physical size than the template and kept no rows -- a page-scale mismatch. The parser normalises this automatically; if it persists the template's reference page size is likely wrong.",
              paste(empty_pages, collapse = ", "))
    else if (length(empty_pages))
      sprintf("Page(s) %s carry words but kept no rows -- either the column bands don't line up on this layout, or those pages hold no transactions.",
              paste(empty_pages, collapse = ", "))
    else if (act_tot > 0)
      sprintf("%d row(s) were skipped for an unreadable date or a missing amount%s -- usually OCR quality on a scan, or a band that is slightly off.",
              act_tot, if (any_ocr) " (this document was machine-read / OCR'd)" else "")
    else "Every candidate row was kept."

  list(applicable = TRUE, ref_width = round(frame$width, 2), ref_height = round(frame$height, 2),
       page_count = length(pages), kept_total = kept_tot, actionable_skips_total = act_tot,
       any_page_rescaled = any_scaled, any_ocr = any_ocr, empty_pages = empty_pages,
       band_words_total = band_totals, empty_bands = empty_bands,
       diagnosis = diag, pages = pages)
}

# format_row_coverage(cov) -> markdown, safe to share (no PII).
format_row_coverage <- function(cov) {
  if (!isTRUE(cov$applicable)) return(paste0("Row coverage not available: ", cov$reason %||% "n/a"))
  L <- c(); add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  add("# Statement row-coverage diagnostic (safe to share - no statement contents)\n")
  add(sprintf("Band frame (the page size the bands were drawn on): **%g x %g pt**. Pages: **%d**. Rows kept: **%d**. Rows skipped as unreadable-date / missing-amount: **%d**.",
      cov$ref_width, cov$ref_height, cov$page_count, cov$kept_total, cov$actionable_skips_total))
  add(sprintf("\n**Diagnosis:** %s\n", cov$diagnosis))
  if (length(cov$band_words_total))
    add(sprintf("Words read by each band, whole document: %s\n",
        paste(sprintf("%s %d", names(cov$band_words_total), cov$band_words_total), collapse = " | ")))
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
