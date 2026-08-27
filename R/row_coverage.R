# row_coverage.R -- a PII-SAFE diagnostic explaining why a PDF statement lost ROWS without anyone
# seeing the statement. Shapes and counts only.

# Collapse a per-row skip reason into a safe category. ORDER MATTERS: the heading reason contains the
# substring "no amount", so testing "no amount" first swallowed it into amount_missing and every
# heading counted as an ACTIONABLE skip, telling the analyst rows had been lost when none were.
.rowcov_bucket <- function(reason) {
  if (is.null(reason) || !nzchar(reason)) return("kept")
  # A split row re-joined above is captured, not lost - decided by the caller, so matched on text.
  if (grepl("^split row", reason)) return("continuation")
  if (grepl("continuation", reason)) return("continuation")
  # Everything else is one of the engine's own codes, mapped back from the sentence it produced.
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
  # THE BAND FRAME, resolved by the parser's own function, so this diagnostic can never report a
  # different page-scale verdict than the reader used.
  frame <- pdf_band_frame(template)
  # Every band the template maps. A band that claims no words is the measurement behind "the credit
  # column has a box drawn over it but reads nothing".
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
    # The band frame's own scale, and nothing beside it. Computing the printed ratio separately gave
    # two answers from two sums, so the report could say "rescaled: yes" and print 1.00x. The printed
    # ratio is the reciprocal - page over frame, the direction a person reads.
    s <- pdf_band_frame_scale(frame, pw[i], ph[i])
    sx <- 1 / s[1]; sy <- 1 / s[2]
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
         # Words each band claimed on this page, and words inside the table region that NO band
         # claimed. Counts only. EVIDENCE, not a verdict.
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
  # TWO MEASUREMENTS, AND NEITHER IS A VERDICT. band_totals is words each mapped band read;
  # unbanded_tot is words in the table region no band claimed. Joining them into "redraw that band" was
  # withdrawn (docs/context/findings-register.md, "the empty-band verdict"): it fired on correct bands,
  # and a word is unclaimed exactly when it is outside EVERY band, so it can never say WHICH is wrong.
  band_totals <- if (!length(band_names)) integer(0) else
    stats::setNames(vapply(band_names, function(k)
      sum(vapply(pages, function(p) as.integer(p$band_words[[k]]), integer(1))), integer(1)), band_names)
  unbanded_tot <- sum(vapply(pages, function(p) p$unbanded_words, integer(1)))
  # Bands that read no words anywhere in the document - exactly that, and nothing implied by it. A
  # column with no entries this period reads nothing and is fine.
  empty_bands <- if (!length(band_totals)) character(0) else names(band_totals)[band_totals == 0L]

  diag <- if (length(empty_pages) && any_scaled)
      sprintf("Page(s) %s are a different physical size than the template and kept no rows -- a page-scale mismatch. The parser normalises this automatically; if it persists the template's reference page size is likely wrong.",
              paste(empty_pages, collapse = ", "))
    else if (length(empty_pages))
      sprintf("Page(s) %s carry words but kept no rows -- either the column bands don't line up on this layout, or those pages hold no transactions.",
              paste(empty_pages, collapse = ", "))
    else if (act_tot > 0)
      sprintf("%d row(s) were skipped for an unreadable date or a missing amount%s -- usually OCR quality on a scan, or a band that is slightly off.",
              act_tot, if (any_ocr) " (this document was machine-read / OCR'd)" else "")
    # All clear, but NOT unconditionally: a band that read no words anywhere can sit under a fully
    # green sentence, because a dead band produces no rows to skip. Staying silent is the opposite
    # error to the withdrawn verdict, so the fact is stated and the judgement left to the reader.
    else if (length(empty_bands))
      sprintf("Every candidate row was kept. Note: the %s band read no words anywhere on this document - check that column against the statement; it may simply have no entries this period.",
              paste(empty_bands, collapse = ", "))
    else "Every candidate row was kept."

  list(applicable = TRUE, ref_width = round(frame$width, 2), ref_height = round(frame$height, 2),
       page_count = length(pages), kept_total = kept_tot, actionable_skips_total = act_tot,
       any_page_rescaled = any_scaled, any_ocr = any_ocr, empty_pages = empty_pages,
       band_words_total = band_totals, empty_bands = empty_bands,
       unbanded_words_total = as.integer(unbanded_tot),
       diagnosis = diag, pages = pages)
}

# Markdown, safe to share.
format_row_coverage <- function(cov) {
  if (!isTRUE(cov$applicable)) return(paste0("Row coverage not available: ", cov$reason %||% "n/a"))
  L <- c(); add <- function(...) L[[length(L) + 1L]] <<- paste0(...)
  add("# Statement row-coverage diagnostic (safe to share - no statement contents)\n")
  add(sprintf("Band frame (the page size the bands were drawn on): **%g x %g pt**. Pages: **%d**. Rows kept: **%d**. Rows skipped as unreadable-date / missing-amount: **%d**.",
      cov$ref_width, cov$ref_height, cov$page_count, cov$kept_total, cov$actionable_skips_total))
  add(sprintf("\n**Diagnosis:** %s\n", cov$diagnosis))
  # The band counts are EVIDENCE for whoever maintains the template. Deliberately NOT folded into the
  # diagnosis: a band reading 0 and words in no band are both normal on a correct template, and
  # joining them into a verdict told maintainers to redraw correct bands.
  if (length(cov$band_words_total)) {
    add(sprintf("Words read by each band, whole document: %s",
        paste(sprintf("%s %d", names(cov$band_words_total), cov$band_words_total), collapse = " | ")))
    add(sprintf("Words inside the table region that no band read: **%d**.\n",
        cov$unbanded_words_total %||% 0L))
    add(paste0("_Both lines are counts, not faults. A band reads 0 when its column has no ",
               "entries this period, and a template maps only the columns it needs, so other ",
               "printing is expected to sit in no band. Read them against the statement ",
               "before moving a band._\n"))
  }
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
