# inspect.R -- geometry for the "Statement X-ray" view: show, on the page image,
# EXACTLY what the engine selects and where every value is pulled from. Pure
# functions (no Shiny) so they are unit-testable; the app just draws the result.
#
# Everything is in PDF points, top-left origin -- the same space read_pdf()/OCR
# word boxes, the template's x-bands, and the rendered page raster already share,
# so overlay rectangles line up without any coordinate conversion.

# .word_column(cx, cols) -- name of the first column band whose [x_min,x_max]
# contains centre-x cx, or NA. Mirrors .pdf_cell's membership test exactly.
.word_column <- function(cx, cols) {
  for (nm in names(cols)) {
    b <- cols[[nm]]
    if (!is.null(b$x_min) && !is.null(b$x_max) && cx >= b$x_min && cx <= b$x_max) return(nm)
  }
  NA_character_
}

# .in_region(w, region) -- logical vector: which words fall in the table region
# (same four filters parse_pdf_table applies before grouping rows).
.in_region <- function(w, region) {
  keep <- rep(TRUE, nrow(w))
  if (!is.null(region$x_min)) keep <- keep & (w$x + w$width) >= region$x_min
  if (!is.null(region$x_max)) keep <- keep & w$x <= region$x_max
  if (!is.null(region$y_min)) keep <- keep & w$y >= region$y_min
  if (!is.null(region$y_max)) keep <- keep & w$y <= region$y_max
  keep
}

# inspect_pdf_layout(input, template) -> per-page overlay geometry:
#   list(pages = list(<page> = list(
#     region = list(x_min,x_max,y_min,y_max) | NULL,
#     bands  = named list(field -> list(x_min,x_max)),
#     words  = data.frame(x,y,width,height,text,redacted,in_region,column,ocr_conf),
#     rows   = data.frame(x0,y0,x1,y1,kept,date)   # one per visual row in-region
#   )))
# `column` is which template column a word is selected into (NA = none / outside
# region). `rows$kept` marks a row the engine would keep as a transaction (its
# date cell parses as a real date, or is redacted) -- i.e. a boxed transaction.
inspect_pdf_layout <- function(input, template, force_rows = NULL) {
  t <- template$table %||% list()
  cols <- t$columns %||% list()
  meta_regions <- t$metadata_regions %||% list()
  region <- t$region %||% list()
  date_fmt <- t$date_format %||% "%d/%m/%Y"
  style <- t$amount_sign %||% "signed"
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% PARAM_PDF_ROW_TOL)); if (is.na(row_tol)) row_tol <- PARAM_PDF_ROW_TOL
  wbp <- input$words %||% list()

  # Bands live in the template's reference space. The X-ray draws overlays ON each
  # page's rendered raster, which is in that page's OWN point space, so scale the
  # bands INTO the page's space per page. Membership then matches parse_pdf_table
  # (which does the mirror -- scaling the words into the reference space). Same-size
  # pages are unchanged (snap-to-1), so text-layer X-rays are identical to before.
  ref_w <- suppressWarnings(as.numeric(t$ref_width  %||% .A4_W)); if (is.na(ref_w) || ref_w <= 0) ref_w <- .A4_W
  ref_h <- suppressWarnings(as.numeric(t$ref_height %||% .A4_H)); if (is.na(ref_h) || ref_h <= 0) ref_h <- .A4_H
  page_w <- input$page_width  %||% rep(NA_real_, length(wbp))
  page_h <- input$page_height %||% rep(NA_real_, length(wbp))
  .band_to_page <- function(b, sx, sy) {
    if (is.null(b) || !length(b)) return(b)
    if (!is.null(b$x_min)) b$x_min <- b$x_min * sx
    if (!is.null(b$x_max)) b$x_max <- b$x_max * sx
    if (!is.null(b$y_min)) b$y_min <- b$y_min * sy
    if (!is.null(b$y_max)) b$y_max <- b$y_max * sy
    b
  }
  .page_scale <- function(p) {
    sx <- if (is.finite(page_w[p]) && page_w[p] > 0) page_w[p] / ref_w else 1
    sy <- if (is.finite(page_h[p]) && page_h[p] > 0) page_h[p] / ref_h else 1
    if (abs(sx - 1) < .PAGE_SCALE_SNAP) sx <- 1
    if (abs(sy - 1) < .PAGE_SCALE_SNAP) sy <- 1
    c(sx, sy)
  }

  # metadata_regions that belong on page p (default page 1), scaled into page space
  # -> drawn as pinned header-value boxes so the X-ray shows where each is read from.
  .page_meta <- function(p, sx = 1, sy = 1) {
    if (!length(meta_regions)) return(list())
    keep <- vapply(meta_regions, function(b) identical(as.integer(b$page %||% 1), as.integer(p)), logical(1))
    lapply(meta_regions[keep], .band_to_page, sx, sy)
  }
  pages <- lapply(seq_along(wbp), function(p) {
    s <- .page_scale(p); sx <- s[1]; sy <- s[2]
    cols_p   <- lapply(cols, .band_to_page, sx, sy)
    region_p <- .band_to_page(region, sx, sy)
    # force_rows are stored in reference space; scale to this page for the overlay.
    fr_p <- if (is.null(force_rows) || !length(force_rows)) force_rows
            else lapply(force_rows, function(fb) { fb$y_min <- (fb$y_min %||% NA) * sy; fb$y_max <- (fb$y_max %||% NA) * sy; fb })
    w <- wbp[[p]]
    if (is.null(w) || !nrow(w)) return(list(region = region_p, bands = cols_p,
      words = .empty_words(), rows = .empty_rows(), meta_regions = .page_meta(p, sx, sy)))
    w <- as.data.frame(w, stringsAsFactors = FALSE)
    if (is.null(w$redacted)) w$redacted <- FALSE
    cx <- w$x + w$width / 2
    inreg <- .in_region(w, region_p)
    colassign <- vapply(seq_len(nrow(w)), function(i)
      if (inreg[i]) .word_column(cx[i], cols_p) else NA_character_, character(1))
    # ocr_conf: per-word OCR confidence (0-100) on a machine-read page, NA on a
    # text-layer page -- always present so the app can shade doubtful words.
    words <- data.frame(x = w$x, y = w$y, width = w$width, height = w$height,
      text = as.character(w$text), redacted = as.logical(w$redacted),
      in_region = inreg, column = colassign,
      ocr_conf = suppressWarnings(as.numeric(w$ocr_conf %||% rep(NA_real_, nrow(w)))),
      stringsAsFactors = FALSE)

    # Row boxes: group the in-region words by y exactly like parse_pdf_table.
    rw <- w[inreg, , drop = FALSE]
    rows <- .empty_rows()
    if (nrow(rw)) {
      rw <- rw[order(rw$y, rw$x), , drop = FALSE]
      grp <- .group_rows(rw$y, row_tol)   # SAME grouping as parse_pdf_table -> counts match
      rows <- do.call(rbind, lapply(unique(grp), function(g) {
        rg <- rw[grp == g, , drop = FALSE]
        dcell <- .pdf_cell(rg, cols_p$date)
        d_ok <- !is.na(dcell) && !is.na(parse_date(.first_n_date(dcell, date_fmt), date_fmt)$iso)
        redacted_date <- any(rg$redacted[
          (rg$x + rg$width / 2) >= (cols_p$date$x_min %||% Inf) &
          (rg$x + rg$width / 2) <= (cols_p$date$x_max %||% -Inf)])
        # Apply the ENGINE's full keep predicate, not just the date test: a dated
        # line still has to carry a money amount AND not be a summary line, or the
        # reader drops it. Sharing .pdf_has_amount/.pdf_is_summary keeps them in step.
        rec <- list(amount = .pdf_cell(rg, cols_p$amount), debit = .pdf_cell(rg, cols_p$debit),
                    credit = .pdf_cell(rg, cols_p$credit), description = .pdf_cell(rg, cols_p$description),
                    raw = paste(rg$text[order(rg$x)], collapse = " "))
        date_ok <- isTRUE(d_ok) || isTRUE(redacted_date)   # for the skipped-reason text
        # KEEP RULE (mirrors parse_pdf_table.is_txn): a real date keeps a row with
        # any amount slot; a REDACTED date keeps it only on a real amount; a merely
        # unparseable date is dropped; a whole-row / header box (no real value) is
        # never a transaction.
        real_amount <- .has_real_money(if (identical(style, "debit_credit_cols"))
          paste(rec$debit %||% "", rec$credit %||% "") else (rec$amount %||% ""))
        natural_keep <- !.pdf_is_summary(rec$description, rec$raw) && .pdf_has_amount(rec, style) &&
          (isTRUE(d_ok) || (isTRUE(redacted_date) && real_amount))
        # A row the user forced in (from the skipped list) is painted kept here too,
        # so the X-ray reflects exactly what the reader now emits.
        forced <- .forced_band_hit(p, min(rg$y), max(rg$y + rg$height), fr_p)
        kept <- natural_keep || forced
        # reason: why the engine did NOT keep this row (empty when kept). Same
        # helper the engine uses, so the X-ray can never explain it differently.
        data.frame(x0 = min(rg$x), y0 = min(rg$y),
                   x1 = max(rg$x + rg$width), y1 = max(rg$y + rg$height),
                   kept = isTRUE(kept),
                   date = dcell %||% NA_character_,
                   reason = if (kept) "" else .pdf_row_reason(rec, style, date_ok),
                   raw = rec$raw,
                   h = suppressWarnings(stats::median(rg$height, na.rm = TRUE)),
                   stringsAsFactors = FALSE)
      }))
      rows <- .mark_continuations(rows)
    }
    list(region = region_p, bands = cols_p, words = words, rows = rows, meta_regions = .page_meta(p, sx, sy))
  })
  names(pages) <- as.character(seq_along(wbp))
  # Year-less fallback, mirrored from parse_pdf_table so the X-ray never
  # disagrees with the reader: when the template's own date format reads ZERO
  # dates in the whole document but the statement period supplies the year,
  # the engine keeps day+month rows ("17 Sep") via the fallback - so rows the
  # overlay marked "date didn't parse" that the fallback reads must flip to
  # kept here too.
  all_dates <- unlist(lapply(pages, function(P)
    if (!is.null(P$rows) && nrow(P$rows)) P$rows$date else character(0)))
  all_dates <- all_dates[!is.na(all_dates)]
  prim_any <- any(vapply(all_dates, function(cc) !is.na(suppressWarnings(
    parse_date(.first_n_date(cc, date_fmt), date_fmt)$iso)), logical(1)))
  if (!prim_any && length(all_dates)) {
    md <- safe(extract_metadata(input), NULL)
    yr <- NULL
    for (s in c(md$period_start %||% NA, md$period_end %||% NA)) {
      dd <- .plausible_period_date(s)   # shared with the reader (R/params.R) so the year can't drift
      if (!is.na(dd)) yr <- c(yr, as.integer(format(dd, "%Y")))
    }
    yr <- unique(yr)
    # With no year found anywhere, mirror the reader's sentinel keep: a clear
    # name-month cell (either order) still keeps its row, flagged upstream.
    yr_eff <- if (length(yr)) yr else 2000L
    fb_ok <- function(cell) {
      if (is.na(cell)) return(FALSE)
      toks <- strsplit(trimws(cell), "[[:space:]]+")[[1]]
      if (length(toks) >= 2) cell <- paste(toks[1:2], collapse = " ")
      s <- paste(.normalise_date_str(cell), yr_eff)
      for (f in c("%d %b %Y", "%d %B %Y", "%b %d %Y", "%B %d %Y"))
        if (any(!is.na(suppressWarnings(as.Date(s, f))))) return(TRUE)
      FALSE
    }
    for (p in names(pages)) {
      R <- pages[[p]]$rows
      if (is.null(R) || !nrow(R)) next
      flip <- !R$kept & grepl("didn't parse", R$reason %||% "") &
        vapply(R$date, fb_ok, logical(1), USE.NAMES = FALSE)
      if (any(flip)) { R$kept[flip] <- TRUE; R$reason[flip] <- "" ; pages[[p]]$rows <- R }
    }
  }
  list(pages = pages)
}

# keep just the leading date piece (same idea as parse_pdf_table's .first_date),
# so a two-date band still validates the row's date.
.first_n_date <- function(cell, date_fmt) {
  n <- length(strsplit(trimws(date_fmt), "[[:space:]]+")[[1]])
  toks <- strsplit(trimws(as.character(cell)), "[[:space:]]+")[[1]]
  if (length(toks) <= n) as.character(cell) else paste(toks[seq_len(n)], collapse = " ")
}

.empty_words <- function() data.frame(x = numeric(0), y = numeric(0), width = numeric(0),
  height = numeric(0), text = character(0), redacted = logical(0),
  in_region = logical(0), column = character(0), ocr_conf = numeric(0),
  stringsAsFactors = FALSE)
.empty_rows <- function() data.frame(x0 = numeric(0), y0 = numeric(0), x1 = numeric(0),
  y1 = numeric(0), kept = logical(0), date = character(0), reason = character(0),
  raw = character(0), h = numeric(0), stringsAsFactors = FALSE)

# .mark_continuations(rows) -- upgrade the reason of a dropped "no date and no
# amount" row to "continuation" when it sits directly under a KEPT transaction
# (same proximity rule parse_pdf_table uses to fold wrapped descriptions in), so
# the X-ray shows it as captured-not-lost rather than a missed transaction. It
# also GROWS the parent transaction's box down over the folded line, so the green
# "kept" rectangle visibly includes the wrapped description -- proof the text was
# captured, not dropped.
.mark_continuations <- function(rows) {
  if (!nrow(rows)) return(rows)
  last_kept <- NA_integer_
  for (i in seq_len(nrow(rows))) {
    if (isTRUE(rows$kept[i])) { last_kept <- i; next }
    if (!grepl("^no date and no amount", rows$reason[i])) next
    if (is.na(last_kept)) next
    lh <- if (is.finite(rows$h[i]) && rows$h[i] > 0) rows$h[i] else 10
    gap <- rows$y0[i] - rows$y1[last_kept]
    if (is.finite(gap) && gap <= 0.9 * lh && gap >= -lh && !.is_footer_noise(rows$raw[i])) {
      rows$reason[i] <- "continuation - its text is folded into the transaction above"
      rows$x0[last_kept] <- min(rows$x0[last_kept], rows$x0[i])
      rows$x1[last_kept] <- max(rows$x1[last_kept], rows$x1[i])
      rows$y1[last_kept] <- max(rows$y1[last_kept], rows$y1[i])
    }
  }
  rows
}

# locate_values_on_page(words_page, targets) -> data.frame(field, value, found,
# x0,y0,x1,y1). For each labelled value (opening/closing balance, period dates,
# account, any metadata), find the contiguous run of page words whose text
# matches the value's tokens and return its bounding box -- so the view can draw
# a box around WHERE that value was pulled from. Pragmatic and read-only: it
# needs no change to the label engine. Returns found=FALSE (NA box) when the
# value's word run can't be located (e.g. a text-layer page with no word boxes).
locate_values_on_page <- function(words_page, targets, row_tol = PARAM_PDF_ROW_TOL) {
  na_row <- function(field, value) data.frame(field = field, value = value, found = FALSE,
    x0 = NA_real_, y0 = NA_real_, x1 = NA_real_, y1 = NA_real_, stringsAsFactors = FALSE)
  targets <- targets[!vapply(targets, function(v) is.null(v) || is.na(v) || !nzchar(trimws(v)), logical(1))]
  if (!length(targets)) return(na_row(character(0), character(0))[0, ])
  w <- as.data.frame(words_page, stringsAsFactors = FALSE)
  if (!nrow(w)) return(do.call(rbind, Map(na_row, names(targets), unlist(targets))))
  # reading order: top-to-bottom by row band, then left-to-right
  w <- w[order(round(w$y / max(row_tol, 1)), w$x), , drop = FALSE]
  norm <- function(s) gsub("\\s+", "", tolower(as.character(s)))
  wtext <- norm(w$text)

  rows <- lapply(names(targets), function(field) {
    val <- as.character(targets[[field]])
    toks <- norm(strsplit(trimws(val), "[[:space:]]+")[[1]]); toks <- toks[nzchar(toks)]
    if (!length(toks)) return(na_row(field, val))
    n <- length(toks); best <- NULL
    for (i in seq_len(max(0, nrow(w) - n + 1L))) {
      window <- wtext[i:(i + n - 1L)]
      hit <- if (n == 1) grepl(toks[1], window[1], fixed = TRUE) else all(window == toks)
      if (isTRUE(hit)) { best <- i:(i + n - 1L); break }
    }
    if (is.null(best)) return(na_row(field, val))
    sel <- w[best, , drop = FALSE]
    data.frame(field = field, value = val, found = TRUE,
      x0 = min(sel$x), y0 = min(sel$y),
      x1 = max(sel$x + sel$width), y1 = max(sel$y + sel$height), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
