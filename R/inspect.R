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
#     words  = data.frame(x,y,width,height,text,redacted,in_region,column),
#     rows   = data.frame(x0,y0,x1,y1,kept,date)   # one per visual row in-region
#   )))
# `column` is which template column a word is selected into (NA = none / outside
# region). `rows$kept` marks a row the engine would keep as a transaction (its
# date cell parses as a real date, or is redacted) -- i.e. a boxed transaction.
inspect_pdf_layout <- function(input, template) {
  t <- template$table %||% list()
  cols <- t$columns %||% list()
  region <- t$region %||% list()
  date_fmt <- t$date_format %||% "%d/%m/%Y"
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% 3)); if (is.na(row_tol)) row_tol <- 3
  wbp <- input$words %||% list()

  pages <- lapply(seq_along(wbp), function(p) {
    w <- wbp[[p]]
    if (is.null(w) || !nrow(w)) return(list(region = region, bands = cols,
      words = .empty_words(), rows = .empty_rows()))
    w <- as.data.frame(w, stringsAsFactors = FALSE)
    if (is.null(w$redacted)) w$redacted <- FALSE
    cx <- w$x + w$width / 2
    inreg <- .in_region(w, region)
    colassign <- vapply(seq_len(nrow(w)), function(i)
      if (inreg[i]) .word_column(cx[i], cols) else NA_character_, character(1))
    words <- data.frame(x = w$x, y = w$y, width = w$width, height = w$height,
      text = as.character(w$text), redacted = as.logical(w$redacted),
      in_region = inreg, column = colassign, stringsAsFactors = FALSE)

    # Row boxes: group the in-region words by y exactly like parse_pdf_table.
    rw <- w[inreg, , drop = FALSE]
    rows <- .empty_rows()
    if (nrow(rw)) {
      rw <- rw[order(rw$y, rw$x), , drop = FALSE]
      grp <- cumsum(c(TRUE, diff(rw$y) > row_tol))
      rows <- do.call(rbind, lapply(unique(grp), function(g) {
        rg <- rw[grp == g, , drop = FALSE]
        dcell <- .pdf_cell(rg, cols$date)
        d_ok <- !is.na(dcell) && !is.na(parse_date(.first_n_date(dcell, date_fmt), date_fmt)$iso)
        redacted_date <- any(rg$redacted[
          (rg$x + rg$width / 2) >= (cols$date$x_min %||% Inf) &
          (rg$x + rg$width / 2) <= (cols$date$x_max %||% -Inf)])
        data.frame(x0 = min(rg$x), y0 = min(rg$y),
                   x1 = max(rg$x + rg$width), y1 = max(rg$y + rg$height),
                   kept = isTRUE(d_ok) || isTRUE(redacted_date),
                   date = dcell %||% NA_character_, stringsAsFactors = FALSE)
      }))
    }
    list(region = region, bands = cols, words = words, rows = rows)
  })
  names(pages) <- as.character(seq_along(wbp))
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
  in_region = logical(0), column = character(0), stringsAsFactors = FALSE)
.empty_rows <- function() data.frame(x0 = numeric(0), y0 = numeric(0), x1 = numeric(0),
  y1 = numeric(0), kept = logical(0), date = character(0), stringsAsFactors = FALSE)

# field_source_map(template) -> data.frame(field, source) for a delimited / excel
# template: which named source column feeds each canonical field. There is no
# page to draw on, so the inspector shows this mapping as a table instead.
field_source_map <- function(template) {
  cols <- template$columns %||% list()
  empty <- data.frame(field = character(0), source = character(0), stringsAsFactors = FALSE)
  if (!length(cols)) return(empty)
  do.call(rbind, lapply(names(cols), function(f) {
    s <- cols[[f]]; src <- if (is.list(s)) s$source else s
    data.frame(field = f, source = as.character(src %||% NA_character_), stringsAsFactors = FALSE)
  }))
}

# locate_values_on_page(words_page, targets) -> data.frame(field, value, found,
# x0,y0,x1,y1). For each labelled value (opening/closing balance, period dates,
# account, any metadata), find the contiguous run of page words whose text
# matches the value's tokens and return its bounding box -- so the view can draw
# a box around WHERE that value was pulled from. Pragmatic and read-only: it
# needs no change to the label engine. Returns found=FALSE (NA box) when the
# value's word run can't be located (e.g. a text-layer page with no word boxes).
locate_values_on_page <- function(words_page, targets, row_tol = 3) {
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
