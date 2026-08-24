# tables_detect.R -- propose the tables and the label/value pairs on a document,
# so that setting one up is CONFIRMING what the tool found rather than drawing
# thirty tables by hand.
#
# WHY PROPOSE AT ALL. The documents this is for carry thirty-odd tables over forty
# pages. Drawing every column of every one of them is hours of work that nobody
# will do twice, and a template half-drawn is worse than none -- it produces
# confident output with a column missing. So the tool reads the page geometry and
# offers what it found; the person names it, corrects it and confirms it. The
# proposal is never applied on its own.
#
# WHAT IT LOOKS FOR. A table, geometrically, is a run of consecutive lines that
# each break into three or more cells at the SAME x positions. Everything else on
# the page -- a heading, a paragraph, a footer -- fails that test, which is why it
# is the test. The column bands come from the vertical gaps that stay EMPTY down
# the whole run: that is the one construction that gets a right-aligned money
# column and a left-aligned text column equally right, because it looks at where
# there is no ink rather than at where a cell happens to start.
#
# WHAT IT WILL GET WRONG, AND WHY THAT IS SAFE. Two tables printed side by side
# read as one. A table with a single column is not a table by this test. A run of
# three lines of a wrapped address can look tabular. All three are visible on the
# screen the moment the proposal is drawn on the page, which is where they get
# corrected -- and none of them can reach an output without a person confirming.

.DOC_MIN_TABLE_ROWS <- 3L    # fewer consecutive tabular lines than this is prose
.DOC_MIN_CELLS      <- 3L    # fewer cells than this on a line is not a table row
.DOC_COL_GAP        <- 6     # an x gap this wide, empty down the run, is a column break

# .doc_cells(d, gap) -- one line's words grouped into CELLS: a new cell starts
# where the horizontal gap from the previous word exceeds `gap`. Returns a list of
# data.frames in reading order.
.doc_cells <- function(d, gap = .DOC_COL_GAP) {
  if (is.null(d) || !nrow(d)) return(list())
  d <- d[order(d$x), , drop = FALSE]
  if (nrow(d) == 1L) return(list(d))
  g <- cumsum(c(0L, as.integer(d$x[-1] - (d$x[-nrow(d)] + d$width[-nrow(d)]) > gap)))
  unname(split(d, g)[order(unique(g))])
}

# .doc_gap_for(w) -- the cell gap for this page, from its own type size. A
# document set in 8pt has smaller column gutters than one set in 12pt, and a
# single constant gets one of them wrong.
.doc_gap_for <- function(w) {
  h <- stats::median(as.numeric(w$height))
  if (!is.finite(h) || h <= 0) return(.DOC_COL_GAP)
  max(.DOC_COL_GAP, h * 0.55)
}

# .doc_bands(words, gap) -- the column bands of a run of lines: project every word
# box onto the x axis, merge the intervals that touch (or nearly touch), and each
# surviving interval is a column. The band is then widened to the midpoint of the
# empty gutter on each side, so a word that shifts a few points on the next copy
# of the document still lands in its own column rather than falling out of every
# band.
.doc_bands <- function(words, gap = .DOC_COL_GAP) {
  if (is.null(words) || !nrow(words)) return(list())
  iv <- data.frame(lo = words$x, hi = words$x + words$width)
  iv <- iv[order(iv$lo), , drop = FALSE]
  lo <- iv$lo[1]; hi <- iv$hi[1]; out <- list()
  for (i in seq_len(nrow(iv))[-1]) {
    if (iv$lo[i] - hi <= gap) { hi <- max(hi, iv$hi[i]) }
    else { out[[length(out) + 1L]] <- c(lo, hi); lo <- iv$lo[i]; hi <- iv$hi[i] }
  }
  out[[length(out) + 1L]] <- c(lo, hi)
  if (length(out) < 2L) return(lapply(out, function(b) list(x_min = b[1], x_max = b[2])))
  lapply(seq_along(out), function(j) {
    left  <- if (j == 1L) out[[j]][1] - gap / 2 else (out[[j - 1]][2] + out[[j]][1]) / 2
    right <- if (j == length(out)) out[[j]][2] + gap / 2 else (out[[j]][2] + out[[j + 1]][1]) / 2
    list(x_min = round(left, 1), x_max = round(right, 1))
  })
}

# .doc_has_money(d) -- does this line carry a figure? The header/data question
# turns on it, and so does the "is this line part of the table or the paragraph
# under it" question.
.doc_has_money <- function(d) {
  any(vapply(as.character(d$text), function(t)
    !is.na(.value_from_line(t, "money")), logical(1)))
}

# .doc_page_candidates(input, page, tmpl) -- the tables proposed on ONE page.
.doc_page_candidates <- function(input, page, tmpl = NULL) {
  w <- .doc_page_words(input, page, tmpl)
  if (is.null(w) || nrow(w) < .DOC_MIN_CELLS) return(list())
  gap <- .doc_gap_for(w)
  lines <- .doc_lines(w)
  if (length(lines) < .DOC_MIN_TABLE_ROWS) return(list())
  tops <- vapply(lines, function(d) as.numeric(attr(d, "top")), numeric(1))
  ncell <- vapply(lines, function(d) length(.doc_cells(d, gap)), integer(1))
  pitch <- .doc_pitch(w$y); if (is.na(pitch)) pitch <- stats::median(w$height) * 1.6
  if (!is.finite(pitch) || pitch <= 0) pitch <- 12

  tabular <- ncell >= .DOC_MIN_CELLS
  # A run is consecutive tabular lines that are also vertically adjacent: a
  # tabular-looking line half a page below the last one is a different table.
  runs <- list(); cur <- integer(0)
  for (i in seq_along(lines)) {
    adjacent <- length(cur) == 0L || (tops[i] - tops[cur[length(cur)]]) <= pitch * 2.2
    if (tabular[i] && adjacent) { cur <- c(cur, i); next }
    if (length(cur) >= .DOC_MIN_TABLE_ROWS) runs[[length(runs) + 1L]] <- cur
    cur <- if (tabular[i]) i else integer(0)
  }
  if (length(cur) >= .DOC_MIN_TABLE_ROWS) runs[[length(runs) + 1L]] <- cur

  lapply(runs, function(ix) {
    body <- do.call(rbind, lapply(lines[ix], function(d) d[, c("x", "width", "y", "height", "text")]))
    bands <- .doc_bands(body, gap)
    first <- lines[[ix[1]]]
    # A header row is a first line with no figures on it, above lines that have
    # them. Where the first line already carries figures the table simply has no
    # header, and one is not invented.
    has_header <- !.doc_has_money(first) &&
      any(vapply(lines[ix[-1]], .doc_has_money, logical(1)))
    hdr_cells <- if (has_header)
      vapply(.doc_cells(first, gap), function(cell) paste(cell$text, collapse = " "),
             character(1))
      else character(0)
    # The name is the nearest line ABOVE the run that is not itself tabular and is
    # short enough to be a heading. That is where these documents print it.
    nm <- NA_character_
    above <- which(seq_along(lines) < ix[1])
    for (i in rev(above)) {
      if (tops[ix[1]] - tops[i] > pitch * 3.5) break
      if (tabular[i]) next
      t <- trimws(.doc_line_text(lines[[i]]))
      if (nzchar(t) && length(strsplit(t, "\\s+")[[1]]) <= 8L) { nm <- t; break }
    }
    cols <- lapply(seq_along(bands), function(j) {
      list(name = if (j <= length(hdr_cells) && nzchar(trimws(hdr_cells[j])))
                    trimws(hdr_cells[j]) else sprintf("column_%d", j),
           x_min = bands[[j]]$x_min, x_max = bands[[j]]$x_max, type = "auto")
    })
    body_ix <- if (has_header) ix[-1] else ix
    list(name = if (is.na(nm)) sprintf("Table on page %d", page) else nm,
         start = list(page = as.integer(page), y = round(tops[ix[1]] - 1, 1)),
         end   = list(page = as.integer(page),
                      y = round(max(vapply(lines[ix], function(d) max(d$y + d$height),
                                           numeric(1))) + 1, 1)),
         header_rows = if (has_header) 1L else 0L,
         columns = cols,
         anchor = list(header_text = as.list(hdr_cells),
                       first_column = as.list(utils::head(vapply(lines[body_ix], function(d) {
                         cl <- .doc_cells(d, gap)
                         if (!length(cl)) "" else trimws(paste(cl[[1]]$text, collapse = " "))
                       }, character(1)), 3L))),
         n_rows = length(body_ix),
         .bands = bands, .page = as.integer(page),
         .bottom = tops[ix[length(ix)]], .page_last_line = identical(ix[length(ix)], length(lines)))
  })
}

# .doc_same_shape(a, b, tol) -- do two candidates use the same column bands?
# Compared on band CENTRES, which is what survives one document being a hair
# wider than the other, and requires the same column count.
.doc_same_shape <- function(a, b, tol = 12) {
  if (length(a$.bands) != length(b$.bands)) return(FALSE)
  ca <- vapply(a$.bands, function(z) (z$x_min + z$x_max) / 2, numeric(1))
  cb <- vapply(b$.bands, function(z) (z$x_min + z$x_max) / 2, numeric(1))
  all(abs(ca - cb) <= tol)
}

# .doc_same_header(a, b) -- the same header wording, normalised.
.doc_same_header <- function(a, b) {
  ha <- .doc_norm(paste(unlist(a$anchor$header_text %||% character(0)), collapse = " "))
  hb <- .doc_norm(paste(unlist(b$anchor$header_text %||% character(0)), collapse = " "))
  nzchar(ha) && identical(ha, hb)
}

# propose_tables(input, tmpl, pages, max_tables) -> a list of table specs, in
# document order, ready to be shown for confirmation.
#
# CONTINUATION. A table that runs onto the next page appears as two candidates,
# and joining them is the difference between one table of 80 rows and two tables
# of 40 that each look complete. They are joined when the first reaches the bottom
# of its page and the second either repeats its header or uses the same column
# bands -- and never otherwise, because joining two DIFFERENT tables is the worse
# mistake of the two: it silently interleaves them.
propose_tables <- function(input, tmpl = NULL, pages = NULL, max_tables = 40L) {
  npg <- length(input$words %||% list())
  if (npg < 1L) return(list())
  pages <- pages %||% seq_len(npg)
  pages <- sort(unique(pages[pages >= 1L & pages <= npg]))
  cands <- list()
  for (p in pages) for (cd in .doc_page_candidates(input, p, tmpl))
    cands[[length(cands) + 1L]] <- cd
  if (!length(cands)) return(list())

  merged <- list()
  for (cd in cands) {
    joined <- FALSE
    if (length(merged)) {
      prev <- merged[[length(merged)]]
      last_page <- as.integer(prev$end$page)
      if (identical(cd$.page, last_page + 1L) && isTRUE(prev$.page_last_line) &&
          (.doc_same_header(prev, cd) || .doc_same_shape(prev, cd))) {
        prev$end <- cd$end
        prev$n_rows <- prev$n_rows + cd$n_rows
        prev$.page_last_line <- cd$.page_last_line
        prev$.bottom <- cd$.bottom
        prev$continued <- TRUE
        merged[[length(merged)]] <- prev
        joined <- TRUE
      }
    }
    if (!joined) merged[[length(merged) + 1L]] <- cd
  }
  # A table that ends at the bottom of its last page may go further on another
  # copy of the document; doc_locate_table() follows it there. Say so here so the
  # setting is visible while the template is being confirmed, not discovered later.
  merged <- lapply(merged, function(m) { m$follow <- isTRUE(m$.page_last_line); m })
  if (length(merged) > max_tables) merged <- merged[seq_len(max_tables)]
  # Names must be unique: they become worksheet names and the `table_name` column.
  nms <- make.unique(vapply(merged, function(m) as.character(m$name)[1], character(1)), sep = " ")
  for (i in seq_along(merged)) merged[[i]]$name <- nms[i]
  merged
}

# propose_pairs(input, tmpl, page) -> candidate label/value pairs on one page
# (the front matter, by default page 1), each as the TWO boxes the template
# stores. Two arrangements are looked for, and between them they cover what these
# documents actually print:
#
#   side by side  -- a line of exactly two cells. Which one is the LABEL is
#                    decided by reading them: if the LEFT cell parses as a figure
#                    or a date and the right does not, then the value came first
#                    and the label follows it. That is the awkward arrangement
#                    ("IN CONFIDENCE  Classification") that a left-to-right
#                    assumption gets backwards.
#   one above the other -- a short line with no figure on it, directly above a
#                    single-cell line that has one.
propose_pairs <- function(input, tmpl = NULL, page = 1L) {
  w <- .doc_page_words(input, page, tmpl)
  if (is.null(w) || !nrow(w)) return(list())
  gap <- .doc_gap_for(w)
  lines <- .doc_lines(w)
  if (!length(lines)) return(list())
  pitch <- .doc_pitch(w$y); if (is.na(pitch) || !is.finite(pitch)) pitch <- 14
  looks_value <- function(s) !is.na(.value_from_line(s, "money")) ||
                             !is.na(.value_from_line(s, "date"))
  bbox <- function(d) list(page = as.integer(page),
                           x_min = round(min(d$x), 1),
                           x_max = round(max(d$x + d$width), 1),
                           y_min = round(min(d$y), 1),
                           y_max = round(max(d$y + d$height), 1))
  txt <- function(d) trimws(paste(d$text, collapse = " "))
  kind <- function(s) if (!is.na(.value_from_line(s, "money"))) "money"
                      else if (!is.na(.value_from_line(s, "date"))) "date" else "text"

  out <- list()
  cells_of <- lapply(lines, .doc_cells, gap = gap)
  for (i in seq_along(lines)) {
    cl <- cells_of[[i]]
    if (length(cl) == 2L) {
      a <- txt(cl[[1]]); b <- txt(cl[[2]])
      if (!nzchar(a) || !nzchar(b)) next
      if (looks_value(a) && !looks_value(b)) {
        out[[length(out) + 1L]] <- list(label_text = b, label = bbox(cl[[2]]),
                                        value = bbox(cl[[1]]), type = kind(a),
                                        read = a)
      } else if (!looks_value(a)) {
        out[[length(out) + 1L]] <- list(label_text = a, label = bbox(cl[[1]]),
                                        value = bbox(cl[[2]]), type = kind(b),
                                        read = b)
      }
      next
    }
    # label above value
    if (length(cl) != 1L || i == length(lines)) next
    below <- cells_of[[i + 1L]]
    if (length(below) != 1L) next
    dy <- as.numeric(attr(lines[[i + 1L]], "top")) - as.numeric(attr(lines[[i]], "top"))
    if (!is.finite(dy) || dy <= 0 || dy > pitch * 1.8) next
    a <- txt(cl[[1]]); b <- txt(below[[1]])
    if (!nzchar(a) || !nzchar(b) || looks_value(a) || !nzchar(.doc_norm(b))) next
    if (length(strsplit(a, "\\s+")[[1]]) > 6L) next
    out[[length(out) + 1L]] <- list(label_text = a, label = bbox(cl[[1]]),
                                    value = bbox(below[[1]]), type = kind(b),
                                    read = b)
  }
  # One pair per label wording: the same label found twice is one value with a
  # repeat, not two fields, and two identically-named fields would collide in the
  # template.
  seen <- character(0); keep <- list()
  for (o in out) {
    k <- .doc_norm(o$label_text)
    if (!nzchar(k) || k %in% seen) next
    seen <- c(seen, k); keep[[length(keep) + 1L]] <- o
  }
  keep
}

# doc_suggest_name(s) -- a proposed table/pair name turned into the snake_case key
# the template stores it under. Names are free text on screen; the key has to be
# a stable identifier, and this is the one place that conversion happens.
doc_suggest_name <- function(s) {
  k <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(as.character(s %||% ""))))
  k <- gsub("^_+|_+$", "", k)
  if (!nzchar(k)) "table" else k
}
