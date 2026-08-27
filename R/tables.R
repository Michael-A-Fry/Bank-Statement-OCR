# tables.R -- the REGION extractor: named tables and label/value pairs out of a document that is not a
# bank statement. A table is a START (page, y), an END (page, y), COLUMNS as x-bands meaningful only
# between them, and an ANCHOR of header and first-column wording - TEXT FIRST, POSITION SECOND. It
# cannot judge a figure wrong, so it MEASURES: how each table was found, how full each column came out,
# and every word inside the boundary that no column claimed.

.doc_num <- function(v, dflt = NA_real_) {
  v <- suppressWarnings(as.numeric(v)[1])
  if (length(v) != 1L || is.na(v) || !is.finite(v)) dflt else v
}

.doc_int <- function(v, dflt = NA_integer_) {
  v <- suppressWarnings(as.integer(v)[1])
  if (length(v) != 1L || is.na(v)) dflt else v
}

.doc_frame <- function(tmpl) {
  list(width  = .doc_num(.doc_key(tmpl, "ref_width"),  .A4_W),
       height = .doc_num(.doc_key(tmpl, "ref_height"), .A4_H))
}

.doc_npages <- function(input) {
  n <- .doc_int(.doc_key(input, "page_count"))
  if (is.na(n)) n <- .doc_int(.doc_key(.doc_key(input, "meta"), "page_count"))
  if (is.na(n) || n < 1L) n <- length(.doc_key(input, "words") %||% list())
  as.integer(n)
}

.doc_page_words <- function(input, page, tmpl = NULL) {
  wl <- input$words %||% list()
  if (is.na(page) || page < 1L || page > length(wl)) return(NULL)
  w <- wl[[page]]
  if (is.null(w) || !nrow(w)) return(NULL)
  w <- as.data.frame(w, stringsAsFactors = FALSE)
  if (!is.null(tmpl)) {
    pw <- (input$page_width  %||% rep(NA_real_, length(wl)))[page]
    ph <- (input$page_height %||% rep(NA_real_, length(wl)))[page]
    fr <- .doc_frame(tmpl)
    same_way <- !is.finite(pw) || !is.finite(ph) ||
      identical(pw >= ph, .doc_num(fr$width, 1) >= .doc_num(fr$height, 1))
    if (same_way) w <- .words_to_band_frame(w, fr, pw, ph)
  }
  w$text <- as.character(w$text)
  w <- w[!is.na(w$text) & nzchar(trimws(w$text)) &
           !grepl("^\\|+$", trimws(w$text)), , drop = FALSE]
  if (!nrow(w)) return(NULL)
  w$cx <- w$x + w$width / 2
  w$cy <- w$y + w$height / 2
  w$page <- as.integer(page)
  w
}

.doc_key <- function(x, k) if (is.list(x) && k %in% names(x)) x[[k]] else NULL

.doc_value_kind <- function(v) {
  v <- trimws(as.character(v %||% ""))
  if (!nzchar(v)) return("text")
  m <- .value_from_line(v, "money")
  if (!is.na(m) && identical(trimws(m), v)) return("money")
  d <- .value_from_line(v, "date")
  if (!is.na(d) && identical(trimws(d), v)) return("date")
  "text"
}

.doc_looks_amount <- function(t) {
  t <- trimws(as.character(t %||% ""))
  if (!nzchar(t)) return(FALSE)
  grepl("^[-+(]?[0-9]{1,3}([,. ][0-9]{3})+([.,][0-9]+)?[)]?$", t) ||
    grepl("^[-+(]?[0-9]+[.,][0-9]{2}[)]?$", t) ||
    !is.na(.value_from_line(t, "money"))
}

.doc_norm <- function(s) gsub("[^a-z0-9]+", "", tolower(as.character(s %||% "")))

.doc_pitch <- function(ys, q = 0.5) {
  ys <- sort(ys[is.finite(ys)])
  if (length(ys) < 3L) return(NA_real_)
  g <- .group_rows(ys, PARAM_PDF_ROW_TOL)
  tops <- as.numeric(tapply(ys, g, min))
  dif <- diff(sort(tops))
  dif <- dif[is.finite(dif) & dif > 0]
  if (!length(dif)) return(NA_real_)
  p <- as.numeric(stats::quantile(dif, probs = q, names = FALSE, type = 7))
  if (!is.finite(p) || p <= 0) NA_real_ else p
}

# Is this line the tail of the row above? Two shapes - indented past the row's left edge, or at the same
# left edge under a tighter gap. Three guards, each load bearing: a tight gap (a sparse row is printed
# with MORE air), more than one column (a one-column block of prose folded 14 lines into 1), and the
# row above filled more than one column.
.doc_is_wrap <- function(d, prev_left, gap, n_filled = 1L,
                         n_cols = 2L, prev_filled = 2L, prev_height = NA_real_) {
  if (!is.finite(prev_left) || !is.finite(gap)) return(FALSE)
  if (n_filled > 1L || gap <= 0) return(FALSE)
  h  <- suppressWarnings(max(as.numeric(d$height %||% NA_real_), na.rm = TRUE))
  ph <- .doc_num(prev_height, NA_real_)
  if (!is.finite(h)) h <- ph
  if (!is.finite(ph)) ph <- h
  if (!is.finite(h) || !is.finite(ph) || h <= 0) return(FALSE)
  span <- h + ph
  if (n_cols < 2L || prev_filled < 2L) return(FALSE)
  if (min(d$x) > prev_left + 4) return(gap <= span * 1.4)
  gap <= span * 1.15
}

.doc_is_rule <- function(d) {
  t <- gsub("[[:space:]]", "", paste(as.character(d$text), collapse = ""))
  nchar(t) >= 3L && !grepl("[^-=_+|~.]", t)
}

.doc_row_tol <- function(ys, declared = NULL) {
  d <- .doc_num(declared)
  if (!is.na(d) && d > 0) return(d)
  pitch <- .doc_pitch(ys, q = 0.25)
  if (is.na(pitch)) return(as.numeric(PARAM_PDF_ROW_TOL))
  # 0.6 of pitch, floored at the statement tolerance. The cap of 14 stops a 40pt title block
  # swallowing a 22pt gap and merging a label with the value under it.
  max(as.numeric(PARAM_PDF_ROW_TOL), min(pitch * 0.6, 14))
}

.doc_lines <- function(w, tol = NULL) {
  if (is.null(w) || !nrow(w)) return(list())
  w <- w[order(w$y, w$x), , drop = FALSE]
  tol <- .doc_row_tol(w$y, tol)
  g <- .group_rows(w$y, tol)
  lapply(split(seq_len(nrow(w)), g), function(ix) {
    d <- w[ix, , drop = FALSE]
    d <- d[order(d$x), , drop = FALSE]
    attr(d, "top") <- min(d$y)
    d
  })
}

.doc_line_text <- function(d) paste(d$text, collapse = " ")

# No space when the first fragment ends in a joining character or the second opens with one: a PDF
# breaks an email mid-token and "xys@ gmail.com" is not what is printed.
.doc_join_wrapped <- function(a, b) {
  a <- as.character(a)[1]; b <- as.character(b)[1]
  if (is.na(a) || !nzchar(a)) return(b)
  if (is.na(b) || !nzchar(b)) return(a)
  if (grepl("[A-Za-z0-9][@/\\\\_=+-]$", a) || grepl("^[@./\\\\][A-Za-z0-9]", b))
    return(paste0(a, b))
  paste(a, b)
}

.doc_join_lines <- function(w, tol = NULL) {
  if (is.null(w) || !NROW(w)) return("")
  lns <- .doc_lines(w, tol)
  if (!length(lns)) return("")
  txt <- vapply(lns, function(d) paste(as.character(d$text), collapse = " "), character(1))
  Reduce(.doc_join_wrapped, txt)
}

.doc_header_hit <- function(d, need) {
  need <- as.character(unlist(need %||% character(0)))
  need <- need[!is.na(need) & nzchar(trimws(need))]
  if (!length(need)) return(0)
  hay <- .doc_norm(.doc_line_text(d))
  if (!nzchar(hay)) return(0)
  mean(vapply(need, function(n) {
    k <- .doc_norm(n); nzchar(k) && grepl(k, hay, fixed = TRUE)
  }, logical(1)))
}

.doc_find_header <- function(lines, need, min_hit = 0.6) {
  if (!length(lines)) return(NULL)
  hits <- vapply(lines, .doc_header_hit, numeric(1), need = need)
  if (!length(hits) || !any(is.finite(hits))) return(NULL)
  best <- which.max(hits)
  if (!length(best) || hits[best] < min_hit) return(NULL)
  list(index = as.integer(best), top = attr(lines[[best]], "top"),
       hit = as.numeric(hits[best]))
}

# The heading as THIS document prints it. When the remembered wording missed on a continuation page
# the window became the whole page and the reprinted heading was read as rows. Refused if any
# learned line carries a figure, and matched WHOLE-LINE, so it cannot delete a real row.
.doc_header_printed <- function(lines, y_from, n) {
  n <- .doc_int(n, 0L)
  if (!length(lines) || is.na(n) || n < 1L) return(character(0))
  tops <- vapply(lines, function(d) .doc_num(attr(d, "top"), NA_real_), numeric(1))
  ix <- which(is.finite(tops) & tops >= .doc_num(y_from, -Inf) - 1)
  if (!length(ix)) return(character(0))
  ix <- utils::head(ix[order(tops[ix])], n)
  for (i in ix) if (.doc_has_money(lines[[i]])) return(character(0))
  sig <- unname(vapply(ix, function(i) .doc_norm(.doc_line_text(lines[[i]])),
                       character(1)))
  sig <- sig[nchar(sig) >= 6L]
  if (!length(sig)) character(0) else sig
}

.doc_find_printed_header <- function(lines, sig) {
  if (!length(lines) || !length(sig)) return(NULL)
  norm <- vapply(lines, function(d) .doc_norm(.doc_line_text(d)), character(1))
  for (i in seq_along(lines)) {
    if (!nzchar(norm[i])) next
    k <- 0L
    while (k < length(sig) && (i + k) <= length(lines) &&
           nzchar(norm[i + k]) &&
           (identical(norm[i + k], sig[k + 1L]) ||
            grepl(sig[k + 1L], norm[i + k], fixed = TRUE))) k <- k + 1L
    if (k >= 1L)
      return(list(index = as.integer(i), top = attr(lines[[i]], "top"),
                  n_lines = as.integer(k), hit = 1))
  }
  NULL
}

# Where this copy prints the table SIDEWAYS: a heading match moves the window vertically only, so at a
# 120pt shift the table was found with confidence 1.00 and every value read one column across. Three
# refusals, because a wrong shift is worse than none: fewer than two columns matched, the columns
# disagree by more than a few points, or every heading cell already sits inside its own band.
.doc_band_shift <- function(d, cols) {
  if (is.null(d) || !NROW(d) || !length(cols)) return(0)
  cells <- .doc_cells(d, .doc_gap_for(d))
  if (length(cells) < 2L) return(0)
  ctext <- vapply(cells, function(z) .doc_norm(paste(z$text, collapse = " ")),
                  character(1))
  ccx   <- vapply(cells, function(z)
    mean(c(min(z$x), max(z$x + .doc_num(z$width, 0)))), numeric(1))
  used <- rep(FALSE, length(cells))
  diffs <- numeric(0); outside <- logical(0)
  for (j in seq_along(cols)) {
    nm <- .doc_norm(as.character(cols[[j]]$name %||% ""))
    lo <- .doc_num(cols[[j]]$x_min, NA_real_); hi <- .doc_num(cols[[j]]$x_max, NA_real_)
    if (!nzchar(nm) || !is.finite(lo) || !is.finite(hi)) next
    hit <- which(!used & nzchar(ctext) & ctext == nm)
    if (!length(hit))
      hit <- which(!used & nzchar(ctext) &
                     (vapply(ctext, function(t) grepl(t, nm, fixed = TRUE), logical(1)) |
                      vapply(ctext, function(t) grepl(nm, t, fixed = TRUE), logical(1))))
    if (length(hit) != 1L) next     # ambiguous is the same as unknown
    used[hit] <- TRUE
    diffs <- c(diffs, ccx[hit] - (lo + hi) / 2)
    outside <- c(outside, ccx[hit] < lo || ccx[hit] > hi)
  }
  if (length(diffs) < 2L) return(0)
  if (!any(outside)) return(0)      # it already fits -- leave it alone
  m <- stats::median(diffs)
  if (max(abs(diffs - m)) > 12) return(0)
  if (abs(m) < 2) return(0)
  as.numeric(m)
}

# A column the document GAINED or LOST is not a shift: a right-anchored comparative table moves
# every figure one place left with the bands unmoved, and Q1 held Q2's figure. Reported, never
# repaired - a guessed remapping is the tool inventing which quarter is which.
.doc_heading_disagreements <- function(input, tab, tmpl, cols, loc) {
  none <- data.frame(column = character(0), heading = character(0),
                     stringsAsFactors = FALSE)
  if (!identical(loc$anchor, "header") || !length(loc$windows)) return(none)
  want <- .doc_column_names(tab)
  if (length(want) < 2L || length(cols) != length(want)) return(none)
  tab2 <- tab; tab2$columns <- cols
  edges <- doc_column_edges(tab2)
  if (is.null(edges) || length(edges) - 1L != length(want)) return(none)
  got <- tryCatch(doc_header_names(input, tab2, tmpl, edges, loc),
                  error = function(e) character(0))
  if (length(got) != length(want)) return(none)
  same <- function(a, b) {
    a <- .doc_norm(a); b <- .doc_norm(b)
    nzchar(a) && nzchar(b) && (grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE))
  }
  # A column may be named whatever its author named it, so a heading that is not the column's name
  # proves nothing. A band under ANOTHER of this table's headings is the shifted-by-one shape.
  bad <- vapply(seq_along(want), function(j) {
    if (!nzchar(trimws(got[j])) || same(want[j], got[j])) return(FALSE)
    any(vapply(want[-j], function(o) same(o, got[j]), logical(1)))
  }, logical(1))
  if (!any(bad)) return(none)
  data.frame(column = want[bad], heading = trimws(got[bad]),
             stringsAsFactors = FALSE)
}

.doc_shift_cols <- function(cols, dx) {
  dx <- .doc_num(dx, 0)
  if (!is.finite(dx) || dx == 0 || !length(cols)) return(cols)
  lapply(cols, function(cc) {
    if (is.finite(.doc_num(cc$x_min, NA_real_))) cc$x_min <- .doc_num(cc$x_min) + dx
    if (is.finite(.doc_num(cc$x_max, NA_real_))) cc$x_max <- .doc_num(cc$x_max) + dx
    cc
  })
}

.doc_find_first_column <- function(lines, need, col) {
  need <- .doc_norm(as.character(unlist(need %||% character(0))))
  need <- need[nzchar(need)]
  if (!length(need) || is.null(col)) return(NULL)
  for (i in seq_along(lines)) {
    d <- lines[[i]]
    inband <- d$cx >= .doc_num(col$x_min, -Inf) & d$cx <= .doc_num(col$x_max, Inf)
    if (!any(inband)) next
    k <- .doc_norm(paste(d$text[inband], collapse = " "))
    if (nzchar(k) && any(vapply(need, function(n) grepl(n, k, fixed = TRUE), logical(1))))
      return(list(index = as.integer(i), top = attr(d, "top")))
  }
  NULL
}

.DOC_ANCHOR_RANK <- c(header = 1L, first_column = 2L,
                      position_same_page = 3L, position_other_page = 4L,
                      not_found = 5L)
.doc_anchor_confidence <- function(anchor, hit = NA_real_) {
  switch(anchor,
    header             = if (is.na(hit)) 0.9 else max(0.6, min(1, hit)),
    first_column       = 0.7,
    position_same_page = 0.4,
    position_other_page = 0.2,
    not_found          = 0,
    0.2)
}

# The fifth outcome: this table is NOT here. Position always succeeded, so a table the document
# lacks came back full of whatever ink sits at those coordinates - 34 copies of one row.
.doc_no_table <- function(detail) {
  list(pages = integer(0), windows = list(), anchor = "not_found",
       confidence = .doc_anchor_confidence("not_found"),
       detail = as.character(detail)[1], extended_to = NA_integer_,
       header_tops = list(), header_sig = character(0), band_shift = 0)
}

# Which page the table is on, asked of the whole document: reading tab$start$page lost every table
# below an inserted page. Identity is NAME plus COLUMN NAMES; the declared page only breaks ties.
.DOC_PAGE_MIN_SCORE <- 0.6

.doc_page_has_title <- function(lines, title) {
  key <- .doc_norm(title)
  if (nchar(key) < 8L) return(FALSE)
  any(vapply(lines, function(d) grepl(key, .doc_norm(.doc_line_text(d)), fixed = TRUE),
             logical(1)))
}

.doc_page_score <- function(input, tab, tmpl, p, need, title) {
  lines <- .doc_lines(.doc_page_words(input, p, tmpl), tab$row_tol)
  if (!length(lines)) return(0)
  h <- if (length(need)) .doc_find_header(lines, need) else NULL
  (if (is.null(h)) 0 else as.numeric(h$hit)) +
    (if (.doc_page_has_title(lines, title)) 1 else 0)
}

.doc_search_terms <- function(tab) {
  need <- tab$anchor$header_text %||% .doc_column_names(tab)
  need <- as.character(unlist(need %||% character(0)))
  need <- need[!is.na(need) & nzchar(trimws(need))]
  title <- as.character(tab$name %||% "")[1]
  if (!length(need) && nchar(.doc_norm(title)) < 8L) return(NULL)
  list(need = need, title = title)
}

.doc_page_scores <- function(input, tab, tmpl = NULL) {
  npg <- .doc_npages(input)
  if (is.na(npg) || npg < 1L) return(numeric(0))
  s <- .doc_search_terms(tab); if (is.null(s)) return(numeric(0))
  vapply(seq_len(npg), function(p)
    .doc_page_score(input, tab, tmpl, p, s$need, s$title), numeric(1))
}

# The declared page wins whenever the table is on it: a report can print two DIFFERENT tables
# under the same headings, and a whole-document search would let the first answer for both.
.doc_find_page <- function(input, tab, tmpl = NULL) {
  s <- .doc_search_terms(tab); if (is.null(s)) return(NA_integer_)
  npg <- .doc_npages(input); if (is.na(npg) || npg < 1L) return(NA_integer_)
  p0 <- .doc_int(tab$start$page, 1L); if (is.na(p0) || p0 < 1L) p0 <- 1L
  if (p0 <= npg &&
      .doc_page_score(input, tab, tmpl, p0, s$need, s$title) >= .DOC_PAGE_MIN_SCORE)
    return(NA_integer_)
  sc <- vapply(seq_len(npg), function(p)
    .doc_page_score(input, tab, tmpl, p, s$need, s$title), numeric(1))
  if (!any(sc >= .DOC_PAGE_MIN_SCORE)) return(NA_integer_)
  best <- which(sc >= max(sc) - 1e-9)
  as.integer(best[which.min(abs(best - p0))])
}

doc_locate_repeats <- function(input, tab, tmpl = NULL) {
  sc <- .doc_page_scores(input, tab, tmpl)
  if (!length(sc)) return(integer(0))
  hit <- which(sc >= .DOC_PAGE_MIN_SCORE)
  if (!length(hit)) return(integer(0))
  as.integer(hit[order(-sc[hit], hit)])
}

# `windows` is one list(page, y_min, y_max) per page in the template's frame; rows are only read
# inside them, which is what makes several tables on one page work.
doc_locate_table <- function(input, tab, tmpl = NULL) {
  npg <- .doc_npages(input)
  p0 <- .doc_int(tab$start$page, 1L); if (is.na(p0) || p0 < 1L) p0 <- 1L
  p1 <- .doc_int(tab$end$page, p0);   if (is.na(p1) || p1 < p0) p1 <- p0
  # A page this document does not have is not the last page - clamping p0 read the table off
  # whatever ink the last page carries. The END is still clamped.
  moved_from <- NA_integer_
  pf <- .doc_find_page(input, tab, tmpl)
  if (!is.na(pf) && pf != p0) {
    moved_from <- p0
    p1 <- min(p1 + (pf - p0), max(npg, 1L)); p0 <- pf
  }
  if (npg >= 1L && p0 > npg)
    return(.doc_no_table(sprintf(
      "this table is set to start on page %d and this document only has %d page%s",
      p0, npg, if (npg == 1L) "" else "s")))
  p0 <- min(p0, max(npg, 1L)); p1 <- min(p1, max(npg, 1L))

  band <- tab$band %||% list()
  band_top <- .doc_num(band$y_min, 0)
  # The page bottom is the document's, not A4's: it decides whether a window is open-ended and
  # whether the table may follow onto the next page.
  page_h <- suppressWarnings(as.numeric(
    (input$page_height %||% NA_real_)[min(p0, length(input$page_height %||% 1))]))
  if (!is.finite(page_h) || page_h <= 0) page_h <- NA_real_
  band_bot <- .doc_num(band$y_max,
    .doc_num(.doc_key(tmpl, "ref_height"), page_h %||% .A4_H))
  if (is.na(band_bot)) band_bot <- if (is.na(page_h)) .A4_H else page_h
  y_start_declared <- .doc_num(tab$start$y, band_top)
  y_end_declared   <- .doc_num(tab$end$y, band_bot)

  need_hdr <- tab$anchor$header_text %||% .doc_column_names(tab)
  lines0 <- .doc_lines(.doc_page_words(input, p0, tmpl), tab$row_tol)

  n_header_rows <- max(0L, .doc_int(tab$header_rows, 1L))
  cols0 <- .doc_columns(tab)
  anchor <- NULL; hit <- NA_real_; y_start <- y_start_declared
  start_at_header <- FALSE
  h <- .doc_find_header(lines0, need_hdr)
  if (!is.null(h)) {
    anchor <- "header"; hit <- h$hit; y_start <- h$top; start_at_header <- TRUE
  } else {
    fc <- if (length(cols0))
            .doc_find_first_column(lines0, tab$anchor$first_column, cols0[[1]]) else NULL
    if (!is.null(fc)) {
      anchor <- "first_column"
      pitch <- .doc_pitch(if (length(lines0))
                            unlist(lapply(lines0, function(d) d$y)) else numeric(0))
      if (is.na(pitch)) pitch <- as.numeric(PARAM_PDF_ROW_TOL) * 4
      y_start <- fc$top - pitch * n_header_rows - 1
      start_at_header <- n_header_rows > 0L
    } else {
      sought <- c(as.character(unlist(.doc_key(.doc_key(tab, "anchor"), "header_text")
                                      %||% character(0))),
                  as.character(unlist(.doc_key(.doc_key(tab, "anchor"), "first_column")
                                      %||% character(0))))
      sought <- sought[!is.na(sought) & nzchar(trimws(sought))]
      if (length(sought))
        return(.doc_no_table("this table was not found on this document"))
      declared_pages <- .doc_int(.doc_key(tab, "doc_pages"),
                                 .doc_int(.doc_key(tmpl, "doc_pages"), npg))
      anchor <- if (identical(npg, declared_pages))
                  "position_same_page" else "position_other_page"
      y_start <- y_start_declared
      start_at_header <- n_header_rows > 0L
    }
  }

  hdr_sig <- if (start_at_header)
    .doc_header_printed(lines0, y_start, n_header_rows) else character(0)

  band_shift <- if (identical(anchor, "header") && !is.null(h))
    .doc_band_shift(lines0[[h$index]], cols0) else 0

  # The table is as long as it is HERE: a declared end treated as exact cut a three-hundred-row
  # schedule off at twelve, and truncation is invisible. It can only ever make the table LONGER, is
  # only asked where the example's end is not already the paper's edge, and `auto_end: false` is off.
  grew_to <- NULL
  if (!isFALSE(.doc_key(tab, "auto_end")) &&
      is.finite(y_end_declared) && y_end_declared < band_bot - 1) {
    probe <- tab
    probe$start$page <- p0; probe$end$page <- p1
    ae  <- tryCatch(doc_auto_end(input, probe, tmpl, follow = FALSE),
                    error = function(e) NULL)
    aep <- .doc_int(ae$page, NA_integer_); aey <- .doc_num(ae$y, NA_real_)
    if (is.finite(aep) && is.finite(aey) && aep >= p0 && aep <= max(npg, 1L) &&
        (aep > p1 || (aep == p1 && aey > y_end_declared + 1))) {
      grew_to <- list(page = aep, y = aey)
      p1 <- aep; y_end_declared <- aey
    }
  }

  windows <- list(); header_tops <- list()
  for (p in seq.int(p0, p1)) {
    y_min <- if (p == p0) y_start else band_top
    y_max <- if (p == p1) y_end_declared else band_bot
    skip <- if (p == p0) (if (start_at_header) n_header_rows else 0L) else 0L
    if (p > p0) {
      lp <- .doc_lines(.doc_page_words(input, p, tmpl), tab$row_tol)
      hp <- .doc_find_header(lp, need_hdr)
      if (is.null(hp)) hp <- .doc_find_printed_header(lp, hdr_sig)
      if (!is.null(hp)) {
        y_min <- hp$top; header_tops[[as.character(p)]] <- hp$top
        skip <- .doc_int(hp$n_lines, n_header_rows)
      }
    } else if (!is.null(h)) header_tops[[as.character(p)]] <- h$top
    windows[[length(windows) + 1L]] <- list(page = as.integer(p),
                                            y_min = y_min, y_max = y_max,
                                            skip = as.integer(skip),
                                            open_end = (p != p1) || y_max >= band_bot - 1)
  }

  # Follow: a table whose declared end is the BOTTOM of its last page keeps going while following
  # pages repeat its header. Recorded and reported, never silent.
  extended_to <- NA_integer_
  if (!isFALSE(tab$follow) && p1 < npg && y_end_declared >= band_bot - 1) {
    p <- p1 + 1L
    while (p <= npg) {
      lp <- .doc_lines(.doc_page_words(input, p, tmpl), tab$row_tol)
      hp <- .doc_find_header(lp, need_hdr, min_hit = 0.75)
      if (is.null(hp)) hp <- .doc_find_printed_header(lp, hdr_sig)
      if (is.null(hp)) break
      windows[[length(windows) + 1L]] <- list(page = as.integer(p),
                                              y_min = hp$top, y_max = band_bot,
                                              skip = .doc_int(hp$n_lines, n_header_rows),
                                              open_end = TRUE)
      header_tops[[as.character(p)]] <- hp$top
      extended_to <- as.integer(p)
      p <- p + 1L
    }
  }

  pages <- vapply(windows, function(z) z$page, integer(1))
  detail <- switch(anchor,
    header = sprintf("found by its heading on page %d", p0),
    first_column = sprintf("found by the rows it starts with on page %d", p0),
    position_same_page = sprintf("read from where it sat on the example (page %d)", p0),
    sprintf("read from where it sat on the example, and this document is a different length (page %d)", p0))
  if (!is.na(moved_from))
    detail <- paste0(detail, sprintf(
      "; the template had it on page %d, and it is on page %d here",
      moved_from, p0))
  if (!is.null(grew_to))
    detail <- paste0(detail, "; it runs further down the page here than on the example")
  if (!is.na(extended_to))
    detail <- paste0(detail, sprintf("; it carries on to page %d here", extended_to))
  rep_pages <- setdiff(as.integer(names(header_tops)), p0)
  rep_pages <- sort(rep_pages[!is.na(rep_pages)])
  if (length(rep_pages))
    detail <- paste0(detail, sprintf(
      "; its heading is printed again on page%s %s and is not read as rows",
      if (length(rep_pages) == 1L) "" else "s",
      paste(rep_pages, collapse = ", ")))
  if (is.finite(band_shift) && band_shift != 0)
    detail <- paste0(detail, sprintf(
      "; this copy prints it %.0fpt further %s than the example, and its columns were moved to match",
      abs(band_shift), if (band_shift > 0) "right" else "left"))
  list(pages = pages, windows = windows, anchor = anchor,
       confidence = .doc_anchor_confidence(anchor, hit),
       detail = detail, extended_to = extended_to, header_tops = header_tops,
       header_sig = hdr_sig, band_shift = as.numeric(band_shift))
}

.doc_columns <- function(tab) {
  cols <- tab$columns %||% list()
  if (!length(cols)) return(list())
  if (!is.null(names(cols)) && all(nzchar(names(cols))) && is.list(cols[[1]]) &&
      is.null(cols[[1]]$name)) {
    cols <- lapply(names(cols), function(n) { c(list(name = n), cols[[n]]) })
  }
  lapply(cols, function(c) if (is.list(c)) c else list(name = as.character(c)))
}

.DOC_RESERVED <- c("page", "row")
.doc_column_names <- function(tab) {
  cols <- .doc_columns(tab)
  if (!length(cols)) return(character(0))
  nm <- vapply(seq_along(cols), function(i) {
    n <- trimws(as.character(cols[[i]]$name %||% "")[1])
    if (is.na(n) || !nzchar(n)) sprintf("column_%d", i) else n
  }, character(1))
  nm[tolower(nm) %in% .DOC_RESERVED] <- paste0(nm[tolower(nm) %in% .DOC_RESERVED], "_")
  make.unique(nm, sep = "_")
}

.doc_col_of <- function(cx, cols) {
  out <- rep(NA_integer_, length(cx))
  for (j in seq_along(cols)) {
    lo <- .doc_num(cols[[j]]$x_min, -Inf); hi <- .doc_num(cols[[j]]$x_max, Inf)
    hit <- is.na(out) & cx >= lo & cx < hi
    out[hit] <- j
  }
  if (length(cols)) {
    j <- length(cols)
    hi <- .doc_num(cols[[j]]$x_max, Inf)
    out[is.na(out) & cx == hi] <- j
  }
  out
}

# Columns as EDGES - what the builder manipulates. Columns tile, so the real choice is the N-1
# dividers plus the outer edge each side: a click cannot produce a gap or overlap that loses one.
doc_column_edges <- function(tab) {
  cols <- .doc_columns(tab)
  if (!length(cols)) return(NULL)
  lo <- vapply(cols, function(cc) .doc_num(cc$x_min, NA_real_), numeric(1))
  hi <- vapply(cols, function(cc) .doc_num(cc$x_max, NA_real_), numeric(1))
  if (any(is.na(lo)) || any(is.na(hi))) return(NULL)
  o <- order(lo)
  sort(unique(round(c(lo[o], hi[o[length(o)]]), 2)))
}

doc_columns_from_edges <- function(edges, old = list(), names = character(0)) {
  edges <- sort(unique(round(as.numeric(edges), 2)))
  edges <- edges[is.finite(edges)]
  if (length(edges) < 2L) return(list())
  same <- function(a, b) is.finite(a) && is.finite(b) && abs(a - b) < 0.5
  lapply(seq_len(length(edges) - 1L), function(j) {
    lo <- edges[j]; hi <- edges[j + 1L]
    keep <- NULL
    for (cc in old)
      if (same(.doc_num(cc$x_min), lo) && same(.doc_num(cc$x_max), hi)) { keep <- cc; break }
    if (!is.null(keep)) { keep$x_min <- lo; keep$x_max <- hi; return(keep) }
    nm <- if (j <= length(names) && nzchar(trimws(names[j]))) trimws(names[j])
          else sprintf("column_%d", j)
    list(name = nm, x_min = lo, x_max = hi, type = "auto", may_be_blank = FALSE)
  })
}

# One gesture, four meanings: on a divider remove it, on an outer edge move it, outside the table
# move the nearer outer edge out, anywhere else inside add a divider. Removing beats adding.
doc_edge_click <- function(edges, x, tol = 6) {
  x <- .doc_num(x)
  if (is.na(x)) return(edges)
  edges <- sort(unique(round(as.numeric(edges), 2)))
  edges <- edges[is.finite(edges)]
  if (length(edges) < 2L) return(edges)
  n <- length(edges)
  inner <- if (n > 2L) 2:(n - 1L) else integer(0)
  if (length(inner)) {
    d <- abs(edges[inner] - x)
    if (min(d) <= tol) return(edges[-inner[which.min(d)]])
  }
  if (abs(x - edges[1]) <= tol) { edges[1] <- x; return(sort(edges)) }
  if (abs(x - edges[n]) <= tol) { edges[n] <- x; return(sort(edges)) }
  if (x < edges[1]) { edges[1] <- x; return(edges) }
  if (x > edges[n]) { edges[n] <- x; return(edges) }
  sort(c(edges, x))
}

# Columns may have whitespace between them; they may never overlap. An overlap destroys data -
# whatever falls in it is read into the first column and lost from the second - so an overlapping
# move is CLAMPED to the free space rather than refused. Every unclaimed word is counted.
.doc_col_bands <- function(cols) {
  cols <- if (is.list(cols) && !is.null(cols$columns)) .doc_columns(cols) else cols
  if (!length(cols)) return(matrix(numeric(0), ncol = 2))
  lo <- vapply(cols, function(cc) .doc_num(cc$x_min, NA_real_), numeric(1))
  hi <- vapply(cols, function(cc) .doc_num(cc$x_max, NA_real_), numeric(1))
  ok <- is.finite(lo) & is.finite(hi) & hi > lo
  cbind(lo, hi)[ok, , drop = FALSE]
}

.doc_clamp_band <- function(bands, skip, lo, hi) {
  if (NROW(bands)) {
    keep <- if (is.na(skip)) seq_len(nrow(bands)) else setdiff(seq_len(nrow(bands)), skip)
    for (k in keep) {
      a <- bands[k, 1]; b <- bands[k, 2]
      if (b <= lo || a >= hi) next                 # clear of it
      if (a <= lo && b >= hi) return(NULL)         # entirely inside a column
      if (a <= lo) lo <- b else hi <- a            # trim to its near side
    }
  }
  if (!is.finite(lo) || !is.finite(hi) || hi - lo < 1) return(NULL)
  c(lo, hi)
}

# attr "clamped" is TRUE when the new column had to be trimmed off a neighbour. Not two edge
# clicks - fed through doc_edge_click twice, a drag beside the table stretched one column over all.
doc_add_column <- function(cols, x0, x1, names = character(0)) {
  old <- if (is.list(cols) && !is.null(cols$columns)) .doc_columns(cols) else (cols %||% list())
  lo <- suppressWarnings(min(x0, x1)); hi <- suppressWarnings(max(x0, x1))
  if (!isTRUE(is.finite(lo)) || !isTRUE(is.finite(hi)) || hi - lo < 1) return(old)
  b <- .doc_clamp_band(.doc_col_bands(old), NA_integer_, lo, hi)
  if (is.null(b)) return(old)
  nm <- if (length(names) && nzchar(trimws(names[1]))) trimws(names[1]) else
          sprintf("column_%d", length(old) + 1L)
  new <- list(name = nm, x_min = round(b[1], 2), x_max = round(b[2], 2),
              type = "auto", may_be_blank = FALSE)
  out <- .doc_sort_columns(c(old, list(new)))
  attr(out, "clamped") <- !isTRUE(all.equal(c(lo, hi), b, tolerance = 1e-6))
  out
}

doc_set_column_band <- function(cols, j, x0, x1) {
  old <- if (is.list(cols) && !is.null(cols$columns)) .doc_columns(cols) else (cols %||% list())
  j <- .doc_int(j)
  if (is.na(j) || j < 1L || j > length(old)) return(old)
  lo <- suppressWarnings(min(x0, x1)); hi <- suppressWarnings(max(x0, x1))
  if (!isTRUE(is.finite(lo)) || !isTRUE(is.finite(hi)) || hi - lo < 1) return(old)
  b <- .doc_clamp_band(.doc_col_bands(old), j, lo, hi)
  if (is.null(b)) return(old)
  old[[j]]$x_min <- round(b[1], 2); old[[j]]$x_max <- round(b[2], 2)
  out <- .doc_sort_columns(old)
  attr(out, "clamped") <- !isTRUE(all.equal(c(lo, hi), b, tolerance = 1e-6))
  out
}

.doc_sort_columns <- function(cols) {
  if (length(cols) < 2L) return(cols)
  lo <- vapply(cols, function(cc) .doc_num(cc$x_min, Inf), numeric(1))
  cols[order(lo)]
}

doc_columns_touch <- function(cols) {
  b <- .doc_col_bands(cols)
  if (nrow(b) < 2L) return(TRUE)
  all(abs(b[-1, 1] - b[-nrow(b), 2]) < 0.5)
}

doc_header_names <- function(input, tab, tmpl = NULL, edges = NULL, loc = NULL) {
  edges <- edges %||% doc_column_edges(tab)
  if (is.null(edges) || length(edges) < 2L) return(character(0))
  n <- length(edges) - 1L
  out <- rep("", n)
  loc <- loc %||% tryCatch(doc_locate_table(input, tab, tmpl), error = function(e) NULL)
  if (is.null(loc) || !length(loc$windows)) return(out)
  win <- loc$windows[[1]]
  w <- .doc_page_words(input, win$page, tmpl)
  if (is.null(w)) return(out)
  w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
  lines <- .doc_lines(w, tab$row_tol)
  nh <- max(1L, .doc_int(win$skip, .doc_int(tab$header_rows, 1L)))
  if (!length(lines)) return(out)
  hdr <- lines[seq_len(min(nh, length(lines)))]
  d <- do.call(rbind, hdr)
  for (j in seq_len(n)) {
    sel <- d[d$cx >= edges[j] & d$cx < edges[j + 1L], , drop = FALSE]
    if (nrow(sel)) {
      sel <- sel[order(sel$y, sel$x), , drop = FALSE]
      # A column heading is not an amount: one header row too many otherwise names a column
      # "Revenue Medical & Public Health 47,824,589", which becomes a key in the long output.
      keep <- !vapply(as.character(sel$text), .doc_looks_amount, logical(1))
      if (any(keep)) sel <- sel[keep, , drop = FALSE]
      nm <- trimws(gsub("\\s+", " ", paste(sel$text, collapse = " ")))
      if (any(keep)) out[j] <- nm
    }
  }
  out
}

doc_auto_end <- function(input, tab, tmpl = NULL, follow = TRUE) {
  probe <- tab
  # Open the end of the start page and FOLLOW rather than pointing at the last page: a middle
  # window covers its whole page whether or not the table is on it, so a four-row summary came back
  # with 103 rows. Forcing follow at read time doubled every identically-headed table.
  probe$follow <- isTRUE(follow)
  probe$end <- list(page = .doc_int(tab$start$page, 1L),
                    y = .doc_frame(tmpl)$height)
  r <- tryCatch(doc_table_rows(input, probe, tmpl), error = function(e) NULL)
  if (is.null(r) || is.na(r$last_page) || is.na(r$last_y)) return(tab$end)
  list(page = as.integer(r$last_page), y = round(as.numeric(r$last_y) + 2, 1))
}

# A division any ONE line shows is a division: merging touching intervals is right for where the
# columns are and wrong for how many, since one long value reaching under its neighbour's heading
# joins two intervals for good. Can only ever ADD a division.
.doc_split_bands_by_cells <- function(bands, lines, gap) {
  if (length(bands) < 1L || !length(lines)) return(bands)
  spans <- lapply(lines, function(d) {
    if (is.null(d) || !NROW(d)) return(list())
    lapply(.doc_cells(d[order(d$x), , drop = FALSE], gap), function(z) {
      lo <- min(z$x); hi <- max(z$x + z$width)
      c(lo = lo, hi = hi, cx = (lo + hi) / 2)
    })
  })
  out <- list()
  for (b in bands) {
    lo <- .doc_num(b$x_min, NA_real_); hi <- .doc_num(b$x_max, NA_real_)
    if (!is.finite(lo) || !is.finite(hi)) { out[[length(out) + 1L]] <- b; next }
    best <- list()
    for (cs in spans) {
      inside <- Filter(function(z) z[["cx"]] >= lo && z[["cx"]] <= hi, cs)
      if (length(inside) > length(best)) best <- inside
    }
    if (length(best) < 2L) { out[[length(out) + 1L]] <- b; next }
    best <- best[order(vapply(best, function(z) z[["lo"]], numeric(1)))]
    cuts <- vapply(seq_len(length(best) - 1L), function(k)
      (best[[k]][["hi"]] + best[[k + 1L]][["lo"]]) / 2, numeric(1))
    e <- sort(unique(round(c(lo, cuts[cuts > lo & cuts < hi], hi), 1)))
    for (k in seq_len(length(e) - 1L))
      out[[length(out) + 1L]] <- list(x_min = e[k], x_max = e[k + 1L])
  }
  out
}

doc_columns_from_box <- function(input, box, tmpl = NULL, y_to = NA_real_) {
  pg <- .doc_int(.doc_key(box, "page"), 1L)
  w <- .doc_page_words(input, pg, tmpl)
  if (is.null(w)) return(list())
  # A column with a blank heading has no ink in the header row, and it is usually the TOTAL column
  # - four bands read four columns, every row complete, the totals silently gone. So the bands come
  # from the whole table and the names from its heading line, and the box's x limits are dropped.
  yb <- .doc_num(y_to, NA_real_)
  wide <- is.finite(yb) && yb > .doc_num(box$y_max, -Inf)
  sel <- if (wide)
    w[w$cy >= .doc_num(box$y_min, -Inf) & w$cy <= yb, , drop = FALSE]
  else
    w[w$cx >= .doc_num(box$x_min, -Inf) & w$cx <= .doc_num(box$x_max, Inf) &
      w$cy >= .doc_num(box$y_min, -Inf) & w$cy <= .doc_num(box$y_max, Inf), ,
      drop = FALSE]
  if (!nrow(sel)) return(list())
  gap <- .doc_gap_for(sel)
  # Only table-shaped lines vote on where the columns are; a footer is one cell wide and would
  # smear two bands into one. The heading line is kept whatever its shape.
  if (wide) {
    keep <- unlist(lapply(.doc_lines(sel), function(d)
      if (length(.doc_cells(d[order(d$x), , drop = FALSE], gap)) >= 2L)
        as.numeric(d$cy) else numeric(0)))
    hdr_y <- .doc_num(box$y_max, -Inf)
    sel <- sel[sel$cy <= hdr_y | sel$cy %in% keep, , drop = FALSE]
    if (!nrow(sel)) return(list())
    gap <- .doc_gap_for(sel)
  }
  lines <- .doc_lines(sel)
  top <- if (length(lines)) lines[[1]] else sel
  cells <- .doc_cells(top[order(top$x), , drop = FALSE], gap)
  if (!length(cells)) return(list())
  bands <- .doc_bands(sel, gap)
  if (!length(bands)) return(list())
  bands <- .doc_split_bands_by_cells(bands, lines, gap)
  edges <- c(vapply(bands, function(b) b$x_min, numeric(1)),
             bands[[length(bands)]]$x_max)
  nm <- make.unique(vapply(seq_along(bands), function(j) {
    hit <- Filter(function(cl) {
      cx <- mean(cl$x + cl$width / 2)
      cx >= bands[[j]]$x_min && cx <= bands[[j]]$x_max
    }, cells)
    if (!length(hit)) sprintf("column_%d", j)
    else trimws(gsub("^[|+:_-]+|[|+:_-]+$", "",
                     trimws(paste(unlist(lapply(hit, function(cl) cl$text)), collapse = " "))))
  }, character(1)), sep = "_")
  nm[!nzchar(nm)] <- sprintf("column_%d", which(!nzchar(nm)))
  doc_columns_from_edges(edges, names = nm)
}

# It may never take a column away: deleting one because this month's ink is sparse is how a whole
# column of figures goes missing. A drawn column whose band holds no ink is kept exactly as drawn.
doc_fit_columns <- function(input, tab, tmpl = NULL) {
  old <- .doc_columns(tab)
  loc <- tryCatch(doc_locate_table(input, tab, tmpl), error = function(e) NULL)
  if (is.null(loc) || !length(loc$windows)) return(if (length(old)) old else list())
  parts <- list()
  for (win in loc$windows) {
    w <- .doc_page_words(input, win$page, tmpl)
    if (is.null(w)) next
    w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
    if (!nrow(w)) next
    for (d in .doc_lines(w, tab$row_tol)) if (!.doc_is_rule(d)) parts[[length(parts) + 1L]] <- d
  }
  if (!length(parts)) return(if (length(old)) old else list())
  body <- do.call(rbind, parts)
  gap <- .doc_gap_for(body)
  bands <- .doc_bands(body, gap)
  if (length(bands) < 1L) return(if (length(old)) old else list())
  bands <- .doc_split_bands_by_cells(bands, parts, gap)
  edges <- c(vapply(bands, function(b) b$x_min, numeric(1)),
             bands[[length(bands)]]$x_max)
  for (cc in old) {
    lo <- .doc_num(cc$x_min, NA_real_); hi <- .doc_num(cc$x_max, NA_real_)
    if (!is.finite(lo) || !is.finite(hi) || hi <= lo) next
    if (any(body$cx >= lo & body$cx < hi)) next
    edges <- c(edges[edges <= lo | edges >= hi], lo, hi)
  }
  edges <- sort(unique(round(edges, 2)))
  cols <- doc_columns_from_edges(edges, old = old)
  if (length(cols) < length(old)) return(old)
  tab2 <- tab; tab2$columns <- cols
  nm <- doc_header_names(input, tab2, tmpl, edges, loc)
  out <- doc_columns_from_edges(edges, old = old, names = nm)
  if (length(out) < length(old)) old else out
}

.DOC_TYPES <- c("auto", "text", "money", "number", "date")

# The companion value is a value, not a substring: .value_from_line returns the matched substring
# verbatim, so "(1,234.56)", "987.65-", "55.00 CR" and "12.34 DR" all arrived with no sign resolved.
# parse_amount() resolves all four.
.doc_money_values <- function(txt) {
  m <- vapply(as.character(txt), function(t) {
    t <- trimws(as.character(t))
    if (is.na(t) || !nzchar(t)) return(NA_character_)
    v <- .value_from_line(t, "money")
    if (is.na(v) || !nzchar(trimws(v))) NA_character_ else v
  }, character(1), USE.NAMES = FALSE)
  out <- rep(NA_real_, length(m))
  ok <- !is.na(m)
  if (any(ok)) out[ok] <- as.numeric(parse_amount(m[ok], "signed")$value)
  out
}

.DOC_NUM_RX <- "\\(?[-+]?[0-9][0-9,]*(?:\\.[0-9]+)?\\)?-?"

.doc_number_values <- function(txt) {
  m <- vapply(as.character(txt), function(t) {
    t <- trimws(as.character(t))
    if (is.na(t) || !nzchar(t)) return(NA_character_)
    v <- regmatches(t, regexpr(.DOC_NUM_RX, t, perl = TRUE))
    if (!length(v) || !nzchar(v)) NA_character_ else v[1]
  }, character(1), USE.NAMES = FALSE)
  out <- rep(NA_real_, length(m))
  ok <- !is.na(m)
  if (any(ok)) out[ok] <- as.numeric(parse_amount(m[ok], "signed")$value)
  out
}

# .DATE_RX matched "2025-04-07" from the third character and returned "25-04-07" - 2007 in one
# reader, 1925 in another. .date_strict round-trips the parse and bounds the year, so ISO or
# nothing comes out. A declared format is the only one tried; otherwise the candidates below.
.DOC_DATE_ISO_RX <- "[0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}"
.DOC_DATE_FORMATS <- c("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%d %b %Y",
                       "%d.%m.%Y", "%Y/%m/%d", "%Y.%m.%d", "%d %B %Y",
                       "%d/%m/%y", "%d-%m-%y", "%d.%m.%y", "%d %b %y")

.doc_date_values <- function(txt, fmt = NULL) {
  raw <- vapply(as.character(txt), function(t) {
    t <- trimws(as.character(t))
    if (is.na(t) || !nzchar(t)) return(NA_character_)
    iso <- regmatches(t, regexpr(.DOC_DATE_ISO_RX, t, perl = TRUE))
    v <- if (length(iso) && nzchar(iso)) iso[1] else .value_from_line(t, "date")
    if (is.na(v) || !nzchar(trimws(v))) NA_character_ else v
  }, character(1), USE.NAMES = FALSE)
  out <- rep(NA_character_, length(raw))
  ok <- which(!is.na(raw))
  if (!length(ok)) return(out)
  fmts <- as.character(fmt %||% character(0))
  fmts <- fmts[!is.na(fmts) & nzchar(trimws(fmts))]
  if (!length(fmts)) fmts <- .DOC_DATE_FORMATS
  s <- .normalise_date_str(raw[ok])
  todo <- seq_along(ok)
  for (f in fmts) {
    if (!length(todo)) break
    v <- suppressWarnings(.date_strict(s[todo], f))
    hit <- which(!is.na(v))
    if (length(hit)) {
      out[ok[todo[hit]]] <- as.character(v[hit])
      todo <- todo[-hit]
    }
  }
  out
}

.doc_value_proto <- function(type)
  if (type %in% c("money", "number")) numeric(0) else character(0)

# Only ever called for a column whose type the person DECLARED: an "auto" column keeps exactly what
# the page says, because a guess landing in a column headed "amount" looks like a fact.
.doc_cell_values <- function(txt, type, fmt = NULL) {
  txt <- as.character(txt %||% character(0))
  num <- type %in% c("money", "number")
  if (!length(txt)) return(.doc_value_proto(type))
  v <- switch(type,
    money  = .doc_money_values(txt),
    number = .doc_number_values(txt),
    date   = .doc_date_values(txt, fmt),
    text   = trimws(txt),
    trimws(txt))         # any other declared type: the page's own words stand
  if (is.null(v) || length(v) != length(txt))
    v <- rep(if (num) NA_real_ else NA_character_, length(txt))
  if (num) as.numeric(v) else {
    v <- as.character(v); v[!is.na(v) & !nzchar(v)] <- NA_character_; v
  }
}

.doc_cell_value <- function(txt, type, fmt = NULL)
  .doc_cell_values(as.character(txt %||% "")[1], type, fmt)[1]

# A$2,000.00 and US$4,000.00 both parse to a number with no room for which money it is, so the
# marker is recorded per column. An alternation matched as bytes, never a bracket expression: in a
# C locale a multibyte character inside [...] is read one byte at a time and eats the byte beside it.
.DOC_CCY_RX <- paste0("[A-Za-z]{0,3}[$]",
                      "|[A-Za-z]{0,3}\u00a3", "|[A-Za-z]{0,3}\u20ac",
                      "|[A-Za-z]{0,3}\u00a5",
                      "|(?<![A-Za-z])[A-Z]{3}(?![A-Za-z])")
.doc_currency_mark <- function(txt) {
  t <- trimws(as.character(txt %||% ""))
  if (!nzchar(t)) return("")
  m <- regmatches(t, regexpr(.DOC_CCY_RX, t, perl = TRUE, useBytes = TRUE))
  if (!length(m) || !nzchar(m)) "" else toupper(trimws(m[1]))
}

# THE CONTRACT: every cell is emitted exactly as the page prints it - nothing dropped for looking
# wrong, nothing repaired, no row discarded for being sparse. What it does instead is measure.
doc_table_rows <- function(input, tab, tmpl = NULL, loc = NULL) {
  cols <- .doc_columns(tab)
  cnames <- .doc_column_names(tab)
  ctypes <- vapply(seq_along(cols), function(i) {
    t <- as.character(.doc_key(cols[[i]], "type") %||% "auto")[1]
    if (is.na(t) || !(t %in% .DOC_TYPES)) "auto" else t
  }, character(1))
  cfmts <- vapply(seq_along(cols), function(i) {
    f <- as.character(.doc_key(cols[[i]], "format") %||%
                      .doc_key(cols[[i]], "date_format") %||% "")[1]
    if (is.na(f)) "" else trimws(f)
  }, character(1))
  loc <- loc %||% doc_locate_table(input, tab, tmpl)
  cols <- .doc_shift_cols(cols, loc$band_shift %||% 0)

  empty_rows <- function() {
    d <- data.frame(page = integer(0), row = integer(0), stringsAsFactors = FALSE)
    for (i in seq_along(cnames)) d[[cnames[i]]] <- character(0)
    for (i in seq_along(cnames)) if (!identical(ctypes[i], "auto"))
      d[[paste0(cnames[i], "__value")]] <- .doc_value_proto(ctypes[i])
    d
  }
  spilled <- data.frame(page = integer(0), y = numeric(0), x = numeric(0),
                        text = character(0), stringsAsFactors = FALSE)
  stops <- data.frame(page = integer(0), y = numeric(0), stringsAsFactors = FALSE)

  if (!length(cols)) {
    return(list(rows = empty_rows(),
                report = .doc_col_report(cnames, ctypes, integer(0), 0L, tab),
                spilled = spilled, stops = stops, row_fill = numeric(0), n_rows = 0L,
                last_page = NA_integer_, last_y = NA_real_,
                n_header = 0L, anchor = loc$anchor, confidence = loc$confidence,
                detail = "this table has no columns drawn on it yet"))
  }

  hdr_need <- tab$anchor$header_text %||% cnames
  n_header_rows <- max(0L, .doc_int(tab$header_rows, 1L))
  out <- list(); fills <- numeric(0); n_header <- 0L; rowno <- 0L
  last_pg <- NA_integer_; last_y <- NA_real_

  for (win in loc$windows) {
    w <- .doc_page_words(input, win$page, tmpl)
    if (is.null(w)) next
    w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
    if (!nrow(w)) next

    lines <- .doc_lines(w, tab$row_tol)
    skip <- max(0L, .doc_int(win$skip, 0L))
    # The row run, for a window with no real end: a table followed to the page bottom swallows what
    # is printed under it. It stops on a vertical gap much bigger than this table's own AND a line
    # filling fewer columns - the gap alone cuts a table at the whitespace before its total row.
    open_end <- isTRUE(win$open_end)
    max_gap <- .doc_num(tab$max_gap, 2.2)
    kept_top <- numeric(0); kept_fill <- integer(0); kept_money <- logical(0)
    prev_left <- NA_real_; prev_i <- 0L
    last_top <- NA_real_
    last_h <- NA_real_
    prev_filled <- 0L
    for (li in seq_along(lines)) {
      d <- lines[[li]]
      # Three ways a line is a heading and not a row: one of the leading `skip` lines, a score
      # against the remembered wording, or its whole text IS this document's heading. The last two
      # apply only at the TOP of a window - in the body they DELETE the line they fire on.
      at_window_top <- !length(kept_top)
      is_header <- li <= skip ||
        (at_window_top &&
         (.doc_header_hit(d, hdr_need) >= 0.6 ||
          (length(loc$header_sig) &&
             !is.null(.doc_find_printed_header(list(d), loc$header_sig)))) &&
         !any(vapply(d$text, function(t) !is.na(.value_from_line(t, "money")),
                     logical(1))))
      if (is_header) { n_header <- n_header + 1L; next }
      if (.doc_is_rule(d)) next

      j <- .doc_col_of(d$cx, cols)
      cells <- rep(NA_character_, length(cols))
      for (k in which(!is.na(j))) {
        add <- trimws(d$text[k])
        cells[j[k]] <- if (is.na(cells[j[k]])) add else paste(cells[j[k]], add)
      }
      n_filled <- sum(!is.na(cells) & nzchar(cells))
      has_money <- any(vapply(d$text, function(t)
        !is.na(.value_from_line(t, "money")), logical(1)))

      # A wrapped cell belongs to the row above and is folded before the stop rule sees it. A wrap
      # never overwrites a figure: a line putting a second figure into a cell that already holds one
      # is the next thing printed, not a continuation.
      clash <- prev_i > 0L && any(vapply(seq_along(cells), function(i) {
        if (is.na(cells[i]) || !nzchar(cells[i])) return(FALSE)
        if (!(ctypes[i] %in% c("money", "number", "date"))) return(FALSE)
        was <- out[[prev_i]][[cnames[i]]]
        !is.null(was) && !is.na(was) && nzchar(was)
      }, logical(1)))

      if (prev_i > 0L && n_filled >= 1L && !clash &&
          .doc_is_wrap(d, prev_left,
                       as.numeric(attr(d, "top")) -
                         (if (is.finite(last_top)) last_top else kept_top[length(kept_top)]),
                       n_filled, length(cols), prev_filled, last_h)) {
        for (i in seq_along(cells)) if (!is.na(cells[i]) && nzchar(cells[i])) {
          was <- out[[prev_i]][[cnames[i]]]
          out[[prev_i]][[cnames[i]]] <-
            if (is.na(was) || !nzchar(was)) cells[i] else .doc_join_wrapped(was, cells[i])
        }
        last_top <- as.numeric(attr(d, "top"))
        last_h <- suppressWarnings(max(as.numeric(d$height), na.rm = TRUE))
        next
      }

      # Decided BEFORE anything about this line is recorded, so a line that ends the table
      # contributes neither a row nor an unclaimed word.
      if (open_end && length(kept_top) >= 2L) {
        gaps <- diff(kept_top); gaps <- gaps[is.finite(gaps) & gaps > 0]
        pitch_k <- if (length(gaps)) stats::median(gaps) else NA_real_
        this_top <- as.numeric(attr(d, "top"))
        gap <- this_top - kept_top[length(kept_top)]
        typical <- stats::median(kept_fill)
        # Column count alone let a heading through, so a FIGURE is the second signal. The floor
        # cannot exceed the table's width, or the first paragraph gap ends every one-column table.
        rows_have_money <- length(kept_money) && mean(kept_money) >= 0.5
        floor_filled <- min(length(cols), max(2, ceiling(typical / 2)))
        looks_wrong <- n_filled < floor_filled ||
                       (rows_have_money && !has_money)
        if (is.finite(pitch_k) && pitch_k > 0 && is.finite(gap) &&
            gap > pitch_k * max_gap && looks_wrong) {
          stops <- rbind(stops, data.frame(page = as.integer(win$page),
                                           y = this_top,
                                           stringsAsFactors = FALSE))
          break
        }
      }

      if (any(is.na(j))) {
        s <- d[is.na(j), , drop = FALSE]
        spilled <- rbind(spilled, data.frame(
          page = as.integer(s$page), y = as.numeric(s$y), x = as.numeric(s$x),
          text = as.character(s$text), stringsAsFactors = FALSE))
      }
      if (all(is.na(j)) || n_filled == 0L) next
      kept_top <- c(kept_top, as.numeric(attr(d, "top")))
      kept_fill <- c(kept_fill, n_filled)
      kept_money <- c(kept_money, has_money)
      rowno <- rowno + 1L
      rec <- list(page = as.integer(win$page), row = as.integer(rowno))
      for (i in seq_along(cnames)) rec[[cnames[i]]] <- cells[i]
      out[[length(out) + 1L]] <- rec
      prev_i <- length(out); prev_left <- min(d$x)
      last_top <- as.numeric(attr(d, "top")); prev_filled <- as.integer(n_filled)
      last_h <- suppressWarnings(max(as.numeric(d$height), na.rm = TRUE))
      last_pg <- as.integer(win$page); last_y <- max(d$y + d$height)
      fills <- c(fills, mean(!is.na(cells) & nzchar(cells)))
    }
  }

  n_header <- max(0L, n_header)
  # On the measured wrong-column read this held the four real closing balances - the figures the
  # table lost, with page and position.
  if (nrow(spilled)) {
    spilled <- spilled[order(spilled$page, spilled$y, spilled$x), , drop = FALSE]
    rownames(spilled) <- NULL
  }
  rows <- if (!length(out)) empty_rows() else {
    df <- do.call(rbind, lapply(out, function(r)
      as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)))
    rownames(df) <- NULL
    for (i in seq_along(cnames)) if (!identical(ctypes[i], "auto"))
      df[[paste0(cnames[i], "__value")]] <-
        .doc_cell_values(df[[cnames[i]]], ctypes[i], cfmts[i])
    df
  }
  filled <- if (!nrow(rows)) integer(0) else vapply(cnames, function(n) {
    v <- rows[[n]]; sum(!is.na(v) & nzchar(v))
  }, integer(1))
  parsed <- if (!nrow(rows)) integer(0) else vapply(seq_along(cnames), function(i) {
    if (identical(ctypes[i], "auto")) return(NA_integer_)
    v <- rows[[paste0(cnames[i], "__value")]]
    if (is.null(v)) NA_integer_ else sum(!is.na(v) & nzchar(as.character(v)))
  }, integer(1))
  multi_token <- if (!nrow(rows)) integer(0) else vapply(seq_along(cnames), function(i) {
    if (identical(ctypes[i], "auto")) return(NA_integer_)
    v <- trimws(as.character(rows[[cnames[i]]]))
    sum(!is.na(v) & nzchar(v) & grepl("[[:space:]]", v))
  }, integer(1))
  ccy <- rep("", length(cnames))
  if (nrow(rows)) for (i in seq_along(cnames)) {
    if (!identical(ctypes[i], "money")) next
    v <- rows[[paste0(cnames[i], "__value")]]
    txt <- as.character(rows[[cnames[i]]])
    seen <- unique(vapply(txt[!is.na(v)], .doc_currency_mark, character(1),
                          USE.NAMES = FALSE))
    seen <- sort(seen[nzchar(seen)])
    ccy[i] <- paste(seen, collapse = ", ")
  }
  dis <- .doc_heading_disagreements(input, tab, tmpl, cols, loc)
  heading <- rep("", length(cnames))
  heading[match(dis$column, cnames)] <- dis$heading
  report <- .doc_col_report(cnames, ctypes, filled, nrow(rows), tab,
                            parsed, multi_token, ccy, heading)

  detail <- if (nrow(stops)) paste0(loc$detail, sprintf(
              "; it stops on page %d, where the rows stop fitting it",
              stops$page[nrow(stops)])) else loc$detail
  bad <- which(report$wrong_kind %in% TRUE)
  if (length(bad) == 1L)
    detail <- paste0(detail, sprintf(
      "; nothing in %s reads as %s, so check that column against the page",
      report$column[bad], .doc_kind_word(report$type[bad])))
  else if (length(bad) > 1L)
    detail <- paste0(detail, sprintf(
      "; nothing in %s reads as the kind of value it was set to hold, so check those columns against the page",
      .doc_and_list(report$column[bad])))
  if (nrow(dis))
    detail <- paste0(detail, sprintf(
      "; on this copy the %s column sits under the heading %s, so this table has gained or lost a column",
      dis$column[1], dis$heading[1]))
  header_budget <- max(n_header_rows, 1L) * max(length(loc$windows), 1L)
  if (n_header > header_budget)
    detail <- paste0(detail, sprintf(
      "; %d lines were skipped as its heading where %d were expected, so check nothing was dropped",
      n_header, header_budget))
  mixed <- report$column[grepl(",", report$currency %||% "", fixed = TRUE)]
  if (length(mixed))
    detail <- paste0(detail, sprintf(
      "; %s carr%s figures in more than one currency",
      .doc_and_list(mixed), if (length(mixed) == 1L) "ies" else "y"))

  list(rows = rows, report = report,
       spilled = spilled, stops = stops, row_fill = fills, n_rows = nrow(rows),
       n_header = n_header, anchor = loc$anchor, confidence = loc$confidence,
       last_page = last_pg, last_y = last_y,
       detail = detail,
       extended_to = loc$extended_to,
       pages = loc$pages, header_rows = n_header_rows)
}

.doc_kind_word <- function(type) switch(as.character(type)[1],
  money = "an amount", number = "a number", date = "a date",
  "the kind of value it was set to hold")

.doc_and_list <- function(x) {
  x <- as.character(x[!is.na(x) & nzchar(x)])
  n <- length(x)
  if (n == 0L) return("")
  if (n == 1L) return(x)
  paste(paste(x[-n], collapse = ", "), "and", x[n])
}

# One row per column: how full it came out, whether that is below this table's threshold, and -
# where a kind was DECLARED - whether what landed in it is that kind. The threshold FLAGS, it never
# drops. fill_rate asks only whether a cell has any text, so `parsed` and `multi_token` come too.
.doc_col_report <- function(cnames, ctypes, filled, n, tab,
                            parsed = NULL, multi_token = NULL, currency = NULL,
                            heading = NULL) {
  min_fill <- .doc_num(tab$min_fill, 0.5)
  cols <- .doc_columns(tab)
  blank_ok <- vapply(seq_along(cnames), function(i)
    isTRUE(cols[[i]]$may_be_blank), logical(1))
  if (!length(cnames))
    return(data.frame(column = character(0), type = character(0),
                      filled = integer(0), rows = integer(0), fill_rate = numeric(0),
                      parsed = integer(0), parse_rate = numeric(0),
                      multi_token = integer(0), currency = character(0),
                      heading = character(0),
                      may_be_blank = logical(0), low_fill = logical(0),
                      wrong_kind = logical(0), wrong_column = logical(0),
                      stringsAsFactors = FALSE))
  filled <- if (length(filled) == length(cnames)) as.integer(filled)
            else rep(0L, length(cnames))
  fit <- function(v) if (length(v) == length(cnames)) as.integer(v)
                     else rep(NA_integer_, length(cnames))
  parsed <- fit(parsed); multi_token <- fit(multi_token)
  currency <- if (length(currency) == length(cnames)) as.character(currency)
              else rep("", length(cnames))
  currency[is.na(currency)] <- ""
  heading <- if (length(heading) == length(cnames)) as.character(heading)
             else rep("", length(cnames))
  heading[is.na(heading)] <- ""
  typed <- !(ctypes %in% "auto")
  parsed[!typed] <- NA_integer_; multi_token[!typed] <- NA_integer_
  rate <- if (n > 0L) filled / n else rep(NA_real_, length(cnames))
  prate <- ifelse(is.na(parsed) | filled == 0L, NA_real_, parsed / pmax(filled, 1L))
  data.frame(column = cnames, type = ctypes, filled = filled,
             rows = rep(as.integer(n), length(cnames)),
             fill_rate = round(rate, 3),
             parsed = parsed, parse_rate = round(prate, 3),
             multi_token = multi_token, currency = currency,
             heading = heading,
             may_be_blank = blank_ok,
             low_fill = !blank_ok & !is.na(rate) & rate < min_fill,
             wrong_kind = !is.na(prate) & prate == 0 & filled > 0L,
             wrong_column = nzchar(heading),
             stringsAsFactors = FALSE)
}

# A pair is TWO boxes: front matter prints a label and its value in every arrangement there is,
# including value first. What is stored is the label's WORDING plus the OFFSET between the boxes, so
# the label is found wherever it moved to. The drawn boxes remain the fallback.
.doc_box_words <- function(w, box) {
  if (is.null(w) || !nrow(w)) return(w[0, , drop = FALSE])
  keep <- w$cx >= .doc_num(box$x_min, -Inf) & w$cx <= .doc_num(box$x_max, Inf) &
          w$cy >= .doc_num(box$y_min, -Inf) & w$cy <= .doc_num(box$y_max, Inf)
  d <- w[keep, , drop = FALSE]
  if (!nrow(d)) return(d)
  d[order(round(d$y / 3), d$x), , drop = FALSE]
}

.doc_find_phrase <- function(lines, phrase) {
  key <- .doc_norm(phrase)
  if (!nzchar(key)) return(NULL)
  for (d in lines) {
    toks <- .doc_norm(d$text)
    n <- length(toks)
    if (!n) next
    for (i in seq_len(n)) {
      acc <- ""
      for (k in i:n) {
        acc <- paste0(acc, toks[k])
        if (grepl(key, acc, fixed = TRUE)) {
          sel <- d[i:k, , drop = FALSE]
          # Trim to the smallest run that still carries the phrase: this box is the anchor the
          # value's offset is measured from, so a box one word too wide moves the value box.
          while (nrow(sel) > 1L &&
                 grepl(key, paste(.doc_norm(sel$text[-1]), collapse = ""), fixed = TRUE))
            sel <- sel[-1, , drop = FALSE]
          return(list(x_min = min(sel$x), x_max = max(sel$x + sel$width),
                      y_min = min(sel$y), y_max = max(sel$y + sel$height),
                      page = sel$page[1]))
        }
        if (nchar(acc) > nchar(key) + 40L) break
      }
    }
  }
  NULL
}

# Which side the value is on. An offset alone is rigid, so the SIDE is stored and searched along.
# where: right|left|below|above; gap: clear distance along it; cross: offset across it.
.doc_pair_rel <- function(lb, vb) {
  n <- function(v, d) { x <- .doc_num(v, d); if (is.na(x)) d else x }
  lx0 <- n(lb$x_min, 0); lx1 <- n(lb$x_max, 0)
  ly0 <- n(lb$y_min, 0); ly1 <- n(lb$y_max, 0)
  vx0 <- n(vb$x_min, 0); vx1 <- n(vb$x_max, 0)
  vy0 <- n(vb$y_min, 0); vy1 <- n(vb$y_max, 0)
  dx_r <- vx0 - lx1; dx_l <- lx0 - vx1
  dy_b <- vy0 - ly1; dy_a <- ly0 - vy1
  same_line <- min(ly1, vy1) - max(ly0, vy0) > 0
  side <- if (dx_r >= 0) "right" else if (dx_l >= 0) "left" else NA_character_
  updown <- if (dy_b >= 0) "below" else if (dy_a >= 0) "above" else NA_character_
  where <- if (!is.na(side) && (same_line || is.na(updown))) side
           else if (!is.na(updown)) updown
           else if (!is.na(side)) side
           else if (abs(vx0 - lx0) >= abs(vy0 - ly0)) "right" else "below"
  gap <- switch(where, right = dx_r, left = dx_l, below = dy_b, above = dy_a, 0)
  cross <- switch(where,
    right = , left  = (vy0 + vy1) / 2 - (ly0 + ly1) / 2,
    below = , above = (vx0 + vx1) / 2 - (lx0 + lx1) / 2, 0)
  list(where = where, gap = round(max(gap, 0), 1), cross = round(cross, 1),
       width = round(max(vx1 - vx0, 0), 1), height = round(max(vy1 - vy0, 0), 1))
}

.doc_pair_where_text <- function(rel) {
  w <- as.character(.doc_key(rel, "where") %||% "")[1]
  switch(w, right = "to the right of it", left = "to the left of it",
         below = "under it", above = "above it", "beside it")
}

.doc_pair_window <- function(hb, rel) {
  gap <- max(.doc_num(.doc_key(rel, "gap"), 0), 0, na.rm = TRUE)
  wdt <- max(.doc_num(.doc_key(rel, "width"), 0), 8, na.rm = TRUE)
  hgt <- max(.doc_num(.doc_key(rel, "height"), 0), 6, na.rm = TRUE)
  lh <- max(.doc_num(hb$y_max, 0) - .doc_num(hb$y_min, 0), hgt, 6)
  reach_x <- gap + wdt * 2 + 90
  reach_y <- gap + hgt * 2 + 26
  switch(as.character(.doc_key(rel, "where") %||% "right"),
    right = list(x_min = hb$x_max - 1, x_max = hb$x_max + reach_x,
                 y_min = hb$y_min - lh * 0.6, y_max = hb$y_max + lh * 0.6),
    left  = list(x_min = hb$x_min - reach_x, x_max = hb$x_min + 1,
                 y_min = hb$y_min - lh * 0.6, y_max = hb$y_max + lh * 0.6),
    below = list(x_min = hb$x_min - wdt * 0.6 - 24, x_max = hb$x_max + wdt + 70,
                 y_min = hb$y_max - 1, y_max = hb$y_max + reach_y),
    above = list(x_min = hb$x_min - wdt * 0.6 - 24, x_max = hb$x_max + wdt + 70,
                 y_min = hb$y_min - reach_y, y_max = hb$y_min + 1),
    NULL)
}

.doc_pair_pick <- function(w, win, where) {
  d <- .doc_box_words(w, win)
  if (is.null(d) || !nrow(d)) return(NULL)
  where <- as.character(where %||% "right")[1]
  d <- d[order(switch(where, right = d$x, left = -d$x, above = -d$y, d$y)), , drop = FALSE]
  seed <- d[1, , drop = FALSE]
  lh <- max(as.numeric(seed$height), 6)
  line <- w[abs(w$cy - as.numeric(seed$cy)) <= lh * 0.6, , drop = FALSE]
  line <- switch(where,
    right = line[line$x + line$width > .doc_num(win$x_min, -Inf), , drop = FALSE],
    left  = line[line$x < .doc_num(win$x_max, Inf), , drop = FALSE],
    line)
  if (!nrow(line)) line <- seed
  line <- line[order(line$x), , drop = FALSE]
  k <- which.min(abs(line$x - as.numeric(seed$x)))
  lim <- max(lh * 1.4, 8)
  lo <- k
  while (lo > 1L && line$x[lo] - (line$x[lo - 1L] + line$width[lo - 1L]) <= lim) lo <- lo - 1L
  hi <- k
  while (hi < nrow(line) &&
         line$x[hi + 1L] - (line$x[hi] + line$width[hi]) <= lim) hi <- hi + 1L
  d <- line[lo:hi, , drop = FALSE]
  box <- list(x_min = min(d$x), x_max = max(d$x + d$width),
              y_min = min(d$y), y_max = max(d$y + d$height))

  # A value broken across two lines inside one word. Narrow on purpose: one line down, only when the
  # run ends on a character a word is broken after, and only for words starting underneath.
  last <- as.character(d$text[nrow(d)])
  if (isTRUE(grepl("[A-Za-z0-9][@/\\\\_=+-]$", last))) {
    nxt <- w[w$cy > box$y_max & w$cy <= box$y_max + lh * 1.8 &
             w$x + w$width > box$x_min - lh * 0.5 &
             w$x < box$x_max + lh * 2, , drop = FALSE]
    if (NROW(nxt)) {
      nxt <- nxt[order(nxt$x), , drop = FALSE]
      keep <- 1L
      while (keep < nrow(nxt) &&
             nxt$x[keep + 1L] - (nxt$x[keep] + nxt$width[keep]) <= lim) keep <- keep + 1L
      nxt <- nxt[seq_len(keep), , drop = FALSE]
      box$x_min <- min(box$x_min, min(nxt$x))
      box$x_max <- max(box$x_max, max(nxt$x + nxt$width))
      box$y_max <- max(box$y_max, max(nxt$y + nxt$height))
    }
  }
  box
}

doc_pairs <- function(input, tmpl) {
  specs <- tmpl$pairs %||% list()
  cols <- c("pair", "label", "value", "raw", "page", "found_by", "matched")
  if (!length(specs)) {
    d <- data.frame(matrix(character(0), 0, length(cols)), stringsAsFactors = FALSE)
    names(d) <- cols; d$page <- integer(0); d$matched <- logical(0)
    return(d)
  }
  npg <- length(input$words %||% list())
  rows <- lapply(names(specs), function(nm) {
    sp <- specs[[nm]]
    lb <- .doc_key(sp, "label"); if (!is.list(lb)) lb <- list()
    vb <- .doc_key(sp, "value"); if (!is.list(vb)) vb <- list()
    vtype <- as.character(.doc_key(sp, "type") %||% "text")[1]
    label_text <- trimws(as.character(.doc_key(sp, "label_text") %||% "")[1])
    if (is.na(label_text)) label_text <- ""
    pg <- .doc_int(.doc_key(vb, "page") %||% .doc_key(lb, "page"), 1L)
    if (is.na(pg) || pg < 1L) pg <- 1L

    if (!is.list(vb) || is.na(.doc_num(vb$x_min))) {
      m <- if (nzchar(label_text))
        tryCatch(match_label(list(any_of = list(label_text), value = vtype),
                             input$pages %||% character(0)), error = function(e) NULL)
        else NULL
      got <- if (!is.null(m) && isTRUE(m$matched)) trimws(m$value %||% "") else ""
      return(data.frame(pair = nm, label = if (nzchar(label_text)) label_text else nm,
                        value = got, raw = if (is.null(m)) "" else trimws(m$raw %||% ""),
                        page = NA_integer_, found_by = "its wording",
                        matched = nzchar(got), stringsAsFactors = FALSE))
    }

    rel <- .doc_key(sp, "where")
    rel <- if (is.list(rel)) rel
           else if (is.character(rel) && nzchar(rel[1]))
             utils::modifyList(.doc_pair_rel(lb, vb), list(where = rel[1]))
           else .doc_pair_rel(lb, vb)

    box <- NULL; found_by <- "its place on the page"; use_page <- pg
    if (nzchar(label_text)) {
      order_pages <- unique(c(pg, seq_len(max(npg, 0L))))
      for (p in order_pages) {
        w <- .doc_page_words(input, p, tmpl)
        if (is.null(w)) next
        hitbox <- .doc_find_phrase(.doc_lines(w), label_text)
        if (is.null(hitbox)) next
        win <- .doc_pair_window(hitbox, rel)
        box <- if (is.null(win)) NULL else .doc_pair_pick(w, win, rel$where)
        if (is.null(box)) {
          dx <- .doc_num(vb$x_min, 0) - .doc_num(lb$x_min, 0)
          dy <- .doc_num(vb$y_min, 0) - .doc_num(lb$y_min, 0)
          wdt <- .doc_num(vb$x_max, 0) - .doc_num(vb$x_min, 0)
          hgt <- .doc_num(vb$y_max, 0) - .doc_num(vb$y_min, 0)
          box <- list(x_min = hitbox$x_min + dx, x_max = hitbox$x_min + dx + wdt,
                      y_min = hitbox$y_min + dy, y_max = hitbox$y_min + dy + hgt)
        }
        found_by <- "its wording"; use_page <- p
        break
      }
    }
    if (is.null(box)) box <- list(x_min = .doc_num(vb$x_min, -Inf),
                                  x_max = .doc_num(vb$x_max, Inf),
                                  y_min = .doc_num(vb$y_min, -Inf),
                                  y_max = .doc_num(vb$y_max, Inf))
    w <- .doc_page_words(input, use_page, tmpl)
    raw <- if (is.null(w)) "" else .doc_join_lines(.doc_box_words(w, box))
    raw <- trimws(raw)
    val <- if (identical(vtype, "text") || !nzchar(raw)) raw
           else { v <- .value_from_line(raw, vtype); if (is.na(v)) "" else v }
    data.frame(pair = nm,
               label = if (nzchar(label_text)) label_text else nm,
               value = val, raw = raw, page = as.integer(use_page),
               found_by = found_by, matched = nzchar(val),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}
