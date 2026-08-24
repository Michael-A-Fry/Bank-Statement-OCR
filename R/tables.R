# tables.R -- the REGION extractor: pull named tables and label/value pairs out of
# a document that is not a bank statement.
#
# WHY THIS EXISTS, AND WHY IT IS NOT THE STATEMENT PARSER.
# The statement parser (R/parse_pdf_table.R) reads ONE table, whose columns run
# the full height of every page, and it judges what it read against a running
# balance. Everything it does well depends on those three facts. The documents
# this file is for break all three: a forty-page report carrying thirty tables of
# different shapes, several to a page, some starting half-way down and ending
# half-way down the page after next, with no balance anywhere to check them
# against. Bolting that onto the statement parser would have put the untested,
# uncheckable case inside the code path that every real conversion runs through.
# So this is a separate engine with its own template mode ("document"), its own
# outputs, and NO route to the Qlik feed -- see write_feed(), which refuses this
# kind outright.
#
# WHAT A TABLE IS HERE.
#   * a START (page, y) and an END (page, y). Both are positions ON a page, not
#     whole pages, because a table can begin below a paragraph and finish above
#     the next heading.
#   * COLUMNS as x-bands that do NOT span the full height of the page -- they are
#     only meaningful between that start and that end.
#   * an ANCHOR: the wording of its header row, and the wording in its first
#     column. TEXT FIRST, POSITION SECOND -- the coordinates are the fallback,
#     never the primary, because the next copy of this document will have moved.
#
# WHAT IT REFUSES TO DO. It does not decide that a number is wrong, because it
# has nothing to check it against. What it does instead is MEASURE and REPORT:
# how it found each table (and how sure it is), what fraction of each column and
# each row it actually filled, and every word inside the table's boundary that no
# column claimed. A silently dropped column is the failure mode this design is
# built to make impossible to miss.
#
# Entry points:
#   doc_locate_table(input, tab, tmpl)      -> where this table is, and how we know
#   doc_table_rows(input, tab, tmpl)        -> its rows + the fill report
#   doc_pairs(input, tmpl)                  -> the label/value pairs
#   extract_document(input, tmpl)           -> all of it (see R/doc_extract.R)

# ---------------------------------------------------------------------------
# Geometry helpers
# ---------------------------------------------------------------------------

# .doc_num(v, dflt) -- one finite number out of whatever YAML handed us.
.doc_num <- function(v, dflt = NA_real_) {
  v <- suppressWarnings(as.numeric(v)[1])
  if (length(v) != 1L || is.na(v) || !is.finite(v)) dflt else v
}

# .doc_int(v, dflt) -- one finite integer, same contract.
.doc_int <- function(v, dflt = NA_integer_) {
  v <- suppressWarnings(as.integer(v)[1])
  if (length(v) != 1L || is.na(v)) dflt else v
}

# .doc_frame(tmpl) -- the point-space a document template's boxes were drawn in.
# Identical idea to pdf_band_frame(): the boxes are absolute coordinates, so a
# page of a different physical size has to be mapped into this frame before they
# mean anything. Defaults to A4, which is what every drawn box will be unless the
# template says otherwise.
.doc_frame <- function(tmpl) {
  list(width  = .doc_num(tmpl$ref_width,  .A4_W),
       height = .doc_num(tmpl$ref_height, .A4_H))
}

# .doc_page_words(input, page, tmpl) -- one page's word boxes, mapped into the
# template's frame, with the centre coordinates every rule below works from.
# Returns NULL when the page has no readable words.
.doc_page_words <- function(input, page, tmpl = NULL) {
  wl <- input$words %||% list()
  if (is.na(page) || page < 1L || page > length(wl)) return(NULL)
  w <- wl[[page]]
  if (is.null(w) || !nrow(w)) return(NULL)
  w <- as.data.frame(w, stringsAsFactors = FALSE)
  if (!is.null(tmpl)) {
    pw <- (input$page_width  %||% rep(NA_real_, length(wl)))[page]
    ph <- (input$page_height %||% rep(NA_real_, length(wl)))[page]
    w <- .words_to_band_frame(w, .doc_frame(tmpl), pw, ph)
  }
  w$text <- as.character(w$text)
  w <- w[!is.na(w$text) & nzchar(trimws(w$text)), , drop = FALSE]
  if (!nrow(w)) return(NULL)
  w$cx <- w$x + w$width / 2
  w$cy <- w$y + w$height / 2
  w$page <- as.integer(page)
  w
}

# .doc_norm(s) -- the comparison form of a piece of wording: lowercase, letters
# and digits only. Every text anchor in this file is compared this way, so a
# heading that gained a colon, a stray space or a different case still matches.
.doc_norm <- function(s) gsub("[^a-z0-9]+", "", tolower(as.character(s %||% "")))

# ---------------------------------------------------------------------------
# Rows: grouping words into visual lines, with the line pitch LEARNED
# ---------------------------------------------------------------------------

# .doc_row_tol(ys, declared) -- how far apart two words' tops may be and still be
# the same row.
#
# WHY IT IS LEARNED AND NOT A CONSTANT. The statement parser's fixed 3pt tolerance
# is right for the one page-shape it reads. Across thirty tables in one report the
# line pitch varies from about 10pt (a dense schedule) to 24pt (a summary set in a
# larger face), and a tolerance that is too big MERGES rows -- two transactions
# reported as one, the exact silent corruption this engine exists to prevent --
# while one that is too small SPLITS a row whose cells sit on slightly different
# baselines. So: group once at the conservative statement tolerance, measure the
# gap between the row tops that produces, and take a fraction of that as the real
# tolerance. The measurement is per table, because that is the scope over which
# the pitch is actually constant.
.doc_pitch <- function(ys) {
  ys <- sort(ys[is.finite(ys)])
  if (length(ys) < 3L) return(NA_real_)
  g <- .group_rows(ys, PARAM_PDF_ROW_TOL)
  tops <- as.numeric(tapply(ys, g, min))
  dif <- diff(sort(tops))
  dif <- dif[is.finite(dif) & dif > 0]
  if (!length(dif)) return(NA_real_)
  p <- stats::median(dif)
  if (!is.finite(p) || p <= 0) NA_real_ else p
}

.doc_row_tol <- function(ys, declared = NULL) {
  d <- .doc_num(declared)
  if (!is.na(d) && d > 0) return(d)
  pitch <- .doc_pitch(ys)
  if (is.na(pitch)) return(as.numeric(PARAM_PDF_ROW_TOL))
  # 0.6 of the pitch: comfortably more than the baseline jitter within one row,
  # comfortably less than the distance to the next one. Floored at the statement
  # tolerance so this can never be LESS forgiving than the proven parser.
  #
  # THE CAP IS 14, AND IT IS THE INTERESTING NUMBER. Baseline jitter inside one
  # printed row is a couple of points; 14 is already generous for that. What the
  # cap protects against is the other direction: a widely-leaded page (a title
  # block set at 40pt) hands back a pitch of 40, and 0.6 of that would swallow a
  # 22pt gap -- merging a label and the value printed under it into one line and
  # losing the pair entirely. A table whose rows genuinely stand further apart
  # than this, or one whose descriptions wrap, sets row_tol explicitly.
  max(as.numeric(PARAM_PDF_ROW_TOL), min(pitch * 0.6, 14))
}

# .doc_lines(w, tol) -- one page's words grouped into visual lines, in reading
# order. Returns a list of data.frames, each the words of one line ordered left to
# right, with attr "top" = the line's highest word top.
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

# .doc_line_text(d) -- a line's words as one string, in reading order.
.doc_line_text <- function(d) paste(d$text, collapse = " ")

# ---------------------------------------------------------------------------
# Anchoring: find the table by its WORDING, fall back to its POSITION
# ---------------------------------------------------------------------------

# .doc_header_hit(d, need) -> fraction of the header wordings present on line `d`.
# Substring on the normalised forms, so "Opening" matches a header cell printed
# "Opening balance", and a header split across two word boxes still matches
# because the whole LINE is what gets searched.
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

# .doc_find_header(lines, need, min_hit) -> list(index, top, hit) for the BEST
# line on the page, or NULL. The best is the highest-scoring line; on a tie the
# FIRST one wins, so a repeated header always resolves to the top of the table
# rather than to a total row that happens to echo the wording.
.doc_find_header <- function(lines, need, min_hit = 0.6) {
  if (!length(lines)) return(NULL)
  hits <- vapply(lines, .doc_header_hit, numeric(1), need = need)
  if (!length(hits) || !any(is.finite(hits))) return(NULL)
  best <- which.max(hits)
  if (!length(best) || hits[best] < min_hit) return(NULL)
  list(index = as.integer(best), top = attr(lines[[best]], "top"),
       hit = as.numeric(hits[best]))
}

# .doc_find_first_column(lines, need, col) -> the top of the first line whose
# FIRST-COLUMN band carries one of the remembered row labels, or NULL. This is
# the second anchor: a table whose header wording changed (or was never printed)
# is still identifiable by the rows it always starts with.
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

# The four ways a table can be found, WORST last, each with the confidence the
# result is reported at. These are not probabilities; they are an ordering the
# screen turns into words ("found by its heading" / "read from where it was on the
# example"), and the whole point of keeping them distinct is that the last two
# deserve a look and the first does not.
.DOC_ANCHOR_RANK <- c(header = 1L, first_column = 2L,
                      position_same_page = 3L, position_other_page = 4L)
.doc_anchor_confidence <- function(anchor, hit = NA_real_) {
  switch(anchor,
    header             = if (is.na(hit)) 0.9 else max(0.6, min(1, hit)),
    first_column       = 0.7,
    position_same_page = 0.4,
    position_other_page = 0.2,
    0.2)
}

# doc_locate_table(input, tab, tmpl) -> list(
#   pages, windows, anchor, confidence, detail, extended_to, header_tops)
#
# `windows` is one entry per page the table covers: list(page, y_min, y_max),
# in the template's frame. Rows are only ever read inside these windows, which is
# what makes "several tables on one page" work at all.
doc_locate_table <- function(input, tab, tmpl = NULL) {
  npg <- .doc_int(input$page_count, length(input$words %||% list()))
  if (is.na(npg) || npg < 1L) npg <- length(input$words %||% list())
  p0 <- .doc_int(tab$start$page, 1L); if (is.na(p0) || p0 < 1L) p0 <- 1L
  p1 <- .doc_int(tab$end$page, p0);   if (is.na(p1) || p1 < p0) p1 <- p0
  p0 <- min(p0, max(npg, 1L)); p1 <- min(p1, max(npg, 1L))

  band <- tab$band %||% list()
  band_top <- .doc_num(band$y_min, 0)
  band_bot <- .doc_num(band$y_max, .doc_frame(tmpl)$height)
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
      # The remembered rows are the table's BODY, so back up over the header rows
      # to where the table actually starts -- one line of pitch per header row.
      pitch <- .doc_pitch(if (length(lines0))
                            unlist(lapply(lines0, function(d) d$y)) else numeric(0))
      if (is.na(pitch)) pitch <- as.numeric(PARAM_PDF_ROW_TOL) * 4
      y_start <- fc$top - pitch * n_header_rows - 1
      start_at_header <- n_header_rows > 0L
    } else {
      anchor <- if (identical(npg, .doc_int(tab$doc_pages, npg)))
                  "position_same_page" else "position_other_page"
      y_start <- y_start_declared
      # The declared start is the top of the table as it was DRAWN, which includes
      # its header -- so the same number of leading lines are headers here too.
      start_at_header <- n_header_rows > 0L
    }
  }

  # A found header on the start page tells us what the SAME header looks like on
  # a continuation page, which is the only reliable way to know a page is more of
  # this table rather than the start of the next one.
  windows <- list(); header_tops <- list()
  for (p in seq.int(p0, p1)) {
    y_min <- if (p == p0) y_start else band_top
    y_max <- if (p == p1) y_end_declared else band_bot
    # `skip` = how many leading lines of THIS window are the header. It is the
    # declared header_rows wherever the window opens at a header, and zero where
    # the window opens mid-table (a continuation page that does not repeat it).
    skip <- if (p == p0) (if (start_at_header) n_header_rows else 0L) else 0L
    if (p > p0) {
      lp <- .doc_lines(.doc_page_words(input, p, tmpl), tab$row_tol)
      hp <- .doc_find_header(lp, need_hdr)
      if (!is.null(hp)) {
        y_min <- hp$top; header_tops[[as.character(p)]] <- hp$top
        skip <- n_header_rows
      }
    } else if (!is.null(h)) header_tops[[as.character(p)]] <- h$top
    windows[[length(windows) + 1L]] <- list(page = as.integer(p),
                                            y_min = y_min, y_max = y_max,
                                            skip = as.integer(skip))
  }

  # FOLLOW: the same table, longer on this copy than on the example.
  #
  # A template drawn on a document whose schedule ran to page 5 will silently
  # TRUNCATE the same schedule when it runs to page 7 -- and truncation is
  # invisible: the rows that came out all look right. So a table whose declared
  # end is the BOTTOM of its last page keeps going while the following pages
  # repeat its header, and stops at the first page that does not. The extension is
  # recorded (`extended_to`) and reported, never silent, because the opposite
  # mistake -- swallowing the table that follows -- is just as bad.
  extended_to <- NA_integer_
  if (!isFALSE(tab$follow) && p1 < npg && y_end_declared >= band_bot - 1) {
    p <- p1 + 1L
    while (p <= npg) {
      lp <- .doc_lines(.doc_page_words(input, p, tmpl), tab$row_tol)
      hp <- .doc_find_header(lp, need_hdr, min_hit = 0.75)
      if (is.null(hp)) break
      windows[[length(windows) + 1L]] <- list(page = as.integer(p),
                                              y_min = hp$top, y_max = band_bot,
                                              skip = as.integer(n_header_rows))
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
  if (!is.na(extended_to))
    detail <- paste0(detail, sprintf("; it carries on to page %d here", extended_to))
  list(pages = pages, windows = windows, anchor = anchor,
       confidence = .doc_anchor_confidence(anchor, hit),
       detail = detail, extended_to = extended_to, header_tops = header_tops)
}

# ---------------------------------------------------------------------------
# Columns
# ---------------------------------------------------------------------------

# .doc_columns(tab) -> the column specs as a plain list, whichever way YAML gave
# them (a sequence of mappings, or a mapping keyed by name).
.doc_columns <- function(tab) {
  cols <- tab$columns %||% list()
  if (!length(cols)) return(list())
  if (!is.null(names(cols)) && all(nzchar(names(cols))) && is.list(cols[[1]]) &&
      is.null(cols[[1]]$name)) {
    cols <- lapply(names(cols), function(n) { c(list(name = n), cols[[n]]) })
  }
  lapply(cols, function(c) if (is.list(c)) c else list(name = as.character(c)))
}

# .doc_column_names(tab) -- the column names as they will appear in the output,
# made safe and unique. `page` and `row` are the reader's own columns, and a
# duplicate name would make one column silently overwrite another when the rows
# are assembled -- so both are renamed rather than allowed to collide.
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

# .doc_col_of(cx, cols) -> which column each x-centre falls in, NA for none.
# Bands are half-open on the right so two touching bands cannot both claim a word.
.doc_col_of <- function(cx, cols) {
  out <- rep(NA_integer_, length(cx))
  for (j in seq_along(cols)) {
    lo <- .doc_num(cols[[j]]$x_min, -Inf); hi <- .doc_num(cols[[j]]$x_max, Inf)
    hit <- is.na(out) & cx >= lo & cx < hi
    out[hit] <- j
  }
  # The last band takes its own right edge, so a word sitting exactly on the
  # outermost boundary is kept rather than reported as spilled.
  if (length(cols)) {
    j <- length(cols)
    hi <- .doc_num(cols[[j]]$x_max, Inf)
    out[is.na(out) & cx == hi] <- j
  }
  out
}

# ---------------------------------------------------------------------------
# Cell values
# ---------------------------------------------------------------------------

.DOC_TYPES <- c("auto", "text", "money", "number", "date")

# .doc_cell_value(txt, type) -> the machine-readable form of a cell, or NA.
# ONLY ever called for a column whose type the person DECLARED: an undeclared
# ("auto") column keeps exactly what the page says and nothing is inferred from
# it, because a guess that lands in a spreadsheet column headed "amount" is
# indistinguishable from a fact.
.doc_cell_value <- function(txt, type) {
  txt <- trimws(as.character(txt %||% ""))
  if (!nzchar(txt)) return(NA_character_)
  v <- switch(type,
    money  = .value_from_line(txt, "money"),
    date   = .value_from_line(txt, "date"),
    number = { m <- regmatches(txt, regexpr("[-+]?[0-9][0-9,]*(\\.[0-9]+)?",
                                            txt, perl = TRUE))
               if (length(m) && nzchar(m)) m[1] else NA_character_ },
    text   = txt,
    txt)                 # any other declared type: the page's own words stand
  # switch() with no matching branch returns NULL, and a NULL assigned into the
  # row list would DELETE that field -- leaving one record narrower than the rest
  # and breaking the rbind that assembles them. Always a length-1 character.
  if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1]
}

# ---------------------------------------------------------------------------
# The table reader
# ---------------------------------------------------------------------------

# doc_table_rows(input, tab, tmpl, loc) -> list(
#   rows      the table as a data.frame: page, row, one column per declared column
#             (plus <name>__value for any column with a declared type)
#   report    one row per column: fill rate, whether it was flagged, its type
#   spilled   every word inside the table's boundary that no column claimed
#   row_fill  the per-row fill rate, in row order
#   n_rows / n_header / anchor / confidence / detail
#
# THE CONTRACT. Every cell is emitted EXACTLY as the page prints it. Nothing is
# dropped for looking wrong, nothing is repaired, and no row is discarded for
# being sparse -- a total row with four empty cells is a real row of the document
# and this engine has no basis on which to disagree with it. What the reader does
# instead is measure: the fill rate per column and per row, and the words its
# columns did not claim. Those two numbers are the whole quality signal here.
doc_table_rows <- function(input, tab, tmpl = NULL, loc = NULL) {
  cols <- .doc_columns(tab)
  cnames <- .doc_column_names(tab)
  ctypes <- vapply(seq_along(cols), function(i) {
    t <- as.character(cols[[i]]$type %||% "auto")[1]
    if (!(t %in% .DOC_TYPES)) "auto" else t
  }, character(1))
  loc <- loc %||% doc_locate_table(input, tab, tmpl)

  empty_rows <- function() {
    d <- data.frame(page = integer(0), row = integer(0), stringsAsFactors = FALSE)
    for (i in seq_along(cnames)) d[[cnames[i]]] <- character(0)
    for (i in seq_along(cnames)) if (!identical(ctypes[i], "auto"))
      d[[paste0(cnames[i], "__value")]] <- character(0)
    d
  }
  spilled <- data.frame(page = integer(0), y = numeric(0), x = numeric(0),
                        text = character(0), stringsAsFactors = FALSE)

  if (!length(cols)) {
    return(list(rows = empty_rows(),
                report = .doc_col_report(cnames, ctypes, integer(0), 0L, tab),
                spilled = spilled, row_fill = numeric(0), n_rows = 0L,
                n_header = 0L, anchor = loc$anchor, confidence = loc$confidence,
                detail = "this table has no columns drawn on it yet"))
  }

  hdr_need <- tab$anchor$header_text %||% cnames
  n_header_rows <- max(0L, .doc_int(tab$header_rows, 1L))
  out <- list(); fills <- numeric(0); n_header <- 0L; rowno <- 0L

  for (win in loc$windows) {
    w <- .doc_page_words(input, win$page, tmpl)
    if (is.null(w)) next
    # A word belongs to the window when its CENTRE is inside it, so a line sitting
    # exactly on the boundary lands on one side of it and not on both.
    w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
    if (!nrow(w)) next

    lines <- .doc_lines(w, tab$row_tol)
    skip <- max(0L, .doc_int(win$skip, 0L))
    for (li in seq_along(lines)) {
      d <- lines[[li]]
      # THE HEADER, TWO WAYS. Positionally: the leading lines of a window that
      # opens at the table's top (see doc_locate_table) -- this is what handles a
      # two-row header, which no wording rule can see. By wording: a header
      # repeated further down, on a continuation page whose top we did not find.
      # The wording rule refuses to fire on a line carrying money, so a totals row
      # that echoes the column names is kept as the row of data it is.
      is_header <- li <= skip ||
        (.doc_header_hit(d, hdr_need) >= 0.6 &&
         !any(vapply(d$text, function(t) !is.na(.value_from_line(t, "money")),
                     logical(1))))
      if (is_header) { n_header <- n_header + 1L; next }

      j <- .doc_col_of(d$cx, cols)
      # Words inside the table's boundary that NO column claimed. Recorded, never
      # silently dropped: a column band drawn a few points too narrow loses a
      # whole column of figures and every remaining row still looks perfect, so
      # this list is the only thing standing between that and a wrong answer.
      if (any(is.na(j))) {
        s <- d[is.na(j), , drop = FALSE]
        spilled <- rbind(spilled, data.frame(page = s$page, y = s$y, x = s$x,
                                             text = s$text, stringsAsFactors = FALSE))
      }
      if (all(is.na(j))) next
      cells <- rep(NA_character_, length(cols))
      for (k in which(!is.na(j))) {
        add <- trimws(d$text[k])
        cells[j[k]] <- if (is.na(cells[j[k]])) add else paste(cells[j[k]], add)
      }
      if (!any(!is.na(cells) & nzchar(cells))) next
      rowno <- rowno + 1L
      rec <- list(page = as.integer(win$page), row = as.integer(rowno))
      for (i in seq_along(cnames)) rec[[cnames[i]]] <- cells[i]
      for (i in seq_along(cnames)) if (!identical(ctypes[i], "auto"))
        rec[[paste0(cnames[i], "__value")]] <- .doc_cell_value(cells[i], ctypes[i])
      out[[length(out) + 1L]] <- rec
      fills <- c(fills, mean(!is.na(cells) & nzchar(cells)))
    }
  }

  # The FIRST header row is expected and is not evidence of anything; extra ones
  # are the continuations, which is worth saying out loud.
  n_header <- max(0L, n_header)
  rows <- if (!length(out)) empty_rows() else {
    df <- do.call(rbind, lapply(out, function(r)
      as.data.frame(r, stringsAsFactors = FALSE, check.names = FALSE)))
    rownames(df) <- NULL
    df
  }
  filled <- if (!nrow(rows)) integer(0) else vapply(cnames, function(n) {
    v <- rows[[n]]; sum(!is.na(v) & nzchar(v))
  }, integer(1))

  list(rows = rows,
       report = .doc_col_report(cnames, ctypes, filled, nrow(rows), tab),
       spilled = spilled, row_fill = fills, n_rows = nrow(rows),
       n_header = n_header, anchor = loc$anchor, confidence = loc$confidence,
       detail = loc$detail, extended_to = loc$extended_to,
       pages = loc$pages, header_rows = n_header_rows)
}

# .doc_col_report(names, types, filled, n, tab) -> one row per column:
# how full it came out, and whether that is below this table's threshold.
#
# A column can be legitimately empty -- the middle columns of a totals row, a
# "notes" column used twice in forty rows -- so the threshold FLAGS, it never
# drops, and a column marked `may_be_blank` is exempt from it entirely. The
# default 0.5 is a starting point to be tuned per table, which is why it lives on
# the table and not in params.R.
.doc_col_report <- function(cnames, ctypes, filled, n, tab) {
  min_fill <- .doc_num(tab$min_fill, 0.5)
  cols <- .doc_columns(tab)
  blank_ok <- vapply(seq_along(cnames), function(i)
    isTRUE(cols[[i]]$may_be_blank), logical(1))
  if (!length(cnames))
    return(data.frame(column = character(0), type = character(0),
                      filled = integer(0), rows = integer(0), fill_rate = numeric(0),
                      may_be_blank = logical(0), low_fill = logical(0),
                      stringsAsFactors = FALSE))
  filled <- if (length(filled) == length(cnames)) as.integer(filled)
            else rep(0L, length(cnames))
  rate <- if (n > 0L) filled / n else rep(NA_real_, length(cnames))
  data.frame(column = cnames, type = ctypes, filled = filled,
             rows = rep(as.integer(n), length(cnames)),
             fill_rate = round(rate, 3),
             may_be_blank = blank_ok,
             low_fill = !blank_ok & !is.na(rate) & rate < min_fill,
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Label / value pairs -- TWO drawn boxes, not one
# ---------------------------------------------------------------------------

# WHY TWO BOXES. The front matter of these documents prints a label and its value
# in every arrangement there is: side by side, one under the other, the value
# inside a ruled panel away to the right, and -- the awkward one -- the value
# FIRST with its label after it. A single box round the value cannot survive any
# of that moving, and a label-only matcher (the existing mode: fields) cannot
# express "the value is the thing 120pt to the right and 4pt down".
#
# So a pair is two boxes, and what is stored is the label's WORDING plus the
# OFFSET between the boxes. On the next document the label is found by its
# wording wherever it has moved to, and the same offset is applied from there.
# Direction never has to be named, because an offset already carries it.
# The declared boxes remain the fallback for when the wording is not found.

# .doc_box_words(w, box) -- the words whose centres lie in a box, reading order.
.doc_box_words <- function(w, box) {
  if (is.null(w) || !nrow(w)) return(w[0, , drop = FALSE])
  keep <- w$cx >= .doc_num(box$x_min, -Inf) & w$cx <= .doc_num(box$x_max, Inf) &
          w$cy >= .doc_num(box$y_min, -Inf) & w$cy <= .doc_num(box$y_max, Inf)
  d <- w[keep, , drop = FALSE]
  if (!nrow(d)) return(d)
  d[order(round(d$y / 3), d$x), , drop = FALSE]
}

# .doc_find_phrase(lines, phrase) -> the bounding box of the words that make up
# `phrase` on this page, or NULL. Matched on the normalised line text, then
# narrowed to the words that actually contributed, so the box is the LABEL's box
# and not the whole line's.
.doc_find_phrase <- function(lines, phrase) {
  key <- .doc_norm(phrase)
  if (!nzchar(key)) return(NULL)
  for (d in lines) {
    toks <- .doc_norm(d$text)
    n <- length(toks)
    if (!n) next
    # Slide a window over the line's words and take the shortest run whose
    # concatenation contains the phrase. Words are compared normalised, so
    # punctuation between label and value ("Prepared for:") does not matter.
    for (i in seq_len(n)) {
      acc <- ""
      for (k in i:n) {
        acc <- paste0(acc, toks[k])
        if (grepl(key, acc, fixed = TRUE)) {
          sel <- d[i:k, , drop = FALSE]
          # Trim to the SMALLEST run of words that still carries the phrase. The
          # outer loop already stops at the earliest start and the shortest end,
          # but a phrase can begin part-way into the first word it swept up
          # ("for" found inside "Prepared for"), and the box returned here is the
          # anchor the value's offset is measured from -- a box one word too wide
          # moves the value box by exactly that word.
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

# doc_pairs(input, tmpl) -> data.frame(pair, label, value, raw, page, found_by,
#                                      matched)
# `found_by` is "its wording" (the label was located and the offset applied) or
# "its place on the page" (the wording was not found and the drawn box was read).
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
    lb <- sp$label %||% list(); vb <- sp$value %||% list()
    vtype <- as.character(sp$type %||% "text")[1]
    label_text <- as.character(sp$label_text %||% "")[1]
    pg <- .doc_int(vb$page %||% lb$page, 1L); if (is.na(pg) || pg < 1L) pg <- 1L

    box <- NULL; found_by <- "its place on the page"; use_page <- pg
    if (nzchar(label_text)) {
      # Search the declared page first, then every other page: the front matter of
      # a longer copy of the same document does move.
      order_pages <- unique(c(pg, seq_len(max(npg, 0L))))
      for (p in order_pages) {
        w <- .doc_page_words(input, p, tmpl)
        if (is.null(w)) next
        hitbox <- .doc_find_phrase(.doc_lines(w), label_text)
        if (is.null(hitbox)) next
        # The offset between the two DRAWN boxes, applied from where the label
        # actually is. Direction is carried by the sign, so "value above label"
        # needs no special case.
        dx <- .doc_num(vb$x_min, 0) - .doc_num(lb$x_min, 0)
        dy <- .doc_num(vb$y_min, 0) - .doc_num(lb$y_min, 0)
        wdt <- .doc_num(vb$x_max, 0) - .doc_num(vb$x_min, 0)
        hgt <- .doc_num(vb$y_max, 0) - .doc_num(vb$y_min, 0)
        box <- list(x_min = hitbox$x_min + dx, x_max = hitbox$x_min + dx + wdt,
                    y_min = hitbox$y_min + dy, y_max = hitbox$y_min + dy + hgt)
        found_by <- "its wording"; use_page <- p
        break
      }
    }
    if (is.null(box)) box <- list(x_min = .doc_num(vb$x_min, -Inf),
                                  x_max = .doc_num(vb$x_max, Inf),
                                  y_min = .doc_num(vb$y_min, -Inf),
                                  y_max = .doc_num(vb$y_max, Inf))
    w <- .doc_page_words(input, use_page, tmpl)
    raw <- if (is.null(w)) "" else paste(.doc_box_words(w, box)$text, collapse = " ")
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
