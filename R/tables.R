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
  # .doc_key, not $: `tmpl` is an optional argument that several callers pass
  # positionally, and propose_pairs(input, 1L) -- which reads as "page 1" and is
  # not -- put a bare integer here. `1L$ref_width` is an ERROR, so a caller's slip
  # became a crash in the middle of an extraction instead of a default frame.
  list(width  = .doc_num(.doc_key(tmpl, "ref_width"),  .A4_W),
       height = .doc_num(.doc_key(tmpl, "ref_height"), .A4_H))
}

# .doc_npages(input) -- how many pages this document has.
#
# read_pdf() puts the count at input$page_count; read_input(), which is what the
# app and the front door actually call, moves it to input$meta$page_count and
# leaves the top level empty. Reading only one of them means the count is right
# in the tests and NULL in production, which is the worst way round.
.doc_npages <- function(input) {
  n <- .doc_int(.doc_key(input, "page_count"))
  if (is.na(n)) n <- .doc_int(.doc_key(.doc_key(input, "meta"), "page_count"))
  if (is.na(n) || n < 1L) n <- length(.doc_key(input, "words") %||% list())
  as.integer(n)
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
  # A LONE VERTICAL BAR IS A BORDER, not a value. A condensed table prints one
  # between every pair of columns, and it arrives as an ordinary word box: it ends
  # up inside the cell beside it ("| 1,240.55" was measured), and it fills the
  # gutter that would otherwise separate the two columns. Dropping it here, at the
  # one point both the proposer and the reader come through, does both jobs at
  # once -- the columns separate on real whitespace and the cells hold only what
  # the document says. Only a bar ON ITS OWN: "A|B" is data.
  w <- w[!is.na(w$text) & nzchar(trimws(w$text)) &
           !grepl("^\\|+$", trimws(w$text)), , drop = FALSE]
  if (!nrow(w)) return(NULL)
  w$cx <- w$x + w$width / 2
  w$cy <- w$y + w$height / 2
  w$page <- as.integer(page)
  w
}

# .doc_key(x, k) -- x[[k]], and ONLY x[[k]].
#
# R's `$` partial-matches on lists. A pair spec carries `label_text`, so `sp$label`
# on a pair that has no label BOX silently returns the label's WORDING -- a
# character vector -- and the next line, `lb$page`, dies with "$ operator is
# invalid for atomic vectors". That is the whole extraction gone, on a template
# whose only sin was describing a value by its wording instead of by two boxes.
#
# It is not hypothetical and it is not confined to this pair: every one of these
# structures comes out of YAML, where a key can be added at any time, and the day
# somebody adds `page_hint` beside `page` the same trap opens under `$page`. So
# the fields of a spec are read by exact name, here, once.
.doc_key <- function(x, k) if (is.list(x) && k %in% names(x)) x[[k]] else NULL

# .doc_value_kind(v) -- money, date or text, decided by READING the value.
#
# THE MATCH HAS TO BE THE WHOLE VALUE. Both matchers find a pattern ANYWHERE in a
# string, which is right when pulling a figure off a line ("Closing balance
# $875.20") and wrong when deciding what a value IS. A client reference of
# "NW-99-9999-9999" contains "99-9999-9999", which is date-shaped -- so it was
# typed as a date, and the extractor then returned the date-shaped fragment and
# dropped the "NW-". A reference silently two characters short is exactly the kind
# of wrong answer that looks right.
.doc_value_kind <- function(v) {
  v <- trimws(as.character(v %||% ""))
  if (!nzchar(v)) return("text")
  m <- .value_from_line(v, "money")
  if (!is.na(m) && identical(trimws(m), v)) return("money")
  d <- .value_from_line(v, "date")
  if (!is.na(d) && identical(trimws(d), v)) return("date")
  "text"
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

# .doc_is_wrap(d, prev_left, gap, pitch) -- is this line the TAIL of the row above
# it rather than a row of its own?
#
# A description too long for its column wraps onto a second physical line, and
# that line has no date, no reference and no amount -- just more words, indented
# under the column it belongs to. Three facts identify it and all three are
# needed: it starts to the RIGHT of where the rows start, it sits within about one
# line of the row above, and it fills only one column. A genuinely sparse row (a
# sub-item, a continuation of a schedule with its own amount) fills two or more,
# so it is emitted as the row it is.
#
# Getting this wrong in the other direction is what a tighter row tolerance costs:
# once the wrap is correctly a separate LINE, a reader with no fold turns a
# four-row table into no table at all, because the one-cell line breaks the run.
.doc_is_wrap <- function(d, prev_left, gap, pitch, n_filled = 1L) {
  is.finite(prev_left) && is.finite(gap) && is.finite(pitch) &&
    n_filled <= 1L && gap > 0 && gap <= pitch * 1.4 && min(d$x) > prev_left + 4
}

# .doc_is_rule(d) -- is this line a printed RULE rather than a row of data?
#
# Plenty of reports draw their table borders with CHARACTERS rather than vector
# strokes: "+--------+--------+", a row of underscores under a total, a line of
# dots leading to a page number. Those arrive from pdf_data() as ordinary word
# boxes, and they do two kinds of damage: a rule spanning the full width merges
# every column band into one (measured: a bordered table proposed ZERO columns),
# and a rule between rows breaks the run of lines that identifies a table.
#
# A rule is a line whose text, with the spaces taken out, is made ONLY of the
# characters used to draw one, and is long enough not to be a stray dash in a
# cell. Anything with a letter or a digit in it is data.
.doc_is_rule <- function(d) {
  t <- gsub("[[:space:]]", "", paste(as.character(d$text), collapse = ""))
  nchar(t) >= 3L && !grepl("[^-=_+|~.]", t)
}

.doc_row_tol <- function(ys, declared = NULL) {
  d <- .doc_num(declared)
  if (!is.na(d) && d > 0) return(d)
  # THE LOWER QUARTILE, NOT THE MEDIAN, and this is the whole point of the
  # function. A table whose row heights alternate -- 10pt, 26pt, 10pt, 26pt, which
  # is what a schedule with a blank line between pairs looks like -- has a median
  # gap of 26. Six tenths of that is 15.6, which SWALLOWS every 10pt gap: measured
  # on the uneven-rows fixture, six rows came back as four, with "R2 R3" and
  # "20.00 30.00" in single cells. Two rows silently merged is the worst thing
  # this engine can do. The lower quartile tracks the TIGHTEST spacing the table
  # actually uses, so the tolerance is safe for every row rather than for the
  # average one, and on an evenly-set table the two are the same number.
  pitch <- .doc_pitch(ys, q = 0.25)
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
  npg <- .doc_npages(input)
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
    # OPEN-ENDED means "we do not actually know where this stops": the window runs
    # to the bottom of the page because the table carries on past it, not because
    # anyone said it ends there. Those are the windows doc_table_rows() has to
    # stop by watching the rows themselves (see the row-run rule); a window with a
    # real declared end is exact and is left alone.
    windows[[length(windows) + 1L]] <- list(page = as.integer(p),
                                            y_min = y_min, y_max = y_max,
                                            skip = as.integer(skip),
                                            open_end = (p != p1) || y_max >= band_bot - 1)
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
                                              skip = as.integer(n_header_rows),
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
# Columns as EDGES -- what the builder actually manipulates
# ---------------------------------------------------------------------------
#
# A table's columns tile its width: they meet, they do not overlap, and there is
# no space between them. So the thing a person is really choosing is not N boxes
# but the N-1 DIVIDERS between them, plus the outer edge on each side.
#
# That reframing is worth more than it looks. Dragging seven boxes to define a
# seven-column table is seven drags, each of which can be a few points wrong in
# two directions -- and a gap loses a column of figures while an overlap deletes
# one (see .doc_table_problems). Clicking six dividers is six clicks, each of
# which cannot produce either fault, because the edges are shared by construction.
#
# doc_column_edges(tab)        columns   -> edges
# doc_columns_from_edges(...)  edges     -> columns
# doc_edge_click(edges, x)     one click -> the edges it should produce

# doc_column_edges(tab) -> the sorted edge positions of a table's columns:
# left edge, every divider, right edge. NULL when it has no columns yet.
doc_column_edges <- function(tab) {
  cols <- .doc_columns(tab)
  if (!length(cols)) return(NULL)
  lo <- vapply(cols, function(cc) .doc_num(cc$x_min, NA_real_), numeric(1))
  hi <- vapply(cols, function(cc) .doc_num(cc$x_max, NA_real_), numeric(1))
  if (any(is.na(lo)) || any(is.na(hi))) return(NULL)
  o <- order(lo)
  sort(unique(round(c(lo[o], hi[o[length(o)]]), 2)))
}

# doc_columns_from_edges(edges, old, names) -> the column list those edges make.
#
# EVERY COLUMN THAT DID NOT MOVE KEEPS WHAT IT WAS GIVEN. Rebuilding the whole
# list from scratch on every click would throw away the name and the kind a person
# had already set on the six columns they were happy with, which makes correcting
# the seventh feel like starting again. A column whose two edges are unchanged is
# the same column; anything else is new and takes its name from the header row.
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

# doc_edge_click(edges, x, tol) -> the edges after clicking the page at x.
#
# ONE GESTURE, FOUR MEANINGS, none of them ambiguous:
#   on a divider          remove it        (the two columns become one)
#   on an outer edge      move it there    (the table gets wider or narrower)
#   outside the table     move the nearer outer edge out to the click
#   anywhere else inside  add a divider    (one column becomes two)
#
# "On" means within `tol` points, which is what makes removing a divider possible
# with a mouse at all. Removing takes priority over adding, so a click that is
# nearly on a divider never quietly creates a three-point column beside it.
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

# doc_header_names(input, tab, tmpl) -> a name for each column, read from the
# table's own header row. Called after every edge change, so the names follow the
# columns instead of having to be typed: split a column and the two halves are
# named from the two header cells that were in it.
doc_header_names <- function(input, tab, tmpl = NULL, edges = NULL) {
  edges <- edges %||% doc_column_edges(tab)
  if (is.null(edges) || length(edges) < 2L) return(character(0))
  n <- length(edges) - 1L
  out <- rep("", n)
  loc <- tryCatch(doc_locate_table(input, tab, tmpl), error = function(e) NULL)
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
      out[j] <- trimws(gsub("\\s+", " ", paste(sel$text, collapse = " ")))
    }
  }
  out
}

# doc_auto_end(input, tab, tmpl) -> list(page, y): where this table stops, worked
# out by READING it rather than by a second guess.
#
# Somebody who has just said where a table starts and what its columns are should
# not then have to hunt for where it ends -- that is the tool's job, and the tool
# already knows: it opens the table's end right up, reads it, and reports the
# bottom of the last row it kept. The stop rule that ends an open-ended window
# (see doc_table_rows) is what decides, so the end offered here is exactly the end
# the extraction would use. It is a PROPOSAL: it can be wrong, it is drawn on the
# page, and one click moves it.
doc_auto_end <- function(input, tab, tmpl = NULL) {
  probe <- tab
  # OPEN THE END OF THE START PAGE, AND FOLLOW -- do not simply point the end at
  # the last page of the document. Measured: pointing it at page 7 read pages 6
  # and 7 as well, because a middle window covers its whole page whether or not
  # the table is on it, so a four-row summary came back with 103 rows. Following
  # asks the right question one page at a time: does the NEXT page repeat this
  # table's header? The stop rule ends it within a page; the follow rule ends it
  # between pages.
  probe$follow <- TRUE
  probe$end <- list(page = .doc_int(tab$start$page, 1L),
                    y = .doc_frame(tmpl)$height)
  r <- tryCatch(doc_table_rows(input, probe, tmpl), error = function(e) NULL)
  if (is.null(r) || is.na(r$last_page) || is.na(r$last_y)) return(tab$end)
  list(page = as.integer(r$last_page), y = round(as.numeric(r$last_y) + 2, 1))
}

# doc_columns_from_box(input, box, tmpl) -> the columns a HEADER ROW implies.
#
# The header row is the one line on a table that says where its columns are, and
# a person can point at it in one gesture. Every cell of it becomes a column, and
# the boundary between two columns is the middle of the white space between two
# header cells -- which is where it belongs, and is what the eye would pick too.
doc_columns_from_box <- function(input, box, tmpl = NULL) {
  pg <- .doc_int(.doc_key(box, "page"), 1L)
  w <- .doc_page_words(input, pg, tmpl)
  if (is.null(w)) return(list())
  sel <- w[w$cx >= .doc_num(box$x_min, -Inf) & w$cx <= .doc_num(box$x_max, Inf) &
           w$cy >= .doc_num(box$y_min, -Inf) & w$cy <= .doc_num(box$y_max, Inf), ,
           drop = FALSE]
  if (!nrow(sel)) return(list())
  gap <- .doc_gap_for(sel)
  # THE NAMES COME FROM THE TOP LINE, the BANDS from everything in the box.
  # A box drawn round a header row is drawn by hand and will catch the first row
  # of data as often as not -- and it should, because the data is what the bands
  # have to fit. But a column called "Account Everyday 99-9999-9999999-99" is
  # nobody's column name, so the naming only ever reads the first line.
  lines <- .doc_lines(sel)
  top <- if (length(lines)) lines[[1]] else sel
  cells <- .doc_cells(top[order(top$x), , drop = FALSE], gap)
  if (!length(cells)) return(list())
  bands <- .doc_bands(sel, gap)
  if (!length(bands)) return(list())
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

# doc_fit_columns(input, tab, tmpl) -> columns derived from the words actually
# inside this table's current extent, by the same empty-gutter rule the proposer
# uses. This is the one-click answer to "I have set where it starts and stops --
# now work the columns out for me", and it is what makes correcting a table's
# boundary cheap: move the edge, press it again.
doc_fit_columns <- function(input, tab, tmpl = NULL) {
  loc <- tryCatch(doc_locate_table(input, tab, tmpl), error = function(e) NULL)
  if (is.null(loc) || !length(loc$windows)) return(list())
  parts <- list()
  for (win in loc$windows) {
    w <- .doc_page_words(input, win$page, tmpl)
    if (is.null(w)) next
    w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
    if (!nrow(w)) next
    # A printed rule spans the whole width and would merge every band into one.
    for (d in .doc_lines(w, tab$row_tol)) if (!.doc_is_rule(d)) parts[[length(parts) + 1L]] <- d
  }
  if (!length(parts)) return(list())
  body <- do.call(rbind, parts)
  bands <- .doc_bands(body, .doc_gap_for(body))
  if (length(bands) < 1L) return(list())
  edges <- c(vapply(bands, function(b) b$x_min, numeric(1)),
             bands[[length(bands)]]$x_max)
  cols <- doc_columns_from_edges(edges, old = .doc_columns(tab))
  tab2 <- tab; tab2$columns <- cols
  nm <- doc_header_names(input, tab2, tmpl, edges)
  doc_columns_from_edges(edges, old = .doc_columns(tab), names = nm)
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
  # Where an open-ended window decided the table had ended. Reported, because
  # "it stopped here" and "there was nothing more" are different facts.
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
  # Where the last row this table actually kept came to an end. That is the
  # answer to "where does it stop?", and it is the reader's answer rather than a
  # second guess made somewhere else -- see doc_auto_end().
  last_pg <- NA_integer_; last_y <- NA_real_

  for (win in loc$windows) {
    w <- .doc_page_words(input, win$page, tmpl)
    if (is.null(w)) next
    # A word belongs to the window when its CENTRE is inside it, so a line sitting
    # exactly on the boundary lands on one side of it and not on both.
    w <- w[w$cy >= win$y_min & w$cy <= win$y_max, , drop = FALSE]
    if (!nrow(w)) next

    lines <- .doc_lines(w, tab$row_tol)
    skip <- max(0L, .doc_int(win$skip, 0L))
    # THE ROW RUN, for a window with no real end (see doc_locate_table).
    #
    # A table followed to the bottom of the page swallows whatever is printed
    # under it. Measured on the fixture: a schedule followed onto page 5 came back
    # with 93 rows instead of 83 -- the fee table and the contact table read as ten
    # more transactions, each with an amount in the wrong column and a date that
    # was not a date. Nothing about that looks wrong in a spreadsheet.
    #
    # So an open-ended window stops where the ROWS stop: a vertical gap much
    # bigger than the ones this table has been using AND a line that fills fewer
    # of its columns than its rows have been filling. Both halves are needed --
    # the gap alone would cut a table at the whitespace before its own total row,
    # which is a shape these documents genuinely use, and the column count alone
    # would cut it at any sparse row. Where it stops is recorded, never silent.
    open_end <- isTRUE(win$open_end)
    max_gap <- .doc_num(tab$max_gap, 2.2)
    kept_top <- numeric(0); kept_fill <- integer(0); kept_money <- logical(0)
    win_pitch <- .doc_pitch(w$y, q = 0.25)
    prev_left <- NA_real_; prev_i <- 0L
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
      # A printed border is not a row. Skipped without disturbing anything: it
      # does not count as an unclaimed word, does not end an open window, and
      # does not enter the row-spacing the stop rule is measured against.
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

      # A WRAPPED CELL belongs to the row above it, not to a row of its own.
      # Folded before the stop rule looks at it, so a wrap never ends a table and
      # never counts as a row that filled one column.
      if (prev_i > 0L && n_filled >= 1L &&
          .doc_is_wrap(d, prev_left, as.numeric(attr(d, "top")) - kept_top[length(kept_top)],
                       win_pitch, n_filled)) {
        for (i in seq_along(cells)) if (!is.na(cells[i]) && nzchar(cells[i])) {
          was <- out[[prev_i]][[cnames[i]]]
          out[[prev_i]][[cnames[i]]] <-
            if (is.na(was) || !nzchar(was)) cells[i] else paste(was, cells[i])
          if (!identical(ctypes[i], "auto"))
            out[[prev_i]][[paste0(cnames[i], "__value")]] <-
              .doc_cell_value(out[[prev_i]][[cnames[i]]], ctypes[i])
        }
        next
      }

      # Does the table stop here? Decided BEFORE anything about this line is
      # recorded, so a line that ends the table contributes neither a row nor an
      # unclaimed word. Two kept rows are needed first, because the gap this table
      # uses has to be measured before an unusual one can be recognised.
      if (open_end && length(kept_top) >= 2L) {
        gaps <- diff(kept_top); gaps <- gaps[is.finite(gaps) & gaps > 0]
        pitch_k <- if (length(gaps)) stats::median(gaps) else NA_real_
        this_top <- as.numeric(attr(d, "top"))
        gap <- this_top - kept_top[length(kept_top)]
        typical <- stats::median(kept_fill)
        # WHAT MAKES A LINE AFTER THE GAP "NOT ONE OF THESE ROWS".
        #
        # Two signals, and it took a real document to find the second. Column
        # count alone let a heading through: "Fees charged", printed under a
        # schedule, lands in the first two of four narrow columns, so it filled
        # two where two was the floor -- and ten rows of a fee table and a
        # contact table were read as transactions with a date that was not a
        # date. (Measured: 93 rows where the schedule has 83.)
        #
        # The signal that separates a heading from a genuine sparse row is a
        # FIGURE. A total set apart by whitespace still carries its total; a
        # heading carries nothing. So: after a big gap, a line with no figure at
        # all ends a table whose rows normally have one. Tables with no figures
        # anywhere fall back to the column count, which is all there is.
        rows_have_money <- length(kept_money) && mean(kept_money) >= 0.5
        looks_wrong <- n_filled < max(2, ceiling(typical / 2)) ||
                       (rows_have_money && !has_money)
        if (is.finite(pitch_k) && pitch_k > 0 && is.finite(gap) &&
            gap > pitch_k * max_gap && looks_wrong) {
          stops <- rbind(stops, data.frame(page = as.integer(win$page),
                                           y = this_top,
                                           stringsAsFactors = FALSE))
          break
        }
      }

      # Words inside the table's boundary that NO column claimed. Recorded, never
      # silently dropped: a column band drawn a few points too narrow loses a
      # whole column of figures and every remaining row still looks perfect, so
      # this list is the only thing standing between that and a wrong answer.
      if (any(is.na(j))) {
        s <- d[is.na(j), , drop = FALSE]
        spilled <- rbind(spilled, data.frame(page = s$page, y = s$y, x = s$x,
                                             text = s$text, stringsAsFactors = FALSE))
      }
      if (all(is.na(j)) || n_filled == 0L) next
      kept_top <- c(kept_top, as.numeric(attr(d, "top")))
      kept_fill <- c(kept_fill, n_filled)
      kept_money <- c(kept_money, has_money)
      rowno <- rowno + 1L
      rec <- list(page = as.integer(win$page), row = as.integer(rowno))
      for (i in seq_along(cnames)) rec[[cnames[i]]] <- cells[i]
      for (i in seq_along(cnames)) if (!identical(ctypes[i], "auto"))
        rec[[paste0(cnames[i], "__value")]] <- .doc_cell_value(cells[i], ctypes[i])
      out[[length(out) + 1L]] <- rec
      prev_i <- length(out); prev_left <- min(d$x)
      last_pg <- as.integer(win$page); last_y <- max(d$y + d$height)
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
       spilled = spilled, stops = stops, row_fill = fills, n_rows = nrow(rows),
       n_header = n_header, anchor = loc$anchor, confidence = loc$confidence,
       last_page = last_pg, last_y = last_y,
       detail = if (nrow(stops)) paste0(loc$detail, sprintf(
                  "; it stops on page %d, where the rows stop fitting it",
                  stops$page[nrow(stops)])) else loc$detail,
       extended_to = loc$extended_to,
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
    lb <- .doc_key(sp, "label"); if (!is.list(lb)) lb <- list()
    vb <- .doc_key(sp, "value"); if (!is.list(vb)) vb <- list()
    vtype <- as.character(.doc_key(sp, "type") %||% "text")[1]
    label_text <- trimws(as.character(.doc_key(sp, "label_text") %||% "")[1])
    if (is.na(label_text)) label_text <- ""
    pg <- .doc_int(.doc_key(vb, "page") %||% .doc_key(lb, "page"), 1L)
    if (is.na(pg) || pg < 1L) pg <- 1L

    # NO BOX AT ALL, JUST WORDING. This is the "I already know what the fields
    # are called, let me type them" shortcut, and it is the oldest and most
    # portable way to read a form: the label matcher finds the wording anywhere on
    # any page and takes the value beside it, so nothing depends on where the
    # value sat when the template was made. Keeping it here is what lets ONE
    # builder cover a form and a report -- the two used to be separate screens
    # because the two engines were separate, which was never a reason a person
    # holding a document would recognise.
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
