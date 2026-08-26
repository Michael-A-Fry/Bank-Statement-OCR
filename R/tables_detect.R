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

  # Printed rules are TRANSPARENT here: they neither start a run, nor end one,
  # nor contribute a column band. A bordered table is a table.
  is_rule <- vapply(lines, .doc_is_rule, logical(1))
  tabular <- ncell >= .DOC_MIN_CELLS & !is_rule

  # .runs(min_cells, min_rows) -- maximal stretches of consecutive, vertically
  # adjacent lines that each break into at least `min_cells` cells. A
  # tabular-looking line half a page below the last one is a different table.
  .runs <- function(min_cells, min_rows) {
    tabular <- ncell >= min_cells & !is_rule
    out <- list(); cur <- integer(0)
    for (i in seq_along(lines)) {
      if (is_rule[i]) next                       # a border is not a break
      last <- if (length(cur)) cur[length(cur)] else NA_integer_
      adjacent <- length(cur) == 0L || (tops[i] - tops[last]) <= pitch * 2.2
      if (tabular[i] && adjacent) { cur <- c(cur, i); next }
      # A WRAPPED CELL is transparent too. It is one cell, indented past where the
      # rows of this run begin, one line below the last of them -- so it is the
      # tail of that row, not the end of the table. Without this a description too
      # long for its column ends the table at the row it wrapped on.
      if (!is.na(last) && ncell[i] <= 1L &&
          .doc_is_wrap(lines[[i]], min(lines[[last]]$x), tops[i] - tops[last],
                       pitch, ncell[i])) next
      if (length(cur) >= min_rows) out[[length(out) + 1L]] <- cur
      cur <- if (tabular[i]) i else integer(0)
    }
    if (length(cur) >= min_rows) out[[length(out) + 1L]] <- cur
    out
  }
  runs <- .runs(.DOC_MIN_CELLS, .DOC_MIN_TABLE_ROWS)

  # A TWO-COLUMN TABLE IS A TABLE. The three-cell rule is what keeps a pair of
  # labelled values from being proposed as one, so two columns are allowed only
  # on a LONGER run -- four consecutive rows in the same two bands is a schedule,
  # not a coincidence. (Measured: a valuation of item and amount, five rows, was
  # proposed as nothing at all.) Any two-cell run overlapping a three-cell one is
  # dropped: the wider reading of the same ink wins.
  taken <- unlist(runs)
  for (r2 in .runs(2L, .DOC_MIN_TABLE_ROWS + 1L))
    if (!any(r2 %in% taken)) { runs[[length(runs) + 1L]] <- r2; taken <- c(taken, r2) }
  if (length(runs) > 1L)
    runs <- runs[order(vapply(runs, function(z) z[1], integer(1)))]

  # THE TOTAL ROW UNDER THE RULE. A schedule sets its total apart: a blank line,
  # an underline, then a row with only two cells filled. Every one of those breaks
  # the run -- so the row somebody actually came for was the one row left out.
  # Each run therefore reaches DOWN for a few more lines, taking any that land in
  # at least two of the bands it has already established, skipping rules, and
  # stopping at the first line that does not. Bounded to three lines and to five
  # times the pitch, so this can reach a total and cannot reach the next table.
  runs <- lapply(runs, function(ix) {
    core <- do.call(rbind, lapply(lines[ix], function(d) d[, c("x", "width")]))
    b0 <- .doc_bands(core, gap)
    if (length(b0) < 2L) return(ix)
    last <- ix[length(ix)]
    extra <- integer(0)
    for (i in seq_along(lines)) {
      if (i <= last) next
      if (length(extra) >= 3L) break
      if (tops[i] - tops[ix[length(ix)]] > pitch * 5) break
      if (is_rule[i]) next
      hit <- vapply(b0, function(b) any(lines[[i]]$cx >= b$x_min & lines[[i]]$cx <= b$x_max),
                    logical(1))
      if (sum(hit) < 2L) break
      extra <- c(extra, i); ix <- c(ix, i)
    }
    ix
  })

  lapply(runs, function(ix) {
    body <- do.call(rbind, lapply(lines[ix], function(d) d[, c("x", "width", "y", "height", "text")]))
    bands <- .doc_bands(body, gap)
    first <- lines[[ix[1]]]
    # A header row is a first line with no figures on it, above lines that have
    # them. That is the only signal available without font information, and it
    # fails in one direction ON PURPOSE:
    #
    #   * first line carries figures -> no header, and one is NOT invented. A
    #     table whose first row is data must not lose that row.
    #   * NOTHING in the run carries figures (a table of names, addresses, dates
    #     with no amounts) -> also no header, so its heading line comes out as a
    #     first row of data.
    #
    # The second is visible the moment the proposal is drawn -- the row is right
    # there in the preview reading "Channel / Detail / Hours" -- and "Header
    # lines" on the builder fixes it in one click. The opposite mistake, guessing
    # a header where there is none, silently deletes a real row and leaves
    # nothing on screen to notice.
    has_header <- !.doc_has_money(first) &&
      any(vapply(lines[ix[-1]], .doc_has_money, logical(1)))
    hdr_cells <- if (has_header)
      vapply(.doc_cells(first, gap), function(cell) {
        # A condensed table separates its columns with "|", which lands inside the
        # header cell beside it -- so the column would be called "Amount |".
        trimws(gsub("^[|+:_-]+|[|+:_-]+$", "", trimws(paste(cell$text, collapse = " "))))
      }, character(1))
      else character(0)
    # The name is the nearest line ABOVE the run that is not itself tabular and is
    # short enough to be a heading. That is where these documents print it.
    nm <- NA_character_
    above <- which(seq_along(lines) < ix[1])
    for (i in rev(above)) {
      if (tops[ix[1]] - tops[i] > pitch * 3.5) break
      if (tabular[i] || is_rule[i]) next
      t <- trimws(.doc_line_text(lines[[i]]))
      if (nzchar(t) && length(strsplit(t, "\\s+")[[1]]) <= 8L) { nm <- t; break }
    }
    # The names are made unique HERE, not left to the reader. Two tables printed
    # side by side both head a column "Code", and a proposal that shows two
    # columns of the same name while the output silently renames one of them is
    # two different answers to the same question.
    cnm <- make.unique(vapply(seq_along(bands), function(j)
      if (j <= length(hdr_cells) && nzchar(trimws(hdr_cells[j]))) trimws(hdr_cells[j])
      else sprintf("column_%d", j), character(1)), sep = "_")
    cols <- lapply(seq_along(bands), function(j)
      list(name = cnm[j], x_min = bands[[j]]$x_min, x_max = bands[[j]]$x_max,
           type = "auto"))
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
         n_rows = length(body_ix), .own_heading = !is.na(nm),
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

# .doc_header_key(a) -- a candidate's header wording, normalised, or "".
.doc_header_key <- function(a)
  .doc_norm(paste(unlist(a$anchor$header_text %||% character(0)), collapse = " "))

# .doc_same_header(a, b) -- b repeats a's header. Not equality: a continuation
# page routinely prints "(continued)" after the same column names, and demanding
# a byte-for-byte match left an eighteen-row schedule split into two tables of
# eight and ten. One being a prefix of the other is the rule -- which still
# refuses a page that prints the SAME column names in a DIFFERENT order, because
# that is a different table that happens to share a vocabulary.
.doc_same_header <- function(a, b) {
  ha <- .doc_header_key(a); hb <- .doc_header_key(b)
  nzchar(ha) && nzchar(hb) &&
    (identical(ha, hb) || startsWith(hb, ha) || startsWith(ha, hb))
}

# .doc_continues(prev, cd) -- may `cd` be more of `prev`, printed on the next page?
#
# TWO REFUSALS, both learned from a document that got this wrong.
#
#  * A candidate that carries its OWN HEADING is a new table. A genuine
#    continuation page opens with the repeated column names at the top and
#    nothing above them; a page headed "Account two" is a second schedule, and
#    joining it to the first put twelve rows under one account's name with no
#    sign on screen that six of them belonged to another. This is the decisive
#    test, because the two tables are otherwise identical in every measurable way.
#  * Where BOTH pages print a header, the headers must match. Geometry alone is
#    allowed only when the second page has no header at all to compare -- two
#    tables can share a column layout exactly and mean different things.
.doc_continues <- function(prev, cd) {
  if (isTRUE(cd$.own_heading)) return(FALSE)
  if (nzchar(.doc_header_key(cd))) return(.doc_same_header(prev, cd))
  .doc_same_shape(prev, cd)
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
  npg <- .doc_npages(input)
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
          .doc_continues(prev, cd)) {
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
  # THE ROW COUNT IS THE READER'S, NOT THE PROPOSER'S.
  #
  # The two count differently, and both are right about their own job: the
  # proposer counts LINES in a run of tabular-looking text, while the reader folds
  # a wrapped cell into the row above it and skips a printed rule. On a document
  # with wrapped descriptions that is twenty-one against fifteen -- and a count
  # shown beside a proposal that is not the count in the file it produces is worse
  # than no count. So the reader is asked, once, and its answer is the answer.
  # (Measured across 81 third-party PDFs: 40 of them disagreed before this.)
  for (i in seq_along(merged)) {
    n <- tryCatch(.doc_int(doc_table_rows(input, merged[[i]], tmpl)$n_rows, NA_integer_),
                  error = function(e) NA_integer_)
    if (!is.na(n)) merged[[i]]$n_rows <- n
  }
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
  kind <- .doc_value_kind

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

# ---------------------------------------------------------------------------
# WHAT DOES THIS DOCUMENT LOOK LIKE, when nothing recognised it?
#
# The front door tries three pipelines and, when all three come back
# `unsupported`, the screen has to say what to do next. It used to say one thing:
# "set up a template for this STATEMENT", with a button into the statement
# toolkit -- which is right for a statement and flatly wrong for the report,
# schedule or letter somebody has just dropped in. Calling the file a statement
# twice in a sentence, when the whole reason we are here is that nothing could
# tell what it was, is the tool asserting the one thing it does not know.
#
# It does not have to guess blind, though. Two cheap readings of the first pages
# separate the two cases most of the time:
#
#   * how many TABLE-SHAPED blocks are on it (the report proposer, already built)
#   * how many lines carry a DATE and a MONEY AMOUNT together -- which is what a
#     transaction row is, whatever bank printed it
#
# The answer is a HINT, never a decision: both routes are always offered, and the
# sentence says what was seen rather than what the file is. A tool that refuses
# the wrong door is worse than one that guesses and says so.
# ---------------------------------------------------------------------------

# .doc_looks_txn_line(t) -- does this line have the SHAPE of a transaction row:
# a date at the front and a money amount somewhere after it.
#
# Deliberately looser about the date than the label extractor is. That one needs
# a value it can parse; this one only needs to recognise a shape, and statements
# print "02 May", "02/05", "2 May 26" and "02-05-2026" all as the leading date of
# a row. Requiring a parseable three-part date found 3 rows on a page of 11.
.DOC_TXN_DATE_RX <- "^[^0-9A-Za-z]*[0-9]{1,2}[ /.-]{1,2}(?:[0-9]{1,2}|[A-Za-z]{3,9})\\b"
.doc_looks_txn_line <- function(t) {
  t <- trimws(as.character(t %||% ""))
  if (!nzchar(t)) return(FALSE)
  grepl(.DOC_TXN_DATE_RX, t, perl = TRUE) && !is.na(.value_from_line(t, "money"))
}

# doc_shape_hint(input, pages) -> list(tables, txn_lines, lines, looks, sentence)
#
# `looks` is "statement", "report" or "neither". `sentence` is plain English
# about what was counted, safe to print next to both buttons.
#
# THE BEST PAGE, NOT THE TOTAL. A statement's first page is a cover -- name,
# address, four summary figures -- and its rows start on page 2. Summing across
# pages lets a cover dilute the one page that answers the question, so each page
# is counted on its own and the best one is the reading.
doc_shape_hint <- function(input, pages = 1:4) {
  npg <- .doc_npages(input)
  out <- list(tables = 0L, txn_lines = 0L, lines = 0L, looks = "neither",
              sentence = "Nothing could be read off the page to go on.")
  if (npg < 1L) return(out)
  pages <- sort(unique(pages[pages >= 1L & pages <= npg]))
  if (!length(pages)) return(out)

  n_lines <- 0L; best_txn <- 0L
  for (p in pages) {
    w <- .doc_page_words(input, p)
    if (is.null(w) || !NROW(w)) next
    txt <- vapply(.doc_lines(w), function(d) paste(as.character(d$text), collapse = " "),
                  character(1))
    n_lines <- n_lines + length(txt)
    n <- sum(vapply(txt, .doc_looks_txn_line, logical(1)))
    if (n > best_txn) best_txn <- as.integer(n)
  }
  n_tab <- length(tryCatch(propose_tables(input, pages = pages), error = function(e) list()))

  out$tables <- as.integer(n_tab); out$txn_lines <- as.integer(best_txn)
  out$lines <- as.integer(n_lines)
  if (!n_lines) return(out)

  # FOUR is not a magic number, it is "more than a letterhead date and a total".
  # A report's cover routinely carries a dated money figure or two; a statement
  # carries a RUN of them, on one page, one under the other.
  out$looks <- if (best_txn >= 4L) "statement" else if (n_tab >= 1L) "report" else "neither"
  where <- if (length(pages) == 1L) "page" else sprintf("first %d pages", length(pages))
  out$sentence <- switch(out$looks,
    statement = sprintf(paste("Looking at the %s: %d line(s) on one page start with a date and carry",
                              "an amount, which is what a transaction row looks like."),
                        where, best_txn),
    report = sprintf(paste("Looking at the %s: %d table-shaped block(s), and no run of lines starting",
                           "with a date and carrying an amount."), where, n_tab),
    sprintf(paste("Looking at the %s: neither a table nor a run of lines starting with a date and",
                  "carrying an amount."), where))
  out
}
