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
      # `pitch` is still right for `adjacent` above -- that is a distance between
      # ROWS over a whole run, which is what a page statistic measures well. It is
      # the wrong yardstick for one wrapped line, so .doc_is_wrap reads the
      # leading off the two lines instead (see its comment in R/tables.R).
      if (!is.na(last) && ncell[i] <= 1L &&
          .doc_is_wrap(lines[[i]], min(lines[[last]]$x), tops[i] - tops[last],
                       ncell[i], 2L, 2L,
                       suppressWarnings(max(as.numeric(lines[[last]]$height), na.rm = TRUE)))) next
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
    hdr_cl <- if (has_header) .doc_cells(first[order(first$x), , drop = FALSE], gap) else list()
    hdr_cells <- vapply(hdr_cl, function(cell) {
      # A condensed table separates its columns with "|", which lands inside the
      # header cell beside it -- so the column would be called "Amount |".
      trimws(gsub("^[|+:_-]+|[|+:_-]+$", "", trimws(paste(cell$text, collapse = " "))))
    }, character(1))
    # A BAND THE HEADING BREAKS IN TWO IS TWO COLUMNS. The bands come from where
    # there is no ink down the WHOLE run, so a column whose data rows are all
    # blank in this sample, or whose long values reach under its neighbour's
    # heading, merges into that neighbour and its territory is lost for good --
    # silently, because a merged band still claims every word. The heading row
    # knows better, and this is the same rule doc_columns_from_box() applies to a
    # drag box (.doc_split_bands_by_cells, R/tables.R): one rule, two places.
    bands <- .doc_bands(body, gap)
    if (length(hdr_cl)) bands <- .doc_split_bands_by_cells(bands, list(first), gap)
    # The name is the nearest line ABOVE the run that is not itself tabular and is
    # short enough to be a heading. That is where these documents print it.
    #
    # ...and never a line that NAMES A PERSON. A proposed title is saved into the
    # template verbatim and A2b makes it the search key, so "Prepared for Mr John
    # Smith" would become a table name in a file that gets copied off the box.
    # The drafter already refuses one as a fingerprint phrase; the same gate, the
    # same rule, and the search simply carries on up the page.
    nm <- NA_character_
    above <- which(seq_along(lines) < ix[1])
    for (i in rev(above)) {
      if (tops[ix[1]] - tops[i] > pitch * 3.5) break
      if (tabular[i] || is_rule[i]) next
      t <- trimws(.doc_line_text(lines[[i]]))
      if (!nzchar(t) || length(strsplit(t, "\\s+")[[1]]) > 8L) next
      if (length(.fp_pii_problems(t))) next
      nm <- t; break
    }
    # A HEADING BELONGS TO THE BAND IT IS PRINTED OVER, not to the band with the
    # same number. Mapping heading j to band j shifts every name one column left
    # the moment the two counts differ -- measured on a schedule whose rows print
    # a code the heading row has nothing over: the code band was called "Basis",
    # the basis band "Amount", and the money band "column_4". So the heading cell's
    # CENTRE picks its band, exactly as doc_columns_from_box() does.
    #
    # The names are made unique HERE, not left to the reader. Two tables printed
    # side by side both head a column "Code", and a proposal that shows two
    # columns of the same name while the output silently renames one of them is
    # two different answers to the same question.
    cnm <- make.unique(vapply(seq_along(bands), function(j) {
      hit <- Filter(function(cl) {
        cx <- mean(cl$x + cl$width / 2)
        cx >= bands[[j]]$x_min && cx <= bands[[j]]$x_max
      }, hdr_cl)
      if (!length(hit)) sprintf("column_%d", j)
      else trimws(gsub("^[|+:_-]+|[|+:_-]+$", "",
                       trimws(paste(unlist(lapply(hit, function(cl) cl$text)), collapse = " "))))
    }, character(1)), sep = "_")
    cnm[!nzchar(cnm)] <- sprintf("column_%d", which(!nzchar(cnm)))
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

# SEVEN NEAR-IDENTICAL ENTRIES ARE ONE ENTRY.
#
# A pack with a holdings table per fund proposes one entry per fund -- "Fund A
# holdings", "Fund A holdings 1", ... -- seven things to read, name and confirm
# where there is one thing to teach. They are the same table by the only two
# tests that matter, and this file already owns both: the same header wording
# (.doc_same_header) and the same column bands (.doc_same_shape). So they fold
# into ONE entry that says it is read WHEREVER it appears -- `occurrence: all`,
# which the template carries and the reader obeys -- remembering every place it
# was seen so the row count stays the reader's and the builder can say "7 places".
#
# This REMOVES things from the screen rather than adding them: measured, a
# seven-fund pack goes from seven entries to one and the shipped fixture from six
# to five, "Account summary 1" being the one that goes.
#
# TWO TABLES WITH NO HEADER WORDING CAN NEVER BE FOLDED. .doc_same_header refuses
# an empty key on either side, and that refusal is the point: geometry alone
# never says two tables MEAN the same thing, and folding two that do not is how
# twelve rows end up under one account's name.
.doc_fold_repeats <- function(cands) {
  kept <- list()
  for (cd in cands) {
    at <- NA_integer_
    for (i in seq_along(kept))
      if (.doc_same_header(kept[[i]], cd) && .doc_same_shape(kept[[i]], cd)) { at <- i; break }
    if (is.na(at)) {
      cd$.places <- list(list(start = cd$start, end = cd$end))
      kept[[length(kept) + 1L]] <- cd
      next
    }
    o <- kept[[at]]
    o$occurrence <- "all"
    o$.places <- c(o$.places, list(list(start = cd$start, end = cd$end)))
    o$n_rows <- as.integer(o$n_rows) + as.integer(cd$n_rows)
    kept[[at]] <- o
  }
  # An entry seen in exactly one place is not a repeating table; it keeps the
  # shape it has always had, so nothing about a document with no repeats moves.
  lapply(kept, function(o) { if (length(o$.places) < 2L) o$.places <- NULL; o })
}

# .DOC_FOLD_READER / .doc_fold_available() -- the fold is only safe once the
# READER can find every occurrence of a table set to `occurrence: all`.
#
# Folding seven entries into one is right only if the reader then reads all
# seven. While it reads one place per entry, folding would delete six tables from
# the template with nothing on screen to say so -- the silent loss this register
# exists to stop, arriving through the fix for it. So the fold asks for the half
# that reads repeats and does nothing until it is there, which means the proposer
# can never be ahead of the reader and no document is ever proposed a template
# that reads less of it than the old one did.
.DOC_FOLD_READER <- "doc_locate_repeats"
.doc_fold_available <- function() exists(.DOC_FOLD_READER, mode = "function")

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
  # A COLUMN BLANK ON PAGE 1 IS STILL A COLUMN. The bands above were derived from
  # the FIRST page's ink only and then carried across the join, so a column that
  # prints nothing until page 2 was either missing from the template for ever or
  # -- worse -- drawn narrow enough that its real values fell outside it and read
  # as nothing. Measured on a two-page schedule: the Ref band came out 415-446
  # off the heading word alone, and all ten reference values on page 2 were
  # unclaimed while every Ref cell read blank. So the divisions are re-derived
  # ONCE over the joined extent, keeping the names already given. doc_fit_columns
  # can never return fewer columns than the table has (R/tables.R), so this can
  # only ever widen or add.
  for (i in seq_along(merged)) {
    if (!isTRUE(merged[[i]]$continued)) next
    old <- merged[[i]]$columns
    fit <- tryCatch(doc_fit_columns(input, merged[[i]], tmpl), error = function(e) list())
    if (length(fit) < length(old)) next
    if (length(fit) == length(old))
      for (j in seq_along(fit)) fit[[j]]$name <- old[[j]]$name
    merged[[i]]$columns <- fit
    merged[[i]]$.bands <- lapply(fit, function(cc) list(x_min = cc$x_min, x_max = cc$x_max))
  }
  # ...and seven near-identical entries are ONE entry -- but only where the
  # reader can find all seven. See .doc_fold_repeats() and .DOC_FOLD_READER.
  if (.doc_fold_available()) merged <- .doc_fold_repeats(merged)
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
  # A folded entry is asked once per place it was seen and the answers are added
  # up, so the number beside "7 places" is still the number of rows the file will
  # hold -- not the proposer's count of lines wearing the reader's name.
  for (i in seq_along(merged)) {
    places <- merged[[i]]$.places %||% list(list(start = merged[[i]]$start,
                                                 end   = merged[[i]]$end))
    tot <- 0L; ok <- TRUE
    for (q in places) {
      m <- merged[[i]]; m$start <- q$start; m$end <- q$end
      n <- tryCatch(.doc_int(doc_table_rows(input, m, tmpl)$n_rows, NA_integer_),
                    error = function(e) NA_integer_)
      if (is.na(n)) { ok <- FALSE; break }
      tot <- tot + n
    }
    if (ok) merged[[i]]$n_rows <- as.integer(tot)
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
  #
  # ...and never a label that NAMES A PERSON. The label wording is stored in the
  # template verbatim and is what the reader searches the page for, so a pair
  # proposed off the line "Mr John Smith  1,240.55" would write a customer's name
  # into a file that gets copied off the box -- and it would only ever find that
  # one customer's document anyway. Same gate as the fingerprint (.fp_has_pii).
  seen <- character(0); keep <- list()
  for (o in out) {
    k <- .doc_norm(o$label_text)
    if (!nzchar(k) || k %in% seen) next
    if (length(.fp_pii_problems(o$label_text))) next
    seen <- c(seen, k); keep[[length(keep) + 1L]] <- o
  }
  keep
}

# ---------------------------------------------------------------------------
# THE MATCHING RULES: is THIS line one of these tables?
#
# A template entry set to be read wherever it appears has no page number to fall
# back on, so every line of every page is a candidate and the rules below are the
# whole of what stands between "found it seven times" and seven wrong readings.
# They live here, beside .doc_same_header and .doc_same_shape, because they are
# the same question asked of a template's DECLARED wordings and bands rather than
# of a second candidate.
# ---------------------------------------------------------------------------

# .doc_header_variants(need) -> the remembered wordings as a list of variants.
#
# A template may remember ONE header row ("Code", "Units", "Price", "Value") or
# SEVERAL wordings of the same header, one per variant. Flattening the second
# into the first scores every variant together and so matches none of them, which
# is why this branches on the STRUCTURE rather than calling unlist(): a list with
# any element holding more than one wording is a list of variants; anything else
# is one flat set and behaves exactly as it always has.
.doc_header_variants <- function(need) {
  if (is.null(need)) return(list(character(0)))
  if (is.list(need) && any(vapply(need, function(z) length(unlist(z)), integer(1)) > 1L))
    return(lapply(need, function(z) as.character(unlist(z))))
  list(as.character(unlist(need)))
}

# .doc_line_cell_words(d, gap) -> one line as its CELLS, each a character vector
# of normalised words. The cell is the unit because a header cell IS a column
# heading; splitting on the gutters is what makes a whole-word test possible at
# all once .doc_norm has taken the spaces out.
.doc_line_cell_words <- function(d, gap = NULL) {
  if (is.null(d) || !NROW(d)) return(list())
  words <- function(v) {
    k <- .doc_norm(unlist(strsplit(paste(as.character(v), collapse = " "), "\\s+")))
    k[nzchar(k)]
  }
  if (is.null(d$x) || is.null(d$width))
    return(Filter(length, lapply(as.character(d$text), words)))
  if (is.null(gap)) gap <- .doc_gap_for(d)
  Filter(length, lapply(.doc_cells(d[order(d$x), , drop = FALSE], gap),
                        function(cl) words(cl$text)))
}

# .doc_header_score(d, need, gap) -> the fraction of one remembered header row
# found on this line, and with VARIANTS the best of them.
#
# WHOLE HEADINGS, NOT SUBSTRINGS -- and this is the load-bearing half. Scoring a
# wording as a substring of the whole normalised line finds one header's words
# inside another's: measured on the line "Security Units Unit price Market value",
# a DIFFERENT table's header "Code, Units, Price, Value" scored 0.75 and cleared
# the 0.6 bar, because `price` is inside `unitprice` and `value` inside
# `marketvalue`. On a pack of forty similarly-shaped tables that is not a corner
# case, it is the normal case, and the table found is confidently the wrong one.
#
# So a wording matches a heading when the heading's words BEGIN with it: "Price"
# does not match the cell "Unit price", while "Opening" still matches the cell
# "Opening balance", which is the tolerance the old rule was written for and the
# only part of it worth keeping.
.doc_header_score <- function(d, need, gap = NULL) {
  cells <- .doc_line_cell_words(d, gap)
  if (!length(cells)) return(0)
  score1 <- function(want) {
    want <- want[!is.na(want) & nzchar(trimws(want))]
    if (!length(want)) return(0)
    mean(vapply(want, function(n) {
      wt <- .doc_norm(unlist(strsplit(trimws(n), "\\s+"))); wt <- wt[nzchar(wt)]
      if (!length(wt)) return(FALSE)
      any(vapply(cells, function(ct)
        length(ct) >= length(wt) && identical(ct[seq_along(wt)], wt), logical(1)))
    }, logical(1)))
  }
  max(vapply(.doc_header_variants(need), score1, numeric(1)), 0)
}

# .doc_find_title(lines, any_of) -> the first line carrying any of the remembered
# title phrases, or NULL. A title says WHICH table this is; a header only says
# what SHAPE it is, which is why the locator asks this one first. Matched through
# .doc_norm like everything else, so case, spacing and punctuation cannot break
# it -- a title is a distinctive phrase, so a substring test is safe here in a way
# it is not for a single column heading.
.doc_find_title <- function(lines, any_of) {
  want <- .doc_norm(as.character(unlist(any_of %||% character(0))))
  want <- want[nzchar(want)]
  if (!length(lines) || !length(want)) return(NULL)
  for (i in seq_along(lines)) {
    hay <- .doc_norm(.doc_line_text(lines[[i]]))
    if (!nzchar(hay)) next
    if (any(vapply(want, function(k) grepl(k, hay, fixed = TRUE), logical(1))))
      return(list(index = as.integer(i), top = attr(lines[[i]], "top"),
                  text = trimws(.doc_line_text(lines[[i]]))))
  }
  NULL
}

# .doc_is_occurrence(d, need, bands, gap, min_hit) -> is this line the top of
# another copy of that table?
#
# THREE TESTS, ALL THREE REQUIRED, and the third one is why prose does not count:
#   the wording  it scores at or above the bar on the remembered header row(s);
#   the shape    it puts a word in at least half the declared bands (two at the
#                very least), so a line that happens to share a word or two with
#                the heading but is printed across none of its columns is out;
#   the breaks   it splits into at least that many gutter-separated cells.
# Measured without the third: the sentence "Refer to the Security Units Cost
# Market value table" scored as an occurrence -- it names every column and lands
# in every band, because it runs straight across all of them as ONE cell. With
# it, zero.
.doc_is_occurrence <- function(d, need, bands, gap = NULL, min_hit = 0.75) {
  if (is.null(d) || !NROW(d)) return(FALSE)
  bands <- bands %||% list()
  if (length(bands) < 2L) return(FALSE)
  if (is.null(gap)) gap <- .doc_gap_for(d)
  if (.doc_header_score(d, need, gap) < min_hit) return(FALSE)
  cx <- as.numeric(d$x) + as.numeric(d$width) / 2
  n_in <- sum(vapply(bands, function(b) {
    lo <- .doc_num(b$x_min, NA_real_); hi <- .doc_num(b$x_max, NA_real_)
    is.finite(lo) && is.finite(hi) && any(cx >= lo & cx <= hi)
  }, logical(1)))
  if (n_in < max(2L, as.integer(ceiling(length(bands) / 2)))) return(FALSE)
  length(.doc_cells(d[order(d$x), , drop = FALSE], gap)) >= n_in
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
