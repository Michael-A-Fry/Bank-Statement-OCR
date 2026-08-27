# tables_detect.R -- propose the tables and label/value pairs on a document, so setting one up is
# CONFIRMING what the tool found. A proposal is never applied on its own. A table is a run of
# consecutive lines that each break into three or more cells at the SAME x positions, with column bands
# from the vertical gaps that stay EMPTY down the whole run.

.DOC_MIN_TABLE_ROWS <- 3L    # fewer consecutive tabular lines than this is prose
.DOC_MIN_CELLS      <- 3L    # fewer cells than this on a line is not a table row
.DOC_COL_GAP        <- 6     # an x gap this wide, empty down the run, is a column break

.doc_cells <- function(d, gap = .DOC_COL_GAP) {
  if (is.null(d) || !nrow(d)) return(list())
  d <- d[order(d$x), , drop = FALSE]
  if (nrow(d) == 1L) return(list(d))
  g <- cumsum(c(0L, as.integer(d$x[-1] - (d$x[-nrow(d)] + d$width[-nrow(d)]) > gap)))
  unname(split(d, g)[order(unique(g))])
}

.doc_gap_for <- function(w) {
  h <- stats::median(as.numeric(w$height))
  if (!is.finite(h) || h <= 0) return(.DOC_COL_GAP)
  max(.DOC_COL_GAP, h * 0.55)
}

# Project every word box onto the x axis and merge touching intervals; each survivor is a column,
# widened to the midpoint of the empty gutter each side so a shifted word still lands in its own.
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

.doc_has_money <- function(d) {
  any(vapply(as.character(d$text), function(t)
    !is.na(.value_from_line(t, "money")), logical(1)))
}

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

  is_rule <- vapply(lines, .doc_is_rule, logical(1))
  tabular <- ncell >= .DOC_MIN_CELLS & !is_rule

  .runs <- function(min_cells, min_rows) {
    tabular <- ncell >= min_cells & !is_rule
    out <- list(); cur <- integer(0)
    for (i in seq_along(lines)) {
      if (is_rule[i]) next                       # a border is not a break
      last <- if (length(cur)) cur[length(cur)] else NA_integer_
      adjacent <- length(cur) == 0L || (tops[i] - tops[last]) <= pitch * 2.2
      if (tabular[i] && adjacent) { cur <- c(cur, i); next }
      # A wrapped cell is transparent too, or a description too long for its column ends the table
      # at the row it wrapped on. `pitch` is the wrong yardstick for one wrapped line.
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

  # A two-column table is a table, but the three-cell rule keeps a pair of labelled values from
  # being proposed as one, so two columns are allowed only on a LONGER run.
  taken <- unlist(runs)
  for (r2 in .runs(2L, .DOC_MIN_TABLE_ROWS + 1L))
    if (!any(r2 %in% taken)) { runs[[length(runs) + 1L]] <- r2; taken <- c(taken, r2) }
  if (length(runs) > 1L)
    runs <- runs[order(vapply(runs, function(z) z[1], integer(1)))]

  # A schedule sets its total apart with a blank line and an underline, and both break the run. So
  # each run reaches DOWN a few lines for any landing in at least two established bands.
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
    # A header row is a first line with no figures above lines that have them, and it fails in one
    # direction on purpose: a first line carrying figures gets no header, so a table whose first row
    # is data does not lose that row. Guessing a header where there is none deletes a real row.
    has_header <- !.doc_has_money(first) &&
      any(vapply(lines[ix[-1]], .doc_has_money, logical(1)))
    hdr_cl <- if (has_header) .doc_cells(first[order(first$x), , drop = FALSE], gap) else list()
    hdr_cells <- vapply(hdr_cl, function(cell) {
      trimws(gsub("^[|+:_-]+|[|+:_-]+$", "", trimws(paste(cell$text, collapse = " "))))
    }, character(1))
    # A band the HEADING breaks in two is two columns: bands come from where there is no ink down
    # the whole run, so a column blank in this sample merges into its neighbour.
    bands <- .doc_bands(body, gap)
    if (length(hdr_cl)) bands <- .doc_split_bands_by_cells(bands, list(first), gap)
    # The name is the nearest short non-tabular line above the run - and never one that NAMES A
    # PERSON, because a proposed title is saved verbatim and becomes the search key.
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
    # A heading belongs to the band it is PRINTED OVER, not the band with the same number: mapping
    # heading j to band j shifts every name one column left the moment the counts differ.
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
         .frame = c(.doc_num((input$page_width  %||% NA_real_)[page], NA_real_),
                    .doc_num((input$page_height %||% NA_real_)[page], NA_real_)),
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

.doc_same_shape <- function(a, b, tol = 12) {
  if (length(a$.bands) != length(b$.bands)) return(FALSE)
  ca <- vapply(a$.bands, function(z) (z$x_min + z$x_max) / 2, numeric(1))
  cb <- vapply(b$.bands, function(z) (z$x_min + z$x_max) / 2, numeric(1))
  all(abs(ca - cb) <= tol)
}

.doc_header_key <- function(a)
  .doc_norm(paste(unlist(a$anchor$header_text %||% character(0)), collapse = " "))

# Not equality: a continuation page routinely prints "(continued)" after the same column names, and
# a byte-for-byte match split an eighteen-row schedule into two.
.doc_same_header <- function(a, b) {
  ha <- .doc_header_key(a); hb <- .doc_header_key(b)
  nzchar(ha) && nzchar(hb) &&
    (identical(ha, hb) || startsWith(hb, ha) || startsWith(ha, hb))
}

# Do these two print the SAME heading? Testing only the columns folded "AccountOne" and
# "AccountTwo" into one twelve-row table. An untitled candidate is named for its page so it never
# matches another. .doc_same_frame: portrait and landscape print their bands in different spaces.
.doc_same_frame <- function(a, b) {
  fa <- as.numeric(a$.frame %||% c(NA_real_, NA_real_))
  fb <- as.numeric(b$.frame %||% c(NA_real_, NA_real_))
  if (length(fa) != 2L || length(fb) != 2L) return(TRUE)   # unknown: not a refusal
  if (!all(is.finite(c(fa, fb)))) return(TRUE)
  all(abs(fa - fb) <= 2)
}

.doc_same_title <- function(a, b) {
  na <- .doc_norm(a$name %||% ""); nb <- .doc_norm(b$name %||% "")
  nzchar(na) && nzchar(nb) && identical(na, nb)
}

# May `cd` be more of `prev`? A candidate carrying its OWN heading is a new table, and where both
# pages print a header the headers must match - two tables can share a layout and mean different
# things.
.doc_continues <- function(prev, cd) {
  if (isTRUE(cd$.own_heading)) return(FALSE)
  if (nzchar(.doc_header_key(cd))) return(.doc_same_header(prev, cd))
  .doc_same_shape(prev, cd)
}

# Seven near-identical entries are one entry, read WHEREVER it appears. Two tables with no header
# wording can NEVER be folded - geometry alone never says two tables MEAN the same thing.
.doc_fold_repeats <- function(cands) {
  kept <- list()
  for (cd in cands) {
    at <- NA_integer_
    for (i in seq_along(kept))
      if (.doc_same_title(kept[[i]], cd) && .doc_same_frame(kept[[i]], cd) &&
          .doc_same_header(kept[[i]], cd) && .doc_same_shape(kept[[i]], cd)) { at <- i; break }
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
  lapply(kept, function(o) { if (length(o$.places) < 2L) o$.places <- NULL; o })
}

# The fold is only safe once the READER can find every occurrence of an `occurrence: all` table;
# otherwise folding deletes six tables from the template with nothing on screen to say so.
.DOC_FOLD_READER <- "doc_locate_repeats"
.doc_fold_available <- function() exists(.DOC_FOLD_READER, mode = "function")

# Table specs in document order. A table running onto the next page appears as two candidates, and
# joining them is the difference between one table of 80 rows and two of 40 that each look
# complete - joined only when the first reaches its page bottom and the second repeats its header.
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
  # A column blank on page 1 is still a column: bands from the first page's ink alone left a column
  # that prints nothing until page 2 missing or too narrow. doc_fit_columns never returns fewer.
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
  if (.doc_fold_available()) merged <- .doc_fold_repeats(merged)
  merged <- lapply(merged, function(m) { m$follow <- isTRUE(m$.page_last_line); m })
  if (length(merged) > max_tables) merged <- merged[seq_len(max_tables)]
  nms <- make.unique(vapply(merged, function(m) as.character(m$name)[1], character(1)), sep = " ")
  for (i in seq_along(merged)) merged[[i]]$name <- nms[i]
  # The row count is the READER's, not the proposer's: the proposer counts LINES while the reader
  # folds a wrapped cell into the row above and skips a printed rule - twenty-one against fifteen.
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

# Candidate label/value pairs, each as the TWO boxes the template stores. Side by side is a line of
# exactly two cells, where the LABEL is whichever does not parse as a figure or date.
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
  # One pair per label wording: the same label twice is one value with a repeat. Never a label that
  # NAMES A PERSON - the wording is stored verbatim into a file that gets copied off the box.
  seen <- character(0); keep <- list()
  for (o in out) {
    k <- .doc_norm(o$label_text)
    if (!nzchar(k) || k %in% seen) next
    if (length(.fp_pii_problems(o$label_text))) next
    seen <- c(seen, k); keep[[length(keep) + 1L]] <- o
  }
  keep
}

# The matching rules: is THIS line one of these tables? An `occurrence: all` entry has no page
# number to fall back on, so every line of every page is a candidate.
# A template may remember one header row or several wordings of the same header; flattening the
# second into the first scores every variant together and matches none.
.doc_header_variants <- function(need) {
  if (is.null(need)) return(list(character(0)))
  if (is.list(need) && any(vapply(need, function(z) length(unlist(z)), integer(1)) > 1L))
    return(lapply(need, function(z) as.character(unlist(z))))
  list(as.character(unlist(need)))
}

# One line as its CELLS, each a vector of normalised words - a header cell IS a column heading.
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

# WHOLE HEADINGS, NOT SUBSTRINGS: scoring against the whole normalised line found one header's
# words inside another's - "Code, Units, Price, Value" scored 0.75 against "Security Units Unit
# price Market value". A wording matches when the heading's words BEGIN with it.
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

# A title says WHICH table this is; a header only says what SHAPE it is. A title is a distinctive
# phrase, so a substring test is safe here in a way it is not for a single column heading.
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

# Three tests, all required: WORDING scores at or above the bar, SHAPE puts a word in half the
# declared bands, and BREAKS split it into that many cells. Without the third, a sentence naming
# the columns scored as an occurrence, because it runs across every band as ONE cell.
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

# A proposed name turned into the snake_case key the template stores it under.
doc_suggest_name <- function(s) {
  k <- tolower(gsub("[^A-Za-z0-9]+", "_", trimws(as.character(s %||% ""))))
  k <- gsub("^_+|_+$", "", k)
  if (!nzchar(k)) "table" else k
}

# What does this document look like, when nothing recognised it? Two cheap readings separate the
# cases: how many TABLE-SHAPED blocks, and how many lines carry a DATE and a MONEY AMOUNT together.
# A HINT, never a decision.
# Deliberately looser about the date than the label extractor: requiring a parseable three-part
# date found 3 rows on a page of 11.
.DOC_TXN_DATE_RX <- "^[^0-9A-Za-z]*[0-9]{1,2}[ /.-]{1,2}(?:[0-9]{1,2}|[A-Za-z]{3,9})\\b"
.doc_looks_txn_line <- function(t) {
  t <- trimws(as.character(t %||% ""))
  if (!nzchar(t)) return(FALSE)
  grepl(.DOC_TXN_DATE_RX, t, perl = TRUE) && !is.na(.value_from_line(t, "money"))
}

# THE BEST PAGE, NOT THE TOTAL: a statement's first page is a cover and its rows start on page 2,
# so summing lets a cover dilute the one page that answers the question.
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
