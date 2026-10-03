# auto_read_pdf.R -- the geometry half of the automatic reader (spec section 4,
# steps 1-5): every word on every page typed, lines and cells measured in the
# page's own type size, and ONE column model for the whole document.
#
# Geometry only PROPOSES. Nothing here decides which column is money out, money
# in or the balance; R/auto_read_prove.R does that with the statement's own
# arithmetic. So this file is free to be generous (a sparse column with one
# figure in it is still a column) wherever the arithmetic can veto the mistake,
# and strict only where it cannot: what counts as a body row, and where a
# boundary may go.
#
# All coordinates are in the BAND FRAME (R/parse_pdf_table.R), so the boxes this
# file draws are the boxes the table reader applies, on every page.

# ---- token shapes --------------------------------------------------------------

# A money figure as one printed word: optional bracket and sign, optional currency
# glyph (an ASCII-only negated class, so a source file with a literal pound or
# euro sign cannot change meaning under another encoding), digits with
# optional thousands groups, EXACTLY two decimals after a point or a comma, and an
# optional closing bracket, trailing sign or glued marker ("150.00CR"). Units,
# rates, references and times never have that shape.
.AR_MONEY_RX <- paste0("^[(]?[-+]?[^0-9A-Za-z[:space:].,'()+-]{0,3}[-+]?",
                       "(?:[0-9]{1,3}(?:[,.'][0-9]{3})+|[0-9]+)[.,][0-9]{2}[)]?[-+]?",
                       "(?:[CcDdOo][RrDd])?$")
# A function, not a constant: .MONEY_WORDS lives in R/normalise.R, sourced later.
.ar_currency <- function() c(.MONEY_WORDS, "$", "NZ$", "AU$", "US$", "\u00a3", "\u20ac")
.AR_WEEKDAY_RX <- "^(?:mon|tue|tues|wed|thu|thur|thurs|fri|sat|sun)(?:day)?[.,]?$"

# .ar_markers() -- the sign words a figure may carry (CR / DR / OD by default),
# from the lexicon so a bank's own marker is taught in the dictionary.
.ar_markers <- function() {
  m <- toupper(c(safe(lex("dr_cr_suffix_debit"), "DR"), safe(lex("dr_cr_suffix_credit"), "CR"),
                 safe(lex("overdrawn_markers"), "OD")))
  unique(m[!is.na(m) & nzchar(m)])
}

# .ar_date_formats(tabular) -- the reader's own date table (so nothing is called a
# date that parse_date would refuse). A bare eight-digit run is a date only in a
# spreadsheet column, where the whole column has to agree.
.ar_date_formats <- function(tabular = FALSE) {
  f <- safe(lex("date_formats"), wd_date_table())
  extra <- list(list(fmt = "%d-%b-%y", rx = "^[0-9]{1,2}-[A-Za-z]{3,9}-[0-9]{2}$"),
                list(fmt = "%d %b %y", rx = "^[0-9]{1,2} [A-Za-z]{3,9} [0-9]{2}$"))
  if (tabular) extra <- c(extra, list(list(fmt = "%Y%m%d", rx = "^[0-9]{8}$")))
  have <- vapply(f, function(e) e$fmt, "")
  c(f, Filter(function(e) !(e$fmt %in% have), extra))
}

# .ar_date_fmts(s, fmts) -- for each string, every format it parses under STRICTLY
# (the reader's parse_date; a sentinel year for a year-less form), "|"-joined, or
# "" when it is not a date. Every format needs a separator or a month name, so a
# bare "12" is never a date.
.ar_date_fmts <- function(s, fmts) {
  out <- rep("", length(s))
  if (!length(s)) return(out)
  n <- .normalise_date_str(s)
  for (e in fmts) {
    hit <- which(grepl(e$rx, n, perl = TRUE))
    if (!length(hit)) next
    ok <- if (isTRUE(e$yearless))
      !is.na(parse_date(paste(n[hit], "2000"), paste(e$fmt, "%Y"))$iso)
    else !is.na(parse_date(n[hit], e$fmt)$iso)
    hit <- hit[ok]
    out[hit] <- ifelse(nzchar(out[hit]), paste(out[hit], e$fmt, sep = "|"), e$fmt)
  }
  out
}

# ---- one page: words, lines, phrases --------------------------------------------

# .ar_page(w, p, frame, pw, ph, row_tol, fmts, markers, ocr) -- one page's words in
# the band frame (a scan's straightened and its glued date pieces split first),
# grouped into lines EXACTLY as parse_pdf_table groups them (so a line here is a
# row there), with every date phrase and money phrase found. NULL for a page with
# no words.
.ar_page <- function(w, p, frame, pw, ph, row_tol, fmts, markers, ocr = FALSE) {
  if (is.null(w) || !nrow(w)) return(NULL)
  w <- as.data.frame(w, stringsAsFactors = FALSE)
  w <- .words_to_band_frame(w, frame, pw, ph)
  w$text <- .ascii_dashes(trimws(as.character(w$text)))
  w <- w[!is.na(w$x) & !is.na(w$y) & nzchar(w$text), , drop = FALSE]
  if (!nrow(w)) return(NULL)
  if (ocr) w <- .ar_ocr_split(.ar_deskew(w, pw, ph))
  w <- w[order(w$y, w$x), , drop = FALSE]
  w$line <- .group_rows(w$y, row_tol)
  w <- w[order(w$line, w$x), , drop = FALSE]
  rownames(w) <- NULL
  w$x1 <- w$x + w$width
  w$y1 <- w$y + w$height
  w$cx <- w$x + w$width / 2
  h <- suppressWarnings(stats::median(w$height, na.rm = TRUE)); if (!isTRUE(h > 0)) h <- 8
  n <- nrow(w)
  same <- c(FALSE, w$line[-1] == w$line[-n])
  gap <- c(NA, w$x[-1] - w$x1[-n]); gap[!same] <- NA
  # The page's word space: the commonest small gap. A cell boundary is a gap well
  # beyond it, measured in this page's own type size, never in fixed points.
  g <- gap[!is.na(gap) & gap >= 0 & gap < h]
  space <- if (length(g)) stats::quantile(g, 0.35, names = FALSE) else 0.3 * h
  thr <- min(1.6 * h, max(0.6 * h, 2.5 * max(space, 1)))
  # A word that starts a cell: the first on its line, or after a gutter.
  w$cell_start <- is.na(gap) | gap > thr
  up <- toupper(w$text)
  w$money_core <- grepl(.AR_MONEY_RX, w$text, perl = TRUE)
  w$marker <- up %in% markers
  w$cur <- up %in% .ar_currency()
  w$kind <- "text"; w$phrase <- NA_integer_
  ph <- .ar_phrases(w, gap, h, fmts)
  if (nrow(ph)) for (k in seq_len(nrow(ph))) {
    ix <- ph$i0[k]:ph$i1[k]
    w$kind[ix] <- ph$kind[k]; w$phrase[ix] <- k
  }
  # Standalone: a figure is a cell of its own only when a gutter (or another
  # figure / date) separates it from the words either side. "USD 25.00" set inside
  # a description with a word space is part of that description.
  if (nrow(ph)) {
    prev_i <- ph$i0 - 1L; next_i <- ph$i1 + 1L
    pl <- prev_i >= 1L & w$line[pmax(prev_i, 1L)] == ph$line
    nl <- next_i <= n & w$line[pmin(next_i, n)] == ph$line
    gb <- ifelse(pl, ph$x - w$x1[pmax(prev_i, 1L)], Inf)
    ga <- ifelse(nl, w$x[pmin(next_i, n)] - ph$x1, Inf)
    pk <- ifelse(pl, w$kind[pmax(prev_i, 1L)], "none")
    nk <- ifelse(nl, w$kind[pmin(next_i, n)], "none")
    ph$standalone <- (gb > thr | pk %in% c("date", "money")) & (ga > thr | nk %in% c("date", "money"))
    ph$gap_before <- gb; ph$gap_after <- ga
  } else ph$standalone <- logical(0)
  w <- .ar_join_staggered(w, ph, h)
  if (nrow(ph)) ph$line <- w$line[ph$i0]
  # One row per line: where it is, what it says, and whether it is a summary line
  # (opening / closing / brought or carried forward / a total) -- the reader's own
  # whole-label test, so this and the table reader never disagree.
  li <- split(seq_len(n), w$line)
  raw <- vapply(li, function(ix) paste(w$text[ix], collapse = " "), "")
  lines <- data.frame(page = p, line = as.integer(names(li)),
    y = vapply(li, function(ix) min(w$y[ix]), 0), y1 = vapply(li, function(ix) max(w$y1[ix]), 0),
    x = vapply(li, function(ix) min(w$x[ix]), 0), x1 = vapply(li, function(ix) max(w$x1[ix]), 0),
    raw = unname(raw), stringsAsFactors = FALSE)
  lines$summary <- vapply(lines$raw, function(s) isTRUE(.pdf_is_summary(NA, s)), logical(1), USE.NAMES = FALSE)
  lines$footer <- vapply(lines$raw, function(s) isTRUE(.is_footer_noise(s)), logical(1), USE.NAMES = FALSE)
  # The label is the line's words with its dates, figures and sign words taken
  # out, wherever they sit ("BALANCE C/F 758.73-" is "balance c/f").
  lines$label <- unname(vapply(li, function(ix) {
    k <- ix[w$kind[ix] == "text" & !w$marker[ix] & !w$cur[ix]]
    tolower(paste(w$text[k], collapse = " "))
  }, ""))
  # The reader's own summary test misses some wordings (a card's "New balance");
  # any line that names an opening, closing, carried balance or a total is an
  # anchor here, never a row. If the table reader keeps one as a row anyway, the
  # row counts disagree and the reading is held back.
  lines$aclass <- .ar_anchor_class(lines$label)
  lines$summary <- lines$summary | nzchar(lines$aclass)
  if (nrow(ph)) ph$page <- p
  list(page = p, w = w, ph = ph, lines = lines, h = h, thr = thr, space = space,
       width = frame$width, ocr = FALSE,
       seg = .ar_segment_anchors(w, ph, lines, gap, thr))
}

# .ar_join_staggered(w, ph, h) -- a transaction printed over two lines with its date
# on the first and its figures on the second ("07/12/25 DEPOSIT" / "MOBILE CHEQUE
# 806662   641.98   4,609.47") is one row: the figure line joins the date line.
# Only when the date line opens with the date and has no figure, and the next line
# (set tight under it) has a standalone figure and starts right of the date (so it
# has no date of its own in the date column; "24/7" in a payee is no date). The table reader stitches the same two lines (its split-row recovery),
# and if it does not, the row counts disagree and the reading is held back.
.ar_join_staggered <- function(w, ph, h) {
  if (!nrow(ph)) return(w)
  ls <- unique(w$line)
  if (length(ls) < 2L) return(w)
  first <- vapply(ls, function(l) min(which(w$line == l)), 0L)
  y0 <- vapply(ls, function(l) min(w$y[w$line == l]), 0)
  y1 <- vapply(ls, function(l) max(w$y1[w$line == l]), 0)
  x0 <- vapply(ls, function(l) min(w$x[w$line == l]), 0)
  dfirst <- vapply(seq_along(ls), function(k) any(ph$kind == "date" & ph$line == ls[k] & ph$i0 == first[k]), logical(1))
  has_m <- ls %in% ph$line[ph$kind == "money"]
  has_sm <- ls %in% ph$line[ph$kind == "money" & ph$standalone]
  dx1 <- vapply(ls, function(l) { k <- which(ph$kind == "date" & ph$line == l); if (length(k)) ph$x1[k[1]] else NA_real_ }, 0)
  for (k in seq_along(ls)[-length(ls)]) {
    if (!dfirst[k] || has_m[k] || !has_sm[k + 1L]) next
    if (y0[k + 1L] - y1[k] > 1.0 * h || x0[k + 1L] <= dx1[k]) next
    w$line[w$line == ls[k + 1L]] <- ls[k]
  }
  w
}

# .ar_segment_anchors(w, ph, lines, gap, thr) -- summary figures printed in a box
# beside other print ("RD 2      Previous balance   1,497.28"): the whole line is no
# label, but the words directly before the figure (back to the last gutter) are.
# Only lines that are not already a summary line as a whole.
.ar_segment_anchors <- function(w, ph, lines, gap, thr) {
  out <- data.frame(line = integer(0), class = character(0), label = character(0),
                    value_text = character(0), x = numeric(0), x1 = numeric(0), stringsAsFactors = FALSE)
  if (!nrow(ph)) return(out)
  plain <- lines$line[!nzchar(lines$aclass)]
  for (k in which(ph$kind == "money" & ph$line %in% plain)) {
    i <- ph$i0[k] - 1L; ix <- integer(0)
    while (i >= 1L && w$line[i] == ph$line[k] && w$kind[i] == "text" &&
           (!length(ix) || isTRUE(gap[i + 1L] <= thr))) {
      ix <- c(i, ix); i <- i - 1L
    }
    if (!length(ix)) next
    lab <- paste(w$text[ix], collapse = " ")
    cl <- .ar_anchor_class(.pdf_line_label(lab))
    if (!nzchar(cl)) next
    out[nrow(out) + 1L, ] <- list(ph$line[k], cl, .pdf_line_label(lab), ph$text[k], ph$x[k], ph$x1[k])
  }
  out
}

# .ar_deskew(w, pw, ph) -- a scan's leftover skew, measured from its own print:
# the words of a printed line share one top line only when the page is straight.
# Angles between -2 and 2 degrees are tried in 0.05 degree steps; the one that
# puts the most pairs of words on a common line wins (straight wins ties), and
# every word is turned back by it, so a row's date and its figures share a line
# again.
.ar_deskew <- function(w, pw, ph) {
  k <- which(nchar(w$text) >= 2L & !is.na(w$x) & !is.na(w$y) & !is.na(w$height))
  if (length(k) < 10L) return(w)
  if (length(k) > 400L) k <- k[round(seq(1, length(k), length.out = 400L))]
  cx <- if (isTRUE(pw > 0)) pw / 2 else 300; cy <- if (isTRUE(ph > 0)) ph / 2 else 420
  # Tops, as the lines are grouped by them (an OCR box's foot moves with descenders).
  xc <- w$x[k] + w$width[k] / 2 - cx; yc <- w$y[k] - cy
  tol <- 0.25 * stats::median(w$height[k])
  score <- function(a) {
    r <- -xc * sin(a) + yc * cos(a)
    sum(abs(outer(r, r, "-")) < tol)
  }
  angs <- seq(-2, 2, by = 0.05) * pi / 180
  sc <- vapply(angs, score, 0)
  a <- angs[sc == max(sc)]
  a <- a[order(abs(a))][1]
  if (a == 0 || max(sc) <= sc[which.min(abs(angs))]) return(w)
  xa <- w$x + w$width / 2 - cx; ya <- w$y + w$height / 2 - cy
  w$x <- xa * cos(a) + ya * sin(a) + cx - w$width / 2
  w$y <- -xa * sin(a) + ya * cos(a) + cy - w$height / 2
  w
}

# .ar_ocr_split(w) -- OCR glues a date's pieces that print a narrow space apart
# ("01Dec", "13.Nov", "Jul2025", "01Dec2025"); each piece becomes its own word
# again, its box cut in proportion to its characters. Only date pieces: a figure's characters
# are never touched.
.ar_ocr_split <- function(w) {
  # A stray mark after a numeric date ("13-04-25.") is OCR noise, not print.
  nd <- grepl("^[0-9]{1,2}[-/.][0-9]{1,2}[-/.][0-9]{2,4}[.,:;']$", w$text)
  w$text[nd] <- sub(".$", "", w$text[nd])
  mon <- paste0("(?:", .PDF_MONTH, ")")
  rx <- paste0("^(?:([0-9]{1,2})[.,]?(", mon, ")([0-9]{2,4})?|(", mon, ")([0-9]{4}))[.,]?$")
  hit <- which(grepl(rx, w$text, perl = TRUE, ignore.case = TRUE))
  if (!length(hit)) return(w)
  out <- list(); last <- 0L
  for (i in hit) {
    if (i > last + 1L) out[[length(out) + 1L]] <- w[(last + 1L):(i - 1L), , drop = FALSE]
    m <- regmatches(w$text[i], regexec(rx, w$text[i], perl = TRUE, ignore.case = TRUE))[[1]][-1]
    pc <- m[nzchar(m)]
    nc <- nchar(pc); x0 <- w$x[i] + w$width[i] * c(0, cumsum(nc)[-length(nc)]) / sum(nc)
    r <- w[rep(i, length(pc)), , drop = FALSE]
    r$text <- pc; r$x <- x0; r$width <- w$width[i] * nc / sum(nc)
    out[[length(out) + 1L]] <- r
    last <- i
  }
  if (last < nrow(w)) out[[length(out) + 1L]] <- w[(last + 1L):nrow(w), , drop = FALSE]
  w <- do.call(rbind, out)
  rownames(w) <- NULL
  w
}

# .ar_phrases(w, gap, h, fmts) -- the date and money phrases on a page's lines.
# A date phrase is the longest run of 1-4 close words that parses as a date; a
# money phrase is one figure word plus a currency word or sign in front of it, a
# space-separated thousands group, and a marker or trailing sign behind it.
.ar_phrases <- function(w, gap, h, fmts) {
  n <- nrow(w)
  empty <- data.frame(kind = character(0), line = integer(0), i0 = integer(0), i1 = integer(0),
                      x = numeric(0), x1 = numeric(0), y = numeric(0), text = character(0),
                      fmts = character(0), stringsAsFactors = FALSE)
  if (!n) return(empty)
  lo <- tolower(w$text)
  startable <- grepl("[0-9]", lo) | grepl(paste0("^(?:", .PDF_MONTH, ")[.,]?$"), lo, perl = TRUE) |
    grepl(.AR_WEEKDAY_RX, lo, perl = TRUE)
  dgap <- max(3, 1.0 * h)
  cand_i <- integer(0); cand_k <- integer(0); cand_s <- character(0)
  for (k in 1:4) {
    i <- which(startable)
    i <- i[i + k - 1L <= n]
    if (!length(i)) next
    if (k > 1L) for (d in 1:(k - 1L)) {
      j <- i + d
      ok <- !is.na(gap[j]) & gap[j] <= dgap
      i <- i[ok]
    }
    if (!length(i)) next
    s <- vapply(i, function(a) paste(w$text[a:(a + k - 1L)], collapse = " "), "")
    cand_i <- c(cand_i, i); cand_k <- c(cand_k, rep(k, length(i))); cand_s <- c(cand_s, s)
  }
  out <- list()
  used <- rep(FALSE, n)
  if (length(cand_i)) {
    f <- .ar_date_fmts(cand_s, fmts)
    keep <- nzchar(f) & !w$money_core[cand_i]
    cand_i <- cand_i[keep]; cand_k <- cand_k[keep]; f <- f[keep]
    o <- order(cand_i, -cand_k)
    cand_i <- cand_i[o]; cand_k <- cand_k[o]; f <- f[o]
    for (q in seq_along(cand_i)) {
      ix <- cand_i[q]:(cand_i[q] + cand_k[q] - 1L)
      if (any(used[ix])) next
      used[ix] <- TRUE
      out[[length(out) + 1L]] <- list(kind = "date", i0 = ix[1], i1 = ix[length(ix)], fmts = f[q])
    }
  }
  # Money: each figure word, grown over its currency / sign / marker neighbours.
  same_next <- c(w$line[-1] == w$line[-n], FALSE)
  for (i in which(w$money_core & !used)) {
    s <- i; e <- i
    if (s > 1L && !used[s - 1L] && w$line[s - 1L] == w$line[s] && !is.na(gap[s])) {
      pv <- w$text[s - 1L]
      if (gap[s] <= 0.6 * h && grepl("^[0-9]{1,3},?$", pv) && grepl("^[0-9]{3}[.,][0-9]{2}", w$text[s]))
        s <- s - 1L
      else if (gap[s] <= 1.2 * h && (w$cur[s - 1L] || pv %in% c("-", "+", "(")))
        s <- s - 1L
    }
    if (e < n && same_next[e] && !used[e + 1L] && !is.na(gap[e + 1L])) {
      nx <- w$text[e + 1L]
      if ((w$marker[e + 1L] && gap[e + 1L] <= 2 * h) || (nx %in% c("-", ")") && gap[e + 1L] <= h))
        e <- e + 1L
    }
    used[s:e] <- TRUE
    out[[length(out) + 1L]] <- list(kind = "money", i0 = s, i1 = e, fmts = "")
  }
  if (!length(out)) return(empty)
  ph <- data.frame(kind = vapply(out, `[[`, "", "kind"),
                   i0 = vapply(out, `[[`, 0L, "i0"), i1 = vapply(out, `[[`, 0L, "i1"),
                   fmts = vapply(out, `[[`, "", "fmts"), stringsAsFactors = FALSE)
  ph$line <- w$line[ph$i0]
  ph$x <- w$x[ph$i0]; ph$x1 <- w$x1[ph$i1]; ph$y <- w$y[ph$i0]
  ph$text <- vapply(seq_len(nrow(ph)), function(k) paste(w$text[ph$i0[k]:ph$i1[k]], collapse = " "), "")
  ph <- ph[order(ph$line, ph$x), , drop = FALSE]
  rownames(ph) <- NULL
  ph[, c("kind", "line", "i0", "i1", "x", "x1", "y", "text", "fmts")]
}

# .ar_sign_kind(text) -- how a money phrase carries its sign: "CR", "DR", "OD",
# "()", "-lead", "-trail", "+" or "" (printed without one).
.ar_sign_kind <- function(text, markers = .ar_markers()) {
  s <- toupper(trimws(text))
  out <- rep("", length(s))
  m <- regmatches(s, regexpr("[A-Z]{2}\\s*$", s))
  has_m <- grepl("[A-Z]{2}\\s*$", s)
  suf <- rep("", length(s)); suf[has_m] <- trimws(m)
  cr <- toupper(safe(lex("dr_cr_suffix_credit"), "CR"))
  od <- toupper(safe(lex("overdrawn_markers"), "OD"))
  out[suf %in% cr] <- "CR"
  out[suf %in% od] <- "OD"
  out[!(suf %in% c(cr, od)) & suf %in% markers] <- "DR"
  core <- sub("\\s*[A-Z]{2}\\s*$", "", s)
  out[out == "" & grepl("^[(].*[)]$", core)] <- "()"
  out[out == "" & grepl("^[^0-9]*-", core)] <- "-lead"
  out[out == "" & grepl("-$", core)] <- "-trail"
  out[out == "" & grepl("^[^0-9]*[+]", core)] <- "+"
  out
}

# ---- the document model ----------------------------------------------------------

# .ar_cluster(v, tol) -- single-linkage groups of sorted values no more than `tol`
# apart; returns a group id per value (in the input order), numbered by position.
.ar_cluster <- function(v, tol) {
  if (!length(v)) return(integer(0))
  o <- order(v); g <- integer(length(v)); cur <- 1L; last <- v[o[1]]
  for (q in seq_along(o)) {
    if (v[o[q]] - last > tol) cur <- cur + 1L
    g[o[q]] <- cur; last <- v[o[q]]
  }
  g
}

# .ar_union(lo, hi, slack) -- intervals that overlap (within `slack`) form one group:
# the columns of print, as the eye sees them. Group id per interval, left to right.
.ar_union <- function(lo, hi, slack = 0.5) {
  n <- length(lo); if (!n) return(integer(0))
  o <- order(lo); g <- integer(n); cur <- 0L; reach <- -Inf
  for (i in o) {
    if (lo[i] > reach + slack) cur <- cur + 1L
    g[i] <- cur; reach <- max(reach, hi[i])
  }
  g
}

# .ar_seed_lines(pg) -- a page's SEED lines: a date with a standalone figure to its
# right, on a line that is not a summary line. That is the one shape only a
# transaction has; every column is measured from these first.
.ar_seed_lines <- function(pg) {
  ph <- pg$ph
  if (is.null(ph) || !nrow(ph)) return(integer(0))
  ln <- pg$lines$line[!pg$lines$summary]
  d <- ph[ph$kind == "date", , drop = FALSE]
  m <- ph[ph$kind == "money" & ph$standalone, , drop = FALSE]
  cand <- intersect(intersect(unique(d$line), unique(m$line)), ln)
  cand[vapply(cand, function(l) any(m$x[m$line == l] > min(d$x1[d$line == l])), logical(1))]
}

# .ar_page_shifts(pgs, seeds, tol) -- how far each page's table sits left or right of
# the reference page's (the page with the most seed lines). Scored by how many of the
# page's date left edges and figure right edges land on the reference's; ties go to
# the smallest shift, and no shift at all wins whenever it scores as well.
.ar_page_shifts <- function(pgs, seeds, tol) {
  np <- length(pgs); shift <- rep(0, np)
  nseed <- vapply(seq_len(np), function(p) length(seeds[[p]]), 0)
  if (!any(nseed > 0)) return(shift)
  ref <- which.max(nseed)
  edges <- function(p) {
    ph <- pgs[[p]]$ph; s <- seeds[[p]]
    d <- ph[ph$kind == "date" & ph$line %in% s, , drop = FALSE]
    m <- ph[ph$kind == "money" & ph$standalone & ph$line %in% s, , drop = FALSE]
    list(d = d$x, m = m$x1)
  }
  re <- edges(ref)
  rd <- unique(round(re$d)); rm <- unique(round(re$m))
  for (p in seq_len(np)) {
    if (p == ref || nseed[p] == 0) next
    pe <- edges(p)
    cands <- unique(c(0, round(outer(pe$d, rd, "-")), round(outer(pe$m, rm, "-"))))
    cands <- cands[abs(cands) <= 120]
    score <- vapply(cands, function(s) {
      sum(vapply(pe$d - s, function(v) any(abs(v - re$d) <= tol), logical(1))) +
        sum(vapply(pe$m - s, function(v) any(abs(v - re$m) <= tol), logical(1)))
    }, 0)
    best <- max(score)
    if (score[cands == 0][1] >= best) next
    top <- cands[score == best]
    shift[p] <- top[order(abs(top), top)][1]
  }
  shift
}

# .ar_model(pgs, opts) -- the whole document's column model. Returns NULL when no
# page prints a single dated line with a figure beside it.
#
# Steps, each deterministic:
#   1. seed lines (date + standalone figure); per-page shift from them;
#   2. the date column: the left-edge cluster most seed lines share;
#   3. figure columns: standalone figures on body lines grouped by the ink they
#      occupy; a column needs 3 members, or must sit clear of every description,
#      and is never one printed inside a column of text;
#   4. the table region on each page: the seed lines, extended over the dateless
#      body lines, anchors and wrapped lines that touch them -- never across a
#      heading, footer or a blank band;
#   5. body rows, anchors (opening / closing / carried lines), continuation lines;
#   6. text columns from the white space that runs down every body row.
.ar_model <- function(pgs, opts = list()) {
  np <- length(pgs)
  live <- which(!vapply(pgs, is.null, logical(1)))
  if (!length(live)) return(NULL)
  scan <- any(vapply(pgs[live], function(pg) isTRUE(pg$ocr), logical(1)))
  tol <- if (scan) 4 else 3
  seeds <- lapply(seq_len(np), function(p) if (is.null(pgs[[p]])) integer(0) else .ar_seed_lines(pgs[[p]]))
  if (!sum(lengths(seeds))) return(NULL)
  shift <- if (isFALSE(opts$shift)) rep(0, np) else .ar_page_shifts(pgs, seeds, tol)
  if (!is.null(opts$shift_override)) shift <- opts$shift_override
  # Shifted copies: every measurement below is in the reference page's frame.
  for (p in live) {
    s <- shift[p]
    pgs[[p]]$w$xs <- pgs[[p]]$w$x - s; pgs[[p]]$w$x1s <- pgs[[p]]$w$x1 - s
    if (nrow(pgs[[p]]$ph)) { pgs[[p]]$ph$xs <- pgs[[p]]$ph$x - s; pgs[[p]]$ph$x1s <- pgs[[p]]$ph$x1 - s }
    else { pgs[[p]]$ph$xs <- numeric(0); pgs[[p]]$ph$x1s <- numeric(0) }
  }
  allph <- do.call(rbind, lapply(pgs[live], function(pg) pg$ph))
  key <- function(p, l) paste(p, l)
  seedkey <- unlist(lapply(seq_len(np), function(p) if (length(seeds[[p]])) key(p, seeds[[p]])))
  ph_key <- key(allph$page, allph$line)
  # 2. the date column(s)
  dph <- allph[allph$kind == "date" & ph_key %in% seedkey, , drop = FALSE]
  dk <- key(dph$page, dph$line)
  dg <- .ar_cluster(dph$xs, tol)
  sup <- tapply(dk, dg, function(z) length(unique(z)))
  dmain <- as.integer(names(sup))[order(-sup, tapply(dph$xs, dg, min))][1]
  dcol <- list(x = min(dph$xs[dg == dmain]), x1 = max(dph$x1s[dg == dmain]))
  date2 <- NULL
  rest <- setdiff(as.integer(names(sup)), dmain)
  rest <- rest[sup[as.character(rest)] >= 0.5 * sup[as.character(dmain)]]
  rest <- rest[vapply(rest, function(g) min(dph$xs[dg == g]) > dcol$x1 || max(dph$x1s[dg == g]) < dcol$x, logical(1))]
  # A second date COLUMN (a processed or value date) is printed on the same lines as
  # the transaction date. Dates in another place on OTHER lines are another table
  # -- a cover page's upcoming payments -- and must not shape this one's columns.
  rest <- rest[vapply(rest, function(g) {
    gl <- unique(dk[dg == g]); ml <- unique(dk[dg == dmain])
    length(intersect(gl, ml)) >= 0.5 * length(gl)
  }, logical(1))]
  if (length(rest)) {
    g2 <- rest[order(-sup[as.character(rest)], vapply(rest, function(g) min(dph$xs[dg == g]), 0))][1]
    date2 <- list(x = min(dph$xs[dg == g2]), x1 = max(dph$x1s[dg == g2]))
  }
  in_dcol <- function(ph) ph$kind == "date" & ph$xs <= dcol$x + tol & ph$xs >= dcol$x - tol
  seedkey <- intersect(seedkey, key(allph$page, allph$line)[in_dcol(allph)])
  if (!length(seedkey)) return(NULL)
  # 3-5. figure columns, region and body lines, twice: the second pass measures the
  # columns on every body row (dateless rows included), not only the seeds.
  body <- seedkey
  groups <- NULL
  for (pass in 1:3) {
    groups <- .ar_figure_groups(allph, body, dcol, date2, tol, pgs)
    if (is.null(groups) || !length(groups$cols)) return(NULL)
    reg <- .ar_regions(pgs, allph, groups, seedkey, dcol, tol)
    nb <- reg$body
    if (setequal(nb, body)) break
    body <- nb
  }
  body <- reg$body
  .ar_finish_model(pgs, allph, groups, reg, body, dcol, date2, shift, tol, scan)
}

# .ar_figure_groups(allph, body, dcol, date2, tol, pgs) -- the figure columns: the
# standalone figures on body lines, grouped by the ink they occupy. A group with 3
# members is a column. A smaller group (a sparse money-in column with two deposits
# in it) is a column only when it sits clear of every description, so one stray
# figure inside one description can never become a column. And a group where more
# lines start a text cell than print a figure ("USD 142.32" in a Code column) is
# that text column's, whatever its size.
.ar_figure_groups <- function(allph, body, dcol, date2, tol, pgs) {
  k <- paste(allph$page, allph$line)
  m <- allph[allph$kind == "money" & allph$standalone & k %in% body, , drop = FALSE]
  if (!nrow(m)) return(NULL)
  clear_of_dates <- !(m$xs < dcol$x1 + 1 & m$x1s > dcol$x - 1)
  if (!is.null(date2)) clear_of_dates <- clear_of_dates & !(m$xs < date2$x1 + 1 & m$x1s > date2$x - 1)
  m <- m[clear_of_dates, , drop = FALSE]
  if (!nrow(m)) return(NULL)
  g <- .ar_union(m$xs, m$x1s, 0.5)
  mk <- paste(m$page, m$line)
  sup <- tapply(mk, g, function(z) length(unique(z)))
  # The text zone: how far right the words on body lines reach, figures and dates
  # aside. A weak group must start clear of it.
  tx_reach <- -Inf
  for (pg in pgs) {
    if (is.null(pg)) next
    w <- pg$w
    sel <- paste(pg$page, w$line) %in% body & w$kind == "text" & !w$marker & !w$cur
    if (any(sel)) tx_reach <- max(tx_reach, w$x1s[sel])
  }
  # Text cells that START inside a span, per body line: a column of text there
  # (a code or reference column) owns any figures printed in it.
  tcell <- do.call(rbind, lapply(Filter(Negate(is.null), pgs), function(pg) {
    w <- pg$w
    sel <- paste(pg$page, w$line) %in% body & w$kind == "text" & w$cell_start & !w$marker & !w$cur
    data.frame(key = paste0(pg$page, " ", w$line[sel])[seq_len(sum(sel))], xs = w$xs[sel], x1s = w$x1s[sel],
               stringsAsFactors = FALSE)
  }))
  cols <- list()
  for (gi in sort(unique(g))) {
    s <- m[g == gi, , drop = FALSE]
    strong <- sup[as.character(gi)] >= 3
    if (!strong && !(min(s$xs) > tx_reach && min(s$xs) > dcol$x1)) next
    inside <- tcell$xs <= max(s$x1s) & tcell$x1s >= min(s$xs) & !(tcell$key %in% paste(s$page, s$line))
    if (length(unique(tcell$key[inside])) >= max(3, sup[as.character(gi)])) next
    rs <- diff(range(s$x1s)); ls <- diff(range(s$xs))
    cols[[length(cols) + 1L]] <- list(x = min(s$xs), x1 = max(s$x1s), n = unname(sup[as.character(gi)]),
      med_x1 = stats::median(s$x1s), med_x = stats::median(s$xs),
      align = if (rs <= max(2, ls)) "right" else if (ls <= 2) "left" else "centre",
      strong = unname(strong))
  }
  o <- order(vapply(cols, `[[`, 0, "x"))
  list(cols = cols[o], tx_reach = tx_reach)
}

# .ar_member(ph, col, tol) -- does a money phrase sit in this figure column? Its ink
# must overlap the column's; a figure set too close to its description to be a
# cell of its own still counts when its right edge is the column's right edge.
.ar_member <- function(ph, col, tol) {
  ov <- ph$xs <= col$x1 + 0.5 & ph$x1s >= col$x - 0.5
  ok <- ph$standalone | (col$align == "right" & abs(ph$x1s - col$med_x1) <= tol & isTRUE(col$strong))
  ph$kind == "money" & ov & ok
}

# .ar_regions(pgs, allph, groups, seedkey, dcol, tol) -- on each page, the run of
# lines that IS the table: its seed lines, extended over the dateless body rows,
# anchors and wrapped lines that touch them. A heading, a footer or a gap wider
# than 2.5 line pitches ends it; a wrapped line must sit tight under the line it
# continues. Returns the body line keys and per-page regions.
.ar_regions <- function(pgs, allph, groups, seedkey, dcol, tol) {
  cols <- groups$cols
  zone_lo <- min(dcol$x, vapply(cols, `[[`, 0, "x"))
  zone_hi <- max(dcol$x1, vapply(cols, `[[`, 0, "x1"))
  body <- character(0); regions <- list()
  for (pg in pgs) {
    if (is.null(pg)) next
    p <- pg$page; ln <- pg$lines
    ph <- allph[allph$page == p, , drop = FALSE]
    w <- pg$w
    infig <- rep(FALSE, nrow(ph))
    for (cl in cols) infig <- infig | .ar_member(ph, cl, tol)
    figlines <- unique(ph$line[infig])
    is_seed <- paste(p, ln$line) %in% seedkey
    if (!any(is_seed)) { regions[[as.character(p)]] <- NULL; next }
    bodyish <- ln$line %in% figlines & !ln$summary
    anchor <- ln$summary & ln$line %in% unique(ph$line[ph$kind == "money"])
    # A wrapped line: words only, inside the table's span, none of them over a
    # figure column, and not a page footer.
    wl <- split(seq_len(nrow(w)), w$line)
    textonly <- vapply(as.character(ln$line), function(l) {
      ix <- wl[[l]]
      if (any(w$kind[ix] == "money")) return(FALSE)
      if (min(w$xs[ix]) < zone_lo - 30 || max(w$x1s[ix]) > zone_hi + 30) return(FALSE)
      !any(vapply(cols, function(cl) any(w$xs[ix] < cl$x1 & w$x1s[ix] > cl$x), logical(1)))
    }, logical(1))
    cont <- textonly & !ln$footer & !ln$summary
    pitch <- stats::median(diff(ln$y[is_seed | bodyish]))
    if (!isTRUE(pitch > 0)) pitch <- 2 * pg$h
    first <- min(which(is_seed)); last <- max(which(is_seed))
    while (first > 1L && (bodyish[first - 1L] || anchor[first - 1L]) &&
           ln$y[first] - ln$y[first - 1L] <= 2.5 * pitch) first <- first - 1L
    # A wrapped line is set tight under the line it continues (never under a
    # summary line); a row or a summary line may sit up to 2.5 line pitches down.
    while (last < nrow(ln)) {
      nx <- last + 1L
      ok <- if (cont[nx]) !anchor[last] && ln$y[nx] - ln$y1[last] <= 1.2 * pg$h
            else (bodyish[nx] || anchor[nx] || is_seed[nx]) && ln$y[nx] - ln$y[last] <= 2.5 * pitch
      if (!ok) break
      last <- nx
    }
    # A wrapped line ending the region is kept only when a body row precedes it
    # directly; trailing text after the last row is a note, not a continuation.
    while (last > first && cont[last] && !(bodyish[last] || is_seed[last])) {
      prev <- last - 1L
      if (bodyish[prev] || is_seed[prev] || cont[prev]) break
      last <- last - 1L
    }
    inreg <- seq(first, last)
    b <- inreg[(bodyish[inreg] | is_seed[inreg]) & !ln$summary[inreg]]
    body <- c(body, paste(p, ln$line[b]))
    regions[[as.character(p)]] <- list(first = first, last = last, lines = ln$line[inreg],
      y0 = ln$y[first], y1 = ln$y1[last], cont = ln$line[inreg][cont[inreg] & !(bodyish[inreg] | is_seed[inreg])],
      anchors = ln$line[inreg][anchor[inreg]], pitch = pitch)
  }
  list(body = unique(body), regions = regions)
}

# .ar_finish_model(...) -- everything the prover and the template need, measured
# on the final body rows: per-row cells, anchors, text columns, headings, and the
# pages' own extents of every column.
.ar_finish_model <- function(pgs, allph, groups, reg, body, dcol, date2, shift, tol, scan) {
  cols <- groups$cols
  K <- length(cols)
  rows <- list(); anchors <- list(); row_n <- 0L
  key <- paste(allph$page, allph$line)
  for (pg in pgs) {
    if (is.null(pg)) next
    p <- pg$page
    rg <- reg$regions[[as.character(p)]]
    ln <- pg$lines
    php <- allph[allph$page == p, , drop = FALSE]
    # Lines in printed order, so an anchor knows how many rows were printed before
    # it. Anchors anywhere on the page count (a summary box above the table too);
    # only those inside the table have a place among the rows.
    for (i in seq_len(nrow(ln))) {
      l <- ln$line[i]
      mm <- php[php$line == l, , drop = FALSE]
      if (ln$summary[i]) {
        mon <- mm[mm$kind == "money", , drop = FALSE]
        if (!nrow(mon)) next
        figs <- vapply(seq_len(K), function(j) {
          hit <- which(.ar_member(mon, cols[[j]], tol)); if (length(hit)) mon$text[hit[1]] else NA_character_ }, "")
        anchors[[length(anchors) + 1L]] <- list(page = p, line = l, y = ln$y[i],
          label = ln$label[i], class = ln$aclass[i], raw = ln$raw[i],
          in_table = !is.null(rg) && l %in% rg$lines, before_rows = row_n,
          value_text = mon$text[nrow(mon)], n_money = nrow(mon), figs = figs)
        next
      }
      if (is.null(rg) || !(paste(p, l) %in% body)) {
        sg <- pg$seg[pg$seg$line == l, , drop = FALSE]
        for (s in seq_len(nrow(sg)))
          anchors[[length(anchors) + 1L]] <- list(page = p, line = l, y = ln$y[i],
            label = sg$label[s], class = sg$class[s], raw = ln$raw[i], in_table = FALSE,
            before_rows = row_n, value_text = sg$value_text[s], n_money = 1L,
            figs = rep(NA_character_, K))
        next
      }
      cells <- rep(NA_character_, K)
      for (j in seq_len(K)) {
        hit <- which(.ar_member(mm, cols[[j]], tol))
        if (length(hit)) cells[j] <- paste(mm$text[hit], collapse = " ")
      }
      dd <- mm[mm$kind == "date" & mm$xs <= dcol$x + tol & mm$xs >= dcol$x - tol, , drop = FALSE]
      d2 <- if (!is.null(date2)) mm[mm$kind == "date" & mm$xs <= date2$x + tol & mm$xs >= date2$x - tol, , drop = FALSE] else mm[0, ]
      row_n <- row_n + 1L
      rows[[row_n]] <- list(page = p, line = l, y = ln$y[i], y1 = ln$y1[i], raw = ln$raw[i],
        date = if (nrow(dd)) dd$text[1] else NA_character_, date_fmts = if (nrow(dd)) dd$fmts[1] else "",
        date2 = if (nrow(d2)) d2$text[1] else NA_character_, cells = cells)
    }
  }
  if (!row_n) return(NULL)
  R <- do.call(rbind, lapply(rows, function(r) data.frame(page = r$page, line = r$line, y = r$y,
    y1 = r$y1, raw = r$raw, date = r$date, date_fmts = r$date_fmts, date2 = r$date2,
    stringsAsFactors = FALSE)))
  cells <- do.call(rbind, lapply(rows, function(r) r$cells))
  if (is.null(dim(cells))) cells <- matrix(cells, ncol = K)
  list(pgs = pgs, cols = cols, rows = R, cells = cells, anchors = anchors, regions = reg$regions,
       dcol = dcol, date2 = date2, shift = shift, tol = tol, scan = scan, body = body,
       tx_reach = groups$tx_reach)
}

# ---- columns and boxes -------------------------------------------------------------

# .ar_word_roles(model, roles) -- every word on every body row, labelled with the
# column it belongs to: "date", "weekday" (a weekday printed before the date, kept
# in a band of its own because the table reader trims a date to its format's
# pieces), "date2", "fig<j>" for figure column j, or "text" for everything else
# (descriptions, references, and any figure the arithmetic called "other" that is
# not a column of its own).
.ar_word_roles <- function(model, roles) {
  out <- list()
  keep_fig <- which(roles != "other" | vapply(model$cols, function(c) isTRUE(c$strong), logical(1)))
  for (pg in model$pgs) {
    if (is.null(pg)) next
    p <- pg$page; w <- pg$w
    bl <- model$rows$line[model$rows$page == p]
    if (!length(bl)) next
    sel <- which(w$line %in% bl)
    lab <- rep("text", length(sel))
    ph <- pg$ph
    if (nrow(ph)) {
      ph$xs <- ph$x - model$shift[p]; ph$x1s <- ph$x1 - model$shift[p]
      for (k in which(ph$line %in% bl)) {
        ix <- which(w$phrase[sel] == k & w$kind[sel] == ph$kind[k])
        if (!length(ix)) next
        if (ph$kind[k] == "date") {
          if (abs(ph$xs[k] - model$dcol$x) <= model$tol) {
            lab[ix] <- "date"
            if (length(ix) > 1L && grepl(.AR_WEEKDAY_RX, tolower(w$text[sel[ix[1]]]), perl = TRUE))
              lab[ix[1]] <- "weekday"
          } else if (!is.null(model$date2) && abs(ph$xs[k] - model$date2$x) <= model$tol) lab[ix] <- "date2"
        } else {
          for (j in keep_fig) if (.ar_member(ph[k, , drop = FALSE], model$cols[[j]], model$tol)) {
            lab[ix] <- paste0("fig", j); break }
        }
      }
    }
    out[[length(out) + 1L]] <- data.frame(page = p, line = w$line[sel], idx = sel, x = w$x[sel],
      x1 = w$x1[sel], xs = w$x[sel] - model$shift[p], x1s = w$x1[sel] - model$shift[p],
      cx = w$cx[sel], text = w$text[sel], marker = w$marker[sel], role = lab, stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

# .ar_columns(model, roles, headings) -- the document's columns, left to right, each
# with its field name, its ink on every page, and the boxes the table reader will
# use. Text is split into columns only at white space that runs down EVERY body
# row; the widest text column is the description.
.ar_columns <- function(model, roles, headings = NULL) {
  W <- .ar_word_roles(model, roles)
  if (is.null(W) || !nrow(W)) return(NULL)
  h <- stats::median(vapply(Filter(Negate(is.null), model$pgs), `[[`, 0, "h"))
  tx <- which(W$role == "text")
  if (length(tx)) {
    g <- .ar_union(W$xs[tx], W$x1s[tx], max(4, 0.9 * h))
    W$role[tx] <- paste0("txt", g)
  }
  # A run of nothing but sign words just right of a figure column is that column's
  # marker, printed a little apart ("403.47   DR").
  for (tg in unique(W$role[startsWith(W$role, "txt")])) {
    ix <- which(W$role == tg)
    if (!all(W$marker[ix])) next
    figs <- unique(W$role[startsWith(W$role, "fig")])
    if (!length(figs)) next
    fx1 <- vapply(figs, function(f) max(W$x1s[W$role == f]), 0)
    left <- figs[fx1 <= min(W$xs[ix]) + 0.5]
    if (!length(left)) next
    near <- left[which.max(fx1[left])]
    if (min(W$xs[ix]) - fx1[near] <= 3 * h) W$role[ix] <- near
  }
  ids <- unique(W$role)
  spec <- lapply(ids, function(id) {
    ix <- which(W$role == id)
    list(id = id, xs = min(W$xs[ix]), x1s = max(W$x1s[ix]), chars = sum(nchar(W$text[ix])))
  })
  names(spec) <- ids
  # Fields. Text columns: the one with the most print is the description; the
  # others are named by the heading over them when it says (particulars, code,
  # reference, type, payee), else text1, text2, ... in order.
  txt <- ids[startsWith(ids, "txt")]
  txt <- txt[order(vapply(txt, function(i) spec[[i]]$xs, 0))]
  field <- stats::setNames(rep(NA_character_, length(ids)), ids)
  field[ids == "date"] <- "date"; field[ids == "date2"] <- "date2"; field[ids == "weekday"] <- "weekday"
  for (i in ids[startsWith(ids, "fig")]) {
    j <- as.integer(sub("fig", "", i))
    field[i] <- if (roles[j] == "other") paste0("other", sum(roles[seq_len(j)] == "other")) else roles[j]
  }
  if (length(txt)) {
    desc <- txt[which.max(vapply(txt, function(i) spec[[i]]$chars, 0))]
    field[desc] <- "description"
    k <- 0L
    for (i in setdiff(txt, desc)) {
      hd <- .ar_heading_over(headings, spec[[i]]$xs, spec[[i]]$x1s)
      nm <- if (grepl("particular", hd)) "particulars" else if (grepl("\\bcode\\b", hd)) "code"
            else if (grepl("reference|\\bref\\b", hd)) "reference" else if (grepl("\\btype\\b", hd)) "type"
            else if (grepl("payee|other party|\\bparty\\b", hd)) "other_party" else ""
      if (!nzchar(nm) || nm %in% field) { k <- k + 1L; nm <- paste0("text", k) }
      field[i] <- nm
    }
  }
  W$field <- field[W$role]
  o <- order(vapply(ids, function(i) spec[[i]]$xs, 0))
  list(W = W, ids = ids[o], spec = spec[o], field = field[ids[o]], h = h)
}

# .ar_heading_over(headings, x, x1) -- the heading words printed over an ink span.
.ar_heading_over <- function(headings, x, x1) {
  if (is.null(headings) || !nrow(headings)) return("")
  hw <- headings[headings$x1s >= x - 2 & headings$xs <= x1 + 2, , drop = FALSE]
  tolower(paste(hw$text[order(hw$y, hw$xs)], collapse = " "))
}

# .ar_heading_roles(headings, model) -- the money word printed over each figure
# column: "debit", "credit", "balance", "amount" or NA. The headings are read as
# cells (words a word-space apart; stacked lines joined where they overlap). When
# the cells that name money are exactly as many as the figure columns, they name
# them in order, left to right, so a heading set well off its column still names
# the column it heads (the heading lines are read 60pt wider than the table for
# that); otherwise each column takes the words over its own ink.
.ar_heading_roles <- function(headings, model) {
  role <- function(s) safe(.wa_money_role(s), NA_character_)
  over <- vapply(model$cols, function(cl) role(.ar_heading_over(headings, cl$x, cl$x1)), "")
  headings <- .ar_headings(model, pad = 60)
  if (is.null(headings) || !nrow(headings) || !length(model$cols)) return(over)
  ref <- as.integer(names(which.max(table(model$rows$page))))
  hd <- headings[headings$page == (if (ref %in% headings$page) ref else headings$page[1]), , drop = FALSE]
  g <- .ar_union(hd$xs, hd$x1s, model$pgs[[hd$page[1]]]$thr %||% 6)
  cells <- vapply(sort(unique(g)), function(k) {
    ix <- which(g == k); paste(hd$text[ix][order(hd$y[ix], hd$xs[ix])], collapse = " ") }, "")
  # A "Value date" heading names a date, not money.
  cr <- vapply(cells, function(s) if (grepl("date", s, ignore.case = TRUE)) NA_character_ else role(s), "",
               USE.NAMES = FALSE)
  cr <- cr[!is.na(cr)]
  if (length(cr) == length(model$cols)) cr else over
}

# .ar_boxes(model, cs, page) -- one page's boxes, in that page's own coordinates.
# Each boundary goes in the white space between two neighbouring columns: at the
# middle of the ink gap when every word's centre falls on the right side of it,
# otherwise at the middle of the range the word centres allow. A page with no
# print in a column uses the document's ink for it.
.ar_boxes <- function(model, cs, page) {
  W <- cs$W[cs$W$page == page, , drop = FALSE]
  s <- model$shift[page]
  n <- length(cs$ids)
  ink <- lapply(cs$ids, function(id) {
    ix <- which(W$role == id)
    if (length(ix)) c(lo = min(W$xs[ix]), hi = max(W$x1s[ix]),
                      clo = min((W$xs[ix] + W$x1s[ix]) / 2), chi = max((W$xs[ix] + W$x1s[ix]) / 2))
    else { sp <- cs$spec[[id]]; c(lo = sp$xs, hi = sp$x1s, clo = sp$xs, chi = sp$x1s) }
  })
  lo <- numeric(n); hi <- numeric(n); conflict <- FALSE
  lo[1] <- ink[[1]][["lo"]] - 3
  hi[n] <- ink[[n]][["hi"]] + 3
  if (n > 1L) for (i in seq_len(n - 1L)) {
    a <- ink[[i]]; b <- ink[[i + 1L]]
    mid <- (a[["hi"]] + b[["lo"]]) / 2
    if (a[["chi"]] < b[["clo"]]) {
      bnd <- if (mid > a[["chi"]] && mid < b[["clo"]]) mid else (a[["chi"]] + b[["clo"]]) / 2
    } else { bnd <- mid; conflict <- TRUE }
    hi[i] <- bnd; lo[i + 1L] <- bnd
  }
  pw <- model$pgs[[page]]$width %||% Inf
  data.frame(id = cs$ids, field = unname(cs$field), x_min = round(pmax(0, lo + s), 2),
             x_max = round(pmin(pw, hi + s), 2),
             ink_min = vapply(ink, function(v) v[["lo"]], 0) + s, ink_max = vapply(ink, function(v) v[["hi"]], 0) + s,
             conflict = conflict, stringsAsFactors = FALSE)
}

# .ar_headings(model, pad) -- the heading line(s) printed directly over the table on
# each page: text-only lines within four line pitches above its first line, inside
# its span (widened by `pad` points each side). Shifted to the reference frame.
.ar_headings <- function(model, pad = 10) {
  out <- list()
  lo <- min(model$dcol$x, vapply(model$cols, `[[`, 0, "x")) - pad
  hi <- max(model$dcol$x1, vapply(model$cols, `[[`, 0, "x1")) + pad
  for (pg in model$pgs) {
    if (is.null(pg)) next
    rg <- model$regions[[as.character(pg$page)]]
    if (is.null(rg)) next
    ln <- pg$lines
    top <- ln$y[rg$first]
    cand <- which(ln$y < top & ln$y >= top - 4 * rg$pitch - 2)
    if (!length(cand)) next
    cand <- cand[cand >= max(1L, rg$first - 3L)]
    w <- pg$w
    s <- model$shift[pg$page]
    for (i in cand) {
      ix <- which(w$line == ln$line[i])
      if (any(w$kind[ix] != "text")) next
      ix <- ix[w$x1[ix] - s >= lo & w$x[ix] - s <= hi]
      if (!length(ix)) next
      out[[length(out) + 1L]] <- data.frame(page = pg$page, y = w$y[ix], xs = w$x[ix] - s,
        x1s = w$x1[ix] - s, text = w$text[ix], stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}
