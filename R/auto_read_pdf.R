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
.AR_MONEY_RX <- paste0("^[(]?[-+]?(?:(?:NZ|AU|US|CA|HK|SG)[$]|[^0-9A-Za-z[:space:].,'()+-]{0,3})[-+]?",
                       "(?:[0-9]{1,3}(?:[,.'][0-9]{3})+|[0-9]+)[.,][0-9]{2}[)]?[-+]?",
                       "(?:[CcDdOo][RrDd])?$")
# A function, not a constant: .MONEY_WORDS lives in R/normalise.R, sourced later.
.ar_currency <- function() c(.MONEY_WORDS, "$", "NZ$", "AU$", "US$", "\u00a3", "\u20ac")
# A weekday, short or spelt out in full ("Wed", "Wednesday,", "Saturday").
.AR_WEEKDAY_RX <- "^(?:mon|tue|tues|wed|wednes|thu|thur|thurs|fri|sat|satur|sun)(?:day)?[.,]?$"

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
  ph <- .ar_phrases(w, gap, h, fmts, thr)
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
  # The same for a row whose figures are printed on the LAST of its wrapped lines
  # ("03 Jun 2026  J J WIGGINS  re lot 28" / "bagpipe lessons" / "part 1  -515.37
  # 8,434.71"). Only a dated line that opens with its date and prints no figure,
  # followed -- each set tight under the one above, all starting right of the date
  # -- by up to five lines of words only and then a line whose figures stand
  # apart and that has no date. The FIGURES of that last line move up to the dated
  # line (its words stay where they are, as wrapped text), so the model and the
  # table reader both see one row with its date and its figures.
  has_d <- ls %in% ph$line[ph$kind == "date"]
  moved <- rep(FALSE, length(ls))
  for (k in seq_along(ls)) {
    if (!dfirst[k] || has_m[k] || !any(w$line == ls[k])) next
    j <- k
    while (j < length(ls) && j - k <= 6L) {
      nx <- j + 1L
      if (y0[nx] - y1[j] > 1.0 * h || x0[nx] <= dx1[k] || has_d[nx]) break
      if (has_m[nx]) break
      j <- nx
    }
    f <- j + 1L
    if (j == k || f > length(ls) || moved[f] || !has_sm[f] || has_d[f]) next
    if (y0[f] - y1[j] > 1.0 * h || x0[f] <= dx1[k]) next
    if (!any(w$line == ls[k]) || !any(w$line == ls[f])) next
    fi <- unlist(lapply(which(ph$line == ls[f] & ph$kind == "money"), function(q) ph$i0[q]:ph$i1[q]))
    if (!length(fi)) next
    yk <- min(w$y[w$line == ls[k]])
    w$y1[fi] <- w$y1[fi] - (w$y[fi] - yk)
    w$y[fi] <- yk
    w$line[fi] <- ls[k]
    moved[f] <- TRUE
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
.ar_phrases <- function(w, gap, h, fmts, thr_ph = 1.6 * h) {
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
      if (gap[s] <= 0.6 * h && grepl("^[0-9]{1,3},?$", pv) && grepl("^[0-9]{3}[.,][0-9]{2}", w$text[s])) {
        s <- s - 1L
        # More space-separated thousands groups ("1 234 567.89"), only when the
        # whole run starts a cell (a gutter before it), so a description ending in
        # a number is never read into the figure.
        k <- s
        while (k > 1L && !used[k - 1L] && w$line[k - 1L] == w$line[k] && !is.na(gap[k]) &&
               gap[k] <= 0.6 * h && grepl("^[0-9]{3}$", w$text[k]) && grepl("^[0-9]{1,3}$", w$text[k - 1L])) k <- k - 1L
        if (k < s && (k == 1L || w$line[k - 1L] != w$line[k] || is.na(gap[k]) || gap[k] > thr_ph)) s <- k
      } else if (gap[s] <= 1.2 * h && (w$cur[s - 1L] || pv %in% c("-", "+", "(")))
        s <- s - 1L
      # A sign word printed BEFORE the figure ("DR 32.22", "cr 5.69"), only when the
      # two make a cell of their own (a gutter or the line start before the sign
      # word), so a payee's "DR" is never read as a sign.
      else if (gap[s] <= 1.2 * h && w$marker[s - 1L] &&
               (s - 1L == 1L || w$line[s - 2L] != w$line[s - 1L] || is.na(gap[s - 1L]) || gap[s - 1L] > thr_ph))
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
  # A sign word printed before the figure ("DR 32.22") says the same as after it.
  pre <- rep("", length(s))
  hp <- grepl("^[A-Z]{2}(?![A-Z])", s, perl = TRUE)
  pre[hp] <- substr(s[hp], 1, 2)
  pm <- out == "" & !has_m & pre %in% markers
  out[pm & pre %in% cr] <- "CR"; out[pm & pre %in% od] <- "OD"
  out[pm & !(pre %in% c(cr, od))] <- "DR"
  core <- sub("\\s*[A-Z]{2}\\s*$", "", s)
  core[pm] <- trimws(substring(core[pm], 3))
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
.ar_seed_lines <- function(pg, right = FALSE) {
  ph <- pg$ph
  if (is.null(ph) || !nrow(ph)) return(integer(0))
  ln <- pg$lines$line[!pg$lines$summary]
  d <- ph[ph$kind == "date", , drop = FALSE]
  m <- ph[ph$kind == "money" & ph$standalone, , drop = FALSE]
  cand <- intersect(intersect(unique(d$line), unique(m$line)), ln)
  if (isTRUE(right)) {
    # The mirror image: a table that prints its date LAST, to the right of every
    # figure on the line, with nothing after it.
    w <- pg$w
    return(cand[vapply(cand, function(l) {
      dx <- max(d$x[d$line == l]); dx1 <- max(d$x1[d$line == l])
      all(m$x1[m$line == l] < dx) && max(w$x1[w$line == l]) <= dx1 + 0.5
    }, logical(1))])
  }
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
  # No line anywhere prints a figure right of its date: the table may print its
  # dates last, right of every figure (the mirror image of the usual shape).
  right <- FALSE
  if (!sum(lengths(seeds))) {
    seeds <- lapply(seq_len(np), function(p) if (is.null(pgs[[p]])) integer(0) else .ar_seed_lines(pgs[[p]], right = TRUE))
    right <- TRUE
  }
  # A table-at-a-time reading (R/auto_read_blocks.R) measures one table: only its
  # own blocks' lines seed the columns.
  if (!is.null(opts$keep)) seeds <- lapply(seq_len(np), function(p) intersect(seeds[[p]], opts$keep[[as.character(p)]] %||% integer(0)))
  if (!sum(lengths(seeds))) return(NULL)
  # Rows that each sit a little further right (or left) than the row above are set
  # straight first, on a text page only (.ar_undrift).
  if (!scan) for (p in live) pgs[[p]] <- .ar_undrift(pgs[[p]], seeds[[p]], tol)
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
  date_by <- "only"; date_names <- character(0); date_differ <- 0L
  if (length(rest)) {
    g2 <- rest[order(-sup[as.character(rest)], vapply(rest, function(g) min(dph$xs[dg == g]), 0))][1]
    # Which of the two is the transaction's date is never decided by where the
    # column sits (swapping the columns would change every date): the heading that
    # names the transaction date wins ("Transaction date" over "Processed date",
    # the rule spreadsheets follow, .ar_date_heading_rank). With no heading to
    # decide, the column whose dates come first is shown (a transaction is made
    # before it is processed) and, when the two give different dates, the reading
    # is held back (date_by "open": dates_settled fails).
    h1 <- .ar_date_col_heading(pgs, dph, dg, dmain); h2 <- .ar_date_col_heading(pgs, dph, dg, g2)
    r1 <- .ar_date_heading_rank(h1); r2 <- .ar_date_heading_rank(h2)
    tx_of <- function(g) { v <- dph$text[dg == g]; k <- dk[dg == g]; tapply(v, k, function(z) gsub("[[:space:]]+", " ", z[1])) }
    t1 <- tx_of(dmain); t2 <- tx_of(g2); both <- intersect(names(t1), names(t2))
    date_differ <- sum(t1[both] != t2[both])
    swap <- r2 > r1
    if (r1 == r2 && date_differ > 0L) {
      first <- function(g) { f <- strsplit(dph$fmts[dg == g], "|", fixed = TRUE)
        d <- vapply(seq_along(f), function(i) { fm <- f[[i]][1]
          iso <- if (grepl("%[Yy]", fm)) parse_date(dph$text[dg == g][i], fm)$iso else parse_date(paste(dph$text[dg == g][i], "2000"), paste(fm, "%Y"))$iso
          as.numeric(as.Date(iso)) }, 0)
        sum(d, na.rm = TRUE) }
      swap <- first(g2) < first(dmain)
    }
    if (swap) { tmp <- dmain; dmain <- g2; g2 <- tmp; tmp <- h1; h1 <- h2; h2 <- tmp }
    dcol <- list(x = min(dph$xs[dg == dmain]), x1 = max(dph$x1s[dg == dmain]))
    date2 <- list(x = min(dph$xs[dg == g2]), x1 = max(dph$x1s[dg == g2]))
    date_by <- if (r1 != r2) "heading" else if (date_differ > 0L) "open" else "same"
    date_names <- c(if (nzchar(h1)) sprintf("\"%s\"", h1) else "one", if (nzchar(h2)) sprintf("\"%s\"", h2) else "the other")
  }
  in_dcol <- function(ph) ph$kind == "date" & ph$xs <= dcol$x + tol & ph$xs >= dcol$x - tol
  seedkey <- intersect(seedkey, key(allph$page, allph$line)[in_dcol(allph)])
  if (!length(seedkey)) return(NULL)
  # 3-5. figure columns, region and body lines, twice: the second pass measures the
  # columns on every body row (dateless rows included), not only the seeds.
  body <- seedkey
  groups <- NULL
  for (pass in 1:3) {
    groups <- .ar_figure_groups(allph, body, dcol, date2, tol, pgs, right = right)
    if (is.null(groups) || !length(groups$cols)) return(NULL)
    reg <- .ar_regions(pgs, allph, groups, seedkey, dcol, tol, keep = opts$keep)
    nb <- reg$body
    if (setequal(nb, body)) break
    body <- nb
  }
  body <- reg$body
  block_mode <- !is.null(opts$keep)
  m <- .ar_finish_model(pgs, allph, groups, reg, body, dcol, date2, shift, tol, scan,
                        aside = opts$aside, block_mode = block_mode)
  if (!is.null(m)) { m$date_by <- date_by; m$date_names <- date_names; m$date_differ <- date_differ; m$right_dates <- right }
  if (!is.null(m) && block_mode) {
    m$keep <- opts$keep; m$aside <- opts$aside %||% list(); m$aside_dated <- opts$aside_dated %||% list()
    m$block_mode <- TRUE
  }
  m
}

# .ar_date_col_heading(pgs, dph, dg, g) -- the heading printed over date column g,
# as plain words: on the first page the column's dates are on, going up from its
# first date, the nearest line of words only with a word over the column (lines
# with nothing over the column -- a cardholder's name line, an opening balance in
# the description -- are passed over, at most three), with any words-only line
# stacked tight above it that also has a word over the column ("Transaction" over
# "date"). Only the words whose ink overlaps the column count. "" when none is
# printed.
.ar_date_col_heading <- function(pgs, dph, dg, g) {
  m <- dph[dg == g, , drop = FALSE]
  p <- min(m$page); pg <- pgs[[p]]
  if (is.null(pg) || !nrow(pg$lines)) return("")
  mm <- m[m$page == p, , drop = FALSE]
  lo <- min(mm$xs) - 2; hi <- max(mm$x1s) + 2
  ln <- pg$lines[order(pg$lines$y), , drop = FALSE]; w <- pg$w
  # The top of the first dated line, as the line's own words put it (on a scan a
  # line's words do not share one top), and never that line itself.
  y0 <- min(ln$y[ln$line %in% mm$line])
  over <- function(l) which(w$line == l & w$xs <= hi & w$x1s >= lo)
  heading_line <- function(l) { ix <- over(l); length(ix) > 0L && all(w$kind[w$line == l] == "text") }
  pitch <- 2.2 * pg$h; skipped <- 0L; take <- integer(0)
  for (i in rev(which(ln$y < y0 - 0.5 & !(ln$line %in% mm$line)))) {
    if (y0 - ln$y[i] > 6 * pitch) break
    if (heading_line(ln$line[i])) { take <- i; break }
    # A date or a figure printed over the column: a row of something else, not a heading.
    if (length(over(ln$line[i])) && any(w$kind[over(ln$line[i])] != "text")) break
    skipped <- skipped + 1L
    if (skipped > 3L) break
  }
  if (!length(take)) return("")
  i <- take
  while (i > 1L && ln$y[i] - ln$y1[i - 1L] <= 1.2 * pg$h && heading_line(ln$line[i - 1L])) { i <- i - 1L; take <- c(i, take) }
  sel <- unlist(lapply(ln$line[take], over))
  .ar_head_words(paste(w$text[sel][order(w$y[sel], w$xs[sel])], collapse = " "))
}

# .ar_figure_groups(allph, body, dcol, date2, tol, pgs) -- the figure columns: the
# standalone figures on body lines, grouped by the ink they occupy. A group with 3
# members is a column. A smaller group (a sparse money-in column with two deposits
# in it) is a column only when it sits clear of every description, so one stray
# figure inside one description can never become a column. And a group where more
# lines start a text cell than print a figure ("USD 142.32" in a Code column) is
# that text column's, whatever its size.
.ar_figure_groups <- function(allph, body, dcol, date2, tol, pgs, right = FALSE) {
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
    if (!strong && !(min(s$xs) > tx_reach && (min(s$xs) > dcol$x1 || (isTRUE(right) && max(s$x1s) < dcol$x)))) next
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
.ar_regions <- function(pgs, allph, groups, seedkey, dcol, tol, keep = NULL) {
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
    # One table of a pack: its region never reaches over another table's lines.
    if (!is.null(keep)) {
      allowed <- ln$line %in% (keep[[as.character(p)]] %||% integer(0))
      bodyish <- bodyish & allowed; anchor <- anchor & allowed; cont <- cont & allowed
    }
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
.ar_finish_model <- function(pgs, allph, groups, reg, body, dcol, date2, shift, tol, scan, aside = NULL,
                             block_mode = FALSE) {
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
    # In a table-at-a-time reading, summary lines inside another (set-aside) table
    # are that table's, and lines on pages without this table's rows are not its
    # own, with one exception: a table's closing line can spill onto the next page,
    # and there the summary lines printed above every other table are still its own.
    al <- aside[[as.character(p)]] %||% integer(0)
    spill <- block_mode && is.null(rg) && length(reg$regions) &&
      p == max(as.integer(names(reg$regions))) + 1L
    spill_y <- if (spill && length(al)) min(ln$y[ln$line %in% al]) else Inf
    for (i in seq_len(nrow(ln))) {
      l <- ln$line[i]
      if (block_mode && (l %in% al || (is.null(rg) && !(spill && ln$y[i] < spill_y)))) next
      mm <- php[php$line == l, , drop = FALSE]
      if (ln$summary[i]) {
        mon <- mm[mm$kind == "money", , drop = FALSE]
        if (!nrow(mon)) next
        figs <- vapply(seq_len(K), function(j) {
          hit <- which(.ar_member(mon, cols[[j]], tol)); if (length(hit)) mon$text[hit[1]] else NA_character_ }, "")
        anchors[[length(anchors) + 1L]] <- list(page = p, line = l, y = ln$y[i],
          label = ln$label[i], class = ln$aclass[i], raw = ln$raw[i], named = isTRUE(ln$named[i]),
          in_table = (!is.null(rg) && l %in% rg$lines) || spill, before_rows = row_n,
          under_table = !is.null(rg) && ln$y[i] >= rg$y0,
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
  # In a table-at-a-time reading, a total printed outside the table is this
  # statement's only inside a summary box that also prints an opening or closing
  # balance (a "Total deposits" in another table's "At a glance" box is that
  # table's).
  if (block_mode && length(anchors)) {
    keep_a <- vapply(anchors, function(a) {
      if (!identical(a$class, "total") || isTRUE(a$in_table)) return(TRUE)
      pg <- pgs[[a$page]]; ln <- pg$lines; i <- match(a$line, ln$line)
      lo <- i; while (lo > 1L && ln$y[lo] - ln$y1[lo - 1L] <= 1.2 * pg$h) lo <- lo - 1L
      hi <- i; while (hi < nrow(ln) && ln$y[hi + 1L] - ln$y1[hi] <= 1.2 * pg$h) hi <- hi + 1L
      any(ln$aclass[lo:hi] %in% c("open", "close"))
    }, logical(1))
    anchors <- anchors[keep_a]
  }
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
  # A weekday spelt out in full ("Wednesday, 14 May") is as wide as the date beside
  # it is narrow, so its band and the date's overlap. It is then read as part of
  # the date cell (the table reader drops a leading weekday).
  if (any(W$role == "weekday") && any(W$role == "date") &&
      max(W$x1s[W$role == "weekday"]) >= min(W$xs[W$role == "date"])) W$role[W$role == "weekday"] <- "date"
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

# ---- unlabelled balance lines at the table's edges -------------------------------------

# .ar_edge_lines(model) -- body rows at the ENDS of each page's table that print
# exactly one figure and no date: the balance the table starts from or ends on, or
# one carried over a page break, under a label no dictionary knows ("Treasury at
# dawn  5,559.05"). They are only CANDIDATES: the reading that uses them must put
# that one figure in its balance column and the balance chain must hold through
# it (.ar_anchor_points, check "edge_lines"). Only on a statement that prints a
# date on every other row: there a dateless line is not a transaction, so making
# it a chain point can never drop a row. NULL when there are none.
.ar_edge_lines <- function(model) {
  R <- model$rows; C <- model$cells
  n <- nrow(R)
  if (is.null(R) || n < 3L || isTRUE(model$scan)) return(list())
  nfig <- rowSums(!is.na(C))
  cand <- is.na(R$date) & nfig == 1L
  pages <- sort(unique(R$page))
  edge <- function(i, cls) list(i = i, page = R$page[i], line = R$line[i], class = cls)
  fixed <- list(); top <- integer(0)
  for (p in pages) {
    ix <- which(R$page == p)
    if (length(ix) < 2L) next
    if (p == pages[1]) {
      # On the first page, every dateless one-figure line before the first dated
      # row (a header's summary figures can line up with the balance column).
      k <- 0L
      while (k < length(ix) && cand[ix[k + 1L]]) k <- k + 1L
      if (k > 3L) return(list())
      top <- ix[seq_len(k)]
    } else if (cand[ix[1]]) fixed[[length(fixed) + 1L]] <- edge(ix[1], "edge_in")
    l <- ix[length(ix)]
    if (cand[l] && !(l %in% top))
      fixed[[length(fixed) + 1L]] <- edge(l, if (p == pages[length(pages)]) "edge_bottom" else "edge_out")
  }
  ei <- c(top, vapply(fixed, `[[`, 0L, "i"))
  if (!length(ei)) return(list())
  others <- setdiff(seq_len(n), ei)
  # Every other row prints its own date: no date is carried down on this statement.
  if (length(others) < 2L || anyNA(R$date[others])) return(list())
  if (!length(top)) return(list(fixed))
  # One alternative per line of the top run as the opening balance (the line next
  # to the rows first); the others are set aside -- dateless, before every dated
  # row, they are not transactions. The arithmetic says which, if any, fits.
  lapply(rev(top), function(j) c(fixed, list(edge(j, "edge_top")),
                                  lapply(setdiff(top, j), function(q) edge(q, "edge_skip"))))
}

# .ar_mark_edges(pgs, edges) -- the pages with each edge line made a summary line of
# its edge class, so the model takes it as an anchor and not as a row.
.ar_mark_edges <- function(pgs, edges) {
  for (e in edges) {
    pg <- pgs[[e$page]]
    k <- match(e$line, pg$lines$line)
    if (is.na(k)) next
    pg$lines$summary[k] <- TRUE
    pg$lines$aclass[k] <- e$class
    pgs[[e$page]] <- pg
  }
  pgs
}

# ---- a column of sign marks beside an unsigned amount -----------------------------------

# .ar_indicator_tokens(model) -- per body row, the mark printed in a column of
# two-valued short marks ("C"/"D", "#"/"*", "+"/"-"): the words of three letters at
# most outside every date and figure phrase, grouped by left edge across the rows;
# a group qualifies when it holds exactly one such word on every row and exactly
# two values in all. NA where a row has none; NULL when no such column exists.
.ar_indicator_tokens <- function(model) {
  R <- model$rows; n <- nrow(R)
  if (is.null(R) || n < 4L) return(NULL)
  tk <- list()
  for (i in seq_len(n)) {
    pg <- model$pgs[[R$page[i]]]; w <- pg$w
    ix <- which(w$line == R$line[i] & (w$kind == "text" | (w$marker & is.na(w$phrase))))
    ix <- ix[nchar(w$text[ix]) <= 3L]
    if (length(ix)) tk[[length(tk) + 1L]] <- data.frame(row = i, xs = w$x[ix] - model$shift[R$page[i]],
                                                         text = w$text[ix], stringsAsFactors = FALSE)
  }
  if (!length(tk)) return(NULL)
  tk <- do.call(rbind, tk)
  g <- .ar_cluster(tk$xs, model$tol)
  for (k in unique(g)) {
    s <- tk[g == k, , drop = FALSE]
    if (anyDuplicated(s$row) || length(unique(s$row)) < n) next
    if (length(unique(s$text)) != 2L) next
    out <- rep(NA_character_, n); out[s$row] <- s$text
    return(out)
  }
  NULL
}

# .ar_indicator_fill(model, rd) -- an unsigned amount column (sign convention "B")
# whose row signs the running balance settles everywhere but on a few rows (the
# first row, with no opening balance before it). A column of marks beside it signs
# those rows only when the marks are the reader's own sign words -- one of the
# lexicon's credit markers and one of its debit markers (the words a template's
# indicator column is read by: C / D, CR / DR, IN / OUT ...), or "+" and "-" -- and
# the balance-settled rows show each meaning just that: every settled row of the
# credit mark is money in, every one of the debit mark money out, each seen at
# least twice. A made-up pair, or a code column that merely happens to split the
# same way on a short table, is never taken for signs. Returns rd with those signs
# filled (rd$sign_by_token lists the rows), or rd unchanged.
.ar_indicator_fill <- function(model, rd) {
  if (is.null(rd) || !identical(rd$conv, "B") || is.null(rd$chain)) return(rd)
  a <- which(rd$roles == "amount"); if (length(a) != 1L) return(rd)
  n <- nrow(model$rows)
  sg <- rd$chain$signs[seq_len(n)]
  mag <- abs(.ar_values(model$cells, "auto")$S[, a])
  need <- which(is.na(sg) & !is.na(mag) & mag > 0)
  if (!length(need)) return(rd)
  tok <- .ar_indicator_tokens(model)
  if (is.null(tok)) return(rd)
  vals <- unique(tok[!is.na(tok)])
  set <- !is.na(sg) & !is.na(tok) & !is.na(mag) & mag > 0
  meaning <- vapply(vals, function(v) {
    s <- unique(sg[set & tok == v])
    if (length(s) != 1L || sum(set & tok == v) < 2L) NA_real_ else s
  }, 0)
  if (anyNA(meaning) || meaning[1] == meaning[2]) return(rd)
  up <- toupper(vals)
  crw <- c(toupper(safe(lex("credit_markers"), character(0))), "+")
  drw <- c(toupper(safe(lex("debit_markers"), character(0))), "-")
  inn <- which(up %in% crw & !(up %in% drw)); out <- which(up %in% drw & !(up %in% crw))
  if (length(inn) != 1L || length(out) != 1L || meaning[inn] != 1 || meaning[out] != -1) return(rd)
  fill <- need[!is.na(tok[need])]
  if (!length(fill)) return(rd)
  sg[fill] <- meaning[match(tok[fill], vals)]
  rd$chain$signs[fill] <- sg[fill]
  rd$A[fill] <- sg[fill] * mag[fill]
  rd$sign_by_token <- fill
  rd
}

# ---- rows that drift sideways down the page ---------------------------------------------

# .ar_undrift(pg, seeds, tol) -- a page whose rows are each set a little further
# right (or left) than the row above: measured on the seed lines from TWO columns
# at once, the dates' left edges and the right edges of the figures that end each
# line. Only when both drift the same way at the same rate, steadily (every line
# within 1pt of the straight line through them), and by more than the column
# tolerance over the page, is every line moved back by its own seed line's drift
# (a wrapped line by the drift of the row above it). Otherwise the page is
# returned untouched.
.ar_undrift <- function(pg, seeds, tol) {
  if (is.null(pg) || length(seeds) < 6L) return(pg)
  ph <- pg$ph
  ln <- pg$lines[pg$lines$line %in% seeds, , drop = FALSE]
  ln <- ln[order(ln$y), , drop = FALSE]
  dx <- vapply(ln$line, function(l) min(ph$x[ph$line == l & ph$kind == "date"]), 0)
  mx <- vapply(ln$line, function(l) max(ph$x1[ph$line == l & ph$kind == "money" & ph$standalone]), 0)
  k <- seq_along(dx)
  fd <- stats::lm.fit(cbind(1, k), dx); fm <- stats::lm.fit(cbind(1, k), mx)
  sd <- fd$coefficients[2]; sm <- fm$coefficients[2]
  if (!all(is.finite(c(sd, sm))) || abs(sd) < 0.05 || sign(sd) != sign(sm) ||
      abs(sd - sm) > 0.2 * max(abs(sd), abs(sm)) || abs(sd) * length(k) <= tol) return(pg)
  if (max(abs(fd$residuals)) > 1 || max(abs(fm$residuals)) > 1) return(pg)
  off <- ((dx - dx[1]) + (mx - mx[1])) / 2
  # every line takes the drift of the nearest seed line at or above it
  all_l <- pg$lines$line[order(pg$lines$y)]
  ly <- pg$lines$y[match(all_l, pg$lines$line)]
  lo <- vapply(ly, function(y) { j <- which(ln$y <= y + 0.5); if (length(j)) off[max(j)] else NA_real_ }, 0)
  # lines above the first seed (headings, the header) and far below the last are left alone
  last_y <- max(ln$y1); pitch <- stats::median(diff(ln$y))
  lo[ly > last_y + 3 * pitch] <- NA
  shift <- stats::setNames(ifelse(is.na(lo), 0, lo), all_l)
  s <- shift[as.character(pg$w$line)]
  pg$w$x <- pg$w$x - s; pg$w$x1 <- pg$w$x1 - s; pg$w$cx <- pg$w$cx - s
  if (nrow(ph)) { sp <- shift[as.character(ph$line)]; pg$ph$x <- ph$x - sp; pg$ph$x1 <- ph$x1 - sp }
  pg$lines$x <- pg$lines$x - shift[as.character(pg$lines$line)]
  pg$lines$x1 <- pg$lines$x1 - shift[as.character(pg$lines$line)]
  pg$undrift <- unname(sd)
  pg
}
