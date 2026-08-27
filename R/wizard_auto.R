# wizard_auto.R -- turn an unsupported statement into a starting template so a maintainer's job is
# confirm-and-save. Adds the PDF piece: suggesting column boxes by clustering word positions.

# The leading currency glyph is any run of non-number, non-letter characters, not a literal: a raw
# pound sign is non-ASCII on a C-locale box, and pinning "$" blinded the sniffer to other currencies.
.WA_MONEY <- "^-?[^0-9A-Za-z[:space:].,'()+-]{0,3}-?[0-9][0-9,]*\\.[0-9]{2}$"
.WA_DATE  <- paste0("^[0-9]{1,2}(st|nd|rd|th|ST|ND|RD|TH)?",
                    "([/. -][A-Za-z0-9]{2,9}([/. -][0-9]{2,4})?)?$")

# Is a phrase distinctive enough to identify a bank layout? Shared with validate_template.
.fp_specific <- function(ph) {
  ph <- trimws(as.character(ph))
  nzchar(ph) & ((lengths(strsplit(ph, "\\s+")) >= 2) | (nchar(ph) >= 10))
}

# A line names a LAYOUT when it carries bank, product or statement BRANDING. From the LEXICON:
# hardcoded, an unknown bank's masthead was mistaken for a customer NAME.
.FP_BRAND_DEFAULT <- c("bank", "card", "mastercard", "visa", "amex", "eftpos",
  "account", "statement", "everyday", "savings", "cheque", "current", "credit",
  "debit", "platinum", "gold", "classic", "standard", "airpoints", "rewards",
  "business", "personal", "transaction", "summary", "loan", "mortgage",
  "kiwibank", "westpac", "anz", "asb", "bnz", "tsb", "sbs", "rabobank",
  "heartland", "co-operative", "cooperative")

.fp_brand_words <- function() {
  extra <- safe(as.character(lex("fingerprint_brand_words")), character(0))
  extra <- tolower(trimws(extra[!is.na(extra)]))
  unique(c(.FP_BRAND_DEFAULT, extra[nzchar(extra)]))
}

# Every word is regex-ESCAPED first, so an admin can type "M&S Bank" and a stray metacharacter
# cannot break or silently widen the match.
.fp_brand_rx <- function() {
  w <- gsub("([^A-Za-z0-9 _-])", "\\\\\\1", .fp_brand_words())
  paste0("\\b(", paste(w, collapse = "|"), ")\\b")
}

# A heading carrying a personal title names a PERSON, not a layout: it fingerprints ONE customer
# and writes their name into a template file copied into the offline bundle.
.FP_TITLE_WORDS <- c("mr", "mrs", "ms", "miss", "dr", "mstr", "master",
                     "sir", "madam", "mx", "messrs")
.FP_TITLE_NOT_NAME <- c("cr", "dr", "bal", "balance", "amount", "amt", "account",
                        "accounts", "no", "number", "ref", "reference", "total")

# Shared by the drafter and validate_template, so the hand-edited Advanced-YAML path is covered too.
.fp_has_pii <- function(ph) {
  vapply(as.character(ph), function(p) {
    if (is.na(p)) return(FALSE)
    w <- unlist(regmatches(p, gregexpr("[A-Za-z][A-Za-z'-]*", p)))
    if (length(w) < 2) return(FALSE)
    lw <- tolower(w)
    at <- which(lw %in% .FP_TITLE_WORDS)
    at <- at[at < length(lw)]                    # only a title with a word after it
    length(at) > 0 && any(!(lw[at + 1L] %in% .FP_TITLE_NOT_NAME))
  }, logical(1), USE.NAMES = FALSE)
}

# The same rule over any other text captured verbatim off the page. The gate guarded exactly one
# field: "Prepared for Mr John Smith" was refused as a fingerprint phrase and accepted as a table
# TITLE and a label wording.
.fp_pii_problems <- function(x, what = "phrase") {
  v <- trimws(as.character(unlist(x %||% character(0))))
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(character(0))
  hit <- .fp_has_pii(v)
  if (!any(hit)) return(character(0))
  sprintf(paste0("the %s %s names a person, and a template describes a layout ",
                 "and never a customer -- use the wording without the name"),
          what, paste(sprintf("'%s'", unique(v[hit])), collapse = ", "))
}

# A line names a CUSTOMER when it carries a personal title anywhere, or is a bare five-word-or-fewer
# name with no branding. Five, not four: a full name with middle names ran past the old cap.
.fp_is_customer <- function(ln, brand_rx = .fp_brand_rx()) {
  lo <- tolower(trimws(ln))
  w <- unlist(regmatches(ln, gregexpr("[A-Za-z]+", ln)))
  isTRUE(.fp_has_pii(ln)) ||
    (length(w) <= 5 && length(w) >= 1 && all(grepl("^[A-Za-z'-]+$", w)) && !grepl(brand_rx, lo))
}

# Up to n DISTINCTIVE phrases. Prefers MULTI-WORD phrases: single generic words match almost any
# statement and turn a correct "unsupported" verdict into a wrong match.
header_phrases <- function(input, n = 3) {
  txt <- paste(input$pages %||% character(0), collapse = "\n")
  lines <- trimws(unlist(strsplit(txt, "\n", fixed = TRUE)))
  lines <- gsub("\\s+", " ", lines[nzchar(lines)])
  if (!length(lines)) return(character(0))
  out <- character(0)
  hdr_keys <- lex("header_keywords")   # from the lexicon (admin/ML-extendable)
  brand_rx <- .fp_brand_rx()           # ditto -- resolved once per draft
  # A distinctive brand or product line near the top: multi-word, no digits, not just column labels,
  # and not a customer name.
  top <- utils::head(lines, 15L)
  titleish <- vapply(top, function(ln) {
    w <- unlist(regmatches(ln, gregexpr("[A-Za-z]+", ln)))
    nchar(ln) >= 10 && nchar(ln) <= 60 && length(w) >= 2 && !grepl("[0-9]", ln) &&
      (length(w) == 0 || mean(tolower(w) %in% hdr_keys) < 0.5) &&
      !.fp_is_customer(ln, brand_rx)             # never a customer / account-holder name
  }, logical(1))
  kept <- top[titleish]
  brandy <- grepl(brand_rx, tolower(kept))       # branded lines first (most distinctive)
  out <- c(out, utils::head(c(kept[brandy], kept[!brandy]), 2L))
  score <- vapply(lines, function(ln) {
    w <- tolower(unlist(regmatches(ln, gregexpr("[A-Za-z]+", ln))))
    length(unique(w[w %in% hdr_keys]))
  }, integer(1))
  best <- which.max(score)
  if (length(best) && score[best] >= 2 && nchar(lines[best]) <= 60)
    out <- c(out, lines[best])
  if (length(best) && score[best] >= 1) {
    words <- unlist(regmatches(lines[best], gregexpr("[A-Za-z]+", lines[best])))
    out <- c(out, words[tolower(words) %in% hdr_keys])
  }
  out <- unique(out[nzchar(out)])
  utils::head(out[.fp_offerable(out)], n)
}

# Would the SAVE accept this phrase on its own? The picker is built from header_phrases(), so
# choosing "Date" got a refusal from the screen that had just suggested it.
.fp_offerable <- function(ph) {
  if (!length(ph)) return(logical(0))
  vapply(as.character(ph), function(p)
    length(safe(.fp_fingerprint_problems(list(p), 1L, "pdf"), "unavailable")) == 0L,
    logical(1), USE.NAMES = FALSE)
}

.cluster_1d <- function(x, gap) {
  if (!length(x)) return(integer(0))
  o <- order(x); xs <- x[o]
  g <- cumsum(c(TRUE, diff(xs) > gap))
  grp <- integer(length(x)); grp[o] <- g; grp
}

# THE COLUMN SNIFFER: a starting point the analyst confirms or redraws, but one that READS ROWS - it
# measures the table the way the READER will. Three faults each emptied the preview on real statements:
# the month was not in the date column ("28 Apr" is two words and only "28" is date-shaped); money was
# measured over the whole page, so clustering found five money columns on a three-column table; and the
# printed column labels were never read, though the header row says which money column is the withdrawal.

# The reader's own month spellings as a whole-token test, built on call so this file does not depend
# on source() order.
.wa_month_rx <- function() paste0("^(?:", .PDF_MONTH, ")\\.?$")

.wa_date_part <- function(tok) {
  lo <- tolower(trimws(as.character(tok)))
  grepl("^[0-9]{1,4}$", lo) | grepl(.WA_DATE, lo) |
    grepl(.wa_month_rx(), lo, perl = TRUE) |
    grepl("^(?:mon|tue|tues|wed|thu|thur|thurs|fri|sat|sun)(?:day)?\\.?$", lo)
}

# What the statement's own heading calls this money column, from the LEXICON. NA means the heading
# did not say, and the caller falls back to position.
.wa_money_role <- function(label) {
  s <- tolower(trimws(label %||% ""))
  if (!nzchar(s)) return(NA_character_)
  rx <- function(cat, dflt) paste(safe(as.character(lex(cat)), dflt), collapse = "|")
  d <- grepl(rx("amount_style_debit_headers",  c("debit", "withdrawal")), s)
  cr <- grepl(rx("amount_style_credit_headers", c("credit", "deposit")), s)
  if (grepl("balance", s)) return("balance")
  if (d && !cr) return("debit")
  if (cr && !d) return("credit")
  if (grepl("amount|value", s)) return("amount")
  NA_character_
}

# Money columns labelled by POSITION, for a statement whose header row could not be read. Three is
# the money-out / money-in / balance shape every NZ retail statement here uses; four or more is a
# guess, so it returns NULL and the caller says so.
.wa_positional_roles <- function(n) switch(as.character(n),
  "1" = "amount", "2" = c("amount", "balance"),
  "3" = c("debit", "credit", "balance"), NULL)

# One page's word boxes with the derived fields every step reads, including the READER's own visual
# row grouping - any other notion of "row" would measure a table the reader will not see.
.wa_page_frame <- function(w) {
  w <- as.data.frame(w, stringsAsFactors = FALSE)
  w <- w[order(w$y, w$x), , drop = FALSE]
  w$cx <- w$x + w$width / 2
  w$x1 <- w$x + w$width
  w$row <- .group_rows(w$y, PARAM_PDF_ROW_TOL)
  w$money <- grepl(.WA_MONEY, w$text)
  w$dateish <- grepl(.WA_DATE, w$text) & grepl("[0-9]", w$text) & !w$money
  w
}

# The drafter's honesty check: a document that never prints a dated money line is a guide or a
# form, and drafting it anyway produced a template that read nothing and said nothing.
pdf_has_transaction_rows <- function(input) {
  for (w in input$words %||% list()) {
    if (is.null(w) || !nrow(w)) next
    w <- .wa_page_frame(w)
    for (ix in split(seq_len(nrow(w)), w$row))
      if (any(w$money[ix]) && any(w$dateish[ix])) return(TRUE)
  }
  FALSE
}

suggest_pdf_columns <- function(input, gap = 18) {
  wl <- input$words %||% list()
  if (!length(wl)) return(data.frame())
  cnt <- vapply(wl, function(w) if (is.null(w) || !nrow(w)) 0L else
    sum(grepl(.WA_MONEY, w$text)), integer(1))
  pg <- which.max(cnt); if (!length(pg) || cnt[pg] == 0) return(data.frame())
  w <- .wa_page_frame(wl[[pg]])
  rows <- split(seq_len(nrow(w)), w$row)

  # THE HEADER ROW: the visual row naming the most distinct column keywords - the statement telling
  # us its own layout.
  hdr_keys <- safe(tolower(as.character(lex("header_keywords"))), character(0))
  hscore <- vapply(rows, function(ix) {
    wd <- tolower(unlist(regmatches(w$text[ix], gregexpr("[A-Za-z]+", w$text[ix]))))
    length(unique(wd[wd %in% hdr_keys]))
  }, integer(1))
  hrow <- if (length(hscore) && max(hscore) >= 2L) which.max(hscore) else NA_integer_
  hdr <- if (!is.na(hrow)) w[rows[[hrow]], , drop = FALSE] else NULL

  # A printed transaction carries a date AND money on one baseline, BELOW the header row. All three
  # earn their keep: a summary block carries money with no date, a heading carries neither, and an
  # "upcoming payments" table above the header carries both while its columns are elsewhere.
  has_dm <- vapply(rows, function(ix) any(w$money[ix]) && any(w$dateish[ix]), logical(1))
  below <- if (is.na(hrow)) rep(TRUE, length(rows)) else seq_along(rows) > hrow
  is_txn <- if (any(has_dm & below)) has_dm & below else has_dm
  tw <- if (any(is_txn)) w[unlist(rows[is_txn], use.names = FALSE), , drop = FALSE] else w

  # A money column is right-aligned and its label left-aligned, so they only partly overlap; any
  # overlap is that column's label.
  .hdr_span <- function(lo, hi) {
    if (is.null(hdr)) return(NULL)
    sel <- which(hdr$x1 >= lo & hdr$x <= hi)
    if (!length(sel)) return(NULL)
    list(text = paste(hdr$text[sel], collapse = " "),
         x_min = min(hdr$x[sel]), x_max = max(hdr$x1[sel]))
  }

  mi <- which(tw$money)
  if (!length(mi)) return(data.frame())
  mg <- .cluster_1d(tw$cx[mi], gap)
  ord <- as.integer(names(sort(tapply(tw$cx[mi], mg, mean))))
  money <- lapply(ord, function(k) {
    ix <- mi[mg == k]
    list(x_min = min(tw$x[ix]), x_max = max(tw$x1[ix]))
  })
  # The heading's left edge is where the BANK says the column starts - the only honest boundary
  # against a left-aligned description that may run longer on a page we were not shown.
  roles <- rep(NA_character_, length(money))
  for (k in seq_along(money)) {
    h <- .hdr_span(money[[k]]$x_min, money[[k]]$x_max)
    if (is.null(h)) next
    money[[k]]$x_min <- min(money[[k]]$x_min, h$x_min)
    money[[k]]$x_max <- max(money[[k]]$x_max, h$x_max)
    roles[k] <- .wa_money_role(h$text)
  }

  # A money column the heading NAMES but this page never printed a figure in - commonly Deposits on
  # a first page of withdrawals. Without it the template has no credit band and every later deposit
  # is dropped silently. Only when the heading was read for every column we DID see.
  if (!is.null(hdr) && !anyNA(roles)) {
    hg <- .cluster_1d(hdr$cx, gap)
    for (k in unique(hg)) {
      ix <- which(hg == k)
      r <- .wa_money_role(paste(hdr$text[ix], collapse = " "))
      if (is.na(r) || r %in% roles) next
      lo <- min(hdr$x[ix]); hi <- max(hdr$x1[ix])
      if (any(vapply(money, function(m) m$x_max >= lo && m$x_min <= hi, logical(1)))) next
      money <- c(money, list(list(x_min = lo, x_max = hi))); roles <- c(roles, r)
    }
    o <- order(vapply(money, `[[`, numeric(1), "x_min"))
    money <- money[o]; roles <- roles[o]
  }

  # A heading-derived set is only usable when it names every money column, names none twice, and
  # includes somewhere for the transaction's own figure to go.
  if (anyNA(roles) || anyDuplicated(roles) ||
      !any(roles %in% c("amount", "debit", "credit")))
    roles <- .wa_positional_roles(length(money))
  if (is.null(roles)) return(data.frame())     # cannot say which column is which

  # THE DATE COLUMN: the leftmost cluster of date-shaped tokens, GROWN along each row over the rest
  # of the printed date. Growth stops at a gap wider than a printed space - a column gutter runs
  # 11-28pt where the space inside a date runs 2-4pt, so the date never reaches the description.
  di <- which(tw$dateish)
  if (!length(di)) return(data.frame())
  dg <- .cluster_1d(tw$cx[di], gap)
  date_ix <- di[dg == dg[which.min(tw$cx[di])]]
  for (a in date_ix) {
    same <- which(tw$row == tw$row[a] & tw$x > tw$x[a])
    same <- same[order(tw$x[same])]
    prev <- a
    for (b in same) {
      lh <- suppressWarnings(as.numeric(tw$height[prev]))
      if (!isTRUE(lh > 0)) lh <- 10                  # unknown line height -> A4 body text
      if (tw$x[b] - tw$x1[prev] > 0.75 * lh) break
      if (!.wa_date_part(tw$text[b])) break
      date_ix <- c(date_ix, b); prev <- b
    }
  }
  date_ix <- unique(date_ix)
  dh <- .hdr_span(min(tw$x[date_ix]), max(tw$x1[date_ix]))
  date_col <- list(x_min = min(c(tw$x[date_ix], dh$x_min)),
                   x_max = max(c(tw$x1[date_ix], dh$x_max)))

  # THE DESCRIPTION: everything between the date and the first money column. Its right boundary is
  # that column's LEFT edge, never a midpoint - a verbatim description must not lose its last word.
  first_money <- money[[1]]$x_min
  desc_ix <- which(tw$cx > date_col$x_max & tw$cx < first_money)
  desc_col <- if (length(desc_ix))
    list(x_min = min(tw$x[desc_ix]), x_max = max(tw$x1[desc_ix])) else NULL

  cols <- list(list(field = "date", kind = "date", x_min = date_col$x_min, x_max = date_col$x_max))
  if (!is.null(desc_col)) cols[[length(cols) + 1L]] <-
    list(field = "description", kind = "text", x_min = desc_col$x_min, x_max = desc_col$x_max)
  for (k in seq_along(money)) cols[[length(cols) + 1L]] <-
    list(field = roles[k], kind = "money", x_min = money[[k]]$x_min, x_max = money[[k]]$x_max)

  # TILE THE BANDS: measured extents are what THIS page happened to print, and a longer figure on a
  # page we were not shown falls between two tight bands and is lost without a word.
  n <- length(cols)
  bnd <- numeric(n - 1L)
  for (i in seq_len(n - 1L)) {
    lo <- cols[[i]]$x_max; hi <- cols[[i + 1L]]$x_min
    b <- if (identical(cols[[i]]$field, "description")) hi else (lo + hi) / 2
    # A boundary must never cut into either column's own words: extents can OVERLAP where a heading
    # is printed wider than its figures, and a boundary left of the last word truncates it.
    bnd[i] <- if (lo <= hi) min(max(b, lo), hi) else (lo + hi) / 2
  }
  left_edge <- cols[[1L]]$x_min - 3          # the date's own words, not the row's
  # The right edge follows the last money column and its sign marker and stops there: running to the
  # margin swallowed hand-written notes printed beside the table into the balance cell.
  mk <- toupper(unlist(lapply(c("overdrawn_markers", "dr_cr_suffix_debit",
    "dr_cr_suffix_credit"), function(k) safe(as.character(lex(k)), character(0)))))
  last <- cols[[n]]
  right_edge <- last$x_max + 3
  near <- which(tw$x >= last$x_max & tw$x - last$x_max <= 0.75 * tw$height &
                toupper(trimws(tw$text)) %in% mk)
  if (length(near)) right_edge <- max(right_edge, max(tw$x1[near]) + 3)
  for (i in seq_len(n)) {
    cols[[i]]$x_min <- if (i > 1L) bnd[i - 1L] else left_edge
    cols[[i]]$x_max <- if (i < n) bnd[i] else right_edge
  }

  out <- do.call(rbind, lapply(cols, function(c) data.frame(field = c$field,
    x_min = floor(c$x_min), x_max = ceiling(c$x_max), kind = c$kind,
    stringsAsFactors = FALSE)))
  out <- out[order(out$x_min), , drop = FALSE]
  rownames(out) <- NULL
  # The cells these bands actually yield, so the sniffs that follow read the same strings the reader
  # will: a currency mark in the DESCRIPTION is a foreign purchase, not the statement's currency.
  cells <- function(b) {
    v <- vapply(split(seq_len(nrow(tw)), tw$row), function(ix) {
      s <- ix[tw$cx[ix] >= b$x_min[1] & tw$cx[ix] <= b$x_max[1]]
      if (!length(s)) "" else paste(tw$text[s][order(tw$x[s])], collapse = " ")
    }, character(1), USE.NAMES = FALSE)
    v[nzchar(v)]
  }
  attr(out, "page") <- as.integer(pg)
  attr(out, "date_cells") <- cells(out[out$field == "date", , drop = FALSE])
  attr(out, "money_cells") <- unlist(lapply(which(out$kind == "money"),
    function(i) cells(out[i, , drop = FALSE])), use.names = FALSE) %||% character(0)
  out
}
