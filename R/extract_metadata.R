# extract_metadata.R -- GENERIC statement metadata and multi-statement detection. Pattern-based
# only, with no bank-specific logic; NA for anything not present.

.ACCT_RX  <- "[0-9]{2}-[0-9]{4}-[0-9]{6,7}-[0-9]{2,3}"          # NZ bank account
.CARD_RX  <- "[0-9]{4}[- ]?[0-9X*]{4}[- ]?[0-9X*]{4}[- ]?[0-9]{4}" # masked card
.PAGE1_MARKER_RX <- "[Pp]age\\s+1\\s+of\\s+[0-9]+"

.all_matches <- function(text, rx, perl = FALSE)
  unique(regmatches(text, gregexpr(rx, text, perl = perl))[[1]])

# ONE SPAN over several printed periods: reading only the FIRST made two checks fail on a file that
# was fine, so the proof becomes first opening + every transaction = last closing. The DANGER is the
# pairing - a wrong opening/closing pair reconciles to a plausible, wrong figure - so the balances
# move only when the file's structure PROVES the pairing.

# Every printed period range read as two dates, in document order; NULL if ANY cannot be. All-or-
# nothing on purpose: one unreadable range means the ORDER of the whole set is unknown.
.period_bounds <- function(periods, date_rx) {
  rows <- lapply(periods, function(p) {
    ds <- regmatches(p, gregexpr(date_rx, p))[[1]]
    if (length(ds) < 2) return(NULL)
    s <- .plausible_period_date(ds[1]); e <- .plausible_period_date(ds[2])
    if (is.na(s) || is.na(e) || s > e) return(NULL)
    data.frame(raw = p, start_raw = ds[1], end_raw = ds[2], start = s, end = e,
               stringsAsFactors = FALSE)
  })
  if (!length(rows) || any(vapply(rows, is.null, logical(1)))) return(NULL)
  do.call(rbind, rows)
}

# Returns the merged span (or NA to keep today's values), the paired balances (or NULL) and one
# plain sentence. Only called when more than one period was printed.
.period_span <- function(pages, periods, date_rx, ospec, cspec, dict) {
  quiet <- list(period_start = NA_character_, period_end = NA_character_,
                opening = NULL, closing = NULL, note = NA_character_)

  pb <- .period_bounds(periods, date_rx)
  if (is.null(pb))
    return(modifyList(quiet, list(note = sprintf(paste0(
      "%d statement periods are printed but at least one could not be read as two dates, so ",
      "they could not be put in date order. This file is read as its FIRST printed period ",
      "only, so the period and balance checks may fail on it."), length(periods)))))

  pb <- pb[!duplicated(paste(pb$start, pb$end)), , drop = FALSE]
  if (nrow(pb) < 2) return(modifyList(quiet, list(n = nrow(pb))))

  pb <- pb[order(pb$start, pb$end), , drop = FALSE]
  span_start <- pb$start_raw[1]; span_end <- pb$end_raw[nrow(pb)]

  # NOTHING MOVES UNTIL THE WHOLE THING IS PROVEN - not the balances, and not the dates. A statement
  # prints plenty of date ranges that are not periods, and the reader takes its YEAR CONTEXT from
  # period_start/period_end: a January statement mentioning a 2024-2026 loan term resolved every
  # year-less "15 Jan" to 2024, and dates_within_period read the same widened period and PASSED.
  unproven <- function(why) list(
    period_start = NA_character_, period_end = NA_character_, n = nrow(pb),
    opening = NULL, closing = NULL,
    note = sprintf(paste0("%d date ranges are printed on this file, but %s, so it is read as ",
                          "its FIRST printed period only. If this really is one account over ",
                          "%s to %s, the period and balance checks may fail on it."),
                   nrow(pb), why, span_start, span_end))

  # Contiguity: consecutive periods of one account meet end-to-start. A GAP means the span would
  # silently cover days no printed period covers; an OVERLAP means these are not one account's
  # consecutive sections. A SHARED BOUNDARY DAY IS NOT AN OVERLAP - banks print the previous
  # closing date as the next opening date.
  gaps <- character(0); overlap <- FALSE
  for (k in seq_len(nrow(pb))[-1]) {
    if (pb$start[k] < pb$end[k - 1L]) overlap <- TRUE
    else if (pb$start[k] > pb$end[k - 1L] + 1)
      gaps <- c(gaps, sprintf("%s to %s", format(pb$end[k - 1L] + 1), format(pb$start[k] - 1)))
  }
  if (overlap) return(unproven("they overlap, so they are not one account's consecutive sections"))

  # Sections: each period's line down to the line before the next period's. Balances are read PER
  # SECTION with the very same matcher used on the whole document.
  lines <- trimws(unlist(strsplit(paste(pages %||% character(0), collapse = "\n"),
                                  "\n", fixed = TRUE)))
  pos <- vapply(pb$raw, function(p) {
    i <- which(grepl(p, lines, fixed = TRUE)); if (length(i)) i[1] else NA_integer_
  }, integer(1), USE.NAMES = FALSE)
  if (anyNA(pos) || anyDuplicated(pos))
    return(unproven("their sections could not be told apart on the page"))
  o <- order(pos); bnd <- c(pos[o], length(lines) + 1L)
  sec <- vector("list", nrow(pb))
  for (k in seq_along(o)) sec[[o[k]]] <- lines[bnd[k]:(bnd[k + 1L] - 1L)]

  # ONE ACCOUNT. The chain proof below can be satisfied by coincidence - two dormant accounts that
  # both open and close on 0.00 chain perfectly. Sections that name no account are not evidence.
  accts <- lapply(sec, function(s) unique(.all_matches(paste(s, collapse = " "), .ACCT_RX)))
  named <- unique(unlist(accts[lengths(accts) > 0]))
  if (length(named) > 1L)
    return(unproven("the sections name more than one account, so they are not one account's sections"))

  # THE PAIRING EVIDENCE: every period's section prints exactly ONE opening and exactly ONE closing.
  # That is what makes "the earliest period's opening" a fact rather than a guess.
  ob <- lapply(sec, function(s) match_label(ospec, s, dict))
  cb <- lapply(sec, function(s) match_label(cspec, s, dict))
  exactly_one <- function(r) isTRUE(r$n == 1L) && !is.na(r$value)
  if (!all(vapply(ob, exactly_one, logical(1))) || !all(vapply(cb, exactly_one, logical(1))))
    return(unproven("not every period prints exactly one opening and one closing balance"))

  # SAME ACCOUNT, PROVEN BY THE MONEY: each period must CLOSE on the figure the next period OPENS
  # on - a better proof than matching account numbers, since a real statement names other accounts
  # in its transfer narratives.
  opens  <- .num(vapply(ob, function(r) r$value, character(1)))
  closes <- .num(vapply(cb, function(r) r$value, character(1)))
  links  <- abs(closes[-length(closes)] - opens[-1]) < PARAM_MONEY_TOL
  if (anyNA(links) || !all(links))
    return(unproven(paste0("one period does not close on the figure the next opens on, so they ",
                           "are not one account's consecutive sections")))

  list(period_start = span_start, period_end = span_end, n = nrow(pb), merged = TRUE,
       opening = ob[[1]]$value, closing = cb[[nrow(pb)]]$value,
       note = if (!length(gaps)) NA_character_ else sprintf(paste0(
         "%d statement periods are read as one span %s to %s, but they do NOT join up: no ",
         "printed period covers %s. Transactions in %s are not part of this file - check it ",
         "is complete before relying on the totals."),
         nrow(pb), span_start, span_end, paste(gaps, collapse = "; "),
         if (length(gaps) > 1) "those windows" else "that window"))
}

extract_metadata <- function(input, dict = default_label_dict()) {
  pages <- input$pages %||% character(0)
  text <- paste(pages, collapse = "\n")

  pages_actual <- input$meta$page_count %||% (if (length(pages)) length(pages) else NA_integer_)

  max_page_pt <- NA_real_
  if (identical(input$kind, "pdf") && requireNamespace("pdftools", quietly = TRUE) &&
      !is.null(input$path) && file.exists(input$path)) {
    sz <- tryCatch(pdftools::pdf_pagesize(input$path), error = function(e) NULL)
    if (!is.null(sz)) max_page_pt <- suppressWarnings(max(c(sz$width, sz$height), na.rm = TRUE))
  }

  pageofs <- .all_matches(text, "[Pp]age\\s+[0-9]+\\s+of\\s+[0-9]+")
  pages_stated <- if (length(pageofs))
    suppressWarnings(max(as.integer(sub(".*of\\s+([0-9]+).*", "\\1", pageofs)), na.rm = TRUE)) else NA_integer_
  page1_markers <- length(regmatches(text, gregexpr(.PAGE1_MARKER_RX, text))[[1]])

  # Two dates joined by a connective, regardless of the wording around them. The dash class is built
  # from code points so the pattern is valid UTF-8 whatever the locale.
  dash <- paste0("-", intToUtf8(0x2013), intToUtf8(0x2014))
  date_rx <- lex("date_regex"); conn <- paste(lex("period_connectives"), collapse = "|")
  per_rx <- sprintf("(?:%s)\\s*(?:%s|[%s])\\s*(?:%s)", date_rx, conn, dash, date_rx)
  periods <- .all_matches(enc2utf8(text), per_rx, perl = TRUE)
  period_start <- NA_character_; period_end <- NA_character_
  if (length(periods)) {
    ds <- regmatches(periods[1], gregexpr(date_rx, periods[1]))[[1]]
    if (length(ds) >= 2) { period_start <- ds[1]; period_end <- ds[2] }
  }
  # Resolved ONCE: the whole-document match and the per-period match inside .period_span must ask
  # exactly the same question, or the two could disagree about what an opening balance is.
  ospec <- dict$opening_balance %||% list(any_of = "opening balance", value = "money")
  cspec <- dict$closing_balance %||% list(any_of = "closing balance", value = "money")
  # ONE STATEMENT WITH SEVERAL SECTIONS IS NOT A BUNDLE: a statement restarts its page numbering, so
  # more than one "Page 1 of N" means separately-issued documents were concatenated, and merging
  # would throw the per-statement anchors away. Same marker split.R cuts on.
  span <- if (length(periods) > 1 && page1_markers <= 1)
    .period_span(pages, periods, date_rx, ospec, cspec, dict) else NULL
  period_note <- span$note %||% NA_character_
  if (!is.null(span) && !is.na(span$period_start)) {
    period_start <- span$period_start; period_end <- span$period_end
  }
  if (is.na(period_start) || is.na(period_end)) {
    ps <- match_label(dict$statement_start %||% list(any_of = "opening date", value = "date"), pages, dict)
    pe <- match_label(dict$statement_end   %||% list(any_of = "closing date", value = "date"), pages, dict)
    if (!is.na(ps$value) && !is.na(pe$value)) {
      period_start <- ps$value; period_end <- pe$value
      if (!length(periods)) periods <- sprintf("%s to %s", ps$value, pe$value)
    }
  }

  accounts <- unique(c(.all_matches(text, lex("account_regex")),
                       .all_matches(text, lex("card_regex"))))

  # A single statement prints each header wording once; a concatenated bundle repeats the whole
  # block. Counted from the SAME dictionary synonyms match_label uses.
  .count_occ <- function(phrases) {
    ph <- unique(tolower(unlist(phrases))); ph <- ph[nzchar(ph)]
    if (!length(ph)) return(0L)
    lc <- tolower(text)
    total <- 0L
    for (p in ph) { m <- gregexpr(p, lc, fixed = TRUE)[[1]]; if (m[1] > 0) total <- total + length(m) }
    total
  }
  n_opening_labels <- .count_occ(dict$opening_balance$any_of %||% "opening balance")
  n_closing_labels <- .count_occ(dict$closing_balance$any_of %||% "closing balance")

  # The labelled STATEMENT DATE is a third, deterministic source of the YEAR for a table printing
  # day+month only: otherwise the reader has the printed period, or a text scan that works only when
  # the page carries one distinct 4-digit year - and a credit-card statement prints several.
  sd <- match_label(dict$statement_date %||% list(any_of = "statement date", value = "date"),
                    pages, dict)
  ob <- match_label(ospec, pages, dict)
  cb <- match_label(cspec, pages, dict)
  opening_balance <- ob$value; closing_balance <- cb$value
  if (!is.null(span) && !is.null(span$opening)) {
    opening_balance <- span$opening; closing_balance <- span$closing
  }

  # When a statement prints a transaction count it becomes an INDEPENDENT completeness check. Only
  # very specific labels are used so a stray word never invents a count.
  sc <- match_label(dict$transaction_count %||% list(
          any_of = c("number of transactions", "no. of transactions",
                     "no of transactions", "total number of transactions",
                     "transaction count"),
          value = "regex:[0-9]{1,6}"), pages, dict)
  stated_count <- suppressWarnings(as.integer(sc$value))
  if (!is.na(stated_count) && (stated_count < 1L || stated_count > PARAM_STATED_COUNT_MAX))
    stated_count <- NA_integer_

  list(
    pages_actual   = pages_actual,
    max_page_pt    = max_page_pt,
    pages_stated   = pages_stated,
    page1_markers  = page1_markers,
    period_start   = period_start,
    period_end     = period_end,
    # DISTINCT periods, not distinct SPELLINGS: `periods` is unique on the matched string, so a
    # statement printing its period twice in two styles counted as two. .period_span dedupes by date.
    n_periods      = span$n %||% length(periods),
    period_note    = period_note,
    # TRUE only when the several printed periods were actually merged - FALSE for a BUNDLE and for a
    # set the merge refused to pair.
    period_merged  = isTRUE(span$merged),
    statement_date = sd$value,
    accounts       = accounts,
    n_accounts     = length(accounts),
    opening_balance = opening_balance,
    closing_balance = closing_balance,
    n_opening_labels = n_opening_labels,
    n_closing_labels = n_closing_labels,
    stated_count    = stated_count
  )
}

# Flags a bundle of more than one statement in one upload. The reliable STRONG signal is more than one
# distinct statement PERIOD; distinct ACCOUNT NUMBERS are supporting context only, because real
# statements name other accounts in transfer narratives (one ANZ statement showed 5).
detect_multiple_statements <- function(input, meta = NULL) {
  if (is.null(meta)) meta <- extract_metadata(input)
  reasons <- character(0); strong <- FALSE

  # The span is named here only when one was MADE: period_start/period_end always hold something -
  # on a bundle simply the FIRST period's dates - so claiming them as a span misled the reader.
  if (isTRUE(meta$n_periods > 1)) {
    ps <- meta$period_start %||% NA_character_; pe <- meta$period_end %||% NA_character_
    say_span <- isTRUE(meta$period_merged) && !is.na(ps) && !is.na(pe)
    reasons <- c(reasons, sprintf("%d distinct statement periods found%s", meta$n_periods,
      if (say_span) sprintf(", read as one span %s to %s", ps, pe) else ""))
    strong <- TRUE
  }
  # Anything .period_span needed to say about that merge - the one thing about a multi-period file
  # the reviewer cannot work out for herself.
  if (!is.na(meta$period_note %||% NA_character_))
    reasons <- c(reasons, meta$period_note)
  if (isTRUE(meta$page1_markers > 1)) {
    reasons <- c(reasons, sprintf("%d 'Page 1 of N' markers (each statement restarts page numbering)",
                                  meta$page1_markers))
    strong <- TRUE
  }
  if (isTRUE(meta$n_opening_labels > 1) && isTRUE(meta$n_closing_labels > 1)) {
    reasons <- c(reasons, sprintf("the opening/closing-balance block appears %d times",
      min(meta$n_opening_labels, meta$n_closing_labels)))
    strong <- TRUE
  }
  if (isTRUE(meta$n_accounts > 1))
    reasons <- c(reasons, sprintf("%d account numbers seen (transfers/products may inflate this)", meta$n_accounts))

  list(likely_multiple = strong,
       combined_accounts = isTRUE(meta$n_accounts > 1) && !strong,
       n_accounts = meta$n_accounts %||% 0L,
       reasons = reasons)
}

metadata_df <- function(meta) {
  flat <- meta
  flat$accounts <- paste(meta$accounts, collapse = "; ")
  data.frame(field = names(flat),
             value = vapply(flat, function(v)
               if (is.null(v) || length(v) == 0) NA_character_ else paste(as.character(v), collapse = "; "),
               character(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}
