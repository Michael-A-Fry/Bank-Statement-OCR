# extract_metadata.R -- GENERIC statement metadata + multi-statement detection.
# Pattern-based only: NO bank-specific logic and NO per-sample hardcoding. Works
# on any English statement's page text; returns NA for anything not present.
# Feeds the Metadata output sheet, the run log (to understand what people convert
# and what errors), and diagnostics.

# .MONEY_RX / .DATE_RX are defined in labels.R (single source of truth).
.ACCT_RX  <- "[0-9]{2}-[0-9]{4}-[0-9]{6,7}-[0-9]{2,3}"          # NZ bank account
.CARD_RX  <- "[0-9]{4}[- ]?[0-9X*]{4}[- ]?[0-9X*]{4}[- ]?[0-9]{4}" # masked card
# A statement restarts page numbering, so a "Page 1 of N" is a statement START.
# Shared with split.R so its boundaries and the count that gates them agree exactly.
.PAGE1_MARKER_RX <- "[Pp]age\\s+1\\s+of\\s+[0-9]+"

.all_matches <- function(text, rx, perl = FALSE)
  unique(regmatches(text, gregexpr(rx, text, perl = perl))[[1]])

# ---- ONE SPAN over several printed periods ----------------------------------
# One uploaded file often covers SEVERAL printed periods: three monthly sections
# for one account, or a statement reissued to cover Jan-Mar. Reading only the
# FIRST period made two checks fail on a file that was completely fine -- rows
# from the other periods fell "outside" the period, and the reconciliation held
# ALL the transactions while comparing the first period's opening against the
# first period's closing. A false alarm is not harmless: it is how people learn
# to ignore the one warning that matters.
#
# So such a file is read as ONE ACCOUNT OVER ONE LONG SPAN:
#   period_start    = the EARLIEST period start printed anywhere
#   period_end      = the LATEST period end
#   opening_balance = the opening of the earliest period
#   closing_balance = the closing of the latest period
# and the proof becomes: first opening + every transaction = last closing.
#
# The DANGER is the pairing. A wrong opening/closing pair reconciles to a
# plausible, wrong figure -- worse than the false alarm it replaces. So the
# balances move only when the file's structure PROVES the pairing, and otherwise
# they are left exactly where a single-period read would leave them, with the
# reason said out loud. Nothing here is bank-specific.

# .period_bounds(periods, date_rx) -- every printed period range read as two
# dates, in document order; NULL if ANY of them cannot be. All-or-nothing on
# purpose: one unreadable range means the ORDER of the whole set is unknown, and
# an unknown order is exactly when a guess produces a wrong pairing.
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

# .period_span(pages, periods, date_rx, ospec, cspec, dict) -> list(
#   period_start, period_end,   -- the merged span, or NA to keep today's values
#   opening, closing,           -- the paired balances, or NULL to keep today's
#   note)                       -- one plain sentence, or NA when there is nothing
#                                  to say. It reaches the screen via
#                                  detect_multiple_statements().
# Only called when more than one period was printed.
.period_span <- function(pages, periods, date_rx, ospec, cspec, dict) {
  quiet <- list(period_start = NA_character_, period_end = NA_character_,
                opening = NULL, closing = NULL, note = NA_character_)

  pb <- .period_bounds(periods, date_rx)
  if (is.null(pb))
    return(modifyList(quiet, list(note = sprintf(paste0(
      "%d statement periods are printed but at least one could not be read as two dates, so ",
      "they could not be put in date order. This file is read as its FIRST printed period ",
      "only, so the period and balance checks may fail on it."), length(periods)))))

  # The same period written two ways ("1 Jan 2026 to 31 Jan 2026" and "... - ...")
  # is ONE period, not two -- dedupe by the dates before deciding anything.
  pb <- pb[!duplicated(paste(pb$start, pb$end)), , drop = FALSE]
  # Nothing to merge -- but the DEDUPED count still has to be reported, or the
  # screen goes on announcing "2 periods" for the one period it just collapsed.
  if (nrow(pb) < 2) return(modifyList(quiet, list(n = nrow(pb))))

  pb <- pb[order(pb$start, pb$end), , drop = FALSE]
  span_start <- pb$start_raw[1]; span_end <- pb$end_raw[nrow(pb)]

  # NOTHING MOVES UNTIL THE WHOLE THING IS PROVEN -- not the balances, and not the
  # dates either. The first version widened the DATES on the theory that "the
  # earliest start and the latest end are both facts printed on the page", and that
  # reasoning is wrong: what is printed is a DATE RANGE, and a statement prints
  # plenty of those that are not statement periods. A fixed-rate loan window, an
  # RWT tax year, a term-deposit maturity - any one of them made a perfectly
  # ordinary single-period statement report a span it never printed.
  #
  # That was not cosmetic. The reader takes its YEAR CONTEXT from period_start /
  # period_end (R/parse_pdf_table.R), so a January statement that also mentioned a
  # 2024-2026 loan term resolved every year-less "15 Jan" to 2024 - two years out,
  # on every row - and dates_within_period then read the SAME widened period and
  # PASSED. A wrong figure that looks right, which is the one thing the charter
  # forbids. The pre-change code got it right, so it was a regression too.
  #
  # So there are exactly two outcomes now: proven, and everything moves together;
  # or not proven, and nothing moves and the reason is said out loud. One gate.
  unproven <- function(why) list(
    period_start = NA_character_, period_end = NA_character_, n = nrow(pb),
    opening = NULL, closing = NULL,
    note = sprintf(paste0("%d date ranges are printed on this file, but %s, so it is read as ",
                          "its FIRST printed period only. If this really is one account over ",
                          "%s to %s, the period and balance checks may fail on it."),
                   nrow(pb), why, span_start, span_end))

  # Contiguity. Consecutive periods of one account meet end-to-start. A GAP means
  # the single span would silently cover days no printed period covers, so a
  # missing transaction in that window could never be noticed -- say it loudly. An
  # OVERLAP means these are not one account's consecutive sections at all.
  #
  # A SHARED BOUNDARY DAY IS NOT AN OVERLAP. Banks routinely print the previous
  # closing date as the next opening date ("31 Dec to 31 Jan", then "31 Jan to
  # 28 Feb"). Treating that as an overlap rejected the commonest real multi-period
  # statement there is -- the exact false alarm this feature exists to remove.
  gaps <- character(0); overlap <- FALSE
  for (k in seq_len(nrow(pb))[-1]) {
    if (pb$start[k] < pb$end[k - 1L]) overlap <- TRUE
    else if (pb$start[k] > pb$end[k - 1L] + 1)
      gaps <- c(gaps, sprintf("%s to %s", format(pb$end[k - 1L] + 1), format(pb$start[k] - 1)))
  }
  if (overlap) return(unproven("they overlap, so they are not one account's consecutive sections"))

  # Sections: each period's line down to the line before the next period's. The
  # balances are then read PER SECTION with the very same matcher used on the whole
  # document, so no wording is understood differently here than anywhere else.
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

  # ONE ACCOUNT. The chain proof below is strong, but it can be satisfied by
  # COINCIDENCE, and the commonest coincidence there is: two dormant or closed
  # accounts that both open and close on 0.00. That chains perfectly, so the
  # balances would be paired across two different accounts and -- worse -- the
  # design would believe the pairing proven and say nothing at all.
  # So: if the sections name accounts and they are not the same account, stop.
  # Sections that name none are not evidence either way and do not block it.
  accts <- lapply(sec, function(s) unique(.all_matches(paste(s, collapse = " "), .ACCT_RX)))
  named <- unique(unlist(accts[lengths(accts) > 0]))
  if (length(named) > 1L)
    return(unproven("the sections name more than one account, so they are not one account's sections"))

  # THE PAIRING EVIDENCE: every period's section prints exactly ONE opening and
  # exactly ONE closing. That is what makes "the earliest period's opening" and
  # "the latest period's closing" facts rather than guesses. Anything else (a
  # section with none, or with two) and the balances stay where they were --
  # which, when the file prints a single opening and a single closing overall, is
  # already the right answer.
  ob <- lapply(sec, function(s) match_label(ospec, s, dict))
  cb <- lapply(sec, function(s) match_label(cspec, s, dict))
  exactly_one <- function(r) isTRUE(r$n == 1L) && !is.na(r$value)
  if (!all(vapply(ob, exactly_one, logical(1))) || !all(vapply(cb, exactly_one, logical(1))))
    return(unproven("not every period prints exactly one opening and one closing balance"))

  # SAME ACCOUNT, PROVEN BY THE MONEY: each period must CLOSE on the figure the
  # next period OPENS on. That is the accounting definition of consecutive
  # sections of one ledger, and it is a far better proof than matching account
  # numbers -- a real statement names other accounts in its transfer narratives,
  # so the numbers on a page are not a reliable identity (the same reason
  # detect_multiple_statements() treats an account count as supporting only).
  # Two different accounts would have to print the identical figure at every
  # boundary to slip through. Anything that does not chain is not a sequence, and
  # the balances stay put.
  opens  <- .num(vapply(ob, function(r) r$value, character(1)))
  closes <- .num(vapply(cb, function(r) r$value, character(1)))
  links  <- abs(closes[-length(closes)] - opens[-1]) < PARAM_MONEY_TOL
  if (anyNA(links) || !all(links))
    return(unproven(paste0("one period does not close on the figure the next opens on, so they ",
                           "are not one account's consecutive sections")))

  # After the sort these are 1 and nrow(pb) by construction -- the earliest
  # period's opening, the latest period's closing.
  list(period_start = span_start, period_end = span_end, n = nrow(pb), merged = TRUE,
       opening = ob[[1]]$value, closing = cb[[nrow(pb)]]$value,
       note = if (!length(gaps)) NA_character_ else sprintf(paste0(
         "%d statement periods are read as one span %s to %s, but they do NOT join up: no ",
         "printed period covers %s. Transactions in %s are not part of this file - check it ",
         "is complete before relying on the totals."),
         nrow(pb), span_start, span_end, paste(gaps, collapse = "; "),
         if (length(gaps) > 1) "those windows" else "that window"))
}

# extract_metadata(input, dict) -> named list of statement-level metadata.
# Labelled scalars (opening/closing balance) come from the label dictionary --
# synonyms live in dictionaries/labels.yaml, NOT hardcoded here.
extract_metadata <- function(input, dict = default_label_dict()) {
  pages <- input$pages %||% character(0)
  text <- paste(pages, collapse = "\n")

  pages_actual <- input$meta$page_count %||% (if (length(pages)) length(pages) else NA_integer_)

  # largest page dimension in points (Hubdoc-style pre-flight: <= 2880 pt / 40 in)
  max_page_pt <- NA_real_
  if (identical(input$kind, "pdf") && requireNamespace("pdftools", quietly = TRUE) &&
      !is.null(input$path) && file.exists(input$path)) {
    sz <- tryCatch(pdftools::pdf_pagesize(input$path), error = function(e) NULL)
    if (!is.null(sz)) max_page_pt <- suppressWarnings(max(c(sz$width, sz$height), na.rm = TRUE))
  }

  # "Page X of Y" -> the largest Y is the stated length; count of "Page 1 of N".
  pageofs <- .all_matches(text, "[Pp]age\\s+[0-9]+\\s+of\\s+[0-9]+")
  pages_stated <- if (length(pageofs))
    suppressWarnings(max(as.integer(sub(".*of\\s+([0-9]+).*", "\\1", pageofs)), na.rm = TRUE)) else NA_integer_
  page1_markers <- length(regmatches(text, gregexpr(.PAGE1_MARKER_RX, text))[[1]])

  # statement period(s): two dates joined by a connective (to / through / dash),
  # regardless of wording around them ("From X to Y", "Statement period X - Y",
  # "X through Y"). Generic: no bank-specific phrase. Distinct ranges are counted.
  # connective between the two dates: a word or a dash. The dash class is built
  # from code points (ASCII hyphen, en-dash, em-dash) so the pattern is valid
  # UTF-8 regardless of the source file's locale -- no raw non-ASCII literal.
  dash <- paste0("-", intToUtf8(0x2013), intToUtf8(0x2014))
  # date shape + the "to / through / ..." connectives come from the lexicon.
  date_rx <- lex("date_regex"); conn <- paste(lex("period_connectives"), collapse = "|")
  per_rx <- sprintf("(?:%s)\\s*(?:%s|[%s])\\s*(?:%s)", date_rx, conn, dash, date_rx)
  periods <- .all_matches(enc2utf8(text), per_rx, perl = TRUE)
  period_start <- NA_character_; period_end <- NA_character_
  if (length(periods)) {
    ds <- regmatches(periods[1], gregexpr(date_rx, periods[1]))[[1]]
    if (length(ds) >= 2) { period_start <- ds[1]; period_end <- ds[2] }
  }
  # The opening/closing-balance matchers, resolved ONCE: the whole-document match
  # further down and the per-period match inside .period_span must ask exactly the
  # same question, or the two could disagree about what an opening balance is.
  ospec <- dict$opening_balance %||% list(any_of = "opening balance", value = "money")
  cspec <- dict$closing_balance %||% list(any_of = "closing balance", value = "money")
  # SEVERAL printed periods -> read the file as one long span (see .period_span).
  # One period is left completely untouched: the overwhelmingly common case must
  # come out byte-identical to before.
  #
  # ...but ONE STATEMENT WITH SEVERAL SECTIONS IS NOT A BUNDLE OF STATEMENTS, and
  # only the first is merged. A statement restarts its page numbering, so more than
  # one "Page 1 of N" means several separately-issued statement DOCUMENTS were
  # concatenated -- and the engine has a better answer for those than one long
  # span: split them and reconcile each on its own anchors (R/split.R), or flag and
  # refuse. Merging them would throw the per-statement anchors away and hand a
  # bundle a clean bill of health on the merged path. Same marker split.R cuts on,
  # so the two cannot disagree about what a statement is.
  span <- if (length(periods) > 1 && page1_markers <= 1)
    .period_span(pages, periods, date_rx, ospec, cspec, dict) else NULL
  period_note <- span$note %||% NA_character_
  if (!is.null(span) && !is.na(span$period_start)) {
    period_start <- span$period_start; period_end <- span$period_end
  }
  # Fallback: period given as two LABELLED dates (Westpac/ASB "Opening date" /
  # "Closing date"), not an inline range. Fills the period so year-less
  # transaction dates ("15 Jun") can still be resolved.
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

  # How many times the opening / closing-balance HEADER wording appears. A single
  # statement prints each once; a concatenated bundle repeats the whole block.
  # Counted from the SAME dictionary synonyms match_label uses, so the wording
  # stays configurable and the reader/detector never disagree.
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

  # The labelled STATEMENT DATE ("Statement date: 12 October 2026", "Date of
  # issue"). It has been in dictionaries/labels.yaml since the beginning and was
  # read by NOTHING -- a documented label that did nothing.
  #
  # It matters because it is a third, deterministic source of the YEAR for a
  # statement whose table prints day+month only ("October 12"). The reader had
  # exactly two: the printed period, or -- only when the whole page carries ONE
  # distinct 4-digit year -- a text scan. A credit-card statement routinely prints
  # several (payment due date, card expiry, a copyright line), so the scan finds
  # nothing usable and EVERY date comes back blank on a statement that prints its
  # own date at the top. Reading a labelled fact is not a guess, so it belongs
  # ahead of counting digits in the page text.
  sd <- match_label(dict$statement_date %||% list(any_of = "statement date", value = "date"),
                    pages, dict)
  # opening/closing balance via the label dictionary (synonyms, not hardcoded).
  ob <- match_label(ospec, pages, dict)
  cb <- match_label(cspec, pages, dict)
  opening_balance <- ob$value; closing_balance <- cb$value
  # ...replaced by the EARLIEST period's opening and the LATEST period's closing
  # when, and only when, .period_span could prove that pairing.
  if (!is.null(span) && !is.null(span$opening)) {
    opening_balance <- span$opening; closing_balance <- span$closing
  }

  # Stated transaction count: many statements print "Number of transactions: 42".
  # When present it becomes an INDEPENDENT completeness check (reconcile compares
  # it to the parsed row count). Only very specific labels are used so a stray
  # word never invents a count; absent -> NA -> the check simply doesn't run.
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
    # DISTINCT periods, not distinct SPELLINGS of them. `periods` is unique on the
    # matched string, so a statement that prints its period twice in two styles
    # ("1 Jan 2026 to 31 Jan 2026" and "1 Jan 2026 - 31 Jan 2026") counted as two
    # and the screen announced "2 periods" on an ordinary single-period statement.
    # .period_span already dedupes by the parsed DATES to decide anything at all;
    # its count is the honest one, so it is the one reported.
    n_periods      = span$n %||% length(periods),
    # NA unless a multi-period file needed something said about it (a hole between
    # the periods, or balances that could not be paired). Reaches the screen via
    # detect_multiple_statements(), and the Metadata sheet via metadata_df().
    period_note    = period_note,
    # TRUE only when the several printed periods were actually merged into one
    # span. It is FALSE for a BUNDLE (split.R handles those, and the merge is
    # deliberately skipped) and for a set the merge refused to pair -- and the
    # screen may only claim a span when this says one was made.
    period_merged  = isTRUE(span$merged),
    # The date the statement says it was issued. NA when it prints none. Used as a
    # year source for day+month-only tables (see the note above match_label).
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

# detect_multiple_statements(input, meta) -- flags a bundle of >1 statement in a
# single upload (which would corrupt a single parse).
#
# The reliable STRONG signal is more than one distinct statement PERIOD: two
# different date ranges in one file means two statements. (Confirmed on real
# data: a 46-page bundle shows 6 distinct periods.)
#
# A count of distinct ACCOUNT NUMBERS is NOT reliable and is deliberately only
# supporting context: real statements name other accounts in transaction
# narratives (transfers) and list several products of one account, so a normal
# single statement routinely shows several account numbers. (Confirmed on real
# data: a single ANZ statement showed 5 account numbers yet had one continuous
# running balance.) Multiple accounts within ONE period => a combined statement,
# flagged separately, not a bundle.
detect_multiple_statements <- function(input, meta = NULL) {
  if (is.null(meta)) meta <- extract_metadata(input)
  reasons <- character(0); strong <- FALSE

  # STRONG 1: more than one distinct statement PERIOD (inline date ranges).
  # The span is named here too when one was MADE, because that is the difference
  # between "3 periods, 1 Jan to 31 Mar" and pretending there was one.
  #
  # ...and ONLY when one was made. period_start/period_end always hold something -
  # on a bundle they are simply the FIRST period's dates - so claiming them as a
  # span told a reader that three statements had been read end to end when they
  # had in fact been SPLIT and reconciled separately. A sentence describing a
  # merge that did not happen is worse than no sentence: it is the tool
  # misreporting how its own figures were produced.
  if (isTRUE(meta$n_periods > 1)) {
    ps <- meta$period_start %||% NA_character_; pe <- meta$period_end %||% NA_character_
    say_span <- isTRUE(meta$period_merged) && !is.na(ps) && !is.na(pe)
    reasons <- c(reasons, sprintf("%d distinct statement periods found%s", meta$n_periods,
      if (say_span) sprintf(", read as one span %s to %s", ps, pe) else ""))
    strong <- TRUE
  }
  # Anything .period_span needed to say about that merge -- a gap between the
  # periods, or a pairing it refused to make. This is the one thing about a
  # multi-period file the reviewer cannot work out for herself, so it goes on the
  # same screen as the flag, never into silence.
  if (!is.na(meta$period_note %||% NA_character_))
    reasons <- c(reasons, meta$period_note)
  # STRONG 2: more than one "Page 1 of N" marker. Each concatenated statement
  # restarts its own page numbering, so >1 first page means >1 statement -- and
  # this catches bundles the period signal misses (labelled/year-less/non-inline
  # periods that collapse to one range). Deterministic and independent.
  if (isTRUE(meta$page1_markers > 1)) {
    reasons <- c(reasons, sprintf("%d 'Page 1 of N' markers (each statement restarts page numbering)",
                                  meta$page1_markers))
    strong <- TRUE
  }
  # STRONG 3: the whole opening-AND-closing-balance header block repeats. A single
  # statement prints each once; requiring BOTH to repeat avoids a stray mention in
  # a summary line falsely flagging a normal statement.
  if (isTRUE(meta$n_opening_labels > 1) && isTRUE(meta$n_closing_labels > 1)) {
    reasons <- c(reasons, sprintf("the opening/closing-balance block appears %d times",
      min(meta$n_opening_labels, meta$n_closing_labels)))
    strong <- TRUE
  }
  # SUPPORTING only: multiple account numbers (transfers/products routinely inflate
  # this on a normal single statement, so it never flags a bundle on its own).
  if (isTRUE(meta$n_accounts > 1))
    reasons <- c(reasons, sprintf("%d account numbers seen (transfers/products may inflate this)", meta$n_accounts))

  list(likely_multiple = strong,
       combined_accounts = isTRUE(meta$n_accounts > 1) && !strong,
       n_accounts = meta$n_accounts %||% 0L,
       reasons = reasons)
}

# metadata_df(meta) -- flatten metadata to a two-column field/value frame for the
# Metadata output sheet.
metadata_df <- function(meta) {
  flat <- meta
  flat$accounts <- paste(meta$accounts, collapse = "; ")
  data.frame(field = names(flat),
             value = vapply(flat, function(v)
               if (is.null(v) || length(v) == 0) NA_character_ else paste(as.character(v), collapse = "; "),
               character(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}
