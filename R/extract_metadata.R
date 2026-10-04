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

# .month_period(text) -- a statement period printed as ONE calendar month, tied to
# the word "statement" ("Statement for December 2025", "Statement period: Dec
# 2025", "December 2025 statement"), as c(first day, last day) in "%d %B %Y"; NULL
# when none is printed or when two different months are (a bundle, or a letter
# naming another statement): then nothing is assumed. A month and year printed
# anywhere else ("Copyright 2025", "Rates from March 2026") is not a period.
.month_period <- function(text) {
  mon <- paste0("(jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|june?|july?|aug(?:ust)?|",
                "sep(?:t(?:ember)?)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?)")
  yr <- "((?:19|20)[0-9]{2})"
  # Spaces and tabs only, never a line break: "matures on 15 January 2027" ending
  # one line and "Statement period ..." starting the next is not "January 2027
  # statement". A pattern that reaches across lines joins two facts into one.
  sp <- "[ \\t]"
  rx <- c(paste0("(?i)\\bstatement", sp, "+(?:for|of)", sp, "+(?:the", sp, "+month", sp, "+of", sp, "+)?", mon, ",?", sp, "+", yr, "\\b"),
          paste0("(?i)\\bstatement", sp, "+(?:period|month)", sp, "*:?", sp, "*", mon, ",?", sp, "+", yr, "\\b"),
          paste0("(?i)\\b", mon, ",?", sp, "+", yr, sp, "+statement\\b"))
  hits <- character(0)
  for (r in rx) for (h in regmatches(text, gregexpr(r, text, perl = TRUE))[[1]]) {
    g <- regmatches(h, regexec(r, h, perl = TRUE))[[1]]
    if (length(g) == 3L) hits <- c(hits, paste(substr(tolower(g[2]), 1L, 3L), g[3]))
  }
  hits <- unique(hits)
  if (length(hits) != 1L) return(NULL)
  start <- suppressWarnings(as.Date(paste("1", hits), "%d %b %Y"))
  if (is.na(start) || !.plausible_year(format(start, "%Y"))) return(NULL)
  end <- seq(start, by = "month", length.out = 2L)[2] - 1
  c(format(start, "%d %B %Y"), format(end, "%d %B %Y"))
}

# .period_labels(dict) -- the wordings a statement uses to label its own period
# ("Statement period", "Period"), from the label dictionary (statement_period),
# lower case. A range is the statement's period when one of these, alone, is the
# words in front of it.
.period_labels <- function(dict = NULL) {
  l <- as.character(unlist(dict$statement_period$any_of %||%
                             c("statement period", "period", "statement dates", "statement for the period",
                               "statement from", "period covered", "statement covers")))
  unique(tolower(trimws(l[!is.na(l) & nzchar(l)])))
}

# .period_ranges(pages, date_rx, conn, labels) -> list(labelled, unlabelled): the
# date ranges printed on the pages, each on ONE line. A range is LABELLED when the
# cell in front of it on its line (the words since the last wide gap or colon) is
# one of the statement's period labels; then the two dates may be joined by any
# short punctuation as well as a connective ("16 December 2025 ... 15 January
# 2026"), since the label already says what they are. Anything else is a bare
# range: two dates joined by a connective or a hyphen ("1 Apr 2024 to 31 Mar 2025"
# in an RWT notice is one). `pages` has its dashes made hyphens already.
.period_ranges <- function(pages, date_rx, conn, labels) {
  lines <- unlist(strsplit(paste(pages %||% character(0), collapse = "\n"), "\n", fixed = TRUE))
  bare_rx <- sprintf("(?:%s)[ \\t]*(?:(?:%s)\\b|-)[ \\t]*(?:%s)", date_rx, conn, date_rx)
  lab_rx <- sprintf("(?:%s)[ \\t]*(?:(?:%s)\\b|[^A-Za-z0-9\\t\\n]{1,5})[ \\t]*(?:%s)", date_rx, conn, date_rx)
  labelled <- character(0); bare <- character(0)
  for (ln in lines) {
    if (!grepl("[0-9]", ln)) next
    m <- gregexpr(lab_rx, ln, perl = TRUE)[[1]]
    if (m[1] > 0) for (k in seq_along(m)) {
      pre <- sub("[[:blank:]:]+$", "", substr(ln, 1L, m[k] - 1L))
      cell <- utils::tail(strsplit(pre, "[[:blank:]]{2,}|:")[[1]], 1L)
      cell <- tolower(gsub("[[:blank:]]+", " ", trimws(cell %||% "")))
      if (length(cell) && cell %in% labels)
        labelled <- c(labelled, substr(ln, m[k], m[k] + attr(m, "match.length")[k] - 1L))
    }
    bare <- c(bare, regmatches(ln, gregexpr(bare_rx, ln, perl = TRUE))[[1]])
  }
  list(labelled = unique(labelled), unlabelled = unique(bare))
}

# .ranges_join(ranges, date_rx) -- do the printed ranges join into one run, each
# starting on or the day after the one before ends (or are they all one range)?
# Ranges that do not read count as not joining.
.ranges_join <- function(ranges, date_rx) {
  b <- lapply(ranges, function(r) {
    ds <- regmatches(r, gregexpr(date_rx, r))[[1]]
    if (length(ds) < 2) return(NULL)
    v <- c(.plausible_period_date(ds[1]), .plausible_period_date(ds[2]))
    if (anyNA(v) || v[1] > v[2]) NULL else v
  })
  if (any(vapply(b, is.null, logical(1)))) return(FALSE)
  key <- unique(vapply(b, function(v) paste(v, collapse = "|"), ""))
  if (length(key) <= 1L) return(TRUE)
  st <- as.Date(vapply(strsplit(key, "|", fixed = TRUE), `[`, "", 1)); en <- as.Date(vapply(strsplit(key, "|", fixed = TRUE), `[`, "", 2))
  o <- order(st, en); st <- st[o]; en <- en[o]
  all(st[-1] >= en[-length(en)] & st[-1] <= en[-length(en)] + 1)
}

# .own_label(spec) -- a label spec that takes only the statement's OWN label: one
# that starts its phrase on the line (match_label's `own`), so "Account opening
# date", "Loan start date", "terms and conditions issued" or "card issued" -- a
# label naming something else's date -- is never read as the statement's.
.own_label <- function(spec) { spec$own <- TRUE; spec }

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

  # statement period(s): two dates joined by a connective (to / through / any
  # dash), on ONE line. Every dash a PDF may print (en dash, em dash, a minus) is
  # made a plain hyphen first, so "16 December 2025 - 15 January 2026" reads
  # whichever dash the bank typeset; matching dash code points inside a pattern
  # failed in a C locale, and the year then came from anywhere else on the page.
  # date shape + the "to / through / ..." connectives come from the lexicon.
  date_rx <- lex("date_regex"); conn <- paste(lex("period_connectives"), collapse = "|")
  ppages <- .ascii_dashes(enc2utf8(as.character(pages)))
  pr <- .period_ranges(ppages, date_rx, conn, .period_labels(dict))
  # THE LABELLED PERIOD WINS. A statement prints other date ranges -- an RWT
  # certificate's tax year, a loan's fixed-rate term, a bonus-interest window --
  # and the first range on the page used to become the period, so every
  # year-less row took its year from a notice. A range the statement labels as
  # its period ("Statement period ...") is the period; the others are not read as
  # periods at all. Only when nothing is labelled are the unlabelled ranges used,
  # after the labelled dates and a month named as the statement's (below).
  periods <- pr$labelled
  period_source <- if (length(periods)) "labelled" else NA_character_
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
  # Fallback: period given as two LABELLED dates (Westpac/ASB "Opening date" /
  # "Closing date"), not an inline range. Fills the period so year-less
  # transaction dates ("15 Jun") can still be resolved. Only the statement's OWN
  # labels count (`own`): "Account opening date 12 Mar 2019" or "Loan start date"
  # names another thing's date and once stretched the period back seven years.
  if (!length(periods)) {
    ps <- match_label(.own_label(dict$statement_start %||% list(any_of = "opening date", value = "date")), pages, dict)
    pe <- match_label(.own_label(dict$statement_end   %||% list(any_of = "closing date", value = "date")), pages, dict)
    if (!is.na(ps$value) && !is.na(pe$value)) {
      period_start <- ps$value; period_end <- pe$value
      periods <- sprintf("%s to %s", ps$value, pe$value); period_source <- "labelled_dates"
    }
  }
  # Next: a period named as a calendar month ("Statement for December 2025",
  # "December 2025 statement"). It is the statement's own word for its period, so
  # it settles the year of a "03 Dec" row; the statement's issue date
  # ("Statement date 5 Jan 2026") does not, on its own (R/parse_pdf_table.R).
  if (!length(periods)) {
    mp <- .month_period(text)
    if (!is.null(mp)) {
      period_start <- mp[1]; period_end <- mp[2]
      periods <- sprintf("%s to %s", mp[1], mp[2]); period_source <- "month"
    }
  }
  # Last: a range nobody labelled. It may be the period, or a notice's; when the
  # page prints two or more that do not join end to start, which one is the
  # period is a guess, and `period_unsure` says so (the reader's year_settled).
  period_unsure <- FALSE
  if (!length(periods) && length(pr$unlabelled)) {
    periods <- pr$unlabelled; period_source <- "unlabelled"
    ds <- regmatches(periods[1], gregexpr(date_rx, periods[1]))[[1]]
    if (length(ds) >= 2) { period_start <- ds[1]; period_end <- ds[2] }
    period_unsure <- !.ranges_join(periods, date_rx)
  } else if (length(pr$unlabelled) && !identical(period_source, "labelled")) {
    # A labelled date pair or a named month, and also an unlabelled range that is
    # not that period: the two say different things, and nothing says which wins.
    p0 <- .plausible_period_date(period_start); p1 <- .plausible_period_date(period_end)
    other <- Filter(function(r) {
      ds <- regmatches(r, gregexpr(date_rx, r))[[1]]
      length(ds) >= 2 && !(identical(.plausible_period_date(ds[1]), p0) && identical(.plausible_period_date(ds[2]), p1))
    }, pr$unlabelled)
    period_unsure <- length(other) > 0L
  }
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
  span <- if (length(periods) > 1 && page1_markers <= 1 && period_source %in% c("labelled", "unlabelled"))
    .period_span(ppages, periods, date_rx, ospec, cspec, dict) else NULL
  period_note <- span$note %||% NA_character_
  if (!is.null(span) && !is.na(span$period_start)) {
    period_start <- span$period_start; period_end <- span$period_end
  }
  # Ranges the balances prove are one account's consecutive periods are one span.
  if (isTRUE(span$merged)) period_unsure <- FALSE

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
  # ...but ONE statement can print its block twice or more: a home loan's "Loan
  # summary" box (opening balance, interest, repayments, closing balance, rate) and
  # the table's own first and last lines, or the box again on every page. Those
  # state the SAME two figures, where a bundle's next statement opens on the balance
  # the last one closed on. So the figure printed after each wording on its line is
  # read too: every opening wording stating one figure and every closing wording
  # another is one statement's block, however often it is printed. A wording with
  # no figure on its line (a box laid out in columns) leaves the count as it was.
  .label_figs <- function(phrases) {
    ph <- unique(tolower(unlist(phrases))); ph <- ph[nzchar(ph)]
    out <- character(0)
    for (ln in strsplit(tolower(text), "\n", fixed = TRUE)[[1]]) for (p in ph) {
      at <- gregexpr(p, ln, fixed = TRUE)[[1]]
      if (at[1] < 0) next
      for (k in at) {
        rest <- substring(ln, k + nchar(p))
        f <- regmatches(rest, regexpr("[0-9][0-9,]*[.][0-9]{2}", rest))
        out <- c(out, if (length(f)) gsub(",", "", f, fixed = TRUE) else NA_character_)
      }
    }
    out
  }
  n_balance_blocks <- min(n_opening_labels, n_closing_labels)
  if (n_balance_blocks > 1L) {
    of <- .label_figs(dict$opening_balance$any_of %||% "opening balance")
    cf <- .label_figs(dict$closing_balance$any_of %||% "closing balance")
    if (length(of) && length(cf) && !anyNA(of) && !anyNA(cf) &&
        length(unique(of)) == 1L && length(unique(cf)) == 1L) n_balance_blocks <- 1L
  }

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
  # Only the statement's OWN date label counts: "Our terms and conditions issued
  # 1 Jun 2025" or "Card issued 15 Apr 2025" is another thing's date, and it once
  # gave every year-less row its year (`own`, R/labels.R).
  sd <- match_label(.own_label(dict$statement_date %||% list(any_of = "statement date", value = "date")),
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
    # Where the period came from: "labelled" (a range the statement labels as its
    # period), "labelled_dates" (its own opening and closing date labels), "month"
    # (a month named as the statement's), "unlabelled" (a bare range), or NA.
    period_source  = period_source,
    # TRUE when the page prints date ranges that disagree and none is labelled as
    # the statement's: the period, and any year taken from it, is a guess.
    period_unsure  = isTRUE(period_unsure),
    # The period ranges themselves, as printed (the reader checks rows against
    # every one of them; a bundle prints one per statement).
    period_ranges  = as.character(periods),
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
    # How many statements' opening/closing blocks those wordings print: one when the
    # repeats all state the same two figures (see above).
    n_balance_blocks = n_balance_blocks,
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
  # a summary line falsely flagging a normal statement. And the repeats must state
  # different balances: a home loan's summary box beside its table repeats ONE
  # statement's opening and closing, and is no second statement (n_balance_blocks).
  if (isTRUE(meta$n_opening_labels > 1) && isTRUE(meta$n_closing_labels > 1)) {
    nb <- meta$n_balance_blocks %||% min(meta$n_opening_labels, meta$n_closing_labels)
    if (isTRUE(nb > 1)) {
      reasons <- c(reasons, sprintf("the opening/closing-balance block appears %d times", nb))
      strong <- TRUE
    }
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
