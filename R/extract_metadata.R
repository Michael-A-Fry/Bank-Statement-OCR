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

  # opening/closing balance via the label dictionary (synonyms, not hardcoded).
  ob <- match_label(dict$opening_balance %||% list(any_of = "opening balance", value = "money"),
                    pages, dict)
  cb <- match_label(dict$closing_balance %||% list(any_of = "closing balance", value = "money"),
                    pages, dict)

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
  if (!is.na(stated_count) && (stated_count < 1L || stated_count > 100000L))
    stated_count <- NA_integer_

  list(
    pages_actual   = pages_actual,
    max_page_pt    = max_page_pt,
    pages_stated   = pages_stated,
    page1_markers  = page1_markers,
    period_start   = period_start,
    period_end     = period_end,
    n_periods      = length(periods),
    accounts       = accounts,
    n_accounts     = length(accounts),
    opening_balance = ob$value,
    closing_balance = cb$value,
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
  if (isTRUE(meta$n_periods > 1)) {
    reasons <- c(reasons, sprintf("%d distinct statement periods found", meta$n_periods))
    strong <- TRUE
  }
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
