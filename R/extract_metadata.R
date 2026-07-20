# extract_metadata.R -- GENERIC statement metadata + multi-statement detection.
# Pattern-based only: NO bank-specific logic and NO per-sample hardcoding. Works
# on any English statement's page text; returns NA for anything not present.
# Feeds the Metadata output sheet, the run log (to understand what people convert
# and what errors), and diagnostics.

# .MONEY_RX / .DATE_RX are defined in labels.R (single source of truth).
.ACCT_RX  <- "[0-9]{2}-[0-9]{4}-[0-9]{6,7}-[0-9]{2,3}"          # NZ bank account
.CARD_RX  <- "[0-9]{4}[- ]?[0-9X*]{4}[- ]?[0-9X*]{4}[- ]?[0-9]{4}" # masked card

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
  page1_markers <- length(regmatches(text, gregexpr("[Pp]age\\s+1\\s+of\\s+[0-9]+", text))[[1]])

  # statement period(s): two dates joined by a connective (to / through / dash),
  # regardless of wording around them ("From X to Y", "Statement period X - Y",
  # "X through Y"). Generic: no bank-specific phrase. Distinct ranges are counted.
  # connective between the two dates: a word or a dash. The dash class is built
  # from code points (ASCII hyphen, en-dash, em-dash) so the pattern is valid
  # UTF-8 regardless of the source file's locale -- no raw non-ASCII literal.
  dash <- paste0("-", intToUtf8(0x2013), intToUtf8(0x2014))
  per_rx <- sprintf("(?:%s)\\s*(?:to|through|thru|until|[%s])\\s*(?:%s)",
                    .DATE_RX, dash, .DATE_RX)
  periods <- .all_matches(enc2utf8(text), per_rx, perl = TRUE)
  period_start <- NA_character_; period_end <- NA_character_
  if (length(periods)) {
    ds <- regmatches(periods[1], gregexpr(.DATE_RX, periods[1]))[[1]]
    if (length(ds) >= 2) { period_start <- ds[1]; period_end <- ds[2] }
  }

  accounts <- unique(c(.all_matches(text, .ACCT_RX), .all_matches(text, .CARD_RX)))

  # opening/closing balance via the label dictionary (synonyms, not hardcoded).
  ob <- match_label(dict$opening_balance %||% list(any_of = "opening balance", value = "money"),
                    pages, dict)
  cb <- match_label(dict$closing_balance %||% list(any_of = "closing balance", value = "money"),
                    pages, dict)

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
    closing_balance = cb$value
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
  reasons <- character(0)
  strong <- isTRUE(meta$n_periods > 1)
  if (strong)
    reasons <- c(reasons, sprintf("%d distinct statement periods found", meta$n_periods))
  if (isTRUE(meta$n_accounts > 1))
    reasons <- c(reasons, sprintf("%d account numbers seen (transfers/products may inflate this)", meta$n_accounts))
  if (strong && isTRUE(meta$page1_markers > 1))
    reasons <- c(reasons, sprintf("%d 'Page 1 of N' markers", meta$page1_markers))
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
