# extract_metadata.R -- GENERIC statement metadata + multi-statement detection.
# Pattern-based only: NO bank-specific logic and NO per-sample hardcoding. Works
# on any English statement's page text; returns NA for anything not present.
# Feeds the Metadata output sheet, the run log (to understand what people convert
# and what errors), and diagnostics.

.MONEY_RX <- "-?\\$?-?[0-9][0-9,]*\\.[0-9]{2}"
.DATE_RX  <- "[0-9]{1,2}[/ .-][A-Za-z0-9]{2,9}[/ .-][0-9]{2,4}"
.ACCT_RX  <- "[0-9]{2}-[0-9]{4}-[0-9]{6,7}-[0-9]{2,3}"          # NZ bank account
.CARD_RX  <- "[0-9]{4}[- ]?[0-9X*]{4}[- ]?[0-9X*]{4}[- ]?[0-9]{4}" # masked card

.all_matches <- function(text, rx, perl = FALSE)
  unique(regmatches(text, gregexpr(rx, text, perl = perl))[[1]])

# extract_metadata(input) -> named list of statement-level metadata.
extract_metadata <- function(input) {
  pages <- input$pages %||% character(0)
  text <- paste(pages, collapse = "\n")
  lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))

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

  # statement period(s): every "From <date> to <date>" block, distinct.
  per_rx <- sprintf("[Ff]rom\\s+%s\\s+to\\s+%s", .DATE_RX, .DATE_RX)
  periods <- .all_matches(text, per_rx, perl = TRUE)
  period_start <- NA_character_; period_end <- NA_character_
  if (length(periods)) {
    ds <- regmatches(periods[1], gregexpr(.DATE_RX, periods[1]))[[1]]
    if (length(ds) >= 2) { period_start <- ds[1]; period_end <- ds[2] }
  }

  accounts <- unique(c(.all_matches(text, .ACCT_RX), .all_matches(text, .CARD_RX)))

  grab_after <- function(label) {
    ln <- lines[grepl(label, lines, ignore.case = TRUE) & grepl(.MONEY_RX, lines)]
    if (!length(ln)) return(NA_character_)
    m <- regmatches(ln[1], gregexpr(.MONEY_RX, ln[1]))[[1]]
    if (length(m)) m[length(m)] else NA_character_
  }

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
    opening_balance = grab_after("opening balance"),
    closing_balance = grab_after("closing balance")
  )
}

# detect_multiple_statements(input, meta) -- flags a bundle of >1 statement in a
# single upload (which would corrupt a single parse). STRONG signals only:
# more than one distinct account number, or more than one distinct statement
# period. (Repeated "Page 1 of N" alone is noisy on guides/cover pages, so it is
# only a supporting reason, never the sole trigger.)
detect_multiple_statements <- function(input, meta = NULL) {
  if (is.null(meta)) meta <- extract_metadata(input)
  reasons <- character(0)
  if (isTRUE(meta$n_accounts > 1))
    reasons <- c(reasons, sprintf("%d distinct account numbers found", meta$n_accounts))
  if (isTRUE(meta$n_periods > 1))
    reasons <- c(reasons, sprintf("%d distinct statement periods found", meta$n_periods))
  strong <- length(reasons) > 0
  if (strong && isTRUE(meta$page1_markers > 1))
    reasons <- c(reasons, sprintf("%d 'Page 1 of N' markers", meta$page1_markers))
  list(likely_multiple = strong, reasons = reasons)
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
