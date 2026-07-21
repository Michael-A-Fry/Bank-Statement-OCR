# parse_pdf_table.R -- extract a transaction table from PDF word boxes using a
# declarative `format: pdf` template. Same forensic contract as the delimited
# path: verbatim descriptions, redactions honoured, deterministic, never crashes.
#
# Deliberately simple + generic (no per-statement code):
#   * words are grouped into visual ROWS by y-position (row_tol),
#   * each word is placed in a COLUMN by which x-band its centre falls in,
#   * a row is KEPT only if its date cell parses as a real date -- which cleanly
#     ignores headers, annotations, footers and section-header "gaps".
#   Multi-page tables are stitched by processing every page the same way.

# .pdf_cell(rw, cspec) -- text of the words whose centre falls in a column band,
# left-to-right, space-joined; NA when the band is empty or unmapped.
.pdf_cell <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_character_)
  cx <- rw$x + rw$width / 2
  sel <- rw[cx >= cspec$x_min & cx <= cspec$x_max, , drop = FALSE]
  if (!nrow(sel)) return(NA_character_)
  paste(sel$text[order(sel$x)], collapse = " ")
}

# .cell_minconf(rw, cspec) -- lowest OCR word confidence (0-100) among the words
# that fall in a column band; NA when there is no confidence data (a text-layer
# page has none) or the band is empty. Used to flag a low-confidence value in a
# CRITICAL cell (date/amount/balance) that a page-mean confidence would hide.
.cell_minconf <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_real_)
  if (!("conf" %in% names(rw))) return(NA_real_)
  cx <- rw$x + rw$width / 2
  sel <- rw[cx >= cspec$x_min & cx <= cspec$x_max, , drop = FALSE]
  cf <- suppressWarnings(as.numeric(sel$conf)); cf <- cf[!is.na(cf) & cf >= 0]
  if (!length(cf)) NA_real_ else min(cf)
}

# .has_money(x) -- TRUE when a cell carries any digit (used only to decide a row
# is a transaction). Parsing itself is left to the sign-aware parse_amount/.num,
# which read the raw cell verbatim: pre-stripping here used to remove a trailing
# OD/DR/CR and silently flip an overdrawn balance's sign.
.has_money <- function(x) grepl("[0-9]", as.character(x))

# .pdf_has_amount(r, style) / .pdf_is_summary(description, raw) -- the amount and
# summary-line halves of the row KEEP predicate, lifted to module level so the
# table reader (parse_pdf_table) and the Inspect overlay (inspect_pdf_layout)
# share ONE definition and can never disagree about which rows are transactions.
.pdf_has_amount <- function(r, style) {
  .has_money(if (identical(style, "debit_credit_cols"))
    paste(r$debit %||% "", r$credit %||% "") else (r$amount %||% ""))
}
# A summary line (opening/closing balance, brought/carried forward, totals) is NOT
# a transaction even though it carries a money value on a dated line. Match the
# WHOLE label so a real "Total Payments to ACME Ltd" is KEPT; errs toward keeping.
.pdf_is_summary <- function(description, raw = NULL) {
  d <- tolower(trimws(description %||% ""))
  if (!nzchar(d)) d <- tolower(trimws(raw %||% ""))
  lbl <- trimws(sub("[-:]*\\s*[$(]?[0-9][0-9,. ]*[0-9)]*\\s*(dr|cr|od)?\\s*$", "", d))
  grepl(paste0(
    "^(statement\\s+)?(opening|closing)\\s+balance$",
    "|^balance\\s+(brought|carried)\\s+(forward|fwd|f/?wd?)$",
    "|^balance\\s+[bc]/f$",
    "|^(brought|carried)\\s+forward$",
    "|^total\\s+(withdrawals|deposits|credits|debits|payments|fees|transactions)$"),
    lbl)
}

# Per-cell OCR confidence floor: a word below this (0-100) in a date/amount/
# balance cell earns an `ocr_low_conf` flag. Deliberately conservative -- only
# clearly-doubtful reads are flagged, so the signal stays meaningful.
.OCR_CELL_MIN_CONF <- 60

parse_pdf_table <- function(input, template) {
  t <- template$table %||% list()
  cols <- t$columns %||% list()
  extras_cols <- t$extras %||% list()
  region <- t$region %||% list()
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% 3)); if (is.na(row_tol)) row_tol <- 3
  date_fmt <- t$date_format %||% "%d/%m/%Y"
  style <- t$amount_sign %||% "signed"
  # decimal_mark: dot | comma | auto. Accepted top-level or inside the table block
  # so a European PDF template can declare its locale.
  dec <- template$decimal_mark %||% t$decimal_mark %||% "auto"
  udef <- template$unsigned_default %||% t$unsigned_default %||% "debit"
  words_by_page <- input$words %||% list()

  # .first_date(cell) -- keep only the FIRST date in a date cell. A PDF date band
  # can capture two dates on a row (a transaction date AND a processed/value
  # date); appending a year to "17 Oct 17 Sep" makes R read the second day as the
  # year (-> 0017-10-17), so dates come out wildly wrong. Trim to the leading date
  # (this format's count of whitespace-separated pieces). It also drops a stray
  # word that bleeds into the band. date_raw keeps the verbatim cell.
  .date_fields <- length(strsplit(trimws(date_fmt), "[[:space:]]+")[[1]])
  .first_date <- function(cells) vapply(cells, function(cc) {
    if (is.na(cc)) return(NA_character_)
    toks <- strsplit(trimws(cc), "[[:space:]]+")[[1]]
    if (length(toks) <= .date_fields) as.character(cc)
    else paste(toks[seq_len(.date_fields)], collapse = " ")
  }, character(1), USE.NAMES = FALSE)

  recs <- list()
  for (p in seq_along(words_by_page)) {
    w <- words_by_page[[p]]
    if (is.null(w) || !nrow(w)) next
    w <- as.data.frame(w, stringsAsFactors = FALSE)
    if (!is.null(region$x_min)) w <- w[(w$x + w$width) >= region$x_min, , drop = FALSE]
    if (!is.null(region$x_max)) w <- w[w$x <= region$x_max, , drop = FALSE]
    if (!is.null(region$y_min)) w <- w[w$y >= region$y_min, , drop = FALSE]
    if (!is.null(region$y_max)) w <- w[w$y <= region$y_max, , drop = FALSE]
    if (!nrow(w)) next
    w <- w[order(w$y, w$x), , drop = FALSE]
    grp <- cumsum(c(TRUE, diff(w$y) > row_tol))
    for (g in unique(grp)) {
      rw <- w[grp == g, , drop = FALSE]
      rec <- list(page = p,
        date = .pdf_cell(rw, cols$date), description = .pdf_cell(rw, cols$description),
        amount = .pdf_cell(rw, cols$amount), balance = .pdf_cell(rw, cols$balance),
        debit = .pdf_cell(rw, cols$debit), credit = .pdf_cell(rw, cols$credit),
        particulars = .pdf_cell(rw, cols$particulars), code = .pdf_cell(rw, cols$code),
        reference = .pdf_cell(rw, cols$reference), other_party = .pdf_cell(rw, cols$other_party),
        type = .pdf_cell(rw, cols$type),
        raw = paste(rw$text[order(rw$x)], collapse = " "))
      # lowest OCR confidence across this row's CRITICAL cells (NA on text pages).
      cc <- c(.cell_minconf(rw, cols$date), .cell_minconf(rw, cols$amount),
              .cell_minconf(rw, cols$balance), .cell_minconf(rw, cols$debit),
              .cell_minconf(rw, cols$credit))
      cc <- cc[!is.na(cc)]
      rec$ocr_minconf <- if (length(cc)) min(cc) else NA_real_
      for (ef in names(extras_cols)) rec[[paste0("x.", ef)]] <- .pdf_cell(rw, extras_cols[[ef]])
      recs[[length(recs) + 1L]] <- rec
    }
  }

  # Year context: many statements show the day/month only ("21 Apr") and put the
  # year in the statement period. When the date_format has no year token, attach
  # the year from the period (single year -> that year; a period spanning a
  # year-end -> the year that lands each date inside the period). Generic: no
  # bank-specific logic, driven entirely by the statement's own period text.
  # Statement-level metadata (period + opening/closing balance) via the label
  # dictionary. Wiring the balances into the header lets balance_reconciliation
  # actually run for PDFs -- so a PDF that reconciles earns "high" trust and the
  # completeness guard is satisfied, exactly like a delimited statement.
  md <- safe(extract_metadata(input), NULL)
  has_year <- grepl("%[Yy]", date_fmt)
  full_date <- function(raw) raw
  eff_fmt <- date_fmt
  if (!has_year) {
    eff_fmt <- paste(date_fmt, "%Y")
    # Parse the period bounds (2-digit years too, e.g. ASB "13 Jun 26") and take
    # the year(s) from the parsed dates -- more robust than a 4-digit regex.
    # Reject implausible years: as.Date("13 Aug 25", "%d %b %Y") yields 0025 (not
    # NA), so without this the 4-digit format greedily eats a 2-digit year.
    pdate <- function(s) { for (f in c("%d %b %Y", "%d %B %Y", "%d %b %y", "%d %B %y",
        "%d/%m/%Y", "%d/%m/%y", "%Y-%m-%d")) {
      dd <- suppressWarnings(as.Date(s, f))
      if (!is.na(dd) && as.integer(format(dd, "%Y")) >= 1990) return(dd) }; as.Date(NA) }
    p0 <- pdate(md$period_start); p1 <- pdate(md$period_end)
    yrs <- suppressWarnings(as.integer(format(c(p0, p1)[!is.na(c(p0, p1))], "%Y")))
    yrs <- unique(yrs[!is.na(yrs)])
    # Fallback: some statements print day/month only in the table AND give no
    # parseable period. Rather than silently drop EVERY row (year-less dates parse
    # to NA and fail the date filter), scan the page text for a plausible 4-digit
    # year. Only used when it is UNAMBIGUOUS (a single distinct year on the page):
    # if the text shows zero or several years we do not guess, keeping to the
    # "never silently wrong" contract. date_raw stays verbatim regardless.
    if (!length(yrs)) {
      alltext <- paste(unlist(input$pages %||% input$text %||% character(0)), collapse = " ")
      cy <- suppressWarnings(as.integer(regmatches(alltext,
              gregexpr("\\b(?:19|20)[0-9]{2}\\b", alltext, perl = TRUE))[[1]]))
      cy <- unique(cy[!is.na(cy) & cy >= 1990 & cy <= 2099])
      if (length(cy) == 1L) yrs <- cy
    }
    full_date <- function(raw) {
      if (!length(yrs)) return(raw)
      bad <- is.na(raw) | !nzchar(trimws(raw))
      if (length(yrs) == 1) { out <- paste(raw, yrs[1]); out[bad] <- raw[bad]; return(out) }
      out <- vapply(raw, function(r) {
        if (is.na(r) || !nzchar(trimws(r))) return(NA_character_)
        cand <- suppressWarnings(as.Date(paste(r, yrs), eff_fmt))
        inp <- !is.na(cand) & (is.na(p0) | cand >= p0) & (is.na(p1) | cand <= p1)
        pick <- if (any(inp)) which(inp)[1] else which(!is.na(cand))[1]
        if (is.na(pick)) pick <- 1L
        paste(r, yrs[pick])
      }, character(1))
      out
    }
  }

  # Keep only genuine transaction rows: the date cell must parse AND the row must
  # carry a real money amount (in the amount, or debit/credit, column). Requiring
  # an amount drops date-only lines that leak into the date band -- a statement's
  # issue date, a page header, a "balance brought forward" carry line -- which a
  # date-parse-only filter would wrongly keep. Balance is deliberately NOT enough
  # on its own (carry-forward rows aren't transactions).
  # Balance alone is deliberately NOT enough (carry-forward rows aren't
  # transactions); a real transaction is never *named* "closing balance". Both
  # halves live in module-level helpers so the Inspect overlay applies the SAME
  # rule -- errs toward keeping (a stray summary breaks reconciliation LOUDLY,
  # dropping a real transaction loses money SILENTLY, which the contract forbids).
  .has_amount <- function(r) .pdf_has_amount(r, style)
  .is_summary <- function(r) .pdf_is_summary(r$description, r$raw)
  # Did we manage to resolve a year for a year-less date format? When we did NOT
  # (no period, no year anywhere in the text), dropping every row would silently
  # lose a whole statement's transactions -- the worst forensic outcome, and one
  # seen on real data. Instead, still KEEP a dated money line if its day/month is
  # valid under the base format (sentinel year), carry date_raw verbatim, leave
  # date_iso NA (the real year is genuinely unknown), and flag it date_unresolved
  # so the reviewer can assign the year -- data preserved, never silently wrong.
  year_resolved <- has_year || length(yrs) > 0
  .date_ok <- function(raw) {
    raw <- .first_date(raw)
    if (year_resolved)
      return(!is.na(suppressWarnings(parse_date(full_date(raw), eff_fmt)$iso)))
    !is.na(suppressWarnings(parse_date(paste(raw, "2000"),
                                       paste(date_fmt, "%Y"))$iso))
  }
  # A REDACTED date cell must NOT drop a real transaction: if a redaction overlay
  # sits over the date column, the amount/description are still there, so keep the
  # row (it is flagged redacted below and its date_iso is left NA). Losing it would
  # silently delete a transaction -- forbidden.
  .redacted_cell <- function(v) !is.na(v) && grepl("REDACT", toupper(as.character(v)))
  keep <- vapply(recs, function(r)
    (.date_ok(r$date) || .redacted_cell(r$date)) && .has_amount(r) && !.is_summary(r),
    logical(1))
  recs <- recs[keep]
  n <- length(recs)
  getc <- function(f) if (n == 0) character(0) else
    vapply(recs, function(r) r[[f]] %||% NA_character_, character(1))

  if (n == 0) {
    date_iso <- character(0); date_raw <- character(0); description <- character(0)
    amt <- list(value = numeric(0), direction = character(0), raw = character(0))
  } else {
    d <- parse_date(full_date(.first_date(getc("date"))), eff_fmt)
    date_iso <- d$iso; date_raw <- getc("date")   # date_raw stays verbatim (both dates, no year)
    if (identical(style, "debit_credit_cols")) {
      deb_raw <- getc("debit"); cr_raw <- getc("credit")
      amt <- parse_amount(NULL, "debit_credit_cols",
                          list(debit = deb_raw, credit = cr_raw, decimal = dec))
      cr_has <- !is.na(cr_raw) & nzchar(trimws(cr_raw))
      amt$raw <- ifelse(cr_has, cr_raw, deb_raw)
    } else {
      amt_raw <- getc("amount")
      amt <- parse_amount(amt_raw, style, list(decimal = dec, unsigned_default = udef)); amt$raw <- amt_raw
    }
    description <- clean_description(getc("description"))
  }
  vb <- function(f) if (n == 0) character(0) else blank_to_na(getc(f))
  has_bal <- !is.null(cols$balance)
  balance <- if (n == 0 || !has_bal) rep(NA_real_, n) else parse_amount(getc("balance"), "signed", list(decimal = dec))$value
  balance_raw <- if (n == 0 || !has_bal) rep(NA_character_, n) else getc("balance")

  # amt_redacted: the AMOUNT itself was hidden (amount cell, or a debit/credit
  # cell) -> the value is genuinely unknown and is nulled. `redacted` (the row
  # flag) is broader: any of date/amount/description hidden marks the row, but a
  # row whose DATE was redacted still keeps its real amount.
  amt_redacted <- if (n == 0) logical(0) else if (identical(style, "debit_credit_cols"))
    grepl("REDACTED", getc("debit"), ignore.case = TRUE) |
    grepl("REDACTED", getc("credit"), ignore.case = TRUE)
  else grepl("REDACTED", getc("amount"), ignore.case = TRUE)
  redacted <- if (n == 0) logical(0) else
    amt_redacted |
    grepl("REDACTED", getc("description"), ignore.case = TRUE) |
    grepl("REDACTED", getc("date"), ignore.case = TRUE)
  # malformed: the row was kept (dated line carrying a money-looking amount) yet
  # the amount could not be parsed to a number -- a genuine parse failure, not a
  # redaction. Flagging it lets the no_unparsed_rows KPI catch PDF parse gaps the
  # same way it already does for delimited (previously this path never set the
  # flag, so no_unparsed_rows was blind to a mis-read PDF amount).
  malformed <- if (n == 0) logical(0) else (is.na(amt$value) & !redacted)
  # date_unresolved: kept despite an unknown year (see .date_ok) -- date_iso is NA
  # but the transaction is preserved. Marked so trust/review reflect the gap.
  date_unresolved <- if (n == 0) logical(0)
    else (!year_resolved & !is.na(date_raw) & nzchar(trimws(date_raw)))
  # ocr_low_conf: an OCR'd date/amount/balance cell held a word below the
  # per-cell confidence floor -- a likely misread digit that the page-mean
  # confidence would mask. Only fires on OCR pages (text pages carry no conf).
  ocr_minconf <- if (n == 0) numeric(0) else vapply(recs, function(r)
    if (is.null(r$ocr_minconf)) NA_real_ else as.numeric(r$ocr_minconf), numeric(1))
  ocr_low <- if (n == 0) logical(0) else (!is.na(ocr_minconf) & ocr_minconf < .OCR_CELL_MIN_CONF)
  flags <- if (n == 0) character(0) else {
    add <- function(base, cond, tok)
      ifelse(cond, ifelse(nzchar(base), paste0(base, ",", tok), tok), base)
    f <- ifelse(redacted, "redacted", ifelse(malformed, "malformed", ""))
    f <- add(f, date_unresolved, "date_unresolved")
    f <- add(f, ocr_low, "ocr_low_conf")
    f
  }
  if (n > 0) amt$value[amt_redacted] <- NA_real_   # only null when the AMOUNT was hidden

  core <- coerce_core(data.frame(
    row_id = seq_len(n), date = date_iso, date_raw = date_raw, description = description,
    amount = if (n == 0) numeric(0) else amt$value,
    amount_raw = if (n == 0) character(0) else amt$raw,
    direction = if (n == 0) character(0) else amt$direction,
    balance = balance, balance_raw = balance_raw,
    particulars = vb("particulars"), code = vb("code"), reference = vb("reference"),
    other_party = vb("other_party"), type = vb("type"),
    currency = rep(template$currency %||% "NZD", n), flags = flags,
    stringsAsFactors = FALSE))

  if (length(extras_cols) && n > 0) {
    ex <- list(row_id = seq_len(n))
    for (ef in names(extras_cols)) ex[[ef]] <- blank_to_na(getc(paste0("x.", ef)))
    extras <- data.frame(ex, stringsAsFactors = FALSE, check.names = FALSE)
  } else extras <- data.frame(row_id = integer(0))

  # sign-aware: a "$1,234.56 DR" / "(1,234.56)" opening/closing balance keeps its
  # negative sign (via .num) instead of being read as a positive number; the
  # template's decimal locale applies here too.
  .money_num <- function(x) .num(x %||% NA_character_, dec)
  header <- list(
    bank = template$bank %||% NA_character_, statement_type = template$statement_type %||% NA_character_,
    template_id = template$id %||% NA_character_, template_version = template$version %||% NA,
    account_number = NA_character_, account_name = NA_character_,
    period_start = md$period_start %||% NA_character_, period_end = md$period_end %||% NA_character_,
    opening_balance = .money_num(md$opening_balance), closing_balance = .money_num(md$closing_balance),
    currency = template$currency %||% "NZD",
    source_file = basename(input$path), source_sha256 = input$sha256,
    page_count = input$meta$page_count %||% NA_integer_, row_count = n,
    stated_count = md$stated_count %||% NA_integer_,
    ocr_pages = input$meta$ocr_pages %||% 0L,
    ocr_min_confidence = input$meta$ocr_min_conf %||% NA_real_)

  pages_v <- if (n == 0) integer(0) else vapply(recs, function(r) as.integer(r$page), integer(1))
  provenance <- data.frame(row_id = seq_len(n),
    source_ref = if (n == 0) character(0) else sprintf("pdf:p%d", pages_v),
    raw = getc("raw"), stringsAsFactors = FALSE)

  list(transactions = core, extras = extras, header = header,
       provenance = provenance, source_line_count = NA_integer_)
}
