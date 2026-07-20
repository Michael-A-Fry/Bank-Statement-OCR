# wizard_detect.R -- auto-detection helpers so the template wizard can pre-fill
# everything from a sample. Goal: an analyst with ZERO data background only has
# to CONFIRM plain-English guesses, never type a date code or pick jargon.
# Pure base R.

wd_delims <- function() c(",", "\t", ";", "|")

# Sniff the delimiter from the header line.
detect_delimiter <- function(path) {
  line <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) "")
  if (!length(line) || !nzchar(line)) return(",")
  counts <- vapply(wd_delims(), function(d)
    length(gregexpr(d, line, fixed = TRUE)[[1]][gregexpr(d, line, fixed = TRUE)[[1]] > 0]),
    integer(1))
  if (max(counts) == 0) return(",")
  wd_delims()[which.max(counts)]
}

# Candidate date formats: strptime code, plain label, and a shape regex so a
# 2-digit year is never mistaken for a 4-digit one. Ordered by auto-detect
# priority: unambiguous / year-bearing forms first, the ambiguous US order after
# the day/month default, and the YEAR-LESS forms ("2 Dec") last -- those take the
# year from the statement period (works on PDF statements; see parse_pdf_table).
wd_date_table <- function() list(
  # numeric, with a year
  list(fmt = "%d/%m/%Y", label = "31/12/2025  (day/month/year)",             rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$"),
  list(fmt = "%d/%m/%y", label = "31/12/25  (day/month/2-digit year)",       rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$"),
  list(fmt = "%Y-%m-%d", label = "2025-12-31  (year-month-day, ISO)",        rx = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$"),
  list(fmt = "%d-%m-%Y", label = "31-12-2025  (day-month-year)",             rx = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$"),
  list(fmt = "%d.%m.%Y", label = "31.12.2025  (day.month.year)",             rx = "^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}$"),
  list(fmt = "%Y/%m/%d", label = "2025/12/31  (year/month/day)",             rx = "^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$"),
  list(fmt = "%m/%d/%Y", label = "12/31/2025  (US month/day/year)",          rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$"),
  # month-name, with a year
  list(fmt = "%d %b %Y", label = "31 Dec 2025  (day month-name year)",       rx = "^[0-9]{1,2} [A-Za-z]{3,9} [0-9]{4}$"),
  list(fmt = "%d %B %Y", label = "31 December 2025  (day full-month year)",  rx = "^[0-9]{1,2} [A-Za-z]{3,9} [0-9]{4}$"),
  list(fmt = "%d %b %y", label = "31 Dec 25  (day month-name 2-digit year)", rx = "^[0-9]{1,2} [A-Za-z]{3,9} [0-9]{2}$"),
  list(fmt = "%d-%b-%Y", label = "31-Dec-2025  (day-month-name-year)",       rx = "^[0-9]{1,2}-[A-Za-z]{3,9}-[0-9]{4}$"),
  list(fmt = "%b %d, %Y", label = "Dec 31, 2025  (US month-name day, year)", rx = "^[A-Za-z]{3,9} [0-9]{1,2}, ?[0-9]{4}$"),
  list(fmt = "%B %d, %Y", label = "December 31, 2025  (US full-month day, year)", rx = "^[A-Za-z]{3,9} [0-9]{1,2}, ?[0-9]{4}$"),
  # YEAR-LESS: the year comes from the statement period, not the cell
  list(fmt = "%d %b", label = "2 Dec  (day + month-name; year from the statement)",  rx = "^[0-9]{1,2} [A-Za-z]{3,9}$", yearless = TRUE),
  list(fmt = "%d %B", label = "2 December  (day + full month; year from the statement)", rx = "^[0-9]{1,2} [A-Za-z]{3,9}$", yearless = TRUE),
  list(fmt = "%d/%m", label = "2/12  (day/month; year from the statement)",   rx = "^[0-9]{1,2}/[0-9]{1,2}$", yearless = TRUE)
)

# Return the strptime code that parses ALL sample values, or "" if none fit.
# Normalises exactly like parse_date (drops ordinal suffixes "2nd"->"2", folds
# "Sept"->"Sep") so detection agrees with parsing. A year-less format is validated
# by appending a sentinel year (a bare "2 Dec" can't be an R Date on its own).
detect_date_format <- function(values) {
  v <- trimws(as.character(values)); v <- v[nzchar(v) & !is.na(v)]
  if (!length(v)) return("")
  v <- utils::head(v, 50L)
  v <- gsub("(?<=[0-9])(st|nd|rd|th)\\b", "", v, perl = TRUE, ignore.case = TRUE)
  v <- gsub("\\bSept\\b", "Sep", v, ignore.case = TRUE)
  parses <- function(fmt, yearless)
    if (isTRUE(yearless)) !any(is.na(as.Date(paste(v, "2000"), paste(fmt, "%Y"))))
    else !any(is.na(as.Date(v, format = fmt)))
  for (e in wd_date_table()) {
    if (all(grepl(e$rx, v)) && parses(e$fmt, isTRUE(e$yearless))) return(e$fmt)
  }
  ""
}

date_format_label <- function(fmt) {
  for (e in wd_date_table()) if (identical(e$fmt, fmt)) return(e$label)
  fmt
}

# Plain-English labels for each amount style (names = engine style codes).
wd_amount_labels <- function() c(
  "signed"            = "One amount column, a minus sign means money out",
  "type_dc"           = "A D / C (debit / credit) indicator column",
  "debit_credit_cols" = "Separate money-in and money-out columns",
  "dr_cr_suffix"      = "Amounts ending in DR / CR"
)

# Guess the amount style from headers + a small character sample.
detect_amount_style <- function(headers, df = NULL) {
  h <- tolower(headers)
  hit <- function(pats) any(vapply(pats, function(p) any(grepl(p, h)), logical(1)))
  if (hit(c("debit", "withdrawal", "money out", "paid out")) &&
      hit(c("credit", "deposit", "money in", "paid in"))) return("debit_credit_cols")
  if (!is.null(df)) {
    for (cn in names(df)) {
      vals <- toupper(trimws(as.character(df[[cn]]))); vals <- vals[nzchar(vals)]
      if (length(vals) && all(vals %in% c("D", "C", "DR", "CR"))) return("type_dc")
    }
    amtcol <- headers[grepl("amount|value", h)][1]
    if (!is.na(amtcol) && amtcol %in% names(df) &&
        any(grepl("(DR|CR)[[:space:]]*$", toupper(as.character(df[[amtcol]]))))) return("dr_cr_suffix")
  }
  "signed"
}

# Best-guess a source column for a canonical field.
wd_field_patterns <- function() list(
  date = "date", amount = "amount|value",
  description = "payee|description|details|memo|narrative",
  particulars = "particulars", code = "^code$|analysis",
  reference = "reference|unique", type = "type",
  other_party = "other party|counterparty", balance = "balance"
)

guess_mapping <- function(headers, field) {
  p <- wd_field_patterns()[[field]]
  if (is.null(p)) return("(none)")
  hit <- grep(p, headers, ignore.case = TRUE)
  if (length(hit)) headers[hit[1]] else "(none)"
}
