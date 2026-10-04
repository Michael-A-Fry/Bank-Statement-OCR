# normalise.R -- deterministic field normalisation.
# parse_date / parse_amount / clean_description. No locale guessing, no ML.

# .normalise_date_str(s) -- fold the human spellings of a date onto the canonical
# form the strptime codes expect. For PARSING/DETECTION ONLY -- the raw cell is
# always kept verbatim elsewhere. This is the SINGLE source of truth shared by
# parse_date and the automatic reader's date typing, so the two can never disagree
# about what a date looks like. It:
#   * drops a leading weekday word     "Tuesday 12 October" -> "12 October"
#   * drops ordinal suffixes           "12th October" / "21st" -> "12 October" / "21"
#   * drops the connective "of"        "12 of October" -> "12 October"
#   * folds the 4-letter "Sept"->"Sep" that %b expects ("September"/%B is untouched:
#     the word boundary after "Sept" fails inside the longer word)
#   * collapses any doubled spaces the removals leave behind
.normalise_date_str <- function(s) {
  s <- trimws(as.character(s))
  # A weekday may be followed by a comma ("Monday, 2 March").
  s <- gsub("^(mon|tue|wed|thu|fri|sat|sun)[a-z]*[.,]?\\s+", "", s, perl = TRUE, ignore.case = TRUE)
  # An apostrophe for the century ("02 Feb '25") stands before a 2-digit year.
  s <- gsub("(?<=\\s)'([0-9]{2})$", "\\1", s, perl = TRUE)
  s <- gsub("(?<=[0-9])(st|nd|rd|th)\\b", "", s, perl = TRUE, ignore.case = TRUE)
  s <- gsub("\\bof\\b", "", s, perl = TRUE, ignore.case = TRUE)
  s <- gsub("\\bSept\\b", "Sep", s, ignore.case = TRUE)
  # ordinal day suffixes: "17th Sep" / "1st Aug" -> "17 Sep" / "1 Aug"
  s <- gsub("\\b([0-9]{1,2})(st|nd|rd|th)\\b", "\\1", s, ignore.case = TRUE)
  trimws(gsub("\\s+", " ", s))
}

# .date_canon(z) -- fold a date string onto a single comparable form so the
# round-trip check in .date_strict never FALSE-rejects on a cosmetic difference
# the declared format legitimately allows. It normalises away, on BOTH sides of
# the comparison:
#   * case               ("Oct" vs "oct")
#   * month-name width    (%b "Oct" round-trip vs a source that wrote "October")
#   * zero-padding        (%d "01" round-trip vs a source that wrote "1")
#   * run-together spaces
# COMPARISON aid only -- the raw cell is always kept verbatim elsewhere. English
# month names, matching the reader's existing %b/%B parse assumption.
.date_canon <- function(z) {
  z <- tolower(trimws(as.character(z)))
  # any month word -> its 3-letter key (unique per English month) so a %b vs %B
  # width difference between the round-trip and the source never mismatches.
  z <- gsub("\\b(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*", "\\1",
            z, perl = TRUE)
  z <- gsub("(?<![0-9])0+([0-9])", "\\1", z, perl = TRUE)  # strip leading zeros in numbers
  # A digit run together with a month NAME ("20Apr") is separated, on BOTH sides of
  # the comparison. WHY: OCR reads a tightly-set date column as ONE token -- measured
  # at 86% of day+month tokens on a real ANZ scan. base as.Date() parses "20Apr 2026"
  # under "%d %b %Y" CORRECTLY (strptime treats format whitespace as optional), but
  # the round-trip below reformats it to "20 Apr 2026", and without this fold the two
  # strings differed, the date failed closed to NA, and the reader dropped the row --
  # 41 rows survived out of 300 on that statement. Because .date_strict canonicalises
  # the round-trip AND the source with this same function, the change is symmetric: it
  # adds no parse leniency, a genuinely space-less declared format still matches, and
  # the guard's real target ("13/08/2025" misread under %d/%m/%y) carries no month
  # name and is untouched, as is the year bound.
  z <- gsub("(?<=[0-9])(?=(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec))", " ", z, perl = TRUE)
  z <- gsub("(?<=(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec))(?=[0-9])", " ", z, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", z))
}

# .date_strict(s, fmt) -- parse already-normalised date strings under `fmt`,
# returning ISO ONLY when the parse is TRUSTWORTHY, else NA. Base as.Date() is
# dangerously lenient: it reads "13/08/2025" under "%d/%m/%y" as 2020 (year<-"20",
# the trailing "25" silently ignored) -- a wrong figure that looks right, the
# cardinal failure. Two deterministic guards close that hole:
#   1. ROUND-TRIP -- reformat the parsed Date back through the SAME `fmt` and
#      require it to canonically match the input. If the format didn't consume
#      the whole string (or read the wrong field widths) the two disagree.
#   2. YEAR BOUND [PARAM_YEAR_MIN, PARAM_YEAR_MAX] -- a 2-digit source read under a
#      4-digit "%Y" (year 0025) round-trips clean but is obvious nonsense. The
#      trusted-year window is one shared decision (see R/params.R); reconcile and
#      diagnose apply the same bound.
# Both are needed: the round-trip catches (1)'s wrong-width case (year stays in
# range), the bound catches (2)'s out-of-range case (round-trip matches).
.date_strict <- function(s, fmt) {
  d <- suppressWarnings(as.Date(s, format = fmt))
  parsed <- which(!is.na(d))
  if (length(parsed)) {
    yr <- as.integer(format(d[parsed], "%Y"))
    rt <- format(d[parsed], fmt)                       # round-trip through same fmt
    bad <- !.plausible_year(yr) |
           .date_canon(rt) != .date_canon(s[parsed])
    d[parsed[bad]] <- as.Date(NA)
  }
  format(d, "%Y-%m-%d")                                # format(NA) -> NA
}

# parse_date(x, fmt) -> list(iso, raw)
# `iso` is YYYY-MM-DD (NA when unparseable OR untrustworthy -- see .date_strict);
# `raw` is the input verbatim. Never emits a silently-wrong date.
parse_date <- function(x, fmt) {
  raw <- as.character(x)
  iso <- rep(NA_character_, length(raw))
  ok <- !is.na(raw) & nzchar(trimws(raw))
  if (any(ok)) {
    s <- .normalise_date_str(raw[ok])
    iso[ok] <- .date_strict(s, fmt)
  }
  list(iso = iso, raw = raw)
}

# .num(s, decimal) -- parse a money string to numeric, ROBUSTLY. Handles the real
# formats that appear on statements, and returns NA (never a silently-wrong value)
# when it can't be sure -- the caller then flags the NA. Covered:
#   thousands : 1,234.56  /  1 234.56  /  1'234.56
#   decimals  : 1,234.56 (US)  and  1.234,56 (European comma) via last-separator
#   negatives : -123.45  /  (123.45)  /  123.45-  /  trailing DR or OD ; CR = +ve
#   currency  : dollar, pound, euro and any other symbol/letters stripped
#
# `decimal` selects how a LONE separator is read (a template can declare its
# bank's locale via `decimal_mark:` so nothing is guessed):
#   "auto"  (default) -- lone dot = decimal, lone comma uses the 1-2-digit rule.
#             Correct for NZ/AU/UK/US ("1.234"=1.234, "1,234"=1234).
#   "dot"   -- dot is the decimal point, comma is thousands (US/UK/NZ explicit).
#   "comma" -- comma is the decimal, dot is thousands (European: "1.234"=1234,
#             "1.234,56"=1234.56, "1234,56"=1234.56).
# The mixed case ("1.234,56" / "1,234.56", both separators present) is
# unambiguous and read the same way under every mode.
# debit_rx / credit_rx: the trailing balance-sign markers, defaulted to the
# built-ins so a direct call is unchanged; .num builds them from the lexicon once
# per column (so "cow"/"OD"-style markers plumb in without a code change).
# .money_contaminated(raw) -- TRUE when a cell that must hold ONE money value
# holds something else as well, so no number may honestly be taken from it.
#
# MEASURED, NOT IMAGINED. A column band collects every word whose centre falls in
# it (.pdf_cell, R/parse_pdf_table.R). A long transaction description that
# overflows its own band therefore drops words into the amount band beside it, and
# .num_one's "drop everything that is not a digit" then GLUED THE DIGITS TOGETHER:
# the cell "ASSESSMENT 2291104A 7.44" -- a reference number and a $7.44 credit --
# came back as 22911047.44. Not a crash, not an NA: a plausible-looking figure
# four orders of magnitude wrong, in a row whose date and description are right.
# Found by scoring the synthetic corpus against its own ground truth
# (tools/synth/, case band_narrow_desc: 13 of 20 rows wrong this way).
#
# The answer is NA, never a guess. Taking "the rightmost money-looking token"
# would be right most of the time and silently wrong the rest, which is the worse
# failure; NA fires the `malformed` flag that already exists and already says
# "the amount could not be read as a number" on the row.
#
# What is still legitimate, and must keep working:
#   "$1,234.56"  "NZD 1,234.56"  "1,234.56 CR"  "1,234.56 DR"  "(123.45)"
#   "1 234,56"   -- space-separated thousands (French/Nordic), a real shape
#   "500.00 PMT" -- whatever THIS bank's sign marker is, which is why the caller's
#                   own debit_rx / credit_rx are stripped rather than a fixed list:
#                   the marker vocabulary is admin-approved in the lexicon, and a
#                   guard with its own hardcoded copy would reject the very
#                   wording the lexicon exists to let a site configure.
.MONEY_WORDS <- c("NZD", "AUD", "USD", "GBP", "EUR", "JPY", "CAD")
.money_contaminated <- function(raw, debit_rx = NULL, credit_rx = NULL) {
  s <- toupper(trimws(as.character(raw)))
  # the caller's own sign markers are declared vocabulary, not contamination
  for (rx in c(debit_rx, credit_rx)) if (!is.null(rx)) s <- sub(rx, " ", s)
  # ...and so is a currency CODE, which is letters and so must be named. A currency
  # SYMBOL needs no stripping: it is not a letter, so it never looks like a word
  # here, and the digit-only pass below drops it. Matching one would also mean a
  # multibyte character class in a regex, which THROWS under LC_ALL=C -- the locale
  # this deploys and tests under (see the note above .value_from_line in R/labels.R).
  s <- gsub(sprintf("\\b(%s)\\b", paste(.MONEY_WORDS, collapse = "|")), " ", s)
  # ...and so is a currency code glued to its dollar sign ("NZ$317.02").
  s <- gsub("(^|[^A-Z])(NZ|AU|US|CA|HK|SG)[$]", "\\1 ", s)
  # any OTHER letter means this cell is carrying words, not a figure
  if (grepl("[A-Za-z]", s, useBytes = TRUE)) return(TRUE)
  # two or more separate digit runs means two or more things in one cell, unless
  # they are space-separated thousands: 1-3 digits, then groups of exactly 3.
  runs <- regmatches(s, gregexpr("[0-9][0-9.,]*", s))[[1]]
  if (length(runs) < 2) return(FALSE)
  lead <- sub("[.,].*$", "", runs[1])
  if (nchar(lead) > 3L) return(TRUE)
  rest <- runs[-1]
  ok <- vapply(rest, function(r) grepl("^[0-9]{3}([.,][0-9]{1,2})?$", r), logical(1))
  !all(ok)
}

# .DASHES -- every character a PDF may use where a minus sign belongs, as its
# \uXXXX escape so no non-ASCII byte enters this file.
#
# MEASURED, AND IT WAS A LIVE SIGN INVERSION. poppler returns a typeset minus as
# U+2212 MINUS SIGN, not as an ASCII hyphen -- this repository's own fixture
# generator carries the note "base pdf() maps '-' to U+2212" -- and .num_one only
# ever looked for "-". So "\u2212123.45" came back as +123.45: a withdrawal read
# as a deposit, which is the worst single error this tool can make. Every shipped
# fixture happens to use an ASCII hyphen, so the whole suite passed over it.
#
# On a statement that prints a running balance the continuity check would catch
# it. On one that does not (and they exist) nothing would.
#
# Matched as BYTES, one fixed string at a time, never as a character class: a
# multibyte class in a regex throws under LC_ALL=C, which is the locale this
# deploys and tests under.
.DASHES <- c("\u2212",  # MINUS SIGN -- what poppler emits for a typeset minus
             "\u2010",  # HYPHEN
             "\u2011",  # NON-BREAKING HYPHEN
             "\u2013",  # EN DASH
             "\u2014",  # EM DASH
             "\ufe63",  # SMALL HYPHEN-MINUS
             "\uff0d")  # FULLWIDTH HYPHEN-MINUS
# U+00AD SOFT HYPHEN is a line-break hint, not a sign: it is REMOVED, not read as
# a minus. Treating an invisible formatting character as a negation would invent a
# sign the page never showed.
.SOFT_HYPHEN <- "\u00ad"
.ascii_dashes <- function(x) {
  x <- gsub(.SOFT_HYPHEN, "", x, fixed = TRUE, useBytes = TRUE)
  for (d in .DASHES) x <- gsub(d, "-", x, fixed = TRUE, useBytes = TRUE)
  x
}

.num_one <- function(raw, decimal = "auto",
                     debit_rx = "(DR|OD)\\s*$", credit_rx = "CR\\s*$") {
  if (is.na(raw)) return(NA_real_)
  raw <- .ascii_dashes(trimws(as.character(raw)))
  if (!nzchar(raw)) return(NA_real_)
  # A sign word printed BEFORE the figure ("DR 32.22", "cr 5.69") reads as the
  # same word printed after it. A figure carrying one before AND one after is two
  # signs, and reads as nothing.
  pre_neg <- NA
  pm <- regmatches(toupper(raw), regexpr("^[A-Z]{2}(?![A-Z])", toupper(raw), perl = TRUE))
  if (length(pm) == 1L) {
    if (grepl(debit_rx, pm)) pre_neg <- TRUE else if (grepl(credit_rx, pm)) pre_neg <- FALSE
    if (!is.na(pre_neg)) {
      raw <- trimws(substring(raw, 3))
      if (!nzchar(raw) || grepl(debit_rx, toupper(raw)) || grepl(credit_rx, toupper(raw))) return(NA_real_)
    }
  }
  if (.money_contaminated(raw, debit_rx, credit_rx)) return(NA_real_)
  neg <- isTRUE(pre_neg)
  up <- toupper(raw)
  if (grepl(debit_rx, up)) neg <- TRUE              # debit / overdrawn balance
  else if (grepl(credit_rx, up)) neg <- FALSE       # explicit credit -> positive
  s <- gsub("[^0-9.,()+-]", "", raw)                # drop currency/letters/space/apostrophe
  if (grepl("\\(", s) && grepl("\\)", s)) neg <- TRUE   # (123.45) accounting negative
  if (grepl("-", s)) neg <- TRUE                        # any minus -> negative
  s <- gsub("[()+-]", "", s)
  if (!nzchar(s)) return(NA_real_)
  hasdot <- grepl("\\.", s); hascomma <- grepl(",", s)
  if (identical(decimal, "dot")) {
    s <- gsub(",", "", s)                              # comma = thousands, dot = decimal
  } else if (identical(decimal, "comma")) {
    s <- gsub("\\.", "", s); s <- sub(",", ".", s)     # dot = thousands, comma = decimal
  } else if (hasdot && hascomma) {
    # auto + both separators: the LAST separator is the decimal one (unambiguous).
    if (max(gregexpr(",", s)[[1]]) > max(gregexpr("\\.", s)[[1]])) {
      s <- gsub("\\.", "", s); s <- sub(",", ".", s)  # European: . thousands, , decimal
    } else s <- gsub(",", "", s)                       # US/UK: , thousands
  } else if (hascomma) {
    # auto + lone comma: treat as decimal only when it looks like cents.
    parts <- strsplit(s, ",", fixed = TRUE)[[1]]
    if (length(parts) == 2 && nchar(parts[2]) %in% c(1L, 2L))
      s <- sub(",", ".", s)                            # decimal comma "1234,56"
    else s <- gsub(",", "", s)                          # thousands "1,234"
  }
  v <- suppressWarnings(as.numeric(s))
  if (is.na(v)) return(NA_real_)
  if (neg) -abs(v) else v
}
.num <- function(s, decimal = "auto") {
  debit_rx  <- sprintf("(%s)\\s*$", paste(toupper(c(lex("dr_cr_suffix_debit"),
                       lex("overdrawn_markers"))), collapse = "|"))
  credit_rx <- sprintf("(%s)\\s*$", paste(toupper(lex("dr_cr_suffix_credit")), collapse = "|"))
  vapply(as.character(s), .num_one, numeric(1), decimal = decimal,
         debit_rx = debit_rx, credit_rx = credit_rx, USE.NAMES = FALSE)
}

# .direction(v) -- sign -> "debit" (<0) / "credit" (>0) / NA (0 or NA).
.direction <- function(v) {
  ifelse(is.na(v), NA_character_,
    ifelse(v < 0, "debit", ifelse(v > 0, "credit", NA_character_)))
}

# parse_amount(x, style, opts) -> list(value, direction, raw)
# Styles: signed | debit_credit_cols | dr_cr_suffix | type_dc.
parse_amount <- function(x, style = "signed", opts = list()) {
  style <- style %||% "signed"
  dec <- opts[["decimal"]] %||% "auto"     # locale of the decimal separator

  if (style == "signed") {
    raw <- as.character(x)
    value <- .num(raw, dec)
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "debit_credit_cols") {
    deb <- opts[["debit"]]
    cr  <- opts[["credit"]]
    dv <- .num(deb, dec); cv <- .num(cr, dec)
    dz <- ifelse(is.na(dv), 0, dv)
    cz <- ifelse(is.na(cv), 0, cv)
    value <- cz - abs(dz)
    # If both columns blank for a row, value is unknown, not zero.
    both_blank <- is.na(dv) & is.na(cv)
    value[both_blank] <- NA_real_
    raw <- ifelse(!is.na(cv) & cv != 0, as.character(cr),
            ifelse(!is.na(dv) & dv != 0, as.character(deb),
              paste0(as.character(deb %||% ""), "|", as.character(cr %||% ""))))
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "dr_cr_suffix") {
    raw <- as.character(x)
    # debit / credit suffix markers come from the lexicon (default DR / CR).
    dset <- toupper(lex("dr_cr_suffix_debit")); cset <- toupper(lex("dr_cr_suffix_credit"))
    suf <- toupper(sub(".*?([A-Za-z]{2})\\s*$", "\\1", trimws(raw)))
    strip_rx <- sprintf("\\s*(%s)\\s*$", paste(c(dset, cset), collapse = "|"))
    # The SUFFIX is the sole source of sign in this style, so read the magnitude
    # UNSIGNED: .num already makes "(500.00)" and "-500.00" negative, which would
    # then be flipped a SECOND time by a DR suffix -> a wrong +500 for a figure
    # marked debit twice over. abs() keeps the suffix authoritative.
    mag <- abs(.num(sub(strip_rx, "", trimws(raw), perl = TRUE, ignore.case = TRUE), dec))
    sign <- ifelse(suf %in% dset, -1, ifelse(suf %in% cset, 1, NA_real_))
    value <- mag * sign
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "unsigned") {
    # Credit-card style: one amount column of UNSIGNED magnitudes, where the sign
    # is implied, not printed. An unmarked amount is a CHARGE; a trailing CR is a
    # PAYMENT (the opposite). `unsigned_default` sets the charge's sign:
    #   "debit"  (default) -> charge = -mag (money out), CR payment = +mag. This
    #            is the cash-flow view, consistent with a withdrawal column.
    #   "credit"           -> charge = +mag, CR payment = -mag. Charges raise the
    #            balance, so this ties out to a card's owed closing balance.
    # The CR marker always flips RELATIVE to the charge sign. amount_raw stays
    # verbatim either way.
    raw <- as.character(x)
    mag <- abs(.num(raw, dec))
    base <- if (identical(opts[["unsigned_default"]] %||% "debit", "credit")) 1 else -1
    up <- toupper(trimws(raw))
    sgn <- rep(base, length(mag))
    # The payment marker comes from the LEXICON (default "CR"), exactly as the
    # dr_cr_suffix style above reads it. Hardcoding "CR" here meant the SAME
    # admin-approved vocabulary was honoured in one amount style and silently
    # ignored in the other: a bank whose payment marker is written any other way
    # had every payment read with the CHARGE sign -- a wrong sign that looks right,
    # and invisible because the marker still prints beside it.
    credit_rx <- sprintf("(%s)\\s*$", paste(toupper(lex("dr_cr_suffix_credit")), collapse = "|"))
    sgn[grepl(credit_rx, up)] <- -base             # a CR payment is the opposite of a charge
    value <- ifelse(is.na(mag), NA_real_, sgn * mag)
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "type_dc") {
    raw <- as.character(x)
    mag <- abs(.num(raw, dec))
    # Compare the indicator CASE- and whitespace-insensitively: a statement may
    # print it as "D" / "d" / "Debit" / " DR ", and a case-sensitive "D" match
    # silently flips every debit to a credit -- a wrong sign that looks right.
    # Exact `[[` indexing (never `$`) avoids partial-matching `type` onto
    # `type_debit_value` when the type column is unmapped.
    tv   <- toupper(trimws(as.character(opts[["type"]] %||% rep(NA_character_, length(raw)))))
    dval <- toupper(trimws(as.character(opts[["type_debit_value"]] %||% "D")))
    cval <- opts[["type_credit_value"]]
    cval <- if (is.null(cval)) NA_character_ else toupper(trimws(as.character(cval)))
    is_debit <- !is.na(tv) & nzchar(tv) & tv == dval
    if (!is.na(cval) && nzchar(cval)) {
      # A credit token is declared: a value matching NEITHER token is genuinely
      # ambiguous, so fail CLOSED (NA, flagged downstream) rather than silently
      # signing it a credit. This is the fail-closed contract at work.
      is_credit <- !is.na(tv) & nzchar(tv) & tv == cval
      value <- ifelse(is_debit, -mag, ifelse(is_credit, mag, NA_real_))
    } else {
      # Back-compat (no credit token declared): the long-standing binary rule --
      # anything that is not the debit token is treated as a credit.
      value <- ifelse(is_debit, -mag, mag)
    }
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  stop(sprintf("parse_amount: unknown style '%s'", style))
}

# clean_description(x) -- VERBATIM. Only trim outer whitespace. Never strip
# apostrophes, ampersands, unicode, or any interior character.
clean_description <- function(x) {
  trimws(as.character(x))
}

# ---- vocabularies the table reader and the lexicon share -------------------------

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
  list(fmt = "%d-%m-%y", label = "31-12-25  (day-month-2-digit year)",       rx = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2}$"),
  list(fmt = "%d.%m.%Y", label = "31.12.2025  (day.month.year)",             rx = "^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4}$"),
  list(fmt = "%d.%m.%y", label = "31.12.25  (day.month.2-digit year)",       rx = "^[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{2}$"),
  list(fmt = "%Y/%m/%d", label = "2025/12/31  (year/month/day)",             rx = "^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$"),
  list(fmt = "%m/%d/%Y", label = "12/31/2025  (US month/day/year)",          rx = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$"),
  # month-name, with a year
  list(fmt = "%d %b %Y", label = "31 Dec 2025  (day month-name year)",       rx = "^[0-9]{1,2} [A-Za-z]{3,9} [0-9]{4}$"),
  list(fmt = "%d %B %Y", label = "31 December 2025  (day full-month year)",  rx = "^[0-9]{1,2} [A-Za-z]{3,9} [0-9]{4}$"),
  list(fmt = "%d %b %y", label = "31 Dec 25  (day month-name 2-digit year)", rx = "^[0-9]{1,2} [A-Za-z]{3,9} [0-9]{2}$"),
  list(fmt = "%d-%b-%Y", label = "31-Dec-2025  (day-month-name-year)",       rx = "^[0-9]{1,2}-[A-Za-z]{3,9}-[0-9]{4}$"),
  list(fmt = "%b %d, %Y", label = "Dec 31, 2025  (US month-name day, year)", rx = "^[A-Za-z]{3,9} [0-9]{1,2}, ?[0-9]{4}$"),
  list(fmt = "%B %d, %Y", label = "December 31, 2025  (US full-month day, year)", rx = "^[A-Za-z]{3,9} [0-9]{1,2}, ?[0-9]{4}$"),
  # YEAR-LESS: the year comes from the statement period, not the cell. Ordinal
  # suffixes ("12th"), a leading weekday ("Tue 12 Oct") and the connective "of"
  # ("12 of October") are folded away by .normalise_date_str before these match,
  # so "12th October" and "12 October" are the same format to the tool.
  list(fmt = "%d %b", label = "2 Dec  (day + month-name, e.g. 12th October; year from the statement)",  rx = "^[0-9]{1,2} [A-Za-z]{3,9}$", yearless = TRUE),
  list(fmt = "%d %B", label = "2 December  (day + full month; year from the statement)", rx = "^[0-9]{1,2} [A-Za-z]{3,9}$", yearless = TRUE),
  list(fmt = "%b %d", label = "Oct 12  (month-name + day; year from the statement)",   rx = "^[A-Za-z]{3,9} [0-9]{1,2}$", yearless = TRUE),
  list(fmt = "%B %d", label = "October 12  (full month + day; year from the statement)", rx = "^[A-Za-z]{3,9} [0-9]{1,2}$", yearless = TRUE),
  list(fmt = "%d/%m", label = "2/12  (day/month; year from the statement)",   rx = "^[0-9]{1,2}/[0-9]{1,2}$", yearless = TRUE),
  # "1-FEB", "14-Mar": day, hyphen, month name; the year from the statement.
  list(fmt = "%d-%b", label = "2-Dec  (day-month-name; year from the statement)", rx = "^[0-9]{1,2}-[A-Za-z]{3,9}$", yearless = TRUE)
)

# Field-name patterns: the words a spreadsheet heading uses for each canonical
# field. Kept as the lexicon's built-in `field_name_patterns` (R/lexicon.R).
wd_field_patterns <- function() list(
  # Kept deliberately conservative: word-bounded or exact where a loose match
  # could hit the wrong column ("Money In" must not become the amount).
  date = "date|\\bday\\b", amount = "amount|value|^money$|^sum$",
  description = "payee|description|details|memo|narrative|narration",
  particulars = "particulars", code = "^code$|analysis",
  reference = "reference|unique", type = "type",
  other_party = "other party|counterparty", balance = "balance|^running$"
)

# ---- a template's own choices, resolved against the file ----------------------------

# resolve_date_format(values, formats) -> the ONE declared format that reads EVERY
# non-empty value, or NA_character_ when none of them does.
#
# WHY a template may declare a LIST of candidate formats: the same bank, the same
# export, the same header row -- and two different date styles across eras. ASB's
# FastNet CSV writes "2014/12/20" in one export and "13/10/2025" in another, so
# pinning one format made the other detect confidently and then return EVERY date
# NA (dates_readable = fail).
#
# WHY it is all-or-nothing: reading some rows under one format and the rest under
# another is precisely the silently-wrong outcome the charter forbids -- "13/10"
# and "10/13" are both readable, and a per-row mixture would swap day and month
# with nothing to show for it. So a candidate only wins if it reads the WHOLE
# column, the first such candidate (declaration order) wins so the result is
# deterministic, and "none of them fits" returns NA so the caller fails closed
# rather than guessing.
resolve_date_format <- function(values, formats) {
  fmts <- trimws(as.character(unlist(formats %||% character(0))))
  fmts <- fmts[!is.na(fmts) & nzchar(fmts)]
  if (!length(fmts)) return(NA_character_)
  if (length(fmts) == 1L) return(fmts[1])       # the ordinary case, behaviour unchanged
  v <- as.character(unlist(values))
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(fmts[1])               # nothing to judge on -> as declared
  for (f in fmts) if (all(!is.na(parse_date(v, f)$iso))) return(f)
  NA_character_
}

# resolve_delimiter(header_line, template) -> the ONE declared delimiter to read
# this file with. Like the date format above, a template MAY declare a LIST -- the
# same bank publishing the same export as CSV and as tab-delimited (ASB FastNet
# ships both: asb_transaction_export_01.csv and asb_transaction_export_02.tdv,
# byte-identical layout, different separator). Pinning one meant the other's header
# never split into columns at all, so it scored 0 and came back "unsupported".
#
# THE ALL-OR-NOTHING RULE, and why it is this one: a candidate wins only if the
# header row, split by it, contains EVERY column name the template's fingerprint
# names. Splitting a tab-delimited line on commas yields ONE field, so it can never
# satisfy that -- the wrong separator is rejected outright rather than "sort of"
# working. The test deliberately looks at the HEADER ONLY: making it depend on the
# data rows would let a single ragged row (which the reader is built to isolate and
# flag) reject the correct delimiter and pick a catastrophic one. When no candidate
# satisfies it, the FIRST declared delimiter is used, so a file the template does
# not fit reads badly and fails its checks rather than reading half right.
# A single declared delimiter short-circuits: every existing template is untouched.
resolve_delimiter <- function(header_line, template) {
  d <- as.character(unlist(template$delimiter %||% ","))
  d <- d[!is.na(d) & nzchar(d)]
  if (!length(d)) return(",")
  if (length(d) == 1L) return(d[1])            # the ordinary case, behaviour unchanged
  need <- trimws(as.character(unlist(template$fingerprint$header_contains_all %||% character(0))))
  need <- need[!is.na(need) & nzchar(need)]
  hl <- as.character(header_line)[1]
  if (!length(need) || is.na(hl) || !nzchar(hl)) return(d[1])
  for (delim in d) {
    fields <- trimws(as.character(.record_fields(hl, delim)))
    if (all(tolower(need) %in% tolower(fields))) return(delim)
  }
  d[1]
}
