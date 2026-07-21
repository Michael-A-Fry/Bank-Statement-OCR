# normalise.R -- deterministic field normalisation.
# parse_date / parse_amount / clean_description. No locale guessing, no ML.

# .normalise_date_str(s) -- fold the human spellings of a date onto the canonical
# form the strptime codes expect. For PARSING/DETECTION ONLY -- the raw cell is
# always kept verbatim elsewhere. This is the SINGLE source of truth shared by
# parse_date and the wizard's detect_date_format, so the reader and the detector
# can never disagree about what a date looks like. It:
#   * drops a leading weekday word     "Tuesday 12 October" -> "12 October"
#   * drops ordinal suffixes           "12th October" / "21st" -> "12 October" / "21"
#   * drops the connective "of"        "12 of October" -> "12 October"
#   * folds the 4-letter "Sept"->"Sep" that %b expects ("September"/%B is untouched:
#     the word boundary after "Sept" fails inside the longer word)
#   * collapses any doubled spaces the removals leave behind
.normalise_date_str <- function(s) {
  s <- trimws(as.character(s))
  s <- gsub("^(mon|tue|wed|thu|fri|sat|sun)[a-z]*\\.?\\s+", "", s, perl = TRUE, ignore.case = TRUE)
  s <- gsub("(?<=[0-9])(st|nd|rd|th)\\b", "", s, perl = TRUE, ignore.case = TRUE)
  s <- gsub("\\bof\\b", "", s, perl = TRUE, ignore.case = TRUE)
  s <- gsub("\\bSept\\b", "Sep", s, ignore.case = TRUE)
  # ordinal day suffixes: "17th Sep" / "1st Aug" -> "17 Sep" / "1 Aug"
  s <- gsub("\\b([0-9]{1,2})(st|nd|rd|th)\\b", "\\1", s, ignore.case = TRUE)
  trimws(gsub("\\s+", " ", s))
}

# parse_date(x, fmt) -> list(iso, raw)
# `iso` is YYYY-MM-DD (NA when unparseable); `raw` is the input verbatim.
parse_date <- function(x, fmt) {
  raw <- as.character(x)
  iso <- rep(NA_character_, length(raw))
  ok <- !is.na(raw) & nzchar(trimws(raw))
  if (any(ok)) {
    s <- .normalise_date_str(raw[ok])
    d <- suppressWarnings(as.Date(s, format = fmt))
    iso[ok] <- format(d, "%Y-%m-%d")  # format(NA) -> NA
  }
  list(iso = iso, raw = raw)
}

# .num(s, decimal) -- parse a money string to numeric, ROBUSTLY. Handles the real
# formats that appear on statements, and returns NA (never a silently-wrong value)
# when it can't be sure -- the caller then flags the NA. Covered:
#   thousands : 1,234.56  /  1 234.56  /  1'234.56
#   decimals  : 1,234.56 (US)  and  1.234,56 (European comma) via last-separator
#   negatives : -123.45  /  (123.45)  /  123.45-  /  trailing DR or OD ; CR = +ve
#   currency  : $ £ € and any other symbol/letters stripped
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
.num_one <- function(raw, decimal = "auto") {
  if (is.na(raw)) return(NA_real_)
  raw <- trimws(as.character(raw))
  if (!nzchar(raw)) return(NA_real_)
  neg <- FALSE
  up <- toupper(raw)
  if (grepl("(DR|OD)\\s*$", up)) neg <- TRUE        # debit / overdrawn balance
  else if (grepl("CR\\s*$", up)) neg <- FALSE       # explicit credit -> positive
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
.num <- function(s, decimal = "auto")
  vapply(as.character(s), .num_one, numeric(1), decimal = decimal, USE.NAMES = FALSE)

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
    suf <- toupper(sub(".*?([A-Za-z]{2})\\s*$", "\\1", trimws(raw)))
    mag <- .num(sub("(?i)\\s*(DR|CR)\\s*$", "", trimws(raw), perl = TRUE), dec)
    sign <- ifelse(suf == "DR", -1, ifelse(suf == "CR", 1, NA_real_))
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
    sgn[grepl("CR\\s*$", up)] <- -base             # a CR payment is the opposite of a charge
    value <- ifelse(is.na(mag), NA_real_, sgn * mag)
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "type_dc") {
    raw <- as.character(x)
    mag <- abs(.num(raw, dec))
    # Exact indexing (`[[`) -- `$` partial-matches `type` to `type_debit_value`
    # when the type column is unmapped, which would flip every row to debit.
    tv  <- as.character(opts[["type"]] %||% rep(NA_character_, length(raw)))
    debit_val <- as.character(opts[["type_debit_value"]] %||% "D")
    is_debit <- !is.na(tv) & tv == debit_val
    value <- ifelse(is_debit, -mag, mag)
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  stop(sprintf("parse_amount: unknown style '%s'", style))
}

# clean_description(x) -- VERBATIM. Only trim outer whitespace. Never strip
# apostrophes, ampersands, unicode, or any interior character.
clean_description <- function(x) {
  trimws(as.character(x))
}
