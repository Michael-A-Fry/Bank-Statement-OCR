# normalise.R -- deterministic field normalisation.
# parse_date / parse_amount / clean_description. No locale guessing, no ML.

# parse_date(x, fmt) -> list(iso, raw)
# `iso` is YYYY-MM-DD (NA when unparseable); `raw` is the input verbatim.
parse_date <- function(x, fmt) {
  raw <- as.character(x)
  iso <- rep(NA_character_, length(raw))
  ok <- !is.na(raw) & nzchar(trimws(raw))
  if (any(ok)) {
    s <- trimws(raw[ok])
    # Normalise for PARSING ONLY (raw is kept verbatim): drop ordinal suffixes
    # ("1st April" -> "1 April", "21st" -> "21") and fold the 4-letter "Sept"
    # to the 3-letter "Sep" that %b expects. "September" (%B) is untouched -- the
    # word boundary after "Sept" fails inside it.
    s <- gsub("(?<=[0-9])(st|nd|rd|th)\\b", "", s, perl = TRUE, ignore.case = TRUE)
    s <- gsub("\\bSept\\b", "Sep", s, ignore.case = TRUE)
    d <- suppressWarnings(as.Date(s, format = fmt))
    iso[ok] <- format(d, "%Y-%m-%d")  # format(NA) -> NA
  }
  list(iso = iso, raw = raw)
}

# .num(s) -- parse a money string to numeric, ROBUSTLY. Handles the real formats
# that appear on statements, and returns NA (never a silently-wrong value) when
# it can't be sure -- the caller then flags the NA. Covered:
#   thousands : 1,234.56  /  1 234.56  /  1'234.56
#   decimals  : 1,234.56 (US)  and  1.234,56 (European comma) via last-separator
#   negatives : -123.45  /  (123.45)  /  123.45-  /  trailing DR or OD ; CR = +ve
#   currency  : $ £ € and any other symbol/letters stripped
.num_one <- function(raw) {
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
  if (hasdot && hascomma) {
    # the LAST separator is the decimal one
    if (max(gregexpr(",", s)[[1]]) > max(gregexpr("\\.", s)[[1]])) {
      s <- gsub("\\.", "", s); s <- sub(",", ".", s)  # European: . thousands, , decimal
    } else s <- gsub(",", "", s)                       # US/UK: , thousands
  } else if (hascomma) {
    parts <- strsplit(s, ",", fixed = TRUE)[[1]]
    if (length(parts) == 2 && nchar(parts[2]) %in% c(1L, 2L))
      s <- sub(",", ".", s)                            # decimal comma "1234,56"
    else s <- gsub(",", "", s)                          # thousands "1,234"
  }
  v <- suppressWarnings(as.numeric(s))
  if (is.na(v)) return(NA_real_)
  if (neg) -abs(v) else v
}
.num <- function(s) vapply(as.character(s), .num_one, numeric(1), USE.NAMES = FALSE)

# .direction(v) -- sign -> "debit" (<0) / "credit" (>0) / NA (0 or NA).
.direction <- function(v) {
  ifelse(is.na(v), NA_character_,
    ifelse(v < 0, "debit", ifelse(v > 0, "credit", NA_character_)))
}

# parse_amount(x, style, opts) -> list(value, direction, raw)
# Styles: signed | debit_credit_cols | dr_cr_suffix | type_dc.
parse_amount <- function(x, style = "signed", opts = list()) {
  style <- style %||% "signed"

  if (style == "signed") {
    raw <- as.character(x)
    value <- .num(raw)
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "debit_credit_cols") {
    deb <- opts[["debit"]]
    cr  <- opts[["credit"]]
    dv <- .num(deb); cv <- .num(cr)
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
    mag <- .num(sub("(?i)\\s*(DR|CR)\\s*$", "", trimws(raw), perl = TRUE))
    sign <- ifelse(suf == "DR", -1, ifelse(suf == "CR", 1, NA_real_))
    value <- mag * sign
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "type_dc") {
    raw <- as.character(x)
    mag <- abs(.num(raw))
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
