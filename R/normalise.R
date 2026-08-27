# normalise.R -- deterministic field normalisation: parse_date / parse_amount / clean_description.

# Fold the human spellings of a date onto the form strptime expects. For PARSING and DETECTION only -
# the raw cell stays verbatim. Shared by parse_date and detect_date_format so they cannot disagree.
.normalise_date_str <- function(s) {
  s <- trimws(as.character(s))
  s <- gsub("^(mon|tue|wed|thu|fri|sat|sun)[a-z]*\\.?\\s+", "", s, perl = TRUE, ignore.case = TRUE)
  s <- gsub("(?<=[0-9])(st|nd|rd|th)\\b", "", s, perl = TRUE, ignore.case = TRUE)
  s <- gsub("\\bof\\b", "", s, perl = TRUE, ignore.case = TRUE)
  s <- gsub("\\bSept\\b", "Sep", s, ignore.case = TRUE)
  # Ordinal day suffixes: "17th Sep" -> "17 Sep".
  s <- gsub("\\b([0-9]{1,2})(st|nd|rd|th)\\b", "\\1", s, ignore.case = TRUE)
  trimws(gsub("\\s+", " ", s))
}

# Fold a date onto one comparable form so .date_strict's round-trip never FALSE-rejects on a cosmetic
# difference the declared format allows. A comparison aid only.
.date_canon <- function(z) {
  z <- tolower(trimws(as.character(z)))
  # Any month word to its 3-letter key, so a %b vs %B width difference never mismatches.
  z <- gsub("\\b(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*", "\\1",
            z, perl = TRUE)
  z <- gsub("(?<![0-9])0+([0-9])", "\\1", z, perl = TRUE)  # strip leading zeros in numbers
  # A digit run together with a month NAME ("20Apr") is separated on BOTH sides. OCR reads a tightly-set
  # date column as ONE token - 86% of day+month tokens on a real ANZ scan - and the round-trip reformats
  # it with a space, so without this the date failed closed and 41 rows survived out of 300. Symmetric,
  # so it adds no parse leniency.
  z <- gsub("(?<=[0-9])(?=(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec))", " ", z, perl = TRUE)
  z <- gsub("(?<=(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec))(?=[0-9])", " ", z, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", z))
}

# Parse already-normalised date strings under `fmt`, returning ISO ONLY when the parse is trustworthy.
# Base as.Date() reads "13/08/2025" under "%d/%m/%y" as 2020, silently ignoring the trailing "25". Two
# guards close that - a ROUND-TRIP through the same fmt, and a YEAR BOUND - each catching the other's miss.
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

# -> list(iso, raw). `iso` is NA when unparseable or untrustworthy; `raw` is verbatim. Never wrong.
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

# Parse a money string to numeric, returning NA rather than a silently-wrong value. Handles thousands
# (comma, space, apostrophe), US and European decimals by the last-separator rule, and negatives written
# as -123.45 / (123.45) / 123.45- / trailing DR or OD (CR is positive). `decimal` fixes how a LONE
# separator is read, so nothing is guessed.
.num_one <- function(raw, decimal = "auto",
                     debit_rx = "(DR|OD)\\s*$", credit_rx = "CR\\s*$") {
  if (is.na(raw)) return(NA_real_)
  raw <- trimws(as.character(raw))
  if (!nzchar(raw)) return(NA_real_)
  neg <- FALSE
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
    # auto + both separators: the LAST separator is the decimal one.
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

# Sign to "debit" (<0) / "credit" (>0) / NA (0 or NA).
.direction <- function(v) {
  ifelse(is.na(v), NA_character_,
    ifelse(v < 0, "debit", ifelse(v > 0, "credit", NA_character_)))
}

# -> list(value, direction, raw). Styles: signed | debit_credit_cols | dr_cr_suffix | type_dc.
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
    # If both columns are blank for a row, value is unknown, not zero.
    both_blank <- is.na(dv) & is.na(cv)
    value[both_blank] <- NA_real_
    raw <- ifelse(!is.na(cv) & cv != 0, as.character(cr),
            ifelse(!is.na(dv) & dv != 0, as.character(deb),
              paste0(as.character(deb %||% ""), "|", as.character(cr %||% ""))))
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "dr_cr_suffix") {
    raw <- as.character(x)
    # Debit and credit suffix markers come from the lexicon.
    dset <- toupper(lex("dr_cr_suffix_debit")); cset <- toupper(lex("dr_cr_suffix_credit"))
    suf <- toupper(sub(".*?([A-Za-z]{2})\\s*$", "\\1", trimws(raw)))
    strip_rx <- sprintf("\\s*(%s)\\s*$", paste(c(dset, cset), collapse = "|"))
    # The SUFFIX is the sole source of sign here, so read the magnitude UNSIGNED: .num already makes
    # "(500.00)" negative, which a DR suffix would flip again into a wrong +500.
    mag <- abs(.num(sub(strip_rx, "", trimws(raw), perl = TRUE, ignore.case = TRUE), dec))
    sign <- ifelse(suf %in% dset, -1, ifelse(suf %in% cset, 1, NA_real_))
    value <- mag * sign
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "unsigned") {
    # Credit-card style: one column of UNSIGNED magnitudes, an unmarked amount is a CHARGE and a trailing
    # CR a PAYMENT. `unsigned_default` sets the charge's sign and CR always flips relative to it.
    raw <- as.character(x)
    mag <- abs(.num(raw, dec))
    base <- if (identical(opts[["unsigned_default"]] %||% "debit", "credit")) 1 else -1
    up <- toupper(trimws(raw))
    sgn <- rep(base, length(mag))
    # The payment marker comes from the LEXICON, exactly as dr_cr_suffix reads it. Hardcoding "CR" meant
    # a bank whose payment marker is written otherwise had every payment read with the CHARGE sign.
    credit_rx <- sprintf("(%s)\\s*$", paste(toupper(lex("dr_cr_suffix_credit")), collapse = "|"))
    sgn[grepl(credit_rx, up)] <- -base             # a CR payment is the opposite of a charge
    value <- ifelse(is.na(mag), NA_real_, sgn * mag)
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  if (style == "type_dc") {
    raw <- as.character(x)
    mag <- abs(.num(raw, dec))
    # Compare the indicator case- and whitespace-insensitively: a statement may print "D" / "d" / "Debit"
    # / " DR ", and a case-sensitive match silently flips every debit to a credit. Exact `[[` indexing
    # avoids partial-matching `type` onto `type_debit_value`.
    tv   <- toupper(trimws(as.character(opts[["type"]] %||% rep(NA_character_, length(raw)))))
    dval <- toupper(trimws(as.character(opts[["type_debit_value"]] %||% "D")))
    cval <- opts[["type_credit_value"]]
    cval <- if (is.null(cval)) NA_character_ else toupper(trimws(as.character(cval)))
    is_debit <- !is.na(tv) & nzchar(tv) & tv == dval
    if (!is.na(cval) && nzchar(cval)) {
      # A credit token is declared, so a value matching NEITHER is genuinely ambiguous: fail CLOSED.
      is_credit <- !is.na(tv) & nzchar(tv) & tv == cval
      value <- ifelse(is_debit, -mag, ifelse(is_credit, mag, NA_real_))
    } else {
      # Back-compat with no credit token declared: anything not the debit token is a credit.
      value <- ifelse(is_debit, -mag, mag)
    }
    return(list(value = value, direction = .direction(value), raw = raw))
  }

  stop(sprintf("parse_amount: unknown style '%s'", style))
}

# VERBATIM. Only trim outer whitespace - never strip apostrophes, ampersands, unicode or anything inside.
clean_description <- function(x) {
  trimws(as.character(x))
}
