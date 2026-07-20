# normalise.R -- deterministic field normalisation.
# parse_date / parse_amount / clean_description. No locale guessing, no ML.

# parse_date(x, fmt) -> list(iso, raw)
# `iso` is YYYY-MM-DD (NA when unparseable); `raw` is the input verbatim.
parse_date <- function(x, fmt) {
  raw <- as.character(x)
  iso <- rep(NA_character_, length(raw))
  ok <- !is.na(raw) & nzchar(trimws(raw))
  if (any(ok)) {
    d <- suppressWarnings(as.Date(trimws(raw[ok]), format = fmt))
    iso[ok] <- format(d, "%Y-%m-%d")  # format(NA) -> NA
  }
  list(iso = iso, raw = raw)
}

# Internal: strip thousands separators / currency symbols / spaces -> numeric.
.num <- function(s) {
  s <- trimws(as.character(s))
  s <- gsub("[,$]", "", s)
  s <- gsub("[[:space:]]", "", s)
  s[!nzchar(s)] <- NA_character_
  suppressWarnings(as.numeric(s))
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
