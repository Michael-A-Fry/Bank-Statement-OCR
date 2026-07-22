# util.R -- small shared helpers: safe IO, hashing, status/message objects.

# Null-coalescing: return `a` unless it is NULL (or length 0), else `b`.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}

# is_blank(x) -- TRUE for NA or whitespace-only strings.
is_blank <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

# blank_to_na(x) -- collapse "" / whitespace to NA, otherwise trimmed value.
blank_to_na <- function(x) {
  x <- as.character(x)
  out <- trimws(x)
  out[is_blank(out)] <- NA_character_
  out
}

# locate_header(lines, template) -- single source of truth for finding the
# header row of a delimited statement (used by BOTH detection and the reader so
# they can never disagree). Honours an optional preamble.header_regex; otherwise
# the first non-empty line. Agreed no-match behaviour: when a preamble regex is
# supplied but matches no line, return NA_integer_ (no header found) so callers
# treat the input as unrecognised rather than guessing line 1.
locate_header <- function(lines, template) {
  if (length(lines) == 0) return(NA_integer_)
  hr <- template$preamble$header_regex
  if (!is.null(hr) && nzchar(hr)) {
    m <- grep(hr, lines, perl = TRUE)
    return(if (length(m)) m[1] else NA_integer_)
  }
  nz <- which(nzchar(trimws(lines)))
  if (length(nz)) nz[1] else NA_integer_
}

# file_sha256(path) -- deterministic content hash. Uses openssl when available,
# otherwise digest, otherwise tools::md5sum as a last-resort fallback.
file_sha256 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb"); on.exit(close(con), add = TRUE)  # MUST close, or
    return(paste0(openssl::sha256(con)))                             # connections leak
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(file = path, algo = "sha256"))
  }
  unname(tools::md5sum(path))
}

# status_message(status, why, needs) -- build an actionable status message
# ("why" + "what it needs"), per build-contract section 7.
status_message <- function(status, why, needs = NULL) {
  msg <- paste0(status, ": ", why)
  if (!is.null(needs) && nzchar(needs)) msg <- paste0(msg, "; ", needs)
  msg
}

# safe(expr, default) -- evaluate `expr`, returning `default` on any error.
safe <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) default)
}

# current_user() -- the OS-authenticated logged-in user (Windows %USERNAME%,
# else $USER/$LOGNAME). This is how the tool records WHO ran a conversion
# without any password, prompt, or login screen: the person is already
# authenticated by Windows at sign-in, and only members of the authorised AD
# group can reach the tool at all (folder permissions), so this name is
# trustworthy. It is only ever stored as a string -- never used in a query or
# evaluated -- so there is no injection surface.
current_user <- function() {
  for (v in c("USERNAME", "USER", "LOGNAME")) {
    u <- Sys.getenv(v)
    if (nzchar(u)) return(u)
  }
  "unknown"
}

# .transcode_lines(raw_bytes, from) -- convert raw file bytes of a known encoding
# to UTF-8 and split into lines. sub="byte" keeps any stray undefined byte VISIBLE
# (as \xNN) rather than silently dropping content.
.transcode_lines <- function(raw_bytes, from) {
  s <- safe(iconv(list(raw_bytes), from = from, to = "UTF-8", sub = "byte"), NA_character_)
  if (length(s) != 1L || is.na(s)) return(character(0))
  s <- gsub("\r\n", "\n", s, fixed = TRUE); s <- gsub("\r", "\n", s, fixed = TRUE)
  strsplit(s, "\n", fixed = TRUE)[[1]]
}

# safe_readlines(path, encoding) -- read text lines without warnings/crashes, and
# WITHOUT corrupting non-UTF-8 input. readLines(encoding="UTF-8") only TAGS bytes
# as UTF-8, it does not transcode; a Windows-1252 / Latin-1 bank export (a payee
# with a £, é, or non-breaking space) then flows in as mojibake / invalid UTF-8,
# breaking the verbatim-description guarantee, and a UTF-16 file garbles entirely.
# So: sniff the byte-order mark, else validate UTF-8 and fall back to the dominant
# 8-bit codepage -- all deterministic (`encoding` overrides the sniff when known).
#   * UTF-8 BOM (Excel writes one on every CSV export) is stripped -- left in it
#     corrupts the first header name ("﻿Date" is not "Date").
#   * UTF-16 LE/BE BOM -> transcoded (readLines would garble it).
#   * no BOM + valid UTF-8 -> read as UTF-8 (the common path, unchanged).
#   * no BOM + invalid UTF-8 -> Windows-1252 (superset of Latin-1; every byte maps,
#     nothing is silently dropped), which covers the real NZ/UK bank exports.
safe_readlines <- function(path, encoding = NULL) {
  size <- safe(file.info(path)$size, NA_real_)
  raw <- safe(readBin(path, "raw", n = if (is.na(size)) 0L else as.integer(size)), raw(0))
  if (!length(raw)) return(character(0))
  b <- as.integer(raw[seq_len(min(3L, length(raw)))])
  # UTF-16 byte-order marks: must be transcoded, never read as UTF-8/8-bit.
  if (length(b) >= 2 && b[1] == 0xFF && b[2] == 0xFE) return(.transcode_lines(raw[-(1:2)], "UTF-16LE"))
  if (length(b) >= 2 && b[1] == 0xFE && b[2] == 0xFF) return(.transcode_lines(raw[-(1:2)], "UTF-16BE"))
  had_utf8_bom <- length(b) >= 3 && b[1] == 0xEF && b[2] == 0xBB && b[3] == 0xBF
  body <- if (had_utf8_bom) raw[-(1:3)] else raw
  # An explicitly declared non-UTF-8 encoding wins over the sniff.
  if (!is.null(encoding) && nzchar(encoding) && !toupper(encoding) %in% c("UTF-8", "UTF8"))
    return(.transcode_lines(body, encoding))
  s <- safe(rawToChar(body), NA_character_)
  if (!is.na(s) && validUTF8(s)) {
    # COMMON PATH -- valid UTF-8: read exactly as before (readLines + BOM strip).
    lines <- safe(readLines(path, warn = FALSE, encoding = "UTF-8"), character(0))
    if (length(lines)) lines[1] <- sub("^\xef\xbb\xbf", "", lines[1], useBytes = TRUE)
    return(lines)
  }
  # No BOM, not valid UTF-8, no declared encoding -> Windows-1252 fallback.
  .transcode_lines(body, "WINDOWS-1252")
}
