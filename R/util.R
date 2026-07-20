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

# file_sha256(path) -- deterministic content hash. Uses openssl when available,
# otherwise digest, otherwise tools::md5sum as a last-resort fallback.
file_sha256 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (requireNamespace("openssl", quietly = TRUE)) {
    return(paste0(openssl::sha256(file(path, open = "rb"))))
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

# safe_readlines(path) -- read text lines without warnings/crashes.
safe_readlines <- function(path) {
  safe(readLines(path, warn = FALSE, encoding = "UTF-8"), character(0))
}
