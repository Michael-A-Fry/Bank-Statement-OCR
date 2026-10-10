# bank_memory.R -- the bank remembered for an account, or for a file name seen
# before, so the Convert table can fill in the bank box next time.
#
# NEVER THE NUMBER. An account is kept only as a salted mark (bank_identity.R,
# .bi_mark): a hash of the install's own secret and the number, cut short. The salt
# lives in its own file beside the memory, made once per install, so a copy of the
# memory alone says nothing about any account. A file name is kept as its PATTERN:
# lower-case, digits and month names replaced ("anz_march_2024.pdf" and
# "ANZ_April_2024.pdf" are both "anz_<m>_#"), which holds no account number either.
#
# The memory only ever SUGGESTS: it fills the bank box as a choice a person can
# change, and the statement's own evidence still wins (the bank check on Convert).

.BM_MONTHS <- "(jan(uary)?|feb(ruary)?|mar(ch)?|apr(il)?|may|june?|july?|aug(ust)?|sep(t(ember)?)?|oct(ober)?|nov(ember)?|dec(ember)?)"

# bank_memory_salt(dir) -- this install's salt, made on first use. "" when the
# folder cannot be written (and then nothing is remembered by account).
bank_memory_salt <- function(dir) {
  if (is.null(dir) || !nzchar(dir %||% "")) return("")
  f <- file.path(dir, "bank_memory.salt")
  s <- if (file.exists(f)) tryCatch(trimws(readLines(f, n = 1L, warn = FALSE)), error = function(e) "") else ""
  if (length(s) == 1L && nchar(s) >= 32L) return(s)
  s <- paste(sprintf("%02x", sample.int(256L, 32L, replace = TRUE) - 1L), collapse = "")
  ok <- tryCatch({ dir.create(dir, showWarnings = FALSE, recursive = TRUE); writeLines(s, f); TRUE },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (isTRUE(ok)) s else ""
}

# bank_name_pattern(name) -- a file name as the pattern remembered for it.
bank_name_pattern <- function(name) {
  x <- tolower(tools::file_path_sans_ext(basename(as.character(name %||% "")[1])))
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  x <- gsub(paste0("(?<![a-z])", .BM_MONTHS, "(?![a-z])"), "<m>", x, perl = TRUE)
  x <- gsub("[0-9]+", "#", x)
  x <- gsub("[^a-z#<>]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  # a name that is nothing but a date or a number says nothing about a bank
  if (!grepl("[a-z]{2,}", gsub("<m>", "", x, fixed = TRUE))) return(NA_character_)
  x
}

.bm_file <- function(dir) file.path(dir, "bank_memory.json")
bank_memory_load <- function(dir) {
  f <- .bm_file(dir)
  m <- if (file.exists(f)) tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL) else NULL
  if (!is.list(m)) m <- list()
  list(accounts = as.list(m$accounts %||% list()), names = as.list(m$names %||% list()))
}

# bank_memory_note(dir, name, mark, bank) -- remember `bank` for this account mark
# and this file-name pattern. Quietly does nothing it cannot do.
bank_memory_note <- function(dir, name = NA, mark = NA, bank = NA) {
  bank <- as.character(bank %||% NA)[1]
  if (is.null(dir) || is.na(bank) || !nzchar(bank)) return(invisible(FALSE))
  m <- bank_memory_load(dir)
  mk <- as.character(mark %||% NA)[1]
  if (!is.na(mk) && grepl("^[0-9a-f]{16,64}$", mk)) m$accounts[[mk]] <- bank
  pt <- bank_name_pattern(name)
  if (!is.na(pt)) m$names[[pt]] <- bank
  # bounded: the newest 5000 of each
  if (length(m$accounts) > 5000L) m$accounts <- utils::tail(m$accounts, 5000L)
  if (length(m$names) > 5000L) m$names <- utils::tail(m$names, 5000L)
  f <- .bm_file(dir); tmp <- paste0(f, ".tmp")
  ok <- tryCatch({
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    writeLines(jsonlite::toJSON(m, auto_unbox = TRUE), tmp); file.rename(tmp, f)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  invisible(isTRUE(ok))
}

# bank_memory_recall(dir, name, mark) -> list(bank, why) or NULL. The account
# comes first: it is the statement's own, where the name is only what it was saved as.
bank_memory_recall <- function(dir, name = NA, mark = NA, memory = NULL) {
  if (is.null(dir)) return(NULL)
  m <- memory %||% bank_memory_load(dir)
  mk <- as.character(mark %||% NA)[1]
  if (!is.na(mk) && !is.null(m$accounts[[mk]]))
    return(list(bank = as.character(m$accounts[[mk]])[1], why = "This account's bank, remembered from last time."))
  pt <- bank_name_pattern(name)
  if (!is.na(pt) && !is.null(m$names[[pt]]))
    return(list(bank = as.character(m$names[[pt]])[1], why = "The bank used last time for a file named like this."))
  NULL
}
