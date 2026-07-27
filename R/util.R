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

# with_image_scratch(expr) -- run a piece of image work with ImageMagick's
# spill files confined to a private folder that is deleted straight afterwards.
#
# WHY: when a page is too big to hold in memory, ImageMagick stops using RAM and
# writes the raw pixels to a temporary file instead (measured at ~83 MB for ONE
# 300 dpi A4 statement page). It only tidies those away when the PROCESS exits --
# and this server is meant to run for months without a restart, so in practice
# never. They are raw pixels of a CLIENT'S BANK STATEMENT sitting on the system
# drive. Giving each call its own folder means the sweep below is guaranteed to
# find every spill file, and can never delete another program's.
#
# HOW TO USE IT: put the WHOLE piece of image work inside the braces --
# everything that reads a magick image, right through to the plain R value you
# want out of it. Once this returns, the scratch folder is gone, so a magick
# image left over from inside must not be read again.
#
#   dark <- with_image_scratch({
#     img <- magick::image_read(page_png)
#     as.integer(magick::image_data(img, channels = "gray"))   # plain R value
#   })
with_image_scratch <- function(expr) {
  scratch <- tempfile("imgscratch_")
  if (dir.create(scratch, showWarnings = FALSE)) {
    previous <- Sys.getenv("MAGICK_TMPDIR", unset = NA_character_)
    Sys.setenv(MAGICK_TMPDIR = scratch)
    on.exit({
      if (is.na(previous)) Sys.unsetenv("MAGICK_TMPDIR")
      else Sys.setenv(MAGICK_TMPDIR = previous)
      unlink(scratch, recursive = TRUE, force = TRUE)
    }, add = TRUE)
  }
  # `expr` is only evaluated HERE, on this last line -- after MAGICK_TMPDIR has
  # been pointed at the scratch folder and before on.exit() removes it. (That is
  # R's normal "arguments are evaluated when first used" behaviour.)
  expr
}

# ---------------------------------------------------------------------------
# yaml_append_phrase(path, key, value, under) -- teach a hand-maintained YAML
# dictionary ONE new wording, WITHOUT rewriting the file.
#
# WHY it edits text instead of dumping the parsed object back:
# dictionaries/lexicon.yaml and dictionaries/labels.yaml are the maintainer's
# documentation as much as they are data -- a header explaining how each category
# merges, a worked cow/horse example, and per-line notes ("# credit-card
# wording"). `yaml::write_yaml` over the top deletes every one of them the first
# time anybody approves a word, and the analyst who inherits the file never knows
# what it used to say. So: find the list, insert one item, and only write if the
# result still parses AND really contains the new value. Everything else in the
# file stays byte-identical.
#
# Handles the two shapes these dictionaries actually use, block and flow:
#   key:                     |  key: ["a", "b"]        (the flow form may run
#     - "a"                  |                          over several lines)
# `under` adds the one nesting level labels.yaml uses (field -> any_of -> list).
#
# Returns TRUE/FALSE with attr "reason" -- one plain sentence the UI can show as
# is, so the screen and the engine cannot disagree about why an add failed.
yaml_append_phrase <- function(path, key, value, under = NULL) {
  fail <- function(why) structure(FALSE, reason = why)
  key   <- trimws(as.character(key %||% "")[1])
  value <- trimws(as.character(value %||% "")[1])
  under <- if (is.null(under)) NULL else trimws(as.character(under)[1])
  if (is.na(key) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", key))
    return(fail("that is not a usable name for a dictionary entry"))
  # Quotes and backslashes are the two characters that could change the SHAPE of
  # the file rather than add to it, so they are refused rather than escaped.
  if (is.na(value) || !nzchar(value) || grepl("[\r\n\"\\\\]", value))
    return(fail("type the wording first - one line, without quote marks"))
  path <- as.character(path %||% "")[1]
  if (is.na(path) || !nzchar(path)) return(fail("no dictionary file is set"))

  existed <- file.exists(path)
  lines <- if (existed) safe_readlines(path) else character(0)
  before <- if (existed) safe(yaml::read_yaml(path), NA) else list()
  # Fail closed on a file we cannot read: appending to a broken dictionary would
  # either lose it or hide the breakage.
  if (identical(before, NA))
    return(fail("the dictionary file does not parse as YAML - fix that before adding to it"))
  if (!is.list(before)) before <- list()
  if (any(tolower(.yaml_phrase_list(before, key, under)) == tolower(value)))
    return(structure(TRUE, reason = "it already knew that wording", added = FALSE))

  out <- .yaml_insert_item(lines, key, value, under)
  if (is.null(out)) return(fail("could not find where to add it - edit the file directly"))
  after <- safe(yaml::yaml.load(paste(out, collapse = "\n")), NA)
  if (identical(after, NA) || !is.list(after) ||
      !any(tolower(.yaml_phrase_list(after, key, under)) == tolower(value)))
    return(fail("that edit would not have come out as valid YAML, so nothing was changed"))
  if (existed) safe(file.copy(path, paste0(path, ".bak"), overwrite = TRUE))
  ok <- isTRUE(tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(out, path); TRUE }, error = function(e) FALSE))
  if (!ok) return(fail("could not write the file - check the folder permissions"))
  structure(TRUE, reason = "added", added = TRUE)
}

# .yaml_phrase_list(obj, key, under) -- the wordings already recorded under a key.
.yaml_phrase_list <- function(obj, key, under = NULL) {
  v <- if (is.list(obj)) obj[[key]] else NULL
  if (!is.null(under)) v <- if (is.list(v)) v[[under]] else NULL
  as.character(unlist(v %||% character(0)))
}

# .yaml_block_end(lines, k) -- the last line that belongs to the top-level block
# opened at line k (i.e. up to the next line that starts in column 0).
.yaml_block_end <- function(lines, k) {
  if (k >= length(lines)) return(length(lines))
  nxt <- which(grepl("^[^#[:space:]]", lines[(k + 1L):length(lines)]))
  if (length(nxt)) k + nxt[1] - 1L else length(lines)
}

# .yaml_insert_item(lines, key, value, under) -- the text edit itself, or NULL if
# there is no safe place to put it.
.yaml_insert_item <- function(lines, key, value, under = NULL) {
  item <- sprintf("\"%s\"", value)
  at <- grep(sprintf("^%s[[:space:]]*:", key), lines)
  if (!length(at)) {                     # a wording for something not listed yet
    block <- if (is.null(under)) c(sprintf("%s:", key), sprintf("  - %s", item))
             else c(sprintf("%s:", key), sprintf("  %s:", under), sprintf("    - %s", item))
    pad <- if (length(lines) && nzchar(utils::tail(lines, 1))) "" else NULL
    return(c(lines, pad, block))
  }
  k <- at[1]; end <- .yaml_block_end(lines, k)
  if (!is.null(under)) {
    u <- grep(sprintf("^[[:space:]]+%s[[:space:]]*:", under), lines[k:end])
    if (!length(u))                      # the key exists but has no such list yet
      return(append(lines, c(sprintf("  %s:", under), sprintf("    - %s", item)), after = k))
    k <- k + u[1] - 1L
  }
  hdr <- lines[k]
  if (grepl(":[[:space:]]*\\[", hdr)) {                       # flow: ["a", "b"]
    close <- NA_integer_
    for (i in k:end) if (grepl("]", lines[i], fixed = TRUE)) { close <- i; break }
    if (is.na(close)) return(NULL)
    p <- regexpr("]", lines[close], fixed = TRUE)
    lines[close] <- paste0(substr(lines[close], 1L, p - 1L), ", ", item,
                           substr(lines[close], p, nchar(lines[close])))
    return(lines)
  }
  items <- grep("^[[:space:]]*-[[:space:]]", lines[k:end])     # block: - "a"
  if (length(items)) {
    last <- k + utils::tail(items, 1) - 1L
    return(append(lines, paste0(sub("^([[:space:]]*)-.*$", "\\1", lines[last]), "- ", item),
                  after = last))
  }
  append(lines, paste0(sub("^([[:space:]]*).*$", "\\1", hdr), "  - ", item), after = k)
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

# fingerprint_phrases(text) -> the identifying phrases typed in the toolkit's
# "a distinctive phrase printed on this statement" box, one per line.
#
# Trimmed, blanks dropped, order kept, duplicates dropped. A fingerprint phrase is
# matched against the page text CHARACTER FOR CHARACTER, so a stray trailing space
# or a blank line is the difference between a template that recognises the bank and
# one that never matches anything -- which is precisely the failure the analyst
# cannot debug. Pure, and in R/ rather than app.R, so the suite can hold it to that.
fingerprint_phrases <- function(text) {
  s <- as.character(text %||% "")[1]
  if (is.na(s)) return(character(0))
  ph <- trimws(strsplit(s, "\n", fixed = TRUE)[[1]])
  unique(ph[!is.na(ph) & nzchar(ph)])
}

# recognition_summary(det, saved_id) -> list(ok, headline, detail).
#
# Turns a REAL detection result into the one sentence the analyst needs after
# saving a template: "will this statement be recognised on its own next time?".
#
# WHY it matters: after a save the app re-converts with the new template FORCED by
# id, which short-circuits detection entirely -- so the one moment the app could
# prove the fingerprint works was the one moment it never tested it, and templates
# whose fingerprint could never match shipped looking fine. The caller now runs a
# genuine detect_statement() over the whole template set and passes the result
# here. `ok`: TRUE = recognised as this template, FALSE = it will not be, NA =
# the check itself could not run (say so, never imply a pass).
#
# It lives here, not in app.R, because app.R is not sourced by the test suite --
# a pure helper in R/ is a helper the suite can actually hold to its word.
recognition_summary <- function(det, saved_id) {
  sid <- trimws(as.character(saved_id %||% NA_character_)[1])
  if (is.null(det) || !is.list(det))
    return(list(ok = NA,
      headline = "Saved - but we could not check whether this statement will be recognised next time.",
      detail = paste("Upload it again on Convert to find out. If it comes back as",
                     "\"no template for this statement yet\", open the toolkit and set an",
                     "identifying phrase that is really printed on the page.")))
  got <- trimws(as.character(det$template_id %||% NA_character_)[1])
  if (isTRUE(det$matched) && !is.na(got) && identical(got, sid))
    return(list(ok = TRUE,
      headline = "Next time, a statement like this one is recognised automatically.",
      detail = sprintf("We re-checked it against every template and it matched yours (\"%s\") on its own, with nothing forced.", sid)))
  if (isTRUE(det$matched))
    return(list(ok = FALSE,
      headline = sprintf("Careful: this statement is still recognised as \"%s\", not your new template.", got),
      detail = paste("Your template was saved, but another one wins on this file, so next",
                     "time it would be read with that other template. Open the toolkit again",
                     "and make the identifying phrase more specific to this bank - or use the",
                     "existing template if it is the right one.")))
  why <- trimws(as.character(det$detail %||% "")[1])
  # A TIE IS NOT A TYPO, and it used to be diagnosed as one: the headline said the
  # template was not recognised and the lead sentence sent the analyst back to
  # retype an identifying phrase that "is not printed on the page exactly as
  # typed". On a tie that phrase IS printed on the page -- it is printed for both
  # templates, which is the whole problem -- so she would retype something already
  # correct, and the charter forbids the other reading of that advice (a third
  # template would tie too). The real cause leads, and the cure is the one that
  # actually separates them.
  tied <- as.character(det$tied %||% character(0))
  others <- setdiff(tied, sid)
  if (length(tied) >= 2 && length(others))
    return(list(ok = FALSE,
      headline = if (length(others) == 1L)
        sprintf("Saved - but \"%s\" fits this statement exactly as well as yours.", others)
        else sprintf("Saved - but %d other templates fit this statement exactly as well as yours.",
                     length(others)),
      detail = paste0(
        "Nothing on this file tells them apart, so the tool cannot know which one you ",
        "meant. It still converts: it picks the tested template, reads the statement ",
        "and holds the run for a check, so no figures are lost. To settle it, add a ",
        "phrase to your template that ONLY this bank prints - that breaks the tie in ",
        "its favour - or retire the duplicate (",
        paste(others, collapse = ", "), "). Do not build a third: it would tie too.")))
  list(ok = FALSE,
    headline = "Saved - but this statement is NOT recognised by it yet.",
    detail = paste0(
      "Next time you upload a statement like this one it would still come back as ",
      "\"no template for this statement yet\". Almost always the identifying phrase ",
      "is not printed on the page exactly as typed. Open the toolkit again and set a ",
      "phrase you can see on the statement.",
      if (nzchar(why) && !is.na(why)) sprintf(" (The detector said: %s)", why) else ""))
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
