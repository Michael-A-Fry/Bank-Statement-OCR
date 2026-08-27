# util.R -- small shared helpers: safe IO, hashing, status/message objects.

# Null-coalescing: return `a` unless it is NULL (or length 0), else `b`.
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}

is_blank <- function(x) {
  is.na(x) | !nzchar(trimws(as.character(x)))
}

blank_to_na <- function(x) {
  x <- as.character(x)
  out <- trimws(x)
  out[is_blank(out)] <- NA_character_
  out
}

# locate_header - one rule for both detection and the reader. A preamble regex that matches no line
# returns NA (unrecognised), never a guess at line 1.
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

# Run image work with ImageMagick's spill files confined to a private folder that is deleted straight
# afterwards: magick spills raw pixels of a client statement to disk and only tidies them on process
# exit, and this server runs for months. Put the WHOLE piece of image work inside the braces.
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
  # expr is evaluated here, after MAGICK_TMPDIR points at the scratch folder.
  expr
}

# Teach a hand-maintained YAML dictionary ONE new wording by editing the text, not by re-dumping the
# parsed object: write_yaml would delete the maintainer's comments, which are half the value of these
# files. Returns TRUE/FALSE with attr "reason" - one plain sentence the UI shows as is.
yaml_append_phrase <- function(path, key, value, under = NULL) {
  fail <- function(why) structure(FALSE, reason = why)
  key   <- trimws(as.character(key %||% "")[1])
  value <- trimws(as.character(value %||% "")[1])
  under <- if (is.null(under)) NULL else trimws(as.character(under)[1])
  if (is.na(key) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", key))
    return(fail("that is not a usable name for a dictionary entry"))
  # Quotes and backslashes could change the SHAPE of the file, so they are refused not escaped.
  if (is.na(value) || !nzchar(value) || grepl("[\r\n\"\\\\]", value))
    return(fail("type the wording first - one line, without quote marks"))
  path <- as.character(path %||% "")[1]
  if (is.na(path) || !nzchar(path)) return(fail("no dictionary file is set"))

  existed <- file.exists(path)
  lines <- if (existed) safe_readlines(path) else character(0)
  before <- if (existed) safe(yaml::read_yaml(path), NA) else list()
  # Fail closed on a file we cannot read: appending to a broken dictionary loses it or hides it.
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

.yaml_phrase_list <- function(obj, key, under = NULL) {
  v <- if (is.list(obj)) obj[[key]] else NULL
  if (!is.null(under)) v <- if (is.list(v)) v[[under]] else NULL
  as.character(unlist(v %||% character(0)))
}

# The last line belonging to the top-level block opened at line k.
.yaml_block_end <- function(lines, k) {
  if (k >= length(lines)) return(length(lines))
  nxt <- which(grepl("^[^#[:space:]]", lines[(k + 1L):length(lines)]))
  if (length(nxt)) k + nxt[1] - 1L else length(lines)
}

# The text edit itself, or NULL if there is no safe place to put it.
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

# ONE CLOCK. Every timestamp WRITTEN is UTC with a Z; every timestamp SHOWN is local and names its
# zone. Mixed zones put twelve hours between an upload id and its run_id, and local stamps do not
# sort - the hour the clocks go back reverses a plain string sort of `ts`.

# utc_stamp(t) -- the timestamp to WRITE: "2026-08-26T07:31:18Z".
utc_stamp <- function(t = Sys.time()) {
  t <- safe(as.POSIXct(t), NA)
  if (length(t) != 1L || is.na(t)) return(NA_character_)
  format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# The same instant as an id fragment, so an upload id and a run_id agree by construction.
utc_id <- function(t = Sys.time()) {
  t <- safe(as.POSIXct(t), NA)
  if (length(t) != 1L || is.na(t)) return(NA_character_)
  format(t, "%Y%m%d%H%M%S", tz = "UTC")
}

# A written stamp back to an instant. Accepts the Z form and the older "+1200" form so records
# already on the box still read. NA for anything else - a stamp read in the wrong zone is a wrong
# figure that looks right.
.parse_stamp <- function(x) {
  s <- trimws(as.character(x))
  s[is.na(s) | !nzchar(s)] <- NA_character_
  s <- sub("Z$", "+0000", s)
  s <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", s)
  as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
}

# The timestamp to SHOW: local, with the zone named. Anything unparseable comes back exactly as
# given, never blank and never a guess.
local_time_text <- function(x) {
  t <- .parse_stamp(x)
  raw <- as.character(x)
  # tz = "" is THIS SERVER's zone; without it the stamp formats in the zone it was parsed in.
  out <- format(t, "%d %b %Y %H:%M %Z", tz = "")
  # %Z is empty on a few R builds, and the zone name must never be missing.
  zone <- safe(Sys.timezone(), NA_character_)
  blank <- !is.na(t) & (is.na(out) | !grepl("[A-Za-z]{2,}$", out))
  if (any(blank))
    out[blank] <- paste(format(t[blank], "%d %b %Y %H:%M"),
                        if (is.na(zone) || !nzchar(zone)) "local time" else zone)
  out[is.na(t)] <- raw[is.na(t)]
  out
}

# Write a YAML file so one bad save cannot cost months of template work: copy to <path>.bak (one
# save's undo), write a temp file in the SAME folder, rename it over <path>. A reader sees the old
# file or the new one, never a half-written one. Returns TRUE/FALSE with attr "reason".
save_yaml_safely <- function(x, path) {
  path <- as.character(path %||% "")[1]
  if (is.na(path) || !nzchar(path))
    return(structure(FALSE, reason = "no file name was given to save to"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path) && !isTRUE(safe(file.copy(path, paste0(path, ".bak"),
                                                  overwrite = TRUE), FALSE)))
    return(structure(FALSE, reason = "could not keep a copy of the previous version, so nothing was changed"))
  # The temp and backup suffixes are ones no template loader lists, so neither can be loaded.
  tmp <- paste0(path, ".", Sys.getpid(), ".part")
  # Warnings are caught as well as errors: on a read-only share file() WARNS and the write returns
  # normally, so an error-only guard reports a failed save as a success.
  warned <- FALSE
  ok <- isTRUE(withCallingHandlers(
    tryCatch({ yaml::write_yaml(x, tmp); TRUE }, error = function(e) FALSE),
    warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }))
  if (warned) ok <- FALSE                 # a warned-about write did not happen
  if (!ok || !file.exists(tmp)) {
    safe(unlink(tmp))
    return(structure(FALSE, reason = "could not write the file - check the folder permissions"))
  }
  if (!isTRUE(safe(file.rename(tmp, path), FALSE))) {
    safe(unlink(tmp))
    return(structure(FALSE, reason = "could not put the new version in place, so the previous one is still there"))
  }
  structure(TRUE, reason = "saved")
}

status_message <- function(status, why, needs = NULL) {
  msg <- paste0(status, ": ", why)
  if (!is.null(needs) && nzchar(needs)) msg <- paste0(msg, "; ", needs)
  msg
}

# safe(expr, default) -- evaluate `expr`, returning `default` on any error.
safe <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) default)
}

# The identifying phrases typed in the toolkit, one per line. Matched against page text CHARACTER FOR
# CHARACTER, so a stray trailing space is the difference between a template that recognises the bank
# and one that never matches anything.
fingerprint_phrases <- function(text) {
  s <- as.character(text %||% "")[1]
  if (is.na(s)) return(character(0))
  ph <- trimws(strsplit(s, "\n", fixed = TRUE)[[1]])
  unique(ph[!is.na(ph) & nzchar(ph)])
}

# A template said in words the person holding the statement can check, never its id.
.saved_name <- function(id, templates = NULL) {
  id <- trimws(as.character(id %||% NA_character_)[1])
  if (is.na(id) || !nzchar(id)) return(id)
  t <- if (is.list(templates) && !is.null(templates[[id]])) templates[[id]] else NULL
  nm <- if (is.null(t)) NA_character_ else safe(template_display_name(t), NA_character_)
  nm <- as.character(nm %||% NA_character_)[1]
  if (!is.na(nm) && nzchar(nm)) nm else id
}

# Turn a REAL detection result into "will this statement be recognised on its own next time?". After
# a save the app re-converts with the template FORCED by id, which skips detection entirely, so
# without this a fingerprint that could never match ships looking fine. ok: NA = could not run.
recognition_summary <- function(det, saved_id, templates = NULL) {
  sid <- trimws(as.character(saved_id %||% NA_character_)[1])
  nm  <- function(id) .saved_name(id, templates)
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
      detail = sprintf("We re-checked it against every template and it matched yours (\"%s\") on its own, with nothing forced.", nm(sid))))
  if (isTRUE(det$matched))
    return(list(ok = FALSE,
      headline = sprintf("Careful: this statement is still recognised as \"%s\", not your new template.", nm(got)),
      detail = paste("Your template was saved, but another one wins on this file, so next",
                     "time it would be read with that other template. Open the toolkit again",
                     "and make the identifying phrase more specific to this bank - or use the",
                     "existing template if it is the right one.")))
  why <- trimws(as.character(det$detail %||% "")[1])
  # A tie is not a typo: on a tie the phrase IS printed on the page - for both templates - so advice
  # to retype it sends the analyst to correct something already correct.
  tied <- as.character(det$tied %||% character(0))
  # Differenced by id (the identity), said in names. Two ids can share a display name, so the names
  # are deduped and the count that leads the headline counts ids.
  others <- setdiff(tied, sid)
  other_names <- unique(vapply(others, nm, character(1), USE.NAMES = FALSE))
  if (length(tied) >= 2 && length(others))
    return(list(ok = FALSE,
      headline = if (length(others) == 1L)
        sprintf("Saved - but \"%s\" fits this statement exactly as well as yours.", other_names[1])
        else sprintf("Saved - but %d other templates fit this statement exactly as well as yours.",
                     length(others)),
      detail = paste0(
        "Nothing on this file tells them apart, so the tool cannot know which one you ",
        "meant. It still converts: it picks the tested template, reads the statement ",
        "and holds the run for a check, so no figures are lost. To settle it, add a ",
        "phrase to your template that ONLY this bank prints - that breaks the tie in ",
        "its favour - or retire the duplicate (",
        paste(other_names, collapse = ", "), "). Do not build a third: it would tie too.")))
  list(ok = FALSE,
    headline = "Saved - but this statement is NOT recognised by it yet.",
    detail = paste0(
      "Next time you upload a statement like this one it would still come back as ",
      "\"no template for this statement yet\". Almost always the identifying phrase ",
      "is not printed on the page exactly as typed. Open the toolkit again and set a ",
      "phrase you can see on the statement.",
      if (nzchar(why) && !is.na(why)) sprintf(" (The detector said: %s)", why) else ""))
}

# The OS-authenticated logged-in user. Only ever stored as a string.
current_user <- function() {
  for (v in c("USERNAME", "USER", "LOGNAME")) {
    u <- Sys.getenv(v)
    if (nzchar(u)) return(u)
  }
  "unknown"
}

# sub="byte" keeps a stray undefined byte VISIBLE as \\xNN rather than silently dropping it.
.transcode_lines <- function(raw_bytes, from) {
  s <- safe(iconv(list(raw_bytes), from = from, to = "UTF-8", sub = "byte"), NA_character_)
  if (length(s) != 1L || is.na(s)) return(character(0))
  s <- gsub("\r\n", "\n", s, fixed = TRUE); s <- gsub("\r", "\n", s, fixed = TRUE)
  strsplit(s, "\n", fixed = TRUE)[[1]]
}

# Read text lines without corrupting non-UTF-8 input. readLines(encoding="UTF-8") only TAGS bytes, it
# does not transcode, so a Windows-1252 bank export flows in as mojibake and breaks the
# verbatim-description guarantee. Sniff the BOM, else validate UTF-8, else fall back to Windows-1252.
# A UTF-8 BOM is stripped - left in, it corrupts the first header name.
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
    lines <- safe(readLines(path, warn = FALSE, encoding = "UTF-8"), character(0))
    if (length(lines)) lines[1] <- sub("^\xef\xbb\xbf", "", lines[1], useBytes = TRUE)
    return(lines)
  }
  # No BOM, not valid UTF-8, no declared encoding -> Windows-1252 fallback.
  .transcode_lines(body, "WINDOWS-1252")
}
