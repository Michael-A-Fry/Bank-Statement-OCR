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

# ---------------------------------------------------------------------------
# ONE CLOCK. Every timestamp WRITTEN is UTC and says so with a Z; every timestamp
# SHOWN is local and names its zone.
#
# WHY: measured in Pacific/Auckland from the same second, the two handles a
# reviewer uses to tie an upload to a conversion were TWELVE HOURS apart --
# uploads/<id> was stamped 20260826193118 (local, no marker), the run_id
# 20260826073118 (UTC, no marker), and record.json 2026-08-26T19:31:18+1200. The
# Admin uploads table lists them side by side and invites matching them by eye, so
# across the overnight boundary a reviewer pairs the wrong rows and nothing on
# either handle says which zone it is in. The feed's converted_ts was UTC while
# every other record was local, so an NZ morning conversion was dated the day
# before on the dashboards.
#
# A second, quieter reason for UTC on the written side: local stamps do not SORT.
# One hour a year the clocks go back, and 02:30+1300 written before 02:30+1200
# sorts after it, so a plain string sort of `ts` -- which is what the log readers
# and the uploads table do -- puts an hour of that night in reverse.
#
# So there are exactly two helpers and every writer uses one of them.
# ---------------------------------------------------------------------------

# utc_stamp(t) -- the timestamp to WRITE: "2026-08-26T07:31:18Z".
utc_stamp <- function(t = Sys.time()) {
  t <- safe(as.POSIXct(t), NA)
  if (length(t) != 1L || is.na(t)) return(NA_character_)
  format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# utc_id(t) -- the same instant as an id fragment: "20260826073118". This is what
# makes an upload id and a run_id agree by construction rather than by luck.
utc_id <- function(t = Sys.time()) {
  t <- safe(as.POSIXct(t), NA)
  if (length(t) != 1L || is.na(t)) return(NA_character_)
  format(t, "%Y%m%d%H%M%S", tz = "UTC")
}

# .parse_stamp(x) -- a written stamp back to an instant. Accepts the Z form this
# tool writes now and the "+1200" form it wrote before, so records already on the
# box still read and still sort. NA for anything else -- never a guess, because a
# stamp read in the wrong zone is a wrong figure that looks right.
.parse_stamp <- function(x) {
  s <- trimws(as.character(x))
  s[is.na(s) | !nzchar(s)] <- NA_character_
  s <- sub("Z$", "+0000", s)
  s <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", s)
  as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
}

# local_time_text(x) -- the timestamp to SHOW: "26 Aug 2026 19:31 NZST", in this
# server's own zone, with the zone named so nobody has to ask "where?". Anything
# unparseable comes back exactly as it was given, never as a blank or a guess.
local_time_text <- function(x) {
  t <- .parse_stamp(x)
  raw <- as.character(x)
  # tz = "" is THIS SERVER's zone. Without it the stamp formats in the zone it was
  # parsed in (UTC), which would show a New Zealand morning as the previous
  # evening -- the very fault this helper exists to end.
  out <- format(t, "%d %b %Y %H:%M %Z", tz = "")
  # %Z is empty on a few R builds; the zone name must never be missing, since the
  # whole point of showing local time is that it says which local.
  zone <- safe(Sys.timezone(), NA_character_)
  blank <- !is.na(t) & (is.na(out) | !grepl("[A-Za-z]{2,}$", out))
  if (any(blank))
    out[blank] <- paste(format(t[blank], "%d %b %Y %H:%M"),
                        if (is.na(zone) || !nzchar(zone)) "local time" else zone)
  out[is.na(t)] <- raw[is.na(t)]
  out
}

# ---------------------------------------------------------------------------
# save_yaml_safely(x, path) -- write a YAML file so that ONE bad save cannot cost
# the accumulated value of the tool.
#
# WHY: the dictionaries have had a .bak on every save for a long time (see
# yaml_append_phrase above). The TEMPLATES -- which backup-and-restore.md calls
# "the accumulated value of the tool", months of somebody's work that exists
# nowhere else on an offline box -- were written with a bare yaml::write_yaml.
# That is one call with two ways to lose a template: it truncates the target
# before it writes, so an error or a full disk part-way through leaves a file that
# no longer parses AND no copy of what it used to say; and there is no way back
# from a save that wrote perfectly valid YAML the person did not mean.
#
# Three steps, in this order, and each one is why the next is safe:
#   1. copy the existing file to <path>.bak         -- one save's worth of undo
#   2. write a temp file in the SAME folder         -- a failure never touches <path>
#   3. rename the temp over <path>                  -- atomic on a same-volume move
# A reader (a conversion loading templates) therefore sees the old file or the new
# one, never a half-written one.
#
# Returns TRUE/FALSE with attr "reason" -- one plain sentence a screen can show as
# it stands, so the engine and the screen cannot disagree about why a save failed.
save_yaml_safely <- function(x, path) {
  path <- as.character(path %||% "")[1]
  if (is.na(path) || !nzchar(path))
    return(structure(FALSE, reason = "no file name was given to save to"))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(path) && !isTRUE(safe(file.copy(path, paste0(path, ".bak"),
                                                  overwrite = TRUE), FALSE)))
    return(structure(FALSE, reason = "could not keep a copy of the previous version, so nothing was changed"))
  # The temp and the backup both end in something no template loader looks for
  # (they all list "*.yaml"/"*.yml"), so neither can ever be loaded as a template.
  tmp <- paste0(path, ".", Sys.getpid(), ".part")
  # Warnings are caught as well as errors, for the reason .atomic_write_csv gives
  # in R/feed.R: on a read-only share file() WARNS and the write returns normally,
  # so an error-only guard reports a failed save as a success.
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

# .saved_name(id, templates) -- a template said in the words the person holding
# the statement can check ("SAMPLE BANK statement"), never its id.
#
# WHY: the card two inches to the left of this message already says "Read as:
# SAMPLE BANK statement", while the save confirmation said, twice, Saved
# "sample_bank_statement_pdf" ... it matched yours ("sample_bank_statement_pdf").
# An id is a maintainer's handle -- an accountant cannot check a figure against
# it, and the charter's interface rule forbids putting an engine code in front of
# her. One screen must not speak two languages about the same template.
#
# The id remains the fallback, and deliberately: when no template set is supplied
# there is no name to be had, and a save confirmation that named NOTHING would be
# worse than one naming a handle. Callers on a customer-facing screen pass the
# set they already loaded (app.R) and get words.
.saved_name <- function(id, templates = NULL) {
  id <- trimws(as.character(id %||% NA_character_)[1])
  if (is.na(id) || !nzchar(id)) return(id)
  t <- if (is.list(templates) && !is.null(templates[[id]])) templates[[id]] else NULL
  nm <- if (is.null(t)) NA_character_ else safe(template_display_name(t), NA_character_)
  nm <- as.character(nm %||% NA_character_)[1]
  if (!is.na(nm) && nzchar(nm)) nm else id
}

# recognition_summary(det, saved_id, templates) -> list(ok, headline, detail).
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
#
# `templates` is the template SET the caller already loaded. Given it, every
# template named below is named in WORDS (see .saved_name); without it they are
# named by id, as they always were. It is optional rather than required so that
# adding it could not break a caller, and it defaults to the old behaviour rather
# than to silence -- a confirmation that named no template at all would be worse
# than one naming a handle.
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
  # A TIE IS NOT A TYPO, and it used to be diagnosed as one: the headline said the
  # template was not recognised and the lead sentence sent the analyst back to
  # retype an identifying phrase that "is not printed on the page exactly as
  # typed". On a tie that phrase IS printed on the page -- it is printed for both
  # templates, which is the whole problem -- so she would retype something already
  # correct, and the charter forbids the other reading of that advice (a third
  # template would tie too). The real cause leads, and the cure is the one that
  # actually separates them.
  tied <- as.character(det$tied %||% character(0))
  # Set-differenced by ID (the identity), then said in NAMES. A tie is most often
  # the same layout set up twice, so two ids can share one display name; the names
  # are deduped so the sentence cannot list the same words twice, and the count
  # that leads the headline is the count of ids -- how many templates there really
  # are is the fact she needs, and it is the one a maintainer will act on.
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
# with a pound sign, an accented letter, or a non-breaking space) then flows in as
# mojibake / invalid UTF-8, breaking the verbatim-description guarantee, and a
# UTF-16 file garbles entirely.
# So: sniff the byte-order mark, else validate UTF-8 and fall back to the dominant
# 8-bit codepage -- all deterministic (`encoding` overrides the sniff when known).
#   * UTF-8 BOM (Excel writes one on every CSV export) is stripped -- left in it
#     corrupts the first header name (a BOM followed by "Date" is not "Date").
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
