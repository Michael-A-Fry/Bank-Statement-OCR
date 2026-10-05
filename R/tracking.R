# tracking.R -- how automatic reading is doing, with no personal data (spec
# section 8, Appendix A4).
#
# One JSON line per event, appended to <dir>/tracking-YYYY-MM.jsonl (a file per
# month, by the event's UTC time, so a year is twelve small files and an old month
# can be archived whole). The Admin page reads it through track_summary(); the
# carry-off summary (track_export) holds counts only.
#
# WHAT MAY BE WRITTEN is an ALLOWLIST, not a filter: every field is named and
# typed below -- an enum, an id pattern, a whole number or a measurement -- and
# anything else is dropped with a warning before a byte is written. There is no
# free-text field at all, so a description, an amount, a date, a name, an account
# number or a file name has nowhere to go. The same allowlist is applied again on
# reading, so a hand-edited line cannot carry anything into the summary either.
#
# Shapes matter as much as names: an id field refuses any run of five or more
# digits (an account, card, phone or IRD number has nowhere to hide in one), a
# number field is a whole number in the range its meaning allows, and a time must
# be a real UTC instant.
#
# Lines are appended one write each, under a short lock per file (on Windows two
# processes appending at once can overwrite each other, not just interleave). A
# line torn anyway -- a process killed mid-write -- does not parse, is skipped and
# is counted (unreadable_lines), so it can never be miscounted as an event, and
# the next append starts on a fresh line so it is not lost with it.

TRACK_EVENTS        <- c("convert", "confirm", "correction", "spot_check", "learn")
TRACK_OUTCOMES      <- c("proven", "layout_match", "check", "unread")
TRACK_KINDS         <- c("pdf", "scan", "delimited", "excel")
TRACK_PROOF_KINDS   <- c("chain", "totals", "layout", "person", "none")
TRACK_LEARN_ACTIONS <- c("created", "evidence_added", "promoted", "none",
                         "confirmed", "corrected", "retired", "renamed")
TRACK_SPOT_CHECKS   <- c("right", "wrong", "cant_tell")
# The reader's hard checks (R/auto_read.R), and the recipe reader's (R/recipes.R),
# which asks the same questions of a recipe's table plus one of its own
# (statements_join: a file of several statements read with a recipe). A new check
# must be added here before it can be tracked; until then it is dropped with a
# warning, never written raw. test-tracking.R holds both lists to the readers'
# source, so they cannot drift.
TRACK_CHECKS <- c("rows_read", "rows_match_columns", "pages_with_rows", "words_used_once",
                  "lines_accounted", "dates_settled", "dated_lines_used", "pages_complete",
                  "balance_chain", "chain_across_pages", "opening_closing", "printed_totals",
                  "dates_readable", "dates_in_order", "dates_in_period", "signs_settled",
                  "no_derived_amounts", "amounts_read", "unique", "rows_proven",
                  "reader_agrees", "dates_carried", "other_tables", "ocr_complete",
                  "year_settled", "table_unbroken", "summary_lines_checked",
                  "rows_once", "one_statement", "rows_between_ends", "one_side_per_row",
                  "ends_printed", "sections_set_aside", "currency_own", "workbook_plain",
                  "tables_set_aside", "edge_lines", "compact_dates", "statements_join")
# The reader's repair steps (the "repair:<step>" candidates of R/auto_read.R; a
# step tried more than one way, "edge_lines2", is recorded under its own name).
TRACK_REPAIRS <- c("reocr_rows", "wider_cells", "narrower_cells", "no_page_shift",
                   "tables_apart", "edge_lines", "summary_figures")

.TRACK_ID_RE   <- "^[a-z][a-z0-9_]{0,39}$"
# A column role, as the reader and the Please-check screen name them; nothing
# else (a heading's own wording could be anything) is a role here.
.TRACK_ROLE_RE <- paste0("^(date|date2|weekday|description|debit|credit|amount|balance|type|",
                         "particulars|code|reference|other_party|other[0-9]{0,2}|text[0-9]{1,2}|ignore|none)$")

# The allowlist, in the order fields are written.
# `max` bounds are what the field can mean (a statement of 5,000 pages is not
# one), which also keeps a misplaced account number out of a count.
.TRACK_FIELDS <- list(
  ts             = list(type = "ts"),
  event          = list(type = "enum", values = TRACK_EVENTS),
  # a release number of three or four parts ("1.23.1", so never an amount like
  # "123.45") or "unknown" -- never a free word
  engine_version = list(type = "re", re = "^([0-9]{1,3}([.][0-9]{1,4}){2,3}(-[a-z0-9]{1,12})?|unknown)$"),
  # layouts_state_id(): 12 hex characters, or "empty" / "unknown"
  state_id       = list(type = "re", re = "^([0-9a-f]{12}|empty|unknown)$"),
  bank_code      = list(type = "re", re = "^[0-9]{2}$"),
  institution    = list(type = "re", re = .TRACK_ID_RE, id = TRUE),
  # R/layouts.R ids: <bank slug>_<n>
  layout_id      = list(type = "re", re = "^[a-z0-9]+(_[a-z0-9]+)*_[0-9]{1,4}$", id = TRUE),
  layout_version = list(type = "int", min = 1, max = 9999),
  kind           = list(type = "enum", values = TRACK_KINDS),
  pages          = list(type = "int", min = 0, max = 5000),
  rows           = list(type = "int", min = 0, max = 100000),
  outcome        = list(type = "enum", values = TRACK_OUTCOMES),
  proof_kind     = list(type = "enum", values = TRACK_PROOF_KINDS),
  checks_failed  = list(type = "enum_list", values = TRACK_CHECKS),
  repairs_tried  = list(type = "enum_list", values = TRACK_REPAIRS),
  candidates     = list(type = "int", min = 0, max = 1000),
  secs           = list(type = "num", min = 0, max = 86400, digits = 3),
  derived        = list(type = "int", min = 0, max = 100000),
  learn_action   = list(type = "enum", values = TRACK_LEARN_ACTIONS),
  correction     = list(type = "correction"),
  spot_check     = list(type = "enum", values = TRACK_SPOT_CHECKS)
)

# tracking_dir(cfg) -- where the tracking files live (paths$tracking).
tracking_dir <- function(cfg = load_config()) {
  safe(cfg$paths$tracking, NULL) %||% file.path("logs", "tracking")
}

# .track_value(spec, v) -> list(ok, value, why). `why` names the RULE broken,
# never the value, so a warning cannot leak what it refused.
.track_value <- function(spec, v) {
  bad <- function(why) list(ok = FALSE, why = why)
  scalar_chr <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  whole <- function(x, min, max) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) return(NULL)
    if (x != round(x) || x < min || x > max) return(NULL)
    as.integer(x)
  }
  switch(spec$type,
    enum = if (scalar_chr(v) && v %in% spec$values) list(ok = TRUE, value = v) else bad("not one of the allowed values"),
    re = if (scalar_chr(v) && grepl(spec$re, v) && !(isTRUE(spec$id) && grepl("[0-9]{5,}", v)))
           list(ok = TRUE, value = v) else bad("not in the allowed form"),
    # an event time: a real instant in this tool's UTC form ("2026-02-30..." is
    # not), and not in the future (a day's grace for a clock that is off)
    ts = {
      t <- if (scalar_chr(v) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$", v)) .parse_stamp(v) else NA
      if (is.na(t) || !identical(utc_stamp(t), v) || t > Sys.time() + 86400) bad("not an event time")
      else list(ok = TRUE, value = v)
    },
    int = { x <- whole(v, spec$min, spec$max); if (is.null(x)) bad("not a whole number in range") else list(ok = TRUE, value = x) },
    num = if (is.numeric(v) && length(v) == 1L && is.finite(v) && v >= spec$min && v <= spec$max)
            list(ok = TRUE, value = round(as.numeric(v), spec$digits)) else bad("not a number in range"),
    enum_list = , re_list = {
      x <- unlist(v)
      if (!is.null(v) && !is.character(x) && length(x)) return(bad("not a list of names"))
      x <- as.character(x)
      okv <- !is.na(x) & (if (spec$type == "enum_list") x %in% spec$values else grepl(spec$re, x))
      list(ok = TRUE, value = as.list(unique(x[okv])), dropped = sum(!okv))
    },
    correction = {
      items <- if (is.list(v) && !is.null(names(v)) && "role_from" %in% names(v)) list(v) else v
      if (!is.list(items)) return(bad("not a list of column changes"))
      okd <- function(d) is.numeric(d) && length(d) == 1L && is.finite(d) && abs(d) <= 2000  # points: wider than any page
      keep <- list(); dropped <- 0L
      for (it in items) {
        good <- is.list(it) && setequal(names(it), c("role_from", "role_to", "dx_left", "dx_right")) &&
          scalar_chr(it$role_from) && grepl(.TRACK_ROLE_RE, it$role_from) &&
          scalar_chr(it$role_to) && grepl(.TRACK_ROLE_RE, it$role_to) &&
          okd(it$dx_left) && okd(it$dx_right)
        if (!good) { dropped <- dropped + 1L; next }
        keep[[length(keep) + 1L]] <- list(role_from = it$role_from, role_to = it$role_to,
                                          dx_left = round(it$dx_left, 1), dx_right = round(it$dx_right, 1))
      }
      list(ok = TRUE, value = keep, dropped = dropped)
    },
    bad("has no rule"))
}

# .track_clean(fields, warn) -> the record that may be written (allowlisted
# fields, in allowlist order), or NULL when there is no valid event. With warn,
# each refusal is a warning naming the field and the rule.
.track_clean <- function(fields, warn = TRUE) {
  say <- function(...) if (warn) warning(sprintf(...), call. = FALSE)
  if (!is.list(fields)) { say("tracking: nothing was recorded (the fields were not a named list)"); return(NULL) }
  nm <- names(fields) %||% rep("", length(fields))
  if (any(!nzchar(nm) | is.na(nm))) say("tracking: %d unnamed value(s) were not recorded", sum(!nzchar(nm) | is.na(nm)))
  unknown <- setdiff(unique(nm[nzchar(nm) & !is.na(nm)]), names(.TRACK_FIELDS))
  # An unknown NAME is echoed so the caller can find it, but only its letters:
  # a name built from data ("acct_0104281833424") must not leak through the warning.
  for (u in unknown) say("tracking: field \"%s\" is not on the allowlist and was not recorded", substr(gsub("[^A-Za-z_]", "?", u), 1, 40))
  dup <- unique(nm[duplicated(nm) & nm %in% names(.TRACK_FIELDS)])
  for (d in dup) say("tracking: field \"%s\" was given more than once; only the first was recorded", d)
  rec <- list()
  for (f in names(.TRACK_FIELDS)) {
    if (!(f %in% nm)) next
    r <- .track_value(.TRACK_FIELDS[[f]], fields[[match(f, nm)]])
    if (!isTRUE(r$ok)) { say("tracking: field \"%s\" was not recorded (%s)", f, r$why); next }
    if (isTRUE((r$dropped %||% 0L) > 0L))
      say("tracking: %d item(s) of field \"%s\" were not recorded (not allowed values)", r$dropped, f)
    rec[[f]] <- r$value
  }
  if (is.null(rec$event)) { say("tracking: nothing was recorded (no valid event)"); return(NULL) }
  rec
}

# track_record(fields, path) -- append one event. `fields` is a named list drawn
# from the allowlist above; `ts` is filled with the current UTC time when absent.
# Returns TRUE/FALSE (invisibly) with attr "reason"; never throws.
track_record <- function(fields, path = tracking_dir()) {
  tryCatch({
    if (is.list(fields) && is.null(fields$ts)) fields <- c(list(ts = utc_stamp()), fields)
    rec <- .track_clean(fields, warn = TRUE)
    if (is.null(rec)) return(invisible(structure(FALSE, reason = "nothing valid to record")))
    if (is.null(rec$ts)) rec <- c(list(ts = utc_stamp()), rec)
    line <- as.character(jsonlite::toJSON(rec, auto_unbox = TRUE, digits = NA, null = "null"))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(path, sprintf("tracking-%s.jsonl", substr(rec$ts, 1, 7)))
    .track_locked(paste0(f, ".lock"), function() {
      # a line left without its newline (a writer killed mid-line) is closed off
      # first, so this event is not glued onto it and lost with it
      sz <- safe(file.size(f), NA)
      lead <- ""
      if (!is.na(sz) && sz > 0) {
        last <- safe({
          ci <- file(f, "rb")
          tryCatch({ seek(ci, sz - 1); readBin(ci, "raw", 1L) }, finally = close(ci))
        }, raw(0))
        if (!identical(last, charToRaw("\n"))) lead <- "\n"
      }
      con <- file(f, open = "ab")
      on.exit(close(con), add = TRUE)
      writeBin(charToRaw(paste0(lead, line, "\n")), con)
    })
    invisible(structure(TRUE, reason = "recorded"))
  }, error = function(e) invisible(structure(FALSE, reason = paste0("could not write the tracking file (", conditionMessage(e), ")"))))
}

# .track_locked(lk, fn) -- run fn() holding the lock folder `lk`. An append takes
# milliseconds, so a lock still there after two seconds was left by a process
# that died: it is taken over rather than losing the event.
.track_locked <- function(lk, fn) {
  got <- FALSE
  for (i in seq_len(40L)) {
    if (isTRUE(suppressWarnings(dir.create(lk)))) { got <- TRUE; break }
    Sys.sleep(0.05)
  }
  if (!got) { unlink(lk, recursive = TRUE); got <- isTRUE(suppressWarnings(dir.create(lk))) }
  if (got) on.exit(unlink(lk, recursive = TRUE), add = TRUE)
  fn()
}

# track_reading_fields(reading, bank, state_id, pages, event) -> the fields of a
# "convert" event, taken from an auto_read() reading and a bank_identify() /
# bank_pick() result. Kept here, beside the allowlist, so the screens never pick
# fields out of a reading by hand.
track_reading_fields <- function(reading, bank = NULL, state_id = NULL, pages = NULL) {
  f <- list(event = "convert", engine_version = safe(engine_version(), NULL))
  if (!is.null(state_id)) f$state_id <- state_id
  # A bank is tracked by the same slug its layouts are filed under, so a custom
  # bank name ("Smith Credit Union") is recorded as an id, like a known one.
  inst <- if (is.list(bank)) bank$institution %||% bank$bank else bank
  inst <- .layout_slug(inst)
  if (!is.na(inst)) f$institution <- inst
  if (is.list(bank) && !is.null(bank$bank_code) && !is.na(bank$bank_code[1]))
    f$bank_code <- as.character(bank$bank_code[1])
  if (!is.list(reading)) return(f)
  ml <- reading$matched_layout
  if (is.character(ml) && length(ml) == 1L && grepl("^[a-z0-9_]+@v?[0-9]+$", ml)) {
    f$layout_id <- sub("@v?[0-9]+$", "", ml)
    f$layout_version <- as.integer(sub("^.*@v?", "", ml))
  }
  k <- reading$template$signature$kind %||% reading$signature$kind
  if (!is.null(k)) f$kind <- as.character(k)[1]
  pg <- pages %||% { p <- reading$proof$pages_used; if (length(p)) length(unique(p)) else NULL }
  if (!is.null(pg)) f$pages <- pg
  f$rows <- if (is.data.frame(reading$transactions)) nrow(reading$transactions) else 0L
  f$outcome <- reading$outcome
  if (!is.null(reading$proof$kind)) f$proof_kind <- reading$proof$kind
  ck <- reading$checks
  f$checks_failed <- if (is.data.frame(ck) && nrow(ck)) ck$check[ck$ok %in% FALSE] else character(0)
  src <- if (is.data.frame(reading$candidates)) reading$candidates$source else character(0)
  f$repairs_tried <- unique(sub("[0-9]+$", "", sub("^repair:", "", src[startsWith(src, "repair:")])))
  f$candidates <- length(src)
  if (is.numeric(reading$secs)) f$secs <- reading$secs
  f$derived <- as.integer(reading$proof$derived %||% 0L)
  f
}

# .track_since(since) -> a UTC stamp to compare with (stamps sort as text), NULL
# for "everything", or NA when `since` cannot be read.
.track_since <- function(since) {
  if (is.null(since)) return(NULL)
  if (inherits(since, "Date")) return(paste0(format(since[1]), "T00:00:00Z"))
  if (inherits(since, "POSIXt")) return(utc_stamp(since[1]))
  s <- trimws(as.character(since)[1])
  if (!is.na(s) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s) && !is.na(as.Date(s, optional = TRUE)))
    return(paste0(s, "T00:00:00Z"))
  t <- .parse_stamp(s)
  if (!is.na(t)) return(utc_stamp(t))
  NA_character_
}

# .track_read(path, since) -> list(records, files, unreadable). Each record is
# re-checked against the allowlist, silently.
.track_read <- function(path, since = NULL) {
  out <- list(records = list(), files = 0L, unreadable = 0L)
  if (is.null(path) || !dir.exists(path)) return(out)
  fs <- sort(list.files(path, pattern = "^tracking-[0-9]{4}-[0-9]{2}[.]jsonl$"), method = "radix")
  if (!is.null(since) && !is.na(since))
    fs <- fs[substr(fs, 10, 16) >= substr(since, 1, 7)]
  recs <- list(); bad <- 0L
  for (f in fs) {
    # A damaged file (binary junk, embedded nuls, bytes that are not UTF-8) is
    # unreadable lines, counted -- never an error that would lose every other
    # month's counts with it. Every line this tool writes is ASCII.
    ln <- safe(suppressWarnings(readLines(file.path(path, f), warn = FALSE, encoding = "bytes")), character(0))
    asc <- !grepl("[^ -~\t\r]", ln, useBytes = TRUE)
    bad <- bad + sum(!asc & grepl("[^[:space:]]", ln, useBytes = TRUE))
    ln <- ln[asc & grepl("[^ \t\r]", ln, useBytes = TRUE)]
    for (l in ln) {
      r <- safe(suppressWarnings(jsonlite::fromJSON(l, simplifyVector = FALSE)), NULL)
      r <- if (is.list(r) && length(r) && !is.null(names(r))) safe(.track_clean(r, warn = FALSE), NULL) else NULL
      if (is.null(r) || is.null(r$ts)) { bad <- bad + 1L; next }
      if (!is.null(since) && !is.na(since) && r$ts < since) next
      recs[[length(recs) + 1L]] <- r
    }
  }
  list(records = recs, files = length(fs), unreadable = bad)
}

# .track_count(x, levels) -- counts as a named integer: every level (in order)
# when given, else the values seen, most frequent first, ties by name.
.track_count <- function(x, levels = NULL) {
  x <- as.character(unlist(x))
  x <- x[!is.na(x)]
  if (!is.null(levels)) return(vapply(levels, function(l) sum(x == l), integer(1)))
  if (!length(x)) return(integer(0))
  u <- sort(unique(x), method = "radix")
  n <- vapply(u, function(l) sum(x == l), integer(1))
  n[order(-n, u, method = "radix")]
}

# track_summary(path, since) -> counts for the Admin page and the carry-off
# summary: statements by outcome, kind, bank and layout; checks failed; repairs;
# how readings were proven; learning; corrections; spot checks; and the automatic
# rate overall and per kind (proven + layout_match over statements converted --
# the pass mark is measured per kind). `since`: a Date, a time or "YYYY-MM-DD".
# Whatever happens, the result has the same fields (an unreadable folder is an
# empty summary with a note), so the Admin page never meets a missing one.
track_summary <- function(path = tracking_dir(), since = NULL) {
  tryCatch({
    notes <- character(0)
    s <- .track_since(since)
    if (!is.null(s) && is.na(s)) {
      notes <- c(notes, "The 'since' date could not be read, so every record is counted.")
      s <- NULL
    }
    .track_summarise(.track_read(path, s), s, notes)
  }, error = function(e)
    .track_summarise(list(records = list(), files = 0L, unreadable = 0L), NULL,
                     paste0("The tracking files could not be read (", conditionMessage(e), ").")))
}

.track_summarise <- function(rd, s, notes) {
  R <- rd$records
  get <- function(rs, f) unlist(lapply(rs, function(r) r[[f]]))
  ev <- get(R, "event")
  conv <- R[ev %in% "convert"]
  oc <- .track_count(get(conv, "outcome"), TRACK_OUTCOMES)
  n <- length(conv)
  auto <- unname(oc["proven"] + oc["layout_match"])
  kinds <- vapply(conv, function(r) r$kind %||% "unknown", "")
  outs <- vapply(conv, function(r) r$outcome %||% "unknown", "")
  by_kind <- do.call(rbind, lapply(c(TRACK_KINDS, if (any(kinds == "unknown")) "unknown"), function(k) {
    o <- outs[kinds == k]; a <- sum(o %in% c("proven", "layout_match"))
    data.frame(kind = k, statements = length(o), proven = sum(o == "proven"),
               layout_match = sum(o == "layout_match"), check = sum(o == "check"),
               unread = sum(o == "unread"), automatic = a,
               automatic_rate = if (length(o)) round(a / length(o), 4) else NA_real_,
               stringsAsFactors = FALSE)
  }))
  bank <- vapply(conv, function(r) r$institution %||% (if (!is.null(r$bank_code)) paste0("code ", r$bank_code)) %||% "unknown", "")
  lay <- unlist(lapply(conv, function(r) if (!is.null(r$layout_id))
    paste0(r$layout_id, if (!is.null(r$layout_version)) paste0("@", r$layout_version))))
  spot <- .track_count(get(R[ev %in% "spot_check"], "spot_check"), TRACK_SPOT_CHECKS)
  corr <- R[ev %in% "correction"]
  moves <- unlist(lapply(corr, function(r) vapply(r$correction %||% list(), function(m)
    paste0(m$role_from, " -> ", m$role_to), "")))
  times <- vapply(R, function(r) r$ts, "")
  # A PERSON'S CONFIRM IS ITS OWN EVENT. The statement was counted once, as the
  # "convert" event that sent it to Please check (its proof kind is what the
  # reader put it to, never "person"); the confirm that then vouched for it is a
  # separate "confirm" event, left out of `conv` so the statement is not counted
  # twice. So "a person" counted 0 however many readings people confirmed. Each
  # confirm is one statement a person stood behind: counted here, under its own
  # proof kind, and nowhere else.
  pk <- .track_count(get(conv, "proof_kind"), TRACK_PROOF_KINDS)
  conf <- R[ev %in% "confirm"]
  pk["person"] <- pk["person"] + sum(vapply(conf, function(r) identical(r$proof_kind, "person"), logical(1)))
  list(
    since = s %||% NA_character_,
    first = if (length(times)) min(times) else NA_character_,
    last = if (length(times)) max(times) else NA_character_,
    events = .track_count(ev, TRACK_EVENTS),
    statements = n,
    outcomes = oc,
    automatic = auto,
    automatic_rate = if (n) round(auto / n, 4) else NA_real_,
    by_kind = by_kind,
    banks = .track_count(bank),
    layouts = .track_count(lay),
    checks_failed = .track_count(get(conv, "checks_failed")),
    proof_kinds = pk,
    repairs_tried = .track_count(get(conv, "repairs_tried")),
    with_derived = sum(vapply(conv, function(r) isTRUE((r$derived %||% 0L) > 0L), logical(1))),
    learn_actions = .track_count(get(R, "learn_action")),
    corrections = length(corr),
    roles_changed = .track_count(moves),
    spot_checks = c(spot, total = sum(spot)),
    engine_versions = .track_count(get(conv, "engine_version")),
    files = rd$files,
    unreadable_lines = rd$unreadable,
    notes = notes)
}

# track_export(path, out, since) -- write the carry-off summary to `out` as JSON:
# track_summary()'s counts, with when it was made and by which engine. Counts and
# codes only, by construction: every value in it came through the allowlist.
# Written to a temp file and moved into place, so a reader never sees half of it.
# Returns TRUE/FALSE with attr "reason"; never throws.
track_export <- function(path = tracking_dir(), out, since = NULL) {
  tryCatch({
    out <- as.character(out %||% "")[1]
    if (is.na(out) || !nzchar(out)) return(structure(FALSE, reason = "no file name was given to export to"))
    s <- track_summary(path, since)
    named <- function(x) if (length(x)) as.list(x) else structure(list(), names = character(0))
    doc <- list(
      what = "Statement Studio automatic reading - summary (counts only)",
      generated = utc_stamp(), engine_version = safe(engine_version(), "unknown"),
      since = s$since, first = s$first, last = s$last,
      statements = s$statements, outcomes = named(s$outcomes),
      automatic = s$automatic, automatic_rate = s$automatic_rate,
      by_kind = s$by_kind,
      events = named(s$events), banks = named(s$banks), layouts = named(s$layouts),
      checks_failed = named(s$checks_failed), proof_kinds = named(s$proof_kinds),
      repairs_tried = named(s$repairs_tried), with_derived = s$with_derived,
      learn_actions = named(s$learn_actions), corrections = s$corrections,
      roles_changed = named(s$roles_changed), spot_checks = named(s$spot_checks),
      engine_versions = named(s$engine_versions),
      unreadable_lines = s$unreadable_lines, notes = as.list(s$notes))
    txt <- jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = "null",
                            dataframe = "rows")
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(out, ".", Sys.getpid(), ".part")
    con <- file(tmp, open = "wb")
    wrote <- tryCatch({ writeBin(charToRaw(paste0(txt, "\n")), con); TRUE }, error = function(e) FALSE)
    close(con)
    if (!wrote || !isTRUE(safe(file.rename(tmp, out), FALSE))) {
      safe(unlink(tmp))
      return(structure(FALSE, reason = "could not write the summary file - check the folder permissions"))
    }
    structure(TRUE, reason = "saved")
  }, error = function(e) structure(FALSE, reason = paste0("could not write the summary (", conditionMessage(e), ")")))
}
