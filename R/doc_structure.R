# doc_structure.R -- WHERE EACH SECTION STARTS, AND THEREFORE WHERE THE ONE
# BEFORE IT STOPS.
#
# THE FAULT THIS CLOSES. A table read "everything below my heading" instead of
# "everything until the next section begins", so a short section ran straight
# into the one after it. Observed:
#
#   Nominated Persons History
#     row 1 = the single real record
#     row 2 = Address History                 <- the NEXT section's heading
#     row 3 = Tax Type Address Type Address   <- the NEXT section's header row
#
# The end coordinate could not save it, because the end coordinate was measured
# on a copy where the preceding sections happened to have the row counts they had
# that day. Every optional section that is absent, and every history that gained
# a row, moves everything below it.
#
# THE RULE, AND IT IS THE WHOLE MODULE:
#
#   A section ends immediately before the nearest LATER validated section start,
#   whichever section that turns out to be.
#
# ORDER IS NEVER ENCODED. No section names its successor and no expected sequence
# is stored anywhere. Sections may be absent, may repeat, may carry zero rows, may
# be last; a report variant may order them differently. So every section start is
# detected INDEPENDENTLY, the accepted ones are sorted by where they physically
# landed, and the boundary falls out of that sorted list. An earlier draft of
# this had each table name the tables that could follow it -- that is the same
# overfitting as a page number, just harder to see.
#
# A HEADING STRING IS NOT A SECTION START. A report can print a list of the
# sections it holds NO data for:
#
#   No Information Held
#     Customer Online Details      <- a value, not a section
#     Tax Agent History            <- a value, not a section
#
# Each is on its own line and matches a heading alias exactly, so wording alone
# would close the open section three times over and lose its rows. What tells
# them apart is what is NOT under them: a real section start is followed by its
# COLUMN HEADER. That is the load-bearing rule here -- header evidence is
# required before a candidate is accepted, and a template must say so explicitly
# (`allow_headerless: true`) to be excused it.
#
# OPT-IN, so no existing template changes behaviour. Capping applies only where
# the template asks for it (schema_version: 2, or boundary_policy). A version 1
# template keeps its declared coordinates and reads exactly as it did.

# How many lines below a candidate heading its column header may sit. A heading
# is usually followed immediately by the header, but a section can carry a
# one-line note or a blank between them.
DOC_SECTION_HEADER_LOOKAHEAD <- 4L
# The fraction of a section's remembered column wordings that must be printed on
# one line for that line to count as its header. The same bar .doc_find_header
# already applies everywhere else, so a section start is held to the standard the
# reader uses, not a private one.
DOC_SECTION_MIN_HEADER_HIT <- 0.6

# doc_sections_enabled(tmpl) -- does this template want boundary capping.
#
# Version 1 templates are untouched: their coordinates keep their current exact
# meaning. A template opts in with `schema_version: 2` or by saying so directly.
doc_sections_enabled <- function(tmpl) {
  if (.doc_int(.doc_key(tmpl, "schema_version"), 1L) >= 2L) return(TRUE)
  bp <- .doc_key(tmpl, "boundary_policy")
  is.list(bp) && isTRUE(bp$end_at_nearest_later_section)
}

# .sec_headings(tab, key) -- the wordings that can identify this section.
#
# A version 2 table lists them under detector$headings. A version 1 table has
# only its printed name, which is what the builder captured; using it means an
# opted-in template gets sensible behaviour without every table being rewritten.
.sec_headings <- function(tab, key = NULL) {
  d <- .doc_key(tab, "detector")
  h <- if (is.list(d)) (d$headings %||% d$heading_aliases %||% NULL) else NULL
  h <- as.character(unlist(h %||% character(0)))
  if (!length(h)) h <- as.character(tab$name %||% key %||% "")[1]
  h <- trimws(h[!is.na(h)])
  # A very short heading is refused for the reason .doc_page_has_title refuses
  # one: "Total" or "Notes" is printed on half the pages of a report, and a
  # boundary drawn on it would cut a table in half.
  unique(h[nchar(.doc_norm(h)) >= 8L])
}

# .sec_header_need(tab) -- the column wordings that prove a section start.
.sec_header_need <- function(tab) {
  d <- .doc_key(tab, "detector")
  sig <- if (is.list(d)) (d$header_signatures %||% NULL) else NULL
  need <- if (!is.null(sig)) as.character(unlist(sig)) else
    as.character(unlist(tab$anchor$header_text %||% character(0)))
  need <- trimws(need[!is.na(need)])
  unique(need[nzchar(need)])
}

# .sec_match_mode(tab) -- how the heading wording is compared. Controlled, never
# fuzzy by default: an ambiguous boundary is worse than a missed one, because it
# silently reassigns rows from one table to another.
.SEC_MATCH_MODES <- c("normalized_exact", "contains", "exact")

.sec_match_mode <- function(tab) {
  d <- .doc_key(tab, "detector")
  m <- if (is.list(d)) tolower(as.character(d$heading_match %||% "")[1]) else ""
  if (is.na(m) || !(m %in% .SEC_MATCH_MODES)) "normalized_exact" else m
}

# .sec_heading_hit(d, headings, mode) -- is this LINE this section's heading.
#
# normalized_exact compares the whole line, which is what stops a heading buried
# in a sentence from opening a section. `contains` is available for a heading
# printed with a trailing count or date, and is the caller's decision to make.
.sec_heading_hit <- function(d, headings, mode = "normalized_exact") {
  txt <- .doc_line_text(d)
  if (identical(mode, "exact")) return(any(trimws(txt) == trimws(headings)))
  k <- .doc_norm(txt)
  if (!nzchar(k)) return(FALSE)
  keys <- .doc_norm(headings)
  keys <- keys[nzchar(keys)]
  if (!length(keys)) return(FALSE)
  if (identical(mode, "contains")) return(any(vapply(keys, function(x)
    grepl(x, k, fixed = TRUE), logical(1))))
  any(k == keys)
}

# .sec_header_below(lines, i, need, lookahead, min_hit) -> the header's hit
# fraction, or NA when no line below this one carries the section's columns.
.sec_header_below <- function(lines, i, need, lookahead, min_hit) {
  if (!length(need)) return(NA_real_)
  j_end <- min(length(lines), i + max(1L, lookahead))
  if (i + 1L > j_end) return(NA_real_)
  best <- NA_real_
  for (j in seq.int(i + 1L, j_end)) {
    h <- .doc_header_hit(lines[[j]], need)
    if (is.finite(h) && (is.na(best) || h > best)) best <- h
  }
  if (!is.na(best) && best >= min_hit) best else NA_real_
}

# doc_section_index(input, tmpl) -> a data.frame of every VALIDATED section start
# in the document, sorted by where it physically landed.
#
#   key   the table it starts        page, y  where its heading is printed
#   score the header evidence        headerless  TRUE when accepted without one
#
# The sort is the point. It describes what this PDF contains; it imposes no
# business order, and the same PDF always produces the same index (page, then y,
# then key, so a tie can never depend on the order names came out of a list).
doc_section_index <- function(input, tmpl) {
  none <- data.frame(key = character(0), page = integer(0), y = numeric(0),
                     score = numeric(0), headerless = logical(0),
                     stringsAsFactors = FALSE)
  tabs <- .doc_key(tmpl, "tables")
  if (!is.list(tabs) || !length(tabs)) return(none)
  wl <- input$words %||% list()
  np <- length(wl)
  if (!np) return(none)
  keys <- names(tabs) %||% character(0)
  if (!length(keys)) return(none)

  # The specs are read once, outside the page loop: every page asks the same
  # questions of the same tables.
  specs <- list()
  for (k in keys) {
    tb <- tabs[[k]]
    if (!is.list(tb)) next
    hs <- .sec_headings(tb, k)
    if (!length(hs)) next
    d <- .doc_key(tb, "detector")
    specs[[k]] <- list(
      headings = hs,
      need = .sec_header_need(tb),
      mode = .sec_match_mode(tb),
      headerless = is.list(d) && isTRUE(d$allow_headerless),
      lookahead = .doc_int(if (is.list(d)) d$header_lookahead else NULL,
                           DOC_SECTION_HEADER_LOOKAHEAD),
      min_hit = .doc_num(if (is.list(d)) d$minimum_header_hit else NULL,
                         DOC_SECTION_MIN_HEADER_HIT))
  }
  if (!length(specs)) return(none)

  rows <- list()
  for (p in seq_len(np)) {
    w <- .doc_page_words(input, p, tmpl)
    if (is.null(w) || !nrow(w)) next
    lines <- .doc_lines(w, NULL)
    if (!length(lines)) next
    for (i in seq_along(lines)) {
      d <- lines[[i]]
      if (.doc_is_rule(d)) next
      for (k in names(specs)) {
        s <- specs[[k]]
        if (!.sec_heading_hit(d, s$headings, s$mode)) next
        hit <- .sec_header_below(lines, i, s$need, s$lookahead, s$min_hit)
        # THE REFUSAL THAT MATTERS. No header under it, and the template did not
        # say this section can be headerless: this is the wording printed as a
        # value, not a section start. Refusing it is what keeps a "No Information
        # Held" list from closing three sections that are still open.
        if (is.na(hit) && !isTRUE(s$headerless)) next
        rows[[length(rows) + 1L]] <- data.frame(
          key = k, page = as.integer(p),
          y = .doc_num(attr(d, "top"), NA_real_),
          score = if (is.na(hit)) 0 else hit,
          headerless = is.na(hit), stringsAsFactors = FALSE)
        # One candidate per line: two sections claiming the same printed words is
        # an ambiguity, and taking the first name in an arbitrary list order is
        # exactly the silent decision this file exists to avoid.
        break
      }
    }
  }
  if (!length(rows)) return(none)
  ix <- do.call(rbind, rows)
  ix <- ix[!is.na(ix$y), , drop = FALSE]
  if (!nrow(ix)) return(none)
  ix[order(ix$page, ix$y, ix$key), , drop = FALSE]
}

# doc_section_cap(index, page, y) -> list(page, y) for the nearest validated
# section start strictly BELOW the given point, or NULL when this is the last one.
#
# "Strictly below" is what lets a table's own heading sit in the index without
# capping the table at itself. The caller passes the point its rows START from,
# which is under its own heading and header.
doc_section_cap <- function(index, page, y) {
  if (!is.data.frame(index) || !nrow(index)) return(NULL)
  p <- .doc_int(page, NA_integer_); yy <- .doc_num(y, NA_real_)
  if (is.na(p) || is.na(yy)) return(NULL)
  later <- index[index$page > p | (index$page == p & index$y > yy), , drop = FALSE]
  if (!nrow(later)) return(NULL)
  list(page = as.integer(later$page[1]), y = .doc_num(later$y[1]))
}

# doc_section_note(index, capped) -- what the reader is told about a
# boundary. Never silent: a table that stopped because another section began has
# had its extent decided by something the template did not say, and the person
# reading the workbook is entitled to know which.
doc_section_note <- function(index, capped) {
  if (is.null(capped)) return(NULL)
  if (!is.data.frame(index) || !nrow(index)) return(NULL)
  at <- index[index$page == capped$page & index$y == capped$y, , drop = FALSE]
  nm <- if (nrow(at)) as.character(at$key[1]) else NA_character_
  if (is.na(nm)) return(NULL)
  sprintf("stopped where '%s' begins (page %d), not at the end position recorded on the example",
          nm, as.integer(capped$page))
}
