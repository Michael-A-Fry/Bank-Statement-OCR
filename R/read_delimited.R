# read_delimited.R -- robust base-R delimited reader (CSV/TSV/TDV/TXT).
# Honours an optional preamble skip and returns a character-only data.frame with
# a per-parsed-row -> source-line mapping for provenance. Parses defensively,
# one logical record at a time, so a single malformed record can never shift the
# columns of its neighbours, throw away the whole statement, or hide a lost row.
#
# Key guarantees implemented here:
#  * NO silent drops -- `n_data_lines` reports the count of non-empty physical
#    data lines so reconcile can prove every source record became a row.
#  * Rectangular parse -- every logical record is normalised to exactly
#    `expected_fields` cells (quote-aware pad-short / truncate-long) so an
#    over-long or short row is isolated + flagged, never corrupts other rows.
#  * Provenance survives multi-line records -- a quoted field with an embedded
#    newline spans several physical lines but stays ONE row, with its full line
#    span and raw text recorded per row.

# .count_dquotes(s) -- number of double-quote characters in a string. A CSV
# record is complete when this total (across accumulated physical lines) is
# even: escaped quotes ("") contribute two and stay even, an open quoted field
# leaves it odd until the closing quote arrives.
.count_dquotes <- function(s) {
  m <- gregexpr('"', s, fixed = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) 0L else length(m)
}

# .split_records(lines) -- coalesce physical data lines into logical CSV records
# respecting quoted fields. Returns a list of records, each
# list(text = combined text with embedded newlines, lines = physical indices).
# `lines` are the physical line strings; `idx` their original line numbers.
.split_records <- function(lines, idx) {
  records <- list()
  buf <- NULL; buf_lines <- integer(0); open <- FALSE
  for (k in seq_along(lines)) {
    ln <- lines[k]
    if (is.null(buf)) { buf <- ln; buf_lines <- idx[k] }
    else { buf <- paste(buf, ln, sep = "\n"); buf_lines <- c(buf_lines, idx[k]) }
    open <- (.count_dquotes(buf) %% 2L) == 1L
    if (!open) {
      records[[length(records) + 1L]] <- list(text = buf, lines = buf_lines)
      buf <- NULL; buf_lines <- integer(0)
    }
  }
  # A leftover buffer means the final record had an unbalanced quote; keep it as
  # a record (it will parse defensively + be flagged) rather than dropping it.
  if (!is.null(buf)) {
    records[[length(records) + 1L]] <- list(text = buf, lines = buf_lines)
  }
  records
}

# .record_fields(text, delim) -- split one logical record into its fields,
# respecting quotes. Falls back to a quote-blind split if the record is so
# malformed that read.table refuses it, so a single bad record never crashes the
# reader (the field count will then differ from expected -> flagged malformed).
.record_fields <- function(text, delim) {
  fv <- safe(utils::read.table(
    text = text, sep = delim, header = FALSE, quote = "\"",
    colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = character(0), comment.char = "", fill = TRUE, flush = FALSE,
    strip.white = FALSE), NULL)
  if (is.null(fv) || nrow(fv) == 0) {
    return(strsplit(text, delim, fixed = TRUE)[[1]])
  }
  as.character(unlist(fv[1, , drop = TRUE]))
}

# read_delimited(input, template) -> list(table, source_lines, source_spans,
#   header_line_no, raw, field_counts, expected_fields, n_data_lines)
read_delimited <- function(input, template) {
  lines <- input$lines %||% character(0)
  delim <- template$delimiter %||% ","

  empty <- list(
    table = data.frame(), source_lines = integer(0), source_spans = list(),
    header_line_no = NA_integer_, raw = character(0),
    field_counts = integer(0), expected_fields = NA_integer_, n_data_lines = 0L)

  hidx <- locate_header(lines, template)
  if (is.na(hidx)) return(empty)

  header_line <- lines[hidx]
  header_names <- .record_fields(header_line, delim)
  expected_fields <- length(header_names)

  data_idx_all <- if (hidx < length(lines)) seq.int(hidx + 1L, length(lines)) else integer(0)
  # Keep every non-empty physical data line (no silent drops); blanks ignored.
  data_idx <- data_idx_all[nzchar(trimws(lines[data_idx_all]))]
  n_data_lines <- length(data_idx)

  records <- .split_records(lines[data_idx], data_idx)
  nrec <- length(records)

  # Build a rectangular character matrix: each record normalised to exactly
  # expected_fields cells so a ragged record cannot shift its neighbours.
  mat <- matrix("", nrow = nrec, ncol = expected_fields)
  field_counts <- integer(nrec)
  raw <- character(nrec)
  source_lines <- integer(nrec)
  source_spans <- vector("list", nrec)
  for (r in seq_len(nrec)) {
    rec <- records[[r]]
    fv <- .record_fields(rec$text, delim)
    field_counts[r] <- length(fv)
    if (length(fv) >= expected_fields) {
      mat[r, ] <- fv[seq_len(expected_fields)]
    } else if (length(fv) > 0) {
      mat[r, seq_along(fv)] <- fv
    }
    raw[r] <- rec$text
    source_lines[r] <- rec$lines[1]
    source_spans[[r]] <- rec$lines
  }

  df <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- header_names

  list(
    table = df,
    source_lines = source_lines,
    source_spans = source_spans,
    header_line_no = hidx,
    raw = raw,
    field_counts = field_counts,
    expected_fields = as.integer(expected_fields),
    n_data_lines = n_data_lines
  )
}
