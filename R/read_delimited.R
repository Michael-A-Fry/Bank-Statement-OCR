# read_delimited.R -- robust base-R delimited reader (CSV/TSV/TDV/TXT). Parses one logical record at a
# time, so a single malformed record can never shift its neighbours' columns or hide a lost row.
# Guarantees: NO silent drops (`n_data_lines` lets reconcile prove every record became a row); a
# RECTANGULAR parse; and provenance that survives multi-line records.

# Number of double-quote characters. A CSV record is complete when the total across accumulated
# physical lines is even: escaped quotes contribute two, an open quoted field leaves it odd.
.count_dquotes <- function(s) {
  m <- gregexpr('"', s, fixed = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) 0L else length(m)
}

# TRUE when a physical data line holds nothing but separators and whitespace - spreadsheet padding.
# Ignoring them is not a silent drop: such a line carries no date, amount, description or reference,
# while keeping them produced 24 phantom rows all flagged `malformed`, BURYING a genuinely malformed
# row. The count comes back as `n_padding_lines`.
.is_padding_line <- function(ln, delim) {
  stripped <- gsub(delim, "", as.character(ln), fixed = TRUE)
  !nzchar(gsub("[[:space:]]", "", stripped))
}

# Coalesce physical data lines into logical CSV records respecting quoted fields.
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
  # A leftover buffer means the final record had an unbalanced quote; keep it as a record - it will
  # parse defensively and be flagged - rather than dropping it.
  if (!is.null(buf)) {
    records[[length(records) + 1L]] <- list(text = buf, lines = buf_lines)
  }
  records
}

# Split one logical record into fields, respecting quotes. Falls back to a quote-blind split if the
# record is so malformed that read.table refuses it, so a single bad record never crashes the reader.
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

# read_delimited(input, template) -> list(table, source_lines, source_spans, header_line_no, raw,
# field_counts, expected_fields, n_data_lines, n_padding_lines).
read_delimited <- function(input, template) {
  lines <- input$lines %||% character(0)

  empty <- list(
    table = data.frame(), source_lines = integer(0), source_spans = list(),
    header_line_no = NA_integer_, raw = character(0),
    field_counts = integer(0), expected_fields = NA_integer_, n_data_lines = 0L,
    n_padding_lines = 0L)

  hidx <- locate_header(lines, template)
  if (is.na(hidx)) return(empty)

  header_line <- lines[hidx]
  # The delimiter is resolved from the HEADER LINE, exactly as detection resolves it, so the reader
  # and the detector cannot disagree about how this file splits.
  delim <- resolve_delimiter(header_line, template)
  header_names <- .record_fields(header_line, delim)
  expected_fields <- length(header_names)

  data_idx_all <- if (hidx < length(lines)) seq.int(hidx + 1L, length(lines)) else integer(0)
  # Keep every physical data line carrying CONTENT. Blank lines and spreadsheet padding are excluded
  # from n_data_lines too, so reconcile's completeness proof compares like with like.
  data_idx <- data_idx_all[nzchar(trimws(lines[data_idx_all]))]
  padding <- if (length(data_idx)) .is_padding_line(lines[data_idx], delim) else logical(0)
  n_padding_lines <- sum(padding)
  data_idx <- data_idx[!padding]
  n_data_lines <- length(data_idx)

  records <- .split_records(lines[data_idx], data_idx)
  nrec <- length(records)

  # A rectangular character matrix: each record normalised to exactly expected_fields cells so a
  # ragged record cannot shift its neighbours.
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
    n_data_lines = n_data_lines,
    n_padding_lines = as.integer(n_padding_lines)
  )
}
