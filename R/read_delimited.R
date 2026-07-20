# read_delimited.R -- robust base-R delimited reader (CSV/TSV/TDV/TXT).
# Honours an optional preamble skip and returns a character-only data.frame
# with per-row source line numbers for provenance. Never drops a data row.

# read_delimited(input, template) ->
#   list(table, source_lines, header_line_no, raw, field_counts, expected_fields)
read_delimited <- function(input, template) {
  lines <- input$lines %||% character(0)
  delim <- template$delimiter %||% ","

  # Locate header (preamble.header_regex, else first non-empty line).
  hidx <- 1L
  hr <- template$preamble$header_regex
  if (!is.null(hr) && nzchar(hr)) {
    m <- grep(hr, lines, perl = TRUE)
    if (length(m)) hidx <- m[1]
  } else {
    nz <- which(nzchar(trimws(lines)))
    if (length(nz)) hidx <- nz[1]
  }

  header_line <- lines[hidx]
  data_idx_all <- if (hidx < length(lines)) seq.int(hidx + 1L, length(lines)) else integer(0)
  # Keep every non-empty data line (no silent drops); blank lines are ignored.
  source_lines <- data_idx_all[nzchar(trimws(lines[data_idx_all]))]

  txt <- c(header_line, lines[source_lines])
  df <- utils::read.table(
    text = txt, sep = delim, header = TRUE, quote = "\"",
    colClasses = "character", stringsAsFactors = FALSE,
    check.names = FALSE, na.strings = character(0),
    comment.char = "", fill = TRUE, flush = TRUE, strip.white = FALSE
  )

  # Field-count integrity per row (respecting quotes) for malformed detection.
  expected_fields <- as.integer(utils::count.fields(
    textConnection(header_line), sep = delim, quote = "\"")[1])
  field_counts <- if (length(source_lines)) {
    vapply(lines[source_lines], function(ln) {
      cf <- suppressWarnings(utils::count.fields(
        textConnection(ln), sep = delim, quote = "\""))
      if (length(cf) == 0 || is.na(cf[1])) NA_integer_ else as.integer(cf[1])
    }, integer(1), USE.NAMES = FALSE)
  } else integer(0)

  list(
    table = df,
    source_lines = source_lines,
    header_line_no = hidx,
    raw = lines[source_lines],
    field_counts = field_counts,
    expected_fields = expected_fields
  )
}
