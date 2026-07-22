# read_input.R -- dispatch by file extension to the right reader and compute
# the content hash. Returns a uniform `input` object (build-contract section 6).

# read_excel_input(path) -- .xlsx reader (readxl). Real workbooks are messy, so
# three deterministic clean-ups happen here, before the engine sees the table:
#   sheet  - pick the sheet that actually holds the transaction table (a header
#            row with a date + a money-ish name and data rows under it) instead
#            of blindly taking sheet 1;
#   header - find the real header row within the first 30 rows, skipping the
#            logo / account-info preamble rows exports often carry;
#   dates  - a date-NAMED column stored as Excel serial numbers (e.g. 46023) is
#            turned back into ISO dates. Only date-named columns: an amount
#            column full of five-digit values must never become dates.
# A clean single-sheet file (header on row 1) reads exactly as before, so the
# shipped generic template and its golden test are unaffected.
read_excel_input <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) return(list(table = NULL))
  sheets <- safe(readxl::excel_sheets(path), character(0))
  if (!length(sheets)) return(list(table = NULL))
  moneyish <- function(v) grepl(
    "amount|balance|debit|credit|withdraw|deposit|paid in|paid out|money in|money out|value",
    tolower(v %||% ""))
  best <- NULL
  for (sh in sheets) {
    raw <- safe(suppressMessages(as.data.frame(
      readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = "text",
                         n_max = 2000, .name_repair = "minimal"),
      stringsAsFactors = FALSE)))
    if (is.null(raw) || !nrow(raw) || !ncol(raw)) next
    hdr <- NA_integer_
    for (r in seq_len(min(30L, nrow(raw)))) {
      cells <- trimws(as.character(unlist(raw[r, ], use.names = FALSE)))
      cells <- cells[!is.na(cells) & nzchar(cells)]
      if (length(cells) < 2 || any(nchar(cells) > 60)) next
      if (any(grepl("date", tolower(cells))) && any(moneyish(cells))) { hdr <- r; break }
    }
    if (is.na(hdr) || hdr >= nrow(raw)) next
    score <- nrow(raw) - hdr   # the sheet with the most rows under its header wins
    if (is.null(best) || score > best$score)
      best <- list(sheet = sh, raw = raw, hdr = hdr, score = score)
  }
  # No sheet looked like a transaction table -> old behaviour (sheet 1 as-is),
  # so a plain grid with unusual header names still reads.
  if (is.null(best)) {
    tbl <- safe(as.data.frame(readxl::read_excel(path, col_types = "text"),
                              stringsAsFactors = FALSE))
    return(list(table = tbl))
  }
  raw <- best$raw
  # Preamble = every row ABOVE the header (bank / account / period / balances a
  # statement export prints before the table). Kept as text so parse_statement
  # can mine statement-level metadata from it -- and ONLY from it, never the
  # transaction rows -- exactly as the PDF path does.
  preamble <- if (best$hdr > 1) vapply(seq_len(best$hdr - 1L), function(r)
    paste(trimws(as.character(unlist(raw[r, ], use.names = FALSE))), collapse = " "),
    character(1)) else character(0)
  h <- trimws(as.character(unlist(raw[best$hdr, ], use.names = FALSE)))
  blank <- which(is.na(h) | !nzchar(h))
  if (length(blank)) h[blank] <- paste0("col", blank)
  tbl <- raw[seq.int(best$hdr + 1L, nrow(raw)), , drop = FALSE]
  names(tbl) <- make.unique(h)
  rownames(tbl) <- NULL
  # drop fully-empty spacer rows (merged cells / section gaps)
  keep <- vapply(seq_len(nrow(tbl)), function(i) {
    rr <- as.character(unlist(tbl[i, ], use.names = FALSE))
    any(!is.na(rr) & nzchar(trimws(rr)))
  }, logical(1))
  tbl <- tbl[keep, , drop = FALSE]
  # serial-date fix, date-named columns only
  for (cn in names(tbl)) {
    if (!grepl("date", tolower(cn))) next
    v <- trimws(as.character(tbl[[cn]]))
    num <- suppressWarnings(as.numeric(v))
    ok <- !is.na(num) & num > 20000 & num < 80000
    filled <- !is.na(v) & nzchar(v)
    if (sum(filled) > 0 && sum(ok) >= 0.6 * sum(filled)) {
      # The DATE is the integer part of an Excel serial; the fraction is the time.
      # floor() (never round()) so a noon (.5) serial can't tip to the next day --
      # round() uses banker's rounding, which would shift 45000.5 -> 45000 but
      # 45001.5 -> 45002.
      v[ok] <- format(as.Date(floor(num[ok]), origin = "1899-12-30"), "%Y-%m-%d")
      tbl[[cn]] <- v
    }
  }
  list(table = tbl, sheet = best$sheet, header_row = best$hdr, preamble = preamble)
}

# read_pdf_input(path) -- PDF reader delegating to read_pdf() (R/read_pdf.R):
# page text, positioned + redaction-guarded word boxes, detected sections, and a
# per-page redaction summary. Extraction only -- never crashes; degrades to an
# empty structure when pdftools is missing or the file is unreadable.
read_pdf_input <- function(path, redaction_rects = NULL,
                           markers = pdf_redaction_markers(),
                           anchors = pdf_section_anchors()) {
  pdf <- safe(read_pdf(path, redaction_rects = redaction_rects,
                       markers = markers, anchors = anchors), NULL)
  if (is.null(pdf)) {
    return(list(pages = NULL, words = list(), page_count = NA_integer_,
                sections = NULL, redactions = NULL))
  }
  list(
    pages = if (isTRUE(pdf$ok)) pdf$pages else NULL,
    words = pdf$words,
    page_count = pdf$page_count,
    page_width = pdf$page_width,
    page_height = pdf$page_height,
    sections = pdf$sections,
    redactions = pdf$redactions,
    redaction_scan_incomplete = pdf$redaction_scan_incomplete %||% 0L,
    ocr = pdf$ocr,
    ocr_conf = pdf$ocr_conf
  )
}

# .INPUT_CACHE -- read_input can be EXPENSIVE (a scanned PDF is OCR'd and every
# page is rendered for the redaction scan -- tens of seconds). The GUI reads the
# SAME uploaded file several times across one flow (convert, then draft, preview,
# x-ray, guided columns...), so without a cache a scanned statement is OCR'd 4-5x
# and the front end appears to hang for minutes. read_input is deterministic in the
# file's CONTENT, so we key the parsed result by its SHA-256: the first read does
# the work, every later read of the same bytes is instant. Bounded to a handful of
# recent files so a long session can't grow memory without limit.
.INPUT_CACHE <- new.env(parent = emptyenv())
.INPUT_CACHE_MAX <- 12L

# read_input(path, redaction_rects) -> list(kind, path, sha256, lines, table,
# pages, meta). The tool never redacts; statements arrive already redacted.
# `redaction_rects` is an optional way to tell the reader where a redaction
# ALREADY sits (belt-and-braces alongside the automatic detection of rasterised
# black boxes), so any text a supplied box covers is not emitted; NULL relies on
# the text-layer marker sweep and the scanned-page black-box detector.
read_input <- function(path, redaction_rects = NULL) {
  if (!file.exists(path)) stop(sprintf("input file not found: %s", path))
  ext <- tolower(tools::file_ext(path))
  sha <- file_sha256(path)
  # Cache hit: same content already parsed. Return it, but point $path at the
  # caller's CURRENT file (identical bytes, but a fresh temp path in the GUI) so
  # any downstream re-read of $path still resolves. Only when no redaction_rects
  # were supplied (those change what text is emitted, so they bypass the cache).
  cacheable <- is.null(redaction_rects) && !is.null(sha) && !is.na(sha)
  if (cacheable && exists(sha, envir = .INPUT_CACHE, inherits = FALSE)) {
    cached <- get(sha, envir = .INPUT_CACHE, inherits = FALSE)
    cached$path <- path
    return(cached)
  }
  input <- list(kind = NA_character_, path = path, sha256 = sha,
                lines = NULL, table = NULL, pages = NULL,
                meta = list(ext = ext))

  if (ext %in% c("csv", "tsv", "tdv", "txt")) {
    input$kind <- "delimited"
    input$lines <- safe_readlines(path)
  } else if (ext %in% c("xlsx", "xlsm")) {
    input$kind <- "excel"
    x <- read_excel_input(path)
    input$table <- x$table
    input$meta$preamble <- x$preamble %||% character(0)
  } else if (ext == "pdf") {
    input$kind <- "pdf"
    x <- read_pdf_input(path, redaction_rects = redaction_rects)
    input$pages <- x$pages
    input$words <- x$words
    input$page_width <- x$page_width
    input$page_height <- x$page_height
    input$page_ocr <- x$ocr    # per-page: was this page machine-read (OCR)?
    input$meta$page_count <- x$page_count
    input$meta$sections <- x$sections
    input$meta$redactions <- x$redactions
    ocr <- x$ocr %||% logical(0); conf <- x$ocr_conf %||% numeric(0)
    input$meta$ocr_pages <- sum(ocr)
    on_conf <- conf[which(ocr)]; on_conf <- on_conf[!is.na(on_conf)]
    input$meta$ocr_min_conf <- if (length(on_conf)) min(on_conf) else NA_real_
    input$meta$redaction_scan_incomplete <- x$redaction_scan_incomplete %||% 0L
  } else {
    stop(sprintf("unsupported file extension: '%s'", ext))
  }
  if (cacheable) {
    # crude bound: reset if we're holding too many distinct files (a session works
    # with a handful, so this rarely fires -- it just caps memory).
    if (length(ls(.INPUT_CACHE)) >= .INPUT_CACHE_MAX)
      rm(list = ls(.INPUT_CACHE), envir = .INPUT_CACHE)
    assign(sha, input, envir = .INPUT_CACHE)
  }
  input
}

# clear_input_cache() -- drop all cached inputs (e.g. after a redaction re-run or
# to free memory). read_input rebuilds on the next call.
clear_input_cache <- function() rm(list = ls(.INPUT_CACHE), envir = .INPUT_CACHE)
