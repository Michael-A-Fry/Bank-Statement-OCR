# read_input.R -- dispatch by file extension to the right reader and compute
# the content hash. Returns a uniform `input` object (build-contract section 6).

# read_excel_input(path) -- stub .xlsx reader (openxlsx/readxl). Reads the first
# sheet as character. Cell parsing into the core schema is template-driven.
read_excel_input <- function(path) {
  tbl <- NULL
  if (requireNamespace("readxl", quietly = TRUE)) {
    tbl <- safe(as.data.frame(readxl::read_excel(path, col_types = "text"),
                              stringsAsFactors = FALSE))
  }
  list(table = tbl)
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
    ocr = pdf$ocr,
    ocr_conf = pdf$ocr_conf
  )
}

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
  } else {
    stop(sprintf("unsupported file extension: '%s'", ext))
  }
  input
}
