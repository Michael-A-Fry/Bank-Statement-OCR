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
    sections = pdf$sections,
    redactions = pdf$redactions
  )
}

# read_input(path, redaction_rects) -> list(kind, path, sha256, lines, table,
# pages, meta). `redaction_rects` lets the caller feed overlay rectangles (per
# build-contract 11.2) so text under a drawn redaction is dropped before it ever
# leaves the reader; NULL keeps the text-layer marker sweep only.
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
    input$meta$page_count <- x$page_count
    input$meta$sections <- x$sections
    input$meta$redactions <- x$redactions
  } else {
    stop(sprintf("unsupported file extension: '%s'", ext))
  }
  input
}
