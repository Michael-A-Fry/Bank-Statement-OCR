# identify.R -- WHAT A FILE IS, AND WHICH BANK ISSUED IT, before anything is
# converted.
#
# The Convert screen calls identify_file() once per uploaded file and shows the
# answer in a row the analyst can change before pressing Convert: the file's kind
# and pages, and its BANK, pre-filled from the statement itself (bank_identify,
# R/bank_identity.R -- the holder's account number in the official branch
# register, then the bank's legal name, website, phone and brand words). The bank
# is the only choice there is: which of that bank's learned layouts fits is
# decided by the reader at conversion, from the statement's content.
#
#   - a text PDF: its text layer only (pdftools::pdf_text). The expensive parts of
#     a read (word boxes, the ink scan, OCR) are not needed to name the bank, so a
#     400-page PDF identifies in about a second.
#   - CSV / TSV / Excel: read_input() itself; those reads are cheap already.
#
# A SCANNED PDF IS SAID TO BE ONE. Its text only exists after OCR, at seconds a
# page, so its bank is found by a background job (identify_scan, R/jobs.R task
# "identify_scans") and the row fills in when it is done.
#
# Convert should be given the bank only when the person CHANGED it: left alone,
# conversion pre-fills it from the full reading of the statement itself.
#
# Never throws: a file it cannot open is a row that says so.

# The reader that can read a file of this extension.
.IDENT_FORMAT <- c(pdf = "pdf", csv = "delimited", tsv = "delimited",
                   tdv = "delimited", txt = "delimited",
                   xlsx = "excel", xlsm = "excel", xls = "excel")

# identify_file(path, name) -> list
#   ext          lower-case extension of `name`
#   format       "pdf" / "delimited" / "excel"
#   kind         "PDF", "Scanned PDF", "CSV", "Tab-delimited", "Excel" -- for the screen
#   pages        page count (PDF only, else NA)
#   state        ready | scanned | scanned_no_ocr | unreadable | unsupported_type
#   bank         the institution id the statement names (dictionaries/nz_banks.yaml), or NA
#   bank_display the bank as people know it ("ANZ"), or NA
#   bank_code    the holder account's two-digit bank code, or NA (never the number)
#   confidence   high | medium | low | unknown -- how sure the pre-fill is
#   ask          TRUE when the statement does not settle the bank and a person should pick
#   detail       one plain sentence for a tooltip: why this bank, or why none
identify_file <- function(path, name = basename(path)) {
  ext <- tolower(tools::file_ext(as.character(name %||% "")[1]))
  fmt <- unname(.IDENT_FORMAT[ext])
  out <- list(ext = ext, format = if (length(fmt) && !is.na(fmt)) fmt else NA_character_,
              kind = .ident_kind(ext, FALSE), pages = NA_integer_, state = "ready",
              bank = NA_character_, bank_display = NA_character_, bank_code = NA_character_,
              confidence = "unknown", ask = TRUE, detail = NA_character_)
  if (is.na(out$format)) { out$state <- "unsupported_type"; return(out) }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    out$state <- "unreadable"; return(out)
  }
  input <- if (identical(out$format, "pdf")) {
    tx <- safe(suppressMessages(pdftools::pdf_text(path)), NULL)
    if (is.null(tx) || !length(tx)) {
      out$state <- "unreadable"
      out$detail <- "This PDF could not be opened - it may be damaged, password-protected, or not really a PDF."
      return(out)
    }
    out$pages <- length(tx)
    chars <- nchar(gsub("[[:space:]]", "", paste(tx, collapse = ""), useBytes = TRUE),
                   type = "bytes")
    # the conversion's own threshold for "this page is a picture", per page
    if (chars / length(tx) < PARAM_OCR_MIN_CHARS) {
      out$kind <- .ident_kind(ext, TRUE)
      # A scan on a server with no OCR software cannot be read at all -- say so
      # NOW, in the row, not after a conversion that comes back empty.
      out$state <- if (isTRUE(safe(ocr_available(), FALSE))) "scanned" else "scanned_no_ocr"
      return(out)
    }
    list(kind = "pdf", pages = tx)
  } else {
    safe(read_input(path), NULL)
  }
  if (is.null(input)) {
    out$state <- "unreadable"
    out$detail <- "This file could not be opened - it may be damaged, or not really the type its name says."
    return(out)
  }
  # The conversion refuses some files before reading (a CSV with no table in it,
  # an empty workbook). Same check, same words.
  why <- safe(.unreadable_reason(input), NULL)
  if (!is.null(why)) { out$state <- "unreadable"; out$detail <- why; return(out) }
  .ident_bank(out, bank_identify(input))
}

# .ident_bank(out, ident) -- the bank fields of a row, from bank_identify().
.ident_bank <- function(out, ident) {
  pick <- bank_pick(ident, NULL)
  out$bank <- as.character(pick$bank %||% NA_character_)[1]
  out$bank_display <- if (is.na(out$bank)) NA_character_ else as.character(ident$display %||% out$bank)[1]
  out$bank_code <- as.character(ident$bank_code %||% NA_character_)[1]
  out$confidence <- as.character(ident$confidence %||% "unknown")[1]
  out$ask <- isTRUE(pick$ask)
  out$detail <- as.character(pick$why %||% ident$why %||% NA_character_)[1]
  out
}

.ident_kind <- function(ext, scanned) {
  if (identical(ext, "pdf")) return(if (isTRUE(scanned)) "Scanned PDF" else "PDF")
  switch(ext, csv = "CSV", tsv = , tdv = "Tab-delimited", txt = "Text",
         xlsx = , xlsm = , xls = "Excel", toupper(ext))
}

# bank_choices(dir) -> named character vector (label = display name, value = id)
# for the Bank dropdown: every New Zealand bank in dictionaries/nz_banks.yaml,
# plus any bank the layout store holds that the list does not know (a bank named
# by hand on this box). Sorted by name, without regard to case.
bank_choices <- function(dir = layouts_dir()) {
  ref <- safe(.bi_ref(), NULL)
  ids <- if (is.null(ref)) character(0) else names(ref$display)
  # The register's stand-ins ("a business that banks through ANZ") are not banks
  # a person picks.
  if (!is.null(ref$pseudo)) ids <- ids[!(ids %in% names(ref$pseudo)[ref$pseudo %in% TRUE])]
  lab <- if (is.null(ref)) character(0) else vapply(ids, function(i) as.character(ref$display[[i]])[1], "")
  lb <- safe(layouts_banks(dir), NULL)
  if (is.data.frame(lb) && nrow(lb)) {
    extra <- !(lb$slug %in% ids)
    ids <- c(ids, lb$slug[extra]); lab <- c(lab, lb$bank[extra])
  }
  if (!length(ids)) return(character(0))
  o <- order(tolower(lab), ids, method = "radix")
  stats::setNames(ids[o], lab[o])
}

# identify_scan(path, name) -> list(state, bank, bank_display, bank_code,
# confidence, ask, detail) for a SCANNED PDF: its first two pages are read as
# pictures (OCR, a few seconds each) and the bank is identified from them. TWO,
# not one: page 1 is often the summary and the holder's account number or the
# bank's legal name can sit on page 2.
#
# Slow enough to run in a background job (R/jobs.R, task "identify_scans"), never
# in the app's own process.
#   scan_ready      the first pages were read; the bank fields say what they show
#   scanned         they could not be read; the bank is found while it converts
#   scanned_no_ocr  this server has no OCR software
identify_scan <- function(path, name = basename(path)) {
  out <- list(state = "scanned", bank = NA_character_, bank_display = NA_character_,
              bank_code = NA_character_, confidence = "unknown", ask = TRUE, detail = NA_character_)
  if (!isTRUE(safe(ocr_available(), FALSE))) { out$state <- "scanned_no_ocr"; return(out) }
  np <- suppressWarnings(as.integer(safe(pdftools::pdf_info(path)$pages, 1L)))
  np <- if (length(np) && !is.na(np) && np >= 1L) min(2L, np) else 1L
  pg <- lapply(seq_len(np), function(p) safe(ocr_pdf_page(path, p), NULL))
  txt <- vapply(pg, function(r) if (is.null(r) || !isTRUE(r$ok) || !length(r$text)) "" else paste(r$text, collapse = "\n"), "")
  if (!any(nzchar(trimws(txt)))) {
    out$detail <- "The scan could not be read as a picture; the bank is found while it converts."
    return(out)
  }
  words <- lapply(pg, function(r) if (is.null(r) || !isTRUE(r$ok)) NULL else r$words)
  input <- list(kind = "pdf", pages = txt, words = words,
                page_height = vapply(pg, function(r) as.numeric(r$height %||% NA_real_)[1], 0),
                path = file.path(dirname(path), as.character(name)[1]))
  out <- .ident_bank(out, bank_identify(input))
  out$state <- "scan_ready"
  out
}
