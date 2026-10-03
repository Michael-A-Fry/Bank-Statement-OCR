# identify.R -- WHAT A FILE IS, AND WHICH TEMPLATE THE CONVERSION WILL PICK, before
# anything is converted.
#
# Asked for in these words: "we NEED a backup to be able to specify that isn't a
# tiny little click 'did it do it wrong'. I want it to pre fill a table with the
# upload, its type, and its guessed template with easy dropdown to change it. Same
# thing for single statement." The Convert screen calls identify_file() once per
# uploaded file and shows the answer in a row the analyst can change before pressing
# Convert -- so a wrong pick is caught BEFORE the run, not discovered after it.
#
# THE GUESS IS THE CONVERSION'S OWN ANSWER, NOT AN APPROXIMATION OF IT. A table that
# says "ANZ" and a conversion that then reads the file as Westpac is worse than no
# table. So the guess is made by the SAME detect_statement() on the SAME input:
#   - a text PDF: convert_statement's input$pages IS pdftools::pdf_text() (read_pdf
#     keeps the raw text layer for every page with a text layer), and detection
#     reads nothing else of a PDF -- page text for the phrases, page 1 for the bank
#     name. The expensive parts of a read (word boxes, the ink scan, OCR) are not
#     inputs to detection and are skipped: a 400-page PDF identifies in under a
#     second where it converts in forty.
#   - CSV / TSV / Excel: read_input() itself. Those reads are cheap already.
#   - the file NAME the conversion will see (filename_regex is a tie-breaker), not
#     the upload's temporary path.
# The caller passes the same template set the conversion will load.
#
# A SCANNED PDF IS SAID TO BE ONE, AND NOT GUESSED. Its text only exists after OCR,
# at about nine seconds a page, and OCR-ing a case folder to fill in a table would
# hold the server for minutes. It is detected while it converts, exactly as now --
# and the analyst can still choose its template in the same row.
#
# Never throws: a file it cannot open is a row that says so.

# The template FORMAT that can read a file of this extension. A PDF template cannot
# read a CSV, so the row's dropdown offers only the templates that could.
.IDENT_FORMAT <- c(pdf = "pdf", csv = "delimited", tsv = "delimited",
                   tdv = "delimited", txt = "delimited",
                   xlsx = "excel", xlsm = "excel")

# identify_file(path, templates, name) -> list
#   ext        lower-case extension of `name`
#   format     the template format that can read it ("pdf" / "delimited" / "excel")
#   kind       "PDF", "Scanned PDF", "CSV", "Tab-delimited", "Excel" -- for the screen
#   pages      page count (PDF only, else NA)
#   state      sure | close | tie | none | scanned | unreadable | unsupported_type
#   guess      the template id the conversion will use when the row is left alone
#              (NA for none / scanned / unreadable)
#   runner_up  the template that came closest behind it (NA when none)
#   detail     the detector's own line, for a tooltip; never the headline
identify_file <- function(path, templates, name = basename(path)) {
  ext <- tolower(tools::file_ext(as.character(name %||% "")[1]))
  fmt <- unname(.IDENT_FORMAT[ext])
  out <- list(ext = ext, format = if (length(fmt) && !is.na(fmt)) fmt else NA_character_,
              kind = .ident_kind(ext, FALSE), pages = NA_integer_,
              state = "none", guess = NA_character_, runner_up = NA_character_,
              detail = NA_character_)
  if (is.na(out$format)) { out$state <- "unsupported_type"; return(out) }
  if (!length(path) || is.na(path) || !file.exists(path)) {
    out$state <- "unreadable"; return(out)
  }

  input <- if (identical(out$format, "pdf")) {
    tx <- safe(suppressMessages(pdftools::pdf_text(path)), NULL)
    if (is.null(tx) || !length(tx)) { out$state <- "unreadable"; return(out) }
    out$pages <- length(tx)
    chars <- nchar(gsub("[[:space:]]", "", paste(tx, collapse = ""), useBytes = TRUE),
                   type = "bytes")
    # the conversion's own threshold for "this page is a picture", per page
    if (chars / length(tx) < PARAM_OCR_MIN_CHARS) {
      out$kind <- .ident_kind(ext, TRUE); out$state <- "scanned"; return(out)
    }
    list(kind = "pdf", pages = tx)
  } else {
    safe(read_input(path), NULL)
  }
  if (is.null(input)) { out$state <- "unreadable"; return(out) }
  # The conversion refuses some files BEFORE detection (a CSV with no table in it,
  # an empty workbook). Saying "matched BNZ" about one of those would be the table
  # promising a conversion that is never going to happen -- measured on two of the
  # suite's own fixtures before this line existed. Same check, same words.
  why <- safe(.unreadable_reason(input, templates), NULL)
  if (!is.null(why)) { out$state <- "unreadable"; out$detail <- why; return(out) }
  # filename_regex sees the name the conversion will see, not the upload's temp path
  input$path <- file.path(dirname(path), as.character(name)[1])

  det <- safe(detect_statement(input, templates), NULL)
  if (is.null(det)) { out$state <- "unreadable"; return(out) }
  out$detail <- as.character(det$detail_plain %||% det$detail %||% NA_character_)[1]
  tied <- as.character(det$tied %||% character(0))
  if (isTRUE(det$matched)) {
    out$guess <- det$template_id
    out$runner_up <- as.character(det$runner_up %||% NA_character_)[1]
    # the conversion's own "thin margin" rule (convert_statement): won by one
    # phrase over a template that also fitted means it will be held for review
    thin <- is.finite(det$margin %||% Inf) && det$margin <= 1 && !is.na(out$runner_up)
    out$state <- if (thin) "close" else "sure"
  } else if (length(tied) >= 2L) {
    # the conversion reads a tie with the first tied template and holds the run for
    # review -- so that template IS the guess, and the row says it is a close call
    out$guess <- tied[1]; out$runner_up <- tied[2]; out$state <- "tie"
  }
  out
}

.ident_kind <- function(ext, scanned) {
  if (identical(ext, "pdf")) return(if (isTRUE(scanned)) "Scanned PDF" else "PDF")
  switch(ext, csv = "CSV", tsv = , tdv = "Tab-delimited", txt = "Text",
         xlsx = , xlsm = "Excel", toupper(ext))
}

# template_choices(templates, format) -> named character vector, or a named list of
# them grouped by bank, for a selectInput. Only templates that can read this FORMAT.
# Labelled with the template's own display name; a template built here says so,
# because "the one Sam built last week" is how an analyst knows it.
template_choices <- function(templates, format) {
  if (!length(templates)) return(list())
  keep <- vapply(templates, function(t)
    identical(t$format %||% "delimited", format), logical(1))
  ts <- templates[keep]
  if (!length(ts)) return(list())
  ids <- names(ts)
  bank <- vapply(ts, function(t) as.character(t$bank %||% "Other")[1], character(1))
  bank[is.na(bank) | !nzchar(bank)] <- "Other"
  lab <- vapply(ts, function(t) {
    nm <- safe(template_display_name(t), NULL) %||% t$id %||% "template"
    # every choice here is a statement, and a closed dropdown has room for ~25
    # characters: "ANZ everyday", not "ANZ everyday statement" cut off mid-word
    nm <- sub(" statement$", "", nm)
    if (identical(t$origin %||% "default", "user")) paste(nm, "(built here)") else nm
  }, character(1))
  # two templates with the same bank and type (a variant, a correction) would read
  # as one choice twice; the id is what tells them apart
  dup <- lab %in% lab[duplicated(lab)]
  lab[dup] <- sprintf("%s (%s)", lab[dup], ids[dup])
  o <- order(tolower(bank), tolower(lab))
  ids <- ids[o]; bank <- bank[o]; lab <- lab[o]
  split_ids <- split(stats::setNames(ids, lab), factor(bank, levels = unique(bank)))
  lapply(split_ids, function(v) v)
}
