# Tests for the PDF text path (R/read_pdf.R): extraction, section detection,
# and the forensic redaction guard (build-contract sections 6, 11.2).

SAMPLE_PDF <- "samples/raw/anz/anz_card_summary_sample.pdf"

test_that("pdftools is available in this environment", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE),
              "pdftools not installed")
  succeed()
})

test_that("read_pdf extracts pages and word boxes from a real specimen", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- read_pdf(fixture(SAMPLE_PDF))
  expect_true(pdf$ok)
  expect_gte(pdf$page_count, 1L)
  expect_equal(length(pdf$pages), pdf$page_count)
  # some text was actually extracted
  expect_true(any(nchar(pdf$pages) > 0))
  expect_true(grepl("CARD SUMMARY", paste(pdf$pages, collapse = " "),
                    ignore.case = TRUE))
  # per-page word boxes carry positional geometry
  w1 <- pdf$words[[1]]
  expect_true(all(c("x", "y", "width", "height", "text", "ocr_conf") %in%
                    names(w1)))
  expect_gt(nrow(w1), 0L)
  # a clean specimen must not be spuriously redacted
  expect_equal(sum(pdf$redactions$redacted_words), 0L)
})

test_that("section anchors are detected in the specimen", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- read_pdf(fixture(SAMPLE_PDF))
  expect_s3_class(pdf$sections, "data.frame")
  expect_true("YOUR CARD SUMMARY" %in% pdf$sections$section)
  expect_true(all(c("section", "page", "line_no") %in% names(pdf$sections)))
})

test_that("read_input wires .pdf through read_pdf (extraction only)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  input <- read_input(fixture(SAMPLE_PDF))
  expect_identical(input$kind, "pdf")
  expect_gte(input$meta$page_count, 1L)
  expect_equal(length(input$words), input$meta$page_count)
})

# ---- Document provenance (pdf_info) ---------------------------------------

test_that(".pdf_doc_info always returns the same shape, even with nothing to read", {
  # Downstream never has to ask "did pdf_info work?" -- the fields are always there.
  d <- .pdf_doc_info(NULL)
  expect_true(all(c("producer", "creator", "created", "modified", "encrypted",
                    "pdf_version") %in% names(d)))
  expect_true(is.na(d$producer) && is.na(d$created) && is.na(d$modified))
  # An info list with no keys / no timestamps degrades the same way.
  d2 <- .pdf_doc_info(list(keys = NULL, created = NULL, modified = NULL))
  expect_true(is.na(d2$producer) && is.na(d2$creator))
  # A blank Producer string is "not stated", not an empty-string value.
  d3 <- .pdf_doc_info(list(keys = list(Producer = "   ", Creator = "Acme")))
  expect_true(is.na(d3$producer))
  expect_identical(d3$creator, "Acme")
})

test_that("read_pdf records who wrote the document and when (#46)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- read_pdf(fixture(SAMPLE_PDF))
  d <- pdf$doc_info
  expect_true(is.list(d))
  # The specimen declares an Adobe toolchain and two different timestamps -- the
  # exact forensic signal pdf_info exists to expose.
  expect_match(d$producer, "Adobe", ignore.case = TRUE)
  expect_match(d$creator, "InDesign", ignore.case = TRUE)
  expect_match(d$created, "^[0-9]{4}-[0-9]{2}-[0-9]{2} ")
  expect_match(d$modified, "^[0-9]{4}-[0-9]{2}-[0-9]{2} ")
  expect_false(identical(d$created, d$modified))
  expect_false(isTRUE(d$encrypted))
  # No Title: it routinely carries the customer's name and this travels into the
  # diagnostics table.
  expect_false("title" %in% names(d))
})

test_that("read_input carries the PDF's provenance onto meta (#46)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  input <- read_input(fixture(SAMPLE_PDF))
  expect_true(is.list(input$meta$pdf_doc))
  expect_match(input$meta$pdf_doc$producer, "Adobe", ignore.case = TRUE)
})

# A scan we could not machine-read has to be REPORTED as that. read_pdf works it
# out, but the value has to survive the trip onto input$meta or convert_statement
# reads 0 and the loud "this is a scan, install the OCR tools" diagnostic silently
# reverts to the misleading "unknown layout - go build a template".
test_that("a scan on a machine with no OCR tools reaches input$meta (#54 wiring)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  scan <- fixture("samples/raw/tutorial/sample_everyday_scanned.pdf")
  skip_if_not(file.exists(scan))
  # Force the no-tooling path so this runs identically WITH or WITHOUT tesseract.
  old <- get("ocr_available", envir = globalenv())
  assign("ocr_available", function() FALSE, envir = globalenv())
  # The parsed input is cached by content hash; this stubbed read must not be left
  # in the cache for later tests (it has no OCR words), so clear it either side.
  clear_input_cache()
  res <- tryCatch({
    x <- read_pdf_input(scan)
    inp <- read_input(scan)
    list(x = x, inp = inp)
  }, finally = {
    assign("ocr_available", old, envir = globalenv())
    clear_input_cache()
  })
  expect_gte(res$x$scanned_no_ocr, 1L)
  expect_false(res$x$ocr_tools_available)
  expect_gte(res$inp$meta$scanned_no_ocr, 1L)
  expect_false(res$inp$meta$ocr_tools_available)
  # ...and that is exactly what convert_statement hands build_diagnostics.
  d <- build_diagnostics("unsupported",
         reading = list(outcome = "unread", why = "No line on any page prints a date with a figure beside it."),
         metadata = list(scanned_no_ocr = res$inp$meta$scanned_no_ocr %||% 0L,
                         ocr_tools = res$inp$meta$ocr_tools_available %||% TRUE))
  expect_true("scanned_no_ocr" %in% d$category)
  # the scan is the reason, not the reading: "not read" would send the person to
  # fix columns on a page that has no text at all
  expect_false("not_read" %in% d$category)
})

test_that("a readable PDF reports no un-OCR-able scan pages", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  input <- read_input(fixture(SAMPLE_PDF))
  expect_equal(input$meta$scanned_no_ocr, 0L)
  expect_type(input$meta$ocr_tools_available, "logical")
})

# ---- Forensic redaction guard --------------------------------------------

# ---------------------------------------------------------------------------
# A BOX OVER TEXT HIDES IT WHATEVER COLOUR THE BOX IS.
#
# detect_occluded_words used to ask one question of the rendered page: "is this word
# DARK". That catches a black redaction stripe and is colour-blind in the direction
# that matters most. A WHITE box over live text is one of the commonest failed
# redactions there is -- people do it in Word and Acrobat constantly -- and it hides
# an account number completely while being the least dark thing on the page.
#
# Measured on redaction_overlay_colours.pdf, where the SAME account number is covered
# four ways: the darkness test flagged BLACK and missed WHITE, GREY and YELLOW. Three
# numbers invisible on the statement arrived in the output with nothing saying so --
# so the analyst could not reconcile what they were given against what they could
# see, and if the box was somebody's attempt to protect third-party data the tool had
# quietly undone it.
#
# It now also asks whether the word can still be SEEN. Visible text is dark strokes
# on a lighter ground, so its greyscale range is wide; a word under an opaque fill of
# any colour is a flat patch and the range collapses.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# HOW LONG IS THIS GOING TO TAKE?
#
# A digital page costs 0.17s and a scanned page 9.3s -- measured, 55x apart -- so a
# 120-page scan is nineteen minutes behind the same "Converting statement..." that a
# one-page statement shows for a second. Nineteen minutes of silence is
# indistinguishable from a hung tool, and on a single-process server the natural
# response (reload, upload again) is the one that makes it worse.

test_that("the estimate knows a digital statement from a scan, and is cheap", {
  skip_if_not(nzchar(Sys.which("pdfinfo")) && nzchar(Sys.which("pdftotext")))
  f <- fixture("tests/testthat/fixtures/anz_everyday_pdf_sample.pdf")
  skip_if_not(file.exists(f))
  t0 <- Sys.time()
  e <- conversion_estimate(f)
  spent <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_false(is.null(e))
  expect_identical(e$pages, 1L)
  expect_false(e$scanned)
  expect_lt(spent, 5)                 # it must not be more waiting (measured: 0.05s)
  # under half a minute it says NOTHING: a number nobody needed is noise, and this
  # appears on every conversion
  expect_identical(e$note, "")
})

test_that("the estimate refuses rather than guesses", {
  expect_null(conversion_estimate(character(0)))
  expect_null(conversion_estimate(file.path(tempdir(), "no-such-file.pdf")))
  # not a PDF: a CSV has no pages and nothing to estimate from
  csv <- file.path(tempdir(), "est.csv")
  writeLines("a,b\n1,2", csv)
  expect_null(conversion_estimate(csv))
})

test_that("the sentence scales, and a long scan says it is not stuck", {
  # The numbers here are the measured ones in R/params.R. A digital 400-page
  # statement is a non-event; a 120-page scan is the case this exists for.
  expect_identical(.estimate_note(10L, FALSE, 10 * PARAM_SECS_PER_PAGE), "")
  big <- .estimate_note(400L, FALSE, 400 * PARAM_SECS_PER_PAGE)
  expect_match(big, "400 pages")
  expect_match(big, "seconds")
  scan <- .estimate_note(120L, TRUE, 120 * PARAM_SECS_PER_SCAN_PAGE)
  expect_match(scan, "scans rather than text")
  expect_match(scan, "19 minutes")
  expect_match(scan, "not stuck", fixed = TRUE)
  # and an absurd job is stated in hours rather than a four-digit minute count
  expect_match(.estimate_note(2000L, TRUE, 2000 * PARAM_SECS_PER_SCAN_PAGE), "hours")
})

test_that("a long statement is told it is fine, and NOT told to split", {
  # The advice here used to be "may hit tool limits; split into smaller files". Both
  # halves were wrong: 400 pages convert in 70 seconds, and splitting a statement
  # destroys the opening-plus-transactions-equals-closing check, which is the proof
  # that the figures are right. The tool was telling people to degrade their evidence.
  d <- build_diagnostics("ok", metadata = list(pages = 400L))
  i <- which(d$category == "oversized")
  expect_length(i, 1L)
  expect_identical(d$severity[i], "info")
  expect_match(d$how_to_fix[i], "Do NOT split", fixed = TRUE)
  expect_match(d$how_to_fix[i], "across the whole statement")
  expect_false(grepl("tool limits", d$how_to_fix[i], fixed = TRUE))
  # ...and a long SCAN says where the time went
  ds <- build_diagnostics("ok", metadata = list(pages = 120L, ocr_pages = 120L))
  j <- which(ds$category == "oversized")
  expect_match(ds$detail[j], "read as scans")
  expect_match(ds$how_to_fix[j], "scanned pages as pictures")
})
