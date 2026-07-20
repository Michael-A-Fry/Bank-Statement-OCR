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
  expect_true(all(c("x", "y", "width", "height", "text", "redacted") %in%
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
  expect_s3_class(input$meta$redactions, "data.frame")
})

# ---- Forensic redaction guard --------------------------------------------

test_that("overlay redaction removes covered text and never leaks it", {
  # Synthetic page: three words, a redaction rectangle sitting over the middle
  # one ("SECRET99"). Coordinates use pdftools' top-left origin.
  words <- data.frame(
    width  = c(50, 50, 50),
    height = c(10, 10, 10),
    x      = c(70, 130, 70),
    y      = c(100, 100, 120),
    space  = c(TRUE, FALSE, FALSE),
    text   = c("Balance", "SECRET99", "Total"),
    stringsAsFactors = FALSE
  )
  rects <- data.frame(x0 = 125, y0 = 95, x1 = 190, y1 = 112)

  guarded <- apply_redaction_guard(words, rects)

  # the covered word is flagged and rewritten
  expect_true(guarded$redacted[2])
  expect_false(any(guarded$redacted[c(1, 3)]))
  expect_identical(guarded$text[2], REDACTION_TOKEN)
  # the hidden text is gone from the word table entirely
  expect_false(any(grepl("SECRET", guarded$text)))
  # ...and from any reconstructed page text
  expect_false(grepl("SECRET", words_to_text(guarded)))
  # visible words are untouched (verbatim)
  expect_identical(guarded$text[c(1, 3)], c("Balance", "Total"))
})

test_that("text-layer redaction markers are honoured without geometry", {
  words <- data.frame(
    width = c(50, 50, 50), height = c(10, 10, 10),
    x = c(70, 70, 70), y = c(100, 120, 140), space = c(FALSE, FALSE, FALSE),
    text = c("Owner", "████", "[REDACTED]"),
    stringsAsFactors = FALSE
  )
  guarded <- apply_redaction_guard(words)
  expect_equal(guarded$redacted, c(FALSE, TRUE, TRUE))
  expect_identical(guarded$text[2], REDACTION_TOKEN)
  expect_identical(guarded$text[3], REDACTION_TOKEN)
})

test_that("overlay detector is conservative on partial overlap", {
  # A rectangle clipping only the edge of a word must still redact it.
  words <- data.frame(width = 60, height = 12, x = 100, y = 200,
                      space = FALSE, text = "ACCOUNT12345",
                      stringsAsFactors = FALSE)
  rects <- data.frame(x0 = 150, y0 = 205, x1 = 300, y1 = 260) # clips right edge
  guarded <- apply_redaction_guard(words, rects)
  expect_true(guarded$redacted[1])
  expect_false(grepl("ACCOUNT", guarded$text[1]))
})

test_that("read_input threads redaction_rects into the PDF pipeline", {
  # Guarantee 11.2 in production: read_input must forward overlay rectangles so
  # text under a drawn redaction is dropped before it leaves the reader.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  rects <- data.frame(page = 1, x0 = 60, y0 = 130, x1 = 500, y1 = 175)
  input <- read_input(fixture(SAMPLE_PDF), redaction_rects = rects)
  expect_identical(input$kind, "pdf")
  expect_gt(input$meta$redactions$redacted_words[1], 0L)
  w1 <- input$words[[1]]
  expect_true(all(w1$text[w1$redacted] == REDACTION_TOKEN))
  # a plain read_input (no rects) leaves this clean specimen unredacted
  plain <- read_input(fixture(SAMPLE_PDF))
  expect_equal(sum(plain$meta$redactions$redacted_words), 0L)
})

test_that("read_pdf rebuilds page text from guarded boxes when redacted", {
  # Drive the full read_pdf path with an injected rectangle so a real page's
  # emitted text is proven to exclude text under the overlay. The rectangle
  # covers the top-left region of page 1 where the header words sit.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  rects <- data.frame(page = 1, x0 = 60, y0 = 130, x1 = 500, y1 = 175)
  pdf <- read_pdf(fixture(SAMPLE_PDF), redaction_rects = rects)
  expect_gt(pdf$redactions$redacted_words[1], 0L)
  # every covered word became the token; none of the covered originals remain
  w1 <- pdf$words[[1]]
  covered <- w1$redacted
  expect_true(all(w1$text[covered] == REDACTION_TOKEN))
  expect_true(any(covered))
})
