# Tests for the OCR path (R/ocr.R): the system Tesseract + poppler pipeline
# driven from R, and its integration as the no-text-layer fallback in read_pdf.
# Portable -- OCR-dependent checks skip automatically where the tools are absent.

SAMPLE_PDF_OCR <- "samples/raw/anz/anz_card_summary_sample.pdf"

test_that("ocr_available returns a single logical", {
  expect_type(ocr_available(), "logical")
  expect_length(ocr_available(), 1L)
})

test_that("page_needs_ocr triggers only on empty / near-empty text layers", {
  expect_true(page_needs_ocr(character(0)))
  expect_true(page_needs_ocr(""))
  expect_true(page_needs_ocr("   \n \t "))
  expect_false(page_needs_ocr(
    paste(rep("real statement transaction line 12/03/2025 -45.00", 3),
          collapse = "\n")))
})

test_that("tesseract reads a real PDF page end-to-end", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  pdf <- fixture(SAMPLE_PDF_OCR)
  skip_if_not(file.exists(pdf))
  res <- ocr_pdf_page(pdf, 1L)
  expect_true(res$ok)
  expect_gt(length(res$text), 0L)
  expect_match(toupper(paste(res$text, collapse = " ")),
               "CARD SUMMARY", fixed = TRUE)
})

test_that("read_pdf exposes ocr flags and does not OCR a text-layer page", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  r <- read_pdf(fixture(SAMPLE_PDF_OCR))
  expect_true(r$ok)
  expect_type(r$ocr, "logical")
  expect_length(r$ocr, r$page_count)
  # the page holding the header has a real text layer -> must NOT be OCR-flagged
  idx <- which(grepl("CARD SUMMARY", r$pages, ignore.case = TRUE))
  expect_gte(length(idx), 1L)
  expect_false(any(r$ocr[idx]))
  # words-frame contract: ocr_conf is always present; on a text-layer page it is
  # all NA (typeset text has no recognition step to be unsure about).
  w <- r$words[[idx[1]]]
  expect_true("ocr_conf" %in% names(w))
  expect_true(all(is.na(w$ocr_conf)))
})
