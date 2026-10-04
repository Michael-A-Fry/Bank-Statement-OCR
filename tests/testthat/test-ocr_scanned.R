# End-to-end OCR test: a genuinely NON-SELECTABLE (image-only) statement must be
# read by the automatic reader into the same transactions as its text-layer
# version, and proven by its own arithmetic. sample_everyday_scanned.pdf is the
# tutorial statement rasterised (0 extractable text). Skips where system
# tesseract/poppler are absent (the OCR path is optional).

SCAN <- "samples/raw/tutorial/sample_everyday_scanned.pdf"

test_that("the scanned fixture really is image-only", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SCAN)))
  expect_equal(sum(nchar(pdftools::pdf_text(fixture(SCAN)))), 0L)
})

test_that("a scanned (image-only) statement OCRs into positioned word boxes", {
  skip_if_not(ocr_available())
  skip_if_not(file.exists(fixture(SCAN)))
  inp <- read_input(fixture(SCAN))
  expect_true(inp$meta$ocr_pages >= 1)          # OCR actually ran
  wb <- inp$words[[2]]
  expect_true(!is.null(wb) && nrow(wb) > 20)    # OCR produced word boxes (was 0)
  expect_true(all(c("x", "y", "width", "height", "text", "ocr_conf") %in% names(wb)))
  # per-word confidence contract: numeric 0-100 on an OCR page.
  expect_type(wb$ocr_conf, "double")
  expect_true(any(!is.na(wb$ocr_conf)))
  expect_true(all(wb$ocr_conf >= 0 & wb$ocr_conf <= 100, na.rm = TRUE))
})

test_that("a scanned statement is read and proven like the text version", {
  skip_if_not(ocr_available())
  skip_if_not(file.exists(fixture(SCAN)))
  rd <- auto_read(read_input(fixture(SCAN)))
  expect_identical(rd$outcome, "proven")
  tx <- rd$transactions
  expect_equal(nrow(tx), 12L)
  # reconciles to the same closing balance the text-layer version does
  expect_equal(round(1250.00 + sum(tx$amount, na.rm = TRUE), 2), 2716.50)
})

test_that("a slightly rotated rescan converts or flags - never wrong silently", {
  skip_if_not(ocr_available())
  skip_if_not(ocr_preprocess_available(), "magick not available")
  skip_if_not(file.exists(fixture(SCAN)))
  # Deterministic degraded copy: the scanned sample tilted 2 degrees at its own
  # 200 dpi, the most common real-world scan defect. Before the
  # projection-profile deskew this collapsed the table to 2 of 12 rows.
  vpdf <- tempfile(fileext = ".pdf")
  pages <- magick::image_read_pdf(fixture(SCAN), density = 200)
  rot <- magick::image_background(magick::image_rotate(pages, 2), "white", flatten = TRUE)
  magick::image_write(rot, vpdf, format = "pdf", density = "200x200")
  on.exit(unlink(vpdf), add = TRUE)

  rd <- auto_read(read_input(vpdf))
  tx <- rd$transactions
  correct <- is.data.frame(tx) && nrow(tx) == 12 &&
    isTRUE(abs(1250.00 + sum(tx$amount, na.rm = TRUE) - 2716.50) < 0.005)
  # The forbidden outcome is quiet wrongness: an automatic reading must be right.
  if (rd$outcome %in% c("proven", "layout_match")) expect_true(correct)
  # And the deskew should make it genuinely convert, not just ask.
  expect_identical(rd$outcome, "proven")
})
