# End-to-end OCR test: a genuinely NON-SELECTABLE (image-only) statement must
# parse into transactions and reconcile, using the same template as the
# text-layer version. sample_everyday_scanned.pdf is the tutorial statement
# rasterised (0 extractable text). Skips where system tesseract/poppler are
# absent (the OCR path is optional).

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
  expect_true(all(c("x", "y", "width", "height", "text") %in% names(wb)))
})

test_that("a scanned statement parses AND reconciles like the text version", {
  skip_if_not(ocr_available())
  skip_if_not(file.exists(fixture(SCAN)))
  templates <- load_templates(templates_dir())
  inp <- read_input(fixture(SCAN))
  det <- detect_statement(inp, templates)
  expect_true(det$matched)
  expect_identical(det$template_id, "tutorial_everyday_pdf")
  parsed <- parse_statement(inp, templates[["tutorial_everyday_pdf"]])
  tx <- parsed$transactions
  expect_gte(nrow(tx), 10)
  # reconciles to the same closing balance the text-layer version does
  expect_equal(round(1250.00 + sum(tx$amount, na.rm = TRUE), 2), 2716.50)
})
