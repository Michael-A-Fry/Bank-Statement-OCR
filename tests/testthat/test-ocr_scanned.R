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
  expect_true(all(c("x", "y", "width", "height", "text", "ocr_conf") %in% names(wb)))
  # per-word confidence contract: numeric 0-100 on an OCR page, and it reaches
  # the X-ray words frame so the app can shade doubtful words.
  expect_type(wb$ocr_conf, "double")
  expect_true(any(!is.na(wb$ocr_conf)))
  expect_true(all(wb$ocr_conf >= 0 & wb$ocr_conf <= 100, na.rm = TRUE))
  templates <- load_templates(templates_dir())
  lay <- inspect_pdf_layout(inp, templates[["tutorial_everyday_pdf"]])
  lw <- lay$pages[["2"]]$words
  expect_true("ocr_conf" %in% names(lw))
  expect_true(any(!is.na(lw$ocr_conf)))
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

  templates <- load_templates(templates_dir())
  inp <- read_input(vpdf)
  det <- detect_statement(inp, templates)
  expect_true(det$matched)
  parsed <- parse_statement(inp, templates[["tutorial_everyday_pdf"]])
  recon <- reconcile(parsed, templates[["tutorial_everyday_pdf"]])
  tx <- parsed$transactions
  correct <- nrow(tx) == 12 &&
    isTRUE(abs(1250.00 + sum(tx$amount, na.rm = TRUE) - 2716.50) < 0.005)
  flagged <- identical(recon$trust$level, "low") || any(recon$kpis$status == "fail")
  # The forbidden outcome is quiet wrongness: either the numbers are right, or
  # the run must be flagged for review.
  expect_true(correct || flagged)
  # And the deskew fix should make it genuinely convert, not just fail loudly.
  expect_gte(nrow(tx), 10)
})
