# Tests for OCR image pre-processing (R/ocr_preprocess.R) and its use in the
# OCR path. Portable -- skips where magick / tesseract / poppler are absent.

test_that("preprocess_image produces a readable image (safe no-op otherwise)", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  skip_if_not(nzchar(Sys.which("pdftoppm")), "pdftoppm not available")
  pdf <- fixture("samples/raw/anz/anz_card_summary_sample.pdf")
  skip_if_not(file.exists(pdf))
  prefix <- tempfile("pp_")
  system2("pdftoppm", c("-png", "-r", "150", "-f", "1", "-l", "1", pdf, prefix),
          stdout = FALSE, stderr = FALSE)
  raw <- Sys.glob(paste0(prefix, "*.png"))[1]
  expect_true(file.exists(raw))
  out <- preprocess_image(raw)
  expect_true(file.exists(out))
  info <- magick::image_info(magick::image_read(out))
  expect_gt(info$width, 0)
})

test_that("preprocess_image no-ops safely on a missing file", {
  expect_identical(preprocess_image("/no/such/file.png"), "/no/such/file.png")
})

test_that("OCR still reads real text after pre-processing", {
  skip_if_not(ocr_available(), "tesseract/poppler not available")
  pdf <- fixture("samples/raw/anz/anz_card_summary_sample.pdf")
  skip_if_not(file.exists(pdf))
  res <- ocr_pdf_page(pdf, 1L, preprocess = TRUE)
  expect_true(res$ok)
  expect_match(toupper(paste(res$text, collapse = " ")), "CARD SUMMARY", fixed = TRUE)
})
