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

test_that("the skew estimator recovers a known rotation and leaves straight pages alone", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  img <- magick::image_blank(800, 1100, "white")
  img <- magick::image_draw(img)
  for (yy in seq(100, 1000, by = 60)) rect(100, yy, 700, yy + 4, col = "black", border = NA)
  dev.off()
  rot <- magick::image_background(magick::image_rotate(img, 2), "white", flatten = TRUE)
  expect_lt(abs(.detect_skew_angle(rot) - 2), 0.2)   # finds the 2 degree tilt
  expect_lt(abs(.detect_skew_angle(img)), 0.3)       # a straight page measures straight
})

test_that("deskew straightens the page without changing the canvas", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  img <- magick::image_blank(800, 1100, "white")
  img <- magick::image_draw(img)
  for (yy in seq(100, 1000, by = 60)) rect(100, yy, 700, yy + 4, col = "black", border = NA)
  dev.off()
  rot <- magick::image_background(magick::image_rotate(img, 2), "white", flatten = TRUE)
  fixed <- .deskew_image(rot)
  ri <- magick::image_info(rot); fi <- magick::image_info(fixed)
  expect_equal(fi$width, ri$width)    # crop-back keeps the frame: word geometry
  expect_equal(fi$height, ri$height)  # and page size stay consistent downstream
  expect_lt(abs(.detect_skew_angle(fixed)), 0.3)
})

test_that("scan profile (adaptive local threshold) yields a readable image", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  skip_if_not(nzchar(Sys.which("pdftoppm")), "pdftoppm not available")
  pdf <- fixture("samples/raw/anz/anz_card_summary_sample.pdf")
  skip_if_not(file.exists(pdf))
  prefix <- tempfile("sc_")
  system2("pdftoppm", c("-png", "-r", "150", "-f", "1", "-l", "1", pdf, prefix),
          stdout = FALSE, stderr = FALSE)
  raw <- Sys.glob(paste0(prefix, "*.png"))[1]
  out <- preprocess_image(raw, opts = preprocess_opts_scan())
  expect_true(file.exists(out))
  expect_gt(magick::image_info(magick::image_read(out))$width, 0)
})
