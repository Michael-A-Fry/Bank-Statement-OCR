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

# The mechanism ocr_pdf_page's cleanup relies on (#29): when the caller NAMES the
# output, the processed image goes exactly there -- so it can be put under the
# render prefix and swept away with it, instead of surviving in the temp dir as a
# readable copy of the client's statement. Needs magick only, not the OCR binaries.
test_that("preprocess_image writes to the out_path it is given (#29)", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  src <- tempfile("ppsrc_", fileext = ".png")
  magick::image_write(magick::image_blank(120, 60, "white"), src, format = "png")
  out <- tempfile("ppout_", fileext = ".png")
  got <- preprocess_image(src, out_path = out,
                          opts = list(greyscale = TRUE, normalize = TRUE))
  expect_identical(got, out)
  expect_true(file.exists(out))
  unlink(c(src, out), force = TRUE)
})

test_that("OCR still reads real text after pre-processing", {
  skip_if_not(ocr_available(), "tesseract/poppler not available")
  pdf <- fixture("samples/raw/anz/anz_card_summary_sample.pdf")
  skip_if_not(file.exists(pdf))
  res <- ocr_pdf_page(pdf, 1L, preprocess = TRUE)
  expect_true(res$ok)
  expect_match(toupper(paste(res$text, collapse = " ")), "CARD SUMMARY", fixed = TRUE)
})

# A PAGE OF RULED LINES, BUILT THROUGH A PNG FILE.
#
# These two tests used to draw with `magick::image_draw`, which hands back an
# image still attached to a live graphics device: what a later magick call sees
# then depends on when that device was flushed and on what else has touched
# magick since. The identical drawing gave different answers depending only on
# what had run before it in the same session, so both tests failed on every run
# of the suite and both were written off as "magick not available". magick was
# installed the whole time. Rendered to a PNG and read back, there is no live
# device anywhere near it and the answer is the same every time.
# (The same mistake, and the same fix, as in test-detect_redaction.R.)
.pp_ruled_png <- function() {
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = 800, height = 1100)
  dn <- grDevices::dev.cur()
  on.exit(if (dn %in% grDevices::dev.list()) grDevices::dev.off(dn), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 800), ylim = c(1100, 0), xaxs = "i", yaxs = "i")
  for (yy in seq(100, 1000, by = 60))
    graphics::rect(100, yy, 700, yy + 4, col = "black", border = NA)
  grDevices::dev.off()
  magick::image_read(tf)
}

test_that("the skew estimator recovers a known rotation and leaves straight pages alone", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  img <- .pp_ruled_png()
  rot <- magick::image_background(magick::image_rotate(img, 2), "white", flatten = TRUE)
  expect_lt(abs(.detect_skew_angle(rot) - 2), 0.2)   # finds the 2 degree tilt
  expect_lt(abs(.detect_skew_angle(img)), 0.3)       # a straight page measures straight
})

test_that("deskew straightens the page without changing the canvas", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  img <- .pp_ruled_png()
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
