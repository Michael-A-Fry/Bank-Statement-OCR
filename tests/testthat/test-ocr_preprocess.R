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

# A SPARSE PAGE ON GRAINY PAPER -- the page that broke the old clean-up.
# A statement's last page often holds a few lines of type (here 0.1% ink) on paper
# with scanner grain. image_normalize put its black point in the darkest 2% of
# pixels, which on such a page is the grain itself, and stretched the grain into
# black speckle: four of the fifteen dev scans then read their last page as
# thousands of junk words at median confidence ~17, and Tesseract ran for minutes.
# Built from a PNG with a fixed seed, so it is the same page on every run.
.pp_grainy_page <- function() {
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = 1240, height = 1754, bg = "white")
  dn <- grDevices::dev.cur()
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1240), ylim = c(1754, 0), xaxs = "i", yaxs = "i")
  for (i in 1:3)
    graphics::text(100, 120 + 40 * i, sprintf("Closing balance %d  1,234.%02d", i, i),
                   adj = c(0, 0.5), cex = 1.6)
  grDevices::dev.off(dn)
  img <- magick::image_convert(magick::image_read(tf), colorspace = "gray")
  unlink(tf)
  m <- as.integer(magick::image_data(img, channels = "gray"))[, , 1]
  set.seed(1)
  m[] <- pmax(0L, m - 10L - sample(0:14, length(m), replace = TRUE))  # paper 231-245
  magick::image_convert(magick::image_read(grDevices::as.raster(m / 255)), colorspace = "gray")
}

test_that("a sparse page's paper grain is never stretched into speckle", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  img <- .pp_grainy_page()
  before <- .dark_fraction(img)
  expect_lt(before, 0.01)
  # The root cause, kept as a witness: ImageMagick's normalize turns this page dark.
  expect_gt(.dark_fraction(magick::image_normalize(img)), 0.2)
  # The stretch used instead leaves it a page of a few lines of type.
  expect_lt(.dark_fraction(.stretch_contrast(img)), 2 * before + 0.005)
})

test_that("a page with no ink is not stretched at all", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  set.seed(2)
  grain <- matrix(sample(231:245, 400 * 300, replace = TRUE), 400) / 255
  img <- magick::image_read(grDevices::as.raster(grain))
  expect_null(.ink_levels(.grey_sample(img)$v))
  expect_identical(.stretch_contrast(img), img)
  expect_identical(.detect_skew_angle(img), 0)
})

test_that("preprocess_image refuses a clean-up that would add ink", {
  skip_if_not(ocr_preprocess_available(), "magick not available")
  src <- tempfile("ppgrain_", fileext = ".png")
  magick::image_write(.pp_grainy_page(), src, format = "png")
  out <- tempfile("ppout_", fileext = ".png")
  on.exit(unlink(c(src, out), force = TRUE), add = TRUE)
  # The adaptive local threshold turns grain into speckle on a page this sparse:
  # the guard hands back the source, says why, and writes nothing.
  got <- preprocess_image(src, out_path = out, opts = preprocess_opts_scan())
  expect_identical(as.vector(got), src)
  expect_match(attr(got, "refused"), "speckle")
  expect_false(file.exists(out))
  # The OCR profile on the same page is accepted, as an uncompressed PGM.
  pgm <- tempfile("ppout_", fileext = ".pgm")
  on.exit(unlink(pgm, force = TRUE), add = TRUE)
  got2 <- preprocess_image(src, out_path = pgm, opts = preprocess_opts_geometry())
  expect_identical(got2, pgm)
  expect_identical(readBin(pgm, "raw", 2L), charToRaw("P5"))
})

test_that("a sparse scanned page reads as its text, not as noise, and quickly", {
  skip_if_not(ocr_available(), "tesseract/poppler not available")
  skip_if_not(ocr_preprocess_available(), "magick not available")
  pdf <- tempfile("sparse_", fileext = ".pdf")
  on.exit(unlink(pdf, force = TRUE), add = TRUE)
  magick::image_write(.pp_grainy_page(), pdf, format = "pdf", density = "150x150")
  t0 <- Sys.time()
  res <- ocr_pdf_page(pdf, 1L)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_true(res$ok)
  # Before the fix: median confidence 17 and a page of junk.
  expect_gt(stats::median(res$words$conf), 80)
  expect_lt(nrow(res$words), 30L)
  expect_match(paste(res$text, collapse = "\n"), "Closing balance 3 1,234.03", fixed = TRUE)
  expect_lt(secs, 60)
})
