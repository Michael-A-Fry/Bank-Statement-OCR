# ocr_preprocess.R -- image pre-processing before OCR, using ImageMagick (magick)
# driven from R. These are the "easy, common-sense, high-impact" steps that lift
# OCR accuracy on scanned statements without risk of harming clean pages:
#   greyscale -> deskew -> normalise contrast -> upscale small pages.
# (Tesseract does its own Otsu binarisation internally, so hard thresholding is
# OFF by default -- it hurts more often than it helps on already-clean scans.)
#
# Safe no-op fallback: if magick is unavailable or the image can't be read, the
# original path is returned unchanged, so OCR still runs.

preprocess_opts <- function() list(
  greyscale        = TRUE,
  deskew           = TRUE,
  deskew_threshold = 40L,   # % of pixels that must agree on the skew angle
  normalize        = TRUE,
  upscale_min_width = 2000L, # upscale pages narrower than this (small text)
  adaptive         = FALSE,  # Sauvola-style local threshold (image_lat) -- best
  adaptive_geometry = "25x25+10%", #   for uneven illumination / faded / tinted scans
  threshold        = FALSE,  # hard global binarisation -- off by default
  despeckle        = FALSE
)

# preprocess_opts_scan() -- a stronger profile for difficult SCANS / phone photos:
# deskew + adaptive local threshold + despeckle. Use for image-only pages where
# the default (safe) profile under-reads.
preprocess_opts_scan <- function() {
  o <- preprocess_opts(); o$adaptive <- TRUE; o$despeckle <- TRUE; o
}

# preprocess_image(in_path, out_path, opts) -> path to the processed image
# (or the original path if pre-processing is unavailable/failed).
preprocess_image <- function(in_path, out_path = NULL, opts = preprocess_opts()) {
  if (!requireNamespace("magick", quietly = TRUE) || !file.exists(in_path)) return(in_path)
  img <- tryCatch(magick::image_read(in_path), error = function(e) NULL)
  if (is.null(img)) return(in_path)
  out <- tryCatch({
    if (is.null(out_path)) out_path <- tempfile(fileext = ".png")
    info <- magick::image_info(img)
    if (isTRUE(opts$greyscale)) img <- magick::image_convert(img, colorspace = "gray")
    if (isTRUE(opts$deskew))    img <- magick::image_deskew(img, threshold = opts$deskew_threshold %||% 40L)
    if (isTRUE(opts$normalize)) img <- magick::image_normalize(img)
    if (!is.null(opts$upscale_min_width) && isTRUE(info$width < opts$upscale_min_width))
      img <- magick::image_resize(img, paste0(opts$upscale_min_width, "x"))
    if (isTRUE(opts$despeckle))  img <- magick::image_despeckle(img)
    if (isTRUE(opts$adaptive) && exists("image_lat", where = asNamespace("magick")))
      img <- magick::image_lat(img, geometry = opts$adaptive_geometry %||% "25x25+10%")
    if (isTRUE(opts$threshold))  img <- magick::image_threshold(img, type = "black", threshold = "50%")
    magick::image_write(img, out_path, format = "png")
    out_path
  }, error = function(e) in_path)
  out
}

# ocr_preprocess_available() -- TRUE when magick is usable.
ocr_preprocess_available <- function() requireNamespace("magick", quietly = TRUE)
