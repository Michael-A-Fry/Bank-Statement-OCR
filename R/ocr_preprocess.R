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
  deskew_min       = 0.3,   # degrees; below this the page counts as straight
  deskew_max       = 5,     # degrees; search range of the skew estimator
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

# preprocess_opts_geometry() -- profile for the word-BOX OCR pass. Pixel-VALUE
# cleanups (greyscale + normalize + despeckle) PLUS deskew, but NO resize. Deskew
# is a rigid rotation cropped back to the original canvas, not a rescale, so
# pixel -> point (72/dpi) still holds and the page frame is unchanged; what it
# does is STRAIGHTEN a skewed scan so a transaction's cells land on one
# horizontal line. Without it, even a 1-2 degree scan tilt spreads a row's cells
# across a large vertical gradient and the row splits apart (whole blocks vanish).
# Resize/upscale stays OFF so column x-positions are never shifted by scaling.
preprocess_opts_geometry <- function() {
  list(greyscale = TRUE, deskew = TRUE, deskew_min = 0.3, deskew_max = 5,
       normalize = TRUE, upscale_min_width = NULL, adaptive = FALSE,
       despeckle = TRUE, threshold = FALSE)
}

# .detect_skew_angle(img, max_angle, step, work_width) -- estimate the page's
# skew in DEGREES with a projection-profile search: shear the dark pixels by
# each candidate angle and score how sharply they stack into horizontal lines
# (sum of squared row counts). The candidate that stacks text lines and table
# rules the tightest is the skew. Runs on a downscaled greyscale copy for speed.
# Fully deterministic: fixed grid, fixed threshold, ties go to the smaller
# angle. Returns 0 when there is nothing to measure (blank or unreadable page).
#
# This replaces magick::image_deskew for statements: measured on rotated copies
# of the scanned sample, image_deskew reported 4.3/4.4/5.3 degrees for true
# skews of 1/2/3 degrees (and 0 for a 150 dpi rescan at 2 degrees), leaving the
# page tilted AFTER correction and collapsing the table parse. This estimator
# recovers those same pages to within 0.05 degrees.
.detect_skew_angle <- function(img, max_angle = 5, step = 0.05, work_width = 1000L) {
  ok <- tryCatch({
    g <- magick::image_convert(img, colorspace = "gray")
    if (magick::image_info(g)$width > work_width)
      g <- magick::image_resize(g, paste0(work_width, "x"))
    g <- magick::image_normalize(g)
    hh <- magick::image_info(g)$height
    v <- as.integer(magick::image_data(g, channels = "gray"))
    # Dark ink after normalisation. The raw bitmap is laid out pixel-column-major
    # (channel, then y within a column, then x), hence the %% height decode.
    idx <- which(v < 100L)
    if (length(idx) < 200L || length(idx) > 400000L) return(0)
    y <- (idx - 1L) %% hh
    x <- (idx - 1L) %/% hh
    angles <- seq(-max_angle, max_angle, by = step)
    angles <- angles[order(abs(angles), angles)]   # prefer the smaller angle on a tie
    best <- 0; best_score <- -Inf
    for (a in angles) {
      yy <- floor(y - x * tan(a * pi / 180))
      cnt <- tabulate(yy - min(yy) + 1L)
      s <- sum(as.numeric(cnt)^2)
      if (s > best_score) { best_score <- s; best <- a }
    }
    best
  }, error = function(e) 0)
  if (!is.finite(ok)) 0 else ok
}

# .content_centre(img, work_width) -- centre (in FULL-resolution pixels) of the
# dark-ink bounding box, measured on a downscaled greyscale copy. NULL when the
# page holds no measurable ink. Used to anchor the deskew crop to the CONTENT,
# not the canvas.
.content_centre <- function(img, work_width = 1000L) {
  tryCatch({
    info <- magick::image_info(img)
    g <- magick::image_convert(img, colorspace = "gray")
    s <- 1
    if (info$width > work_width) {
      s <- info$width / work_width
      g <- magick::image_resize(g, paste0(work_width, "x"))
    }
    g <- magick::image_normalize(g)
    hh <- magick::image_info(g)$height
    v <- as.integer(magick::image_data(g, channels = "gray"))
    idx <- which(v < 100L)
    if (length(idx) < 200L) return(NULL)
    y <- (idx - 1L) %% hh
    x <- (idx - 1L) %/% hh
    c(x = (min(x) + max(x)) / 2 * s, y = (min(y) + max(y)) / 2 * s)
  }, error = function(e) NULL)
}

# .deskew_image(img, min_angle, max_angle) -- straighten a skewed page: measure
# the skew, and only when it is a REAL tilt (above min_angle, within the search
# range) rotate by the opposite angle on a white background and crop back to the
# ORIGINAL canvas size. The crop matters twice over. First, rotation expands the
# canvas, which would shift every word box and mis-report the page size; the
# crop keeps the frame identical to the raw render so downstream geometry
# (72/dpi scaling, template x-bands, page-size normalisation) is untouched.
# Second, the crop is anchored so the CONTENT's bounding-box centre lands where
# it was before the rotation -- a rotation about the canvas centre alone adds a
# sideways translation (tens of points at 2-3 degrees, enough to push every
# word out of its template band), because the printed content is never
# perfectly centred on the canvas. Anchoring to the ink undoes the tilt without
# moving the words. A straight page is returned as-is, never resampled.
# `angle`, when supplied, is a skew already measured for THIS render (both OCR
# passes deskew the same page, so the caller can measure once and hand it in);
# NULL measures it here as before. min_angle still gates: a page below it is
# returned untouched regardless of who measured the angle.
.deskew_image <- function(img, min_angle = 0.3, max_angle = 5, angle = NULL) {
  ang <- if (is.null(angle)) .detect_skew_angle(img, max_angle = max_angle) else angle
  if (!is.finite(ang) || abs(ang) <= min_angle) return(img)
  tryCatch({
    info <- magick::image_info(img)
    c_before <- .content_centre(img)
    out <- magick::image_rotate(
      magick::image_background(img, "white", flatten = TRUE), -ang)
    oi <- magick::image_info(out)
    # Default: centred crop. With a measurable content centre, offset the crop so
    # the ink sits exactly where it did in the raw render.
    ox <- (oi$width - info$width) %/% 2
    oy <- (oi$height - info$height) %/% 2
    c_after <- if (is.null(c_before)) NULL else .content_centre(out)
    if (!is.null(c_before) && !is.null(c_after)) {
      ox <- as.integer(round(c_after[["x"]] - c_before[["x"]]))
      oy <- as.integer(round(c_after[["y"]] - c_before[["y"]]))
    }
    ox <- min(max(ox, 0L), max(0L, oi$width - info$width))
    oy <- min(max(oy, 0L), max(0L, oi$height - info$height))
    magick::image_crop(out, sprintf("%dx%d+%d+%d", info$width, info$height, ox, oy))
  }, error = function(e) img)
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
    if (isTRUE(opts$deskew))    img <- .deskew_image(img, min_angle = opts$deskew_min %||% 0.3,
                                                     max_angle = opts$deskew_max %||% 5,
                                                     angle = opts$deskew_angle)
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
