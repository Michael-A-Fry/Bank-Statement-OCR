# ocr_preprocess.R -- image pre-processing before OCR via ImageMagick: greyscale, deskew, normalise
# contrast, upscale small pages. Tesseract does its own Otsu binarisation, so hard thresholding is
# off by default. If magick is unavailable the original path is returned unchanged.

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

# A stronger profile for difficult scans and phone photos, for image-only pages where the safe
# default under-reads.
preprocess_opts_scan <- function() {
  o <- preprocess_opts(); o$adaptive <- TRUE; o$despeckle <- TRUE; o
}

# Profile for the word-BOX OCR pass: pixel-value cleanups PLUS deskew, but NO resize. Deskew is a rigid
# rotation cropped back to the original canvas, so pixel-to-point still holds; it STRAIGHTENS a skewed
# scan so a transaction's cells land on one line, since even a 1-2 degree tilt splits a row apart.
# Resize stays OFF so column x-positions are never shifted.
preprocess_opts_geometry <- function() {
  list(greyscale = TRUE, deskew = TRUE, deskew_min = 0.3, deskew_max = 5,
       normalize = TRUE, upscale_min_width = NULL, adaptive = FALSE,
       despeckle = TRUE, threshold = FALSE)
}

# Estimate the page's skew in DEGREES with a projection-profile search: shear the dark pixels by each
# candidate angle and score how sharply they stack into horizontal lines. Deterministic, ties to the
# smaller angle. It replaces magick::image_deskew, which reported 4.3/4.4/5.3 degrees for true skews
# of 1/2/3, leaving the page tilted and collapsing the table parse; this recovers them to within 0.05.
.detect_skew_angle <- function(img, max_angle = 5, step = 0.05, work_width = 1000L) {
  ok <- tryCatch({
    g <- magick::image_convert(img, colorspace = "gray")
    if (magick::image_info(g)$width > work_width)
      g <- magick::image_resize(g, paste0(work_width, "x"))
    g <- magick::image_normalize(g)
    hh <- magick::image_info(g)$height
    v <- as.integer(magick::image_data(g, channels = "gray"))
    # Dark ink after normalisation. The raw bitmap is pixel-column-major, hence the %% height decode.
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

# Centre, in full-resolution pixels, of the dark-ink bounding box. NULL when the page holds no
# measurable ink. Used to anchor the deskew crop to the CONTENT, not the canvas.
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

# Straighten a skewed page: measure the skew and, only on a REAL tilt, rotate by the opposite angle on
# white and crop back to the ORIGINAL canvas. The crop matters twice - rotation expands the canvas,
# which would shift every word box and mis-report the page size; and it is anchored so the CONTENT's
# bounding-box centre lands where it was, because a rotation about the canvas centre alone adds a
# sideways translation of tens of points at 2-3 degrees, enough to push every word out of its band.
.deskew_image <- function(img, min_angle = 0.3, max_angle = 5, angle = NULL) {
  ang <- if (is.null(angle)) .detect_skew_angle(img, max_angle = max_angle) else angle
  if (!is.finite(ang) || abs(ang) <= min_angle) return(img)
  tryCatch({
    info <- magick::image_info(img)
    c_before <- .content_centre(img)
    out <- magick::image_rotate(
      magick::image_background(img, "white", flatten = TRUE), -ang)
    oi <- magick::image_info(out)
    # Default is a centred crop; with a measurable content centre, offset it so the ink sits exactly
    # where it did in the raw render.
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

# The path to the processed image, or the original path if pre-processing is unavailable or failed.
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

# TRUE when magick is usable.
ocr_preprocess_available <- function() requireNamespace("magick", quietly = TRUE)
