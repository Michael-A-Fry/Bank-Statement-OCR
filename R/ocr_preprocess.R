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

# preprocess_opts_geometry() -- the profile the OCR path reads every scanned page
# with. Pixel-VALUE cleanups (greyscale + normalize) PLUS deskew, but NO resize.
# Deskew is a rigid rotation cropped back to the original canvas, not a rescale,
# so pixel -> point (72/dpi) still holds and the page frame is unchanged; what it
# does is STRAIGHTEN a skewed scan so a transaction's cells land on one
# horizontal line. Without it, even a 1-2 degree scan tilt spreads a row's cells
# across a large vertical gradient and the row splits apart (whole blocks vanish).
# Resize/upscale stays OFF so column x-positions are never shifted by scaling.
# Despeckle is OFF: it cost 1.5-2.5 s a page and, scored end to end on 55 scans,
# read no more statements correctly (the ones it changed went both ways).
preprocess_opts_geometry <- function() {
  list(greyscale = TRUE, deskew = TRUE, deskew_min = 0.3, deskew_max = 5,
       normalize = TRUE, upscale_min_width = NULL, adaptive = FALSE,
       despeckle = FALSE, threshold = FALSE)
}

# WHY NOT magick::image_normalize. It stretches the grey levels so the darkest 2%
# of pixels become black. A sparse page -- a statement's last page, a few lines
# of type -- has less than 2% ink, so that black point lands in the PAPER GRAIN
# and the grain is stretched into black speckle. Measured on a dev scan's last
# page (0.8% ink): after normalize 17.6% of the page was dark, Tesseract read
# 6,900 junk words at median confidence 17 and took 84 s instead of 0.9 s, and
# four of the fifteen dev scans lost their last page that way.
# On a page with plenty of ink the same 2% black point is worth keeping: it
# turns the grey edges of the strokes black, and Tesseract misreads fewer
# figures (measured: a stretch that left the strokes grey read five fewer of the
# fifteen dev scans right). So the stretch
# below keeps normalize's 2% / 99% points but never lets the black point rise
# past halfway from the darkest ink to the paper, and does nothing at all on a
# page whose darkest ink is barely darker than its paper.
.INK_BLACK_Q   <- 0.02    # normalize's black point
.PAPER_WHITE_Q <- 0.99    # normalize's white point
.INK_DARKEST_Q <- 0.001   # the darkest ink: any page with a line of type has this much
.INK_MIN_CONTRAST <- 64L  # grey levels from that ink to the paper; less = no ink to stretch

# .grey_sample(img, work_width, point) -- the page as grey levels on a small
# copy: list(v = integer 0-255, hh = height, s = full-size pixels per sample
# pixel), NULL when unreadable. `point = TRUE` picks pixels instead of averaging
# them, so the copy keeps the full page's mix of grey levels (for the stretch);
# averaging blurs thin strokes, which the skew and centre measures were tuned on.
# The bitmap is laid out pixel-column-major (channel, then y within a column,
# then x), so pixel i sits at y = (i-1) %% hh, x = (i-1) %/% hh.
.grey_sample <- function(img, work_width = 1000L, point = FALSE) {
  tryCatch({
    g <- magick::image_convert(img, colorspace = "gray")
    w <- magick::image_info(g)$width
    s <- 1
    if (w > work_width) {
      s <- w / work_width
      g <- if (point) magick::image_sample(g, paste0(work_width, "x"))
           else magick::image_resize(g, paste0(work_width, "x"))
    }
    list(v = as.integer(magick::image_data(g, channels = "gray")),
         hh = magick::image_info(g)$height, s = s)
  }, error = function(e) NULL)
}

# .ink_levels(v) -- c(black, white) points (0-255) for the stretch, or NULL when
# there is no ink to stretch (see above). Paper is the median grey level.
# Attribute `capped` says the 2% point was in the paper and the cap was applied.
.ink_levels <- function(v) {
  if (!length(v)) return(NULL)
  cdf <- cumsum(tabulate(v + 1L, 256L)) / length(v)
  q <- function(p) which(cdf >= p)[1] - 1L
  ink <- q(.INK_DARKEST_Q); paper <- q(0.5)
  if (paper - ink < .INK_MIN_CONTRAST) return(NULL)
  cap <- (ink + paper) %/% 2L
  lo <- min(q(.INK_BLACK_Q), cap)
  hi <- q(.PAPER_WHITE_Q)
  if (hi <= lo) NULL else structure(c(lo, hi), capped = q(.INK_BLACK_Q) > cap)
}

# .ink_index(smp) -- which sample pixels are ink: darker than 100 on the stretched
# scale (the threshold the skew and centre measures were tuned with), worked out
# on the raw levels so no stretched copy is made. integer(0) on a blank page.
.ink_index <- function(smp) {
  lv <- if (is.null(smp)) NULL else .ink_levels(smp$v)
  if (is.null(lv)) return(integer(0))
  which(smp$v < lv[1] + (lv[2] - lv[1]) * 100 / 255)
}

# .stretch_contrast(img) -- the safe replacement for image_normalize (see above).
# Where normalize's own black point is safe it IS normalize, untouched: Tesseract
# can read a figure differently for a one-level change in the stretch (measured:
# on a dev scan a near-identical hand-made stretch read the "$" of "$80.52" as a
# section sign), so the pages it always read well keep the stretch they had.
# Where that point is in the paper, the capped stretch; where there is no ink,
# nothing.
.stretch_contrast <- function(img) {
  lv <- .ink_levels(.grey_sample(img, point = TRUE)$v)
  if (is.null(lv)) return(img)
  if (!isTRUE(attr(lv, "capped"))) return(magick::image_normalize(img))
  magick::image_level(img, black_point = lv[1] / 2.55, white_point = lv[2] / 2.55)
}

# .dark_fraction(img) -- share of the page darker than mid-grey. Clean-up must
# never ADD ink: deskew fills with white and the stretch only sharpens what is
# there, so a processed page much darker than its source is speckle.
.dark_fraction <- function(img) {
  smp <- .grey_sample(img)
  if (is.null(smp) || !length(smp$v)) NA_real_ else mean(smp$v < 128L)
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
    smp <- .grey_sample(img, work_width)
    idx <- .ink_index(smp)
    if (length(idx) < 200L || length(idx) > 400000L) return(0)
    y <- (idx - 1L) %% smp$hh
    x <- (idx - 1L) %/% smp$hh
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
    smp <- .grey_sample(img, work_width)
    idx <- .ink_index(smp)
    if (length(idx) < 200L) return(NULL)
    y <- (idx - 1L) %% smp$hh
    x <- (idx - 1L) %/% smp$hh
    c(x = (min(x) + max(x)) / 2 * smp$s, y = (min(y) + max(y)) / 2 * smp$s)
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
.deskew_image <- function(img, min_angle = 0.3, max_angle = 5) {
  ang <- .detect_skew_angle(img, max_angle = max_angle)
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

# A processed page darker than its source by more than this (share of the page,
# on top of twice the source's own ink) has been given speckle, not clarity.
.PP_MAX_ADDED_INK <- 0.02

# preprocess_image(in_path, out_path, opts) -> path to the processed image, or the
# original path when pre-processing is unavailable, fails, or would ADD ink (the
# guard below; then the path carries attribute `refused` saying so). The output format follows out_path's extension (PNG by default);
# the OCR path writes PGM, which is written and read many times faster.
preprocess_image <- function(in_path, out_path = NULL, opts = preprocess_opts()) {
  if (!requireNamespace("magick", quietly = TRUE) || !file.exists(in_path)) return(in_path)
  img <- tryCatch(magick::image_read(in_path), error = function(e) NULL)
  if (is.null(img)) return(in_path)
  out <- tryCatch({
    if (is.null(out_path)) out_path <- tempfile(fileext = ".png")
    fmt <- tolower(tools::file_ext(out_path)); if (!nzchar(fmt)) fmt <- "png"
    info <- magick::image_info(img)
    ink_before <- .dark_fraction(img)
    if (isTRUE(opts$greyscale)) img <- magick::image_convert(img, colorspace = "gray")
    if (isTRUE(opts$deskew))    img <- .deskew_image(img, min_angle = opts$deskew_min %||% 0.3,
                                                     max_angle = opts$deskew_max %||% 5)
    if (isTRUE(opts$normalize)) img <- .stretch_contrast(img)
    if (!is.null(opts$upscale_min_width) && isTRUE(info$width < opts$upscale_min_width))
      img <- magick::image_resize(img, paste0(opts$upscale_min_width, "x"))
    if (isTRUE(opts$despeckle))  img <- magick::image_despeckle(img)
    if (isTRUE(opts$adaptive) && exists("image_lat", where = asNamespace("magick")))
      img <- magick::image_lat(img, geometry = opts$adaptive_geometry %||% "25x25+10%")
    if (isTRUE(opts$threshold))  img <- magick::image_threshold(img, type = "black", threshold = "50%")
    # The safety net for every step above, cheap (one downscaled look) and run
    # before any OCR: a page that came out speckled is not used at all.
    ink_after <- .dark_fraction(img)
    if (isTRUE(ink_after > 2 * ink_before + .PP_MAX_ADDED_INK))
      return(structure(in_path, refused = "it would have added speckle"))
    magick::image_write(img, out_path, format = fmt)
    out_path
  }, error = function(e) in_path)
  out
}

# ocr_preprocess_available() -- TRUE when magick is usable.
ocr_preprocess_available <- function() requireNamespace("magick", quietly = TRUE)
