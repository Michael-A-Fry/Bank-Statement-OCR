# detect_redaction.R -- automatic detection of RASTERISED redactions (solid black
# rectangles painted onto a scanned/image page) and reconstruction of the hidden
# cells as [REDACTED] tokens.
#
# WHY THIS EXISTS. read_pdf's redaction guard replaces WORDS that sit under a
# supplied rectangle with [REDACTED]. That works when the redaction is a text-layer
# overlay or a marker. But a REAL redaction -- black marker / filled box on a page
# that was then scanned -- leaves NO word under it at all: OCR reads black pixels
# as nothing, so the cell is simply ABSENT. A transaction whose amount was blacked
# out then loses its amount, fails the keep test, and is DROPPED WITH NO FLAG --
# silent loss of a real row, the worst forensic outcome. read_pdf.R's own notes
# (the detect_overlay_redactions header) anticipated this: "rasterise each page and
# detect solid opaque rectangles ... then feed the SAME rects structure". This is
# that detector, plus the token reconstruction the OCR path needs.
#
# APPROACH. On an OCR (image) page: (1) find solid dark rectangles by a row-run
# projection with a fill-ratio gate (a redaction box is WIDE, TALL and ~solid --
# unlike text, thin rules, or a logo); (2) mark the cells of VISIBLE transaction
# rows that a box covers as [REDACTED], so a row with a blacked amount keeps its
# real date/description and is flagged, not dropped. We do NOT reconstruct rows
# that are fully hidden: a solid block has no visible row to anchor to, so nothing
# is added and those transactions simply do not appear (their neighbours above and
# below are untouched). We NEVER guess how many rows a black block hid. Reading
# only what is visible; accounting for it; inventing nothing.

.empty_rects <- function() data.frame(x0 = numeric(0), y0 = numeric(0),
                                      x1 = numeric(0), y1 = numeric(0),
                                      stringsAsFactors = FALSE)

# detect_dark_regions(img, scale, ...) -> data.frame(x0,y0,x1,y1) in PDF POINTS.
#   img       a magick image of the rendered page (the SAME frame the OCR words
#             live in, so the rects and the words share one coordinate space)
#   scale     points-per-pixel of that render (72 / dpi)
#   min_frac_w  a redaction run must span at least this fraction of page width
#   min_band_pts a redaction band must be at least this tall (points)
#   dark_thresh  0-255 grey below which a pixel counts as "black"
#   fill_ratio   the band's bounding box must be at least this dark-solid
detect_dark_regions <- function(img, scale, min_frac_w = 0.10, min_band_pts = 10,
                                dark_thresh = 60L, fill_ratio = 0.55,
                                small_w = 340L, gap_rows = 2L) {
  if (!requireNamespace("magick", quietly = TRUE) || is.null(img)) return(.empty_rects())
  info <- tryCatch(magick::image_info(img), error = function(e) NULL)
  if (is.null(info) || !nrow(info) || is.na(info$width) || info$width == 0) return(.empty_rects())
  W0 <- info$width; H0 <- info$height
  small <- tryCatch(magick::image_convert(magick::image_resize(img, paste0(small_w, "x")),
                                          colorspace = "gray"), error = function(e) NULL)
  if (is.null(small)) return(.empty_rects())
  si <- magick::image_info(small); sw <- si$width; sh <- si$height
  g <- tryCatch(magick::image_data(small, channels = "gray"), error = function(e) NULL)
  if (is.null(g)) return(.empty_rects())
  m <- matrix(as.integer(g[1, , ]), nrow = sw, ncol = sh)   # [x, y]; 0 black .. 255 white
  dark <- m < dark_thresh
  # per small-row: the longest run of consecutive dark pixels and its x-extent.
  runs <- lapply(seq_len(sh), function(y) {
    dr <- dark[, y]; if (!any(dr)) return(c(0, NA, NA))
    r <- rle(dr); di <- which(r$values); if (!length(di)) return(c(0, NA, NA))
    ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
    k <- di[which.max(r$lengths[di])]; c(r$lengths[k], starts[k], ends[k])
  })
  runlen <- vapply(runs, function(z) z[1], numeric(1))
  isdark <- runlen >= (min_frac_w * sw)
  if (!any(isdark)) return(.empty_rects())
  # group consecutive dark rows into bands (tolerating a couple of gap rows).
  idx <- which(isdark); bands <- list(); start <- idx[1]; prev <- idx[1]
  for (i in idx[-1]) {
    if (i - prev <= gap_rows + 1L) prev <- i
    else { bands[[length(bands) + 1L]] <- c(start, prev); start <- i; prev <- i }
  }
  bands[[length(bands) + 1L]] <- c(start, prev)
  ppx <- (W0 / sw) * scale; ppy <- (H0 / sh) * scale   # points per small-pixel
  out <- lapply(bands, function(b) {
    ys <- seq(b[1], b[2])
    if ((b[2] - b[1] + 1) * ppy < min_band_pts) return(NULL)
    x0 <- min(vapply(runs[ys], function(z) z[2], numeric(1)), na.rm = TRUE)
    x1 <- max(vapply(runs[ys], function(z) z[3], numeric(1)), na.rm = TRUE)
    # solid-fill gate: fraction of dark pixels in the band's bounding box. A real
    # redaction box is ~solid (~1.0); a paragraph of text or a logo is sparse.
    sub <- dark[x0:x1, ys, drop = FALSE]
    if (mean(sub) < fill_ratio) return(NULL)
    data.frame(x0 = (x0 - 1) * ppx, y0 = (b[1] - 1) * ppy,
               x1 = x1 * ppx, y1 = b[2] * ppy, stringsAsFactors = FALSE)
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) .empty_rects() else do.call(rbind, out)
}

# inject_redaction_tokens(words, rects, row_tol) -> words with synthetic
# [REDACTED] boxes added ONLY onto the visible OCR rows a rect overlaps, so a
# PARTIALLY-blacked transaction keeps its visible cells and gains a [REDACTED]
# marker where the box covers it (recorded, flagged -- not dropped). It does NOT
# reconstruct rows that are fully hidden: a solid block has no visible row to
# anchor to, so nothing is added and the hidden transactions simply do not appear
# (their neighbours above/below are untouched). We never estimate how many rows a
# black block hid, and a box over a header / non-transaction line -- which has no
# real date or amount -- is never turned into a transaction (the evidence gate).
inject_redaction_tokens <- function(words, rects, row_tol = 3, x_step = 34) {
  if (is.null(rects) || !nrow(rects) || is.null(words)) return(words)
  vis <- words[!(words$redacted %in% TRUE) & nzchar(trimws(words$text)), , drop = FALSE]
  if (!nrow(vis)) return(words)   # nothing visible to anchor to -> invent nothing
  o <- order(vis$y); vy <- vis$y[o]; vh <- vis$height[o]; vt <- vis$text[o]
  grp <- .group_rows(vy, row_tol)

  add_x <- numeric(0); add_y <- numeric(0); add_w <- numeric(0); add_h <- numeric(0)
  for (g in unique(grp)) {
    idx <- which(grp == g)
    ry0 <- min(vy[idx]); ry1 <- max(vy[idx] + vh[idx])
    rowy <- min(vy[idx]); rowh <- stats::median(vh[idx])
    # evidence gate: only a row that still shows a real DATE or AMOUNT is a
    # transaction whose blacked cells we should record. A header/address line has
    # neither, so a box over it never becomes a row.
    if (!.redaction_row_evidence(paste(vt[idx], collapse = " "))) next
    for (r in seq_len(nrow(rects))) {
      x0 <- rects$x0[r]; y0 <- rects$y0[r]; x1 <- rects$x1[r]; y1 <- rects$y1[r]
      if (!all(is.finite(c(x0, y0, x1, y1))) || x1 <= x0 || y1 <= y0) next
      if (!(ry1 >= y0 && ry0 <= y1)) next          # rect must overlap THIS row's band
      xs <- seq(x0, x1 - 1, by = x_step); if (!length(xs)) xs <- x0
      for (xx in xs) {
        add_x <- c(add_x, xx); add_y <- c(add_y, rowy)
        add_w <- c(add_w, min(x_step - 4, x1 - xx)); add_h <- c(add_h, rowh)
      }
    }
  }
  if (!length(add_x)) return(words)
  addf <- data.frame(width = add_w, height = add_h, x = add_x, y = add_y,
                     space = TRUE, text = REDACTION_TOKEN, redacted = TRUE,
                     stringsAsFactors = FALSE)
  if ("conf" %in% names(words)) addf$conf <- NA_real_
  for (cn in setdiff(names(words), names(addf))) addf[[cn]] <- NA
  addf <- addf[, names(words), drop = FALSE]
  rbind(words, addf)
}

# .redaction_row_evidence(text) -- TRUE when a visible row still shows a real
# transaction anchor (a money amount or a date). Keeps a partially-redacted
# transaction row eligible; rejects a header / name / address line so a box over
# it is never fabricated into a transaction.
.redaction_row_evidence <- function(text) {
  t <- as.character(text)
  grepl("[0-9]+[.,][0-9]{2}", t) ||                          # a money amount (40.00)
  grepl("\\b[0-9]{1,2}[ /-][A-Za-z]{3}", t) ||               # a date (21 Apr / 21-Apr)
  grepl("\\b[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}\\b", t)   # a date (21/04/2026)
}
