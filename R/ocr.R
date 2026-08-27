# OCR via the system Tesseract engine, driven with system2(): just `tesseract-ocr` and `poppler-utils`
# on the host. It reads only VISIBLE pixels, so a painted redaction is inherently respected.

# TRUE only when both external tools are present on PATH.
ocr_available <- function() {
  nzchar(Sys.which("tesseract")) && nzchar(Sys.which("pdftoppm"))
}

# OCR a single image file to a character vector of text lines.
ocr_image <- function(path, lang = "eng", psm = 6L) {
  if (!nzchar(Sys.which("tesseract")) || !file.exists(path)) return(character(0))
  out <- tryCatch(
    system2("tesseract",
            c(path, "stdout", "--psm", as.character(psm), "-l", lang),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  out[!is.na(out)]
}

# OCR an image to Tesseract TSV -> per-word frame including `conf` 0-100 and a bounding box, or NULL.
ocr_image_tsv <- function(path, lang = "eng", psm = 6L) {
  if (!nzchar(Sys.which("tesseract")) || !file.exists(path)) return(NULL)
  out <- tryCatch(
    system2("tesseract",
            c(path, "stdout", "--psm", as.character(psm), "-l", lang, "tsv"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NULL)
  if (is.null(out) || !length(out)) return(NULL)
  tryCatch(utils::read.table(text = paste(out, collapse = "\n"), sep = "\t",
                             header = TRUE, quote = "", comment.char = "",
                             stringsAsFactors = FALSE, fill = TRUE),
           error = function(e) NULL)
}

# Mean confidence of recognised words on an image; NA when none.
ocr_word_confidence <- function(path, lang = "eng", psm = 6L) {
  df <- ocr_image_tsv(path, lang, psm)
  if (is.null(df) || !("conf" %in% names(df))) return(NA_real_)
  conf <- suppressWarnings(as.numeric(df$conf))
  txt <- if ("text" %in% names(df)) trimws(as.character(df$text)) else rep("", length(conf))
  w <- conf[!is.na(conf) & conf >= 0 & nzchar(txt)]
  if (!length(w)) NA_real_ else round(mean(w), 1)
}

# Tesseract TSV to word boxes in PDF POINTS - the shape pdftools::pdf_data uses, plus per-word conf -
# so the table parser columns a SCANNED statement exactly as a text-layer one. `scale` is 72/dpi.
.ocr_tsv_to_words <- function(tsv, scale) {
  if (is.null(tsv) || !nrow(tsv) ||
      !all(c("left", "top", "width", "height", "text") %in% names(tsv))) return(NULL)
  conf <- suppressWarnings(as.numeric(tsv$conf))
  keep <- !is.na(conf) & conf >= 0 & nzchar(trimws(as.character(tsv$text)))
  d <- tsv[keep, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  # `conf` lets the table parser flag a low-confidence digit in a money cell that a page mean hides.
  cf <- suppressWarnings(as.numeric(d$conf))
  data.frame(width = d$width * scale, height = d$height * scale,
             x = d$left * scale, y = d$top * scale, space = TRUE,
             text = trimws(as.character(d$text)),
             conf = cf, ocr_conf = cf, stringsAsFactors = FALSE)
}

# Returns list(text, words, conf, ok). Text uses the preprocessed image; word boxes the RAW render.
ocr_pdf_page <- function(pdf, page, dpi = PARAM_OCR_RENDER_DPI, lang = "eng", preprocess = TRUE) {
  if (!ocr_available() || !file.exists(pdf))
    return(list(text = character(0), words = NULL, ok = FALSE, conf = NA_real_))
  # Inside with_image_scratch(), so ImageMagick's disk spill goes in a folder deleted on the way out.
  with_image_scratch(.ocr_pdf_page_work(pdf, page, dpi, lang, preprocess))
}

# Split out only so ocr_pdf_page() can wrap all of it in one with_image_scratch(). Never call directly.
.ocr_pdf_page_work <- function(pdf, page, dpi, lang, preprocess) {
  prefix <- tempfile("ocrpg_")
  # EVERY intermediate image must be named under `prefix` - they are pictures of a client's statement.
  on.exit(unlink(Sys.glob(paste0(prefix, "*")), force = TRUE), add = TRUE)
  rc <- tryCatch(
    system2("pdftoppm",
            c("-png", "-r", as.character(dpi),
              "-f", as.character(page), "-l", as.character(page),
              pdf, prefix),
            stdout = FALSE, stderr = FALSE),
    error = function(e) 1L
  )
  img <- Sys.glob(paste0(prefix, "*.png"))
  if (!length(img)) return(list(text = character(0), words = NULL, ok = FALSE, conf = NA_real_))
  have_pp <- isTRUE(preprocess) && exists("preprocess_image", mode = "function")
  # Measure the page skew ONCE and share it: both passes deskew the SAME render with the SAME estimator,
  # so re-running the 1.6s projection search per pass buys an identical answer twice.
  skew_angle <- if (have_pp && exists(".detect_skew_angle", mode = "function"))
    tryCatch(.detect_skew_angle(magick::image_read(img[1]), max_angle = 5),
             error = function(e) NULL) else NULL
  # Both preprocessed images go UNDER the render prefix so the one on.exit glob deletes them: each is a
  # full-page picture of the CLIENT'S BANK STATEMENT at 1.4 MB a page, and on a default tempfile() name
  # they survived the life of the R process. Text pass: full preprocessing, deskew and upscale allowed.
  use_img <- if (have_pp)
    preprocess_image(img[1], out_path = paste0(prefix, "_text.png"),
                     opts = c(preprocess_opts(), list(deskew_angle = skew_angle))) else img[1]
  txt <- ocr_image(use_img, lang = lang)
  # Word-box pass: GEOMETRY-PRESERVING preprocessing, no deskew or resize, so the accuracy lift does
  # not move any column. Reuses the shared skew angle.
  box_img <- if (have_pp)
    preprocess_image(img[1], out_path = paste0(prefix, "_box.png"),
                     opts = c(preprocess_opts_geometry(), list(deskew_angle = skew_angle))) else img[1]
  words <- .ocr_tsv_to_words(ocr_image_tsv(box_img, lang = lang), scale = 72 / dpi)
  # Word boxes live in the box-image frame, so report THAT image's point size as the page dimensions.
  bw <- tryCatch({ ii <- magick::image_info(magick::image_read(box_img)); c(ii$width, ii$height) * 72 / dpi },
                 error = function(e) c(NA_real_, NA_real_))
  # Rasterised redactions leave no OCR word behind, so find them by pixels here, in the SAME frame the
  # word boxes use, and hand the regions back for read_pdf to rebuild the cells as [REDACTED].
  dark_rects <- if (exists("detect_dark_regions", mode = "function"))
    tryCatch(detect_dark_regions(magick::image_read(box_img), scale = 72 / dpi),
             error = function(e) NULL) else NULL
  # Page-mean confidence from the SAME box-image words the per-word flags use, so they cannot diverge.
  box_conf <- {
    cf <- if (!is.null(words) && "conf" %in% names(words))
      suppressWarnings(as.numeric(words$conf)) else numeric(0)
    cf <- cf[!is.na(cf) & cf >= 0]
    if (length(cf)) mean(cf) else NA_real_
  }
  list(text = txt, words = words, ok = length(txt) > 0L,
       conf = box_conf,
       width = bw[1], height = bw[2], dark_rects = dark_rects)
}

# Fraction of characters that are UNTRUSTWORTHY: replacement char, C0/C1 controls, private-use area. A
# broken-CID font extracts the right LENGTH of such garbage, so a high ratio means it cannot be believed.
.text_bad_ratio <- function(s) {
  cp <- suppressWarnings(utf8ToInt(enc2utf8(paste(s, collapse = ""))))
  cp <- cp[!is.na(cp)]
  if (!length(cp)) return(0)
  bad <- cp == 0xFFFD |                         # replacement character
         (cp < 32 & !(cp %in% c(9L, 10L, 13L))) |   # C0 controls (keep tab/LF/CR)
         (cp >= 0x7F & cp <= 0x9F) |            # DEL + C1 controls
         (cp >= 0xE000 & cp <= 0xF8FF)          # private-use area (bad CID fonts)
  sum(bad) / length(cp)
}

# Decide whether a page must be read by OCR. More than a flat character count, so it no longer skips a
# scan carrying a thin text layer, trusts broken-font garbage, or OCRs a digital page with empty text.
page_needs_ocr <- function(page_text, word_boxes = NULL, min_chars = PARAM_OCR_MIN_CHARS,
                           min_words = PARAM_OCR_MIN_WORDS, max_bad_ratio = PARAM_OCR_MAX_BAD_RATIO) {
  joined <- paste(page_text %||% "", collapse = "")
  nchar_ns <- nchar(gsub("[[:space:]]", "", joined))
  nwords <- if (is.null(word_boxes)) NA_integer_
            else if (is.data.frame(word_boxes)) nrow(word_boxes) else length(word_boxes)
  have_words <- !is.na(nwords) && nwords >= min_words

  # Effectively empty text: OCR only if there are NO word boxes - boxes mean a digital text layer.
  if (is.null(page_text) || !nzchar(trimws(joined)) || nchar_ns < min_chars)
    return(!have_words)
  # Text present but mostly garbage (broken CID font).
  if (.text_bad_ratio(joined) > max_bad_ratio) return(TRUE)
  # Real text but almost no word boxes: a scan whose only digital text is an incidental stamp.
  if (!is.na(nwords) && nwords < min_words) return(TRUE)
  FALSE
}
