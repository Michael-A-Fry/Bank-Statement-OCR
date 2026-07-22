# OCR support via the system Tesseract engine, driven from R with system2().
# No R 'tesseract' binding and no Python/reticulate are required. The deployment
# host only needs two apt packages installed: `tesseract-ocr` and `poppler-utils`.
#
# Used as the fallback path in read_pdf.R for pages with no usable text layer
# (scanned / image-only statements). OCR only ever reads VISIBLE pixels, so any
# redaction painted over the page is inherently respected - Tesseract cannot read
# what a black box covers - and every OCR'd value is flagged `ocr` with lower
# confidence so forensic reviewers always know machine-read vs. extracted text.

# TRUE only when both external tools are present on PATH.
ocr_available <- function() {
  nzchar(Sys.which("tesseract")) && nzchar(Sys.which("pdftoppm"))
}

# OCR a single image file (PNG/TIFF/JPG) -> character vector of text lines.
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

# OCR an image to Tesseract TSV -> per-word data.frame (incl. `conf` 0-100 and
# bounding box), or NULL. This is what confidence gating + table recovery use.
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

# Mean confidence (0-100) of recognised words on an image; NA when none.
ocr_word_confidence <- function(path, lang = "eng", psm = 6L) {
  df <- ocr_image_tsv(path, lang, psm)
  if (is.null(df) || !("conf" %in% names(df))) return(NA_real_)
  conf <- suppressWarnings(as.numeric(df$conf))
  txt <- if ("text" %in% names(df)) trimws(as.character(df$text)) else rep("", length(conf))
  w <- conf[!is.na(conf) & conf >= 0 & nzchar(txt)]
  if (!length(w)) NA_real_ else round(mean(w), 1)
}

# .ocr_tsv_to_words(tsv, scale) -- Tesseract TSV -> word boxes in PDF POINTS
# (columns x,y,width,height,space,text -- the same shape pdftools::pdf_data uses,
# plus per-word conf/ocr_conf), so the PDF table parser can assign columns for a
# SCANNED statement exactly as for a text-layer one. `scale` = 72/dpi maps image
# pixels to points.
.ocr_tsv_to_words <- function(tsv, scale) {
  if (is.null(tsv) || !nrow(tsv) ||
      !all(c("left", "top", "width", "height", "text") %in% names(tsv))) return(NULL)
  conf <- suppressWarnings(as.numeric(tsv$conf))
  keep <- !is.na(conf) & conf >= 0 & nzchar(trimws(as.character(tsv$text)))
  d <- tsv[keep, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  # `conf` (0-100 per-word confidence) is carried through so the table parser can
  # flag a transaction whose amount/date/balance cell contains a low-confidence
  # word -- a misread digit that a page-mean confidence would otherwise hide.
  # `ocr_conf` is the same figure under the words-frame contract name, so the
  # X-ray view can shade doubtful words; text-layer pages carry it as NA.
  cf <- suppressWarnings(as.numeric(d$conf))
  data.frame(width = d$width * scale, height = d$height * scale,
             x = d$left * scale, y = d$top * scale, space = TRUE,
             text = trimws(as.character(d$text)),
             conf = cf, ocr_conf = cf, stringsAsFactors = FALSE)
}

# Render one PDF page to PNG (poppler pdftoppm) and OCR it.
# Returns list(text = character lines, words = positioned boxes in PDF points,
# conf = mean confidence, ok = logical). The text uses the preprocessed image
# (accuracy); the word boxes use the RAW render (clean, known-scale geometry).
ocr_pdf_page <- function(pdf, page, dpi = 300L, lang = "eng", preprocess = TRUE) {
  if (!ocr_available() || !file.exists(pdf))
    return(list(text = character(0), words = NULL, ok = FALSE, conf = NA_real_))
  prefix <- tempfile("ocrpg_")
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
  # Text pass: full preprocessing (deskew/upscale allowed) for best accuracy.
  use_img <- if (have_pp) preprocess_image(img[1]) else img[1]
  txt <- ocr_image(use_img, lang = lang)
  # Word-box pass: GEOMETRY-PRESERVING preprocessing (no deskew/resize), so the
  # accuracy lift doesn't move any column. Runs in parallel to the text pass.
  box_img <- if (have_pp) preprocess_image(img[1], opts = preprocess_opts_geometry()) else img[1]
  words <- .ocr_tsv_to_words(ocr_image_tsv(box_img, lang = lang), scale = 72 / dpi)
  # The word boxes live in the (possibly deskewed) box-image frame, so report THAT
  # image's size in points as the page dimensions -- keeps the parser's page-size
  # normalisation consistent with where the words actually are.
  bw <- tryCatch({ ii <- magick::image_info(magick::image_read(box_img)); c(ii$width, ii$height) * 72 / dpi },
                 error = function(e) c(NA_real_, NA_real_))
  # Rasterised-redaction detection: solid black rectangles a scanner captured as
  # image (a real blacked-out value) leave no OCR word behind, so find them by
  # pixels here -- in the SAME box-image frame the word boxes use -- and hand the
  # regions back so read_pdf can reconstruct the hidden cells as [REDACTED]
  # (preserved + flagged) instead of silently losing the row.
  dark_rects <- if (exists("detect_dark_regions", mode = "function"))
    tryCatch(detect_dark_regions(magick::image_read(box_img), scale = 72 / dpi),
             error = function(e) NULL) else NULL
  list(text = txt, words = words, ok = length(txt) > 0L,
       conf = ocr_word_confidence(use_img, lang = lang),
       width = bw[1], height = bw[2], dark_rects = dark_rects)
}

# .text_bad_ratio(s) -- fraction of characters that are UNTRUSTWORTHY: the Unicode
# replacement char, C0/C1 control codes (bar tab/newline/CR), and the private-use
# area. A broken-CID / no-ToUnicode font extracts the right LENGTH of such garbage,
# so a high ratio means the "text layer" can't be believed and the page should be
# read by OCR instead.
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

# page_needs_ocr(page_text, word_boxes, ...) -- decide whether a page must be read
# by OCR. Routes on more than a flat character count so it no longer (a) skips a
# scanned transaction page that carries a thin incidental text layer (a Bates
# stamp / footer), (b) trusts corrupt broken-font text of the right length, or
# (c) OCRs a genuine digital page whose pdf_text came back empty but whose word
# boxes are present -- a digital PDF must never be OCR'd.
page_needs_ocr <- function(page_text, word_boxes = NULL, min_chars = 20L,
                           min_words = 3L, max_bad_ratio = 0.30) {
  joined <- paste(page_text %||% "", collapse = "")
  nchar_ns <- nchar(gsub("[[:space:]]", "", joined))
  nwords <- if (is.null(word_boxes)) NA_integer_
            else if (is.data.frame(word_boxes)) nrow(word_boxes) else length(word_boxes)
  have_words <- !is.na(nwords) && nwords >= min_words

  # (c) effectively empty text: OCR only if there are NO real word boxes. Word
  # boxes present => the page HAS a digital text layer; never OCR it.
  if (is.null(page_text) || !nzchar(trimws(joined)) || nchar_ns < min_chars)
    return(!have_words)
  # (b) text present but mostly garbage (broken CID font) -> OCR.
  if (.text_bad_ratio(joined) > max_bad_ratio) return(TRUE)
  # (a) real text but almost no word boxes: a scanned page whose only digital text
  # is an incidental stamp/footer, the transaction rows being image-only -> OCR.
  if (!is.na(nwords) && nwords < min_words) return(TRUE)
  FALSE
}
