# OCR support via the system Tesseract engine, driven from R with system2().
# No R 'tesseract' binding and no Python/reticulate are required. The deployment
# host only needs two apt packages installed: `tesseract-ocr` and `poppler-utils`.
#
# Used as the fallback path in read_pdf.R for pages with no usable text layer
# (scanned / image-only statements). OCR only ever reads VISIBLE pixels, so any
# redaction painted over the page is inherently respected — Tesseract cannot read
# what a black box covers — and every OCR'd value is flagged `ocr` with lower
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
# (columns x,y,width,height,space,text -- the same shape pdftools::pdf_data uses),
# so the PDF table parser can assign columns for a SCANNED statement exactly as
# for a text-layer one. `scale` = 72/dpi maps image pixels to points.
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
  data.frame(width = d$width * scale, height = d$height * scale,
             x = d$left * scale, y = d$top * scale, space = TRUE,
             text = trimws(as.character(d$text)),
             conf = suppressWarnings(as.numeric(d$conf)), stringsAsFactors = FALSE)
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
  list(text = txt, words = words, ok = length(txt) > 0L,
       conf = ocr_word_confidence(use_img, lang = lang))
}

# Decide whether a page needs OCR: TRUE when its extracted text layer is
# effectively empty (image-only / scanned page).
page_needs_ocr <- function(page_text, min_chars = 20L) {
  joined <- paste(page_text, collapse = "")
  is.null(page_text) || !nzchar(trimws(joined)) ||
    nchar(gsub("[[:space:]]", "", joined)) < min_chars
}
