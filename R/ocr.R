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

# Render one PDF page to PNG (poppler pdftoppm) and OCR it.
# Returns list(text = character lines, ok = logical).
ocr_pdf_page <- function(pdf, page, dpi = 300L, lang = "eng", preprocess = TRUE) {
  if (!ocr_available() || !file.exists(pdf)) return(list(text = character(0), ok = FALSE))
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
  if (!length(img)) return(list(text = character(0), ok = FALSE, conf = NA_real_))
  use_img <- if (isTRUE(preprocess) && exists("preprocess_image", mode = "function"))
               preprocess_image(img[1]) else img[1]
  txt <- ocr_image(use_img, lang = lang)
  list(text = txt, ok = length(txt) > 0L,
       conf = ocr_word_confidence(use_img, lang = lang))
}

# Decide whether a page needs OCR: TRUE when its extracted text layer is
# effectively empty (image-only / scanned page).
page_needs_ocr <- function(page_text, min_chars = 20L) {
  joined <- paste(page_text, collapse = "")
  is.null(page_text) || !nzchar(trimws(joined)) ||
    nchar(gsub("[[:space:]]", "", joined)) < min_chars
}
