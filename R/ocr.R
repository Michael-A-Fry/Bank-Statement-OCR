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

# Seconds one Tesseract run may spend on one page picture. A clean 300 dpi A4
# page reads in 1-3 s on one thread; a run past this is reading speckle (stage-1
# measurement: one page ran for over 30 minutes), and is stopped so a 30-page scan
# can never take hours. The page is then read from its other picture or reported
# unread -- never left half-read.
.OCR_RUN_SECONDS <- 60L

# A page whose words were read with a median confidence under this is noise, not
# text: a clean scan reads at 95-97, the speckled pages at 17.
.OCR_NOISE_CONF <- 50

# .tesseract(args, seconds) -- run the tesseract binary -> list(out, timed_out).
# OpenMP THREADS: tesseract takes a thread per core and spin-waits on them, so
# when anything else wants the CPU it collapses. Measured on a 4-core box: one
# page alone reads in 2.2 s with 4 threads and 2.4 s with 1; three pages at once
# read in 2.4 s with 1 thread each and had not finished after 300 s with the
# default. So unless the caller has set its own limit (R/jobs.R does, per job),
# one thread.
.tesseract <- function(args, seconds = .OCR_RUN_SECONDS) {
  if (!nzchar(Sys.which("tesseract"))) return(list(out = NULL, timed_out = FALSE))
  if (!nzchar(Sys.getenv("OMP_THREAD_LIMIT"))) {
    Sys.setenv(OMP_THREAD_LIMIT = "1")
    on.exit(Sys.unsetenv("OMP_THREAD_LIMIT"), add = TRUE)
  }
  out <- tryCatch(suppressWarnings(system2("tesseract", args, stdout = TRUE, stderr = FALSE,
                                           timeout = seconds)),
                  error = function(e) NULL)
  list(out = out, timed_out = identical(as.integer(attr(out, "status")), 124L))
}

# .tesseract_tsv(path, lang, psm, seconds) -> list(tsv = per-word data.frame or
# NULL, timed_out). A run that hit the time limit returns no words at all: half a
# page read is worse than none.
.tesseract_tsv <- function(path, lang = "eng", psm = 6L, seconds = .OCR_RUN_SECONDS) {
  if (!file.exists(path)) return(list(tsv = NULL, timed_out = FALSE))
  r <- .tesseract(c(path, "stdout", "--psm", as.character(psm), "-l", lang, "tsv"), seconds)
  if (r$timed_out || !length(r$out)) return(list(tsv = NULL, timed_out = r$timed_out))
  tsv <- tryCatch(utils::read.table(text = paste(r$out, collapse = "\n"), sep = "\t",
                                    header = TRUE, quote = "", comment.char = "",
                                    stringsAsFactors = FALSE, fill = TRUE),
                  error = function(e) NULL)
  list(tsv = tsv, timed_out = FALSE)
}

# OCR an image to Tesseract TSV -> per-word data.frame (incl. `conf` 0-100 and
# bounding box), or NULL. This is what confidence gating + table recovery use.
ocr_image_tsv <- function(path, lang = "eng", psm = 6L) {
  if (!nzchar(Sys.which("tesseract"))) return(NULL)
  .tesseract_tsv(path, lang, psm)$tsv
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

# .ocr_tsv_lines(tsv) -- the page's text, one string per printed line, from the
# same TSV the word boxes come from (Tesseract's plain-text output is these words
# joined line by line, so a second recognition run for it bought nothing but time).
.ocr_tsv_lines <- function(tsv) {
  if (is.null(tsv) || !nrow(tsv) ||
      !all(c("block_num", "par_num", "line_num", "text") %in% names(tsv))) return(character(0))
  txt <- trimws(as.character(tsv$text)); txt[is.na(txt)] <- ""
  conf <- suppressWarnings(as.numeric(tsv$conf))
  keep <- nzchar(txt) & !is.na(conf) & conf >= 0
  if (!any(keep)) return(character(0))
  key <- paste(tsv$block_num, tsv$par_num, tsv$line_num)[keep]
  unname(vapply(split(txt[keep], factor(key, levels = unique(key))),
                paste, character(1), collapse = " "))
}

# .ocr_read(img, lang, dpi) -- one recognition of one page picture: words in
# points, text lines, median word confidence, the picture's size in points (from
# the TSV's page row) and whether the run was stopped at the time limit.
.ocr_read <- function(img, lang, dpi) {
  r <- .tesseract_tsv(img, lang = lang)
  tsv <- r$tsv
  words <- .ocr_tsv_to_words(tsv, scale = 72 / dpi)
  cf <- if (is.null(words)) numeric(0) else words$conf[!is.na(words$conf)]
  pg <- if (!is.null(tsv) && "level" %in% names(tsv)) tsv[tsv$level == 1, , drop = FALSE] else NULL
  size <- if (!is.null(pg) && nrow(pg)) c(pg$width[1], pg$height[1]) * 72 / dpi else c(NA_real_, NA_real_)
  list(words = words, text = .ocr_tsv_lines(tsv), timed_out = r$timed_out,
       median = if (length(cf)) stats::median(cf) else NA_real_,
       mean = if (length(cf)) mean(cf) else NA_real_, width = size[1], height = size[2])
}

# Render one PDF page and OCR it.
# Returns list(text = character lines, words = positioned boxes in PDF points,
# conf = mean word confidence, ok = logical, width/height = page size in points,
# timed_out = TRUE when every reading of the page hit the time limit, note = what
# the safety nets did, or "").
ocr_pdf_page <- function(pdf, page, dpi = PARAM_OCR_RENDER_DPI, lang = "eng", preprocess = TRUE) {
  if (!ocr_available() || !file.exists(pdf))
    return(list(text = character(0), words = NULL, ok = FALSE, conf = NA_real_,
                timed_out = FALSE, note = ""))
  # All of this page's image work happens inside with_image_scratch(), so
  # ImageMagick's disk spill goes in a folder of ours that is deleted on the way
  # out (see R/util.R). Nothing below reads a magick image after this returns.
  with_image_scratch(.ocr_pdf_page_work(pdf, page, dpi, lang, preprocess))
}

# The actual page work, split out only so ocr_pdf_page() above can wrap the whole
# of it in one with_image_scratch() without indenting every line. Never call this
# directly -- call ocr_pdf_page().
.ocr_pdf_page_work <- function(pdf, page, dpi, lang, preprocess) {
  prefix <- tempfile("ocrpg_")
  # EVERY intermediate image this function makes must be named under `prefix`, so
  # this single sweep removes all of them -- the poppler render AND the cleaned
  # copy. They are pictures of a client's statement; leaving any of them behind
  # leaks readable client data onto the host's disk.
  on.exit(unlink(Sys.glob(paste0(prefix, "*")), force = TRUE), add = TRUE)
  fail <- list(text = character(0), words = NULL, ok = FALSE, conf = NA_real_,
               timed_out = FALSE, note = "")
  # Grey and uncompressed (PGM): Tesseract reads grey anyway, and a 300 dpi page
  # is written and read back in a fraction of the time a PNG takes.
  tryCatch(system2("pdftoppm",
                   c("-gray", "-r", as.character(dpi),
                     "-f", as.character(page), "-l", as.character(page), pdf, prefix),
                   stdout = FALSE, stderr = FALSE),
           error = function(e) 1L)
  raw <- Sys.glob(paste0(prefix, "*.pgm"))
  if (!length(raw)) return(fail)
  raw <- raw[1]
  # One cleaned picture serves both the words and the text. It is
  # GEOMETRY-PRESERVING (see preprocess_opts_geometry): no resize, and a deskew
  # cropped back to the render's canvas, so pixels map to points at 72/dpi.
  # preprocess_image() hands back the render itself when the clean-up would add
  # ink (its cheap pre-OCR check).
  img <- if (isTRUE(preprocess) && ocr_preprocess_available())
    preprocess_image(raw, out_path = paste0(prefix, "_pp.pgm"), opts = preprocess_opts_geometry())
  else raw
  rd <- .ocr_best_reading(img, raw, lang, dpi)
  if (rd$timed_out) {
    fail$timed_out <- TRUE
    fail$note <- sprintf("OCR gave up: every reading of the page ran past %d s", .OCR_RUN_SECONDS)
    return(fail)
  }
  list(text = rd$text, words = rd$words, ok = length(rd$text) > 0L,
       conf = rd$mean, width = rd$width, height = rd$height,
       timed_out = FALSE, note = rd$note)
}

# .ocr_best_reading(img, raw, lang, dpi) -- read the cleaned picture `img`; when
# it read as noise or ran out of time, read the render `raw` as well and keep the
# surer reading (the after-OCR safety net). The reading carries `note`: what the
# nets did, "" when nothing.
.ocr_best_reading <- function(img, raw, lang, dpi) {
  note <- character(0)
  if (!is.null(attr(img, "refused"))) note <- paste("image clean-up not used:", attr(img, "refused"))
  img <- as.vector(img)
  rd <- .ocr_read(img, lang, dpi)
  if (!identical(img, raw) && (rd$timed_out || !isTRUE(rd$median >= .OCR_NOISE_CONF))) {
    rd2 <- .ocr_read(raw, lang, dpi)
    if (!rd2$timed_out && (rd$timed_out || (is.finite(rd2$median) &&
                                            (!is.finite(rd$median) || rd2$median > rd$median)))) {
      conf <- function(r) if (r$timed_out) "it ran out of time"
                          else if (!is.finite(r$median)) "no words"
                          else sprintf("confidence %.0f", r$median)
      note <- c(note, sprintf("read without image clean-up (%s; with it, %s)", conf(rd2), conf(rd)))
      rd <- rd2
    }
  }
  rd$note <- paste(note, collapse = "; ")
  rd
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
page_needs_ocr <- function(page_text, word_boxes = NULL, min_chars = PARAM_OCR_MIN_CHARS,
                           min_words = PARAM_OCR_MIN_WORDS, max_bad_ratio = PARAM_OCR_MAX_BAD_RATIO) {
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
