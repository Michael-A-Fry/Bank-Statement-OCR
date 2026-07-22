# read_pdf.R -- PDF text + word-box reader (pdftools) with a forensic redaction
# guard. Extraction only: this surfaces page text, positioned word boxes, and
# detected sections. Full per-bank PDF transaction-table parsing is future work.
#
# FORENSIC RULE (build-contract section 11.2): text hidden under a redaction
# overlay must NEVER be emitted. Any word covered by a redaction -- whether the
# source already carries a redaction marker in its text layer, or a rectangle
# overlay sits on top of it -- is replaced by REDACTED_TOKEN and its underlying
# text is discarded before anything leaves this module. Over-redaction (dropping
# a word on any overlap) is the deliberate, safe failure mode.

# Reuse the canonical token from parse.R when co-sourced; fall back otherwise so
# this file is usable on its own.
if (!exists("REDACTION_TOKEN")) REDACTION_TOKEN <- "[REDACTED]"

# ---------------------------------------------------------------------------
# Redaction markers already present in the text layer.
#
# A specimen/source PDF may bake a redaction directly into its extractable text
# (a run of block glyphs, an explicit [REDACTED], a long XXXX mask, etc.). These
# heuristics catch those. This list is intentionally a template -- extend it as
# new marker conventions are encountered.
# ---------------------------------------------------------------------------
# Block/shade glyphs commonly used to visually blank out text. Kept as a
# separate vector so the pattern is built with explicit UTF-8 encoding (this
# engine runs in a C locale where raw multibyte regex literals are unreliable).
.PDF_BLOCK_GLYPHS <- c("█", "▓", "▒", "░", "■",
                       "▬", "▮", "▀", "▄", "█")

pdf_redaction_markers <- function() {
  block_run <- paste0(
    "(?:", paste(unique(.PDF_BLOCK_GLYPHS), collapse = "|"), "){1,}")
  c(
    "\\[REDACTED\\]",   # explicit marker
    "\\bREDACTED\\b",   # bare word
    block_run,          # run of block/shade glyphs
    "X{6,}",            # long XXXXXX mask
    "#{6,}"             # long hash mask
  )
}

# .matches_marker(text, markers) -- logical vector: does each string contain a
# redaction marker? Matched at the byte level (useBytes) so block glyphs match
# reliably even under a C locale, where re-encoding would mangle multibyte runs.
.matches_marker <- function(text, markers) {
  text <- as.character(text)
  hit <- rep(FALSE, length(text))
  for (m in markers) {
    hit <- hit | grepl(m, text, perl = TRUE, useBytes = TRUE)
  }
  hit & !is.na(text)
}

# ---------------------------------------------------------------------------
# Rectangle-overlay detection HOOK.
#
# detect_overlay_redactions(words, rects) flags every word box that overlaps a
# supplied redaction rectangle. `rects` is a data.frame with columns
# x0, y0, x1, y1 (top-left origin, matching pdftools word coordinates) OR NULL.
#
# >>> WHERE REAL IMAGE-RECTANGLE DETECTION PLUGS IN <<<
# pdftools does not expose vector fill operators or a rasteriser, and tesseract
# is not installed in this environment, so `rects` is currently supplied by the
# caller (or a per-bank template) rather than derived automatically. A true
# implementation would populate `rects` by either:
#   (a) parsing the PDF content stream for filled rectangles (`re` + `f`/`F`
#       operators) whose fill colour is near-black and whose area is large
#       enough to hide text; or
#   (b) rendering each page to a raster (pdftools::pdf_render_page) and detecting
#       solid opaque rectangles via connected-component analysis, then mapping
#       raster pixels back to PDF points.
# Both feed the SAME `rects` structure consumed here, so the guard below does not
# change when that detector is added -- only the source of `rects` does.
# ---------------------------------------------------------------------------
detect_overlay_redactions <- function(words, rects = NULL) {
  n <- nrow(words)
  if (is.null(rects) || nrow(rects) == 0 || n == 0) return(rep(FALSE, n))
  wx0 <- words$x
  wy0 <- words$y
  wx1 <- words$x + words$width
  wy1 <- words$y + words$height
  covered <- rep(FALSE, n)
  for (r in seq_len(nrow(rects))) {
    rx0 <- rects$x0[r]; ry0 <- rects$y0[r]
    rx1 <- rects$x1[r]; ry1 <- rects$y1[r]
    # axis-aligned overlap (any overlap => covered; conservative on purpose)
    overlap <- (wx0 < rx1) & (wx1 > rx0) & (wy0 < ry1) & (wy1 > ry0)
    covered <- covered | overlap
  }
  covered
}

# ---------------------------------------------------------------------------
# The redaction guard.
#
# apply_redaction_guard(words, rects, markers) -> words with:
#   * a logical `redacted` column,
#   * every redacted word's `text` overwritten with REDACTION_TOKEN and its
#     ORIGINAL text discarded (never retained anywhere in the returned object).
# This is the single choke point every emitted PDF word passes through.
# ---------------------------------------------------------------------------
apply_redaction_guard <- function(words, rects = NULL,
                                  markers = pdf_redaction_markers()) {
  words <- as.data.frame(words, stringsAsFactors = FALSE)
  if (nrow(words) == 0) {
    words$redacted <- logical(0)
    return(words)
  }
  by_marker  <- .matches_marker(words$text, markers)
  by_overlay <- detect_overlay_redactions(words, rects)
  redacted <- by_marker | by_overlay
  # Discard the underlying text of every redacted word BEFORE returning it.
  words$text[redacted] <- REDACTION_TOKEN
  words$redacted <- redacted
  words
}

# ---------------------------------------------------------------------------
# Reconstruct page text from (already guarded) word boxes. Used whenever a page
# carried any redaction, because pdftools::pdf_text reads the raw text layer and
# would leak text sitting under an overlay. Deterministic: words grouped into
# lines by rounded y, ordered by x.
# ---------------------------------------------------------------------------
words_to_text <- function(words, line_tol = 3) {
  if (nrow(words) == 0) return("")
  o <- order(words$y, words$x)
  w <- words[o, , drop = FALSE]
  line_key <- cumsum(c(TRUE, diff(w$y) > line_tol))
  lines <- vapply(split(w$text, line_key), function(tok)
    paste(tok, collapse = " "), character(1))
  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Section detection by anchor phrases.
#
# detect_pdf_sections(pages_text, anchors) scans each page's lines for anchor
# phrases (case-insensitive, whole-line-ish header match) and returns a
# data.frame(section, page, line_no, matched_text). Deterministic.
# ---------------------------------------------------------------------------
pdf_section_anchors <- function() {
  c(
    "YOUR CARD SUMMARY", "YOUR DETAILS", "OUR DETAILS", "ABOUT THIS DOCUMENT",
    "YOUR ANZ CREDIT CARD DETAILS", "ACCOUNT SUMMARY", "ACCOUNT DETAILS",
    "STATEMENT PERIOD", "OPENING BALANCE", "CLOSING BALANCE",
    "TRANSACTION DETAILS", "TRANSACTIONS", "INTEREST", "FEES",
    "PAYMENT DETAILS", "SUMMARY"
  )
}

detect_pdf_sections <- function(pages_text, anchors = pdf_section_anchors()) {
  out <- data.frame(section = character(0), page = integer(0),
                    line_no = integer(0), matched_text = character(0),
                    stringsAsFactors = FALSE)
  if (length(pages_text) == 0) return(out)
  for (p in seq_along(pages_text)) {
    lines <- strsplit(pages_text[[p]] %||% "", "\n", fixed = TRUE)[[1]]
    if (length(lines) == 0) next
    lines_lc <- tolower(lines)
    for (a in anchors) {
      # anchor phrase anywhere in the line, matched literally (case-insensitive)
      hits <- which(grepl(tolower(a), lines_lc, fixed = TRUE))
      for (h in hits) {
        out <- rbind(out, data.frame(
          section = a, page = p, line_no = h,
          matched_text = trimws(lines[h]), stringsAsFactors = FALSE))
      }
    }
  }
  out[order(out$page, out$line_no), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# read_pdf(path, redaction_rects, markers, anchors) -> list(
#   pages        character[]  per-page text (redaction-safe),
#   words        list<df>     per-page guarded word boxes (x,y,width,height,
#                             space,text,redacted,ocr_conf),
#   page_count   integer,
#   sections     data.frame   detected section anchors,
#   redactions   data.frame   per-page redacted-word counts,
#   ok           logical      whether pdftools extraction succeeded
# )
#
# `redaction_rects`: optional named list keyed by page number (as character or
# integer), each element a data.frame(x0,y0,x1,y1). This is the structure the
# rectangle-overlay detector documented above will populate automatically.
# ---------------------------------------------------------------------------
read_pdf <- function(path, redaction_rects = NULL,
                     markers = pdf_redaction_markers(),
                     anchors = pdf_section_anchors(),
                     scan_vector = TRUE, vector_dpi = 150) {
  empty <- list(pages = character(0), words = list(), page_count = NA_integer_,
                sections = detect_pdf_sections(character(0)),
                redactions = data.frame(page = integer(0), redacted_words = integer(0),
                                        stringsAsFactors = FALSE),
                ocr = logical(0),
                ocr_conf = numeric(0),
                ok = FALSE)
  if (!requireNamespace("pdftools", quietly = TRUE)) return(empty)
  if (!file.exists(path)) return(empty)

  raw_text  <- safe(suppressMessages(pdftools::pdf_text(path)), NULL)
  word_list <- safe(suppressMessages(pdftools::pdf_data(path)), NULL)
  if (is.null(raw_text)) return(empty)

  np <- length(raw_text)
  # Per-page point dimensions. A template's x-bands are drawn in ONE page's point
  # space; the parser normalises each page's words into that space (see
  # parse_pdf_table), which needs the page size. Same for OCR pages -- pdf_pagesize
  # reports the page's own point size, which is what the OCR word coordinates use.
  psize <- safe(suppressMessages(pdftools::pdf_pagesize(path)), NULL)
  page_width  <- if (!is.null(psize) && "width"  %in% names(psize)) as.numeric(psize$width)  else rep(NA_real_, np)
  page_height <- if (!is.null(psize) && "height" %in% names(psize)) as.numeric(psize$height) else rep(NA_real_, np)
  length(page_width)  <- np
  length(page_height) <- np
  # pdf_data may return fewer/NULL entries on odd pages; normalise to np slots.
  if (is.null(word_list)) word_list <- vector("list", np)

  pages <- character(np)
  words <- vector("list", np)
  red_counts <- integer(np)
  ocr_flags <- rep(FALSE, np)
  ocr_conf <- rep(NA_real_, np)
  # Pages whose vector-redaction scan could NOT run (no rasteriser) -> surfaced as
  # a loud "redactions not verified" warning downstream, never a silent clean pass.
  red_scan_incomplete <- rep(FALSE, np)

  # OCR is attempted whenever the page's TEXT is effectively empty/sparse -- not
  # only when there are zero word boxes. That covers a scanned transaction page
  # that also carries a thin digital text layer (a Bates stamp, footer or
  # watermark): box-count alone would treat it as a text page and silently yield
  # no rows. Safely no-ops where the OCR tools (R/ocr.R + tesseract/poppler) are
  # absent.
  ocr_ready <- exists("ocr_available", mode = "function") && ocr_available() &&
               exists("page_needs_ocr", mode = "function")

  for (p in seq_len(np)) {
    wp <- word_list[[p]]
    rects_p <- .rects_for_page(redaction_rects, p)
    if (is.null(wp) || nrow(wp) == 0) {
      # No word boxes -> emit raw text as-is; a marker sweep still applies so
      # baked-in redaction tokens never survive even without geometry.
      txt <- raw_text[[p]]
      for (m in markers) txt <- gsub(m, REDACTION_TOKEN, txt,
                                     perl = TRUE, useBytes = TRUE)
      pages[p] <- txt
      words[[p]] <- apply_redaction_guard(
        data.frame(width = integer(0), height = integer(0), x = integer(0),
                   y = integer(0), space = logical(0), text = character(0),
                   stringsAsFactors = FALSE), NULL, markers)
      red_counts[p] <- 0L
    } else {
      wp <- as.data.frame(wp, stringsAsFactors = FALSE)
      guarded <- apply_redaction_guard(wp, rects_p, markers)
      words[[p]] <- guarded
      n_red <- sum(guarded$redacted)
      red_counts[p] <- n_red
      # If ANY redaction touched this page, do NOT trust the raw text layer
      # (it exposes text under overlays); rebuild the page from guarded boxes.
      pages[p] <- if (n_red > 0) words_to_text(guarded) else raw_text[[p]]
    }

    # OCR fallback. Tesseract reads only VISIBLE pixels, so any redaction painted
    # on the page is inherently unreadable, and the OCR word boxes go through the
    # SAME redaction guard. Each OCR'd page is flagged so downstream knows the
    # text was machine-read, not extracted.
    if (ocr_ready && page_needs_ocr(pages[p])) {
      res <- ocr_pdf_page(path, p)
      if (isTRUE(res$ok)) {
        otxt <- paste(res$text, collapse = "\n")
        for (m in markers) otxt <- gsub(m, REDACTION_TOKEN, otxt,
                                        perl = TRUE, useBytes = TRUE)
        ocr_flags[p] <- TRUE
        ocr_conf[p] <- res$conf %||% NA_real_
        # OCR word boxes live in the (deskewed) render frame -> report that frame's
        # point size as this page's dimensions, so band normalisation stays aligned.
        if (!is.null(res$width) && is.finite(res$width) && res$width > 0)   page_width[p]  <- res$width
        if (!is.null(res$height) && is.finite(res$height) && res$height > 0) page_height[p] <- res$height
        if (!is.null(res$words) && nrow(res$words)) {
          # Auto-detected rasterised redactions (solid black boxes) are added to
          # any caller-supplied rects, then any VISIBLE row a box covers has its
          # blacked cell marked [REDACTED] so that partial row keeps its visible
          # data (flagged), never dropped. Fully-hidden rows have no visible anchor
          # and simply do not appear -- we never guess how many a block hid.
          auto_rects <- res$dark_rects
          all_rects <- if (!is.null(auto_rects) && nrow(auto_rects)) {
            base_rects <- if (is.null(rects_p)) NULL else rects_p[, c("x0","y0","x1","y1"), drop = FALSE]
            rbind(base_rects, auto_rects)
          } else rects_p
          guarded_ocr <- apply_redaction_guard(res$words, all_rects, markers)
          if (!is.null(auto_rects) && nrow(auto_rects) &&
              exists("inject_redaction_tokens", mode = "function"))
            guarded_ocr <- inject_redaction_tokens(guarded_ocr, auto_rects,
                                                   row_tol = 3)
          words[[p]] <- guarded_ocr
          nred_ocr <- sum(guarded_ocr$redacted)
          red_counts[p] <- nred_ocr
          # Keep pages[p] consistent with the guarded OCR boxes, so an overlay
          # redaction reaches the metadata/section text too (parity with the
          # text-layer path above).
          pages[p] <- if (nred_ocr > 0) words_to_text(guarded_ocr) else otxt
        } else {
          pages[p] <- otxt
        }
      }
    }

    # Digital vector-redaction guard. A page with a full text layer never triggers
    # OCR, so a solid rectangle DRAWN over still-present text (a vector redaction)
    # would leak the text under it -- pdf_text/pdf_data read the layer, not the
    # picture. Rasterise the page and mark any word whose rendered box is ~solid
    # dark as redacted, the same visibility test the scanned path uses, here at
    # word granularity. Skipped when the page was OCR'd (already covered) or off.
    if (scan_vector && !ocr_flags[p]) {
      gp <- words[[p]]
      if (!is.null(gp) && nrow(gp) > 0) {
        occ <- detect_occluded_words(path, p, gp, page_width[p], page_height[p],
                                     dpi = vector_dpi)
        if (isTRUE(occ$ok)) {
          new_hits <- occ$occluded & !(gp$redacted %in% TRUE)
          if (any(new_hits)) {
            gp$text[new_hits] <- REDACTION_TOKEN
            gp$redacted <- gp$redacted | occ$occluded
            words[[p]] <- gp
            red_counts[p] <- sum(gp$redacted %in% TRUE)
            pages[p] <- words_to_text(gp)      # rebuild text WITHOUT the hidden words
          }
        } else {
          red_scan_incomplete[p] <- TRUE       # loud fallback: couldn't verify
        }
      }
    }
  }
  # LOUD fallback: if any page could not be rasterised to check for vector
  # redactions, say so once -- the visible text on those pages is NOT
  # redaction-verified and must be treated with caution, never assumed clean.
  if (any(red_scan_incomplete))
    warning(sprintf(paste0("read_pdf: could not rasterise %d page(s) to verify ",
      "vector redactions; visible text on those pages is not redaction-checked"),
      sum(red_scan_incomplete)), call. = FALSE)

  # Words-frame contract: every page's words carry a per-word `ocr_conf` column
  # -- Tesseract's 0-100 word confidence on an OCR page, NA on a text-layer page
  # (typeset text has no recognition step, so there is nothing to be unsure of).
  # Uniform presence lets the X-ray shade doubtful words without caring how the
  # page was read.
  for (p in seq_len(np)) {
    if (!is.null(words[[p]]) && is.null(words[[p]]$ocr_conf))
      words[[p]]$ocr_conf <- rep(NA_real_, nrow(words[[p]]))
  }

  list(
    pages = pages,
    words = words,
    page_count = np,
    page_width = page_width,
    page_height = page_height,
    sections = detect_pdf_sections(pages, anchors),
    redactions = data.frame(page = seq_len(np), redacted_words = red_counts,
                            scan_incomplete = red_scan_incomplete,
                            stringsAsFactors = FALSE),
    redaction_scan_incomplete = sum(red_scan_incomplete),
    ocr = ocr_flags,
    ocr_conf = ocr_conf,
    ok = TRUE
  )
}

# .rects_for_page(redaction_rects, p) -- fetch the rectangle data.frame for page
# `p` from a named-by-page list, or NULL.
.rects_for_page <- function(redaction_rects, p) {
  if (is.null(redaction_rects)) return(NULL)
  if (is.data.frame(redaction_rects)) {
    # a single flat data.frame with a `page` column
    if ("page" %in% names(redaction_rects)) {
      sub <- redaction_rects[redaction_rects$page == p, , drop = FALSE]
      if (nrow(sub) == 0) return(NULL)
      return(sub[, c("x0", "y0", "x1", "y1"), drop = FALSE])
    }
    return(redaction_rects)
  }
  key <- as.character(p)
  if (!is.null(redaction_rects[[key]])) return(redaction_rects[[key]])
  if (length(redaction_rects) >= p && !is.null(redaction_rects[[p]]))
    return(redaction_rects[[p]])
  NULL
}
