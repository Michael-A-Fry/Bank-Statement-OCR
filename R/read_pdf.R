# read_pdf.R -- PDF text and word-box reader (pdftools) with a forensic redaction guard.
# FORENSIC RULE: text hidden under a redaction overlay must NEVER be emitted. Any word covered by a
# redaction - a marker in the text layer, or a rectangle overlay - is replaced by REDACTION_TOKEN and
# its underlying text discarded before anything leaves this module. Over-redaction is the safe mode.

# Reuse the canonical token from parse.R when co-sourced; fall back so this file stands alone.
if (!exists("REDACTION_TOKEN")) REDACTION_TOKEN <- "[REDACTED]"

# Redaction markers already baked into a source PDF's extractable text. The block glyphs are
# written as \u escapes: this engine runs in a C locale where multibyte regex literals are
# unreliable, and an escape means the same thing everywhere. Matched with useBytes.
.PDF_BLOCK_GLYPHS <- c("\u2588", "\u2593", "\u2592", "\u2591", "\u25a0",
                       "\u25ac", "\u25ae", "\u2580", "\u2584", "\u2588")

pdf_redaction_markers <- function() {
  # From the lexicon, so a new redaction convention is one edit rather than a code change.
  glyphs <- unique(lex("redaction_block_glyphs"))
  block_run <- paste0("(?:", paste(glyphs, collapse = "|"), "){1,}")
  c(lex("redaction_markers"), block_run)
}

# Matched at the byte level so block glyphs match reliably under a C locale.
.matches_marker <- function(text, markers) {
  text <- as.character(text)
  hit <- rep(FALSE, length(text))
  for (m in markers) {
    hit <- hit | grepl(m, text, perl = TRUE, useBytes = TRUE)
  }
  hit & !is.na(text)
}

# Flags every word box overlapping a supplied redaction rectangle. `rects` is a data.frame with
# x0, y0, x1, y1 (top-left origin, matching pdftools word coordinates) or NULL - the structure an
# automatic overlay detector populates.
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
    # Axis-aligned overlap; any overlap means covered, conservative on purpose.
    overlap <- (wx0 < rx1) & (wx1 > rx0) & (wy0 < ry1) & (wy1 > ry0)
    covered <- covered | overlap
  }
  covered
}

# The redaction guard: returns words with a logical `redacted` column and every redacted word's
# text overwritten with REDACTION_TOKEN. The choke point every emitted PDF word passes through.
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

# Reconstruct page text from already-guarded word boxes. Used whenever a page carried any
# redaction, because pdf_text reads the raw layer and would leak text under an overlay.
words_to_text <- function(words, line_tol = PARAM_PDF_ROW_TOL) {
  if (nrow(words) == 0) return("")
  o <- order(words$y, words$x)
  w <- words[o, , drop = FALSE]
  line_key <- cumsum(c(TRUE, diff(w$y) > line_tol))
  lines <- vapply(split(w$text, line_key), function(tok)
    paste(tok, collapse = " "), character(1))
  paste(lines, collapse = "\n")
}

# Scan each page's lines for anchor phrases -> data.frame(section, page, line_no, matched_text).
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
      # Anchor phrase anywhere in the line, matched literally, case-insensitively.
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

# read_pdf(path, ...) -> list(pages, words, page_count, sections, redactions, ok). `words` is one
# guarded data.frame per page (x, y, width, height, space, text, redacted, ocr_conf).
read_pdf <- function(path, redaction_rects = NULL,
                     markers = pdf_redaction_markers(),
                     anchors = pdf_section_anchors(),
                     scan_vector = TRUE, vector_dpi = PARAM_REDACT_VECTOR_DPI) {
  empty <- list(pages = character(0), words = list(), page_count = NA_integer_,
                sections = detect_pdf_sections(character(0)),
                redactions = data.frame(page = integer(0), redacted_words = integer(0),
                                        stringsAsFactors = FALSE),
                ocr = logical(0),
                ocr_conf = numeric(0),
                doc_info = .pdf_doc_info(NULL),
                ok = FALSE)
  if (!requireNamespace("pdftools", quietly = TRUE)) return(empty)
  if (!file.exists(path)) return(empty)

  raw_text  <- safe(suppressMessages(pdftools::pdf_text(path)), NULL)
  word_list <- safe(suppressMessages(pdftools::pdf_data(path)), NULL)
  if (is.null(raw_text)) return(empty)

  np <- length(raw_text)
  # Per-page point dimensions: a template's x-bands are drawn in ONE page's point space and the
  # parser normalises each page's words into it.
  doc_info <- .pdf_doc_info(safe(suppressMessages(pdftools::pdf_info(path)), NULL))
  psize <- safe(suppressMessages(pdftools::pdf_pagesize(path)), NULL)
  page_width  <- if (!is.null(psize) && "width"  %in% names(psize)) as.numeric(psize$width)  else rep(NA_real_, np)
  page_height <- if (!is.null(psize) && "height" %in% names(psize)) as.numeric(psize$height) else rep(NA_real_, np)
  length(page_width)  <- np
  length(page_height) <- np
  # pdf_data may return fewer or NULL entries on odd pages; normalise to np slots.
  if (is.null(word_list)) word_list <- vector("list", np)
  # A rotated page faces the other way from its page box: pdf_pagesize reports the box BEFORE
  # /Rotate while the text extractor and renderer report after it, so the band frame squashes every
  # word. The WORDS are the authority, because they are what gets read.
  for (p in seq_len(np)) {
    sp <- .pdf_page_space(page_width[p], page_height[p], word_list[[p]])
    page_width[p] <- sp[1]; page_height[p] <- sp[2]
  }

  pages <- character(np)
  words <- vector("list", np)
  red_counts <- integer(np)
  ocr_flags <- rep(FALSE, np)
  ocr_conf <- rep(NA_real_, np)
  # Pages whose vector-redaction scan could NOT run are surfaced as a loud "redactions not
  # verified" warning downstream, never a silent clean pass.
  red_scan_incomplete <- rep(FALSE, np)

  # OCR is attempted whenever the page's TEXT is effectively empty or sparse, not only at zero word
  # boxes: a scanned page carrying a thin digital layer (a Bates stamp, a footer) would otherwise be
  # treated as a text page and silently yield no rows. Whether the router exists and whether the tools
  # are installed are kept apart, so a failed OCR install is reported as exactly that.
  ocr_router <- exists("page_needs_ocr", mode = "function")
  ocr_tools  <- exists("ocr_available", mode = "function") && ocr_available()
  ocr_ready  <- ocr_tools && ocr_router
  scanned_no_ocr <- logical(np)

  for (p in seq_len(np)) {
    wp <- word_list[[p]]
    rects_p <- .rects_for_page(redaction_rects, p)
    if (is.null(wp) || nrow(wp) == 0) {
      # No word boxes: emit raw text as-is, with a marker sweep so baked-in redaction tokens never
      # survive even without geometry.
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
      # If ANY redaction touched this page, do NOT trust the raw text layer - it exposes text under
      # overlays - and rebuild the page from guarded boxes.
      pages[p] <- if (n_red > 0) words_to_text(guarded) else raw_text[[p]]
    }

    # OCR fallback. Tesseract reads only VISIBLE pixels, so a painted redaction is unreadable, and the
    # OCR word boxes go through the SAME guard. The DIGITAL word boxes are passed so the decision
    # routes on their presence - a genuine digital page is never OCR'd even if pdf_text came back
    # empty - and the router is asked regardless of tooling, so an un-OCR-able scan is RECORDED.
    needs_ocr_p <- ocr_router && isTRUE(page_needs_ocr(pages[p], words[[p]]))
    if (needs_ocr_p && !ocr_tools) scanned_no_ocr[p] <- TRUE
    if (ocr_ready && needs_ocr_p) {
      res <- ocr_pdf_page(path, p)
      if (isTRUE(res$ok)) {
        otxt <- paste(res$text, collapse = "\n")
        for (m in markers) otxt <- gsub(m, REDACTION_TOKEN, otxt,
                                        perl = TRUE, useBytes = TRUE)
        ocr_flags[p] <- TRUE
        ocr_conf[p] <- res$conf %||% NA_real_
        # OCR word boxes live in the deskewed render frame, so report that frame's point size as
        # the page's dimensions and band normalisation stays aligned.
        if (!is.null(res$width) && is.finite(res$width) && res$width > 0)   page_width[p]  <- res$width
        if (!is.null(res$height) && is.finite(res$height) && res$height > 0) page_height[p] <- res$height
        if (!is.null(res$words) && nrow(res$words)) {
          # Auto-detected rasterised redactions join any caller-supplied rects; a VISIBLE row a box
          # covers has its blacked cell marked [REDACTED] so the partial row keeps its visible data,
          # flagged, never dropped. Fully hidden rows have no visible anchor and do not appear.
          auto_rects <- res$dark_rects
          all_rects <- if (!is.null(auto_rects) && nrow(auto_rects)) {
            base_rects <- if (is.null(rects_p)) NULL else rects_p[, c("x0","y0","x1","y1"), drop = FALSE]
            rbind(base_rects, auto_rects)
          } else rects_p
          guarded_ocr <- apply_redaction_guard(res$words, all_rects, markers)
          if (!is.null(auto_rects) && nrow(auto_rects) &&
              exists("inject_redaction_tokens", mode = "function"))
            guarded_ocr <- inject_redaction_tokens(guarded_ocr, auto_rects,
                                                   row_tol = PARAM_PDF_ROW_TOL)
          words[[p]] <- guarded_ocr
          nred_ocr <- sum(guarded_ocr$redacted)
          red_counts[p] <- nred_ocr
          # Keep pages[p] consistent with the guarded OCR boxes, so an overlay redaction reaches
          # the metadata and section text too.
          pages[p] <- if (nred_ocr > 0) words_to_text(guarded_ocr) else otxt
        } else {
          pages[p] <- otxt
        }
      }
    }

    # Digital vector-redaction guard: a page with a full text layer never triggers OCR, so a solid
    # rectangle DRAWN over still-present text would leak it. Rasterise and mark any word whose
    # rendered box is nearly solid dark, the same visibility test the scanned path uses.
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
  # LOUD fallback: the visible text on pages that could not be rasterised is NOT
  # redaction-verified, so say so once.
  if (any(red_scan_incomplete))
    warning(sprintf(paste0("read_pdf: could not rasterise %d page(s) to verify ",
      "vector redactions; visible text on those pages is not redaction-checked"),
      sum(red_scan_incomplete)), call. = FALSE)

  # Words-frame contract: every page's words carry a per-word `ocr_conf` - Tesseract's 0-100 on an
  # OCR page, NA on a text-layer page. Uniform presence lets the X-ray shade doubtful words.
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
    # Pages that ARE scans but could not be machine-read because the OCR tools are not installed.
    # Non-zero means the statement was read blind, and diagnose turns it into a specific message.
    scanned_no_ocr = sum(scanned_no_ocr),
    ocr_tools_available = ocr_tools,
    # Self-declared document provenance. Reported, never interpreted.
    doc_info = doc_info,
    ok = TRUE
  )
}

# Document-level PDF metadata (forensic provenance). FACTS ONLY - every field is SELF-DECLARED,
# recorded verbatim and never interpreted. The Title key is deliberately NOT captured: it routinely
# carries the customer's name and this structure travels into the diagnostics table.
# .pdf_page_space(w, h, words) -> the space this page's WORDS are measured in, which is not always the
# space its page box describes. THE WORDS DECIDE, and the page is turned only when they do not fit
# the box AND do fit it on its side.
.pdf_page_space <- function(w, h, words) {
  w <- suppressWarnings(as.numeric(w)[1]); h <- suppressWarnings(as.numeric(h)[1])
  if (!isTRUE(is.finite(w)) || !isTRUE(is.finite(h)) || w <= 0 || h <= 0) return(c(w, h))
  if (is.null(words) || !NROW(words)) return(c(w, h))
  ex <- suppressWarnings(max(words$x + words$width))
  ey <- suppressWarnings(max(words$y + words$height))
  if (!isTRUE(is.finite(ex)) || !isTRUE(is.finite(ey))) return(c(w, h))
  fits_now <- ex <= w + 2 && ey <= h + 2
  fits_turned <- ex <= h + 2 && ey <= w + 2
  if (!fits_now && fits_turned) c(h, w) else c(w, h)
}

.pdf_doc_info <- function(info) {
  s1 <- function(v) {
    v <- as.character(v)
    if (!length(v) || is.na(v[1]) || !nzchar(trimws(v[1]))) NA_character_ else trimws(v[1])
  }
  # Timestamps as ISO strings, not POSIXct: this list is written to CSV/JSON and a bare POSIXct
  # would render in whatever timezone the reader happens to be in.
  ts <- function(v) if (is.null(v) || !length(v) || is.na(v[1])) NA_character_
                    else format(v[1], "%Y-%m-%d %H:%M:%S")
  out <- list(producer = NA_character_, creator = NA_character_,
              created = NA_character_, modified = NA_character_,
              encrypted = NA, pdf_version = NA_character_)
  if (is.null(info) || !is.list(info)) return(out)
  k <- info$keys
  if (!is.list(k)) k <- list()
  out$producer  <- s1(k$Producer)
  out$creator   <- s1(k$Creator)
  out$created   <- ts(info$created)
  out$modified  <- ts(info$modified)
  out$encrypted <- if (is.null(info$encrypted) || !length(info$encrypted)) NA else isTRUE(info$encrypted)
  out$pdf_version <- s1(info$version)
  out
}

# Fetch the rectangle data.frame for page `p` from a named-by-page list, or NULL.
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
