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
#
# WRITTEN AS \u ESCAPES, not as the glyphs themselves. The comment above already
# said the C locale cannot be trusted with multibyte literals, and then spelt
# these ten out in raw bytes anyway -- bytes that also have to survive every
# editor, mail client and zip between here and an air-gapped Windows box. An
# escape is seven ASCII characters meaning the same thing everywhere, and it
# produces byte-identical strings. They are consumed as an ALTERNATION and
# matched with useBytes (see pdf_redaction_markers / .matches_marker below),
# which is what keeps them working whatever the locale.
.PDF_BLOCK_GLYPHS <- c("\u2588", "\u2593", "\u2592", "\u2591", "\u25a0",
                       "\u25ac", "\u25ae", "\u2580", "\u2584", "\u2588")

pdf_redaction_markers <- function() {
  # marker regexes + the block/shade glyphs come from the lexicon (admin/ML
  # extendable), so a new redaction convention is one edit, not a code change.
  glyphs <- unique(lex("redaction_block_glyphs"))
  block_run <- paste0("(?:", paste(glyphs, collapse = "|"), "){1,}")
  c(lex("redaction_markers"), block_run)
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
words_to_text <- function(words, line_tol = PARAM_PDF_ROW_TOL) {
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
  # Per-page point dimensions. A template's x-bands are drawn in ONE page's point
  # space; the parser normalises each page's words into that space (see
  # parse_pdf_table), which needs the page size. Same for OCR pages -- pdf_pagesize
  # reports the page's own point size, which is what the OCR word coordinates use.
  # Document provenance: who wrote this file and when (see .pdf_doc_info). Cheap
  # (one header read), recorded for EVERY PDF, and never allowed to break a
  # conversion -- an unreadable info dictionary just leaves the fields NA.
  doc_info <- .pdf_doc_info(safe(suppressMessages(pdftools::pdf_info(path)), NULL))
  psize <- safe(suppressMessages(pdftools::pdf_pagesize(path)), NULL)
  page_width  <- if (!is.null(psize) && "width"  %in% names(psize)) as.numeric(psize$width)  else rep(NA_real_, np)
  page_height <- if (!is.null(psize) && "height" %in% names(psize)) as.numeric(psize$height) else rep(NA_real_, np)
  length(page_width)  <- np
  length(page_height) <- np
  # pdf_data may return fewer/NULL entries on odd pages; normalise to np slots.
  if (is.null(word_list)) word_list <- vector("list", np)
  # RECONCILE THE TEXT LAYER WITH THE INK ACTUALLY ON THE PAGE. Two constructions
  # real statements use make the text layer wrong about a sign in opposite
  # directions -- see .apply_ink_signs. Both are silent wrong figures, and neither
  # is caught by arithmetic on a statement with no running balance. The counts are
  # carried so a diagnostic can say the page uses them.
  ink_minus <- 0L; faint_dropped <- 0L
  ink <- .pdf_ink(path)
  if (!is.null(ink) && length(ink) >= 1L) {
    for (p in seq_along(word_list)) {
      if (p > length(ink) || is.null(word_list[[p]]) || !nrow(word_list[[p]])) next
      r <- safe(.apply_ink_signs(word_list[[p]], ink[[p]]), NULL)
      if (is.null(r)) next
      word_list[[p]] <- r$words
      ink_minus <- ink_minus + r$ink_minus
      faint_dropped <- faint_dropped + r$faint_dropped
    }
  }
  # A ROTATED PAGE FACES THE OTHER WAY FROM ITS PAGE BOX.
  #
  # pdf_pagesize reports the box BEFORE the page's /Rotate is applied; the text
  # extractor and the renderer both report AFTER it. On such a page the two
  # disagree by ninety degrees, and everything downstream that trusts the box is
  # then measuring in a space at right angles to the one the words are in: the
  # picture in the builder is drawn 612 wide while its own image is 792 wide, so
  # nothing anybody draws on it lines up with anything, and the band frame
  # squashes every word on the page.
  #
  # The WORDS are the authority, because they are what gets read. If they do not
  # fit the box but do fit it turned on its side, the page is turned on its side.
  # (Measured on a corpus of other people's PDFs: 11 pages of 212, across 5 of
  # 81 documents -- landscape scans, a US Senate expenditure report, a bid award.
  # On every one of them the builder was unusable.)
  for (p in seq_len(np)) {
    sp <- .pdf_page_space(page_width[p], page_height[p], word_list[[p]])
    page_width[p] <- sp[1]; page_height[p] <- sp[2]
  }

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
  # Split the two halves of "can we OCR?": whether the ROUTER exists, and whether the
  # TOOLS are installed. Keeping them apart lets us still ASK "did this page need
  # OCR?" on a machine with no tesseract/poppler -- so a scan on a box where the
  # bundle's best-effort OCR install failed is reported as exactly that, instead of
  # silently reading as a blank page and being blamed on the layout. See scanned_no_ocr.
  ocr_router <- exists("page_needs_ocr", mode = "function")
  ocr_tools  <- exists("ocr_available", mode = "function") && ocr_available()
  ocr_ready  <- ocr_tools && ocr_router
  scanned_no_ocr <- logical(np)

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
    # Pass the DIGITAL word boxes so the decision routes on their presence, not a
    # flat char count: a genuine digital page (word boxes present) is never OCR'd
    # even if pdf_text came back empty, while a scanned page carrying only a thin
    # text stamp (few/no word boxes) still gets OCR'd.
    # Ask the router regardless of tooling, so an un-OCR-able scan is RECORDED
    # rather than passing as an empty page.
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
                                                   row_tol = PARAM_PDF_ROW_TOL)
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
    # Signs this page carried as INK rather than as text, and signs the text layer
    # carried that the page does not SHOW. Both are facts about the document worth
    # telling a reviewer, even though the figures are now right.
    ink_minus_signs = ink_minus,
    faint_minus_signs = faint_dropped,
    ocr = ocr_flags,
    ocr_conf = ocr_conf,
    # Pages that ARE scans but could not be machine-read because the OCR tools are
    # not installed on this machine. Non-zero means the statement was read blind:
    # diagnose turns it into a loud, specific message so the cause is never mistaken
    # for "this layout isn't supported".
    scanned_no_ocr = sum(scanned_no_ocr),
    ocr_tools_available = ocr_tools,
    # Self-declared document provenance (producer / creator / timestamps /
    # encryption). Reported, never interpreted -- see .pdf_doc_info.
    doc_info = doc_info,
    ok = TRUE
  )
}

# ---------------------------------------------------------------------------
# Document-level PDF metadata (forensic provenance).
#
# "Was this statement produced by the bank, or re-saved by someone with a PDF
# editor?" is a first-order forensic question, and pdftools -- already a
# dependency -- answers it for free: pdf_info() returns the Producer / Creator
# strings the writing tool stamped in, the creation and modification timestamps,
# and whether the file is encrypted.
#
# FACTS ONLY. Every one of these fields is SELF-DECLARED by whatever wrote the
# file: they can be edited, stripped, or simply reflect an ordinary re-save. So we
# record them verbatim and never interpret them -- they change no figure, no
# status and no trust score. The Title key is deliberately NOT captured: it
# routinely carries the customer's name, and this structure travels into the
# diagnostics table.
# .pdf_doc_info(info) -> a fixed-shape list, all-NA when pdf_info was unavailable,
# so downstream code never has to test whether the call worked.
# .pdf_page_space(w, h, words) -> c(width, height): the space this page's WORDS
# are measured in, which is not always the space its page box describes.
#
# pdf_pagesize reports the box BEFORE the page's /Rotate is applied; the text
# extractor and the renderer both report AFTER it. On a rotated page the two
# disagree by ninety degrees, and everything downstream that trusts the box is
# then measuring at right angles to the words: the builder draws a picture 612
# points wide whose own image is 792 wide, so nothing anybody draws on it lines
# up with anything, and the band frame squashes every word on the page.
#
# THE WORDS DECIDE, because they are what gets read. Turned only when they do
# not fit the box AND do fit it on its side -- never on a guess. A page whose
# words fit neither way is left exactly as it was: something else is wrong with
# it, and turning it would only be a second wrong thing.
#
# (Measured on 81 documents from other projects' test suites: 11 pages of 212,
# across 5 documents -- a landscape scan, a US Senate expenditure report, a
# five-page bid award. The builder was unusable on every one of them.)
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
  # Timestamps as ISO strings, not POSIXct: this list is written to CSV/JSON and
  # shown in a table, and a bare POSIXct would render in whatever timezone the
  # reader happens to be in.
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

# ---------------------------------------------------------------------------
# MEASURED AND NOT DONE: float word boxes.
#
# pdftools::pdf_data() floors x, y, width and height to whole points. Against the
# true boxes, over 9,260 words of the synthetic corpus:
#
#   word x        bias -0.33 pt, range [-1.00, 0]
#   word CENTRE   bias -0.49 pt, range [-1.31, 0]   55% of words off by >0.5 pt
#   word RIGHT    bias -0.64 pt, range [-1.70, 0]   20% of words off by >1.0 pt
#
# A word joins the column band containing its CENTRE, so that is a systematic
# half-point push towards the band on the left, and it looked like a real accuracy
# limit worth fixing. `pdftotext -bbox-layout` reports the same boxes as floats and
# needs no new dependency -- it is the poppler binary the OCR path already needs,
# and its XML parses with base regex.
#
# IT WAS BUILT, WIRED IN AND SCORED AGAINST THE CORPUS: 998 of 1,034 rows correct
# with integer boxes, 998 of 1,034 with floats. Not one row changed, on any of 33
# cases, including three that nudge a column band by 1, 3 and 10 points.
#
# WHY, and it is the useful part: a band boundary lives in the GUTTER between two
# columns, and the gutter on a real statement is several points wide. A systematic
# error of half a point -- worst case 1.3 -- cannot carry a word across it. The
# cases where it could are the ones where the amount sits within a point of the
# boundary, and there the layout is genuinely ambiguous: no reader working from
# column positions can know which column the bank meant.
#
# So 70 lines came back out. If a column-edge misread ever shows up on a real
# statement this is the first thing to try again, and it is in the history of this
# file -- but it is not carried as unmeasured machinery in the meantime.
#
# The right-edge error would matter if the reader ever used right-alignment to tell
# a debit column from a credit one. It does not today.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# WHEN THE TEXT LAYER AND THE PAGE DISAGREE ABOUT A SIGN.
#
# Two constructions real statements use, and the text layer is wrong about both.
# They are opposite faults and each is a silent wrong figure:
#
#   A MINUS DRAWN AS A LINE. The sign is a short stroke of vector ink, not a
#     glyph, so pdftotext reports "789.01" for a figure the page shows as -789.01.
#     Every withdrawal reads as a deposit.
#   A MINUS DRAWN IN THE BACKGROUND COLOUR. Some banks print the sign in white (or
#     near-white) on positive amounts so the column stays right-aligned. It is in
#     the text layer and invisible on the page, so "1,527.57" is reported as
#     "-1,527.57". Every deposit reads as a withdrawal.
#
# NEITHER IS CAUGHT BY ARITHMETIC when the statement has no running balance to
# reconcile against -- and anz_investmentfunds_pdf, a shipped template, is exactly
# that shape. Measured on the corpus (tools/synth/): 14 of 16 rows inverted by the
# drawn minus, 3 of 16 by the invisible one, every one of them at trust `low` with
# no arithmetic objection, because there was nothing to object with.
#
# BOTH ARE VISIBLE IN `pdftocairo -svg`, which is the same poppler bundle the OCR
# path already needs, so this costs no new dependency and no Python.
#
# THREE TRAPS, all measured, all handled below:
#   * `pdftocairo` SHRINKS the page to its default paper unless given -noshrink,
#     which silently scales every coordinate by ~0.9988.
#   * A STROKED path's `d` is in PDF user space (y up from the bottom) and carries
#     the page flip in a `transform` matrix. It must be applied.
#   * A GLYPH's `<use x y>` is already in the surface frame (y down from the top)
#     and carries NO transform. Applying the matrix to it would be wrong.
#
# A faint colour is one at or above .INK_FAINT on every channel: at 87.8% grey on
# white a glyph is not readable, which is the whole point of the trick. Anything
# darker is ink somebody meant to be seen and is left alone.
.INK_FAINT <- 0.85
# A sign stroke is SHORT and horizontal. A table rule is long; an underline is
# long. Lengths are in points and generous at both ends, because a minus is drawn
# at whatever width the font suggests.
.INK_MIN_LEN <- 1.5
.INK_MAX_LEN <- 9.0

# .pdf_ink(path) -> list, one entry per page, each list(strokes, faint); or NULL.
# NULL is a normal answer (no pdftocairo on this box, or output we could not read)
# and the caller simply does not get the extra evidence.
.pdf_ink <- function(path) {
  exe <- Sys.which("pdftocairo")
  if (!nzchar(exe) || !file.exists(path)) return(NULL)
  out <- tempfile(fileext = ".svg"); on.exit(unlink(out), add = TRUE)
  st <- suppressWarnings(safe(system2(exe, c("-svg", "-noshrink", shQuote(path), shQuote(out)),
                                      stdout = FALSE, stderr = FALSE), 1L))
  if (!identical(as.integer(st), 0L) || !file.exists(out)) return(NULL)
  svg <- safe(paste(readLines(out, warn = FALSE), collapse = "\n"), NULL)
  if (is.null(svg) || !nzchar(svg)) return(NULL)
  pages <- strsplit(svg, "<g id=\"surface", fixed = TRUE)[[1]]
  if (length(pages) < 2) pages <- c("", svg) else pages <- pages
  lapply(pages[-1], function(pg) list(strokes = .ink_strokes(pg), faint = .ink_faint(pg)))
}

# .ink_strokes(pg) -> data.frame(x0, x1, y, len, linewidth) for the HORIZONTAL
# strokes on one page, in word coordinates (y measured down from the page top).
.ink_strokes <- function(pg) {
  none <- data.frame(x0 = numeric(0), x1 = numeric(0), y = numeric(0),
                     len = numeric(0), linewidth = numeric(0))
  m <- regmatches(pg, gregexpr("<path fill=\"none\"[^/]*?/>", pg))[[1]]
  if (!length(m)) return(none)
  m <- m[grepl("d=\"M ", m, fixed = TRUE)]
  if (!length(m)) return(none)
  g1 <- function(x, rx) {
    v <- rep(NA_real_, length(x)); hit <- grepl(rx, x)
    v[hit] <- as.numeric(sub(rx, "\\1", x[hit], perl = TRUE)); v
  }
  lw <- g1(m, ".*stroke-width=\"([0-9.]+)\".*")
  # the first "M x y L x y" of the path; a sign is one segment, so this is enough
  seg <- ".*d=\"M ([0-9.eE+-]+) ([0-9.eE+-]+) L ([0-9.eE+-]+) ([0-9.eE+-]+).*"
  keep <- grepl(seg, m)
  if (!any(keep)) return(none)
  m <- m[keep]; lw <- lw[keep]
  px0 <- as.numeric(sub(seg, "\\1", m)); py0 <- as.numeric(sub(seg, "\\2", m))
  px1 <- as.numeric(sub(seg, "\\3", m)); py1 <- as.numeric(sub(seg, "\\4", m))
  # the page flip, from this path's own transform; absent means identity
  mx <- "matrix\\(\\s*([0-9.eE+-]+),\\s*([0-9.eE+-]+),\\s*([0-9.eE+-]+),\\s*([0-9.eE+-]+),\\s*([0-9.eE+-]+),\\s*([0-9.eE+-]+)\\s*\\)"
  tf <- regmatches(m, regexpr(mx, m))
  a <- rep(1, length(m)); b <- rep(0, length(m)); cc <- rep(0, length(m))
  dd <- rep(1, length(m)); e <- rep(0, length(m)); f <- rep(0, length(m))
  has <- vapply(tf, length, integer(1)) > 0L
  if (any(has)) {
    t1 <- unlist(tf[has])
    a[has]  <- as.numeric(sub(paste0(".*", mx, ".*"), "\\1", t1))
    b[has]  <- as.numeric(sub(paste0(".*", mx, ".*"), "\\2", t1))
    cc[has] <- as.numeric(sub(paste0(".*", mx, ".*"), "\\3", t1))
    dd[has] <- as.numeric(sub(paste0(".*", mx, ".*"), "\\4", t1))
    e[has]  <- as.numeric(sub(paste0(".*", mx, ".*"), "\\5", t1))
    f[has]  <- as.numeric(sub(paste0(".*", mx, ".*"), "\\6", t1))
  }
  sx <- function(x, y) a * x + cc * y + e
  sy <- function(x, y) b * x + dd * y + f
  X0 <- sx(px0, py0); Y0 <- sy(px0, py0)
  X1 <- sx(px1, py1); Y1 <- sy(px1, py1)
  horiz <- abs(Y1 - Y0) < 0.6 & is.finite(X0) & is.finite(X1) & is.finite(Y0)
  if (!any(horiz)) return(none)
  data.frame(x0 = pmin(X0, X1)[horiz], x1 = pmax(X0, X1)[horiz],
             y = ((Y0 + Y1) / 2)[horiz], len = abs(X1 - X0)[horiz],
             linewidth = lw[horiz])
}

# .ink_faint(pg) -> data.frame(x, y) for glyphs drawn in a near-background colour,
# in word coordinates. `<use x y>` is already in the surface frame, so it is NOT
# put through the stroke transform -- doing so was the trap worth writing down.
.ink_faint <- function(pg) {
  none <- data.frame(x = numeric(0), y = numeric(0))
  gm <- regmatches(pg, gregexpr("<g fill=\"rgb\\([^)]*\\)\"[^>]*>.*?</g>", pg))[[1]]
  if (!length(gm)) return(none)
  rx <- ".*rgb\\(\\s*([0-9.]+)%,\\s*([0-9.]+)%,\\s*([0-9.]+)%\\s*\\).*"
  r <- as.numeric(sub(rx, "\\1", gm)); g <- as.numeric(sub(rx, "\\2", gm))
  b <- as.numeric(sub(rx, "\\3", gm))
  faint <- is.finite(r) & is.finite(g) & is.finite(b) &
    pmin(r, g, b) >= .INK_FAINT * 100
  if (!any(faint)) return(none)
  out <- lapply(gm[faint], function(one) {
    u <- regmatches(one, gregexpr("<use[^/]*?x=\"[0-9.eE+-]+\"[^/]*?y=\"[0-9.eE+-]+\"[^/]*/>", one))[[1]]
    if (!length(u)) return(NULL)
    data.frame(x = as.numeric(sub(".*x=\"([0-9.eE+-]+)\".*", "\\1", u)),
               y = as.numeric(sub(".*y=\"([0-9.eE+-]+)\".*", "\\1", u)))
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(none)
  do.call(rbind, out)
}

# .money_like(t) -- does this token look like a bare money magnitude? Only such a
# token may acquire a sign from vector ink; a stroke near a DESCRIPTION word is a
# rule, a box edge or an underline, never a minus.
.money_like <- function(t) grepl("^[0-9][0-9,.]*$", trimws(t))

# .apply_ink_signs(w, ink) -> list(words, ink_minus, faint_dropped)
#
# Reconcile one page's words with the ink actually on it.
#
# A FAINT MINUS IS NOT ON THE PAGE, so the word is dropped. It is a separate word
# in the text layer ("-" beside "832.08"), and the column band pastes the two
# together, which is how an invisible sign became a real one. Dropping it leaves
# the band holding the magnitude alone, which is what a reader of the page sees.
#
# A STROKE JUST LEFT OF A MONEY TOKEN IS A MINUS, so the token gains one. The gap
# allowed is a fraction of the word's own HEIGHT rather than a fixed number of
# points, because a minus is set at whatever width the type size suggests -- the
# same reasoning poppler's own layout code uses for word spacing.
#
# NEITHER IS SILENT. The counts travel on the input's metadata so R/diagnose.R can
# say the page uses these constructions: a statement whose signs came from vector
# ink is a fact a reviewer should know, even when the figures are now right.
.apply_ink_signs <- function(w, ink) {
  out <- list(words = w, ink_minus = 0L, faint_dropped = 0L)
  if (is.null(w) || !nrow(w) || is.null(ink)) return(out)
  w <- as.data.frame(w, stringsAsFactors = FALSE)
  need <- c("text", "x", "y", "width", "height")
  if (!all(need %in% names(w))) return(out)
  h <- suppressWarnings(as.numeric(w$height)); h[!is.finite(h) | h <= 0] <- 8
  top <- suppressWarnings(as.numeric(w$y)); left <- suppressWarnings(as.numeric(w$x))

  # ---- the invisible minus -------------------------------------------------
  fa <- ink$faint
  drop <- rep(FALSE, nrow(w))
  if (!is.null(fa) && nrow(fa)) {
    # Any of the dash glyphs a PDF may use, via the one normaliser that knows them
    # (.ascii_dashes, R/normalise.R) rather than a second list here -- and never as
    # a multibyte character class, which throws under LC_ALL=C.
    is_dash <- grepl("^-+$", trimws(.ascii_dashes(w$text)))
    for (i in which(is_dash)) {
      if (any(abs(fa$x - left[i]) <= 3 &
              fa$y >= top[i] & fa$y <= top[i] + h[i] + 1)) drop[i] <- TRUE
    }
  }

  # ---- the minus drawn as a line -------------------------------------------
  st <- ink$strokes
  added <- 0L
  if (!is.null(st) && nrow(st)) {
    sgn <- st[st$len >= .INK_MIN_LEN & st$len <= .INK_MAX_LEN, , drop = FALSE]
    if (nrow(sgn)) for (i in seq_len(nrow(w))) {
      if (drop[i] || !.money_like(w$text[i])) next
      gap <- left[i] - sgn$x1
      if (any(gap >= -0.5 & gap <= 0.8 * h[i] &
              sgn$y >= top[i] & sgn$y <= top[i] + h[i] + 1)) {
        w$text[i] <- paste0("-", w$text[i]); added <- added + 1L
      }
    }
  }
  list(words = w[!drop, , drop = FALSE], ink_minus = added,
       faint_dropped = sum(drop))
}
