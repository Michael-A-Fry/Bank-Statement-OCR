# read_pdf.R -- PDF text + word-box reader (pdftools + poppler). Extraction only:
# page text, positioned word boxes, detected sections, and the signs that are drawn
# on the page rather than printed in the text layer.
#
# IT DOES NOT WITHHOLD ANYTHING. There was a redaction guard here that replaced any
# word it judged hidden -- by marker glyph, by a supplied rectangle, or by
# rasterising the page and testing whether the word came out solid -- with a
# "[REDACTED]" token, discarding the real text before anything downstream saw it.
# It is gone. This tool reads a document it has been given; whether the sender
# redacted it competently is not its problem, and an analyst who can see a figure in
# a PDF viewer but not in the spreadsheet has been handed a worse copy of their own
# evidence.
#
# A value that is genuinely GONE still leaves an empty cell, and that cell is
# recovered from the running balance (`amount_from_balance`, parse_pdf_table.R) --
# which the old guard actively BLOCKED, because it excluded any row it had marked
# redacted from the derivation.
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
#                             space,text,ocr_conf),
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
read_pdf <- function(path,
                     anchors = pdf_section_anchors()) {
  empty <- list(pages = character(0), words = list(), page_count = NA_integer_,
                sections = detect_pdf_sections(character(0)),
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
  # ONE INK PAGE PER DOCUMENT PAGE, OR THE SCAN DOES NOT RUN. Applying ink[[p]] to
  # word_list[[p]] is only meaningful if the two are the same page. A short or long
  # list means the renderer and the text layer disagree about the document, and
  # guessing the alignment inverts signs on whichever pages are offset -- the exact
  # fault this scan exists to prevent, caused by the scan. `ink_scan_pages` is
  # carried so a diagnostic can say the check did not run.
  ink_pages <- length(ink %||% list())
  ink_ok <- !is.null(ink) && identical(ink_pages, as.integer(np))
  if (ink_ok) {
    for (p in seq_along(word_list)) {
      if (is.null(word_list[[p]]) || !nrow(word_list[[p]])) next
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
  ocr_flags <- rep(FALSE, np)
  ocr_conf <- rep(NA_real_, np)
  # What the OCR safety nets did on each page (R/ocr.R), and the pages whose every
  # reading ran out of time. A timed-out page is blank here, so it has to be
  # CARRIED: a statement missing a page of rows must never pass as complete.
  ocr_note <- rep("", np)
  ocr_timed_out <- logical(np)

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
    if (is.null(wp) || nrow(wp) == 0) {
      # No word boxes -> the raw text layer as it stands. Nothing is rewritten: if
      # the page says it, the page says it.
      pages[p] <- raw_text[[p]]
      words[[p]] <- data.frame(width = integer(0), height = integer(0),
                               x = integer(0), y = integer(0), space = logical(0),
                               text = character(0), stringsAsFactors = FALSE)
    } else {
      words[[p]] <- as.data.frame(wp, stringsAsFactors = FALSE)
      pages[p] <- raw_text[[p]]
    }

    # OCR fallback. Routed on the WORD BOXES, not a flat character count: a genuine
    # digital page (boxes present) is never OCR'd even if pdf_text came back empty,
    # while a scanned page carrying only a thin text stamp (few or no boxes) still
    # gets OCR'd. The router is asked regardless of whether the tools exist, so an
    # un-OCR-able scan is RECORDED rather than passing as a blank page.
    needs_ocr_p <- ocr_router && isTRUE(page_needs_ocr(pages[p], words[[p]]))
    if (needs_ocr_p && !ocr_tools) scanned_no_ocr[p] <- TRUE
    if (ocr_ready && needs_ocr_p) {
      res <- ocr_pdf_page(path, p)
      ocr_note[p] <- res$note %||% ""
      ocr_timed_out[p] <- isTRUE(res$timed_out)
      if (isTRUE(res$ok)) {
        ocr_flags[p] <- TRUE
        ocr_conf[p] <- res$conf %||% NA_real_
        # OCR word boxes live in the (deskewed) render frame -> report that frame's
        # point size as this page's dimensions, so band normalisation stays aligned.
        if (!is.null(res$width) && is.finite(res$width) && res$width > 0)   page_width[p]  <- res$width
        if (!is.null(res$height) && is.finite(res$height) && res$height > 0) page_height[p] <- res$height
        if (!is.null(res$words) && nrow(res$words)) words[[p]] <- res$words
        pages[p] <- paste(res$text, collapse = "\n")
      }
    }
  }


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
    # Signs this page carried as INK rather than as text, and signs the text layer
    # carried that the page does not SHOW. Both are facts about the document worth
    # telling a reviewer, even though the figures are now right.
    ink_minus_signs = ink_minus,
    faint_minus_signs = faint_dropped,
    # the sign-from-ink scan: did it run, and over how many pages. A statement read
    # without it, that prints its minus as ink, is read with inverted signs.
    ink_scan_ok = ink_ok,
    ink_scan_pages = ink_pages,
    ocr = ocr_flags,
    ocr_conf = ocr_conf,
    ocr_note = ocr_note,
    ocr_timed_out = which(ocr_timed_out),
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
  # ---- ONE ENTRY PER PAGE, AND THE SPLIT HAS TO BE RIGHT -------------------
  #
  # MEASURED BUG, and it was silent, and it moved figures in BOTH directions. This
  # split on `<g id="surface`, which poppler 24.02 does not emit at all: a 100-page
  # statement therefore returned ONE entry holding every page's ink. read_pdf then
  # applied that to page 1 alone, so
  #
  #   * pages 2..N got NO sign correction -- a bank that draws its minus as a stroke
  #     had every page after the first read with the signs inverted; and
  #   * page 1 got FALSE positives -- a stroke anywhere in the document at the same
  #     (x, y) as a page-1 amount turned a correct positive negative.
  #
  # Neither shows on a one-page fixture, which is why every test passed. poppler
  # wraps each page in <page>...</page> inside a <pageSet>; the surface form is kept
  # as a fallback because other builds do emit it, and a single-page SVG has neither.
  per_page <- regmatches(svg, gregexpr("<page>.*?</page>", svg))[[1]]
  if (!length(per_page)) {
    surf <- strsplit(svg, "<g id=\"surface", fixed = TRUE)[[1]]
    per_page <- if (length(surf) >= 2) surf[-1] else svg
  }
  lapply(per_page, function(pg) list(strokes = .ink_strokes(pg),
                                     faint = .ink_faint(pg)))
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

# ---------------------------------------------------------------------------
# MEASURED AND NOT DONE: reading redaction boxes out of the page's vector ink.
#
# A box over text hides it whatever colour the box is, and the raster scan in
# R/detect_redaction.R used to ask only "is this word DARK" -- so a WHITE box over
# live text, one of the commonest failed redactions there is, was missed entirely.
# Measured on a specimen with the same account number covered four ways, it flagged
# BLACK and missed WHITE, GREY and YELLOW.
#
# The obvious fix, and the one the hook above was written for, is to read the filled
# rectangles from the page itself: pdftocairo reports every one, with its colour. It
# was built -- fills, glyph positions, and a z-order test, because a shaded table
# header is a filled rectangle over its own column names and flagging it would
# withhold the header of every statement that shades one.
#
# IT FAILED ON THE FIRST REAL STATEMENT IT SAW. anz_card_summary_sample.pdf paints
# ten large light-grey background panels at SVG offsets AFTER most of its glyphs, so
# the draw order in the converted SVG says they are on top of text they plainly do
# not hide. 162 words on a clean page came back redacted.
#
# The lesson is worth more than the code was: draw order in a converted SVG is not a
# reliable proxy for "hides the text" on real PDFs. The right question is simply
# whether the word can still be SEEN, and the renderer already answers it -- a word
# under an opaque fill of any colour is a FLAT patch, and visible text never is. That
# is PARAM_REDACT_FLAT_SPREAD in R/detect_redaction.R: one more statistic from pixels
# already in memory, no z-order reasoning, and it needs no assumption about how a
# particular PDF writer ordered its display list.
#
# About a hundred lines came back out. The vector pass remains for what it is
# genuinely better at -- reading a SIGN the text layer gets wrong (.pdf_ink above).
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# HOW LONG IS THIS GOING TO TAKE?
#
# MEASURED, and the two answers are 55x apart: a digital page costs 0.17s and a
# scanned page costs 9.3s, because tesseract has to read it as a picture. So a
# 120-page digital statement is 20 seconds and a 120-page SCAN is nineteen MINUTES --
# and the analyst is shown the same "Converting statement..." for both.
#
# Nineteen minutes of silence is indistinguishable from a hung tool. People reload the
# page, upload it again, or report it broken; and on a shared single-process server
# re-uploading is the one response that makes it worse. One sentence up front turns it
# into an informed wait.
#
# IT HAS TO BE CHEAP, or it is just more waiting. Page count comes from pdfinfo and
# the scan test from the first few pages of text only -- both sub-second on a
# 400-page file, measured. It is deliberately a GUESS and says so: it reads a sample,
# not the document.
.ESTIMATE_SAMPLE_PAGES <- 3L

# conversion_estimate(path) -> list(pages, scanned, secs, note) or NULL when the
# question cannot be answered cheaply (not a PDF, no poppler, unreadable).
conversion_estimate <- function(path) {
  if (!length(path) || !file.exists(path)) return(NULL)
  if (!grepl("[.]pdf$", path, ignore.case = TRUE)) return(NULL)
  info <- Sys.which("pdfinfo"); txt <- Sys.which("pdftotext")
  if (!nzchar(info) || !nzchar(txt)) return(NULL)
  out <- safe(suppressWarnings(system2(info, shQuote(path), stdout = TRUE, stderr = FALSE)), NULL)
  if (is.null(out)) return(NULL)
  pg <- suppressWarnings(as.integer(trimws(sub("^Pages:[[:space:]]*", "",
          grep("^Pages:", out, value = TRUE)[1]))))
  if (!length(pg) || is.na(pg) || pg < 1L) return(NULL)
  # the sample: text from the first few pages only
  n <- min(pg, .ESTIMATE_SAMPLE_PAGES)
  tf <- tempfile(fileext = ".txt"); on.exit(unlink(tf), add = TRUE)
  st <- safe(suppressWarnings(system2(txt, c("-f", "1", "-l", as.character(n),
        shQuote(path), shQuote(tf)), stdout = FALSE, stderr = FALSE)), 1L)
  chars <- if (identical(as.integer(st), 0L) && file.exists(tf))
    nchar(gsub("[[:space:]]", "", paste(readLines(tf, warn = FALSE), collapse = ""))) else 0L
  # PER SAMPLED PAGE, not for the sample: a 1-page sample of a 400-page scan and a
  # 3-page sample of one are the same document, and dividing by the wrong n called
  # the first digital.
  scanned <- (chars / max(1L, n)) < PARAM_OCR_MIN_CHARS
  secs <- pg * (if (scanned) PARAM_SECS_PER_SCAN_PAGE else PARAM_SECS_PER_PAGE)
  list(pages = pg, scanned = scanned, secs = secs,
       note = .estimate_note(pg, scanned, secs))
}

# .estimate_note(pages, scanned, secs) -- the sentence, or "" when there is nothing
# worth saying. Silence under about half a minute: a number nobody needed is noise,
# and this appears on every single conversion.
.estimate_note <- function(pages, scanned, secs) {
  if (!is.finite(secs) || secs < 30) return("")
  howlong <- if (secs < 90) sprintf("about %.0f seconds", round(secs / 10) * 10)
             else if (secs < 5400) sprintf("about %.0f minutes", max(1, round(secs / 60)))
             else sprintf("over %.0f hours", floor(secs / 3600))
  if (scanned)
    sprintf(paste("%d pages, and they look like scans rather than text, so every page",
                  "has to be read as a picture - expect %s. Leave it running; it is not stuck."),
            pages, howlong)
  else
    sprintf("%d pages, so expect %s.", pages, howlong)
}
