# params.R -- the engine's numeric TUNING decisions, in ONE visible place.
#
# These are neither VOCABULARY (that's the lexicon, admin-editable) nor DEPLOYMENT
# switches (that's config) -- they are algorithmic tuning a maintainer changes only
# with tests. They live here, named and commented, so every such decision is
# visible and consistent in one file instead of a bare literal repeated across the
# code. See docs/context/engine-parameters.md for the catalogue and rationale.
#
# It also hosts the two SHARED date helpers (.plausible_year, .tolerant_date) that
# thread the year window through every consumer -- colocated with the bound they
# apply so the parser and its threshold stay in one place.
#
# (Sourced before parse*.R so a top-level use resolves; function-default uses
# resolve at call time regardless of source order.)

# ---- dates -----------------------------------------------------------------
# A statement date's year must fall in this window to be TRUSTED; outside it is
# almost always a mis-parse (a 2-digit year read as 4-digit, OCR noise, a footer /
# copyright year). Real statements sit well inside. To support genuinely older
# archives, widen PARAM_YEAR_MIN here -- one edit, everywhere.
PARAM_YEAR_MIN <- 1990L
PARAM_YEAR_MAX <- 2100L

# ---- money -----------------------------------------------------------------
# Two money figures are "equal" within half a cent (never == on floats).
PARAM_MONEY_TOL <- 0.005


# ---- OCR routing (page_needs_ocr) ------------------------------------------
PARAM_OCR_MIN_CHARS     <- 20L    # fewer non-space chars than this -> treat as image
PARAM_OCR_MIN_WORDS     <- 3L     # fewer real word boxes than this -> scanned page
PARAM_OCR_MAX_BAD_RATIO <- 0.30   # more than this fraction of garbage chars -> OCR
PARAM_OCR_CELL_MIN_CONF <- 60     # per-cell OCR confidence floor (flag a cell below)
PARAM_OCR_PAGE_MIN_CONF <- 70     # page-mean OCR confidence below this -> loud caveat
PARAM_OCR_RENDER_DPI    <- 300L   # dpi a scanned page is rasterised at before OCR
                                  # (higher = sharper glyphs but slower; raise for poor scans)

# ---- PDF table geometry ----------------------------------------------------
# Words whose top edges sit within this many points of each other are treated as
# ONE visual row. The single most behaviour-affecting geometric knob in PDF
# parsing (too small splits a row, too large merges transactions). A template can
# override per-bank with table$row_tol; this is the engine default everywhere.
PARAM_PDF_ROW_TOL <- 3L

# ---- plausibility bounds ---------------------------------------------------
# A statement's stated transaction count above this is almost certainly a mis-read
# (a figure grabbed from the wrong line), so it's dropped rather than trusted -- the
# same "reject the implausible" idea as the year window. Raise for genuinely huge
# statements.
PARAM_STATED_COUNT_MAX <- 100000L

# ---- oversized-input advisories (diagnostics) ------------------------------
# Not hard limits -- the engine still tries. Above these it warns that a very
# large file may hit tool/render limits, so a stall has an explanation.
PARAM_MAX_PAGES   <- 100L         # above this a conversion is worth warning about
# Seconds per page, MEASURED at 1.10.0 on this build (tools/synth/bench.R and a
# rasterised 3-page specimen). The two differ by 55x, which is the whole reason the
# estimate exists: a long DIGITAL statement is a non-event, a long SCAN is an hour.
#   digital: 400 pages / 12,000 rows in 69.5s, flat at 0.17 s/page
#   scanned: 3 pages in 27.8s, 9.3 s/page, and tesseract dominates it
# Re-measure after any change to reading or parsing; the figures are in
# docs/operational/maintaining-the-engine.md.
PARAM_SECS_PER_PAGE      <- 0.17
PARAM_SECS_PER_SCAN_PAGE <- 9.3
PARAM_MAX_PAGE_PT <- 2880         # a page dimension over this (40 in) can break render/OCR

# ---- redaction detection ---------------------------------------------------
# The occlusion scan renders each page to greyscale, calls a pixel "dark" below
# DARK_LEVEL (0 black .. 255 white), then flags a word whose box is OCC_THRESH-or-
# more filled with dark pixels as drawn-over. Together they decide "is this word
# hidden under a box?"; VECTOR_DPI is the render resolution for that scan.
PARAM_REDACT_DARK_LEVEL <- 60L    # greyscale value below which a pixel counts as dark
PARAM_REDACT_OCC_THRESH <- 0.70   # a word box at/above this dark-fill is occluded
# Render dpi for the digital vector-box scan. 100 is ~2x faster than 150 on the
# cold read and measured-equivalent for detection: a solid redaction box reads a
# dark-fill of 1.0 at any dpi (huge margin over the 0.70 gate), and the highest
# fill among VISIBLE words stays well under it (anti-aliasing lightens thin glyph
# strokes slightly MORE at lower dpi, so the false-positive margin is preserved).
# Don't drop below ~72 without re-checking the small-redaction (min_area_pt) margin.
PARAM_REDACT_VECTOR_DPI <- 100L   # render dpi for the digital vector-box scan
# A WORD UNDER AN OPAQUE BOX IS FLAT, WHATEVER COLOUR THE BOX IS. The dark-pixel
# test above asks "is this word black"; this one asks "can this word still be SEEN",
# which is the question that actually matters and the one a darkness test gets wrong
# in a specific and common way: a WHITE box over live text hides it completely and
# is not dark. Measured on a specimen with the same account number covered four ways
# -- black, white, grey, yellow -- the darkness test flagged the black box and
# MISSED the other three, all of which hide the number entirely.
#
# Visible text is dark strokes on a lighter ground, so the greyscale range inside
# its box is wide. A word under an opaque fill of any colour is a flat patch and its
# range collapses. Below this value the word is treated as hidden.
#
# THE NUMBER COMES FROM MEASUREMENT, with a wide margin either side:
#   covered words (all four colours) ........  0
#   lowest VISIBLE word on a real statement .. 23  (a huge watermark word box)
#   lowest visible across the shipped PDF fixtures . 247
# 16 sits clear of both. Too high and a faint watermark is called a redaction,
# withholding legible text -- the opposite failure, and just as bad.
PARAM_REDACT_FLAT_SPREAD <- 16L

# .plausible_year(y) -- is a 4-digit year within the trusted window? Vectorised.
.plausible_year <- function(y) {
  y <- suppressWarnings(as.integer(y))
  !is.na(y) & y >= PARAM_YEAR_MIN & y <= PARAM_YEAR_MAX
}

# .tolerant_date(s) -- parse a verbatim date / period bound to a Date under the
# statement date shapes, or NA, accepting only a plausible year. The SINGLE tolerant
# parser shared by reconcile + diagnose (they used to carry near-duplicates).
.tolerant_date <- function(s) {
  s <- as.character(s %||% NA)
  if (length(s) != 1 || is.na(s) || !nzchar(trimws(s))) return(as.Date(NA))
  for (f in c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%d %b %Y", "%d %B %Y",
              "%d/%m/%y", "%d-%m-%y", "%d %b %y", "%d %B %y")) {
    d <- suppressWarnings(as.Date(trimws(s), f))
    if (!is.na(d) && .plausible_year(format(d, "%Y"))) return(d)
  }
  as.Date(NA)
}

# .plausible_period_date(s) -- parse a statement PERIOD bound (e.g. md$period_start)
# to a Date under the shapes a period line uses, accepting only a plausible year;
# NA otherwise. The reader (parse_pdf_table, for year context) and the X-ray
# (inspect) BOTH derive the statement year from the period this way and MUST agree,
# so the format list lives here once -- add a new period date shape in ONE place and
# both stay in step. (Order matters: earlier formats win on an ambiguous 2-digit
# year, so keep the sequence as-is.)
.plausible_period_date <- function(s) {
  for (f in c("%d %b %Y", "%d %B %Y", "%d %b %y", "%d %B %y",
              "%d/%m/%Y", "%d/%m/%y", "%Y-%m-%d")) {
    d <- suppressWarnings(as.Date(s, f))
    if (!is.na(d) && .plausible_year(format(d, "%Y"))) return(d)
  }
  as.Date(NA)
}
