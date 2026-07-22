# params.R -- the engine's numeric TUNING decisions, in ONE visible place.
#
# These are neither VOCABULARY (that's the lexicon, admin-editable) nor DEPLOYMENT
# switches (that's config) -- they are algorithmic tuning a maintainer changes only
# with tests. They live here, named and commented, so every such decision is
# visible and consistent in one file instead of a bare literal repeated across the
# code. See docs/context/engine-parameters.md for the catalogue and rationale.
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

# ---- oversized-input advisories (diagnostics) ------------------------------
# Not hard limits -- the engine still tries. Above these it warns that a very
# large file may hit tool/render limits, so a stall has an explanation.
PARAM_MAX_PAGES   <- 100L         # PDFs longer than this may hit tool limits
PARAM_MAX_PAGE_PT <- 2880         # a page dimension over this (40 in) can break render/OCR

# ---- redaction detection ---------------------------------------------------
# The occlusion scan renders each page to greyscale, calls a pixel "dark" below
# DARK_LEVEL (0 black .. 255 white), then flags a word whose box is OCC_THRESH-or-
# more filled with dark pixels as drawn-over. Together they decide "is this word
# hidden under a box?"; VECTOR_DPI is the render resolution for that scan.
PARAM_REDACT_DARK_LEVEL <- 60L    # greyscale value below which a pixel counts as dark
PARAM_REDACT_OCC_THRESH <- 0.70   # a word box at/above this dark-fill is occluded
PARAM_REDACT_VECTOR_DPI <- 150L   # render dpi for the digital vector-box scan

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
