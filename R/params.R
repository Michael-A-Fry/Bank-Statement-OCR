# params.R -- the engine's numeric TUNING decisions in ONE place. Not vocabulary (the lexicon) and not
# deployment switches (config). It also hosts the two shared date helpers, beside the bound they apply.
# Catalogue and rationale: docs/context/engine-parameters.md.

# Outside this window a statement date is almost always a mis-parse: a 2-digit year read as 4-digit.
PARAM_YEAR_MIN <- 1990L
PARAM_YEAR_MAX <- 2100L

# Two money figures are "equal" within half a cent (never == on floats).
PARAM_MONEY_TOL <- 0.005


# ---- OCR routing (page_needs_ocr) ------------------------------------------
PARAM_OCR_MIN_CHARS     <- 20L    # fewer non-space chars than this -> treat as image
PARAM_OCR_MIN_WORDS     <- 3L     # fewer real word boxes than this -> scanned page
PARAM_OCR_MAX_BAD_RATIO <- 0.30   # more than this fraction of garbage chars -> OCR
PARAM_OCR_CELL_MIN_CONF <- 60     # per-cell OCR confidence floor (flag a cell below)
PARAM_OCR_PAGE_MIN_CONF <- 70     # page-mean OCR confidence below this -> loud caveat
PARAM_OCR_RENDER_DPI    <- 300L   # dpi a scanned page is rasterised at before OCR

# One visual row = word tops within this many points. Too small splits a row, too large merges rows.
PARAM_PDF_ROW_TOL <- 3L

# The narrowest a band may be squeezed to: below this it holds no whole figure at any type size.
PARAM_DOC_MIN_COL_PT <- 8
# Two page sizes within this many points are the SAME size - a rounding difference, nothing wider.
PARAM_DOC_SAME_PAGE_PT <- 2

# A stated transaction count above this is almost certainly a mis-read, so it is dropped, not trusted.
PARAM_STATED_COUNT_MAX <- 100000L

# Not hard limits: the engine still tries, but warns, so a stall on a very large file has a reason.
PARAM_MAX_PAGES   <- 100L         # PDFs longer than this may hit tool limits
PARAM_MAX_PAGE_PT <- 2880         # a page dimension over this (40 in) can break render/OCR

# The occlusion scan calls a pixel "dark" below DARK_LEVEL and flags a word box OCC_THRESH-or-more filled.
PARAM_REDACT_DARK_LEVEL <- 60L    # greyscale value below which a pixel counts as dark
PARAM_REDACT_OCC_THRESH <- 0.70   # a word box at/above this dark-fill is occluded
# 100 dpi is twice as fast as 150 and measured-equivalent: a solid redaction box reads a dark-fill of
# 1.0 at any dpi, well over the 0.70 gate. Do not go below about 72 without re-checking the margin.
PARAM_REDACT_VECTOR_DPI <- 100L   # render dpi for the digital vector-box scan

# Is a 4-digit year within the trusted window? Vectorised.
.plausible_year <- function(y) {
  y <- suppressWarnings(as.integer(y))
  !is.na(y) & y >= PARAM_YEAR_MIN & y <= PARAM_YEAR_MAX
}

# The single tolerant date parser shared by reconcile and diagnose. NA unless the year is plausible.
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

# The reader and the X-ray BOTH derive the statement year from the period this way and must agree, so
# the format list lives here once. Order matters: earlier formats win on an ambiguous 2-digit year.
.plausible_period_date <- function(s) {
  for (f in c("%d %b %Y", "%d %B %Y", "%d %b %y", "%d %B %y",
              "%d/%m/%Y", "%d/%m/%y", "%Y-%m-%d")) {
    d <- suppressWarnings(as.Date(s, f))
    if (!is.na(d) && .plausible_year(format(d, "%Y"))) return(d)
  }
  as.Date(NA)
}
