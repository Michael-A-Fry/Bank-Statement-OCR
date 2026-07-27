# parse_pdf_table.R -- extract a transaction table from PDF word boxes using a
# declarative `format: pdf` template. Same forensic contract as the delimited
# path: verbatim descriptions, redactions honoured, deterministic, never crashes.
#
# Deliberately simple + generic (no per-statement code):
#   * words are grouped into visual ROWS by y-position (row_tol),
#   * each word is placed in a COLUMN by which x-band its centre falls in,
#   * a row is KEPT only if its date cell parses as a real date -- which cleanly
#     ignores headers, annotations, footers and section-header "gaps".
#   Multi-page tables are stitched by processing every page the same way.

# .pdf_cell(rw, cspec) -- text of the words whose centre falls in a column band,
# left-to-right, space-joined; NA when the band is empty or unmapped.
# Index the atomic columns (x / text) rather than subsetting the data.frame:
# rw[mask, , drop=FALSE] goes through R's slow `[.data.frame` method, and this
# runs 12-17x per visual row across every page (the measured parse hot spot).
# which() also drops any NA in the band mask (finite word boxes never produce one,
# so this is byte-identical on real data), which the old df-subset would have kept.
.pdf_cell <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_character_)
  x <- rw$x; cx <- x + rw$width / 2
  sel <- which(cx >= cspec$x_min & cx <= cspec$x_max)
  if (!length(sel)) return(NA_character_)
  paste(rw$text[sel][order(x[sel])], collapse = " ")
}

# .cell_minconf(rw, cspec) -- lowest OCR word confidence (0-100) among the words
# that fall in a column band; NA when there is no confidence data (a text-layer
# page has none) or the band is empty. Used to flag a low-confidence value in a
# CRITICAL cell (date/amount/balance) that a page-mean confidence would hide.
.cell_minconf <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_real_)
  if (!("conf" %in% names(rw))) return(NA_real_)
  cx <- rw$x + rw$width / 2
  sel <- which(cx >= cspec$x_min & cx <= cspec$x_max)   # atomic index, not df subset
  cf <- suppressWarnings(as.numeric(rw$conf[sel])); cf <- cf[!is.na(cf) & cf >= 0]
  if (!length(cf)) NA_real_ else min(cf)
}

# .has_money(x) -- TRUE when a cell carries any digit (used only to decide a row
# is a transaction). Parsing itself is left to the sign-aware parse_amount/.num,
# which read the raw cell verbatim: pre-stripping here used to remove a trailing
# OD/DR/CR and silently flip an overdrawn balance's sign.
# A REDACTED money cell counts as "has money": the amount existed, it is just
# hidden. Without this, redacting an amount (or a whole row) makes the keep test
# see no digit and DROP the whole transaction -- silently losing a row that was
# really there. Kept this way, the row survives, its amount is nulled, and it is
# flagged redacted -- hidden, never lost.
.has_money <- function(x) { x <- as.character(x); grepl("[0-9]", x, perl = TRUE, useBytes = TRUE) | grepl("REDACT", x, ignore.case = TRUE, perl = TRUE, useBytes = TRUE) }
# .has_real_money -- a VISIBLE money value: a digit that is NOT a redaction token.
# Used for the keep decision's evidence test: a row is only a transaction when it
# still shows a real date or a real amount. A cell that is only [REDACTED] does
# NOT count -- we never invent a transaction out of a redaction (the statement
# arrives already redacted; our job is to read what is there, not guess what is
# hidden). A row with a redacted amount is still kept when its DATE is real.
.has_real_money <- function(x) { x <- as.character(x); grepl("[0-9]", x, perl = TRUE, useBytes = TRUE) & !grepl("REDACT", x, ignore.case = TRUE, perl = TRUE, useBytes = TRUE) }

# =============================== THE BAND FRAME ==============================
# THE one coordinate space every stored band lives in. Defined here, once.
#
# A template's bands -- table$columns$<col>$x_min/x_max, table$region,
# table$metadata_regions, and stored force_rows y-bands -- are PDF points measured
# on a page of size table$ref_width x table$ref_height: the size of the page the
# analyst drew them on, recorded by draft_template() (R/draft.R). THAT page size,
# and nothing else, is the BAND FRAME. Origin top-left, y increasing downward, the
# same axes pdftools reports word boxes in.
#
# Everything that touches a band converts with the two functions below and nowhere
# else. They are exact inverses:
#   * the READER scales each page's WORDS into the frame     (page value  x  scale)
#   * anything DRAWING on a page -- the X-ray, the band editor -- scales each BAND
#     out to that page                                       (band value  /  scale)
#     because it paints over a raster of the page's own size.
# So a band drawn on screen and a band matched by the reader are the same band.
#
# Anything that draws or stores a band in raw page coordinates is displaced on
# every page whose size differs from the frame -- and a displaced band is
# indistinguishable from an untouched default one, which is exactly why this
# contract is written down once rather than re-derived at each call site.
#
# A page within .PAGE_SCALE_SNAP of the frame counts as the SAME size (scale
# exactly 1), so the overwhelmingly common A4-on-A4 case is bit-for-bit untouched
# and never accumulates rounding drift.
# A4 portrait in points -- the size virtually every NZ bank statement PDF uses,
# and the frame assumed when a template records none.
.A4_W <- 595.28
.A4_H <- 841.89
.PAGE_SCALE_SNAP <- 0.02   # a page within 2% of the frame counts as the same size

# pdf_band_frame(template) -- the frame this template's bands are stored in.
# Always a usable positive size: a missing, unparseable or nonsense ref falls back
# to A4 rather than producing a scale of NA/Inf that would move every band.
pdf_band_frame <- function(template) {
  t <- template$table %||% list()
  num <- function(v, dflt) { v <- suppressWarnings(as.numeric(v %||% dflt))
    if (length(v) != 1L || is.na(v) || v <= 0) dflt else v }
  list(width = num(t$ref_width, .A4_W), height = num(t$ref_height, .A4_H))
}

# pdf_band_frame_scale(frame, page_w, page_h) -- c(sx, sy). MULTIPLY a page
# coordinate by this to land in the frame; DIVIDE a band by it to draw that band
# on the page. An unknown page size scales by 1 (assume it is already the frame).
pdf_band_frame_scale <- function(frame, page_w, page_h) {
  one <- function(pv, fv) {
    s <- if (length(pv) == 1L && !is.na(pv) && is.finite(pv) && pv > 0) fv / pv else 1
    if (abs(s - 1) < .PAGE_SCALE_SNAP) 1 else s
  }
  c(one(page_w, frame$width), one(page_h, frame$height))
}

# .words_to_band_frame(w, frame, page_w, page_h) -- map one page's word boxes into
# the band frame, so absolute x-bands line up even when this copy of the statement
# is a different physical size (a rescan, another export, a different scanner DPI).
# Without it a differently-sized page pushes every value out of its band (all rows
# drop) or, for a small difference, pushes right-aligned amounts out on SOME rows
# only -- the "match a chunk, miss a chunk" bug.
.words_to_band_frame <- function(w, frame, page_w, page_h) {
  if (is.null(w) || !nrow(w)) return(w)
  s <- pdf_band_frame_scale(frame, page_w, page_h)
  if (s[1] == 1 && s[2] == 1) return(w)
  w$x <- w$x * s[1]; w$width  <- w$width  * s[1]
  w$y <- w$y * s[2]; w$height <- w$height * s[2]
  w
}

# .stitch_split(a, b) -- combine a date-bearing half-row (a) and an amount-bearing
# half-row (b) into one transaction: keep a's date, fill the money/other cells from
# whichever half has them, concatenate descriptions. Used by the split-row recovery
# when a statement staggers a single row's cells onto different baselines.
.stitch_split <- function(a, b) {
  fld <- function(x, y) { x <- x %||% NA_character_
    if (!is.na(x) && nzchar(trimws(x))) x else (y %||% NA_character_) }
  cat_txt <- function(x, y) { x <- x %||% ""; y <- y %||% ""
    trimws(paste(if (is.na(x)) "" else x, if (is.na(y)) "" else y)) }
  m <- a
  for (f in c("amount", "debit", "credit", "balance", "particulars", "code",
              "reference", "other_party", "type")) m[[f]] <- fld(a[[f]], b[[f]])
  m$description <- cat_txt(a$description, b$description)
  m$raw <- cat_txt(a$raw, b$raw)
  # Carry BOTH halves' words: the merged row's word list must stay complete, or a
  # later continuation fold would measure "which words did no band claim?" against
  # half the row. `a` is the upper half, so a-then-b is reading order.
  m$.cx <- c(a$.cx, b$.cx); m$.txt <- c(a$.txt, b$.txt)
  m$.y0 <- min(a$.y0, b$.y0, na.rm = TRUE); m$.y1 <- max(a$.y1, b$.y1, na.rm = TRUE)
  for (nm in union(names(a), names(b))) if (startsWith(nm, "x.")) m[[nm]] <- fld(a[[nm]], b[[nm]])
  m$.stitched <- TRUE
  m
}

# .group_rows(ys, tol) -- assign each word (y sorted ascending) to a visual ROW.
# A new row starts when a word's top is more than `tol` below the CURRENT row's
# top -- anchored to the row's start, NOT cumulative pairwise gaps. The old
# gap method (cumsum(diff(y) > tol)) collapsed a whole block of tightly-set lines
# into ONE giant row whenever no single word-to-word gap exceeded tol (dense
# leading), which silently merged many transactions -- the "only 3 rows on the
# page" bug. Anchoring to the row start separates lines correctly as long as the
# line pitch exceeds tol, and is identical to the old method on well-spaced pages.
.group_rows <- function(ys, tol) {
  n <- length(ys); if (n == 0) return(integer(0))
  grp <- integer(n); cur <- 1L; ref <- ys[1]
  for (i in seq_len(n)) {
    if (ys[i] > ref + tol) { cur <- cur + 1L; ref <- ys[i] }
    grp[i] <- cur
  }
  grp
}

# .pdf_has_amount(r, style) / .pdf_is_summary(description, raw) -- the amount and
# summary-line halves of the row KEEP predicate, lifted to module level so the
# table reader (parse_pdf_table) and the Inspect overlay (inspect_pdf_layout)
# share ONE definition and can never disagree about which rows are transactions.
.pdf_has_amount <- function(r, style) {
  .has_money(if (identical(style, "debit_credit_cols"))
    paste(r$debit %||% "", r$credit %||% "") else (r$amount %||% ""))
}
# The summary-line VOCABULARY. These labels used to be hardcoded in the matcher
# below, so a bank that writes "Balance b/fwd" or "Sub total" turned a summary
# line into a FABRICATED transaction -- and the only cure was a code change. They
# now live in the lexicon (category `summary_line_labels`, list semantics -- see
# R/lexicon.R) with these built-ins as the default, so an analyst adds a wording in
# YAML, in the Admin vocabulary editor, with no code change.
#
# Each entry is matched as a WHOLE label, case-insensitively; a literal space is
# read as "one or more spaces". They are written WITHOUT ^...$ because the matcher
# anchors them -- matching the whole label is what keeps a real
# "Total Payments to ACME Ltd" a transaction.
.PDF_SUMMARY_LABELS <- c(
  "(statement )?(opening|closing) balance",
  "balance (brought|carried) (forward|fwd|f/?wd?)",
  "balance [bc]/f",
  "(brought|carried) forward",
  "total (withdrawals|deposits|credits|debits|payments|fees|transactions)")

# .pdf_summary_rx() -- the compiled whole-label alternation, built ONCE and cached.
# The cache lives in the LEXICON's own environment so an admin vocabulary edit
# (which calls clear_lexicon_cache()) invalidates this pattern too -- otherwise a
# saved edit would appear to take effect everywhere except here. The key carries NO
# path: this runs 5-8x per visual row, and resolving the lexicon path stats the
# config file, so the hot path must be a single environment lookup and nothing else.
.PDF_SUMMARY_RX_KEY <- "pdf::summary_rx"
.pdf_summary_rx <- function() {
  hit <- get0(.PDF_SUMMARY_RX_KEY, envir = .LEXICON_CACHE, inherits = FALSE, ifnotfound = NULL)
  if (!is.null(hit)) return(hit)
  # lex() already merges the admin's entries with .PDF_SUMMARY_LABELS (list
  # semantics: union with the built-in), so an absent category is an exact no-op.
  # safe(): a lexicon read must never be able to stop a statement parsing.
  labels <- safe(as.character(lex("summary_line_labels", safe(.lexicon_path(), NULL))),
                 .PDF_SUMMARY_LABELS)
  labels <- unique(trimws(labels))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels)) labels <- .PDF_SUMMARY_LABELS   # never end up with no rule at all
  # A single un-compilable admin entry must never break parsing: drop it, keep the rest.
  ok <- vapply(labels, function(p) isTRUE(tryCatch({
    grepl(p, "probe", perl = TRUE); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)),
    logical(1), USE.NAMES = FALSE)
  labels <- labels[ok]
  rx <- paste0("^(?:", paste(gsub(" ", "\\\\s+", labels), collapse = "|"), ")$")
  assign(.PDF_SUMMARY_RX_KEY, rx, envir = .LEXICON_CACHE)
  rx
}

# .PDF_LINE_NOISE -- the tokens that are never part of a line's LABEL: any number
# (a date, an amount, a balance, an account or reference number), a month name, and
# a dr/cr/od marker. Stripping them is how a printed line is reduced to the words a
# human reads as its label -- "13 Jul Closing Balance $0.00 $2.00 1.97" is the
# closing balance line however many money columns it happens to print into.
# A token must be whole (bounded by spaces), so "13.90%" and "74c168003e" survive
# untouched: they are not numbers, they are part of what the line SAYS.
.PDF_LINE_NOISE <- paste0(
  "(?:^|\\s)(?:",
    "[$(]?[0-9][0-9,./:-]*[0-9)]?",                                 # 1,234.56  03/02/2026  12-3456-0789012-50
    "|(?:jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*",    # a month name, abbreviated or full
    "|dr|cr|od",                                                    # debit / credit / overdrawn marker
  ")(?=\\s|$)")

# .pdf_line_label(s) -- one printed line reduced to its label, lower-cased.
# perl + useBytes: this runs 5-8x per visual row across the split / continuation /
# keep passes (~1300-5000 calls per parse) and the cost is almost entirely R's
# default TRE engine compiling the alternations. PCRE is 3-4x faster on the same
# patterns, and useBytes both removes the C-locale warning and makes the
# substitutions robust on invalid-UTF-8 bytes (no new failure mode).
# An empty band reads as NA, which is the commonest input here, so it exits first.
# Only the ENDS are trimmed: .pdf_summary_rx compiles a literal space in a label to
# "\s+", so runs of spaces left INSIDE by the noise strip already match, and the
# squeeze they would need is pure cost. Trailing "-" and ":" go with the
# whitespace -- "Opening balance -" and "Closing balance:" are still the label.
.pdf_line_label <- function(s) {
  s <- s %||% ""
  if (length(s) != 1L || is.na(s) || !nzchar(s)) return("")
  s <- gsub(.PDF_LINE_NOISE, " ", tolower(s), perl = TRUE, useBytes = TRUE)
  sub("[\\s:-]+$", "", sub("^\\s+", "", s, perl = TRUE, useBytes = TRUE),
      perl = TRUE, useBytes = TRUE)
}

# A summary line (opening/closing balance, brought/carried forward, totals) is NOT
# a transaction even though it carries a money value on a dated line. Match the
# WHOLE label so a real "Total Payments to ACME Ltd", or a real "TFR TO SAVINGS -
# BALANCE", is KEPT.
#
# BOTH the description cell AND the whole raw line are checked, ALWAYS (N30). The
# old code read `raw` only when the description band came back EMPTY -- so when a
# band was displaced and the description came back non-empty but WRONG (a date
# fragment, part of a number, a neighbouring column), the fallback never ran,
# "Opening balance" was never seen even though it sat in `raw`, and the row was
# kept as a transaction with the balance figure landing in the debit column: an
# INVENTED transaction at a material amount. Reading a wrongly-captured fragment
# INSTEAD of the full line is never better than reading both.
#
# Checking raw is safe because a label must match WHOLE (.pdf_summary_rx anchors
# ^...$) after .pdf_line_label has removed the date and money columns the raw line
# also carries. "24 Aug TFR To 63A Rent 465.00 17.26" reduces to "tfr to rent",
# not to a label. The residual cost is a payee literally NAMED "Closing Balance",
# which would be skipped -- and skipped VISIBLY (it is counted in skipped_row_count
# and the X-ray names it a summary line), where the bug it replaces was silent.
# An analyst can drop the wording in the Admin vocabulary; no code change.
.pdf_is_summary <- function(description, raw = NULL) {
  rx <- .pdf_summary_rx()
  hit <- function(s) { s <- .pdf_line_label(s)
    nzchar(s) && grepl(rx, s, perl = TRUE, useBytes = TRUE) }
  hit(description) || hit(raw)          # `||` keeps the common path at one reduction
}

# .is_footer_noise(s) -- a page footer / running header ("Page 2 of 2", "continued
# on next page") is NOT a transaction continuation, even though it is a date-less,
# money-less text line. Module-level so parse_pdf_table (continuation merge) and
# inspect_pdf_layout (skipped-row reasons) share ONE definition.
.is_footer_noise <- function(s) {
  s <- tolower(trimws(s %||% ""))
  grepl(paste0("^page\\s+\\d+(\\s+of\\s+\\d+)?$",         # "Page 2 of 2"
               "|^\\d+\\s+of\\s+\\d+$",
               "|continued\\s+(on\\s+)?(next|over)",       # "continued on next page"
               "|^statement\\s+(continued|continues)",
               # "Carried Forward to next page" / "Brought forward from previous
               # page" (N33). Without this the footer is a date-less, money-less
               # text line, so the continuation merge folded it into the LAST real
               # transaction above it and emitted "To 63A Rent Rent 63A Sar Carried
               # Forward to next page" - a description the statement never printed.
               # No figure was wrong, which is why nothing caught it; descriptions
               # are promised VERBATIM, so a silent rewrite is the defect.
               #
               # The page words are required. A bare "Carried forward" IS a real
               # summary line and is caught by .pdf_is_summary as a whole label --
               # loosening THAT anchor to cover this footer is what would start
               # eating "Total Payments to ACME Ltd", so the two stay separate.
               "|(carried|brought)\\s+forward\\s+(to|from)\\s+(the\\s+)?",
               "(next|previous|following|preceding|last)\\b"), s)
}

# .pdf_real_amount(rec, style) -- the money cell holds a VISIBLE number, not just a
# redaction token. The "evidence" half of the keep rule, alongside a real date.
.pdf_real_amount <- function(rec, style) {
  .has_real_money(if (identical(style, "debit_credit_cols"))
    paste(rec$debit %||% "", rec$credit %||% "") else (rec$amount %||% ""))
}

# pdf_keep_row(rec, style, date_ok, date_redacted, keep_dateless) -- THE decision
# "is this visual row a transaction?", in ONE place.
#
# WHY it lives here and not in the reader's closure: the X-ray (inspect_pdf_layout)
# has to paint EXACTLY the rows the reader keeps, and row_coverage counts what the
# X-ray paints. When the two carried separate copies of the rule they drifted -- the
# reader grew a fourth keep branch (keep_dateless_rows) that the X-ray never got, so
# the reader kept rows the X-ray reported as SKIPPED and row_coverage told the
# analyst "N row(s) skipped for an unreadable date", prescribing the exact wrong
# remedy for rows that were never lost. One function, two callers, no drift.
#
#   date_ok       -- the date cell parsed to a real date
#   date_redacted -- the date cell was HIDDEN (present but blacked out)
#   keep_dateless -- template opt-in for shared-date statements (see keep_dateless_rows)
pdf_keep_row <- function(rec, style, date_ok, date_redacted = FALSE,
                         keep_dateless = FALSE) {
  if (.pdf_is_summary(rec$description, rec$raw)) return(FALSE)
  if (!.pdf_has_amount(rec, style)) return(FALSE)   # must carry an amount slot
  if (isTRUE(date_ok)) return(TRUE)                 # real date + an amount slot
  # No real date: only REAL evidence in the money column can make it a transaction.
  # We never fabricate a row out of a redaction alone.
  real_amt <- .pdf_real_amount(rec, style)
  (isTRUE(date_redacted) && real_amt) || (isTRUE(keep_dateless) && real_amt)
}

# .pdf_row_reason(rec, style, date_ok) -- WHY a visual row is NOT kept as a
# transaction, in plain words a non-engineer can act on. rec carries the same
# cells .pdf_has_amount reads; date_ok is whether the date cell parsed (or was
# redacted). "" means it IS a transaction (kept). Shared by the X-ray so the
# reason it shows can never drift from the engine's actual keep rule. The
# continuation case is decided by the caller (it needs the neighbouring row).
# .pdf_row_code(rec, style, date_ok) -- WHY a visual row is not kept, as a stable
# CODE. "" means it IS a transaction. The code is what the engine decides on; the
# sentence a person reads is a lookup below. They used to be the same string, and
# two decisions were made by pattern-matching English prose against it - so
# rewording one sentence (which this codebase does constantly) would have silently
# switched off the thin-parse defence. A code cannot be reworded by accident.
.pdf_row_code <- function(rec, style, date_ok) {
  has_amt <- .pdf_has_amount(rec, style)
  is_summ <- .pdf_is_summary(rec$description, rec$raw)
  if (isTRUE(date_ok) && has_amt && !is_summ) return("")
  if (is_summ) return("summary_line")
  if (!isTRUE(date_ok) && !has_amt) return("heading_or_note")
  if (!isTRUE(date_ok)) return("date_unparsed")
  "amount_missing"
}

# ACTIONABLE codes: the row looked like a transaction and could not be read. These
# are the only skips that mean something is WRONG - a heading or a summary line is
# skipped on every healthy statement. One list, used by the completeness check and
# by row_coverage, so the two can never disagree about what counts as a problem.
.PDF_ACTIONABLE_CODES <- c("date_unparsed", "amount_missing")

# The sentence for each code, in words a non-engineer can act on. Reword freely:
# nothing decides anything on this text.
.PDF_ROW_REASON_TEXT <- c(
  summary_line    = "summary line (opening / closing balance, carried forward, or a total) - not a transaction",
  heading_or_note = "no date and no amount - treated as a heading, note or wrapped line",
  date_unparsed   = "the date didn't parse - usually the date format in the template is wrong",
  amount_missing  = "no amount in the money column(s) - check the amount / debit / credit bands")

.pdf_row_reason <- function(rec, style, date_ok) {
  code <- .pdf_row_code(rec, style, date_ok)
  if (!nzchar(code)) return("")
  unname(.PDF_ROW_REASON_TEXT[code])
}

# .append_cell(cur, add) -- append wrapped text to a cell that may be EMPTY. An
# empty band reads as NA (not NULL), and `NA %||% ""` yields NA because NA is not
# NULL -- so paste() rendered the literal string "NA" into the cell and the first
# continuation line of an otherwise-blank reference / particulars column came out
# as "NA REF-9". That is a fabricated value in a field promised verbatim, so treat
# NA and whitespace as "nothing here" on BOTH sides.
.append_cell <- function(cur, add) {
  txt <- function(v) { v <- as.character(v %||% "")
    if (!length(v) || is.na(v[1])) "" else trimws(v[1]) }
  trimws(paste(txt(cur), txt(add)))
}

# .pdf_in_band(cx, cspec) -- which word centres fall inside a column band (all
# FALSE for an unmapped band). The per-WORD form of .pdf_cell's membership test,
# so the continuation merge can tell which words a band actually claimed and
# therefore which words no band claimed -- the ones that used to be dropped.
.pdf_in_band <- function(cx, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max))
    return(rep(FALSE, length(cx)))
  !is.na(cx) & cx >= cspec$x_min & cx <= cspec$x_max
}

# .forced_band_hit(page, y0, y1, force_rows) -- does a visual row [y0,y1] on `page`
# overlap any user-confirmed force_rows band? Module-level so the reader (which
# keeps the row) and the X-ray (which paints it kept) agree on which rows the user
# forced in. Each band is list(page, y_min, y_max) in PDF points.
.forced_band_hit <- function(page, y0, y1, force_rows) {
  if (is.null(force_rows) || !length(force_rows)) return(FALSE)
  for (fb in force_rows) {
    if (!identical(as.integer(fb$page %||% NA_integer_), as.integer(page))) next
    ymin <- suppressWarnings(as.numeric(fb$y_min %||% -Inf))
    ymax <- suppressWarnings(as.numeric(fb$y_max %||% Inf))
    if (is.finite(y0) && is.finite(y1) && y1 >= ymin && y0 <= ymax) return(TRUE)
  }
  FALSE
}

# Per-cell OCR confidence floor: a word below PARAM_OCR_CELL_MIN_CONF (0-100) in a
# date/amount/balance cell earns an `ocr_low_conf` flag. Deliberately conservative
# -- only clearly-doubtful reads are flagged. Tunable in R/params.R.

parse_pdf_table <- function(input, template, force_rows = NULL, meta = NULL) {
  t <- template$table %||% list()
  cols <- t$columns %||% list()
  extras_cols <- t$extras %||% list()
  region <- t$region %||% list()
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% PARAM_PDF_ROW_TOL)); if (is.na(row_tol)) row_tol <- PARAM_PDF_ROW_TOL
  date_fmt <- t$date_format %||% "%d/%m/%Y"
  style <- t$amount_sign %||% "signed"
  # decimal_mark: dot | comma | auto. Accepted top-level or inside the table block
  # so a European PDF template can declare its locale.
  dec <- template$decimal_mark %||% t$decimal_mark %||% "auto"
  udef <- template$unsigned_default %||% t$unsigned_default %||% "debit"
  # type_dc tokens: the value in the indicator column that means DEBIT, and
  # (optionally) the one that means CREDIT. Accepted top-level or inside the table
  # block, like decimal_mark / unsigned_default, so a PDF template declares them
  # the same way a delimited one does. tcv NULL = the binary back-compat rule
  # (anything that is not the debit token is a credit); declaring it makes an
  # unrecognised indicator fail CLOSED to NA instead of being signed a credit.
  tdv <- template$type_debit_value  %||% t$type_debit_value  %||% "D"
  tcv <- template$type_credit_value %||% t$type_credit_value
  words_by_page <- input$words %||% list()
  # The band frame these bands are stored in (see THE BAND FRAME, top of file).
  frame <- pdf_band_frame(template)
  page_w <- input$page_width  %||% rep(NA_real_, length(words_by_page))
  page_h <- input$page_height %||% rep(NA_real_, length(words_by_page))
  # Bring every page's words into the frame ONCE, so the row loop, the
  # metadata_regions lookups and the force_rows bands all work in one coordinate
  # space. Same-size pages are untouched (snap-to-1).
  words_by_page <- lapply(seq_along(words_by_page), function(p) {
    wp <- words_by_page[[p]]
    if (is.null(wp) || !nrow(wp)) return(wp)
    .words_to_band_frame(as.data.frame(wp, stringsAsFactors = FALSE), frame, page_w[p], page_h[p])
  })
  # force_rows y-bands come from the X-ray, which draws in each page's own space;
  # bring them into the frame too so a forced row on a rescaled page still matches
  # the (now normalised) word rows.
  if (!is.null(force_rows) && length(force_rows)) force_rows <- lapply(force_rows, function(fb) {
    pg <- suppressWarnings(as.integer(fb$page %||% 1L))
    sy <- if (!is.na(pg) && pg >= 1 && pg <= length(page_h))
            pdf_band_frame_scale(frame, page_w[pg], page_h[pg])[2] else 1
    if (!is.null(fb$y_min)) fb$y_min <- fb$y_min * sy
    if (!is.null(fb$y_max)) fb$y_max <- fb$y_max * sy
    fb
  })

  # force_rows: user-confirmed "this IS a transaction" bands (from the X-ray's
  # skipped-row list), each list(page, y_min, y_max) in PDF points. A visual row
  # overlapping a band is KEPT even if its date/amount don't parse -- but it is
  # FLAGGED (`forced`, plus date_unresolved / malformed as they apply) so a
  # manually added row is never silently trusted. The bands come from rows already
  # inside the table region, so no extra region handling is needed here.
  .row_forced <- function(r) .forced_band_hit(r$page, r$.y0, r$.y1, force_rows)

  # .first_date(cell) -- keep only the FIRST date in a date cell. A PDF date band
  # can capture two dates on a row (a transaction date AND a processed/value
  # date); appending a year to "17 Oct 17 Sep" makes R read the second day as the
  # year (-> 0017-10-17), so dates come out wildly wrong. Trim to the leading date
  # (this format's count of whitespace-separated pieces). It also drops a stray
  # word that bleeds into the band. date_raw keeps the verbatim cell.
  .date_fields <- length(strsplit(trimws(date_fmt), "[[:space:]]+")[[1]])
  .first_date <- function(cells) vapply(cells, function(cc) {
    if (is.na(cc)) return(NA_character_)
    toks <- strsplit(trimws(cc), "[[:space:]]+")[[1]]
    if (length(toks) <= .date_fields) as.character(cc)
    else paste(toks[seq_len(.date_fields)], collapse = " ")
  }, character(1), USE.NAMES = FALSE)

  recs <- list()
  for (p in seq_along(words_by_page)) {
    w <- words_by_page[[p]]
    if (is.null(w) || !nrow(w)) next
    w <- as.data.frame(w, stringsAsFactors = FALSE)   # already in the band frame (above)
    if (!is.null(region$x_min)) w <- w[(w$x + w$width) >= region$x_min, , drop = FALSE]
    if (!is.null(region$x_max)) w <- w[w$x <= region$x_max, , drop = FALSE]
    if (!is.null(region$y_min)) w <- w[w$y >= region$y_min, , drop = FALSE]
    if (!is.null(region$y_max)) w <- w[w$y <= region$y_max, , drop = FALSE]
    if (!nrow(w)) next
    w <- w[order(w$y, w$x), , drop = FALSE]
    grp <- .group_rows(w$y, row_tol)
    for (g in unique(grp)) {
      rw <- w[grp == g, , drop = FALSE]
      ord <- order(rw$x)
      rec <- list(page = p,
        # The row's words in reading order, with their centre-x. Kept so the
        # continuation merge can account for EVERY word on a folded line instead
        # of only the words a mapped descriptive band happened to claim.
        .cx = (rw$x + rw$width / 2)[ord], .txt = as.character(rw$text)[ord],
        .y0 = min(rw$y), .y1 = max(rw$y + rw$height),
        .h = suppressWarnings(stats::median(rw$height, na.rm = TRUE)),
        date = .pdf_cell(rw, cols$date), description = .pdf_cell(rw, cols$description),
        amount = .pdf_cell(rw, cols$amount), balance = .pdf_cell(rw, cols$balance),
        debit = .pdf_cell(rw, cols$debit), credit = .pdf_cell(rw, cols$credit),
        particulars = .pdf_cell(rw, cols$particulars), code = .pdf_cell(rw, cols$code),
        reference = .pdf_cell(rw, cols$reference), other_party = .pdf_cell(rw, cols$other_party),
        type = .pdf_cell(rw, cols$type),
        raw = paste(rw$text[order(rw$x)], collapse = " "))
      # lowest OCR confidence across this row's CRITICAL cells (NA on text pages).
      cc <- c(.cell_minconf(rw, cols$date), .cell_minconf(rw, cols$amount),
              .cell_minconf(rw, cols$balance), .cell_minconf(rw, cols$debit),
              .cell_minconf(rw, cols$credit))
      cc <- cc[!is.na(cc)]
      rec$ocr_minconf <- if (length(cc)) min(cc) else NA_real_
      for (ef in names(extras_cols)) rec[[paste0("x.", ef)]] <- .pdf_cell(rw, extras_cols[[ef]])
      recs[[length(recs) + 1L]] <- rec
    }
  }

  # Year context: many statements show the day/month only ("21 Apr") and put the
  # year in the statement period. When the date_format has no year token, attach
  # the year from the period (single year -> that year; a period spanning a
  # year-end -> the year that lands each date inside the period). Generic: no
  # bank-specific logic, driven entirely by the statement's own period text.
  # Statement-level metadata (period + opening/closing balance) via the label
  # dictionary. Wiring the balances into the header lets balance_reconciliation
  # actually run for PDFs -- so a PDF that reconciles earns "high" trust and the
  # completeness guard is satisfied, exactly like a delimited statement.
  # Reuse the caller's metadata when it handed one in (convert_statement already
  # computed the identical extract_metadata(input) for detection); recompute only
  # when parsed standalone. Same input + same dictionary -> byte-identical md.
  md <- meta %||% safe(extract_metadata(input), NULL)
  has_year <- grepl("%[Yy]", date_fmt)
  # Year context is computed ALWAYS (not just for year-less templates): the
  # year-less FALLBACK below needs it even when the template's own format
  # carries a year. Parse the period bounds (2-digit years too, e.g. ASB
  # "13 Jun 26") and take the year(s) from the parsed dates -- more robust than
  # a 4-digit regex. Reject implausible years: as.Date("13 Aug 25", "%d %b %Y")
  # yields 0025 (not NA), so without this the 4-digit format greedily eats a
  # 2-digit year.
  pdate <- .plausible_period_date   # shared with the X-ray (R/params.R) so year context can't drift
  p0 <- pdate(md$period_start); p1 <- pdate(md$period_end)
  yrs <- suppressWarnings(as.integer(format(c(p0, p1)[!is.na(c(p0, p1))], "%Y")))
  yrs <- unique(yrs[!is.na(yrs)])
  # Some statements print day/month only in the table AND give no parseable
  # period. Rather than silently drop EVERY row (year-less dates parse to NA
  # and fail the date filter), scan the page text for a plausible 4-digit year.
  # Only used when it is UNAMBIGUOUS (a single distinct year on the page): if
  # the text shows zero or several years we do not guess, keeping to the
  # "never silently wrong" contract. date_raw stays verbatim regardless.
  year_from_text <- FALSE
  if (!length(yrs)) {
    alltext <- paste(unlist(input$pages %||% input$text %||% character(0)), collapse = " ")
    cy <- suppressWarnings(as.integer(regmatches(alltext,
            gregexpr("\\b(?:19|20)[0-9]{2}\\b", alltext, perl = TRUE))[[1]]))
    cy <- unique(cy[.plausible_year(cy)])
    if (length(cy) == 1L) { yrs <- cy; year_from_text <- TRUE }
  }
  # .with_year(raw, fmt) -- append the period's year to a year-less date string;
  # with two candidate years (a period spanning New Year) pick the one that
  # lands the date inside the period.
  .with_year <- function(raw, fmt) {
    if (!length(yrs)) return(raw)
    bad <- is.na(raw) | !nzchar(trimws(raw))
    if (length(yrs) == 1) { out <- paste(raw, yrs[1]); out[bad] <- raw[bad]; return(out) }
    out <- vapply(raw, function(r) {
      if (is.na(r) || !nzchar(trimws(r))) return(NA_character_)
      cand <- suppressWarnings(as.Date(paste(r, yrs), fmt))
      inp <- !is.na(cand) & (is.na(p0) | cand >= p0) & (is.na(p1) | cand <= p1)
      pick <- if (any(inp)) which(inp)[1] else which(!is.na(cand))[1]
      if (is.na(pick)) pick <- 1L
      paste(r, yrs[pick])
    }, character(1))
    out
  }
  full_date <- function(raw) raw
  eff_fmt <- date_fmt
  if (!has_year) {
    eff_fmt <- paste(date_fmt, "%Y")
    full_date <- function(raw) .with_year(raw, eff_fmt)
  }
  # Year-less FALLBACK for templates whose declared format carries a year: real
  # statements print "17 Sep" in the table while the template (built from a
  # different export of the same bank) says e.g. %d/%m/%Y. When the template
  # format fails on a date cell, try the day+month family with the year taken
  # from the statement period -- deterministic (the year comes from the
  # statement itself, never guessed). Every row read this way is flagged
  # date_alt_format so the mismatch is visible and fixable in the template.
  # .fb_first: the fallback's own leading-date trim. .first_date() trims to the
  # TEMPLATE format's piece count (1 for %d/%m/%Y), which would cut "02 May"
  # down to "02" before the fallback could read it; day+month is always 2 pieces.
  .fb_first <- function(cells) vapply(cells, function(cc) {
    if (is.na(cc)) return(NA_character_)
    toks <- strsplit(trimws(cc), "[[:space:]]+")[[1]]
    if (length(toks) >= 2) paste(toks[1:2], collapse = " ") else as.character(cc)
  }, character(1), USE.NAMES = FALSE)
  # The name-month family, BOTH orders: "17 Sep" can only be day-month and
  # "Sep 17" only month-day (a month name is never a day), so trying both is
  # deterministic - a user who picked the wrong year-less variant in the
  # toolkit still gets every row.
  .FB_FMTS <- c("%d %b %Y", "%d %B %Y", "%b %d %Y", "%B %d %Y")
  .fb_parse <- function(raw) {
    out <- rep(NA_character_, length(raw))
    if (!length(yrs) || !length(raw)) return(out)
    raw <- .fb_first(raw)
    for (f in .FB_FMTS) {
      need <- is.na(out)
      if (!any(need)) break
      out[need] <- suppressWarnings(parse_date(.with_year(raw[need], f), f)$iso)
    }
    out
  }
  # Sentinel variant for when NO year is known anywhere: does the cell read as
  # a name-month date at all (either order)? Used to KEEP the row - date_iso
  # stays NA and the row is flagged date_unresolved, same as the existing
  # unknown-year path: data preserved, never silently wrong.
  .fb_sentinel_ok <- function(raw) {
    raw <- .fb_first(raw)
    for (f in .FB_FMTS)
      if (!is.na(suppressWarnings(parse_date(paste(raw, "2000"), f)$iso))) return(TRUE)
    FALSE
  }

  # Keep only genuine transaction rows: the date cell must parse AND the row must
  # carry a real money amount (in the amount, or debit/credit, column). Requiring
  # an amount drops date-only lines that leak into the date band -- a statement's
  # issue date, a page header, a "balance brought forward" carry line -- which a
  # date-parse-only filter would wrongly keep. Balance is deliberately NOT enough
  # on its own (carry-forward rows aren't transactions).
  # Balance alone is deliberately NOT enough (carry-forward rows aren't
  # transactions); a real transaction is never *named* "closing balance". Both
  # halves live in module-level helpers so the Inspect overlay applies the SAME
  # rule -- errs toward keeping (a stray summary breaks reconciliation LOUDLY,
  # dropping a real transaction loses money SILENTLY, which the contract forbids).
  .has_amount <- function(r) .pdf_has_amount(r, style)
  # .real_amount -- the amount is a VISIBLE number, not a redaction token. This is
  # the evidence half of the keep test alongside a real date.
  .real_amount <- function(r) .pdf_real_amount(r, style)
  .is_summary <- function(r) .pdf_is_summary(r$description, r$raw)
  # Did we manage to resolve a year for a year-less date format? When we did NOT
  # (no period, no year anywhere in the text), dropping every row would silently
  # lose a whole statement's transactions -- the worst forensic outcome, and one
  # seen on real data. Instead, still KEEP a dated money line if its day/month is
  # valid under the base format (sentinel year), carry date_raw verbatim, leave
  # date_iso NA (the real year is genuinely unknown), and flag it date_unresolved
  # so the reviewer can assign the year -- data preserved, never silently wrong.
  year_resolved <- has_year || length(yrs) > 0
  # Document-level gate for the fallback: it exists for statements whose WHOLE
  # table is unreadable under the template's declared format (the reported real
  # cases: "17 Sep" rows under a %d/%m/%Y template, or under the WRONG year-less
  # variant, month-day picked for a day-month statement). If the template's own
  # format reads even ONE date in the document, the fallback stays off - on such
  # documents a stray day+month fragment carrying a number (a distribution note,
  # a wrapped line) must not sneak in as a transaction.
  prim_zero <- length(recs) > 0 && !any(vapply(recs, function(r) {
    rw <- .first_date(r$date %||% NA_character_)
    if (year_resolved)
      !is.na(suppressWarnings(parse_date(full_date(rw), eff_fmt)$iso))
    else
      !is.na(suppressWarnings(parse_date(paste(rw, "2000"), paste(date_fmt, "%Y"))$iso))
  }, logical(1)))
  fb_active <- prim_zero && length(yrs) > 0
  # The keep-decision runs .date_ok on the SAME date cell 4-16 times across the
  # split-row / continuation / final-keep passes, and each call re-runs the full
  # date pipeline (parse_date + .normalise_date_str + the year-less fallback) -- the
  # measured hot spot. It's a PURE function of the cell string given the
  # document-level constants above (all fixed before the passes), so memoise it on
  # the string: a fresh cache per parse_pdf_table call (no cross-document leak), and
  # the date string is invariant under stitching/continuation (which only fold text
  # into description/reference), so the same key stays correct across passes.
  .dok_cache <- new.env(parent = emptyenv())
  .date_ok <- function(raw) {
    key <- if (length(raw) != 1L || is.na(raw)) "<<NA>>" else raw
    # An environment key is a SYMBOL, so a non-ASCII cell name warns ("unable to
    # translate to native encoding") in a non-UTF-8 locale (the air-gapped box).
    # Real date cells are ASCII, so only memoise those; a rare non-ASCII mis-aligned
    # cell simply recomputes (behaviour-identical). useBytes avoids a scan warning.
    cacheable <- !grepl("[^ -~]", key, useBytes = TRUE)
    if (cacheable) {
      hit <- get0(key, envir = .dok_cache, inherits = FALSE, ifnotfound = NULL)
      if (!is.null(hit)) return(hit)
    }
    raw1 <- .first_date(raw)
    v <- if (year_resolved) {
      if (!is.na(suppressWarnings(parse_date(full_date(raw1), eff_fmt)$iso))) TRUE
      else fb_active && !is.na(.fb_parse(raw))
    } else if (!is.na(suppressWarnings(parse_date(paste(raw1, "2000"),
                                                  paste(date_fmt, "%Y"))$iso))) {
      TRUE
    } else {
      # No year known anywhere AND the declared format reads nothing: still keep
      # clear name-month rows (either order) - flagged date_unresolved downstream.
      prim_zero && .fb_sentinel_ok(raw)
    }
    if (cacheable) assign(key, v, envir = .dok_cache)
    v
  }
  .redacted_cell <- function(v) !is.na(v) && grepl("REDACT", toupper(as.character(v)))
  # KEEP RULE. A row is a transaction when it still shows REAL evidence -- a real
  # date OR a real amount -- and carries an amount slot (real, or redacted so the
  # value is merely hidden) and is not a summary line. The statement ARRIVES
  # already redacted; we read what is visible, we never fabricate a transaction
  # from redaction alone:
  #   * amount blacked out, date visible  -> kept (real date), amount = NA, flagged
  #   * date blacked out, amount visible  -> kept (real amount), date = NA, flagged
  #   * a WHOLE row blacked out           -> no real date, no real amount -> NOT a
  #       row; it simply does not appear (rows above/below are unaffected). This is
  #       correct: we don't guess how many transactions a black block hid.
  #   * a header / non-transaction line covered by a box -> no real date/amount ->
  #       never becomes a transaction.
  # A date that is merely UNPARSEABLE (e.g. a template mis-map "13-14-9999") is NOT
  # a redaction: the row is dropped and flagged "date didn't parse" so the template
  # gets fixed. Only an explicitly REDACTED date (hidden) is carried on a real
  # amount.
  # Opt-in for shared-date statements (e.g. HSBC): several transaction rows sit
  # under ONE date printed once, so most rows have no date of their own. Off by
  # default (keeping every dateless money line would add spurious rows on normal
  # statements). When a template sets keep_dateless_rows: true, an amount-bearing,
  # non-summary row is kept even with no date -- its date stays BLANK (never guessed
  # or propagated from a neighbour) and it is flagged `no_date`, so the row is
  # preserved without inventing which date it belongs to.
  keep_dateless <- isTRUE(t$keep_dateless_rows %||% FALSE)
  # ONE keep rule, shared with the X-ray (pdf_keep_row, module level). Adding a
  # branch here now automatically changes what the X-ray paints and what
  # row_coverage counts, so the reader and the diagnostic can never disagree again.
  .is_txn <- function(r)
    pdf_keep_row(r, style, .date_ok(r$date), .redacted_cell(r$date), keep_dateless)

  # Split-row recovery: some statements render one transaction's cells on slightly
  # different baselines, so the DATE and the AMOUNT land in DIFFERENT visual rows (a
  # stagger larger than row_tol). Each half then fails the keep test and a whole
  # block of them vanishes -- the "half the page is missing" bug. Stitch an adjacent
  # date-only group and amount-only group back into one transaction. It fires ONLY
  # on a genuine split -- one side has a real date but no money, the other has money
  # but no date at all -- so a well-formed statement is never touched, a summary or
  # carried-forward line (which keeps its own date) is never merged, and the
  # stitched row is flagged (row_stitched) for review.
  if (length(recs) > 1) {
    d_ok  <- vapply(recs, function(r) .date_ok(r$date) || .redacted_cell(r$date), logical(1))
    has_a <- vapply(recs, .has_amount, logical(1))
    d_txt <- vapply(recs, function(r) !is.na(r$date) && nzchar(trimws(r$date)), logical(1))
    date_only <- function(k) d_ok[k] && !has_a[k] && !.is_summary(recs[[k]])
    amt_only  <- function(k) has_a[k] && !d_ok[k] && !d_txt[k] && !.is_summary(recs[[k]])
    drop <- logical(length(recs)); i <- 1L
    while (i < length(recs)) {
      if (drop[i]) { i <- i + 1L; next }
      j <- i + 1L; while (j <= length(recs) && drop[j]) j <- j + 1L
      if (j > length(recs)) break
      a <- recs[[i]]; b <- recs[[j]]
      lh <- if (is.finite(a$.h) && a$.h > 0) a$.h else 10
      close <- identical(a$page, b$page) && is.finite(a$.y1) && is.finite(b$.y0) &&
               (b$.y0 - a$.y1) <= 1.2 * lh && (b$.y0 - a$.y1) >= -1.2 * lh
      if (close && ((date_only(i) && amt_only(j)) || (amt_only(i) && date_only(j)))) {
        dk <- if (date_only(i)) i else j; ak <- if (amt_only(i)) i else j
        recs[[i]] <- .stitch_split(recs[[dk]], recs[[ak]])
        drop[j] <- TRUE; d_ok[i] <- TRUE; has_a[i] <- TRUE; d_txt[i] <- TRUE
        i <- j + 1L
      } else i <- i + 1L
    }
    recs <- recs[!drop]
  }

  # Multi-line descriptions: a wrapped payee / particulars spills onto the next
  # visual row, which carries NO date and NO money. Instead of dropping it (losing
  # verbatim content), fold its text into the PRECEDING kept transaction. Only a
  # clear continuation merges (no parseable date, no money anywhere, not a summary)
  # so it can never invent a transaction or join two real ones. Template can turn
  # it off with merge_continuation: false. (.is_footer_noise lives at module level
  # so the X-ray's skipped-row reasons apply the same footer test.)
  if (!identical(t$merge_continuation %||% TRUE, FALSE) && length(recs) > 1) {
    # a forced row is a transaction anchor here too, so it is never folded away.
    is_txn <- vapply(recs, .is_txn, logical(1)) | vapply(recs, .row_forced, logical(1))
    # Descriptive columns a wrapped line can spill into. Each keeps its OWN text:
    # a reference (or particulars, etc.) that wraps onto 2-3 lines stays in its own
    # column instead of being smeared into `description`. This is the fix for the
    # reported "the reference column is mapped, its words show (purple) in the
    # X-ray, but the output cell is empty" case -- the reference lived on the
    # transaction's continuation lines and was being folded into description.
    descr_cols <- c("description", "reference", "particulars", "code", "other_party", "type")
    # own_cols: the descriptive columns that keep their OWN wrapped text. NOT
    # `description` -- description is the catch-all home for everything else on the
    # line, and folding it separately would put a stray word AFTER the description
    # band's words instead of in its printed left-to-right place.
    own_cols <- setdiff(descr_cols, "description")
    # last_y1: the baseline of the last line attached to the current transaction
    # (the txn itself, or its most recent continuation). Measuring the gap from
    # HERE -- not from the transaction's own baseline -- lets a value wrap across
    # SEVERAL lines (a reference printed over three lines), each close to the line
    # above it, instead of only the first continuation folding in.
    last_txn <- 0L; last_y1 <- -Inf; drop <- logical(length(recs))
    for (i in seq_along(recs)) {
      if (is_txn[i]) { last_txn <- i; last_y1 <- recs[[i]]$.y1; next }
      if (last_txn == 0L) next
      r <- recs[[i]]; prev <- recs[[last_txn]]
      line_txt <- r$raw %||% ""
      line_txt <- if (is.na(line_txt)) "" else trimws(line_txt)   # nzchar(NA) is TRUE -> guard it
      money_here <- .has_amount(r) || .has_money(r$balance %||% "")
      # Proximity: a continuation is the line right below the previous attached line
      # (same page, gap under ~one line height). A footer far down the page is excluded.
      lh <- if (is.finite(r$.h) && r$.h > 0) r$.h else 10
      close <- identical(r$page, prev$page) &&
               is.finite(r$.y0) && is.finite(last_y1) &&
               (r$.y0 - last_y1) <= 0.9 * lh && (r$.y0 - last_y1) >= -lh
      if (nzchar(line_txt) && !money_here && !.date_ok(r$date) && !.redacted_cell(r$date) &&
          !.is_summary(r) && !.is_footer_noise(line_txt) && close) {
        # NOTHING on a merged line may be dropped. Descriptions are VERBATIM, and a
        # continuation line's words are part of the description that was printed --
        # losing one silently rewrites the payee ("PAYMENT TO JOHN" + "SMITH" +
        # "HOLDINGS LTD" became "PAYMENT TO JOHN HOLDINGS LTD": a different legal
        # entity, presented as verbatim, with the amounts untouched so no check could
        # ever see it). The old loop folded only the words a mapped DESCRIPTIVE band
        # claimed, so a wrapped word that drifted into the date / amount / balance
        # band, or into no band at all, simply vanished.
        #
        # So: each own-column band still keeps its own wrapped text (a reference that
        # wraps stays in `reference`), and EVERY remaining word on the line -- the
        # description band's, the non-descriptive bands', and any unbanded word --
        # folds into the description in printed left-to-right order.
        own_hit <- rep(FALSE, length(r$.cx %||% numeric(0)))
        for (fld in own_cols) {
          add <- r[[fld]] %||% NA_character_
          if (!is.na(add) && nzchar(trimws(add))) {
            recs[[last_txn]][[fld]] <- .append_cell(recs[[last_txn]][[fld]], add)
            own_hit <- own_hit | .pdf_in_band(r$.cx, cols[[fld]])
          }
        }
        # Words no own-column band claimed, still in reading order (.txt is x-sorted).
        rest <- if (length(own_hit)) trimws(paste(r$.txt[!own_hit], collapse = " ")) else line_txt
        if (nzchar(rest))
          recs[[last_txn]]$description <- .append_cell(recs[[last_txn]]$description, rest)
        # Visible when it MATTERS: a fold that pulled in text from outside the
        # descriptive bands is the case that used to lose words, so mark the row.
        # An ordinary description wrap (all words inside the descriptive bands, which
        # the old code already folded whole) stays unflagged -- nothing changed there.
        stray <- if (length(own_hit)) {
          claimed <- own_hit
          for (fld in descr_cols) claimed <- claimed | .pdf_in_band(r$.cx, cols[[fld]])
          any(!claimed) && any(claimed)
        } else FALSE
        if (isTRUE(stray)) recs[[last_txn]]$.merged_stray <- TRUE
        drop[i] <- TRUE
        if (is.finite(r$.y1)) last_y1 <- max(last_y1, r$.y1)   # chain the next line from here
      }
    }
    recs <- recs[!drop]
  }

  # Redaction is deliberately allowed to make a row LOOK like a transaction (a
  # redacted date satisfies .is_txn via .redacted_cell; a redacted amount counts
  # as money). That is what preserves a real transaction whose date or amount was
  # blacked out -- the top priority, since silently DROPPING a real row is the
  # worst forensic outcome. The symmetric cost is that a box drawn over an
  # off-table summary/fee line can add a spurious row. We do NOT try to suppress
  # that here: any position/heuristic guard cannot tell a redacted edge
  # transaction from a redacted off-table line (both are a redacted date with no
  # real date), so it would drop real rows -- trading a visible over-count for
  # silent data loss. Instead the spurious row stays VISIBLE: it is flagged
  # `redacted`, it shows in the X-ray, and it makes row_count differ from the
  # statement's stated_count, which the reconciliation KPI reports. Over-count
  # that is surfaced beats under-count that is silent.
  forced_vec <- vapply(recs, .row_forced, logical(1))
  keep <- vapply(recs, .is_txn, logical(1)) | forced_vec
  # Completeness accounting for the no_unparsed_rows KPI. `candidate_rows` is every
  # visual row still standing at the keep decision (after stitching and after
  # continuation lines were folded into their transaction, so neither is miscounted
  # as lost); `skipped_rows` is how many of those the keep rule rejected -- headers,
  # summary lines, notes, and any row whose date or amount could not be read. It is
  # NOT a count of losses, so it must never be subtracted like a source line count;
  # it is reported so a PDF's completeness check states a real number instead of a
  # green tick for a check that never ran.
  candidate_rows <- length(recs)
  skipped_rows <- sum(!keep)
  # ACTIONABLE skips: rows that looked like transactions and could NOT be read --
  # the date didn't parse, or there was no amount in the money bands. These are the
  # only skips that mean something is WRONG. A heading, a summary line or a wrapped
  # continuation is skipped on every healthy statement, so counting those tells a
  # reviewer nothing.
  #
  # WHY it is counted here rather than left to row_coverage(): row_coverage re-parses
  # the whole PDF, so a completeness check could not use it without doubling the work
  # on every conversion. The keep decision is being made right here and the reason is
  # already computed -- counting is free.
  actionable_skips <- if (!length(recs)) 0L else
    sum(vapply(seq_along(recs), function(i) {
      if (keep[i]) return(FALSE)
      r <- recs[[i]]
      .pdf_row_code(r, style, .date_ok(r$date)) %in% .PDF_ACTIONABLE_CODES
    }, logical(1)))
  recs <- recs[keep]
  forced_vec <- forced_vec[keep]
  stitched_vec <- vapply(recs, function(r) isTRUE(r$.stitched), logical(1))
  merged_vec <- vapply(recs, function(r) isTRUE(r$.merged_stray), logical(1))
  n <- length(recs)
  getc <- function(f) if (n == 0) character(0) else
    vapply(recs, function(r) r[[f]] %||% NA_character_, character(1))

  fb_used <- if (n == 0) logical(0) else rep(FALSE, n)
  if (n == 0) {
    date_iso <- character(0); date_raw <- character(0); description <- character(0)
    amt <- list(value = numeric(0), direction = character(0), raw = character(0))
  } else {
    d <- parse_date(full_date(.first_date(getc("date"))), eff_fmt)
    date_iso <- d$iso; date_raw <- getc("date")   # date_raw stays verbatim (both dates, no year)
    # Cells the template format couldn't read: try the year-less fallback the
    # date gate already accepted them under, and mark each row it reads.
    miss <- is.na(date_iso) & !is.na(date_raw) & nzchar(trimws(date_raw))
    if (fb_active && any(miss)) {
      fbi <- .fb_parse(date_raw[miss])
      date_iso[miss] <- ifelse(is.na(fbi), date_iso[miss], fbi)
      fb_used[miss] <- !is.na(fbi)
    }
    if (identical(style, "debit_credit_cols")) {
      deb_raw <- getc("debit"); cr_raw <- getc("credit")
      amt <- parse_amount(NULL, "debit_credit_cols",
                          list(debit = deb_raw, credit = cr_raw, decimal = dec))
      cr_has <- !is.na(cr_raw) & nzchar(trimws(cr_raw))
      amt$raw <- ifelse(cr_has, cr_raw, deb_raw)
    } else {
      amt_raw <- getc("amount")
      amt_opts <- list(decimal = dec, unsigned_default = udef)
      if (identical(style, "type_dc")) {
        # The debit/credit INDICATOR column and its declared tokens must reach
        # parse_amount, exactly as the delimited path builds them (R/parse.R).
        # Omitting them handed parse_amount no `type` at all, so every row fell into
        # its back-compat "anything that isn't the debit token is a credit" branch
        # and was signed +magnitude: every debit silently became a credit, with the
        # "D" still printing beside a +500 credit so the row looked self-consistent,
        # and the fail-closed unknown-token path was unreachable.
        amt_opts$type <- getc("type")
        amt_opts$type_debit_value  <- tdv
        amt_opts$type_credit_value <- tcv   # NULL keeps the binary back-compat rule
      }
      amt <- parse_amount(amt_raw, style, amt_opts); amt$raw <- amt_raw
    }
    description <- clean_description(getc("description"))
  }
  vb <- function(f) if (n == 0) character(0) else blank_to_na(getc(f))
  has_bal <- !is.null(cols$balance)
  balance <- if (n == 0 || !has_bal) rep(NA_real_, n) else parse_amount(getc("balance"), "signed", list(decimal = dec))$value
  balance_raw <- if (n == 0 || !has_bal) rep(NA_character_, n) else getc("balance")

  # amt_redacted: the AMOUNT itself was hidden (amount cell, or a debit/credit
  # cell) -> the value is genuinely unknown and is nulled. `redacted` (the row
  # flag) is broader: any of date/amount/description hidden marks the row, but a
  # row whose DATE was redacted still keeps its real amount.
  amt_redacted <- if (n == 0) logical(0) else if (identical(style, "debit_credit_cols"))
    grepl("REDACTED", getc("debit"), ignore.case = TRUE) |
    grepl("REDACTED", getc("credit"), ignore.case = TRUE)
  else grepl("REDACTED", getc("amount"), ignore.case = TRUE)
  # The row flag fires when ANY cell was hidden -- date, amount, description,
  # balance, particulars, reference, account, code, type -- so a redaction of any
  # field is never silent. Read it off the row's raw text (which already carries
  # the [REDACTED] token for every guarded word) rather than a hand-listed subset.
  redacted <- if (n == 0) logical(0) else
    (amt_redacted | grepl("REDACT", getc("raw"), ignore.case = TRUE))
  # malformed: the row was kept (dated line carrying a money-looking amount) yet
  # the amount could not be parsed to a number -- a genuine parse failure, not a
  # redaction. Flagging it lets the no_unparsed_rows KPI catch PDF parse gaps the
  # same way it already does for delimited (previously this path never set the
  # flag, so no_unparsed_rows was blind to a mis-read PDF amount).
  malformed <- if (n == 0) logical(0) else (is.na(amt$value) & !redacted)
  # date_unresolved: kept despite an unknown year (see .date_ok) -- date_iso is NA
  # but the transaction is preserved. Marked so trust/review reflect the gap. A
  # user-forced row whose date simply didn't parse (date_iso NA) is flagged the
  # same way, so a manually added row with no usable date is never silently trusted.
  date_unresolved <- if (n == 0) logical(0)
    else ((!year_resolved & !is.na(date_raw) & nzchar(trimws(date_raw))) |
          (forced_vec & is.na(date_iso)))
  # date_year_inferred (P3-a): a year-less table with NO parseable period took its
  # year from a single 4-digit number in free page text (e.g. a footer "(c) 2019").
  # The date parses, so date_unresolved won't fire -- but the year is a GUESS from
  # non-period text, so flag every dated row with a resolved date, so trust/review
  # never treat that year as proven. date_raw stays verbatim regardless.
  date_year_inferred <- if (n == 0) logical(0)
    else (year_from_text & !is.na(date_iso))
  # no_date: a shared-date row kept without a date of its own (keep_dateless_rows).
  # The blank date is deliberate -- flagged so a reviewer sees it was never guessed.
  no_date_kept <- if (n == 0) logical(0)
    else (keep_dateless & is.na(date_iso) &
          (is.na(date_raw) | !nzchar(trimws(date_raw))) & !forced_vec)
  # ocr_low_conf: an OCR'd date/amount/balance cell held a word below the
  # per-cell confidence floor -- a likely misread digit that the page-mean
  # confidence would mask. Only fires on OCR pages (text pages carry no conf).
  ocr_minconf <- if (n == 0) numeric(0) else vapply(recs, function(r)
    if (is.null(r$ocr_minconf)) NA_real_ else as.numeric(r$ocr_minconf), numeric(1))
  ocr_low <- if (n == 0) logical(0) else (!is.na(ocr_minconf) & ocr_minconf < PARAM_OCR_CELL_MIN_CONF)
  flags <- if (n == 0) character(0) else {
    add <- function(base, cond, tok)
      ifelse(cond, ifelse(nzchar(base), paste0(base, ",", tok), tok), base)
    f <- ifelse(redacted, "redacted", ifelse(malformed, "malformed", ""))
    f <- add(f, date_unresolved, "date_unresolved")
    f <- add(f, date_year_inferred, "date_year_inferred")  # year guessed from free page text
    f <- add(f, no_date_kept, "no_date")        # shared-date row kept with a blank date
    f <- add(f, fb_used, "date_alt_format")     # read via the year-less fallback
    f <- add(f, forced_vec, "forced")           # a row the user added by hand from the X-ray
    f <- add(f, stitched_vec, "row_stitched")   # two half-rows the reader re-joined
    # a wrapped line whose words strayed outside the descriptive bands was folded
    # in: the description is complete but was ASSEMBLED, so say so.
    f <- add(f, merged_vec, "row_text_merged")
    f <- add(f, ocr_low, "ocr_low_conf")
    f
  }
  if (n > 0) amt$value[amt_redacted] <- NA_real_   # only null when the AMOUNT was hidden

  core <- coerce_core(data.frame(
    row_id = seq_len(n), date = date_iso, date_raw = date_raw, description = description,
    amount = if (n == 0) numeric(0) else amt$value,
    amount_raw = if (n == 0) character(0) else amt$raw,
    direction = if (n == 0) character(0) else amt$direction,
    balance = balance, balance_raw = balance_raw,
    particulars = vb("particulars"), code = vb("code"), reference = vb("reference"),
    other_party = vb("other_party"), type = vb("type"),
    currency = rep(template$currency %||% "NZD", n), flags = flags,
    stringsAsFactors = FALSE))

  if (n > 0) {
    ex <- list(row_id = seq_len(n))
    for (ef in names(extras_cols)) ex[[ef]] <- blank_to_na(getc(paste0("x.", ef)))
    # Separate money-in / money-out statements: keep the verbatim debit and credit
    # cells. They collapse into the signed `amount` + `direction` for the stable
    # core table, but forensic users want to see the ORIGINAL split, so they are
    # surfaced next to `amount` in the previews + workbook and preserved in JSON.
    if (identical(style, "debit_credit_cols")) {
      if (is.null(ex$debit))  ex$debit  <- blank_to_na(getc("debit"))
      if (is.null(ex$credit)) ex$credit <- blank_to_na(getc("credit"))
    }
    extras <- data.frame(ex, stringsAsFactors = FALSE, check.names = FALSE)
  } else extras <- data.frame(row_id = integer(0))

  # sign-aware: a "$1,234.56 DR" / "(1,234.56)" opening/closing balance keeps its
  # negative sign (via .num) instead of being read as a positive number; the
  # template's decimal locale applies here too.
  .money_num <- function(x) .num(x %||% NA_character_, dec)
  # metadata_regions: user-drawn boxes that PIN a header value (balances, statement
  # period, account details) for statements whose label wording the dictionary
  # can't find. A box wins ONLY when it yields a value -- a correctly-read value is
  # never overwritten with nothing. Stored under table$metadata_regions, separate
  # from the column bands so it never widens the transaction region.
  mregions <- t$metadata_regions %||% list()
  .mr <- function(field, vtype) {
    reg <- mregions[[field]]; if (is.null(reg)) return(NA_character_)
    v <- safe(.field_from_region(words_by_page, reg, vtype)$value, NA_character_)
    if (is.null(v) || is.na(v) || !nzchar(v)) NA_character_ else v
  }
  .mr_or <- function(field, vtype, fallback) { v <- .mr(field, vtype); if (!is.na(v)) v else fallback }
  header <- list(
    bank = template$bank %||% NA_character_, statement_type = template$statement_type %||% NA_character_,
    template_id = template$id %||% NA_character_, template_version = template$version %||% NA,
    account_number = .mr_or("account_number", "text", NA_character_),
    account_name   = .mr_or("account_name", "text", NA_character_),
    period_start = .mr_or("period_start", "text", md$period_start %||% NA_character_),
    period_end   = .mr_or("period_end", "text", md$period_end %||% NA_character_),
    opening_balance = .money_num(.mr_or("opening_balance", "money", md$opening_balance)),
    closing_balance = .money_num(.mr_or("closing_balance", "money", md$closing_balance)),
    currency = template$currency %||% "NZD",
    source_file = basename(input$path), source_sha256 = input$sha256,
    page_count = input$meta$page_count %||% NA_integer_, row_count = n,
    stated_count = md$stated_count %||% NA_integer_,
    # Document provenance (producer / created / modified / encrypted) travels with the
    # header so the audit trail and the diagnostics can both see it. Without this the
    # provenance note could never fire: read_pdf produced it, diagnose read it, and
    # nothing in between carried it.
    pdf_doc = input$meta$pdf_doc,
    ocr_pages = input$meta$ocr_pages %||% 0L,
    ocr_min_confidence = input$meta$ocr_min_conf %||% NA_real_,
    redaction_scan_incomplete = input$meta$redaction_scan_incomplete %||% 0L)

  pages_v <- if (n == 0) integer(0) else vapply(recs, function(r) as.integer(r$page), integer(1))
  provenance <- data.frame(row_id = seq_len(n),
    source_ref = if (n == 0) character(0) else sprintf("pdf:p%d", pages_v),
    raw = getc("raw"), stringsAsFactors = FALSE)

  # source_line_count stays NA on purpose: a PDF has no "physical data line" count
  # that could be differenced against the parsed rows (headers, summary lines and
  # wrapped text are legitimately not transactions), so reconcile must NOT compute
  # a loss figure here. What it CAN state honestly is how many visual rows were
  # examined and how many the keep rule rejected -- see no_unparsed_rows.
  list(transactions = core, extras = extras, header = header,
       provenance = provenance, source_line_count = NA_integer_,
       visual_row_count = as.integer(candidate_rows),
       skipped_row_count = as.integer(skipped_rows),
       actionable_skip_count = as.integer(actionable_skips))
}
