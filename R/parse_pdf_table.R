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
.pdf_cell <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_character_)
  cx <- rw$x + rw$width / 2
  sel <- rw[cx >= cspec$x_min & cx <= cspec$x_max, , drop = FALSE]
  if (!nrow(sel)) return(NA_character_)
  paste(sel$text[order(sel$x)], collapse = " ")
}

# .cell_minconf(rw, cspec) -- lowest OCR word confidence (0-100) among the words
# that fall in a column band; NA when there is no confidence data (a text-layer
# page has none) or the band is empty. Used to flag a low-confidence value in a
# CRITICAL cell (date/amount/balance) that a page-mean confidence would hide.
.cell_minconf <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_real_)
  if (!("conf" %in% names(rw))) return(NA_real_)
  cx <- rw$x + rw$width / 2
  sel <- rw[cx >= cspec$x_min & cx <= cspec$x_max, , drop = FALSE]
  cf <- suppressWarnings(as.numeric(sel$conf)); cf <- cf[!is.na(cf) & cf >= 0]
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
.has_money <- function(x) { x <- as.character(x); grepl("[0-9]", x) | grepl("REDACT", x, ignore.case = TRUE) }
# .has_real_money -- a VISIBLE money value: a digit that is NOT a redaction token.
# Used for the keep decision's evidence test: a row is only a transaction when it
# still shows a real date or a real amount. A cell that is only [REDACTED] does
# NOT count -- we never invent a transaction out of a redaction (the statement
# arrives already redacted; our job is to read what is there, not guess what is
# hidden). A row with a redacted amount is still kept when its DATE is real.
.has_real_money <- function(x) { x <- as.character(x); grepl("[0-9]", x) & !grepl("REDACT", x, ignore.case = TRUE) }

# .group_rows(ys, tol) -- assign each word (y sorted ascending) to a visual ROW.
# A new row starts when a word's top is more than `tol` below the CURRENT row's
# top -- anchored to the row's start, NOT cumulative pairwise gaps. The old
# gap method (cumsum(diff(y) > tol)) collapsed a whole block of tightly-set lines
# into ONE giant row whenever no single word-to-word gap exceeded tol (dense
# leading), which silently merged many transactions -- the "only 3 rows on the
# page" bug. Anchoring to the row start separates lines correctly as long as the
# line pitch exceeds tol, and is identical to the old method on well-spaced pages.
# A4 portrait in points -- the size virtually every NZ bank statement PDF uses.
# When a template doesn't record the page size it was built on, we assume this, so
# a scanned/re-exported A4 statement still normalises correctly.
.A4_W <- 595.28
.A4_H <- 841.89
.PAGE_SCALE_SNAP <- 0.02   # treat a page within 2% of the reference as same-size

# .scale_words_to_ref(w, page_w, page_h, ref_w, ref_h) -- map one page's word
# boxes into the template's REFERENCE point space, so absolute x-bands line up even
# when this copy of the statement is a different physical size (a rescan, a
# different export, a different scanner DPI). A page the same size as the reference
# is left untouched (snap-to-1), so same-size parsing is bit-for-bit unchanged.
# Without this, a differently-sized page pushes every value out of its band (all
# rows drop) or, for a small difference, pushes right-aligned amounts out on SOME
# rows only -- the "match a chunk, miss a chunk" bug.
.scale_words_to_ref <- function(w, page_w, page_h, ref_w, ref_h) {
  if (is.null(w) || !nrow(w)) return(w)
  sx <- if (is.finite(page_w) && page_w > 0 && is.finite(ref_w) && ref_w > 0) ref_w / page_w else 1
  sy <- if (is.finite(page_h) && page_h > 0 && is.finite(ref_h) && ref_h > 0) ref_h / page_h else 1
  if (abs(sx - 1) < .PAGE_SCALE_SNAP) sx <- 1
  if (abs(sy - 1) < .PAGE_SCALE_SNAP) sy <- 1
  if (sx == 1 && sy == 1) return(w)
  w$x <- w$x * sx; w$width  <- w$width  * sx
  w$y <- w$y * sy; w$height <- w$height * sy
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
  m$.y0 <- min(a$.y0, b$.y0, na.rm = TRUE); m$.y1 <- max(a$.y1, b$.y1, na.rm = TRUE)
  for (nm in union(names(a), names(b))) if (startsWith(nm, "x.")) m[[nm]] <- fld(a[[nm]], b[[nm]])
  m$.stitched <- TRUE
  m
}

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
# A summary line (opening/closing balance, brought/carried forward, totals) is NOT
# a transaction even though it carries a money value on a dated line. Match the
# WHOLE label so a real "Total Payments to ACME Ltd" is KEPT; errs toward keeping.
.pdf_is_summary <- function(description, raw = NULL) {
  d <- tolower(trimws(description %||% ""))
  if (!nzchar(d)) d <- tolower(trimws(raw %||% ""))
  lbl <- trimws(sub("[-:]*\\s*[$(]?[0-9][0-9,. ]*[0-9)]*\\s*(dr|cr|od)?\\s*$", "", d))
  grepl(paste0(
    "^(statement\\s+)?(opening|closing)\\s+balance$",
    "|^balance\\s+(brought|carried)\\s+(forward|fwd|f/?wd?)$",
    "|^balance\\s+[bc]/f$",
    "|^(brought|carried)\\s+forward$",
    "|^total\\s+(withdrawals|deposits|credits|debits|payments|fees|transactions)$"),
    lbl)
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
               "|^statement\\s+(continued|continues)"), s)
}

# .pdf_row_reason(rec, style, date_ok) -- WHY a visual row is NOT kept as a
# transaction, in plain words a non-engineer can act on. rec carries the same
# cells .pdf_has_amount reads; date_ok is whether the date cell parsed (or was
# redacted). "" means it IS a transaction (kept). Shared by the X-ray so the
# reason it shows can never drift from the engine's actual keep rule. The
# continuation case is decided by the caller (it needs the neighbouring row).
.pdf_row_reason <- function(rec, style, date_ok) {
  has_amt <- .pdf_has_amount(rec, style)
  is_summ <- .pdf_is_summary(rec$description, rec$raw)
  if (isTRUE(date_ok) && has_amt && !is_summ) return("")
  if (is_summ)  return("summary line (opening / closing balance, carried forward, or a total) — not a transaction")
  if (!isTRUE(date_ok) && !has_amt) return("no date and no amount — treated as a heading, note or wrapped line")
  if (!isTRUE(date_ok)) return("the date didn't parse — usually the date format in the template is wrong")
  "no amount in the money column(s) — check the amount / debit / credit bands"
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

# Per-cell OCR confidence floor: a word below this (0-100) in a date/amount/
# balance cell earns an `ocr_low_conf` flag. Deliberately conservative -- only
# clearly-doubtful reads are flagged, so the signal stays meaningful.
.OCR_CELL_MIN_CONF <- 60

parse_pdf_table <- function(input, template, force_rows = NULL) {
  t <- template$table %||% list()
  cols <- t$columns %||% list()
  extras_cols <- t$extras %||% list()
  region <- t$region %||% list()
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% 3)); if (is.na(row_tol)) row_tol <- 3
  date_fmt <- t$date_format %||% "%d/%m/%Y"
  style <- t$amount_sign %||% "signed"
  # decimal_mark: dot | comma | auto. Accepted top-level or inside the table block
  # so a European PDF template can declare its locale.
  dec <- template$decimal_mark %||% t$decimal_mark %||% "auto"
  udef <- template$unsigned_default %||% t$unsigned_default %||% "debit"
  words_by_page <- input$words %||% list()
  # Reference page size the bands were drawn in (recorded when the template was
  # built; A4 otherwise). Each page's words are scaled into this space below.
  ref_w <- suppressWarnings(as.numeric(t$ref_width  %||% .A4_W)); if (is.na(ref_w) || ref_w <= 0) ref_w <- .A4_W
  ref_h <- suppressWarnings(as.numeric(t$ref_height %||% .A4_H)); if (is.na(ref_h) || ref_h <= 0) ref_h <- .A4_H
  page_w <- input$page_width  %||% rep(NA_real_, length(words_by_page))
  page_h <- input$page_height %||% rep(NA_real_, length(words_by_page))
  # Normalise every page's words into the template's reference space ONCE, so the
  # row loop, the metadata_regions lookups and the force_rows bands all share one
  # coordinate space. Same-size pages are untouched (snap-to-1).
  words_by_page <- lapply(seq_along(words_by_page), function(p) {
    wp <- words_by_page[[p]]
    if (is.null(wp) || !nrow(wp)) return(wp)
    .scale_words_to_ref(as.data.frame(wp, stringsAsFactors = FALSE), page_w[p], page_h[p], ref_w, ref_h)
  })
  # force_rows y-bands come from the X-ray, which is drawn in each page's own space;
  # bring them into the reference space too so a forced row on a rescaled page still
  # matches the (now normalised) word rows.
  if (!is.null(force_rows) && length(force_rows)) force_rows <- lapply(force_rows, function(fb) {
    pg <- suppressWarnings(as.integer(fb$page %||% 1L))
    s <- if (!is.na(pg) && pg >= 1 && pg <= length(page_h) && is.finite(page_h[pg]) && page_h[pg] > 0) ref_h / page_h[pg] else 1
    if (abs(s - 1) < .PAGE_SCALE_SNAP) s <- 1
    if (!is.null(fb$y_min)) fb$y_min <- fb$y_min * s
    if (!is.null(fb$y_max)) fb$y_max <- fb$y_max * s
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
    w <- as.data.frame(w, stringsAsFactors = FALSE)   # already normalised to ref space above
    if (!is.null(region$x_min)) w <- w[(w$x + w$width) >= region$x_min, , drop = FALSE]
    if (!is.null(region$x_max)) w <- w[w$x <= region$x_max, , drop = FALSE]
    if (!is.null(region$y_min)) w <- w[w$y >= region$y_min, , drop = FALSE]
    if (!is.null(region$y_max)) w <- w[w$y <= region$y_max, , drop = FALSE]
    if (!nrow(w)) next
    w <- w[order(w$y, w$x), , drop = FALSE]
    grp <- .group_rows(w$y, row_tol)
    for (g in unique(grp)) {
      rw <- w[grp == g, , drop = FALSE]
      rec <- list(page = p,
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
  md <- safe(extract_metadata(input), NULL)
  has_year <- grepl("%[Yy]", date_fmt)
  full_date <- function(raw) raw
  eff_fmt <- date_fmt
  if (!has_year) {
    eff_fmt <- paste(date_fmt, "%Y")
    # Parse the period bounds (2-digit years too, e.g. ASB "13 Jun 26") and take
    # the year(s) from the parsed dates -- more robust than a 4-digit regex.
    # Reject implausible years: as.Date("13 Aug 25", "%d %b %Y") yields 0025 (not
    # NA), so without this the 4-digit format greedily eats a 2-digit year.
    pdate <- function(s) { for (f in c("%d %b %Y", "%d %B %Y", "%d %b %y", "%d %B %y",
        "%d/%m/%Y", "%d/%m/%y", "%Y-%m-%d")) {
      dd <- suppressWarnings(as.Date(s, f))
      if (!is.na(dd) && as.integer(format(dd, "%Y")) >= 1990) return(dd) }; as.Date(NA) }
    p0 <- pdate(md$period_start); p1 <- pdate(md$period_end)
    yrs <- suppressWarnings(as.integer(format(c(p0, p1)[!is.na(c(p0, p1))], "%Y")))
    yrs <- unique(yrs[!is.na(yrs)])
    # Fallback: some statements print day/month only in the table AND give no
    # parseable period. Rather than silently drop EVERY row (year-less dates parse
    # to NA and fail the date filter), scan the page text for a plausible 4-digit
    # year. Only used when it is UNAMBIGUOUS (a single distinct year on the page):
    # if the text shows zero or several years we do not guess, keeping to the
    # "never silently wrong" contract. date_raw stays verbatim regardless.
    if (!length(yrs)) {
      alltext <- paste(unlist(input$pages %||% input$text %||% character(0)), collapse = " ")
      cy <- suppressWarnings(as.integer(regmatches(alltext,
              gregexpr("\\b(?:19|20)[0-9]{2}\\b", alltext, perl = TRUE))[[1]]))
      cy <- unique(cy[!is.na(cy) & cy >= 1990 & cy <= 2099])
      if (length(cy) == 1L) yrs <- cy
    }
    full_date <- function(raw) {
      if (!length(yrs)) return(raw)
      bad <- is.na(raw) | !nzchar(trimws(raw))
      if (length(yrs) == 1) { out <- paste(raw, yrs[1]); out[bad] <- raw[bad]; return(out) }
      out <- vapply(raw, function(r) {
        if (is.na(r) || !nzchar(trimws(r))) return(NA_character_)
        cand <- suppressWarnings(as.Date(paste(r, yrs), eff_fmt))
        inp <- !is.na(cand) & (is.na(p0) | cand >= p0) & (is.na(p1) | cand <= p1)
        pick <- if (any(inp)) which(inp)[1] else which(!is.na(cand))[1]
        if (is.na(pick)) pick <- 1L
        paste(r, yrs[pick])
      }, character(1))
      out
    }
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
  .real_amount <- function(r) .has_real_money(if (identical(style, "debit_credit_cols"))
    paste(r$debit %||% "", r$credit %||% "") else (r$amount %||% ""))
  .is_summary <- function(r) .pdf_is_summary(r$description, r$raw)
  # Did we manage to resolve a year for a year-less date format? When we did NOT
  # (no period, no year anywhere in the text), dropping every row would silently
  # lose a whole statement's transactions -- the worst forensic outcome, and one
  # seen on real data. Instead, still KEEP a dated money line if its day/month is
  # valid under the base format (sentinel year), carry date_raw verbatim, leave
  # date_iso NA (the real year is genuinely unknown), and flag it date_unresolved
  # so the reviewer can assign the year -- data preserved, never silently wrong.
  year_resolved <- has_year || length(yrs) > 0
  .date_ok <- function(raw) {
    raw <- .first_date(raw)
    if (year_resolved)
      return(!is.na(suppressWarnings(parse_date(full_date(raw), eff_fmt)$iso)))
    !is.na(suppressWarnings(parse_date(paste(raw, "2000"),
                                       paste(date_fmt, "%Y"))$iso))
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
  .is_txn <- function(r) {
    if (.is_summary(r)) return(FALSE)
    if (.date_ok(r$date)) return(.has_amount(r))              # real date + an amount slot
    .redacted_cell(r$date) && .real_amount(r) && .has_amount(r)
  }

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
    last_txn <- 0L; drop <- logical(length(recs))
    for (i in seq_along(recs)) {
      if (is_txn[i]) { last_txn <- i; next }
      if (last_txn == 0L) next
      r <- recs[[i]]; prev <- recs[[last_txn]]
      cont <- r$description %||% NA_character_
      if (is.na(cont) || !nzchar(trimws(cont))) cont <- r$raw %||% NA_character_
      cont <- if (is.na(cont)) "" else trimws(cont)   # nzchar(NA) is TRUE -> guard it
      money_here <- .has_amount(r) || .has_money(r$balance %||% "")
      # Proximity: a continuation is the line right below its transaction (same
      # page, gap under ~one line height). A footer far down the page is excluded.
      lh <- if (is.finite(r$.h) && r$.h > 0) r$.h else 10
      close <- identical(r$page, prev$page) &&
               is.finite(r$.y0) && is.finite(prev$.y1) &&
               (r$.y0 - prev$.y1) <= 0.9 * lh && (r$.y0 - prev$.y1) >= -lh
      if (nzchar(cont) && !money_here && !.date_ok(r$date) && !.redacted_cell(r$date) &&
          !.is_summary(r) && !.is_footer_noise(cont) && close) {
        recs[[last_txn]]$description <- trimws(paste(prev$description %||% "", cont))
        drop[i] <- TRUE
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
  recs <- recs[keep]
  forced_vec <- forced_vec[keep]
  stitched_vec <- vapply(recs, function(r) isTRUE(r$.stitched), logical(1))
  n <- length(recs)
  getc <- function(f) if (n == 0) character(0) else
    vapply(recs, function(r) r[[f]] %||% NA_character_, character(1))

  if (n == 0) {
    date_iso <- character(0); date_raw <- character(0); description <- character(0)
    amt <- list(value = numeric(0), direction = character(0), raw = character(0))
  } else {
    d <- parse_date(full_date(.first_date(getc("date"))), eff_fmt)
    date_iso <- d$iso; date_raw <- getc("date")   # date_raw stays verbatim (both dates, no year)
    if (identical(style, "debit_credit_cols")) {
      deb_raw <- getc("debit"); cr_raw <- getc("credit")
      amt <- parse_amount(NULL, "debit_credit_cols",
                          list(debit = deb_raw, credit = cr_raw, decimal = dec))
      cr_has <- !is.na(cr_raw) & nzchar(trimws(cr_raw))
      amt$raw <- ifelse(cr_has, cr_raw, deb_raw)
    } else {
      amt_raw <- getc("amount")
      amt <- parse_amount(amt_raw, style, list(decimal = dec, unsigned_default = udef)); amt$raw <- amt_raw
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
  # ocr_low_conf: an OCR'd date/amount/balance cell held a word below the
  # per-cell confidence floor -- a likely misread digit that the page-mean
  # confidence would mask. Only fires on OCR pages (text pages carry no conf).
  ocr_minconf <- if (n == 0) numeric(0) else vapply(recs, function(r)
    if (is.null(r$ocr_minconf)) NA_real_ else as.numeric(r$ocr_minconf), numeric(1))
  ocr_low <- if (n == 0) logical(0) else (!is.na(ocr_minconf) & ocr_minconf < .OCR_CELL_MIN_CONF)
  flags <- if (n == 0) character(0) else {
    add <- function(base, cond, tok)
      ifelse(cond, ifelse(nzchar(base), paste0(base, ",", tok), tok), base)
    f <- ifelse(redacted, "redacted", ifelse(malformed, "malformed", ""))
    f <- add(f, date_unresolved, "date_unresolved")
    f <- add(f, forced_vec, "forced")           # a row the user added by hand from the X-ray
    f <- add(f, stitched_vec, "row_stitched")   # two half-rows the reader re-joined
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

  if (length(extras_cols) && n > 0) {
    ex <- list(row_id = seq_len(n))
    for (ef in names(extras_cols)) ex[[ef]] <- blank_to_na(getc(paste0("x.", ef)))
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
    ocr_pages = input$meta$ocr_pages %||% 0L,
    ocr_min_confidence = input$meta$ocr_min_conf %||% NA_real_)

  pages_v <- if (n == 0) integer(0) else vapply(recs, function(r) as.integer(r$page), integer(1))
  provenance <- data.frame(row_id = seq_len(n),
    source_ref = if (n == 0) character(0) else sprintf("pdf:p%d", pages_v),
    raw = getc("raw"), stringsAsFactors = FALSE)

  list(transactions = core, extras = extras, header = header,
       provenance = provenance, source_line_count = NA_integer_)
}
