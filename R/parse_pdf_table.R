# parse_pdf_table.R -- extract a transaction table from PDF word boxes using a `format: pdf`
# template. Words group into rows by y, into columns by which x-band their centre falls in, and a
# row is kept only if its date cell parses.

# Text of the words whose centre falls in a column band; NA when empty or unmapped. Indexes the
# atomic columns rather than the data.frame - this runs 12-17x per row and is the parse hot spot.
.pdf_cell <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_character_)
  x <- rw$x; cx <- x + rw$width / 2
  sel <- which(cx >= cspec$x_min & cx <= cspec$x_max)
  if (!length(sel)) return(NA_character_)
  paste(rw$text[sel][order(x[sel])], collapse = " ")
}

.cell_minconf <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_real_)
  if (!("conf" %in% names(rw))) return(NA_real_)
  cx <- rw$x + rw$width / 2
  sel <- which(cx >= cspec$x_min & cx <= cspec$x_max)   # atomic index, not df subset
  cf <- suppressWarnings(as.numeric(rw$conf[sel])); cf <- cf[!is.na(cf) & cf >= 0]
  if (!length(cf)) NA_real_ else min(cf)
}

# Sign is left to parse_amount: pre-stripping a trailing OD/DR/CR here flipped an overdrawn
# balance's sign. REDACTED counts as money, or redacting an amount DROPS the transaction.
.has_money <- function(x) { x <- as.character(x); grepl("[0-9]", x, perl = TRUE, useBytes = TRUE) | grepl("REDACT", x, ignore.case = TRUE, perl = TRUE, useBytes = TRUE) }
# A VISIBLE money value. A cell that is only [REDACTED] does not count - we never invent a
# transaction out of a redaction.
.has_real_money <- function(x) { x <- as.character(x); grepl("[0-9]", x, perl = TRUE, useBytes = TRUE) & !grepl("REDACT", x, ignore.case = TRUE, perl = TRUE, useBytes = TRUE) }

# The band frame: the one coordinate space every stored band lives in - PDF points on a page of
# table$ref_width x ref_height, origin top-left. The reader scales each page's WORDS into the frame
# (x scale), anything drawing scales each BAND out to the page (/ scale). Backwards, and a displaced
# band is indistinguishable from an untouched default one.
.A4_W <- 595.28
.A4_H <- 841.89
.PAGE_SCALE_SNAP <- 0.02   # a page within 2% of the frame counts as the same size

pdf_band_frame <- function(template) {
  t <- template$table %||% list()
  num <- function(v, dflt) { v <- suppressWarnings(as.numeric(v %||% dflt))
    if (length(v) != 1L || is.na(v) || v <= 0) dflt else v }
  list(width = num(t$ref_width, .A4_W), height = num(t$ref_height, .A4_H))
}

pdf_band_frame_scale <- function(frame, page_w, page_h) {
  one <- function(pv, fv) {
    s <- if (length(pv) == 1L && !is.na(pv) && is.finite(pv) && pv > 0) fv / pv else 1
    if (abs(s - 1) < .PAGE_SCALE_SNAP) 1 else s
  }
  c(one(page_w, frame$width), one(page_h, frame$height))
}

# Map one page's words into the band frame so absolute x-bands line up on a differently-sized copy;
# without it a size difference pushes every value out of its band.
.words_to_band_frame <- function(w, frame, page_w, page_h) {
  if (is.null(w) || !nrow(w)) return(w)
  s <- pdf_band_frame_scale(frame, page_w, page_h)
  if (s[1] == 1 && s[2] == 1) return(w)
  w$x <- w$x * s[1]; w$width  <- w$width  * s[1]
  w$y <- w$y * s[2]; w$height <- w$height * s[2]
  w
}

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
  m$.cx <- c(a$.cx, b$.cx); m$.txt <- c(a$.txt, b$.txt)
  m$.y0 <- min(a$.y0, b$.y0, na.rm = TRUE); m$.y1 <- max(a$.y1, b$.y1, na.rm = TRUE)
  for (nm in union(names(a), names(b))) if (startsWith(nm, "x.")) m[[nm]] <- fld(a[[nm]], b[[nm]])
  m$.stitched <- TRUE
  m
}

# A new row starts when a word's top is more than `tol` below the CURRENT row's top, anchored to
# the row's start: cumulative pairwise gaps collapsed tightly-set lines into one merged row.
.group_rows <- function(ys, tol) {
  n <- length(ys); if (n == 0) return(integer(0))
  grp <- integer(n); cur <- 1L; ref <- ys[1]
  for (i in seq_len(n)) {
    if (ys[i] > ref + tol) { cur <- cur + 1L; ref <- ys[i] }
    grp[i] <- cur
  }
  grp
}

# The row KEEP predicate's halves, at module level so the reader and the X-ray overlay agree.
.pdf_has_amount <- function(r, style) {
  .has_money(if (identical(style, "debit_credit_cols"))
    paste(r$debit %||% "", r$credit %||% "") else (r$amount %||% ""))
}
# Defaults for the lexicon's summary-line vocabulary, so an analyst adds a wording with no code
# change. Matched as a WHOLE label, which keeps a real "Total Payments to ACME Ltd" a transaction.
.PDF_SUMMARY_LABELS <- c(
  "(statement )?(opening|closing) balance",
  "balance (brought|carried) (forward|fwd|f/?wd?)",
  "balance [bc]/f",
  "(brought|carried) forward",
  "total (withdrawals|deposits|credits|debits|payments|fees|transactions)",
  # Scope words are listed rather than allowing anything after "total": a payee is never a scope word.
  "totals?",
  "(sub|grand|page|period|statement|account|running)[-]?\\s*totals?",
  "totals? (at|for|of|on|to)( the)?( end of)?( the)? (page|period|statement|month|year|day|section|account)",
  "totals? (this|the) (page|period|statement|month|year|day|section|account)")

# Cached in the LEXICON's environment so an admin edit invalidates it too. The key carries no path:
# resolving the lexicon path stats the config file, and this runs 5-8x per visual row.
.PDF_SUMMARY_RX_KEY <- "pdf::summary_rx"
.pdf_summary_rx <- function() {
  hit <- get0(.PDF_SUMMARY_RX_KEY, envir = .LEXICON_CACHE, inherits = FALSE, ifnotfound = NULL)
  if (!is.null(hit)) return(hit)
  labels <- safe(as.character(lex("summary_line_labels", safe(.lexicon_path(), NULL))),
                 .PDF_SUMMARY_LABELS)
  labels <- unique(trimws(labels))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels)) labels <- .PDF_SUMMARY_LABELS   # never end up with no rule at all
  ok <- vapply(labels, function(p) isTRUE(tryCatch({
    grepl(p, "probe", perl = TRUE); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)),
    logical(1), USE.NAMES = FALSE)
  labels <- labels[ok]
  rx <- paste0("^(?:", paste(gsub(" ", "\\\\s+", labels), collapse = "|"), ")$")
  assign(.PDF_SUMMARY_RX_KEY, rx, envir = .LEXICON_CACHE)
  rx
}

# Only the leading DATE tokens and the trailing MONEY tokens go. Stripping month-ish words anywhere
# reduced NZ payees - MAYFAIR CLOSING BALANCE - to summary labels and dropped them.
.PDF_MONTH <- paste0("jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?",
                     "|jul(?:y)?|aug(?:ust)?|sep(?:t)?(?:ember)?|oct(?:ober)?",
                     "|nov(?:ember)?|dec(?:ember)?")
.PDF_LEAD_DATE <- paste0(
  "^(?:\\s*(?:[0-9][0-9,./:-]*|", .PDF_MONTH,
  "|mon|tue|wed|thu|fri|sat|sun)(?:day)?\\b)+")
# ONE trailing token: money or a balance marker printed as its OWN word. `\\b` after `od` is
# satisfied by the last two letters of "period", so the run could start inside a word.
.PDF_TRAIL_TOKEN <- "(?:[$(]?[0-9][0-9,./:-]*[0-9)]?|dr|cr|od)\\b\\)?"
.PDF_TRAIL_MONEY <- paste0("(?:^|(?<=\\s))", .PDF_TRAIL_TOKEN,
                           "(?:\\s+", .PDF_TRAIL_TOKEN, ")*\\s*$")

# One printed line reduced to its label. Only the ENDS are trimmed, since a literal space in a
# label compiles to "\\s+".
.pdf_line_label <- function(s) {
  s <- s %||% ""
  if (length(s) != 1L || is.na(s) || !nzchar(s)) return("")
  s <- tolower(s)
  s <- sub(.PDF_LEAD_DATE,   " ", s, perl = TRUE, useBytes = TRUE)
  s <- sub(.PDF_TRAIL_MONEY, " ", s, perl = TRUE, useBytes = TRUE)
  sub("[\\s:-]+$", "", sub("^\\s+", "", s, perl = TRUE, useBytes = TRUE),
      perl = TRUE, useBytes = TRUE)
}

# BOTH the description cell and the whole raw line are checked, always: reading `raw` only when the
# description band was empty meant a displaced band returning a WRONG description never reached the
# fallback, so "Opening balance" was kept as a transaction.
.pdf_is_summary <- function(description, raw = NULL) {
  rx <- .pdf_summary_rx()
  hit <- function(s) { s <- .pdf_line_label(s)
    nzchar(s) && grepl(rx, s, perl = TRUE, useBytes = TRUE) }
  hit(description) || hit(raw)          # `||` keeps the common path at one reduction
}

.is_footer_noise <- function(s) {
  s <- tolower(trimws(s %||% ""))
  grepl(paste0("^page\\s+\\d+(\\s+of\\s+\\d+)?$",         # "Page 2 of 2"
               "|^\\d+\\s+of\\s+\\d+$",
               "|continued\\s+(on\\s+)?(next|over)",       # "continued on next page"
               "|^statement\\s+(continued|continues)",
               # "Carried Forward to next page" would otherwise fold into the last real transaction.
               # The page words are required: a bare "Carried forward" IS a real summary line.
               "|(carried|brought)\\s+forward\\s+(to|from)\\s+(the\\s+)?",
               "(next|previous|following|preceding|last)\\b"), s)
}

.pdf_real_amount <- function(rec, style) {
  .has_real_money(if (identical(style, "debit_credit_cols"))
    paste(rec$debit %||% "", rec$credit %||% "") else (rec$amount %||% ""))
}

# THE decision "is this visual row a transaction?", in one place, because the X-ray must paint
# exactly the rows the reader keeps. Separate copies drifted.
pdf_keep_row <- function(rec, style, date_ok, date_redacted = FALSE,
                         keep_dateless = FALSE) {
  if (.pdf_is_summary(rec$description, rec$raw)) return(FALSE)
  if (!.pdf_has_amount(rec, style)) return(FALSE)   # must carry an amount slot
  if (isTRUE(date_ok)) return(TRUE)                 # real date + an amount slot
  real_amt <- .pdf_real_amount(rec, style)
  (isTRUE(date_redacted) && real_amt) || (isTRUE(keep_dateless) && real_amt)
}

# A stable CODE ("" means it IS a transaction); the sentence a person reads is the lookup below.
# One string for both meant a reword silently switched off the thin-parse defence.
.pdf_row_code <- function(rec, style, date_ok) {
  has_amt <- .pdf_has_amount(rec, style)
  is_summ <- .pdf_is_summary(rec$description, rec$raw)
  if (isTRUE(date_ok) && has_amt && !is_summ) return("")
  if (is_summ) return("summary_line")
  if (!isTRUE(date_ok) && !has_amt) return("heading_or_note")
  if (!isTRUE(date_ok)) return("date_unparsed")
  "amount_missing"
}

# ACTIONABLE codes: the row looked like a transaction and could not be read (a heading is normal).
.PDF_ACTIONABLE_CODES <- c("date_unparsed", "amount_missing")

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

# Map the skipped row's SENTENCE back through the table that produced it. A loose prose search also
# hit the heading sentence - 52 rows marked "looks like a transaction" against the engine's 9.
pdf_reason_actionable <- function(reason) {
  reason <- as.character(reason %||% character(0))
  code <- names(.PDF_ROW_REASON_TEXT)[match(reason, .PDF_ROW_REASON_TEXT)]
  !is.na(code) & code %in% .PDF_ACTIONABLE_CODES
}

.append_cell <- function(cur, add) {
  txt <- function(v) { v <- as.character(v %||% "")
    if (!length(v) || is.na(v[1])) "" else trimws(v[1]) }
  trimws(paste(txt(cur), txt(add)))
}

.pdf_in_band <- function(cx, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max))
    return(rep(FALSE, length(cx)))
  !is.na(cx) & cx >= cspec$x_min & cx <= cspec$x_max
}

# Module level so the reader and the X-ray agree on which rows the user forced in.
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

parse_pdf_table <- function(input, template, force_rows = NULL, meta = NULL) {
  t <- template$table %||% list()
  cols <- t$columns %||% list()
  extras_cols <- t$extras %||% list()
  region <- t$region %||% list()
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% PARAM_PDF_ROW_TOL)); if (is.na(row_tol)) row_tol <- PARAM_PDF_ROW_TOL
  date_fmt <- t$date_format %||% "%d/%m/%Y"
  style <- t$amount_sign %||% "signed"
  dec <- template$decimal_mark %||% t$decimal_mark %||% "auto"
  udef <- template$unsigned_default %||% t$unsigned_default %||% "debit"
  # tcv NULL is the binary back-compat rule (anything not the debit token is a credit); declaring it
  # makes an unrecognised indicator fail closed to NA instead of being signed a credit.
  tdv <- template$type_debit_value  %||% t$type_debit_value  %||% "D"
  tcv <- template$type_credit_value %||% t$type_credit_value
  words_by_page <- input$words %||% list()
  frame <- pdf_band_frame(template)
  page_w <- input$page_width  %||% rep(NA_real_, length(words_by_page))
  page_h <- input$page_height %||% rep(NA_real_, length(words_by_page))
  words_by_page <- lapply(seq_along(words_by_page), function(p) {
    wp <- words_by_page[[p]]
    if (is.null(wp) || !nrow(wp)) return(wp)
    .words_to_band_frame(as.data.frame(wp, stringsAsFactors = FALSE), frame, page_w[p], page_h[p])
  })
  if (!is.null(force_rows) && length(force_rows)) force_rows <- lapply(force_rows, function(fb) {
    pg <- suppressWarnings(as.integer(fb$page %||% 1L))
    sy <- if (!is.na(pg) && pg >= 1 && pg <= length(page_h))
            pdf_band_frame_scale(frame, page_w[pg], page_h[pg])[2] else 1
    if (!is.null(fb$y_min)) fb$y_min <- fb$y_min * sy
    if (!is.null(fb$y_max)) fb$y_max <- fb$y_max * sy
    fb
  })

  # An overlapping row is KEPT even if its date or amount does not parse, but FLAGGED, so a manually
  # added row is never silently trusted.
  .row_forced <- function(r) .forced_band_hit(r$page, r$.y0, r$.y1, force_rows)

  # Keep only the FIRST date: a band can capture a transaction date AND a processed date, and
  # appending a year to "17 Oct 17 Sep" makes R read the second day as the year.
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
      cc <- c(.cell_minconf(rw, cols$date), .cell_minconf(rw, cols$amount),
              .cell_minconf(rw, cols$balance), .cell_minconf(rw, cols$debit),
              .cell_minconf(rw, cols$credit))
      cc <- cc[!is.na(cc)]
      rec$ocr_minconf <- if (length(cc)) min(cc) else NA_real_
      for (ef in names(extras_cols)) rec[[paste0("x.", ef)]] <- .pdf_cell(rw, extras_cols[[ef]])
      recs[[length(recs) + 1L]] <- rec
    }
  }

  # Year context: many statements print day/month only and put the year in the period, so a
  # year-less format takes it from there.
  md <- meta %||% safe(extract_metadata(input), NULL)
  has_year <- grepl("%[Yy]", date_fmt)
  # Computed ALWAYS: the year-less fallback needs it even when the format carries a year. Implausible
  # years are rejected - as.Date("13 Aug 25", "%d %b %Y") yields 0025 rather than NA.
  pdate <- .plausible_period_date   # shared with the X-ray (R/params.R) so year context can't drift
  p0 <- pdate(md$period_start); p1 <- pdate(md$period_end)
  yrs <- suppressWarnings(as.integer(format(c(p0, p1)[!is.na(c(p0, p1))], "%Y")))
  yrs <- unique(yrs[!is.na(yrs)])
  # With day/month only and no parseable period, take a 4-digit year from the page text, but only
  # when UNAMBIGUOUS - zero or several years means no guess.
  year_from_stmt_date <- FALSE
  if (!length(yrs)) {
    sd <- pdate(md$statement_date)
    if (!is.na(sd)) {
      yrs <- as.integer(format(sd, "%Y")); year_from_stmt_date <- TRUE
    }
  }
  year_from_text <- FALSE
  if (!length(yrs)) {
    alltext <- paste(unlist(input$pages %||% input$text %||% character(0)), collapse = " ")
    cy <- suppressWarnings(as.integer(regmatches(alltext,
            gregexpr("\\b(?:19|20)[0-9]{2}\\b", alltext, perl = TRUE))[[1]]))
    cy <- unique(cy[.plausible_year(cy)])
    if (length(cy) == 1L) { yrs <- cy; year_from_text <- TRUE }
  }
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
  # Year-less FALLBACK for templates whose declared format carries a year: real statements print
  # "17 Sep" while the template says %d/%m/%Y. Rows read this way are flagged date_alt_format.
  .fb_first <- function(cells) vapply(cells, function(cc) {
    if (is.na(cc)) return(NA_character_)
    toks <- strsplit(trimws(cc), "[[:space:]]+")[[1]]
    if (length(toks) >= 2) paste(toks[1:2], collapse = " ") else as.character(cc)
  }, character(1), USE.NAMES = FALSE)
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
  .fb_sentinel_ok <- function(raw) {
    raw <- .fb_first(raw)
    for (f in .FB_FMTS)
      if (!is.na(suppressWarnings(parse_date(paste(raw, "2000"), f)$iso))) return(TRUE)
    FALSE
  }

  # The date cell must parse AND the row carry real money in amount or debit/credit; balance alone is
  # deliberately not enough. Errs toward keeping - a stray summary breaks reconciliation loudly,
  # dropping a real transaction loses money silently.
  .has_amount <- function(r) .pdf_has_amount(r, style)
  .real_amount <- function(r) .pdf_real_amount(r, style)
  .is_summary <- function(r) .pdf_is_summary(r$description, r$raw)
  # With no period and no year anywhere, dropping every row would silently lose a whole statement:
  # keep a dated money line, leave date_iso NA and flag date_unresolved.
  year_resolved <- has_year || length(yrs) > 0
  # The fallback is for statements whose WHOLE table is unreadable under the declared format. If that
  # format reads even ONE date the fallback stays off.
  prim_zero <- length(recs) > 0 && !any(vapply(recs, function(r) {
    rw <- .first_date(r$date %||% NA_character_)
    if (year_resolved)
      !is.na(suppressWarnings(parse_date(full_date(rw), eff_fmt)$iso))
    else
      !is.na(suppressWarnings(parse_date(paste(rw, "2000"), paste(date_fmt, "%Y"))$iso))
  }, logical(1)))
  fb_active <- prim_zero && length(yrs) > 0
  # Pure given the document-level constants above, and called 4-16x per cell, so memoise per parse.
  .dok_cache <- new.env(parent = emptyenv())
  .date_ok <- function(raw) {
    key <- if (length(raw) != 1L || is.na(raw)) "<<NA>>" else raw
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
      prim_zero && .fb_sentinel_ok(raw)
    }
    if (cacheable) assign(key, v, envir = .dok_cache)
    v
  }
  .redacted_cell <- function(v) !is.na(v) && grepl("REDACT", toupper(as.character(v)))
  # KEEP RULE. A row is a transaction when it shows real evidence - a real date OR a real amount -
  # carries an amount slot (real or redacted), and is not a summary line. A whole row blacked out is no
  # row: we do not guess how many transactions a black block hid. An UNPARSEABLE date is not a
  # redaction - the row is dropped and flagged so the template gets fixed.
  keep_dateless <- isTRUE(t$keep_dateless_rows %||% FALSE)
  .is_txn <- function(r)
    pdf_keep_row(r, style, .date_ok(r$date), .redacted_cell(r$date), keep_dateless)

  # Split-row recovery: some statements render one transaction's cells on different baselines, so
  # each half fails the keep test. Fires only on a genuine split, and the stitched row is flagged.
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

  # A wrapped payee spills onto a row with no date and no money: fold its text into the preceding
  # kept transaction rather than dropping verbatim content.
  if (!identical(t$merge_continuation %||% TRUE, FALSE) && length(recs) > 1) {
    is_txn <- vapply(recs, .is_txn, logical(1)) | vapply(recs, .row_forced, logical(1))
    descr_cols <- c("description", "reference", "particulars", "code", "other_party", "type")
    # Not `description`: it is the catch-all for the rest of the line, so folding it separately would
    # put a stray word out of its printed place.
    own_cols <- setdiff(descr_cols, "description")
    # last_y1 chains, so a value can wrap across several lines. A sign marker printed on the line
    # BELOW its amount carries no date and no digits, so it looked like wrapped description text and
    # every payment came out as a debit.
    .sign_markers <- toupper(c(lex("dr_cr_suffix_credit"), lex("dr_cr_suffix_debit")))
    # Does the cell ALREADY carry a marker, so the sign is never flipped twice. A marker glued to a
    # number is still a marker.
    .marker_rx <- sprintf("(^|[^A-Za-z])(%s)\\s*$", paste(.sign_markers, collapse = "|"))
    .money_flds <- intersect(c("amount", "debit", "credit", "balance"), names(cols))
    .marker_only <- function(r) {
      tk <- toupper(trimws(as.character(r$.txt %||% character(0))))
      tk <- tk[nzchar(tk)]
      length(tk) > 0L && all(tk %in% .sign_markers)
    }
    last_txn <- 0L; last_y1 <- -Inf; drop <- logical(length(recs))
    for (i in seq_along(recs)) {
      if (is_txn[i]) { last_txn <- i; last_y1 <- recs[[i]]$.y1; next }
      if (last_txn == 0L) next
      r <- recs[[i]]; prev <- recs[[last_txn]]
      line_txt <- r$raw %||% ""
      line_txt <- if (is.na(line_txt)) "" else trimws(line_txt)   # nzchar(NA) is TRUE -> guard it
      money_here <- .has_amount(r) || .has_money(r$balance %||% "")
      lh <- if (is.finite(r$.h) && r$.h > 0) r$.h else 10
      close <- identical(r$page, prev$page) &&
               is.finite(r$.y0) && is.finite(last_y1) &&
               (r$.y0 - last_y1) <= 0.9 * lh && (r$.y0 - last_y1) >= -lh
      # Resolved BEFORE the description merge, which cannot tell it from wrapped text, and appended to
      # the money cell it sits under. Guarded three ways: the cell must already hold a figure, must not
      # already carry a marker, and the marker must sit in that column's band.
      if (close && .marker_only(r)) {
        moved <- FALSE
        for (fld in .money_flds) {
          hit <- .pdf_in_band(r$.cx, cols[[fld]])
          if (!any(hit)) next
          cur <- recs[[last_txn]][[fld]] %||% NA_character_
          if (is.na(cur) || !grepl("[0-9]", cur)) next
          if (grepl(.marker_rx, toupper(trimws(cur)))) next
          recs[[last_txn]][[fld]] <- paste(trimws(cur), paste(r$.txt[hit], collapse = " "))
          moved <- TRUE
        }
        if (moved) {
          drop[i] <- TRUE
          if (is.finite(r$.y1)) last_y1 <- max(last_y1, r$.y1)
          next
        }
      }
      if (nzchar(line_txt) && !money_here && !.date_ok(r$date) && !.redacted_cell(r$date) &&
          !.is_summary(r) && !.is_footer_noise(line_txt) && close) {
        # NOTHING on a merged line may be dropped: folding only the words a mapped descriptive
        # NOTHING on a merged line may be dropped: folding only the words a mapped band claimed turned
        # "PAYMENT TO JOHN" + "SMITH" + "HOLDINGS LTD" into a different legal entity.
        own_hit <- rep(FALSE, length(r$.cx %||% numeric(0)))
        for (fld in own_cols) {
          add <- r[[fld]] %||% NA_character_
          if (!is.na(add) && nzchar(trimws(add))) {
            recs[[last_txn]][[fld]] <- .append_cell(recs[[last_txn]][[fld]], add)
            own_hit <- own_hit | .pdf_in_band(r$.cx, cols[[fld]])
          }
        }
        rest <- if (length(own_hit)) trimws(paste(r$.txt[!own_hit], collapse = " ")) else line_txt
        if (nzchar(rest))
          recs[[last_txn]]$description <- .append_cell(recs[[last_txn]]$description, rest)
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

  # Redaction is deliberately allowed to make a row LOOK like a transaction: silently dropping a real
  # row whose amount was blacked out is worse. The cost - a box over an off-table fee line adding a
  # spurious row - stays VISIBLE: flagged, in the X-ray, and row_count differs from stated_count.
  forced_vec <- vapply(recs, .row_forced, logical(1))
  keep <- vapply(recs, .is_txn, logical(1)) | forced_vec
  # candidate_rows is every visual row still standing at the keep decision; skipped_rows is how many
  # the keep rule rejected. NOT a count of losses - never subtract it like a source line count.
  candidate_rows <- length(recs)
  skipped_rows <- sum(!keep)
  # Counted here rather than in row_coverage(), which would re-parse the whole PDF.
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
        # The indicator column and its tokens must reach parse_amount exactly as the delimited
        # path builds them. Omitting them left every row in the back-compat branch, so every debit
        # became a credit with the "D" still printing beside it.
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

  # The AMOUNT itself was hidden, so the value is genuinely unknown and is nulled. `redacted` is
  # broader: a row whose DATE was redacted still keeps its real amount.
  amt_redacted <- if (n == 0) logical(0) else if (identical(style, "debit_credit_cols"))
    grepl("REDACTED", getc("debit"), ignore.case = TRUE) |
    grepl("REDACTED", getc("credit"), ignore.case = TRUE)
  else grepl("REDACTED", getc("amount"), ignore.case = TRUE)
  redacted <- if (n == 0) logical(0) else
    (amt_redacted | grepl("REDACT", getc("raw"), ignore.case = TRUE))
  malformed <- if (n == 0) logical(0) else (is.na(amt$value) & !redacted)
  date_unresolved <- if (n == 0) logical(0)
    else ((!year_resolved & !is.na(date_raw) & nzchar(trimws(date_raw))) |
          (forced_vec & is.na(date_iso)))
  # A year-less table with no period took its year from a single 4-digit number in page text, so
  # those rows are flagged PER ROW and trust never treats that year as proven.
  date_year_inferred <- if (n == 0) logical(0)
    else (year_from_text & !is.na(date_iso) & (!has_year | fb_used))
  no_date_kept <- if (n == 0) logical(0)
    else (keep_dateless & is.na(date_iso) &
          (is.na(date_raw) | !nzchar(trimws(date_raw))) & !forced_vec)
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
    if (identical(style, "debit_credit_cols")) {
      if (is.null(ex$debit))  ex$debit  <- blank_to_na(getc("debit"))
      if (is.null(ex$credit)) ex$credit <- blank_to_na(getc("credit"))
    }
    extras <- data.frame(ex, stringsAsFactors = FALSE, check.names = FALSE)
  } else extras <- data.frame(row_id = integer(0))

  .money_num <- function(x) .num(x %||% NA_character_, dec)
  # User-drawn boxes that PIN a header value when the dictionary cannot find its wording. A box wins
  # ONLY when it yields a value, so a correctly-read value is never overwritten with nothing.
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
    pdf_doc = input$meta$pdf_doc,
    ocr_pages = input$meta$ocr_pages %||% 0L,
    ocr_min_confidence = input$meta$ocr_min_conf %||% NA_real_,
    redaction_scan_incomplete = input$meta$redaction_scan_incomplete %||% 0L)

  pages_v <- if (n == 0) integer(0) else vapply(recs, function(r) as.integer(r$page), integer(1))
  provenance <- data.frame(row_id = seq_len(n),
    source_ref = if (n == 0) character(0) else sprintf("pdf:p%d", pages_v),
    raw = getc("raw"), stringsAsFactors = FALSE)

  # source_line_count stays NA on purpose: a PDF has no physical data line count to difference
  # against the parsed rows, so reconcile must not compute a loss figure here.
  list(transactions = core, extras = extras, header = header,
       provenance = provenance, source_line_count = NA_integer_,
       visual_row_count = as.integer(candidate_rows),
       skipped_row_count = as.integer(skipped_rows),
       actionable_skip_count = as.integer(actionable_skips))
}
