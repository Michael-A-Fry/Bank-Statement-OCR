# column_fit.R -- "your template still matches, but a column is not where it says,
# and here is which one."
#
# THE FAILURE THIS ANSWERS is the commonest one a working template hits. A template
# is built, it reads that bank perfectly for months, and then the layout shifts at
# the next statement run. The identifying wording has not changed, so the template
# still MATCHES; the x-bands have not moved, so the figures come out of the wrong
# places. The analyst gets "needs review" and no idea why.
#
# MEASURED, and the figures are not merely absent -- they are WRONG. On the synthetic
# corpus (tools/synth/, case drift_amounts_left_55pt) a 55pt shift put the BALANCE
# inside the debit band: 10 of 20 rows came back with the running balance reported as
# the transaction amount. 6781.40 where the statement said -102.54. The arithmetic
# catches the run (trust low, continuity fails), so nothing wrong is published -- but
# "something is wrong with this statement" is a long way from "your balance column is
# empty on every row".
#
# 25pt DID NOT BREAK IT, which is worth writing down: the bands are 65-77pt wide, so
# a shift of a third of a column is absorbed without complaint. This is for the shift
# that is not absorbed.
#
# WHAT IT DOES NOT DO. It never moves a band by itself. A template is evidence of how
# a statement was read, and a reader that silently re-aimed its own columns would make
# two runs of the same file on the same template incomparable. It reports; a person
# decides. Same rule as everything else here: the tool may refuse, and it may explain,
# but it does not guess.
#
# ---------------------------------------------------------------------------
# WHY ONE PAGE-WIDE OFFSET AND NOT ONE PER COLUMN -- the measurement that set the
# design, because the obvious approach is wrong in two ways at once.
#
# 1. A PER-COLUMN OFFSET IS NOT IDENTIFIABLE. An amount is a single point, and the
#    band is 65pt wide, so on drift_amounts_left_55pt the debit band read its amount
#    for EVERY offset from -69 to -4: 65 points of plateau, all scoring 10 of 10.
#    Nothing in "is it in the band" distinguishes -4 from -55. Reporting any single
#    one of them as "the" offset is a guess dressed as a measurement.
#
#    And the generator's own -55 is not even the best member: at -55 the amount sits
#    14pt from a band edge, at the plateau's CENTRE (-37) it sits 32pt from both. So
#    the honest answer is the whole interval, and the best recommendation is its
#    middle -- not the number the page was actually drawn with.
#
# 2. INDEPENDENT SEARCHES FIGHT EACH OTHER. On the same case, credit's best-scoring
#    offset was +15, reading 20 of 20 -- because +15 slid the credit band onto the
#    BALANCE column. Two bands claiming one number is physically impossible, and a
#    per-column search cannot see it. x-BANDS ARE A PARTITION (see THE BAND FRAME in
#    parse_pdf_table.R): gapless, non-overlapping, every word to the band holding its
#    centre. SHIFTING THEM ALL BY ONE dx PRESERVES THAT, so a page-wide offset cannot
#    double-claim a token and needs no overlap logic at all. The constraint comes free
#    from the geometry instead of from a rule somebody has to maintain.
#
# A column that truly moved ALONE is still named -- by its own verdict (`empty`,
# `wrong content`, `mostly empty`), which needs no offset to be useful. If no page
# offset reads the statement better, none is offered. That is the honest outcome, not
# a gap.

# How many data rows before this will say anything at all. Four is the floor at which
# a two-thirds majority means something; below it every answer is noise.
.CFIT_MIN_ROWS <- 4L

# ...and how many PAGES it will look at, spread evenly across the document.
#
# MEASURED: on a 100-page statement this check was 23.0 of the 49.8 seconds a whole
# conversion took -- 46%, the single largest stage, larger than reading the PDF. The
# cost is linear in pages and the INFORMATION is not: a column either sits where the
# template says or it does not, and eight pages spread through the document answer
# that as well as a hundred. The cap makes the check O(1) in document length.
#
# EVENLY SPREAD, and never just the first eight: a bank that re-ran its composition
# halfway through a bundle moves the columns from that point on, and a sample taken
# off the front would report a clean fit for a document that stops fitting on page 50.
# The first and last page are always included.
#
# WHAT THE CAP GIVES UP, stated plainly: a drift confined to a few pages in the middle
# can fall between the samples. The figures are still safe -- the running balance
# breaks across those rows and trust goes low -- so what is lost is the EXPLANATION,
# not the protection. A statement whose layout changes partway through is also a
# bundle rather than one statement, which R/split.R is the right answer to.
.CFIT_MAX_PAGES <- 8L

# Of the rows where a band holds anything, the fraction that must hold the right KIND
# of thing for the band to be called healthy.
.CFIT_OK_RATE <- 0.9

# A candidate page offset has to read nearly everything it touches. An offset that
# picks up 40 cells and misreads 4 of them is not a better place for the bands; it is
# a different wrong place.
.CFIT_SHIFT_RATE <- 0.95

# .cfit_* , not .col_* : R/column_profile.R already owns .col_kind and .looks_money,
# and they mean something different there -- it INFERS a kind from a column of data,
# this reads the kind a TEMPLATE declares. Two functions with one name is the kind of
# collision that is silent until it is a wrong figure.
#
# .cfit_kind(name, spec) -- what KIND of thing should be in this column, from the
# template's own words.
.cfit_kind <- function(name, spec) {
  t <- tolower(as.character(spec$type %||% "")[1])
  if (t %in% c("money", "amount", "numeric")) return("money")
  if (t %in% c("date")) return("date")
  if (name %in% c("debit", "credit", "balance", "amount", "fee")) return("money")
  if (name %in% c("date", "value_date", "posted")) return("date")
  "text"
}

# .cfit_money(x) -- one cell, is it a money value? Deliberately via .num(), the
# engine's own reader, so "money" here means exactly what it means everywhere else:
# a cell contaminated by a description word is NOT money (see .money_contaminated in
# R/normalise.R), which is the whole point -- a band full of description text must
# not be scored as a healthy money column.
.cfit_money <- function(x, dec = "auto") {
  v <- suppressWarnings(.num(x, dec))
  !is.na(v)
}

# .cfit_amountish(tok) -- is this token an amount AS A STATEMENT PRINTS ONE, i.e. with
# cents? Used ONLY to recognise a data row, and it has to be stricter than .cfit_money
# for one measured reason: "Statement period 1 Feb 2026 to 13 Feb 2026" has dates AND
# numbers, so it was being scored as a transaction row whose date band held the word
# "Statement" -- the last false alarm on a flawless statement. The year 2026 parses as
# money; it is not an amount. Being too strict here is safe: fewer data rows means the
# check falls below its evidence floor and says nothing, which is the right way to fail.
.cfit_amountish <- function(tok, dec = "auto") {
  t <- trimws(as.character(tok))
  grepl("[.,][0-9]{2}[^0-9]*$", t) && .cfit_money(t, dec)
}

# .cfit_rate(cells, kind, dec, dfmt) -- of the rows where this band held anything, how
# many held the right KIND of thing? rate is NA when the band was empty on every row,
# which is a different finding from "held the wrong thing" and is reported as such.
.cfit_rate <- function(cells, kind, dec = "auto", dfmt = character(0),
                       money_fn = NULL) {
  seen <- !is.na(cells) & nzchar(trimws(cells))
  if (!any(seen)) return(list(seen = 0L, ok = 0L, rate = NA_real_))
  txt <- cells[seen]
  if (is.null(money_fn)) money_fn <- function(x) .cfit_money(x, dec)
  # A DATE is tested with the template's OWN declared format, through the engine's own
  # parse_date -- never a regex invented here, or a column could be reported as
  # healthy that the reader cannot actually read. No declared format means the kind
  # cannot be checked, and an unverifiable column is reported as FITTING rather than
  # broken: a false alarm on every statement is worse than no alarm.
  ok <- switch(kind,
    money = vapply(txt, money_fn, logical(1)),
    date  = if (length(dfmt) && nzchar(dfmt[1]))
              !is.na(suppressWarnings(parse_date(txt, dfmt)$iso))
            else rep(TRUE, length(txt)),
    rep(TRUE, length(txt)))
  # DESCRIPTION OVERSPILL IS NOT A MISREAD AMOUNT. A money band holding words and no
  # figure has not lost anything -- the row's amount is in another column, where it
  # belongs. A money band holding a FIGURE it cannot parse has.
  #
  # Measured, and this is the distinction that separates a real fault from noise:
  #
  #   band_overflow_debit_only -- the description overruns the debit band on all 20
  #     rows ("...CONVENIENCE"), and the engine read 20 of 20 amounts correctly, 0
  #     refused. Telling that analyst to go and edit a template is noise on a flawless
  #     conversion.
  #   band_narrow_desc -- the description overruns EVERY numeric band and drags the
  #     amounts in with it ("...BRANCH 596.08 0114"), no money column read anything,
  #     and the engine refused all 20 amounts. Here it is the whole problem.
  #
  # AND NOT "the row's amount was read by some other money column", which was tried
  # first and is wrong: on a drifted statement the balance read as the transaction
  # amount satisfies that test, so the test forgave the exact fault it exists to find.
  # The question has to be about THIS cell.
  if (identical(kind, "money"))
    ok <- ok | !vapply(txt, function(z)
      any(vapply(strsplit(z, "[[:space:]]+")[[1]], .cfit_amountish, logical(1),
                 dec = dec)), logical(1))
  list(seen = sum(seen), ok = sum(ok), rate = mean(ok))
}

# .cfit_rows(input, template) -- every visual row on every page that looks like a
# TRANSACTION, in the band frame, as (text, x, centre-x).
#
# Grouped with the ENGINE'S OWN .group_rows at the template's own row_tol, so a row
# here is the same thing a row is to the reader, and read in the same coordinate frame
# the reader uses -- otherwise every offset reported would be measured against a page
# the parser never saw (see THE BAND FRAME, parse_pdf_table.R).
#
# ONLY DATA ROWS ARE SCORED, which is the difference between a useful answer and four
# false alarms on a perfect statement. The first version measured every visual row, so
# the heading row ("Date", "Withdrawals"), the bank's name, and the opening- and
# closing-balance lines were all counted as rows where the date column held something
# that was not a date and the money columns held nothing. On a flawless 6-row
# statement it reported three columns as holding the wrong thing and a fourth as
# moved. Unusable.
#
# A data row has a DATE **and** an AMOUNT. The statement-period line has dates and no
# amount; a closing-balance line has an amount and no date; the heading row has
# neither. The date is looked for ANYWHERE in the row, not in the date band -- because
# the band is the thing under suspicion, and a detector that assumed the date column
# was right could not report that the date column had moved.
.cfit_rows <- function(input, template, frame, date_fmt, dec, row_tol) {
  wbp <- input$words %||% list()
  pw <- input$page_width %||% rep(NA_real_, length(wbp))
  ph <- input$page_height %||% rep(NA_real_, length(wbp))
  rows <- list()
  np <- length(wbp)
  look <- if (np <= .CFIT_MAX_PAGES) seq_len(np) else
    unique(c(1L, round(seq(1, np, length.out = .CFIT_MAX_PAGES)), np))
  for (p in look) {
    w <- wbp[[p]]
    if (is.null(w) || !nrow(w)) next
    w <- .words_to_band_frame(as.data.frame(w, stringsAsFactors = FALSE), frame, pw[p], ph[p])
    w <- w[order(w$y), , drop = FALSE]
    g <- .group_rows(w$y, row_tol)
    for (k in unique(g)) {
      sel <- g == k
      o <- order(w$x[sel])
      rows[[length(rows) + 1L]] <- list(cx = (w$x[sel] + w$width[sel] / 2)[o],
                                        text = w$text[sel][o])
    }
  }
  # NO DECLARED DATE FORMAT, NO ANSWER. Without knowing what a date looks like on this
  # bank's statements there is no way to tell a transaction row from a heading, and
  # scoring headings is precisely what produced four false alarms on a flawless
  # statement. Silence is the honest outcome.
  if (!length(rows) || !length(date_fmt) || !nzchar(date_fmt[1])) return(list())
  is_data <- vapply(rows, function(r) {
    toks <- r$text[nzchar(trimws(r$text))]
    if (!length(toks)) return(FALSE)
    # neighbouring pairs too, because a date is often two tokens ("03 Feb")
    cand <- c(toks, if (length(toks) > 1) paste(toks[-length(toks)], toks[-1]))
    any(!is.na(suppressWarnings(parse_date(cand, date_fmt)$iso))) &&
      any(vapply(toks, .cfit_amountish, logical(1), dec = dec))
  }, logical(1))
  # AND NO FALLBACK TO "SCORE EVERY VISUAL ROW" when too few look like transactions.
  # That fallback was in the first version and it is the same bug wearing a hat: on a
  # statement whose dates the template cannot read, it fell back to scoring the bank's
  # name and the column headings and reported every column as broken.
  if (sum(is_data) >= .CFIT_MIN_ROWS) rows[is_data] else list()
}

# column_fit(input, template, search_pt, step_pt) -> list:
#
#   $rows     how many data rows were scored
#   $columns  one row per declared column:
#               column        the template's own name for it
#               kind          money / date / text -- what it should hold
#               x_min, x_max  where the template says it is
#               rows_seen     data rows where that band held anything at all
#               rows_ok       of those, how many held the right kind
#               rate          rows_ok / rows_seen, NA when the band was empty throughout
#               shift_seen    rows it would fill at $shift$dx (= rows_seen when none)
#               shift_ok      of those, how many would hold the right kind
#               verdict       "fits" | "wrong content" | "mostly empty" | "empty"
#   $strays   data rows carrying an amount that no MONEY column covers
#   $shift    NULL when the bands are where the amounts are, else
#               dx        the recommended offset: the CENTRE of the interval that works
#               lo, hi    the whole interval that works -- the honest answer (see above)
#               ok, seen  money cells read correctly at dx, out of those touched
#               base_ok, base_seen  the same where the template says the bands are
#
# Returns $rows = 0 and a 0-row frame for anything that is not a PDF template, or for
# a page with too few data rows to say anything, so a caller can run it unconditionally.
column_fit <- function(input, template, search_pt = 80, step_pt = 1) {
  none <- list(rows = 0L, shift = NULL, strays = 0L, columns = data.frame(
    column = character(0), kind = character(0), x_min = numeric(0), x_max = numeric(0),
    rows_seen = integer(0), rows_ok = integer(0), rate = numeric(0),
    shift_seen = integer(0), shift_ok = integer(0),
    verdict = character(0), stringsAsFactors = FALSE))

  t <- template$table %||% list()
  cols <- t$columns %||% list()
  if (!identical(template$format %||% "delimited", "pdf") || !length(cols)) return(none)
  if (!length(input$words %||% list())) return(none)
  dec <- t$decimal_mark %||% template$decimal_mark %||% "auto"
  date_fmt <- as.character(t$date_format %||% character(0))
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% PARAM_PDF_ROW_TOL))
  if (!is.finite(row_tol) || row_tol <= 0) row_tol <- PARAM_PDF_ROW_TOL

  rows <- .cfit_rows(input, template, pdf_band_frame(template), date_fmt, dec, row_tol)
  # NOT ENOUGH EVIDENCE IS NOT A FINDING. Under a handful of data rows any answer is
  # noise, and a noisy answer about a template is worse than silence: it teaches an
  # analyst to ignore the one that matters.
  if (length(rows) < .CFIT_MIN_ROWS) return(none)

  spec <- lapply(names(cols), function(nm) {
    s <- cols[[nm]]
    x0 <- suppressWarnings(as.numeric(s$x_min)); x1 <- suppressWarnings(as.numeric(s$x_max))
    if (!is.finite(x0) || !is.finite(x1) || x1 <= x0) return(NULL)
    list(name = nm, x0 = x0, x1 = x1, kind = .cfit_kind(nm, s))
  })
  spec <- spec[!vapply(spec, is.null, logical(1))]
  if (!length(spec)) return(none)

  # the text each row puts in a band -- the reader's own rule, a word joins the band
  # holding its horizontal CENTRE
  cell_at <- function(r, x0, x1) {
    sel <- which(r$cx >= x0 & r$cx <= x1)
    if (!length(sel)) return(NA_character_)
    paste(r$text[sel], collapse = " ")
  }
  # MEMOISED ON THE CELL TEXT, because the offset search asks the same question over
  # and over: an amount sits in a 65pt band for 65 consecutive offsets, so the same
  # cell string is parsed ~65 times. .num() is the engine's own money reader and it is
  # regex-heavy. Measured: 161 offsets x 3 columns x 260 rows is 125,580 parses
  # without this.
  mcache <- new.env(parent = emptyenv(), hash = TRUE)
  money_ok <- function(cell) {
    hit <- mcache[[cell]]
    if (!is.null(hit)) return(hit)
    v <- .cfit_money(cell, dec)
    assign(cell, v, envir = mcache)
    v
  }
  rate_at <- function(s, dx = 0) .cfit_rate(
    vapply(rows, cell_at, character(1), x0 = s$x0 + dx, x1 = s$x1 + dx),
    s$kind, dec, date_fmt, money_ok)

  here <- lapply(spec, rate_at)

  # ---- AMOUNTS NO MONEY COLUMN COVERS --------------------------------------
  # A second, independent question, and the one that tells two identical-looking
  # findings apart. "balance is empty on every row" means either that this bank does
  # not print a running balance -- true of plenty of statements, nothing to fix -- or
  # that the balance column MOVED, which is a template fault. The columns alone cannot
  # say which. An amount that no MONEY column covers can: it is a figure on the page
  # that the template has nowhere to put.
  #
  # OUTSIDE THE MONEY BANDS, NOT OUTSIDE ALL OF THEM, and the difference was measured.
  # On mustflag_drift_amounts_55pt the debit figures drifted into the DESCRIPTION band
  # -- inside a declared column, so the first version of this test saw nothing and the
  # case went unreported while 14 amounts came back fabricated. An amount sitting in a
  # description column is every bit as much a sign of a misaligned template as one off
  # the edge of the table.
  mxk <- vapply(spec, function(s) identical(s$kind, "money"), logical(1))
  mx0 <- vapply(spec[mxk], function(s) s$x0, numeric(1))
  mx1 <- vapply(spec[mxk], function(s) s$x1, numeric(1))
  strays <- if (!length(mx0)) 0L else sum(vapply(rows, function(r) {
    if (!length(r$text)) return(FALSE)
    m <- vapply(r$text, .cfit_amountish, logical(1), dec = dec)
    if (!any(m)) return(FALSE)
    any(vapply(r$cx[m], function(cx) !any(cx >= mx0 & cx <= mx1), logical(1)))
  }, logical(1)))

  # ---- WHERE ARE THE AMOUNTS, IF NOT THERE? -------------------------------
  # One offset applied to every money band at once. See the header for why this is
  # the only form of the question that has an answer.
  money <- which(vapply(spec, function(s) identical(s$kind, "money"), logical(1)))
  shift <- NULL
  shift_cols <- here
  if (length(money)) {
    base_seen <- sum(vapply(here[money], function(q) as.integer(q$seen), integer(1)))
    base_ok   <- sum(vapply(here[money], function(q) as.integer(q$ok), integer(1)))
    # DO NOT GO LOOKING WHEN THE DECLARED BANDS ALREADY READ CLEANLY -- and ask that
    # FIRST, before any offset is scored. Measured: asking it after building the
    # tally cost 43 SECONDS on a 260-row statement that needed no search at all,
    # which would have been 43 seconds added to every large conversion. The whole
    # point of the test is that the common case is free.
    if (base_seen > 0 && base_ok >= .CFIT_SHIFT_RATE * base_seen)
      return(.cfit_result(rows, spec, here, here, NULL, strays))
    dxs <- seq(-search_pt, search_pt, by = step_pt)
    tally <- lapply(dxs, function(dx) {
      z <- lapply(spec[money], rate_at, dx = dx)
      list(dx = dx, per = z,
           seen = sum(vapply(z, function(q) as.integer(q$seen), integer(1))),
           ok   = sum(vapply(z, function(q) as.integer(q$ok), integer(1))))
    })
    seen <- vapply(tally, function(z) z$seen, integer(1))
    ok   <- vapply(tally, function(z) z$ok, integer(1))
    rate <- ifelse(seen > 0, ok / seen, NA_real_)
    # an offset is a candidate only if it reads nearly everything it touches. The
    # reason that bar exists, measured: on baseline_2page, a flawless statement
    # reading 80 of 80, shifting the bands 12pt left read 86 of 86 -- six MORE cells,
    # all of them correct money, because the wider reach had swallowed six reference
    # numbers out of the description. "Reads more" is not "reads better".
    elig <- !is.na(rate) & rate >= .CFIT_SHIFT_RATE
    # RATE BEFORE COUNT, for the same reason. Measured: on drift_amounts_left_55pt the
    # highest count was 41 of 42 at one single offset, which beat the 40 of 40 that the
    # whole interval -68..-4 reads -- the 41st cell was junk and the 42nd was the
    # misread it came with. Perfect-but-smaller is the right placement.
    top_rate <- if (any(elig)) max(rate[elig]) else -1
    elig <- elig & rate >= top_rate - 1e-9
    top <- if (any(elig)) max(ok[elig]) else -1L
    # A FINDING NEEDS A MATERIAL GAIN, not a better-by-one. .CFIT_MIN_ROWS more cells
    # read correctly is the same evidence floor as everywhere else here.
    if (top >= base_ok + .CFIT_MIN_ROWS) {
      win <- which(elig & ok == top)
      # THE INTERVAL, not a point: all the winning offsets form a plateau because an
      # amount is a point inside a 65pt band. Take the run nearest zero and recommend
      # its CENTRE, which is the placement with the most slack on both sides.
      near <- win[which.min(abs(dxs[win]))]
      run <- near
      while (length(run) && (min(run) - 1L) %in% win) run <- c(min(run) - 1L, run)
      while (length(run) && (max(run) + 1L) %in% win) run <- c(run, max(run) + 1L)
      lo <- dxs[min(run)]; hi <- dxs[max(run)]
      pick <- which.min(abs(dxs - (lo + hi) / 2))
      shift <- list(dx = dxs[pick], lo = lo, hi = hi,
                    ok = ok[pick], seen = seen[pick],
                    base_ok = base_ok, base_seen = base_seen)
      shift_cols[money] <- tally[[pick]]$per
    }
  }

  .cfit_result(rows, spec, here, shift_cols, shift, strays)
}

# .cfit_result(...) -- the verdicts, in the order the questions actually matter, and
# the frame they go in. Separate from column_fit only so the "already reads cleanly"
# early return cannot drift out of step with the full path.
.cfit_result <- function(rows, spec, here, shift_cols, shift, strays = 0L) {
  out <- lapply(seq_along(spec), function(i) {
    s <- spec[[i]]; h <- here[[i]]; a <- shift_cols[[i]]
    verdict <-
      # nothing there on any data row. For a declared balance column this also says
      # the arithmetic verifier had nothing to check, which matters more than it looks.
      if (identical(as.integer(h$seen), 0L)) "empty"
      # something is there and it is not what this column is for
      else if (is.na(h$rate) || h$rate < .CFIT_OK_RATE) "wrong content"
      # it reads cleanly, but the offset search found far more of this column
      # elsewhere. This is the drifted-balance shape: 1 row where the template says,
      # 20 just to the left. Corroborated by name -- no coverage threshold to tune,
      # which matters because a credit column legitimately fills 2 rows in 20.
      else if (as.integer(a$seen) >= max(.CFIT_MIN_ROWS, 2L * as.integer(h$seen)))
        "mostly empty"
      else "fits"
    data.frame(column = s$name, kind = s$kind, x_min = s$x0, x_max = s$x1,
               rows_seen = as.integer(h$seen), rows_ok = as.integer(h$ok),
               rate = h$rate,
               shift_seen = as.integer(a$seen), shift_ok = as.integer(a$ok),
               verdict = verdict, stringsAsFactors = FALSE)
  })
  list(rows = length(rows), shift = shift, strays = as.integer(strays),
       columns = do.call(rbind, out))
}

# column_fit_note(fit) -> one sentence for a person, or NA when nothing is wrong.
#
# THE WORDING IS THE WHOLE VALUE. "Check your template" is what the tool could already
# imply; "your balance column is empty on 19 of 20 rows, and every amount column reads
# correctly 7pt to the left" is a thing somebody can act on in ten seconds.
column_fit_note <- function(fit) {
  if (!is.list(fit) || !is.data.frame(fit$columns %||% NULL) || !nrow(fit$columns))
    return(NA_character_)
  f <- fit$columns
  bad <- f[!f$verdict %in% "fits", , drop = FALSE]
  stray <- as.integer(fit$strays %||% 0L)
  if (!nrow(bad) && is.null(fit$shift) && stray < .CFIT_MIN_ROWS) return(NA_character_)
  part <- character(0)
  for (i in seq_len(nrow(bad))) {
    r <- bad[i, ]
    part <- c(part, switch(r$verdict,
      "empty" = sprintf("%s is empty on every row", r$column),
      "mostly empty" = sprintf("%s is empty on %d of the %d rows",
                               r$column, fit$rows - r$rows_seen, fit$rows),
      sprintf("%s holds %s on %d of the %d rows that had anything in it",
              r$column,
              if (identical(r$kind, "money")) "something that is not an amount"
              else sprintf("something that is not a %s", r$kind),
              r$rows_seen - r$rows_ok, r$rows_seen)))
  }
  if (stray >= .CFIT_MIN_ROWS)
    part <- c(part, sprintf("%d of the %d rows carry an amount that no amount column of the template covers",
                            stray, fit$rows))
  if (!is.null(fit$shift)) {
    s <- fit$shift
    part <- c(part, sprintf(
      "every amount column reads correctly with the bands %.0fpt to the %s (anything from %.0f to %.0fpt works): %d of %d amounts there, %d of %d where the template says",
      abs(s$dx), if (s$dx < 0) "left" else "right",
      min(abs(c(s$lo, s$hi))), max(abs(c(s$lo, s$hi))),
      s$ok, s$seen, s$base_ok, s$base_seen))
  }
  paste(part, collapse = "; ")
}
