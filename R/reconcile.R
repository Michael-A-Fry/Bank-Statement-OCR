# reconcile.R -- reconciliation KPIs + deterministic trust mapping.
#
# SHAPE OF THIS FILE, and where to add things:
#   * one `.kpi_*()` builder per check. Each takes only what it needs, and returns
#     ONE .kpi() row -- or NULL when the check does not apply to this statement
#     (`amount_direction`, `ocr_confidence` and `redaction_scan` are only raised
#     when there is something to say).
#   * `.reconcile_trust()` turns the finished KPI table into the trust level,
#     score and reasons.
#   * `reconcile()` is the list of checks, in report order, and nothing else.
#
# TO ADD A KPI: write a `.kpi_<name>()` builder below, then add one line to the
# list in reconcile(). Then give it a plain-English label in `CHECK_PLAIN`
# (ui_labels.R) and, if it can FAIL, a how-to-fix entry in build_diagnostics()
# (R/diagnose.R, section 1) -- a failing check with no fix text falls back to a
# generic "review this against the source statement".

# A CHECK THAT DID NOT RUN SHOWS NO FIGURES.
#
# The Checks table prints Expected and Read beside the verdict, so a row could
# read "No row failed to read | could not be checked | Expected 79 | Read 79" --
# two numbers that agree, under a result saying nothing was proved. On a clean
# CSV the identical "7 | 7" IS the proof, so the figures cannot tell a reviewer
# which of the two she is looking at, and 79 = 79 beside a check that never ran
# reads as a pass. balance_reconciliation already gets this right by passing no
# figures on its "na" path; enforcing it HERE rather than in each builder means
# it holds for every check, including the next one written.
#
# INFORMATIONAL rows are exempt: their `actual` is the fact they exist to report
# (redacted row count, OCR confidence), the table gives them their own word
# rather than "could not be checked", and app.R reads the redaction count off it.
.kpi <- function(name, status, expected = NA, actual = NA,
                 discrepancy = NA, detail = "", informational = FALSE) {
  if (identical(status, "na") && !isTRUE(informational)) {
    expected <- NA; actual <- NA; discrepancy <- NA
  }
  data.frame(name = name, status = status,
             expected = as.character(expected), actual = as.character(actual),
             discrepancy = as.character(discrepancy), detail = detail,
             informational = informational, stringsAsFactors = FALSE)
}

# .row_list(ids, max_show) -- "row 4", "rows 4, 9 and 12", "rows 4, 9, 12 ... (+3 more)".
# A KPI detail has to POINT AT the offending rows or the reviewer cannot act on it,
# but it must stay readable when a whole column failed to parse, hence the cap.
.row_list <- function(ids, max_show = 10L) {
  ids <- suppressWarnings(as.integer(ids)); ids <- ids[!is.na(ids)]
  if (!length(ids)) return("no row identified")
  extra <- length(ids) - max_show
  shown <- paste(utils::head(ids, max_show), collapse = ", ")
  lbl <- if (length(ids) == 1L) "row " else "rows "
  if (extra > 0) sprintf("%s%s and %d more", lbl, shown, extra) else paste0(lbl, shown)
}

# .header_ocr(h) -- the two OCR figures off the header, read ONCE the same way by
# the ocr_confidence KPI and by the trust cap below, so they cannot disagree about
# whether this statement was machine-read. -> list(pages, conf).
.header_ocr <- function(h) {
  pages <- suppressWarnings(as.integer(h$ocr_pages %||% 0L))
  if (is.na(pages)) pages <- 0L
  list(pages = pages, conf = suppressWarnings(as.numeric(h$ocr_min_confidence %||% NA)))
}

# ---- the checks -------------------------------------------------------------

# 1. balance_reconciliation: opening + sum(amount) == closing.
.kpi_balance_reconciliation <- function(tx, h, n) {
  opening <- suppressWarnings(as.numeric(h$opening_balance %||% NA))
  closing <- suppressWarnings(as.numeric(h$closing_balance %||% NA))
  # Many statements don't PRINT a labelled opening/closing balance -- the figure
  # is simply the first/last running balance. Rather than report "na" when the
  # info is plainly there, DERIVE the MISSING one from the running-balance column
  # (closing = last balance; opening = first balance - first amount) -- but ONLY
  # when the OTHER balance is labelled, so at least one INDEPENDENT anchor remains.
  # (Deriving BOTH would just re-check the balance column's endpoints and could
  # mask a mid-column break, so both-missing stays honest "na" and
  # running_balance_continuity is the check there.) Noted in the detail.
  bal <- if (!is.null(tx$balance)) tx$balance else rep(NA_real_, n)
  derived <- character(0)
  if (is.na(closing) && !is.na(opening) && n > 0 && any(!is.na(bal))) {
    closing <- bal[max(which(!is.na(bal)))]; derived <- c(derived, "closing")
  }
  if (is.na(opening) && !is.na(closing) && n > 0 && !is.na(bal[1]) && !is.na(tx$amount[1])) {
    opening <- bal[1] - tx$amount[1]; derived <- c(derived, "opening")
  }
  # Which rows have no readable amount? One blank / unreadable / redacted amount is
  # enough to make the total unprovable, so name them: "it didn't reconcile" is
  # useless to a reviewer who cannot see WHICH figure is missing.
  na_amt <- if (n > 0) which(is.na(tx$amount)) else integer(0)
  na_ids <- if (length(na_amt)) (tx$row_id %||% seq_len(n))[na_amt] else integer(0)

  if (!is.na(opening) && !is.na(closing) && n > 0 && !length(na_amt)) {
    expected_close <- opening + sum(tx$amount)
    disc <- round(closing - expected_close, 2)
    note <- if (length(derived))
      sprintf(" [%s derived from the running-balance column]", paste(derived, collapse = " & ")) else ""
    return(.kpi(
      "balance_reconciliation", if (abs(disc) < PARAM_MONEY_TOL) "pass" else "fail",
      expected = round(expected_close, 2), actual = round(closing, 2),
      discrepancy = disc,
      detail = sprintf("opening %.2f + sum(amount) %.2f vs closing %.2f%s",
                       opening, sum(tx$amount), closing, note)))
  }
  if (!is.na(opening) && !is.na(closing) && n > 0) {
    # BOTH anchors are printed on the statement -- the strongest completeness proof
    # there is was available -- and a single unreadable amount is all that stopped
    # it. Reporting "na" here shrugged off the one check that could have caught a
    # wrong figure, and the run still came out trust medium / status ok / fed to
    # Qlik. Unproven is not the same as not applicable: FAIL, so it routes to
    # needs_review and the feed withholds it until a human resolves the amount.
    return(.kpi(
      "balance_reconciliation", "fail",
      expected = round(opening + sum(tx$amount, na.rm = TRUE), 2),
      actual = round(closing, 2), discrepancy = NA,
      detail = sprintf(paste0("cannot be proved: %d amount(s) could not be read (%s), ",
                              "so opening %.2f + every transaction cannot be totalled ",
                              "against the printed closing %.2f - resolve those amounts, ",
                              "then re-run"),
                       length(na_amt), .row_list(na_ids), opening, closing)))
  }
  # Honest about WHICH anchor is missing. The old single message claimed there was
  # "no opening/closing balance and no running-balance column" even when the
  # statement printed both -- factually untrue, and it sent reviewers looking for
  # a balance that was on the page all along.
  # ...and the same untruth was still live in the BOTH-missing branch: it claimed
  # there was no running-balance column even when the column was present, populated
  # and passing running_balance_continuity two rows lower on the same screen. When
  # the column IS there, the reason both anchors are refused is not that it is
  # missing -- it is that deriving BOTH from it would only check that column against
  # its own endpoints, which proves nothing. Say the real reason.
  has_bal <- any(!is.na(bal))
  why <- if (n == 0) "no transactions to reconcile"
    else if (is.na(opening) && is.na(closing) && has_bal)
      paste("no opening or closing balance was found. This statement DOES have a",
            "running-balance column, but deriving both ends from it would only check that",
            "column against its own endpoints, so it is refused - the running-balance",
            "check below is the one that tests that column")
    else if (is.na(opening) && is.na(closing))
      "no opening or closing balance was found, and no running-balance column to derive one from"
    else if (is.na(opening))
      "no opening balance was found, and none could be derived from the running-balance column"
    else "no closing balance was found, and none could be derived from the running-balance column"
  .kpi("balance_reconciliation", "na", detail = why)
}

# 2. running_balance_continuity: balance[i] == balance[i-1] + amount[i].
# A blank/redacted MIDDLE balance must not open a blind window: skipping both
# pairs around an NA balance lets a real break hide inside the gap (100, NA, 130
# with amounts 0/+20/+5 would wrongly "pass" -- 130 should be 125). Instead
# BRIDGE the gap: carry the last known-good balance plus the running sum of the
# intervening amounts, and check the next known balance against that. If any
# intervening amount is itself NA the bridge is genuinely unverifiable, so it is
# counted and surfaced, never silently passed.
.kpi_running_balance_continuity <- function(tx, n) {
  if (!(n >= 2 && !all(is.na(tx$balance))))
    return(.kpi("running_balance_continuity", "na", detail = "no running balance column"))
  ok <- TRUE; bad <- 0L; unverifiable <- 0L
  last_bal <- NA_real_; carry <- 0; gap_unknown <- FALSE
  for (i in seq_len(n)) {
    bi <- tx$balance[i]; ai <- tx$amount[i]
    if (is.na(bi)) {                      # no balance printed here
      if (is.na(ai)) gap_unknown <- TRUE  # ...and the amount is unknown too
      else carry <- carry + ai            # ...else it is an intervening amount
      next
    }
    if (!is.na(last_bal)) {               # we can test this known balance
      if (is.na(ai) || gap_unknown) unverifiable <- unverifiable + 1L
      else if (abs(bi - (last_bal + carry + ai)) >= PARAM_MONEY_TOL) { ok <- FALSE; bad <- bad + 1L }
    }
    last_bal <- bi; carry <- 0; gap_unknown <- FALSE   # reset the bridge
  }
  detail <- sprintf("%d discontinuity(ies)", bad)
  if (unverifiable > 0)
    detail <- sprintf("%s; %d gap(s) unverifiable (a bridged amount was blank/redacted)",
                      detail, unverifiable)
  .kpi("running_balance_continuity", if (ok) "pass" else "fail",
       expected = 0, actual = bad, discrepancy = bad, detail = detail)
}

# 2b. amount_direction (P2-10): a `signed` statement proves money-in vs money-out
# by its OWN +/- signs. If EVERY amount carries the same sign AND there is no
# running-balance column to cross-check, the file may in fact list UNSIGNED
# magnitudes -- read as all money-in (or all money-out), possibly inverted, with
# nothing to catch it (the loose excel_generic catch-all is the classic case).
# Fail closed so it routes to needs_review and is withheld from the governed
# feed, rather than feeding a silently sign-inverted statement to dashboards.
# NULL (no KPI at all) when the direction is genuinely provable.
.kpi_amount_direction <- function(tx, template) {
  style <- (if (!is.null(template)) template$amount_sign %||% template$table$amount_sign else NULL)
  amt_nz <- tx$amount[!is.na(tx$amount) & tx$amount != 0]
  no_balance <- is.null(tx$balance) || all(is.na(tx$balance))
  if (!(identical(style, "signed") && no_balance && length(amt_nz) >= 2 &&
        (all(amt_nz > 0) || all(amt_nz < 0)))) return(NULL)
  .kpi("amount_direction", "fail", expected = "mixed or balance-checkable",
       actual = if (all(amt_nz > 0)) "all money-in" else "all money-out",
       detail = paste0("every amount shares one sign and there is no running balance to confirm ",
                       "direction -- if this export lists unsigned magnitudes, money-in/out may be ",
                       "inverted; check a few rows against the source or set the correct amount style"))
}

# 3. transaction_count: parsed > 0 and == stated count if present.
#
# WHY THE NO-COUNT CASE KEEPS "pass" (it was challenged, and the answer is not
# "because it always did"). The rule this file already enforces is that a check
# which did not run must not report pass -- that is why no_unparsed_rows says
# "na" on a PDF. The difference is what the row DISPLAYS. The PDF case set
# expected = n and actual = n, so it showed a proof it had not performed. This
# case sets expected = ">0", which is exactly and only the thing it did test: the
# row on screen reads "Row count | Expected >0 | Read 7 | OK", and a reviewer can
# see from the Expected column that no printed figure was involved. A check that
# declares its own weakness on the same line is not a silent one.
#
# The alternative (return "na" when the statement prints no count) was built and
# measured. It is honest, but every real sample in this repo prints no count, so
# it demotes essentially every statement from "high" to "medium" -- and a grade
# that stops varying stops carrying information, which is the same defect moved
# somewhere else. Keeping it would need a compensating trust rule ("high stands
# when the balance proof passed"), i.e. machinery to undo most of what the change
# did. The claim the reviewer reads was already corrected where it belonged: the
# label is "Row count", the topic, not "Row count matches the statement".
#
# n == 0 is still a real failure and still reported as one -- it is reachable from
# every caller that reconciles without going through convert (an auto-split
# segment, the batch audit).
.kpi_transaction_count <- function(h, n) {
  stated <- suppressWarnings(as.integer(h$stated_count %||% NA))
  if (!is.na(stated))
    return(.kpi("transaction_count", if (n == stated && n > 0) "pass" else "fail",
                expected = stated, actual = n, discrepancy = n - stated,
                detail = "parsed vs stated transaction count"))
  .kpi("transaction_count", if (n > 0) "pass" else "fail",
       expected = ">0", actual = n, discrepancy = NA,
       detail = sprintf(paste0("this statement prints no transaction count, so the only thing ",
                               "proved here is that %d row(s) were read - count them against the ",
                               "statement if the total matters"), n))
}

# 4. dates_within_period: all dates within period_start..period_end.
# Period bounds may be verbatim strings ("1 May 2026"), not ISO -> parse both
# tolerantly (the shared .tolerant_date, see R/params.R) so an unparseable bound
# skips the check rather than crashing.
.kpi_dates_within_period <- function(tx, h, n) {
  ps <- .tolerant_date(h$period_start %||% NA); pe <- .tolerant_date(h$period_end %||% NA)
  if (!(!is.na(ps) && !is.na(pe) && n > 0))
    return(.kpi("dates_within_period", "na", detail = "statement period not available"))
  d <- suppressWarnings(as.Date(tx$date))
  within <- !is.na(d) & d >= ps & d <= pe
  outside <- sum(!within, na.rm = TRUE)
  .kpi("dates_within_period", if (outside == 0) "pass" else "fail",
       expected = sprintf("%s..%s", ps, pe), actual = outside,
       discrepancy = outside, detail = sprintf("%d date(s) outside period", outside))
}

# 4b. dates_readable: the never-silently-wrong safety net for the date column.
# A template can fingerprint-match while its date mapping misses (renamed or
# re-cased header, wrong sheet, wrong format) - every date comes back NA and,
# with no period to check against, nothing above would fail. Rows with no
# readable date at ALL must never leave as a clean "ok".
#
# EVERY row, not "at least one". The threshold used to be d_ok > 0, so 1 readable
# date out of 500 passed, under a label ("Row dates could be read") a reviewer
# reads as a statement about the whole column. That is the silently-wrong shape
# the charter forbids: 499 rows with no usable date, behind a green tick. A
# PARTIAL read is not "could not be checked" either -- the check ran and found a
# real defect in specific rows -- so it fails, and the rows are named by the
# date_parse / date_unresolved diagnostics that already exist for them.
.kpi_dates_readable <- function(tx, n) {
  if (n == 0) return(NULL)
  d_ok <- sum(!is.na(suppressWarnings(as.Date(tx$date))))
  .kpi("dates_readable", if (d_ok == n) "pass" else "fail",
       expected = n, actual = d_ok, discrepancy = n - d_ok,
       detail = if (d_ok == n) sprintf("all %d row date(s) read", n)
                else if (d_ok > 0)
                  sprintf(paste0("%d of %d row date(s) could not be read - the date format is ",
                                 "wrong for those rows, or the date was blank/redacted"),
                          n - d_ok, n)
                else "no row dates could be read - the date column mapping or format is wrong")
}

# 5. no_unparsed_rows: every non-empty source data line became a transaction.
# Completeness is proven by comparing the count of non-empty PHYSICAL source
# data lines against parsed rows -- computing it from the parsed table alone
# (n vs n-malformed) can never see a record that was merged/lost, so a stray
# cross-line quote would silently pass. `source_line_count` is threaded from
# the reader; NA (excel/pdf) falls back to the malformed-only check.
.kpi_no_unparsed_rows <- function(parsed, tx, n) {
  malformed <- sum(grepl("malformed", tx$flags))
  src_lines <- suppressWarnings(as.integer(parsed$source_line_count %||% NA))
  # A legitimate multi-line quoted record spans several physical lines but is ONE
  # parsed row; those continuation lines are accounted for here so they are not
  # miscounted as lost. A physical line captured by no record still fails loudly.
  extra_ml <- suppressWarnings(as.integer(parsed$multiline_extra %||% 0L))
  if (is.na(extra_ml)) extra_ml <- 0L
  lost <- if (!is.na(src_lines)) max(0L, src_lines - n - extra_ml) else 0L
  good <- n - malformed
  expected_rows <- if (!is.na(src_lines)) src_lines else n
  # Rows the PDF reader examined but did not keep (headers, summary lines, notes,
  # and any row whose date or amount it could not read). Threaded out of
  # parse_pdf_table so this KPI can state a real number instead of nothing.
  skipped_rows <- suppressWarnings(as.integer(parsed$skipped_row_count %||% NA))
  visual_rows  <- suppressWarnings(as.integer(parsed$visual_row_count %||% NA))

  if (!is.na(src_lines)) {
    # Delimited: the physical data-line count is known, so completeness is provable.
    #
    # THIS ONE KEEPS ITS "pass", deliberately. It is the third of the three checks
    # that were challenged for passing more generously than they sound, and it is
    # the one that holds up: it compares two independently-counted quantities, it
    # can and does come back "fail" on real files, and its label ("No row failed to
    # read") claims exactly what it proves and nothing more. It is true that it
    # never looks inside a cell -- and it must not, because that is what
    # dates_readable, balance_reconciliation and the malformed flag are for. A
    # check that proves one thing soundly is not the same defect as a check that
    # proves nothing and says "pass".
    return(.kpi(
      "no_unparsed_rows", if (malformed == 0 && lost == 0) "pass" else "fail",
      expected = expected_rows, actual = good, discrepancy = expected_rows - good,
      detail = if (lost > 0)
        sprintf("%d source line(s) unaccounted for; %d malformed row(s)", lost, malformed)
      else sprintf("%d malformed row(s)", malformed)))
  }
  if (malformed > 0) {
    return(.kpi(
      "no_unparsed_rows", "fail", expected = expected_rows, actual = good,
      discrepancy = malformed, detail = sprintf("%d malformed row(s)", malformed)))
  }
  # PDF / Excel: there is NO independent source-line count, so `lost` was always
  # 0 by construction and this KPI reported "pass" for every such statement -- a
  # green tick for a check that never ran, which is exactly the failure the
  # charter forbids. Say "not applicable" instead, and show the skipped-row count
  # so the number is a fact the reviewer can check on the page rather than a
  # reassurance nobody computed.
  # THE THIN PARSE. A PDF has no source-line count, so completeness "cannot be
  # proved" -- but there is one thing it CAN prove, and it used to throw away:
  # rows that looked like transactions and could not be read (the date didn't
  # parse, or there was no amount in the money bands). Headings and summary lines
  # are skipped on every healthy statement and mean nothing; these do not.
  #
  # When MORE rows failed to read than were read, the template is reading the
  # wrong part of the page. That is not a percentage anybody had to tune - it is
  # two like quantities compared, and it is indefensible on a real statement.
  # Measured across every sample in the repo, healthy conversions sit at 4-36%
  # actionable skips against kept rows; the broken ones are at 100-1300%.
  #
  # WHY THIS MATTERS: reading 5 rows of a 500-row statement used to pass every
  # check -- transaction_count only requires n > 0 when the statement prints no
  # stated count, and this KPI returned "na". A green run, on the dashboards,
  # missing 99% of the money. That is the exact failure the charter forbids.
  # THE SMALL-STATEMENT ASYMMETRY. The comparison is fair at 300 rows and fragile
  # at 5: a 5-transaction statement needs only 5 unreadable rows to trip, and one
  # unusual "brought forward" line that the summary matcher misses, plus a wrapped
  # note, can get most of the way there. A 300-row statement would need 300.
  #
  # So the comparison must carry enough evidence to mean something. Three is the
  # smallest count that cannot be one stray row plus its neighbour -- below that,
  # "more missed than read" is noise rather than a finding, and crying wolf on a
  # small honest statement teaches people to ignore the one warning that matters.
  #
  # Note this is NOT the same as "more rows didn't look like transactions than
  # did", which is true of most statements and is fine: headings, notes, summary
  # lines and wrapped text are excluded from `actionable` entirely. The worked
  # example measures 12 kept, 17 skipped, 0 actionable.
  actionable <- suppressWarnings(as.integer(parsed$actionable_skip_count %||% NA))
  if (!is.na(actionable) && actionable >= 3L && actionable >= n) {
    return(.kpi(
      "no_unparsed_rows", "fail", expected = n + actionable, actual = n,
      discrepancy = actionable,
      detail = sprintf(paste0("%d row(s) on this statement look like transactions but could not be ",
                              "read, against %d that were - so more of it was missed than captured. ",
                              "The template is almost certainly reading the wrong part of the page ",
                              "(most often the date format, or a column band in the wrong place). ",
                              "See it on the page shows which rows and why."),
                       actionable, n)))
  }
  # THE SKIPPED COUNT IS THE MOST ALARMING NUMBER ON THIS ROW, and it sat beside a
  # Result column reading "could not be checked" with nothing to size it. The
  # engine already knows the fact that settles it: `actionable` counts only the
  # skips that MEAN something -- a row shaped like a transaction that would not
  # read -- while headings, summary lines and wrapped text are excluded and are
  # skipped on every healthy statement. When that count is zero, say so; a reviewer
  # should not have to open a second screen to find out that 122 is the normal
  # number for a statement of this length.
  skip_note <- if (is.na(skipped_rows) || skipped_rows == 0L || is.na(actionable)) ""
    else if (actionable == 0L) " None of the skipped rows looked like a transaction."
    else sprintf(" %d of the skipped rows looked like a transaction and could not be read.",
                 actionable)
  .kpi("no_unparsed_rows", "na", expected = expected_rows, actual = good,
       discrepancy = NA,
       # "the balance proof above", not `balance_reconciliation`: a raw engine code
       # on a customer-facing screen is something the operational guide tells the
       # analyst to REPORT AS A BUG, so this row must not be the one that prints it.
       detail = if (!is.na(skipped_rows) && !is.na(visual_rows))
         sprintf(paste0("cannot be proved for this format: there is no independent ",
                        "source line count. %d of %d visual row(s) were read as ",
                        "transactions and %d were skipped (headings, summary lines, ",
                        "wrapped text, or a date/amount that could not be read).%s ",
                        "See it on the page shows every skipped row and why; ",
                        "completeness otherwise rests on the balance proof above."),
                 n, visual_rows, skipped_rows, skip_note)
       else paste0("cannot be proved for this format: the total source line count is ",
                   "not independently known, so rows dropped by column/date filtering ",
                   "are NOT counted here -- rely on the balance proof above for ",
                   "completeness."))
}

# 6. redaction_summary: informational count of redacted rows.
.kpi_redaction_summary <- function(tx) {
  redacted <- sum(grepl("redacted", tx$flags))
  .kpi("redaction_summary", "na", expected = NA, actual = redacted,
       discrepancy = NA, detail = sprintf("%d redacted row(s)", redacted),
       informational = TRUE)
}

# 7. ocr_confidence: informational -- was any page machine-read (OCR), and how
# confident was the worst page? OCR is never 100% accurate, so this must be
# visible to a forensic reviewer alongside the confidence figure. It does not
# move the score (it is informational) but it DOES cap the trust level below --
# an OCR'd statement is never rated "high". NULL when no page was OCR'd.
.kpi_ocr_confidence <- function(h) {
  o <- .header_ocr(h)
  if (o$pages <= 0) return(NULL)
  .kpi("ocr_confidence", "na", expected = NA,
       actual = if (is.na(o$conf)) sprintf("%d page(s) OCR-read", o$pages)
                else sprintf("%d page(s) OCR-read, min page confidence %.0f%%", o$pages, o$conf),
       discrepancy = NA,
       detail = "machine-read (OCR) text is not guaranteed 100% accurate -- verify amounts and descriptions against the source PDF",
       informational = TRUE)
}

# 8. redaction_scan: did the occlusion scan actually finish on every page?
# The scan is the ONLY thing that proves text under a redaction box stayed
# hidden. When it could not complete, the reader falls back to the raw text
# layer -- so words the sender blacked out may have been emitted verbatim. The
# header carried that fact all the way here and NOTHING read it: the run said
# "Converted successfully", trust was uncapped, and the feed accepted a file
# that may contain hidden text. Honouring redactions is absolute, so this FAILS
# (routing to needs_review and withholding the feed) rather than warning quietly.
# NULL (nothing raised) when the scan was clean, so a good run changes nothing.
.kpi_redaction_scan <- function(h) {
  scan_incomplete <- suppressWarnings(as.integer(h$redaction_scan_incomplete %||% 0L))
  if (is.na(scan_incomplete)) scan_incomplete <- 0L
  if (scan_incomplete <= 0) return(NULL)
  .kpi("redaction_scan", "fail", expected = 0, actual = scan_incomplete,
       discrepancy = scan_incomplete,
       detail = sprintf(paste0("the redaction scan could not complete on %d page(s), so text ",
                               "hidden under a redaction box may have been read and emitted - ",
                               "do NOT release this output; check those pages against the ",
                               "source PDF before use"), scan_incomplete))
}

# ---- deterministic trust -----------------------------------------------------
# .reconcile_trust(kpis, tx, h, n) -> list(level, score, reasons, ...).
# The KPI table decides the base level; the caveats below can only LOWER a "high"
# (never raise anything), because each describes something the checks could not
# prove. `kpis` still carries the internal `informational` column here.
.reconcile_trust <- function(kpis, tx, h, n) {
  applicable <- kpis[!kpis$informational, ]
  n_fail <- sum(applicable$status == "fail")
  n_na   <- sum(applicable$status == "na")
  n_pass <- sum(applicable$status == "pass")
  total  <- nrow(applicable)

  reasons <- character(0)
  if (n_fail > 0) {
    level <- "low"
    reasons <- c(reasons, sprintf("%d KPI(s) failed: %s", n_fail,
      paste(applicable$name[applicable$status == "fail"], collapse = ", ")))
  } else if (n_na > 0) {
    level <- "medium"
    reasons <- c(reasons, sprintf("%d KPI(s) not applicable: %s", n_na,
      paste(applicable$name[applicable$status == "na"], collapse = ", ")))
  } else {
    level <- "high"
    reasons <- c(reasons, "all applicable checks passed")
  }
  score <- if (total == 0) 0 else round(100 * (n_pass + 0.5 * n_na) / total)

  # Balance reconciliation is the strongest proof of completeness (opening + every
  # transaction = closing). When it PASSES, a failing running-balance-continuity
  # check alone -- typical of combined/multi-account statements where the balance
  # column resets between sections -- should not drag trust to "low". The result
  # still surfaces as needs-review (a KPI failed), but honestly rated medium.
  if (identical(level, "low")) {
    fails <- applicable$name[applicable$status == "fail"]
    bal_pass <- any(applicable$name == "balance_reconciliation" & applicable$status == "pass")
    secondary <- c("running_balance_continuity", "dates_within_period")
    if (bal_pass && length(fails) && all(fails %in% secondary)) {
      level <- "medium"
      reasons <- c(reasons, "balance fully reconciles (opening + every transaction = closing); the running-balance / period checks are secondary and commonly flag on combined/multi-account statements")
    }
  }

  # Completeness guard (forensic): if NEITHER a balance reconciliation NOR a
  # running-balance check could run, and there's no stated count, the engine has
  # no independent way to know a transaction was dropped. Say so loudly and never
  # rate such a run "high" -- silence here would be a silent audit gap.
  bal_ok  <- any(kpis$name == "balance_reconciliation"     & kpis$status != "na")
  run_ok  <- any(kpis$name == "running_balance_continuity" & kpis$status != "na")
  cnt_ok  <- !is.na(suppressWarnings(as.integer(h$stated_count %||% NA)))
  completeness_verified <- bal_ok || run_ok || cnt_ok
  if (!completeness_verified && n > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons,
      "completeness UNVERIFIED: no balance or stated count to reconcile against, so a dropped/missing transaction cannot be detected automatically - check the row count against the statement")
  }

  # Unresolved-year caveat: rows kept with date_unresolved carry a verbatim
  # day/month but NO year (no statement period was found). Say so explicitly --
  # the money is preserved but the dates are not fully known.
  n_dateunres <- sum(grepl("date_unresolved", tx$flags))
  if (n_dateunres > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons, sprintf(
      "%d row(s) have an UNRESOLVED year (no statement period found); day and month are captured verbatim but the year could not be determined - assign it before relying on the dates",
      n_dateunres))
  }

  # Inferred-year caveat: the mirror image of the case above. The table printed
  # day+month only, NO statement period could be read, and the reader took the year
  # from the one 4-digit number it found in free page text (a footer, a copyright
  # line). Those dates come out fully formed and LOOK proven, so nothing above would
  # ever question them -- but the year came from outside the period wording and is
  # an inference, not a fact. Cap trust and say where the year came from.
  n_yearinf <- sum(grepl("date_year_inferred", tx$flags))
  if (n_yearinf > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons, sprintf(
      "%d row(s) have an INFERRED year: no statement period was found, so the year was taken from the only 4-digit year printed anywhere on the page (e.g. a footer). Day and month are verbatim - confirm the year against the statement before relying on the dates",
      n_yearinf))
  }

  # OCR caveat (forensic): a statement where ANY page was machine-read by OCR is
  # never rated "high" -- OCR is not guaranteed accurate, and reconciliation math
  # only cross-checks amounts, not the verbatim descriptions. Always surface that
  # OCR was used and the confidence figure so the reviewer verifies the source.
  ocr <- .header_ocr(h)
  if (ocr$pages > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons, sprintf(
      "%d page(s) were read by OCR%s; machine-read text is not guaranteed 100%% accurate - verify amounts and descriptions against the source PDF",
      ocr$pages,
      if (is.na(ocr$conf)) " (page confidence could not be measured)"
      else sprintf(" (min page confidence %.0f%%)", ocr$conf)))
    # A per-cell flag is stronger than the page mean: it points at the exact rows
    # whose date/amount/balance may have been misread.
    n_ocrlow <- sum(grepl("ocr_low_conf", tx$flags))
    if (n_ocrlow > 0)
      reasons <- c(reasons, sprintf(
        "%d row(s) have a LOW-CONFIDENCE OCR value in a date/amount/balance cell - check those cells against the source", n_ocrlow))
  }

  list(level = level, score = score, reasons = reasons,
       completeness_verified = completeness_verified,
       ocr_pages = ocr$pages, ocr_min_confidence = ocr$conf)
}

# reconcile(parsed, template) -> list(kpis, trust)
# The checks, in the order they are reported. A builder returning NULL means "this
# check does not apply to this statement" and simply does not appear.
reconcile <- function(parsed, template = NULL) {
  tx <- parsed$transactions
  h  <- parsed$header
  n  <- nrow(tx)

  rows <- list(
    balance_reconciliation     = .kpi_balance_reconciliation(tx, h, n),
    running_balance_continuity = .kpi_running_balance_continuity(tx, n),
    amount_direction           = .kpi_amount_direction(tx, template),
    transaction_count          = .kpi_transaction_count(h, n),
    dates_within_period        = .kpi_dates_within_period(tx, h, n),
    dates_readable             = .kpi_dates_readable(tx, n),
    no_unparsed_rows           = .kpi_no_unparsed_rows(parsed, tx, n),
    redaction_summary          = .kpi_redaction_summary(tx),
    ocr_confidence             = .kpi_ocr_confidence(h),
    redaction_scan             = .kpi_redaction_scan(h))
  rows <- Filter(Negate(is.null), rows)

  kpis <- do.call(rbind, rows)
  rownames(kpis) <- NULL

  trust <- .reconcile_trust(kpis, tx, h, n)

  # Return KPIs without the internal informational flag column exposed downstream.
  list(kpis = kpis[, c("name", "status", "expected", "actual", "discrepancy", "detail")],
       trust = trust)
}
