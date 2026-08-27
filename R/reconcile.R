# reconcile.R -- reconciliation KPIs and deterministic trust mapping. One .kpi_*() builder per check,
# each returning ONE .kpi() row or NULL when it does not apply. To add one: write the builder, add a
# line to reconcile(), a label in CHECK_PLAIN and, if it can FAIL, a fix entry in build_diagnostics().

# A CHECK THAT DID NOT RUN SHOWS NO FIGURES: "Expected 79 | Read 79" under "could not be checked"
# reads as a pass. An `expected` must be a figure the STATEMENT PRINTS or a plain target, never a
# count synthesised from our own skips. A `detail` is re-emitted verbatim by build_diagnostics().
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

# A detail must POINT AT the offending rows, but stay readable when a whole column failed to parse.
.row_list <- function(ids, max_show = 10L) {
  ids <- suppressWarnings(as.integer(ids)); ids <- ids[!is.na(ids)]
  if (!length(ids)) return("no row identified")
  extra <- length(ids) - max_show
  shown <- paste(utils::head(ids, max_show), collapse = ", ")
  lbl <- if (length(ids) == 1L) "row " else "rows "
  if (extra > 0) sprintf("%s%s and %d more", lbl, shown, extra) else paste0(lbl, shown)
}

.header_ocr <- function(h) {
  pages <- suppressWarnings(as.integer(h$ocr_pages %||% 0L))
  if (is.na(pages)) pages <- 0L
  list(pages = pages, conf = suppressWarnings(as.numeric(h$ocr_min_confidence %||% NA)))
}

.kpi_balance_reconciliation <- function(tx, h, n) {
  opening <- suppressWarnings(as.numeric(h$opening_balance %||% NA))
  closing <- suppressWarnings(as.numeric(h$closing_balance %||% NA))
  # Derive the MISSING anchor from the balance column, but ONLY when the OTHER is labelled, so one
  # independent anchor remains: deriving both could mask a mid-column break.
  bal <- if (!is.null(tx$balance)) tx$balance else rep(NA_real_, n)
  derived <- character(0)
  if (is.na(closing) && !is.na(opening) && n > 0 && any(!is.na(bal))) {
    closing <- bal[max(which(!is.na(bal)))]; derived <- c(derived, "closing")
  }
  if (is.na(opening) && !is.na(closing) && n > 0 && !is.na(bal[1]) && !is.na(tx$amount[1])) {
    opening <- bal[1] - tx$amount[1]; derived <- c(derived, "opening")
  }
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
    # Both anchors are printed and a single unreadable amount is all that stopped the strongest
    # completeness proof there is. Unproven is not not-applicable: FAIL, so the feed withholds it.
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
  # Honest about WHICH anchor is missing: the old message claimed there was no balance and no
  # running-balance column even when the statement printed both.
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

# balance[i] == balance[i-1] + amount[i]. A blank or redacted MIDDLE balance must not open a blind
# window, so BRIDGE it. If any intervening amount is itself NA the bridge is unverifiable and is
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

# If EVERY amount carries the same sign AND there is no running-balance column to cross-check, the
# file may list unsigned magnitudes read as all money-in, possibly inverted. Fail closed.
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

# The no-count case keeps "pass" because expected = ">0" is exactly what it tested. Returning "na"
# was measured: every real sample prints no count, so it demotes essentially every statement to
# medium, and a grade that stops varying stops carrying information.
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

# TRUE only when both bounds read as dates; an unreadable bound is unknown, not backwards. A
# newest-first bundle published "20 Apr 2026 to 19 Feb 2026" and the feed accepted it. Named once
# so the bundle header and every reconciled statement cannot disagree.
.period_runs_backwards <- function(start, end) {
  s <- .tolerant_date(start %||% NA); e <- .tolerant_date(end %||% NA)
  !is.na(s) && !is.na(e) && e < s
}

# Bounds may be verbatim strings, so an unparseable bound skips the check. A BACKWARDS period fails
# in its own words: the old detail read "34 date(s) outside period", blaming the rows.
.kpi_dates_within_period <- function(tx, h, n) {
  ps <- .tolerant_date(h$period_start %||% NA); pe <- .tolerant_date(h$period_end %||% NA)
  if (.period_runs_backwards(h$period_start, h$period_end))
    return(.kpi("dates_within_period", "fail",
                expected = sprintf("%s..%s", ps, pe), actual = "ends before it starts",
                detail = sprintf(paste0("the statement period ends before it starts (%s to %s), ",
                                        "so the row dates could not be checked against it - ",
                                        "the period was read wrongly, the rows may be fine"),
                                 h$period_start, h$period_end)))
  if (!(!is.na(ps) && !is.na(pe) && n > 0))
    return(.kpi("dates_within_period", "na", detail = "statement period not available"))
  d <- suppressWarnings(as.Date(tx$date))
  within <- !is.na(d) & d >= ps & d <= pe
  outside <- sum(!within, na.rm = TRUE)
  # Expected and Actual are rendered side by side, so they must be the same kind of thing: a date
  # RANGE against a COUNT invites the reviewer to read 34 as a date.
  span <- if (any(!is.na(d))) sprintf("%s..%s", min(d, na.rm = TRUE), max(d, na.rm = TRUE))
          else NA_character_
  .kpi("dates_within_period", if (outside == 0) "pass" else "fail",
       expected = sprintf("%s..%s", ps, pe), actual = span,
       discrepancy = outside, detail = sprintf("%d date(s) outside period", outside))
}

# The safety net for the date column: a template can fingerprint-match while its date mapping
# misses. EVERY row, not "at least one" - one readable date out of 500 passed under a label a
# reviewer reads as being about the whole column.
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

# Completeness is proven against non-empty PHYSICAL source lines; computing it from the parsed
# table alone can never see a record that was merged or lost. NA (excel/pdf) falls back to malformed.
.kpi_no_unparsed_rows <- function(parsed, tx, n) {
  malformed <- sum(grepl("malformed", tx$flags))
  src_lines <- suppressWarnings(as.integer(parsed$source_line_count %||% NA))
  # A multi-line quoted record spans several physical lines but is ONE parsed row. A physical line
  # captured by no record still fails loudly.
  extra_ml <- suppressWarnings(as.integer(parsed$multiline_extra %||% 0L))
  if (is.na(extra_ml)) extra_ml <- 0L
  lost <- if (!is.na(src_lines)) max(0L, src_lines - n - extra_ml) else 0L
  good <- n - malformed
  expected_rows <- if (!is.na(src_lines)) src_lines else n
  skipped_rows <- suppressWarnings(as.integer(parsed$skipped_row_count %||% NA))
  visual_rows  <- suppressWarnings(as.integer(parsed$visual_row_count %||% NA))

  if (!is.na(src_lines)) {
    # Delimited: the physical data-line count is known, so completeness is provable. It never looks
    # inside a cell, and must not - that is what dates_readable and balance_reconciliation are for.
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
  # PDF / Excel: there is NO independent source-line count, so `lost` was always 0 by construction and
  # this reported "pass" for every such statement - a green tick for a check that never ran.
  # THE THIN PARSE. A PDF can still prove one thing: rows that looked like transactions and could not
  # be read. "More missed than captured" alone was falsified by a shipped sample sitting at 171%,
  # because `kept` counts transactions while `actionable` counts page furniture carrying a digit, so
  # it gains a MAGNITUDE guard over one denominator - healthy samples run 0-16%, the same statements
  # with the template off the table 34-52%. Both conditions must hold and everything under it is
  # still REPORTED; the small-statement floor stays, or crying wolf teaches people to ignore it.
  actionable <- suppressWarnings(as.integer(parsed$actionable_skip_count %||% NA))
  if (!is.na(actionable) && !is.na(visual_rows) && actionable >= 3L &&
      actionable >= n && actionable * 4L > visual_rows) {
    return(.kpi(
      "no_unparsed_rows", "fail",
      # Nothing in the Expected column. It used to read n + actionable - "Expected 19" beside
      # "Read 7" on a statement that prints no 19 anywhere, invented by the checker.
      expected = NA, actual = n, discrepancy = actionable,
      detail = sprintf(paste0("%d of the %d row(s) the reader examined look like transactions and ",
                              "could not be read, against %d that were read - more than a quarter ",
                              "of everything on these pages, and more than were captured. The ",
                              "template is almost certainly reading the wrong part of the page ",
                              "(most often the date format, or a column band in the wrong place). ",
                              "See it on the page shows which rows and why."),
                       actionable, visual_rows, n)))
  }
  # The skipped count is the most alarming number on this row and sat beside "could not be checked"
  # with nothing to size it. `actionable` counts only the skips that MEAN something.
  skip_note <- if (is.na(skipped_rows) || skipped_rows == 0L || is.na(actionable)) ""
    else if (actionable == 0L) " None of the skipped rows looked like a transaction."
    else sprintf(" %d of the skipped rows looked like a transaction and could not be read.",
                 actionable)
  .kpi("no_unparsed_rows", "na", expected = expected_rows, actual = good,
       discrepancy = NA,
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

.kpi_redaction_summary <- function(tx) {
  redacted <- sum(grepl("redacted", tx$flags))
  .kpi("redaction_summary", "na", expected = NA, actual = redacted,
       discrepancy = NA, detail = sprintf("%d redacted row(s)", redacted),
       informational = TRUE)
}

# Informational: was any page machine-read, and how confident was the worst page? It does not move
# the score but it DOES cap trust - an OCR'd statement is never rated "high".
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

# The occlusion scan is the ONLY thing that proves text under a redaction box stayed hidden; when
# it could not complete the reader fell back to the raw text layer, so blacked-out words may have
# been emitted verbatim. Honouring redactions is absolute, so this FAILS rather than warning.
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

# The KPI table decides the base level; the caveats below can only LOWER a "high", because each
# describes something the checks could not prove.
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

  # Balance reconciliation is the strongest proof of completeness, so a failing
  # running-balance-continuity check alone - typical of combined statements where the balance column
  # resets - should not drag trust to "low". The reason justifies the level; it does not grade.
  if (identical(level, "low")) {
    fails <- applicable$name[applicable$status == "fail"]
    bal_pass <- any(applicable$name == "balance_reconciliation" & applicable$status == "pass")
    secondary <- c("running_balance_continuity", "dates_within_period")
    if (bal_pass && length(fails) && all(fails %in% secondary)) {
      level <- "medium"
      reasons <- c(reasons, paste(
        "balance fully reconciles (opening + every transaction = closing), which is why this is rated",
        "medium rather than low - it does NOT clear the check(s) that failed, and it says nothing about",
        "anything else flagged on this run; read those before relying on the figures"))
    }
  }

  # Completeness guard: with NEITHER a balance reconciliation NOR a running-balance check and no
  # stated count, a dropped transaction cannot be detected. Never rate such a run "high".
  bal_ok  <- any(kpis$name == "balance_reconciliation"     & kpis$status != "na")
  run_ok  <- any(kpis$name == "running_balance_continuity" & kpis$status != "na")
  cnt_ok  <- !is.na(suppressWarnings(as.integer(h$stated_count %||% NA)))
  completeness_verified <- bal_ok || run_ok || cnt_ok
  if (!completeness_verified && n > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons,
      "completeness UNVERIFIED: no balance or stated count to reconcile against, so a dropped/missing transaction cannot be detected automatically - check the row count against the statement")
  }

  n_dateunres <- sum(grepl("date_unresolved", tx$flags))
  if (n_dateunres > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons, sprintf(
      "%d row(s) have an UNRESOLVED year (no statement period found); day and month are captured verbatim but the year could not be determined - assign it before relying on the dates",
      n_dateunres))
  }

  # The mirror image: day+month only, no period, year taken from the one 4-digit number in free
  # page text. Those dates come out fully formed and LOOK proven, but the year is an inference.
  n_yearinf <- sum(grepl("date_year_inferred", tx$flags))
  if (n_yearinf > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons, sprintf(
      "%d row(s) have an INFERRED year: no statement period was found, so the year was taken from the only 4-digit year printed anywhere on the page (e.g. a footer). Day and month are verbatim - confirm the year against the statement before relying on the dates",
      n_yearinf))
  }

  ocr <- .header_ocr(h)
  if (ocr$pages > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons, sprintf(
      "%d page(s) were read by OCR%s; machine-read text is not guaranteed 100%% accurate - verify amounts and descriptions against the source PDF",
      ocr$pages,
      if (is.na(ocr$conf)) " (page confidence could not be measured)"
      else sprintf(" (min page confidence %.0f%%)", ocr$conf)))
    n_ocrlow <- sum(grepl("ocr_low_conf", tx$flags))
    if (n_ocrlow > 0)
      reasons <- c(reasons, sprintf(
        "%d row(s) have a LOW-CONFIDENCE OCR value in a date/amount/balance cell - check those cells against the source", n_ocrlow))
  }

  list(level = level, score = score, reasons = reasons,
       completeness_verified = completeness_verified,
       ocr_pages = ocr$pages, ocr_min_confidence = ocr$conf)
}

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

  list(kpis = kpis[, c("name", "status", "expected", "actual", "discrepancy", "detail")],
       trust = trust)
}
