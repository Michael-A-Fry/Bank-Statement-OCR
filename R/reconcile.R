# reconcile.R -- reconciliation KPIs + deterministic trust mapping.

.kpi <- function(name, status, expected = NA, actual = NA,
                 discrepancy = NA, detail = "", informational = FALSE) {
  data.frame(name = name, status = status,
             expected = as.character(expected), actual = as.character(actual),
             discrepancy = as.character(discrepancy), detail = detail,
             informational = informational, stringsAsFactors = FALSE)
}

# reconcile(parsed, template) -> list(kpis, trust)
reconcile <- function(parsed, template = NULL) {
  tx <- parsed$transactions
  h  <- parsed$header
  n  <- nrow(tx)
  rows <- list()

  # 1. balance_reconciliation: opening + sum(amount) == closing.
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
  if (!is.na(opening) && !is.na(closing) && n > 0 && !any(is.na(tx$amount))) {
    expected_close <- opening + sum(tx$amount)
    disc <- round(closing - expected_close, 2)
    note <- if (length(derived))
      sprintf(" [%s derived from the running-balance column]", paste(derived, collapse = " & ")) else ""
    rows$balance_reconciliation <- .kpi(
      "balance_reconciliation", if (abs(disc) < 0.005) "pass" else "fail",
      expected = round(expected_close, 2), actual = round(closing, 2),
      discrepancy = disc,
      detail = sprintf("opening %.2f + sum(amount) %.2f vs closing %.2f%s",
                       opening, sum(tx$amount), closing, note))
  } else {
    rows$balance_reconciliation <- .kpi(
      "balance_reconciliation", "na",
      detail = "no opening/closing balance and no running-balance column to derive one")
  }

  # 2. running_balance_continuity: balance[i] == balance[i-1] + amount[i].
  if (n >= 2 && !all(is.na(tx$balance))) {
    ok <- TRUE; bad <- 0L
    for (i in 2:n) {
      if (is.na(tx$balance[i]) || is.na(tx$balance[i - 1]) || is.na(tx$amount[i])) next
      if (abs(tx$balance[i] - (tx$balance[i - 1] + tx$amount[i])) >= 0.005) {
        ok <- FALSE; bad <- bad + 1L
      }
    }
    rows$running_balance_continuity <- .kpi(
      "running_balance_continuity", if (ok) "pass" else "fail",
      expected = 0, actual = bad, discrepancy = bad,
      detail = sprintf("%d discontinuity(ies)", bad))
  } else {
    rows$running_balance_continuity <- .kpi(
      "running_balance_continuity", "na",
      detail = "no running balance column")
  }

  # 3. transaction_count: parsed > 0 and == stated count if present.
  stated <- suppressWarnings(as.integer(h$stated_count %||% NA))
  if (!is.na(stated)) {
    rows$transaction_count <- .kpi(
      "transaction_count", if (n == stated && n > 0) "pass" else "fail",
      expected = stated, actual = n, discrepancy = n - stated,
      detail = "parsed vs stated transaction count")
  } else {
    rows$transaction_count <- .kpi(
      "transaction_count", if (n > 0) "pass" else "fail",
      expected = ">0", actual = n, discrepancy = NA,
      detail = "no stated count; require at least one parsed row")
  }

  # 4. dates_within_period: all dates within period_start..period_end.
  # Period bounds may be verbatim strings ("1 May 2026"), not ISO -> parse both
  # tolerantly so an unparseable bound skips the check rather than crashing.
  .rec_date <- function(s) {
    for (f in c("%Y-%m-%d", "%d %b %Y", "%d %B %Y", "%d/%m/%Y", "%d-%m-%Y",
                "%d %b %y", "%d %B %y", "%d/%m/%y", "%d-%m-%y")) {
      d <- suppressWarnings(as.Date(as.character(s), f))
      if (!is.na(d) && as.integer(format(d, "%Y")) >= 1990) return(d)
    }
    as.Date(NA)
  }
  ps <- .rec_date(h$period_start %||% NA); pe <- .rec_date(h$period_end %||% NA)
  if (!is.na(ps) && !is.na(pe) && n > 0) {
    d <- suppressWarnings(as.Date(tx$date))
    within <- !is.na(d) & d >= ps & d <= pe
    outside <- sum(!within, na.rm = TRUE)
    rows$dates_within_period <- .kpi(
      "dates_within_period", if (outside == 0) "pass" else "fail",
      expected = sprintf("%s..%s", ps, pe), actual = outside,
      discrepancy = outside, detail = sprintf("%d date(s) outside period", outside))
  } else {
    rows$dates_within_period <- .kpi(
      "dates_within_period", "na", detail = "statement period not available")
  }

  # 5. no_unparsed_rows: every non-empty source data line became a transaction.
  # Completeness is proven by comparing the count of non-empty PHYSICAL source
  # data lines against parsed rows -- computing it from the parsed table alone
  # (n vs n-malformed) can never see a record that was merged/lost, so a stray
  # cross-line quote would silently pass. `source_line_count` is threaded from
  # the reader; NA (excel/pdf) falls back to the malformed-only check.
  malformed <- sum(grepl("malformed", tx$flags))
  src_lines <- suppressWarnings(as.integer(parsed$source_line_count %||% NA))
  lost <- if (!is.na(src_lines)) src_lines - n else 0L
  good <- n - malformed
  ok_rows <- (malformed == 0) && (lost == 0)
  expected_rows <- if (!is.na(src_lines)) src_lines else n
  rows$no_unparsed_rows <- .kpi(
    "no_unparsed_rows", if (ok_rows) "pass" else "fail",
    expected = expected_rows, actual = good,
    discrepancy = expected_rows - good,
    detail = if (lost > 0)
      sprintf("%d source line(s) unaccounted for; %d malformed row(s)", lost, malformed)
    else if (is.na(src_lines))
      sprintf(paste0("%d malformed row(s). Note: for this format the total source ",
                     "line count is not independently known, so rows dropped by ",
                     "column/date filtering are NOT counted here -- rely on ",
                     "balance_reconciliation for completeness."), malformed)
    else sprintf("%d malformed row(s)", malformed))

  # 6. redaction_summary: informational count of redacted rows.
  redacted <- sum(grepl("redacted", tx$flags))
  rows$redaction_summary <- .kpi(
    "redaction_summary", "na", expected = NA, actual = redacted,
    discrepancy = NA, detail = sprintf("%d redacted row(s)", redacted),
    informational = TRUE)

  # 7. ocr_confidence: informational -- was any page machine-read (OCR), and how
  # confident was the worst page? OCR is never 100% accurate, so this must be
  # visible to a forensic reviewer alongside the confidence figure. It does not
  # move the score (it is informational) but it DOES cap the trust level below --
  # an OCR'd statement is never rated "high".
  ocr_pages <- suppressWarnings(as.integer(h$ocr_pages %||% 0L))
  if (is.na(ocr_pages)) ocr_pages <- 0L
  ocr_conf <- suppressWarnings(as.numeric(h$ocr_min_confidence %||% NA))
  if (ocr_pages > 0) {
    rows$ocr_confidence <- .kpi(
      "ocr_confidence", "na", expected = NA,
      actual = if (is.na(ocr_conf)) sprintf("%d page(s) OCR-read", ocr_pages)
               else sprintf("%d page(s) OCR-read, min page confidence %.0f%%", ocr_pages, ocr_conf),
      discrepancy = NA,
      detail = "machine-read (OCR) text is not guaranteed 100% accurate -- verify amounts and descriptions against the source PDF",
      informational = TRUE)
  }

  kpis <- do.call(rbind, rows)
  rownames(kpis) <- NULL

  # ---- deterministic trust ----
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
      "completeness UNVERIFIED: no balance or stated count to reconcile against, so a dropped/missing transaction cannot be detected automatically — check the row count against the statement")
  }

  # Unresolved-year caveat: rows kept with date_unresolved carry a verbatim
  # day/month but NO year (no statement period was found). Say so explicitly --
  # the money is preserved but the dates are not fully known.
  n_dateunres <- sum(grepl("date_unresolved", tx$flags))
  if (n_dateunres > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons, sprintf(
      "%d row(s) have an UNRESOLVED year (no statement period found); day and month are captured verbatim but the year could not be determined — assign it before relying on the dates",
      n_dateunres))
  }

  # OCR caveat (forensic): a statement where ANY page was machine-read by OCR is
  # never rated "high" -- OCR is not guaranteed accurate, and reconciliation math
  # only cross-checks amounts, not the verbatim descriptions. Always surface that
  # OCR was used and the confidence figure so the reviewer verifies the source.
  if (ocr_pages > 0) {
    if (identical(level, "high")) level <- "medium"
    reasons <- c(reasons, sprintf(
      "%d page(s) were read by OCR%s; machine-read text is not guaranteed 100%% accurate — verify amounts and descriptions against the source PDF",
      ocr_pages,
      if (is.na(ocr_conf)) " (page confidence could not be measured)"
      else sprintf(" (min page confidence %.0f%%)", ocr_conf)))
    # A per-cell flag is stronger than the page mean: it points at the exact rows
    # whose date/amount/balance may have been misread.
    n_ocrlow <- sum(grepl("ocr_low_conf", tx$flags))
    if (n_ocrlow > 0)
      reasons <- c(reasons, sprintf(
        "%d row(s) have a LOW-CONFIDENCE OCR value in a date/amount/balance cell — check those cells against the source", n_ocrlow))
  }

  # Return KPIs without the internal informational flag column exposed downstream.
  kpis_out <- kpis[, c("name", "status", "expected", "actual", "discrepancy", "detail")]

  list(kpis = kpis_out,
       trust = list(level = level, score = score, reasons = reasons,
                    completeness_verified = completeness_verified,
                    ocr_pages = ocr_pages, ocr_min_confidence = ocr_conf))
}
