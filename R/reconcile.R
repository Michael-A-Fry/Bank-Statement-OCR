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
  opening <- suppressWarnings(as.numeric(h$opening_balance))
  closing <- suppressWarnings(as.numeric(h$closing_balance))
  if (!is.na(opening) && !is.na(closing) && n > 0 && !any(is.na(tx$amount))) {
    expected_close <- opening + sum(tx$amount)
    disc <- round(closing - expected_close, 2)
    rows$balance_reconciliation <- .kpi(
      "balance_reconciliation", if (abs(disc) < 0.005) "pass" else "fail",
      expected = round(expected_close, 2), actual = round(closing, 2),
      discrepancy = disc,
      detail = sprintf("opening %.2f + sum(amount) %.2f vs closing %.2f",
                       opening, sum(tx$amount), closing))
  } else {
    rows$balance_reconciliation <- .kpi(
      "balance_reconciliation", "na",
      detail = "opening/closing balance not available")
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
  ps <- h$period_start; pe <- h$period_end
  if (!is.na(ps) && !is.na(pe) && n > 0) {
    d <- suppressWarnings(as.Date(tx$date))
    within <- !is.na(d) & d >= as.Date(ps) & d <= as.Date(pe)
    outside <- sum(!within, na.rm = TRUE)
    rows$dates_within_period <- .kpi(
      "dates_within_period", if (outside == 0) "pass" else "fail",
      expected = sprintf("%s..%s", ps, pe), actual = outside,
      discrepancy = outside, detail = sprintf("%d date(s) outside period", outside))
  } else {
    rows$dates_within_period <- .kpi(
      "dates_within_period", "na", detail = "statement period not available")
  }

  # 5. no_unparsed_rows: every non-empty data row became a transaction.
  malformed <- sum(grepl("malformed", tx$flags))
  rows$no_unparsed_rows <- .kpi(
    "no_unparsed_rows", if (malformed == 0) "pass" else "fail",
    expected = n, actual = n - malformed, discrepancy = malformed,
    detail = sprintf("%d malformed row(s)", malformed))

  # 6. redaction_summary: informational count of redacted rows.
  redacted <- sum(grepl("redacted", tx$flags))
  rows$redaction_summary <- .kpi(
    "redaction_summary", "na", expected = NA, actual = redacted,
    discrepancy = NA, detail = sprintf("%d redacted row(s)", redacted),
    informational = TRUE)

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

  # Return KPIs without the internal informational flag column exposed downstream.
  kpis_out <- kpis[, c("name", "status", "expected", "actual", "discrepancy", "detail")]

  list(kpis = kpis_out,
       trust = list(level = level, score = score, reasons = reasons))
}
