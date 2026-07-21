# diagnose.R -- fail-loud diagnostics. For any run that isn't perfectly clean,
# produce a structured explanation: WHERE it happened, WHY (category), HOW BAD
# (severity), the detail, and HOW TO FIX it. The goal is never a silent wrong
# answer: if the engine can't be fully confident, it says exactly why and what
# to do about it.
#
# build_diagnostics(status, messages, det, parsed, recon) -> data.frame with
# columns: where, category, severity, detail, how_to_fix (most severe first).

.diag_row <- function(where, category, severity, detail, how_to_fix) {
  data.frame(where = where, category = category, severity = severity,
             detail = detail, how_to_fix = how_to_fix, stringsAsFactors = FALSE)
}

# .diag_fix_owner(category) -- WHO fixes this, so a lone analyst never wonders
# whether to draw a box or phone a developer:
#   template = the analyst, in the wizard (a column/box/date/amount setting)
#   input    = the person who supplied the file (split a bundle, re-export, rescan)
#   review   = just eyeball the data (expected situation, not an error)
#   none     = informational, no action
#   escalate = a genuine engine gap -> send it to a developer (rare)
# Unknown categories default to escalate (fail safe: surface it, don't hide it).
.diag_fix_owner <- function(category) {
  vapply(category, function(c) switch(c,
    unknown_format = , reconciliation_mismatch = , balance_break = ,
    row_count = , row_parse = , date_parse = , amount_parse = ,
    date_out_of_range = "template",
    unreadable = , multiple_statements = , oversized = , oversized_page = ,
    low_ocr_confidence = , completeness_unverified = "input",
    combined_statement = , mixed_currency = "review",
    redaction = , ocr = , none = "none",
    "escalate"), character(1))
}

# Compact a set of row indices for display.
.rng <- function(idx) {
  if (!length(idx)) return("")
  if (length(idx) <= 8) return(paste(idx, collapse = ","))
  paste0(paste(utils::head(idx, 8), collapse = ","), ",... (", length(idx), " total)")
}

build_diagnostics <- function(status, messages = character(0), det = NULL,
                              parsed = NULL, recon = NULL, metadata = NULL) {
  rows <- list()
  add <- function(where, category, severity, detail, how_to_fix)
    rows[[length(rows) + 1L]] <<- .diag_row(where, category, severity, detail, how_to_fix)

  if (identical(status, "unsupported")) {
    add("detection", "unknown_format", "high",
        det$detail %||% "no template matched this file",
        paste("Add a template for this layout in the template toolkit (Add a template tab:",
              "upload a sample and confirm what it detects). The closest match and the missing columns are in the detail."))
  } else if (identical(status, "failed")) {
    add("file", "unreadable", "high",
        paste(messages, collapse = " "),
        paste("Check the file opens, is the expected type (CSV / PDF / Excel),",
              "and is not password-protected or corrupt."))
  }

  if (!is.null(metadata)) {
    if (isTRUE(metadata$multi$likely_multiple))
      add("upload", "multiple_statements", "high",
          paste(metadata$multi$reasons, collapse = "; "),
          "This upload looks like more than one statement bundled together, which corrupts a single parse. Split it into one statement per file and re-run.")
    else if (isTRUE(metadata$multi$combined_accounts))
      add("upload", "combined_statement", "info",
          sprintf("%d account numbers appear in one statement period", metadata$multi$n_accounts %||% 0L),
          "Looks like a combined statement (several accounts/products, or transfer counterparties named in transactions). If transactions from more than one account are mixed, running balances won't be continuous across them - review per account.")
    p <- suppressWarnings(as.integer(metadata$pages %||% NA))
    if (!is.na(p) && p > 100)
      add("upload", "oversized", "medium",
          sprintf("%d pages in one file", p),
          "Very long PDFs (>100 pages) may hit tool limits; split into smaller files if extraction stalls.")
    mp <- suppressWarnings(as.numeric(metadata$max_page_pt %||% NA))
    if (!is.na(mp) && mp > 2880)
      add("upload", "oversized_page", "medium",
          sprintf("largest page is %.0f pt (> 2880 pt / 40 in)", mp),
          "Pages larger than 40 inches (2880 pt) can break rendering/OCR. Re-export at a standard page size.")
  }

  if (!is.null(parsed) && !is.null(parsed$transactions)) {
    tx <- parsed$transactions

    # 1. Failing reconciliation KPIs -> a fix per check.
    if (!is.null(recon) && !is.null(recon$kpis)) {
      k <- recon$kpis
      fails <- k[k$status == "fail", , drop = FALSE]
      for (i in seq_len(nrow(fails))) {
        nm <- fails$name[i]
        info <- switch(nm,
          balance_reconciliation = c("balance check", "reconciliation_mismatch", "high",
            "Statement doesn't reconcile: a transaction may be mis-signed, missing, or the opening/closing balance is wrong. Compare the total against the source."),
          running_balance_continuity = c("running balance", "balance_break", "high",
            "Running balance jumps: a row's amount or sign is likely wrong, or a transaction is missing. Check the rows around the break."),
          transaction_count = c("parse", "row_count", "high",
            "Parsed count doesn't match: the template rows/columns may not fit this file. Re-map it in the template toolkit."),
          dates_within_period = c("dates", "date_out_of_range", "medium",
            "Dates fall outside the statement period: the date-format mapping may be wrong (day/month vs month/day)."),
          dates_readable = c("dates", "date_parse", "high",
            "No row dates could be read: the template's date column wasn't found in this file (renamed header?) or the date format is wrong. Fix the Date column / format in the template toolkit."),
          no_unparsed_rows = c("rows", "row_parse", "high",
            "Some source lines didn't parse (malformed or lost): usually a delimiter/quoting issue, or a preamble/footer line read as data. Check those rows."),
          c("check", "reconciliation_mismatch", "medium",
            "Review this check against the source statement."))
        add(info[1], info[2], info[3], fails$detail[i], info[4])
      }
    }

    # 1b. Completeness cannot be verified (no balance / stated count).
    if (!is.null(recon) && isFALSE(recon$trust$completeness_verified) && nrow(tx) > 0)
      add("completeness", "completeness_unverified", "medium",
        "no balance or stated count to reconcile against",
        "The engine can't confirm every transaction was captured (nothing to reconcile the total against). Count the rows against the statement, or prefer a CSV/Excel export or a statement that shows a running balance.")

    # 2. Row-level parse problems (independent of KPI wiring).
    mal <- which(grepl("malformed", tx$flags %||% ""))
    if (length(mal)) add(sprintf("rows %s", .rng(mal)), "row_parse", "high",
      sprintf("%d row(s) had the wrong number of fields", length(mal)),
      "Wrong field count: check the delimiter/quoting, or a preamble/footer line was read as data.")

    dalt <- which(grepl("date_alt_format", tx$flags %||% ""))
    if (length(dalt)) add(sprintf("rows %s (date)", .rng(dalt)), "date_format_mismatch", "medium",
      sprintf("%d date(s) were written in a different style than the template declares", length(dalt)),
      "Rows like '17 Sep' were read with the year taken from the statement period. Update the template's date format in the toolkit to make this explicit.")

    dbad <- which(is.na(tx$date) & !is.na(tx$date_raw) & nzchar(tx$date_raw %||% ""))
    if (length(dbad)) add(sprintf("rows %s (date)", .rng(dbad)), "date_parse", "medium",
      sprintf("%d date(s) could not be read", length(dbad)),
      "The date-format mapping is likely wrong for these rows. Set the correct format in the template toolkit (e.g. day/month/year).")

    abad <- which(is.na(tx$amount) & !grepl("redacted", tx$flags %||% ""))
    if (length(abad)) add(sprintf("rows %s (amount)", .rng(abad)), "amount_parse", "high",
      sprintf("%d amount(s) could not be read", length(abad)),
      "The amount style/format is wrong: check the amount style (signed vs D/C vs debit/credit columns) and the thousands/decimal separators.")

    # 3. Informational context.
    cur <- unique(tx$currency[!is.na(tx$currency)])
    if (length(cur) > 1) add("currency", "mixed_currency", "info",
      sprintf("multiple currencies present: %s", paste(cur, collapse = ", ")),
      "Foreign-currency lines are present. Confirm downstream handling of non-base currencies.")

    red <- which(grepl("redacted", tx$flags %||% ""))
    if (length(red)) add(sprintf("rows %s", .rng(red)), "redaction", "info",
      sprintf("%d redacted value(s), kept as shown", length(red)),
      "Redactions are intentional; values are left as [REDACTED]. No action needed.")

    ocrp <- suppressWarnings(as.integer(parsed$header$ocr_pages %||% NA))
    if (!is.na(ocrp) && ocrp > 0) add("pages (OCR)", "ocr", "info",
      sprintf("%d page(s) were machine-read via OCR", ocrp),
      "OCR pages can contain recognition errors. Spot-check machine-read values against the image.")
    ocrc <- suppressWarnings(as.numeric(parsed$header$ocr_min_confidence %||% NA))
    if (!is.na(ocrc) && ocrc < 70) add("OCR text", "low_ocr_confidence", "high",
      sprintf("lowest page-mean OCR confidence was %.0f%%", ocrc),
      "OCR is unsure of some characters. Re-scan at higher DPI/contrast, or verify the flagged pages against the image; reconciliation still guards the totals. Rows with a doubtful cell carry an 'ocr_low_conf' flag.")
    # OCR ran but confidence could not be measured (the TSV pass failed): a
    # distinct caveat, since the generic info note above understates it.
    if (!is.na(ocrp) && ocrp > 0 && is.na(ocrc)) add("OCR text", "ocr_confidence_unknown", "high",
      "OCR ran but its confidence could not be measured",
      "Treat the machine-read values as unverified and check them against the image.")
  }

  if (!length(rows)) {
    clean <- .diag_row("-", "none", "info",
                       "No issues detected; all applicable checks passed.", "-")
    clean$fix_owner <- "none"
    return(clean)
  }

  out <- do.call(rbind, rows)
  out$fix_owner <- .diag_fix_owner(out$category)
  out[order(match(out$severity, c("high", "medium", "info"))), , drop = FALSE]
}

# diag_fix_owner_label(owner) -- plain-language "who fixes this" for display.
diag_fix_owner_label <- function(owner) {
  unname(c(
    template = "You - adjust the template (toolkit)",
    input    = "You - fix the file (split / re-export / rescan)",
    review   = "You - review the data (expected, not an error)",
    none     = "No action",
    escalate = "Developer - engine gap (escalate)"
  )[owner])
}
