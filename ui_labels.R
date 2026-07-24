# ui_labels.R -- the plain-English WORDING a non-technical user (Beth) sees, in ONE
# place, plus the small pure helpers that shape the transactions preview. The
# engine's internal codes (needs_review, balance_reconciliation, ...) stay in the
# logs; these maps turn them into the sentences on screen. This is COPY, not logic --
# reword it freely. Sourced by app.R after the engine (needs %||% from R/util.R) and
# before ui_content.R.

# Plain-English labels for the everyday screen. The engine's internal codes
# (needs_review, balance_reconciliation, ...) stay in the logs; a non-technical
# user only ever sees these sentences.
STATUS_PLAIN <- c(
  ok           = "Converted successfully",
  needs_review = "Converted - please double-check it",
  unsupported  = "No template for this statement yet",
  failed       = "Could not read this file")
CHECK_PLAIN <- c(
  balance_reconciliation     = "Opening + transactions = closing balance",
  running_balance_continuity = "Each running balance follows from the last",
  transaction_count          = "Row count matches the statement",
  dates_within_period        = "All dates fall in the statement period",
  dates_readable             = "Row dates could be read",
  no_unparsed_rows           = "Every row was read",
  redaction_summary          = "Redactions found and honoured",
  ocr_confidence             = "Scan / OCR read quality")
COVERAGE_PLAIN <- c(populated = "present", partial = "some rows empty",
                    empty = "empty (check the mapping)", unmapped = "not on this statement")
# Diagnostics 'category' codes -> plain words for the customer-facing table
# (the codes themselves stay in the logs / workbook Diagnostics sheet).
DIAG_PLAIN <- c(
  unknown_format          = "layout not recognised",
  unreadable              = "file could not be read",
  multiple_statements     = "several statements in one file",
  combined_statement      = "several accounts in one statement",
  mixed_currency          = "more than one currency",
  oversized               = "unusually large file",
  oversized_page          = "unusually large page",
  reconciliation_mismatch = "the balance doesn't add up",
  balance_break           = "running balance jumps",
  row_count               = "row count doesn't match",
  date_out_of_range       = "date outside the period",
  date_format_mismatch    = "dates in a different style than expected",
  row_parse               = "rows didn't parse",
  date_parse              = "dates couldn't be read",
  amount_parse            = "amounts couldn't be read",
  completeness_unverified = "completeness not auto-verified",
  low_ocr_confidence      = "scan read with low confidence",
  ocr                     = "page(s) machine-read (OCR)",
  ocr_confidence_unknown  = "scan quality unknown")
plain_status <- function(s) { s <- s %||% "?"; v <- STATUS_PLAIN[s]; if (is.na(v)) toupper(s) else unname(v) }
plain_label  <- function(x, map) { out <- unname(map[x]); ifelse(is.na(out), x, out) }
# Human-readable HEADERS for the transactions preview. The stored core schema uses
# machine names that read as an internal tool to a forensic reviewer, so relabel
# for DISPLAY. The verbatim *_raw cells no longer surface here (they live in the
# JSON + Provenance); debit/credit appear when a statement splits money in / out.
CV_COL_LABELS <- c(
  row_id = "#", date = "Date", date_raw = "Date (as shown)",
  description = "Description", amount = "Amount", amount_raw = "Amount (as shown)",
  debit = "Debit (money out)", credit = "Credit (money in)",
  direction = "In / out", balance = "Balance", balance_raw = "Balance (as shown)",
  particulars = "Particulars", code = "Code", reference = "Reference",
  type = "Type", other_party = "Other party", currency = "Currency", flags = "Flags")
cv_friendly_cols <- function(cols) vapply(cols, function(cn) {
  lab <- CV_COL_LABELS[[cn]]
  if (is.null(lab)) tools::toTitleCase(gsub("_", " ", cn)) else lab
}, character(1), USE.NAMES = FALSE)
# .cols_with_data(df) -- names of columns carrying at least one non-blank value.
# Used to trim always-empty columns (a field this statement doesn't have) from the
# previews so the reviewer sees only what was actually read. row_id is always kept.
.cols_with_data <- function(df, always = "row_id") {
  keep <- vapply(df, function(c) any(!is.na(c) & nzchar(trimws(as.character(c)))), logical(1))
  union(intersect(always, names(df)), names(df)[keep])
}
# The friendly line shown when a file simply can't be read (technical detail -> log).
FRIENDLY_READ_ERROR <- paste(
  "We couldn't read this file. It may be password-protected, an image-only scan we can't open,",
  "or not a bank statement. Try re-saving it as a PDF or CSV, or open the template toolkit to set it up.")
