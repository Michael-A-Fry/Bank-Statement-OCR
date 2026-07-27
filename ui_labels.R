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
# "unsupported" covers two OPPOSITE situations, and one headline cannot say both.
# Nothing fit -> a layout we have genuinely never seen, so go and build a template.
# Two or more fit equally AND the one the tool used read nothing -> we already HAVE
# templates for this statement, and the screen must not tell the analyst to build a
# third; it offers the pick instead (cv_tie_pick).
#
# ONLY on that path. An ordinary tie CONVERTS: R/convert.R picks deterministically
# (tested over hand-built), reads the statement and holds it at needs_review. This
# headline was replacing "Converted - please double-check it" on those runs and
# demanding a pick from a screen that had no picker on it, while the confidence
# grade the engine had already computed was thrown away beside it.
STATUS_PLAIN_AMBIGUOUS <- "More than one template fits - pick which one"
# ONE entry per check reconcile() can emit (R/reconcile.R -- the list at the
# bottom of that file is the authoritative set). A check with no entry here shows
# its raw code on screen, which is the moment a forensic reviewer stops trusting
# the screen; test-seams.R fails the suite if one is missing.
#
# EACH LABEL SAYS ONLY WHAT ITS CHECK ACTUALLY PROVES. Three used to claim more:
#   * "Every row was read" -- .kpi_no_unparsed_rows compares the count of physical
#     source lines against parsed rows. It never looks at a cell, so it passed on a
#     file where 499 of 500 dates were gone. It proves no row FAILED to read.
#   * "Row count matches the statement" -- with no printed count the check degrades
#     to n > 0 and reports pass, so on most statements the headline asserted a match
#     against a number that does not exist. The topic, not a claim; the Expected /
#     Read columns beside it carry the figures.
#   * "Redactions found and honoured" -- .kpi_redaction_summary is a COUNT
#     (`sum(grepl("redacted", flags))`). Honouring is proved by redaction_scan,
#     which is a separate check.
CHECK_PLAIN <- c(
  balance_reconciliation     = "Opening + transactions = closing balance",
  running_balance_continuity = "Each running balance follows from the last",
  amount_direction           = "Money in / money out is the right way round",
  transaction_count          = "Row count",
  dates_within_period        = "All dates fall in the statement period",
  dates_readable             = "Row dates could be read",
  no_unparsed_rows           = "No row failed to read",
  redaction_summary          = "Redactions found",
  ocr_confidence             = "Scan / OCR read quality",
  redaction_scan             = "Nothing hidden under a redaction was read")
# plain_check(names) -- CHECK_PLAIN, but aware of auto-split. An auto-split run
# tags every KPI "<name> [statement 2]", which matches nothing in the map, so a
# split bundle showed RAW CODES for every check on the page. Strip the tag, look
# the check up, put the statement back in words.
plain_check <- function(x) {
  x <- as.character(x)
  base <- sub("[[:space:]]*\\[statement ([0-9]+)\\]$", "", x)
  n <- sub("^.*\\[statement ([0-9]+)\\]$", "\\1", x)
  lab <- plain_label(base, CHECK_PLAIN)
  tagged <- n != x
  lab[tagged] <- sprintf("%s (statement %s)", lab[tagged], n[tagged])
  lab
}
COVERAGE_PLAIN <- c(populated = "present", partial = "some rows empty",
                    empty = "empty (check the mapping)", unmapped = "not on this statement")
# How a single check came out (the `status` column of the KPI table). Lived inline
# in app.R, which meant one of the five wording maps was somewhere else; all five
# are here now, and test-seams.R holds each to the values the engine really emits.
#
# "na" USED TO SAY "not on this statement" -- the same words as COVERAGE_PLAIN's
# `unmapped` above, where they are right (a field this statement does not have) and
# here they were not. A scanned statement was OCR'd on two pages at 88% confidence
# and the Scan / OCR row read "not on this statement"; the balance checks read the
# same over a statement that prints both balances. The status means the check could
# not be PROVED, which is what the grey dash in the proof strip already means, so
# both places on that one screen now say the same thing.
RESULT_PLAIN <- c(pass = "OK", fail = "Problem", na = "could not be checked")
# ...except for the INFORMATIONAL checks: redaction_summary and ocr_confidence are
# COUNTS the engine read successfully, never checks that could not run. Both come
# back with status "na" because there is nothing to pass or fail, so on the old
# wording a statement OCR'd on two pages at 88% confidence had its "Scan / OCR read
# quality" row say "not on this statement" -- with the OCR detail printed beside it.
#
# Named, not read off the row: reconcile() marks them (.kpi(..., informational =
# TRUE)) and then DROPS that column before returning (R/reconcile.R's last line),
# so the result the screen holds does not carry the flag. test-seams.R reads
# reconcile.R for the KPIs that really set it, so this list cannot fall behind the
# engine without the suite saying so -- and app.R still prefers a real
# `informational` column if the engine ever starts exposing one.
RESULT_PLAIN_INFO <- "for information"
INFORMATIONAL_CHECKS <- c("redaction_summary", "ocr_confidence")
# Diagnostics 'category' codes -> plain words for the customer-facing table
# (the codes themselves stay in the logs / workbook Diagnostics sheet). ONE entry
# per category the engine can raise -- the authoritative set is .DIAG_FIX_OWNER
# in R/diagnose.R, and test-seams.R fails the suite if this map falls behind it.
DIAG_PLAIN <- c(
  unknown_format          = "layout not recognised",
  ambiguous_template      = "more than one template fits",
  matched_but_empty       = "matched the wording, read no transactions",
  unreadable              = "file could not be read",
  scanned_no_ocr          = "a scan with no readable text",
  document_provenance     = "what the PDF says about itself",
  multiple_statements     = "several statements in one file",
  combined_statement      = "several accounts in one statement",
  mixed_currency          = "more than one currency",
  oversized               = "unusually large file",
  oversized_page          = "unusually large page",
  reconciliation_mismatch = "the balance doesn't add up",
  balance_break           = "running balance jumps",
  row_count               = "row count doesn't match",
  # Three different situations share this one code: dates outside the statement
  # period, dates outside the matched template's declared valid range, and a year
  # inferred from a stray number on the page. "outside the period" described only
  # the first, so the other two read as a headline about something that hadn't
  # happened. The detail always says which; the headline now covers all three.
  date_out_of_range       = "date range doesn't look right",
  date_format_mismatch    = "dates in a different style than expected",
  row_parse               = "rows didn't parse",
  date_parse              = "dates couldn't be read",
  amount_parse            = "amounts couldn't be read",
  # NOT amount_parse. The amounts were read perfectly; it is their DIRECTION that
  # may be inverted, and "amounts couldn't be read" is the opposite of what
  # happened - on the one line a forensic accountant acts on.
  amount_direction        = "money in / out may be the wrong way round",
  completeness_unverified = "completeness not auto-verified",
  redaction               = "redactions found and kept",
  redaction_unverified    = "hidden text could not be checked",
  low_ocr_confidence      = "scan read with low confidence",
  ocr                     = "page(s) machine-read (OCR)",
  ocr_confidence_unknown  = "scan quality unknown",
  none                    = "no issues found")
plain_status <- function(s) { s <- s %||% "?"; v <- STATUS_PLAIN[s]; if (is.na(v)) toupper(s) else unname(v) }
plain_label  <- function(x, map) { out <- unname(map[x]); ifelse(is.na(out), x, out) }
# plain_failing_check(x) -- the batch table's "What to check" column, in words.
#
# convert_batch() (R/batch.R) carries the engine's own CODE with the map that
# words it as a prefix: "check:balance_reconciliation", "diag:unknown_format",
# "status:needs_review". The engine deliberately does NOT do this translation --
# it used to read this very file off disk with sys.source() to get the wording,
# for a bare-console caller that does not exist. The words belong here, with the
# other four wording maps, and only the app has them loaded.
#
# The prefix is not decoration. The three key sets are disjoint today, but a code
# that appeared in two of them would render the wrong sentence with nothing to
# notice it -- so the map is named rather than guessed.
#
# "Failed: " on a check because CHECK_PLAIN words a check as what it PROVES ("Row
# dates could be read"), for use beside a separate pass/fail column. This table
# has no such column, so the bare phrase would say the OPPOSITE of what happened.
#
# NOT plain_check(): that re-appends "(statement N)", and batch.R strips that tag
# precisely so a split bundle's files group together under one kind of failure.
# An unknown code falls back to itself -- never to a blank cell, which would hide
# a file that needs attention.
plain_failing_check <- function(x) vapply(x, function(e) {
  if (is.na(e)) return(NA_character_)
  code <- sub("^[^:]*:", "", e)
  switch(sub(":.*$", "", e),
         check  = paste0("Failed: ", plain_label(code, CHECK_PLAIN)),
         diag   = plain_label(code, DIAG_PLAIN),
         status = plain_label(code, STATUS_PLAIN),
         e)
}, character(1), USE.NAMES = FALSE)

# ---------------------------------------------------------------------------
# The governed feed, said out loud. write_feed() (R/feed.R) decides whether a
# conversion's rows reach the Qlik dashboards, records the verdict in the feed
# manifest and its own log -- and, until now, the person who ran the conversion
# was never told. So a clean statement read by a template SHE built was withheld
# ("not_proven") with nothing on screen saying so, and she had every reason to
# believe her figures were on the dashboard. One entry per reason .feed_gate()
# can return; test-seams.R fails the suite if the engine grows one this map
# doesn't have.
#
# `why` may be "" -- the line already says it, and cv_feed drops an empty one. It
# is not a place to restate the verdict in longer words.
# `withheld:needs_review` used to promise "Fix what is flagged above and convert
# again, and it goes through". Passing the checks clears only the FIRST gate: a
# statement read by a template built here is then withheld again as `not_proven`
# (R/feed.R, allowed_template_origins), which only a data analyst can change. An
# unconditional promise about a conditional gate is the one thing this note exists
# to stop, so it states the rule and stops there.
FEED_PLAIN <- list(
  accepted = list(
    ok = TRUE, line = "Sent to the dashboards.", why = ""),
  `withheld:needs_review` = list(
    ok = FALSE, line = "Held back from the dashboards - it needs checking first.",
    why = "Only conversions that pass every check are published."),
  `withheld:unsupported` = list(
    ok = FALSE, line = "Nothing was sent to the dashboards.",
    why = "No template read this statement, so there are no transactions to publish."),
  `withheld:failed` = list(
    ok = FALSE, line = "Nothing was sent to the dashboards.",
    why = "The file could not be read."),
  `withheld:low_trust` = list(
    ok = FALSE, line = "Held back from the dashboards - the confidence level is below what they accept.",
    why = ""),
  `withheld:not_proven` = list(
    ok = FALSE, line = "Held back from the dashboards - it was read by a template built here.",
    why = "Only shipped, tested templates feed them. Your download is complete; ask your data analyst to promote the template."),
  `withheld:not_in_allowlist` = list(
    ok = FALSE, line = "Held back from the dashboards - this template isn't on their list.",
    why = "Ask your data analyst to add it."))
FEED_PLAIN_UNKNOWN <- list(
  ok = FALSE, line = "Held back from the dashboards.",
  why = "The feed recorded a reason this screen doesn't have wording for; it is in the feed log.")
# The two things that can go wrong AFTER the verdict: the feed folder refused the
# write, or there were no rows to send. Both are appended to the reason by
# write_feed(), and both mean the dashboards did not get this run.
FEED_PLAIN_SUFFIX <- c(
  write_failed = "The feed folder could not be written to, so the dashboards did NOT receive this run. Tell whoever looks after the server.",
  no_rows      = "No transaction rows were produced, so nothing was sent.")
# plain_feed(gate) -- write_feed()'s return value -> list(ok, line, why), or NULL
# when there is nothing to say (feed switched off, or a form result).
plain_feed <- function(gate) {
  if (is.null(gate) || is.null(gate$reason)) return(NULL)
  out <- FEED_PLAIN[[as.character(gate$reason)[1]]] %||% FEED_PLAIN_UNKNOWN
  suffix <- sub("^.*:", "", as.character(gate$gate_result %||% "")[1])
  extra <- unname(FEED_PLAIN_SUFFIX[suffix])
  if (!is.na(extra)) { out$ok <- FALSE; out$why <- trimws(paste(out$why, extra)) }
  out
}
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
  type = "Type", other_party = "Other party", currency = "Currency", flags = "Flags",
  # An auto-split bundle stamps every row with the statement it came from.
  statement_index = "Statement #")
# The fallback here is load-bearing, and `[[` was quietly the wrong bracket for
# it: on a NAMED CHARACTER VECTOR, x[["not_a_name"]] does not return NULL, it
# THROWS. So any column the map had not met -- a template's `extras` (fx_amount,
# conversion_charge), or the statement_index an auto-split bundle adds -- took
# the whole transactions table down with a subscript error, and the payoff of the
# page rendered as nothing at all. Single brackets give NA for a name that is not
# there, which is what the fallback was always written to expect.
cv_friendly_cols <- function(cols) vapply(cols, function(cn) {
  lab <- unname(CV_COL_LABELS[cn])
  if (is.na(lab)) tools::toTitleCase(gsub("_", " ", cn)) else lab
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
  "We couldn't read this file. It may be password-protected, an image-only scan,",
  "or not a statement. Try a PDF or CSV export from your bank.")
