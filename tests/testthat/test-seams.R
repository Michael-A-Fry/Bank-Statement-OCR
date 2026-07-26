# test-seams.R -- the contracts BETWEEN the engine and the screen.
#
# Most of this repo's tests hold one half to its word. These hold the JOIN: every
# code the engine can emit has wording on screen, every wording on screen is for a
# code the engine can still emit, and the one thing the engine decides silently --
# whether a conversion reaches the dashboards -- is said out loud.
#
# The failure they exist to prevent is quiet and specific: the engine grows a new
# check, category or gate reason, nothing errors, and a forensic reviewer is shown
# a raw snake_case code where a sentence should be. That is not a crash, so only a
# test that reads BOTH sides can catch it.

# ui_labels.R holds the user-facing wording and is not part of the R/ engine, so
# the suite does not load it; source it into this file's environment (it needs
# only %||%, which the engine provides).
.ui_labels <- function() {
  f <- file.path(engine_root(), "ui_labels.R")
  skip_if_not(file.exists(f))
  e <- new.env(parent = globalenv())
  sys.source(f, envir = e)
  e
}
.src <- function(rel) {
  f <- file.path(engine_root(), rel)
  skip_if_not(file.exists(f))
  readLines(f, warn = FALSE)
}

# ---------------------------------------------------------------------------
# CHECKS. reconcile() names its checks in one list at the bottom of R/reconcile.R;
# CHECK_PLAIN (ui_labels.R) turns each into the sentence in the Checks table and
# in the "What to check" list on the verdict card. `amount_direction` and
# `redaction_scan` -- the two failures a forensic reviewer most needs to read --
# were missing, so they rendered as codes.
test_that("every check the engine can emit has plain-English wording", {
  L <- .ui_labels()
  src <- .src("R/reconcile.R")
  # the builder list inside reconcile(): "  name = .kpi_name(...)"
  kpis <- unique(sub("^\\s*([a-z_]+)\\s*=\\s*\\.kpi_.*$", "\\1",
                     grep("^\\s*[a-z_]+\\s*=\\s*\\.kpi_", src, value = TRUE)))
  expect_gte(length(kpis), 8L)                       # the scan must not go quiet
  expect_true("balance_reconciliation" %in% kpis)
  expect_setequal(kpis, names(L$CHECK_PLAIN))        # BOTH ways: no gaps, no dead entries
})

# An auto-split run tags every KPI "<name> [statement N]", which matches nothing
# in CHECK_PLAIN -- so a split bundle showed a raw code for EVERY check on screen.
test_that("a split bundle's checks are still named in plain English", {
  L <- .ui_labels()
  got <- L$plain_check(c("balance_reconciliation",
                         "dates_readable [statement 2]",
                         "a_check_that_does_not_exist"))
  expect_identical(got[1], unname(L$CHECK_PLAIN[["balance_reconciliation"]]))
  expect_identical(got[2], paste0(L$CHECK_PLAIN[["dates_readable"]], " (statement 2)"))
  expect_identical(got[3], "a_check_that_does_not_exist")   # unknown falls back, never NA
})

# ---------------------------------------------------------------------------
# DIAGNOSTICS. .DIAG_FIX_OWNER (R/diagnose.R) is the authoritative set of
# categories the engine can raise -- it is already asserted complete against the
# file in test-diagnose.R -- so DIAG_PLAIN must cover exactly it. Five live
# categories were missing (scanned_no_ocr fires on every unreadable scan,
# document_provenance on any PDF written by a general-purpose tool).
test_that("every diagnostic category the engine can raise has plain-English wording", {
  L <- .ui_labels()
  expect_setequal(names(.DIAG_FIX_OWNER), names(L$DIAG_PLAIN))
})

test_that("every result status has plain-English wording", {
  L <- .ui_labels()
  expect_setequal(c("ok", "needs_review", "unsupported", "failed"), names(L$STATUS_PLAIN))
})

# The other two small maps, held to the values the engine really emits: a KPI's
# status (R/reconcile.R's .kpi()) and a field's coverage verdict (R/coverage.R).
test_that("check results and coverage verdicts are wording for real engine values", {
  L <- .ui_labels()
  expect_setequal(c("pass", "fail", "na"), names(L$RESULT_PLAIN))
  expect_setequal(c("populated", "partial", "empty", "unmapped"), names(L$COVERAGE_PLAIN))
  # ...and the engine really does produce those coverage verdicts.
  verdicts <- unique(unlist(regmatches(.src("R/coverage.R"),
    gregexpr("\"(populated|partial|empty|unmapped)\"", .src("R/coverage.R")))))
  expect_true(length(verdicts) >= 4L)
})

# ---------------------------------------------------------------------------
# THE GOVERNED FEED. write_feed() decides whether a conversion's figures reach the
# Qlik dashboards, records the verdict in the manifest and its own log -- and the
# person who ran the conversion was never told. The quiet case is the dangerous
# one: a clean statement read by a template SHE built is withheld ("not_proven"),
# which is correct governance and was completely invisible on screen.
test_that("every reason the feed gate can give has plain-English wording", {
  L <- .ui_labels()
  src <- .src("R/feed.R")
  # every literal reason .feed_gate() returns, plus the statuses it interpolates
  literal <- unique(unlist(regmatches(src, gregexpr("\"(accepted|withheld:[a-z_]+)\"", src))))
  literal <- gsub("\"", "", literal)
  expect_true(length(literal) >= 4L)
  statuses <- paste0("withheld:", c("failed", "unsupported", "needs_review"))
  for (r in c(literal, statuses)) {
    p <- L$plain_feed(list(reason = r, gate_result = r))
    expect_false(is.null(p), info = r)
    expect_false(identical(p$line, L$FEED_PLAIN_UNKNOWN$line), info = r)
  }
})

test_that("the feed note says accepted, held back, and 'the write failed' differently", {
  L <- .ui_labels()
  expect_true(L$plain_feed(list(reason = "accepted", gate_result = "accepted"))$ok)
  expect_false(L$plain_feed(list(reason = "withheld:not_proven",
                                 gate_result = "withheld:not_proven"))$ok)
  # accepted by the gate, but the feed folder refused the write: NOT a success.
  wf <- L$plain_feed(list(reason = "accepted", gate_result = "accepted:write_failed"))
  expect_false(wf$ok)
  expect_match(wf$why, "could not be written")
  expect_null(L$plain_feed(NULL))                       # feed off -> say nothing
  # An unknown reason still renders, and still reads as "held back" (fail closed).
  unk <- L$plain_feed(list(reason = "withheld:something_new", gate_result = "withheld:something_new"))
  expect_false(unk$ok)
})

# The screen must actually USE all of this. app.R is not sourced by the suite, so
# these are static assertions, the same way test-app-adoption.R does it.
test_that("the Convert screen reports the feed outcome and names failing checks", {
  src <- .src("app.R")
  joined <- paste(src, collapse = "\n")
  # the gate's verdict is kept, not discarded -- and there is exactly ONE place
  # that feeds, so what reaches Qlik and what the screen claims about Qlik always
  # come from the same run (adding a forced row re-publishes through it too).
  expect_true(any(grepl("publish_result <- function", src)))
  expect_equal(length(grep("safe\\(write_feed\\(", src)), 1L)     # ONE writer
  expect_true(any(grepl("^\\s*publish_result\\(res, record\\)", src)))       # a conversion
  expect_true(any(grepl("^\\s*publish_result\\(res, cv_recorded\\(\\)\\)", src)))  # a forced-row re-run
  expect_true(any(grepl("output\\$cv_feed <- renderUI", src)))
  expect_true(any(grepl("uiOutput\\(\"cv_feed\"\\)", src)))
  expect_true(grepl("plain_feed\\(cv_feed_gate\\(\\)\\)", joined))
  # which check failed, in the Checks table's own words, on the verdict card
  expect_true(any(grepl("failed_checks_ui <- function", src)))
  expect_true(grepl("failed_checks_ui\\(res\\)", joined))
  # ...and the checks table itself is split-aware
  expect_true(grepl("Check\\s*=\\s*plain_check\\(k\\$name\\)", joined))
  expect_false(grepl("plain_label\\(k\\$name, CHECK_PLAIN\\)", joined))
})

test_that("marking a conversion wrong says what left the dashboards", {
  src <- .src("app.R")
  joined <- paste(src, collapse = "\n")
  # submit_feedback() retracts the run's rows from the accepted feed; the screen
  # used to say only "Thanks - your feedback was recorded".
  expect_true(grepl("retracted_rows", joined))
  expect_true(grepl("withdrawn from the dashboards", joined))
  # and the retraction must target the SAME feed folder the app fed
  expect_true(grepl("config = CONFIG", joined))
})

# ---------------------------------------------------------------------------
# ONE FACT, REPORTED ONCE. A failing redaction_scan KPI and the header signal both
# raise `redaction_unverified` from the same header field; two rows each headed
# "do NOT release this output" read as two separate leaks.
test_that("a failed redaction scan is reported once, whether or not recon is passed", {
  tx <- data.frame(row_id = 1L, date = "2025-01-01", date_raw = "1/1/25",
    description = "a", amount = -1, amount_raw = "-1", direction = "debit",
    balance = NA_real_, balance_raw = NA_character_, particulars = NA_character_,
    code = NA_character_, reference = NA_character_, other_party = NA_character_,
    type = NA_character_, currency = "NZD", flags = "", stringsAsFactors = FALSE)
  parsed <- list(transactions = tx, header = list(redaction_scan_incomplete = 2L))
  recon <- reconcile(parsed, NULL)
  with_recon <- build_diagnostics("needs_review", parsed = parsed, recon = recon)
  expect_equal(sum(with_recon$category == "redaction_unverified"), 1L)
  # the surviving row is the KPI one -- it carries the "Do NOT release" instruction
  expect_match(with_recon$how_to_fix[with_recon$category == "redaction_unverified"][1],
               "Do NOT release")
  # a caller that passes no recon still gets told (build_diagnostics has such callers)
  no_recon <- build_diagnostics("needs_review", parsed = parsed)
  expect_equal(sum(no_recon$category == "redaction_unverified"), 1L)
})

# ---------------------------------------------------------------------------
# THE DICTIONARIES ARE DOCUMENTATION. Both are hand-maintained files whose
# comments explain themselves; a whole-file YAML dump silently deletes all of it.
test_that("teaching a dictionary a wording keeps the file's comments", {
  p <- tempfile(fileext = ".yaml")
  writeLines(c("# the header that explains this file",
               "# and must survive an edit",
               "",
               "opening_balance:",
               "  any_of:",
               "    - \"opening balance\"",
               "    - \"previous balance\"   # credit-card wording",
               "  value: money",
               "",
               "total_credits:",
               "  any_of: [\"total credits\", \"total deposits\",",
               "           \"credits\"]",
               "  value: money"), p)
  expect_true(dictionary_append("opening_balance", "balance at start", path = p))
  expect_true(dictionary_append("total_credits", "money received", path = p))   # flow style
  expect_true(dictionary_append("ird_number", "IRD number", path = p))          # a new value
  txt <- readLines(p)
  expect_true(any(grepl("^# the header that explains this file", txt)))
  expect_true(any(grepl("# credit-card wording", txt)))
  d <- load_label_dict(p)
  expect_true("balance at start" %in% unlist(d$opening_balance$any_of))
  expect_true("money received" %in% unlist(d$total_credits$any_of))
  expect_true("IRD number" %in% unlist(d$ird_number$any_of))
  # and the point of the whole exercise: the engine now reads that wording.
  expect_equal(match_label(d$opening_balance, "Balance at start   1,234.56")$value, "1,234.56")
})

test_that("a wording already known, or one that cannot be added, is refused with a reason", {
  p <- tempfile(fileext = ".yaml")
  writeLines(c("opening_balance:", "  any_of:", "    - \"opening balance\""), p)
  again <- dictionary_append("opening_balance", "OPENING BALANCE", path = p)
  expect_true(again)                                  # not an error...
  expect_false(isTRUE(attr(again, "added")))          # ...but nothing was written
  expect_match(attr(again, "reason"), "already")
  expect_false(isTRUE(dictionary_append("opening_balance", "", path = p)))
  expect_match(attr(dictionary_append("opening_balance", "", path = p), "reason"), "type the wording")
  # a file that does not parse is left alone rather than rewritten
  broken <- tempfile(fileext = ".yaml"); writeLines(c("a: [", "  oops"), broken)
  expect_false(isTRUE(dictionary_append("a", "x", path = broken)))
  expect_match(attr(dictionary_append("a", "x", path = broken), "reason"), "does not parse")
})

test_that("approving a word into the vocabulary keeps lexicon.yaml's explanation", {
  p <- tempfile(fileext = ".yaml")
  file.copy(file.path(engine_root(), "dictionaries", "lexicon.yaml"), p)
  before <- length(grep("^#", readLines(p)))
  expect_true(before > 10L)
  expect_true(lexicon_append("debit_markers", "cow", path = p))
  expect_equal(length(grep("^#", readLines(p))), before)
  expect_true("cow" %in% lex("debit_markers", path = p))
  # a non-list category is refused, and says why in words the screen can show
  bad <- lexicon_append("money_regex", "x", path = p)
  expect_false(isTRUE(bad))
  expect_match(attr(bad, "reason"), "whole-file editor")
})

# ---------------------------------------------------------------------------
# The per-statement facts of an auto-split bundle: the engine keeps a period,
# balances, row count and confidence for EACH statement in the file, and the
# screen showed one set of cards for the whole bundle plus the WEAKEST
# statement's confidence, as if they described one statement.
test_that("the Convert screen shows an auto-split bundle statement by statement", {
  src <- .src("app.R")
  joined <- paste(src, collapse = "\n")
  expect_true(grepl("res\\$metadata\\$split\\$statements", joined))
  expect_true(any(grepl("uiOutput\\(\"cv_split\"\\)", src)))
  expect_true(grepl("split-table", joined))
})
