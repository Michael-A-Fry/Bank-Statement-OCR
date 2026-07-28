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

# ---------------------------------------------------------------------------
# ROW FLAGS. Every transactions table on screen mapped its column HEADERS through
# cv_friendly_cols() and then printed the VALUES verbatim -- so the Convert table
# showed a Flags column reading `ocr_low_conf`, and a drafted template previewed
# nineteen rows of `date_year_inferred`, in the cells a forensic reviewer checks
# figures in. FLAG_PLAIN is the wording; this is the seam that keeps it honest.
#
# The authoritative set is the two files that WRITE the column: R/parse.R for the
# flags every path can emit, R/parse_pdf_table.R for the PDF-only ones. Read off
# the emission sites themselves rather than off a doc table, so a token the engine
# starts or stops emitting shows up here and not on screen.
.row_flag_tokens <- function() {
  src <- c(.src("R/parse.R"), .src("R/parse_pdf_table.R"))
  # the delimited path appends with `f <- c(f, "malformed")`; the PDF path with
  # `f <- add(f, cond, "no_date")`
  hits <- unlist(regmatches(src, gregexpr('(c\\(f,\\s*|add\\(f,[^,]+,\\s*)"[a-z_]+"', src)))
  toks <- sub('^.*"([a-z_]+)"$', "\\1", hits)
  # ...and the PDF path SEEDS the column with the two a redacted or unreadable row
  # carries, which no add() call names.
  seed <- grep('ifelse\\(redacted, "redacted", ifelse\\(malformed, "malformed", ""\\)\\)', src)
  unique(c(toks, if (length(seed)) c("redacted", "malformed") else character(0)))
}

test_that("every row flag the engine can emit has plain-English wording", {
  L <- .ui_labels()
  toks <- .row_flag_tokens()
  expect_gte(length(toks), 10L)                      # the scan must not go quiet
  expect_true("ocr_low_conf" %in% toks)
  expect_setequal(toks, names(L$FLAG_PLAIN))         # BOTH ways: no gaps, no dead entries
  # no entry may be the code itself dressed up as a sentence
  expect_false(any(names(L$FLAG_PLAIN) == unname(L$FLAG_PLAIN)))
  expect_false(any(grepl("_", unname(L$FLAG_PLAIN), fixed = TRUE)))
})

test_that("a row's flags render as words, and an unknown one is never dropped", {
  L <- .ui_labels()
  expect_identical(L$plain_flags(c("", NA)), c("", ""))
  expect_identical(L$plain_flags("ocr_low_conf"), unname(L$FLAG_PLAIN[["ocr_low_conf"]]))
  # several on one row, in the order the engine wrote them
  expect_identical(L$plain_flags("redacted,date_year_inferred"),
                   paste(unname(L$FLAG_PLAIN[["redacted"]]),
                         unname(L$FLAG_PLAIN[["date_year_inferred"]]), sep = "; "))
  # a flag this screen has no wording for is still a fact about that row
  expect_identical(L$plain_flags("something_new"), "something_new")
})

test_that("both transactions tables put the flags through that map", {
  src <- .src("app.R")
  for (pat in c("output\\$cv_txns <- renderDT", "output\\$g_preview <- renderDT")) {
    i <- grep(pat, src)
    expect_length(i, 1L)
    blk <- paste(src[i:(i + 26)], collapse = " ")
    expect_match(blk, "plain_flags\\(", info = pat)
  }
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

# ADDED. The two maps above were only ever held to their KEYS, and the values
# quietly collided: RESULT_PLAIN["na"] and COVERAGE_PLAIN["unmapped"] were both
# "not on this statement". In COVERAGE_PLAIN that is right -- a field this
# statement does not have. In RESULT_PLAIN it was wrong, and both render inside
# the same "Checks & detail" disclosure, so one screen carried two meanings for
# one phrase: a statement OCR'd on two pages at 88% confidence had its
# "Scan / OCR read quality" row read "not on this statement". A KPI "na" means the
# check could not be PROVED, which is what the grey dash in the proof strip
# already means (app.R, cv_proof: "na / anything else: could not run").
test_that("a check that could not run and a field that isn't there read differently", {
  L <- .ui_labels()
  expect_false(identical(unname(L$RESULT_PLAIN[["na"]]),
                         unname(L$COVERAGE_PLAIN[["unmapped"]])))
  expect_identical(unname(L$COVERAGE_PLAIN[["unmapped"]]), "not on this statement")
  expect_false(grepl("not on this statement", L$RESULT_PLAIN[["na"]], fixed = TRUE))
  # ...and the informational KPIs get a third word: they are counts the engine READ
  # successfully, not checks that could not run, even though both carry status "na".
  expect_true(nzchar(L$RESULT_PLAIN_INFO))
  expect_false(L$RESULT_PLAIN_INFO %in% unname(L$RESULT_PLAIN))
  src <- .src("app.R")
  expect_true(any(grepl("RESULT_PLAIN_INFO", src, fixed = TRUE)))
  expect_true(any(grepl("INFORMATIONAL_CHECKS", src, fixed = TRUE)))
})

# INFORMATIONAL_CHECKS is a list of names the screen keeps because reconcile()
# strips its own `informational` column on the way out ("Return KPIs without the
# internal informational flag column exposed downstream"), so the result the app
# holds cannot be asked. A hand-kept copy of an engine fact falls behind silently,
# which is the whole reason this file exists -- so it is read back off the engine.
test_that("the screen's list of informational checks is the engine's list", {
  L <- .ui_labels()
  src <- .src("R/reconcile.R")
  # per KPI builder: does .kpi_<name>() pass informational = TRUE?
  from_engine <- Filter(function(nm) {
    i <- grep(sprintf("^\\.kpi_%s <- function", nm), src)
    if (!length(i)) return(FALSE)
    j <- grep("^\\.kpi_[a-z_]+ <- function|^# ", src)
    end <- j[j > i[1]]; end <- if (length(end)) min(end) - 1L else length(src)
    any(grepl("informational = TRUE", src[i[1]:end], fixed = TRUE))
  }, names(L$CHECK_PLAIN))
  expect_gte(length(from_engine), 2L)                 # the scan must not go quiet
  expect_setequal(from_engine, L$INFORMATIONAL_CHECKS)
  # and they really do come back as "na", which is why they needed their own word
  expect_true(all(from_engine %in% names(L$CHECK_PLAIN)))
})

# ADDED. Each CHECK_PLAIN label is read as a claim about the statement, beside a
# green "OK". Three of them claimed more than their KPI proves, and the Checks
# table dropped the expected/actual columns that would have shown the gap -- so a
# file whose date column did not map showed "Every row was read: OK",
# "Row count matches the statement: OK" and "Row dates could be read: OK" over
# 499 unreadable dates. The labels are held to what the engine actually computes.
test_that("no check label claims more than its KPI proves", {
  L <- .ui_labels()
  # no_unparsed_rows compares LINE COUNTS; it never looks at a cell.
  expect_false(grepl("every row", L$CHECK_PLAIN[["no_unparsed_rows"]], ignore.case = TRUE))
  # transaction_count degrades to n > 0 when the statement prints no count.
  expect_false(grepl("matches", L$CHECK_PLAIN[["transaction_count"]], ignore.case = TRUE))
  # redaction_summary is sum(grepl("redacted", flags)) -- honouring is proved by
  # redaction_scan, which is a different check with its own row.
  expect_false(grepl("honoured", L$CHECK_PLAIN[["redaction_summary"]], ignore.case = TRUE))
  expect_match(L$CHECK_PLAIN[["redaction_scan"]], "redaction")
  # ...and the figures the verdict rests on are on screen, so a generous pass is
  # verifiable rather than silent.
  src <- .src("app.R")
  blk <- paste(src[grep("output\\$cv_kpis <- renderDT", src)[1] + 0:14], collapse = " ")
  expect_match(blk, "Expected = ")
  expect_match(blk, "Read\\s+= ")
  expect_match(blk, "k\\$expected"); expect_match(blk, "k\\$actual")
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

# ADDED. `withheld:needs_review` promised "Fix what is flagged above and convert
# again, and it goes through". Passing the checks clears only the FIRST gate
# (R/feed.R): a conversion read by a template built here is then withheld again as
# `not_proven`, whose own wording correctly says only a data analyst can change
# that. An unconditional promise in front of a conditional gate is precisely the
# "silently wrong" shape the charter forbids, dressed up as helpfulness.
test_that("the feed note does not promise a gate it cannot clear", {
  L <- .ui_labels()
  nr <- L$FEED_PLAIN[["withheld:needs_review"]]
  expect_false(isTRUE(nr$ok))
  expect_false(grepl("goes through", paste(nr$line, nr$why), fixed = TRUE))
  # the second gate is still stated where it applies, and still names who can move it
  expect_match(L$FEED_PLAIN[["withheld:not_proven"]]$why, "data analyst")
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
  # Assert on the DETAIL, not the cure. Both raise sites now share one cure
  # constant, so "Do NOT release" no longer distinguishes them and an assertion on
  # it would pass whichever row survived -- a green test checking nothing.
  expect_match(with_recon$detail[with_recon$category == "redaction_unverified"][1],
               "the redaction scan could not complete")
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

# ---------------------------------------------------------------------------
# THE FORM RESULT: the card counts contested labels, and the table must NAME the
# same ones. convert_form() (R/forms.R) counts the `conflict` column into "N
# label(s) appear more than once with different values"; the Values-found table
# marked only `flagged` (required and not found), so on a real ANZ KiwiSaver
# summary the card said three and the NEEDS A LOOK column was empty on all seven
# rows. A reviewer told that three of seven figures are contested and not which
# three has to distrust all seven -- the opposite of what the message is for.
.app_helper <- function(name) {
  src <- .src("app.R")
  env <- new.env(parent = globalenv())
  lab <- file.path(engine_root(), "ui_labels.R")
  skip_if_not(file.exists(lab))
  sys.source(lab, envir = env)
  i <- grep(sprintf("^\\s*\\Q%s\\E <- function", name), src, perl = TRUE)
  expect_length(i, 1L)
  for (j in seq(i[1], min(i[1] + 250L, length(src)))) {
    f <- tryCatch(eval(parse(text = paste(src[i[1]:j], collapse = "\n"))[[1]], envir = env),
                  error = function(e) NULL)
    if (is.function(f)) return(f)
  }
  fail(paste("could not lift", name, "out of app.R"))
}

test_that("every conflicted form field the card counts is marked on the table", {
  look <- .app_helper(".field_needs_look")
  f <- data.frame(field = c("a", "b", "c", "d"),
                  flagged  = c(FALSE, FALSE, TRUE, TRUE),
                  conflict = c(FALSE, TRUE, FALSE, TRUE),
                  stringsAsFactors = FALSE)
  got <- look(f)
  expect_identical(got[1], "")                              # clean row: nothing said
  expect_match(got[2], "appears more than once")            # the card's own fact
  expect_match(got[3], "required and not found")
  expect_match(got[4], "required and not found")            # both, not the first match
  expect_match(got[4], "appears more than once")
  # the count the card prints and the count the table marks are the same number,
  # because both are sum(conflict)
  expect_identical(sum(nzchar(got[f$conflict])), sum(f$conflict))
  # a frame with no conflict column at all (an older result) still renders
  expect_identical(look(data.frame(flagged = c(TRUE, FALSE))), c("yes - required and not found", ""))
})

test_that("the two Needs-a-look sentences are wording, kept with the other wording", {
  L <- .ui_labels()
  expect_setequal(names(L$FIELD_LOOK_PLAIN), c("flagged", "conflict"))
  # ...and those are really the engine's own two columns on a field
  ef <- .src("R/extract_fields.R")
  expect_true(any(grepl("flagged  = required && !res\\$matched", ef)))
  expect_true(any(grepl("conflict = res\\$conflict", ef)))
})

# ---------------------------------------------------------------------------
# WHICH SKIPPED ROWS "LOOK LIKE TRANSACTIONS" IS THE ENGINE'S ANSWER, NOT A
# SEARCH OF ITS PROSE. The X-ray drew its amber boxes, counted its key and sorted
# its table with grepl("didn't parse|no amount", reason) -- and the heading
# sentence is "no date and no amount - treated as a heading, note or wrapped
# line", which contains "no amount". So the picture called 26 rows on page 1 of a
# real Westpac statement "a skipped row that looks like a transaction" against
# the engine's 2, and the table beside it called those same rows headings. One
# screen, two answers, about one row.
test_that("the X-ray asks the engine which skipped rows look like transactions", {
  src <- .src("app.R")
  # The prose search is gone from every one of the three places that had it. Read
  # off the PARSER's own string tokens, so the comment above .ix_unread that
  # quotes the old pattern (and should stay) cannot pass for a live search.
  pd <- utils::getParseData(parse(file.path(engine_root(), "app.R"), keep.source = TRUE))
  lits <- pd$text[pd$terminal & pd$token == "STR_CONST"]
  expect_gt(length(lits), 100L)                          # the scan must not go quiet
  expect_identical(grep("didn't parse\\|no amount", lits, value = TRUE), character(0))
  expect_gte(length(grep("\\.ix_unread\\(", src)), 4L)   # helper + its three callers
  unread <- .app_helper(".ix_unread")
  rows <- data.frame(
    kept   = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    reason = c("", .PDF_ROW_REASON_TEXT[["heading_or_note"]],
               .PDF_ROW_REASON_TEXT[["summary_line"]],
               .PDF_ROW_REASON_TEXT[["date_unparsed"]],
               .PDF_ROW_REASON_TEXT[["amount_missing"]]),
    stringsAsFactors = FALSE)
  expect_identical(unread(rows), c(FALSE, FALSE, FALSE, TRUE, TRUE))
  # the heading sentence really does contain the words the old search looked for
  expect_true(grepl("no amount", .PDF_ROW_REASON_TEXT[["heading_or_note"]], fixed = TRUE))
  # and the engine's own predicate agrees, row for row
  expect_identical(unread(rows),
                   !rows$kept & pdf_reason_actionable(rows$reason))
})

test_that("the layer, the key and the engine use one name for that one fact", {
  L <- .ui_labels()
  src <- .src("app.R")
  joined <- paste(src, collapse = "\n")
  for (nm in c("UNREAD_ROW_PLAIN_LAYER", "UNREAD_ROW_PLAIN_KEY")) {
    expect_true(nzchar(L[[nm]]))
    expect_true(grepl(nm, joined, fixed = TRUE))     # the screen really uses it
  }
  # "Skipped rows" named a layer that drew only the unread ones, so ticking it and
  # counting the table's rows gave two different numbers
  expect_false(grepl('"Skipped rows" = "skipped"', joined, fixed = TRUE))
  # both say "looks like a transaction", which is the verdict card's phrase for
  # the same rows (R/reconcile.R, no_unparsed_rows)
  expect_match(L$UNREAD_ROW_PLAIN_KEY, "looks like a transaction")
  expect_match(L$UNREAD_ROW_PLAIN_LAYER, "look like transactions")
  expect_true(any(grepl("look like transactions", .src("R/reconcile.R"))))
})

# ---------------------------------------------------------------------------
# ONE FAULT, ONE BULLET. "What to check" is built from two frames -- the top
# diagnostics and the failing checks -- and the engine writes the SAME sentence
# into both when they describe the same fault. On a real ASB statement the list
# read "dates couldn't be read - no row dates could be read ..." immediately
# followed by "Row dates could be read - no row dates could be read ...": one
# fault twice, the second bullet named by what its check PROVES, which beside a
# failure says the opposite of what happened.
test_that("a fault listed as a diagnostic is not listed again as a check", {
  src <- .src("app.R")
  i <- grep("failed_checks_ui <- function", src)
  expect_length(i, 1L)
  blk <- paste(src[i:min(i + 12L, length(src))], collapse = " ")
  expect_match(blk, "f\\$detail %\\|\\|% \"\"\\) %in% dg\\$detail")
  # and a failing check says it failed, the way the batch table's column does
  # (ui_labels.R, plain_failing_check: CHECK_PLAIN words a check as what it
  # PROVES, which beside a failure and with no pass/fail column says the opposite)
  expect_match(paste(src, collapse = "\n"), 'sprintf\\("Failed: %s", plain_check')
  expect_match(.ui_labels()$plain_failing_check("check:dates_readable"), "^Failed: ")
})
