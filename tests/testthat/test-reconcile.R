# Reconciliation KPI coverage (build-contract sections 8, 11.3). Exercises the
# pass AND fail branches that no bank fixture reaches: balance_reconciliation,
# dates_within_period, running_balance_continuity(fail), transaction_count
# (stated), and no_unparsed_rows completeness.

# .parsed(...) -- assemble a minimal parsed object from a core-shaped list plus
# header overrides, so KPI branches can be driven directly.
.parsed <- function(tx, header = list(), source_line_count = NA_integer_,
                    multiline_extra = 0L) {
  base_h <- list(
    bank = "X", statement_type = "e", template_id = "t", template_version = 1,
    account_number = NA, account_name = NA, period_start = NA, period_end = NA,
    opening_balance = NA_real_, closing_balance = NA_real_, currency = "NZD",
    source_file = "x.csv", source_sha256 = "s", page_count = NA, row_count = nrow(tx))
  for (k in names(header)) base_h[[k]] <- header[[k]]
  list(transactions = coerce_core(tx),
       extras = data.frame(row_id = integer(0)),
       header = base_h,
       provenance = data.frame(row_id = seq_len(nrow(tx))),
       source_line_count = source_line_count,
       multiline_extra = multiline_extra)
}

.tx <- function(amount, balance = NA, date = NA, flags = "") {
  data.frame(row_id = seq_along(amount), amount = amount, balance = balance,
             date = date, flags = flags, stringsAsFactors = FALSE)
}

# ---- the shape of reconcile(): one builder per check -------------------------
# reconcile() is a list of `.kpi_*()` builders. Adding a KPI is adding a builder
# and one line to that list, so this pins the contract every builder must keep:
# ONE row, or NULL when the check does not apply to this statement.
test_that("each check is one builder returning a single KPI row (or NULL)", {
  p <- .parsed(.tx(c(-10, 40, -5)),
               header = list(opening_balance = 100, closing_balance = 125),
               source_line_count = 3)
  tx <- p$transactions; h <- p$header; n <- nrow(tx)
  one_row <- function(k) is.data.frame(k) && nrow(k) == 1L
  expect_true(one_row(.kpi_balance_reconciliation(tx, h, n)))
  expect_true(one_row(.kpi_running_balance_continuity(tx, n)))
  expect_true(one_row(.kpi_transaction_count(h, n)))
  expect_true(one_row(.kpi_dates_within_period(tx, h, n)))
  expect_true(one_row(.kpi_dates_readable(tx, n)))
  expect_true(one_row(.kpi_no_unparsed_rows(p, tx, n)))
  expect_true(one_row(.kpi_redaction_summary(tx)))
  # the three that are raised only when there IS something to say
  expect_null(.kpi_amount_direction(tx, list(amount_sign = "signed")))
  expect_null(.kpi_ocr_confidence(h))
  expect_null(.kpi_redaction_scan(h))
  # ...and every builder's row carries the .kpi() column set reconcile() rbinds
  expect_named(.kpi_redaction_summary(tx),
               c("name", "status", "expected", "actual", "discrepancy", "detail",
                 "informational"))
  # the internal `informational` flag never leaves reconcile()
  expect_false("informational" %in% names(reconcile(p)$kpis))
})

test_that("balance_reconciliation PASSES on consistent opening/sum/closing", {
  p <- .parsed(.tx(c(-10, 40, -5)),
               header = list(opening_balance = 100, closing_balance = 125),
               source_line_count = 3)
  k <- reconcile(p)$kpis
  expect_equal(k$status[k$name == "balance_reconciliation"], "pass")
})

test_that("balance_reconciliation FAILS and trust is low when tampered", {
  p <- .parsed(.tx(c(-10, 40, -5)),
               header = list(opening_balance = 100, closing_balance = 999),
               source_line_count = 3)
  r <- reconcile(p)
  expect_equal(r$kpis$status[r$kpis$name == "balance_reconciliation"], "fail")
  expect_equal(r$trust$level, "low")
})

test_that("dates_within_period flags an out-of-period date", {
  p_ok <- .parsed(.tx(c(-10, 40), date = c("2020-01-05", "2020-01-20")),
                  header = list(period_start = "2020-01-01", period_end = "2020-01-31"),
                  source_line_count = 2)
  expect_equal(reconcile(p_ok)$kpis$status[
    reconcile(p_ok)$kpis$name == "dates_within_period"], "pass")
  p_bad <- .parsed(.tx(c(-10, 40), date = c("2020-01-05", "2020-02-20")),
                   header = list(period_start = "2020-01-01", period_end = "2020-01-31"),
                   source_line_count = 2)
  expect_equal(reconcile(p_bad)$kpis$status[
    reconcile(p_bad)$kpis$name == "dates_within_period"], "fail")
})

test_that("running_balance_continuity FAILS on a broken balance column", {
  # balance jumps inconsistently with amount at row 2.
  p <- .parsed(.tx(c(-9, -15, -3.15),
                   balance = c(895.69, 999.99, 877.54)),
               source_line_count = 3)
  r <- reconcile(p)
  expect_equal(r$kpis$status[r$kpis$name == "running_balance_continuity"], "fail")
  expect_equal(r$trust$level, "low")
})

test_that("running_balance_continuity fail branch via a real fixture", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("tests/testthat/fixtures/kiwibank_broken_balance.csv"))
  p <- parse_statement(input, templates[["kiwibank_everyday_csv"]])
  r <- reconcile(p, templates[["kiwibank_everyday_csv"]])
  expect_equal(r$kpis$status[r$kpis$name == "running_balance_continuity"], "fail")
  expect_equal(r$trust$level, "low")
})

test_that("a signed statement with one-sign amounts and no balance fails direction (P2-10)", {
  signed <- list(amount_sign = "signed")
  # all money-in, no balance to cross-check -> unverifiable direction -> fail.
  p_pos <- .parsed(.tx(c(10, 20, 30)), source_line_count = 3)
  k <- reconcile(p_pos, signed)$kpis
  expect_equal(k$status[k$name == "amount_direction"], "fail")
  # mixed signs -> the +/- is genuinely present -> no such KPI.
  p_mix <- .parsed(.tx(c(-10, 20)), source_line_count = 2)
  expect_false("amount_direction" %in% reconcile(p_mix, signed)$kpis$name)
  # one-sign but WITH a balance column -> continuity can verify -> no such KPI.
  p_bal <- .parsed(.tx(c(10, 20), balance = c(110, 130)), source_line_count = 2)
  expect_false("amount_direction" %in% reconcile(p_bal, signed)$kpis$name)
})

test_that("continuity bridges a blank middle balance, catching a hidden break (P2-3)", {
  # 100, NA, 130 with amounts 0/+20/+5: the 130 should be 125. The old loop
  # skipped both pairs around the NA and reported a clean pass; the bridge sees it.
  p <- .parsed(.tx(c(0, 20, 5), balance = c(100, NA, 130)), source_line_count = 3)
  k <- reconcile(p)$kpis
  expect_equal(k$status[k$name == "running_balance_continuity"], "fail")
  # a bridge that DOES reconcile (100 + 20 + 5 == 125) passes.
  p2 <- .parsed(.tx(c(0, 20, 5), balance = c(100, NA, 125)), source_line_count = 3)
  expect_equal(reconcile(p2)$kpis$status[
    reconcile(p2)$kpis$name == "running_balance_continuity"], "pass")
  # a blank amount INSIDE the gap makes the bridge unverifiable -> surfaced, not
  # silently passed as verified.
  p3 <- .parsed(.tx(c(0, NA, 5), balance = c(100, NA, 130)), source_line_count = 3)
  det <- reconcile(p3)$kpis$detail[reconcile(p3)$kpis$name == "running_balance_continuity"]
  expect_match(det, "unverifiable")
})

test_that("no_unparsed_rows does NOT fail on a legitimate multi-line record (P2-2)", {
  # 3 physical data lines, 2 parsed rows, 1 extra line accounted for by a quoted
  # embedded newline -> complete, must PASS (was crying wolf on every such file).
  p <- .parsed(.tx(c(-10, 40)), source_line_count = 3, multiline_extra = 1)
  k <- reconcile(p)$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "pass")
  # but an UNaccounted extra line (no multi-line record) still fails loudly.
  p2 <- .parsed(.tx(c(-10, 40)), source_line_count = 3, multiline_extra = 0)
  expect_equal(reconcile(p2)$kpis$status[
    reconcile(p2)$kpis$name == "no_unparsed_rows"], "fail")
})

test_that("transaction_count matches / mismatches a stated count", {
  p_match <- .parsed(.tx(c(-10, 40)), header = list(stated_count = 2),
                     source_line_count = 2)
  expect_equal(reconcile(p_match)$kpis$status[
    reconcile(p_match)$kpis$name == "transaction_count"], "pass")
  p_miss <- .parsed(.tx(c(-10, 40)), header = list(stated_count = 5),
                    source_line_count = 2)
  expect_equal(reconcile(p_miss)$kpis$status[
    reconcile(p_miss)$kpis$name == "transaction_count"], "fail")
})

test_that("no_unparsed_rows FAILS when a source line was not accounted for", {
  # 3 source data lines but only 2 parsed rows -> a record was lost/merged.
  p <- .parsed(.tx(c(-10, 40)), source_line_count = 3)
  k <- reconcile(p)$kpis
  row <- k[k$name == "no_unparsed_rows", ]
  expect_equal(row$status, "fail")
  expect_equal(as.integer(row$expected), 3L)
})

test_that("no_unparsed_rows FAILS on a malformed row even if counts align", {
  p <- .parsed(.tx(c(-10, 40), flags = c("malformed", "")), source_line_count = 2)
  k <- reconcile(p)$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "fail")
})

# ---- balance_reconciliation: an unreadable amount must not be shrugged off ----
test_that("an unreadable amount with BOTH balances printed FAILS, and names the rows", {
  # One blank/redacted/unreadable amount used to disable the strongest completeness
  # proof for the WHOLE statement, and the KPI then reported "na" with the detail
  # "no opening/closing balance and no running-balance column to derive one" --
  # factually untrue when the statement printed both. The run came out trust
  # medium, status ok, and was fed to the dashboards.
  p <- .parsed(.tx(c(10, NA, 5)),
               header = list(opening_balance = 1000, closing_balance = 1015),
               source_line_count = 3)
  k <- reconcile(p)$kpis
  row <- k[k$name == "balance_reconciliation", ]
  expect_equal(row$status, "fail")                    # unproven is not "not applicable"
  expect_match(row$detail, "cannot be proved")
  expect_match(row$detail, "row 2")                   # points at the offending row
  expect_false(grepl("no opening/closing balance", row$detail))  # the false claim is gone
  expect_equal(reconcile(p)$trust$level, "low")       # ...and trust reflects it

  # the same statement with every amount readable still reconciles cleanly
  ok <- .parsed(.tx(c(10, 0, 5)),
                header = list(opening_balance = 1000, closing_balance = 1015),
                source_line_count = 3)
  expect_equal(reconcile(ok)$kpis$status[
    reconcile(ok)$kpis$name == "balance_reconciliation"], "pass")
})

test_that("the balance_reconciliation 'na' detail says WHICH balance is missing", {
  neither <- .parsed(.tx(c(-10, 40)), source_line_count = 2)
  d <- reconcile(neither)$kpis$detail[
    reconcile(neither)$kpis$name == "balance_reconciliation"]
  expect_match(d, "no opening or closing balance")

  # opening printed, closing absent and underivable (no balance column)
  no_close <- .parsed(.tx(c(-10, 40)), header = list(opening_balance = 100),
                      source_line_count = 2)
  d2 <- reconcile(no_close)$kpis$detail[
    reconcile(no_close)$kpis$name == "balance_reconciliation"]
  expect_match(d2, "no closing balance")
  expect_false(grepl("no opening", d2))

  no_open <- .parsed(.tx(c(-10, 40)), header = list(closing_balance = 100),
                     source_line_count = 2)
  d3 <- reconcile(no_open)$kpis$detail[
    reconcile(no_open)$kpis$name == "balance_reconciliation"]
  expect_match(d3, "no opening balance")
})

# The same untruth, still live in the BOTH-missing branch: it said "and no
# running-balance column to derive one from" unconditionally -- on a statement
# whose balance column was present, populated, and passing its own check two rows
# lower on the same screen. It sent reviewers looking for a balance that was on
# the page all along, which is the exact wording of the comment above the fix.
test_that("the both-missing balance detail does not deny a balance column that is there", {
  with_col <- .parsed(.tx(c(-10, 40), balance = c(90, 130)), source_line_count = 2)
  k <- reconcile(with_col)$kpis
  d <- k$detail[k$name == "balance_reconciliation"]
  expect_equal(k$status[k$name == "balance_reconciliation"], "na")
  expect_false(grepl("no running-balance column", d, fixed = TRUE))
  # ...and it says the REAL reason both anchors are refused
  expect_match(d, "its own endpoints")
  # the column really is there and really does pass, on the same result
  expect_equal(k$status[k$name == "running_balance_continuity"], "pass")

  # with genuinely no column, the original sentence is still the true one
  without <- .parsed(.tx(c(-10, 40)), source_line_count = 2)
  expect_match(reconcile(without)$kpis$detail[
    reconcile(without)$kpis$name == "balance_reconciliation"],
    "no running-balance column")
})

# A real conversion, end to end: the kiwibank export is the file the untrue
# sentence was found on.
test_that("a real statement with a balance column is not told it has none", {
  out <- tempfile("reconbal_"); dir.create(out)
  res <- convert_statement(fixture("samples/raw/kiwibank/kiwibank_transaction_01.csv"),
                           outdir = out, templates_dir = templates_dir(), logdir = out)
  k <- res$kpis
  expect_false(grepl("no running-balance column",
                     k$detail[k$name == "balance_reconciliation"], fixed = TRUE))
  expect_equal(k$status[k$name == "running_balance_continuity"], "pass")
})

# ---- no_unparsed_rows on formats with no source-line count ------------------
test_that("no_unparsed_rows is 'na', not a false 'pass', when it cannot be computed", {
  # `lost` is 0 by construction without a source line count, so this KPI reported
  # "pass" for EVERY pdf/excel statement -- a green tick for a check that never ran.
  p <- .parsed(.tx(c(-10, 40)), source_line_count = NA_integer_)
  p$visual_row_count <- 5L; p$skipped_row_count <- 3L
  row <- reconcile(p)$kpis
  row <- row[row$name == "no_unparsed_rows", ]
  expect_equal(row$status, "na")
  expect_match(row$detail, "cannot be proved for this format")
  expect_match(row$detail, "3 were skipped")          # the threaded count is stated
  expect_match(row$detail, "2 of 5 visual row")

  # a malformed row is a REAL defect and still fails loudly on such a format
  pm <- .parsed(.tx(c(-10, 40), flags = c("malformed", "")), source_line_count = NA_integer_)
  expect_equal(reconcile(pm)$kpis$status[
    reconcile(pm)$kpis$name == "no_unparsed_rows"], "fail")

  # a delimited statement (source line count known) still PASSES exactly as before
  pd <- .parsed(.tx(c(-10, 40)), source_line_count = 2)
  expect_equal(reconcile(pd)$kpis$status[
    reconcile(pd)$kpis$name == "no_unparsed_rows"], "pass")
})

# ---- a check that did not run shows no figures ------------------------------
# The Checks table prints Expected and Read beside the verdict. "could not be
# checked | Expected 2 | Read 2" is the SAME pair of numbers a clean CSV shows
# under "OK", so the figures could not tell the two apart -- and two numbers that
# agree, beside a check that never ran, read as a pass.
test_that("an 'na' check reports no expected/actual figures", {
  p <- .parsed(.tx(c(-10, 40)), source_line_count = NA_integer_)
  p$visual_row_count <- 5L; p$skipped_row_count <- 3L
  k <- reconcile(p)$kpis
  row <- k[k$name == "no_unparsed_rows", ]
  expect_equal(row$status, "na")
  expect_true(is.na(row$expected))
  expect_true(is.na(row$actual))
  expect_match(row$detail, "2 of 5 visual row")   # the numbers stay, in the Detail
  # ...and it holds for EVERY check, because the rule lives in .kpi() itself --
  # a builder cannot opt out of it by forgetting.
  expect_true(is.na(.kpi("anything", "na", expected = 9, actual = 9)$expected))
  expect_true(is.na(.kpi("anything", "na", expected = 9, actual = 9)$actual))
  # INFORMATIONAL rows are not checks that could not run -- their figure is the
  # whole point of the row, and the screen gives them their own word.
  expect_equal(as.integer(k$actual[k$name == "redaction_summary"]), 0L)
  expect_equal(as.integer(
    .kpi("anything", "na", actual = 9, informational = TRUE)$actual), 9L)
  # and a check that DID run still shows both figures
  pd <- .parsed(.tx(c(-10, 40)), source_line_count = 2)
  kd <- reconcile(pd)$kpis
  expect_equal(as.integer(kd$expected[kd$name == "no_unparsed_rows"]), 2L)
  expect_equal(as.integer(kd$actual[kd$name == "no_unparsed_rows"]), 2L)
})

# That same detail is customer-facing, and it carried two things it should not:
# a raw KPI name (which the operational guide tells the analyst to report AS A
# BUG when it appears on screen), and a screen name -- "the Inspect view" -- that
# is printed on screen as "See it on the page".
test_that("the no_unparsed_rows detail names no engine code and no other screen", {
  p <- .parsed(.tx(c(-10, 40)), source_line_count = NA_integer_)
  p$visual_row_count <- 5L; p$skipped_row_count <- 3L; p$actionable_skip_count <- 0L
  d <- reconcile(p)$kpis
  d <- d$detail[d$name == "no_unparsed_rows"]
  expect_false(grepl("balance_reconciliation", d, fixed = TRUE))
  expect_match(d, "the balance proof above", fixed = TRUE)
  expect_false(grepl("Inspect view", d, fixed = TRUE))
  expect_match(d, "See it on the page", fixed = TRUE)
  # the same, for the branch with no skipped/visual counts at all
  q <- .parsed(.tx(c(-10, 40)), source_line_count = NA_integer_)
  dq <- reconcile(q)$kpis
  dq <- dq$detail[dq$name == "no_unparsed_rows"]
  expect_false(grepl("balance_reconciliation", dq, fixed = TRUE))
})

# The skipped count is the most alarming number on that row, sitting beside a
# Result of "could not be checked". The engine already knows the fact that settles
# it -- how many of those skips looked like transactions -- and said nothing.
test_that("the skipped-row count says how many of them looked like transactions", {
  none <- .parsed(.tx(c(-10, 40)), source_line_count = NA_integer_)
  none$visual_row_count <- 29L; none$skipped_row_count <- 17L; none$actionable_skip_count <- 0L
  dn <- reconcile(none)$kpis
  expect_match(dn$detail[dn$name == "no_unparsed_rows"],
               "None of the skipped rows looked like a transaction.", fixed = TRUE)

  # ...and when some DID, the number is stated rather than implied
  some <- .parsed(.tx(c(-10, 40)), source_line_count = NA_integer_)
  some$visual_row_count <- 29L; some$skipped_row_count <- 17L; some$actionable_skip_count <- 2L
  ds <- reconcile(some)$kpis
  expect_match(ds$detail[ds$name == "no_unparsed_rows"],
               "2 of the skipped rows looked like a transaction", fixed = TRUE)

  # a format that cannot count them says neither thing rather than guessing
  unk <- .parsed(.tx(c(-10, 40)), source_line_count = NA_integer_)
  unk$visual_row_count <- 29L; unk$skipped_row_count <- 17L
  du <- reconcile(unk)$kpis
  expect_false(grepl("looked like a transaction",
                     du$detail[du$name == "no_unparsed_rows"], fixed = TRUE))
})

# The real statement the wording was found on.
test_that("a real PDF's skipped-row sentence is complete and code-free", {
  tpl <- load_templates(templates_dir())[["tutorial_everyday_pdf"]]
  skip_if(is.null(tpl))
  p <- parse_statement(read_input(fixture("samples/raw/tutorial/sample_everyday_statement.pdf")), tpl)
  k <- reconcile(p, tpl)$kpis
  d <- k$detail[k$name == "no_unparsed_rows"]
  expect_match(d, "None of the skipped rows looked like a transaction.", fixed = TRUE)
  expect_match(d, "See it on the page", fixed = TRUE)
  expect_false(grepl("balance_reconciliation", d, fixed = TRUE))
})

# ---- dates_readable: "at least one" was never what the label says ------------
# The threshold was d_ok > 0, so 1 readable date out of 500 passed, under a label
# a reviewer reads as a claim about the whole column.
test_that("dates_readable passes only when EVERY row date read", {
  all_ok <- .parsed(.tx(c(-10, 40), date = c("2024-01-05", "2024-01-06")),
                    source_line_count = 2)
  k <- reconcile(all_ok)$kpis
  expect_equal(k$status[k$name == "dates_readable"], "pass")
  expect_match(k$detail[k$name == "dates_readable"], "all 2 row date")

  partial <- .parsed(.tx(c(-10, 40), date = c("2024-01-05", NA)), source_line_count = 2)
  kp <- reconcile(partial)$kpis
  expect_equal(kp$status[kp$name == "dates_readable"], "fail")
  expect_match(kp$detail[kp$name == "dates_readable"], "1 of 2 row date\\(s\\) could not be read")
  expect_equal(as.integer(kp$actual[kp$name == "dates_readable"]), 1L)
  expect_equal(as.integer(kp$expected[kp$name == "dates_readable"]), 2L)

  # none readable is still the loudest case, with its own cause named
  gone <- .parsed(.tx(c(-10, 40), date = c(NA, NA)), source_line_count = 2)
  kg <- reconcile(gone)$kpis
  expect_equal(kg$status[kg$name == "dates_readable"], "fail")
  expect_match(kg$detail[kg$name == "dates_readable"], "mapping or format is wrong")
})

# transaction_count KEEPS "pass" when the statement prints no count -- see the
# comment on .kpi_transaction_count for why. What must stay true is that the row
# SAYS what it tested, so a generous pass is visible rather than silent.
test_that("a transaction_count with no printed count declares what it proved", {
  p <- .parsed(.tx(c(-10, 40)), source_line_count = 2)
  k <- reconcile(p)$kpis
  row <- k[k$name == "transaction_count", ]
  expect_equal(row$status, "pass")
  expect_identical(row$expected, ">0")          # the Expected column carries the weakness
  expect_match(row$detail, "prints no transaction count")
  expect_match(row$detail, "the only thing proved here")
})

# ---- redaction_scan: an incomplete scan may have leaked hidden text ---------
test_that("an incomplete redaction scan FAILS a KPI (it never reached reconcile before)", {
  clean <- .parsed(.tx(c(-10, 40)), header = list(redaction_scan_incomplete = 0L),
                   source_line_count = 2)
  expect_false("redaction_scan" %in% reconcile(clean)$kpis$name)   # nothing to report

  leak <- .parsed(.tx(c(-10, 40)), header = list(redaction_scan_incomplete = 2L),
                  source_line_count = 2)
  r <- reconcile(leak)
  row <- r$kpis[r$kpis$name == "redaction_scan", ]
  expect_equal(row$status, "fail")             # drives needs_review + feed withholding
  expect_equal(as.integer(row$actual), 2L)
  expect_match(row$detail, "hidden under a redaction box")
  expect_equal(r$trust$level, "low")
})

# ---- inferred year: a guessed year must never read as proven ----------------
test_that("date_year_inferred caps trust and states where the year came from", {
  # The reader can take a year from a stray 4-digit number in free page text when
  # no statement period parses. The dates then look fully formed, so nothing else
  # questions them -- reconcile had no branch for the flag at all.
  p <- .parsed(.tx(c(-10, 40), balance = c(90, 130), date = c("2019-01-05", "2019-01-06"),
                   flags = c("date_year_inferred", "date_year_inferred")),
               header = list(opening_balance = 100, closing_balance = 130,
                             stated_count = 2, period_start = "2019-01-01",
                             period_end = "2019-01-31"),
               source_line_count = 2)
  r <- reconcile(p)
  expect_false(any(r$kpis$status == "fail"))              # nothing is actually wrong
  expect_equal(r$trust$level, "medium")                   # ...but it is not "high"
  expect_true(any(grepl("INFERRED year", r$trust$reasons)))

  # the identical statement without the flag is rated high
  q <- .parsed(.tx(c(-10, 40), balance = c(90, 130), date = c("2019-01-05", "2019-01-06")),
               header = list(opening_balance = 100, closing_balance = 130,
                             stated_count = 2, period_start = "2019-01-01",
                             period_end = "2019-01-31"),
               source_line_count = 2)
  expect_equal(reconcile(q)$trust$level, "high")
})

# --- N20: a THIN parse is the same defect as an empty one --------------------
# Reading 5 rows of a 500-row statement used to pass everything: transaction_count
# only requires n > 0 when the statement prints no stated count, and
# no_unparsed_rows returned "na" for every PDF. A green run, on the dashboards,
# missing 99% of the money.
#
# The signal is rows that LOOKED like transactions and could not be read (date
# wouldn't parse, or no amount in the money bands). Headings, summary lines and
# wrapped text are skipped on every healthy statement and are excluded.
#
# IT TAKES TWO CONDITIONS, and it used to take one. "More rows missed than
# captured" alone was calibrated in the engine as "healthy 4-36%, broken
# 100-1300%", and a file shipped in this repo falsifies that: anz_0382025_feb.pdf
# converts perfectly -- its money-in and money-out agree to the cent with the
# totals the statement prints on page 2 -- and sat at 171%, failed this check and
# was held out of the dashboards. Re-measured, healthy conversions in this repo
# reach 800% on that ratio (an ASB statement with ONE transaction and eight lines
# of account-number / statement-number / page-number furniture). So the comparison
# keeps its place and gains the magnitude guard it never had: the unreadable rows
# must ALSO be more than a quarter of every visual row examined. Measured that
# way, healthy runs 0-16% and a template pushed off the table runs 34-52%.
#
# `visual` defaults to kept + actionable, which is the WORST case for the
# magnitude guard (no furniture at all in the denominator); pass it explicitly for
# the shape a real statement has.
.thin <- function(n, actionable, visual = n + actionable) {
  parsed <- list(transactions = data.frame(flags = rep("", n), stringsAsFactors = FALSE),
                 source_line_count = NA_integer_, visual_row_count = visual,
                 skipped_row_count = visual - n, actionable_skip_count = actionable)
  .kpi_no_unparsed_rows(parsed, parsed$transactions, n)
}

test_that("a page mostly made of unreadable transaction rows is a FAILURE", {
  k <- .thin(n = 0, actionable = 12, visual = 31)   # tutorial, date band off column
  expect_identical(k$status, "fail")
  expect_match(k$detail, "look like transactions and could not be read")
  # the numbers are stated, so a reviewer can check them on the page
  expect_match(k$detail, "12 of the 31 row", fixed = TRUE)
  expect_match(k$detail, "See it on the page", fixed = TRUE)   # ONE name for that screen
})

test_that("a healthy statement's ordinary skips are left alone", {
  # 311 kept / 2 actionable over 433 visual rows -- anz_single.pdf, measured.
  # Headings and summary lines are normal; flagging these would cry wolf on every
  # real conversion.
  expect_identical(.thin(n = 311, actionable = 2,  visual = 433)$status, "na")
  expect_identical(.thin(n = 79,  actionable = 18, visual = 148)$status, "na")  # asb_B.pdf
  expect_identical(.thin(n = 12,  actionable = 0,  visual = 30)$status,  "na")  # tutorial
})

test_that("a PERFECT conversion is never held back by its page furniture (G1)", {
  # THE CASE THAT MADE THIS RULE CHANGE. Each of these reconciles exactly against
  # the totals its own statement prints, and each has MORE unreadable-looking rows
  # than transactions, because a short statement's headings, upcoming-payment
  # blocks, other-account balances and footers all carry digits. Under the old
  # single condition every one of them failed this check and was withheld from the
  # dashboards.
  expect_identical(.thin(n = 7,  actionable = 9,  visual = 63)$status,  "na")  # anz_0382025_feb
  expect_identical(.thin(n = 1,  actionable = 8,  visual = 60)$status,  "na")  # asb
  expect_identical(.thin(n = 10, actionable = 11, visual = 67)$status,  "na")  # wp_ROT2
  expect_identical(.thin(n = 6,  actionable = 8,  visual = 70)$status,  "na")  # wp_2026_MORPH
  expect_identical(.thin(n = 8,  actionable = 10, visual = 131)$status, "na")  # anz investmentfunds
})

test_that("BOTH conditions are required, and each one alone is not enough", {
  # more missed than captured, but a handful of rows on a full page -> furniture
  expect_identical(.thin(n = 5,  actionable = 6,  visual = 60)$status, "na")
  # a quarter of the page unreadable, but most of the statement still read
  expect_identical(.thin(n = 40, actionable = 20, visual = 60)$status, "na")
  # both -> the template really is reading the wrong part of the page
  expect_identical(.thin(n = 5,  actionable = 20, visual = 60)$status, "fail")
})

test_that("a genuinely small statement is not cried wolf over", {
  # The comparison is fair at 300 rows and fragile at 5. One unusual "brought
  # forward" line the summary matcher misses, plus a wrapped note, is most of the
  # way to tripping a 2-row statement. Three is the smallest count that cannot be
  # one stray row and its neighbour.
  expect_identical(.thin(n = 1, actionable = 1)$status, "na")
  expect_identical(.thin(n = 2, actionable = 2)$status, "na")
  expect_identical(.thin(n = 1, actionable = 3)$status, "fail")   # 3x the statement
  expect_identical(.thin(n = 5, actionable = 5)$status, "fail")   # half unread: real
})

test_that("the Expected column never shows a figure the statement never printed (G4)", {
  # It used to read n + actionable: "Expected 19 | Read 7" on a statement that
  # prints no 19 anywhere -- the checker's own arithmetic in the column a reviewer
  # uses to check the checker, on the row that quarantines the run.
  k <- .thin(n = 0, actionable = 12, visual = 31)
  expect_identical(k$status, "fail")
  expect_true(is.na(k$expected))
  expect_identical(k$actual, "0")           # rows read: countable in the output
  expect_identical(k$discrepancy, "12")     # rows that would not read: countable on the page
})

test_that("skipped rows that are not transactions never count against a statement", {
  # This is the case that worried the owner: most statements skip MORE rows than
  # they keep -- addresses, headings, totals, wrapped description lines. None of
  # them are actionable, so none of them can trip the check. Measured on the worked
  # example: 12 kept, 17 skipped, 0 actionable.
  expect_identical(.thin(n = 12, actionable = 0)$status, "na")
  expect_true(all(c("summary_line", "heading_or_note") %in% names(.PDF_ROW_REASON_TEXT)))
  expect_false(any(c("summary_line", "heading_or_note") %in% .PDF_ACTIONABLE_CODES))
})

test_that("a format that cannot count actionable skips is unaffected", {
  # Delimited files are already covered by the source-line-count difference, and
  # they carry no actionable count -- the new branch must not fire on NA.
  parsed <- list(transactions = data.frame(flags = "", stringsAsFactors = FALSE),
                 source_line_count = NA_integer_, actionable_skip_count = NA_integer_)
  expect_identical(.kpi_no_unparsed_rows(parsed, parsed$transactions, 1L)$status, "na")
})

test_that("a real PDF with a wrong date band really does produce actionable skips", {
  # The unit tests above inject actionable_skip_count directly. This one proves the
  # PARSER produces it on a real file, so the defence cannot die while the suite
  # stays green -- which is exactly how a prose-matching version of it would have.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  b <- readLines(fixture("templates/tutorial_everyday_pdf.yaml"))
  b <- b[!grepl("^sample: true", b)]
  # Move ONLY the date band off its column. Every row then has a real amount and
  # an unreadable date -> date_unparsed, the actionable case. (Moving every band
  # instead gives "no date and no amount", which is a heading, not actionable --
  # that case yields zero rows and is caught by the unsupported path instead.)
  b <- sub("date:.*x_min: 28.*", "date:        {x_min: 2,   x_max: 9}", b)
  d <- tempfile("tpl_datewrong_"); dir.create(d)
  writeLines(b, file.path(d, "t.yaml"))
  tpl <- load_templates(d, strict = FALSE)[[1]]
  inp <- read_input(fixture("samples/raw/tutorial/sample_everyday_statement.pdf"))
  ps <- parse_statement(inp, tpl, meta = extract_metadata(inp))
  expect_gt(ps$actionable_skip_count, 0L)
  expect_gte(ps$actionable_skip_count, nrow(ps$transactions))
  # ...and it is a MAGNITUDE, not a handful: this is the measurement the quarter
  # threshold sits under (12 of 31 rows examined = 39%). If a change ever pushes a
  # real break below the threshold this fails here rather than on a real statement.
  expect_gt(4L * ps$actionable_skip_count, ps$visual_row_count)
  expect_identical(reconcile(ps, tpl)$kpis$status[
    reconcile(ps, tpl)$kpis[[1]] == "no_unparsed_rows"], "fail")
})

test_that("the actionable codes are codes, not sentences a reword can break", {
  # The defence decides on .PDF_ACTIONABLE_CODES. If someone turns those back into
  # prose, or reworders the display text expecting the engine to follow, this fails.
  expect_true(all(.PDF_ACTIONABLE_CODES %in% names(.PDF_ROW_REASON_TEXT)))
  expect_false(any(grepl(" ", .PDF_ACTIONABLE_CODES)))    # codes, not sentences
  src <- readLines(file.path(engine_root(), "R", "parse_pdf_table.R"), warn = FALSE)
  expect_false(any(grepl("grepl\\(\"didn't parse", src, useBytes = TRUE)))
})

# ---------------------------------------------------------------------------
# THE TRUST REASON JUSTIFIES THE LEVEL. IT DOES NOT GRADE ANOTHER CHECK (G3).
#
# The medium-instead-of-low reason used to add "the running-balance / period
# checks are secondary and commonly flag on combined/multi-account statements".
# On a real needs_review run that sentence was printed directly above a HIGH
# diagnostic reading "This upload looks like more than one statement bundled
# together, which corrupts a single parse. Split it into one statement per file
# and re-run" -- the tool calling commonplace the exact condition another line of
# the same screen was calling corrupting. .reconcile_trust cannot see the
# diagnostics (build_diagnostics runs after reconcile and takes its output), so it
# can never know whether something worse is being said, which is precisely why it
# must not rank the failing check at all.
# ---------------------------------------------------------------------------
.trust_bal_pass_rbc_fail <- function() {
  kpis <- rbind(
    .kpi("balance_reconciliation", "pass", expected = 130, actual = 130, discrepancy = 0),
    .kpi("running_balance_continuity", "fail", expected = 0, actual = 2, discrepancy = 2),
    .kpi("transaction_count", "pass", expected = ">0", actual = 3))
  tx <- data.frame(flags = rep("", 3), stringsAsFactors = FALSE)
  .reconcile_trust(kpis, tx, list(stated_count = 3), 3L)
}

test_that("the medium-not-low reason states the proof and downplays nothing", {
  tr <- .trust_bal_pass_rbc_fail()
  expect_identical(tr$level, "medium")               # the rating itself is unchanged
  why <- paste(tr$reasons, collapse = " | ")
  expect_match(why, "balance fully reconciles", fixed = TRUE)   # the fact that earns it
  # ...and nothing that ranks, excuses or normalises the check that failed
  expect_false(grepl("secondary", why, ignore.case = TRUE))
  expect_false(grepl("commonly flag", why, ignore.case = TRUE))
  expect_match(why, "does NOT clear", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# A FAILING CHECK'S DETAIL IS RENDERED UNDER TWO DIFFERENT NAMES (G5).
#
# build_diagnostics() re-emits every failing check as a diagnostic carrying this
# exact string, and the verdict card prints the diagnostics and the failing checks
# in ONE list -- so a failing check's detail appears twice on one screen, once
# under the diagnostic's wording ("rows didn't parse") and once under the check's
# own, which for a check named as what it PROVES states the opposite of what
# happened ("No row failed to read"). This file cannot stop it being listed twice;
# it can make sure the sentence leads with the fault, so it stays true under a
# heading worded either way.
# ---------------------------------------------------------------------------
test_that("every failing check's detail leads with what went wrong", {
  fails <- list(
    .kpi_no_unparsed_rows(list(transactions = data.frame(flags = rep("", 0)),
                               source_line_count = NA_integer_, visual_row_count = 31L,
                               skipped_row_count = 31L, actionable_skip_count = 12L),
                          data.frame(flags = character(0)), 0L),
    .kpi_dates_readable(data.frame(date = c("2026-01-01", NA), stringsAsFactors = FALSE), 2L),
    .kpi_running_balance_continuity(
      data.frame(balance = c(100, 130), amount = c(0, 5), stringsAsFactors = FALSE), 2L),
    .kpi_transaction_count(list(stated_count = 9L), 7L))
  for (k in fails) {
    expect_identical(k$status, "fail", info = k$name)
    # not empty, and not a bare restatement of the check's own reassuring name
    expect_true(nzchar(k$detail), info = k$name)
    expect_false(grepl("^(all|no|every|each) ", k$detail, ignore.case = TRUE), info = k$name)
  }
})
