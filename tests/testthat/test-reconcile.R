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
# The signal is not a tuned percentage. It is two like quantities: rows that
# LOOKED like transactions and could not be read (date wouldn't parse, or no
# amount in the money bands) against rows that were read. Headings, summary lines
# and wrapped text are skipped on every healthy statement and are excluded.
# Measured across every sample in this repo: healthy 4-36%, broken 100-1300%.

.thin <- function(n, actionable) {
  parsed <- list(transactions = data.frame(flags = rep("", n), stringsAsFactors = FALSE),
                 source_line_count = NA_integer_, visual_row_count = n + actionable,
                 skipped_row_count = actionable, actionable_skip_count = actionable)
  .kpi_no_unparsed_rows(parsed, parsed$transactions, n)
}

test_that("more rows missed than captured is a FAILURE, not 'cannot be proved'", {
  k <- .thin(n = 1, actionable = 13)          # asb.pdf, measured
  expect_identical(k$status, "fail")
  expect_match(k$detail, "look like transactions but could not be read")
  # the numbers are stated, so a reviewer can check them in the Inspect view
  expect_match(k$detail, "13 row", fixed = TRUE)
})

test_that("a healthy statement's ordinary skips are left alone", {
  # 311 kept / 14 actionable -- anz_single.pdf, measured. Headings and summary
  # lines are normal; flagging these would cry wolf on every real conversion.
  expect_identical(.thin(n = 311, actionable = 14)$status, "na")
  expect_identical(.thin(n = 79,  actionable = 22)$status, "na")   # asb_B.pdf
  expect_identical(.thin(n = 12,  actionable = 0)$status,  "na")
})

test_that("the boundary is 'more missed than captured', with no tuning knob", {
  expect_identical(.thin(n = 10, actionable = 9)$status,  "na")    # fewer missed
  expect_identical(.thin(n = 10, actionable = 10)$status, "fail")  # equal
  expect_identical(.thin(n = 10, actionable = 11)$status, "fail")  # more
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
