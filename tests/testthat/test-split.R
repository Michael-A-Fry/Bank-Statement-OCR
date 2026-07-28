# Deterministic auto-split of statement bundles (R/split.R).
# The forensic contract: split ONLY when the boundaries are independently confirmed
# (an independent count agrees, or every segment's balance math ties out).
# Otherwise refuse -> the caller keeps the flag-and-refuse default.
#
# It is no longer OPT-IN. Only one of thirteen shipped templates ever declared a
# `split:` block, so a bundle read by any of the others got no split at all; the
# opt-in was a second lock on a door the commit gate already holds shut. Explicit
# `split: false` still turns it off for a template that must never be cut.

# .sp_words -- two transaction rows (day/amount/balance) as positioned word boxes,
# with the month name supplied so a statement can sit in its own month.
.sp_words <- function(mon, d1, amt1, bal1, d2, amt2, bal2) {
  rows <- list(
    c("Date", 40, 10, 20), c("Amount", 340, 10, 40), c("Balance", 470, 10, 40),
    c(d1, 45, 40, 12), c(mon, 60, 40, 16), c("DESCA", 100, 40, 45), c(amt1, 345, 40, 34), c(bal1, 472, 40, 34),
    c(d2, 45, 70, 12), c(mon, 60, 70, 16), c("DESCB", 100, 70, 45), c(amt2, 345, 70, 34), c(bal2, 472, 70, 34))
  data.frame(text = vapply(rows, `[`, "", 1),
    x = as.numeric(vapply(rows, `[`, "", 2)), y = as.numeric(vapply(rows, `[`, "", 3)),
    width = as.numeric(vapply(rows, `[`, "", 4)), height = rep(10, length(rows)),
    stringsAsFactors = FALSE)
}

.sp_template <- function(split = list(on = "page1_marker")) {
  t <- list(id = "synth_split", bank = "SYNTH", statement_type = "everyday", format = "pdf",
    version = 1, currency = "NZD", min_score = 1,
    fingerprint = list(page_contains_all = list("Statement period")),
    table = list(row_tol = 3, date_format = "%d %b", amount_sign = "signed",
      columns = list(date = list(x_min = 40, x_max = 80), description = list(x_min = 80, x_max = 330),
        amount = list(x_min = 330, x_max = 460), balance = list(x_min = 460, x_max = 545))))
  if (!is.null(split)) t$split <- split
  t
}

# A clean 2-statement bundle: distinct periods, a "Page 1 of 1" per statement, and
# a running balance that ties out within each statement.
.sp_bundle <- function() list(kind = "pdf", path = tempfile(fileext = ".pdf"),
  pages = c("Statement period from 1 Jan 2026 to 31 Jan 2026  Page 1 of 1",
            "Statement period from 1 Feb 2026 to 28 Feb 2026  Page 1 of 1"),
  words = list(.sp_words("Jan", "05", "-4.50", "95.50", "06", "1000.00", "1095.50"),
               .sp_words("Feb", "03", "-50.00", "1045.50", "10", "200.00", "1245.50")),
  page_width = c(595.28, 595.28), page_height = c(841.89, 841.89),
  meta = list(page_count = 2L))

test_that("a confirmed bundle splits, tags rows, and rolls trust to the weakest", {
  sb <- split_bundle(.sp_bundle(), .sp_template())
  expect_false(is.null(sb))
  expect_equal(sb$n_statements, 2L)
  expect_equal(nrow(sb$parsed$transactions), 4L)
  # rows carry the statement they came from
  expect_equal(sb$parsed$transactions$statement_index, c(1, 1, 2, 2))
  # each statement's dates took ITS OWN period year/month
  expect_equal(sb$parsed$transactions$date,
               c("2026-01-05", "2026-01-06", "2026-02-03", "2026-02-10"))
  # KPIs are stacked per statement
  expect_true(all(grepl("\\[statement 1\\]", sb$recon$kpis$name[1:7])))
  expect_true(any(grepl("\\[statement 2\\]", sb$recon$kpis$name)))
  # per-statement summary present
  expect_equal(length(sb$statements), 2L)
  expect_equal(sb$statements[[2]]$period_end, "28 Feb 2026")
  # trust rolls up to the WEAKEST segment (both medium here -> medium)
  expect_true(sb$recon$trust$level %in% c("low", "medium", "high"))
  seg_levels <- vapply(sb$statements, function(s) s$trust_level, character(1))
  expect_identical(sb$recon$trust$level,
                   c("low", "medium", "high")[min(match(seg_levels, c("low", "medium", "high")))])
  # per-statement identity fields are nulled in the combined header (the feed
  # stamps header onto every row, so one account/balance must not mislabel others)
  expect_true(is.na(sb$parsed$header$account_number))
  expect_true(is.na(sb$parsed$header$opening_balance))
  expect_equal(sb$parsed$header$page_count, 2L)
})

test_that("a template that declares nothing still splits a CONFIRMED bundle", {
  # This used to assert the opposite. The opt-in was refusing bundles the commit
  # gate would have accepted, which is what left twelve of thirteen templates
  # unable to split at all.
  sb <- split_bundle(.sp_bundle(), .sp_template(split = NULL))
  expect_false(is.null(sb))
  expect_equal(sb$n_statements, 2L)
})

test_that("an explicit split: false still refuses", {
  # The opt-OUT survives, for a template that must never be cut.
  expect_null(split_bundle(.sp_bundle(), .sp_template(split = FALSE)))
})

test_that("a single statement is never split", {
  one <- .sp_bundle()
  one$pages <- one$pages[1]; one$words <- one$words[1]
  one$page_width <- one$page_width[1]; one$page_height <- one$page_height[1]
  one$meta$page_count <- 1L
  expect_null(split_bundle(one, .sp_template()))
})

test_that("unconfirmed boundaries are refused, even when every segment reconciles", {
  # THE forensic guard: two pages with the SAME period (n_periods = 1) so no
  # independent count corroborates a 2-way split -- but each segment's running
  # balance ties out perfectly (a continuous statement stays continuous across any
  # cut). Reconciliation alone must NOT be allowed to commit the split; only an
  # independent count can. So this must refuse.
  same_period <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period from 1 Jan 2026 to 31 Jan 2026 Page 1 of 1",
              "Statement period from 1 Jan 2026 to 31 Jan 2026 Page 1 of 1"),
    words = list(.sp_words("Jan", "05", "-4.50", "95.50", "06", "1000.00", "1095.50"),
                 .sp_words("Jan", "07", "-50.00", "1045.50", "10", "200.00", "1245.50")),
    page_width = c(595.28, 595.28), page_height = c(595.28, 595.28), meta = list(page_count = 2L))
  expect_null(split_bundle(same_period, .sp_template()))
})

test_that(".segment_starts finds page-1 markers and always includes page 1", {
  spec <- .split_spec(.sp_template())
  expect_equal(.segment_starts(.sp_bundle(), spec), c(1L, 2L))
  # a marker only on a later page still makes page 1 the first segment start
  inp <- .sp_bundle(); inp$pages[1] <- "Statement period from 1 Jan 2026 to 31 Jan 2026 (no marker)"
  expect_equal(.segment_starts(inp, spec), c(1L, 2L))
})

test_that(".subinput_pages yields a standalone one-statement input", {
  si <- .subinput_pages(.sp_bundle(), 2L)
  expect_equal(length(si$pages), 1L)
  expect_equal(si$meta$page_count, 1L)
  expect_match(si$pages[1], "Feb")
})

test_that(".split_spec is on by default and off only when explicitly refused", {
  expect_equal(.split_spec(list())$on, "page1_marker")   # nothing declared -> on
  expect_null(.split_spec(list(split = FALSE)))          # only this turns it off
  expect_equal(.split_spec(list(split = TRUE))$on, "page1_marker")
  expect_equal(.split_spec(list(split = list(on = "opening_label")))$on, "opening_label")
  expect_equal(.split_spec(list(split = list(min_statements = 5)))$min_statements, 5L)
  # an unknown signal falls back to the safe default
  expect_equal(.split_spec(list(split = list(on = "nonsense")))$on, "page1_marker")
})

test_that("validate_template rejects a bad split block, accepts a good one", {
  good <- c(.sp_template(), list())
  expect_length(validate_template(good), 0)
  # split on a non-PDF template
  csv <- list(id = "x", bank = "B", statement_type = "e", format = "delimited",
    amount_sign = "signed", date_format = "%d/%m/%Y", min_score = 1,
    fingerprint = list(header_contains_all = list("Date")),
    columns = list(date = list(source = "Date"), amount = list(source = "Amount")),
    split = TRUE)
  expect_true(any(grepl("split is only supported for pdf", validate_template(csv))))
  # bad signal / bad count
  bad <- .sp_template(list(on = "bogus", min_statements = 1))
  probs <- validate_template(bad)
  expect_true(any(grepl("split.on", probs)))
  expect_true(any(grepl("min_statements", probs)))
})

# ---------------------------------------------------------------------------
# THE OPT-IN (#61). Everything above proves R/split.R works on a synthetic input.
# None of it proved any SHIPPED template ever asks for it -- and until
# anz_everyday_pdf carried a `split:` block, none did, so the whole subsystem was
# shipped, tested and dark while the commonest real shape a forensic accountant is
# handed (a year of one account's statements in one PDF) still dead-ended.
#
# The fixture is a synthetic 2-statement ANZ bundle drawn at the SHIPPED template's
# own column bands (tests/testthat/fixtures/make_pdf_fixtures.R). It reproduces the
# real failure in miniature: parsed as ONE statement it fails balance_reconciliation
# and puts 3 dates outside the period; split, every per-statement KPI passes.
# ---------------------------------------------------------------------------

.anz_bundle_fixture <- function() fixture("tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf")

test_that("the shipped ANZ template needs no split block to split", {
  # It used to carry `split: {on: page1_marker, min_statements: 2}` - which is now
  # exactly the default, so the block said nothing and implied ANZ was special.
  # What matters is the SETTINGS that apply, not that a file restates them.
  t <- load_templates(templates_dir())[["anz_everyday_pdf"]]
  expect_null(t$split)
  spec <- .split_spec(t)
  expect_equal(spec$on, "page1_marker")
  expect_equal(spec$min_statements, 2L)
})

test_that("the ANZ bundle splits into its statements with every KPI passing", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(.anz_bundle_fixture()))
  tp <- load_templates(templates_dir())
  input <- read_input(.anz_bundle_fixture())

  # it really is detected as the shipped ANZ template, and really does look like a
  # bundle to the independent multi-statement check that gates the split.
  det <- detect_statement(input, tp)
  expect_identical(det$template_id, "anz_everyday_pdf")
  meta <- extract_metadata(input)
  expect_true(isTRUE(detect_multiple_statements(input, meta)$likely_multiple))

  sb <- split_bundle(input, tp[["anz_everyday_pdf"]], meta)
  expect_false(is.null(sb))
  expect_equal(sb$n_statements, 2L)
  # statement 1 spans TWO pages, statement 2 one -> page RANGES, not one-per-page
  expect_identical(vapply(sb$statements, function(s) s$pages, character(1)), c("1-2", "3-3"))

  # every row is tagged with the statement it came from (this is what the feed and
  # the workbook use to keep the statements apart)
  tx <- sb$parsed$transactions
  expect_equal(nrow(tx), 9L)
  expect_equal(tx$statement_index, c(rep(1, 6), rep(2, 3)))
  expect_false(any(is.na(tx$statement_index)))
  # ...and each statement's dates took ITS OWN period year/month
  expect_identical(tx$date[c(1, 6, 7, 9)],
                   c("2026-02-03", "2026-02-26", "2026-03-04", "2026-03-25"))

  # NO per-statement KPI fails; each statement reconciles on its own terms.
  expect_false(any(sb$recon$kpis$status == "fail"))
  for (i in 1:2) {
    ki <- sb$recon$kpis[grepl(sprintf("\\[statement %d\\]$", i), sb$recon$kpis$name), ]
    expect_equal(ki$status[grepl("^balance_reconciliation", ki$name)], "pass")
    expect_equal(ki$status[grepl("^dates_within_period", ki$name)], "pass")
    expect_equal(ki$status[grepl("^running_balance_continuity", ki$name)], "pass")
  }
  expect_equal(vapply(sb$statements, function(s) s$rows, numeric(1)), c(6, 3))
})

test_that("WITHOUT splitting the same bundle fails - this is what #61 fixes", {
  # The before-picture, kept as a test so splitting can never be quietly dropped:
  # one statement's opening balance is reconciled against two statements of
  # transactions, and statement 2's dates fall outside statement 1's period.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(.anz_bundle_fixture()))
  t <- load_templates(templates_dir())[["anz_everyday_pdf"]]
  t$split <- FALSE            # the explicit opt-OUT, now the only way off
  expect_null(split_bundle(read_input(.anz_bundle_fixture()), t))
  p <- parse_statement(read_input(.anz_bundle_fixture()), t)
  k <- reconcile(p, t)$kpis
  expect_equal(k$status[k$name == "balance_reconciliation"], "fail")
  expect_equal(k$status[k$name == "dates_within_period"], "fail")
})

test_that("a genuine SINGLE ANZ statement is never split by the opt-in", {
  # The opt-in must cost nothing for the ordinary case: one statement has one
  # "Page 1 of N" marker, so it never reaches min_statements.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  f <- fixture("tests/testthat/fixtures/anz_everyday_pdf_sample.pdf")
  skip_if_not(file.exists(f))
  tp <- load_templates(templates_dir())
  expect_null(split_bundle(read_input(f), tp[["anz_everyday_pdf"]]))
})

test_that("a bundle from a template that declares nothing converts cleanly", {
  # The measurement that justified dropping the opt-in. A two-statement Westpac
  # bundle is read by westpac_everyday_pdf, which declares no split: block. Under
  # the opt-in it fell through to the merged parse - needs_review, trust LOW, with
  # balance_reconciliation AND running_balance_continuity both failing, which is
  # the exact symptom auto-split exists to remove.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  src <- file.path(engine_root(), "samples/_private_staging/wp_2025.pdf")
  skip_if_not(file.exists(src))
  bundle <- tempfile(fileext = ".pdf")
  pdftools::pdf_combine(c(src, src), output = bundle)
  t <- load_templates(templates_dir())[["westpac_everyday_pdf"]]
  expect_null(t$split)                      # it really does declare nothing
  sb <- split_bundle(read_input(bundle), t)
  expect_false(is.null(sb))
  expect_equal(sb$n_statements, 2L)
  expect_true(any(grepl("\\[statement 2\\]", sb$recon$kpis$name)))
})

# ---------------------------------------------------------------------------
# N1xx: THE BUNDLE'S PERIOD -- and why an empty one is the right answer.
#
# The combined header used to take statement 1's period_start and statement k's
# period_end IN FILE ORDER. Bank bundles are routinely filed newest-first, so on
# samples/_private_staging/anz_0382004_multi.pdf the card, the header and the
# governed feed's run manifest all published
#     "Statement period: 20 Apr 2026 to 19 Feb 2026"
# -- a period that ENDS TWO MONTHS BEFORE IT STARTS -- and the manifest row said
# "accepted". Nothing on screen and nothing in the checks noticed.
#
# Date order fixes the direction. It does NOT settle whether a bundle should
# publish one period at all: on that same file the three statements leave
# 18-19 Apr 2026 covered by no printed statement, so min..max would silently
# assert two days of coverage the file does not have. So the rule is: publish the
# true span when the parts JOIN UP, publish nothing when they do not, and say why.
# ---------------------------------------------------------------------------

# .sp_period_bundle(p1, p2) -- a 2-statement bundle whose two printed periods are
# whatever the caller asks for. Only the period lines differ from .sp_bundle();
# the transactions stay in Jan/Feb so both segments still parse and reconcile.
.sp_period_bundle <- function(p1, p2) {
  b <- .sp_bundle()
  b$pages <- c(sprintf("Statement period from %s  Page 1 of 1", p1),
               sprintf("Statement period from %s  Page 1 of 1", p2))
  b
}

test_that("a bundle's period is the true span in DATE order, not file order", {
  # Newest-first, exactly as the real ANZ bundle is filed: statement 1 is the
  # LATER period. File order would publish "1 Feb 2026 to 31 Jan 2026".
  sb <- split_bundle(.sp_period_bundle("1 Feb 2026 to 28 Feb 2026",
                                       "1 Jan 2026 to 31 Jan 2026"), .sp_template())
  expect_false(is.null(sb))
  h <- sb$parsed$header
  expect_identical(h$period_start, "1 Jan 2026")     # earliest START...
  expect_identical(h$period_end,   "28 Feb 2026")    # ...latest END
  # verbatim, as the statements print it -- not reformatted behind her back
  expect_true(all(vapply(sb$statements, function(s) !is.na(s$period_start), logical(1))))
  # and the file order really was the other way round
  expect_identical(sb$statements[[1]]$period_start, "1 Feb 2026")
})

test_that("a bundle with a HOLE in it publishes no period, and names the hole", {
  # This is the real sample's shape in miniature: 1-31 Jan then 3-28 Feb, so
  # 1-2 Feb is covered by nothing. A merged span would claim those two days.
  sb <- split_bundle(.sp_period_bundle("1 Jan 2026 to 31 Jan 2026",
                                       "3 Feb 2026 to 28 Feb 2026"), .sp_template())
  expect_false(is.null(sb))
  expect_true(is.na(sb$parsed$header$period_start))
  expect_true(is.na(sb$parsed$header$period_end))
  # ...and the reason is on the same screen as the split, never left to silence
  why <- paste(sb$recon$trust$reasons, collapse = " | ")
  expect_match(why, "2026-02-01 to 2026-02-02", fixed = TRUE)
  expect_match(why, "a statement is missing", fixed = TRUE)
  # the per-statement truth is untouched -- nothing was lost by publishing nothing
  expect_identical(sb$statements[[2]]$period_start, "3 Feb 2026")
})

test_that("overlapping statements publish no period, and are named as an overlap", {
  # An overlap is a different fault from a gap and has a different cure: these are
  # not one account's consecutive statements at all, so there is no span to state.
  sb <- split_bundle(.sp_period_bundle("1 Jan 2026 to 20 Feb 2026",
                                       "1 Feb 2026 to 28 Feb 2026"), .sp_template())
  expect_false(is.null(sb))
  expect_true(is.na(sb$parsed$header$period_start))
  expect_match(paste(sb$recon$trust$reasons, collapse = " | "),
               "overlap in time", fixed = TRUE)
})

test_that("a shared boundary day is NOT an overlap -- banks print them", {
  # "31 Dec to 31 Jan" then "31 Jan to 28 Feb" is the commonest real multi-period
  # shape there is. .period_span already refuses to call it an overlap; this must
  # agree, or the engine holds two opinions about when periods join up.
  p <- .bundle_period(list(
    list(period_start = "31 Dec 2025", period_end = "31 Jan 2026"),
    list(period_start = "31 Jan 2026", period_end = "28 Feb 2026")))
  expect_identical(p$start, "31 Dec 2025")
  expect_identical(p$end, "28 Feb 2026")
  expect_true(is.na(p$why))
})

test_that(".bundle_period refuses when a statement's own period is unreadable", {
  # One unreadable bound means the ORDER of the whole set is unknown, and an
  # unknown order is exactly when a guess produces a wrong pairing.
  p <- .bundle_period(list(
    list(period_start = "1 Jan 2026", period_end = "31 Jan 2026"),
    list(period_start = NA_character_, period_end = "28 Feb 2026")))
  expect_true(is.na(p$start))
  expect_match(p$why, "readable period", fixed = TRUE)
})

test_that(".bundle_period refuses a statement whose OWN period runs backwards", {
  p <- .bundle_period(list(list(period_start = "28 Feb 2026", period_end = "1 Feb 2026")))
  expect_true(is.na(p$start))
  expect_match(p$why, "ends before it starts", fixed = TRUE)
})

test_that("a backwards bundle period cannot be published, even if one is built", {
  # THE GUARD, tested by defeating the thing it guards. .bundle_period cannot
  # construct a backwards span -- but the author of the code this replaces did not
  # think he could either, so the header is checked as it is built. Stub the span
  # builder into producing the exact backwards period the real sample produced and
  # the whole split must refuse rather than publish it.
  backwards <- function(statements) list(start = "20 Apr 2026", end = "19 Feb 2026",
                                         why = NA_character_)
  where <- environment(split_bundle)          # where split_bundle looks it up
  orig <- get(".bundle_period", envir = where)
  on.exit(assign(".bundle_period", orig, envir = where), add = TRUE)
  assign(".bundle_period", backwards, envir = where)
  expect_error(split_bundle(.sp_bundle(), .sp_template()), "backwards")
  # and through the caller (safe(), R/convert.R) the run falls back to
  # flag-and-refuse rather than reaching an extract with a backwards period on it
  expect_null(safe(split_bundle(.sp_bundle(), .sp_template()), NULL))
  assign(".bundle_period", orig, envir = where)
  # the guard is off again, so the ordinary bundle still splits
  expect_false(is.null(split_bundle(.sp_bundle(), .sp_template())))
})
