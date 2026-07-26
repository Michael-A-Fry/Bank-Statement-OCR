# Opt-in, deterministic auto-split of statement bundles (R/split.R).
# The forensic contract: split ONLY when a template opts in AND the boundaries are
# independently confirmed (an independent count agrees, or every segment's balance
# math ties out). Otherwise refuse -> the caller keeps the flag-and-refuse default.

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

test_that("split is refused unless the template opts in", {
  expect_null(split_bundle(.sp_bundle(), .sp_template(split = NULL)))
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

test_that(".split_spec normalises the opt-in block", {
  expect_null(.split_spec(list()))
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

test_that("anz_everyday_pdf opts in to auto-split", {
  t <- load_templates(templates_dir())[["anz_everyday_pdf"]]
  spec <- .split_spec(t)
  expect_false(is.null(spec))
  expect_identical(spec$on, "page1_marker")
  expect_equal(spec$min_statements, 2L)
  expect_length(validate_template(t), 0L)
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

test_that("WITHOUT the opt-in the same bundle fails - this is what #61 fixes", {
  # The before-picture, kept as a test so the opt-in can never be quietly dropped:
  # one statement's opening balance is reconciled against two statements of
  # transactions, and statement 2's dates fall outside statement 1's period.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(.anz_bundle_fixture()))
  t <- load_templates(templates_dir())[["anz_everyday_pdf"]]
  t$split <- NULL
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
