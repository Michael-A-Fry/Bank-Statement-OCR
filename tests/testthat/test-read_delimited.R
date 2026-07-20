# Delimited reader resilience + completeness (build-contract sections 11.3/11.5).
# Ragged rows must be ISOLATED and flagged 'malformed' without corrupting their
# clean neighbours; merged/lost records must be caught by no_unparsed_rows;
# multi-line quoted records must keep per-row provenance; malformed quoting must
# never crash the whole statement.

.bnz_tmpl <- function() {
  load_templates(templates_dir())[["bnz_everyday_csv"]]
}

test_that("a too-LONG row is flagged malformed and neighbours stay intact", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_ragged_long.csv"))
  p <- parse_statement(input, .bnz_tmpl())
  tx <- p$transactions
  expect_equal(nrow(tx), 3L)
  # offending row 1 flagged; clean rows carry no flags
  expect_true(grepl("malformed", tx$flags[1]))
  expect_identical(tx$flags[2:3], c("", ""))
  # neighbours parsed byte-for-byte correctly (no column shift)
  expect_equal(tx$amount[2:3], c(-100, 11.5))
  expect_identical(tx$date[2:3], c("2026-01-02", "2026-01-03"))
  expect_identical(tx$description[2:3], c("Savings", "Friend Share"))
  # completeness KPI fails (a row could not be accounted for cleanly)
  k <- reconcile(p, .bnz_tmpl())$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "fail")
})

test_that("a too-SHORT row is flagged malformed and neighbours stay intact", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_ragged_short.csv"))
  p <- parse_statement(input, .bnz_tmpl())
  tx <- p$transactions
  expect_equal(nrow(tx), 2L)
  expect_true(grepl("malformed", tx$flags[1]))
  expect_identical(tx$flags[2], "")
  expect_equal(tx$amount[2], -100)
  expect_identical(tx$description[2], "Savings")
  k <- reconcile(p, .bnz_tmpl())$kpis
  expect_equal(k$status[k$name == "no_unparsed_rows"], "fail")
})

test_that("an embedded-newline quoted record stays one row with provenance", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_embedded_newline.csv"))
  p <- parse_statement(input, .bnz_tmpl())
  tx <- p$transactions
  expect_equal(nrow(tx), 2L)
  # description preserved verbatim, including the embedded newline
  expect_identical(tx$description[1], "OConnor & Sons\nLTD")
  expect_equal(tx$amount, c(-25, 11.5))
  # provenance is populated per row (NOT blanked); the multi-line span recorded
  expect_equal(nrow(p$provenance), 2L)
  expect_false(any(is.na(p$provenance$source_ref)))
  expect_false(any(is.na(p$provenance$raw)))
  expect_identical(p$provenance$source_ref[1], "csv:line=2-3")
})

test_that("a cross-line stray quote that merges records is caught, not silent", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_merged_quote.csv"))
  p <- parse_statement(input, .bnz_tmpl())
  k <- reconcile(p, .bnz_tmpl())$kpis
  row <- k[k$name == "no_unparsed_rows", ]
  # 3 non-empty source data lines but fewer good rows -> completeness FAILS
  expect_equal(row$status, "fail")
  expect_equal(as.integer(row$expected), 3L)
  expect_gt(3L - as.integer(row$actual), 0L)
})

test_that("an unbalanced quote never fails the whole statement", {
  input <- read_input(fixture("tests/testthat/fixtures/bnz_merged_quote.csv"))
  # convert_statement must never throw; it degrades to needs_review, not failed.
  expect_error(parse_statement(input, .bnz_tmpl()), NA)
})
