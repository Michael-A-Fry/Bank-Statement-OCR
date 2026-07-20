# Redaction guarantee on the delimited path (build-contract 11.2). A redacted
# amount must never be derived: value NA, raw kept as [REDACTED], 'redacted'
# flag set, and counted in redaction_summary.

test_that("a redacted amount is honoured and never derived (CSV path)", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("tests/testthat/fixtures/bnz_redacted_amount.csv"))
  p <- parse_statement(input, templates[["bnz_everyday_csv"]])
  tx <- p$transactions

  # the redacted value is NOT derived
  expect_true(is.na(tx$amount[1]))
  expect_true(is.na(tx$direction[1]))
  # raw is preserved verbatim as the redaction token
  expect_identical(tx$amount_raw[1], "[REDACTED]")
  # the row carries the redacted flag; it is NOT falsely malformed
  expect_true(grepl("redacted", tx$flags[1]))
  expect_false(grepl("malformed", tx$flags[1]))
  # the clean neighbour is untouched
  expect_equal(tx$amount[2], -100)

  # redaction_summary counts the redacted row
  k <- reconcile(p, templates[["bnz_everyday_csv"]])$kpis
  expect_equal(as.integer(k$actual[k$name == "redaction_summary"]), 1L)
})
