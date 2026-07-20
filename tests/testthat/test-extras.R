# Template `extras:` mapping + the `fx` flag (build-contract sections 2, 5, 9).
# Per-bank extra columns declared in YAML must be populated, keyed by row_id,
# and never silently dropped; a populated foreign-currency amount sets `fx`.

test_that("declared extras are populated per row and keyed by row_id", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("samples/raw/anz/anz_creditcard_01.csv"))
  p <- parse_statement(input, templates[["anz_creditcard_csv"]])
  ex <- p$extras
  expect_true(all(c("row_id", "card", "posted_date", "fx_amount") %in% names(ex)))
  expect_equal(nrow(ex), nrow(p$transactions))
  expect_identical(ex$card[1], "4879-****-****-6843")
  expect_identical(ex$posted_date[1], "17/02/2020")
  # no foreign currency in this specimen -> fx_amount all NA, no fx flags
  expect_true(all(is.na(ex$fx_amount)))
  expect_false(any(grepl("fx", p$transactions$flags)))
})

test_that("a populated ForeignCurrencyAmount sets the fx flag", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("tests/testthat/fixtures/anz_creditcard_fx.csv"))
  p <- parse_statement(input, templates[["anz_creditcard_csv"]])
  # row 2 carries a foreign-currency amount
  expect_identical(p$extras$fx_amount, c(NA, "8000.00 JPY"))
  expect_false(grepl("fx", p$transactions$flags[1]))
  expect_true(grepl("fx", p$transactions$flags[2]))
})

test_that("a template with no extras block yields an empty extras frame", {
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"))
  p <- parse_statement(input, templates[["bnz_everyday_csv"]])
  expect_equal(nrow(p$extras), 0L)
  expect_identical(names(p$extras), "row_id")
})
