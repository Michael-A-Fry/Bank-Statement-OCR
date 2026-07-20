# Tests for the key-value / IRD-style extraction paradigm (R/extract_fields.R).

IF_KS_PDF <- "samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf"

test_that("labelled values are extracted from a KiwiSaver summary", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(IF_KS_PDF)))
  tmpl <- yaml::read_yaml(file.path(engine_root(), "fields_templates",
                                    "anz_kiwisaver_fields.yaml"))
  fields <- extract_fields(read_input(fixture(IF_KS_PDF)), tmpl)
  expect_true(is.data.frame(fields))
  expect_true(all(c("field", "label", "value", "raw") %in% names(fields)))
  get <- function(f) fields$value[fields$field == f]
  expect_equal(get("opening_balance"), "$51,904.55")
  expect_equal(get("closing_balance"), "$61,060.94")
  expect_equal(get("investment_return"), "$4,806.18")
  expect_equal(get("government_contribution"), "$521.43")
})

test_that("extract_fields skips annotation lines without a value", {
  # a label that appears both in prose and in a real value line -> pick the value
  input <- list(pages = c(
    "Government contribution in 2025 was described here with no figure",
    "Government contribution                        $521.43"))
  tmpl <- list(fields = list(gov = list(label = "Government contribution")))
  f <- extract_fields(input, tmpl)
  expect_equal(f$value, "$521.43")
})
