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

test_that("positional (region) fields read the value by location, not by label", {
  mkw <- function(text, x, y, wd = 20, ht = 8)
    data.frame(text = text, x = x, y = y, width = wd, height = ht, stringsAsFactors = FALSE)
  # label 'Total GST' top-left; the value sits bottom-right with no nearby label
  page <- do.call(rbind, list(
    mkw("Total", 40, 40), mkw("GST", 70, 40),
    mkw("Acme", 40, 200), mkw("Ltd", 75, 200),
    mkw("$1,234.56", 400, 320)))
  input <- list(words = list(page), pages = "Total GST Acme Ltd $1,234.56")

  # money value pulled from its box, far from the label
  f <- extract_fields(input, list(fields = list(
    gst = list(region = list(page = 1, x_min = 380, x_max = 460, y_min = 310, y_max = 330), value = "money"))))
  expect_true(f$matched); expect_equal(f$value, "$1,234.56")

  # text box (default type) grabs the box contents in reading order
  f2 <- extract_fields(input, list(fields = list(
    name = list(at = list(page = 1, x_min = 35, x_max = 90, y_min = 195, y_max = 210)))))
  expect_equal(f2$value, "Acme Ltd")

  # an empty box is honestly "not found" (never a wrong guess)
  f3 <- extract_fields(input, list(fields = list(
    x = list(region = list(page = 1, x_min = 900, x_max = 950)))))
  expect_false(f3$matched)
  expect_true(is.na(f3$value))
})

# --- A NEGATIVE MUST SURVIVE EXTRACTION --------------------------------------
# The money pattern allowed a minus only INSIDE the currency symbol ("$-577.80"),
# so the two forms banks actually print - a minus BEFORE the symbol, and a
# trailing minus - matched without their sign. A deduction came out positive.
# The ANZ KiwiSaver sample shipped in this repo prints "-$577.80" for tax, so the
# one shipped form template was reading it as a POSITIVE $577.80: a silently wrong
# figure in the exact path a reviewer would trust.

test_that("every way a bank prints a negative keeps its sign", {
  grab <- function(v) {
    m <- regmatches(v, gregexpr(.MONEY_RX, v, perl = TRUE, ignore.case = TRUE))[[1]]
    if (length(m)) m[length(m)] else NA_character_
  }
  expect_identical(grab("Tax -$577.80"),      "-$577.80")   # minus before the symbol
  expect_identical(grab("Tax $-577.80"),      "$-577.80")   # minus inside
  expect_identical(grab("Tax 577.80-"),       "577.80-")    # trailing (accounting)
  expect_identical(grab("Tax ($577.80)"),     "($577.80)")  # brackets
  expect_identical(grab("Tax -$1,234.56"),    "-$1,234.56") # with thousands
  expect_identical(grab("Fees $489.22"),      "$489.22")    # positives unchanged
})

test_that("the money pattern still does not swallow dates or ranges", {
  # The trailing minus must not turn "2025-04-01" into money. Both alternatives
  # require cents or a leading "$", which no date carries.
  none <- function(v) length(regmatches(v, gregexpr(.MONEY_RX, v, perl = TRUE))[[1]]) == 0
  expect_true(none("1 April 2025"))
  expect_true(none("2025-04-01"))
  expect_true(none("period 1-2"))
})

test_that("the shipped form template reads its own sample with the right signs", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  p <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(p))
  out <- tempfile("formout_"); dir.create(out)
  r <- convert_document(p, outdir = out, templates_dir = fixture("templates"),
                        user_templates_dir = NULL,
                        fields_dir = fixture("fields_templates"), user_fields_dir = NULL,
                        logdir = out)
  expect_identical(r$kind, "form")
  expect_identical(r$status, "ok")
  v <- setNames(r$fields$value, r$fields$field)
  # deductions are NEGATIVE, as the document prints them
  expect_match(v[["tax"]],  "^-", info = paste("tax came out as", v[["tax"]]))
  expect_match(v[["fees"]], "^-", info = paste("fees came out as", v[["fees"]]))
  # ...and the balances are not
  expect_false(grepl("^-", v[["opening_balance"]]))
  expect_false(grepl("^-", v[["closing_balance"]]))
})
