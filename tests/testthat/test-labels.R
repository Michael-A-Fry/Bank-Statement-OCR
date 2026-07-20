# Tests for the declarative label dictionary + matcher (R/labels.R).
# This is the generic answer to "hundreds of different wordings" for labelled
# scalar values. All pattern/config-driven -- no bank-specific logic.

test_that("synonyms: any_of matches whichever wording a statement uses", {
  spec <- list(any_of = c("opening balance", "balance brought forward",
                          "starting balance"), value = "money")
  expect_equal(match_label(spec, "Opening balance            $1,000.00")$value, "$1,000.00")
  expect_equal(match_label(spec, "Balance brought forward     $2,500.50")$value, "$2,500.50")
  expect_equal(match_label(spec, "STARTING BALANCE: $3,000.00")$value, "$3,000.00")
  # absent -> NA, not an error, and matched is FALSE
  r <- match_label(spec, "no balance line here")
  expect_true(is.na(r$value)); expect_false(r$matched)
})

test_that("value sits on the label line, else the next line", {
  spec <- list(any_of = "closing balance", value = "money")
  # same line
  expect_equal(match_label(spec, "Closing balance  $9.99")$value, "$9.99")
  # value on the following line (label is a heading)
  pages <- "Closing balance\n$42.00"
  expect_equal(match_label(spec, pages)$value, "$42.00")
})

test_that("occurrence controls which of several matches is returned", {
  pages <- c("Fee $1.00", "Fee $2.00", "Fee $3.00")
  base <- list(any_of = "Fee", value = "money")
  expect_equal(match_label(c(base, list(occurrence = "first")), pages)$value, "$1.00")
  expect_equal(match_label(c(base, list(occurrence = "last")),  pages)$value, "$3.00")
  expect_equal(match_label(c(base, list(occurrence = "all")),   pages)$value, "$1.00; $2.00; $3.00")
})

test_that("conflicting repeats are flagged (never silently guessed)", {
  pages <- c("Total $10.00", "Total $99.00")
  r <- match_label(list(any_of = "Total", value = "money"), pages)   # default on_conflict: flag
  expect_true(r$conflict)
  expect_equal(r$value, "$10.00")                    # keeps first, but conflict=TRUE
  r2 <- match_label(list(any_of = "Total", value = "money", on_conflict = "last"), pages)
  expect_equal(r2$value, "$99.00")
})

test_that("where scopes the search to a page", {
  pages <- c("Statement date 01/02/2025 page one",
             "Statement date 09/09/2099 back page")
  expect_equal(match_label(list(any_of = "Statement date", value = "date",
                                where = "page1"), pages)$value, "01/02/2025")
  expect_equal(match_label(list(any_of = "Statement date", value = "date",
                                where = "last_page"), pages)$value, "09/09/2099")
})

test_that("value types: text keeps the verbatim remainder; regex/date extract", {
  expect_equal(match_label(list(any_of = "Account name", value = "text"),
                           "Account name: O'Connor & Sons")$value, "O'Connor & Sons")
  expect_equal(match_label(list(any_of = "IRD", pattern = "[0-9]{2,3}-[0-9]{3}-[0-9]{3}"),
                           "IRD number 123-456-789")$value, "123-456-789")
  expect_equal(match_label(list(any_of = "Period", value = "date_range"),
                           "Period 1 April 2025 to 31 March 2026")$value,
               "1 April 2025 | 31 March 2026")
})

test_that("a bare string or {label:} spec still works (back-compat)", {
  expect_equal(match_label("opening balance", "Opening balance $5.00")$value, "$5.00")
  expect_equal(match_label(list(label = "opening balance"),
                           "Opening balance $6.00")$value, "$6.00")
})

test_that("the shipped base dictionary loads and carries synonyms", {
  d <- default_label_dict()
  skip_if(length(d) == 0)   # only meaningful when the yaml resolves
  expect_true("opening_balance" %in% names(d))
  expect_true(any(grepl("brought forward", .spec_terms(d$opening_balance))))
})

test_that("extract_fields flags a required-but-missing field", {
  tmpl <- list(fields = list(
    total = list(any_of = "Grand total", value = "money", required = TRUE)))
  f <- extract_fields(list(pages = "nothing relevant here"), tmpl, dict = list())
  expect_true(f$flagged[f$field == "total"])
  expect_false(f$matched[f$field == "total"])
})

test_that("extract_fields inherits dictionary synonyms by field name", {
  dict <- list(opening_balance = list(
    any_of = c("opening balance", "balance brought forward"), value = "money"))
  tmpl <- list(fields = list(opening_balance = list()))   # no wording of its own
  f <- extract_fields(list(pages = "Balance brought forward   $77.00"), tmpl, dict = dict)
  expect_equal(f$value[f$field == "opening_balance"], "$77.00")
})
