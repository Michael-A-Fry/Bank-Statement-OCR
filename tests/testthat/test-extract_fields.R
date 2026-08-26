# Tests for the key-value / IRD-style extraction paradigm (R/extract_fields.R).

IF_KS_PDF <- "samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf"

test_that("labelled values are extracted from a KiwiSaver summary", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(IF_KS_PDF)))
  tmpl <- yaml::read_yaml(file.path(fields_templates_dir(),
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
  r <- convert_document(p, outdir = out, templates_dir = templates_dir(),
                        user_templates_dir = NULL,
                        fields_dir = fields_templates_dir(), user_fields_dir = NULL,
                        logdir = out)
  expect_identical(r$kind, "form")
  # needs_review, not ok: this guide document prints Contributions, Tax and Fees
  # more than once with different values. The tool takes the first and SAYS so;
  # it used to pick silently and call the result clean.
  expect_identical(r$status, "needs_review")
  v <- setNames(r$fields$value, r$fields$field)
  # deductions are NEGATIVE, as the document prints them
  expect_match(v[["tax"]],  "^-", info = paste("tax came out as", v[["tax"]]))
  expect_match(v[["fees"]], "^-", info = paste("fees came out as", v[["fees"]]))
  # ...and the balances are not
  expect_false(grepl("^-", v[["opening_balance"]]))
  expect_false(grepl("^-", v[["closing_balance"]]))
})

# --- G2/G2b: A FORM VALUE MUST CARRY ITS OWN EVIDENCE -------------------------
# A form has no reconciliation behind it, so the only things standing between a
# figure and a reviewer are: was this label printed more than once, what were the
# other readings, and where on the document did this one come from. All three
# were computed and thrown away - `conflict` reached a screen that gets closed,
# and the page and the "how" were never worked out at all, while the report
# route's pairs have carried page and found_by all along.

test_that("a value carries the page it was read from and how it was found", {
  input <- list(pages = c(
    "Kowhai Scheme annual statement\nOpening balance   $1,000.00",
    "Continued\nsome prose here\nClosing balance   $2,500.00"))
  tmpl <- list(fields = list(
    opening_balance = list(any_of = "Opening balance", value = "money"),
    closing_balance = list(any_of = "Closing balance", value = "money")))
  f <- extract_fields(input, tmpl, dict = list())
  expect_true(all(c("page", "found_by") %in% names(f)))
  expect_identical(f$page[f$field == "opening_balance"], 1L)
  # THE POINT OF THE TEST: page 2, not page 1 and not NA. match_label flattens
  # every page into one string, so before this the page was simply not knowable.
  expect_identical(f$page[f$field == "closing_balance"], 2L)
  # ...in the report route's own words, so one product does not have two
  # vocabularies for the same fact.
  expect_true(all(f$found_by == "its wording"))
})

test_that("a positional field says it was found by its place, on its own page", {
  mkw <- function(text, x, y, wd = 20, ht = 8)
    data.frame(text = text, x = x, y = y, width = wd, height = ht,
               stringsAsFactors = FALSE)
  p1 <- mkw("nothing", 40, 40)
  p2 <- do.call(rbind, list(mkw("$1,234.56", 400, 320)))
  input <- list(words = list(p1, p2), pages = c("nothing", "$1,234.56"))
  f <- extract_fields(input, list(fields = list(
    gst = list(region = list(page = 2, x_min = 380, x_max = 460,
                             y_min = 310, y_max = 330), value = "money"))),
    dict = list())
  expect_true(f$matched)
  expect_identical(f$page, 2L)
  expect_identical(f$found_by, "its place on the page")
})

test_that("a label printed twice reports HOW MANY and WHAT IT DID NOT TAKE", {
  input <- list(pages = paste(
    "Fund A", "Opening balance   $1,000.00",
    "Fund B", "Opening balance   $2,000.00", sep = "\n"))
  tmpl <- list(fields = list(
    opening_balance = list(any_of = "Opening balance", value = "money")))
  f <- extract_fields(input, tmpl, dict = list())
  expect_true(f$conflict)
  expect_identical(f$value, "$1,000.00")      # the first, as it always was
  expect_identical(f$n, 2L)                   # ...but it says there were two
  # ...and it says what the other one WAS. A boolean tells a reviewer a figure is
  # disputed without telling her what disputes it, and the disputing figure is
  # the one thing she can check against the document in five seconds.
  expect_identical(f$other_values, "$2,000.00")
})

test_that("a clean field carries no alternatives and counts one reading", {
  input <- list(pages = "Opening balance   $1,000.00")
  f <- extract_fields(input, list(fields = list(
    opening_balance = list(any_of = "Opening balance", value = "money"))),
    dict = list())
  expect_false(f$conflict)
  expect_identical(f$other_values, "")
  expect_identical(f$n, 1L)
})

test_that("occurrence: all keeps every reading in the value and repeats none beside it", {
  input <- list(pages = paste("Opening balance   $1,000.00",
                              "Opening balance   $2,000.00", sep = "\n"))
  f <- extract_fields(input, list(fields = list(
    opening_balance = list(any_of = "Opening balance", value = "money",
                           occurrence = "all"))), dict = list())
  expect_match(f$value, "1,000.00", fixed = TRUE)
  expect_match(f$value, "2,000.00", fixed = TRUE)
  expect_identical(f$other_values, "")   # already in the value; saying it twice
})                                       # would read as a disagreement

test_that(".field_scope_pages answers exactly what .pages_in_scope does", {
  # Two functions, one rule: one returns the page TEXT, the other the page
  # NUMBERS, and the page a value is credited to is wrong the moment they differ.
  pages <- c("one", "two", "three")
  for (w in list(NULL, "any", "page1", "last_page", 2, 9, "nonsense")) {
    idx <- .field_scope_pages(length(pages), w)
    expect_identical(pages[idx], .pages_in_scope(pages, w),
                     info = paste("where =", paste(w, collapse = ",")))
  }
  expect_identical(.field_scope_pages(0L, "any"), integer(0))
})
