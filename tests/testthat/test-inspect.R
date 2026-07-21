# Tests for the "X-ray" overlay geometry (R/inspect.R).

.mkw <- function(text, x, y, wd = 20, ht = 8, red = FALSE)
  data.frame(text = text, x = x, y = y, width = wd, height = ht,
             redacted = red, stringsAsFactors = FALSE)

.demo_input <- function() {
  page <- do.call(rbind, list(
    .mkw("Date", 50, 40), .mkw("Details", 120, 40), .mkw("Amount", 300, 40),      # header
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),
    .mkw("02/04/2025", 50, 80), .mkw("Rent", 120, 80), .mkw("1,200.00", 300, 80),
    .mkw("Closing", 120, 120), .mkw("Balance", 160, 120), .mkw("3,800.00", 300, 120)))
  list(words = list(page))
}
.demo_tmpl <- function() list(format = "pdf", table = list(
  region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
  date_format = "%d/%m/%Y", amount_sign = "signed",
  columns = list(date = list(x_min = 40, x_max = 110),
                 description = list(x_min = 111, x_max = 280),
                 amount = list(x_min = 281, x_max = 360))))

test_that("inspect_pdf_layout assigns words to columns exactly as the engine", {
  p <- inspect_pdf_layout(.demo_input(), .demo_tmpl())$pages[["1"]]
  # header (y=40) and summary (y=120) are outside the region -> not assigned
  expect_equal(sum(p$words$in_region), 6L)
  incol <- p$words$column[p$words$in_region]
  expect_setequal(incol, c("date", "description", "amount"))
  expect_equal(sum(p$words$column == "amount", na.rm = TRUE), 2L)
})

test_that("inspect_pdf_layout boxes the kept transaction rows only", {
  p <- inspect_pdf_layout(.demo_input(), .demo_tmpl())$pages[["1"]]
  expect_equal(nrow(p$rows), 2L)          # two in-region visual rows
  expect_true(all(p$rows$kept))           # both have a parseable date
  expect_true(all(p$rows$x1 > p$rows$x0 & p$rows$y1 > p$rows$y0))
})

test_that("inspect kept mirrors the engine: no amount / summary rows are NOT kept", {
  # region now includes a date-only line and a 'Closing Balance' summary line
  page <- do.call(rbind, list(
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),  # txn
    .mkw("02/04/2025", 50, 80), .mkw("Statement", 120, 80), .mkw("date", 160, 80),    # dated, NO amount
    .mkw("30/04/2025", 50, 100), .mkw("Closing", 120, 100), .mkw("Balance", 165, 100),
      .mkw("3,800.00", 300, 100)))                                                     # dated summary + amount
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  p <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_equal(nrow(p$rows), 3L)
  # only the real transaction is kept; the date-only line (no amount) and the
  # 'Closing Balance' summary line are NOT - exactly as parse_pdf_table drops them.
  expect_equal(sum(p$rows$kept), 1L)
  expect_true(p$rows$kept[grepl("^01/04", p$rows$date)])
  expect_false(p$rows$kept[grepl("^02/04", p$rows$date)])   # no amount
  expect_false(p$rows$kept[grepl("^30/04", p$rows$date)])   # summary line
})

test_that("inspect reason explains WHY each dropped row was skipped", {
  page <- do.call(rbind, list(
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),  # kept txn
    .mkw("02/04/2025", 50, 80), .mkw("Statement", 120, 80), .mkw("date", 160, 80),    # dated, NO amount
    .mkw("30/04/2025", 50, 100), .mkw("Closing", 120, 100), .mkw("Balance", 165, 100),
      .mkw("3,800.00", 300, 100)))                                                     # dated summary + amount
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  p <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_true("reason" %in% names(p$rows))
  expect_equal(p$rows$reason[grepl("^01/04", p$rows$date)], "")          # kept -> no reason
  expect_match(p$rows$reason[grepl("^02/04", p$rows$date)], "no amount")  # dated, no money
  expect_match(p$rows$reason[grepl("^30/04", p$rows$date)], "summary")    # closing balance
})

test_that("inspect flags a row whose date didn't parse (the fixable case)", {
  page <- do.call(rbind, list(
    .mkw("13-14-9999", 50, 60), .mkw("Odd", 120, 60), .mkw("500.00", 300, 60)))
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  p <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_false(p$rows$kept[1])
  expect_match(p$rows$reason[1], "date didn't parse")   # points at the template fix
})

test_that("inspect marks a wrapped line as a continuation, not a missed transaction", {
  page <- do.call(rbind, list(
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),  # kept txn
    .mkw("Employer", 120, 70), .mkw("Ltd", 165, 70)))                                # wrap right below it
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  p <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_equal(nrow(p$rows), 2L)
  expect_true(p$rows$kept[1])
  expect_false(p$rows$kept[2])
  expect_match(p$rows$reason[2], "continuation")
})

test_that("inspect paints a force_rows band as kept (matches the reader)", {
  page <- do.call(rbind, list(
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),  # kept txn
    .mkw("02/04/2025", 50, 80), .mkw("Statement", 120, 80), .mkw("date", 160, 80)))   # dated, NO amount
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  base <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_false(base$rows$kept[grepl("^02/04", base$rows$date)])            # skipped by default
  forced <- inspect_pdf_layout(list(words = list(page)), tmpl,
    force_rows = list(list(page = 1, y_min = 78, y_max = 82)))$pages[["1"]]
  i <- grepl("^02/04", forced$rows$date)
  expect_true(forced$rows$kept[i])                                         # now painted kept
  expect_equal(forced$rows$reason[i], "")                                  # ...and off the skipped list
})

test_that("locate_values_on_page boxes single- and multi-token values", {
  page <- .demo_input()$words[[1]]
  loc <- locate_values_on_page(page, list(closing_balance = "3,800.00",
                                          period_start = "01/04/2025",
                                          missing = "9,999.99"))
  expect_true(loc$found[loc$field == "closing_balance"])
  expect_equal(loc$x0[loc$field == "closing_balance"], 300)
  expect_false(loc$found[loc$field == "missing"])   # not on the page -> not found
})

test_that("inspect_pdf_layout is well-formed on an empty / word-less page", {
  lay <- inspect_pdf_layout(list(words = list(NULL)), .demo_tmpl())
  p <- lay$pages[["1"]]
  expect_equal(nrow(p$words), 0L)
  expect_equal(nrow(p$rows), 0L)
})
