# Tests for the Excel (.xlsx) path via a synthetic workbook fixture.

XLSX_FIX <- "samples/raw/synthetic/synthetic_excel_01.xlsx"

test_that("the automatic reader proves an Excel statement to the golden figures", {
  skip_if_not(requireNamespace("readxl", quietly = TRUE))
  skip_if_not(file.exists(fixture(XLSX_FIX)))
  expect_auto_read_golden(XLSX_FIX, "tests/testthat/expected/excel_generic_xlsx.csv",
                          outcomes = "proven")
})

test_that("an Excel statement is parsed by the table reader", {
  skip_if_not(requireNamespace("readxl", quietly = TRUE))
  skip_if_not(file.exists(fixture(XLSX_FIX)))
  input <- read_input(fixture(XLSX_FIX))
  tx <- parse_statement(input, fixture_template("excel_generic_xlsx"))$transactions
  expect_equal(nrow(tx), 5L)
  expect_equal(tx$amount[tx$description == "Salary"], 3200.00)
  expect_equal(tx$amount[grepl("Groceries", tx$description)], -184.55)
  expect_false(any(is.na(tx$date)))
  expect_true(all(tx$flags == ""))
})

test_that("an Excel statement converts end-to-end", {
  skip_if_not(requireNamespace("readxl", quietly = TRUE))
  skip_if_not(file.exists(fixture(XLSX_FIX)))
  res <- convert_sandbox()(fixture(XLSX_FIX))
  expect_identical(res$status, "ok")
  expect_identical(res$outcome, "proven")
  expect_identical(res$feed_basis, "proven")
  expect_true(file.exists(res$outputs[["xlsx"]]))
  # the figures read back from the file it wrote are the golden's
  got <- utils::read.csv(res$outputs[["csv"]], stringsAsFactors = FALSE)
  exp <- read_core_csv(fixture("tests/testthat/expected/excel_generic_xlsx.csv"))
  expect_identical(got$date, exp$date)
  expect_equal(got$amount, exp$amount)
  expect_equal(got$balance, exp$balance)
})

# BNZ's "Excel" download is the OLD Excel format, .xls. It was refused at the door
# ("unsupported file extension"), so a team's whole BNZ intake could not be read.
test_that("an old-style .xls workbook is read like any other Excel file", {
  f <- file.path(engine_root(), "tests", "testthat", "fixtures", "bnz_export_old_excel.xls")
  inp <- read_input(f)
  expect_identical(inp$kind, "excel")
  expect_true(is.data.frame(inp$table) && nrow(inp$table) > 0L)
  # whether it hides rows cannot be told from an .xls: not claimed either way
  expect_true(is.na(inp$meta$hidden_rows))
  expect_identical(.IDENT_FORMAT[["xls"]], "excel")
  r <- auto_read(inp)
  expect_true(r$outcome %in% c("proven", "check", "layout_match", "unread"))
  ck <- r$checks
  if (is.data.frame(ck) && "workbook_plain" %in% ck$check)
    expect_true(is.na(ck$ok[ck$check == "workbook_plain"]) || isFALSE(ck$ok[ck$check == "workbook_plain"]))
})
