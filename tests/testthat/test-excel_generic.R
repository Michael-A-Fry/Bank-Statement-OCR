# Tests for the Excel (.xlsx) path via a synthetic workbook fixture.

XLSX_FIX <- "samples/raw/synthetic/synthetic_excel_01.xlsx"

test_that("an Excel statement is detected and parsed", {
  skip_if_not(requireNamespace("readxl", quietly = TRUE))
  skip_if_not(file.exists(fixture(XLSX_FIX)))
  tp <- load_templates(templates_dir())
  input <- read_input(fixture(XLSX_FIX))
  det <- detect_statement(input, tp)
  expect_identical(det$template_id, "excel_generic_xlsx")
  tx <- parse_statement(input, tp[["excel_generic_xlsx"]])$transactions
  expect_equal(nrow(tx), 5L)
  expect_equal(tx$amount[tx$description == "Salary"], 3200.00)
  expect_equal(tx$amount[grepl("Groceries", tx$description)], -184.55)
  expect_false(any(is.na(tx$date)))
  expect_true(all(tx$flags == ""))
})

test_that("an Excel statement converts end-to-end", {
  skip_if_not(requireNamespace("readxl", quietly = TRUE))
  skip_if_not(file.exists(fixture(XLSX_FIX)))
  out <- tempfile("xl_out_")
  res <- convert_statement(fixture(XLSX_FIX), outdir = out,
                           templates_dir = templates_dir(), logdir = tempfile("l_"))
  expect_identical(res$template_id, "excel_generic_xlsx")
  expect_true(res$status %in% c("ok", "needs_review"))
  expect_true(file.exists(res$outputs[["xlsx"]]))
})
