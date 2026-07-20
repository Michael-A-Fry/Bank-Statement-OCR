# Tests for the cross-bank Xero-standard import template (one template, many banks).

test_that("Xero-standard import is detected for every bank's export", {
  tp <- load_templates(templates_dir())
  for (b in c("anz", "asb", "bnz", "kiwibank", "westpac")) {
    f <- fixture(sprintf("samples/raw/%s/%s_xero_import_sample_01.csv", b, b))
    skip_if_not(file.exists(f))
    det <- detect_statement(read_input(f), tp)
    expect_identical(det$template_id, "xero_standard_csv")
  }
})

test_that("debit/credit column drives the sign; balance continuity holds", {
  tp <- load_templates(templates_dir())
  f <- fixture("samples/raw/anz/anz_xero_import_sample_01.csv")
  skip_if_not(file.exists(f))
  parsed <- parse_statement(read_input(f), tp[["xero_standard_csv"]])
  recon <- reconcile(parsed, tp[["xero_standard_csv"]])
  tx <- parsed$transactions
  expect_equal(nrow(tx), 8L)
  expect_equal(tx$amount[tx$description == "Payroll deposit"], 4850.00)   # credit -> +
  expect_equal(tx$amount[tx$description == "Office supplies"], -312.54)   # debit  -> -
  expect_equal(recon$kpis$status[recon$kpis$name == "running_balance_continuity"], "pass")
  expect_true(all(c("memo", "source_currency") %in% names(parsed$extras)))
  expect_true(all(tx$flags == ""))
})
