# Tests for the Admin panel's auto-draft helpers (suggest_pdf_columns).

test_that("suggest_pdf_columns finds the columns of the tutorial sample", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  fx <- fixture("samples/raw/tutorial/sample_everyday_statement.pdf")
  skip_if_not(file.exists(fx))
  s <- suggest_pdf_columns(read_input(fx))
  expect_true(is.data.frame(s) && nrow(s) >= 4)
  expect_true(all(c("date", "description", "balance") %in% s$field))
  expect_true(any(s$field == "amount"))
  # ordered left-to-right, date leftmost, balance rightmost
  expect_equal(s$field[1], "date")
  expect_equal(s$field[nrow(s)], "balance")
  # bands are sane (min < max, non-overlapping in order)
  expect_true(all(s$x_min < s$x_max))
  expect_true(all(diff(s$x_min) > 0))
})

test_that("header_phrases prefers a distinctive multi-word phrase (P2-7)", {
  input <- list(pages = paste(
    "Kōwhai Bank — Statement of transactions",
    "Account 12-3456-7890123-00",
    "Date Withdrawals Deposits Balance",
    "01 May COFFEE 4.50 995.50", sep = "\n"))
  ph <- header_phrases(input)
  expect_true(length(ph) >= 1)
  expect_true(any(.fp_specific(ph)))                 # at least one distinctive phrase
  expect_false(identical(ph, "Balance"))             # never the bare generic word
})

test_that("validate_template rejects a generic single-word PDF fingerprint (P2-7)", {
  base <- list(id = "x", bank = "B", statement_type = "s", format = "pdf",
    version = 1, min_score = 1, currency = "NZD",
    table = list(row_tol = 3, date_format = "%d/%m/%Y", amount_sign = "signed",
      columns = list(date = list(x_min = 0, x_max = 90),
                     description = list(x_min = 90, x_max = 300),
                     amount = list(x_min = 400, x_max = 470))))
  generic <- c(base, list(fingerprint = list(page_contains_all = list("Balance"))))
  expect_true(any(grepl("too generic", validate_template(generic))))
  # a distinctive phrase passes.
  ok <- c(base, list(fingerprint = list(page_contains_all = list("Statement of transactions"))))
  expect_length(validate_template(ok), 0)
})

test_that("suggest_pdf_columns is safe on input with no words", {
  expect_equal(nrow(suggest_pdf_columns(list(words = list()))), 0L)
  expect_equal(nrow(suggest_pdf_columns(list())), 0L)
})
