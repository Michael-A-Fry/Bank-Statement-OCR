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

test_that("suggest_pdf_columns is safe on input with no words", {
  expect_equal(nrow(suggest_pdf_columns(list(words = list()))), 0L)
  expect_equal(nrow(suggest_pdf_columns(list())), 0L)
})
