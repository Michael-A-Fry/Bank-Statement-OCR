# Tests for the PII-safe row-coverage diagnostic (R/row_coverage.R): it explains
# why rows go missing using only shapes and counts, never the statement content.

.simple_tmpl <- function(row_tol = 3) list(id = "s", bank = "S", statement_type = "e",
  format = "pdf", version = 1, currency = "NZD",
  table = list(row_tol = row_tol, date_format = "%d %b", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 74), description = list(x_min = 74, x_max = 360),
      amount = list(x_min = 360, x_max = 470), balance = list(x_min = 470, x_max = 545))))

.rc_input <- function(mult = 1, pw = 595.28, ph = 841.89, page_ocr = FALSE) {
  w <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",  "06","Jan","PAY","10.00","105.50",
              "Closing","Balance","0.00"),
    x     = c(45,60,110,415,490,  45,60,110,415,490,  110,150,415) * mult,
    y     = c(40,40,40,40,40,      70,70,70,70,70,     100,100,100) * mult,
    width = c(12,16,45,25,30,      12,16,30,30,30,     50,50,30)    * mult,
    height= rep(10 * mult, 13))
  list(kind = "pdf", path = tempfile(fileext = ".pdf"),
       pages = "Statement period 1 Jan 2026 to 31 Jan 2026",
       words = list(w), page_width = pw, page_height = ph, page_ocr = page_ocr,
       meta = list(page_count = 1L))
}

test_that("row_coverage reports kept rows and page size, no PII", {
  cov <- row_coverage(.rc_input(), .simple_tmpl())
  expect_true(cov$applicable)
  expect_equal(cov$page_count, 1L)
  expect_equal(cov$kept_total, 2L)                 # two real transactions kept
  expect_equal(cov$pages[[1]]$width, 595L)
  expect_false(cov$pages[[1]]$scaled)              # A4 == reference -> not rescaled
  # the formatted report is markdown and carries no transaction text
  md <- format_row_coverage(cov)
  expect_match(md, "safe to share")
  expect_false(grepl("COFFEE|PAY|Closing", md))    # never leaks descriptions
})

test_that("row_coverage flags a rescaled page (the missing-rows signal)", {
  cov <- row_coverage(.rc_input(mult = 2, pw = 1190.56, ph = 1683.78), .simple_tmpl())
  expect_true(cov$any_page_rescaled)
  expect_true(cov$pages[[1]]$scaled)
  expect_equal(cov$kept_total, 2L)                 # still recovered after normalisation
})

test_that("row_coverage is not applicable to a delimited template", {
  t <- list(format = "delimited", columns = list(date = list(source = "Date")))
  expect_false(row_coverage(list(kind = "pdf"), t)$applicable)
})
