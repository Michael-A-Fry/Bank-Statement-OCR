# Tests for the safe-to-share statement audit (R/audit.R) -- the PII guarantee is
# the point: nothing real may survive masking.

test_that("mask_text leaves NO real letter or digit, keeps shape + [REDACTED]", {
  expect_equal(mask_text(c("Countdown 47.20", "17 Sep 2024", "12-3456-7890123-00")),
               c("Xxxxxxxxx 99.99", "99 Xxx 9999", "99-9999-9999999-99"))
  expect_identical(mask_text("[REDACTED]"), "[REDACTED]")
  expect_identical(mask_text(NA_character_), NA_character_)
  # accented / non-ASCII letters must also be masked (Unicode-aware)
  m <- mask_text("O'Connér & Søns 12")
  expect_false(grepl("[[:alpha:]]", gsub("[xX]", "", m)))   # only x/X survive as letters
  expect_false(grepl("[0-8]", m))                            # only 9 survives as a digit
})

test_that("the audit report contains no real transaction text", {
  csv <- tempfile(fileext = ".csv")
  writeLines(c("Date,Amount,Payee",
               "2024-01-05,-12.50,SECRETMERCHANTNAME",
               "2024-01-06,99.99,ANOTHERSECRET"), csv)
  tmpl <- list(id = "t", bank = "T", statement_type = "e", format = "delimited",
    version = 1, currency = "NZD", amount_sign = "signed", min_score = 1,
    fingerprint = list(header_contains_all = list("Payee")),
    columns = list(date = list(source = "Date", format = "%Y-%m-%d"),
      amount = list(source = "Amount"), description = list(source = "Payee")))
  a <- statement_audit(csv, templates = list(t = tmpl))
  rep <- format_audit(a)
  expect_false(grepl("SECRETMERCHANTNAME", rep))    # no real description leaks
  expect_false(grepl("ANOTHERSECRET", rep))
  expect_false(grepl("12.50|99.99", rep))           # no real amount leaks
  expect_true(grepl("safe to share", rep))
})

test_that("a PDF row whose DATE is redacted is KEPT with its amount, not dropped", {
  w <- data.frame(stringsAsFactors = FALSE,
    text = c("[REDACTED]","COFFEE","4.50", "[REDACTED]","RENT","89.00", "17","Sep","PAY","500.00"),
    x = c(50,150,415, 50,150,415, 45,60,150,415),
    y = c(40,40,40,   60,60,60,   80,80,80,80),
    width = c(30,45,25, 30,40,30, 14,16,30,30), height = rep(10,10))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("period from 1 Sep 2024 to 30 Sep 2024"), words = list(w), meta = list(page_count = 1L))
  tmpl <- list(id = "s", bank = "S", statement_type = "e", format = "pdf", version = 1,
    currency = "NZD", table = list(row_tol = 3, date_format = "%d %b", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110), description = list(x_min = 110, x_max = 360),
      amount = list(x_min = 360, x_max = 470))))
  tx <- parse_pdf_table(input, tmpl)$transactions
  expect_equal(nrow(tx), 3L)                              # nothing silently dropped
  expect_equal(tx$amount, c(4.50, 89.00, 500))           # redacted-date rows keep their amount
  expect_true(all(grepl("redacted", tx$flags[1:2])))     # and are flagged
  expect_true(all(is.na(tx$date[1:2])))                  # date unknown (was hidden)
})
