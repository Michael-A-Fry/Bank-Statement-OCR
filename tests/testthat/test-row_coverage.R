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

test_that("a heading / note is NOT counted as an actionable missing-amount skip", {
  # The heading reason reads "no date and no amount - treated as a heading, note or
  # wrapped line". It CONTAINS "no amount", so the loose substring test swallowed it
  # into amount_missing: every heading inflated actionable_skips and the diagnosis
  # told the analyst rows had been skipped for a missing amount when nothing was lost.
  expect_equal(.rowcov_bucket("no date and no amount - treated as a heading, note or wrapped line"),
               "heading_or_note")
  # the genuinely actionable reasons are unchanged
  expect_equal(.rowcov_bucket("no amount in the money column(s) - check the amount / debit / credit bands"),
               "amount_missing")
  expect_equal(.rowcov_bucket("the date didn't parse - usually the date format in the template is wrong"),
               "date_unreadable")
  expect_equal(.rowcov_bucket("continuation - its text is folded into the transaction above"),
               "continuation")
  expect_equal(.rowcov_bucket("split row - re-joined with the line above into one transaction"),
               "continuation")
  expect_equal(.rowcov_bucket(""), "kept")
})

# .rowcov_bucket classifies by matching SUBSTRINGS of sentences written in
# R/parse_pdf_table.R and R/inspect.R -- an implicit contract between three files
# that nothing enforced. The test above pins the strings as typed here, so
# re-wording a reason (it is user-facing copy, and copy gets reworded) would leave
# it passing while the live pipeline quietly mis-bucketed. This drives the REAL
# .pdf_row_reason() to generate them, so the contract is checked, not assumed.
test_that("every reason the engine actually emits lands in the right bucket", {
  txn     <- list(description = "COFFEE SHOP", raw = "01 Jun COFFEE SHOP 4.50", amount = "4.50")
  summary <- list(description = "Closing Balance", raw = "Closing Balance 1,234.56",
                  amount = "1,234.56")
  note    <- list(description = "Transactions continued", raw = "Transactions continued",
                  amount = "")
  expect_equal(.rowcov_bucket(.pdf_row_reason(txn, "signed", TRUE)), "kept")
  expect_equal(.rowcov_bucket(.pdf_row_reason(summary, "signed", TRUE)), "summary_line")
  expect_equal(.rowcov_bucket(.pdf_row_reason(note, "signed", FALSE)), "heading_or_note")
  expect_equal(.rowcov_bucket(.pdf_row_reason(txn, "signed", FALSE)), "date_unreadable")
  expect_equal(.rowcov_bucket(.pdf_row_reason(note, "signed", TRUE)), "amount_missing")
  # ...and the two reasons inspect_pdf_layout writes itself (R/inspect.R)
  expect_equal(.rowcov_bucket("split row - re-joined with the line above into one transaction"),
               "continuation")
  expect_equal(.rowcov_bucket("continuation - its text is folded into the transaction above"),
               "continuation")
})

test_that("a page of headings and notes reports zero actionable skips", {
  # End to end: a title line and a note above the table are not transactions and
  # were never lost, so the report must not send Beth chasing a band that is fine.
  w <- data.frame(stringsAsFactors = FALSE,
    text  = c("Transaction","History",                 # heading: no date, no amount
              "05","Jan","COFFEE","4.50","95.50",      # a real transaction
              "Interest","is","calculated","daily"),   # a note, far below the table
    x     = c(110,180,   45,60,110,415,490,   110,160,200,270),
    y     = c(10,10,     40,40,40,40,40,      200,200,200,200),
    width = c(60,50,     12,16,45,25,30,      40,20,60,30),
    height= rep(10, 11))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = "Statement period 1 Jan 2026 to 31 Jan 2026",
    words = list(w), page_width = 595.28, page_height = 841.89,
    meta = list(page_count = 1L))
  cov <- row_coverage(input, .simple_tmpl())
  expect_equal(cov$kept_total, 1L)
  expect_equal(cov$actionable_skips_total, 0L)        # was 2 (both swallowed as amount_missing)
  expect_match(cov$diagnosis, "Every candidate row was kept")
  expect_equal(unname(cov$pages[[1]]$by_reason[["heading_or_note"]]), 2L)
})
