# Tests for the safe-to-share statement audit (R/audit.R) -- the PII guarantee is
# the point: nothing real may survive masking, and the reader's own sentences
# (which can quote a line of the statement) are never in it.

test_that("mask_text leaves NO real letter or digit, only shape", {
  expect_equal(mask_text(c("Countdown 47.20", "17 Sep 2024", "12-3456-7890123-00")),
               c("Xxxxxxxxx 99.99", "99 Xxx 9999", "99-9999-9999999-99"))
  # There used to be an exemption here that passed "[REDACTED]" through unmasked.
  # Nothing writes that token any more -- the engine no longer rewrites readable
  # text -- and an exemption in a PII mask has to earn its place, so it is gone.
  expect_identical(mask_text("[REDACTED]"), "[XXXXXXXX]")
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
  a <- statement_audit(csv)
  rep <- format_audit(a)
  expect_identical(a$reading$outcome, "check")        # read, and nothing proves it
  expect_match(rep, "reading: check", fixed = TRUE)
  expect_match(rep, "checks failed:", fixed = TRUE)    # names only, never the sentences
  expect_false(grepl("SECRETMERCHANTNAME", rep))    # no real description leaks
  expect_false(grepl("ANOTHERSECRET", rep))
  expect_false(grepl("12.50|99.99", rep))           # no real amount leaks
  expect_true(grepl("safe to share", rep))
})

test_that("an audit of a statement naming its account never carries the number", {
  acct <- nz_test_account()
  a <- format_audit(statement_audit(proven_csv(acct)))
  expect_false(grepl(strsplit(acct, "-")[[1]][3], a, fixed = TRUE))
  expect_match(a, "bank: bnz (high confidence)", fixed = TRUE)
  expect_match(a, "reading: proven", fixed = TRUE)
})

# The audit's "row shapes" table is the report a reviewer opens to work out why a
# conversion looks wrong, so it has to show the same LINES the reader saw. It used
# to carry its own copy of the row grouper -- the pairwise-gap version that merges
# a block of tightly-set lines into one -- so on a dense statement it reported a
# handful of giant rows for a page the reader read correctly.
test_that("the audit sees the same visual rows the reader does (dense lines)", {
  # Eight printed lines, 5pt apart, whose three words sit on tops 1pt apart (real
  # PDFs do this). Every word-to-word gap in y-order is then <= the 3pt row_tol, so
  # the old pairwise-gap grouping saw ONE row for the whole page; anchoring to each
  # row's start (.group_rows) separates all eight.
  ys <- as.vector(t(outer(seq(40, 75, by = 5), 0:2, "+")))
  w <- data.frame(stringsAsFactors = FALSE,
    text = rep(c("01/06/2025", "COFFEE", "4.50"), times = 8),
    x = rep(c(50, 150, 415), times = 8), y = ys,
    width = rep(c(50, 45, 25), times = 8), height = rep(4, 24))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = "period from 1 Jun 2025 to 30 Jun 2025", words = list(w),
    meta = list(page_count = 1L))
  tmpl <- list(id = "s", bank = "S", statement_type = "e", format = "pdf", version = 1,
    currency = "NZD", table = list(row_tol = 3, date_format = "%d/%m/%Y",
      amount_sign = "signed",
      columns = list(date = list(x_min = 40, x_max = 110),
                     description = list(x_min = 110, x_max = 360),
                     amount = list(x_min = 360, x_max = 470))))
  shapes <- .audit_rows(input, tmpl)
  expect_equal(nrow(shapes), 8L)                       # one shape per printed line
  expect_equal(nrow(parse_pdf_table(input, tmpl)$transactions), 8L)  # ...and per read row
  expect_true(all(shapes$date == "99/99/9999"))        # cells still masked to shape
  expect_true(all(shapes$amount == "9.99"))
})
