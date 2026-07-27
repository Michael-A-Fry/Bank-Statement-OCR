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

# ---- THE EMPTY-BAND VERDICT, WITHDRAWN ---------------------------------------
# row_coverage used to lead its diagnosis with: "The <x> band read no words
# anywhere, yet N word(s) inside the table are in no band at all -- so the printing
# is there and the band is not over it. Redraw that band". It was gated on
# unbanded_words > 0, i.e. on ONE stray word, and it was the FIRST branch of the
# chain so it suppressed the page-scale and low-yield messages under it.
#
# Measured on samples/_private_staging/anz_single.pdf with the shipped
# anz_everyday_pdf: all five bands correct, 311 of 311 rows kept, and exactly one
# word of 4251 unclaimed (a page-corner mark at x=29, y=1). Map one more column
# over a strip this statement does not print in and that single word made the
# report order a maintainer to redraw a correct band -- and hid the true message,
# "14 row(s) were skipped for an unreadable date or a missing amount".
#
# No threshold rescues it, in either units: a REAL misplaced band on that file
# leaves 5.13% of the region's words unclaimed while the CORRECT shipped
# westpac_everyday_pdf leaves 9.44% on westpac_B.pdf; by count, that real fault on
# the 2-page ANZ leaves 54 unclaimed against the correct Westpac's 124. Nor does
# the shape: 79% of Westpac's unclaimed words sit in one 20pt strip against 62%
# for the genuine fault, because a template maps only the columns it needs and
# leaves the rest of the printing unmapped by design. The counts stay as counts.
.rc_band_input <- function() {
  w <- data.frame(stringsAsFactors = FALSE,
    text  = c("P1",                                     # page-corner mark: in no band
              "05", "Jan", "COFFEE", "4.50", "95.50",   # a real transaction
              "13-14-9999", "ODD", "500.00", "595.50"), # date won't parse -> actionable
    x     = c(20,    45, 60, 110, 415, 490,    45, 110, 415, 490),
    y     = c(5,     40, 40, 40, 40, 40,       70, 70, 70, 70),
    width = c(14,    12, 16, 45, 25, 30,       50, 30, 30, 30),
    height = rep(10, 10))
  list(kind = "pdf", path = tempfile(fileext = ".pdf"),
       pages = "Statement period 1 Jan 2026 to 31 Jan 2026",
       words = list(w), page_width = 595.28, page_height = 841.89,
       meta = list(page_count = 1L))
}

test_that("an empty band plus one stray word is not a verdict about that band", {
  tmpl <- .simple_tmpl()
  tmpl$table$columns$code <- list(x_min = 546, x_max = 575)   # blank on this statement
  cov <- row_coverage(.rc_band_input(), tmpl)
  expect_equal(cov$band_words_total[["code"]], 0L)            # the band did read nothing
  expect_equal(cov$unbanded_words_total, 1L)                  # and one word is unclaimed
  # ...and neither fact, nor the pair, is turned into an instruction
  expect_false(grepl("Redraw", cov$diagnosis, fixed = TRUE))
  expect_false(grepl("read no words", cov$diagnosis, fixed = TRUE))
  expect_false(grepl("Redraw", format_row_coverage(cov), fixed = TRUE))
})

test_that("the empty band no longer suppresses the message underneath it", {
  # Same file, same rows. The band verdict was the first branch of the chain, so
  # adding one unmapped-but-blank column replaced the true row-loss message.
  input <- .rc_band_input()
  plain <- row_coverage(input, .simple_tmpl())
  expect_equal(plain$kept_total, 1L)
  expect_equal(plain$actionable_skips_total, 1L)
  expect_match(plain$diagnosis, "skipped for an unreadable date or a missing amount")

  tmpl <- .simple_tmpl()
  tmpl$table$columns$code <- list(x_min = 546, x_max = 575)
  withband <- row_coverage(input, tmpl)
  expect_equal(withband$kept_total, plain$kept_total)          # nothing about the rows changed
  expect_identical(withband$diagnosis, plain$diagnosis)        # ...so neither does the diagnosis
})

test_that("the band counts survive as EVIDENCE, in the shareable report", {
  tmpl <- .simple_tmpl()
  tmpl$table$columns$code <- list(x_min = 546, x_max = 575)
  cov <- row_coverage(.rc_band_input(), tmpl)
  expect_identical(cov$empty_bands, "code")                    # named, as a measurement
  md <- format_row_coverage(cov)
  expect_match(md, "Words read by each band")
  expect_match(md, "code 0", fixed = TRUE)                     # the per-band count is there
  expect_match(md, "no band read: \\*\\*1\\*\\*")              # ...and the unclaimed count
  expect_match(md, "counts, not faults", fixed = TRUE)         # ...said not to be a fault
  expect_false(grepl("COFFEE|ODD", md))                        # still no statement contents
})

test_that("empty_bands means what it says, with no hidden gate", {
  # It used to be blanked whenever nothing was unclaimed, so a band that genuinely
  # read nothing was reported on one statement and not on the next depending on
  # whether a page-corner mark happened to fall outside every band.
  w <- data.frame(stringsAsFactors = FALSE,
    text  = c("05", "Jan", "COFFEE", "4.50", "95.50"),
    x     = c(45, 60, 110, 415, 490), y = rep(40, 5),
    width = c(12, 16, 45, 25, 30), height = rep(10, 5))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = "Statement period 1 Jan 2026 to 31 Jan 2026", words = list(w),
    page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))
  tmpl <- .simple_tmpl()
  tmpl$table$columns$credit <- list(x_min = 546, x_max = 575)   # no entries this period
  cov <- row_coverage(input, tmpl)
  expect_equal(cov$unbanded_words_total, 0L)                    # nothing unclaimed at all
  expect_identical(cov$empty_bands, "credit")                   # still reported: it read 0
  expect_equal(cov$band_words_total[["credit"]], 0L)            # same fact, same number
  # and it is still not a fault: every candidate row was kept
  expect_match(cov$diagnosis, "Every candidate row was kept")
})

test_that("unbanded_words_total is the sum of the per-page counts", {
  cov <- row_coverage(.rc_band_input(), .simple_tmpl())
  expect_equal(cov$unbanded_words_total,
               sum(vapply(cov$pages, function(p) p$unbanded_words, integer(1))))
})

# ---- THE BAND FRAME: one scale, used for both the verdict and the number -----
test_that("the printed page ratio is the reciprocal of the shared frame scale", {
  # row_coverage used to call pdf_band_frame_scale for `scaled` and then
  # hand-compute page/frame for the ratio it PRINTED -- two sums, so the flag and
  # the number could describe different things.
  cov <- row_coverage(.rc_input(mult = 2, pw = 1190.56, ph = 1683.78), .simple_tmpl())
  frame <- pdf_band_frame(.simple_tmpl())
  s <- pdf_band_frame_scale(frame, 1190.56, 1683.78)
  expect_true(cov$pages[[1]]$scaled)
  expect_equal(cov$pages[[1]]$scale_x, round(1 / s[1], 3))
  expect_equal(cov$pages[[1]]$scale_y, round(1 / s[2], 3))
  # a page the parser treats as the same size prints exactly 1, never 1.01
  near <- row_coverage(.rc_input(pw = 595.28 * 1.01, ph = 841.89 * 1.01), .simple_tmpl())
  expect_false(near$pages[[1]]$scaled)
  expect_equal(near$pages[[1]]$scale_x, 1)
  expect_equal(near$pages[[1]]$scale_y, 1)
})

test_that("row_coverage.R keeps no private copy of the band frame", {
  code <- sub("#.*$", "", readLines(file.path(engine_root(), "R", "row_coverage.R"), warn = FALSE))
  joined <- paste(code, collapse = "\n")
  # reporting frame$width is fine -- it IS the shared frame. Dividing a page size
  # by it to re-derive the scale beside pdf_band_frame_scale() is the copy.
  for (dead in c("/ frame$width", "/ frame$height", ".PAGE_SCALE_SNAP", ".A4_W", ".A4_H"))
    expect_false(grepl(dead, joined, fixed = TRUE), info = dead)
  expect_true(grepl("pdf_band_frame(template)", joined, fixed = TRUE))
  expect_true(grepl("pdf_band_frame_scale(frame,", joined, fixed = TRUE))
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
