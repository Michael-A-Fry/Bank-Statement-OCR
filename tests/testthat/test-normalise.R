# parse_amount unit coverage for ALL four sign styles, incl. edge cases
# (build-contract section 5). Guards the R `$` partial-match landmine on type_dc.

test_that("signed style: sign, direction, verbatim raw", {
  r <- parse_amount(c("-23.40", "2,000.00", "", "abc"), "signed")
  expect_equal(r$value, c(-23.40, 2000, NA, NA))
  expect_identical(r$direction, c("debit", "credit", NA, NA))
  expect_identical(r$raw, c("-23.40", "2,000.00", "", "abc"))
})

test_that("dr_cr_suffix: DR negative, CR positive, invalid suffix -> NA", {
  r <- parse_amount(c("123.45 DR", "10.00 CR", "5.00 ZZ", "7.00"),
                    "dr_cr_suffix")
  expect_equal(r$value, c(-123.45, 10.00, NA, NA))
  expect_identical(r$direction, c("debit", "credit", NA, NA))
})

test_that("dr_cr_suffix: a doubly-marked debit is not sign-flipped back (P2-11)", {
  # both markers say debit -> the answer is negative, never +500 from applying
  # the sign twice (accounting-parens/minus AND the DR suffix).
  expect_equal(parse_amount("(500.00) DR", "dr_cr_suffix")$value, -500)
  expect_equal(parse_amount("-500.00 DR",  "dr_cr_suffix")$value, -500)
  # a credit stays positive even if the magnitude was written in parentheses.
  expect_equal(parse_amount("(500.00) CR", "dr_cr_suffix")$value, 500)
})

test_that("debit_credit_cols: credit +, debit -, both blank -> NA (not 0)", {
  r <- parse_amount(NULL, "debit_credit_cols",
                    list(debit = c("", "5.00", ""), credit = c("10.00", "", "")))
  expect_equal(r$value, c(10.00, -5.00, NA))
  expect_identical(r$direction, c("credit", "debit", NA))
  # raw reflects whichever column carried the value
  expect_identical(r$raw[1], "10.00")
  expect_identical(r$raw[2], "5.00")
})

test_that("type_dc: type column controls sign; magnitude always absolute", {
  r <- parse_amount(c("4.50", "2000.00"), "type_dc",
                    list(type = c("D", "C"), type_debit_value = "D"))
  expect_equal(r$value, c(-4.50, 2000.00))
  expect_identical(r$direction, c("debit", "credit"))
})

test_that("type_dc: absent type key defaults to credit (no `$` partial match)", {
  # Regression: `opts$type` used to partial-match `type_debit_value`, flipping
  # every row to debit. Exact indexing must default all rows to credit.
  r <- parse_amount(c("4.50", "2000.00"), "type_dc",
                    list(type_debit_value = "D"))
  expect_equal(r$value, c(4.50, 2000.00))
  expect_identical(r$direction, c("credit", "credit"))
})

test_that("type_dc: indicator match is case- and whitespace-insensitive (P0-2)", {
  # The bug: a case-sensitive "D" match silently flips the sign when the bank
  # writes the indicator any other way ("d", " D ", "Debit").
  expect_equal(parse_amount(c("4.50", "2000.00"), "type_dc",
    list(type = c("d", "C"), type_debit_value = "D"))$value, c(-4.50, 2000.00))
  expect_equal(parse_amount(c("10", "20", "30"), "type_dc",
    list(type = c("Debit", "Credit", " DEBIT "), type_debit_value = "debit"))$value,
    c(-10, 20, -30))
})

test_that("type_dc: a declared credit token makes an unknown indicator fail closed (P0-2)", {
  # With BOTH tokens declared, a value matching neither is genuinely ambiguous
  # and must be NA (flagged), never silently signed. Without a credit token the
  # long-standing binary rule (non-debit -> credit) is preserved for back-compat.
  expect_identical(parse_amount(c("10", "20", "30"), "type_dc",
    list(type = c("D", "C", "X"), type_debit_value = "D", type_credit_value = "C"))$value,
    c(-10, 20, NA))
  expect_equal(parse_amount(c("10", "20"), "type_dc",
    list(type = c("D", "C"), type_debit_value = "D"))$value, c(-10, 20))
})

test_that("unsigned (credit card): bare = charge (debit), CR = payment (credit)", {
  r <- parse_amount(c("45.00", "12.50", "500.00 CR", "9.99"), "unsigned")
  expect_equal(r$value, c(-45, -12.50, 500, -9.99))            # charges negative, payment positive
  expect_identical(r$direction, c("debit", "debit", "credit", "debit"))
  expect_identical(r$raw, c("45.00", "12.50", "500.00 CR", "9.99"))  # verbatim
  # unsigned_default = credit flips the convention so it ties to an owed balance
  expect_equal(parse_amount(c("45.00", "500.00 CR"), "unsigned",
                            list(unsigned_default = "credit"))$value, c(45, -500))
  # blank -> NA (not zero)
  expect_true(is.na(parse_amount("", "unsigned")$value))
})

test_that("clean_description is verbatim: only outer whitespace trimmed", {
  expect_identical(
    clean_description(c("  O'Connor & Sons  ", "A  B", "café")),
    c("O'Connor & Sons", "A  B", "café"))
})

test_that("parse_date folds ordinals, weekday prefix and 'of'; raw kept verbatim", {
  # "12th October"-style with an explicit year
  expect_equal(parse_date("12th October 2025", "%d %b %Y")$iso, "2025-10-12")
  expect_equal(parse_date("1st Nov 2025", "%d %b %Y")$iso,      "2025-11-01")
  # the connective "of"
  expect_equal(parse_date("12th of October 2025", "%d %b %Y")$iso, "2025-10-12")
  # a leading weekday word is dropped before parsing
  expect_equal(parse_date("Tuesday 12 October 2025", "%d %b %Y")$iso, "2025-10-12")
  expect_equal(parse_date("Wed 3 Sep 2025", "%d %b %Y")$iso,         "2025-09-03")
  # month-first form
  expect_equal(parse_date("October 12 2025", "%B %d %Y")$iso, "2025-10-12")
  # raw is ALWAYS kept verbatim, never the normalised copy
  expect_identical(parse_date("Tuesday 12th of October 2025", "%d %b %Y")$raw,
                   "Tuesday 12th of October 2025")
  # a genuinely unparseable value stays NA (never silently wrong)
  expect_true(is.na(parse_date("not a date", "%d %b %Y")$iso))
})

test_that("parse_date NEVER silently invents a year (P0-1)", {
  # THE bug: a 4-digit-year value read under a 2-digit "%y" format. Base as.Date
  # takes "20" as the year and silently drops the trailing "25" -> 2020, a wrong
  # figure that looks right. Must fail closed to NA, not guess.
  expect_true(is.na(parse_date("13/08/2025", "%d/%m/%y")$iso))
  # the mirror image: a 2-digit value under a 4-digit "%Y" yields year 0025 --
  # round-trips clean, so the [1990,2100] year bound is what rejects it.
  expect_true(is.na(parse_date("13/08/25", "%d/%m/%Y")$iso))
  # trailing junk the format never consumed
  expect_true(is.na(parse_date("13/08/2025 extra", "%d/%m/%Y")$iso))
  # an impossible calendar day stays NA (Feb 31), never rolls over
  expect_true(is.na(parse_date("31/02/2025", "%d/%m/%Y")$iso))
  # ...while every LEGITIMATE date under a matching format still parses, incl.
  # non-zero-padded fields and full-vs-abbreviated month names (round-trip must
  # not false-reject on cosmetic width/case differences).
  expect_equal(parse_date("13/08/2025", "%d/%m/%Y")$iso, "2025-08-13")
  expect_equal(parse_date("13/08/25",   "%d/%m/%y")$iso, "2025-08-13")
  expect_equal(parse_date("1/8/2025",   "%d/%m/%Y")$iso, "2025-08-01")
  expect_equal(parse_date("12th October 2025", "%d %b %Y")$iso, "2025-10-12")
  # vectorised: unparseable rows go NA, valid neighbours unaffected (no row is
  # dropped -- the caller keeps the row and flags the NA date).
  expect_identical(
    parse_date(c("13/08/2025", "01/09/2025", "bad", ""), "%d/%m/%Y")$iso,
    c("2025-08-13", "2025-09-01", NA, NA))
})

test_that("detect_date_format agrees with the strict reader (no format it'd reject)", {
  # A column of 4-digit-year dates must resolve to the 4-digit format, never the
  # 2-digit "%y" the reader would now reject -- detector and reader share one
  # strict validation, so they can never disagree.
  expect_equal(detect_date_format(c("13/08/2025", "01/09/2025")), "%d/%m/%Y")
  expect_equal(detect_date_format(c("13/08/25", "01/09/25")),     "%d/%m/%y")
  expect_equal(detect_date_format(c("12th October 2025")),        "%d %b %Y")
})

test_that(".normalise_date_str is the shared fold used by reader and detector", {
  expect_equal(.normalise_date_str("12th October"),       "12 October")
  expect_equal(.normalise_date_str("12th of October"),    "12 October")
  expect_equal(.normalise_date_str("Tuesday 12 October"), "12 October")
  expect_equal(.normalise_date_str("2 Sept"),             "2 Sep")
  # "September" (%B) must be left intact -- only the 4-letter "Sept" folds
  expect_equal(.normalise_date_str("2 September"),        "2 September")
})
