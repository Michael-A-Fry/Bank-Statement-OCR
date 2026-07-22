# The externalised recognition vocabularies (R/lexicon.R): built-in defaults must
# equal what the engine shipped with (zero regression), the file EXTENDS/overrides
# per category, an invalid entry fails safe, and one edit plumbs a new vocabulary
# (a bank writing "cow"/"horse" for debit/credit) through detection + drafting +
# parsing with no code change.

.with_lexicon <- function(text, code) {
  lx <- tempfile(fileext = ".yaml"); writeLines(text, lx)
  old <- Sys.getenv("BSO_LEXICON", NA_character_)
  Sys.setenv(BSO_LEXICON = lx); clear_lexicon_cache()
  on.exit({ if (is.na(old)) Sys.unsetenv("BSO_LEXICON") else Sys.setenv(BSO_LEXICON = old)
            clear_lexicon_cache() }, add = TRUE)
  force(code)
}

test_that("with no lexicon file, lex() equals the built-in constants", {
  clear_lexicon_cache()
  expect_identical(lex("header_keywords", path = NULL), .HDR_KEYS)
  expect_identical(lex("money_regex", path = NULL), .MONEY_RX)
  expect_identical(lex("date_regex", path = NULL), .DATE_RX)
  expect_identical(lex("account_regex", path = NULL), .ACCT_RX)
  expect_true(all(c("D", "DR", "DEBIT") %in% lex("debit_markers", path = NULL)))
})

test_that("word lists UNION with the built-in; regexes REPLACE (validated)", {
  lx <- tempfile(fileext = ".yaml")
  writeLines(c("debit_markers: [cow]", "credit_markers: [horse]"), lx)
  clear_lexicon_cache()
  expect_true(all(c("D", "DR", "cow") %in% lex("debit_markers", path = lx)))  # union, keeps built-ins
  # a bad regex falls back to the built-in (never breaks parsing) and is flagged.
  writeLines("money_regex: \"[unterminated\"", lx); clear_lexicon_cache()
  expect_identical(lex("money_regex", path = lx), .MONEY_RX)
  expect_true(length(validate_lexicon(list(money_regex = "[unterminated"))) > 0)
  expect_length(validate_lexicon(list(debit_markers = c("cow", "horse"))), 0)
})

test_that("type_dc_domain is the union of debit + credit markers (cow/horse enter it)", {
  .with_lexicon(c("debit_markers: [cow]", "credit_markers: [horse]"), {
    dom <- type_dc_domain()
    expect_true(all(c("COW", "HORSE", "D", "C") %in% dom))
  })
})

test_that("one lexicon edit teaches the WHOLE engine a new debit/credit vocabulary", {
  csv <- c("Date,Type,Amount,Details",
           "01/06/2025,cow,4.50,Coffee",
           "02/06/2025,horse,2000.00,Salary",
           "03/06/2025,cow,12.00,Lunch")
  tf <- tempfile(fileext = ".csv"); writeLines(csv, tf)

  # WITHOUT the lexicon entry, the drafter can't infer the indicator -> signed.
  clear_lexicon_cache()
  d0 <- draft_template(tf, bank = "ZooBank")
  expect_identical(d0$amount_sign, "signed")

  # WITH cow/horse in the lexicon, detection + drafting + parsing all recognise it.
  .with_lexicon(c("debit_markers: [cow]", "credit_markers: [horse]"), {
    d1 <- draft_template(tf, bank = "ZooBank")
    expect_identical(d1$amount_sign, "type_dc")
    expect_identical(d1$type_debit_value, "cow")
    expect_identical(d1$type_credit_value, "horse")
    tx <- draft_preview(tf, d1)
    # cow rows are money OUT (negative), horse rows money IN (positive).
    expect_equal(tx$amount, c(-4.50, 2000.00, -12.00))
  })
})
