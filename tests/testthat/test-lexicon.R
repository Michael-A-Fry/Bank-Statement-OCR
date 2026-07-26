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

# The drafter's brand vocabulary is documented as dictionary-driven ("teach it a
# new bank in YAML, never in code"). It only works if the category is registered.
test_that("fingerprint_brand_words is a real, admin-extendable category", {
  expect_true("fingerprint_brand_words" %in% names(lexicon_categories()))
  d <- lex("fingerprint_brand_words")
  expect_true(is.character(d) && length(d) > 0)
  expect_true("bank" %in% tolower(d))
})

# The PDF summary-line vocabulary (R/parse_pdf_table.R) is documented as
# dictionary-driven: "a bank that writes 'Balance b/fwd' turns a summary line into a
# fabricated transaction, and the cure is a YAML edit, never code". It was reading
# the raw file directly while the category was NOT registered, so the promise held
# only for someone hand-editing the file: the Admin editor never listed it, the
# defaults button never showed it, and validate_lexicon REJECTED it -- so saving it
# from Admin failed with "unknown category". Registering it is what makes the
# documented route actually work.
test_that("summary_line_labels is a real, admin-extendable category", {
  expect_true("summary_line_labels" %in% names(lexicon_categories()))
  expect_length(validate_lexicon(list(summary_line_labels = c("sub total"))), 0)
  d <- lex("summary_line_labels", path = NULL)
  expect_true(is.character(d) && length(d) > 0)
  expect_true(all(.PDF_SUMMARY_LABELS %in% d))            # built-ins are the default
  expect_true(grepl("summary_line_labels", lexicon_defaults_yaml(), fixed = TRUE))
  # a wording added in YAML is UNIONED with the built-ins and reaches the matcher
  .with_lexicon("summary_line_labels: ['sub total']", {
    expect_true("sub total" %in% lex("summary_line_labels"))
    expect_true(.pdf_is_summary("Sub Total"))            # the added wording
    expect_true(.pdf_is_summary("Closing Balance"))      # ...and the built-ins survive
    expect_false(.pdf_is_summary("Total Payments to ACME Ltd"))  # whole-label only
  })
})

# The two halves of a category declaration (merge semantics + built-in default)
# live in separate functions, and a category is only wired up when it is in BOTH.
# Register it in one and it silently misbehaves: in .lexicon_spec only, lex()
# returns NULL; in .lexicon_defaults only, validate_lexicon rejects it.
test_that("every lexicon category declares BOTH its merge type and its default", {
  expect_setequal(names(.lexicon_spec()), names(.lexicon_defaults()))
  for (cat in names(.lexicon_spec())) {
    d <- lex(cat, path = NULL)
    expect_true(!is.null(d) && length(d) > 0,
                info = sprintf("category '%s' resolves to nothing", cat))
  }
})

# An admin typo in the vocabulary file must produce a readable problem, not an
# R error -- validate_lexicon exists precisely to hand that message back.
test_that("validate_lexicon reports an unknown category instead of erroring", {
  p <- validate_lexicon(list(not_a_real_category = list("x")))
  expect_true(any(grepl("unknown category", p)))
  expect_true(any(grepl("not_a_real_category", p)))
})
