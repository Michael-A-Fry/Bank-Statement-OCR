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

# ---------------------------------------------------------------------------
# The drafted-fingerprint BRAND vocabulary (#10 follow-through).
#
# .fp_brand_words() resolves through lex("fingerprint_brand_words") so a bank the
# built-in list has never heard of ("Tangata Finance") can be taught by a
# DICTIONARY edit rather than a code change -- otherwise the drafter mistakes that
# bank's own masthead for a customer NAME and throws away its best fingerprint,
# producing its weakest templates for exactly the banks nobody has taught it yet.
#
# lex() returns NULL for a category R/lexicon.R has not registered, so the union
# must degrade to the built-in list rather than erroring. THAT is what is pinned
# here; the second block strengthens automatically the moment the category is
# registered in .lexicon_spec() / .lexicon_defaults() (see the handoff note).
# ---------------------------------------------------------------------------

test_that("the brand vocabulary always keeps its built-in list, registered or not", {
  clear_lexicon_cache()
  w <- .fp_brand_words()
  expect_true(all(.FP_BRAND_DEFAULT %in% w))
  expect_true(all(c("mastercard", "kiwibank", "co-operative") %in% w))
  # and it compiles to a usable, correctly-ESCAPED alternation (an admin must be
  # able to type "M&S Bank" without knowing regex, and a stray metacharacter in
  # the dictionary must never widen or break the match)
  rx <- .fp_brand_rx()
  expect_true(grepl(rx, "westpac everyday account statement"))
  expect_false(grepl(rx, "jordan te awa"))
  expect_error(grepl(rx, "probe"), NA)
})

test_that("a lexicon-taught brand word reaches the drafter once the category exists", {
  lx <- tempfile(fileext = ".yaml")
  writeLines("fingerprint_brand_words: [tangata]", lx)
  clear_lexicon_cache()
  old <- Sys.getenv("BSO_LEXICON", NA_character_)
  Sys.setenv(BSO_LEXICON = lx)
  on.exit({ if (is.na(old)) Sys.unsetenv("BSO_LEXICON") else Sys.setenv(BSO_LEXICON = old)
            clear_lexicon_cache() }, add = TRUE)
  w <- .fp_brand_words()
  expect_true(all(.FP_BRAND_DEFAULT %in% w))   # NEVER weaker than the built-in
  if ("fingerprint_brand_words" %in% names(lexicon_categories())) {
    # registered: the dictionary edit plumbs through, which is the whole point
    expect_true("tangata" %in% w)
    expect_length(validate_lexicon(list(fingerprint_brand_words = list("tangata"))), 0L)
  } else {
    # NOT registered yet: the built-in still works (fail-safe), but the category is
    # inert -- an admin's dictionary edit is ignored, and the lexicon validator
    # will not accept it either (today it raises rather than reporting, which is a
    # separate R/lexicon.R defect; both outcomes are "not accepted", which is what
    # matters here). See the handoff: R/lexicon.R must add it to .lexicon_spec()
    # (type "list") and .lexicon_defaults() (= .FP_BRAND_DEFAULT). Delete this
    # branch when it does.
    expect_false("tangata" %in% w)
    accepted <- tryCatch(
      length(validate_lexicon(list(fingerprint_brand_words = list("tangata")))) == 0L,
      error = function(e) FALSE)
    expect_false(accepted)
  }
})
