# The deterministic learning loop (R/suggestions.R): aggregate what the engine did
# not recognise across the local metadata, ranked; approve into the lexicon.

.mk_meta <- function(logdir, id, toks, cols) {
  write_log_record(logdir, "metadata", id,
    list(run_id = id, novelty = list(
      unrecognised_type_values = as.list(toks), unmapped_columns = as.list(cols))))
}

test_that("lexicon_suggestions ranks unrecognised tokens + unmapped columns by frequency", {
  ld <- tempfile("logs_")
  .mk_meta(ld, "r1", c("COW", "HORSE"), c("ConversionCharge"))
  .mk_meta(ld, "r2", c("COW"), c("ConversionCharge", "Memo"))
  .mk_meta(ld, "r3", c("COW", "HORSE"), character(0))
  s <- lexicon_suggestions(ld)
  expect_identical(s$indicator_tokens$token, c("COW", "HORSE"))     # COW first (3 > 2)
  expect_identical(s$indicator_tokens$count, c(3L, 2L))
  expect_identical(s$unmapped_columns$column[1], "ConversionCharge")
  # min_count hides one-offs.
  expect_false("Memo" %in% lexicon_suggestions(ld, min_count = 2L)$unmapped_columns$column)
})

test_that("approving a suggestion appends it to the lexicon (union, cache cleared)", {
  lx <- tempfile(fileext = ".yaml")
  expect_true(lexicon_append("debit_markers", "cow", path = lx))
  # unioned with the built-ins, readable by lex().
  got <- lex("debit_markers", path = lx)
  expect_true(all(c("D", "DR", "cow") %in% got))
  # a second approval unions without duplicating.
  lexicon_append("credit_markers", "horse", path = lx)
  y <- yaml::read_yaml(lx)
  expect_identical(unlist(y$debit_markers), "cow")
  expect_identical(unlist(y$credit_markers), "horse")
  # only LIST categories are appendable this way.
  expect_false(lexicon_append("money_regex", "x", path = lx))
})

test_that("no metadata -> empty suggestions (no crash)", {
  s <- lexicon_suggestions(tempfile("empty_"))
  expect_equal(nrow(s$indicator_tokens), 0)
  expect_equal(nrow(s$unmapped_columns), 0)
})
