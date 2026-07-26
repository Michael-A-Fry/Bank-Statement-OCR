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

# #59 -- logs/metadata keeps one JSON per conversion FOREVER (it is exempt from log
# rollup) and this is called from a Shiny observer, so an unbounded read grows
# without limit (~6 s at 15k records). The read is bounded to the NEWEST N, and it
# has to say so rather than quietly ranking a slice.
test_that("lexicon_suggestions reads only the newest N records, and reports the slice (#59)", {
  ld <- tempfile("logs_")
  for (i in 1:5) {
    .mk_meta(ld, sprintf("r%d", i), if (i <= 3) "OLDTOK" else "NEWTOK", character(0))
    Sys.setFileTime(file.path(ld, "metadata", sprintf("r%d.json", i)),
                    Sys.time() - (10 - i) * 3600)          # r1 oldest ... r5 newest
  }
  all <- lexicon_suggestions(ld)
  expect_identical(all$scanned, 5L); expect_identical(all$total, 5L)
  expect_true(all(c("OLDTOK", "NEWTOK") %in% all$indicator_tokens$token))

  bounded <- lexicon_suggestions(ld, max_records = 2L)
  expect_identical(bounded$scanned, 2L)                     # only the newest two read
  expect_identical(bounded$total, 5L)                       # ...out of five kept
  expect_identical(bounded$indicator_tokens$token, "NEWTOK")
  expect_false("OLDTOK" %in% bounded$indicator_tokens$token)
})

test_that("the bounded read is deterministic when file times tie (#59)", {
  ld <- tempfile("logs_tie_")
  for (i in 1:4) .mk_meta(ld, sprintf("t%d", i), sprintf("TOK%d", i), character(0))
  t0 <- Sys.time()
  for (f in list.files(file.path(ld, "metadata"), full.names = TRUE)) Sys.setFileTime(f, t0)
  a <- lexicon_suggestions(ld, max_records = 2L)$indicator_tokens$token
  b <- lexicon_suggestions(ld, max_records = 2L)$indicator_tokens$token
  expect_identical(a, b)                                    # same answer every time
  expect_equal(length(a), 2L)
})
