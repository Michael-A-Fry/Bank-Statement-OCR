# One question, one answer: "what does this wording on a statement mean?"
# (R/words.R). Admin -> Words and Please check both teach through teach_wording(),
# which picks the file from the meaning and refuses a wording that would clash.

.words_files <- function() {
  d <- tempfile(fileext = ".yaml"); l <- tempfile(fileext = ".yaml")
  file.copy(file.path(engine_root(), "dictionaries", "labels.yaml"), d)
  file.copy(file.path(engine_root(), "dictionaries", "lexicon.yaml"), l)
  clear_lexicon_cache()
  list(d = d, l = l)
}

test_that("every meaning offered is one the reader acts on, and nothing dead is offered", {
  wm <- word_meanings()
  expect_false(anyDuplicated(wm$id) > 0)
  lab <- wm[wm$file == "labels", ]
  expect_setequal(unlist(strsplit(lab$lists, ";")),
                  c("opening_balance", "closing_balance", "statement_period",
                    "statement_start", "statement_end", "statement_date"))
  # nothing reads these since templates were retired, so they are not offered
  expect_false(any(c("total_credits", "total_debits", "account_name") %in% unlist(strsplit(wm$lists, ";"))))
  lx <- unlist(strsplit(wm$lists[wm$file == "lexicon"], ";"))
  spec <- .lexicon_spec()
  expect_true(all(lx %in% names(spec)))
  expect_true(all(spec[lx] == "list"))      # only word lists are taught one word at a time
  # every choice on screen carries an example
  expect_true(all(grepl("\\(e\\.g\\. \"[^\"]+\"\\)$", names(word_meaning_choices()))))
})

test_that("the shipped label file holds only what the reader reads, and still loads", {
  d <- load_label_dict(file.path(engine_root(), "dictionaries", "labels.yaml"))
  wm <- word_meanings()
  expect_setequal(names(d), unlist(strsplit(wm$lists[wm$file == "labels"], ";")))
  expect_true("balance brought forward" %in% d$opening_balance$any_of)
})

test_that("the vocabulary file's examples name real lists and parse once uncommented", {
  x <- readLines(file.path(engine_root(), "dictionaries", "lexicon.yaml"))
  ex <- grep("^# [a-z_]+:", x, value = TRUE)
  expect_true(length(ex) >= 6L)
  for (e in ex) {
    y <- yaml::yaml.load(sub("^# ", "", e))
    expect_true(names(y) %in% names(.lexicon_spec()), info = e)
  }
  # and the shipped file teaches nothing on its own: every line is a comment
  expect_null(yaml::read_yaml(file.path(engine_root(), "dictionaries", "lexicon.yaml")))
})

test_that("a new wording is taught to the right file, and the reader then uses it", {
  f <- .words_files()
  out <- teach_wording("opening_balance", "Kickoff kitty", f$d, f$l)
  expect_true(isTRUE(out)); expect_true(attr(out, "added"))
  expect_match(attr(out, "reason"), "^From now on")
  expect_true("kickoff kitty" %in% load_label_dict(f$d)$opening_balance$any_of)
  # the file keeps its explanation
  expect_true(any(grepl("WHAT A FACT ABOUT THE STATEMENT IS CALLED", readLines(f$d), fixed = TRUE)))
  # read through the same matcher the metadata step uses
  m <- match_label(load_label_dict(f$d)$opening_balance, "Kickoff kitty    $1,250.00")
  expect_equal(m$value, "$1,250.00")
  # and it reaches the table reader's opening-balance anchor too
  old <- Sys.getenv("BSO_DICTIONARY", NA_character_)
  Sys.setenv(BSO_DICTIONARY = f$d)
  on.exit(if (is.na(old)) Sys.unsetenv("BSO_DICTIONARY") else Sys.setenv(BSO_DICTIONARY = old), add = TRUE)
  expect_equal(.ar_anchor_class("Kickoff kitty"), "open")
  # taught twice: known, nothing written
  again <- teach_wording("opening_balance", "KICKOFF KITTY", f$d, f$l)
  expect_true(isTRUE(again)); expect_false(attr(again, "added"))
})

test_that("a wording that would clash with another meaning is refused, and says why", {
  f <- .words_files()
  no <- function(id, w) { o <- teach_wording(id, w, f$d, f$l); expect_false(isTRUE(o), info = w); attr(o, "reason") }
  # part of another label: "balance" would catch the Closing balance line too
  expect_match(no("opening_balance", "balance"), "part of")
  # holds another label: this line would be read as the closing balance as well
  expect_match(no("opening_balance", "previous closing balance"), "holds \"closing balance\"")
  # already means something else
  teach_wording("opening_balance", "Kickoff kitty", f$d, f$l)
  expect_match(no("closing_balance", "kickoff kitty"), "already means opening balance")
  expect_match(no("money_in_mark", "DR"), "already means")
  # too short to be a label; symbols that would break a pattern list
  expect_match(no("statement_date", "on"), "too short")
  expect_match(no("money_out_heading", "spent (nzd)"), "brackets")
  # nothing was written by any refusal
  expect_false(any(grepl("spent", readLines(f$l), fixed = TRUE) & !grepl("^#", readLines(f$l))))
  # a mark and a heading on the same side may share a word: "Debit" is both
  expect_true(isTRUE(teach_wording("money_out_heading", "Debit", f$d, f$l)))
})

test_that("a money-out mark is written to both lists that read marks, in capitals", {
  f <- .words_files()
  expect_true(isTRUE(teach_wording("money_out_mark", "Paid", f$d, f$l)))
  expect_true("PAID" %in% lex("debit_markers", f$l))
  expect_true("PAID" %in% lex("dr_cr_suffix_debit", f$l))
  expect_true(isTRUE(teach_wording("money_in_heading", "Received", f$d, f$l)))
  expect_true("received" %in% lex("amount_style_credit_headers", f$l))
  # the explanation at the top of the file is still all there
  expect_true(any(grepl("WHAT WORDS INSIDE THE TRANSACTION TABLE MEAN", readLines(f$l), fixed = TRUE)))
  clear_lexicon_cache()
})

test_that("Please check offers the wordings in front of a figure that the tool does not read yet", {
  f <- .words_files()
  pg <- paste("ACME BANK", "Statement period: 1 Mar 2026 to 31 Mar 2026",
              "Kickoff kitty     $1,250.00", "Opening balance   $1,250.00",
              "02 Mar  EFTPOS Countdown   45.20   1,204.80",
              "Page total      45.20", "Money at the end    $1,204.80",
              "Statement dated   from 3 April 2026", sep = "\n")
  got <- statement_wordings(pg, load_label_dict(f$d), f$l)
  expect_true(all(c("Kickoff kitty", "Money at the end") %in% got))
  # known labels, totals lines and transactions are not offered
  expect_false(any(c("Opening balance", "Page total", "Statement period") %in% got))
  expect_false(any(grepl("Countdown", got)))
  expect_false("from" %in% tolower(got))          # a joining word labels nothing
  # once taught, it is no longer offered
  teach_wording("opening_balance", "Kickoff kitty", f$d, f$l)
  expect_false("Kickoff kitty" %in% statement_wordings(pg, load_label_dict(f$d), f$l))
  clear_lexicon_cache()
})

test_that("both screens open on nothing picked, and a word with no meaning is refused", {
  ch <- word_meaning_choices(blank = TRUE)
  expect_identical(unname(ch[1]), "")
  f <- .words_files()
  out <- teach_wording("", "Kickoff kitty", f$d, f$l)
  expect_false(isTRUE(out)); expect_match(attr(out, "reason"), "^Pick what the wording means")
})
