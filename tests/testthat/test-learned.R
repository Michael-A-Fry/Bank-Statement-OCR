# Tests for R/learned.R -- the template chosen for a layout before, suggested next
# time. The failures these exist to prevent:
#   * one bank's correction being suggested for ANOTHER bank that prints the same
#     column headings (the collision behind "33% auto-pick");
#   * anything about a customer ending up in the file;
#   * a remembered template that no longer exists being suggested.

.ln_tset <- function() load_template_set(templates_dir(), "does_not_exist")
.ln_csv <- function(name, lines) { d <- tempfile(); dir.create(d); p <- file.path(d, name); writeLines(lines, p); p }
.LN_ODD <- c("colA;colB;colC", "Kauri Trust;2;3", "Rimu Holdings;5;6")

test_that("one layout gets one key, whoever the customer is", {
  ts <- .ln_tset()
  a <- read_input(.ln_csv("a.csv", .LN_ODD))
  b <- read_input(.ln_csv("b.csv", c("colA;colB;colC", "Someone Else;9;9")))
  expect_identical(learned_key(a, "delimited", ts)$key, learned_key(b, "delimited", ts)$key)
  # a different layout is a different key
  c <- read_input(.ln_csv("c.csv", c("colX;colY", "1;2")))
  expect_false(identical(learned_key(a, "delimited", ts)$key, learned_key(c, "delimited", ts)$key))
  # ...and so is the same layout as another FORMAT
  expect_false(identical(learned_key(a, "delimited", ts)$key, learned_key(a, "excel", ts)$key))
})

test_that("two banks with the same column headings do not share a key", {
  # The collision behind "33% auto-pick": keyed on the layout alone, correcting one
  # bank's statement would start mis-suggesting for the other.
  mk <- function(bank) list(pages = paste(c(bank, "Statement of Accounts",
    "Date Details Withdrawals Deposits Balance", "03 Feb EFTPOS 12.40 2,398.15",
    "Page 1 of 1"), collapse = "\n"), kind = "pdf")
  ts <- list(anz = list(id = "anz", bank = "ANZ", format = "pdf"),
             kow = list(id = "kow", bank = "Kowhai Bank", format = "pdf"))
  k1 <- learned_key(mk("ANZ Bank New Zealand"), "pdf", ts)
  k2 <- learned_key(mk("Kowhai Bank of Aotearoa"), "pdf", ts)
  expect_identical(k1$banks, "ANZ"); expect_identical(k2$banks, "Kowhai Bank")
  expect_false(identical(k1$key, k2$key))
})

test_that("a choice is remembered, reinforced, replaced and forgotten", {
  f <- tempfile(fileext = ".json")
  expect_identical(nrow(learned_load(f)), 0L)                    # nothing yet, no error
  learned_record(f, "k1", "anz_everyday_csv", format = "delimited", hint = "cola | colb", by = "AB1234")
  learned_record(f, "k1", "anz_everyday_csv", by = "CD5678")
  d <- learned_load(f)
  expect_identical(nrow(d), 1L); expect_identical(d$times, 2L); expect_identical(d$by, "CD5678")
  # a DIFFERENT template for the same layout replaces it and starts the count again
  learned_record(f, "k1", "bnz_everyday_csv", by = "AB1234")
  d <- learned_load(f)
  expect_identical(d$template, "bnz_everyday_csv"); expect_identical(d$times, 1L)
  learned_record(f, "k2", "asb_everyday_csv")
  learned_forget(f, "k1")
  expect_identical(learned_load(f)$key, "k2")
  # never half a file: written aside and renamed
  expect_false(file.exists(paste0(f, ".tmp")))
  # an empty choice is not a choice
  learned_record(f, "k3", NA_character_); learned_record(f, NA_character_, "x")
  expect_identical(learned_load(f)$key, "k2")
})

test_that("nothing about the customer reaches the file", {
  ts <- .ln_tset()
  inp <- read_input(.ln_csv("odd.csv", .LN_ODD))
  k <- learned_key(inp, "delimited", ts)
  f <- tempfile(fileext = ".json")
  learned_record(f, k$key, "anz_everyday_csv", format = "delimited", hint = k$hint,
                 banks = paste(k$banks, collapse = " | "), by = "AB1234")
  txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
  for (w in c("Kauri", "Rimu", "Trust", "Holdings")) expect_false(grepl(w, txt, fixed = TRUE), info = w)
})

test_that("a remembered template that is gone, or cannot read this kind of file, is not suggested", {
  ts <- .ln_tset()
  d <- data.frame(key = "k", template = "anz_everyday_csv", stringsAsFactors = FALSE)
  expect_identical(learned_lookup(d, "k", ts, "delimited"), "anz_everyday_csv")
  expect_true(is.na(learned_lookup(d, "k", ts, "pdf")))            # a CSV template, a PDF file
  d$template <- "deleted_since"
  expect_true(is.na(learned_lookup(d, "k", ts, "delimited")))
  expect_true(is.na(learned_lookup(d, "other", ts, "delimited")))
  expect_true(is.na(learned_lookup(NULL, "k", ts, "delimited")))
})

test_that("the Convert table suggests what was chosen before, and says so", {
  ts <- .ln_tset()
  p <- .ln_csv("odd.csv", .LN_ODD)
  first <- identify_file(p, ts, "odd.csv")
  expect_identical(first$state, "none")
  expect_false(is.na(first$key))
  f <- tempfile(fileext = ".json")
  learned_record(f, first$key, "anz_everyday_csv", format = "delimited")
  again <- identify_file(.ln_csv("odd2.csv", c("colA;colB;colC", "Other Co;1;1")), ts, "odd2.csv",
                         learned = learned_load(f))
  expect_identical(again$state, "learned")
  expect_identical(again$guess, "anz_everyday_csv")
  expect_true(is.na(again$det_guess))                                # detection alone: nothing
  expect_match(again$detail, "Chosen for a statement laid out like this one before", fixed = TRUE)
  # a remembered choice that IS detection's answer changes nothing
  anz <- .ln_csv("anz.csv", c(
    "Type,Details,Particulars,Code,Reference,Amount,Date,ForeignCurrencyAmount,ConversionCharge",
    "Credit,Payroll Ltd,Salary,,,2000.00,19/06/2014,,"))
  k <- identify_file(anz, ts, "anz.csv")$key
  learned_record(f, k, "anz_everyday_csv", format = "delimited")
  expect_identical(identify_file(anz, ts, "anz.csv", learned = learned_load(f))$state, "sure")
})
