# Tests for the Admin panel's auto-draft helpers (suggest_pdf_columns).

test_that("suggest_pdf_columns finds the columns of the tutorial sample", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  fx <- fixture("samples/raw/tutorial/sample_everyday_statement.pdf")
  skip_if_not(file.exists(fx))
  s <- suggest_pdf_columns(read_input(fx))
  expect_true(is.data.frame(s) && nrow(s) >= 4)
  expect_true(all(c("date", "description", "balance") %in% s$field))
  # The statement's own header row says "Withdrawals | Deposits | Balance", so the
  # two money columns are a debit and a credit -- which is what its shipped
  # template (tutorial_everyday_pdf, debit_credit_cols) declares. This used to
  # assert a second column called "amount": the sniffer labelled every non-balance
  # money column "amount" regardless of what the page said, and a template is a
  # LIST, so two of them collapsed to one and the draft read nothing.
  expect_true(all(c("debit", "credit") %in% s$field))
  expect_false(any(s$field == "amount"))
  # ordered left-to-right, date leftmost, balance rightmost
  expect_equal(s$field[1], "date")
  expect_equal(s$field[nrow(s)], "balance")
  # bands are sane (min < max, non-overlapping in order)
  expect_true(all(s$x_min < s$x_max))
  expect_true(all(diff(s$x_min) > 0))
})

# ---------------------------------------------------------------------------
# W1. THE DRAFT HAS TO READ ROWS.
#
# The whole promise ("add a bank in two minutes, no code") rests on the drafted
# template parsing the file it was drafted from. It did not: on real ANZ and ASB
# statements the draft previewed ZERO rows, and redrawing the bands by hand did
# not recover it, because the date band stopped at the day number and the money
# bands had been measured over blocks that are not the transaction table.
#
# These fixtures are the committed, invented equivalents of those statements
# (tests/testthat/fixtures/make_pdf_fixtures.R), each with a PROVEN template in
# templates/. The bar is not "some rows" -- it is that the tool's own draft reads
# the SAME transactions as the template a human wrote for that layout.
# ---------------------------------------------------------------------------

test_that("a drafted PDF template reads the same rows as the proven one", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  cases <- list(
    c("tests/testthat/fixtures/anz_everyday_pdf_sample.pdf",     "anz_everyday_pdf"),
    c("tests/testthat/fixtures/asb_everyday_pdf_sample.pdf",     "asb_everyday_pdf"),
    c("tests/testthat/fixtures/westpac_everyday_pdf_sample.pdf", "westpac_everyday_pdf"),
    c("samples/raw/tutorial/sample_everyday_statement.pdf",      "tutorial_everyday_pdf"))
  templates <- load_templates(templates_dir(), strict = FALSE)
  for (cs in cases) {
    fx <- fixture(cs[1]); if (!file.exists(fx)) next
    drafted <- draft_template(fx)
    got <- draft_preview(fx, drafted)
    ref <- draft_preview(fx, templates[[cs[2]]])
    expect_true(is.data.frame(got) && nrow(got) > 0,
      info = sprintf("%s: the draft reads no rows", basename(cs[1])))
    expect_equal(nrow(got), nrow(ref), info = basename(cs[1]))
    for (f in intersect(c("date", "description", "debit", "credit", "balance", "amount"),
                        intersect(names(got), names(ref))))
      expect_identical(got[[f]], ref[[f]],
        info = sprintf("%s: drafted %s differs from the proven template", basename(cs[1]), f))
  }
})

test_that("the date band spans the whole printed date, not just the day", {
  # "28 Apr" is TWO words and only "28" is date-shaped. Clustering date-shaped
  # tokens alone produced a band that stopped before the month, so the date cell
  # read "28", "%d %b" parsed nothing and every row was dropped -- from a band that
  # looks right drawn on the page, which is why redrawing it did not help either.
  mk <- function(txt, x, y, w, h = 10) data.frame(text = txt, x = x, y = y,
    width = w, height = h, space = TRUE, stringsAsFactors = FALSE)
  page <- rbind(
    mk(c("Date", "Details", "Withdrawals", "Balance"), c(45, 81, 338, 500), 51,
       c(18, 45, 48, 30)),
    mk(c("28", "Apr", "COFFEE", "4.50", "995.50"), c(45, 56, 81, 355, 500), 78,
       c(9, 13, 45, 31, 30)),
    mk(c("29", "Apr", "POWER", "80.00", "915.50"), c(45, 56, 81, 355, 500), 91,
       c(9, 13, 45, 31, 30)))
  s <- suggest_pdf_columns(list(kind = "pdf", words = list(page)))
  db <- s[s$field == "date", ]
  expect_equal(nrow(db), 1L)
  expect_true(db$x_min <= 45 && db$x_max >= 69)     # covers "28" AND "Apr"
  expect_equal(attr(s, "date_cells"), c("28 Apr", "29 Apr"))
  # ...and the month must not ALSO be claimed by the description, which is
  # promised verbatim.
  expect_true(s$x_min[s$field == "description"] > 69)
})

test_that("money columns are labelled from the statement's own header row", {
  mk <- function(txt, x, y, w, h = 10) data.frame(text = txt, x = x, y = y,
    width = w, height = h, space = TRUE, stringsAsFactors = FALSE)
  page <- rbind(
    mk(c("Date", "Details", "Withdrawals", "Deposits", "Balance"),
       c(45, 81, 338, 425, 500), 51, c(18, 45, 48, 33, 30)),
    mk(c("02", "May", "COFFEE", "4.50", "995.50"), c(45, 56, 81, 355, 500), 78,
       c(9, 15, 45, 31, 30)),
    mk(c("03", "May", "SALARY", "500.00", "1,495.50"), c(45, 56, 81, 434, 495), 91,
       c(9, 15, 45, 24, 35)))
  s <- suggest_pdf_columns(list(kind = "pdf", words = list(page)))
  expect_equal(s$field, c("date", "description", "debit", "credit", "balance"))
  # A column the HEADING names but this page never printed a figure in still gets a
  # band. Without it a statement whose first page is all withdrawals drafts with no
  # credit band at all, and every deposit further in is a row with no money in any
  # mapped column -- dropped, silently.
  page2 <- page[page$text != "500.00", ]
  s2 <- suggest_pdf_columns(list(kind = "pdf", words = list(page2)))
  expect_true("credit" %in% s2$field)
  cr <- s2[s2$field == "credit", ]
  expect_true(cr$x_min <= 425 && cr$x_max >= 458)
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

test_that("a statement in another currency, with ordinal dates, is still read", {
  # Neither of these is exotic, and each on its own cost the whole file: the money
  # test was pinned to "$", so a pound-sign statement showed the sniffer no money
  # at all -- no money means no columns, and no columns means the drafter reports
  # "this is not a transaction table" about a document that plainly is one.
  mk <- function(txt, x, y, w, h = 10) data.frame(text = txt, x = x, y = y,
    width = w, height = h, space = TRUE, stringsAsFactors = FALSE)
  pound <- "\u00a3"          # written as an escape: this repo's sources stay ASCII
  page <- rbind(
    mk(c("Date", "Details", "Money Out", "Balance"), c(19, 91, 282, 371), 227,
       c(14, 20, 34, 23)),
    mk(c("1st", "November", "2018", "ATM", paste0(pound, "10.00"), paste0(pound, "20.00")),
       c(19, 31, 64, 91, 303, 382), 247, c(8, 31, 14, 18, 19, 19)),
    mk(c("3rd", "November", "2018", "Cinema", paste0(pound, "6.50"), paste0(pound, "13.50")),
       c(19, 31, 64, 91, 307, 382), 257, c(8, 31, 14, 22, 16, 19)))
  s <- suggest_pdf_columns(list(kind = "pdf", words = list(page)))
  expect_true(all(c("date", "description", "debit", "balance") %in% s$field))
  expect_equal(attr(s, "date_cells"), c("1st November 2018", "3rd November 2018"))
  expect_true(pdf_has_transaction_rows(list(words = list(page))))
})

test_that("the last column stops after its balance marker, not at the page margin", {
  # "17,731.96 OD" has to arrive whole -- OD is the SIGN of that balance. But
  # running the band on to the right-hand margin of the text swallowed the notes
  # some statements print beside the table into the balance cell as well.
  mk <- function(txt, x, y, w, h = 10) data.frame(text = txt, x = x, y = y,
    width = w, height = h, space = TRUE, stringsAsFactors = FALSE)
  hdr <- mk(c("Date", "Details", "Withdrawals", "Balance"), c(45, 81, 338, 500), 51,
            c(18, 45, 48, 30))
  with_od <- rbind(hdr,
    mk(c("18", "Feb", "Vet", "225.00", "17,956.96", "OD"),
       c(45, 56, 81, 362, 493, 532), 78, c(9, 14, 12, 24, 38, 12)))
  s <- suggest_pdf_columns(list(kind = "pdf", words = list(with_od)))
  b <- s[s$field == "balance", ]
  expect_true(b$x_max >= 544)                       # the OD comes with its balance
  with_note <- rbind(hdr,
    mk(c("18", "Feb", "Vet", "225.00", "17,956.96", "my", "share", "of", "the", "bill"),
       c(45, 56, 81, 362, 493, 536, 552, 580, 594, 610), 78,
       c(9, 14, 12, 24, 38, 12, 24, 12, 18, 16)))
  s2 <- suggest_pdf_columns(list(kind = "pdf", words = list(with_note)))
  b2 <- s2[s2$field == "balance", ]
  expect_true(b2$x_max < 536)                       # the marginal note does not
})

test_that("suggest_pdf_columns is safe on input with no words", {
  expect_equal(nrow(suggest_pdf_columns(list(words = list()))), 0L)
  expect_equal(nrow(suggest_pdf_columns(list())), 0L)
})

# ---------------------------------------------------------------------------
# W2. THE BANK NAME IS EVIDENCE, NOT KEYWORD ORDER.
#
# .sniff_bank used to return the first .KNOWN_BANKS entry found anywhere in the
# text, in declaration order. A real ASB statement answered "ANZ" -- ANZ is one of
# the payees in its bill-payment list -- and the name was then pre-filled with a
# Confirm button beside it. A wrong answer somebody is invited to confirm is worse
# than no answer.
# ---------------------------------------------------------------------------

# One A4 page of word boxes, one input line per printed line. `step` is generous so
# a line's index says whether it is in the masthead (the top quarter of the page)
# or well down the page, which is the distinction under test.
.sb_page <- function(lines, y0 = 30, step = 60) {
  do.call(rbind, lapply(seq_along(lines), function(i) {
    w <- strsplit(trimws(lines[i]), " +")[[1]]
    if (!length(w) || !nzchar(w[1])) return(NULL)
    x <- 40 + cumsum(c(0, utils::head(nchar(w) * 6 + 4, -1)))
    data.frame(text = w, x = x, y = y0 + (i - 1) * step, width = nchar(w) * 6,
               height = 10, space = TRUE, stringsAsFactors = FALSE)
  }))
}
.sb_input <- function(lines) {
  pg <- .sb_page(lines)
  list(kind = "pdf", words = list(pg), page_width = 595.28, page_height = 841.89,
       pages = paste(lines, collapse = "\n"))
}

test_that("a bank named in a payee line never outranks the issuer's own imprint", {
  # The shape of the real defect: ANZ, BNZ and Westpac all appear in the payee
  # list; only ASB appears as the issuer.
  inp <- .sb_input(c(
    "Streamline account", "Account no 12-3142-0502886-02", "Opening date 13 Jun 26",
    "Mr M A Fry", "Bill payment authorities",
    "Sam BNZ 01 New authority", "ANZ 03 New authority", "Sam Westpac 13 New authority",
    "ASB Bank Limited terms apply. Refer to asb.co.nz for other fees"))
  expect_identical(.sniff_bank(inp, fallback = "New bank"), "ASB")
})

test_that("a bank printed in the masthead beats one buried further down", {
  inp <- .sb_input(c("Westpac Everyday", "Statement of transactions",
                     "Mr M A Fry", "Date Withdrawals Deposits Balance",
                     "01 May TRANSFER TO ANZ 20.00 980.00"))
  expect_identical(.sniff_bank(inp, fallback = "New bank"), "Westpac")
})

test_that("a masthead names its bank even when the tool has never heard of it", {
  # This is the "Kowhai Bank NZ" case: the name is printed at the top of the page
  # and shown in the identifying-phrase picker on the same screen, so pre-filling a
  # payee's name (or a filename) instead is the tool ignoring what it can already
  # see.
  inp <- .sb_input(c("Kowhai Bank NZ", "Statement of Account", "JAMIE SAMPLE",
                     "Date Withdrawals Deposits Balance",
                     "01 May PAYMENT TO BNZ 20.00 980.00"))
  expect_identical(.sniff_bank(inp, fallback = "New bank"), "Kowhai Bank NZ")
})

test_that("several equally-placed banks mean the tool does not know, and says so", {
  inp <- .sb_input(c("Payment schedule", "Mr M A Fry",
                     "01 May ANZ 20.00", "02 May BNZ 30.00", "03 May Westpac 40.00"))
  expect_identical(.sniff_bank(inp, fallback = "New bank"), "New bank")
  # ...and a name is never invented out of the account holder.
  expect_identical(.sniff_bank(.sb_input(c("Mr John Michael Smith", "1 Example Street")),
                               fallback = "New bank"), "New bank")
})

test_that("a masthead line is trimmed to the bank's name, or rejected as a title", {
  nm <- function(s) .masthead_bank_name(s)
  expect_identical(nm("Kowhai Bank NZ"), "Kowhai Bank NZ")
  expect_identical(nm("Rimu Bank Limited"), "Rimu Bank Limited")
  expect_identical(nm("Totara Savings Bank"), "Totara Savings Bank")
  expect_identical(nm("Bank of New Zealand"), "Bank of New Zealand")
  expect_identical(nm("Dummy Bank Statement"), "Dummy Bank")   # a statement FROM Dummy Bank
  expect_identical(nm("Bank Statement"), "")                   # ...is what the document IS
  expect_identical(nm("Internet Banking"), "")                 # ...and this is an activity
  expect_identical(nm("Mr John Bank Smith"), "")               # never an account holder
})

test_that("one bank named anywhere is still enough when nothing else competes", {
  inp <- .sb_input(c("Statement of Accounts", "MR M A FRY",
                     "Date Withdrawals Deposits Balance",
                     "28 Apr COFFEE 4.50 995.50", "anz.co.nz 0800 269 296"))
  expect_identical(.sniff_bank(inp, fallback = "New bank"), "ANZ")
})

# ---------------------------------------------------------------------------
# W3. THE PICKER MAY ONLY OFFER PHRASES THE SAVE WOULD ACCEPT.
#
# The toolkit's "identifying phrase" dropdown is built from header_phrases(), and
# it was offering bare words like "Date" and "details" -- which validate_template
# then refused as too generic. The same screen suggesting an answer and then
# rejecting it is the tool arguing with itself in front of the user.
# ---------------------------------------------------------------------------

test_that("every phrase header_phrases offers would pass the save on its own", {
  input <- list(pages = paste(
    "Kowhai Bank NZ - Statement of transactions",
    "Account 12-3456-7890123-00",
    "Date Withdrawals Deposits Balance",
    "01 May COFFEE 4.50 995.50", sep = "\n"))
  ph <- header_phrases(input, n = 8)
  expect_true(length(ph) >= 1)
  for (p in ph) {
    tmpl <- list(id = "x", bank = "B", statement_type = "s", format = "pdf",
      version = 1, min_score = 1, currency = "NZD",
      fingerprint = list(page_contains_all = list(p)),
      table = list(row_tol = 3, date_format = "%d/%m/%Y", amount_sign = "signed",
        columns = list(date = list(x_min = 0, x_max = 90),
                       description = list(x_min = 90, x_max = 300),
                       amount = list(x_min = 400, x_max = 470))))
    expect_length(validate_template(tmpl), 0)
  }
  # the specific words that used to be offered and then refused
  expect_false(any(c("Date", "Balance", "details", "Deposits") %in% ph))
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
