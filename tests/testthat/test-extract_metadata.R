# Tests for GENERIC statement metadata + multi-statement detection.
# All pattern-based -- no per-bank / per-sample hardcoding.

IF_META_PDF <- "samples/raw/anz/anz_investmentfunds_statement_guide_sample.pdf"

test_that("generic metadata is extracted from a real statement", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(IF_META_PDF)))
  m <- extract_metadata(read_input(fixture(IF_META_PDF)))
  expect_equal(m$pages_actual, 3L)      # actual PDF pages (front + back + terms)
  expect_equal(m$pages_stated, 2L)      # "Page X of 2"
  expect_match(m$period_start, "1 April 2025", fixed = TRUE)
  expect_match(m$period_end, "31 March 2026", fixed = TRUE)
})

test_that("multi-statement detection keys on PERIODS, not account count", {
  # two distinct periods -> flagged as a genuine bundle
  m3 <- list(n_accounts = 0, n_periods = 2, page1_markers = 1)
  expect_true(detect_multiple_statements(NULL, m3)$likely_multiple)
  # several accounts but ONE period -> NOT a bundle; it's a combined statement.
  # (Real data: a single ANZ statement showed 5 account numbers via transfer
  # narratives yet had one continuous running balance.)
  m <- list(n_accounts = 5, n_periods = 1, page1_markers = 1)
  d <- detect_multiple_statements(NULL, m)
  expect_false(d$likely_multiple)
  expect_true(d$combined_accounts)
  # repeated 'Page 1 of N' IS now a strong bundle signal (P2-4): each statement
  # restarts its page numbering, so >1 first page = >1 statement -- catching
  # bundles the period signal misses. Flagging routes to needs_review (the safe
  # default); a non-statement that trips it wouldn't match a template anyway.
  m2 <- list(n_accounts = 1, n_periods = 1, page1_markers = 3)
  expect_true(detect_multiple_statements(NULL, m2)$likely_multiple)
  # a repeated opening AND closing balance block also flags (P2-4).
  m4 <- list(n_accounts = 1, n_periods = 1, page1_markers = 1,
             n_opening_labels = 2, n_closing_labels = 2)
  expect_true(detect_multiple_statements(NULL, m4)$likely_multiple)
  # a single statement (one of everything) is NOT flagged.
  m5 <- list(n_accounts = 1, n_periods = 1, page1_markers = 1,
             n_opening_labels = 1, n_closing_labels = 1)
  expect_false(detect_multiple_statements(NULL, m5)$likely_multiple)
})

test_that("period is found from LABELLED opening/closing dates (not just a range)", {
  # Westpac/ASB style: two labelled dates on separate lines, no "from X to Y".
  input <- list(kind = "pdf", pages = paste(sep = "\n",
    "Westpac Everyday",
    "Statement Opening date:  10 June 2026",
    "Statement Closing date:   9 July 2026",
    "15 Jun DD Rates 863.90"))
  m <- extract_metadata(input)
  expect_match(m$period_start, "10 June 2026", fixed = TRUE)
  expect_match(m$period_end, "9 July 2026", fixed = TRUE)
  expect_true(m$n_periods >= 1)   # so year-less transaction dates can resolve
})

test_that("account and card regexes are generic", {
  input <- list(pages = c(
    "Account 01-0902-0123456-00 statement",
    "Card 4835-****-****-6843 ending"))
  m <- extract_metadata(input)
  expect_true("01-0902-0123456-00" %in% m$accounts)
  expect_equal(m$n_accounts, 2L)
})

# ---- ONE SPAN: a file that prints several periods -----------------------------
# The reported defect: a statement covering three printed periods came back with
# dates_within_period and balance_reconciliation both red on a file that was
# completely fine -- the other periods' rows fell "outside", and the first
# period's opening was compared with the first period's closing while the parse
# held EVERY transaction. Two false alarms, which is how people learn to ignore
# the one warning that matters.

# .section(...) -- one printed statement section: an account line, its period line,
# an opening balance, some rows and a closing balance. The layouts below are all
# built from it, so what differs between them is only what each test is about.
.section <- function(from, to, opening, rows, closing, account = "01-0902-0123456-00")
  c(sprintf("ACME BANK  Account %s", account),
    sprintf("Statement period %s to %s", from, to),
    sprintf("Opening balance %s", opening),
    rows,
    sprintf("Closing balance %s", closing))

# Three contiguous monthly sections for one account: 1000 -> 900 -> 1400 -> 1000.
.three_periods <- function()
  list(kind = "text", pages = paste(collapse = "\n", c(
    .section("1 January 2026", "31 January 2026", "1,000.00", "05 Jan 2026 Groceries -100.00", "900.00"),
    .section("1 February 2026", "28 February 2026", "900.00", "10 Feb 2026 Wages 500.00", "1,400.00"),
    .section("1 March 2026", "31 March 2026", "1,400.00", "12 Mar 2026 Rent -400.00", "1,000.00"))))

test_that("several printed periods are read as ONE span, first opening to last closing", {
  m <- extract_metadata(.three_periods())
  expect_equal(m$n_periods, 3L)                       # the count is recorded, not hidden
  expect_identical(m$period_start, "1 January 2026")  # EARLIEST start printed anywhere
  expect_identical(m$period_end,   "31 March 2026")   # LATEST end
  expect_identical(m$opening_balance, "1,000.00")     # the FIRST period's opening
  expect_identical(m$closing_balance, "1,000.00")     # the LAST period's closing
  expect_true(is.na(m$period_note))                   # contiguous: nothing to warn about
})

test_that("the two checks that false-failed now pass on that same file", {
  # This is the whole point: first opening + EVERY transaction = last closing,
  # and every row's date falls inside the merged span.
  m <- extract_metadata(.three_periods())
  tx <- data.frame(row_id = 1:3, amount = c(-100, 500, -400), balance = NA_real_,
                   date = c("2026-01-05", "2026-02-10", "2026-03-12"),
                   flags = "", stringsAsFactors = FALSE)
  .with <- function(ps, pe, ob, cb) reconcile(list(
    transactions = coerce_core(tx), extras = data.frame(row_id = integer(0)),
    header = list(period_start = ps, period_end = pe, opening_balance = .num(ob),
                  closing_balance = .num(cb), row_count = 3L),
    provenance = data.frame(row_id = 1:3),
    source_line_count = 3L, multiline_extra = 0L))$kpis
  k <- .with(m$period_start, m$period_end, m$opening_balance, m$closing_balance)
  expect_equal(k$status[k$name == "balance_reconciliation"], "pass")
  expect_equal(k$status[k$name == "dates_within_period"], "pass")
  # ...and the SAME transactions against a first-period-only view are exactly the
  # two red checks that were reported. This pins the defect, so the fix cannot be
  # quietly undone while the test above still passes for some other reason.
  was <- .with("1 January 2026", "31 January 2026", "1,000.00", "900.00")
  expect_equal(was$status[was$name == "balance_reconciliation"], "fail")
  expect_equal(was$status[was$name == "dates_within_period"], "fail")
})

test_that("ONE period is left completely alone", {
  # The overwhelmingly common case. The span code must not run at all: the values
  # are the printed ones and there is nothing extra to say.
  one <- list(kind = "text", pages = paste(collapse = "\n",
    .section("1 January 2026", "31 January 2026", "1,000.00", "05 Jan 2026 Groceries -100.00", "900.00")))
  m <- extract_metadata(one)
  expect_equal(m$n_periods, 1L)
  expect_identical(m$period_start, "1 January 2026")
  expect_identical(m$period_end,   "31 January 2026")
  expect_identical(m$opening_balance, "1,000.00")
  expect_identical(m$closing_balance, "900.00")       # NOT a "last closing" anywhere else
  expect_true(is.na(m$period_note))
  # ...and the same period written two ways is still ONE period, not a merge.
  twice <- list(kind = "text", pages = paste(one$pages,
    "Statement period 1 January 2026 - 31 January 2026", sep = "\n"))
  m2 <- extract_metadata(twice)
  expect_identical(m2$period_start, "1 January 2026")
  expect_identical(m2$period_end,   "31 January 2026")
  expect_identical(m2$closing_balance, "900.00")
  expect_true(is.na(m2$period_note))
})

test_that("a BUNDLE of statements is not merged into a span - split still owns it", {
  # One statement with three sections is not the same thing as three statements in
  # one file. A statement restarts its page numbering, so more than one "Page 1 of
  # N" means separate documents, and those have a better answer than one long span
  # (split each and reconcile it on its own anchors, or flag and refuse). Merging
  # them would hand a bundle a clean bill of health on the merged path.
  bundle <- list(kind = "text", pages = c(
    paste(c(.section("1 January 2026", "31 January 2026", "1,000.00",
                     "05 Jan 2026 Groceries -100.00", "900.00"), "Page 1 of 1"), collapse = "\n"),
    paste(c(.section("1 February 2026", "28 February 2026", "900.00",
                     "10 Feb 2026 Wages 500.00", "1,400.00"), "Page 1 of 1"), collapse = "\n")))
  m <- extract_metadata(bundle)
  expect_equal(m$page1_markers, 2L)
  expect_identical(m$period_start, "1 January 2026")   # the FIRST period only,
  expect_identical(m$period_end,   "31 January 2026")  # exactly as before
  expect_identical(m$closing_balance, "900.00")        # NOT the second statement's
  expect_true(detect_multiple_statements(NULL, m)$likely_multiple)
})

test_that("the span is ordered by DATE, never by where it is printed", {
  # A statement can print its sections (or a summary block) out of order. Taking
  # the first opening and the last closing in DOCUMENT order would pair March's
  # opening with January's closing -- a plausible, wrong reconciliation.
  ood <- list(kind = "text", pages = paste(collapse = "\n", c(
    .section("1 February 2026", "28 February 2026", "900.00", "10 Feb 2026 Wages 500.00", "1,400.00"),
    .section("1 January 2026", "31 January 2026", "1,000.00", "05 Jan 2026 Groceries -100.00", "900.00"))))
  m <- extract_metadata(ood)
  expect_identical(m$period_start, "1 January 2026")
  expect_identical(m$period_end,   "28 February 2026")
  expect_identical(m$opening_balance, "1,000.00")   # January's, though printed second
  expect_identical(m$closing_balance, "1,400.00")   # February's, though printed first
})

test_that("a HOLE between the periods is surfaced, not silently spanned", {
  # One span over non-contiguous periods covers days no printed period covers, so
  # a transaction missing from that window could never be noticed. Fail loudly.
  gapped <- list(kind = "text", pages = paste(collapse = "\n", c(
    .section("1 January 2026", "31 January 2026", "1,000.00", "05 Jan 2026 Groceries -100.00", "900.00"),
    .section("1 March 2026", "31 March 2026", "900.00", "12 Mar 2026 Rent -400.00", "500.00"))))
  m <- extract_metadata(gapped)
  expect_identical(m$period_start, "1 January 2026")
  expect_identical(m$period_end,   "31 March 2026")
  expect_false(is.na(m$period_note))
  expect_true(grepl("2026-02-01", m$period_note, fixed = TRUE))   # the uncovered window,
  expect_true(grepl("2026-02-28", m$period_note, fixed = TRUE))   # named exactly
  # ...and it reaches the screen, where the reviewer is already being asked to look.
  expect_true(any(m$period_note == detect_multiple_statements(NULL, m)$reasons))
})

test_that("an unprovable pairing moves NOTHING and says so", {
  # Only the first section prints an opening/closing block; the second does not.
  # There is then no evidence that "the last closing" belongs to the last period,
  # so the balances must not move -- a wrong pairing reconciles to a plausible,
  # WRONG figure, which is worse than the false alarm being fixed.
  half <- list(kind = "text", pages = paste(collapse = "\n", c(
    .section("1 January 2026", "31 January 2026", "1,000.00", "05 Jan 2026 Groceries -100.00", "900.00"),
    c("Statement period 1 February 2026 to 28 February 2026", "10 Feb 2026 Wages 500.00"))))
  m <- extract_metadata(half)
  # The DATES stay put too. This test used to assert they widened, on the theory
  # that "earliest start and latest end are printed facts" - but what is printed
  # is a date RANGE, and a statement prints ranges that are not statement periods
  # (a loan term, a tax year). Widening on an unproven set moved the reader's year
  # context and produced dates two years out that dates_within_period then PASSED.
  expect_identical(m$period_start, "1 January 2026")
  expect_identical(m$period_end,   "31 January 2026")  # NOT widened to 28 February
  expect_identical(m$opening_balance, "1,000.00")      # unchanged from a single-period read
  expect_identical(m$closing_balance, "900.00")
  expect_false(is.na(m$period_note))                   # ...and never in silence
})

test_that("periods that do not CHAIN are never paired (the same-account proof)", {
  # Two accounts, one period each, in one upload. Pairing account A's opening with
  # account B's closing would reconcile to a plausible figure about nothing at all.
  # What rules it out is the money: A closes on 900 and B opens on 50, so these are
  # not consecutive sections of one ledger. (Account NUMBERS cannot decide this --
  # a real statement names other accounts in its transfer narratives.)
  two_accts <- list(kind = "text", pages = paste(collapse = "\n", c(
    .section("1 January 2026", "31 January 2026", "1,000.00", "05 Jan 2026 Groceries -100.00",
             "900.00", account = "01-0902-0123456-00"),
    .section("1 February 2026", "28 February 2026", "50.00", "10 Feb 2026 Wages 500.00",
             "550.00", account = "12-3456-7890123-45"))))
  m <- extract_metadata(two_accts)
  expect_identical(m$opening_balance, "1,000.00")
  expect_identical(m$closing_balance, "900.00")        # NOT the other account's 550.00
  expect_false(is.na(m$period_note))
  # The very same file with the sections chained (900 -> 900) IS one account, and
  # then the span's balances are the first opening and the last closing.
  chained <- list(kind = "text", pages = sub("Opening balance 50.00", "Opening balance 900.00",
                                             two_accts$pages, fixed = TRUE))
  mc <- extract_metadata(chained)
  expect_identical(mc$opening_balance, "1,000.00")
  expect_identical(mc$closing_balance, "550.00")
})

test_that("periods that cannot be ordered fall back to the first one, loudly", {
  # A period whose dates do not parse leaves the ORDER of the set unknown, and an
  # unknown order is exactly when guessing produces a wrong pairing.
  odd <- list(kind = "text", pages = paste(collapse = "\n", c(
    .section("1 January 2026", "31 January 2026", "1,000.00", "05 Jan 2026 Groceries -100.00", "900.00"),
    .section("1 Smarch 2026", "31 Smarch 2026", "900.00", "10 Feb 2026 Wages 500.00", "1,400.00"))))
  m <- extract_metadata(odd)
  expect_identical(m$period_start, "1 January 2026")   # exactly the pre-existing behaviour
  expect_identical(m$period_end,   "31 January 2026")
  expect_identical(m$opening_balance, "1,000.00")
  expect_identical(m$closing_balance, "900.00")
  expect_false(is.na(m$period_note))                   # ...but never in silence
})

test_that("overlapping periods move nothing at all", {
  # Overlapping ranges are not one account's consecutive sections, so which period
  # is "first" and which "last" is not established -- and neither the balances NOR
  # the dates may move on an unproven set (see the note above).
  lap <- list(kind = "text", pages = paste(collapse = "\n", c(
    .section("1 January 2026", "31 March 2026", "1,000.00", "05 Jan 2026 Groceries -100.00", "900.00"),
    .section("1 February 2026", "30 April 2026", "900.00", "10 Feb 2026 Wages 500.00", "1,400.00"))))
  m <- extract_metadata(lap)
  expect_identical(m$period_start, "1 January 2026")
  expect_identical(m$period_end,   "31 March 2026")    # its OWN end, not the span
  expect_identical(m$opening_balance, "1,000.00")      # left where a single read leaves them
  expect_identical(m$closing_balance, "900.00")
  expect_false(is.na(m$period_note))
})

test_that("metadata reaches the convert result and the workbook", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(IF_META_PDF)))
  out <- tempfile("md_out_")
  res <- convert_statement(fixture(IF_META_PDF), outdir = out,
                           templates_dir = templates_dir(), logdir = tempfile("l_"))
  expect_false(is.null(res$metadata))
  expect_equal(res$metadata$pages_actual, 3L)
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  wb <- openxlsx::loadWorkbook(res$outputs[["xlsx"]])
  expect_true("Metadata" %in% names(wb))
})

test_that("an oversized page (>2880 pt) raises a diagnostic (Hubdoc-style pre-flight)", {
  tx <- data.frame(row_id = 1L, date = "2025-01-01", date_raw = "1/1/25", description = "a",
    amount = -1, amount_raw = "-1", direction = "debit", balance = NA_real_,
    balance_raw = NA_character_, particulars = NA_character_, code = NA_character_,
    reference = NA_character_, other_party = NA_character_, type = NA_character_,
    currency = "NZD", flags = "", stringsAsFactors = FALSE)
  d <- build_diagnostics("ok", parsed = list(transactions = tx), recon = list(kpis = NULL),
    metadata = list(multi = list(likely_multiple = FALSE), pages = 2, max_page_pt = 3000))
  expect_true(any(d$category == "oversized_page"))
})

test_that("a real statement's page size is within limits", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(IF_META_PDF)))
  m <- extract_metadata(read_input(fixture(IF_META_PDF)))
  expect_true(is.finite(m$max_page_pt))
  expect_lt(m$max_page_pt, 2880)   # A4 ~842 pt
})

# ---- The multi-period span: what the adversarial review found -----------------
# Every case below is a defect the first version of .period_span actually had,
# reproduced by an independent reviewer. They are kept as tests because each one
# is a WRONG FIGURE, not a cosmetic slip, and three of them looked correct.
.mp <- function(...) extract_metadata(list(
  kind = "pdf", path = tempfile(fileext = ".pdf"),
  pages = paste(c(...), collapse = "\n"), meta = list(page_count = 1L)))

test_that("a stray date range does NOT widen a single-period statement", {
  # A loan term / tax year / term-deposit window is a DATE RANGE, not a statement
  # period. Widening on it moved the reader's year context: a January statement
  # beside a 2024-2026 loan term resolved every year-less date to 2024, and
  # dates_within_period read the same widened period and PASSED.
  m <- .mp("Statement period 01/01/2026 to 31/01/2026",
           "Your fixed loan runs 01/01/2024 to 31/12/2026")
  expect_identical(m$period_start, "01/01/2026")
  expect_identical(m$period_end,   "31/01/2026")
  expect_false(is.na(m$period_note))          # and it says why it refused
})

test_that("a shared boundary day is not an overlap", {
  # Banks routinely print the previous closing date as the next opening date.
  # Rejecting that rejected the commonest real multi-period statement there is.
  m <- .mp("Statement period 31/12/2025 to 31/01/2026", "Opening balance 100.00",
           "Closing balance 150.00",
           "Statement period 31/01/2026 to 28/02/2026", "Opening balance 150.00",
           "Closing balance 175.00")
  expect_identical(m$period_start, "31/12/2025")
  expect_identical(m$period_end,   "28/02/2026")
  expect_identical(m$opening_balance, "100.00")
  expect_identical(m$closing_balance, "175.00")
  expect_true(is.na(m$period_note))
})

test_that("balances are never paired across two different accounts", {
  # Two dormant accounts both opening and closing on 0.00 chain PERFECTLY. The
  # chain proof alone believed that, paired across two accounts, and said nothing.
  m <- .mp("Statement period 01/01/2026 to 31/01/2026", "Account 12-3456-0789012-50",
           "Opening balance 0.00", "Closing balance 0.00",
           "Statement period 01/02/2026 to 28/02/2026", "Account 12-3456-0789012-99",
           "Opening balance 0.00", "Closing balance 0.00")
  expect_identical(m$period_start, "01/01/2026")   # not merged
  expect_match(m$period_note, "more than one account")
})

test_that("one period printed two ways counts as one period", {
  # `periods` is unique on the matched STRING, so a statement printing its period
  # in two styles announced "2 periods" on screen for a single-period statement.
  m <- .mp("Statement period 01/01/2026 to 31/01/2026",
           "Statement period 01/01/2026 - 31/01/2026")
  expect_equal(m$n_periods, 1L)
  expect_true(is.na(m$period_note))
})

test_that("a genuine consecutive single-account file still merges", {
  m <- .mp("Statement period 01/01/2026 to 31/01/2026", "Opening balance 100.00",
           "Closing balance 150.00",
           "Statement period 01/02/2026 to 28/02/2026", "Opening balance 150.00",
           "Closing balance 175.00",
           "Statement period 01/03/2026 to 31/03/2026", "Opening balance 175.00",
           "Closing balance 200.00")
  expect_identical(m$period_start, "01/01/2026")
  expect_identical(m$period_end,   "31/03/2026")
  expect_identical(m$opening_balance, "100.00")
  expect_identical(m$closing_balance, "200.00")
  expect_equal(m$n_periods, 3L)
})
