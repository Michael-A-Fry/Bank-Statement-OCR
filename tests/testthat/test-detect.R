# Detection tie-breaking / eligibility (build-contract section 6).
# A candidate that scores below its OWN min_score must not create a false tie
# that blocks a genuinely-matching template. Every specimen must detect
# unambiguously with NO bank hint supplied.

test_that("every specimen detects its own template with no hint", {
  templates <- load_templates(templates_dir())
  cases <- list(
    c("samples/raw/anz/anz_transaction_export_01.csv", "anz_everyday_csv"),
    c("samples/raw/anz/anz_creditcard_01.csv",         "anz_creditcard_csv"),
    c("samples/raw/asb/asb_transaction_export_01.csv", "asb_everyday_csv"),
    c("samples/raw/bnz/bnz_transaction_export_01.csv", "bnz_everyday_csv"),
    c("samples/raw/kiwibank/kiwibank_transaction_01.csv", "kiwibank_everyday_csv"),
    c("samples/raw/westpac/westpac_transaction_export_01.csv", "westpac_everyday_csv")
  )
  for (cs in cases) {
    input <- read_input(fixture(cs[1]))
    det <- detect_statement(input, templates)   # NO hint
    expect_true(det$matched,
      info = sprintf("%s should match with no hint: %s", cs[2], det$detail))
    expect_identical(det$template_id, cs[2])
  }
})

test_that("an ineligible higher-alphabetical candidate cannot block a match", {
  # BNZ file: bnz_everyday_csv (min 3) is eligible at score 5; anz_everyday_csv
  # (min 6) scores 5 too but is INELIGIBLE. It must not tie/steal the match.
  templates <- load_templates(templates_dir())
  input <- read_input(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"))
  det <- detect_statement(input, templates)
  expect_true(det$matched)
  expect_identical(det$template_id, "bnz_everyday_csv")
})

test_that("genuinely ambiguous eligible candidates report unmatched", {
  # Two synthetic templates that both fully match and both clear min_score must
  # be reported ambiguous (not silently picked).
  t <- function(id) list(
    id = id, bank = "X", statement_type = "e", format = "delimited", version = 1,
    min_score = 1, fingerprint = list(header_contains_all = c("Date", "Amount")),
    delimiter = ",", columns = list(date = list(source = "Date"),
      amount = list(source = "Amount"), description = list(source = "Amount")),
    amount_sign = "signed", currency = "NZD")
  templates <- list(aaa = t("aaa"), bbb = t("bbb"))
  input <- list(kind = "delimited", path = "x.csv", sha256 = "s",
                lines = c("Date,Amount", "01/01/2020,1.00"))
  det <- detect_statement(input, templates)
  expect_false(det$matched)
  expect_true(grepl("ambiguous", det$detail))
})

test_that("filename_regex cannot create eligibility, only break a tie (P2-6)", {
  mk <- function(id, need, min_score, fr = NULL) list(
    id = id, bank = "X", statement_type = "e", format = "delimited", version = 1,
    min_score = min_score,
    fingerprint = list(header_contains_all = need, filename_regex = fr),
    delimiter = ",", columns = list(date = list(source = "Date"),
      amount = list(source = "Amount"), description = list(source = "Amount")),
    amount_sign = "signed", currency = "NZD")
  input <- list(kind = "delimited", path = "anz_statement.csv", sha256 = "s",
                lines = c("Date,Amount", "01/01/2020,1.00"))
  # A template that matches NOTHING in the header (min_score 1) but whose
  # filename_regex matches must NOT become eligible on the name alone.
  det <- detect_statement(input, list(
    fn = mk("fn", c("Payee", "Reference"), 1, "anz")))   # 0 header hits, name matches
  expect_false(det$matched)

  # Among two EQUAL content scores, the filename match breaks the tie.
  det2 <- detect_statement(input, list(
    aaa = mk("aaa", c("Date", "Amount"), 1, NULL),
    bbb = mk("bbb", c("Date", "Amount"), 1, "anz")))      # both score 2 on content
  expect_true(det2$matched)
  expect_identical(det2$template_id, "bbb")               # won on the filename tie-break
})

test_that("sample templates are excluded from the production detection set (P2-8)", {
  set <- load_template_set(templates_dir(), tempfile())   # no user dir
  expect_false("tutorial_everyday_pdf" %in% names(set))   # not a detection candidate
  # ...but it is still loadable directly (wizard walkthrough / self-tests).
  raw <- load_templates(templates_dir())
  expect_true("tutorial_everyday_pdf" %in% names(raw))
  expect_true(isTRUE(raw[["tutorial_everyday_pdf"]]$sample))
})

test_that("an off-band foreign PDF is unsupported, not parsed as ASB (P1-3/56)", {
  # THE defect this package exists for. asb_everyday_pdf used to fingerprint on
  # ["Debit/Withdrawal", "Deposit", "Balance"] with min_score 2, so ANY pdf
  # carrying the words "Deposit" and "Balance" -- i.e. essentially every bank
  # statement on earth -- scored 2, matched ASB, and was parsed with ASB's column
  # bands (a foreign bank's charges came out as credits, status ok, feed ACCEPTED).
  templates <- load_template_set(templates_dir(), tempfile())
  foreign <- list(kind = "pdf", path = "foreign_bank.pdf", sha256 = "s", pages = paste(
    "Banco Ejemplo - Cuenta Corriente",
    "Statement of Account   Page 1 of 2",
    "Date      Concept                   Deposit     Withdrawal    Balance",
    "01/03/2026 Comision de mantenimiento              12.00      1,988.00",
    "04/03/2026 Transferencia recibida     500.00                 2,488.00", sep = "\n"))
  det <- detect_statement(foreign, templates)
  expect_false(det$matched)
  expect_false(identical(det$template_id, "asb_everyday_pdf"))
})

test_that("the ASB pdf fingerprint no longer shadows a genuine ANZ statement (P1-3/56)", {
  # The same two generic words scored 2 on real ANZ statements too, so
  # asb_everyday_pdf was a permanent runner-up: anz_everyday_pdf won 3-vs-2, a
  # margin of 1, which R/convert.R forces to needs_review -- every genuine ANZ PDF
  # was withheld from the Qlik feed forever. The margin must now be Inf (no other
  # eligible candidate at all).
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  fx <- fixture("tests/testthat/fixtures/anz_everyday_pdf_sample.pdf")
  skip_if_not(file.exists(fx))
  det <- detect_statement(read_input(fx), load_template_set(templates_dir(), tempfile()))
  expect_true(det$matched)
  expect_identical(det$template_id, "anz_everyday_pdf")
  expect_true(is.infinite(det$margin))          # no near-miss runner-up
  expect_true(is.na(det$runner_up))
})

test_that("every shipped PDF template detects its own fixture unambiguously", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  cases <- list(
    c("tests/testthat/fixtures/anz_everyday_pdf_sample.pdf",     "anz_everyday_pdf"),
    c("tests/testthat/fixtures/asb_everyday_pdf_sample.pdf",     "asb_everyday_pdf"),
    c("tests/testthat/fixtures/westpac_everyday_pdf_sample.pdf", "westpac_everyday_pdf"))
  templates <- load_template_set(templates_dir(), tempfile())
  for (cs in cases) {
    skip_if_not(file.exists(fixture(cs[1])))
    det <- detect_statement(read_input(fixture(cs[1])), templates)   # NO hint
    expect_true(det$matched, info = sprintf("%s: %s", cs[2], det$detail))
    expect_identical(det$template_id, cs[2])
    expect_true(is.infinite(det$margin))       # nothing else is even eligible
  }
})

test_that("fingerprint matching tolerates a PDF's column padding (P1-48)", {
  # The drafter stores a phrase whitespace-COLLAPSED, but a pdftools page keeps the
  # wide column padding, and matching is a fixed substring -- so a template could
  # not detect the file it was drafted from. Both sides are normalised now.
  tmpl <- list(id = "pad", format = "pdf",
               fingerprint = list(page_contains_all = c("Withdrawals Deposits Balance")))
  padded <- list(pages = "Date   Details            Withdrawals    Deposits    Balance")
  expect_equal(.score_template(padded, tmpl)$score, 1)
  # ...but a phrase must still not match ACROSS a line break: folding newlines too
  # would make detection looser, the wrong direction for a fail-closed engine.
  split_lines <- list(pages = "Date   Details   Withdrawals\nDeposits   Balance")
  expect_equal(.score_template(split_lines, tmpl)$score, 0)
  # a non-breaking space in the page text folds like an ordinary one
  expect_equal(.score_template(list(pages = "x Withdrawals Deposits Balance y"), tmpl)$score, 1)
  # a page whose text layer is not valid UTF-8 must not crash detection
  bad <- rawToChar(as.raw(c(0x57, 0x92, 0x20, 0x44)))
  Encoding(bad) <- "UTF-8"
  expect_silent(sc <- .score_template(list(pages = bad), tmpl))
  expect_equal(sc$score, 0)
})

test_that("a drafted PDF template detects its own source file (P1-48 round trip)", {
  # Regression for the whole loop an accountant actually walks: draft a template
  # from an unsupported PDF, save it, re-upload the same file. Before the fix 3 of
  # these 5 scored below their own min_score and came back "unsupported".
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdfs <- c("samples/raw/tutorial/sample_everyday_statement.pdf",
            "samples/raw/anz/anz_investmentfunds_statement_guide_sample.pdf",
            "tests/testthat/fixtures/anz_everyday_pdf_sample.pdf",
            "tests/testthat/fixtures/asb_everyday_pdf_sample.pdf",
            "tests/testthat/fixtures/westpac_everyday_pdf_sample.pdf")
  for (rel in pdfs) {
    f <- fixture(rel)
    skip_if_not(file.exists(f))
    tmpl <- draft_template(f, bank = "Draft Test")
    expect_identical(tmpl$format, "pdf", info = rel)
    expect_length(validate_template(tmpl), 0)             # savable as drafted
    det <- detect_statement(read_input(f), setNames(list(tmpl), tmpl$id))
    expect_true(det$matched,
      info = sprintf("%s could not detect its own source: %s", basename(rel), det$detail))
  }
})

test_that("a match reports its margin + runner-up over near-duplicate templates", {
  # aaa fingerprints on 2 phrases (both present -> score 2); bbb on 1 (score 1).
  # aaa wins by a THIN margin of 1, and bbb is the runner-up.
  mk <- function(id, need) list(
    id = id, bank = "X", statement_type = "e", format = "delimited", version = 1,
    min_score = 1, fingerprint = list(header_contains_all = need),
    delimiter = ",", columns = list(date = list(source = "Date"),
      amount = list(source = "Amount"), description = list(source = "Amount")),
    amount_sign = "signed", currency = "NZD")
  templates <- list(aaa = mk("aaa", c("Date", "Amount")), bbb = mk("bbb", "Date"))
  input <- list(kind = "delimited", path = "x.csv", sha256 = "s",
                lines = c("Date,Amount", "01/01/2020,1.00"))
  det <- detect_statement(input, templates)
  expect_true(det$matched)
  expect_identical(det$template_id, "aaa")
  expect_equal(det$margin, 1)
  expect_identical(det$runner_up, "bbb")
  expect_true(nrow(det$candidates) == 2)

  # a unique match (no eligible runner-up) has an infinite margin
  solo <- detect_statement(input, list(aaa = mk("aaa", c("Date", "Amount"))))
  expect_true(solo$matched)
  expect_true(is.infinite(solo$margin))
  expect_true(is.na(solo$runner_up))
})

# --- Ambiguity: a TIE and a NEW LAYOUT both fail, and need opposite advice -----
# When two templates fit a statement equally well the engine refuses to guess --
# correct. But the screen used to report that identically to "we have never seen
# this layout", which sends the analyst off to build a THIRD template that ties
# with the other two. The candidate frame now says which templates were genuine
# contenders (met their own min_score), so the two cases can be told apart.

.twin_dir <- function(min_score = 3, header = c("Tran Date", "Particulars", "Balance")) {
  d <- tempfile("twintpl_"); dir.create(d)
  for (v in c("a", "b")) writeLines(c(
    sprintf("id: twinbank_everyday_%s", v), "bank: TwinBank",
    "statement_type: everyday", "format: delimited", "version: 1",
    sprintf("min_score: %s", min_score),
    "fingerprint:",
    sprintf("  header_contains_all: [%s]", paste(sprintf('"%s"', header), collapse = ", ")),
    'delimiter: ","', "columns:",
    '  date: {source: "Tran Date", format: "%d/%m/%Y"}',
    "  amount: {source: Amount}", "  description: {source: Particulars}",
    "amount_sign: signed", "currency: NZD"), file.path(d, paste0(v, ".yaml")))
  d
}
.twin_csv <- function() {
  f <- tempfile("twin_", fileext = ".csv")
  writeLines(c("Tran Date,Particulars,Amount,Balance",
               "01/04/2025,COFFEE,-4.50,100.00",
               "02/04/2025,WAGES,2000.00,2100.00"), f)
  f
}

test_that("detection reports which templates were genuine contenders", {
  det <- detect_statement(read_input(.twin_csv()),
                          load_templates(.twin_dir(), strict = FALSE))
  expect_type(det$eligible_ids, "character")
  expect_setequal(det$eligible_ids, c("twinbank_everyday_a", "twinbank_everyday_b"))
})

test_that("two templates that fit equally well are reported as a TIE, not a new layout", {
  det <- detect_statement(read_input(.twin_csv()),
                          load_templates(.twin_dir(), strict = FALSE))
  expect_false(det$matched)                       # still fails closed
  expect_setequal(det$tied, c("twinbank_everyday_a", "twinbank_everyday_b"))
})

test_that("a genuinely unknown layout is NOT reported as a tie", {
  # min_score 3 over headings this file does not carry -> nothing eligible.
  det <- detect_statement(read_input(.twin_csv()),
                          load_templates(.twin_dir(header = c("Booking Ref", "Nights", "Room Rate")),
                                         strict = FALSE))
  expect_false(det$matched)
  expect_identical(det$tied, character(0))       # build a template IS right here
})

test_that("an unambiguous match is never called ambiguous", {
  det <- detect_statement(read_input(fixture("samples/raw/kiwibank/kiwibank_transaction_01.csv")),
                          load_templates(fixture("templates"), strict = FALSE))
  expect_true(det$matched)
  expect_identical(det$tied, character(0))
})

# A tie is a question only a maintainer can answer -- an accountant cannot know
# which internal variant a bank printed with. Stopping her to ask left her with no
# spreadsheet over a problem that was never hers, so the tool now decides and
# flags. It is not a guess: every figure still goes through reconciliation, and
# needs_review never reaches the dashboards.
test_that("a tie converts with the best template and is held for review, not refused", {
  f <- .twin_csv(); d <- .twin_dir()
  out <- tempfile("twinout_"); dir.create(out)
  res <- convert_statement(f, outdir = out, templates_dir = d, logdir = out)
  expect_gt(nrow(res$feed_rows), 0L)              # she gets her data
  expect_identical(res$status, "needs_review")    # ...and it is never called clean
  expect_true(isTRUE(res$detect$ambiguous))
  expect_setequal(res$detect$tied,
                  c("twinbank_everyday_a", "twinbank_everyday_b"))
  # the tie is stated in words, not buried
  expect_true(any(grepl("fit this statement equally well",
                        as.character(res$messages), fixed = TRUE)))
  # forcing one by hand still works, for whoever is fixing the duplicates
  res2 <- convert_statement(f, outdir = out, templates_dir = d, logdir = out,
                            force_template = "twinbank_everyday_b")
  expect_identical(res2$template_id, "twinbank_everyday_b")
  expect_gt(nrow(res2$feed_rows), 0L)
})

# A SAVE THAT TIES IS NOT A TYPO. recognition_summary used to lead with "Almost
# always the identifying phrase is not printed on the page exactly as typed" and
# relegate the actual cause to a parenthesis -- so on a tie the analyst was sent
# to retype a phrase that IS on the page (it is on the page for both templates),
# and the other reading of that advice is "make another one", which would tie too.
test_that("a save that ties with an existing template is diagnosed as a tie", {
  det <- detect_statement(read_input(.twin_csv()),
                          load_templates(.twin_dir(), strict = FALSE))
  r <- recognition_summary(det, "twinbank_everyday_a")
  expect_false(isTRUE(r$ok))
  expect_match(r$headline, "twinbank_everyday_b", fixed = TRUE)
  expect_match(r$headline, "exactly as well as yours", fixed = TRUE)
  # the wrong cure is gone...
  expect_false(grepl("not printed on the page exactly as typed", r$detail, fixed = TRUE))
  # ...and the charter's rule about a third template is stated out loud
  expect_match(r$detail, "Do not build a third", fixed = TRUE)
  # it still says the run is not lost
  expect_match(r$detail, "It still converts", fixed = TRUE)
})

test_that("a genuinely unrecognised save keeps the phrase advice", {
  det <- detect_statement(read_input(.twin_csv()),
                          load_templates(templates_dir(), strict = FALSE))
  expect_identical(det$tied, character(0))            # not a tie
  r <- recognition_summary(det, "twinbank_everyday_a")
  expect_false(isTRUE(r$ok))
  expect_match(r$detail, "not printed on the page exactly as typed", fixed = TRUE)
})

# The detection detail reaches the customer-facing Diagnostics table, where a
# template id and a score fraction are what the operational guide tells the
# analyst to report as a bug. Both facts are kept -- one for the log, one for her.
test_that("detection carries a plain twin of its closest-match detail", {
  det <- detect_statement(read_input(.twin_csv()),
                          load_templates(.twin_dir(header = c("Booking Ref", "Nights", "Room Rate")),
                                         strict = FALSE))
  expect_match(det$detail, "twinbank_everyday_a", fixed = TRUE)   # the log line, unchanged
  expect_match(det$detail, "score", fixed = TRUE)
  expect_false(grepl("twinbank_everyday_a", det$detail_plain, fixed = TRUE))
  expect_false(grepl("score", det$detail_plain, ignore.case = TRUE))
  expect_match(det$detail_plain, "TwinBank everyday statement", fixed = TRUE)
  expect_match(det$detail_plain, "Booking Ref", fixed = TRUE)
  # a MATCH has nothing customer-facing to say here, and says nothing
  ok <- detect_statement(read_input(fixture("samples/raw/kiwibank/kiwibank_transaction_01.csv")),
                         load_templates(templates_dir(), strict = FALSE))
  expect_true(ok$matched)
  expect_null(ok$detail_plain)
})

test_that("a tie is diagnosed as a tie, with the opposite advice to a new layout", {
  f <- .twin_csv(); d <- .twin_dir()
  out <- tempfile("twindiag_"); dir.create(out)
  res <- convert_statement(f, outdir = out, templates_dir = d, logdir = out)
  cats <- as.character(res$diagnostics$category %||% character(0))
  expect_true("ambiguous_template" %in% cats)
  # NOT the "go and build a template" diagnostic -- that advice would produce a
  # third template that ties with the other two.
  expect_false("unknown_format" %in% cats)
  fix <- as.character(res$diagnostics$how_to_fix[cats == "ambiguous_template"])
  # aimed at whoever can retire a duplicate, not at whoever just wanted a spreadsheet
  expect_true(grepl("near-duplicates", fix, fixed = TRUE))
  expect_false(grepl("Add a template for this layout", fix, fixed = TRUE))

  # A genuinely new layout keeps the old diagnostic and the old advice.
  d2 <- .twin_dir(header = c("Booking Ref", "Nights", "Room Rate"))
  res2 <- convert_statement(f, outdir = out, templates_dir = d2, logdir = out)
  cats2 <- as.character(res2$diagnostics$category %||% character(0))
  expect_true("unknown_format" %in% cats2)
  expect_false("ambiguous_template" %in% cats2)
})

# --- Shipped beats hand-built, and "matched but read nothing" ----------------

test_that("a shipped template beats a hand-built one on an equal score", {
  # Same fingerprint, and the hand-built one is FIRST alphabetically -- which is
  # what used to decide it. A template's filename must never pick which figures
  # reach a dashboard; the one with a golden test wins.
  dflt <- tempfile("tpl_d_"); dir.create(dflt)
  usr  <- tempfile("tpl_u_"); dir.create(usr)
  base <- readLines(fixture("templates/tutorial_everyday_pdf.yaml"))
  base <- base[!grepl("^sample: true", base)]
  writeLines(sub("^id: .*", "id: zzz_shipped_pdf", base), file.path(dflt, "z.yaml"))
  writeLines(sub("^id: .*", "id: aaa_handbuilt_pdf", base), file.path(usr, "a.yaml"))
  det <- detect_statement(read_input(fixture("samples/raw/tutorial/sample_everyday_statement.pdf")),
                          load_template_set(dflt, usr))
  expect_true(det$matched)
  expect_identical(det$template_id, "zzz_shipped_pdf")
  # and it is NOT reported as a tie -- shipped-over-hand-built is a real answer,
  # not a question for the analyst
  expect_identical(det$tied, character(0))
})

# A template can fingerprint perfectly and read NOTHING: detection matches text,
# it never parses. This came back from real use -- "it says yup it matches, but
# there are no transactions" -- and the result was needs_review, an amber notice
# stapled to an empty workbook.
.empty_tpl_dir <- function() {
  d <- tempfile("tpl_empty_"); dir.create(d)
  base <- readLines(fixture("templates/tutorial_everyday_pdf.yaml"))
  base <- base[!grepl("^sample: true", base)]
  base <- sub("^id: .*", "id: badbands_pdf", base)
  # right wording, columns squeezed into the left margin: a mis-drawn band
  base <- sub("date:.*x_min: 28.*", "date:        {x_min: 2,   x_max: 9}", base)
  base <- sub("description:.*x_min: 95.*", "description: {x_min: 9,   x_max: 16}", base)
  base <- sub("debit:.*x_min: 355.*", "debit:       {x_min: 16,  x_max: 20}", base)
  base <- sub("credit:.*x_min: 405.*", "credit:      {x_min: 20,  x_max: 24}", base)
  base <- sub("balance:.*x_min: 480.*", "balance:     {x_min: 24,  x_max: 28}", base)
  writeLines(base, file.path(d, "bad.yaml"))
  d
}

test_that("matched the wording but read no transactions is NOT 'needs review'", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  out <- tempfile("emptyout_"); dir.create(out)
  res <- convert_statement(fixture("samples/raw/tutorial/sample_everyday_statement.pdf"),
                           outdir = out, templates_dir = .empty_tpl_dir(), logdir = out)
  expect_identical(nrow(res$feed_rows %||% data.frame()), 0L)
  # An empty result is not a quality niggle to review -- there is nothing there.
  expect_identical(res$status, "unsupported")
  expect_false(identical(res$status, "needs_review"))
  cats <- as.character(res$diagnostics$category %||% character(0))
  expect_true("matched_but_empty" %in% cats)
  # and it names the template that failed, so the analyst can go and fix THAT
  expect_true(any(grepl("badbands_pdf", as.character(res$messages), fixed = TRUE)))
})

# The re-read that used to try another template here has been deleted. Swapping
# templates behind the analyst's back bought nothing the honest answer does not:
# a template that reads nothing (or reads less than it misses) now fails loudly
# and names itself, and she is one click from fixing it. One mechanism, visible.
test_that("a template that reads nothing says so instead of being swapped out", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  good <- tempfile("tpl_good_"); dir.create(good)
  base <- readLines(fixture("templates/tutorial_everyday_pdf.yaml"))
  base <- base[!grepl("^sample: true", base)]
  writeLines(sub("^id: .*", "id: zzz_working_pdf", base), file.path(good, "z.yaml"))
  out <- tempfile("noswap_"); dir.create(out)
  res <- convert_statement(fixture("samples/raw/tutorial/sample_everyday_statement.pdf"),
                           outdir = out, templates_dir = .empty_tpl_dir(),
                           user_templates_dir = good, logdir = out)
  # the broken template won the match and is NOT quietly replaced
  expect_identical(res$template_id, "badbands_pdf")
  expect_identical(res$status, "unsupported")
  expect_true("matched_but_empty" %in%
                as.character(res$diagnostics$category %||% character(0)))
  expect_true(any(grepl("badbands_pdf", as.character(res$messages), fixed = TRUE)))
})


# --- A correction of a shipped template must be able to take effect -----------
# Every template in production is shipped. When an accountant opened one to fix a
# moved column band, it saved as "<id>_custom" -- carrying the SAME fingerprint,
# so it tied on every statement of that bank, and the tie-break preferred the
# tested one. Her fix could never win, and the only advice on screen was to make
# her identifying phrase more specific, which was not why she lost. Four
# mechanisms producing an outcome none of them intended.
.refine_dirs <- function(refines = TRUE) {
  d <- tempfile("shipped_"); dir.create(d); u <- tempfile("hand_"); dir.create(u)
  b <- readLines(fixture("templates/tutorial_everyday_pdf.yaml"))
  b <- b[!grepl("^sample: true", b)]
  writeLines(b, file.path(d, "t.yaml"))
  w <- sub("^id: tutorial_everyday_pdf", "id: tutorial_everyday_pdf_custom", b)
  if (refines) w <- c(w, "refines: tutorial_everyday_pdf")
  writeLines(w, file.path(u, "w.yaml"))
  list(shipped = d, hand = u)
}

test_that("a template saved as a correction of a shipped one wins over it", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  p <- .refine_dirs()
  det <- detect_statement(read_input(fixture("samples/raw/tutorial/sample_everyday_statement.pdf")),
                          load_template_set(p$shipped, p$hand))
  expect_true(det$matched)
  expect_identical(det$template_id, "tutorial_everyday_pdf_custom")
  expect_identical(det$tied, character(0))   # a real answer, not a question
})

test_that("a hand-built RIVAL still loses to the tested template", {
  # The refinement rule must not become a general "user templates win" rule: only
  # a template that NAMES the one it corrects outranks it, and only that one.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  p <- .refine_dirs(refines = FALSE)
  det <- detect_statement(read_input(fixture("samples/raw/tutorial/sample_everyday_statement.pdf")),
                          load_template_set(p$shipped, p$hand))
  expect_identical(det$template_id, "tutorial_everyday_pdf")
})

test_that("the toolkit stamps what it was correcting", {
  src <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  i <- grep("tmpl\\$refines <- tmpl\\$id", src)
  expect_length(i, 1L)
  # ...and only when refining a SHIPPED template, never on a new one
  expect_match(paste(src[(i - 2):(i + 2)], collapse = " "),
               "g\\$default_ids")
})
