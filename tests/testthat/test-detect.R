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
