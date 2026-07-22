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
