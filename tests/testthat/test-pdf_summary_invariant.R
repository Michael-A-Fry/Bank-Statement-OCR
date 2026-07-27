# THE INVARIANT (N30): no row a PDF template KEEPS may read as a summary line.
#
# An opening/closing balance, a carried-forward or a total is not a transaction.
# When one is kept anyway the output does not merely lose data -- it GAINS a
# transaction that never happened, at the balance's own (material) amount, with the
# figure landing in whichever money band the row's words fell into. That was
# reported from real use as "Balance -9000 debit", and nothing in the suite noticed:
# every shipped golden was compared row-for-row against a snapshot, but no test ever
# asked whether the rows in that snapshot were transactions at all.
#
# The invariant is asserted on BOTH the description cell and the whole raw line,
# because that is what .pdf_is_summary now reads: the bug was that a DISPLACED band
# made the description non-empty but WRONG, so the raw line -- which said "Opening
# balance" all along -- was never consulted.
#
# SCOPE: PDF templates only. .PDF_SUMMARY_LABELS is the PDF reader's vocabulary and
# no other reader applies it. The delimited/Excel goldens (xero_standard_csv,
# excel_generic_xlsx) legitimately carry an "Opening balance" row, because there it
# is a real row of the source file with its own amount, not a printed summary line
# the reader had to recognise. Widening this test to them would assert a rule those
# readers do not have.

# ---------------------------------------------------------------------------
# 1. THE NET: every shipped PDF fixture, parsed for real.
#
# On today's fixtures this is a REGRESSION net, not the proof: their summary lines
# are already excluded by other conditions too (the Westpac fixture's OPENING
# BALANCE line carries a balance but no debit/credit amount, so it fails the
# has-amount test as well). Deleting the summary guard alone does not trip it. The
# case with teeth is section 2 -- but the net is what catches the NEXT bank, whose
# summary line does carry a money-column amount, on the day its template ships.
# ---------------------------------------------------------------------------
PDF_SUMMARY_FIXTURES <- list(
  list(id = "anz_everyday_pdf",        fx = "tests/testthat/fixtures/anz_everyday_pdf_sample.pdf"),
  list(id = "anz_everyday_pdf",        fx = "tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf"),
  list(id = "asb_everyday_pdf",        fx = "tests/testthat/fixtures/asb_everyday_pdf_sample.pdf"),
  list(id = "westpac_everyday_pdf",    fx = "tests/testthat/fixtures/westpac_everyday_pdf_sample.pdf"),
  list(id = "tutorial_everyday_pdf",   fx = "samples/raw/tutorial/sample_everyday_statement.pdf"),
  list(id = "anz_investmentfunds_pdf", fx = "samples/raw/anz/anz_investmentfunds_statement_guide_sample.pdf")
)

for (.g in PDF_SUMMARY_FIXTURES) local({
  g <- .g
  test_that(sprintf("%s keeps no summary line as a transaction (%s)",
                    g$id, basename(g$fx)), {
    skip_if_not(requireNamespace("pdftools", quietly = TRUE))
    skip_if_not(file.exists(fixture(g$fx)))
    res <- parse_fixture(g$fx)
    tx <- res$parsed$transactions
    raw <- res$parsed$provenance$raw
    expect_gt(nrow(tx), 0)            # an empty parse would satisfy this vacuously
    bad <- vapply(seq_len(nrow(tx)),
                  function(i) .pdf_is_summary(tx$description[i], raw[i]), logical(1))
    expect_identical(which(bad), integer(0),
      info = sprintf("%s kept summary line(s): %s", g$fx,
                     paste(raw[bad], collapse = " // ")))
  })
})

# The guard: a new PDF template cannot be shipped without being subjected to the
# invariant. Same shape as the golden-snapshot guard in test-pdf_template_goldens.R
# -- if this fails, add the template and its fixture above; do not delete the test.
test_that("every shipped PDF template is covered by the summary-line invariant", {
  tdir <- templates_dir()
  skip_if_not(dir.exists(tdir))
  ids <- sub("\\.ya?ml$", "", basename(list.files(tdir, pattern = "\\.ya?ml$")))
  is_pdf <- vapply(ids, function(id) {
    tpl <- tryCatch(yaml::yaml.load_file(file.path(tdir, paste0(id, ".yaml"))),
                    error = function(e) NULL)
    identical(tpl$format %||% "delimited", "pdf")
  }, logical(1))
  covered <- unique(vapply(PDF_SUMMARY_FIXTURES, function(g) g$id, character(1)))
  expect_identical(setdiff(ids[is_pdf], covered), character(0))
})

# ---------------------------------------------------------------------------
# 2. THE CASE WITH TEETH: the N30 chain, reproduced.
#
# A dated opening-balance line, printed with its figure in a money column, and the
# DESCRIPTION BAND DISPLACED so it lands on the number instead of the words. The
# description then comes back non-empty and WRONG ("9000.00"), which is precisely
# what stopped the old guard falling back to `raw`. Every other layer says
# "transaction": the date parses, the money band has a figure. Only the summary
# guard stands between this statement and a fabricated 9,000 debit.
# ---------------------------------------------------------------------------

# .disp_input(style) -- one real transaction and one opening-balance line, both
# dated, both carrying a figure in the money column(s).
.disp_input <- function() {
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05", "Jan", "COFFEE",  "4.50",    "95.50",       # a real transaction
              "01", "Jan", "Opening", "balance", "9000.00", "9000.00"),
    x     = c(45,   60,    110,       415,       490,
              45,   60,    110,       170,       415,       490),
    y     = c(40,   40,    40,        40,        40,
              70,   70,    70,        70,        70,        70),
    width = c(12,   16,    45,        25,        30,
              12,   16,    50,        45,        34,        34),
    height = rep(10, 11))
  list(kind = "pdf", path = tempfile(fileext = ".pdf"),
       pages = "Statement period 1 Jan 2026 to 31 Jan 2026",
       words = list(words), meta = list(page_count = 1L))
}

# The description band sits over the MONEY column, not over the words -- exactly the
# displacement N29 describes. cols$description catches "9000.00".
.disp_tmpl <- function(style = "signed") {
  cols <- list(date = list(x_min = 40, x_max = 74),
               description = list(x_min = 400, x_max = 470),   # DISPLACED
               balance = list(x_min = 470, x_max = 545))
  if (identical(style, "debit_credit_cols")) {
    cols$debit  <- list(x_min = 400, x_max = 470)   # the balance figure lands HERE
    cols$credit <- list(x_min = 360, x_max = 400)
  } else cols$amount <- list(x_min = 400, x_max = 470)
  list(id = "disp", bank = "S", statement_type = "e", format = "pdf", version = 1,
       currency = "NZD",
       table = list(row_tol = 3, date_format = "%d %b", amount_sign = style,
                    columns = cols))
}

test_that("a displaced description band cannot turn an opening balance into a transaction", {
  parsed <- parse_pdf_table(.disp_input(), .disp_tmpl("signed"))
  tx <- parsed$transactions
  # The displaced band really did capture the wrong words -- if it did not, this
  # test would be proving nothing.
  expect_true(.pdf_is_summary(NULL, "01 Jan Opening balance 9000.00 9000.00"))
  expect_false(.pdf_is_summary("9000.00"))     # the description alone says nothing

  expect_equal(nrow(tx), 1L)                   # ONLY the real transaction
  expect_equal(tx$date, "2026-01-05")
  expect_false(any(abs(tx$amount) == 9000))    # no fabricated 9,000 anywhere
  # ...and the row that went is REPORTED, not silently dropped: skipped_row_count
  # is what the completeness check states, and the X-ray names the reason.
  expect_equal(parsed$skipped_row_count, 1L)
  expect_equal(parsed$actionable_skip_count, 0L)   # a summary line is not a fault
})

test_that("the displaced balance does not become '-9000 debit' (the reported symptom)", {
  # Two money columns: the displaced description band and the DEBIT band sit on the
  # same spot, so the balance figure lands in the debit column -- word for word the
  # bug as reported.
  tx <- parse_pdf_table(.disp_input(), .disp_tmpl("debit_credit_cols"))$transactions
  expect_equal(nrow(tx), 1L)
  expect_false(any(tx$amount == -9000 & tx$direction == "debit"))
})

test_that("a real transaction whose description mentions a balance is KEPT", {
  # The other direction, and the reason the labels must match WHOLE. Reading the raw
  # line is only safe while "TFR TO SAVINGS - BALANCE" stays a transaction; if this
  # ever fails, the fix has started eating real rows and money is going missing.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05", "Jan", "TFR", "TO", "SAVINGS", "-", "BALANCE", "250.00", "95.50"),
    x     = c(45,   60,    110,   140,  165,       215, 225,       415,      490),
    y     = rep(40, 9), width = c(12, 16, 25, 20, 45, 5, 45, 30, 30),
    height = rep(10, 9))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = "Statement period 1 Jan 2026 to 31 Jan 2026",
    words = list(words), meta = list(page_count = 1L))
  tmpl <- .disp_tmpl("signed")
  tmpl$table$columns$description <- list(x_min = 74, x_max = 400)   # correctly placed
  tx <- parse_pdf_table(input, tmpl)$transactions
  expect_equal(nrow(tx), 1L)
  expect_equal(tx$description, "TFR TO SAVINGS - BALANCE")   # verbatim, and intact
  expect_equal(abs(tx$amount), 250)
})
