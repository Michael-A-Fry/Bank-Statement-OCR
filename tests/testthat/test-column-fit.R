# Tests for R/column_fit.R -- "a column of your template is not where the statement
# puts it, and here is which one".
#
# THE DRIFT IS APPLIED TO THE TEMPLATE, NOT THE PAGE, and that is deliberate: moving a
# band 55pt right is geometrically identical to the bank moving its amounts 55pt left,
# and it adds no binary fixture to a repository that gets hand-carried to an offline
# server. One real bank-shaped PDF, four template geometries.

SAMPLE_PDF <- "tests/testthat/fixtures/anz_everyday_pdf_sample.pdf"

# shift_bands(tpl, dx, cols) -- move named columns dx points right. A POSITIVE dx is
# the template looking too far right, i.e. the statement's figures are to its LEFT.
shift_bands <- function(tpl, dx, cols = c("debit", "credit", "balance")) {
  for (nm in cols) {
    tpl$table$columns[[nm]]$x_min <- tpl$table$columns[[nm]]$x_min + dx
    tpl$table$columns[[nm]]$x_max <- tpl$table$columns[[nm]]$x_max + dx
  }
  tpl
}

cf_setup <- function() {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(file.exists(fixture(SAMPLE_PDF)))
  tp <- load_templates(templates_dir())
  list(template = tp[["anz_everyday_pdf"]], input = read_input(fixture(SAMPLE_PDF)))
}

# ---------------------------------------------------------------------------
# A STATEMENT THAT READS PERFECTLY MUST BE MET WITH SILENCE.
#
# This is the test that matters most. A drift detector that fires on a flawless
# statement teaches an analyst to ignore the one that matters, which is worse than
# having no detector at all. Three earlier versions of this code failed it: one scored
# the heading and balance rows as data, one counted the statement-period line as a
# transaction, and one preferred an offset that read SIX MORE amounts than the perfect
# declared position -- because the wider reach had swallowed reference numbers out of
# the description. "Reads more" is not "reads better".
test_that("a clean statement on its own template reports nothing at all", {
  s <- cf_setup()
  fit <- column_fit(s$input, s$template)
  expect_gte(fit$rows, .CFIT_MIN_ROWS)
  expect_true(all(fit$columns$verdict == "fits"))
  expect_null(fit$shift)
  expect_identical(fit$strays, 0L)
  expect_true(is.na(column_fit_note(fit)))
})

test_that("only transaction rows are scored -- not headings, totals or the period line", {
  s <- cf_setup()
  # The statement-period line ("Statement period 1 Feb 2026 to 12 Mar 2026") has dates
  # AND numbers, and 2026 parses as money, so it was being counted as a transaction
  # whose date band held the word "Statement". The data-row count must equal the
  # transaction count the reader itself finds, exactly.
  expect_identical(column_fit(s$input, s$template)$rows,
                   nrow(parse_statement(s$input, s$template)$transactions))
})

test_that(".cfit_amountish accepts amounts and rejects years and reference numbers", {
  expect_true(.cfit_amountish("491.09"))
  expect_true(.cfit_amountish("8,129.78"))
  expect_true(.cfit_amountish("1,234.56"))
  # a bare year is the regression above; a reference number is the same shape
  expect_false(.cfit_amountish("2026"))
  expect_false(.cfit_amountish("88412-00"))
  expect_false(.cfit_amountish("RIVERSIDE"))
  expect_false(.cfit_amountish(""))
})

# ---------------------------------------------------------------------------
# THE WHOLE TABLE HAS MOVED
test_that("money bands 55pt off are reported, with an interval that contains the truth", {
  s <- cf_setup()
  fit <- column_fit(s$input, shift_bands(s$template, 55))
  expect_false(is.null(fit$shift))
  sh <- fit$shift
  # the statement's figures are to the LEFT of where this template looks
  expect_lt(sh$dx, 0)
  # THE INTERVAL IS THE HONEST ANSWER and the recommendation is its centre. An amount
  # is a point inside a 65pt band, so every offset in a wide interval reads it and no
  # single one of them is "the" offset (see the header of R/column_fit.R). What can be
  # asserted is that the interval brackets the drift actually applied, and that the
  # recommendation lies inside it.
  expect_gte(sh$dx, sh$lo)
  expect_lte(sh$dx, sh$hi)
  expect_lte(sh$lo, -55)
  expect_gte(sh$hi, -55)
  # and it is an improvement, not a lateral move
  expect_gt(sh$ok, sh$base_ok)
  expect_identical(sh$ok, sh$seen)      # reads everything it touches
  expect_match(column_fit_note(fit), "to the left")
})

test_that("the offset search is skipped when the declared bands already read cleanly", {
  s <- cf_setup()
  # 10pt is inside the slack a 65pt band has, so nothing is wrong and nothing is said.
  # Measured in R/column_fit.R: 25pt of real drift did not break the reader either.
  expect_null(column_fit(s$input, shift_bands(s$template, 10))$shift)
})

# ---------------------------------------------------------------------------
# ONE COLUMN HAS MOVED, which no single page-wide offset can fix -- so none is
# offered, and the column is named instead. That is the honest outcome, not a gap.
test_that("a single moved column is named without inventing a page-wide offset", {
  s <- cf_setup()
  fit <- column_fit(s$input, shift_bands(s$template, 55, "balance"))
  bal <- fit$columns[fit$columns$column == "balance", ]
  expect_identical(bal$verdict, "empty")
  expect_null(fit$shift)
  # the other two columns still read perfectly, so the analyst is told exactly one
  # thing to look at
  expect_true(all(fit$columns$verdict[fit$columns$column %in% c("debit", "credit")] == "fits"))
  expect_match(column_fit_note(fit), "balance is empty on every row")
})

# ---------------------------------------------------------------------------
# THE DISCRIMINATOR. "balance is empty on every row" means either that this bank does
# not print a running balance -- nothing to fix -- or that the column moved. The
# columns alone cannot tell those apart; an amount outside every declared band can.
test_that("amounts no money column covers separate a moved column from an absent one", {
  s <- cf_setup()
  moved <- column_fit(s$input, shift_bands(s$template, 55, "balance"))
  expect_gte(moved$strays, .CFIT_MIN_ROWS)
  expect_match(column_fit_note(moved), "no amount column of the template covers")
  # ...and on the untouched template every amount is inside a band
  expect_identical(column_fit(s$input, s$template)$strays, 0L)
})

# ---------------------------------------------------------------------------
# REFUSALS
test_that("column_fit refuses rather than guesses when it cannot know", {
  s <- cf_setup()
  # not a PDF template: there are no x-bands to be wrong about
  csv <- s$template; csv$format <- "delimited"
  expect_identical(column_fit(s$input, csv)$rows, 0L)
  expect_true(is.na(column_fit_note(column_fit(s$input, csv))))
  # no words at all
  expect_identical(column_fit(list(words = list()), s$template)$rows, 0L)
  # NOT ENOUGH EVIDENCE IS NOT A FINDING: too few transaction rows to support a verdict
  one <- s$input
  one$words <- list(utils::head(s$input$words[[1]], 40))
  expect_lt(column_fit(one, s$template)$rows, .CFIT_MIN_ROWS)
  expect_true(is.na(column_fit_note(column_fit(one, s$template))))
  # ...and a template that declares no date format cannot tell a transaction row from
  # a heading, so it says nothing rather than scoring the headings
  nofmt <- s$template; nofmt$table$date_format <- NULL
  expect_identical(column_fit(s$input, nofmt)$rows, 0L)
  # a 0-row fit, and junk, must not throw
  expect_true(is.na(column_fit_note(list())))
  expect_true(is.na(column_fit_note(NULL)))
})

test_that("a column whose band is impossible is skipped, not crashed on", {
  s <- cf_setup()
  bad <- s$template
  bad$table$columns$credit$x_min <- NA
  expect_false("credit" %in% column_fit(s$input, bad)$columns$column)
  expect_true("debit" %in% column_fit(s$input, bad)$columns$column)
})

# ---------------------------------------------------------------------------
# IT REACHES A PERSON. A finding nothing surfaces is not a finding.
test_that("a drifted template raises the column_bands diagnostic, a clean one does not", {
  s <- cf_setup()
  meta <- .column_fit_note_meta(s$input, shift_bands(s$template, 55))
  expect_true(nzchar(meta$column_fit_note))
  expect_identical(meta$column_fit_severity, "medium")
  d <- build_diagnostics("needs_review", metadata = meta)
  expect_true("column_bands" %in% d$category)
  expect_identical(d$severity[d$category == "column_bands"], "medium")
  expect_true(nzchar(d$how_to_fix[d$category == "column_bands"]))

  # clean: no metadata, so no diagnostic
  expect_identical(length(.column_fit_note_meta(s$input, s$template)), 0L)
  expect_false("column_bands" %in% build_diagnostics("ok", metadata = list())$category)
})

test_that("a column the statement simply does not print is information, not a fault", {
  s <- cf_setup()
  # strip the balance band off the page's right-hand edge entirely, so the column is
  # empty AND no amount is left uncovered -- the shape of a bank that prints no running
  # balance. Nothing to fix, but worth recording: the balance checks had nothing to
  # test, and that is the strongest check the engine has.
  # Done by removing the balance figures from the PAGE, not by bending the bands:
  # that is what a statement with no running-balance column actually looks like, and
  # it keeps the real template geometry under test.
  no_bal <- s$input
  no_bal$words <- lapply(s$input$words, function(w)
    w[w$x + w$width / 2 < s$template$table$columns$balance$x_min, , drop = FALSE])
  wide <- s$template
  fit <- column_fit(no_bal, wide)
  expect_identical(fit$columns$verdict[fit$columns$column == "balance"], "empty")
  expect_identical(fit$strays, 0L)
  meta <- .column_fit_note_meta(no_bal, wide)
  expect_identical(meta$column_fit_severity, "info")
  expect_match(build_diagnostics("ok", metadata = meta)$how_to_fix[
    build_diagnostics("ok", metadata = meta)$category == "column_bands"], "Nothing to fix")
})

test_that("a page the wrong way round is left to page_orientation, which outranks this", {
  s <- cf_setup()
  # A landscape page is refused a band frame at all, so every band would read nonsense
  # and this would report all five columns as broken: true, useless, and drowning the
  # one diagnostic that names the real cause.
  land <- s$template
  land$table$page <- list(width = 842, height = 595)
  expect_identical(length(.column_fit_note_meta(s$input, land)), 0L)
})

test_that("column_bands is owned by whoever can edit the template", {
  # DIAG_PLAIN (ui_labels.R) is checked against .DIAG_FIX_OWNER by test-seams.R, which
  # reads both from disk -- ui_labels.R is not sourced into the test environment, so it
  # cannot be named here.
  expect_identical(unname(.diag_fix_owner("column_bands")), "template")
  expect_true("column_bands" %in% names(.DIAG_FIX_OWNER))
})
