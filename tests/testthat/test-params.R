# Engine tuning parameters (R/params.R) and the two shared helpers. These pin the
# consolidated behaviour so the single .plausible_year / .tolerant_date pair can
# never silently drift from the values every consumer relies on.

test_that("tuning constants hold their shipped values", {
  expect_identical(PARAM_YEAR_MIN, 1990L)
  expect_identical(PARAM_YEAR_MAX, 2100L)
  expect_identical(PARAM_MONEY_TOL, 0.005)
  expect_identical(PARAM_OCR_MIN_CHARS, 20L)
  expect_identical(PARAM_OCR_MIN_WORDS, 3L)
  expect_identical(PARAM_OCR_MAX_BAD_RATIO, 0.30)
  expect_identical(PARAM_OCR_CELL_MIN_CONF, 60)
  expect_identical(PARAM_OCR_PAGE_MIN_CONF, 70)
  expect_identical(PARAM_OCR_RENDER_DPI, 300L)
  expect_identical(PARAM_PDF_ROW_TOL, 3L)
  expect_identical(PARAM_STATED_COUNT_MAX, 100000L)
  expect_identical(PARAM_MAX_PAGES, 100L)
  expect_identical(PARAM_MAX_PAGE_PT, 2880)
  expect_identical(PARAM_REDACT_DARK_LEVEL, 60L)
  expect_identical(PARAM_REDACT_OCC_THRESH, 0.70)
  expect_identical(PARAM_REDACT_VECTOR_DPI, 100L)
})

test_that(".plausible_year accepts the trusted window and rejects outside it", {
  expect_true(.plausible_year(1990))
  expect_true(.plausible_year(2026))
  expect_true(.plausible_year(2100))
  expect_false(.plausible_year(1989))
  expect_false(.plausible_year(2101))
  expect_false(.plausible_year(25))      # a 2-digit year read as 4-digit
  expect_false(.plausible_year(NA))
  # vectorised, NA-safe
  expect_identical(.plausible_year(c(2000L, 25L, NA, 2200L)),
                   c(TRUE, FALSE, FALSE, FALSE))
})

test_that(".tolerant_date parses every declared statement date shape", {
  expect_equal(.tolerant_date("2026-01-15"), as.Date("2026-01-15"))
  expect_equal(.tolerant_date("15/01/2026"), as.Date("2026-01-15"))
  expect_equal(.tolerant_date("15-01-2026"), as.Date("2026-01-15"))
  expect_equal(.tolerant_date("15 Jan 2026"), as.Date("2026-01-15"))
  expect_equal(.tolerant_date("15 January 2026"), as.Date("2026-01-15"))
  expect_equal(.tolerant_date("15/01/26"), as.Date("2026-01-15"))
  # the dashed 2-digit-year form the shared parser must keep (reconcile always had
  # it; the consolidation brought diagnose into line -- this pins it so the list
  # cannot silently shrink again).
  expect_equal(.tolerant_date("01-05-26"), as.Date("2026-05-01"))
  expect_equal(.tolerant_date("15 Jan 26"), as.Date("2026-01-15"))
})

test_that(".tolerant_date returns NA on the unparseable / untrusted, never a guess", {
  expect_true(is.na(.tolerant_date("garbage")))
  expect_true(is.na(.tolerant_date("")))
  expect_true(is.na(.tolerant_date(NA)))
  expect_true(is.na(.tolerant_date(NULL)))
  expect_true(is.na(.tolerant_date(character(0))))
  # a whitespace-padded bound is tolerated (trimmed), not rejected
  expect_equal(.tolerant_date("  2026-01-15  "), as.Date("2026-01-15"))
})
