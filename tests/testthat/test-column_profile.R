# Column profiling (R/column_profile.R): the "everything you'd need to DRAFT a
# template" signal. Must be structurally correct (kinds, formats, mapping) and
# PII-safe (masked shapes, no raw values leaking out).

.cp_df <- function() data.frame(
  Date    = c("31/12/2025", "01/01/2026", "15/02/2026"),
  Details = c("COUNTDOWN QUEEN ST", "SALARY ACME LTD", "TFR TO 12-3456-7890123-00"),
  Amount  = c("-45.20", "2,000.00", "-12.00"),
  `D/C`   = c("D", "C", "D"),
  Balance = c("1,000.00", "3,000.00", "2,988.00"),
  check.names = FALSE, stringsAsFactors = FALSE)

test_that("column kinds are inferred correctly", {
  profs <- column_profiles(.cp_df())
  kind <- stats::setNames(vapply(profs, function(p) p$kind, character(1)),
                          vapply(profs, function(p) p$name, character(1)))
  expect_identical(kind[["Date"]], "date")
  expect_identical(kind[["Amount"]], "money")
  expect_identical(kind[["D/C"]], "indicator")
  expect_identical(kind[["Balance"]], "money")
  expect_identical(kind[["Details"]], "text")
})

test_that("date columns carry a detected strptime format", {
  profs <- column_profiles(.cp_df())
  dp <- Filter(function(p) p$name == "Date", profs)[[1]]
  expect_identical(dp$date_format, "%d/%m/%Y")
})

test_that("money columns carry the style facts a template needs", {
  profs <- column_profiles(.cp_df())
  amt <- Filter(function(p) p$name == "Amount", profs)[[1]]
  expect_identical(amt$money$decimal_mark, "dot")
  expect_true(amt$money$thousands_sep)
  expect_true(amt$money$minus_negative)
  expect_false(amt$money$currency_symbol)
})

test_that("indicator columns expose their short distinct tokens", {
  profs <- column_profiles(.cp_df())
  dc <- Filter(function(p) p$name == "D/C", profs)[[1]]
  expect_setequal(unlist(dc$tokens), c("D", "C"))
})

test_that("example shapes are masked -- no real digits or letters leak", {
  profs <- column_profiles(.cp_df())
  det <- Filter(function(p) p$name == "Details", profs)[[1]]
  # The account-like token becomes a SHAPE, not the number: every digit -> 9 and
  # every letter -> A, so no original digit or letter survives.
  expect_false(grepl("[0-8B-Zb-z]", det$example_shape))
  expect_false(grepl("COUNTDOWN|SALARY|QUEEN|ACME", det$example_shape))
  # A date shape masks the same way.
  dp <- Filter(function(p) p$name == "Date", profs)[[1]]
  expect_identical(dp$example_shape, "99/99/9999")
})

test_that("suggested mapping is the engine's own best guess", {
  sm <- .suggest_mapping(.cp_df())
  expect_identical(sm$date, "Date")
  expect_identical(sm$date_format, "%d/%m/%Y")
  expect_identical(sm$amount_style, "type_dc")
  expect_identical(sm$type_debit_value, "D")
  expect_identical(sm$type_credit_value, "C")
  expect_identical(sm$fields$description, "Details")
  expect_identical(sm$fields$balance, "Balance")
})

test_that("template_hints works from a delimited input (no template)", {
  lines <- c("Date,Details,Amount,Type,Balance",
             "31/12/2025,COUNTDOWN QUEEN ST,-45.20,D,1000.00",
             "01/01/2026,SALARY,2000.00,C,3000.00")
  th <- template_hints(list(kind = "delimited", lines = lines), NULL, FALSE)
  expect_identical(th$kind, "delimited")
  expect_identical(th$delimiter, ",")
  expect_identical(th$suggested_mapping$amount_style, "type_dc")
  expect_true(length(th$columns) == 5L)
})

test_that("template_hints is null for an empty / unknown input", {
  expect_null(template_hints(list(kind = "delimited", lines = character(0)), NULL, FALSE))
  expect_null(template_hints(list(kind = "other"), NULL, FALSE))
})

test_that("capture_metadata embeds template_hints at full level only", {
  df <- .cp_df()
  ctx <- list(run_id = "r", ts = "2026-01-01T00:00:00Z", requested_by = "u",
              sha = "x", input = list(kind = "excel", table = df, path = "s.xlsx"),
              parsed = list(transactions = data.frame(), header = list()),
              det = list(matched = FALSE), status = "unsupported")
  full <- capture_metadata(ctx, .config_defaults())
  expect_true(!is.null(full$template_hints))
  expect_identical(full$template_hints$suggested_mapping$amount_style, "type_dc")

  cfg_std <- .config_defaults(); cfg_std$metadata$level <- "standard"
  std <- capture_metadata(ctx, cfg_std)
  expect_null(std$template_hints)          # full-only detail
})
