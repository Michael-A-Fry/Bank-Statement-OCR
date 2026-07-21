# Tests for the mode:fields orchestration (R/forms.R): load, detect, convert.

test_that("load_fields_templates returns only mode:fields templates", {
  ft <- load_fields_templates(file.path(engine_root(), "fields_templates"), NULL)
  expect_true("anz_kiwisaver_fields" %in% names(ft))
  expect_true(all(vapply(ft, is_fields_template, logical(1))))
})

test_that("detect_form matches on identifying phrases, else unsupported", {
  ft <- load_fields_templates(file.path(engine_root(), "fields_templates"), NULL)
  hit <- list(pages = "ANZ KiwiSaver Scheme\nOpening balance $1.00\nClosing balance $2.00")
  d <- detect_form(hit, ft)
  expect_true(d$matched)
  expect_equal(d$template_id, "anz_kiwisaver_fields")

  miss <- list(pages = "A completely unrelated document with no identifying phrases")
  expect_false(detect_form(miss, ft)$matched)

  expect_false(detect_form(hit, list())$matched)   # no templates installed
})

test_that("detect_form reports UNMATCHED on an equal-specificity tie (never guesses)", {
  mk <- function(id) list(id = id, mode = "fields", format = "pdf",
    fingerprint = list(page_contains_all = c("IR3", "Tax year")),
    fields = list(a = "A"))
  ft <- list(ird_a = mk("ird_a"), ird_b = mk("ird_b"))
  page <- list(pages = "IR3 return for the 2025 Tax year")
  d <- detect_form(page, ft)
  expect_false(d$matched)                       # two equally-specific -> ask, don't guess
  expect_true(grepl("equally", d$detail))
  # ...and convert_form must therefore NOT silently extract with a guessed template
  od <- tempfile(); on.exit(unlink(od, recursive = TRUE), add = TRUE)
  # (no real file needed: detect happens on text; use the sample dir with a tie)
  # a UNIQUE best still matches
  ft2 <- list(ird_a = mk("ird_a"),
              ird_b = list(id = "ird_b", mode = "fields", format = "pdf",
                fingerprint = list(page_contains_all = "IR3"), fields = list(a = "A")))
  expect_true(detect_form(page, ft2)$matched)
  expect_equal(detect_form(page, ft2)$template_id, "ird_a")  # more specific wins
})

test_that("extract + write_form_outputs produce the labelled values", {
  ft <- load_fields_templates(file.path(engine_root(), "fields_templates"), NULL)
  input <- list(pages = paste(
    "ANZ KiwiSaver Scheme",
    "Opening balance   $51,904.55",
    "Closing balance   $61,060.94",
    "Government contribution   $521.43", sep = "\n"))
  f <- extract_fields(input, ft[["anz_kiwisaver_fields"]])
  expect_equal(f$value[f$field == "opening_balance"], "$51,904.55")
  expect_equal(f$value[f$field == "government_contribution"], "$521.43")

  od <- tempfile(); on.exit(unlink(od, recursive = TRUE), add = TRUE)
  paths <- write_form_outputs(f, od, "ks", c("csv", "json"))
  expect_true(all(file.exists(paths)))
  csv <- utils::read.csv(paths[grepl("\\.csv$", paths)], stringsAsFactors = FALSE)
  expect_true("value" %in% names(csv))
})

test_that("validate_fields_template flags a non-fields or empty template", {
  expect_length(validate_fields_template(
    list(id = "x", mode = "fields", fields = list(a = "A"))), 0)
  expect_true(length(validate_fields_template(list(id = "x", mode = "fields"))) > 0)
  expect_true(length(validate_fields_template(list(id = "x", fields = list(a = "A")))) > 0)
})

test_that("convert_form on the sample PDF extracts fields (skips if absent)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  pdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(pdf))
  od <- tempfile(); on.exit(unlink(od, recursive = TRUE), add = TRUE)
  res <- convert_form(pdf, fields_dir = file.path(engine_root(), "fields_templates"),
                      outdir = od, formats = c("csv"))
  expect_equal(res$status, "ok")
  expect_equal(res$template_id, "anz_kiwisaver_fields")
  expect_true(res$n_fields >= 4)
  expect_true(any(file.exists(res$outputs)))
})
