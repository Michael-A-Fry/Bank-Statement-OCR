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
  ok <- list(id = "x", mode = "fields", fields = list(a = "A"),
             fingerprint = list(page_contains_all = c("Kowhai KiwiSaver Scheme",
                                                      "Annual member statement")))
  expect_length(validate_fields_template(ok), 0)
  expect_true(length(validate_fields_template(list(id = "x", mode = "fields"))) > 0)
  expect_true(length(validate_fields_template(list(id = "x", fields = list(a = "A")))) > 0)
})

# ---------------------------------------------------------------------------
# F1-9: a form template's fingerprint faces the same distinctiveness gate as a
# statement template's. detect_form is reached for EVERY statement no transaction
# template recognised, and there is no reconciliation behind it -- so a loose
# fingerprint turns a correct "unsupported" verdict into a confident extraction.
# ---------------------------------------------------------------------------

test_that("a form template with no fingerprint, or a generic one, is invalid", {
  base <- list(id = "loose", mode = "fields", format = "pdf", fields = list(a = "A"))
  no_fp <- validate_fields_template(base)
  expect_true(length(no_fp) > 0)
  expect_match(paste(no_fp, collapse = " "), "fingerprint", fixed = TRUE)

  generic <- base; generic$fingerprint <- list(page_contains_all = c("Balance", "Date"))
  expect_true(length(validate_fields_template(generic)) > 0)   # every word is universal

  # ...and a real, distinctive one passes.
  good <- base; good$fingerprint <- list(page_contains_all = c("Balance",
                                                              "Kowhai Bank KiwiSaver"))
  expect_length(validate_fields_template(good), 0)
})

test_that("a form template that names a PERSON is rejected (no PII fingerprints)", {
  t <- list(id = "pii", mode = "fields", format = "pdf", fields = list(a = "A"),
            fingerprint = list(page_contains_all = c("Mr John Smith", "Annual statement")))
  probs <- validate_fields_template(t)
  expect_true(any(grepl("PERSON", probs)))
})

test_that("load_fields_templates skips an invalid template and SAYS which (never silent)", {
  d <- tempfile("ft_"); dir.create(d)
  yaml::write_yaml(list(id = "good_form", mode = "fields", format = "pdf",
    fingerprint = list(page_contains_all = c("Kowhai Bank KiwiSaver",
                                             "Annual member statement")),
    fields = list(opening_balance = "Opening balance")), file.path(d, "good.yaml"))
  yaml::write_yaml(list(id = "loose_form", mode = "fields", format = "pdf",
    fingerprint = list(page_contains_all = c("Balance")),
    fields = list(opening_balance = "Opening balance")), file.path(d, "loose.yaml"))

  ft <- load_fields_templates(d, NULL)
  expect_identical(names(ft), "good_form")          # the loose one cannot match anything
  errs <- attr(ft, "load_errors")
  expect_length(errs, 1)
  expect_match(errs, "loose_form")                  # named, so it can be fixed

  # the loose template can no longer hijack an unrecognised bank statement.
  page <- list(pages = "Kowhai Bank statement\nBalance 100.00\nDate 01/01/2026")
  expect_false(detect_form(page, ft)$matched)
})

test_that("the shipped form template still passes the gate", {
  ft <- load_fields_templates(file.path(engine_root(), "fields_templates"), NULL)
  expect_length(attr(ft, "load_errors"), 0)
  expect_true("anz_kiwisaver_fields" %in% names(ft))
})

test_that("convert_document routes a statement vs a form through one front door", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  fdir <- file.path(engine_root(), "fields_templates")
  tdir <- file.path(engine_root(), "templates")
  od <- tempfile(); ld <- tempfile("dl_")   # own logdir: never write into the repo
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)

  # a form PDF: no transaction template matches -> falls back to form extraction
  fpdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(fpdf))
  fr <- convert_document(fpdf, outdir = od, templates_dir = tdir,
                         user_templates_dir = NULL, fields_dir = fdir, logdir = ld)
  expect_equal(fr$kind, "form")
  expect_equal(fr$status, "ok")
  expect_true(fr$n_fields >= 4)

  # a document that matches nothing at all stays an unsupported statement
  bad <- tempfile(fileext = ".csv"); writeLines(c("a,b", "1,2"), bad)
  r2 <- convert_document(bad, outdir = od, templates_dir = tdir,
                         user_templates_dir = NULL, fields_dir = fdir, logdir = ld)
  expect_equal(r2$kind, "statement")
  expect_equal(r2$status, "unsupported")
})

# ---------------------------------------------------------------------------
# F1-15: ONE run record per run, holding the FINAL outcome. convert_statement
# used to write "unsupported" before the form fallback ran, so a form that
# converted perfectly was on record as a failure -- and counted in Admin's
# "build these next" queue, which reads exactly that field.
# ---------------------------------------------------------------------------

test_that("a converted form is logged ONCE, as a form, not as an unsupported statement", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  fpdf <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(fpdf))
  od <- tempfile("fo_"); ld <- tempfile("fl_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)

  fr <- convert_document(fpdf, outdir = od, templates_dir = file.path(engine_root(), "templates"),
                         user_templates_dir = NULL,
                         fields_dir = file.path(engine_root(), "fields_templates"),
                         logdir = ld)
  expect_equal(fr$kind, "form")
  expect_equal(fr$status, "ok")

  rec <- read_runs(ld)
  expect_equal(nrow(rec), 1L)                              # exactly one record
  expect_identical(rec$status, "ok")                       # the FINAL outcome
  expect_identical(rec$kind, "form")
  expect_identical(rec$detected_template, "anz_kiwisaver_fields")
  expect_true(rec$n_fields >= 4)
  # ...and it therefore does NOT show up as a format nobody has built yet.
  expect_equal(nrow(unsupported_clusters(read_runs(ld))), 0L)
})

test_that("a document nothing can read is still logged once, as unsupported", {
  od <- tempfile("fo_"); ld <- tempfile("fl_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  bad <- tempfile(fileext = ".csv"); writeLines(c("a,b", "1,2"), bad)
  r <- convert_document(bad, outdir = od, templates_dir = file.path(engine_root(), "templates"),
                        user_templates_dir = NULL,
                        fields_dir = file.path(engine_root(), "fields_templates"),
                        logdir = ld)
  expect_equal(r$status, "unsupported")
  rec <- read_runs(ld)
  expect_equal(nrow(rec), 1L)
  expect_identical(rec$status, "unsupported")
  expect_identical(rec$kind, "statement")
  expect_true(nrow(unsupported_clusters(read_runs(ld))) >= 1)   # this one IS a gap
})

test_that("convert_statement still writes its own record when called directly", {
  # the Admin bulk-convert and the CLI call convert_statement, not the front door.
  fx <- fixture("samples/raw/bnz/bnz_transaction_export_01.csv")
  skip_if_not(file.exists(fx))
  od <- tempfile("so_"); ld <- tempfile("sl_")
  on.exit(unlink(c(od, ld), recursive = TRUE), add = TRUE)
  res <- convert_statement(fx, outdir = od, templates_dir = templates_dir(), logdir = ld)
  expect_true(file.exists(file.path(ld, "runs", paste0(res$run_id, ".json"))))
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
