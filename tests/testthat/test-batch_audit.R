# Tests for the bulk statement audit (R/batch_audit.R) -- the "paste 250
# statements" review: status, gap clustering, editable draft recommendations, and
# a PII-safe combined report.

.mk_csv <- function(dir, name, lines) { p <- file.path(dir, name); writeLines(lines, p); p }

test_that("batch_audit summarises a mixed set: parsed, unsupported, gaps, drafts", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  # two files a shipped template matches (Xero import), two that nothing matches
  xero <- c("*Date,*Amount,Payee,Description,Reference",
            "05/01/2024,-12.50,SECRETPAYEE,coffee,R1",
            "06/01/2024,100.00,ACME,pay,R2")
  .mk_csv(dir, "a_xero.csv", xero); .mk_csv(dir, "b_xero.csv", xero)
  weird <- c("colA;colB;colC", "1;2;3", "4;5;6")
  .mk_csv(dir, "x_unknown.csv", weird); .mk_csv(dir, "y_unknown.csv", weird)

  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  paths <- list.files(dir, full.names = TRUE)
  b <- batch_audit(paths, templates = tmpls)

  expect_equal(nrow(b$per_file), 4L)
  expect_true(all(c("status", "kind", "signature", "amount_style") %in% names(b$per_file)))
  expect_true(sum(b$per_file$status == "unsupported") >= 2)   # the two unknowns
  expect_true(b$feature_gaps$total == 4)
  expect_true(b$feature_gaps$unsupported >= 2)
  # the unsupported files cluster and each cluster gets an editable draft
  expect_true(nrow(b$clusters) >= 1)
  expect_true(length(b$recommendations) >= 1)
  expect_true(nzchar(b$recommendations[[1]]$draft_yaml))
})

test_that("the combined report leaks no PII", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .mk_csv(dir, "s.csv", c("*Date,*Amount,Payee",
                          "05/01/2024,-12.50,SECRETPAYEE12345", "06/01/2024,9.99,ANOTHERSECRET99"))
  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  rep <- format_batch_audit(batch_audit(list.files(dir, full.names = TRUE), templates = tmpls))
  expect_false(grepl("SECRETPAYEE12345", rep))
  expect_false(grepl("ANOTHERSECRET99", rep))
  expect_true(grepl("safe to share", rep))
})

# ---------------------------------------------------------------------------
# FORM documents are covered work, not a coverage gap (#31).
#
# The app's real front door is convert_document(): transaction pipeline first,
# then FORM (mode: fields) extraction. The bulk audit only ever asked the
# transaction half, so a working IRD / KiwiSaver document came back
# "unsupported" -- it inflated the gap count, was clustered as a layout to build,
# and consumed one of the N draft-template recommendations that should have gone
# to a real gap. A coverage report that under-reports coverage sends an analyst
# off to build a template that already exists.
# ---------------------------------------------------------------------------

test_that("a form-template match is reported as its own kind, not as a gap", {
  ftpls <- load_fields_templates(fields_templates_dir())
  skip_if_not(length(ftpls) > 0)
  form_src <- fixture("samples/raw/anz/anz_kiwisaver_statement_guide_sample.pdf")
  skip_if_not(file.exists(form_src))
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  # the fixture must actually be a form match, or this test proves nothing
  skip_if_not(isTRUE(detect_form(read_input(form_src), ftpls)$matched))

  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  weird <- c("colA;colB;colC", "1;2;3", "4;5;6")
  .mk_csv(dir, "x_unknown.csv", weird)
  file.copy(form_src, file.path(dir, "a_form.pdf"))

  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  b <- batch_audit(list.files(dir, full.names = TRUE), templates = tmpls,
                   fields_templates = ftpls)

  row <- b$per_file[b$per_file$file_type == "pdf", ]
  expect_equal(nrow(row), 1L)
  expect_identical(row$status, "form")
  expect_identical(row$template, names(ftpls)[1])
  expect_true(row$detected)                 # covered -> counted as detected
  expect_gt(row$n_fields, 0L)               # a form is measured in FIELDS...
  expect_equal(row$n_rows, 0L)              # ...not transaction rows

  # it is NOT a gap: not counted as unsupported, not clustered, no draft spent
  expect_equal(b$feature_gaps$forms, 1L)
  expect_equal(b$feature_gaps$unsupported, 1L)          # only the unknown CSV
  expect_false(any(b$clusters$example_idx == row$idx))
  expect_equal(nrow(b$clusters), 1L)

  # the report says so out loud, and still leaks nothing
  rep <- format_batch_audit(b)
  expect_true(grepl("form \\(labelled-value\\) documents covered by a fields template: 1", rep))
  expect_true(grepl("forms excluded", rep))
})

test_that("form detection never changes a statement's verdict", {
  # The form pass runs only when NO transaction template matched, exactly as
  # convert_document() orders it -- so installing form templates can never
  # reclassify a statement the engine already handles.
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .mk_csv(dir, "s.csv", c("*Date,*Amount,Payee", "05/01/2024,-12.50,Shop"))
  tmpls <- load_template_set(templates_dir(), "does_not_exist")
  paths <- list.files(dir, full.names = TRUE)
  none <- batch_audit(paths, templates = tmpls, fields_templates = list())
  some <- batch_audit(paths, templates = tmpls,
                      fields_templates = load_fields_templates(fields_templates_dir()))
  expect_identical(none$per_file$status, some$per_file$status)
  expect_equal(some$feature_gaps$forms, 0L)
})
