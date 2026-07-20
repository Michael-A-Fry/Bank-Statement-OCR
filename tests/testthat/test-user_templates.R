# Tests for default-vs-user templates and the guided draft pipeline.

test_that("load_templates stamps origin and load_template_set gives default precedence", {
  d <- load_templates(templates_dir(), origin = "default")
  expect_true(length(d) > 0)
  expect_true(all(vapply(d, function(t) identical(t$origin, "default"), logical(1))))

  # a user dir whose template collides with a default id must NOT win
  udir <- tempfile("ut_"); dir.create(udir)
  clash <- d[[1]]; clash$origin <- NULL
  yaml::write_yaml(clash, file.path(udir, paste0(clash$id, ".yaml")))
  # a genuinely new user template
  newt <- list(id = "user_only_csv", bank = "UserBank", statement_type = "everyday",
    format = "delimited", version = 1, min_score = 2, currency = "NZD",
    fingerprint = list(header_contains_all = list("Date", "Amount")),
    delimiter = ",", columns = list(date = list(source = "Date", format = "%d/%m/%Y"),
      amount = list(source = "Amount"), description = list(source = "Payee")),
    amount_sign = "signed")
  yaml::write_yaml(newt, file.path(udir, "user_only_csv.yaml"))

  merged <- load_template_set(templates_dir(), udir)
  expect_identical(merged[[clash$id]]$origin, "default")   # default wins the clash
  expect_identical(merged[["user_only_csv"]]$origin, "user")
})

test_that("an invalid user template is skipped, not fatal (strict=FALSE)", {
  udir <- tempfile("ut_"); dir.create(udir)
  writeLines("id: broken\nformat: delimited\n", file.path(udir, "broken.yaml"))  # missing required keys
  expect_warning(u <- load_templates(udir, origin = "user", strict = FALSE))
  expect_length(u, 0)
  # and the merged set still loads fine
  expect_silent(suppressWarnings(load_template_set(templates_dir(), udir)))
})

test_that("save_user_template validates then round-trips", {
  udir <- tempfile("ut_")
  good <- list(id = "roundtrip_csv", bank = "B", statement_type = "everyday",
    format = "delimited", version = 1, min_score = 2, currency = "NZD",
    fingerprint = list(header_contains_all = list("Date", "Amount")),
    delimiter = ",", columns = list(date = list(source = "Date", format = "%d/%m/%Y"),
      amount = list(source = "Amount"), description = list(source = "Payee")),
    amount_sign = "signed", origin = "user")
  p <- save_user_template(good, udir)
  expect_true(file.exists(p))
  back <- load_templates(udir, origin = "user", strict = FALSE)
  expect_identical(back[["roundtrip_csv"]]$bank, "B")

  bad <- list(id = "nope", format = "delimited")   # missing required keys
  expect_error(save_user_template(bad, udir), "not valid")
})

test_that("draft_template turns a delimited file into a parsing template", {
  fx <- fixture("samples/raw/bnz/bnz_transaction_export_01.csv")
  skip_if_not(file.exists(fx))
  tmpl <- draft_template(fx, bank = "BNZ")
  expect_identical(tmpl$format, "delimited")
  expect_identical(tmpl$origin, "user")
  expect_false(is.null(tmpl$columns$date))
  tx <- draft_preview(fx, tmpl)
  expect_true(is.data.frame(tx) && nrow(tx) >= 1)
  expect_true(all(!is.na(tx$date)))          # dates parsed with the drafted format
})

test_that("draft_template auto-drafts the tutorial PDF (2 amount cols + year-less dates)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  fx <- fixture("samples/raw/tutorial/sample_everyday_statement.pdf")
  skip_if_not(file.exists(fx))
  tmpl <- draft_template(fx, bank = "Kowhai")
  expect_identical(tmpl$format, "pdf")
  expect_identical(tmpl$table$amount_sign, "debit_credit_cols")   # two money cols detected
  expect_identical(tmpl$table$date_format, "%d %b")               # year-less date sniffed
  tx <- draft_preview(fx, tmpl)
  expect_true(is.data.frame(tx) && nrow(tx) >= 10)
  expect_true(all(startsWith(tx$date, "2026-05-")))              # year from the period
})
