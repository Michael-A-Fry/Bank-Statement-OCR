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

test_that("save_user_template never clobbers a different id that shares a slug (P3-g)", {
  dir <- tempfile("utpl_"); dir.create(dir)
  base <- list(bank = "B", statement_type = "e", format = "delimited", version = 1,
    min_score = 1, currency = "NZD", delimiter = ",",
    fingerprint = list(header_contains_all = list("Date", "Amount")),
    columns = list(date = list(source = "Date", format = "%d/%m/%Y"),
                   amount = list(source = "Amount"), description = list(source = "Amount")),
    amount_sign = "signed")
  p1 <- save_user_template(c(base, list(id = "ANZ Go!")), dir)
  p2 <- save_user_template(c(base, list(id = "ANZ-Go")), dir)   # sanitises to the same slug
  expect_false(identical(p1, p2))                                # distinct files, no clobber
  expect_length(list.files(dir, pattern = "\\.ya?ml$"), 2)
  # both ids survive (templates are keyed by their id field, not the filename).
  ids <- vapply(list.files(dir, full.names = TRUE), function(f) yaml::read_yaml(f)$id, character(1))
  expect_setequal(unname(ids), c("ANZ Go!", "ANZ-Go"))
  # re-saving the SAME id overwrites its own file (a genuine edit), no new file.
  save_user_template(c(base, list(id = "ANZ Go!", currency = "AUD")), dir)
  expect_length(list.files(dir, pattern = "\\.ya?ml$"), 2)
})

test_that("an auto-drafted delimited fingerprint tolerates a one-column change (P3-h)", {
  fx <- fixture("samples/raw/bnz/bnz_transaction_export_01.csv")
  skip_if_not(file.exists(fx))
  tmpl <- draft_template(fx, bank = "BNZ")
  n_headers <- length(tmpl$fingerprint$header_contains_all)
  expect_true(n_headers >= 4)
  expect_equal(tmpl$min_score, n_headers - 1L)   # all-but-one, not all
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

test_that("draft_template pins the type_dc debit token, never a blind 'D' (P0-2)", {
  fx <- fixture("samples/raw/anz/anz_creditcard_01.csv")
  skip_if_not(file.exists(fx))
  tmpl <- draft_template(fx, bank = "ANZ")
  expect_identical(tmpl$amount_sign, "type_dc")
  expect_false(is.null(tmpl$columns$type))            # indicator column mapped
  expect_true(nzchar(tmpl$type_debit_value %||% ""))  # debit token pinned, not defaulted
  expect_length(validate_template(tmpl), 0)           # a type_dc template MUST carry it
  # signs come out right: a "D" row is money out (negative), a "C" row money in.
  tx <- draft_preview(fx, tmpl)
  expect_true(any(tx$amount < 0) && any(tx$amount > 0))
})

test_that("validate_template rejects a type_dc template with no debit token (P0-2)", {
  bad <- list(id = "x", bank = "B", statement_type = "s", format = "delimited",
              version = 1, min_score = 1, currency = "NZD",
              fingerprint = list(header_contains_all = list("Type")),
              columns = list(date = list(source = "d", format = "%d/%m/%Y"),
                             amount = list(source = "a"),
                             description = list(source = "x"),
                             type = list(source = "Type")),
              amount_sign = "type_dc")
  expect_true(any(grepl("type_debit_value", validate_template(bad))))
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
