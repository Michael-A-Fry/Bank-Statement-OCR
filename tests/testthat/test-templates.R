# Template validation (build-contract sections 6, 7, 11.5). amount_sign
# prerequisites must be caught at LOAD with a clear per-id message rather than
# crashing (or silently mis-signing) inside parse_statement.

.min_tmpl <- function(...) {
  t <- list(
    id = "x", bank = "B", statement_type = "e", format = "delimited",
    version = 1, min_score = 1,
    fingerprint = list(header_contains_all = c("Date", "Amount")),
    delimiter = ",",
    columns = list(date = list(source = "Date"), amount = list(source = "Amount"),
                   description = list(source = "Amount")),
    amount_sign = "signed", currency = "NZD")
  ov <- list(...)
  for (k in names(ov)) t[[k]] <- ov[[k]]
  t
}

test_that("shipped templates all validate cleanly", {
  templates <- load_templates(templates_dir())
  expect_gte(length(templates), 6L)
})

test_that("debit_credit_cols without debit/credit columns is rejected at load", {
  t <- .min_tmpl(amount_sign = "debit_credit_cols")
  probs <- validate_template(t)
  expect_true(any(grepl("columns.debit", probs)))
  expect_true(any(grepl("columns.credit", probs)))
})

test_that("type_dc without a type column is rejected at load", {
  t <- .min_tmpl(amount_sign = "type_dc")
  probs <- validate_template(t)
  expect_true(any(grepl("columns.type", probs)))
})

test_that("valid debit_credit_cols / type_dc templates pass", {
  # debit_credit_cols has NO single 'amount' column -- it supplies debit + credit.
  # (Do not add an 'amount' key here: that masked the bug where validate_template
  # wrongly demanded columns.amount for this style, breaking guided-setup save.)
  ok1 <- .min_tmpl(amount_sign = "debit_credit_cols",
    columns = list(date = list(source = "Date"), description = list(source = "Details"),
      debit = list(source = "Dr"), credit = list(source = "Cr")))
  expect_length(validate_template(ok1), 0L)
  ok2 <- .min_tmpl(amount_sign = "type_dc",
    columns = list(date = list(source = "Date"), amount = list(source = "Amount"),
      description = list(source = "Amount"), type = list(source = "T")))
  expect_length(validate_template(ok2), 0L)
})

test_that("a debit_credit_cols draft round-trips through save (guided-setup save bug)", {
  # The tool's OWN draft of a Debit/Credit CSV must validate + save -- otherwise
  # guided setup dead-ends with 'Couldn't save'.
  f <- fixture("tests/testthat/fixtures/debit_credit_cols.csv")
  skip_if_not(file.exists(f))
  d <- draft_template(f, bank = "TestBank")
  expect_equal(d$amount_sign, "debit_credit_cols")
  expect_null(d$columns$amount)                       # the draft has no amount column
  expect_length(validate_template(d), 0L)             # ...and that is valid
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_silent(save_user_template(d, dir))           # save must not throw
  expect_true(file.exists(file.path(dir, paste0(d$id, ".yaml"))))
})

test_that("extras with a missing source column is rejected", {
  t <- .min_tmpl(extras = list(card = list(note = "no source here")))
  expect_true(any(grepl("extras.card", validate_template(t))))
})

test_that("load_templates hard-errors listing the bad template id", {
  dir <- file.path(tempdir(), paste0("badtmpl_", as.integer(runif(1, 1, 1e6))))
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  yaml::write_yaml(.min_tmpl(id = "broken", amount_sign = "type_dc"),
                   file.path(dir, "broken.yaml"))
  expect_error(load_templates(dir), "broken")
})

test_that("template_overview summarises every template with tested/user origin", {
  ov <- template_overview(load_template_set(templates_dir(), "does_not_exist"))
  expect_true(all(c("id","bank","type","format","amount_sign","date_format","origin","version")
                  %in% names(ov)))
  expect_true(nrow(ov) >= 10)
  expect_true(all(ov$origin == "tested"))                 # shipped defaults
  # a pdf template reports its table amount_sign / date_format, not NA
  anz <- ov[ov$id == "anz_everyday_pdf", ]
  expect_equal(anz$amount_sign, "debit_credit_cols")
  expect_equal(anz$date_format, "%d %b")
})

test_that("template_yaml round-trips back to a valid template", {
  t <- load_template_set(templates_dir(), "does_not_exist")[["westpac_everyday_pdf"]]
  back <- yaml::yaml.load(template_yaml(t))
  expect_length(validate_template(back), 0)
  expect_null(back$origin)                                 # origin stripped for edit
})

test_that("user templates can be listed, renamed, and deleted (management)", {
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  t <- .min_tmpl(); t$id <- "mybank_csv"
  save_user_template(t, dir)
  expect_true("mybank_csv" %in% user_template_ids(dir))

  new_id <- rename_user_template("mybank_csv", "mybank everyday csv", dir)  # spaces sanitised
  expect_equal(new_id, "mybank_everyday_csv")
  expect_true("mybank_everyday_csv" %in% user_template_ids(dir))
  expect_false("mybank_csv" %in% user_template_ids(dir))                    # old removed

  expect_true(delete_user_template("mybank_everyday_csv", dir))
  expect_false("mybank_everyday_csv" %in% user_template_ids(dir))
  expect_false(delete_user_template("does_not_exist", dir))
})

test_that("a user template can be hidden and un-hidden without deleting it", {
  udir <- tempfile(); on.exit(unlink(udir, recursive = TRUE), add = TRUE)
  t <- .min_tmpl(); t$id <- "parkme_csv"
  save_user_template(t, udir)

  # visible by default, and part of the active set used for detection/conversion
  expect_true("parkme_csv" %in% user_template_ids(udir))
  expect_true("parkme_csv" %in% names(load_template_set(templates_dir(), udir)))

  expect_true(set_user_template_hidden("parkme_csv", TRUE, udir))
  # gone from the active set (won't take part in detection) but still on disk
  expect_false("parkme_csv" %in% names(load_template_set(templates_dir(), udir)))
  expect_true("parkme_csv" %in% user_template_ids(udir))
  # ...and still visible to the management view
  active_ids <- names(load_template_set(templates_dir(), udir, include_hidden = TRUE))
  expect_true("parkme_csv" %in% active_ids)
  ov <- template_overview(load_template_set(templates_dir(), udir, include_hidden = TRUE))
  expect_equal(ov$hidden[ov$id == "parkme_csv"], "hidden")

  expect_false(set_user_template_hidden("parkme_csv", FALSE, udir))   # un-hide
  expect_true("parkme_csv" %in% names(load_template_set(templates_dir(), udir)))
})

test_that("shipped templates cannot be hidden", {
  udir <- tempfile(); on.exit(unlink(udir, recursive = TRUE), add = TRUE)
  expect_error(set_user_template_hidden("anz_everyday_pdf", TRUE, udir), "user-created")
})

test_that("duplicate_template_groups clusters same-layout user templates", {
  base <- .min_tmpl()
  a <- base; a$id <- "acme_v1"; a$bank <- "Acme"
  b <- base; b$id <- "acme_v2"; b$bank <- "Acme (branch)"    # same shape, different id/label
  c <- base; c$id <- "other"; c$columns$amount <- list(source = "Value")   # different column mapping
  tset <- list(acme_v1 = a, acme_v2 = b, other = c)
  for (id in names(tset)) tset[[id]]$origin <- "user"
  groups <- duplicate_template_groups(tset)
  expect_length(groups, 1L)                          # only the two acme variants group
  expect_setequal(groups[[1]], c("acme_v1", "acme_v2"))

  # shipped templates are excluded (user_only), and a lone template never groups
  none <- duplicate_template_groups(list(x = `$<-`(a, "origin", "default")))
  expect_length(none, 0L)
})
