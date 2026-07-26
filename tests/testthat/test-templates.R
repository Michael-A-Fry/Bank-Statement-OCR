# Template validation (build-contract sections 6, 7, 11.5). amount_sign
# prerequisites must be caught at LOAD with a clear per-id message rather than
# crashing (or silently mis-signing) inside parse_statement.

.min_tmpl <- function(...) {
  t <- list(
    id = "x", bank = "B", statement_type = "e", format = "delimited",
    version = 1, min_score = 1,
    # A DISTINCTIVE header set: "Date"/"Amount" alone appear on every statement
    # ever exported, so validate_template now rejects them as a fingerprint (that
    # looseness is what let one template swallow other banks' files). These tests
    # are about saving / hiding / origin, so they carry a realistic fingerprint.
    fingerprint = list(header_contains_all = c("Date", "Amount", "Particulars")),
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
  ok2 <- .min_tmpl(amount_sign = "type_dc", type_debit_value = "D",
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

# ---------------------------------------------------------------------------
# columns.date.format may be a LIST of candidate formats (#42).
#
# THE BUG THIS CLOSES. templates/asb_everyday_csv.yaml pinned "%Y/%m/%d". ASB's
# own committed sample samples/raw/asb/asb_transaction_export_03.csv writes
# 13/10/2025, so it matched ASB confidently (7/6) and then returned EVERY date as
# NA with dates_readable = fail -- the "a bank tweaked its export" brittleness,
# with a green suite.
#
# THE RULE. All-or-nothing: a candidate wins only if it reads EVERY non-empty
# value in the column. A per-row fallback would be silently wrong -- "03/04" reads
# as 3 April under one candidate and 4 March under another, on the same statement.
# ---------------------------------------------------------------------------

test_that("resolve_date_format picks the ONE format that reads the whole column", {
  fmts <- c("%Y/%m/%d", "%d/%m/%Y")
  expect_identical(resolve_date_format(c("2014/12/20", "2014/12/21"), fmts), "%Y/%m/%d")
  expect_identical(resolve_date_format(c("13/10/2025", "17/10/2025"), fmts), "%d/%m/%Y")
  # blanks are not evidence either way and must not veto a candidate
  expect_identical(resolve_date_format(c("13/10/2025", "", NA, "  "), fmts), "%d/%m/%Y")
  # a single declared format resolves to itself, whatever the data says: the
  # ordinary case must behave exactly as it did before lists were allowed.
  expect_identical(resolve_date_format(c("nonsense"), "%d/%m/%Y"), "%d/%m/%Y")
  # nothing to judge on -> the first declared candidate (deterministic)
  expect_identical(resolve_date_format(character(0), fmts), "%Y/%m/%d")
  expect_identical(resolve_date_format(c("", NA), fmts), "%Y/%m/%d")
})

test_that("resolve_date_format refuses a MIXED column rather than guessing", {
  # The forensic case: one format reads rows 1-2, the other reads row 3, neither
  # reads all three. Reading them under different formats would put two dates in
  # the file under a day/month reading and one under month/day -- plausible and
  # wrong. Fail closed instead: NA -> every date NA -> dates_readable fails.
  mixed <- c("2014/12/20", "2014/12/21", "13/10/2025")
  expect_true(is.na(resolve_date_format(mixed, c("%Y/%m/%d", "%d/%m/%Y"))))
  # ...and no declared candidate at all is also NA, never a silent default
  expect_true(is.na(resolve_date_format(c("13/10/2025"), character(0))))
})

test_that("a format LIST is allowed for delimited but REJECTED for pdf", {
  ok <- .min_tmpl()
  ok$columns$date <- list(source = "Date", format = c("%Y/%m/%d", "%d/%m/%Y"))
  expect_length(validate_template(ok), 0L)

  # A pdf template's table.date_format goes straight to as.Date(), which RECYCLES
  # a format vector element-wise -- row 1 read one way, row 2 the other. That is a
  # whole column of plausible wrong dates, so it must be refused at load.
  pdf_t <- list(id = "p", bank = "B", statement_type = "e", format = "pdf",
    version = 1, min_score = 1, currency = "NZD",
    fingerprint = list(page_contains_all = list("Transaction type and details")),
    table = list(date_format = c("%d %b", "%d/%m/%Y"), amount_sign = "signed",
      columns = list(date = list(x_min = 0, x_max = 80),
                     description = list(x_min = 80, x_max = 300),
                     amount = list(x_min = 300, x_max = 400))))
  expect_true(any(grepl("must be a single date format", validate_template(pdf_t))))

  # a garbage entry anywhere in the list is still rejected (not just position 1)
  bad <- .min_tmpl()
  bad$columns$date <- list(source = "Date", format = c("%Y/%m/%d", "not a format"))
  expect_true(any(grepl("is not a date format", validate_template(bad))))
})

test_that("a multi-format template still shows ONE date_format per overview row", {
  # the overview and the duplicate-layout signature need a scalar per template
  expect_identical(.one_date_format(c("%Y/%m/%d", "%d/%m/%Y")), "%Y/%m/%d | %d/%m/%Y")
  ov <- template_overview(load_template_set(templates_dir(), "does_not_exist"))
  asb <- ov[ov$id == "asb_everyday_csv", ]
  expect_equal(nrow(asb), 1L)
  expect_identical(asb$date_format, "%Y/%m/%d | %d/%m/%Y")
})

# ---------------------------------------------------------------------------
# The fingerprint distinctiveness helpers are SHARED, not private (#9).
# R/forms.R reuses .fp_fingerprint_problems so a form template passes the SAME
# generic-fingerprint gate as a statement template. If either helper is ever moved
# into a local scope or renamed, forms.R silently falls back to a weaker inline
# rule -- so pin their visibility and signature here.
# ---------------------------------------------------------------------------
test_that("the fingerprint distinctiveness helpers are visible at top level", {
  expect_true(exists(".fp_fingerprint_problems", mode = "function"))
  expect_true(exists(".fp_specific", mode = "function"))
  expect_identical(names(formals(.fp_fingerprint_problems)), c("ph", "min_score", "fmt"))
  # and they still judge: a bare generic word cannot carry a pdf fingerprint
  expect_true(length(.fp_fingerprint_problems(c("Balance"), 1, "pdf")) > 0)
  expect_length(.fp_fingerprint_problems(c("Transaction type and details"), 1, "pdf"), 0L)
  expect_identical(.fp_specific(c("Balance", "Transaction type and details")), c(FALSE, TRUE))
})

test_that("resolve_delimiter is all-or-nothing and leaves single-delimiter templates alone", {
  one <- .min_tmpl()                                    # delimiter: ","
  # the ordinary case short-circuits: the header is never even consulted
  expect_identical(resolve_delimiter("anything at all", one), ",")
  expect_identical(resolve_delimiter(NA_character_, one), ",")

  multi <- .min_tmpl(delimiter = c(",", "\t"))
  expect_identical(resolve_delimiter("Date\tAmount\tParticulars", multi), "\t")
  expect_identical(resolve_delimiter("Date,Amount,Particulars", multi), ",")
  # a header carrying only SOME of the fingerprinted columns is not a fit under
  # either candidate -> first declared, never a partial split
  expect_identical(resolve_delimiter("Date\tAmount", multi), ",")
  # no header to judge on -> first declared (deterministic)
  expect_identical(resolve_delimiter("", multi), ",")

  # a missing delimiter key still means comma, as it always has
  expect_identical(resolve_delimiter("Date,Amount", .min_tmpl(delimiter = NULL)), ",")
})

test_that("an empty delimiter entry is rejected at load", {
  # an empty separator would split every line into single characters
  expect_true(any(grepl("non-empty separator",
                        validate_template(.min_tmpl(delimiter = c(",", ""))))))
  expect_length(validate_template(.min_tmpl(delimiter = c(",", "\t"))), 0L)
})
