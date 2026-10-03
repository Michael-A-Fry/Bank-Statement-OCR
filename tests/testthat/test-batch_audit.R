# Tests for the bulk statement audit (R/batch_audit.R) -- the "paste 250
# statements" review: what the reader proves, what it cannot read (clustered by
# layout), which checks stop a proof, and a PII-safe combined report.

.mk_csv <- function(dir, name, lines) { p <- file.path(dir, name); writeLines(lines, p); p }

test_that("batch_audit summarises a mixed set: proven, unproven, unread, gaps", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  file.copy(proven_csv(), file.path(dir, "a_proven.csv"))
  xero <- c("*Date,*Amount,Payee,Description,Reference",
            "05/01/2024,-12.50,SECRETPAYEE,coffee,R1",
            "06/01/2024,100.00,ACME,pay,R2")
  .mk_csv(dir, "b_xero.csv", xero)
  weird <- c("colA;colB;colC", "1;2;3", "4;5;6")
  .mk_csv(dir, "x_unknown.csv", weird); .mk_csv(dir, "y_unknown.csv", weird)

  paths <- list.files(dir, full.names = TRUE)
  b <- batch_audit(paths)
  expect_equal(nrow(b$per_file), 4L)
  expect_true(all(c("outcome", "kind", "bank", "signature", "amount_style", "checks_failed") %in% names(b$per_file)))
  expect_identical(b$per_file$outcome[1], "proven")
  expect_identical(b$per_file$bank[1], "bnz")
  expect_identical(b$per_file$outcome[2], "check")
  expect_equal(sum(b$per_file$outcome == "unread"), 2L)
  expect_equal(b$feature_gaps$total, 4L)
  expect_equal(b$feature_gaps$unread, 2L)
  expect_equal(nrow(b$clusters), 1L)                  # the two unknowns are one layout
  expect_equal(b$clusters$count, 2L)
  expect_true(length(b$feature_gaps$checks_failed) >= 1L)
})

test_that("an audit learns nothing and converts nothing", {
  ld <- tempfile("ba_ly_")
  batch_audit(proven_csv(), layouts_dir = ld)
  expect_false(dir.exists(ld) && length(list.files(ld, recursive = TRUE)) > 0)
})

test_that("the combined report leaks no PII", {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .mk_csv(dir, "s.csv", c("*Date,*Amount,Payee",
                          "05/01/2024,-12.50,SECRETPAYEE12345", "06/01/2024,9.99,ANOTHERSECRET99"))
  rep <- format_batch_audit(batch_audit(list.files(dir, full.names = TRUE)))
  expect_false(grepl("SECRETPAYEE12345", rep))
  expect_false(grepl("ANOTHERSECRET99", rep))
  expect_true(grepl("safe to share", rep))
})

test_that("the safe-to-share report keeps no words but layout vocabulary", {
  # G9. layout_signature()'s hint reaches format_batch_audit(), which is headed
  # "safe to share - no PII". On a document with no transaction header -- exactly
  # the class that reaches the form and report routes -- the hint used to be the
  # commonest words on the page, which on a real report are the people named on
  # it. Whatever R/layout.R's fallback does, nothing this file writes down may
  # carry a name.
  expect_identical(.layout_hint_safe("ambrose | whitcombe", "pdf"), "")
  expect_identical(.layout_hint_safe("amount | balance | date | description", "pdf"),
                   "amount | balance | date | description")
  expect_identical(.layout_hint_safe("ambrose | amount | whitcombe", "pdf"), "amount")
  # a delimited file's hint is its HEADER ROW: column names, structure by
  # construction, and no frequency fallback exists on that branch to abuse.
  expect_identical(.layout_hint_safe("*amount | *date | payee", "delimited"),
                   "*amount | *date | payee")
  expect_identical(.layout_hint_safe("", "pdf"), "")
  expect_identical(.layout_hint_safe(NULL, "pdf"), "")
})
