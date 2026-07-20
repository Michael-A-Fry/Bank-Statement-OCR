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
