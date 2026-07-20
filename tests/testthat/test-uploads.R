# Tests for upload capture + lifecycle tracking (R/uploads.R).

test_that("record_upload saves the file + record, set_upload_status transitions", {
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f <- tempfile(fileext = ".csv"); writeLines(c("Date,Amount", "2024-01-01,1.00"), f)

  id <- record_upload(f, name = "stmt.csv", requested_by = "tester",
                      status = "unsupported", detail = "no template matched", dir = dir)
  expect_true(nzchar(id))
  expect_true(file.exists(file.path(dir, id, "stmt.csv")))       # the file is saved
  expect_true(file.exists(file.path(dir, id, "record.json")))

  u <- read_uploads(dir)
  expect_equal(nrow(u), 1L)
  expect_equal(u$status, "unsupported")
  expect_true(u$needs_pickup)                                    # unsupported + not taught

  # teach it -> wizard_saved: no longer needs pickup
  set_upload_status(id, "wizard_saved", template = "newbank_csv", dir = dir)
  u2 <- read_uploads(dir)
  expect_equal(u2$status, "wizard_saved")
  expect_equal(u2$template, "newbank_csv")
  expect_false(u2$needs_pickup)
  # the saved file is retrievable for a re-audit
  expect_true(file.exists(upload_file_path(id, dir)))
})

test_that("read_uploads on an empty/missing folder is a well-formed empty frame", {
  u <- read_uploads(tempfile())
  expect_equal(nrow(u), 0L)
  expect_true(all(c("id", "status", "needs_pickup") %in% names(u)))
})
