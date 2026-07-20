# Tests for the folder-intake view (R/inbox.R).

test_that("inbox_status reports per-folder listings and counts", {
  root <- tempfile(); on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (d in c("inbox", "processed", "failed", "stuck", "outbox"))
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
  writeLines("x", file.path(root, "processed", "a.csv"))
  writeLines("x", file.path(root, "processed", "b.csv"))
  writeLines("x", file.path(root, "failed", "bad.pdf"))
  dir.create(file.path(root, "outbox", "a"))

  s <- inbox_status(root)
  expect_equal(s$counts[["processed"]], 2L)
  expect_equal(s$counts[["failed"]], 1L)
  expect_equal(s$counts[["inbox"]], 0L)
  expect_true("bad.pdf" %in% s$folders$failed$file)
  expect_true(all(c("file", "size_kb", "modified") %in% names(s$folders$processed)))
})

test_that("inbox_status on a missing root is all-empty, never errors", {
  s <- inbox_status(tempfile())
  expect_equal(unname(s$counts), rep(0L, 5))
  expect_equal(nrow(s$folders$failed), 0L)
})

test_that("failed_file_path resolves a failed original or NA", {
  root <- tempfile(); on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "failed"), recursive = TRUE)
  writeLines("x", file.path(root, "failed", "bad.pdf"))
  expect_true(file.exists(failed_file_path("bad.pdf", root)))
  expect_true(is.na(failed_file_path("nope.pdf", root)))
})
