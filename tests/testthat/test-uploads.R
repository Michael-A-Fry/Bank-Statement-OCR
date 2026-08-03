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
  expect_true(all(c("id", "status", "needs_pickup", "purged") %in% names(u)))
})

# The upload id is (content hash + WHOLE SECOND). Re-converting the same statement
# twice inside one second therefore produced the same id -- and both uploads shared
# one folder and one record.json, so the first upload's lifecycle history was
# silently erased. In a forensic tool a destroyed record is the cardinal failure.
test_that("two uploads of the same file in the same second stay two records", {
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f <- tempfile(fileext = ".csv"); writeLines(c("Date,Amount", "2024-01-01,1.00"), f)
  a <- record_upload(f, name = "a.csv", status = "unsupported", dir = dir)
  b <- record_upload(f, name = "b.csv", status = "ok", dir = dir)
  expect_false(identical(a, b))
  u <- read_uploads(dir)
  expect_equal(nrow(u), 2L)
  expect_setequal(u$status, c("unsupported", "ok"))
  # the disambiguated id is still a plain path segment, so the traversal guard on
  # upload_file_path keeps accepting it
  expect_false(is.na(upload_file_path(a, dir)))
  expect_false(is.na(upload_file_path(b, dir)))
})

# #6/#32: after the retention purge the RECORD survives and the statement does not.
# Admin must be able to see that, or "open it in the toolkit" is a dead end.
test_that("read_uploads shows when the saved statement has been purged", {
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  f <- tempfile(fileext = ".csv"); writeLines(c("Date,Amount", "2024-01-01,1.00"), f)
  id <- record_upload(f, name = "old.csv", status = "unsupported", dir = dir)
  Sys.setFileTime(file.path(dir, id, "old.csv"), Sys.time() - 200 * 86400)
  purge_uploads(dir, keep_days = 90)
  u <- read_uploads(dir)
  expect_true(u$purged[u$id == id])
  expect_equal(u$status[u$id == id], "unsupported")     # the lifecycle is intact
  expect_true(is.na(upload_file_path(id, dir)))         # the statement itself is gone
})

# The id comes from the browser: a path separator or .. would read any file the
# server process can see, from a handler that (until #37) was not even gated.
test_that("upload_file_path refuses an id that is not a plain path segment (#37)", {
  d <- file.path(tempfile("up_")); dir.create(d, recursive = TRUE)
  expect_true(is.na(upload_file_path("../../etc/passwd", d)))
  expect_true(is.na(upload_file_path("a/b", d)))
  expect_true(is.na(upload_file_path("..", d)))
  expect_true(is.na(upload_file_path("", d)))
  expect_true(is.na(upload_file_path(NA_character_, d)))
})

# The FILENAME comes from the browser too, and Shiny hands input$file$name through
# untouched. It was pasted straight into file.path(): "../../config/config.yaml"
# copied the uploaded statement over that path instead of into uploads/<id>/ --
# and a client's statement written outside uploads/ is also outside
# purge_uploads(), so the retention the Convert page promises never reaches it.
test_that("record_upload cannot be made to write outside uploads/<id>/", {
  d <- tempfile("up_"); dir.create(d, recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- tempfile(fileext = ".csv"); writeLines(c("Date,Amount", "2024-01-01,1.00"), f)
  victim <- file.path(d, "victim.yaml"); writeLines("original: settings", victim)

  for (nm in c("../victim.yaml", "..\\victim.yaml", "../../victim.yaml",
               "a/b/victim.yaml", "..", ".", "")) {
    id <- record_upload(f, name = nm, dir = d)
    stored <- setdiff(list.files(file.path(d, id)), "record.json")
    expect_equal(length(stored), 1L, info = nm)
    # one plain path segment, inside this upload's own folder, always
    expect_false(grepl("[/\\\\]", stored), info = nm)
    expect_false(stored %in% c(".", ".."), info = nm)
    expect_true(file.exists(file.path(d, id, stored)), info = nm)
  }
  # nothing outside the per-upload folders was touched
  expect_identical(readLines(victim, warn = FALSE), "original: settings")

  # ...and an ordinary name is still stored verbatim, so Admin and the re-audit
  # still open the file people recognise.
  id <- record_upload(f, name = "J Smith BNZ Jan.csv", dir = d)
  expect_true(file.exists(file.path(d, id, "J Smith BNZ Jan.csv")))
  expect_identical(basename(upload_file_path(id, d)), "J Smith BNZ Jan.csv")
  # the record keeps the name as SENT -- it is evidence about what arrived
  rec <- jsonlite::fromJSON(file.path(d, id, "record.json"), simplifyVector = FALSE)
  expect_identical(rec$file, "J Smith BNZ Jan.csv")
})
