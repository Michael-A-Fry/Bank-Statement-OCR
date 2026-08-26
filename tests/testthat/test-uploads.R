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

# ---------------------------------------------------------------------------
# K1: ONE INSTANT, THREE CLOCKS.
#
# Measured under TZ=Pacific/Auckland, all from the SAME second:
#   upload folder id   20260826193118       local, no marker
#   run_id             20260826073118       UTC,   no marker
#   record.json ts     2026-08-26T19:31:18+1200
# The two handles a reviewer uses to tie an upload to a conversion were twelve
# hours -- and across the overnight boundary a whole DAY -- apart, with nothing on
# either saying which zone it was in, while the Admin uploads table lists them
# side by side and invites matching them by eye.
#
# ONE RULE: every timestamp WRITTEN is UTC with a Z, every timestamp SHOWN is
# local with the zone named. The upload id and the run id then agree by
# construction rather than by luck.
test_that("an upload is stamped in the same clock as the run_id it will be tied to", {
  d <- tempfile("uptz_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- tempfile(fileext = ".csv"); writeLines(c("Date,Amount", "2024-01-01,1.00"), f)
  old_tz <- Sys.getenv("TZ", unset = NA_character_)
  Sys.setenv(TZ = "Pacific/Auckland")
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz), add = TRUE)

  id <- record_upload(f, name = "stmt.csv", dir = d)
  # the id's date-and-time half is UTC, which is what R/convert.R stamps a run_id
  # with -- so the two read as the same instant instead of twelve hours apart
  stamp <- sub("^.*-", "", id)
  expect_match(stamp, "^[0-9]{14}$")
  expect_equal(as.numeric(as.POSIXct(stamp, format = "%Y%m%d%H%M%S", tz = "UTC")),
               as.numeric(Sys.time()), tolerance = 120)

  rec <- jsonlite::fromJSON(file.path(d, id, "record.json"))
  expect_match(rec$ts, "Z$")               # WRITTEN: UTC, and it says so
  expect_false(grepl("\\+", rec$ts))
  set_upload_status(id, "converted", dir = d)
  rec2 <- jsonlite::fromJSON(file.path(d, id, "record.json"))
  expect_match(utils::tail(rec2$history$ts, 1), "Z$")
})

test_that("the uploads table shows local time with the zone named, and keeps the UTC record", {
  d <- tempfile("upshow_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- tempfile(fileext = ".csv"); writeLines("Date,Amount", f)
  old_tz <- Sys.getenv("TZ", unset = NA_character_)
  Sys.setenv(TZ = "Pacific/Auckland")
  on.exit(if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz), add = TRUE)
  id <- record_upload(f, name = "s.csv", dir = d)
  u <- read_uploads(d)
  # SHOWN: local, and the zone is on the page so nobody has to ask "where?"
  expect_match(u$ts[1], "NZ[SD]T$")
  expect_false(grepl("T[0-9]{2}:", u$ts[1]))    # not the machine stamp
  # and the record's own UTC stamp is still there for anything comparing or exporting
  expect_match(u$ts_utc[1], "Z$")
})

# A plain string sort of `ts` -- which is what "newest first" used to be -- mixes
# records written before this tool settled on UTC (".. +1200") with the ones
# written since (".. Z"), and reverses the one hour a year the clocks go back.
test_that("uploads are ordered by the instant, not by the text of the stamp", {
  d <- tempfile("uporder_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # 02:30+1300 is one hour BEFORE 02:30+1200 -- the hour the clocks go back --
  # and it sorts after it as text.
  mk <- function(id, ts) {
    dir.create(file.path(d, id))
    writeLines(jsonlite::toJSON(list(id = id, ts = ts, status = "ok", history = list()),
                                auto_unbox = TRUE), file.path(d, id, "record.json"))
  }
  mk("earlier", "2026-04-05T02:30:00+1300")
  mk("later",   "2026-04-05T02:30:00+1200")
  expect_identical(read_uploads(d)$id, c("later", "earlier"))   # newest first, truly
})
