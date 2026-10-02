# ---------------------------------------------------------------------------
# WHO OPENED WHOSE STATEMENT, AND WHEN.
#
# The uploads were recorded and the conversions were recorded. The DOWNLOAD -- the
# moment somebody's bank statement actually lands on somebody's screen -- was
# recorded nowhere, so the first question anyone reviewing this tool asks could not
# be answered at all.
#
# It is what the standards ask for too. ACPO Principle 3: "An audit trail or other
# record of all processes applied to digital evidence should be created and
# preserved." NIST SP 800-86 4.2 wants "a log of every person who had physical
# custody of the evidence, documenting the actions that they performed on the
# evidence and at what time". A conversion log says what was produced; only a
# download log says who took a copy.
# ---------------------------------------------------------------------------

test_that("a download is recorded with who, what, when and the hash of the bytes", {
  d <- tempfile("dlog"); dir.create(d)
  f <- tempfile(fileext = ".xlsx"); writeLines("pretend workbook bytes", f)
  r <- log_download(d, "xlsx", id = "up_abc123", path = f,
                    who = "real.detective", source = "sso", run_id = "run_999")
  expect_identical(r$event, "download")
  expect_identical(r$what, "xlsx")
  expect_identical(r$object_id, "up_abc123")
  expect_identical(r$run_id, "run_999")
  expect_identical(r$by, "real.detective")
  expect_identical(r$identity_source, "sso")
  expect_match(r$at_utc, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$")
  # THE HASH IS THE LOAD-BEARING FIELD. Retention deletes the source statement; after
  # that this is the only proof of what was produced and taken, and it is what
  # settles "the spreadsheet I was given said X".
  expect_identical(r$sha256, file_sha256(f))
  expect_identical(r$bytes, as.integer(file.info(f)$size))
  # on disk, as its own file, readable
  files <- list.files(file.path(d, "downloads"), "[.]json$")
  expect_length(files, 1L)
  back <- jsonlite::fromJSON(file.path(d, "downloads", files[1]))
  expect_identical(back$sha256, r$sha256)
})

test_that("a second download never overwrites the first", {
  # A record that can be replaced is not an audit trail. Two downloads in the SAME
  # SECOND are the case to worry about, because a timestamp alone would collide.
  d <- tempfile("dlog2"); dir.create(d)
  f <- tempfile(); writeLines("x", f)
  for (who in c("detective.a", "detective.b", "detective.c"))
    log_download(d, "xlsx", id = "up_same", path = f, who = who, source = "sso")
  files <- list.files(file.path(d, "downloads"), "[.]json$")
  expect_length(files, 3L)
  whos <- vapply(files, function(x)
    jsonlite::fromJSON(file.path(d, "downloads", x))$by, character(1))
  expect_setequal(unname(whos), c("detective.a", "detective.b", "detective.c"))
})

test_that("logging never breaks a download", {
  # A download that works but is not logged beats one that fails because the logging
  # did: the opposite trade turns a full disk into an outage.
  expect_silent(r <- log_download(file.path(tempfile(), "no", "such", "tree"),
                                  "xlsx", path = "/definitely/not/a/file"))
  expect_identical(r$event, "download")
  expect_true(is.na(r$sha256))
  # and an absent identity is "none", never a guess
  expect_identical(r$identity_source, "none")
  expect_true(is.na(r$by))
})

test_that("EVERY download handler records the download", {
  # The rule has to hold for handlers nobody has written yet, so this walks app.R
  # rather than naming the six that exist. A download that hands back only an
  # explanation is still a download and is still recorded: "she asked for the
  # workbook and got a note saying why there wasn't one" is a fact a reviewer needs.
  src <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  starts <- grep("downloadHandler\\(", src)
  expect_gt(length(starts), 4L)              # the scan itself must not go quiet
  unlogged <- character(0)
  for (i in starts) {
    j <- i; depth <- 0L
    repeat {
      l <- src[j]
      depth <- depth + lengths(regmatches(l, gregexpr("[({]", l))) -
        lengths(regmatches(l, gregexpr("[)}]", l)))
      if (depth <= 0L && j > i) break
      if (j >= length(src)) break
      j <- j + 1L
    }
    if (!any(grepl("\\.dl_log\\(", src[i:j])))
      unlogged <- c(unlogged, sprintf("app.R:%d", i))
  }
  expect_identical(unlogged, character(0),
    info = paste("downloadHandler with no .dl_log:", paste(unlogged, collapse = ", ")))
})

test_that(".dl_log reads the identity the same way the audit log does", {
  # It must not invent a second answer to "who is this". One source of truth:
  # detected_identity_info(), the same function the run record uses -- so a name in
  # a download record carries exactly the weight a name in a run record does, and
  # `identity_source` is there to be judged by.
  src <- readLines(file.path(engine_root(), "app.R"), warn = FALSE)
  blk <- .src_block(src, "\\.dl_log <- function", 20L)
  expect_match(blk, "detected_identity_info\\(\\)")
  expect_match(blk, "log_download\\(")
  expect_match(blk, "LOGDIR")
  # and it cannot throw: safe() around both halves
  expect_match(blk, "safe\\(")
})
