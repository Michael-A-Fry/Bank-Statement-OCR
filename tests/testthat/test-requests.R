# Tests for the format requests raised in 1.x (R/requests.R). Nothing in 2.0.0
# raises a new one -- the writer went with the template screens (N228) -- so a
# request is written here in the shape 1.x wrote it, and the Admin queue must
# still read it and triage it.

.rq_write <- function(dir, id, detail, context = list(), by = "AB", ts = "2026-09-01T10:00:00+1200") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  rec <- list(id = id, ts = ts, requested_by = by, status = "open", detail = detail,
              context = context, history = list(list(ts = ts, status = "open")))
  writeLines(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", pretty = TRUE),
             file.path(dir, paste0(id, ".json")))
  id
}

test_that("a request raised in 1.x is read with its detail and generic context", {
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  .rq_write(dir, "req-20260901100000-1234",
            "Dates look like 2 Dez and amounts use a comma decimal.",
            list(file_ext = "pdf", bank = "Some EU Bank", date_format = "(none fit)", amount_style = "unsigned"))
  q <- read_template_requests(dir)
  expect_equal(nrow(q), 1L)
  expect_equal(q$status, "open")
  expect_equal(q$requested_by, "AB")
  expect_true(grepl("comma decimal", q$detail))
  expect_true(grepl("file_ext=pdf", q$context))
  expect_true(grepl("bank=Some EU Bank", q$context))
})

test_that("set_request_status triages and read reflects it", {
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  id <- .rq_write(dir, "req-20260901100500-5678", "format X", list(file_ext = "csv"), by = "CD")
  expect_true(set_request_status(id, "actioned", dir = dir))
  q <- read_template_requests(dir)
  expect_equal(q$status, "actioned")
  expect_false(set_request_status("nope", "actioned", dir = dir))  # unknown id
})

test_that("read_template_requests on an empty folder is a well-formed empty frame", {
  q <- read_template_requests(tempfile())
  expect_equal(nrow(q), 0L)
  expect_true(all(c("id", "status", "detail", "context") %in% names(q)))
})

test_that("nothing raises a new request any more (N228)", {
  expect_false(exists("record_template_request", mode = "function"))
})
