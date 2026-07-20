# Tests for the "tell our team" escape hatch (R/requests.R).

test_that("record_template_request stores detail + generic context, no file needed", {
  dir <- tempfile(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  id <- record_template_request(
    detail = "Dates look like 2·Dez and amounts use a comma decimal.",
    context = list(file_ext = "pdf", bank = "Some EU Bank",
                   date_format = "(none fit)", amount_style = "unsigned"),
    requested_by = "AB", dir = dir)
  expect_true(nzchar(id))

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
  id <- record_template_request("format X", list(file_ext = "csv"), "CD", dir = dir)
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
