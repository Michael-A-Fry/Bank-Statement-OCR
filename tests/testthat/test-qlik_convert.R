# The Qlik-facing entrypoint: PROVEN templates only, and a no-template miss points
# the user at the Shiny app (never tries to build a template in Qlik).

.qlik_cfg <- function(templates_path, ...) {
  cfg <- load_config(path = file.path(tempdir(), "absent.yaml"))
  cfg$paths$templates <- templates_path
  cfg$qlik$proven_templates_dir <- templates_path
  cfg$paths$logs    <- file.path(tempdir(), paste0("qk_logs_", as.integer(runif(1, 1, 1e8))))
  cfg$paths$uploads <- file.path(tempdir(), paste0("qk_up_",   as.integer(runif(1, 1, 1e8))))
  dir.create(cfg$paths$logs, showWarnings = FALSE, recursive = TRUE)
  dir.create(cfg$paths$uploads, showWarnings = FALSE, recursive = TRUE)
  modifyList(cfg, list(...))
}

test_that("a proven bank converts and hands Qlik the CSV to load", {
  cfg <- .qlik_cfg(templates_dir())
  cfg$qlik$queue_unsupported <- FALSE
  out <- file.path(tempdir(), paste0("qk_ok_", as.integer(runif(1, 1, 1e8))))
  st <- convert_for_qlik(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"), out, cfg)
  expect_true(st$status %in% c("ok", "needs_review"))
  expect_false(st$needs_template)
  expect_true(!is.na(st$csv) && file.exists(st$csv))
  expect_true(file.exists(file.path(out, "status.json")))
})

test_that("no proven template -> needs_template + Shiny link, never a draft", {
  # An empty 'proven' dir means NOTHING matches, exactly like a bank we haven't
  # templated. The Qlik path must NOT fall through to any draft; it must flag it.
  empty <- file.path(tempdir(), paste0("no_tpl_", as.integer(runif(1, 1, 1e8))))
  dir.create(empty, showWarnings = FALSE, recursive = TRUE)
  cfg <- .qlik_cfg(empty)
  cfg$paths$fields <- empty                       # no proven form templates either
  cfg$app$shiny_url <- "http://statements.internal:8100"
  out <- file.path(tempdir(), paste0("qk_miss_", as.integer(runif(1, 1, 1e8))))
  st <- convert_for_qlik(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"), out, cfg)
  expect_identical(st$status, "unsupported")
  expect_true(st$needs_template)
  expect_identical(st$shiny_url, "http://statements.internal:8100")
  # the miss was queued for the Shiny team (auto "reach out to us")
  expect_true(length(list.files(cfg$paths$uploads, recursive = TRUE)) > 0)
  # status.json carries the branch signal + link for the Qlik sheet
  j <- jsonlite::fromJSON(file.path(out, "status.json"))
  expect_true(j$needs_template)
  expect_identical(j$shiny_url, "http://statements.internal:8100")
})

test_that("the SSE wrapper returns the transactions frame for a proven bank", {
  cfg <- .qlik_cfg(templates_dir()); cfg$qlik$queue_unsupported <- FALSE
  out <- file.path(tempdir(), paste0("qk_sse_", as.integer(runif(1, 1, 1e8))))
  df <- convert_statement_sse(fixture("samples/raw/bnz/bnz_transaction_export_01.csv"), out, cfg)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
  expect_true("date" %in% names(df))
})
