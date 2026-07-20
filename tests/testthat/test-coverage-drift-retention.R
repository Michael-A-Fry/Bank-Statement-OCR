# Tests for field coverage ("what's present but missing"), drift detection, and
# log rollup/retention.

.cov_tx <- function() data.frame(
  row_id = 1:2, date = c("2025-01-01","2025-01-02"), date_raw = c("a","b"),
  description = c("Shop","Cafe"), amount = c(-5, 9), amount_raw = c("-5","9"),
  direction = c("debit","credit"), balance = c(NA_real_, NA_real_),
  balance_raw = c(NA_character_, NA_character_), particulars = c(NA, NA),
  code = c(NA, NA), reference = c(NA, NA), other_party = c(NA, NA),
  type = c(NA, NA), currency = c("NZD","NZD"), flags = c("",""), stringsAsFactors = FALSE)

test_that("field_coverage flags present-but-empty vs unmapped vs populated", {
  tmpl <- list(format = "delimited", columns = list(
    date = list(source = "D"), amount = list(source = "A"),
    description = list(source = "P"), balance = list(source = "B")))  # balance MAPPED but data all-NA
  cov <- field_coverage(list(transactions = .cov_tx()), tmpl)
  v <- function(f) cov$verdict[cov$field == f]
  expect_equal(v("date"), "populated")
  expect_equal(v("amount"), "populated")
  expect_equal(v("balance"), "empty")        # mapped but every row blank -> the thing to check
  expect_equal(v("particulars"), "unmapped") # not mapped -> fine
  expect_match(coverage_summary(cov), "present-but-empty")
  expect_match(coverage_summary(cov), "balance")
})

test_that("debit+credit columns count as amount being mapped", {
  tmpl <- list(format = "pdf", table = list(columns = list(
    date = list(x_min = 1, x_max = 2), description = list(x_min = 2, x_max = 3),
    debit = list(x_min = 3, x_max = 4), credit = list(x_min = 4, x_max = 5))))
  cov <- field_coverage(list(transactions = .cov_tx()), tmpl)
  expect_equal(cov$verdict[cov$field == "amount"], "populated")
})

test_that("template_drift flags a template whose health recently dropped", {
  # 6 earlier healthy 'ok' runs, then 4 recent 'needs_review' -> drift
  runs <- data.frame(
    detected_template = rep("bnz_everyday_csv", 10),
    ts = sprintf("2026-01-%02d", 1:10),
    status = c(rep("ok", 6), rep("needs_review", 4)),
    kpi_fail_count = c(rep(0, 6), rep(1, 4)),
    trust_level = c(rep("high", 6), rep("low", 4)),
    stringsAsFactors = FALSE)
  d <- template_drift(runs, recent_frac = 0.4, min_runs = 6)
  expect_equal(nrow(d), 1L)
  expect_equal(d$template[1], "bnz_everyday_csv")
  expect_true(d$drop[1] >= 25)
})

test_that("template_drift ignores a consistently healthy template", {
  runs <- data.frame(detected_template = rep("asb_everyday_csv", 10),
    ts = sprintf("2026-02-%02d", 1:10), status = rep("ok", 10),
    kpi_fail_count = rep(0, 10), trust_level = rep("high", 10), stringsAsFactors = FALSE)
  expect_equal(nrow(template_drift(runs)), 0L)
})

test_that("rollup_logs archives old run files and keeps recent ones", {
  ld <- tempfile("ret_"); dir.create(file.path(ld, "runs"), recursive = TRUE)
  wr <- function(id, ts) writeLines(jsonlite::toJSON(list(run_id = id, ts = ts), auto_unbox = TRUE),
                                    file.path(ld, "runs", paste0(id, ".json")))
  wr("old1", "2020-01-01T00:00:00+0000"); wr("old2", "2020-02-01T00:00:00+0000")
  wr("new1", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  # make the old files genuinely old on disk
  for (f in c("old1", "old2")) Sys.setFileTime(file.path(ld, "runs", paste0(f, ".json")),
                                               Sys.time() - 400 * 86400)
  res <- rollup_logs(ld, "runs", keep_days = 90)
  expect_equal(res$archived, 2L)
  expect_equal(res$kept, 1L)
  expect_true(file.exists(file.path(ld, "runs", "new1.json")))        # recent kept
  expect_false(file.exists(file.path(ld, "runs", "old1.json")))       # old moved out
  expect_true(any(grepl("runs-2020", list.files(file.path(ld, "archive")))))
  # history still readable after rollup
  all <- read_runs_all(ld)
  expect_true(all(c("old1", "old2", "new1") %in% all$run_id))
})

test_that("completeness guard fires when there's no balance or stated count", {
  tx <- .cov_tx(); tx$balance <- NA_real_    # no balance at all
  parsed <- list(transactions = tx, header = list())   # no opening/closing, no stated_count
  r <- reconcile(parsed, list(format = "pdf"))
  expect_false(r$trust$completeness_verified)
  expect_true(any(grepl("completeness", r$trust$reasons, ignore.case = TRUE)))
  d <- build_diagnostics("needs_review", parsed = parsed, recon = r)
  expect_true(any(d$category == "completeness_unverified"))
})

test_that("completeness guard stays quiet when a running balance verifies it", {
  tx <- .cov_tx(); tx$balance <- c(95, 104)           # a running balance is present
  r <- reconcile(list(transactions = tx, header = list()), list(format = "pdf"))
  expect_true(r$trust$completeness_verified)
  expect_false(any(grepl("completeness UNVERIFIED", r$trust$reasons)))
})
