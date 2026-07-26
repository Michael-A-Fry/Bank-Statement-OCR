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

# ---------------------------------------------------------------------------
# #6/#32 -- uploads/ held a byte-for-byte copy of every converted statement and
# nothing ever removed it. purge_uploads deletes the STATEMENT and keeps the small
# record of what happened to it, so the audit trail outlives the client data.
.mk_upload <- function(dir, name, age_days = 0) {
  f <- tempfile(fileext = ".csv"); writeLines(c("Date,Amount", "2024-01-01,1.00"), f)
  id <- record_upload(f, name = name, status = "unsupported", dir = dir)
  if (age_days > 0)
    Sys.setFileTime(file.path(dir, id, name), Sys.time() - age_days * 86400)
  id
}

test_that("purge_uploads deletes statements past their keep period, keeps the record", {
  d <- tempfile("up_"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  old <- .mk_upload(d, "old.csv", age_days = 200)
  new <- .mk_upload(d, "new.csv", age_days = 0)
  r <- purge_uploads(d, keep_days = 90)
  expect_equal(r$purged, 1L)
  expect_equal(r$kept, 1L)
  expect_false(file.exists(file.path(d, old, "old.csv")))   # client data gone
  expect_true(file.exists(file.path(d, old, "record.json"))) # the audit of it stays
  expect_true(file.exists(file.path(d, new, "new.csv")))     # still inside the period
  rec <- jsonlite::fromJSON(file.path(d, old, "record.json"), simplifyVector = FALSE)
  expect_true(isTRUE(rec$purged))
  expect_true("purged" %in% vapply(rec$history, function(h) h$status %||% "", character(1)))
  # safe to run again: nothing left to do, nothing double-counted
  r2 <- purge_uploads(d, keep_days = 90)
  expect_equal(r2$purged, 0L)
  expect_equal(r2$kept, 1L)
})

test_that("purge_uploads honours 'keep indefinitely' and an absent folder", {
  d <- tempfile("up0_"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  id <- .mk_upload(d, "ancient.csv", age_days = 5000)
  for (kd in list(0, -1, NA, "not a number")) {
    r <- purge_uploads(d, keep_days = kd)
    expect_equal(r$purged, 0L)
  }
  expect_true(file.exists(file.path(d, id, "ancient.csv")))
  expect_equal(purge_uploads(tempfile("absent_"), keep_days = 1)$purged, 0L)
})

test_that("purge_uploads uses the injected clock, like rollup_logs", {
  d <- tempfile("upn_"); dir.create(d); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  id <- .mk_upload(d, "today.csv", age_days = 0)
  # nothing is old yet...
  expect_equal(purge_uploads(d, keep_days = 30)$purged, 0L)
  # ...until "now" is a year on
  expect_equal(purge_uploads(d, keep_days = 30, now = as.numeric(Sys.time()) + 365 * 86400)$purged, 1L)
  expect_false(file.exists(file.path(d, id, "today.csv")))
})

test_that("uploads_retention_note tells the truth for every setting", {
  expect_match(uploads_retention_note(90), "90 days")
  expect_match(uploads_retention_note(1), "1 day,")            # not "1 days"
  expect_match(uploads_retention_note(0), "indefinitely")
  expect_match(uploads_retention_note(NULL), "indefinitely")
  expect_match(uploads_retention_note("nonsense"), "indefinitely")
  # every wording admits a copy is kept -- that is the whole point of the line
  for (v in list(90, 0, NULL)) expect_match(uploads_retention_note(v), "copy of the file")
})

# #7 -- the per-session scratch folders (outputs + a copy of the source) were only
# reclaimed when the R process exited, and this app is a service that runs for
# months.
test_that("sweep_temp_dirs removes stale session folders and nothing else", {
  td <- tempfile("sw_"); dir.create(td); on.exit(unlink(td, recursive = TRUE), add = TRUE)
  stale <- file.path(td, "cv_stale"); dir.create(stale); writeLines("x", file.path(stale, "statement.csv"))
  fresh <- file.path(td, "cv_fresh"); dir.create(fresh)
  live  <- file.path(td, "cv_live");  dir.create(live)
  other <- file.path(td, "not_ours"); dir.create(other)
  Sys.setFileTime(stale, Sys.time() - 48 * 3600)
  Sys.setFileTime(live,  Sys.time() - 48 * 3600)
  n <- sweep_temp_dirs(td, prefixes = "cv_", keep_hours = 24, exclude = live)
  expect_equal(n, 1L)
  expect_false(dir.exists(stale))     # stale, swept (statement copy and all)
  expect_true(dir.exists(fresh))      # too new to touch
  expect_true(dir.exists(live))       # a session still using it
  expect_true(dir.exists(other))      # never ours to delete
  expect_equal(sweep_temp_dirs(tempfile("gone_")), 0L)
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
