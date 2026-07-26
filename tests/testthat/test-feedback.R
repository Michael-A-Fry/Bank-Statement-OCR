# Tests for the per-conversion feedback capture (feedback.jsonl).

# An isolated feed folder, so a "wrong" verdict's retraction can never reach the
# real feed while tests run.
.fb_cfg <- function() {
  cfg <- load_config(file.path(tempdir(), "absent.yaml"))
  cfg$feed$feed_dir <- tempfile("fbfeed"); cfg$paths$logs <- tempfile("fblog")
  cfg
}

test_that("submit_feedback writes a flagged record for anything but 'correct'", {
  ld <- tempfile("fb_"); cfg <- .fb_cfg()
  r1 <- submit_feedback("run-abc", "correct", comment = "  ", requested_by = "amy",
                        template_id = "anz_everyday_csv", logdir = ld, config = cfg)
  r2 <- submit_feedback("run-xyz", "wrong", comment = "amount column swapped",
                        requested_by = "ben", template_id = "anz_everyday_csv",
                        logdir = ld, config = cfg)
  expect_false(r1$flagged)
  expect_true(r2$flagged)
  expect_true(is.na(r1$comment))           # blank comment collapses to NA
  expect_equal(r2$comment, "amount column swapped")

  fb <- read_feedback(ld)
  expect_equal(nrow(fb), 2L)
  expect_equal(sum(fb$flagged), 1L)
  expect_true(all(c("run-abc", "run-xyz") %in% fb$run_id))
})

test_that("submit_feedback rejects an unknown verdict", {
  ld <- tempfile("fb_")
  expect_error(submit_feedback("r", "totally_broken", logdir = ld), "verdict must be")
})

test_that("verdict is case-insensitive and trimmed", {
  ld <- tempfile("fb_")
  r <- submit_feedback("r", "  Minor_Issues ", logdir = ld, config = .fb_cfg())
  expect_equal(r$verdict, "minor_issues")
  expect_true(r$flagged)
})

test_that("read_feedback on a missing log is an empty frame, not an error", {
  fb <- read_feedback(tempfile("nope_"))
  expect_equal(nrow(fb), 0L)
  expect_true(all(c("run_id", "verdict", "flagged") %in% names(fb)))
})

test_that("convert_statement stamps a run_id that feedback can reference", {
  fx <- fixture("samples/raw/bnz/bnz_transaction_export_01.csv")
  skip_if_not(file.exists(fx))
  out <- tempfile("cv_"); ld <- tempfile("l_")
  res <- convert_statement(fx, outdir = out, templates_dir = templates_dir(), logdir = ld)
  expect_true(nzchar(res$run_id))
  # one record per run, findable by run_id (concurrency-safe, no shared append)
  runs <- read_runs(ld)
  expect_equal(nrow(runs), 1L)
  expect_identical(runs$run_id, res$run_id)

  submit_feedback(res$run_id, "correct", requested_by = "test",
                  template_id = res$template_id, logdir = ld, config = .fb_cfg())
  fb <- read_feedback(ld)
  expect_true(res$run_id %in% fb$run_id)
})

test_that("many concurrent runs/feedback each get their own file (no collisions)", {
  ld <- tempfile("cc_"); cfg <- .fb_cfg()
  ids <- sprintf("run-%03d", 1:25)
  for (id in ids) submit_feedback(id, "correct", logdir = ld, config = cfg)
  fb <- read_feedback(ld)
  expect_equal(nrow(fb), 25L)
  expect_equal(length(unique(fb$run_id)), 25L)
})

# ---------------------------------------------------------------------------
# F1-19: a "wrong" verdict RETRACTS that run's rows from the accepted feed.
# Feedback used to be write-only: Beth could say the figures were wrong and they
# kept feeding the dashboards for ever.
# ---------------------------------------------------------------------------

.fb_result <- function(run_id = "deadbeef01-20260101000000") list(
  status = "ok", template_id = "bnz_everyday_csv", kind = "statement", run_id = run_id,
  trust = list(level = "high"),
  header = list(source_file = "s.csv", source_sha256 = "deadbeef0123456789",
                bank = "BNZ", statement_type = "everyday", template_version = 1,
                row_count = 2L, period_start = "2026-01-01", period_end = "2026-01-31",
                account_number = "12-3456-7890123-00"),
  feed_rows = data.frame(row_id = 1:2, date = c("2026-01-01", "2026-01-02"),
                         description = c("A", "B"), amount = c(-5, 9),
                         stringsAsFactors = FALSE))

test_that("a 'wrong' verdict withdraws that run's rows from the accepted feed", {
  cfg <- .fb_cfg(); ld <- tempfile("fb_")
  write_feed(.fb_result(), cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  tx <- file.path(cfg$feed$feed_dir, "transactions")
  rv <- file.path(cfg$feed$feed_dir, "review")
  expect_length(list.files(tx), 1)

  rec <- submit_feedback("deadbeef01-20260101000000", "wrong",
                         comment = "amounts inverted", logdir = ld, config = cfg)
  expect_equal(rec$retracted_files, 1L)
  expect_equal(rec$retracted_rows, 2L)

  expect_length(list.files(tx), 0)     # gone from the dashboard table...
  expect_length(list.files(rv), 1)     # ...but MOVED, not destroyed (reversible)
  moved <- utils::read.csv(list.files(rv, full.names = TRUE)[1], stringsAsFactors = FALSE,
                           colClasses = "character")
  expect_identical(unique(moved$gate_result), "retracted:user_reported_wrong")
  expect_equal(nrow(moved), 2L)        # every row kept, nothing silently dropped

  # the Qlik coverage table says so too, rather than just losing the row.
  man <- utils::read.csv(list.files(file.path(cfg$feed$feed_dir, "runs"), full.names = TRUE)[1],
                         stringsAsFactors = FALSE, colClasses = "character")
  expect_identical(man$gate_result, "retracted:user_reported_wrong")
  expect_true(!nzchar(man$feed_file) || is.na(man$feed_file))

  # and it is logged next to the feedback that caused it, so a retraction is
  # never invisible.
  fl <- read_feed_log(ld)
  expect_true(any(fl$gate_result == "retracted:user_reported_wrong"))
})

test_that("'correct' and 'minor_issues' never retract anything", {
  for (v in c("correct", "minor_issues")) {
    cfg <- .fb_cfg(); ld <- tempfile("fb_")
    write_feed(.fb_result(), cfg, ts = "t", proven_ids = "bnz_everyday_csv")
    rec <- submit_feedback("deadbeef01-20260101000000", v, logdir = ld, config = cfg)
    expect_true(is.na(rec$retracted_files), info = v)   # not attempted, not "0"
    expect_length(list.files(file.path(cfg$feed$feed_dir, "transactions")), 1)
  }
})

test_that("retraction only touches the run that was reported, and is idempotent", {
  cfg <- .fb_cfg(); ld <- tempfile("fb_")
  a <- .fb_result("run-A")
  b <- .fb_result("run-B"); b$header$source_sha256 <- "cafebabe9876543210"
  write_feed(a, cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  write_feed(b, cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  expect_length(list.files(file.path(cfg$feed$feed_dir, "transactions")), 2)

  submit_feedback("run-A", "wrong", logdir = ld, config = cfg)
  left <- list.files(file.path(cfg$feed$feed_dir, "transactions"))
  expect_length(left, 1)                                # run-B untouched
  expect_identical(left, "cafebabe98765432.csv")

  # saying it twice changes nothing (the rows are already withdrawn).
  again <- submit_feedback("run-A", "wrong", logdir = ld, config = cfg)
  expect_equal(again$retracted_files, 0L)               # 0 = looked, found nothing
  expect_length(list.files(file.path(cfg$feed$feed_dir, "transactions")), 1)
})

test_that("a re-convert after a retraction restores the row (reversible)", {
  cfg <- .fb_cfg(); ld <- tempfile("fb_")
  write_feed(.fb_result(), cfg, ts = "t1", proven_ids = "bnz_everyday_csv")
  submit_feedback("deadbeef01-20260101000000", "wrong", logdir = ld, config = cfg)
  expect_length(list.files(file.path(cfg$feed$feed_dir, "transactions")), 0)

  write_feed(.fb_result("deadbeef01-20260102000000"), cfg, ts = "t2",
             proven_ids = "bnz_everyday_csv")
  expect_length(list.files(file.path(cfg$feed$feed_dir, "transactions")), 1)
  expect_length(list.files(file.path(cfg$feed$feed_dir, "review")), 0)  # stale copy cleared
})

test_that("an unreachable feed never blocks the verdict being captured", {
  cfg <- .fb_cfg()
  cfg$feed$feed_dir <- file.path(tempfile("nope_"), "deeper")   # never created
  ld <- tempfile("fb_")
  rec <- submit_feedback("run-zzz", "wrong", logdir = ld, config = cfg)
  expect_equal(rec$verdict, "wrong")
  expect_true(rec$flagged)
  expect_equal(rec$retracted_files, 0L)
  expect_equal(nrow(read_feedback(ld)), 1L)
})
