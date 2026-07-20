# Tests for the per-conversion feedback capture (feedback.jsonl).

test_that("submit_feedback writes a flagged record for anything but 'correct'", {
  ld <- tempfile("fb_")
  r1 <- submit_feedback("run-abc", "correct", comment = "  ", requested_by = "amy",
                        template_id = "anz_everyday_csv", logdir = ld)
  r2 <- submit_feedback("run-xyz", "wrong", comment = "amount column swapped",
                        requested_by = "ben", template_id = "anz_everyday_csv", logdir = ld)
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
  r <- submit_feedback("r", "  Minor_Issues ", logdir = ld)
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
  # one file per run, named by run_id (concurrency-safe, no shared append)
  expect_true(file.exists(file.path(ld, "runs", paste0(res$run_id, ".json"))))

  submit_feedback(res$run_id, "correct", requested_by = "test",
                  template_id = res$template_id, logdir = ld)
  fb <- read_feedback(ld)
  expect_true(res$run_id %in% fb$run_id)
})

test_that("many concurrent runs/feedback each get their own file (no collisions)", {
  ld <- tempfile("cc_")
  ids <- sprintf("run-%03d", 1:25)
  for (id in ids) submit_feedback(id, "correct", logdir = ld)
  fb <- read_feedback(ld)
  expect_equal(nrow(fb), 25L)
  expect_equal(length(unique(fb$run_id)), 25L)
})
