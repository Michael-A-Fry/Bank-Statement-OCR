# The Qlik analytics feed writer: the governance gate (reconciled + proven), the
# flat stamped transactions, and the always-written per-run manifest.

.mk_csv <- function() {
  p <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(date = c("2026-01-01", "2026-01-02"),
                              description = c("A", "B"), amount = c(-5, 9),
                              stringsAsFactors = FALSE), p, row.names = FALSE)
  p
}

.mk_result <- function(status = "ok", trust = "high", tid = "bnz_everyday_csv") {
  list(status = status, template_id = tid, kind = "statement",
       run_id = "deadbeef01-20260101000000",
       trust = list(level = trust),
       header = list(source_file = "s.csv", source_sha256 = "deadbeef0123456789",
                     bank = "BNZ", statement_type = "everyday", template_version = 1,
                     row_count = 2L, period_start = "2026-01-01", period_end = "2026-01-31",
                     account_number = NA_character_),
       outputs = c(csv = .mk_csv()))
}

.cfg <- function() {
  cfg <- load_config(file.path(tempdir(), "absent.yaml"))
  cfg$feed$feed_dir <- tempfile("feed"); cfg
}

test_that("a reconciled, proven conversion is written to the feed (accepted)", {
  cfg <- .cfg()
  g <- write_feed(.mk_result(), cfg, ts = "2026-01-01T00:00:00",
                  proven_ids = "bnz_everyday_csv")
  expect_true(g$accept)
  tx  <- list.files(file.path(cfg$feed$feed_dir, "transactions"), full.names = TRUE)
  run <- list.files(file.path(cfg$feed$feed_dir, "runs"), full.names = TRUE)
  expect_length(tx, 1); expect_length(run, 1)
  expect_match(basename(tx), "^deadbeef01234567\\.csv$")   # keyed by content hash
  df <- utils::read.csv(tx[1], stringsAsFactors = FALSE, check.names = FALSE)
  expect_true(all(c("run_id", "source_sha256", "template_id", "trust_level",
                    "date", "amount") %in% names(df)))
  expect_equal(nrow(df), 2)                                # stamped on every row
  man <- utils::read.csv(run[1], stringsAsFactors = FALSE)
  expect_identical(man$gate_result, "accepted")
})

test_that("a draft (non-proven) template is withheld, never in the dashboard table", {
  cfg <- .cfg()
  g <- write_feed(.mk_result(), cfg, ts = "t", proven_ids = character(0))  # nothing proven
  expect_false(g$accept)
  expect_identical(g$reason, "withheld:not_proven")
  expect_length(list.files(file.path(cfg$feed$feed_dir, "transactions")), 0)  # NOT in the feed
  expect_length(list.files(file.path(cfg$feed$feed_dir, "review")), 1)        # in review instead
  man <- utils::read.csv(list.files(file.path(cfg$feed$feed_dir, "runs"), full.names = TRUE)[1],
                         stringsAsFactors = FALSE)
  expect_identical(man$gate_result, "withheld:not_proven")
})

test_that("needs_review and low trust are withheld", {
  cfg <- .cfg()
  expect_identical(write_feed(.mk_result(status = "needs_review"), cfg, ts = "t",
                              proven_ids = "bnz_everyday_csv")$reason, "withheld:needs_review")
  cfg2 <- .cfg()
  expect_identical(write_feed(.mk_result(trust = "low"), cfg2, ts = "t",
                              proven_ids = "bnz_everyday_csv")$reason, "withheld:low_trust")
})

test_that("the manifest is always written, even when withheld (coverage never silent)", {
  cfg <- .cfg()
  write_feed(.mk_result(status = "unsupported"), cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  expect_length(list.files(file.path(cfg$feed$feed_dir, "runs")), 1)
})

test_that("feed.enabled = false is a no-op", {
  cfg <- .cfg(); cfg$feed$enabled <- FALSE
  expect_null(write_feed(.mk_result(), cfg, ts = "t", proven_ids = "bnz_everyday_csv"))
  expect_false(dir.exists(cfg$feed$feed_dir))
})
