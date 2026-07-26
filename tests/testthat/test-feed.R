# The Qlik analytics feed writer: the governance gate (reconciled + proven), the
# flat stamped transactions, and the always-written per-run manifest.

.mk_rows <- function() data.frame(
  row_id = 1:2, date = c("2026-01-01", "2026-01-02"),
  description = c("A", "B"), amount = c(-5, 9), code = c("007", "008"),
  stringsAsFactors = FALSE)

.mk_result <- function(status = "ok", trust = "high", tid = "bnz_everyday_csv",
                       rows = .mk_rows()) {
  list(status = status, template_id = tid, kind = "statement",
       run_id = "deadbeef01-20260101000000",
       trust = list(level = trust),
       header = list(source_file = "s.csv", source_sha256 = "deadbeef0123456789",
                     bank = "BNZ", statement_type = "everyday", template_version = 1,
                     row_count = nrow(rows), period_start = "2026-01-01",
                     period_end = "2026-01-31", account_number = NA_character_),
       feed_rows = rows,
       outputs = character(0))
}

.cfg <- function() {
  cfg <- load_config(file.path(tempdir(), "absent.yaml"))
  cfg$feed$feed_dir <- tempfile("feed")
  cfg$paths$logs <- tempfile("feedlog")   # keep the feed-outcome log out of ./logs
  cfg
}

.read_feed <- function(path)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  colClasses = "character", na.strings = character(0))

test_that("a reconciled, proven conversion is written to the feed (accepted)", {
  cfg <- .cfg()
  g <- write_feed(.mk_result(), cfg, ts = "2026-01-01T00:00:00Z",
                  proven_ids = "bnz_everyday_csv")
  expect_true(g$accept)
  expect_true(g$written)
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
  r <- .mk_result(status = "unsupported"); r$feed_rows <- NULL; r$header$row_count <- 0L
  write_feed(r, cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  expect_length(list.files(file.path(cfg$feed$feed_dir, "runs")), 1)
})

test_that("re-converting flips the feed folder, never leaving a stale row (P1-3)", {
  key <- "deadbeef01234567"
  tx  <- function(cfg) file.path(cfg$feed$feed_dir, "transactions", paste0(key, ".csv"))
  rev <- function(cfg) file.path(cfg$feed$feed_dir, "review", paste0(key, ".csv"))

  # accepted first -> lands in transactions/.
  cfg <- .cfg()
  write_feed(.mk_result(), cfg, ts = "t1", proven_ids = "bnz_everyday_csv")
  expect_true(file.exists(tx(cfg))); expect_false(file.exists(rev(cfg)))

  # same statement re-converted but now withheld (template no longer proven):
  # the accepted row MUST be gone, not left feeding the dashboard.
  write_feed(.mk_result(), cfg, ts = "t2", proven_ids = character(0))
  expect_false(file.exists(tx(cfg)))                 # stale accepted row removed
  expect_true(file.exists(rev(cfg)))                 # now in review instead

  # and back again: withheld -> accepted must clear the review row.
  write_feed(.mk_result(), cfg, ts = "t3", proven_ids = "bnz_everyday_csv")
  expect_true(file.exists(tx(cfg)))
  expect_false(file.exists(rev(cfg)))                # stale review row removed
})

test_that("feed CSVs are written atomically -- no partial/temp files linger (P2-12)", {
  cfg <- .cfg()
  write_feed(.mk_result(), cfg, ts = "t1", proven_ids = "bnz_everyday_csv")
  all_files <- list.files(cfg$feed$feed_dir, recursive = TRUE)
  expect_true(length(all_files) > 0)
  expect_false(any(grepl("\\.part$", all_files)))     # temp renamed away, never left behind
  # the delivered file is complete and readable (rename published it whole).
  tx <- list.files(file.path(cfg$feed$feed_dir, "transactions"), full.names = TRUE)
  expect_equal(nrow(utils::read.csv(tx[1], stringsAsFactors = FALSE)), 2)
})

test_that("an NA trust level fails closed to withheld, never errors in the gate (P3-e)", {
  cfg <- .cfg()
  g <- write_feed(.mk_result(trust = NA_character_), cfg, ts = "t",
                  proven_ids = "bnz_everyday_csv")
  expect_false(g$accept)                              # NA trust -> lowest -> withheld
  # the manifest is still written (coverage never silent), not dropped by an error.
  expect_length(list.files(file.path(cfg$feed$feed_dir, "runs")), 1)
})

test_that("the manifest is keyed by content hash -- a re-convert doesn't double-count (P3-d)", {
  cfg <- .cfg()
  r1 <- .mk_result(); r1$run_id <- "run-A-0001"
  r2 <- .mk_result(); r2$run_id <- "run-B-0002"   # same statement (same sha), new run
  write_feed(r1, cfg, ts = "t1", proven_ids = "bnz_everyday_csv")
  write_feed(r2, cfg, ts = "t2", proven_ids = "bnz_everyday_csv")
  runs <- list.files(file.path(cfg$feed$feed_dir, "runs"), full.names = TRUE)
  expect_length(runs, 1)                             # one manifest row per statement
  expect_identical(utils::read.csv(runs[1], stringsAsFactors = FALSE)$run_id, "run-B-0002")  # latest wins
})

test_that("feed.enabled = false is a no-op", {
  cfg <- .cfg(); cfg$feed$enabled <- FALSE
  expect_null(write_feed(.mk_result(), cfg, ts = "t", proven_ids = "bnz_everyday_csv"))
  expect_false(dir.exists(cfg$feed$feed_dir))
})

# ---------------------------------------------------------------------------
# F1-17: the feed is built from the PARSED data, not a re-read of the output CSV.
# The old writer re-read its own CSV with utils::read.csv and no colClasses, so
# type inference rewrote the values on the way to the dashboard.
# ---------------------------------------------------------------------------

test_that("a leading-zero code and an at-sign description reach the feed verbatim", {
  # BNZ layout with a leading-zero Code and a Payee starting '@' -- the two cases
  # that were corrupted: read.csv turned "000001" into 1, and the spreadsheet
  # formula guard prefixed "@Cafe" with an apostrophe for Excel's benefit only.
  src <- file.path(tempfile("bnzsrc_"), "bnz_lead_zero.csv")
  dir.create(dirname(src), recursive = TRUE)
  writeLines(c(
    "Date,Amount,Payee,Particulars,Code,Reference,Tran Type,This Party Account,Other Party Account,Serial,Transaction Code,Batch Number,Originating Bank/Branch,Processed Date",
    "01/01/26,-25.00,@Cafe Karori,TEST CITY,007,0012345,POS,11-1111-1111111-00,---,,\"00\",1001,\"11-1111\",01/01/26",
    "02/01/26,10.00,Dairy Refund,TEST CITY,000001,0000042,POS,11-1111-1111111-00,---,,\"00\",1001,\"11-1111\",02/01/26"), src)

  out <- tempfile("cv_"); ld <- tempfile("l_")
  res <- convert_statement(src, outdir = out, templates_dir = templates_dir(), logdir = ld)
  expect_equal(res$status, "ok")

  cfg <- .cfg()
  g <- write_feed(res, cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  expect_true(isTRUE(g$accept) && isTRUE(g$written))
  fd <- .read_feed(list.files(file.path(cfg$feed$feed_dir, "transactions"), full.names = TRUE)[1])

  # 1. leading zeros survive -- the dashboard shows the code the statement shows.
  expect_identical(fd$code, c("007", "000001"))
  expect_identical(fd$reference, c("0012345", "0000042"))
  # 2. the description is VERBATIM -- the Excel-only apostrophe guard is not in it.
  expect_identical(fd$description, c("@Cafe Karori", "Dairy Refund"))

  # ...and the same values in the JSON (the full, verbatim record) are identical.
  js <- jsonlite::fromJSON(paste(readLines(res$outputs[["json"]]), collapse = "\n"))
  expect_identical(as.character(js$transactions$code), fd$code)
  expect_identical(as.character(js$transactions$description), fd$description)

  # The workbook/CSV keep the guard -- that is what it is for, and precisely why
  # the feed must not be built from them.
  wb <- utils::read.csv(res$outputs[["csv"]], stringsAsFactors = FALSE,
                        colClasses = "character", na.strings = character(0))
  expect_identical(wb$description[1], "'@Cafe Karori")
})

# ---------------------------------------------------------------------------
# F1-58: one fixed field set, so Qlik's wildcard LOAD concatenates cleanly.
# ---------------------------------------------------------------------------

test_that("the feed's core columns are identical and in order for every proven template", {
  ids <- sub("\\.ya?ml$", "", basename(list.files(templates_dir(), pattern = "\\.ya?ml$")))
  expect_true(length(ids) > 0)
  for (id in ids) {
    g <- fixture(file.path("tests", "testthat", "expected", paste0(id, ".csv")))
    if (!file.exists(g)) next
    core <- .feed_core_frame(display_transactions(read_core_csv(g), NULL))
    expect_identical(names(core)[seq_along(FEED_CORE_COLUMNS)], FEED_CORE_COLUMNS,
                     info = id)
    # debit/credit are always present, empty when the layout is signed-amount.
    expect_true(all(c("debit", "credit") %in% names(core)), info = id)
  }
})

test_that("templates with different extras still share one header prefix (wildcard-safe)", {
  fixed <- c(FEED_CONTEXT_COLUMNS, FEED_CORE_COLUMNS)
  head_of <- function(fx, tid) {
    out <- tempfile("cv_"); ld <- tempfile("l_")
    res <- convert_statement(fixture(fx), outdir = out, templates_dir = templates_dir(),
                             logdir = ld)
    cfg <- .cfg()
    write_feed(res, cfg, ts = "t", proven_ids = tid)
    f <- list.files(file.path(cfg$feed$feed_dir, "transactions"), full.names = TRUE)
    if (!length(f)) f <- list.files(file.path(cfg$feed$feed_dir, "review"), full.names = TRUE)
    names(.read_feed(f[1]))
  }
  # bnz has NO extras; anz_creditcard declares card / posted_date / fx_amount.
  h1 <- head_of("samples/raw/bnz/bnz_transaction_export_01.csv", "bnz_everyday_csv")
  h2 <- head_of("samples/raw/anz/anz_creditcard_01.csv", "anz_creditcard_csv")
  expect_identical(h1[seq_along(fixed)], fixed)
  expect_identical(h2[seq_along(fixed)], fixed)
  # the named extras are APPENDED and keep their names (never extra_1..n).
  expect_identical(setdiff(h1, fixed), character(0))
  expect_true(all(c("card", "posted_date", "fx_amount") %in% setdiff(h2, fixed)))
})

# ---------------------------------------------------------------------------
# F1-12: an accepted row and a withheld row are not interchangeable.
# ---------------------------------------------------------------------------

test_that("every feed row carries its own gate_result, accepted or withheld", {
  cfg <- .cfg()
  write_feed(.mk_result(), cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  acc <- .read_feed(list.files(file.path(cfg$feed$feed_dir, "transactions"), full.names = TRUE)[1])
  expect_true("gate_result" %in% names(acc))
  expect_identical(unique(acc$gate_result), "accepted")

  cfg2 <- .cfg()
  write_feed(.mk_result(status = "needs_review"), cfg2, ts = "t",
             proven_ids = "bnz_everyday_csv")
  rev <- .read_feed(list.files(file.path(cfg2$feed$feed_dir, "review"), full.names = TRUE)[1])
  expect_identical(unique(rev$gate_result), "withheld:needs_review")
  # the two tables are NOT field-identical by accident -- the verdict distinguishes
  # every row, so a wildcard concatenation cannot sum withheld money into a total.
  expect_false(identical(unique(acc$gate_result), unique(rev$gate_result)))
})

# ---------------------------------------------------------------------------
# F1-38 / F1-44: a feed that stops writing must never look like success.
# ---------------------------------------------------------------------------

test_that("a failed feed write is reported, never recorded as a clean accept", {
  cfg <- .cfg()
  dir.create(cfg$feed$feed_dir, recursive = TRUE, showWarnings = FALSE)
  # simulate the documented failure: the destination cannot be written (a UNC
  # share gone read-only). A plain FILE where the folder should be is the
  # portable equivalent -- root ignores permission bits, a non-directory never is.
  writeLines("not a directory", file.path(cfg$feed$feed_dir, "transactions"))

  g <- write_feed(.mk_result(), cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  expect_true(g$accept)                    # the gate DID accept it...
  expect_false(g$written)                  # ...and the feed did not receive it
  expect_identical(g$gate_result, "accepted:write_failed")
  man <- utils::read.csv(list.files(file.path(cfg$feed$feed_dir, "runs"), full.names = TRUE)[1],
                         stringsAsFactors = FALSE)
  expect_identical(man$gate_result, "accepted:write_failed")
  expect_true(is.na(man$feed_file))

  # and it is visible in the log, which lives with the app rather than on the
  # broken share -- the only place that can report the share being broken.
  fl <- read_feed_log(cfg$paths$logs)
  expect_equal(nrow(fl), 1L)
  expect_false(as.logical(fl$feed_written[1]))
  expect_true(nzchar(fl$detail[1]))        # WHY, not just that it failed
  expect_equal(nrow(feed_write_failures(fl)), 1L)
  expect_equal(feed_health(fl)$written[1], 0L)
})

test_that("a successful write happens BEFORE the stale sibling is removed", {
  # Ordering matters: unlinking first meant a transient write failure left the
  # statement with no row in either folder and nothing saying so.
  cfg <- .cfg()
  write_feed(.mk_result(), cfg, ts = "t1", proven_ids = "bnz_everyday_csv")
  tx <- file.path(cfg$feed$feed_dir, "transactions", "deadbeef01234567.csv")
  expect_true(file.exists(tx))
  before <- readLines(tx)

  # re-convert, still accepted: the file is replaced in place, never absent.
  write_feed(.mk_result(), cfg, ts = "t2", proven_ids = "bnz_everyday_csv")
  expect_true(file.exists(tx))
  expect_false(identical(before, readLines(tx)))   # genuinely rewritten (new ts)
})

test_that("the feed log records every conversion's feed outcome (health is visible)", {
  cfg <- .cfg()
  write_feed(.mk_result(), cfg, ts = "t1", proven_ids = "bnz_everyday_csv")
  r2 <- .mk_result(status = "needs_review"); r2$run_id <- "cafebabe02-2026"
  r2$header$source_sha256 <- "cafebabe0123456789"
  write_feed(r2, cfg, ts = "t2", proven_ids = "bnz_everyday_csv")
  fl <- read_feed_log(cfg$paths$logs)
  expect_equal(nrow(fl), 2L)
  hh <- feed_health(fl)
  expect_setequal(hh$gate_result, c("accepted", "withheld:needs_review"))
  expect_equal(sum(hh$written), 2L)          # both were written (one to review)
  expect_equal(nrow(feed_write_failures(fl)), 0L)
})

test_that("a statement with no content hash is refused loudly, not silently", {
  cfg <- .cfg()
  r <- .mk_result(); r$header$source_sha256 <- NA_character_
  expect_null(write_feed(r, cfg, ts = "t", proven_ids = "bnz_everyday_csv"))
  fl <- read_feed_log(cfg$paths$logs)
  expect_equal(nrow(fl), 1L)
  expect_identical(fl$gate_result[1], "skipped:no_source_hash")
})

# ---------------------------------------------------------------------------
# F1-16: auto-split bundles must not publish rows with no account / a bundle period.
# ---------------------------------------------------------------------------

test_that("split rows are stamped with THEIR statement's period and account", {
  rows <- .mk_rows(); rows$statement_index <- c(1L, 2L)
  r <- .mk_result(rows = rows)
  # exactly what R/split.R produces: the combined header has no account number and
  # the period spans the whole bundle.
  r$header$account_number <- NA_character_
  r$header$period_start <- "2026-01-01"; r$header$period_end <- "2026-12-31"
  r$metadata <- list(split = list(n_statements = 2L, statements = list(
    list(index = 1L, period_start = "2026-01-01", period_end = "2026-01-31",
         account_number = "12-3456-7890123-00"),
    list(index = 2L, period_start = "2026-02-01", period_end = "2026-02-28",
         account_number = "12-3456-7890123-01"))))
  cfg <- .cfg()
  write_feed(r, cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  fd <- .read_feed(list.files(file.path(cfg$feed$feed_dir, "transactions"), full.names = TRUE)[1])
  expect_identical(fd$statement_index, c("1", "2"))
  expect_identical(fd$period_start, c("2026-01-01", "2026-02-01"))
  expect_identical(fd$period_end, c("2026-01-31", "2026-02-28"))
  expect_identical(fd$account_number, c("12-3456-7890123-00", "12-3456-7890123-01"))
})

test_that("a split summary missing a field falls back to the header, never guesses", {
  rows <- .mk_rows(); rows$statement_index <- c(1L, 2L)
  r <- .mk_result(rows = rows)
  r$header$account_number <- NA_character_
  # today's R/split.R does not carry account_number per statement -- the feed must
  # leave it empty rather than borrow another statement's.
  r$metadata <- list(split = list(n_statements = 2L, statements = list(
    list(index = 1L, period_start = "2026-01-01", period_end = "2026-01-31"),
    list(index = 2L, period_start = "2026-02-01", period_end = "2026-02-28"))))
  cfg <- .cfg()
  write_feed(r, cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  fd <- .read_feed(list.files(file.path(cfg$feed$feed_dir, "transactions"), full.names = TRUE)[1])
  expect_identical(fd$period_start, c("2026-01-01", "2026-02-01"))
  expect_identical(fd$account_number, c("", ""))
})

# ---------------------------------------------------------------------------
# F1-45: which build produced this figure?
# ---------------------------------------------------------------------------

test_that("the manifest stamps the engine build and the template content hash", {
  fx <- fixture("samples/raw/bnz/bnz_transaction_export_01.csv")
  skip_if_not(file.exists(fx))
  out <- tempfile("cv_"); ld <- tempfile("l_")
  res <- convert_statement(fx, outdir = out, templates_dir = templates_dir(), logdir = ld)
  cfg <- .cfg()
  write_feed(res, cfg, ts = "t", proven_ids = "bnz_everyday_csv")
  man <- .read_feed(list.files(file.path(cfg$feed$feed_dir, "runs"), full.names = TRUE)[1])
  expect_identical(man$engine_version, engine_version())
  expect_true(nzchar(man$template_sha256) && !identical(man$template_sha256, "NA"))
  # the hash is of the template CONTENT, so an edited YAML is a different template
  # even at the same declared version.
  tpl <- load_templates(templates_dir())[["bnz_everyday_csv"]]
  expect_identical(man$template_sha256, template_sha256(tpl))
  edited <- tpl; edited$min_score <- 99
  expect_false(identical(template_sha256(edited), template_sha256(tpl)))
})
