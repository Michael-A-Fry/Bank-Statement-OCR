# Tests for layout signatures + log analytics (Admin panel brains).

test_that("same delimited layout clusters; different layout doesn't", {
  a <- list(kind = "delimited", lines = c("Date,Amount,Payee", "1/1/25,-5,Shop"))
  b <- list(kind = "delimited", lines = c("Date,Amount,Payee", "2/2/25,9,Cafe"))   # same header
  c <- list(kind = "delimited", lines = c("Txn Date,Debit,Credit,Details", "x"))    # different
  expect_equal(layout_signature(a)$signature, layout_signature(b)$signature)
  expect_false(identical(layout_signature(a)$signature, layout_signature(c)$signature))
  expect_match(layout_signature(a)$hint, "amount")
})

test_that("pdf layout signature is robust to per-customer content (names/amounts)", {
  # two different customers, same bank layout -> recurring labels dominate
  p1 <- list(kind = "pdf", pages = paste(
    "Kowhai Bank Statement account transaction details withdrawals deposits balance",
    "ALICE EXAMPLE 12 Road 05 May COFFEE 4.50 100.00"))
  p2 <- list(kind = "pdf", pages = paste(
    "Kowhai Bank Statement account transaction details withdrawals deposits balance",
    "BOB SAMPLE 9 Street 06 Jun SALARY 3200.00 4444.50"))
  expect_equal(layout_signature(p1)$signature, layout_signature(p2)$signature)
})

test_that("runs_overview counts by status", {
  runs <- data.frame(status = c("ok","ok","unsupported","needs_review","failed"),
                     stringsAsFactors = FALSE)
  ov <- runs_overview(runs)
  expect_equal(ov$n[ov$status == "ok"], 2L)
  expect_equal(sum(ov$n), 5L)
  expect_equal(ov$status[1], "ok")   # ranked by count
})

test_that("unsupported_clusters groups by signature and ranks by count", {
  runs <- data.frame(
    status = c("unsupported","unsupported","unsupported","ok","failed"),
    layout_signature = c("sigA","sigA","sigB","sigX","sigA"),
    layout_hint = c("date | amount","date | amount","txn | debit | credit","x","date | amount"),
    closest_template = c("anz_everyday_csv","anz_everyday_csv","asb_everyday_csv","ok_tmpl","anz_everyday_csv"),
    detect_detail = rep("closest ... (missing 'Card')", 5),
    source_file = c("a.pdf","b.pdf","c.csv","d.csv","e.pdf"),
    ts = c("2026-01-01","2026-01-02","2026-01-03","2026-01-04","2026-01-05"),
    stringsAsFactors = FALSE)
  cl <- unsupported_clusters(runs)
  expect_equal(nrow(cl), 2L)                       # sigA (3: 2 unsupported + 1 failed) + sigB (1)
  expect_equal(cl$count[1], 3L)                    # sigA ranked first
  expect_equal(cl$signature[1], "sigA")
  expect_equal(cl$closest_template[1], "anz_everyday_csv")
})

test_that("template_usage summarises matched runs and flagged feedback", {
  runs <- data.frame(
    status = c("ok","needs_review","ok","unsupported"),
    detected_template = c("bnz_everyday_csv","bnz_everyday_csv","asb_everyday_csv", NA),
    trust_level = c("high","low","medium", NA),
    stringsAsFactors = FALSE)
  fb <- data.frame(template_id = c("bnz_everyday_csv","bnz_everyday_csv","asb_everyday_csv"),
                   flagged = c(TRUE, FALSE, TRUE), stringsAsFactors = FALSE)
  tu <- template_usage(runs, fb)
  bnz <- tu[tu$template == "bnz_everyday_csv", ]
  expect_equal(bnz$n, 2L)
  expect_equal(bnz$needs_review, 1L)
  expect_equal(bnz$low_trust, 1L)
  expect_equal(bnz$flagged_feedback, 1L)
  expect_false(any(is.na(tu$template)))            # the unsupported (NA template) is excluded
})

test_that("analytics functions are safe on empty logs", {
  e <- data.frame()
  expect_equal(nrow(runs_overview(e)), 0L)
  expect_equal(nrow(unsupported_clusters(e)), 0L)
  expect_equal(nrow(template_usage(e)), 0L)
  expect_equal(nrow(feed_health(e)), 0L)
  expect_equal(nrow(feed_write_failures(e)), 0L)
  expect_equal(nrow(read_feed_log(tempfile("nofeed_"))), 0L)
})

# ---------------------------------------------------------------------------
# F1-38 / F1-44: feed health. A UNC share going read-only used to mean every
# conversion still reported success while the feed silently stopped for ever.
# ---------------------------------------------------------------------------

test_that("feed_health rolls the feed log up by verdict, and names the failures", {
  fl <- data.frame(
    ts = c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04"),
    run_id = c("r1", "r2", "r3", "r4"),
    source_file = c("a.csv", "b.csv", "c.pdf", "d.pdf"),
    gate_result = c("accepted", "accepted", "accepted:write_failed",
                    "withheld:needs_review"),
    feed_written = c(TRUE, TRUE, FALSE, TRUE),
    feed_dir = rep("//fileserver/share/feed", 4),
    stringsAsFactors = FALSE)
  hh <- feed_health(fl)
  expect_equal(hh$n[hh$gate_result == "accepted"], 2L)
  expect_equal(hh$written[hh$gate_result == "accepted"], 2L)
  expect_equal(hh$written[hh$gate_result == "accepted:write_failed"], 0L)

  wf <- feed_write_failures(fl)
  expect_equal(nrow(wf), 1L)
  expect_identical(wf$run_id, "r3")
  expect_identical(wf$source_file, "c.pdf")
  expect_identical(wf$feed_dir, "//fileserver/share/feed")   # where to go and look
})

test_that("feed_health treats a healthy log as having no failures", {
  fl <- data.frame(ts = "2026-01-01", run_id = "r1", gate_result = "accepted",
                   feed_written = TRUE, stringsAsFactors = FALSE)
  expect_equal(nrow(feed_write_failures(fl)), 0L)
  expect_equal(feed_health(fl)$written, 1L)
})

test_that("end-to-end: a converted unsupported file is reportable from the log", {
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  ld <- tempfile("al_"); out <- tempfile("ao_")
  # a CSV that matches no template -> unsupported, logged
  f <- file.path(tempdir(), "weird_unknown_layout.csv")
  writeLines(c("Wibble,Wobble,Splunge", "1,2,3"), f)
  convert_statement(f, outdir = out, templates_dir = templates_dir(), logdir = ld)
  runs <- read_runs(ld)
  expect_true(nrow(runs) >= 1)
  cl <- unsupported_clusters(runs)
  expect_true(nrow(cl) >= 1)
  expect_true(cl$count[1] >= 1)
  expect_match(cl$layout[1], "wibble")             # layout hint from the header
})
