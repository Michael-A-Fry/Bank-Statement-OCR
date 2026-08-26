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

# ---------------------------------------------------------------------------
# L6. ADMIN'S DRIFT AND USAGE TABLES WERE STATEMENT-SHAPED.
#
# Health was one statement-shaped test -- status ok AND no failed check AND trust
# is not low -- and a report carries no trust level at all, so `trust != "low"`
# was NA, the whole vector was NA, both percentages were NA, and Admin rendered
# one row of NAs for any other-route template with six or more runs. A screen
# that says nothing about a template is worse than one that says it is fine: the
# admin reads it as "nothing to see" and it means "never measured".
# ---------------------------------------------------------------------------

# .an_runs(kind, ...) -- n run records for one template, of one kind.
.an_runs <- function(kind, status, n = 10, ...) {
  d <- data.frame(detected_template = rep("t1", n),
                  ts = sprintf("2026-03-%02dT00:00:00Z", seq_len(n)),
                  kind = rep(kind, n), status = status,
                  trust_level = rep(NA_character_, n),
                  kpi_fail_count = rep(NA_integer_, n),
                  stringsAsFactors = FALSE)
  extra <- list(...)
  for (k in names(extra)) d[[k]] <- extra[[k]]
  d
}

test_that("run_healthy is never NA, whatever route the run took", {
  rep_runs <- .an_runs("tables", rep("ok", 6), n = 6,
                       unclaimed_words = rep(0L, 6), weak_tables = rep(0L, 6))
  expect_false(anyNA(run_healthy(rep_runs)))
  expect_true(all(run_healthy(rep_runs)))
  # a report is healthy when every table was found by its heading and no word
  # fell outside a column -- NOT merely when the status is ok, which tolerates
  # one unclaimed word.
  spilt <- rep_runs; spilt$unclaimed_words <- rep(1L, 6)
  expect_false(any(run_healthy(spilt)))
  weak <- rep_runs; weak$weak_tables <- rep(2L, 6)
  expect_false(any(run_healthy(weak)))
  # a form: nothing disputed, nothing required missing
  frm <- .an_runs("form", rep("ok", 4), n = 4, n_conflicts = rep(0L, 4),
                  required_missing = rep(0L, 4))
  expect_true(all(run_healthy(frm)))
  frm$n_conflicts <- rep(3L, 4)
  expect_false(any(run_healthy(frm)))
})

test_that("a statement's health test is exactly what it always was", {
  # The only route with arithmetic behind it: its bar must not move because two
  # other routes arrived.
  s <- data.frame(detected_template = rep("t1", 3), ts = c("a", "b", "c"),
                  status = c("ok", "ok", "needs_review"),
                  kpi_fail_count = c(0L, 1L, 0L),
                  trust_level = c("high", "high", "low"), stringsAsFactors = FALSE)
  expect_identical(run_healthy(s), c(TRUE, FALSE, FALSE))
  # and with no `kind` column at all -- every record written before today
  expect_false("kind" %in% names(s))
})

test_that("template_drift reports a report template instead of a row of NAs", {
  # six clean runs then four where the tables stopped being found by heading
  r <- .an_runs("tables", c(rep("ok", 6), rep("needs_review", 4)),
                unclaimed_words = c(rep(0L, 6), rep(4L, 4)),
                weak_tables = c(rep(0L, 6), rep(2L, 4)))
  d <- template_drift(r, recent_frac = 0.4, min_runs = 6)
  expect_equal(nrow(d), 1L)
  expect_identical(d$template[1], "t1")
  expect_false(anyNA(d))                       # THE FAULT: every cell was NA
  expect_equal(d$earlier_ok_pct[1], 100)
  expect_equal(d$recent_ok_pct[1], 0)
  expect_true(d$drop[1] >= 25)
})

test_that("a healthy report template is not reported as drifting", {
  r <- .an_runs("tables", rep("ok", 10), unclaimed_words = rep(0L, 10),
                weak_tables = rep(0L, 10))
  expect_equal(nrow(template_drift(r)), 0L)
})

test_that("one report run does not turn a template's low-trust count into NA", {
  # `NA == "low"` is NA, so a single form or report run -- neither of which
  # records a trust level, because neither has any reconciliation to be confident
  # about -- made this whole count NA and Admin printed a row that said nothing.
  mixed <- data.frame(
    detected_template = c("t1", "t1", "t1"), ts = c("a", "b", "c"),
    kind = c("statement", "statement", "tables"),
    status = c("ok", "needs_review", "ok"),
    trust_level = c("high", "low", NA_character_), stringsAsFactors = FALSE)
  u <- template_usage(mixed)
  expect_equal(nrow(u), 1L)
  expect_false(anyNA(u))
  expect_equal(u$low_trust[1], 1L)         # the one statement that WAS graded low
  expect_equal(u$n[1], 3L)
})

test_that("the layout hint on a page with no transaction header names nobody", {
  # G9. The signature's fallback was the twelve most frequent long non-stopword
  # words on the page. A statement takes the header-keyword branch and never gets
  # there; a document that reaches the form or report route is BY DEFINITION one
  # with no transaction header, so it is exactly the class that takes the
  # fallback -- and the commonest words on such a page are the people named on
  # it. Measured before this: "ambrose | whitcombe", written to logs/runs and to
  # logs/metadata, both kept after the uploaded file itself is purged, while the
  # metadata module's own header states that names are never stored.
  p <- list(kind = "pdf", pages = paste(c(
    "NORTHWIND TRUSTEE SERVICES",
    "Consolidated position report",
    "Prepared for Ambrose Family Trust",
    "Trustee Whitcombe Ambrose",
    "Ambrose Whitcombe Ambrose Whitcombe",
    "Ambrose Whitcombe holdings schedule"), collapse = "\n"))
  h <- layout_signature(p)$hint
  expect_false(grepl("ambrose", h, fixed = TRUE))
  expect_false(grepl("whitcombe", h, fixed = TRUE))
  expect_false(grepl("northwind", h, fixed = TRUE))

  # ...and what IS layout vocabulary still comes through, so two copies of one
  # report family still cluster together -- which is the only reason the hint
  # exists.
  q <- list(kind = "pdf", pages = paste(c(
    "Opening balance Closing balance",
    "Prepared for Ambrose Family Trust",
    "Type Opening Closing"), collapse = "\n"))
  r <- list(kind = "pdf", pages = paste(c(
    "Opening balance Closing balance",
    "Prepared for Delacroix Family Trust",
    "Type Opening Closing"), collapse = "\n"))
  expect_true(nzchar(layout_signature(q)$hint))
  expect_false(grepl("ambrose", layout_signature(q)$hint, fixed = TRUE))
  expect_identical(layout_signature(q)$signature, layout_signature(r)$signature)
})

test_that("a statement's layout hint is unchanged by the PII-safe fallback", {
  p <- list(kind = "pdf", pages = paste(
    "Date Description Withdrawals Deposits Balance",
    "01/04/2025 COFFEE 4.50 1,000.00", sep = "\n"))
  expect_identical(layout_signature(p)$hint,
                   "balance | date | deposits | description | withdrawals")
})
