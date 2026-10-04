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
    reason = c("No table", "No table", "Nothing adds up", "", "No table"),
    source_file = c("a.pdf","b.pdf","c.csv","d.csv","e.pdf"),
    ts = c("2026-01-01","2026-01-02","2026-01-03","2026-01-04","2026-01-05"),
    stringsAsFactors = FALSE)
  cl <- unsupported_clusters(runs)
  expect_equal(nrow(cl), 2L)                       # sigA (3: 2 unsupported + 1 failed) + sigB (1)
  expect_equal(cl$count[1], 3L)                    # sigA ranked first
  expect_equal(cl$signature[1], "sigA")
  expect_equal(cl$why[1], "No table")              # the commonest reason in the cluster
})

test_that("layout_usage summarises runs per learned layout and flagged feedback", {
  runs <- data.frame(
    status = c("ok","needs_review","ok","unsupported"),
    layout = c("bnz_1@2","bnz_1@3","asb_1@1", NA),
    trust_level = c("high","low","medium", NA),
    stringsAsFactors = FALSE)
  fb <- data.frame(template_id = c("bnz_1@2","bnz_1@3","asb_1@1"),
                   flagged = c(TRUE, FALSE, TRUE), stringsAsFactors = FALSE)
  tu <- layout_usage(runs, fb)
  bnz <- tu[tu$layout == "bnz_1", ]                # a layout's versions are one layout
  expect_equal(bnz$n, 2L)
  expect_equal(bnz$needs_review, 1L)
  expect_equal(bnz$low_trust, 1L)
  expect_equal(bnz$flagged_feedback, 1L)
  expect_false(any(is.na(tu$layout)))              # the unread run (no layout) is excluded
})

test_that("analytics functions are safe on empty logs", {
  e <- data.frame()
  expect_equal(nrow(runs_overview(e)), 0L)
  expect_equal(nrow(unsupported_clusters(e)), 0L)
  expect_equal(nrow(layout_usage(e)), 0L)
  expect_equal(nrow(layout_drift(e)), 0L)
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
  # a CSV the reader cannot read as a statement -> unsupported, logged
  f <- file.path(tempdir(), "weird_unknown_layout.csv")
  writeLines(c("Wibble,Wobble,Splunge", "1,2,3"), f)
  convert_statement(f, outdir = out, logdir = ld, layouts_dir = tempfile("al_ly_"), tracking_dir = NA)
  runs <- read_runs(ld)
  expect_true(nrow(runs) >= 1)
  cl <- unsupported_clusters(runs)
  expect_true(nrow(cl) >= 1)
  expect_true(cl$count[1] >= 1)
  expect_true(nzchar(cl$why[1]))                   # the reader's reason
  expect_identical(cl$example_file[1], basename(f))
  # No heading row was found, so no hint is kept: the first line of a file the
  # reader could not read may be a preamble naming the account and its holder.
  expect_false(grepl("wibble", tolower(paste(cl$layout[1])), fixed = TRUE))
})

# N227: every unread file used to cluster as ONE "(unknown)" layout, because an
# unread run carried no layout signature. It now carries a structural fingerprint
# (kind, page band, producer, column shape, field count -- no content).
test_that("unread files are grouped by their shape, not all as one unknown", {
  skip_if_not(requireNamespace("jsonlite", quietly = TRUE))
  ld <- tempfile("al_"); out <- tempfile("ao_"); src <- tempfile("as_"); dir.create(src)
  put <- function(name, lines) { f <- file.path(src, name); writeLines(lines, f); f }
  files <- c(put("three_a.csv", c("Mr A Person,Acct 12-3456-7654321-00,x", "1,2,3", "4,5,6")),
             put("three_b.csv", c("Ms B Other,Acct 12-3456-1234567-00,y", "7,8,9", "1,2,3")),
             put("five.csv", c("a,b,c,d,e", "1,2,3,4,5")))
  for (f in files) convert_statement(f, outdir = out, logdir = ld, layouts_dir = tempfile("al_ly_"), tracking_dir = NA)
  runs <- read_runs(ld)
  expect_true(all(runs$status == "unsupported"))
  expect_false(anyNA(runs$layout_signature))
  sig <- stats::setNames(runs$layout_signature, runs$source_file)
  expect_identical(sig[["three_a.csv"]], sig[["three_b.csv"]])        # one shape, one row
  expect_false(identical(sig[["three_a.csv"]], sig[["five.csv"]]))
  cl <- unsupported_clusters(runs)
  expect_identical(sort(cl$count), c(1L, 2L))
  expect_match(cl$layout[cl$count == 2L], "CSV, 3 fields a row", fixed = TRUE)
  # no content: neither the holder's name nor any digit of the account number
  txt <- tolower(paste(c(cl$layout, runs$layout_hint), collapse = " "))
  for (w in c("person", "other", "7654321", "1234567", "acct")) expect_false(grepl(w, txt, fixed = TRUE), info = w)

  # runs logged before the fingerprint (no signature) group by kind and pages
  old <- data.frame(status = "unsupported", layout_signature = NA_character_, layout_hint = NA_character_,
                    file_kind = c("pdf", "pdf", "scan", "delimited"), pages = c(2L, 3L, 12L, NA),
                    reason = "x", source_file = c("a.pdf", "b.pdf", "c.pdf", "d.csv"),
                    ts = sprintf("2026-01-0%d", 1:4), stringsAsFactors = FALSE)
  co <- unsupported_clusters(old)
  expect_identical(nrow(co), 3L)
  expect_identical(co$count[1], 2L)
  expect_identical(co$layout[1], "PDF, 2-3 pages - no layout recorded")
  expect_true("scanned PDF, over 10 pages - no layout recorded" %in% co$layout)
  expect_true("CSV - no layout recorded" %in% co$layout)
})

test_that("a PDF's fingerprint names its kind, page band, maker and found columns", {
  input <- list(kind = "pdf", meta = list(pdf_doc = list(producer = "Skia/PDF m141")))
  cols <- list(data.frame(page = c(1L, 1L, 1L, 2L), kind = c("money", "date", "text", "date"),
                          x_min = c(400, 30, 90, 30), stringsAsFactors = FALSE))
  a <- unread_fingerprint(input, "pdf", 4L, cols, list(signature = "empty", hint = ""))
  expect_match(a$signature, "^[0-9a-f]{12}$")
  expect_identical(a$hint, "PDF, 4-10 pages, made by skia pdf, columns found: date text figure")
  # the same design, a longer statement in the same band: the same group
  expect_identical(unread_fingerprint(input, "pdf", 9L, cols, NULL)$signature, a$signature)
  # another maker, or no columns found: another group
  expect_false(identical(unread_fingerprint(list(kind = "pdf", meta = list(pdf_doc = list(producer = "iText 5.5"))),
                                            "pdf", 4L, cols, NULL)$signature, a$signature))
  expect_match(unread_fingerprint(input, "scan", 1L, list(), NULL)$hint, "scanned PDF, 1 page, made by skia pdf, no columns found")
})

# ---------------------------------------------------------------------------
# L6. ADMIN'S DRIFT AND USAGE TABLES WERE STATEMENT-SHAPED.
#
# Health was one statement-shaped test -- status ok AND no failed check AND trust
# is not low -- and a report carries no trust level at all, so `trust != "low"`
# was NA, the whole vector was NA, both percentages were NA, and Admin rendered
# one row of NAs for any other-route row with six or more runs. A screen that says
# nothing is worse than one that says it is fine: the admin reads it as "nothing
# to see" and it means "never measured".
# ---------------------------------------------------------------------------

# .an_runs(kind, ...) -- n run records for one layout, of one kind.
.an_runs <- function(kind, status, n = 10, ...) {
  d <- data.frame(layout = rep("t_1@1", n),
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

test_that("a statement logged before automatic reading is judged as it was then", {
  s <- data.frame(layout = rep("t_1@1", 3), ts = c("a", "b", "c"),
                  status = c("ok", "ok", "needs_review"),
                  kpi_fail_count = c(0L, 1L, 0L),
                  trust_level = c("high", "high", "low"), stringsAsFactors = FALSE)
  expect_identical(run_healthy(s), c(TRUE, FALSE, FALSE))
  # and with no `kind` column at all -- every record written before today
  expect_false("kind" %in% names(s))
})

test_that("an automatic reading is healthy only when the arithmetic proved it", {
  s <- data.frame(layout = rep("t_1@1", 4), ts = c("a", "b", "c", "d"),
                  status = c("ok", "ok", "ok", "needs_review"),
                  outcome = c("proven", "layout_match", "check", "check"),
                  kpi_fail_count = 0L, trust_level = "high", stringsAsFactors = FALSE)
  # the third was ok because a person confirmed it: not the layout's own health
  expect_identical(run_healthy(s), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("layout_drift reports a drifting layout instead of a row of NAs", {
  # six clean runs then four where the tables stopped being found by heading
  r <- .an_runs("tables", c(rep("ok", 6), rep("needs_review", 4)),
                unclaimed_words = c(rep(0L, 6), rep(4L, 4)),
                weak_tables = c(rep(0L, 6), rep(2L, 4)))
  d <- layout_drift(r, recent_frac = 0.4, min_runs = 6)
  expect_equal(nrow(d), 1L)
  expect_identical(d$layout[1], "t_1")
  expect_false(anyNA(d))                       # THE FAULT: every cell was NA
  expect_equal(d$earlier_ok_pct[1], 100)
  expect_equal(d$recent_ok_pct[1], 0)
  expect_true(d$drop[1] >= 25)
})

test_that("a healthy layout is not reported as drifting", {
  r <- .an_runs("tables", rep("ok", 10), unclaimed_words = rep(0L, 10),
                weak_tables = rep(0L, 10))
  expect_equal(nrow(layout_drift(r)), 0L)
})

test_that("one report run does not turn a layout's low-trust count into NA", {
  # `NA == "low"` is NA, so a single form or report run -- neither of which
  # records a trust level, because neither has any reconciliation to be confident
  # about -- made this whole count NA and Admin printed a row that said nothing.
  mixed <- data.frame(
    layout = c("t_1@1", "t_1@1", "t_1@2"), ts = c("a", "b", "c"),
    kind = c("statement", "statement", "tables"),
    status = c("ok", "needs_review", "ok"),
    trust_level = c("high", "low", NA_character_), stringsAsFactors = FALSE)
  u <- layout_usage(mixed)
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
