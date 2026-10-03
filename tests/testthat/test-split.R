# Deterministic split of statement bundles (R/split.R).
# The forensic contract: split ONLY when the boundaries are independently confirmed
# (an independent count agrees with the "Page 1" markers). Otherwise the file is
# read whole, and a whole-file reading of something that looks like several
# statements never converts without a person. Each statement found is read and
# proven by the automatic reader on its own (R/convert.R).

# .sp_words -- two transaction rows (day/amount/balance) as positioned word boxes,
# with the month name supplied so a statement can sit in its own month.
.sp_words <- function(mon, d1, amt1, bal1, d2, amt2, bal2) {
  rows <- list(
    c("Date", 40, 10, 20), c("Amount", 340, 10, 40), c("Balance", 470, 10, 40),
    c(d1, 45, 40, 12), c(mon, 60, 40, 16), c("DESCA", 100, 40, 45), c(amt1, 345, 40, 34), c(bal1, 472, 40, 34),
    c(d2, 45, 70, 12), c(mon, 60, 70, 16), c("DESCB", 100, 70, 45), c(amt2, 345, 70, 34), c(bal2, 472, 70, 34))
  data.frame(text = vapply(rows, `[`, "", 1),
    x = as.numeric(vapply(rows, `[`, "", 2)), y = as.numeric(vapply(rows, `[`, "", 3)),
    width = as.numeric(vapply(rows, `[`, "", 4)), height = rep(10, length(rows)),
    stringsAsFactors = FALSE)
}

# A clean 2-statement bundle: distinct periods, a "Page 1 of 1" per statement, and
# a running balance that ties out within each statement.
.sp_bundle <- function() list(kind = "pdf", path = tempfile(fileext = ".pdf"),
  pages = c("Statement period from 1 Jan 2026 to 31 Jan 2026  Page 1 of 1",
            "Statement period from 1 Feb 2026 to 28 Feb 2026  Page 1 of 1"),
  words = list(.sp_words("Jan", "05", "-4.50", "95.50", "06", "1000.00", "1095.50"),
               .sp_words("Feb", "03", "-50.00", "1045.50", "10", "200.00", "1245.50")),
  page_width = c(595.28, 595.28), page_height = c(841.89, 841.89),
  meta = list(page_count = 2L))

# .sp_reading(dates, amounts, period, outcome) -- one statement's reading, in the
# shape auto_read() returns, so the combining is tested on its own.
.sp_reading <- function(dates, amounts, period = c("1 Jan 2026", "31 Jan 2026"), outcome = "proven",
                        trust = "high") {
  tx <- data.frame(row_id = seq_along(dates), date = dates, description = rep("X", length(dates)),
                   amount = amounts, flags = rep("", length(dates)), stringsAsFactors = FALSE)
  list(outcome = outcome, why = sprintf("read as %s", outcome), matched_layout = NULL,
       transactions = tx,
       parsed = list(transactions = tx, extras = NULL,
                     header = list(period_start = period[1], period_end = period[2],
                                   account_number = "acct", opening_balance = 1, closing_balance = 2),
                     provenance = data.frame(row_id = seq_along(dates), source_ref = rep("pdf:p1", length(dates)),
                                             stringsAsFactors = FALSE)),
       recon = list(kpis = data.frame(name = "balance_reconciliation", status = "pass", stringsAsFactors = FALSE),
                    trust = list(level = trust, score = 90, completeness_verified = TRUE)))
}

test_that("a confirmed bundle is cut at its Page 1 pages", {
  segs <- bundle_segments(.sp_bundle())
  expect_identical(segs, list(1L, 2L))
})

test_that("a single statement is never split", {
  one <- .sp_bundle()
  one$pages <- one$pages[1]; one$words <- one$words[1]
  one$page_width <- one$page_width[1]; one$page_height <- one$page_height[1]
  one$meta$page_count <- 1L
  expect_null(bundle_segments(one))
})

test_that("unconfirmed boundaries are refused", {
  # THE forensic guard: two pages with the SAME period (n_periods = 1) so no
  # independent count corroborates a 2-way split -- and a continuous running
  # balance would add up across any cut. Only an independent count may commit it.
  same_period <- .sp_bundle()
  same_period$pages <- c("Statement period from 1 Jan 2026 to 31 Jan 2026 Page 1 of 1",
                         "Statement period from 1 Jan 2026 to 31 Jan 2026 Page 1 of 1")
  expect_null(bundle_segments(same_period))
  # ...and a CSV is never cut
  expect_null(bundle_segments(list(kind = "delimited", lines = c("a,b", "1,2"))))
})

test_that(".segment_starts finds page-1 markers and always includes page 1", {
  expect_equal(.segment_starts(.sp_bundle()), c(1L, 2L))
  # a marker only on a later page still makes page 1 the first segment start
  inp <- .sp_bundle(); inp$pages[1] <- "Statement period from 1 Jan 2026 to 31 Jan 2026 (no marker)"
  expect_equal(.segment_starts(inp), c(1L, 2L))
})

test_that(".subinput_pages yields a standalone one-statement input that knows its pages", {
  si <- .subinput_pages(.sp_bundle(), 2L)
  expect_equal(length(si$pages), 1L)
  expect_equal(si$meta$page_count, 1L)
  expect_match(si$pages[1], "Feb")
  # where the page sits in the file, for anything that reads it again from the picture
  expect_identical(si$page_map, 2L)
  expect_identical(.subinput_pages(si, 1L)$page_map, 2L)
  expect_identical(.ar_file_page(si, 1L), 2L)
})

test_that("each statement's rows are kept, tagged, and the trust is the weakest", {
  rd <- list(.sp_reading(c("2026-01-05", "2026-01-06"), c(-4.5, 1000)),
             .sp_reading(c("2026-02-03", "2026-02-10"), c(-50, 200), c("1 Feb 2026", "28 Feb 2026"),
                         outcome = "check", trust = "medium"))
  cb <- bundle_combine(rd, list(1L, 2L), 2L)
  expect_equal(cb$n_statements, 2L)
  tx <- cb$parsed$transactions
  expect_equal(tx$statement_index, c(1, 1, 2, 2))
  expect_equal(tx$row_id, 1:4)
  expect_identical(vapply(cb$statements, `[[`, "", "outcome"), c("proven", "check"))
  expect_identical(cb$recon$trust$level, "medium")
  expect_true(all(grepl("\\[statement [12]\\]$", cb$recon$kpis$name)))
  # per-statement identity fields are emptied in the combined header (the feed
  # stamps header onto every row, so one account/balance must not mislabel others)
  expect_true(is.na(cb$parsed$header$account_number))
  expect_true(is.na(cb$parsed$header$opening_balance))
  expect_equal(cb$parsed$header$page_count, 2L)
  # the periods join up, so the whole span is published
  expect_identical(cb$parsed$header$period_start, "1 Jan 2026")
  expect_identical(cb$parsed$header$period_end, "28 Feb 2026")
})

test_that("a statement that read nothing keeps its place, and says so", {
  none <- .sp_reading(character(0), numeric(0), outcome = "unread")
  none$parsed <- NULL; none$recon <- NULL
  cb <- bundle_combine(list(.sp_reading("2026-01-05", -4.5), none), list(1L, 2L), 2L)
  expect_equal(nrow(cb$parsed$transactions), 1L)
  expect_identical(cb$statements[[2]]$outcome, "unread")
  expect_equal(cb$statements[[2]]$rows, 0L)
  expect_identical(cb$recon$trust$level, "low")
})

# The combined header used to take statement 1's period_start and statement k's
# period_end IN FILE ORDER. Bank bundles are routinely filed newest-first, and a
# period that ENDS BEFORE IT STARTS reached the governed feed's run manifest as
# "accepted". Date order fixes the direction; a bundle that does not JOIN UP
# publishes no period at all, and says why.
test_that("a bundle's period is the true span in DATE order, not file order", {
  rd <- list(.sp_reading("2026-02-03", -50, c("1 Feb 2026", "28 Feb 2026")),
             .sp_reading("2026-01-05", -4.5, c("1 Jan 2026", "31 Jan 2026")))
  h <- bundle_combine(rd, list(1L, 2L), 2L)$parsed$header
  expect_identical(h$period_start, "1 Jan 2026")
  expect_identical(h$period_end, "28 Feb 2026")
})

test_that("a bundle with a HOLE in it publishes no period, and names the hole", {
  rd <- list(.sp_reading("2026-01-05", -4.5, c("1 Jan 2026", "31 Jan 2026")),
             .sp_reading("2026-02-05", -50, c("3 Feb 2026", "28 Feb 2026")))
  cb <- bundle_combine(rd, list(1L, 2L), 2L)
  expect_true(is.na(cb$parsed$header$period_start))
  why <- paste(cb$recon$trust$reasons, collapse = " | ")
  expect_match(why, "2026-02-01 to 2026-02-02", fixed = TRUE)
  expect_match(why, "a statement is missing", fixed = TRUE)
  expect_identical(cb$statements[[2]]$period_start, "3 Feb 2026")
})

test_that("overlapping statements publish no period, and are named as an overlap", {
  rd <- list(.sp_reading("2026-01-05", -4.5, c("1 Jan 2026", "20 Feb 2026")),
             .sp_reading("2026-02-05", -50, c("1 Feb 2026", "28 Feb 2026")))
  cb <- bundle_combine(rd, list(1L, 2L), 2L)
  expect_true(is.na(cb$parsed$header$period_start))
  expect_match(paste(cb$recon$trust$reasons, collapse = " | "), "overlap in time", fixed = TRUE)
})

test_that("a shared boundary day is NOT an overlap -- banks print them", {
  # "31 Dec to 31 Jan" then "31 Jan to 28 Feb" is the commonest real multi-period
  # shape there is. .period_span already refuses to call it an overlap; this must
  # agree, or the engine holds two opinions about when periods join up.
  p <- .bundle_period(list(
    list(period_start = "31 Dec 2025", period_end = "31 Jan 2026"),
    list(period_start = "31 Jan 2026", period_end = "28 Feb 2026")))
  expect_identical(p$start, "31 Dec 2025")
  expect_identical(p$end, "28 Feb 2026")
  expect_true(is.na(p$why))
})

test_that(".bundle_period refuses when a statement's own period is unreadable", {
  # One unreadable bound means the ORDER of the whole set is unknown, and an
  # unknown order is exactly when a guess produces a wrong pairing.
  p <- .bundle_period(list(
    list(period_start = "1 Jan 2026", period_end = "31 Jan 2026"),
    list(period_start = NA_character_, period_end = "28 Feb 2026")))
  expect_true(is.na(p$start))
  expect_match(p$why, "readable period", fixed = TRUE)
})

test_that(".bundle_period refuses a statement whose OWN period runs backwards", {
  p <- .bundle_period(list(list(period_start = "28 Feb 2026", period_end = "1 Feb 2026")))
  expect_true(is.na(p$start))
  expect_match(p$why, "ends before it starts", fixed = TRUE)
})

test_that("a backwards bundle period cannot be published, even if one is built", {
  # THE GUARD, tested by defeating the thing it guards: stub the span builder into
  # producing a backwards period and the combining must refuse rather than publish
  # it (convert_statement's funnel then fails the run, never an extract).
  backwards <- function(statements) list(start = "20 Apr 2026", end = "19 Feb 2026",
                                         why = NA_character_)
  where <- environment(bundle_combine)
  orig <- get(".bundle_period", envir = where)
  on.exit(assign(".bundle_period", orig, envir = where), add = TRUE)
  assign(".bundle_period", backwards, envir = where)
  rd <- list(.sp_reading("2026-01-05", -4.5), .sp_reading("2026-02-05", -50, c("1 Feb 2026", "28 Feb 2026")))
  expect_error(bundle_combine(rd, list(1L, 2L), 2L), "backwards")
  assign(".bundle_period", orig, envir = where)
  expect_false(is.null(bundle_combine(rd, list(1L, 2L), 2L)))
})

test_that("a real two-statement PDF is converted statement by statement", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  f <- fixture("tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf")
  skip_if_not(file.exists(f))
  input <- read_input(f)
  segs <- bundle_segments(input)
  expect_length(segs, 2L)
  # and a genuine single statement is never split
  expect_null(bundle_segments(read_input(fixture("tests/testthat/fixtures/anz_everyday_pdf_sample.pdf"))))
})
