# Tests for identify_file() / template_choices() (R/identify.R) -- the Convert
# table: one row per uploaded file, its type, and the template it will be read with.
#
# The failure these exist to prevent above all others: THE TABLE SAYING ONE
# TEMPLATE AND THE CONVERSION USING ANOTHER. A row left on its guess is converted by
# ordinary detection, so the guess has to BE detection's answer, file for file.

.id_tset <- function() load_template_set(templates_dir(), "does_not_exist")
.id_conv <- function(p, ...) {
  od <- tempfile(); dir.create(od)
  suppressWarnings(convert_statement(p, outdir = od, logdir = od,
    templates_dir = templates_dir(), user_templates_dir = "does_not_exist",
    formats = "csv", log = FALSE, ...))
}
.id_csv <- function(name, lines) { d <- tempfile(); dir.create(d); p <- file.path(d, name); writeLines(lines, p); p }
.ID_ANZ <- c(
  "Type,Details,Particulars,Code,Reference,Amount,Date,ForeignCurrencyAmount,ConversionCharge",
  "Visa Purchase,Acme Inc,Acme LLB Inc,Smith Vj,,-23.40,19/06/2014,,",
  "Credit,Payroll Ltd,Salary,,,2000.00,19/06/2014,,")

test_that("the guess is the template the conversion then uses, file for file", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  ts <- .id_tset()
  fx <- file.path(engine_root(), "tests", "testthat", "fixtures")
  files <- c(file.path(fx, c("anz_everyday_pdf_sample.pdf", "asb_everyday_pdf_sample.pdf",
                             "westpac_everyday_pdf_sample.pdf", "anz_creditcard_fx.csv",
                             "kiwibank_broken_balance.csv", "bnz_embedded_newline.csv")),
             .id_csv("anz.csv", .ID_ANZ))
  for (p in files) {
    skip_if_not(file.exists(p), p)
    id <- identify_file(p, ts, basename(p))
    r <- .id_conv(p)
    expect_true(id$state %in% c("sure", "close", "tie"), info = basename(p))
    expect_identical(id$guess, r$template_id, info = basename(p))
  }
})

test_that("a PDF says how many pages it has; a CSV says it is a CSV", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  ts <- .id_tset()
  p <- file.path(engine_root(), "tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf")
  skip_if_not(file.exists(p))
  id <- identify_file(p, ts, basename(p))
  expect_identical(id$kind, "PDF"); expect_identical(id$format, "pdf")
  expect_identical(id$pages, length(pdftools::pdf_text(p)))
  c1 <- identify_file(.id_csv("x.csv", .ID_ANZ), ts, "x.csv")
  expect_identical(c1$kind, "CSV"); expect_identical(c1$format, "delimited")
  expect_true(is.na(c1$pages))
})

test_that("a scan is SAID to be a scan, and not guessed (no OCR to fill a table)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  # a picture of a page with no text layer at all -- what a scanner produces
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p, width = 8.27, height = 11.69)
  graphics::plot.new()
  graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1)
  grDevices::dev.off()
  expect_false(any(nzchar(trimws(pdftools::pdf_text(p)))))
  t0 <- Sys.time()
  id <- identify_file(p, .id_tset(), "scan.pdf")
  expect_identical(id$state, "scanned")
  expect_identical(id$kind, "Scanned PDF")
  expect_true(is.na(id$guess))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 5)   # never OCR'd
})

test_that("a file the conversion would refuse is never promised a template", {
  ts <- .id_tset()
  # a .csv holding prose: the conversion refuses it before detection runs
  prose <- .id_csv("notes.csv", c("Dear Sir", "Please find attached", "Regards"))
  id <- identify_file(prose, ts, "notes.csv")
  expect_identical(id$state, "unreadable")
  expect_true(is.na(id$guess))
  r <- .id_conv(prose)
  expect_identical(r$status, "failed")
  # same words as the conversion's own refusal
  expect_match(r$messages, id$detail, fixed = TRUE)
  # a damaged PDF
  junk <- tempfile(fileext = ".pdf"); writeBin(as.raw(sample(0:255, 500, TRUE)), junk)
  jk <- identify_file(junk, ts, "junk.pdf")
  expect_identical(jk$state, "unreadable")
  # ...and says why, so its row's hover is not empty
  expect_match(jk$detail, "could not be opened", fixed = TRUE)
  # a missing file
  expect_identical(identify_file(file.path(tempdir(), "gone.pdf"), ts, "gone.pdf")$state, "unreadable")
})

test_that("a file type nothing reads says so, and is offered no template", {
  p <- .id_csv("letter.docx", "x")
  id <- identify_file(p, .id_tset(), "letter.docx")
  expect_identical(id$state, "unsupported_type")
  expect_true(is.na(id$format))
})

test_that("an unrecognised layout is 'none', with the detector's reason kept", {
  p <- .id_csv("odd.csv", c("colA;colB;colC", "1;2;3", "4;5;6"))
  id <- identify_file(p, .id_tset(), "odd.csv")
  expect_identical(id$state, "none")
  expect_true(is.na(id$guess))
  expect_true(nzchar(id$detail))
  expect_identical(.id_conv(p)$status, "unsupported")
})

test_that("a tie is flagged, and its guess is the template the conversion will use", {
  ts <- .id_tset()
  twin <- ts[["anz_everyday_csv"]]; twin$id <- "anz_everyday_csv_twin"
  ts2 <- c(ts, list(anz_everyday_csv_twin = twin))
  p <- .id_csv("anz.csv", .ID_ANZ)
  id <- identify_file(p, ts2, "anz.csv")
  expect_identical(id$state, "tie")
  det <- detect_statement(read_input(p), ts2)
  expect_identical(id$guess, det$tied[1])           # what convert_statement reads with
  expect_identical(id$runner_up, det$tied[2])
})

test_that("a win by one phrase is a close call, as the conversion treats it", {
  ts <- .id_tset()
  near <- ts[["anz_everyday_csv"]]; near$id <- "anz_near"
  need <- unlist(near$fingerprint$header_contains_all)
  near$fingerprint$header_contains_all <- as.list(need[-1])
  near$min_score <- length(need) - 1L
  ts2 <- c(ts, list(anz_near = near))
  id <- identify_file(.id_csv("anz.csv", .ID_ANZ), ts2, "anz.csv")
  expect_identical(id$state, "close")
  expect_identical(id$guess, "anz_everyday_csv")
  expect_identical(id$runner_up, "anz_near")
})

test_that("the dropdown offers only templates that can read this kind of file", {
  ts <- .id_tset()
  pdf <- template_choices(ts, "pdf")
  ids <- unlist(lapply(pdf, unname))
  expect_true(length(ids) > 0)
  expect_true(all(vapply(ts[ids], function(t) identical(t$format, "pdf"), logical(1))))
  csv <- unlist(lapply(template_choices(ts, "delimited"), unname))
  expect_false(any(ids %in% csv))
  # grouped by bank, and the groups are the banks
  expect_true(all(names(pdf) %in% vapply(ts, function(t) t$bank %||% "Other", "")))
  expect_identical(template_choices(list(), "pdf"), list())
})

test_that("a template built here says so, and two of the same name are told apart", {
  ts <- .id_tset()
  u <- ts[["anz_everyday_pdf"]]; u$id <- "anz_mine"; u$origin <- "user"
  v <- ts[["anz_everyday_pdf"]]; v$id <- "anz_variant"
  ch <- template_choices(c(ts, list(anz_mine = u, anz_variant = v)), "pdf")
  lab <- names(unlist(unname(ch)))
  expect_true(any(grepl("(built here)", lab, fixed = TRUE)))
  # anz_everyday_pdf and anz_variant share a display name -> each carries its id
  expect_true(any(grepl("(anz_variant)", lab, fixed = TRUE)))
  expect_true(any(grepl("(anz_everyday_pdf)", lab, fixed = TRUE)))
})

test_that("the bank-on-the-page tie-break reaches the table too", {
  # The production case: a template built here for "Kowhai Bank" tying a shipped
  # template on generic headings. The table must show the one that names the bank.
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p, width = 8.27, height = 11.69)
  graphics::plot.new()
  graphics::text(0.5, c(0.95, 0.9, 0.8, 0.75, 0.1), c("Kowhai Bank of Aotearoa",
    "Statement of Accounts", "Date Details Withdrawals Deposits Balance",
    "03 Feb EFTPOS DAIRY 12.40 2,398.15", "Kowhai Bank of Aotearoa Limited"))
  grDevices::dev.off()
  mk <- function(id, bank, origin) list(id = id, bank = bank, format = "pdf",
    origin = origin, min_score = 3L,
    fingerprint = list(page_contains_all = list("Withdrawals", "Deposits", "Balance")))
  ts <- list(anz_x = mk("anz_x", "ANZ", "default"), kowhai_x = mk("kowhai_x", "Kowhai Bank", "user"))
  id <- identify_file(p, ts, "kowhai.pdf")
  expect_identical(id$guess, "kowhai_x")
  # Both scored 3 of 3 -- but the bank's own name settled it, and the runner-up is
  # another bank's template, not a variant of this one. So it is a plain suggestion,
  # and the conversion does not hold it for review either (bank_clear).
  expect_identical(id$state, "sure")
})

test_that("the hover on a suggestion is a sentence, never a template id and a score", {
  # det$detail is the LOG line -- "matched anz_everyday_csv (score 9/9)". The
  # customer-facing screens never show an id and a fraction; the hover is the
  # same fact in words, naming the template the way the dropdown does.
  ts <- .id_tset()
  p <- .id_csv("anz.csv", .ID_ANZ)
  id <- identify_file(p, ts, "anz.csv")
  expect_identical(id$state, "sure")
  expect_false(grepl("anz_everyday_csv", id$detail, fixed = TRUE))
  expect_false(grepl("[0-9]+/[0-9]+|score", id$detail))
  expect_match(id$detail, "matches the ANZ everyday template", fixed = TRUE)
  # a tie names both, in words
  twin <- ts[["anz_everyday_csv"]]; twin$id <- "anz_twin"; twin$statement_type <- "twin"
  tie <- identify_file(p, c(ts, list(anz_twin = twin)), "anz.csv")
  expect_identical(tie$state, "tie")
  expect_false(grepl("anz_twin|anz_everyday_csv", tie$detail))
  expect_match(tie$detail, "fit this file equally well", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# SCANS. The quick check only says "Scanned"; then a background job reads the first
# two pages as pictures and suggests a template if they clearly match one.
.id_scan_of <- function(fixture_pdf) {
  out <- tempfile(fileext = ".pdf")
  im <- magick::image_read_pdf(fixture_pdf, density = 200)
  magick::image_write(magick::image_convert(im, colorspace = "gray"), out, format = "pdf")
  out
}

test_that("a scan of a known statement is suggested the template its conversion uses", {
  skip_if_not(requireNamespace("magick", quietly = TRUE) && isTRUE(ocr_available()))
  ts <- .id_tset()
  scan <- .id_scan_of(file.path(engine_root(), "tests/testthat/fixtures/anz_everyday_pdf_sample.pdf"))
  expect_identical(identify_file(scan, ts, "scan.pdf")$state, "scanned")   # the quick check
  s <- identify_scan(scan, ts, "scan.pdf")
  expect_identical(s$state, "scan_sure")
  expect_identical(s$guess, "anz_everyday_pdf")
  expect_identical(s$guess, .id_conv(scan)$template_id)
  expect_match(s$detail, "Read from the first pages of the scan", fixed = TRUE)
})

test_that("a scan that matches nothing is left to be detected while it converts", {
  skip_if_not(isTRUE(ocr_available()))
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p); graphics::plot.new()
  graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1); grDevices::dev.off()
  s <- identify_scan(p, .id_tset(), "noise.pdf")
  expect_identical(s$state, "scanned")
  expect_true(is.na(s$guess))
})

test_that("on a server with no OCR, a scan says so in its row", {
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p); graphics::plot.new()
  graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1); grDevices::dev.off()
  real <- get("ocr_available", envir = globalenv())
  assign("ocr_available", function() FALSE, envir = globalenv())
  on.exit(assign("ocr_available", real, envir = globalenv()), add = TRUE)
  expect_identical(identify_file(p, .id_tset(), "scan.pdf")$state, "scanned_no_ocr")
  expect_identical(identify_scan(p, .id_tset(), "scan.pdf")$state, "scanned_no_ocr")
})

test_that("the scan-reading job leaves one verdict per scan as it goes", {
  skip_if_not(isTRUE(ocr_available()))
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p); graphics::plot.new()
  graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1); grDevices::dev.off()
  jd <- tempfile("tscan_"); dir.create(jd)
  res <- job_run_task("identify_scans", p,
    list(names = "noise.pdf", templates_dir = templates_dir(), user_templates_dir = NULL), jobdir = jd)
  expect_length(res, 1L)
  expect_identical(res[[1]]$state, "scanned")
  h <- new.env(); h$dir <- jd
  got <- job_done_rows(h)
  expect_identical(got$rows$state, "scanned")
  expect_identical(got$rows$k, 1L)
})
