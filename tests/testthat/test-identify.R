# Tests for identify_file() / identify_scan() / bank_choices() (R/identify.R) --
# the Convert table: one row per uploaded file, its kind and pages, and its BANK,
# pre-filled from the statement itself.
#
# The failure these exist to prevent above all others: THE TABLE SAYING ONE BANK
# AND THE CONVERSION USING ANOTHER. A row left on its pre-fill is converted with no
# bank given, so the conversion pre-fills the same bank from the same evidence.

.id_conv <- function(p, ...) {
  od <- tempfile(); dir.create(od)
  suppressWarnings(convert_statement(p, outdir = od, logdir = od, layouts_dir = file.path(od, "ly"),
                                     tracking_dir = NA, formats = "csv", log = FALSE, ...))
}
.id_csv <- function(name, lines) { d <- tempfile(); dir.create(d); p <- file.path(d, name); writeLines(lines, p); p }

test_that("the pre-filled bank is the bank the conversion then uses", {
  p <- proven_csv()
  id <- identify_file(p, "export.csv")
  expect_identical(id$state, "ready")
  expect_identical(id$bank, "bnz")
  expect_identical(id$bank_display, "BNZ")
  expect_identical(id$bank_code, "02")
  expect_identical(id$confidence, "high")
  expect_false(id$ask)
  expect_match(id$detail, "BNZ", fixed = TRUE)
  r <- .id_conv(p)
  expect_identical(r$bank$bank, id$bank)
  # never the account number, in any field of the row
  body <- strsplit(nz_test_account(), "-")[[1]][3]
  expect_false(grepl(body, paste(unlist(id), collapse = " "), fixed = TRUE))
})

test_that("a statement that does not name its bank asks for it", {
  id <- identify_file(unproven_csv(), "export.csv")
  expect_identical(id$state, "ready")
  expect_true(is.na(id$bank))
  expect_true(id$ask)
  expect_true(nzchar(id$detail))
})

test_that("a PDF says how many pages it has; a CSV says it is a CSV", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  p <- file.path(engine_root(), "tests/testthat/fixtures/anz_everyday_pdf_bundle_sample.pdf")
  skip_if_not(file.exists(p))
  id <- identify_file(p, basename(p))
  expect_identical(id$kind, "PDF"); expect_identical(id$format, "pdf")
  expect_identical(id$pages, length(pdftools::pdf_text(p)))
  c1 <- identify_file(unproven_csv(), "x.csv")
  expect_identical(c1$kind, "CSV"); expect_identical(c1$format, "delimited")
  expect_true(is.na(c1$pages))
})

test_that("a scan is SAID to be a scan, and not read (no OCR to fill a table)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  # a picture of a page with no text layer at all -- what a scanner produces
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p, width = 8.27, height = 11.69)
  graphics::plot.new()
  graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1)
  grDevices::dev.off()
  expect_false(any(nzchar(trimws(pdftools::pdf_text(p)))))
  t0 <- Sys.time()
  id <- identify_file(p, "scan.pdf")
  expect_true(id$state %in% c("scanned", "scanned_no_ocr"))
  expect_identical(id$kind, "Scanned PDF")
  expect_true(is.na(id$bank))
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 5)   # never OCR'd
})

test_that("a file the conversion would refuse is said to be unreadable, in its words", {
  # a .csv holding prose: the conversion refuses it before reading
  prose <- .id_csv("notes.csv", c("Dear Sir", "Please find attached", "Regards"))
  id <- identify_file(prose, "notes.csv")
  expect_identical(id$state, "unreadable")
  expect_true(is.na(id$bank))
  r <- .id_conv(prose)
  expect_identical(r$status, "failed")
  expect_match(r$messages, id$detail, fixed = TRUE)    # same words as the refusal
  # a damaged PDF, and says why, so its row's hover is not empty
  junk <- tempfile(fileext = ".pdf"); writeBin(as.raw(sample(0:255, 500, TRUE)), junk)
  jk <- identify_file(junk, "junk.pdf")
  expect_identical(jk$state, "unreadable")
  expect_match(jk$detail, "could not be opened", fixed = TRUE)
  # a missing file
  expect_identical(identify_file(file.path(tempdir(), "gone.pdf"), "gone.pdf")$state, "unreadable")
})

test_that("a file type nothing reads says so", {
  p <- .id_csv("letter.docx", "x")
  id <- identify_file(p, "letter.docx")
  expect_identical(id$state, "unsupported_type")
  expect_true(is.na(id$format))
})

test_that("identify_file never throws", {
  for (a in list(NULL, NA_character_, 1L, character(0)))
    expect_true(is.list(identify_file(a, "x.csv")))
})

test_that("the bank dropdown offers every NZ bank and any bank learned here", {
  d <- tempfile("bc_"); dir.create(d)
  ch <- bank_choices(d)
  expect_true(all(c("anz", "asb", "bnz", "westpac", "kiwibank") %in% ch))
  expect_identical(unname(ch[["ANZ"]]), "anz")
  expect_false(is.unsorted(tolower(names(ch))))
  # a bank named by hand on this box joins the list once something is learned for it
  cv <- convert_sandbox()
  cv(ambiguous_csv(), bank = "Rimu Bank", overrides = list(roles = c(debit = "debit", credit = "credit")))
  ch2 <- bank_choices(file.path(sandbox_dir(cv), "layouts"))
  expect_true("rimu_bank" %in% ch2)
})

.id_scan_of <- function(fixture_pdf) {
  out <- tempfile(fileext = ".pdf")
  im <- magick::image_read_pdf(fixture_pdf, density = 200)
  magick::image_write(magick::image_convert(im, colorspace = "gray"), out, format = "pdf")
  out
}

test_that("a scan's bank is read from its first pages, in the background", {
  skip_if_not(requireNamespace("magick", quietly = TRUE) && isTRUE(ocr_available()))
  scan <- .id_scan_of(file.path(engine_root(), "tests/testthat/fixtures/anz_everyday_pdf_sample.pdf"))
  expect_identical(identify_file(scan, "scan.pdf")$state, "scanned")   # the quick check
  s <- identify_scan(scan, "scan.pdf")
  expect_identical(s$state, "scan_ready")
  expect_true(all(c("bank", "bank_display", "confidence", "detail") %in% names(s)))
})

test_that("a scan with nothing on it is left to be read while it converts", {
  skip_if_not(isTRUE(ocr_available()))
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p); graphics::plot.new()
  graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1); grDevices::dev.off()
  s <- identify_scan(p, "noise.pdf")
  expect_true(s$state %in% c("scanned", "scan_ready"))
  expect_true(is.na(s$bank))
})

test_that("on a server with no OCR, a scan says so in its row", {
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p); graphics::plot.new()
  graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1); grDevices::dev.off()
  real <- get("ocr_available", envir = globalenv())
  assign("ocr_available", function() FALSE, envir = globalenv())
  on.exit(assign("ocr_available", real, envir = globalenv()), add = TRUE)
  expect_identical(identify_file(p, "scan.pdf")$state, "scanned_no_ocr")
  expect_identical(identify_scan(p, "scan.pdf")$state, "scanned_no_ocr")
})

test_that("the scan-reading job leaves one verdict per scan as it goes", {
  skip_if_not(isTRUE(ocr_available()))
  p <- tempfile(fileext = ".pdf")
  grDevices::pdf(p); graphics::plot.new()
  graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1); grDevices::dev.off()
  jd <- tempfile("tscan_"); dir.create(jd)
  res <- job_run_task("identify_scans", p, list(names = "noise.pdf"), jobdir = jd)
  expect_length(res, 1L)
  h <- new.env(); h$dir <- jd
  got <- job_done_rows(h)
  expect_identical(got$rows$state, res[[1]]$state)
  expect_true(all(c("bank", "bank_display", "confidence", "detail") %in% names(got$rows)))
  expect_identical(got$rows$k, 1L)
})
