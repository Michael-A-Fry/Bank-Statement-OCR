# Tests for the OCR path (R/ocr.R): the system Tesseract + poppler pipeline
# driven from R, and its integration as the no-text-layer fallback in read_pdf.
# Portable -- OCR-dependent checks skip automatically where the tools are absent.

SAMPLE_PDF_OCR <- "samples/raw/anz/anz_card_summary_sample.pdf"

test_that("ocr_available returns a single logical", {
  expect_type(ocr_available(), "logical")
  expect_length(ocr_available(), 1L)
})

test_that("page_needs_ocr triggers only on empty / near-empty text layers", {
  expect_true(page_needs_ocr(character(0)))
  expect_true(page_needs_ocr(""))
  expect_true(page_needs_ocr("   \n \t "))
  expect_false(page_needs_ocr(
    paste(rep("real statement transaction line 12/03/2025 -45.00", 3),
          collapse = "\n")))
})

test_that("page_needs_ocr routes on word boxes + text health, not a char count (P2-1)", {
  wb <- function(n) data.frame(text = rep("x", n), stringsAsFactors = FALSE)
  # (c) a digital page whose pdf_text came back empty but has word boxes is NEVER
  # OCR'd (OCR would overwrite good boxes); with no boxes it still is.
  expect_false(page_needs_ocr("", wb(50)))
  expect_true(page_needs_ocr("", wb(0)))
  # (a) a scanned page carrying only a thin text stamp/footer (few boxes) -> OCR,
  # even though the char count clears the old 20-char bar.
  expect_true(page_needs_ocr("Confidential exhibit A page 1 of 5 Bates stamp", wb(2)))
  # (b) a broken-CID / no-ToUnicode font extracts garbage of the right length -> OCR
  expect_true(page_needs_ocr(intToUtf8(rep(0xE010, 40)), wb(40)))
  # a healthy digital page with many boxes is left alone.
  expect_false(page_needs_ocr(paste(rep("Payment 100.00", 20), collapse = " "), wb(60)))
})

test_that("tesseract reads a real PDF page end-to-end", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  pdf <- fixture(SAMPLE_PDF_OCR)
  skip_if_not(file.exists(pdf))
  res <- ocr_pdf_page(pdf, 1L)
  expect_true(res$ok)
  expect_gt(length(res$text), 0L)
  expect_match(toupper(paste(res$text, collapse = " ")),
               "CARD SUMMARY", fixed = TRUE)
})

# A client's statement must not be left lying on the server's disk. ocr_pdf_page
# renders the page and then writes TWO preprocessed copies of it; those copies used
# to be plain tempfile() names, outside the render-prefix glob the cleanup sweeps,
# so every OCR'd page left two full-page pictures of the statement in the R temp
# dir for the life of the process (measured 1.38 MB per page on a real 300 dpi A4
# scan -- ~15 MB for one 11-page statement, on a server meant to run for months).
test_that("ocr_pdf_page leaves no page images behind (#29)", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  pdf <- fixture(SAMPLE_PDF_OCR)
  skip_if_not(file.exists(pdf))
  snapshot <- function() list.files(tempdir(), recursive = TRUE, all.files = TRUE,
                                    no.. = TRUE)
  before <- snapshot()
  res <- ocr_pdf_page(pdf, 1L)
  expect_true(res$ok)
  # gc() first: when ImageMagick's in-memory pixel pool is full it spills a page to
  # a disk-backed cache file, which IT deletes when the image object is finalised.
  # That one is magick's to clean and it does; the files this test is about are
  # OURS and were never cleaned at all, so gc() cannot hide them.
  invisible(gc())
  leaked <- setdiff(snapshot(), before)
  # Named so a failure says WHAT was left behind and how big it was.
  info <- if (length(leaked))
    paste(sprintf("%s (%.2f MB)", leaked,
                  file.info(file.path(tempdir(), leaked))$size / 1024^2), collapse = ", ")
  else ""
  expect_identical(leaked, character(0), info = info)
})

test_that("read_pdf exposes ocr flags and does not OCR a text-layer page", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  r <- read_pdf(fixture(SAMPLE_PDF_OCR))
  expect_true(r$ok)
  expect_type(r$ocr, "logical")
  expect_length(r$ocr, r$page_count)
  # the page holding the header has a real text layer -> must NOT be OCR-flagged
  idx <- which(grepl("CARD SUMMARY", r$pages, ignore.case = TRUE))
  expect_gte(length(idx), 1L)
  expect_false(any(r$ocr[idx]))
  # words-frame contract: ocr_conf is always present; on a text-layer page it is
  # all NA (typeset text has no recognition step to be unsure about).
  w <- r$words[[idx[1]]]
  expect_true("ocr_conf" %in% names(w))
  expect_true(all(is.na(w$ocr_conf)))
})

# --- N17: ImageMagick's disk spill is confined and cleaned up ---------------
# Under memory pressure ImageMagick writes a page's raw pixels to a temp file and
# only removes it when the process exits -- on a server that runs for months,
# never. with_image_scratch() gives each call its own folder and deletes it, so
# no readable statement imagery is left on the host's disk.

test_that("with_image_scratch points MAGICK_TMPDIR at a private folder and removes it", {
  outside_before <- Sys.getenv("MAGICK_TMPDIR", unset = NA_character_)
  seen <- with_image_scratch({
    d <- Sys.getenv("MAGICK_TMPDIR", unset = NA_character_)
    expect_false(is.na(d))
    expect_true(dir.exists(d))
    # a spill file written while inside must be inside the scratch folder
    writeLines("pretend pixel cache", file.path(d, "magick-spill"))
    d
  })
  expect_false(dir.exists(seen))                       # folder gone, spill with it
  after <- Sys.getenv("MAGICK_TMPDIR", unset = NA_character_)
  expect_identical(after, outside_before)              # env restored exactly
})

test_that("with_image_scratch restores a pre-existing MAGICK_TMPDIR", {
  keep <- Sys.getenv("MAGICK_TMPDIR", unset = NA_character_)
  on.exit(if (is.na(keep)) Sys.unsetenv("MAGICK_TMPDIR") else
            Sys.setenv(MAGICK_TMPDIR = keep), add = TRUE)
  Sys.setenv(MAGICK_TMPDIR = "/some/preset/dir")
  inner <- with_image_scratch(Sys.getenv("MAGICK_TMPDIR"))
  expect_false(identical(inner, "/some/preset/dir"))
  expect_identical(Sys.getenv("MAGICK_TMPDIR"), "/some/preset/dir")
})

test_that("with_image_scratch returns the value and cleans up even on an error", {
  expect_identical(with_image_scratch(1 + 1), 2)
  before <- length(Sys.glob(file.path(tempdir(), "imgscratch_*")))
  expect_error(with_image_scratch(stop("boom")), "boom")
  expect_identical(length(Sys.glob(file.path(tempdir(), "imgscratch_*"))), before)
})

test_that("ocr_pdf_page leaves no scratch folder behind", {
  skip_if_not(ocr_available(), "tesseract/pdftoppm not installed")
  pdf <- fixture(SAMPLE_PDF_OCR)
  before <- length(Sys.glob(file.path(tempdir(), "imgscratch_*")))
  invisible(ocr_pdf_page(pdf, 1))
  expect_identical(length(Sys.glob(file.path(tempdir(), "imgscratch_*"))), before)
})

# --- Speed and the safety nets ----------------------------------------------

# A picture that is nothing but speckle: Tesseract takes tens of seconds over it.
# Written as a PGM by hand, so no image package is needed.
.ocr_noise_pgm <- function(w = 2000L, h = 2000L) {
  f <- tempfile("noise_", fileext = ".pgm")
  set.seed(7)
  con <- file(f, "wb")
  writeBin(charToRaw(sprintf("P5\n%d %d\n255\n", w, h)), con)
  writeBin(as.raw(sample(c(0L, 255L), w * h, replace = TRUE, prob = c(0.3, 0.7))), con)
  close(con)
  f
}

test_that("a Tesseract run past its time limit is stopped and says so", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  f <- .ocr_noise_pgm()
  on.exit(unlink(f), add = TRUE)
  t0 <- Sys.time()
  r <- .tesseract_tsv(f, seconds = 1L)
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 15)
  expect_true(r$timed_out)
  expect_null(r$tsv)          # half a page read is worse than none
})

test_that("tesseract runs on one thread unless the caller chose otherwise", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  keep <- Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_)
  on.exit(if (is.na(keep)) Sys.unsetenv("OMP_THREAD_LIMIT") else
            Sys.setenv(OMP_THREAD_LIMIT = keep), add = TRUE)
  Sys.unsetenv("OMP_THREAD_LIMIT")
  invisible(.tesseract("--version"))
  expect_identical(Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_), NA_character_)
  Sys.setenv(OMP_THREAD_LIMIT = "3")
  invisible(.tesseract("--version"))
  expect_identical(Sys.getenv("OMP_THREAD_LIMIT"), "3")
})

test_that("a cleaned picture that reads as noise is replaced by the render", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  pdf <- fixture("samples/raw/tutorial/sample_everyday_scanned.pdf")
  skip_if_not(file.exists(pdf))
  prefix <- tempfile("best_")
  system2("pdftoppm", c("-gray", "-r", "300", "-f", "2", "-l", "2", pdf, prefix),
          stdout = FALSE, stderr = FALSE)
  raw <- Sys.glob(paste0(prefix, "*.pgm"))[1]
  noise <- .ocr_noise_pgm(600L, 600L)
  on.exit(unlink(c(raw, noise), force = TRUE), add = TRUE)
  rd <- .ocr_best_reading(noise, raw, "eng", 300L)
  expect_gt(rd$median, 80)
  expect_match(rd$note, "read without image clean-up")
  # A good cleaned picture is used as it is, with nothing to say.
  rd2 <- .ocr_best_reading(raw, raw, "eng", 300L)
  expect_identical(rd2$note, "")
  expect_equal(rd2$median, rd$median)
})

test_that("the page text is rebuilt line by line from the word boxes' TSV", {
  tsv <- data.frame(level = c(1, 5, 5, 5, 5, 5),
                    block_num = c(0, 1, 1, 1, 2, 2), par_num = c(0, 1, 1, 1, 1, 1),
                    line_num = c(0, 1, 1, 2, 1, 1), conf = c(-1, 96, 95, 90, 91, 92),
                    text = c(NA, "Opening", "balance", "12.00", "Closing", ""),
                    stringsAsFactors = FALSE)
  expect_identical(.ocr_tsv_lines(tsv), c("Opening balance", "12.00", "Closing"))
  expect_identical(.ocr_tsv_lines(NULL), character(0))
})

test_that("read_pdf reports pages OCR could not finish (none on a normal scan)", {
  skip_if_not(ocr_available(), "tesseract/poppler not installed")
  r <- read_pdf(fixture("samples/raw/tutorial/sample_everyday_scanned.pdf"))
  expect_identical(r$ocr_timed_out, integer(0))
  expect_length(r$ocr_note, r$page_count)
  expect_true(all(r$ocr))
})
