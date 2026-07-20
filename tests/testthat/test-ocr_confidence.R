# Tests for OCR word-confidence (TSV) and the low-confidence diagnostic.

test_that("ocr_image_tsv exposes per-word confidence; a clean page reads well", {
  skip_if_not(ocr_available(), "tesseract/poppler not available")
  pdf <- fixture("samples/raw/anz/anz_card_summary_sample.pdf")
  skip_if_not(file.exists(pdf))
  prefix <- tempfile("cf_")
  system2("pdftoppm", c("-png", "-r", "200", "-f", "1", "-l", "1", pdf, prefix),
          stdout = FALSE, stderr = FALSE)
  img <- Sys.glob(paste0(prefix, "*.png"))[1]
  df <- ocr_image_tsv(img)
  expect_true(is.data.frame(df))
  expect_true("conf" %in% names(df))
  conf <- ocr_word_confidence(img)
  expect_false(is.na(conf))
  expect_true(conf > 50 && conf <= 100)
})

test_that("low OCR confidence raises a high-severity diagnostic", {
  parsed <- list(
    transactions = data.frame(
      row_id = 1L, date = "2025-01-01", date_raw = "1/1/25", description = "a",
      amount = -5, amount_raw = "-5", direction = "debit", balance = NA_real_,
      balance_raw = NA_character_, particulars = NA_character_, code = NA_character_,
      reference = NA_character_, other_party = NA_character_, type = NA_character_,
      currency = "NZD", flags = "", stringsAsFactors = FALSE),
    header = list(ocr_pages = 2L, ocr_min_confidence = 42))
  d <- build_diagnostics("needs_review", parsed = parsed, recon = list(kpis = NULL))
  expect_true(any(d$category == "low_ocr_confidence"))
  expect_equal(d$severity[d$category == "low_ocr_confidence"], "high")
  expect_true(any(d$category == "ocr"))   # informational OCR note too
})
