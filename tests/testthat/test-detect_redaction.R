# Tests for the rasterised-redaction detector (R/detect_redaction.R): a real
# black box on a scanned page must be found and its hidden cells reconstructed as
# [REDACTED], so a blacked-out value keeps its row (flagged) instead of vanishing.

.dr_words <- function() {
  # two visible transaction rows (date | desc | amount | balance), no redaction.
  # Dates are day+month ("05 Jan") so they parse under %d %b.
  data.frame(stringsAsFactors = FALSE,
    width  = c(12,16,50,30,30,   12,16,50,30,30),
    height = rep(10, 10),
    x      = c(45,60,110,360,470, 45,60,110,360,470),
    y      = c(40,40,40,40,40,    70,70,70,70,70),
    space  = TRUE,
    text   = c("05","Jan","COFFEE","40.00","955.50", "06","Jan","SHOP","10.00","945.50"),
    redacted = FALSE)
}

# P1-4: a DIGITAL PDF that hides a value under a DRAWN (vector) black rectangle,
# while the text stays in the layer, must not leak that text. detect_occluded_words
# rasterises the page and flags any word whose box renders ~solid dark.
.make_vector_redacted_pdf <- function() {
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf, width = 8.27, height = 11.69)
  op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op), add = TRUE)
  graphics::plot.new(); graphics::plot.window(xlim = c(0, 100), ylim = c(0, 100))
  graphics::text(5,  50, "01/06/2025", pos = 4, cex = 1.5)
  graphics::text(30, 50, "123.45",     pos = 4, cex = 1.5)
  graphics::text(55, 50, "SECRETNAME", pos = 4, cex = 1.5)
  graphics::rect(54, 47, 82, 53, col = "black", border = NA)   # painted OVER the text
  grDevices::dev.off()
  tf
}

test_that("a vector-drawn redaction over live text does not leak (P1-4)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(requireNamespace("magick", quietly = TRUE))
  tf <- .make_vector_redacted_pdf()
  # sanity: the raw text layer DOES still carry the hidden word (that's the leak).
  skip_if_not(grepl("SECRETNAME", paste(pdftools::pdf_text(tf), collapse = " ")))

  r <- read_pdf(tf)
  txt <- paste(r$pages, collapse = " ")
  expect_false(grepl("SECRETNAME", txt))          # the hidden word must NOT be emitted
  expect_true(grepl("01/06/2025", txt))           # visible words survive
  expect_true(grepl("123.45", txt))
  w <- r$words[[1]]
  expect_true(any(w$redacted %in% TRUE))          # the covered word is flagged
  expect_false(any(grepl("SECRETNAME", w$text)))  # its text is discarded everywhere
  expect_equal(r$redaction_scan_incomplete, 0)    # the scan ran (no loud fallback)
})

test_that("detect_occluded_words spares normal text, flags an occluded word (P1-4)", {
  skip_if_not(requireNamespace("pdftools", quietly = TRUE))
  skip_if_not(requireNamespace("magick", quietly = TRUE))
  tf <- .make_vector_redacted_pdf()
  d <- as.data.frame(pdftools::pdf_data(tf)[[1]])
  ps <- pdftools::pdf_pagesize(tf)
  occ <- detect_occluded_words(tf, 1, d, ps$width[1], ps$height[1])
  expect_true(isTRUE(occ$ok))
  # exactly the SECRETNAME box is occluded; the date/amount are not.
  expect_true(occ$occluded[grepl("SECRETNAME", d$text)][1])
  expect_false(any(occ$occluded[grepl("01/06/2025|123.45", d$text)]))
})

test_that("inject_redaction_tokens reconstructs a blacked cell on its own row", {
  words <- .dr_words()
  # a rect over the AMOUNT column of row 1 only (y 36..52, x 350..410)
  rect <- data.frame(x0 = 350, y0 = 36, x1 = 410, y1 = 52)
  out <- inject_redaction_tokens(words, rect, row_tol = 3)

  added <- out[out$redacted %in% TRUE, , drop = FALSE]
  expect_gt(nrow(added), 0)                                  # something was injected
  expect_true(all(added$text == REDACTION_TOKEN))           # as [REDACTED]
  # injected inside the rect's x-range and anchored to row 1's y (not row 2)
  expect_true(all(added$x >= 349 & added$x <= 411))
  expect_true(all(abs(added$y - 40) <= 4))
  # the visible words are untouched (still present, not redacted)
  expect_equal(sum(!(out$redacted %in% TRUE)), nrow(words))
})

test_that("a reconstructed cell makes the row keep + flag (never silently dropped)", {
  # Feed words with a blacked amount THROUGH the parser: row must survive with a
  # real date, a NULL amount (not fabricated) and a redacted flag.
  words <- .dr_words()
  rect <- data.frame(x0 = 350, y0 = 36, x1 = 410, y1 = 52)   # blackout row-1 amount
  # mirror read_pdf's OCR order: guard scrubs any word under the box, then inject
  # reconstructs the hidden cell (a true blackout leaves no word, so inject is
  # what supplies the [REDACTED]).
  words <- apply_redaction_guard(words, rect[, c("x0","y0","x1","y1")])
  words <- inject_redaction_tokens(words, rect, row_tol = 3)
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))
  tmpl <- list(id = "s", bank = "S", statement_type = "e", format = "pdf",
    version = 1, currency = "NZD",
    table = list(row_tol = 3, date_format = "%d %b", amount_sign = "signed",
      columns = list(date = list(x_min = 40, x_max = 74),
        description = list(x_min = 74, x_max = 340),
        amount = list(x_min = 340, x_max = 460),
        balance = list(x_min = 460, x_max = 545))))
  tx <- parse_pdf_table(input, tmpl)$transactions
  expect_equal(nrow(tx), 2L)                                  # BOTH rows kept
  r1 <- tx[grepl("redacted", tx$flags, ignore.case = TRUE), ]
  expect_equal(nrow(r1), 1L)                                  # row 1 flagged
  expect_true(is.na(r1$amount))                               # amount nulled, not invented
})

test_that("inject is a no-op when there are no rects", {
  words <- .dr_words()
  expect_identical(inject_redaction_tokens(words, NULL), words)
  expect_identical(inject_redaction_tokens(words, .empty_rects()), words)
})

test_that("detect_dark_regions finds a solid black rectangle (skips a logo/text)", {
  skip_if_not_installed("magick")
  # white page, one solid black rectangle in the middle, plus sparse 'text'.
  img <- magick::image_blank(400, 600, "white")
  d <- magick::image_draw(img)
  graphics::rect(120, 180, 300, 260, col = "black", border = "black")   # redaction box
  graphics::text(60, 60, "a bit of text", col = "black")                # sparse -> ignored
  grDevices::dev.off()
  rects <- detect_dark_regions(d, scale = 1)                 # scale 1 -> points == pixels
  expect_equal(nrow(rects), 1L)                              # exactly the box, not the text
  # roughly matches the drawn rectangle (generous tolerance for downsampling)
  expect_true(rects$x0 <= 140 && rects$x1 >= 280)
  expect_true(rects$y0 <= 200 && rects$y1 >= 240)
})
