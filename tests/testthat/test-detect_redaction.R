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
  # THE DEVICE IS THE THING TO CLEAN UP, NOT `par`.
  #
  # This used to save par and restore it on.exit -- which runs AFTER dev.off(),
  # when no device is open at all. `par()` with no device OPENS one: in a
  # non-interactive session that is `Rplots.pdf`, written into the app folder and
  # left open for the rest of the process. Every later test that drew then drew
  # into THAT instead of into its own device, which is how three
  # detect_dark_regions assertions failed on a machine where the function is
  # provably correct -- and why the suite has carried three red lines that
  # everybody, including the documentation, wrote off as "needs a rasterising
  # magick". magick was installed the whole time.
  #
  # There is nothing to restore: the device is closed a few lines down and its
  # par settings die with it. What is worth guaranteeing is that the device
  # cannot outlive this function, however it exits.
  dev_no <- grDevices::dev.cur()
  on.exit(if (dev_no %in% grDevices::dev.list()) grDevices::dev.off(dev_no), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
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

test_that("inject_redaction_tokens strides finely enough for a narrow column (P3-i)", {
  # a row of NARROW words (width ~10): a fixed 34-pt stride could step clean over a
  # ~12-pt column, leaving its redacted cell blank. The adaptive stride must place
  # tokens densely enough that a narrow band still catches one.
  words <- data.frame(stringsAsFactors = FALSE,
    width  = c(10, 10, 10, 10, 10),
    height = rep(9, 5),
    x      = c(45, 60, 75, 90, 470),          # tight 15-pt columns + a date anchor
    y      = rep(40, 5),
    text   = c("05", "Jan", "12.00", "AB", "955.50"),   # a date + amount => a real row
    redacted = FALSE)
  rect <- data.frame(x0 = 44, y0 = 36, x1 = 100, y1 = 52)   # covers the narrow cluster
  out <- inject_redaction_tokens(words, rect, row_tol = 3)
  added <- out[out$redacted %in% TRUE, , drop = FALSE]
  # at least one token per ~15-pt column across the 56-pt rect (>= 3 tokens),
  # where the old fixed 34-pt stride would have placed only ~2.
  expect_gte(nrow(added), 3)
  gaps <- diff(sort(added$x))
  expect_true(all(gaps <= 16))                 # no >16-pt hole a narrow column falls in
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

# THE PICTURE IS BUILT THROUGH A PNG FILE, NOT THROUGH `magick::image_draw`.
#
# image_draw hands back an image that is still attached to a live graphics
# device, and what a later magick call sees depends on when that device was
# flushed and on what else has touched magick since. Measured: the identical
# drawing gave this function 0 regions or 1 depending only on what ran before it
# in the same session -- so the assertion below was testing magick's device
# bookkeeping, not detect_dark_regions.
#
# That is worse than a flaky test. It failed on every run of the suite, and the
# failure was written off -- in this file's history, in the maintainer's guide,
# and by more than one person since -- as "needs a rasterising magick". magick
# was installed the whole time and the function was right the whole time. Three
# permanently red lines in a suite whose pass condition is `failed: 0` teach
# everybody to read past red, which is the one thing a forensic tool's suite
# must never teach.
#
# Rendered to a PNG and read back, it is the same picture with no live device
# anywhere near it, and the answer is the same on every run.
.dr_black_box_png <- function() {
  tf <- tempfile(fileext = ".png")
  grDevices::png(tf, width = 400, height = 600)
  dn <- grDevices::dev.cur()
  on.exit(if (dn %in% grDevices::dev.list()) grDevices::dev.off(dn), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 400), ylim = c(600, 0), xaxs = "i", yaxs = "i")
  graphics::rect(120, 180, 300, 260, col = "black", border = "black")   # redaction box
  graphics::text(60, 60, "a bit of text", col = "black")                # sparse -> ignored
  grDevices::dev.off()
  magick::image_read(tf)
}

test_that("detect_dark_regions finds a solid black rectangle (skips a logo/text)", {
  skip_if_not_installed("magick")
  d <- .dr_black_box_png()
  rects <- detect_dark_regions(d, scale = 1)                 # scale 1 -> points == pixels
  expect_equal(nrow(rects), 1L)                              # exactly the box, not the text
  # roughly matches the drawn rectangle (generous tolerance for downsampling)
  expect_true(rects$x0 <= 140 && rects$x1 >= 280)
  expect_true(rects$y0 <= 200 && rects$y1 >= 240)
  # THE SAME ANSWER EVERY TIME, whatever else has touched magick. This is the
  # assertion that would have caught the original fixture: it is not enough for
  # the answer to be right once.
  again <- detect_dark_regions(.dr_black_box_png(), scale = 1)
  expect_identical(nrow(again), nrow(rects))
  expect_equal(again$x0, rects$x0); expect_equal(again$x1, rects$x1)
})
