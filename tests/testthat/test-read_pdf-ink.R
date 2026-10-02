# ---------------------------------------------------------------------------
# WHEN THE TEXT LAYER AND THE PAGE DISAGREE ABOUT A SIGN.
#
# Two constructions real statements use, and the text layer is wrong about both.
# They are opposite faults and each one is a silent wrong figure:
#
#   A MINUS DRAWN AS A LINE. The sign is a short stroke of vector ink, not a glyph,
#     so pdftotext reports "789.01" for a figure the page shows as -789.01. Every
#     withdrawal reads as a deposit.
#   A MINUS DRAWN IN THE BACKGROUND COLOUR. Some banks print the sign in near-white
#     on POSITIVE amounts so the column stays right-aligned. It is in the text layer
#     and invisible on the page, so "1,527.57" is reported as "-1,527.57". Every
#     deposit reads as a withdrawal.
#
# NEITHER IS CAUGHT BY ARITHMETIC on a statement with no running balance to
# reconcile against -- and anz_investmentfunds_pdf, a shipped template, is exactly
# that shape. Measured on the synthetic corpus before the fix: 14 of 16 rows
# inverted by the drawn minus, 3 of 16 by the invisible one, all at trust `low`
# with no arithmetic objection, because there was nothing to object with.
#
# Both are visible in `pdftocairo -svg`, from the poppler bundle the OCR path
# already needs. No new dependency, no Python.
# ---------------------------------------------------------------------------

# .ink_pdf(kind) -- draw a one-page specimen with reportlab if it is available.
# Skipped rather than faked when it is not: a fixture that did not come out of a
# real PDF writer would prove nothing about reading a real PDF.
.ink_pdf <- function(kind) {
  py <- Sys.which("python3")
  skip_if_not(nzchar(py), "python3 not available")
  gen <- file.path(engine_root(), "tools", "synth", "make_corpus.py")
  skip_if_not(file.exists(gen))
  d <- file.path(tempdir(), paste0("inkfix_", kind))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(d, paste0(kind, ".pdf"))
  if (!file.exists(f)) {
    st <- suppressWarnings(system2(py, c(shQuote(gen), "--out", shQuote(d), "--only", kind),
                                   stdout = FALSE, stderr = FALSE))
    skip_if_not(identical(as.integer(st), 0L) && file.exists(f),
                "reportlab not available to draw the specimen")
  }
  f
}

test_that("a minus drawn as vector ink is read, and the row is not inverted", {
  f <- .ink_pdf("signed_minus_as_ink")
  ink <- .pdf_ink(f)
  expect_false(is.null(ink))
  st <- ink[[1]]$strokes
  # the sign strokes are SHORT; the table rule across the page is not
  short <- st[st$len >= .INK_MIN_LEN & st$len <= .INK_MAX_LEN, , drop = FALSE]
  expect_gt(nrow(short), 0L)
  expect_true(all(short$len < .INK_MAX_LEN))
  expect_true(any(st$len > 100))            # the rule really is in there, and excluded

  w <- pdftools::pdf_data(f)[[1]]
  r <- .apply_ink_signs(w, ink[[1]])
  expect_gt(r$ink_minus, 0L)                # signs were recovered
  expect_identical(r$faint_dropped, 0L)     # and nothing was dropped
  expect_identical(nrow(r$words), nrow(w))  # no word lost
  # every recovered sign landed on a money token, never on a description
  gained <- setdiff(r$words$text, w$text)
  expect_true(all(grepl("^-[0-9][0-9,.]*$", gained)))
})

test_that("a minus printed in the background colour is dropped, not honoured", {
  f <- .ink_pdf("signed_minus_invisible")
  ink <- .pdf_ink(f)
  expect_false(is.null(ink))
  expect_gt(nrow(ink[[1]]$faint), 0L)

  w <- pdftools::pdf_data(f)[[1]]
  r <- .apply_ink_signs(w, ink[[1]])
  expect_gt(r$faint_dropped, 0L)
  expect_identical(r$ink_minus, 0L)
  expect_identical(nrow(r$words), nrow(w) - r$faint_dropped)
  # what went is a dash and nothing else: no figure may be dropped with it
  gone <- w$text[!(seq_len(nrow(w)) %in% match(r$words$text, w$text))]
  expect_true(all(grepl("^[-]+$", trimws(.ascii_dashes(w$text[!w$text %in% r$words$text])))))
})

test_that("a statement that prints its signs normally is left completely alone", {
  # The guard against over-reading. A table rule, an underline and an ordinary
  # printed minus must produce no change at all -- a false positive here would
  # invent a negative transaction, which is the fault being fixed, in reverse.
  for (kind in c("signed_baseline", "baseline_1page")) {
    f <- .ink_pdf(kind)
    w <- pdftools::pdf_data(f)[[1]]
    r <- .apply_ink_signs(w, .pdf_ink(f)[[1]])
    expect_identical(r$ink_minus, 0L, info = kind)
    expect_identical(r$faint_dropped, 0L, info = kind)
    expect_identical(r$words$text, w$text, info = kind)
  }
})

test_that("the counts reach a diagnostic, through all three places they are copied", {
  # ink_minus_signs is copied read_pdf -> read_pdf_input -> read_input$meta ->
  # convert_statement -> build_diagnostics. A gap at any one of them leaves the
  # figures unexplained, and read_input.R already carries a comment saying so about
  # scanned_no_ocr -- the same trap, hit again. This walks the whole chain.
  f <- .ink_pdf("signed_minus_as_ink")
  p <- read_pdf(f)
  expect_gt(p$ink_minus_signs, 0L)
  expect_identical(read_pdf_input(f)$ink_minus_signs, p$ink_minus_signs)
  clear_input_cache()
  expect_identical(read_input(f)$meta$ink_minus_signs, p$ink_minus_signs)

  d <- build_diagnostics("needs_review",
                         metadata = list(ink_minus_signs = 14L, faint_minus_signs = 0L))
  expect_true("sign_from_ink" %in% d$category)
  row <- d[d$category == "sign_from_ink", , drop = FALSE]
  expect_identical(row$severity[1], "info")        # nothing is wrong
  expect_match(row$detail[1], "drawn as a line")
  # ...and it says nothing at all when the page prints its signs normally
  d0 <- build_diagnostics("ok", metadata = list(ink_minus_signs = 0L, faint_minus_signs = 0L))
  expect_false("sign_from_ink" %in% (d0$category %||% character(0)))
})
