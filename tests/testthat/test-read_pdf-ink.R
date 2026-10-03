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

# ---------------------------------------------------------------------------
# ...AND IT HAS TO SEE EVERY PAGE.
#
# MEASURED BUG, and it was the worst class of error this tool can make. .pdf_ink split
# the renderer's output on `<g id="surface`, a marker poppler 24.02 does not emit, so a
# multi-page statement collapsed into ONE ink entry holding every page's ink. read_pdf
# applied that entry to page 1 alone, which is wrong in both directions at once:
#
#   * pages 2..N got NO sign correction, so a bank that draws its minus as a stroke
#     had every page after the first read with the signs INVERTED; and
#   * page 1 got FALSE POSITIVES, because a stroke anywhere in the document at the
#     same (x, y) as a page-1 amount turned a correct positive negative.
#
# Every ink case in this file and in the corpus was a SINGLE PAGE, so the whole suite
# passed while this was live. On the 3-page corpus cases it produced 48 fabricated
# figures -- 29 sign inversions on the drawn-minus case, 19 on the invisible-minus
# case, every one of them from row 31 on, which is page 2 -- and trust stayed MEDIUM,
# so they would have published.
#
# The lesson is not about SVG. It is that a per-page mechanism tested only on
# one-page documents is untested.

test_that(".pdf_ink returns one entry per page, not one for the document", {
  f <- .ink_pdf("signed_minus_as_ink_3page")
  expect_identical(suppressMessages(pdftools::pdf_info(f))$pages, 3L)
  ink <- .pdf_ink(f)
  expect_false(is.null(ink))
  expect_length(ink, 3L)
  # and every page really carries its own strokes -- a split that yielded three
  # entries by accident, two of them empty, would pass a length check alone
  expect_true(all(vapply(ink, function(p) nrow(p$strokes) > 0L, logical(1))))
})

test_that("the automatic reader honours a drawn minus on every page, figure for figure", {
  f <- .ink_pdf("signed_minus_as_ink_3page")
  truth <- jsonlite::fromJSON(sub("[.]pdf$", ".truth.json", f))
  skip_if_not(is.data.frame(truth$rows), "specimen came without its answer key")
  rd <- auto_read(read_input(f))
  want <- ifelse(is.na(truth$rows$credit), -truth$rows$debit, truth$rows$credit)
  expect_equal(rd$transactions$amount, want)
  expect_identical(rd$transactions$date, truth$rows$date)
})

test_that("a drawn minus is honoured on page 3 as well as page 1", {
  f <- .ink_pdf("signed_minus_as_ink_3page")
  tp <- fixture_templates()
  input <- read_input(f)
  expect_true(isTRUE(input$meta$ink_scan_ok))
  expect_identical(as.integer(input$meta$ink_scan_pages), 3L)
  parsed <- parse_statement(input, tp[["anz_investmentfunds_pdf"]])
  tx <- parsed$transactions
  skip_if_not(nrow(tx) > 60, "specimen did not parse to three pages of rows")
  # the fault was confined to pages 2+, so the test has to look there: with the bug
  # every withdrawal after row 30 came back POSITIVE
  late <- tx$amount[31:nrow(tx)]
  expect_true(any(late < 0), info = "no negative amount after page 1 -- signs inverted")
  # ...and the proportion of withdrawals is the same on page 1 as later, because the
  # generator draws them from one distribution
  expect_gt(mean(late < 0), 0.25)
})

test_that("ink that does not line up with the document is not applied at all", {
  # Applying ink[[p]] to page p is only meaningful if they are the same page. A list
  # of the wrong length means the renderer and the text layer disagree about the
  # document, and guessing the alignment inverts signs on whichever pages are
  # offset -- the exact fault the scan exists to prevent, caused by the scan.
  src <- paste(readLines(file.path(engine_root(), "R", "read_pdf.R"), warn = FALSE),
               collapse = "\n")
  expect_match(src, "ink_ok <- !is.null(ink) && identical(ink_pages, as.integer(np))",
               fixed = TRUE)
  # and a one-page document still gets its scan, which is the case that always worked
  f <- .ink_pdf("signed_minus_as_ink")
  m <- read_input(f)$meta
  expect_true(isTRUE(m$ink_scan_ok))
  expect_identical(as.integer(m$ink_scan_pages), 1L)
})

# ---------------------------------------------------------------------------
# THE SAME RENDER PASS ALSO SAYS WHICH PAGES COULD BE HIDING SOMETHING.
#
# The vector-redaction scan rasterises a page to check whether each word came out
# solid. It is the only thing that catches a box DRAWN over text still present in the
# text layer, which would otherwise leak blacked-out text into the spreadsheet -- and
# it costs one external rasterisation PER PAGE: measured, about 11 of the 14.6 seconds
# that reading a 100-page statement took, nearly all of it on statements with no
# redactions at all.
#
# A word can only be hidden by paint, and paint is a filled path or an image. Both are
# already in the renderer's output, produced in ONE pass for the sign check. So a page
# that draws neither needs no rasterising, and the question costs nothing.

test_that("a statement read WITHOUT the sign scan says so, loudly", {
  # The reason this is severity `high` and owner `escalate`: both ink faults are
  # invisible to the text layer, in OPPOSITE directions, and on a statement with no
  # running balance nothing else catches either -- the figures come out inverted and
  # every check passes. So the only honest output is "this check did not happen".
  d <- build_diagnostics("ok", metadata = list(ink_scan_ran = FALSE))
  expect_true("sign_scan_unavailable" %in% d$category)
  i <- which(d$category == "sign_scan_unavailable")
  expect_identical(d$severity[i], "high")
  expect_match(d$detail[i], "inverted")
  expect_match(d$how_to_fix[i], "pdftocairo", fixed = TRUE)
  expect_identical(unname(.diag_fix_owner("sign_scan_unavailable")), "escalate")

  # ...and it does NOT fire when the scan ran, nor on a delimited file, which has no
  # page to read ink from and must not be told its signs are in doubt.
  expect_false("sign_scan_unavailable" %in%
    build_diagnostics("ok", metadata = list(ink_scan_ran = TRUE))$category)
  expect_false("sign_scan_unavailable" %in%
    build_diagnostics("ok", metadata = list())$category)
})
