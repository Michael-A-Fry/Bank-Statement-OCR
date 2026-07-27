# Tests for the "X-ray" overlay geometry (R/inspect.R).

.mkw <- function(text, x, y, wd = 20, ht = 8, red = FALSE)
  data.frame(text = text, x = x, y = y, width = wd, height = ht,
             redacted = red, stringsAsFactors = FALSE)

.demo_input <- function() {
  page <- do.call(rbind, list(
    .mkw("Date", 50, 40), .mkw("Details", 120, 40), .mkw("Amount", 300, 40),      # header
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),
    .mkw("02/04/2025", 50, 80), .mkw("Rent", 120, 80), .mkw("1,200.00", 300, 80),
    .mkw("Closing", 120, 120), .mkw("Balance", 160, 120), .mkw("3,800.00", 300, 120)))
  list(words = list(page))
}
.demo_tmpl <- function() list(format = "pdf", table = list(
  region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
  date_format = "%d/%m/%Y", amount_sign = "signed",
  columns = list(date = list(x_min = 40, x_max = 110),
                 description = list(x_min = 111, x_max = 280),
                 amount = list(x_min = 281, x_max = 360))))

test_that("inspect_pdf_layout assigns words to columns exactly as the engine", {
  p <- inspect_pdf_layout(.demo_input(), .demo_tmpl())$pages[["1"]]
  # header (y=40) and summary (y=120) are outside the region -> not assigned
  expect_equal(sum(p$words$in_region), 6L)
  incol <- p$words$column[p$words$in_region]
  expect_setequal(incol, c("date", "description", "amount"))
  expect_equal(sum(p$words$column == "amount", na.rm = TRUE), 2L)
})

test_that("inspect_pdf_layout boxes the kept transaction rows only", {
  p <- inspect_pdf_layout(.demo_input(), .demo_tmpl())$pages[["1"]]
  expect_equal(nrow(p$rows), 2L)          # two in-region visual rows
  expect_true(all(p$rows$kept))           # both have a parseable date
  expect_true(all(p$rows$x1 > p$rows$x0 & p$rows$y1 > p$rows$y0))
})

test_that("inspect kept mirrors the engine: no amount / summary rows are NOT kept", {
  # region now includes a date-only line and a 'Closing Balance' summary line
  page <- do.call(rbind, list(
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),  # txn
    .mkw("02/04/2025", 50, 80), .mkw("Statement", 120, 80), .mkw("date", 160, 80),    # dated, NO amount
    .mkw("30/04/2025", 50, 100), .mkw("Closing", 120, 100), .mkw("Balance", 165, 100),
      .mkw("3,800.00", 300, 100)))                                                     # dated summary + amount
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  p <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_equal(nrow(p$rows), 3L)
  # only the real transaction is kept; the date-only line (no amount) and the
  # 'Closing Balance' summary line are NOT - exactly as parse_pdf_table drops them.
  expect_equal(sum(p$rows$kept), 1L)
  expect_true(p$rows$kept[grepl("^01/04", p$rows$date)])
  expect_false(p$rows$kept[grepl("^02/04", p$rows$date)])   # no amount
  expect_false(p$rows$kept[grepl("^30/04", p$rows$date)])   # summary line
})

test_that("inspect reason explains WHY each dropped row was skipped", {
  page <- do.call(rbind, list(
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),  # kept txn
    .mkw("02/04/2025", 50, 80), .mkw("Statement", 120, 80), .mkw("date", 160, 80),    # dated, NO amount
    .mkw("30/04/2025", 50, 100), .mkw("Closing", 120, 100), .mkw("Balance", 165, 100),
      .mkw("3,800.00", 300, 100)))                                                     # dated summary + amount
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  p <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_true("reason" %in% names(p$rows))
  expect_equal(p$rows$reason[grepl("^01/04", p$rows$date)], "")          # kept -> no reason
  expect_match(p$rows$reason[grepl("^02/04", p$rows$date)], "no amount")  # dated, no money
  expect_match(p$rows$reason[grepl("^30/04", p$rows$date)], "summary")    # closing balance
})

test_that("inspect flags a row whose date didn't parse (the fixable case)", {
  page <- do.call(rbind, list(
    .mkw("13-14-9999", 50, 60), .mkw("Odd", 120, 60), .mkw("500.00", 300, 60)))
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  p <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_false(p$rows$kept[1])
  expect_match(p$rows$reason[1], "date didn't parse")   # points at the template fix
})

test_that("inspect marks a wrapped line as a continuation, not a missed transaction", {
  page <- do.call(rbind, list(
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),  # kept txn
    .mkw("Employer", 120, 70), .mkw("Ltd", 165, 70)))                                # wrap right below it
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  p <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_equal(nrow(p$rows), 2L)
  expect_true(p$rows$kept[1])
  expect_false(p$rows$kept[2])
  expect_match(p$rows$reason[2], "continuation")
})

test_that("inspect paints a force_rows band as kept (matches the reader)", {
  page <- do.call(rbind, list(
    .mkw("01/04/2025", 50, 60), .mkw("Salary", 120, 60), .mkw("2,500.00", 300, 60),  # kept txn
    .mkw("02/04/2025", 50, 80), .mkw("Statement", 120, 80), .mkw("date", 160, 80)))   # dated, NO amount
  tmpl <- list(format = "pdf", table = list(
    region = list(x_min = 40, x_max = 360, y_min = 50, y_max = 110),
    date_format = "%d/%m/%Y", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 110),
                   description = list(x_min = 111, x_max = 280),
                   amount = list(x_min = 281, x_max = 360))))
  base <- inspect_pdf_layout(list(words = list(page)), tmpl)$pages[["1"]]
  expect_false(base$rows$kept[grepl("^02/04", base$rows$date)])            # skipped by default
  forced <- inspect_pdf_layout(list(words = list(page)), tmpl,
    force_rows = list(list(page = 1, y_min = 78, y_max = 82)))$pages[["1"]]
  i <- grepl("^02/04", forced$rows$date)
  expect_true(forced$rows$kept[i])                                         # now painted kept
  expect_equal(forced$rows$reason[i], "")                                  # ...and off the skipped list
})

# ---- THE BAND FRAME: the X-ray draws bands where the READER reads them --------
# R/parse_pdf_table.R declares the frame once and exports pdf_band_frame() +
# pdf_band_frame_scale(); the contract names the X-ray as one of the DRAWERS, which
# divide a band by the scale to land on the page. inspect.R used to keep a private
# copy of the frame, the scale and the snap, and the copy was NOT equivalent: it
# snapped on |page/frame - 1| < snap where the shared function snaps on
# |frame/page - 1| < snap. For a page 2.00-2.04% off the frame exactly one of them
# treated the page as the same size, so the X-ray drew a band up to ~11pt from
# where the reader matched it -- and the X-ray is what an analyst uses to decide a
# template is wrong. These pin the two directions to the shared functions.

# .reader_columns(input, tmpl) -- the column each word is read into by the READER's
# own rule: scale the WORDS into the frame (.words_to_band_frame, the reader's
# helper) and test them against the stored bands. Whatever the X-ray draws, its
# word-to-column answer has to be this one.
.reader_columns <- function(input, tmpl) {
  frame <- pdf_band_frame(tmpl)
  cols <- tmpl$table$columns
  unlist(lapply(seq_along(input$words), function(p) {
    w <- .words_to_band_frame(input$words[[p]], frame,
                              input$page_width[p], input$page_height[p])
    vapply(w$x + w$width / 2, .word_column, character(1), cols)
  }), use.names = FALSE)
}

.bf_input <- function(pw = 595.28, ph = 841.89) {
  # the last word's centre (79) sits inside the stored date band [40,80] but
  # OUTSIDE the page-space band the reader matches once the frame scale bites --
  # so a wrong scale shows up as a wrong column, not just a wrong picture.
  w <- data.frame(stringsAsFactors = FALSE,
    text  = c("05", "Jan", "COFFEE", "4.50", "95.50", "X"),
    x     = c(45, 60, 110, 415, 490, 74),
    y     = c(40, 40, 40, 40, 40, 70),
    width = c(12, 16, 45, 25, 30, 10),
    height = rep(10, 6))
  list(kind = "pdf", path = tempfile(fileext = ".pdf"),
       pages = "Statement period 1 Jan 2026 to 31 Jan 2026", words = list(w),
       page_width = pw, page_height = ph, meta = list(page_count = 1L))
}
.bf_tmpl <- function(ratio) list(id = "bf", bank = "BF", statement_type = "e",
  format = "pdf", version = 1, currency = "NZD",
  table = list(row_tol = 3, date_format = "%d %b", amount_sign = "signed",
    ref_width = 595.28 * ratio, ref_height = 841.89 * ratio,   # the frame, vs the page
    columns = list(date = list(x_min = 40, x_max = 80),
                   description = list(x_min = 80, x_max = 360),
                   amount = list(x_min = 360, x_max = 470),
                   balance = list(x_min = 470, x_max = 545))))

test_that("the X-ray divides a band by the shared frame scale, and nothing else", {
  # ratios chosen either side of the snap AND inside the window where the old
  # reciprocal snap disagreed with the shared one (1.0200 <= r < 1.020408, and
  # 0.98 < r <= 0.980392).
  for (r in c(0.5, 0.95, 0.9802, 0.99, 1.0, 1.01, 1.0201, 1.05, 2.0)) {
    tmpl <- .bf_tmpl(r); input <- .bf_input()
    s <- pdf_band_frame_scale(pdf_band_frame(tmpl), input$page_width, input$page_height)
    drawn <- inspect_pdf_layout(input, tmpl)$pages[["1"]]$bands
    for (nm in names(tmpl$table$columns)) {
      expect_equal(drawn[[nm]]$x_min, tmpl$table$columns[[nm]]$x_min / s[1], info = paste(r, nm))
      expect_equal(drawn[[nm]]$x_max, tmpl$table$columns[[nm]]$x_max / s[1], info = paste(r, nm))
    }
  }
})

test_that("the X-ray's columns match the reader's, right through the snap window", {
  for (r in c(0.5, 0.95, 0.9799, 0.9802, 0.9805, 0.99, 1.0, 1.0199, 1.0201,
              1.0205, 1.05, 2.0)) {
    tmpl <- .bf_tmpl(r); input <- .bf_input()
    xray <- inspect_pdf_layout(input, tmpl)$pages[["1"]]$words$column
    expect_identical(xray, .reader_columns(input, tmpl), info = sprintf("frame = page x %.4f", r))
  }
})

test_that("the reciprocal snap really did move a band, and no longer does", {
  # r = 1.0201: |frame/page - 1| = 0.0201 -> the shared scale does NOT snap, while
  # |page/frame - 1| = 0.0197 -> the old private copy DID, and drew the band at its
  # stored coordinate instead of 1/1.0201 of it.
  r <- 1.0201; tmpl <- .bf_tmpl(r); input <- .bf_input()
  s <- pdf_band_frame_scale(pdf_band_frame(tmpl), input$page_width, input$page_height)
  expect_false(s[1] == 1)                                  # the shared rule scales
  expect_lt(abs(input$page_width / (595.28 * r) - 1), .PAGE_SCALE_SNAP)  # the old one snapped
  drawn <- inspect_pdf_layout(input, tmpl)$pages[["1"]]$bands$balance
  expect_equal(drawn$x_max, 545 / s[1])
  expect_gt(abs(drawn$x_max - 545), 10)                    # ~11pt from where it used to be drawn
})

test_that("a stored force_rows band is put on the page by the same scale", {
  # force_rows y-bands are stored in the frame like every other band, so the X-ray
  # DIVIDES them onto the page too -- and the reader tests them in the frame. At
  # r = 0.9802 the shared scale is 1 while the old private copy came out 1.0202, so
  # a band ending at y=69 was stretched to 70.4 and swallowed the y=70 line: the
  # X-ray painted a row kept that the reader had NOT forced.
  r <- 0.9802; tmpl <- .bf_tmpl(r); input <- .bf_input()
  s <- pdf_band_frame_scale(pdf_band_frame(tmpl), input$page_width, input$page_height)
  expect_equal(s[2], 1)                        # the shared rule treats this as A4
  xr_kept <- function(fr) sum(inspect_pdf_layout(input, tmpl, force_rows = fr)$pages[["1"]]$rows$kept)
  rd_kept <- function(fr) nrow(parse_pdf_table(input, tmpl, force_rows = fr)$transactions)
  just_short <- list(list(page = 1, y_min = 60, y_max = 69))   # stops above the y=70 line
  reaches    <- list(list(page = 1, y_min = 60, y_max = 71))   # touches it
  expect_equal(xr_kept(just_short), rd_kept(just_short))
  expect_equal(xr_kept(reaches),    rd_kept(reaches))
  expect_equal(xr_kept(reaches), xr_kept(just_short) + 1L)     # the band really bites
})

test_that("inspect.R keeps no private copy of the band frame", {
  src <- readLines(file.path(engine_root(), "R", "inspect.R"), warn = FALSE)
  code <- sub("#.*$", "", src)                 # comments may name them; code may not
  joined <- paste(code, collapse = "\n")
  for (dead in c("ref_width", "ref_height", ".PAGE_SCALE_SNAP", ".A4_W", ".A4_H",
                 ".page_scale"))
    expect_false(grepl(dead, joined, fixed = TRUE), info = dead)
  expect_true(grepl("pdf_band_frame(template)", joined, fixed = TRUE))
  expect_true(grepl("pdf_band_frame_scale(frame,", joined, fixed = TRUE))
})

test_that("locate_values_on_page boxes single- and multi-token values", {
  page <- .demo_input()$words[[1]]
  loc <- locate_values_on_page(page, list(closing_balance = "3,800.00",
                                          period_start = "01/04/2025",
                                          missing = "9,999.99"))
  expect_true(loc$found[loc$field == "closing_balance"])
  expect_equal(loc$x0[loc$field == "closing_balance"], 300)
  expect_false(loc$found[loc$field == "missing"])   # not on the page -> not found
})

test_that("inspect_pdf_layout is well-formed on an empty / word-less page", {
  lay <- inspect_pdf_layout(list(words = list(NULL)), .demo_tmpl())
  p <- lay$pages[["1"]]
  expect_equal(nrow(p$words), 0L)
  expect_equal(nrow(p$rows), 0L)
})

# ---- the X-ray and the reader must agree, row for row ----------------------
# The two used to carry SEPARATE copies of the keep rule and drifted: the reader
# grew a keep_dateless_rows branch the overlay never got, so it kept rows the X-ray
# reported as SKIPPED and row_coverage told the analyst "N row(s) skipped for an
# unreadable date" -- prescribing the exact wrong remedy for rows that were never
# lost. They now share pdf_keep_row(); this asserts the counts can't diverge again.
.xr_tmpl <- function() list(id = "s", bank = "S", statement_type = "e", format = "pdf",
  version = 1, currency = "NZD",
  table = list(row_tol = 3, date_format = "%d %b", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 74),
                   description = list(x_min = 74, x_max = 360),
                   amount = list(x_min = 360, x_max = 470),
                   balance = list(x_min = 470, x_max = 545))))
.xr_input <- function(words) list(kind = "pdf", path = tempfile(fileext = ".pdf"),
  pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
  page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))
.xr_kept <- function(input, tmpl) sum(vapply(inspect_pdf_layout(input, tmpl)$pages,
  function(P) if (is.null(P$rows) || !nrow(P$rows)) 0L else sum(P$rows$kept), integer(1)))

test_that("inspect's kept count equals the reader's row count (keep_dateless)", {
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",
              "SHOP","10.00","85.50",                 # shares the date above
              "BOOKS","5.00","80.50"),                 # shares the date above
    x     = c(45,60,110,415,490,   110,415,490,   110,415,490),
    y     = c(40,40,40,40,40,      70,70,70,      100,100,100),
    width = c(12,16,45,25,30,      45,25,30,      45,25,30),
    height= rep(9, 11))
  input <- .xr_input(words)
  tmpl <- .xr_tmpl(); tmpl$table$merge_continuation <- FALSE

  off <- tmpl
  expect_equal(.xr_kept(input, off), nrow(parse_pdf_table(input, off)$transactions))

  on <- tmpl; on$table$keep_dateless_rows <- TRUE
  expect_equal(nrow(parse_pdf_table(input, on)$transactions), 3L)   # sanity: reader keeps 3
  expect_equal(.xr_kept(input, on), 3L)                             # X-ray now paints 3
})

test_that("inspect's kept count equals the reader's row count (redacted date)", {
  R <- "[REDACTED]"
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c(R,"COFFEE","-40.00","955.50",           # date hidden, real amount -> kept
              "06","Jan","SHOP","-10.00","945.50",
              R,R,R,R),                                # wholly redacted -> never a transaction
    x     = c(45,110,415,490,   45,60,110,415,490,   45,110,415,490),
    y     = c(40,40,40,40,      70,70,70,70,70,      100,100,100,100),
    width = c(55,45,34,30,      12,16,45,34,30,      55,45,34,30),
    height= rep(10, 13))
  # the X-ray reads redaction off the word-level `redacted` column
  words$redacted <- grepl("REDACT", words$text)
  input <- .xr_input(words)
  expect_equal(nrow(parse_pdf_table(input, .xr_tmpl())$transactions), 2L)
  expect_equal(.xr_kept(input, .xr_tmpl()), 2L)
})

test_that("inspect's kept count equals the reader's row count (stitched split rows)", {
  # One transaction per pair, staggered so the date and the amount land in
  # DIFFERENT visual rows. The reader stitches them; the overlay used to show both
  # halves as skipped, so its count could never match.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("COFFEE","4.50","95.50",   "05","Jan",
              "RENT","10.00","85.50",     "06","Jan"),
    x     = c(110,415,490,   45,60,        110,415,490,     45,60),
    y     = c(40,40,40,       45,45,        70,70,70,        75,75),
    width = c(45,25,30,       12,16,        30,30,30,        12,16),
    height= rep(9, 10))
  input <- .xr_input(words)
  tx <- parse_pdf_table(input, .xr_tmpl())$transactions
  expect_equal(nrow(tx), 2L)
  expect_true(all(grepl("row_stitched", tx$flags)))
  expect_equal(.xr_kept(input, .xr_tmpl()), 2L)
  # the absorbed half is explained, not reported as a missed transaction
  rows <- inspect_pdf_layout(input, .xr_tmpl())$pages[["1"]]$rows
  expect_match(paste(rows$reason, collapse = " "), "split row")
})
