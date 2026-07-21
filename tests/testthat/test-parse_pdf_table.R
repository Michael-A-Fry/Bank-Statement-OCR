# Tests for the PDF table parser's generic features that real statements need:
#  - debit/credit as two separate columns (Withdrawals | Deposits)
#  - year-less dates ("05 Jan") with the year taken from the statement period
# Uses a SYNTHETIC word-box table (no real statement committed).

# .word(text, x, y, w) -- one word box; centre x = x + w/2 drives column choice.
.mk_words <- function() {
  rows <- list(
    # header row (y=10) -- date cell won't parse, so it is dropped
    c("Date", 45, 10, 20), c("Withdrawals", 340, 10, 50), c("Deposits", 420, 10, 40), c("Balance", 490, 10, 40),
    # 05 Jan  COFFEE            4.50 (debit)                 95.50
    c("05", 45, 40, 12), c("Jan", 60, 40, 16), c("COFFEE", 90, 40, 45),
    c("4.50", 355, 40, 22), c("95.50", 488, 40, 30),
    # 06 Jan  SALARY                        1000.00 (credit) 1095.50
    c("06", 45, 70, 12), c("Jan", 60, 70, 16), c("SALARY", 90, 70, 45),
    c("1000.00", 415, 70, 34), c("1095.50", 485, 70, 34)
  )
  data.frame(
    text   = vapply(rows, `[`, "", 1),
    x      = as.numeric(vapply(rows, `[`, "", 2)),
    y      = as.numeric(vapply(rows, `[`, "", 3)),
    width  = as.numeric(vapply(rows, `[`, "", 4)),
    height = rep(10, length(rows)),
    stringsAsFactors = FALSE)
}

.mk_input <- function() list(
  kind = "pdf",
  path = tempfile(fileext = ".pdf"),   # does not exist -> pdf_pagesize skipped
  pages = c("Statement period from 1 Jan 2026 to 31 Jan 2026\nDate Withdrawals Deposits Balance"),
  words = list(.mk_words()),
  meta = list(page_count = 1L))

.tmpl <- list(
  id = "synth_pdf", bank = "SYNTH", statement_type = "everyday", format = "pdf",
  version = 1, currency = "NZD",
  table = list(row_tol = 3, date_format = "%d %b", amount_sign = "debit_credit_cols",
    columns = list(
      date = list(x_min = 40, x_max = 80), description = list(x_min = 80, x_max = 330),
      debit = list(x_min = 330, x_max = 395), credit = list(x_min = 395, x_max = 472),
      balance = list(x_min = 472, x_max = 545))))

test_that("PDF parser splits debit/credit columns and injects the period year", {
  parsed <- parse_pdf_table(.mk_input(), .tmpl)
  tx <- parsed$transactions
  expect_equal(nrow(tx), 2L)                       # header row dropped
  expect_equal(tx$date, c("2026-01-05", "2026-01-06"))   # year 2026 from the period
  expect_equal(tx$date_raw, c("05 Jan", "06 Jan"))       # raw stays verbatim, no year
  expect_equal(tx$amount, c(-4.50, 1000.00))             # debit negative, credit positive
  expect_equal(tx$direction, c("debit", "credit"))
  expect_equal(tx$balance, c(95.50, 1095.50))
})

test_that("the two-column PDF template validates", {
  expect_length(validate_template(c(.tmpl, list(
    min_score = 1, fingerprint = list(page_contains_all = list("Withdrawals"))))), 0)
})

test_that("a row is kept only with a real amount; type codes don't break the date", {
  # Westpac-style: a type code (DC) after the date, a date-only header line, and
  # a real transaction. Date band must exclude the type code; the header line has
  # no amount so it must be dropped.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("9",  "May",                                 # y=10: a stray issue-date line (NO amount) -> drop
              "05", "Jan", "DC", "RENT", "500.00", "95.50",# y=40: real txn (type code DC after date)
              "06", "Jan",       "PAY",           "1000.00", "1095.50"),
    x     = c(45, 60,   45, 60, 79, 110, 415, 490,   45, 60, 110, 415, 490),
    y     = c(10, 10,   40, 40, 40, 40,  40,  40,    70, 70, 70,  70,  70),
    width = c(12, 16,   12, 16, 14, 40,  34,  30,    12, 16, 40,  34,  30),
    height = rep(10, 13))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement Opening date 1 Jan 2026 Closing date 31 Jan 2026\nOpening balance 595.50\nClosing balance 1095.50"),
    words = list(words), meta = list(page_count = 1L))
  tmpl <- list(id = "wp_synth", bank = "S", statement_type = "everyday", format = "pdf",
    version = 1, currency = "NZD", table = list(row_tol = 3, date_format = "%d %b",
      amount_sign = "debit_credit_cols", columns = list(
        date = list(x_min = 40, x_max = 74),          # excludes the DC type code at x~79
        description = list(x_min = 74, x_max = 360),
        debit = list(x_min = 360, x_max = 440), credit = list(x_min = 440, x_max = 520),
        balance = list(x_min = 470, x_max = 545))))
  # widen so debit vs credit don't overlap balance in this tiny fixture
  tmpl$table$columns$credit$x_max <- 465
  p <- parse_pdf_table(input, tmpl)
  tx <- p$transactions
  expect_equal(nrow(tx), 2L)                       # stray date-only line dropped
  expect_equal(tx$date, c("2026-01-05", "2026-01-06"))
  expect_true("RENT" %in% tx$description[1] || grepl("RENT", tx$description[1]))
  # opening/closing balance wired into the header -> balance_reconciliation runs
  expect_equal(p$header$opening_balance, 595.50)
  expect_equal(p$header$closing_balance, 1095.50)
})

test_that("summary lines are dropped and 2-digit period years resolve", {
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",                  # real txn
              "31","Jan","Closing","Balance","1234.00","1234.00"), # summary line -> drop
    x     = c(45,60,110,415,490,   45,60,110,180,415,490),
    y     = c(40,40,40,40,40,      70,70,70,70,70,70),
    width = c(12,16,45,25,30,      12,16,50,50,34,30),
    height = rep(10, 11))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    # period uses a 2-DIGIT year -> must resolve to 2026 (not 0026)
    pages = c("Statement Opening date 1 Jan 26 Closing date 31 Jan 26"),
    words = list(words), meta = list(page_count = 1L))
  tmpl <- list(id = "s", bank = "S", statement_type = "e", format = "pdf", version = 1,
    currency = "NZD", table = list(row_tol = 3, date_format = "%d %b", amount_sign = "signed",
      columns = list(date = list(x_min = 40, x_max = 74), description = list(x_min = 74, x_max = 360),
        amount = list(x_min = 360, x_max = 470), balance = list(x_min = 470, x_max = 545))))
  tx <- parse_pdf_table(input, tmpl)$transactions
  expect_equal(nrow(tx), 1L)                        # "Closing Balance" row dropped
  expect_equal(tx$date, "2026-01-05")              # 2-digit year -> 2026, not 0026
})

.simple_tmpl <- function(row_tol = 3) list(id = "s", bank = "S", statement_type = "e",
  format = "pdf", version = 1, currency = "NZD",
  table = list(row_tol = row_tol, date_format = "%d %b", amount_sign = "signed",
    columns = list(date = list(x_min = 40, x_max = 74), description = list(x_min = 74, x_max = 360),
      amount = list(x_min = 360, x_max = 470), balance = list(x_min = 470, x_max = 545))))

test_that("multi-line descriptions are folded into the transaction, footers are not", {
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","VISA","SHOP","-40.00","955.50",   # y=40 txn line
              "CARD","1234","Orig","06/01",                  # y=52 continuation (no date, no money)
              "06","Jan","PAY","-10.00","945.50",            # y=80 next txn
              "Page","1","of","1"),                          # y=300 footer -> must NOT merge
    x     = c(45,60,110,150,415,490,   110,150,190,230,   45,60,110,415,490,   250,275,290,305),
    y     = c(40,40,40,40,40,40,       52,52,52,52,        80,80,80,80,80,      300,300,300,300),
    width = c(12,16,30,40,34,30,       30,30,30,30,        12,16,30,34,30,      25,10,15,10),
    height = rep(10, 19))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .simple_tmpl())$transactions
  expect_equal(nrow(tx), 2L)                                  # 2 txns; footer + continuation not counted
  expect_true(grepl("SHOP", tx$description[1]) && grepl("CARD 1234", tx$description[1]))  # continuation folded in
  expect_false(grepl("Page", paste(tx$description, collapse = " ")))  # footer excluded
})

test_that("a differently-sized page normalises to the reference (scan/scale fix)", {
  # Same statement, two physical sizes. A template has no explicit ref -> defaults
  # to A4; the A4 page is untouched and the 2x page is normalised back to it, so
  # BOTH yield the same rows. Before the fix the 2x page dropped every row.
  base_words <- function(mult) data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",  "06","Jan","PAY","10.00","105.50"),
    x     = c(45,60,110,415,490,   45,60,110,415,490) * mult,
    y     = c(40,40,40,40,40,      70,70,70,70,70)    * mult,
    width = c(12,16,45,25,30,      12,16,30,30,30)    * mult,
    height= rep(10 * mult, 10))
  mkinput <- function(mult, pw, ph) list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"),
    words = list(base_words(mult)), page_width = pw, page_height = ph,
    meta = list(page_count = 1L))
  ref <- parse_pdf_table(mkinput(1, 595.28, 841.89), .simple_tmpl())$transactions
  big <- parse_pdf_table(mkinput(2, 1190.56, 1683.78), .simple_tmpl())$transactions
  expect_equal(nrow(ref), 2L)
  expect_equal(nrow(big), 2L)                       # 2x page: rows recovered, not dropped
  expect_equal(big$date, ref$date)
  expect_equal(big$amount, ref$amount)              # amounts land in the same bands
})

test_that("a template's recorded reference page size drives normalisation", {
  # A template built on a 1190-wide page: its bands AND its ref live in that space.
  # An A4 copy (595 wide) of the statement is scaled UP into the 1190 bands.
  tmpl <- .simple_tmpl(); tmpl$table$ref_width <- 1190.56; tmpl$table$ref_height <- 1683.78
  for (k in names(tmpl$table$columns)) { b <- tmpl$table$columns[[k]]
    tmpl$table$columns[[k]] <- list(x_min = b$x_min * 2, x_max = b$x_max * 2) }
  w <- data.frame(stringsAsFactors = FALSE,
    text=c("05","Jan","COFFEE","4.50","95.50"), x=c(45,60,110,415,490), y=rep(40,5),
    width=c(12,16,45,25,30), height=rep(10,5))
  inp <- list(kind="pdf", path=tempfile(fileext=".pdf"), pages="period 1 Jan 2026 to 31 Jan 2026",
    words=list(w), page_width=595.28, page_height=841.89, meta=list(page_count=1L))
  expect_equal(nrow(parse_pdf_table(inp, tmpl)$transactions), 1L)
})

test_that("missing page dimensions are a safe no-op (backward compatible)", {
  # An input without page_width/height (older reader) must parse exactly as before.
  w <- data.frame(stringsAsFactors = FALSE,
    text=c("05","Jan","COFFEE","4.50","95.50"), x=c(45,60,110,415,490), y=rep(40,5),
    width=c(12,16,45,25,30), height=rep(10,5))
  inp <- list(kind="pdf", path=tempfile(fileext=".pdf"), pages="period 1 Jan 2026 to 31 Jan 2026",
    words=list(w), meta=list(page_count=1L))   # no page_width/height
  expect_equal(nrow(parse_pdf_table(inp, .simple_tmpl())$transactions), 1L)
})

test_that("force_rows adds a dropped row back as a transaction, flagged 'forced'", {
  # y=70 is a dated line with NO amount -> normally dropped. The user boxes it in
  # the X-ray ("this IS a transaction"), so force_rows keeps it -- flagged forced
  # + malformed (its amount genuinely couldn't be read), never silently trusted.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",     # y=40 real txn
              "06","Jan","NOTE-ONLY"),                 # y=70 dated, NO amount -> dropped
    x     = c(45,60,110,415,490,   45,60,110),
    y     = c(40,40,40,40,40,      70,70,70),
    width = c(12,16,45,25,30,      12,16,60),
    height = rep(10, 8))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  base <- parse_pdf_table(input, .simple_tmpl())$transactions
  expect_equal(nrow(base), 1L)                                   # dropped by default

  forced <- parse_pdf_table(input, .simple_tmpl(),
    force_rows = list(list(page = 1, y_min = 65, y_max = 75)))$transactions
  expect_equal(nrow(forced), 2L)                                 # the boxed row is back
  fr <- forced[grepl("NOTE-ONLY", forced$description), ]
  expect_equal(nrow(fr), 1L)
  expect_equal(fr$date, "2026-01-06")                            # date still parsed + year-filled
  expect_true(is.na(fr$amount))                                  # amount genuinely unknown
  expect_match(fr$flags, "forced")
  expect_match(fr$flags, "malformed")                            # and it says the amount is missing
})

test_that("force_rows on an unparseable date keeps the row but flags date_unresolved", {
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",   # y=40 real txn
              "XX","Yy","THING","9.99","5.00"),      # y=70 bad date, but a real amount
    x     = c(45,60,110,415,490,   45,60,110,415,490),
    y     = c(40,40,40,40,40,      70,70,70,70,70),
    width = c(12,16,45,25,30,      12,16,40,25,20),
    height = rep(10, 10))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  forced <- parse_pdf_table(input, .simple_tmpl(),
    force_rows = list(list(page = 1, y_min = 68, y_max = 78)))$transactions
  expect_equal(nrow(forced), 2L)
  fr <- forced[grepl("THING", forced$description), ]
  expect_true(is.na(fr$date))                        # date genuinely unresolved
  expect_equal(fr$amount, 9.99)                      # amount was fine
  expect_match(fr$flags, "forced")
  expect_match(fr$flags, "date_unresolved")
})

test_that("force_rows that overlaps nothing is a safe no-op", {
  input <- .mk_input()
  base <- parse_pdf_table(input, .tmpl)$transactions
  same <- parse_pdf_table(input, .tmpl,
    force_rows = list(list(page = 1, y_min = 5000, y_max = 5010)))$transactions
  expect_equal(nrow(same), nrow(base))
})

test_that("metadata_regions pins a header value the label engine misses", {
  # A closing-balance value that sits below the table with no wording the label
  # dictionary recognises: the automatic reader can't find it, but a drawn box can.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",    # a normal transaction row
              "1,234.56"),                            # the closing balance value, on its own
    x     = c(45,60,110,415,490,   200),
    y     = c(40,40,40,40,40,      120),
    width = c(12,16,45,25,30,      60),
    height = rep(10, 6))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  base <- parse_pdf_table(input, .simple_tmpl())$header
  expect_true(is.na(base$closing_balance))           # not found without a region

  tmpl <- .simple_tmpl()
  tmpl$table$metadata_regions <- list(
    closing_balance = list(page = 1, x_min = 195, x_max = 265, y_min = 115, y_max = 132))
  h <- parse_pdf_table(input, tmpl)$header
  expect_equal(h$closing_balance, 1234.56)           # the box pins it
  # the pinned box must NOT invent a transaction row
  expect_equal(nrow(parse_pdf_table(input, tmpl)$transactions), 1L)
})

test_that("metadata_regions validates: good passes, malformed / unknown rejected", {
  base <- c(.tmpl, list(min_score = 1, fingerprint = list(page_contains_all = list("Withdrawals"))))
  ok <- base
  ok$table$metadata_regions <- list(
    closing_balance = list(page = 1, x_min = 195, x_max = 265, y_min = 115, y_max = 132))
  expect_length(validate_template(ok), 0)
  bad1 <- base; bad1$table$metadata_regions <- list(closing_balance = list(x_min = 195))  # no x_max
  expect_true(any(grepl("metadata_regions.closing_balance", validate_template(bad1))))
  bad2 <- base; bad2$table$metadata_regions <- list(total_spend = list(x_min = 1, x_max = 2))  # unknown
  expect_true(any(grepl("metadata_regions.total_spend", validate_template(bad2))))
})

test_that("tightly-set lines are NOT merged into one giant row (row-height grouping)", {
  # line pitch 4pt: the OLD cumsum(diff(y)>tol) merged both lines into one row
  # (no word-gap exceeded 3); the anchored grouping keeps them as two transactions.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","AAA","-1.00","10.00",     # y=40
              "06","Jan","BBB","-2.00","8.00"),      # y=44 (only 4pt below)
    x     = c(45,60,110,415,490,   45,60,110,415,490),
    y     = c(40,40,40,40,40,      44,44,44,44,44),
    width = c(12,16,30,30,30,      12,16,30,30,30),
    height = rep(9, 10))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .simple_tmpl())$transactions
  expect_equal(nrow(tx), 2L)                          # two rows, not one merged blob
  expect_equal(tx$date, c("2026-01-05", "2026-01-06"))
})
