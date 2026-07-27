# Tests for the PDF table parser's generic features that real statements need:
#  - debit/credit as two separate columns (Withdrawals | Deposits)
#  - year-less dates ("05 Jan") with the year taken from the statement period
# Uses a SYNTHETIC word-box table (no real statement stored).

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

test_that("split-row recovery stitches a staggered date + amount back together", {
  # One transaction whose DATE sits 5pt below its amount/balance -> with row_tol 3
  # the date and the amount fall into different groups. Each half fails the keep
  # test on its own; the recovery re-joins them into one row, flagged row_stitched.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("COFFEE","4.50","95.50",   "05","Jan",       # row 1: amount y=40, date y=45
              "RENT","10.00","85.50",     "06","Jan"),      # row 2: amount y=70, date y=75
    x     = c(110,415,490,   45,60,        110,415,490,     45,60),
    y     = c(40,40,40,       45,45,        70,70,70,        75,75),
    width = c(45,25,30,       12,16,        30,30,30,        12,16),
    height= rep(9, 10))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .simple_tmpl())$transactions
  expect_equal(nrow(tx), 2L)                              # both staggered rows recovered
  expect_equal(tx$date, c("2026-01-05", "2026-01-06"))
  expect_equal(tx$amount, c(4.50, 10.00))
  expect_true(all(grepl("row_stitched", tx$flags)))       # honestly flagged as re-joined
})

test_that("split-row recovery does NOT merge a carried-forward line with a real row", {
  # A "Balance brought forward" line (date + balance, NO amount) followed by a real
  # transaction (its OWN date + amount). The amount row has a date -> not amount-only
  # -> the recovery must NOT fire, and only the real transaction is kept.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","Balance","brought","forward","225.89",   # carried fwd: date + balance, no amount
              "06","Jan","COFFEE","4.50","220.00"),                 # real txn: date + amount
    x     = c(45,60,110,150,190,490,   45,60,110,415,490),
    y     = c(40,40,40,40,40,40,        70,70,70,70,70),
    width = c(12,16,45,45,45,30,        12,16,45,25,30),
    height= rep(9, 11))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .simple_tmpl())$transactions
  expect_equal(nrow(tx), 1L)                              # only the real transaction
  expect_false(any(grepl("row_stitched", tx$flags)))      # nothing was stitched
})

test_that("partial redactions keep the row (null+flag); a wholly-redacted row does not appear", {
  # Statements ARRIVE already redacted; the reader records what is still visible.
  # A PARTIALLY-redacted transaction (date, amount or balance hidden but a real
  # date or amount still showing) is kept: the hidden value is NULLED (never
  # fabricated) and the row is flagged. A row that is WHOLLY redacted has no real
  # date or amount, so it is not a transaction -- it simply does not appear, and we
  # never guess it was there. Neighbours above/below are untouched.
  R <- "[REDACTED]"
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","-40.00","955.50",  # y=40 clean baseline row
              R,"SHOP","-10.00","945.50",              # y=70 DATE redacted
              "07","Jan","RENT",R,"935.50",            # y=100 AMOUNT redacted
              "08","Jan","BILL","-5.00",R,             # y=130 BALANCE redacted
              R,R,R,R),                                # y=160 WHOLE row redacted
    x     = c(45,60,110,415,490,   45,110,415,490,   45,60,110,415,490,
              45,60,110,415,490,   45,110,415,490),
    y     = c(40,40,40,40,40,      70,70,70,70,       100,100,100,100,100,
              130,130,130,130,130, 160,160,160,160),
    width = c(12,16,45,34,30,      55,45,34,30,       12,16,45,34,30,
              12,16,45,34,30,      55,45,34,30),
    height = rep(10, 23))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .simple_tmpl())$transactions

  # 4 partial rows recorded; the wholly-redacted 5th row does NOT appear.
  expect_equal(nrow(tx), 4L)
  isred <- grepl("redacted", tx$flags, ignore.case = TRUE)

  # row 1: clean -> no redaction flag, values intact
  expect_false(isred[1])
  expect_equal(tx$amount[1], -40.00); expect_equal(tx$balance[1], 955.50)

  # row 2: DATE hidden -> row kept (real amount), date_iso NA, amount preserved, flagged
  expect_true(isred[2])
  expect_true(is.na(tx$date[2]))
  expect_equal(tx$amount[2], -10.00)                      # a redacted DATE never loses the amount

  # row 3: AMOUNT hidden -> row kept (real date), amount NULLED (not fabricated), flagged
  expect_true(isred[3])
  expect_true(is.na(tx$amount[3]))
  expect_equal(tx$balance[3], 935.50)

  # row 4: BALANCE hidden -> balance NULLED, amount intact, flagged
  expect_true(isred[4])
  expect_true(is.na(tx$balance[4]))
  expect_equal(tx$amount[4], -5.00)

  # row 5 was WHOLLY redacted (no real date, no real amount) -> not a transaction,
  # so it is absent. We never fabricate a row from redaction alone.
  expect_equal(sum(is.na(tx$date) & is.na(tx$amount) & is.na(tx$balance)), 0L)
})

test_that("redacting the date of an EDGE transaction keeps it (never dropped)", {
  # Safety invariant: a redaction that covers the date of the FIRST (or last) real
  # transaction must NOT drop it -- redaction may only null values and add flags,
  # never change which rows are kept (except to preserve a row whose date/amount
  # was hidden). This is the exact case a position-based "promotion guard" would
  # get wrong: with the first row's date blacked out, the remaining real dates all
  # sit BELOW it, so a span heuristic would demote and silently lose a real
  # transaction. We keep it instead -- silent loss of a real row is the worst
  # forensic outcome, worse than a visible, flagged over-count elsewhere.
  R <- "[REDACTED]"
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c(R,"COFFEE","-40.00","955.50",       # y=40 FIRST row, date redacted (edge)
              "06","Jan","SHOP","-10.00","945.50", # y=70 real
              "07","Jan","RENT","-5.00","940.50"), # y=100 real
    x     = c(45,110,415,490,   45,60,110,415,490,   45,60,110,415,490),
    y     = c(40,40,40,40,       70,70,70,70,70,       100,100,100,100,100),
    width = c(55,45,34,30,       12,16,45,34,30,       12,16,45,34,30),
    height= rep(10, 14))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .simple_tmpl())$transactions
  expect_equal(nrow(tx), 3L)                              # the edge row is NOT lost
  expect_true(is.na(tx$date[1]))                          # its hidden date is NA
  expect_equal(tx$amount[1], -40.00)                      # its visible amount survives
  expect_true(grepl("redacted", tx$flags[1], ignore.case = TRUE))  # and it is flagged
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

test_that("a redacted header field is honest: text shows [REDACTED], money stays NA", {
  # A redaction overlay can cover a HEADER value (account number, opening balance),
  # not just the transaction table. The extracted header must never silently drop
  # the field or invent a number: a text field surfaces the [REDACTED] token
  # (present-but-hidden) and a money field is NA (unknown), never fabricated.
  R <- "[REDACTED]"
  mk <- function(acct, ob) data.frame(stringsAsFactors = FALSE,
    text  = c("Account", acct, "Opening", "Balance", ob, "05","Jan","COFFEE","-40.00","955.50"),
    x     = c(45,140,45,100,200,   45,60,110,415,490),
    y     = c(12,12,24,24,24,      60,60,60,60,60),
    width = c(50,90,50,45,60,      12,16,45,34,30),
    height= rep(10, 10))
  tmpl <- .simple_tmpl()
  tmpl$table$metadata_regions <- list(
    account_number  = list(page = 1, x_min = 120, x_max = 260, y_min = 8,  y_max = 20),
    opening_balance = list(page = 1, x_min = 150, x_max = 300, y_min = 20, y_max = 34))
  mkinput <- function(w) list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(w),
    page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))

  clean  <- parse_pdf_table(mkinput(mk("1234567", "500.00")), tmpl)$header
  redact <- parse_pdf_table(mkinput(mk(R, R)), tmpl)$header

  expect_equal(clean$account_number, "1234567")        # sanity: read correctly when visible
  expect_equal(clean$opening_balance, 500.00)
  expect_true(grepl("REDACT", redact$account_number))  # hidden text is surfaced, not dropped
  expect_true(is.na(redact$opening_balance))           # hidden money is NA, never invented
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

# ---- amount_sign: type_dc on a PDF template --------------------------------
# A PDF template declaring `amount_sign: type_dc` used to reach parse_amount with
# NO type column and NO tokens, so every row fell into the back-compat "not the
# debit token -> credit" branch and was signed +magnitude. Every debit silently
# became a credit while the "D" still printed beside a +500 credit, so the row
# looked internally consistent and nothing could catch it.
.tdc_words <- function(t1 = "D", t2 = "C") data.frame(stringsAsFactors = FALSE,
  text  = c("05","Jan",t1,"RENT","500.00",   "06","Jan",t2,"SALARY","1000.00"),
  x     = c(45,60,90,140,415,                45,60,90,140,415),
  y     = c(40,40,40,40,40,                  70,70,70,70,70),
  width = c(12,16,10,40,34,                  12,16,10,45,34),
  height= rep(10, 10))
.tdc_input <- function(...) list(kind = "pdf", path = tempfile(fileext = ".pdf"),
  pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"),
  words = list(.tdc_words(...)), meta = list(page_count = 1L))
.tdc_tmpl <- function() list(id = "tdc", bank = "S", statement_type = "e", format = "pdf",
  version = 1, currency = "NZD", type_debit_value = "D",
  table = list(row_tol = 3, date_format = "%d %b", amount_sign = "type_dc",
    columns = list(date = list(x_min = 40, x_max = 74), type = list(x_min = 85, x_max = 105),
      description = list(x_min = 105, x_max = 360), amount = list(x_min = 360, x_max = 470))))

test_that("a type_dc PDF template signs from its indicator column", {
  tx <- parse_pdf_table(.tdc_input(), .tdc_tmpl())$transactions
  expect_equal(nrow(tx), 2L)
  expect_equal(tx$amount, c(-500, 1000))                 # "D RENT 500.00" -> -500
  expect_identical(tx$direction, c("debit", "credit"))
  expect_identical(tx$type, c("D", "C"))                 # the indicator is kept verbatim
})

test_that("a type_dc PDF honours the declared tokens, and fails closed on an unknown one", {
  # Case-insensitively matched, exactly as the delimited path does.
  low <- parse_pdf_table(.tdc_input(t1 = "d"), .tdc_tmpl())$transactions
  expect_equal(low$amount[1], -500)
  # With BOTH tokens declared, an indicator matching neither is genuinely ambiguous
  # -> NA + malformed, never silently signed a credit.
  tmpl <- .tdc_tmpl(); tmpl$type_credit_value <- "C"
  amb <- parse_pdf_table(.tdc_input(t2 = "X"), tmpl)$transactions
  expect_equal(amb$amount[1], -500)
  expect_true(is.na(amb$amount[2]))
  expect_match(amb$flags[2], "malformed")
  # tokens declared inside the table block work too (same as decimal_mark)
  t2 <- .tdc_tmpl(); t2$type_debit_value <- NULL; t2$table$type_debit_value <- "D"
  expect_equal(parse_pdf_table(.tdc_input(), t2)$transactions$amount, c(-500, 1000))
})

# ---- continuation merge: no word on a folded line may be lost ---------------
.cont_tmpl <- function() { tm <- .simple_tmpl(); tm$table$columns$reference <-
  list(x_min = 250, x_max = 340); tm$table$columns$description$x_max <- 250; tm }

test_that("a continuation line loses NO word, even outside the descriptive bands", {
  # THE bug: the wrapped payee "SMITH HOLDINGS LTD" wraps under "PAYMENT TO JOHN",
  # and "SMITH" prints far enough left to land in the DATE band. The old merge
  # folded only the words a descriptive band claimed, so "SMITH" vanished and
  # "PAYMENT TO JOHN HOLDINGS LTD" -- a DIFFERENT legal entity -- was presented as
  # verbatim, with amounts untouched so reconciliation could never see it.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","PAYMENT","TO","JOHN","-40.00","955.50",   # y=40 transaction
              "SMITH","HOLDINGS","LTD"),                            # y=49 wrapped payee
    x     = c(45,60,110,160,190,415,490,    45,110,170),            # "SMITH" at x=45 -> date band
    y     = c(40,40,40,40,40,40,40,         49,49,49),
    width = c(12,16,45,25,30,34,30,         40,50,25),
    height= rep(9, 10))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .simple_tmpl())$transactions
  expect_equal(nrow(tx), 1L)
  expect_equal(tx$description, "PAYMENT TO JOHN SMITH HOLDINGS LTD")  # verbatim, in print order
  expect_match(tx$flags, "row_text_merged")     # the assembly is visible, not silent
  expect_equal(tx$amount, -40)                  # amounts untouched by the fold
})

test_that("a continuation still routes its own-column text, and keeps the strays", {
  # The second reported shape: the wrap spans a MAPPED reference band and a stray
  # word in the balance band. The reference keeps its own column (the existing
  # behaviour), and the stray still reaches the description rather than vanishing.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","VISA","SHOP","-40.00","955.50",   # y=40 transaction
              "CARD","REF-9","TAIL"),                       # y=49 wrap: desc | reference | balance band
    x     = c(45,60,110,150,415,490,   110,255,490),
    y     = c(40,40,40,40,40,40,       49,49,49),
    width = c(12,16,30,40,34,30,       30,40,25),
    height= rep(9, 9))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .cont_tmpl())$transactions
  expect_equal(nrow(tx), 1L)
  # The transaction line itself printed NO reference, so the wrapped one lands in an
  # EMPTY cell. An empty band reads as NA, and pasting onto NA rendered the literal
  # string "NA" into the cell ("NA REF-9") -- a fabricated value in a field promised
  # verbatim. It must be exactly what was printed.
  expect_identical(tx$reference, "REF-9")
  expect_false(grepl("NA", tx$reference, fixed = TRUE))
  expect_equal(tx$description, "VISA SHOP CARD TAIL")       # stray "TAIL" preserved
  expect_match(tx$flags, "row_text_merged")
})

test_that("an ordinary description wrap is unchanged and carries no merge flag", {
  # Regression guard: when every wrapped word is inside the descriptive bands
  # nothing was ever dropped, so the output AND the flags must be exactly as before.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","VISA","SHOP","-40.00","955.50",
              "CARD","1234","Orig","06/01"),
    x     = c(45,60,110,150,415,490,   110,150,190,230),
    y     = c(40,40,40,40,40,40,       49,49,49,49),
    width = c(12,16,30,40,34,30,       30,30,30,30),
    height= rep(9, 10))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  tx <- parse_pdf_table(input, .simple_tmpl())$transactions
  expect_equal(tx$description, "VISA SHOP CARD 1234 Orig 06/01")
  expect_false(grepl("row_text_merged", tx$flags))
})

# ---- keep_dateless_rows (shared-date statements) ---------------------------
.kd_input <- function() {
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",   # dated transaction
              "SHOP","10.00","85.50",                # shares the date above
              "BOOKS","5.00","80.50"),               # shares the date above
    x     = c(45,60,110,415,490,   110,415,490,   110,415,490),
    y     = c(40,40,40,40,40,      70,70,70,      100,100,100),
    width = c(12,16,45,25,30,      45,25,30,      45,25,30),
    height= rep(9, 11))
  list(kind = "pdf", path = tempfile(fileext = ".pdf"),
       pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
       page_width = 595.28, page_height = 841.89, meta = list(page_count = 1L))
}
.kd_tmpl <- function() { tm <- .simple_tmpl()
  tm$table$keep_dateless_rows <- TRUE; tm$table$merge_continuation <- FALSE; tm }

test_that("keep_dateless_rows keeps a shared-date row with a BLANK date, flagged", {
  off <- parse_pdf_table(.kd_input(), local({ tm <- .kd_tmpl()
    tm$table$keep_dateless_rows <- FALSE; tm }))$transactions
  expect_equal(nrow(off), 1L)                       # default: dateless money lines dropped

  on <- parse_pdf_table(.kd_input(), .kd_tmpl())$transactions
  expect_equal(nrow(on), 3L)                        # the shared-date rows survive
  expect_identical(on$date, c("2026-01-05", NA, NA))  # never guessed from a neighbour
  expect_equal(on$amount, c(4.50, 10.00, 5.00))
  expect_true(all(grepl("no_date", on$flags[2:3])))   # and say so
})

# ---- completeness counts threaded out for the no_unparsed_rows KPI ---------
test_that("parse_pdf_table reports how many visual rows it examined and skipped", {
  # no_unparsed_rows had no PDF number to report, so it printed "pass" for every
  # PDF by construction -- a green tick for a check that never ran.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("Date","Details","Amount","Balance",                  # header -> skipped
              "05","Jan","COFFEE","4.50","95.50",                   # kept
              "31","Jan","Closing","Balance","1234.00","1234.00"),  # summary -> skipped
    x     = c(45,110,415,490,   45,60,110,415,490,   45,60,110,180,415,490),
    y     = c(10,10,10,10,      40,40,40,40,40,      70,70,70,70,70,70),
    width = c(20,40,30,30,      12,16,45,25,30,      12,16,50,50,34,30),
    height= rep(10, 15))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  p <- parse_pdf_table(input, .simple_tmpl())
  expect_equal(nrow(p$transactions), 1L)
  expect_equal(p$visual_row_count, 3L)              # header + txn + summary
  expect_equal(p$skipped_row_count, 2L)             # header and summary line
  expect_true(is.na(p$source_line_count))           # a PDF has no source-line count
})

# ---- summary-line vocabulary lives in the lexicon --------------------------
test_that("the summary-line vocabulary is lexicon-driven (defaults unchanged)", {
  # "Balance b/fwd" is not a built-in wording, so by DEFAULT it is still treated as
  # a transaction -- behaviour identical to the hardcoded list. Adding it to the
  # lexicon must stop it being read as a fabricated transaction, with no code change.
  words <- data.frame(stringsAsFactors = FALSE,
    text  = c("05","Jan","COFFEE","4.50","95.50",
              "01","Jan","Balance","b/fwd","1000.00","1000.00"),
    x     = c(45,60,110,415,490,   45,60,110,170,415,490),
    y     = c(40,40,40,40,40,      70,70,70,70,70,70),
    width = c(12,16,45,25,30,      12,16,50,40,34,30),
    height= rep(10, 11))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = c("Statement period 1 Jan 2026 to 31 Jan 2026"), words = list(words),
    meta = list(page_count = 1L))
  clear_lexicon_cache()
  expect_equal(nrow(parse_pdf_table(input, .simple_tmpl())$transactions), 2L)
  # built-in wordings keep working with no lexicon file present
  expect_true(.pdf_is_summary("Closing Balance 1,234.00"))
  expect_true(.pdf_is_summary("Balance carried forward 12.00"))
  expect_false(.pdf_is_summary("Total Payments to ACME Ltd 12.00"))  # a real transaction

  lx <- tempfile(fileext = ".yaml")
  writeLines("summary_line_labels: ['balance b/fwd', 'sub total']", lx)
  old <- Sys.getenv("BSO_LEXICON", NA_character_)
  Sys.setenv(BSO_LEXICON = lx); clear_lexicon_cache()
  on.exit({ if (is.na(old)) Sys.unsetenv("BSO_LEXICON") else Sys.setenv(BSO_LEXICON = old)
            clear_lexicon_cache() }, add = TRUE)
  expect_true(.pdf_is_summary("Balance b/fwd 1,000.00"))
  expect_true(.pdf_is_summary("Sub total 99.00"))
  expect_true(.pdf_is_summary("Closing Balance 1,234.00"))          # built-ins still union in
  expect_equal(nrow(parse_pdf_table(input, .simple_tmpl())$transactions), 1L)
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

# --------------------------------------------------------------------------
# The summary-line guard reads the WHOLE LINE, always (N30).
#
# .pdf_is_summary used to read `raw` only when the description band came back
# EMPTY. A displaced band comes back non-empty and WRONG, so the fallback never
# ran and "Opening balance" was never seen even though it sat in `raw` -- the row
# survived as a transaction carrying the balance figure. The end-to-end proof of
# the fix, and the invariant over every shipped golden, are in
# test-pdf_summary_invariant.R; these pin the matcher's own behaviour.
# --------------------------------------------------------------------------
test_that("a summary line is recognised from the raw line, whatever the band caught", {
  # description non-empty and WRONG (the displaced-band case) -- raw still decides
  expect_true(.pdf_is_summary("9,000.00", "01 Jan Opening balance 9,000.00 9,000.00"))
  expect_true(.pdf_is_summary("$0.00", "13 Jul Closing Balance $0.00 $2.00 1.97"))
  expect_true(.pdf_is_summary("17,731.96", "18 Feb Opening balance 17,731.96 OD"))
  expect_true(.pdf_is_summary("Jun", "13 Jun Balance carried forward 0.03 OD"))
  # the description alone said nothing -- which is why the old code kept the row
  expect_false(.pdf_is_summary("9,000.00"))
  # and a correctly-caught description still decides on its own
  expect_true(.pdf_is_summary("Opening Balance", "01 Jan Opening Balance 9,000.00"))
})

test_that("reading the raw line does NOT swallow a real transaction", {
  # The whole reason .pdf_summary_rx anchors ^...$: a real payee may mention a
  # balance, a total or a carry. If any of these ever flips to TRUE the guard has
  # started eating real rows, and money goes missing silently -- the one
  # unforgivable failure.
  expect_false(.pdf_is_summary(NULL, "05 Apr TFR TO SAVINGS - BALANCE 250.00"))
  expect_false(.pdf_is_summary(NULL, "15 Mar Total Payments to ACME Ltd 1,250.00"))
  expect_false(.pdf_is_summary(NULL, "03 Feb EFTPOS RIVERSIDE DAIRY 12.40 2,398.15"))
  expect_false(.pdf_is_summary(NULL, "24 Aug TFR To 63A Rent Rent 63A Sar 465.00 17.26"))
  expect_false(.pdf_is_summary(NULL, "12 Jun BALANCE PHARMACY LTD 41.20 900.00"))
  expect_false(.pdf_is_summary(NULL, "Overdraft Interest Rate at closing date is 13.90% per annum."))
  # ...including when the label is a genuine prefix of a longer, real description
  expect_false(.pdf_is_summary("Closing Balance Holdings Ltd"))
})

test_that("a line is reduced to its label by dropping dates, money and markers", {
  # The reduction is what makes reading the raw line safe: it removes the date and
  # money COLUMNS the raw line also carries, so a whole-label match still means the
  # whole label. Numbers glued into a word are not numbers and must survive.
  expect_equal(.pdf_line_label("13 Jul Closing Balance $0.00 $2.00 1.97"), "closing balance")
  expect_equal(.pdf_line_label("Closing balance:  1,234.00"), "closing balance")
  expect_equal(.pdf_line_label("Opening balance -  9,000.00"), "opening balance")
  expect_equal(.pdf_line_label("19 Feb DC 06-0705-0503820-28 CREDIT TRANSFER 133307 901.11"),
               "dc  credit transfer")
  expect_equal(.pdf_line_label("Interest 0502 0792845-93 147.43"), "interest")
  expect_equal(.pdf_line_label("13.90% per annum"), "13.90% per annum")   # not a number
  expect_equal(.pdf_line_label(NA_character_), "")
  expect_equal(.pdf_line_label(NULL), "")
})

# --------------------------------------------------------------------------
# THE BAND FRAME -- the contract the reader, the X-ray and the band editor share.
# Pinned here because it is now stated in ONE place (R/parse_pdf_table.R) and
# everything that draws or matches a band has to obey it. A band drawn or stored
# in raw page coordinates is displaced on any page that is not the frame's size,
# and a displaced band is indistinguishable from an untouched default one -- so
# the contract has to be a test, not a comment.
# --------------------------------------------------------------------------
test_that("the band frame's two directions are exact inverses", {
  frame <- pdf_band_frame(list(table = list(ref_width = 595.28, ref_height = 841.89)))
  s <- pdf_band_frame_scale(frame, 1190.56, 1683.78)     # this page is 2x the frame
  expect_equal(s, c(0.5, 0.5))
  expect_equal(200 * s[1], 100)     # a WORD at page x=200 is matched at frame x=100
  expect_equal(100 / s[1], 200)     # a BAND at frame x=100 is DRAWN at page x=200
})

test_that("a page within the snap counts as the frame; a missing ref means A4", {
  frame <- pdf_band_frame(list())                        # no ref recorded
  expect_equal(frame, list(width = .A4_W, height = .A4_H))
  expect_equal(pdf_band_frame_scale(frame, .A4_W * 1.01, .A4_H * 1.01), c(1, 1))
  expect_false(all(pdf_band_frame_scale(frame, .A4_W * 1.5, .A4_H) == 1))
  # An unknown page size assumes the page IS the frame, rather than scaling by NA
  # and moving every band off the statement.
  expect_equal(pdf_band_frame_scale(frame, NA_real_, NA_real_), c(1, 1))
})

test_that("a nonsense reference page size falls back to A4, never to a bad scale", {
  expect_equal(pdf_band_frame(list(table = list(ref_width = 0, ref_height = -3))),
               list(width = .A4_W, height = .A4_H))
  expect_equal(pdf_band_frame(list(table = list(ref_width = "not a number")))$width, .A4_W)
})

# --------------------------------------------------------------------------
# row_coverage's band measurements (R/row_coverage.R). They live here because the
# frame and the bands they measure are the parser's, and because they exist to
# answer the reported "a box is drawn over the credit column but it reads nothing
# while the debit beside it reads fine" without anyone having to see the statement.
# --------------------------------------------------------------------------
test_that("row_coverage counts the words each band read, and what no band read", {
  cov <- row_coverage(.mk_input(), .tmpl)
  expect_true(cov$applicable)
  expect_true(all(c("date", "description", "debit", "credit", "balance") %in%
                  names(cov$band_words_total)))
  expect_gt(cov$band_words_total[["debit"]], 0)
  expect_gt(cov$band_words_total[["credit"]], 0)
  expect_identical(cov$empty_bands, character(0))
  # counts only -- the report must stay safe to share
  expect_false(grepl("COFFEE|SALARY", format_row_coverage(cov)))
})

test_that("a band that reads nothing while words go unclaimed is named", {
  # The credit band is moved off the page's right edge: its words then belong to no
  # band at all. That pair -- an empty band AND unclaimed words -- is what makes
  # this a misplaced band rather than a column with no entries this month.
  tmpl <- .tmpl
  tmpl$table$columns$credit <- list(x_min = 560, x_max = 580)
  cov <- row_coverage(.mk_input(), tmpl)
  expect_identical(cov$empty_bands, "credit")
  expect_gt(sum(vapply(cov$pages, function(p) p$unbanded_words, integer(1))), 0)
  expect_match(cov$diagnosis, "credit band read no words")
})

test_that("an empty band on a statement with nothing unclaimed is NOT reported", {
  # A month with no deposits leaves the credit band empty and everything else in a
  # band. Nothing is wrong, so the diagnostic must not send the analyst to redraw a
  # band that is fine -- the same mistake this file's headings bug once made.
  w <- data.frame(stringsAsFactors = FALSE,
    text  = c("05", "Jan", "COFFEE", "4.50", "95.50"),
    x     = c(45, 60, 110, 355, 490), y = rep(40, 5),
    width = c(12, 16, 45, 25, 30), height = rep(10, 5))
  input <- list(kind = "pdf", path = tempfile(fileext = ".pdf"),
    pages = "Statement period 1 Jan 2026 to 31 Jan 2026", words = list(w),
    page_width = .A4_W, page_height = .A4_H, meta = list(page_count = 1L))
  cov <- row_coverage(input, .tmpl)
  expect_equal(cov$band_words_total[["credit"]], 0L)      # genuinely no deposits
  expect_equal(sum(vapply(cov$pages, function(p) p$unbanded_words, integer(1))), 0L)
  expect_identical(cov$empty_bands, character(0))         # ...so nothing is reported
  expect_match(cov$diagnosis, "Every candidate row was kept")
})
