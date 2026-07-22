# P1-1: statement-level metadata (period / opening / closing balance) for a
# delimited or excel file is mined from the PREAMBLE above the table and wired
# into the header, so reconciliation KPIs that need it can fire. Critically, it
# reads ONLY the preamble -- never the transaction rows -- so a bare CSV can
# never have a stray date pair or money value misread as a period or balance.

.syn_tmpl <- function() list(
  id = "syn", bank = "Syn", statement_type = "everyday", format = "delimited",
  version = 1, min_score = 1, currency = "NZD", delimiter = ",",
  preamble = list(header_regex = "^Date,Amount"),
  fingerprint = list(header_contains_all = list("Date", "Amount", "Description", "Balance")),
  columns = list(date = list(source = "Date", format = "%d/%m/%Y"),
                 amount = list(source = "Amount"),
                 description = list(source = "Description"),
                 balance = list(source = "Balance")),
  amount_sign = "signed")

.write_csv <- function(lines) { tf <- tempfile(fileext = ".csv"); writeLines(lines, tf); tf }

.DATA_ROWS <- c("Date,Amount,Description,Balance",
                "02/06/2025,100.00,Deposit A,1100.00",
                "15/06/2025,200.00,Deposit B,1300.00",
                "20/06/2025,-50.00,Payment C,1250.00")

test_that("preamble metadata is wired into a delimited header and reconciles (P1-1)", {
  csv <- c("Statement period 01/06/2025 to 30/06/2025",
           "Opening balance: 1000.00", "Closing balance: 1250.00", .DATA_ROWS)
  p <- parse_statement(read_input(.write_csv(csv)), .syn_tmpl())
  expect_equal(p$header$opening_balance, 1000)
  expect_equal(p$header$closing_balance, 1250)
  expect_identical(p$header$period_start, "01/06/2025")
  expect_identical(p$header$period_end,   "30/06/2025")
  # the header metadata lets balance_reconciliation actually run (and pass:
  # 1000 + (100 + 200 - 50) == 1250), where before it reported "na".
  r <- reconcile(p, .syn_tmpl())
  st <- function(nm) r$kpis$status[r$kpis$name == nm]
  expect_identical(st("balance_reconciliation"), "pass")
  expect_identical(st("dates_within_period"),    "pass")
})

test_that("a bare transaction CSV yields NO fabricated metadata (P1-1 safety)", {
  # No preamble -> the extractor must not read the transaction rows: the header
  # stays all-NA exactly as before, and balance_reconciliation leans on the
  # running-balance column instead (never inventing a period from row dates).
  p <- parse_statement(read_input(.write_csv(.DATA_ROWS)), .syn_tmpl())
  expect_true(is.na(p$header$opening_balance))
  expect_true(is.na(p$header$closing_balance))
  expect_true(is.na(p$header$period_start))
  expect_true(is.na(p$header$period_end))
  # continuity still proves completeness from the balance column alone
  r <- reconcile(p, .syn_tmpl())
  expect_identical(r$kpis$status[r$kpis$name == "running_balance_continuity"], "pass")
})
