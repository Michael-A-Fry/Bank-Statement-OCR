# test-recipes-sheet.R -- spreadsheet recipes (kind: excel / csv, R/recipes_sheet.R):
# a bank's Excel or CSV export read with its recipe and proven by the same
# arithmetic as every other reading. The workbooks are synthetic, modelled on the
# owner's real export designs (docs/context/RECIPE_STATEMENTS blocks 69-80): the
# same headings, preamble rows, sheets and money conventions, with made-up figures
# and no real names or numbers.

skip_if_not_installed("openxlsx")
skip_if_not_installed("readxl")

sh_shipped <- function() recipes_load(fixture("recipes"))
sh_recipe <- function(id) Filter(function(r) identical(r$id, id), sh_shipped())[[1]]
sh_dir <- function() { d <- tempfile("sheetrc_"); dir.create(d); d }

# sh_book(sheets, path) -- a workbook from a list of sheets, each list(name, rows =
# a list of character rows, hidden, fmt = list(list(rows, cols, numFmt))). Cells
# that read as numbers are written as numbers (as a bank's export stores them),
# "=d:YYYY-MM-DD" as an Excel date.
sh_book <- function(sheets, path = tempfile(fileext = ".xlsx")) {
  wb <- openxlsx::createWorkbook()
  for (k in seq_along(sheets)) {
    s <- sheets[[k]]
    openxlsx::addWorksheet(wb, s$name)
    for (i in seq_along(s$rows)) for (j in seq_along(s$rows[[i]])) {
      v <- s$rows[[i]][j]
      if (is.na(v) || !nzchar(v)) next
      x <- if (startsWith(v, "=d:")) as.Date(sub("^=d:", "", v))
           else if (grepl("^-?[0-9]+([.][0-9]+)?$", v)) as.numeric(v) else v
      openxlsx::writeData(wb, k, x, startRow = i, startCol = j, colNames = FALSE)
    }
    for (f in s$fmt %||% list())
      openxlsx::addStyle(wb, k, openxlsx::createStyle(numFmt = f$numFmt), rows = f$rows, cols = f$cols, gridExpand = TRUE)
    if (isTRUE(s$hidden)) openxlsx::sheetVisibility(wb)[k] <- "hidden"
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

# Block 73: BNZ "Statement Copy" -- account number and the opening balance above the
# table, "Particulars" spread over five cells (one heading), the date as an Excel
# date shown month/day/year, an overdrawn balance stored negative and shown with OD
# by the cell's number format, then an "Unstatemented transactions only" section.
sh_bnz <- function(bal4 = "-11.11", extra = list()) sh_book(list(list(name = "Statement Copy", rows = c(list(
  c("Account Number", "00-0000-0000000-00"),
  c("Opening Balance as at", "17/05/2026", "7.53"),
  c(""),
  c("Particulars", "", "Payment Type", "", "", "Withdrawals", "Deposits", "Date", "Balance"),
  c("MERCHANT A", "CODE 3003", "PS", "", "", "6.8", "", "=d:2026-05-18", "0.73"),
  c("BUSINESS A", "", "SW", "", "", "", "158.16", "=d:2026-05-20", "158.89"),
  c("", "", "", "D"),
  c("ATM WITHDRAWAL", "", "AT", "", "", "150", "", "=d:2026-05-21", "8.89"),
  c("MERCHANT B", "", "PS", "", "", "20", "", "=d:2026-05-22", bal4),
  c(""),
  c("Unstatemented transactions only"),
  c("Name of Other Party", "", "", "", "", "Withdrawals", "Deposits", "Date", "Balance"),
  c("BUSINESS C", "", "", "", "", "", "550", "11 Nov 26", "538.89")), extra),
  fmt = list(list(rows = 5:9, cols = 8, numFmt = "mm/dd/yyyy"), list(rows = 5:9, cols = 9, numFmt = "#,##0.00;#,##0.00 \"OD\"")))))

test_that("the shipped spreadsheet recipes load, each one kind excel with its columns under headings", {
  rcs <- sh_shipped()
  expect_identical(attr(rcs, "problems"), character(0))
  sheet <- Filter(function(r) r$kind %in% c("excel", "csv"), rcs)
  expect_true(all(c("bnz_undisclosed_transactional_acco", "bnz_bank_transaction_dataset", "kiwibank_search_by_access_number",
                    "westpac_transaction_export", "asb_customer_investigations_report") %in% vapply(sheet, `[[`, "", "id")))
  for (rc in sheet) {
    expect_true(all(c("date", "description") %in% rc$cols$field), info = rc$ref)
    expect_true(grepl("%[Yy]", rc$date_format), info = rc$ref)
  }
  expect_identical(sh_recipe("kiwibank_search_by_access_number")$split_by, "KiwiAcc")
  expect_identical(sh_recipe("bnz_bank_transaction_dataset")$style, "type_words")
})

test_that("a spreadsheet recipe that cannot be read the same way twice is refused, in plain words", {
  base <- list(recipe = "x_sheet", format = 1, version = 1, bank = "bnz", kind = "excel", status = "proven",
               recognise = list(all = list("Payment Type")),
               table = list(header = list("Date", "Particulars", "Amount", "Type"),
                            columns = list(date = list(under = "Date"), description = list(under = "Particulars"),
                                           amount = list(under = "Amount"), type = list(under = "Type"))),
               dates = list(format = "%d/%m/%Y"), money = list(style = "signed"))
  expect_null(.rc_validate(base)$error)
  b <- base; b$dates$format <- "%d %b"
  expect_match(.rc_validate(b)$error, "print the year")
  b <- base; b$money <- list(style = "type_words")
  expect_match(.rc_validate(b)$error, "out_words")
  b <- base; b$money <- list(style = "type_words", out_words = list("Debit"), in_words = list("debit"))
  expect_match(.rc_validate(b)$error, "both money in and money out")
  b <- base; b$table$columns$balance <- list(under = "Running Balance")
  expect_match(.rc_validate(b)$error, "not in `table: header:`")
  b <- base; b$table$split_by <- "KiwiAcc"
  expect_match(.rc_validate(b)$error, "split_by")
  b <- base; b$kind <- "word"
  expect_match(.rc_validate(b)$error, "pdf, excel or csv")
})

test_that("a BNZ statement copy reads with its recipe: opening above the table, OD in the number format, US dates", {
  inp <- read_input(sh_bnz())
  rc <- sh_recipe("bnz_undisclosed_transactional_acco")
  expect_identical(recipe_recognise(inp, list(rc))$recipe$ref, rc$ref)
  r <- recipe_read(inp, rc)
  expect_identical(r$outcome, "proven")
  tx <- r$transactions
  expect_identical(tx$date, c("2026-05-18", "2026-05-20", "2026-05-21", "2026-05-22"))
  expect_equal(tx$amount, c(-6.80, 158.16, -150.00, -20.00))
  expect_equal(r$parsed$header$opening_balance, 7.53)
  # the unheaded cells beside Particulars belong to the description; the
  # detached "D" mark is not a row; the unstatemented section is not read
  expect_match(tx$description[1], "MERCHANT A CODE 3003")
  expect_false(any(grepl("BUSINESS C", tx$description)))
  # auto_read uses it first, and says which recipe read the file
  a <- auto_read(inp, opts = list(recipes = list(rc)))
  expect_identical(a$outcome, "proven"); expect_identical(a$matched_recipe, rc$ref)
})

test_that("a spreadsheet whose figures do not add up is never proven by its recipe", {
  inp <- read_input(sh_bnz(bal4 = "-12.11"))
  rc <- sh_recipe("bnz_undisclosed_transactional_acco")
  expect_false(identical(recipe_read(inp, rc)$outcome, "proven"))
  a <- auto_read(inp, opts = list(recipes = list(rc)))
  expect_false(identical(a$outcome, "proven"))
  expect_null(a$matched_recipe)
  expect_true(any(grepl("does not prove", a$notes)))
})

test_that("a row the recipe ends the table with is skipped while the table has not begun", {
  # a calculation-support row printed between the heading and the first row
  f <- sh_book(list(list(name = "Sheet1", rows = list(
    c("Opening Balance as at", "", "100.00"),
    c("Particulars", "Payment Type", "Withdrawals", "Deposits", "Date", "Balance"),
    c("DO NOT DELETE OR CHANGE THIS ROW!!!!!", "", "", "", "", ""),
    c("SHOP A", "PS", "10", "", "04/18/2026", "90"),
    c("PAY", "DC", "", "50", "04/19/2026", "140"),
    c("DO NOT DELETE OR CHANGE THIS ROW!!!!!"),
    c("SHOP B", "PS", "5", "", "04/20/2026", "135")))))
  r <- recipe_read(read_input(f), sh_recipe("bnz_undisclosed_transactional_acco"))
  expect_identical(r$outcome, "proven")
  expect_equal(r$transactions$amount, c(-10, 50))
})

test_that("a Westpac export: headings with leading spaces, %m-%d-%y text dates, $ and bracket figures, \".\" for no balance", {
  hd <- c("Source-Type", " This-Party-Reference", " This-Party-Desc", " This-Party-Code", " Other-Party_Name",
          " Other-Party-Account-Number", " Date", " Amount", " Running-Balance", " Tran-Code")
  f <- sh_book(list(list(name = "Sheet1", rows = list(
    c("Account Name", "PERSON A"), c("Account Number", "00-0000-0000000-000"), hd,
    c("CODE A", "PART 1", "SALARY", "", "BUSINESS A", "00-0000-0000000-000", "05-17-26", "$500.00", "$500.00", "CODE B"),
    c("CODE A", "PART 2", "SALARY", "", "BUSINESS A", "00-0000-0000000-000", "05-24-26", "$500.00", "$1,000.00", "CODE B"),
    c("CODE C", "REF 1", "RENT", "", "PERSON B", "00-0000-0000000-000", "06-21-26", "($290.00)", "$710.00", "CODE D"),
    c("CODE C", "", "CARD", "", "MERCHANT A", "", "06-28-26", "($23.90)", "$686.10", "CODE D"),
    c("CODE C", "", "FEE", "", "", "", "06-29-26", "($100.00)", "$586.10", "CODE D")))), path = tempfile(fileext = ".xlsx"))
  rc <- sh_recipe("westpac_transaction_export")
  inp <- read_input(f)
  expect_identical(recipe_recognise(inp, list(rc))$recipe$ref, rc$ref)
  r <- recipe_read(inp, rc)
  expect_identical(r$outcome, "proven")
  expect_identical(r$transactions$date, c("2026-05-17", "2026-05-24", "2026-06-21", "2026-06-28", "2026-06-29"))
  expect_equal(r$transactions$amount, c(500, 500, -290, -23.90, -100))
  # A "." where the last balance should be is no balance: that row is not proven
  # by a balance step, so the reading waits for a person; it is never made up.
  wb <- openxlsx::loadWorkbook(f); openxlsx::writeData(wb, 1, ".", startRow = 8, startCol = 9); openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  clear_input_cache()
  r2 <- recipe_read(read_input(f), rc)
  expect_identical(r2$outcome, "check")
  expect_equal(r2$transactions$amount, c(500, 500, -290, -23.90, -100))
})

test_that("a Kiwibank export: a hidden sheet is never read, and one table of two accounts is two statements", {
  hd <- c("Customer", "RelType", "KiwiAcc", "ProcessDate", "ReceiptTime", "TranCode", "Amount", "Narration1", "PayingBankDRN",
          "OtherPartyAcc", "PayeeDetails", "Running Balance", "CardNo", "EODBalance", "ThisPartyParticulars", "ThisPartyCode",
          "ThisPartyReference")
  row <- function(acc, d, amt, desc, bal) c("000000", "PJT", acc, d, "10:00:00", "62", amt, desc, "", "", "", bal, "", "", "", "", "")
  f <- sh_book(list(
    list(name = "Search by Access Number", rows = list(
      c("Access number", "000000"), c("Start Date", "17/04/2026", "End Date", "20/05/2026"), c(""), c(""), hd,
      row("38-0000-0000001-00", "2026-05-20", "-25.43", "MERCHANT A", "3438.87"),
      row("38-0000-0000001-00", "2026-05-19", "-301.69", "MERCHANT B", "3464.30"),
      row("38-0000-0000001-00", "2026-05-18", "3000.00", "BILL PAYMENT", "3765.99"),
      row("38-0000-0000001-00", "2026-05-17", "-17.80", "MERCHANT C", "765.99"),
      row("38-0000-0000002-00", "2026-04-30", "2.43", "INTEREST EARNED", "2003.80"),
      row("38-0000-0000002-00", "2026-04-30", "-0.26", "PIE TAX", "2001.37"),
      row("38-0000-0000002-00", "2026-04-17", "2000.00", "BUSINESS B", "2001.63"))),
    list(name = "CancelBeforeSave", hidden = TRUE, rows = list(c("Date", "Amount"), c("2026-01-01", "5.00"), c("2026-01-02", "6.00")))))
  inp <- read_input(f)
  rc <- sh_recipe("kiwibank_search_by_access_number")
  book <- .rc_book(inp)
  expect_true(book[[2]]$hidden)
  r <- recipe_read(inp, rc)
  expect_identical(r$outcome, "proven")
  expect_equal(sort(r$transactions$amount), sort(c(-25.43, -301.69, 3000, -17.80)))
  expect_length(r$other_accounts, 1L)
  expect_identical(r$other_accounts[[1]]$account, "38-0000-0000002-00")
  expect_equal(sort(r$other_accounts[[1]]$tx$amount), sort(c(2.43, -0.26, 2000)))
  expect_identical(r$parsed$header$account_number, "38-0000-0000001-00")
})

test_that("an ASB workbook of one sheet per account reads each sheet as its own statement; sheet names never recognise", {
  hd <- c("Date", "Time", "Amount", "Balance", "", "Description", "Reference Text", "Other Party", "", "Card Number",
          "Service Type Description", "Source Type Description", "Location Description", "Device", "", "Latitude", "Longitude",
          "Location Address", "Google Maps Link")
  acct <- function(rows) c(list(
    c("Customer Investigations Report"), c("00-0000-0000000-00 Account (Retail Funding)"),
    c("Please note the balance field may not appear accurate"), c(""), hd), rows)
  tr <- function(d, amt, bal, desc, svc) c(d, "11:51", amt, bal, "", desc, "", "", "", "-", svc, "MTS Batch", "", "", "", "-36.8", "174.7", "", "")
  sheets <- list(
    list(name = "PERSON A", rows = list(c("Customer Investigations Report"), c("Customer:", "PERSON A"),
                                        c("For Period:", "17 May 2016 to 20 May 2023"), c("Transaction listing for customer:", "PERSON A"))),
    list(name = "00-0000-0000000-00", rows = acct(list(
      tr("2026-05-17", "0.00", "0.00", "OPENED", "Unknown"),
      tr("2026-06-01", "1191.58", "1191.58", "BUSINESS A", "Direct Credit"),
      tr("2026-06-08", "1191.58", "2383.16", "BUSINESS A", "Direct Credit"),
      tr("2026-06-15", "-153.79", "2229.37", "MERCHANT A", "Other Automated Withdrawal")))),
    list(name = "00-0000-0000000-50", rows = acct(list(
      tr("2026-05-30", "0.00", "0.00", "OPENED", "Unknown"),
      tr("2026-06-01", "500.00", "500.00", "TRANSFER", "Funds Transfer In"),
      tr("2026-06-02", "-4.88", "495.12", "FEE", "Fee")))),
    list(name = "FOOTNOTES", rows = list(c("Abbreviation", "Meaning"), c("MTS", "Batch"))))
  rc <- sh_recipe("asb_customer_investigations_report")
  r <- recipe_read(read_input(sh_book(sheets)), rc)
  expect_identical(r$outcome, "proven")
  expect_equal(r$transactions$amount, c(0, 1191.58, 1191.58, -153.79))   # a zero-value event is a row
  expect_length(r$other_accounts, 1L)
  expect_equal(r$other_accounts[[1]]$tx$amount, c(0, 500, -4.88))
  # the same workbook with its sheets named otherwise reads the same
  for (k in seq_along(sheets)) sheets[[k]]$name <- sprintf("Sheet%d", k)
  r2 <- recipe_read(read_input(sh_book(sheets)), rc)
  expect_identical(r2$outcome, "proven")
  expect_equal(r2$transactions$amount, r$transactions$amount)
})

test_that("a dataset export: a type column of words says money in or out, $ text amounts, an opening balance row", {
  hd <- c("File Name", "Row ID", "Bank", "Account Name", "Account Number", "Transaction Description", "Transaction Category",
          "Transaction Type", "Date", "Details", "Transaction Code", "Amount", "Balance", "Doc Reference Bank Statement",
          "Doc Reference Bank Voucher", "Year", "Tax Year", "Balance Check", "Balance Pass")
  tr <- function(id, desc, ty, d, det, amt, chk) c("FILE A", id, "BNZ", "PERSON A", "xxxx-xxxx-xxxx-0000", desc, desc, ty, d, det,
                                                    "400", amt, "Unidentified", "N/A", "N/A", "2026", "2026", chk, "Pass")
  rows <- list(hd,
    tr("1-1", "Opening Balance", "Deposit", "17/05/2026", "Opening Balance", "$87,654.32", "$87,654.32"),
    tr("1-2", "To be done", "Withdrawal", "18/05/2026", "MERCHANT A", "$35.49", "$87,618.83"),
    tr("1-3", "To be done", "Deposit", "19/05/2026", "BUSINESS A", "$332.27", "$87,951.10"),
    tr("1-4", "Financial expenses", "Withdrawal", "20/05/2026", "MERCHANT B", "$47.96", "$87,903.14"))
  rc <- sh_recipe("bnz_bank_transaction_dataset")
  r <- recipe_read(read_input(sh_book(list(list(name = "Sheet1", rows = rows)))), rc)
  expect_identical(r$outcome, "proven")
  expect_equal(r$transactions$amount, c(-35.49, 332.27, -47.96))
  expect_equal(r$parsed$header$opening_balance, 87654.32)
  # a type the recipe does not know is never given a direction by guess
  rows[[3]][8] <- "Transfer"
  r3 <- recipe_read(read_input(sh_book(list(list(name = "Sheet1", rows = rows)))), rc)
  expect_false(identical(r3$outcome, "proven"))
  expect_match(r3$why, "money in or out")
})

test_that("a CSV recipe reads a CSV export, and a PDF recipe never reads a spreadsheet", {
  d <- sh_dir(); on.exit(unlink(d, recursive = TRUE))
  writeLines(c("Kauri Bank export", "Date,Details,Amount,Balance",
               "04/17/2026,OPENING DEPOSIT,500.00,500.00", "04/18/2026,SHOP A,-45.20,454.80",
               "04/19/2026,PAY,\"1,250.00\",\"1,704.80\""), file.path(d, "kauri.csv"))
  writeLines(c("recipe: kauri_csv", "format: 1", "version: 1", "bank: kauri", "kind: csv", "status: proven",
               "recognise: {all: [\"Kauri Bank export\"]}",
               "table:", "  header: [\"Date\", \"Details\", \"Amount\", \"Balance\"]",
               "  columns: {date: {under: \"Date\"}, description: {under: \"Details\"}, amount: {under: \"Amount\"}, balance: {under: \"Balance\"}}",
               "dates: {format: \"%m/%d/%Y\", year: printed}", "money: {style: signed}"), file.path(d, "kauri_csv.yaml"))
  rcs <- recipes_load(d)
  expect_identical(attr(rcs, "problems"), character(0))
  inp <- read_input(file.path(d, "kauri.csv"))
  r <- recipe_first(inp, opts = list(recipes = rcs))
  expect_identical(r$outcome, "proven"); expect_identical(r$matched_recipe, "kauri_csv@1")
  expect_identical(r$transactions$date, c("2026-04-17", "2026-04-18", "2026-04-19"))
  # the shipped PDF recipes are never tried on it, and a spreadsheet recipe never on a PDF
  rg <- recipe_recognise(inp, sh_shipped())
  expect_null(rg$recipe)
  expect_false(any(vapply(Filter(function(x) x$kind == "pdf", sh_shipped()), `[[`, "", "ref") %in% rg$scores$recipe))
  pdf <- list(kind = "pdf", pages = "Kauri Bank export Date Details Amount Balance", words = list(), meta = list())
  expect_null(recipe_recognise(pdf, rcs)$recipe)
})

test_that("ask once: a new spreadsheet design waits for a person, and their check drafts a spreadsheet recipe", {
  withr::local_options(bso.unknown_design = "ask")
  f <- sh_book(list(list(name = "Export", rows = list(
    c("Account", "PERSON A 00-0000-0000000-00"),
    c("When", "What", "Paid out", "Paid in", "Left"),
    c("=d:2026-03-02", "SHOP A", "12.5", "", "487.5"),
    c("=d:2026-03-03", "PAY", "", "1000", "1487.5"),
    c("=d:2026-03-05", "RENT", "400", "", "1087.5"),
    c("=d:2026-03-09", "SHOP B", "7.25", "", "1080.25")))))
  cv <- convert_sandbox(); d <- sandbox_dir(cv)
  r1 <- cv(f, bank = "Kiwibank")
  expect_identical(r1$status, "needs_review")
  expect_match(r1$reason, "not seen this statement design before")
  r2 <- cv(f, bank = "Kiwibank", confirm = TRUE)
  expect_identical(r2$status, "ok")
  expect_identical(r2$run_log$learn_action, "created")
  drafts <- list.files(file.path(d, "recipes"), "[.]yaml$", full.names = TRUE)
  expect_length(drafts, 1L)
  txt <- paste(readLines(drafts), collapse = "\n")
  expect_match(txt, "kind: excel"); expect_match(txt, "status: draft")
  expect_false(grepl("PERSON|0000000|Export", txt))       # headings only: no name, account or sheet name
  rc <- recipes_load(file.path(d, "recipes"))[[1]]
  expect_identical(rc$kind, "excel")
  rr <- recipe_read(read_input(f), rc)
  expect_identical(rr$outcome, "proven")
  expect_equal(rr$transactions$amount, c(-12.5, 1000, -400, -7.25))
  # the next statement of the design comes back filled in by the draft, and waits
  r3 <- cv(f, bank = "Kiwibank")
  expect_identical(r3$status, "needs_review")
  expect_match(r3$reason, "kiwibank_draft_1@1")
})
