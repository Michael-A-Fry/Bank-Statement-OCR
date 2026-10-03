# test-bank-identity.R -- bank_identify() and bank_pick() (spec section 5, A2).
#
# Statements are built here from lines of text, so each test says exactly what is
# on the page. Account numbers are made from register branches with a body found
# by the check-digit rule, never copied from anyone's real account.

# The first body at or after `from` whose check digits pass for this bank/branch.
valid_body <- function(code, branch, from = 1000001) {
  for (b in from:(from + 200)) {
    body <- sprintf("%07d", b)
    if (isTRUE(nz_account_checksum(code, branch, body, "00"))) return(body)
  }
  stop("no valid body found")
}
acct <- function(code, branch, suffix = "00") paste(code, branch, valid_body(code, branch), suffix, sep = "-")

# A text-layer PDF input from pages of lines: one word box per word, 5pt a
# character, 12pt a line, so runs of spaces become the gaps between cells.
mk_pdf <- function(...) {
  pages <- list(...)
  words <- lapply(pages, function(lines) {
    rows <- lapply(seq_along(lines), function(i) {
      m <- gregexpr("\\S+", lines[i])[[1]]
      if (m[1] < 0) return(NULL)
      tx <- regmatches(lines[i], list(m))[[1]]
      data.frame(width = nchar(tx) * 5, height = 8, x = (as.numeric(m) - 1) * 5,
                 y = i * 12, space = TRUE, text = tx, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  })
  list(kind = "pdf", pages = vapply(pages, paste, "", collapse = "\n"), words = words,
       page_height = vapply(pages, function(l) (length(l) + 1) * 12, 0), meta = list())
}

statement <- function(masthead, holder_line, rows, footer = character(0)) c(
  masthead,
  "J SAMPLE                              Statement period 1 Sep 2025 to 30 Sep 2025",
  holder_line,
  "",
  "Date      Details                                   Withdrawals   Deposits     Balance",
  rows,
  "", "", "",
  footer)

tx_rows <- function(extra = character(0)) c(
  "01 Sep    OPENING BALANCE                                                     1,000.00",
  "02 Sep    EFTPOS HARBOUR FOODMARKET                        25.10                974.90",
  extra,
  "05 Sep    SALARY DEMO SERVICES                                       500.00  1,474.90",
  "07 Sep    ATM WITHDRAWAL                                   40.00              1,434.90")

test_that("check digits follow the IRD specification", {
  expect_true(nz_account_checksum("01", "0902", "0068389", "00"))    # IRD example 1, A
  expect_true(nz_account_checksum("08", "6523", "1954512", "001"))   # IRD example 2, D
  expect_true(nz_account_checksum("26", "2600", "0320871", "032"))   # IRD example 3, G
  expect_true(nz_account_checksum("03", "0990", "0998907", "082"))   # algorithm B
  expect_false(nz_account_checksum("01", "0123", "0123456", "00"))   # a bank's dummy example
  # The p %% 9 shortcut gets this one wrong; the specification's digit sum does not.
  expect_false(nz_account_checksum("26", "2600", "0629072", "088"))
  expect_true(is.na(nz_account_checksum("31", "2800", "1234567", "00")))  # algorithm X
  expect_false(nz_account_checksum("25", "2500", "0000000", "00"))        # all-zero body
  # A register branch IRD never listed: a failure is "cannot say", not "invalid".
  bad <- NULL
  for (b in 1000001:1000050) if (!isTRUE(nz_account_checksum("02", "2030", sprintf("%07d", b), "00"))) { bad <- b; break }
  expect_true(is.na(nz_account_checksum("02", "2030", sprintf("%07d", bad), "00")))
})

test_that("account-shaped runs are found in every printed form", {
  f <- .bi_find_accounts
  expect_equal(f("Account number 01-0902-0068389-00")$branch, "0902")
  expect_equal(f("03 0990 0998907 082")$suffix, "082")
  expect_equal(f("12\u20133456\u20130789012\u201350")$code, "12")
  expect_equal(f("OCR read 0l-09O2-0068389-00")$code, "01")       # l and O inside the run
  expect_equal(nrow(f("Card 4894 **** **** 7504  Call 0800 269 296  01-10-2025")), 0L)
  m <- f("Account 01-XXXX-XXXXXXX-00 and 12-3456-XXXXXXX-00")
  expect_equal(m$branch, c("XXXX", "3456"))
})

test_that("the holder's own account decides, and the result never carries the number", {
  a <- acct("01", "0902")
  r <- bank_identify(mk_pdf(statement("ANZ", paste("12 EXAMPLE STREET                      Account number", a), tx_rows())))
  expect_equal(r$institution, "anz")
  expect_equal(r$bank_code, "01")
  expect_equal(r$confidence, "high")
  expect_true(nzchar(r$why))
  expect_true(all(c("kind", "institution", "strength", "zone") %in% names(r$evidence)))
  expect_true("account" %in% r$evidence$kind)
  flat <- paste(capture.output(str(r)), deparse(r), collapse = " ")
  body <- strsplit(a, "-")[[1]][3]
  expect_false(grepl(body, flat, fixed = TRUE))
  expect_false(grepl("0902", flat, fixed = TRUE))
})

test_that("other people's numbers and bank names in the transactions count for nothing", {
  payee <- acct("12", "3456")
  rows <- tx_rows(c(
    paste("03 Sep    TFR TO", payee, "                     100.00                874.90"),
    "04 Sep    WESTPAC MASTERCARD PAYMENT                       50.00                824.90",
    "          ASB BANK LIMITED LOAN"))
  r <- bank_identify(mk_pdf(statement("Kiwibank", "12 EXAMPLE STREET", rows)))
  expect_equal(r$institution, "kiwibank")
  expect_false(any(r$evidence$institution %in% c("asb", "westpac")))
  # The same payee number with no bank name anywhere: nothing to go on.
  r2 <- bank_identify(mk_pdf(statement("Statement", "12 EXAMPLE STREET", rows)))
  expect_equal(r2$confidence, "unknown")
  expect_true(is.na(r2$institution))
})

test_that("numbers after To/From or in a payment box are not the holder's", {
  w <- acct("03", "0990", "082")
  lines <- statement("Kiwibank", "12 EXAMPLE STREET", tx_rows(),
                     footer = c(paste("Pay to account", w), paste("Transfer from account", w)))
  r <- bank_identify(mk_pdf(lines))
  expect_false("westpac" %in% r$evidence$institution)
})

test_that("masked numbers still give what is visible", {
  r <- bank_identify(mk_pdf(statement("Westpac", "12 EXAMPLE STREET        Account number 03-XXXX-XXXXXXX-00", tx_rows())))
  expect_equal(r$institution, "westpac")
  expect_equal(r$bank_code, "03")
  expect_true("account_code" %in% r$evidence$kind)
  r2 <- bank_identify(mk_pdf(statement("", "12 EXAMPLE STREET        Account number 12-3456-XXXXXXX-00", tx_rows())))
  expect_equal(r2$institution, "asb")
  expect_true("account_unverified" %in% r2$evidence$kind)
  # Only BNZ's code, nothing else: BNZ shares 02 with other banks, so no institution.
  r3 <- bank_identify(mk_pdf(statement("Statement", "12 EXAMPLE STREET   Account number 02-XXXX-XXXXXXX-00", tx_rows())))
  expect_true(is.na(r3$institution))
  expect_equal(r3$bank_code, "02")
  expect_equal(r3$confidence, "low")
})

test_that("an agency bank's own register branch and its own name win inside the family", {
  coop <- acct("02", "1242")
  r <- bank_identify(mk_pdf(statement("Statement", paste("12 EXAMPLE STREET   Account number", coop), tx_rows())))
  expect_equal(r$institution, "coop")
  expect_equal(r$bank_code, "02")
  # SBS's customers carry Westpac's code: the SBS legal name decides.
  wp <- acct("03", "0049")
  r2 <- bank_identify(mk_pdf(statement("SBS Bank", paste("12 EXAMPLE STREET   Account number", wp), tx_rows(),
                                       footer = "Southland Building Society, trading as SBS Bank")))
  expect_equal(r2$institution, "sbs")
  expect_equal(r2$confidence, "high")
})

test_that("a misread bank code is repaired from the branch", {
  # 06-1458 is an ANZ (ex National Bank) branch; 08 has no 1458, and 6/8 is an OCR slip.
  a <- acct("06", "1458")
  misread <- sub("^06", "08", a)
  r <- bank_identify(mk_pdf(statement("Statement", paste("12 EXAMPLE STREET   Account number", misread), tx_rows())))
  expect_equal(r$institution, "anz")
  expect_equal(r$bank_code, "06")
  expect_true("account_repaired" %in% r$evidence$kind)
})

test_that("a holder account and a legal name from different banks go to a person", {
  a <- acct("01", "0902")
  r <- bank_identify(mk_pdf(statement("ANZ", paste("12 EXAMPLE STREET   Account number", a), tx_rows(),
                                      footer = "ASB Bank Limited, Auckland")))
  expect_true(is.na(r$institution))
  expect_true(r$needs_decision)
  expect_equal(r$confidence, "low")
})

test_that("text alone identifies, at the strength the text deserves", {
  r <- bank_identify(mk_pdf(statement("Westpac", "12 EXAMPLE STREET", tx_rows(),
                                      footer = c("Westpac New Zealand Limited", "westpac.co.nz  0800 400 600"))))
  expect_equal(r$institution, "westpac")
  expect_equal(r$confidence, "high")
  r2 <- bank_identify(mk_pdf(statement("Westpac", "12 EXAMPLE STREET", tx_rows())))
  expect_equal(r2$institution, "westpac")
  expect_equal(r2$confidence, "low")
  expect_true(is.na(r2$bank_code))
})

test_that("a name after 'not' is not evidence, and look-alike names are not banks", {
  r <- bank_identify(mk_pdf(statement("Statement", "12 EXAMPLE STREET", tx_rows(),
                                      footer = "Synthetic test data. Not issued by ANZ Bank New Zealand Limited.")))
  expect_equal(r$confidence, "unknown")
  r2 <- bank_identify(mk_pdf(statement("Kauri Bank New Zealand", "12 EXAMPLE STREET", tx_rows(),
                                       footer = "Reserve Bank of New Zealand statistics")))
  expect_equal(r2$confidence, "unknown")
})

test_that("a bundle is identified page by page and disagreement is reported", {
  p1 <- statement("ANZ", paste("12 EXAMPLE STREET   Account number", acct("01", "0902")), tx_rows())
  p2 <- statement("ASB", paste("12 EXAMPLE STREET   Account number", acct("12", "3456")), tx_rows())
  r <- bank_identify(mk_pdf(p1, p2))
  expect_false(r$pages_agree)
  expect_equal(r$pages$institution, c("anz", "asb"))
  expect_true(is.na(r$institution))
  expect_true(r$needs_decision)
  same <- bank_identify(mk_pdf(p1, p1))
  expect_true(same$pages_agree)
  expect_equal(same$institution, "anz")
})

test_that("CSV exports: the This Party column and the preamble count, Other Party does not", {
  mine <- acct("38", "9027"); theirs <- acct("12", "3456")
  lines <- c("Date,Amount,Payee,This Party Account,Other Party Account",
             paste0("01/01/26,-25.00,SHOP,", mine, ",", theirs),
             paste0("02/01/26,11.50,FRIEND,", mine, ",", theirs))
  r <- bank_identify(list(kind = "delimited", lines = lines))
  expect_equal(r$institution, "kiwibank")
  expect_equal(r$evidence$zone[1], "column")
  expect_false("asb" %in% r$evidence$institution)
  # ASB's preamble prints bank, branch and account apart.
  b <- valid_body("12", "3456")
  asb <- c(paste0("Bank 12; Branch 3456; Account ", b, "-00"), "", "Date,Unique Id,Tran Type,Payee,Amount",
           "2025/12/01,1,DEBIT,SHOP,-5.00")
  r2 <- bank_identify(list(kind = "delimited", lines = asb))
  expect_equal(r2$institution, "asb")
  expect_equal(r2$evidence$zone[1], "preamble")
  # Transfers in the rows of an export say nothing.
  rows_only <- c("Type,Details,Amount,Date", paste0("TFR,TO ", theirs, ",-10.00,03/12/2025"))
  expect_equal(bank_identify(list(kind = "delimited", lines = rows_only))$confidence, "unknown")
})

test_that("Excel exports: preamble lines and account columns", {
  inp <- list(kind = "excel", meta = list(preamble = c(paste("BNZ - Transactions -", acct("02", "0018")), "Period 01/10/2025 to 31/10/2025")),
              table = data.frame(Date = "2025-10-01", Payee = "SHOP", Amount = "-5.00"))
  r <- bank_identify(inp)
  expect_equal(r$institution, "bnz")
  expect_equal(r$confidence, "high")
})

test_that("plain page text (no word boxes) and OCR-like text are read too", {
  lines <- statement("TSB", paste("12 EXAMPLE STREET   Account number", acct("15", "3941")), tx_rows())
  inp <- list(kind = "pdf", pages = paste(lines, collapse = "\n"), words = list(NULL), meta = list())
  r <- bank_identify(inp)
  expect_equal(r$institution, "tsb")
  expect_equal(r$confidence, "high")
})

test_that("never throws, and is deterministic", {
  for (x in list(NULL, list(), list(kind = "pdf"), list(kind = "pdf", words = list(data.frame(x = 1))),
                 list(kind = "delimited", lines = NA_character_), "no/such/file.pdf", 42)) {
    r <- bank_identify(x)
    expect_true(r$confidence %in% c("high", "medium", "low", "unknown"))
    expect_true(is.character(r$why) && nzchar(r$why))
  }
  inp <- mk_pdf(statement("ANZ", paste("12 EXAMPLE STREET   Account number", acct("01", "0902")), tx_rows()))
  expect_identical(bank_identify(inp), bank_identify(inp))
})

test_that("bank_pick: pre-fill, agree, ask, and block learning only when it must", {
  high <- list(institution = "anz", confidence = "high", why = "ANZ: x.", needs_decision = FALSE, pages_agree = TRUE)
  med <- modifyList(high, list(confidence = "medium"))
  low <- modifyList(high, list(confidence = "low"))
  none <- list(institution = NA_character_, confidence = "unknown", why = "Nothing.", needs_decision = FALSE, pages_agree = TRUE)
  p <- bank_pick(high, NULL)
  expect_equal(p$bank, "anz"); expect_false(p$ask); expect_false(p$block_learning)
  p <- bank_pick(high, "ANZ")                       # by display name
  expect_equal(p$bank, "anz"); expect_false(p$ask); expect_false(p$block_learning)
  p <- bank_pick(high, "asb")
  expect_equal(p$bank, "asb"); expect_true(p$ask); expect_true(p$block_learning)
  p <- bank_pick(high, "asb", confirmed = TRUE)
  expect_false(p$ask); expect_false(p$block_learning)
  p <- bank_pick(med, "asb")
  expect_true(p$ask); expect_false(p$block_learning)
  p <- bank_pick(low, "asb")
  expect_false(p$ask); expect_false(p$block_learning)
  p <- bank_pick(none, NULL)
  expect_true(is.na(p$bank)); expect_true(p$ask)
  p <- bank_pick(none, "Rimu Bank")                 # a bank not in the list is kept as picked
  expect_equal(p$bank, "Rimu Bank"); expect_false(p$ask)
  conflict <- modifyList(none, list(confidence = "low", needs_decision = TRUE, why = "Two banks."))
  p <- bank_pick(conflict, "anz")
  expect_true(p$ask); expect_true(p$block_learning)
})

test_that("the bundled register keeps its promises", {
  br <- utils::read.csv(file.path(engine_root(), "dictionaries", "nz_bank_branches.csv"),
                        colClasses = "character")
  expect_true(all(c("bank_code", "branch", "institution") %in% names(br)))
  expect_gt(nrow(br), 3000)
  expect_false(any(duplicated(br$branch)))          # a branch implies its bank code
  banks <- yaml::read_yaml(file.path(engine_root(), "dictionaries", "nz_banks.yaml"))$institutions
  expect_true(all(br$institution %in% names(banks)))
  expect_equal(br$institution[br$bank_code == "02" & br$branch == "1242"], "coop")
})
