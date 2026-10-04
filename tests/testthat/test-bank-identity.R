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
  # SBS's customers carry Westpac's code: the SBS legal name decides. The register
  # puts the number in Westpac's own branch, so the name alone is not enough to be
  # sure; the name and SBS's own website together are.
  wp <- acct("03", "0049")
  r2 <- bank_identify(mk_pdf(statement("SBS Bank", paste("12 EXAMPLE STREET   Account number", wp), tx_rows(),
                                       footer = "Southland Building Society, trading as SBS Bank")))
  expect_equal(r2$institution, "sbs")
  expect_equal(r2$confidence, "medium")
  r3 <- bank_identify(mk_pdf(statement("SBS Bank", paste("12 EXAMPLE STREET   Account number", wp), tx_rows(),
                                       footer = c("Southland Building Society, trading as SBS Bank",
                                                  "www.sbsbank.co.nz"))))
  expect_equal(r3$institution, "sbs")
  expect_equal(r3$confidence, "high")
  # A brand word alone never moves a number in the clearing bank's own branch to
  # an agency bank, and it stops the reading from being sure.
  r4 <- bank_identify(mk_pdf(statement("Westpac", paste("12 EXAMPLE STREET   Account number", wp), tx_rows(),
                                       footer = "Heartland Bank")))
  expect_equal(r4$institution, "westpac")
  expect_equal(r4$confidence, "medium")
})

test_that("OCR damage: a split body, a letter-spaced logo", {
  a <- acct("15", "3941")
  parts <- strsplit(a, "-")[[1]]
  split <- paste(parts[1], parts[2], paste(substr(parts[3], 1, 4), substring(parts[3], 5)), parts[4], sep = "-")
  r <- bank_identify(mk_pdf(statement("Statement", paste("12 EXAMPLE STREET   Account number", split), tx_rows())))
  expect_equal(r$institution, "tsb")
  expect_equal(r$confidence, "high")
  r2 <- bank_identify(mk_pdf(statement("B N Z       Visa statement", "12 EXAMPLE STREET", tx_rows())))
  expect_equal(r2$institution, "bnz")
  expect_equal(r2$confidence, "low")
})

test_that("a single-bank code with an unregistered branch is only a low guess", {
  r <- bank_identify(mk_pdf(statement("Statement", "12 EXAMPLE STREET   Account number 12-9999-4826153-00", tx_rows())))
  expect_equal(r$institution, "asb")
  expect_equal(r$confidence, "low")
  expect_equal(r$bank_code, "12")
  # 03 is shared by Westpac and the banks that clear through it: no guess.
  r2 <- bank_identify(mk_pdf(statement("Statement", "12 EXAMPLE STREET   Account number 03-9999-4826153-00", tx_rows())))
  expect_true(is.na(r2$institution))
  expect_equal(r2$confidence, "low")
})

test_that("placeholder numbers in guides and test files are nobody's account", {
  for (n in c("11-1111-1111111-00", "02-1300-1234567-00", "22-2222-2222222-00"))
    expect_equal(bank_identify(mk_pdf(statement("Statement", paste("12 EXAMPLE STREET   Account number", n),
                                                tx_rows())))$confidence, "unknown")
})

test_that("a payment-instruction box and a payee cell beside a label are not the holder's", {
  w <- acct("03", "0990", "082")
  lines <- statement("Kiwibank", "12 EXAMPLE STREET", tx_rows(),
                     footer = c("How to pay your card", "Internet banking", paste("Account number", w)))
  r <- bank_identify(mk_pdf(lines))
  expect_false("westpac" %in% r$evidence$institution)
  lines2 <- statement("Kiwibank", paste("Pay to:          Account number", w), tx_rows())
  r2 <- bank_identify(mk_pdf(lines2))
  expect_false("westpac" %in% r2$evidence$institution)
})

test_that("a combined statement with two of one bank's codes reports one of them", {
  p <- statement("ANZ", paste("12 EXAMPLE STREET   Account number", acct("01", "0902")), tx_rows())
  p <- c(p, "", paste("Account number", acct("06", "0501")), tx_rows())
  r <- bank_identify(mk_pdf(p))
  expect_equal(r$institution, "anz")
  expect_true(r$bank_code %in% c("01", "06"))
  inp <- mk_pdf(p); inp$kind <- "scan"
  expect_equal(bank_identify(inp)$institution, "anz")
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
  # The second statement shows only another bank's masthead: still a bundle.
  p3 <- statement("Kiwibank", "12 EXAMPLE STREET", tx_rows())
  r3 <- bank_identify(mk_pdf(p1, p3))
  expect_false(r3$pages_agree)
  expect_true(is.na(r3$institution))
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

# N216: "Co-operative Bank" typed without "The" (or the bank's legal name, current or
# former) was kept as a bank of its own and shown under that name, disagreeing with
# a statement the register says is the Co-operative Bank's.
test_that("bank_pick knows the Co-operative Bank by any of its names, and shows its own", {
  coop <- list(institution = "coop", confidence = "high", why = "The Co-operative Bank: x.",
               needs_decision = FALSE, pages_agree = TRUE)
  for (nm in c("coop", "The Co-operative Bank", "Co-operative Bank", "Cooperative Bank", "co-op bank",
               "The Co-operative Bank Limited", "PSIS Limited", "PSIS")) {
    p <- bank_pick(coop, nm)
    expect_identical(p$bank, "coop", info = nm)
    expect_false(p$ask, info = nm)
    expect_identical(p$why, "The Co-operative Bank agrees with the statement.", info = nm)
  }
  # A different bank picked is still named as the list names the Co-operative Bank.
  p <- bank_pick(coop, "ASB")
  expect_match(p$why, "looks like The Co-operative Bank", fixed = TRUE)
  expect_identical(bank_choices(tempfile("noly_"))[["The Co-operative Bank"]], "coop")
  # Names are matched whole: another bank's legal name is not a near miss.
  expect_identical(bank_pick(coop, "Reserve Bank of New Zealand")$bank, "Reserve Bank of New Zealand")
})

test_that("a payee's number wrapped under its row, or on a row the table finder missed, is not the holder's", {
  payee <- acct("12", "3456")
  rows <- tx_rows(c("03 Sep    DIRECT DEBIT                                    100.00                874.90",
                    paste("          ACCOUNT", payee),
                    "04 Sep    ONLINE BANKING                                   50.00                824.90",
                    paste("          ACCT", payee)))
  r <- bank_identify(mk_pdf(statement("Statement", "12 EXAMPLE STREET", rows)))
  expect_equal(r$confidence, "unknown")
  # Amounts without cents and year-first dates: no table is found, but each line
  # still starts with a date and carries a figure.
  rows2 <- paste0("2025/09/0", 1:6, "   Online banking acct ", payee, "    100   ", 1000 - 1:6 * 100)
  r2 <- bank_identify(mk_pdf(statement("Statement", "12 EXAMPLE STREET", rows2)))
  expect_equal(r2$confidence, "unknown")
})

test_that("the account a loan or card is repaid from, and a list of payees, are not the holder's", {
  theirs <- acct("12", "3456")
  for (lab in c("Direct debit account", "Linked account", "Nominated account", "Funding account",
                "Your repayment account", "Debit account", "Settlement account")) {
    r <- bank_identify(mk_pdf(statement("Westpac", paste("12 EXAMPLE STREET      ", lab, theirs), tx_rows(),
                                        footer = "Westpac New Zealand Limited")))
    expect_false("asb" %in% r$evidence$institution, info = lab)
    expect_equal(r$institution, "westpac", info = lab)
  }
  lines <- c("Kiwibank", "J SAMPLE", "", "Automatic payments", "Account                    Amount     Frequency",
             paste(theirs, "      100.00     Weekly"), "", "", "", "Date      Details    Withdrawals   Deposits     Balance",
             tx_rows())
  r <- bank_identify(mk_pdf(lines))
  expect_false("asb" %in% r$evidence$institution)
})

test_that("bullets, black boxes, dashes and accents are read in any locale", {
  kb <- acct("38", "9027")
  p <- strsplit(kb, "-")[[1]]
  dotted <- paste(p[1], p[2], strrep("\u2022", 7), p[4], sep = "\u2013")
  r <- bank_identify(mk_pdf(statement("Statement", paste("12 EXAMPLE STREET   Account number", dotted), tx_rows())))
  expect_equal(r$institution, "kiwibank")
  expect_equal(r$confidence, "medium")     # bank and branch visible, body masked
  boxed <- paste(strrep("\u2588", 2), strrep("\u2588", 4), strrep("\u2588", 7), "00", sep = "-")
  r2 <- bank_identify(mk_pdf(statement("Co\u00f6perative Caf\u00e9", paste("12 EXAMPLE STREET   Account number", boxed),
                                       tx_rows())))
  expect_equal(r2$confidence, "unknown")
  expect_false(grepl("could not be examined", r2$why))
  expect_equal(.bi_clean(c("a\u2013b \u2022\u2022", NA)), c("a-b **", NA))
})

test_that("a bank the list does not know, or a foreign statement, keeps the reading unsure", {
  asb <- acct("12", "3456")
  r <- bank_identify(mk_pdf(statement("Rimu Bank", paste("12 EXAMPLE STREET   Account number", asb), tx_rows())))
  expect_equal(r$institution, "asb")
  expect_equal(r$confidence, "medium")
  expect_true("other_bank" %in% r$evidence$kind)
  # Only a code (branch not in the register): no "possibly ASB" guess.
  r2 <- bank_identify(mk_pdf(statement("Rimu Bank", "12 EXAMPLE STREET   Account number 12-9999-4826153-00", tx_rows())))
  expect_true(is.na(r2$institution))
  r3 <- bank_identify(mk_pdf(statement("Rimu Bank", "12 EXAMPLE STREET", tx_rows(), footer = "Rimu Bank of Aotearoa Limited")))
  expect_equal(r3$confidence, "unknown")
  expect_match(r3$why, "not in the bank list")
  # Phrases that name no bank.
  for (h in c("your bank statement", "internet bank", "the reserve bank of new zealand", "not a registered bank"))
    expect_false(.bi_other_bank(h), info = h)
  # An Australian Westpac or a UK TSB statement carries the same names.
  au <- bank_identify(mk_pdf(statement("Westpac", "BSB 032-000 Account number 123456", tx_rows(),
                                       footer = "Westpac Banking Corporation")))
  expect_true(is.na(au$institution))
  uk <- bank_identify(mk_pdf(statement("TSB", "Sort code 77-12-34", tx_rows(), footer = "TSB Bank plc")))
  expect_true(is.na(uk$institution))
})

test_that("a misread code that cannot be repaired names no bank", {
  # Kiwibank's branch under Bank of China's code 88, with check digits that fail
  # for 38 too: the number contradicts itself.
  for (b in 1000001:1000100) if (isFALSE(nz_account_checksum("38", "9027", sprintf("%07d", b), "00"))) break
  r <- bank_identify(mk_pdf(statement("Statement", sprintf("12 EXAMPLE STREET   Account number 88-9027-%07d-00", b),
                                      tx_rows())))
  expect_true(is.na(r$institution))
  expect_match(r$why, "misread")
})

test_that("export account columns: many different numbers are payees, digit runs are read safely", {
  nums <- c(acct("12", "3456"), acct("01", "0902"), acct("38", "9027"), acct("15", "3941"))
  lines <- c("Date,Amount,Payee,Account", paste0("0", 1:4, "/01/26,-25.00,SHOP,", nums))
  expect_equal(bank_identify(list(kind = "delimited", lines = lines))$confidence, "unknown")
  anz <- gsub("-", "", acct("01", "0902", "000"))
  for (v in list(anz, sub("^0", "", anz), as.numeric(anz))) {
    tbl <- data.frame(Date = "2025-10-01", `Account Number` = rep(v, 2), Amount = "-5.00", check.names = FALSE)
    r <- bank_identify(list(kind = "excel", table = tbl, meta = list()))
    expect_equal(r$institution, "anz")
    expect_equal(r$bank_code, "01")
  }
})

test_that("nothing returned or warned carries the number", {
  a <- acct("12", "3456")
  p <- strsplit(a, "-")[[1]]
  inp <- mk_pdf(statement("ASB", paste("12 EXAMPLE STREET   Account number", a), tx_rows(),
                          footer = c("ASB Bank Limited", paste("Pay to", acct("01", "0902")))))
  said <- character(0)
  r <- withCallingHandlers(bank_identify(inp),
                           warning = function(w) { said <<- c(said, conditionMessage(w)); invokeRestart("muffleWarning") },
                           message = function(m) { said <<- c(said, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_length(said, 0)
  expect_equal(r$institution, "asb")
  # Every value, name and attribute, at any depth.
  flat <- paste(deparse(r, control = c("keepNA", "keepInteger", "showAttributes")), collapse = " ")
  expect_false(grepl(p[3], flat, fixed = TRUE))
  expect_false(grepl(p[2], flat, fixed = TRUE))
  expect_false(grepl("0902", flat, fixed = TRUE))
  bad <- bank_identify(list(kind = "delimited", lines = c(paste0("\"Account,", a), "Date,Amount", "\"x,1")))
  expect_false(grepl(p[3], paste(deparse(bad), collapse = " "), fixed = TRUE))
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
  # Branches that may be the Co-operative Bank's say only "BNZ's family".
  expect_equal(br$institution[br$bank_code == "02" & br$branch %in% c("1243", "1255")], c("bnz_agency", "bnz_agency"))
})
