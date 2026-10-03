# helper-statements.R -- small statements written for a test, so each test says
# exactly what is in the file it converts, and a sandbox to convert them in.
#
# Account numbers are made from register branches with a body found by the
# check-digit rule, never copied from anyone's real account.

nz_test_account <- function(code = "02", branch = "0018", suffix = "00") {
  for (b in 1000001:1000201) {
    body <- sprintf("%07d", b)
    if (isTRUE(nz_account_checksum(code, branch, body, suffix))) return(paste(code, branch, body, suffix, sep = "-"))
  }
  stop("no valid account body found")
}

# write_statement_csv(lines) -> the path of a CSV holding `lines`.
write_statement_csv <- function(lines) {
  p <- tempfile("stmt_", fileext = ".csv")
  writeLines(lines, p)
  p
}

# A BNZ export the arithmetic proves on its own: a preamble naming the holder's
# account (so the bank is identified with high confidence), money-out / money-in
# columns, a running balance on every row, and a 0.00 printed in money out.
proven_csv <- function(account = nz_test_account()) write_statement_csv(c(
  paste("BNZ - Transactions -", account), "Period 13/04/2025 to 30/04/2025",
  "Date,Description,Debit,Credit,Balance",
  "13/04/2025,Opening balance,,,1000.00",
  "14/04/2025,Salary,,2500.00,3500.00",
  "15/04/2025,Rent,1200.00,,2300.00",
  "16/04/2025,Account fee,0.00,,2300.00",
  "17/04/2025,Coffee,4.50,,2295.50",
  "18/04/2025,Groceries,85.20,,2210.30"))

# No balance and no totals: nothing on it can prove which column is which.
unproven_csv <- function() write_statement_csv(c(
  "Date,Details,Amount",
  "14/04/2025,Salary,2500.00",
  "15/04/2025,Rent,-1200.00",
  "17/04/2025,Coffee,-4.50"))

# The arithmetic holds two ways round (money out/in swapped, read as a card), and
# nothing on the file -- no heading, no wording -- says which: a person decides.
ambiguous_csv <- function() write_statement_csv(c(
  "Date,Narrative,Col A,Col B,Col C",
  "13/04/2025,Opening balance,,,1000.00",
  "14/04/2025,Item one,,2500.00,3500.00",
  "15/04/2025,Item two,1200.00,,2300.00",
  "17/04/2025,Item three,4.50,,2295.50",
  "18/04/2025,Item four,85.20,,2210.30"))

# convert_sandbox() -> a function converting a file with every output, log, layout
# and tracking line in one throwaway folder; `$dir` names the folder.
convert_sandbox <- function() {
  d <- tempfile("convsandbox_")
  dir.create(d)
  f <- function(path, ...) convert_statement(path, outdir = file.path(d, "out"), logdir = file.path(d, "logs"),
                                              layouts_dir = file.path(d, "layouts"),
                                              tracking_dir = file.path(d, "tracking"), requested_by = "tester", ...)
  attr(f, "dir") <- d
  f
}
sandbox_dir <- function(cv) attr(cv, "dir")

# every_file_text(dir) -- all text written under a folder, for "never written" checks.
every_file_text <- function(dir) {
  fs <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  fs <- fs[!grepl("[.](xlsx)$", fs)]
  paste(unlist(lapply(fs, function(f) readLines(f, warn = FALSE))), collapse = "\n")
}
