# tools/compare-qvf.R -- the side-by-side check, run on the owner's own machine.
#
#   Rscript tools/compare-qvf.R "<QVF output .csv or .xlsx>" "<this tool's .csv or .xlsx>"
#
# For ONE statement converted by both the QVF and this tool, prints COUNTS ONLY:
# rows in each, money out and money in totals in each, rows matched on date +
# amount, and rows found by only one of them. No date, description, account or
# figure of any row is printed, so the printout is safe to paste to Claude.
#
# Columns are found by their headings: a date column ("Date", "Transaction Date"),
# and either one amount column ("Amount") or a money-out and a money-in column
# ("Debit"/"Withdrawals" and "Credit"/"Deposits"). If a heading cannot be found,
# the script says which one and stops.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !all(file.exists(args))) {
  cat("usage: Rscript tools/compare-qvf.R <QVF output file> <this tool's output file>\n"); quit(status = 1)
}
here <- tryCatch(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))),
                 error = function(e) ".")
setwd(normalizePath(file.path(here, "..")))
suppressWarnings(suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)))

read_any <- function(p) {
  if (grepl("[.]xlsx?$", p, ignore.case = TRUE)) {
    sh <- openxlsx::getSheetNames(p)
    s <- if ("Transactions" %in% sh) "Transactions" else sh[1]
    as.data.frame(openxlsx::read.xlsx(p, s, detectDates = FALSE), stringsAsFactors = FALSE)
  } else utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
}
pick <- function(df, rx, what, who) {
  h <- tolower(gsub("[^a-z]+", " ", tolower(names(df))))
  k <- which(grepl(rx, h))
  if (!length(k)) return(NULL)
  df[[k[1]]]
}
num <- function(v) {
  v <- trimws(as.character(v)); v[is.na(v)] <- ""
  neg <- grepl("^\\(.*\\)$|-|\\bDR\\b|\\bOD\\b", v, ignore.case = TRUE)
  x <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", v)))
  ifelse(neg, -x, x)
}
day <- function(v) {
  v <- as.character(v)
  serial <- grepl("^[0-9]{5}(\\.0+)?$", v)
  out <- rep(NA_character_, length(v))
  out[serial] <- format(as.Date(as.numeric(v[serial]), origin = "1899-12-30"))
  # each format only on text of its own shape: "03/02/26" is never read as year 26
  fm <- c("^[0-9]{4}-[0-9]{2}-[0-9]{2}" = "%Y-%m-%d", "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$" = "%d/%m/%Y",
          "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$" = "%d/%m/%y", "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$" = "%d-%m-%Y",
          "^[0-9]{1,2}[.][0-9]{1,2}[.][0-9]{4}$" = "%d.%m.%Y", "^[0-9]{1,2} [A-Za-z]{3} [0-9]{4}$" = "%d %b %Y",
          "^[0-9]{1,2} [A-Za-z]{4,} [0-9]{4}$" = "%d %B %Y", "^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}$" = "%d-%b-%Y",
          "^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2}$" = "%d-%b-%y")
  for (rx in names(fm)) {
    k <- is.na(out) & grepl(rx, trimws(v))
    if (any(k)) out[k] <- format(as.Date(substr(trimws(v[k]), 1, if (fm[[rx]] == "%Y-%m-%d") 10 else 99), fm[[rx]]))
  }
  out
}
side <- function(p, who) {
  df <- read_any(p)
  d <- pick(df, "(^| )date( |$)|transaction date|posted|processed", "date", who)
  if (is.null(d)) { cat(sprintf("%s: no date column found (headings: %d columns).\n", who, ncol(df))); quit(status = 1) }
  a <- pick(df, "^ *amount *$|^ *amount nz", "amount", who)
  if (is.null(a)) {
    o <- pick(df, "debit|withdraw|money out|payments", "out", who)
    i <- pick(df, "credit|deposit|money in|receipts", "in", who)
    if (is.null(o) || is.null(i)) { cat(sprintf("%s: no amount column, and no money-out and money-in pair, found.\n", who)); quit(status = 1) }
    a <- ifelse(is.na(num(i)), 0, abs(num(i))) - ifelse(is.na(num(o)), 0, abs(num(o)))
  } else a <- num(a)
  keep <- !is.na(day(d)) & !is.na(a)
  data.frame(date = day(d)[keep], amount = round(a[keep], 2), stringsAsFactors = FALSE)
}

q <- side(args[1], "QVF"); n <- side(args[2], "This tool")
key <- function(x) paste(x$date, sprintf("%.2f", x$amount))
# matched as multisets: two identical rows on both sides match twice
kq <- table(key(q)); kn <- table(key(n))
both <- intersect(names(kq), names(kn))
matched <- sum(pmin(kq[both], kn[both]))
cat(sprintf("%-28s %12s %12s\n", "", "QVF", "This tool"))
cat(sprintf("%-28s %12d %12d\n", "rows", nrow(q), nrow(n)))
cat(sprintf("%-28s %12.2f %12.2f\n", "money out (total)", -sum(q$amount[q$amount < 0]), -sum(n$amount[n$amount < 0])))
cat(sprintf("%-28s %12.2f %12.2f\n", "money in (total)", sum(q$amount[q$amount > 0]), sum(n$amount[n$amount > 0])))
cat(sprintf("%-28s %12d %12d\n", "rows matched (date+amount)", matched, matched))
cat(sprintf("%-28s %12d %12d\n", "rows only in this one", nrow(q) - matched, nrow(n) - matched))
cat(if (matched == nrow(q) && matched == nrow(n)) "\nSAME: every row matches.\n" else "\nDIFFERENT: see the counts above.\n")
