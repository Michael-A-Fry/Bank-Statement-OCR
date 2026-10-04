# truth.R -- reading a `.truth.json`, in ONE place.
#
# Every scorer compares the engine's output against the generator's ground truth,
# and they must agree about what the truth SAYS or their verdicts are not
# comparable. The truth file records `debit` and `credit`; it never records `amount`.
#
# This file exists because of a measured mistake: the 1.x bench script (removed at
# 2.0.0 with the template functions it called) was written with its own
# comparison, read `row$amount` -- a field that does not exist -- and reported 900 of
# 900 amounts FABRICATED on a 30-page statement the engine had read perfectly. A
# measuring instrument that can accuse the engine has to be as checkable as the
# engine, which is the same lesson the corpus learnt when its own money() printed
# abs(x) and blamed the reader for 221 wrong balances.

# .signed(debit, credit) -- the truth's two columns as the one signed figure the
# engine emits. A debit is money out and is negative.
.signed <- function(debit, credit) {
  if (!is.null(debit) && !is.na(debit)) return(-abs(as.numeric(debit)))
  if (!is.null(credit) && !is.na(credit)) return(abs(as.numeric(credit)))
  NA_real_
}

# eq_money(a, b) -- cents, not floats. NA equals NA: "neither of us has a figure" is
# agreement, and it is how a redacted cell has to compare.
eq_money <- function(a, b, tol = 0.005) {
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)
  abs(a - b) < tol
}

# read_truth(path) -- the truth file as the list of rows both harnesses score against.
read_truth <- function(path) {
  tr <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  tr$want <- lapply(tr$rows, function(r) list(
    date = r$date,
    amount = .signed(r$debit, r$credit),
    balance = if (is.null(r$balance)) NA_real_ else as.numeric(r$balance),
    description = r$description))
  tr
}
