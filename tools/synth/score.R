#!/usr/bin/env Rscript
# score.R -- run the engine over the synthetic corpus and SCORE it against the
# ground truth each PDF was drawn from.
#
# WHY A SCORE AND NOT A PASS/FAIL. The suite's golden files answer "has this
# changed?". They cannot answer "how accurate is it?", because a golden file IS
# the reader's own output -- if the reader is wrong, so is the golden. Every PDF
# in tools/synth/ carries a .truth.json written by the generator that drew it, so
# the right answer is known independently and the reader can be measured.
#
# THE NUMBER THAT MATTERS IS `fabricated`: a transaction the reader produced with
# an amount that is NOT the amount on the page and is not NA. A missing row is bad
# and a phantom row is bad, but both are visible to a reviewer counting rows. A
# wrong FIGURE in a row that otherwise looks right is the failure this tool exists
# to prevent, and it is the one a human cannot catch by reading the screen.
#
# `refused` IS COUNTED SEPARATELY AND IS NOT THE SAME FAULT. A cell the reader
# could not honestly read comes back NA and the row carries the `malformed` flag,
# so the screen says "the amount could not be read as a number". That is a gap a
# reviewer can see and act on. Scoring the two together hides the only improvement
# that matters: when the contaminated-cell guard went into .num_one, this corpus's
# fabricated count fell from 13 to 0 while refused rose from 0 to 20 -- a strictly
# better engine that a single combined figure would have scored as unchanged.
#
# A `mustflag_` CASE IS SCORED THE OTHER WAY ROUND. Some pages cannot be read
# correctly by any reader and must not be read wrongly in silence -- a debit drawn
# inside the credit column, say: the page says credit, and no column reader can
# know better. Scoring that on figure accuracy scores an impossibility. The real
# requirement is that the statement comes back FLAGGED, so for these cases the test
# is `trust` is not high, and the fabricated figures are expected rather than
# counted against the engine.
#
# Run:  Rscript tools/synth/score.R <corpus-dir> [case-name-filter]
# The corpus is built by tools/synth/make_corpus.py.

suppressWarnings(suppressMessages({
  library(yaml); library(jsonlite)
}))
root <- normalizePath(".")
Sys.setenv(ENGINE_ROOT = root)
for (f in list.files(file.path(root, "R"), "[.]R$", full.names = TRUE)) source(f)

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("usage: score.R <corpus-dir> [filter]")
corpus <- args[1]
filt <- if (length(args) > 1) args[2] else NULL

TPL_DIR <- file.path(root, "templates", "statements")
tset <- load_templates(TPL_DIR)

# Which template each case is drawn for. The corpus draws at anz_everyday_pdf's
# bands, so that is the template to force; a case that tests DETECTION is scored
# with detection left to the engine instead (template = NA).
tpl_for <- function(case) if (grepl("^detect_", case)) NA_character_ else "anz_everyday_pdf"

# .signed(debit, credit) -- the truth's two columns as the one signed number the
# engine produces. A debit is money out: negative.
.signed <- function(debit, credit) {
  if (!is.null(debit) && !is.na(debit)) return(-abs(as.numeric(debit)))
  if (!is.null(credit) && !is.na(credit)) return(abs(as.numeric(credit)))
  NA_real_
}

eq_money <- function(a, b, tol = 0.005) {
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)
  abs(a - b) < tol
}

score_one <- function(pdf, truth_path) {
  tr <- jsonlite::fromJSON(truth_path, simplifyDataFrame = FALSE)
  case <- tr$case
  want <- lapply(tr$rows, function(r) list(
    date = r$date,
    amount = .signed(r$debit, r$credit),
    balance = if (is.null(r$balance)) NA_real_ else as.numeric(r$balance),
    description = r$description))

  out <- list(case = case, note = tr$note, n_truth = length(want),
              n_parsed = NA_integer_, matched = NA_integer_,
              fabricated = NA_integer_, refused = NA_integer_,
              wrong_dates = NA_integer_, wrong_balances = NA_integer_,
              missing = NA_integer_, phantom = NA_integer_,
              status = NA_character_, trust = NA_character_, detail = "")

  inp <- tryCatch(read_input(pdf), error = function(e) e)
  if (inherits(inp, "error")) {
    out$status <- "READ ERROR"; out$detail <- conditionMessage(inp); return(out)
  }
  tid <- tpl_for(case)
  if (is.na(tid)) {
    det <- tryCatch(detect_statement(inp, tset), error = function(e) NULL)
    if (is.null(det) || !isTRUE(det$matched)) {
      out$status <- "not detected"
      out$detail <- as.character(det$detail %||% "no match")[1]
      out$n_parsed <- 0L; out$matched <- 0L
      out$fabricated <- 0L; out$refused <- 0L
      out$missing <- out$n_truth; out$phantom <- 0L
      out$wrong_dates <- 0L; out$wrong_balances <- 0L
      return(out)
    }
    tid <- det$template_id
  }
  tpl <- tset[[tid]]
  p <- tryCatch(parse_statement(inp, tpl), error = function(e) e)
  if (inherits(p, "error")) {
    out$status <- "PARSE ERROR"; out$detail <- conditionMessage(p); return(out)
  }
  tx <- p$transactions
  out$n_parsed <- nrow(tx)

  rec <- tryCatch(reconcile(p, tpl), error = function(e) NULL)
  if (!is.null(rec)) out$trust <- as.character(rec$trust$level %||% NA)

  # Pair the parsed rows to the truth rows IN DOCUMENT ORDER. Both are in the
  # order the page prints them, so position is the honest pairing; pairing by
  # nearest amount would hide exactly the error being looked for.
  n <- min(nrow(tx), length(want))
  fab <- ref <- wd <- wb <- 0L
  matched <- 0L
  firstbad <- character(0)
  for (i in seq_len(n)) {
    w <- want[[i]]
    g_amt <- suppressWarnings(as.numeric(tx$amount[i]))
    g_dat <- as.character(tx$date[i])
    g_bal <- suppressWarnings(as.numeric(
      if ("balance" %in% names(tx)) tx$balance[i] else NA))
    ok_a <- eq_money(g_amt, w$amount)
    ok_d <- identical(substr(g_dat, 1, 10), w$date)
    ok_b <- eq_money(g_bal, w$balance)
    if (!ok_a) {
      if (is.na(g_amt) && !is.na(w$amount)) {
        ref <- ref + 1L                 # refused: honest, and the row says so
      } else {
        fab <- fab + 1L                 # fabricated: the fault that must be zero
        if (length(firstbad) < 2)
          firstbad <- c(firstbad, sprintf("row %d amount %s want %s", i,
                                          format(g_amt), format(w$amount)))
      }
    }
    if (!ok_d) wd <- wd + 1L
    if (!ok_b) wb <- wb + 1L
    if (ok_a && ok_d && ok_b) matched <- matched + 1L
  }
  out$matched <- matched
  out$fabricated <- fab
  out$refused <- ref
  # A `mustflag_` case passes by being CAUGHT, not by being right.
  out$mustflag <- grepl("^mustflag_", case)
  if (out$mustflag) out$caught <- !identical(out$trust, "high")
  out$wrong_dates <- wd
  out$wrong_balances <- wb
  out$missing <- max(0L, length(want) - nrow(tx))
  out$phantom <- max(0L, nrow(tx) - length(want))
  out$status <- "parsed"
  out$detail <- paste(firstbad, collapse = "; ")
  out
}

cases <- sort(list.files(corpus, "[.]truth[.]json$", full.names = TRUE))
if (!is.null(filt)) cases <- cases[grepl(filt, basename(cases), fixed = TRUE)]
if (!length(cases)) stop("no cases found in ", corpus)

rows <- list()
for (tp in cases) {
  pdf <- sub("[.]truth[.]json$", ".pdf", tp)
  if (!file.exists(pdf)) next
  r <- score_one(pdf, tp)
  rows[[length(rows) + 1]] <- r
}

fmt <- "%-24s %5s %5s %5s %5s %5s %5s %5s %5s  %-7s %s\n"
cat(sprintf(fmt, "case", "truth", "got", "ok", "FABR", "refus", "date", "bal",
            "miss", "trust", ""))
cat(strrep("-", 116), "\n")
bad <- 0L
for (r in rows) {
  clean <- if (isTRUE(r$mustflag)) isTRUE(r$caught) else
    (identical(r$status, "parsed") && r$fabricated == 0L &&
     r$missing == 0L && r$phantom == 0L)
  flag <- if (clean) "" else "  <-- "
  if (!clean) bad <- bad + 1L
  cat(sprintf(fmt, r$case, r$n_truth, r$n_parsed, r$matched, r$fabricated,
              r$refused, r$wrong_dates, r$wrong_balances, r$missing + r$phantom,
              r$trust %||% "-",
              paste0(flag, if (isTRUE(r$mustflag))
                sprintf("must be caught: trust %s -> %s", r$trust %||% "-",
                        if (isTRUE(r$caught)) "CAUGHT" else "MISSED")
                else if (identical(r$status, "parsed")) r$detail else r$status)))
}
cat(strrep("-", 116), "\n")
.tot <- function(f) sum(vapply(rows, function(r)
  if (isTRUE(r$mustflag)) 0L else as.integer(r[[f]] %||% 0L), integer(1)), na.rm = TRUE)
cat(sprintf("%d of %d cases clean (no fabricated figure, no missing or phantom row)\n",
            length(rows) - bad, length(rows)))
cat(sprintf("FABRICATED FIGURES: %d        (this must be 0)\n", .tot("fabricated")))
cat(sprintf("refused figures:    %d        (honest: the row carries `malformed`)\n",
            .tot("refused")))
