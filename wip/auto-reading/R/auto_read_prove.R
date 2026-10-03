# auto_read_prove.R -- the arithmetic half of the automatic reader (spec section 4,
# steps 7-8): which column is money out, money in, a signed amount or the balance,
# which way the statement runs, and whether the reading PROVES.
#
# The rule throughout: a reading is proven only when every balance step holds to
# the cent and no other reading does. Headings and description wording are votes,
# used to choose what to SHOW when nothing proves; they never prove anything.
#
# One sign question the arithmetic cannot answer: a reading and its exact
# negation (every amount and every balance flipped) satisfy the same equations. A
# printed CR / DR / OD settles it, because those words mean the same thing on
# every statement. When nothing printed settles it, the statement's own account
# type does: a credit card or loan prints what is OWED, so its plain figures run
# the other way. That decision is recorded in the proof, never hidden.

# ---- anchors: opening, closing, carried balances and totals ------------------------

# Wordings beyond the label dictionary's: the carried-balance shorthands banks print
# at page breaks, and card wordings the table reader's summary test does not know.
.AR_OPEN_EXTRA <- c("opening balance", "balance brought forward", "brought forward", "balance b/f",
  "b/f", "b/fwd", "bal b/f", "balance b/fwd", "balance bfwd", "balance forward", "previous balance",
  "balance from previous statement", "balance from previous page", "starting balance",
  "beginning balance", "opening ledger balance", "previous statement balance",
  "balance brought forward from previous page", "brought forward from previous page")
.AR_CLOSE_EXTRA <- c("closing balance", "balance carried forward", "carried forward", "balance c/f",
  "c/f", "c/fwd", "bal c/f", "balance c/fwd", "balance cfwd", "new balance", "ending balance",
  "final balance", "balance at end", "balance at end of period", "closing ledger balance",
  "balance carried forward to next page", "carried forward to next page")
.AR_TOTAL_RX <- paste0("^(?:(?:sub|grand|page|period|statement|account|running)[- ]?)?totals?",
  "(?: (?:at|for|of|on|to|this|the)(?: the)?(?: end of)?(?: the)? ",
  "(?:page|period|statement|month|year|day|section|account))?$|",
  "^(?:total )?(?:withdrawals|deposits|credits|debits|payments|purchases|charges|receipts|",
  "money (?:in|out)|paid (?:in|out)|payments (?:in|out))",
  "(?: (?:&|and) (?:debits|credits|other charges|other debits|other credits|refunds|other))?$")

.ar_norm_label <- function(s) {
  s <- tolower(trimws(as.character(s)))
  s <- gsub("[[:space:]]+", " ", s)
  s <- sub("^statement ", "", s)
  trimws(sub("[[:space:]:.-]+$", "", s))
}

# .ar_anchor_phrases() -- opening and closing wordings: the label dictionary's own
# (so an analyst's added wording reaches the reader) plus the shorthands above.
.ar_anchor_phrases <- function() {
  d <- safe(default_label_dict(), list())
  op <- unlist(d$opening_balance$any_of %||% character(0))
  cl <- unlist(d$closing_balance$any_of %||% character(0))
  list(open = unique(.ar_norm_label(c(op, .AR_OPEN_EXTRA))),
       close = unique(.ar_norm_label(c(cl, .AR_CLOSE_EXTRA))))
}

# .ar_anchor_class(label) -- "open", "close", "total" or "" for each line label
# (a line already reduced to its words by .pdf_line_label). Whole-label matches
# only, so a payee called "Closing Down Sale" is never an anchor.
.ar_anchor_class <- function(label) {
  ph <- .ar_anchor_phrases()
  s <- .ar_norm_label(label)
  out <- rep("", length(s))
  out[s %in% ph$open] <- "open"
  out[s %in% ph$close] <- "close"
  out[out == "" & nzchar(s) & grepl(.AR_TOTAL_RX, s, perl = TRUE)] <- "total"
  out
}

# .ar_total_side(label, liability) -- which movements a printed total adds up:
# "debit", "credit", "both" (a totals line printing a figure per column) or "".
# "Payments" on a card are money IN; on a bank account they are money OUT.
.ar_total_side <- function(label, liability = FALSE) {
  s <- .ar_norm_label(label)
  if (grepl("withdrawal|debit|purchase|charges|money out|paid out|payments out", s)) return("debit")
  if (grepl("deposit|credit|receipt|money in|paid in|payments in", s)) return("credit")
  if (grepl("payment", s)) return(if (liability) "credit" else "debit")
  if (grepl("^(?:(?:sub|grand|page|period|statement|account|running)[- ]?)?totals?", s, perl = TRUE)) return("both")
  ""
}

# ---- the account's own declaration --------------------------------------------------

# .ar_liability_evidence(lines_text) -- does the statement declare itself a credit
# card or loan (a balance that is OWED)? Counted outside the transaction rows, so a
# "VISA DEBIT" purchase never makes an everyday account a card. Two distinct
# phrases are needed.
.AR_LIAB_PHRASES <- c("credit card", "credit limit", "available credit", "minimum payment",
  "minimum repayment", "card limit", "card number", "payment due", "amount owing",
  "visa statement", "mastercard statement", "card statement", "purchase rate", "cash advance")
.ar_liability_evidence <- function(text) {
  s <- tolower(paste(text, collapse = " \n "))
  hits <- .AR_LIAB_PHRASES[vapply(.AR_LIAB_PHRASES, function(p) grepl(p, s, fixed = TRUE), logical(1))]
  list(liability = length(hits) >= 2L, phrases = hits)
}

# ---- values ----------------------------------------------------------------------------

# .ar_values(cells) -- the figures as read (S = what .num reads: a minus, brackets,
# DR and OD negative, CR and plain positive), their magnitudes and how each carries
# its sign. NA where the cell is blank.
.ar_values <- function(cells, decimal = "auto") {
  cells <- as.matrix(cells)
  u <- unique(cells[!is.na(cells)])
  sv <- if (length(u)) stats::setNames(.num(u, decimal), u) else numeric(0)
  sk <- if (length(u)) stats::setNames(.ar_sign_kind(u), u) else character(0)
  S <- matrix(NA_real_, nrow(cells), ncol(cells)); SK <- matrix("", nrow(cells), ncol(cells))
  has <- !is.na(cells)
  S[has] <- sv[cells[has]]; SK[has] <- sk[cells[has]]
  list(S = S, M = abs(S), SK = SK, has = has)
}

# .ar_sem(S, SK, liab) -- a figure in the account holder's terms (money in and a
# positive balance are positive). On a liability the printed plain figure is owed,
# so it flips; a printed CR / DR / OD already says which way it goes and never does.
.ar_sem <- function(S, SK, liab) {
  if (!isTRUE(liab)) return(S)
  ifelse(SK %in% c("CR", "DR", "OD"), S, -S)
}

# .ar_anchor_value(text, liab, decimal) -- an anchor's figure in the same terms.
.ar_anchor_value <- function(text, liab, decimal = "auto") {
  if (is.null(text) || is.na(text)) return(NA_real_)
  v <- .num(text, decimal)
  .ar_sem(v, .ar_sign_kind(text), liab)
}

# .ar_anchor_points(anchors, n, dir, liab, bal_col, decimal) -- the balances the
# statement prints OUTSIDE its rows, as chain points: (position in time order,
# value). Opening sits before the first transaction and closing after the last,
# wherever they are printed. A brought / carried forward line inside the table sits
# where it is printed. Totals are returned separately.
.ar_anchor_points <- function(anchors, n, dir, liab, bal_col = 0L, decimal = "auto") {
  pts <- data.frame(pos = numeric(0), val = numeric(0), src = character(0), stringsAsFactors = FALSE)
  for (a in anchors) {
    if (!(a$class %in% c("open", "close"))) next
    txt <- if (bal_col > 0L && !is.na(a$figs[bal_col])) a$figs[bal_col] else a$value_text
    # A line printing several figures and none in the balance column is a summary
    # band (opening, money out, money in, closing on one line), not one balance.
    if (bal_col > 0L && is.na(a$figs[bal_col]) && isTRUE(a$n_money > 1L)) next
    v <- .ar_anchor_value(txt, liab, decimal)
    if (is.na(v)) next
    r <- a$before_rows
    pos <- if (a$class == "open" && (!a$in_table || r == 0L)) 0
           else if (a$class == "close" && (!a$in_table || r == n)) n
           else if (!a$in_table) next
           else if (identical(dir, "new")) n - r else r
    if (identical(dir, "new") && a$in_table && a$class == "open" && r == n) pos <- 0
    if (identical(dir, "new") && a$in_table && a$class == "close" && r == 0L) pos <- n
    pts[nrow(pts) + 1L, ] <- list(pos, v, paste0(a$class, if (a$in_table) "@table" else "@summary"))
  }
  pts
}

# ---- the chain ---------------------------------------------------------------------------

# .ar_chain(A, uns, bal, apts, dir) -- every balance step, in time order. A step
# joins two consecutive printed balances (a row's own, or an anchor's); the
# movements between them must account for the difference to the cent.
#   A    amounts in the account holder's terms, print order (NA = not read)
#   uns  rows whose sign is unknown (an unsigned column): the step must be met by
#        exactly ONE choice of signs, or the sign is left unsettled
# Returns the steps, the settled signs, and which rows each kind of step covers.
.ar_chain <- function(A, uns, bal, apts, dir) {
  n <- length(A)
  ord <- if (identical(dir, "new")) rev(seq_len(n)) else seq_len(n)
  Ac <- A[ord]; Uc <- uns[ord]; Bc <- bal[ord]
  bq <- which(!is.na(Bc))
  pts <- rbind(data.frame(pos = bq, val = Bc[bq], src = rep("row", length(bq)), stringsAsFactors = FALSE), apts)
  pts <- pts[order(pts$pos, pts$src != "row"), , drop = FALSE]
  signs <- rep(NA_real_, n)
  L <- nrow(pts) - 1L
  steps <- data.frame(from = integer(0), to = integer(0), expected = numeric(0), got = numeric(0),
                      ok = logical(0), unknown = integer(0), ambiguous = logical(0))
  if (L < 1L) return(list(steps = steps, signs = signs, ord = ord, one_row = integer(0), covered = integer(0)))
  st <- vector("list", L)
  covered <- integer(0); one_row <- integer(0)
  for (q in seq_len(L)) {
    p1 <- pts$pos[q]; p2 <- pts$pos[q + 1L]
    expv <- round(pts$val[q + 1L] - pts$val[q], 2)
    rows <- if (p2 > p1) (p1 + 1L):p2 else integer(0)
    a <- Ac[rows]; u <- Uc[rows]
    unk <- sum(is.na(a)); amb <- FALSE; ok <- NA; got <- NA_real_
    if (!length(rows)) { ok <- abs(expv) < PARAM_MONEY_TOL; got <- 0 }
    else if (unk == 0L && !any(u)) { got <- round(sum(a), 2); ok <- abs(got - expv) < PARAM_MONEY_TOL }
    else if (unk == 0L) {
      fixed <- sum(a[!u]); mags <- abs(a[u])
      if (length(mags) <= 12L) {
        sg <- as.matrix(expand.grid(rep(list(c(-1, 1)), length(mags))))
        tot <- round(fixed + as.numeric(sg %*% mags), 2)
        sol <- which(abs(tot - expv) < PARAM_MONEY_TOL)
        ok <- length(sol) >= 1L
        if (length(sol) == 1L) { signs[ord[rows[u]]] <- sg[sol, ]; got <- expv }
        else if (length(sol) > 1L) { amb <- TRUE; got <- expv }
      } else { ok <- FALSE; amb <- TRUE }
    }
    if (isTRUE(ok)) { covered <- c(covered, ord[rows]); if (length(rows) == 1L) one_row <- c(one_row, ord[rows]) }
    st[[q]] <- data.frame(from = as.integer(p1), to = as.integer(p2), expected = expv, got = got,
                          ok = ok, unknown = unk, ambiguous = amb)
  }
  list(steps = do.call(rbind, st), signs = signs, ord = ord, one_row = one_row, covered = unique(covered))
}

# .ar_chain_score(ch) -- the counts a candidate is judged on.
.ar_chain_score <- function(ch) {
  s <- ch$steps
  if (!nrow(s)) return(c(links = 0, held = 0, failed = 0, unknown = 0, ambiguous = 0))
  c(links = nrow(s), held = sum(s$ok %in% TRUE & !s$ambiguous), failed = sum(s$ok %in% FALSE),
    unknown = sum(is.na(s$ok) & s$unknown > 0), ambiguous = sum(s$ambiguous))
}

# ---- roles by arithmetic ------------------------------------------------------------------

# .ar_role_candidates(K) -- every way to read K figure columns: at most one balance;
# then one amount column, or a money-out / money-in pair, or one of them alone;
# everything else "other" (a figure that is not money in or out, e.g. a foreign
# amount). Deterministic order.
.ar_role_candidates <- function(K) {
  out <- list()
  for (b in c(0L, seq_len(K))) {
    rest <- setdiff(seq_len(K), b)
    base <- rep("other", K); if (b > 0L) base[b] <- "balance"
    for (a in rest) { r <- base; r[a] <- "amount"; out[[length(out) + 1L]] <- r }
    for (d in rest) for (c in rest) if (d != c) {
      r <- base; r[d] <- "debit"; r[c] <- "credit"; out[[length(out) + 1L]] <- r }
    for (d in rest) { r <- base; r[d] <- "debit"; out[[length(out) + 1L]] <- r }
    for (c in rest) { r <- base; r[c] <- "credit"; out[[length(out) + 1L]] <- r }
  }
  out
}

# .ar_amounts(V, roles, conv, liab) -- the movement each row makes under one
# reading, in the account holder's terms, and which rows have an unknown sign.
#   conv "S" figures as printed; "U" an unsigned column whose plain figure is money
#   out (a CR marks money in); "B" an unsigned column whose signs only the balance
#   can settle.
.ar_amounts <- function(V, roles, conv, liab) {
  n <- nrow(V$S)
  A <- rep(NA_real_, n); uns <- rep(FALSE, n)
  a <- which(roles == "amount"); d <- which(roles == "debit"); cc <- which(roles == "credit")
  if (length(a)) {
    S <- V$S[, a]; SK <- V$SK[, a]; M <- V$M[, a]
    A <- switch(conv,
      S = .ar_sem(S, SK, liab),
      U = ifelse(SK == "CR", M, -M),
      B = M)
    if (identical(conv, "B")) uns <- !is.na(M)
  } else {
    dv <- if (length(d)) V$M[, d] else rep(NA_real_, n)
    cv <- if (length(cc)) V$M[, cc] else rep(NA_real_, n)
    A <- ifelse(is.na(dv) & is.na(cv), NA_real_, ifelse(is.na(cv), 0, cv) - ifelse(is.na(dv), 0, dv))
  }
  list(A = A, uns = uns)
}

# .ar_roles(V, anchors, liab_evidence, opts) -- try every reading and keep the ones
# the arithmetic allows. Returns the candidates that pass, the one chosen, and why.
.ar_roles <- function(V, anchors, liab_ev, decimal = "auto", heading_roles = NULL) {
  K <- ncol(V$S); n <- nrow(V$S)
  roles_list <- .ar_role_candidates(K)
  res <- list()
  col_signed <- vapply(seq_len(K), function(j) any(V$SK[, j] %in% c("-lead", "-trail", "()", "+"), na.rm = TRUE), logical(1))
  col_marked <- vapply(seq_len(K), function(j) any(V$SK[, j] %in% c("CR", "DR", "OD"), na.rm = TRUE), logical(1))
  for (roles in roles_list) {
    b <- which(roles == "balance"); b <- if (length(b)) b else 0L
    a <- which(roles == "amount")
    convs <- if (length(a)) c("S", if (!col_signed[a]) "U", if (!col_signed[a] && !col_marked[a] && b > 0L) "B") else "S"
    for (conv in convs) for (liab in c(FALSE, TRUE)) {
      if (liab && conv %in% c("U", "B")) next
      am <- .ar_amounts(V, roles, conv, liab)
      na_rows <- sum(is.na(am$A))
      if (na_rows > max(1, floor(0.1 * n))) next
      bal <- if (b > 0L) .ar_sem(V$S[, b], V$SK[, b], liab) else rep(NA_real_, n)
      for (dir in c("old", "new")) {
        apts <- .ar_anchor_points(anchors, n, dir, liab, b, decimal)
        ch <- .ar_chain(am$A, am$uns, bal, apts, dir)
        sc <- .ar_chain_score(ch)
        if (sc[["links"]] == 0) next
        A_out <- am$A
        if (any(am$uns)) A_out <- ifelse(am$uns, ch$signs * am$A, am$A)
        res[[length(res) + 1L]] <- list(roles = roles, conv = conv, liab = liab, dir = dir,
          score = sc, chain = ch, A = A_out, bal = bal, b = b, na_rows = na_rows,
          uncovered = setdiff(seq_len(n), ch$covered))
      }
    }
  }
  passing <- Filter(function(r) r$score[["failed"]] == 0 && r$score[["held"]] >= 1 &&
                      r$score[["ambiguous"]] == 0, res)
  # A step left unknown (an unreadable amount) is weaker than one that holds: keep
  # the readings with the fewest.
  if (length(passing)) {
    mu <- min(vapply(passing, function(r) r$score[["unknown"]] + r$na_rows, 0))
    passing <- Filter(function(r) r$score[["unknown"]] + r$na_rows == mu, passing)
  }
  # Identical amounts are one reading, however they were reached; the one that rests
  # on the most balance steps stands for it.
  keyf <- function(r) paste(ifelse(is.na(r$A), "NA", sprintf("%.2f", r$A)), collapse = "|")
  if (length(passing)) {
    passing <- passing[order(-vapply(passing, function(r) r$score[["held"]], 0))]
  }
  keys <- vapply(passing, keyf, "")
  distinct <- passing[!duplicated(keys)]
  note <- character(0)
  if (length(distinct) == 2L) {
    r1 <- distinct[[1]]; r2 <- distinct[[2]]
    neg <- isTRUE(all.equal(ifelse(is.na(r1$A), 0, r1$A), -ifelse(is.na(r2$A), 0, r2$A))) &&
           identical(is.na(r1$A), is.na(r2$A))
    if (neg) {
      want <- isTRUE(liab_ev$liability)
      pick <- Filter(function(r) identical(r$liab, want), distinct)
      if (length(pick) == 1L) {
        distinct <- pick
        note <- if (want) sprintf("the arithmetic holds either way round; the statement declares itself a card or loan (%s), so its plain figures are what is owed",
                                  paste(utils::head(liab_ev$phrases, 3), collapse = ", "))
                else "the arithmetic holds either way round; nothing declares a card or loan, so plain figures are money in"
      }
    }
  }
  best <- NULL
  if (length(res)) {
    sc <- t(vapply(res, function(r) r$score, numeric(5)))
    o <- order(-sc[, "held"], sc[, "failed"], sc[, "unknown"], vapply(res, function(r) sum(r$roles == "other"), 0))
    best <- res[[o[1]]]
  }
  list(chosen = if (length(distinct) == 1L) distinct[[1]] else NULL,
       n_distinct = length(distinct), distinct = distinct, best = best, note = note,
       tried = length(res))
}

# .ar_vote_roles(V, heading_roles) -- what to SHOW when the arithmetic cannot decide
# (no running balance and no printed totals): the headings' own words, else the
# position every NZ retail statement uses. Never a proof.
.ar_vote_roles <- function(K, heading_roles) {
  hr <- heading_roles
  if (length(hr) == K && !anyNA(hr) && !anyDuplicated(hr[hr != "other"]) &&
      any(hr %in% c("amount", "debit", "credit")) && !("amount" %in% hr && any(c("debit", "credit") %in% hr)))
    return(list(roles = hr, by = "heading"))
  pos <- .wa_positional_roles(K)
  if (!is.null(pos)) return(list(roles = pos, by = "position"))
  if (K >= 1L) return(list(roles = c(rep("other", K - 1L), "amount"), by = "position"))
  NULL
}
