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
# the other way -- but only when the page agrees (a row's wording or a money
# heading) and nothing on it says the opposite; otherwise nothing is decided. That
# decision is recorded in the proof, never hidden.

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
.AR_TOTAL_RX <- paste0("^(?:(?:sub|grand|page|period|statement|account|running|closing|opening)[- ]?)?totals?",
  "(?: (?:at|for|of|on|to|this|the)(?: the)?(?: end of)?(?: the)? ",
  "(?:page|period|statement|month|year|day|section|account))?$|",
  "^(?:total )?(?:withdrawals|deposits|credits|debits|payments|purchases|charges|receipts|",
  "money (?:in|out)|paid (?:in|out)|payments (?:in|out))",
  "(?: (?:&|and) (?:debits|credits|other charges|other debits|other credits|refunds|other))?$|",
  # A section's own total ("Total for card ending 5133 J SAMPLE"): the scope words
  # come first, so a payee called "Total Fitness" never matches.
  "^(?:sub[- ]?)?totals? for (?:card|account|cardholder)\\b.*$")

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
  if (grepl("^(?:(?:sub|grand|page|period|statement|account|running|closing|opening)[- ]?)?totals?", s, perl = TRUE)) return("both")
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
# Wordings only a card or loan's own statement prints (an advert for a card says
# "credit card" and "credit limit", not what is due).
.AR_LIAB_OWN <- c("minimum payment", "minimum repayment", "payment due", "amount owing", "available credit")
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
  # The role search asks for the same few figures hundreds of times; each is read
  # once. The cache lives in the lexicon's own cache, so a vocabulary edit (a new
  # sign word) clears it with everything else read under the old words.
  fc <- get0("auto_read::figs", envir = .LEXICON_CACHE, inherits = FALSE)
  if (is.null(fc) || length(ls(fc)) > 5000L) {
    fc <- new.env(parent = emptyenv()); assign("auto_read::figs", fc, envir = .LEXICON_CACHE)
  }
  key <- paste(decimal, text, sep = "\r")
  hit <- get0(key, envir = fc, inherits = FALSE)
  if (is.null(hit)) {
    hit <- list(v = .num(text, decimal), sk = .ar_sign_kind(text))
    assign(key, hit, envir = fc)
  }
  .ar_sem(hit$v, hit$sk, liab)
}

# A carried balance (brought / carried forward, b/f, c/f, from or to another page)
# continues ONE chain across a break; an opening or closing balance may start or
# end one.
.AR_CARRY_RX <- "forward|fwd|\\b[bc]/f\\b|previous page|next page"

# .ar_section_break(pts, q) -- a printed closing balance followed, at the same
# place in the table, by a printed opening balance: one account's section ends and
# the next account's begins (a combined statement). Never a carried balance, so a
# missing page between a carried-forward and a brought-forward still breaks.
.ar_section_break <- function(src1, src2) src1 == "close@table" & src2 == "open@table"

# .ar_anchor_points(anchors, n, dir, liab, bal_col, decimal) -- the balances the
# statement prints OUTSIDE its rows, as chain points: (position in time order,
# value). Opening sits before the first transaction and closing after the last,
# wherever they are printed. A brought / carried forward line inside the table sits
# where it is printed. Totals are returned separately.
.ar_anchor_points <- function(anchors, n, dir, liab, bal_col = 0L, decimal = "auto") {
  pos <- numeric(0); val <- numeric(0); src <- character(0)
  # A bundle of statements prints a summary box (opening, closing) per statement.
  # Each box's opening then sits where its statement's rows begin, and its closing
  # where the next statement's begin (or after the last row).
  box_open <- which(vapply(anchors, function(a) identical(a$class, "open") && !isTRUE(a$in_table), logical(1)))
  bundle <- length(box_open) > 1L && !identical(dir, "new")
  for (k in seq_along(anchors)) {
    a <- anchors[[k]]
    if (!(a$class %in% c("open", "close"))) next
    txt <- if (bal_col > 0L && !is.na(a$figs[bal_col])) a$figs[bal_col] else a$value_text
    # A line printing several figures and none in the balance column is a summary
    # band (opening, money out, money in, closing on one line), not one balance.
    if (bal_col > 0L && is.na(a$figs[bal_col]) && isTRUE(a$n_money > 1L)) next
    v <- .ar_anchor_value(txt, liab, decimal)
    if (is.na(v)) next
    r <- a$before_rows
    if (bundle && !a$in_table) {
      nxt <- box_open[box_open > k]
      p <- if (a$class == "open") r else if (length(nxt)) anchors[[nxt[1]]]$before_rows else n
      pos <- c(pos, min(max(p, 0), n)); val <- c(val, v)
      src <- c(src, paste0(a$class, "@summary"))
      next
    }
    p <- if (a$class == "open" && (!a$in_table || r == 0L)) 0
         else if (a$class == "close" && (!a$in_table || r == n)) n
         else if (!a$in_table) next
         else if (identical(dir, "new")) n - r else r
    if (identical(dir, "new") && a$in_table && a$class == "open" && r == n) p <- 0
    if (identical(dir, "new") && a$in_table && a$class == "close" && r == 0L) p <- n
    carry <- grepl(.AR_CARRY_RX, .ar_norm_label(a$label), perl = TRUE)
    # Positions are counted in the model's rows; if the reader produced fewer, a
    # check elsewhere already fails, and the point must stay on the chain.
    pos <- c(pos, min(max(p, 0), n)); val <- c(val, v)
    src <- c(src, paste0(a$class, if (carry) "~carry", if (a$in_table) "@table" else "@summary"))
  }
  data.frame(pos = pos, val = val, src = src, stringsAsFactors = FALSE)
}

# ---- the chain ---------------------------------------------------------------------------

# .ar_chain(A, uns, bal, apts, dir) -- every balance step, in time order. A step
# joins two consecutive printed balances (a row's own, or an anchor's); the
# movements between them must account for the difference to the cent.
#   A    amounts in the account holder's terms, print order (NA = not read)
#   uns  rows whose sign is unknown (an unsigned column): the step must be met by
#        exactly ONE choice of signs, or the sign is left unsettled
# Returns the steps, the settled signs, and which rows each kind of step covers.
# Vectorised over the steps: the role search runs this hundreds of times.
.ar_chain <- function(A, uns, bal, apts, dir) {
  n <- length(A)
  ord <- if (identical(dir, "new")) rev(seq_len(n)) else seq_len(n)
  Ac <- A[ord]; Uc <- uns[ord]; Bc <- bal[ord]
  bq <- which(!is.na(Bc))
  pos <- c(bq, apts$pos); val <- c(Bc[bq], apts$val); src <- c(rep("row", length(bq)), apts$src)
  o <- order(pos, src != "row")
  pos <- pos[o]; val <- val[o]; src <- src[o]
  signs <- rep(NA_real_, n)
  L <- length(pos) - 1L
  if (L < 1L) return(list(steps = data.frame(from = integer(0), to = integer(0), expected = numeric(0),
                                             got = numeric(0), ok = logical(0), unknown = integer(0),
                                             ambiguous = logical(0)),
                          signs = signs, ord = ord, one_row = integer(0), covered = integer(0),
                          sections = 1L, breaks = numeric(0)))
  p1 <- pos[-(L + 1L)]; p2 <- pos[-1]
  expv <- round(val[-1] - val[-(L + 1L)], 2)
  brk <- p1 == p2 & .ar_section_break(src[-(L + 1L)], src[-1])
  na <- is.na(Ac); un <- Uc & !na
  cs <- c(0, cumsum(ifelse(na | un, 0, Ac)))
  cna <- c(0, cumsum(na)); cun <- c(0, cumsum(un))
  fixed <- cs[p2 + 1L] - cs[p1 + 1L]
  unk <- as.integer(cna[p2 + 1L] - cna[p1 + 1L]); nu <- cun[p2 + 1L] - cun[p1 + 1L]
  got <- round(fixed, 2)
  ok <- ifelse(unk > 0L, NA, abs(got - expv) < PARAM_MONEY_TOL)
  amb <- rep(FALSE, L)
  # Steps with an unsigned figure: exactly one choice of signs must meet them.
  for (q in which(nu > 0 & unk == 0L)) {
    rows <- (p1[q] + 1L):p2[q]
    u <- Uc[rows]; mags <- abs(Ac[rows][u])
    if (length(mags) > 12L) { ok[q] <- FALSE; amb[q] <- TRUE; next }
    sg <- as.matrix(expand.grid(rep(list(c(-1, 1)), length(mags))))
    tot <- round(fixed[q] + as.numeric(sg %*% mags), 2)
    sol <- which(abs(tot - expv[q]) < PARAM_MONEY_TOL)
    ok[q] <- length(sol) >= 1L
    got[q] <- if (length(sol)) expv[q] else NA_real_
    if (length(sol) == 1L) signs[ord[rows[u]]] <- sg[sol, ] else if (length(sol) > 1L) amb[q] <- TRUE
  }
  ok[brk] <- TRUE; expv[brk] <- 0; got[brk] <- 0
  good <- which(ok %in% TRUE & p2 > p1)
  covered <- unique(ord[unlist(lapply(good, function(q) (p1[q] + 1L):p2[q]))])
  one <- good[p2[good] - p1[good] == 1L]
  list(steps = data.frame(from = as.integer(p1), to = as.integer(p2), expected = expv, got = got,
                          ok = ok, unknown = unk, ambiguous = amb),
       signs = signs, ord = ord, one_row = ord[p2[one]], covered = if (is.null(covered)) integer(0) else covered,
       sections = 1L + sum(brk), breaks = p1[brk])
}

# .ar_chain_score(ch) -- the counts a candidate is judged on.
.ar_chain_score <- function(ch) {
  s <- ch$steps
  if (!nrow(s)) return(c(links = 0, held = 0, failed = 0, unknown = 0, ambiguous = 0))
  # A step between two balances printed at the same place (a closing balance in the
  # summary box and again under the table) must agree, but proves no row: it is
  # not a link.
  rows <- s$to > s$from
  c(links = sum(rows), held = sum(s$ok %in% TRUE & !s$ambiguous & rows), failed = sum(s$ok %in% FALSE),
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

# .ar_roles(V, anchors, liab_ev, decimal, heading_roles, texts) -- try every reading
# and keep the ones the arithmetic allows. Returns the candidates that pass, the one
# chosen, and why. `texts` (each row's words) only ever vote, on the one question
# the arithmetic cannot answer: which way round a reading and its negation go.
# `only` -- the figure roles a person gave on Please check: just that assignment is
# tried, and the arithmetic still settles its sign convention and order. The
# account type is the statement's own (liab_ev), never the arithmetic's choice
# here: money in and out swapped, read as a card instead of an account, adds up
# just as well, so a swapped fix would otherwise "prove" itself with every sign
# inverted.
.ar_roles <- function(V, anchors, liab_ev, decimal = "auto", heading_roles = NULL, texts = NULL,
                      only = NULL) {
  K <- ncol(V$S); n <- nrow(V$S)
  roles_list <- if (!is.null(only)) list(as.character(only)) else .ar_role_candidates(K)
  res <- list()
  col_signed <- vapply(seq_len(K), function(j) any(V$SK[, j] %in% c("-lead", "-trail", "()", "+"), na.rm = TRUE), logical(1))
  col_marked <- vapply(seq_len(K), function(j) any(V$SK[, j] %in% c("CR", "DR", "OD"), na.rm = TRUE), logical(1))
  for (roles in roles_list) {
    b <- which(roles == "balance"); b <- if (length(b)) b else 0L
    a <- which(roles == "amount")
    convs <- if (length(a)) c("S", if (!col_signed[a]) "U", if (!col_signed[a] && !col_marked[a] && b > 0L) "B") else "S"
    for (conv in convs) for (liab in c(FALSE, TRUE)) {
      if (liab && conv %in% c("U", "B")) next
      if (!is.null(only) && liab != isTRUE(liab_ev$liability)) next
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
      # The account type says which way round only when the page agrees. A money
      # heading is the bank's own word on its columns ("Debits" is purchases on a
      # card too), so one naming them the other way stops it, and one agreeing
      # settles it. Without such a heading (an "Amount" column), the rows' wording
      # decides with it: each row that says which way it went (a salary paid in, a
      # purchase paid out) counts for or against, a card's own statement wording
      # counts for, and it must win outright. No card word anywhere is no proof of
      # a bank account -- a card may simply not say so -- and an advert for a
      # credit card must never flip an everyday statement.
      if (length(pick) == 1L) {
        r <- pick[[1]]
        hr <- heading_roles
        agree <- length(hr) == length(r$roles) && any(!is.na(hr) & hr %in% c("debit", "credit") & hr == r$roles)
        if (.ar_heading_flips(hr, r$roles)) pick <- list()
        else if (!agree) {
          v <- .ar_desc_votes(texts, r$A)
          ph <- liab_ev$phrases %||% character(0)
          support <- (want && length(ph) >= 3L && any(ph %in% .AR_LIAB_OWN)) + v[["for"]]
          if (support == 0 || v[["against"]] >= support) pick <- list()
        }
      }
      if (length(pick) == 1L) {
        distinct <- pick
        note <- if (want) sprintf("the arithmetic holds either way round; the statement declares itself a card or loan (%s), so its plain figures are what is owed",
                                  paste(utils::head(liab_ev$phrases, 3), collapse = ", "))
                else "the arithmetic holds either way round; nothing declares a card or loan and the rows agree, so plain figures are money in"
      }
    }
  }
  # A reading that leaves printed balances unexplained proves nothing: when another
  # reading holds MORE balance steps but breaks somewhere, the statement contradicts
  # itself, and a reading that calls that balance column "other" (or ignores it)
  # only hides where. Two rows' figures swapped keep opening + movements =
  # closing; only the running balance shows it.
  if (length(distinct) == 1L && length(res)) {
    hmax <- max(vapply(res, function(r) r$score[["held"]], 0))
    if (distinct[[1]]$score[["held"]] < hmax) {
      distinct <- list()
      note <- "a reading using more of the printed balances does not add up"
    }
  }
  best <- NULL
  if (length(res)) {
    sc <- t(vapply(res, function(r) r$score, numeric(5)))
    # Ties (a reading and its negation break in the same places) go to the one the
    # account type and the headings favour, so the reading shown for checking has
    # its signs the right way round wherever the statement says which way that is.
    o <- order(-sc[, "held"], sc[, "failed"], sc[, "unknown"], vapply(res, function(r) sum(r$roles == "other"), 0),
               vapply(res, function(r) !identical(r$liab, isTRUE(liab_ev$liability)) +
                        .ar_heading_flips(heading_roles, r$roles), 0))
    best <- res[[o[1]]]
  }
  list(chosen = if (length(distinct) == 1L) distinct[[1]] else NULL,
       n_distinct = length(distinct), distinct = distinct, best = best, note = note,
       tried = length(res))
}

# .ar_heading_flips(hroles, roles) -- a heading names a column money out where the
# reading has it money in, or the other way round. Only those two words count: a
# heading measured over a right-aligned figure column can take in its neighbour's
# "Balance", so a balance heading is too loose to overrule anything.
.ar_heading_flips <- function(hroles, roles) {
  if (is.null(hroles) || length(hroles) != length(roles)) return(FALSE)
  any(!is.na(hroles) & ((hroles == "debit" & roles == "credit") | (hroles == "credit" & roles == "debit")))
}

# Description wording that says which way money moved, whatever the account: a
# vote, never a proof. "Automatic payment" and "direct debit" are left out, as on
# a card they are the holder paying the card off (money in).
.AR_IN_WORDS <- "\\b(?:salary|wages|deposit|refund|interest credit|credit interest|payment received|thank you|direct credit|tfr from|transfer from|ird refund)\\b"
.AR_OUT_WORDS <- "\\b(?:eftpos|pos w/d|atm|withdrawal|purchase|bill payment|fee|fees|tfr to|transfer to|visa debit)\\b"

# .ar_desc_votes(texts, A) -- how many rows' wording agrees with the sign a reading
# gives them, and how many disagree.
.ar_desc_votes <- function(texts, A) {
  t <- tolower(texts %||% character(0))
  if (length(t) != length(A)) return(c("for" = 0, against = 0))
  inw <- grepl(.AR_IN_WORDS, t, perl = TRUE); outw <- grepl(.AR_OUT_WORDS, t, perl = TRUE)
  one <- xor(inw, outw) & !is.na(A) & A != 0
  agree <- one & ((inw & A > 0) | (outw & A < 0))
  c("for" = sum(agree), against = sum(one & !agree))
}

# .wa_money_role(label) -- what the statement's own column heading calls this money
# column. The debit / credit wordings come from the LEXICON, so a bank that writes
# "Paid out" / "Money in" is taught in the dictionary, never in code. NA means the
# heading did not say, or said both -- the caller then falls back to position.
.wa_money_role <- function(label) {
  s <- tolower(trimws(label %||% ""))
  if (!nzchar(s)) return(NA_character_)
  rx <- function(cat, dflt) paste(safe(as.character(lex(cat)), dflt), collapse = "|")
  d <- grepl(rx("amount_style_debit_headers",  c("debit", "withdrawal")), s)
  cr <- grepl(rx("amount_style_credit_headers", c("credit", "deposit")), s)
  if (grepl("balance", s)) return("balance")
  if (d && !cr) return("debit")
  if (cr && !d) return("credit")
  if (grepl("amount|value", s)) return("amount")
  NA_character_
}

# .wa_positional_roles(n) -- the money columns labelled by POSITION, for a
# statement whose header row could not be read. Three money columns is the
# money-out / money-in / balance shape every NZ retail statement in this repo
# uses. Four or more, with nothing on the page saying which is which, is a guess
# -- and the tool is not a guesser, so it returns NULL and the caller says so.
.wa_positional_roles <- function(n) switch(as.character(n),
  "1" = "amount", "2" = c("amount", "balance"),
  "3" = c("debit", "credit", "balance"), NULL)

# .ar_vote_roles(K, heading_roles) -- what to SHOW when the arithmetic cannot decide
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
