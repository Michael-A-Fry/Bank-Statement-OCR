# auto_read_summ.R -- opening, closing and total figures printed under labels no
# dictionary knows ("Kickoff kitty: 8,968.54", "Sum of departures 4,387.11"), named
# by the statement's own arithmetic instead of by their words.
#
# Nothing here proves anything on its own. It only NAMES lines (opening, closing,
# money-in total, money-out total); the reading is then made again with those names
# and must pass every check exactly as a labelled statement must. A line named this
# way helps prove the rows, but counts as the statement's printed start or end
# (ends_printed) only when it is printed above the table's first row, in its
# header or a summary box: a figure under the rows that equals the last balance
# read may be a balance carried to a page that is missing, but a carried balance is
# never printed above the rows, and a file cut short still prints the whole
# statement's closing up there, which the rows read then do not reach. A name is
# given only when it is over-determined:
#   * with a running balance: the opening is the one figure that, with the rows
#     before the first printed balance, gives that balance (one way only), AND the
#     closing is the one figure equal to the last balance -- both or neither;
#   * without one: exactly one figure pair and one reading of the columns satisfy
#     opening + movements = closing, AND (unless both sit at the table's own edges)
#     at least one other figure equals a column's total.
# Lines INSIDE the table that end up unnamed keep it from being read this way: a
# line that may be a transaction is never set aside.

# .ar_summary_cands(model) -- the figures that may be summaries: lines outside the
# table that print words and then one figure, ending the line, and no date; and the
# table's own dateless lines before its first dated row (page 1) or after its last
# (last page), on a statement that prints a date on every other row.
.ar_summary_cands <- function(model, decimal) {
  R <- model$rows; n <- nrow(R)
  rowkey <- paste(R$page, R$line)
  # Above the table's first row (in its header or a summary box over it).
  above <- function(p, y) p < R$page[1] || (p == R$page[1] && y < R$y[1])
  out <- list()
  add <- function(...) out[[length(out) + 1L]] <<- list(...)
  for (pg in Filter(Negate(is.null), model$pgs)) {
    p <- pg$page; ln <- pg$lines; ph <- pg$ph; w <- pg$w
    rg <- model$regions[[as.character(p)]]
    for (i in seq_len(nrow(ln))) {
      l <- ln$line[i]
      if (paste(p, l) %in% rowkey || ln$summary[i] || (!is.null(rg) && l %in% rg$lines)) next
      m <- ph[ph$line == l & ph$kind == "money", , drop = FALSE]
      if (nrow(m) != 1L || any(ph$line == l & ph$kind == "date")) next
      wl <- w[w$line == l, , drop = FALSE]
      if (max(wl$x1) > m$x1 + 0.5 || !any(wl$kind == "text" & wl$x1 <= m$x)) next
      v <- .ar_anchor_value(m$text, FALSE, decimal)
      if (is.na(v)) next
      add(page = p, line = l, y = ln$y[i], where = "outside", vals = v, figs = NA, i = NA_integer_, head = above(p, ln$y[i]))
    }
  }
  # The table's own leading and trailing dateless lines.
  if (n >= 3L) {
    first_d <- which(!is.na(R$date))[1]; last_d <- utils::tail(which(!is.na(R$date)), 1)
    lead <- if (!is.na(first_d) && first_d > 1L) seq_len(first_d - 1L) else integer(0)
    lead <- lead[R$page[lead] == R$page[1]]
    trail <- if (length(last_d) && last_d < n) (last_d + 1L):n else integer(0)
    trail <- trail[R$page[trail] == R$page[n]]
    inner <- setdiff(seq_len(n), c(lead, trail))
    if ((length(lead) || length(trail)) && length(lead) + length(trail) <= 5L && !anyNA(R$date[inner])) {
      for (i in c(lead, trail)) {
        f <- model$cells[i, ]
        add(page = R$page[i], line = R$line[i], y = R$y[i], where = if (i %in% lead) "top" else "bottom",
            vals = vapply(f, function(t) if (is.na(t)) NA_real_ else .ar_anchor_value(t, FALSE, decimal), 0),
            figs = f, i = i)
      }
    }
  }
  out
}

# .ar_summary_names(model, decimal, content) -- which candidate lines are the
# opening, the closing and the column totals, by the arithmetic; NULL when that is
# not over-determined. Returns list(page, line, class, label) per named line.
.ar_summary_names <- function(model, decimal, content = NULL) {
  cands <- .ar_summary_cands(model, decimal)
  if (length(cands) < 2L) return(NULL)
  ins <- Filter(function(c) c$where != "outside", cands)
  table_i <- vapply(ins, function(c) c$i, 0L)
  n <- nrow(model$rows)
  rows <- setdiff(seq_len(n), table_i)
  single <- Filter(function(c) sum(!is.na(c$vals)) == 1L, cands)
  sval <- function(c) c$vals[!is.na(c$vals)][1]
  K <- ncol(model$cells)
  V <- .ar_values(model$cells[rows, , drop = FALSE], decimal)
  name <- function(c, cls, lab) list(page = c$page, line = c$line, class = cls, label = lab, head = isTRUE(c$head))
  rdc <- content$rd
  if (!is.null(rdc) && isTRUE(rdc$b > 0L) && identical(content$basis, "arithmetic") && !length(ins)) {
    # ---- with a running balance: opening AND closing, each one way only ----
    bal <- rdc$bal; A <- rdc$A
    if (length(bal) != n) return(NULL)
    M <- abs(.ar_values(model$cells, decimal)$S[, which(rdc$roles %in% c("amount", "debit", "credit")), drop = FALSE])
    ord <- if (identical(rdc$dir, "new")) rev(seq_len(n)) else seq_len(n)
    t1 <- which(!is.na(bal[ord]))[1]; tn <- utils::tail(which(!is.na(bal[ord])), 1)
    if (is.na(t1) || !length(tn)) return(NULL)
    before <- ord[seq_len(t1)]
    sg <- rdc$chain$signs %||% rep(NA_real_, n)
    known <- !is.na(A[before]) & !(identical(rdc$conv, "B") & is.na(sg[before]))
    mags <- rowSums(M[before, , drop = FALSE], na.rm = TRUE)
    fixed <- sum(A[before][known]); um <- mags[!known]
    if (length(um) > 8L) return(NULL)
    combos <- if (length(um)) as.matrix(expand.grid(rep(list(c(-1, 1)), length(um)))) else matrix(0, 1, 0)
    o_req <- round(bal[ord[t1]] - fixed - as.numeric(combos %*% um), 2)
    after <- if (tn < n) ord[(tn + 1L):n] else integer(0)
    if (anyNA(A[after])) return(NULL)
    c_req <- round(bal[ord[tn]] + sum(A[after]), 2)
    vs <- vapply(single, sval, 0)
    o_hit <- which(vapply(vs, function(v) sum(abs(v - o_req) < PARAM_MONEY_TOL), 0) == 1L)
    c_hit <- which(abs(vs - c_req) < PARAM_MONEY_TOL)
    if (sum(vapply(vs, function(v) any(abs(v - o_req) < PARAM_MONEY_TOL), logical(1))) != 1L || length(o_hit) != 1L) return(NULL)
    if (length(c_hit) != 1L || o_hit == c_hit) return(NULL)
    return(list(name(single[[o_hit]], "open", "opening balance"), name(single[[c_hit]], "close", "closing balance")))
  }
  # ---- without a running balance: opening + movements = closing, one way only ----
  roles_list <- Filter(function(r) !("balance" %in% r), .ar_role_candidates(K))
  # Which figure is the opening decides which column is money in (opening + in -
  # out = closing holds just as well with both swapped). Without a balance that
  # rests on WHERE they are printed: a line above the table's first row and one
  # below its last, the dates running oldest first (or the mirror, newest first).
  # A header or a summary box printed opening-before-closing is the same rule on
  # weaker ground. It is OFF: it is used only when the environment variable
  # AR_SUMMARY_ORDER is "1", which waits on a product-owner decision.
  dd <- .ar_rows_iso(model)
  asc <- length(dd) >= 2L && !anyNA(dd) && all(diff(dd) >= 0) && any(diff(dd) > 0)
  desc <- length(dd) >= 2L && !anyNA(dd) && all(diff(dd) <= 0) && any(diff(dd) < 0)
  box_ok <- identical(Sys.getenv("AR_SUMMARY_ORDER"), "1")
  ordered <- function(a, b) {
    A <- single[[a]]; B <- single[[b]]
    if (A$where == "top" && B$where == "bottom") return(asc)
    if (A$where == "bottom" && B$where == "top") return(desc)
    box_ok && (A$page < B$page || (A$page == B$page && A$y < B$y))
  }
  hits <- list()
  for (r in roles_list) {
    am <- .ar_amounts(V, r, "S", FALSE)
    if (anyNA(am$A)) next
    net <- round(sum(am$A), 2)
    for (a in seq_along(single)) for (b in seq_along(single)) {
      if (a == b || !ordered(a, b)) next
      if (abs(sval(single[[a]]) + net - sval(single[[b]])) < PARAM_MONEY_TOL)
        hits[[length(hits) + 1L]] <- list(roles = r, A = am$A, o = a, c = b)
    }
  }
  if (!length(hits)) return(NULL)
  # Each fit names two figures; every OTHER figure must then be a column total, or
  # (outside the table only) something else. Two printed totals fit the equation
  # with the columns swapped (in + (out - in) = out), so the fit kept is the one
  # that explains the MOST printed figures, every one inside the table among them,
  # and it must be the only one that does.
  explain <- function(h) {
    A <- h$A
    tin <- round(sum(A[A > 0]), 2); tout <- round(sum(-A[A < 0]), 2)
    dj <- which(h$roles == "debit"); cj <- which(h$roles == "credit")
    O <- single[[h$o]]; C <- single[[h$c]]
    named <- list(name(O, "open", "opening balance"), name(C, "close", "closing balance"))
    used <- c(paste(O$page, O$line), paste(C$page, C$line))
    ntot <- 0L; bad <- FALSE
    for (c in cands) {
      if (paste(c$page, c$line) %in% used) next
      v <- c$vals; ok <- FALSE
      if (sum(!is.na(v)) == 1L) {
        x <- abs(v[!is.na(v)][1])
        if (abs(x - tout) < PARAM_MONEY_TOL && abs(x - tin) >= PARAM_MONEY_TOL) {
          named[[length(named) + 1L]] <- name(c, "total", "total debits"); ok <- TRUE }
        else if (abs(x - tin) < PARAM_MONEY_TOL && abs(x - tout) >= PARAM_MONEY_TOL) {
          named[[length(named) + 1L]] <- name(c, "total", "total credits"); ok <- TRUE }
      } else if (sum(!is.na(v)) == 2L && length(dj) && length(cj) && !is.na(v[dj]) && !is.na(v[cj]) &&
                 abs(abs(v[dj]) - tout) < PARAM_MONEY_TOL && abs(abs(v[cj]) - tin) < PARAM_MONEY_TOL) {
        named[[length(named) + 1L]] <- name(c, "total", "totals"); ok <- TRUE
      }
      if (ok) ntot <- ntot + 1L
      else if (c$where != "outside") bad <- TRUE   # a table line that may be a transaction
    }
    at_edges <- (O$where == "top" && C$where == "bottom") || (O$where == "bottom" && C$where == "top")
    list(named = named, n = 2L + ntot, ok = !bad && (at_edges || ntot > 0L),
         key = paste(c(sprintf("%.2f", A), h$o, h$c), collapse = "|"))
  }
  ex <- Filter(function(e) e$ok, lapply(hits, explain))
  if (!length(ex)) return(NULL)
  best <- max(vapply(ex, `[[`, 0L, "n"))
  top <- Filter(function(e) e$n == best, ex)
  if (length(unique(vapply(top, `[[`, "", "key"))) != 1L) return(NULL)
  top[[1]]$named
}

# .ar_mark_summaries(pgs, named) -- the pages with each named line made a summary
# line of its class, under a label the reader's own tests understand.
.ar_mark_summaries <- function(pgs, named) {
  for (e in named) {
    pg <- pgs[[e$page]]
    k <- match(e$line, pg$lines$line)
    if (is.na(k)) next
    pg$lines$summary[k] <- TRUE
    pg$lines$aclass[k] <- e$class
    pg$lines$label[k] <- e$label
    if (is.null(pg$lines$named)) pg$lines$named <- FALSE
    pg$lines$named[k] <- !isTRUE(e$head)
    pgs[[e$page]] <- pg
  }
  pgs
}

# .ar_rows_iso(model) -- the rows' dates as Dates, read under the format most of
# them read under (year-less ones in a sentinel year: only their order is used).
.ar_rows_iso <- function(model) {
  R <- model$rows
  has <- !is.na(R$date) & nzchar(R$date_fmts)
  if (!any(has)) return(as.Date(character(0)))
  fv <- unlist(strsplit(R$date_fmts[has], "|", fixed = TRUE))
  f <- names(sort(table(fv), decreasing = TRUE))[1]
  yl <- !grepl("%[Yy]", f)
  iso <- if (yl) parse_date(paste(R$date[has], "2000"), paste(f, "%Y"))$iso else parse_date(R$date[has], f)$iso
  as.Date(iso)
}
