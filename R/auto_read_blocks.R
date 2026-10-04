# auto_read_blocks.R -- reading a statement pack one table at a time.
#
# A statement pack prints other tables beside the statement's own: rate and fee
# tables, an account summary, a loan schedule, another account's mini-statement.
# The whole-document column model (.ar_model) takes every dated line with a figure
# as a row of ONE table, so those tables' rows and columns are mixed in and
# nothing proves. Only then (.ar_read_pdf), the pack is read again a table at a
# time:
#
#   * each page is cut into TABLE BLOCKS: runs of dated lines with a figure, with
#     the dateless rows, summary lines and wrapped lines that touch them; a
#     heading, a title, a paragraph or a blank band ends a block. Runs of lines
#     that print several figures, or a list of single figures on one right edge,
#     are FIGURE TABLES (a summary, a rate or fee table), never transactions;
#   * blocks with the same date column and the same figure columns are one table
#     across its pages (a cluster);
#   * each cluster is read ON ITS OWN, every other block set aside, and the
#     arithmetic decides, with every check the whole-document reading has. A
#     reading that sets a table aside passes only when the statement's own printed
#     opening and closing balances add up over the rows read (tables_set_aside),
#     so none of the statement's own rows can be among what was set aside;
#   * the statement's own table is the cluster with the most rows dated inside the
#     statement period, then the largest; a tie is not decided;
#   * another cluster that proves on its own is another account's table (a linked
#     account, a loan, a term deposit): it is returned apart, labelled, in
#     rd$other_accounts, and never mixed into the statement's rows.

# At most this many of the largest clusters are read, plus at most
# .AR_BLOCK_MAX_ENDS more that print their own opening or closing balance (another
# account's mini-statement), each in at most .AR_BLOCK_MAX_RUNS runs of its blocks.
.AR_BLOCK_MAX_CLUSTERS <- 4L
.AR_BLOCK_MAX_ENDS <- 12L
.AR_BLOCK_MAX_RUNS <- 6L

# .ar_blocks(pgs) -- every table block on every page, in document order. Each is
# list(page, lines, seeds, y0, y1, date_x, date_x1, edges, title, figure_only).
.ar_blocks <- function(pgs) {
  out <- list()
  for (pg in pgs) {
    if (is.null(pg) || !nrow(pg$ph)) next
    first_q <- length(out) + 1L
    seeds <- .ar_seed_lines(pg)
    ln <- pg$lines; ph <- pg$ph; w <- pg$w; h <- pg$h
    nl <- nrow(ln)
    is_seed <- ln$line %in% seeds
    sm <- ph[ph$kind == "money" & ph$standalone, , drop = FALSE]
    has_money <- ln$line %in% sm$line
    has_date <- ln$line %in% ph$line[ph$kind == "date"]
    anchor <- ln$summary & ln$line %in% ph$line[ph$kind == "money"]
    bodyish <- has_money & !ln$summary
    sd <- ph[ph$kind == "date" & ph$line %in% seeds, , drop = FALSE]
    sd <- sd[!duplicated(sd$line), , drop = FALSE]
    wl <- split(seq_len(nrow(w)), w$line)
    minx <- vapply(as.character(ln$line), function(l) min(w$x[wl[[l]]]), 0)
    textonly <- vapply(as.character(ln$line), function(l) !any(w$kind[wl[[l]]] %in% c("money", "date")), logical(1))
    sx <- which(is_seed)
    pitch <- if (length(sx) >= 2L) stats::median(diff(ln$y[sx])) else 2 * h
    pitch <- min(max(pitch, 1.2 * h), 2.4 * h)
    used <- rep(FALSE, nl)
    for (i0 in sx) {
      if (used[i0]) next
      dx <- sd$x[sd$line == ln$line[i0]][1]; dx1 <- sd$x1[sd$line == ln$line[i0]][1]
      first <- i0; last <- i0
      # Upwards over the dateless rows and summary lines that touch the block (a
      # dated line that is not a row shape -- a title with a date -- ends it).
      while (first > 1L && !used[first - 1L] && (bodyish[first - 1L] || anchor[first - 1L]) &&
             !(has_date[first - 1L] && !is_seed[first - 1L] && !anchor[first - 1L]) &&
             ln$y[first] - ln$y[first - 1L] <= 2.5 * pitch) first <- first - 1L
      # Downwards over rows dated in the same date column, dateless rows, summary
      # lines, and wrapped lines set tight under the line they continue.
      while (last < nl) {
        nx <- last + 1L
        if (used[nx]) break
        cont <- textonly[nx] && !ln$footer[nx] && !ln$summary[nx] && minx[nx] > dx1 + 2 &&
                ln$y[nx] - ln$y1[last] <= 1.2 * h && !anchor[last]
        same_dcol <- function() { d2 <- sd$x[sd$line == ln$line[nx]][1]; isTRUE(abs(d2 - dx) <= 3) }
        row_ok <- (is_seed[nx] && same_dcol()) || (bodyish[nx] && !has_date[nx]) || anchor[nx]
        ok <- cont || (row_ok && ln$y[nx] - ln$y[last] <= 2.5 * pitch)
        if (!ok) break
        last <- nx
      }
      while (last > i0 && textonly[last] && !anchor[last]) last <- last - 1L
      ix <- first:last
      used[ix] <- TRUE
      bseeds <- ln$line[ix][is_seed[ix]]
      m <- sm[sm$line %in% bseeds, , drop = FALSE]
      e <- if (nrow(m)) { g <- .ar_cluster(m$x1, 2); as.numeric(tapply(m$x1, g, stats::median)) } else numeric(0)
      out[[length(out) + 1L]] <- list(page = pg$page, lines = ln$line[ix], seeds = bseeds,
        y0 = ln$y[first], y1 = ln$y1[last], date_x = dx, date_x1 = dx1, edges = sort(e),
        first_i = first, title = "", figure_only = FALSE)
    }
    # Figure tables: runs of the remaining lines that print figures, where one line
    # prints two or more figures apart (an account summary, a tax or rate table),
    # with the total lines under them; or a list of three or more lines each with
    # one figure on a shared right edge, none of them a balance or total line (a
    # fee schedule -- a summary box of opening and closing balances is the
    # statement's own and stays). Never read as the statement and always set
    # aside, so a total printed under one is never taken as the statement's own.
    nfig <- vapply(ln$line, function(l) sum(sm$line == l), 0L)
    i <- 1L
    while (i <= nl) {
      if (used[i] || nfig[i] == 0L) { i <- i + 1L; next }
      j <- i
      while (j < nl && !used[j + 1L] && nfig[j + 1L] > 0L && ln$y[j + 1L] - ln$y[j] <= 2.5 * pitch) j <- j + 1L
      ix <- i:j
      edge1 <- vapply(ix, function(q) max(sm$x1[sm$line == ln$line[q]]), 0)
      list1 <- length(ix) >= 3L && !any(ln$summary[ix]) && diff(range(edge1)) <= 2
      if (any(nfig[ix] >= 2L & !ln$summary[ix]) || list1) {
        used[ix] <- TRUE
        out[[length(out) + 1L]] <- list(page = pg$page, lines = ln$line[ix], seeds = integer(0),
          y0 = ln$y[i], y1 = ln$y1[j], date_x = NA_real_, date_x1 = NA_real_, edges = numeric(0),
          first_i = i, title = "", figure_only = TRUE)
      }
      i <- j + 1L
    }
    # The title zone of each dated table: up to 6 lines above it, stopping at any
    # line of another table (a summary table above is not its title). Its heading
    # row is the words-only line set directly over it, as plain words.
    for (q in seq_along(out)[seq_along(out) >= first_q]) {
      b <- out[[q]]
      if (isTRUE(b$figure_only)) next
      tz <- integer(0); r <- b$first_i - 1L
      while (r >= 1L && length(tz) < 6L && !used[r]) { tz <- c(r, tz); r <- r - 1L }
      out[[q]]$title <- paste(ln$raw[tz], collapse = " / ")
      hr <- b$first_i - 1L
      out[[q]]$head <- if (hr >= 1L && !used[hr] && textonly[hr] && ln$y[b$first_i] - ln$y[hr] <= 2.5 * pitch)
        paste(sort(unique(tolower(unlist(regmatches(ln$raw[hr], gregexpr("[A-Za-z]{2,}", ln$raw[hr])))))), collapse = " ")
      else ""
    }
  }
  out
}

# .ar_acct_numbers(s) -- the account and card numbers printed in some text.
.ar_acct_numbers <- function(s) {
  s <- paste(s %||% character(0), collapse = "\n")
  rx <- c(safe(lex("account_regex"), .ACCT_RX), safe(lex("card_regex"), .CARD_RX))
  unique(unlist(lapply(rx, function(r) regmatches(s, gregexpr(r, s, perl = TRUE))[[1]])))
}

# .ar_holder_numbers(pages) -- the statement's own account numbers: those printed
# after an account-number label ("Account number: 06-2901-5302607-001"), else the
# first account or card number the file prints (the convention .ar_own_text uses).
.ar_holder_numbers <- function(pages) {
  s <- paste(pages %||% character(0), collapse = "\n")
  lab <- regmatches(s, gregexpr("(?i)\\b(?:account|acct|card)\\s*(?:number|no\\.?|#)\\s*:?\\s*[^\\n]{0,40}", s, perl = TRUE))[[1]]
  own <- .ar_acct_numbers(lab)
  if (length(own)) return(own)
  utils::head(.ar_acct_numbers(s), 1L)
}

# .ar_block_same_design(core, b) -- does block b print in the core block's columns?
# The same date column, and every figure column b prints one of the core's (a page
# without deposits prints fewer). A block on another page may sit further left or
# right as a whole (a page printed further over, as .ar_page_shifts allows for the
# whole file): then its date column and two or more figure columns must all line up
# with the core's under that one shift.
.ar_block_same_design <- function(core, b, tol = 2) {
  if (is.na(core$date_x) || is.na(b$date_x)) return(FALSE)
  ec <- core$edges; eb <- b$edges
  if (!length(ec) || !length(eb)) return(FALSE)
  s <- 0
  if (abs(core$date_x - b$date_x) > tol) {
    if (identical(core$page, b$page) || length(eb) < 2L || abs(b$date_x - core$date_x) > 120) return(FALSE)
    s <- b$date_x - core$date_x
  }
  all(vapply(eb - s, function(v) any(abs(v - ec) <= 1.5), logical(1)))
}

# .ar_block_look_alike(core, b) -- is block b printed as the core's table is: in
# the same columns, or under the same heading row?
.ar_block_look_alike <- function(core, b)
  .ar_block_same_design(core, b) || (nzchar(core$head %||% "") && identical(core$head, b$head))

# .ar_block_compat(core, b) -- is block b part of the core block's table? The same
# design, and no other account's number in b's title that the core's title does
# not name too: a table titled with another account's number is that account's
# table, never a page of this one.
.ar_block_compat <- function(core, b) identical(core$foreign, b$foreign) && .ar_block_same_design(core, b)

# .ar_block_clusters(blocks) -- each block joins the cluster of the largest block it
# is compatible with; returns the cluster index per block, clusters numbered by
# size (dated rows), largest first.
.ar_block_clusters <- function(blocks) {
  ns <- vapply(blocks, function(b) length(b$seeds), 0)
  ord <- order(-ns, seq_along(blocks))
  cl <- integer(length(blocks)); k <- 0L
  for (i in ord) {
    if (cl[i]) next
    k <- k + 1L; cl[i] <- k
    for (j in ord) if (!cl[j] && .ar_block_compat(blocks[[i]], blocks[[j]])) cl[j] <- k
  }
  tot <- tapply(ns, cl, sum)
  rk <- rank(-tot, ties.method = "first")
  as.integer(rk[as.character(cl)])
}

# .ar_keep_list(blocks, idx) -- the lines of the blocks idx, per page ("3" = page 3).
.ar_keep_list <- function(blocks, idx) {
  out <- list()
  for (i in idx) { p <- as.character(blocks[[i]]$page); out[[p]] <- c(out[[p]], blocks[[i]]$lines) }
  out
}

# .ar_block_runs(members, core) -- the runs of a cluster's blocks (document order)
# that hold its largest block: the whole cluster first, then shorter runs.
.ar_block_runs <- function(members, core) {
  k <- length(members); c0 <- match(core, members)
  runs <- list()
  for (len in k:1) for (i in seq_len(k - len + 1L)) {
    j <- i + len - 1L
    if (i <= c0 && j >= c0) runs[[length(runs) + 1L]] <- members[i:j]
  }
  utils::head(runs, .AR_BLOCK_MAX_RUNS)
}

# .ar_flags(lst, name) -- the TRUE-or-not of one field across a list of lists.
.ar_flags <- function(lst, name) vapply(lst, function(r) isTRUE(r[[name]]), logical(1))

# .ar_in_period_rows(tx, md) -- how many rows are dated inside the printed period
# (every row with a date when no period is printed).
.ar_in_period_rows <- function(tx, md) {
  if (is.null(tx) || !nrow(tx)) return(0)
  d <- suppressWarnings(as.Date(tx$date))
  per <- .ar_period_dates(md)
  if (!length(per)) return(sum(!is.na(d)))
  sum(Reduce(`|`, lapply(per, function(pp) !is.na(d) & d >= pp[1] & d <= pp[2])))
}

# .ar_block_reading(ctx, base) -- read the clusters one at a time. Returns the
# statement's candidate (`pick`, or NULL with the reason in `why`) and the other
# accounts that prove on their own (`others`).
.ar_block_reading <- function(ctx, base) {
  allb <- .ar_blocks(base)
  if (!length(allb)) return(NULL)
  fig <- vapply(allb, function(b) isTRUE(b$figure_only), logical(1))
  figb <- allb[fig]
  blocks <- allb[!fig]
  if (!length(blocks) || (length(blocks) < 2L && !length(figb))) return(NULL)
  holder <- .ar_holder_numbers(ctx$pages_text)
  for (i in seq_along(blocks)) {
    nums <- .ar_acct_numbers(blocks[[i]]$title)
    blocks[[i]]$accts <- nums
    blocks[[i]]$foreign <- sort(setdiff(nums, holder))
  }
  cl <- .ar_block_clusters(blocks)
  ns <- vapply(blocks, function(b) length(b$seeds), 0)
  tot <- tapply(ns, cl, sum)
  if (length(tot) < 2L && !length(figb)) return(NULL)
  fig_aside <- .ar_keep_list(figb, seq_along(figb))
  with_figs <- function(aside) { for (pp in names(fig_aside)) aside[[pp]] <- c(aside[[pp]], fig_aside[[pp]]); aside }
  key <- function(cd) if (is.null(cd$tx) || !nrow(cd$tx)) "" else paste(cd$tx$date, sprintf("%.2f", cd$tx$amount), collapse = "|")
  ends_in <- function(i) { b <- blocks[[i]]; pg <- base[[b$page]]
    any(pg$lines$aclass[pg$lines$line %in% b$lines] %in% c("open", "close")) }
  # The largest clusters, and every cluster that prints its own opening or closing
  # balance (another account's mini-statement is read on its own too).
  has_ends <- vapply(seq_along(tot), function(k) any(vapply(which(cl == k), ends_in, logical(1))), logical(1))
  todo <- sort(unique(c(seq_len(min(.AR_BLOCK_MAX_CLUSTERS, length(tot))),
                        utils::head(which(has_ends), .AR_BLOCK_MAX_ENDS))))
  # A block of the same design may be set aside from its cluster only when it is
  # not a transaction table in its own right: it prints no opening or closing
  # balance, and read alone its running balance does not hold on every step (a
  # page of this statement, or another account printed in the same design, does
  # both; a coincidence in a table of limits or rates holds one step at most among
  # several that fail).
  own_table <- list()
  is_own_table <- function(i) {
    kk <- as.character(i)
    if (!is.null(own_table[[kk]])) return(own_table[[kk]])
    v <- ends_in(i)
    if (!v) {
      mo <- list(keep = .ar_keep_list(blocks, i), aside = with_figs(.ar_keep_list(blocks, setdiff(seq_along(blocks), i))))
      cd1 <- safe(.ar_pdf_attempt(ctx, base, mo, "alone", model = .ar_model(base, mo)), NULL)
      v <- isTRUE(cd1$proof$kind == "chain") && isTRUE(cd1$proof$held >= 1L) &&
           isTRUE(cd1$proof$held == cd1$proof$links)
    }
    own_table[[kk]] <<- v
    v
  }
  res <- list()
  for (k in todo) {
    mem <- which(cl == k)
    if (sum(ns[mem]) < 2L && !has_ends[k]) next
    core <- mem[which.max(ns[mem])]
    best <- NULL; passed <- list()
    for (run in .ar_block_runs(mem, core)) {
      dropped <- setdiff(mem, run)
      if (length(dropped) && any(vapply(dropped, is_own_table, logical(1)))) next
      mopts <- list(keep = .ar_keep_list(blocks, run),
                    aside = with_figs(.ar_keep_list(blocks, setdiff(seq_along(blocks), run))),
                    aside_dated = .ar_keep_list(blocks, setdiff(seq_along(blocks), run)))
      bm <- .ar_model(base, mopts)
      # The account type (card or loan?) is read from this table's own title and
      # summary, never from a table set aside (a fee schedule's "Cash advance
      # (credit card)" does not make an everyday account a card).
      ctx_b <- ctx
      if (!is.null(bm)) { ctx_b$own_text <- .ar_own_text(bm); ctx_b$liab <- .ar_liability_evidence(ctx_b$own_text) }
      src <- sprintf("blocks:%d:%s", k, paste(run, collapse = "+"))
      cd <- safe(.ar_pdf_attempt(ctx_b, base, mopts, src, model = bm), NULL)
      if (is.null(cd)) next
      cd$block_run <- run
      if (isTRUE(cd$passed)) passed[[length(passed) + 1L]] <- cd
      if (is.null(best) || length(cd$failing %||% 99) < length(best$failing %||% 99)) best <- cd
    }
    if (is.null(best)) {
      # Nothing could be read of it: kept, so a table printing its own balances is
      # still weighed below (its rows' dates unknown, so taken as in the period).
      res[[length(res) + 1L]] <- list(k = k, core = core, run = mem, size = unname(tot[k]), in_period = 0,
        unread = TRUE, best = NULL, passed = list(), unique = FALSE, title = blocks[[min(mem)]]$title,
        accts = unique(unlist(lapply(blocks[mem], `[[`, "accts"))), foreign = unique(unlist(lapply(blocks[mem], `[[`, "foreign"))))
      next
    }
    shown <- if (length(passed)) passed[[1]] else best
    run1 <- if (length(passed)) passed[[1]]$block_run else mem
    res[[length(res) + 1L]] <- list(k = k, core = core, run = run1, size = unname(tot[k]),
      in_period = .ar_in_period_rows(shown$tx, ctx$md),
      best = best, passed = passed, unique = length(unique(vapply(passed, key, ""))) == 1L,
      title = blocks[[min(run1)]]$title,
      accts = unique(unlist(lapply(blocks[run1], `[[`, "accts"))),
      foreign = unique(unlist(lapply(blocks[run1], `[[`, "foreign"))))
  }
  # A cluster printing its own balances that was not read (past the limit) is still
  # weighed below, as unread.
  for (k in setdiff(which(has_ends), todo)) {
    mem <- which(cl == k)
    res[[length(res) + 1L]] <- list(k = k, core = mem[which.max(ns[mem])], run = mem, size = unname(tot[k]), in_period = 0,
      unread = TRUE, best = NULL, passed = list(), unique = FALSE, title = blocks[[min(mem)]]$title,
      accts = unique(unlist(lapply(blocks[mem], `[[`, "accts"))), foreign = unique(unlist(lapply(blocks[mem], `[[`, "foreign"))))
  }
  if (!any(vapply(res, function(r) !isTRUE(r$unread), logical(1)))) return(NULL)
  # The statement's own table: the most rows dated inside the statement period (a
  # loan schedule runs into the future, a rate history into the past), then the
  # largest. A tie is not decided.
  ip <- vapply(res, function(r) r$in_period, 0); sz <- vapply(res, function(r) r$size, 0)
  o <- order(.ar_flags(res, "unread"), -ip, -sz)
  res <- res[o]
  main <- res[[1]]
  tie <- length(res) > 1L && !isTRUE(res[[2]]$unread) && ip[o[2]] == ip[o[1]] && sz[o[2]] == sz[o[1]]
  others <- list()
  for (r in res[-1]) if (length(r$passed) && r$unique) {
    cd <- r$passed[[1]]
    others[[length(others) + 1L]] <- list(account = if (length(r$accts)) r$accts[1] else NA_character_,
      title = r$title, rows = nrow(cd$tx), tx = cd$tx, proof = cd$proof, why = cd$why,
      pages = sort(unique(cd$proof$pages_used %||% integer(0))))
  }
  # The file may be ONE statement of several accounts, each under its own title
  # with its own opening and closing balance, printed one after another in the
  # same table design: then every account's rows are the statement's. So a table
  # of another account's transactions -- its own opening or closing printed, or a
  # balance that holds on its own, with rows inside the statement period -- that is
  # printed like the statement's own table (in its columns, or under its heading
  # row) sends the file to a person. One in a design of its own (a linked
  # account's activity box, a term deposit's or a loan's table) is another account,
  # read apart; the statement's own opening and closing balances have already
  # shown that none of the statement's rows is in it (tables_set_aside).
  core_b <- blocks[[main$core]]
  account_like <- Filter(function(r) r$k != main$k && (has_ends[r$k] || length(r$passed) > 0L) &&
                           (r$in_period > 0 || isTRUE(r$unread)), res)
  section <- NULL
  for (r in account_like) {
    mem <- which(cl == r$k)
    if (any(vapply(mem, function(j) .ar_block_look_alike(core_b, blocks[[j]]), logical(1)))) {
      section <- list(page = blocks[[min(mem)]]$page); break
    }
  }
  pick <- NULL; why <- NULL
  if (length(main$passed) && main$unique && !tie) {
    if (length(main$foreign)) {
      why <- "The table that proves is titled with another account's number; which table is this statement's own is not clear."
    } else if (!is.null(section)) {
      why <- sprintf(paste("Page %d prints another table of transactions with its own balances, printed like this",
                           "statement's own, so the file may be one statement of several accounts; whether those rows",
                           "belong to this statement is not clear."), section$page)
    } else {
      pick <- main$passed[[1]]
      pick$why <- paste(pick$why, sprintf("Read on its own: %d other table(s) in the file were set aside.",
                                          length(unique(cl)) - 1L + length(figb)))
    }
  } else if (length(main$passed) && !main$unique) {
    why <- "The statement's table reads more than one way when the other tables are set aside."
  } else if (length(main$passed) && tie) why <- "Two tables of the same size each look like the statement's own."
  list(pick = pick, others = if (is.null(pick)) list() else others, why = why,
       n_blocks = length(blocks), n_clusters = length(tot))
}
