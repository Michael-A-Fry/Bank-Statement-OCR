# auto_read_tabular.R -- the automatic reader for CSV and Excel exports (spec
# section 4 and Appendix A1): the same prove-by-arithmetic as a PDF, on a grid of
# cells instead of word boxes.
#
# Columns are mapped by CONTENT: the column whose cells read as dates is the date,
# columns whose every cell reads as a figure are money, a column of nothing but
# D / C / DR / CR words is the sign of the money beside it, and the rest is text.
# Headings only vote, the way they do on a PDF. Which money column is money out,
# money in, a signed amount or the balance is then decided by the arithmetic
# (R/auto_read_prove.R), never by a heading.
#
# The rows themselves go through the engine's own parse_statement(), so a CSV read
# here gives the transactions a hand-made template would.

# A figure in a spreadsheet cell: optional currency, sign or brackets, digits with
# optional thousands groups, ANY number of decimals (a workbook stores 140 and
# 9543.450000000001), and an optional CR / DR / OD or trailing minus.
.AR_TAB_MONEY_RX <- paste0("^[(]?[-+]?[$]?[-+]?(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:[.][0-9]+)?[)]?",
                           "(?:\\s*(?:[CcDd][Rr]|[Oo][Dd]))?-?$")
# A whole-day Excel serial between 2000-01-01 and 2041-01-01.
.AR_SERIAL_RX <- "^[0-9]{5}(?:[.]0+)?$"
.AR_SERIAL_LO <- 36526
.AR_SERIAL_HI <- 51502

# .ar_tab_grid(input) -- the file as a grid of character cells (row 1 = the first
# printed row) plus the delimiter used. NULL when nothing tabular is in it.
.ar_tab_grid <- function(input) {
  if (identical(input$kind, "excel")) {
    tb <- input$table
    if (is.null(tb) || !ncol(tb)) return(NULL)
    h <- names(tb)
    # readxl's placeholder names for blank header cells are not print.
    h[grepl("^[.]{3}[0-9]+$", h)] <- ""
    m <- rbind(h, as.matrix(tb))
    m[is.na(m)] <- ""
    pre <- input$meta$preamble %||% character(0)
    if (length(pre)) {
      pm <- matrix("", length(pre), ncol(m)); pm[, 1] <- pre
      m <- rbind(pm, m)
    }
    dimnames(m) <- NULL
    return(list(cells = trimws(m), delim = NA_character_))
  }
  lines <- input$lines %||% character(0)
  if (!length(lines)) return(NULL)
  lines[1] <- sub("^\\ufeff", "", lines[1])
  idx <- which(nzchar(trimws(lines)))
  if (!length(idx)) return(NULL)
  # The delimiter: the one that splits the most lines into the same number (>= 3)
  # of fields.
  best <- NULL
  bare <- gsub('"[^"]*"', "", lines[idx])
  for (d in c(",", ";", "\t", "|")) {
    nf <- lengths(regmatches(bare, gregexpr(d, bare, fixed = TRUE))) + 1L
    tab <- table(nf[nf >= 3L])
    if (!length(tab)) next
    sc <- max(tab)
    if (is.null(best) || sc > best$score) best <- list(delim = d, score = sc)
  }
  if (is.null(best)) return(NULL)
  recs <- .split_records(lines[idx], idx)
  fl <- lapply(recs, function(r) trimws(.record_fields(r$text, best$delim)))
  k <- max(lengths(fl))
  m <- t(vapply(fl, function(v) c(v, rep("", k - length(v))), character(k)))
  if (k == 1L) m <- matrix(m, ncol = 1L)
  m[is.na(m)] <- ""
  list(cells = m, delim = best$delim, lines = vapply(recs, function(r) r$lines[1], 0L))
}

# .ar_tab_types(m, fmts) -- per cell: "date", "serial", "money", "marker" or "text"
# ("" when blank), with the date formats each date cell parses under.
.ar_tab_types <- function(m, fmts) {
  v <- as.vector(m)
  ty <- rep("text", length(v)); ty[!nzchar(v)] <- ""
  df <- .ar_date_fmts(v, fmts)
  money <- grepl(.AR_TAB_MONEY_RX, v, perl = TRUE) & grepl("[0-9]", v)
  serial <- grepl(.AR_SERIAL_RX, v, perl = TRUE)
  sv <- suppressWarnings(as.numeric(v))
  serial <- serial & !is.na(sv) & sv >= .AR_SERIAL_LO & sv < .AR_SERIAL_HI
  ty[money] <- "money"
  ty[serial] <- "serial"
  ty[nzchar(df)] <- "date"
  ty[toupper(v) %in% type_dc_domain()] <- "marker"
  list(type = matrix(ty, nrow(m)), fmts = matrix(df, nrow(m)))
}

# .ar_tab_table(g) -- the transaction table inside the grid: the date column (the
# column most rows date), the run of rows from the first dated row to the last, the
# heading row above it, and each column's kind measured on those rows.
.ar_tab_table <- function(g, fmts) {
  m <- g$cells
  ty <- .ar_tab_types(m, fmts)
  T <- ty$type
  datey <- T == "date" | T == "serial"
  # A row is a candidate row when it has a date and a figure in another column.
  has_money <- T == "money" | T == "serial"
  score <- vapply(seq_len(ncol(m)), function(j) {
    other <- if (ncol(m) > 1L) rowSums(has_money[, -j, drop = FALSE]) > 0 else rep(FALSE, nrow(m))
    sum(datey[, j] & other)
  }, 0)
  if (max(score) < 1) return(NULL)
  dj <- which.max(score)
  other <- if (ncol(m) > 1L) rowSums(has_money[, -dj, drop = FALSE]) > 0 else rep(FALSE, nrow(m))
  drow <- which(datey[, dj] & other)
  r0 <- min(drow); r1 <- max(drow)
  # Rows after the last dated row that carry a figure and name a balance or total
  # (a closing line printed without a date) belong to the table too.
  lab <- vapply(seq_len(nrow(m)), function(i) {
    txt <- m[i, T[i, ] == "text"]
    .ar_norm_label(paste(txt, collapse = " "))
  }, "")
  acls <- .ar_anchor_class(lab)
  while (r1 < nrow(m) && nzchar(acls[r1 + 1L]) && any(has_money[r1 + 1L, ])) r1 <- r1 + 1L
  while (r0 > 1L && nzchar(acls[r0 - 1L]) && any(has_money[r0 - 1L, ])) r0 <- r0 - 1L
  body <- seq(r0, r1)
  # The heading row: the nearest row above the table with at least two cells and
  # no date in the date column.
  hr <- NA_integer_
  for (i in rev(seq_len(r0 - 1L))) {
    if (sum(nzchar(m[i, ])) >= 2L && !datey[i, dj]) { hr <- i; break }
    if (sum(nzchar(m[i, ])) >= 1L) break
  }
  heads <- if (!is.na(hr)) m[hr, ] else rep("", ncol(m))
  list(m = m, T = T, F = ty$fmts, dj = dj, body = body, hr = hr, heads = heads,
       acls = acls, lab = lab, pre = if (r0 > 1L) seq_len(r0 - 1L) else integer(0),
       post = if (r1 < nrow(m)) seq(r1 + 1L, nrow(m)) else integer(0))
}

# .ar_tab_columns(tt) -- each column's kind on the transaction rows (anchor rows
# aside): "date", "date2", "money", "marker", "text" or "empty".
.ar_tab_columns <- function(tt, rows) {
  T <- tt$T[rows, , drop = FALSE]
  kind <- vapply(seq_len(ncol(T)), function(j) {
    t <- T[, j]; t <- t[nzchar(t)]
    if (!length(t)) return("empty")
    if (j == tt$dj) return("date")
    if (all(t %in% c("date", "serial")) && mean(nzchar(T[, j])) >= 0.9) return("date2")
    if (all(t %in% c("money", "serial"))) return("money")
    if (all(t == "marker")) return("marker")
    "text"
  }, "")
  kind
}

# .ar_tab_date(tt, rows, md) -- the date column read under ONE format: the formats
# every dated cell parses under; when several give different dates, the one whose
# dates run in order and inside the printed period. Returns the ISO dates, the
# format, and whether another format would read the column differently and
# equally well (then the dates are not settled).
.ar_tab_date <- function(tt, rows, md, j = tt$dj) {
  v <- tt$m[rows, j]
  T <- tt$T[rows, j]
  has <- nzchar(v)
  if (all(T[has] == "serial")) {
    iso <- ifelse(has, format(as.Date(floor(as.numeric(ifelse(has, v, NA))), origin = "1899-12-30"), "%Y-%m-%d"), NA_character_)
    return(list(iso = iso, fmt = "excel_serial", ambiguous = FALSE))
  }
  fl <- strsplit(tt$F[rows, j][has], "|", fixed = TRUE)
  cand <- if (length(fl)) Reduce(intersect, fl) else character(0)
  if (!length(cand)) {
    tab <- sort(table(unlist(fl)), decreasing = TRUE)
    cand <- if (length(tab)) names(tab)[1] else character(0)
  }
  if (!length(cand)) return(list(iso = rep(NA_character_, length(v)), fmt = NA_character_, ambiguous = FALSE))
  p0 <- .plausible_period_date(md$period_start); p1 <- .plausible_period_date(md$period_end)
  reads <- lapply(cand, function(f) {
    iso <- rep(NA_character_, length(v))
    iso[has] <- parse_date(v[has], f)$iso
    d <- as.Date(iso[has])
    ord <- !anyNA(d) && (all(diff(d) >= 0) || all(diff(d) <= 0))
    inp <- !is.na(p0) && !is.na(p1) && !anyNA(d) && all(d >= p0 & d <= p1)
    list(iso = iso, fmt = f, score = 2 * inp + ord + !anyNA(d))
  })
  sc <- vapply(reads, `[[`, 0, "score")
  top <- reads[sc == max(sc)]
  keys <- unique(vapply(top, function(r) paste(r$iso, collapse = "|"), ""))
  list(iso = top[[1]]$iso, fmt = top[[1]]$fmt, ambiguous = length(keys) > 1L)
}

# .ar_tab_anchors(tt, rows, mcols, delim) -- opening / closing / total figures:
# rows of the table that name one, and "label: figure" lines above or below it.
.ar_tab_anchors <- function(tt, body_rows, mcols) {
  out <- list(); nb <- 0L
  for (i in tt$body) {
    if (!(i %in% body_rows)) {
      if (!nzchar(tt$acls[i])) next
      figs <- vapply(mcols, function(j) if (nzchar(tt$m[i, j])) tt$m[i, j] else NA_character_, "")
      if (all(is.na(figs))) next
      out[[length(out) + 1L]] <- list(page = 1L, line = i, y = i, label = tt$lab[i], class = tt$acls[i],
        raw = paste(tt$m[i, ], collapse = " "), in_table = TRUE, before_rows = nb,
        value_text = utils::tail(figs[!is.na(figs)], 1), n_money = sum(!is.na(figs)), figs = figs)
    } else nb <- nb + 1L
  }
  # Outside the table: each "label  figure" pair on a line.
  for (i in c(tt$pre, tt$post)) {
    cells <- tt$m[i, nzchar(tt$m[i, ])]
    s <- paste(cells, collapse = " ")
    hit <- gregexpr("[-(]?[$]?[0-9][0-9,]*[.][0-9]{2}[)]?(?:\\s*(?:CR|DR|OD))?", s, perl = TRUE)[[1]]
    if (hit[1] < 0) next
    for (k in seq_along(hit)) {
      lab <- substr(s, if (k > 1L) hit[k - 1L] + attr(hit, "match.length")[k - 1L] else 1L, hit[k] - 1L)
      cl <- .ar_anchor_class(.ar_norm_label(gsub("[:=]", " ", lab)))
      if (!nzchar(cl)) next
      fig <- substr(s, hit[k], hit[k] + attr(hit, "match.length")[k] - 1L)
      out[[length(out) + 1L]] <- list(page = 1L, line = i, y = i, label = .ar_norm_label(lab), class = cl,
        raw = s, in_table = FALSE, before_rows = if (i %in% tt$pre) 0L else nb, value_text = fig,
        n_money = 1L, figs = rep(NA_character_, length(mcols)))
    }
  }
  out
}

# .ar_read_tabular(input, layouts, bank, opts) -- the content reading, then each
# spreadsheet layout handed in; the same outcome rules as a PDF.
.ar_read_tabular <- function(input, layouts, bank, opts) {
  g <- .ar_tab_grid(input)
  if (is.null(g) || !nrow(g$cells)) return(.ar_unread("The file holds no table of rows and columns."))
  fmts <- .ar_date_formats(tabular = TRUE)
  tt <- .ar_tab_table(g, fmts)
  if (is.null(tt)) return(.ar_unread("No row of the file has a date with a figure beside it, so no transaction table was found."))
  pre_text <- apply(tt$m[c(tt$pre, tt$post), , drop = FALSE], 1, function(r) paste(r[nzchar(r)], collapse = " "))
  md <- safe(extract_metadata(list(kind = input$kind, pages = pre_text[nzchar(pre_text)], meta = list(),
                                   path = input$path)), NULL)
  ctx <- list(input = input, g = g, tt = tt, md = md, bank = bank,
              liab = .ar_liability_evidence(pre_text), decimal = "auto", roles = opts$roles)
  cands <- list()
  cands[["content"]] <- .ar_tab_attempt(ctx, "content")
  # A person's roles are read on their own: no layout stands in for them.
  if (!is.null(ctx$roles)) return(.ar_decide(cands, list(), ctx))
  seen <- if (identical(cands$content$basis, "arithmetic")) .ar_conv_key(cands$content$rd) else ""
  for (ly in layouts) {
    info <- .ar_layout_info(ly)
    if (is.null(info) || !(info$kind %in% c("delimited", "excel"))) next
    src <- paste0("layout:", info$ref)
    if (!is.null(cands[[src]])) next
    fig <- info$roles[info$roles %in% c("debit", "credit", "amount", "balance", "other")]
    if (!.ar_layout_near(cands$content$signature, info$sig)) next
    ck <- .ar_conv_key(list(roles = fig, conv = info$conv, liab = info$liab, dir = info$dir))
    if (ck %in% seen) next
    seen <- c(seen, ck)
    cands[[src]] <- .ar_tab_attempt(ctx, src, forced = info)
    if (sum(startsWith(names(cands), "layout:")) >= .AR_MAX_LAYOUTS) break
  }
  .ar_decide(cands, layouts, ctx)
}

# .ar_tab_attempt(ctx, source, forced) -- one candidate reading of the grid.
.ar_tab_attempt <- function(ctx, source, forced = NULL) {
  fail <- function(why) list(source = source, passed = FALSE, status = "unread", why = why)
  tt <- ctx$tt
  # Transaction rows: the table's rows that are not an opening / closing / total.
  anchor_row <- vapply(tt$body, function(i) nzchar(tt$acls[i]) &&
    any(tt$T[i, -tt$dj] %in% c("money", "serial")), logical(1))
  rows <- tt$body[!anchor_row & vapply(tt$body, function(i) any(nzchar(tt$m[i, ])), logical(1))]
  if (!length(rows)) return(fail("The table has no transaction rows."))
  kind <- .ar_tab_columns(tt, rows)
  mcols <- which(kind == "money")
  if (!length(mcols)) return(fail("No column of the table holds figures."))
  # A column of D / C words signs the figures next to it: glue it on for the
  # arithmetic ("403.47" + "DR").
  cells <- tt$m[rows, mcols, drop = FALSE]
  cells[!nzchar(cells)] <- NA_character_
  mk <- which(kind == "marker")
  mk_for <- NA_integer_
  if (length(mk) == 1L) {
    left <- mcols[mcols < mk]; right <- mcols[mcols > mk]
    mk_for <- if (length(left)) max(left) else if (length(right)) min(right) else NA_integer_
    if (!is.na(mk_for)) {
      k <- match(mk_for, mcols)
      tok <- toupper(tt$m[rows, mk])
      dset <- toupper(lex("debit_markers")); cset <- toupper(lex("credit_markers"))
      suf <- ifelse(tok %in% dset, " DR", ifelse(tok %in% cset, " CR", ""))
      cells[, k] <- ifelse(is.na(cells[, k]), NA_character_, paste0(sub("^-", "", cells[, k]), suf))
    }
  }
  V <- .ar_values(cells, ctx$decimal)
  V$S <- round(V$S, 2); V$M <- round(V$M, 2)
  anchors <- .ar_tab_anchors(tt, rows, mcols)
  hroles <- vapply(mcols, function(j) safe(.wa_money_role(tt$heads[j]), NA_character_), "")
  if (!is.null(forced)) {
    rl <- .ar_forced_reading(V, anchors, forced, ctx$decimal)
    if (is.null(rl$chosen)) return(fail(rl$why))
  } else {
    if (!is.null(ctx$roles) && length(ctx$roles) != length(mcols))
      return(fail(sprintf("The roles given are for %d column(s) of figures; this file has %d.", length(ctx$roles), length(mcols))))
    rl <- .ar_roles(V, anchors, ctx$liab, ctx$decimal, hroles,
                    texts = if (any(kind == "text")) apply(tt$m[rows, kind == "text", drop = FALSE], 1, paste, collapse = " "),
                    only = ctx$roles)
  }
  rd <- rl$chosen
  basis <- if (!is.null(rd)) "arithmetic" else "none"
  if (is.null(rd) && rl$n_distinct > 1L) {
    agree <- vapply(rl$distinct, function(r) sum(!is.na(hroles) & hroles == r$roles) +
                      0.5 * identical(isTRUE(r$liab), isTRUE(ctx$liab$liability)), 0)
    rd <- rl$distinct[[which.max(agree)]]; basis <- "ambiguous"
  }
  if (is.null(rd) && !is.null(rl$best)) { rd <- rl$best; basis <- "broken" }
  if (is.null(rd)) {
    vr <- .ar_vote_roles(length(mcols), hroles)
    if (is.null(vr)) return(fail("Found the table but could not tell which column of figures is which."))
    am <- .ar_amounts(V, vr$roles, "S", isTRUE(ctx$liab$liability))
    b <- which(vr$roles == "balance")
    rd <- list(roles = vr$roles, conv = "S", liab = isTRUE(ctx$liab$liability), dir = "old",
               A = am$A, b = if (length(b)) b else 0L, chain = NULL,
               score = c(links = 0, held = 0, failed = 0, unknown = 0, ambiguous = 0))
    basis <- vr$by
  }
  dt <- .ar_tab_date(tt, rows, ctx$md)
  tpl <- .ar_tab_template(ctx, tt, rows, kind, mcols, mk, mk_for, rd, dt)
  parsed <- .ar_tab_parse(ctx, tt, rows, kind, mcols, dt, tpl)
  if (is.null(parsed)) return(fail("The table reader could not read the rows."))
  tx <- .ar_settle(parsed$transactions, rd, cells, ctx$decimal, anchors, aligned = TRUE)
  parsed$transactions <- tx
  op <- .ar_opening_value(anchors, rd, ctx$decimal, nrow(tx)); cl <- .ar_closing_value(anchors, rd, ctx$decimal, nrow(tx))
  if (!is.na(op)) parsed$header$opening_balance <- op
  if (!is.na(cl)) parsed$header$closing_balance <- cl
  ckl <- .ar_arith_checks(tx, rep(1L, nrow(tx)), anchors, rd, rl, basis, ctx$decimal, ctx$md,
                          two_dates = any(kind == "date2"))
  # The reader's figures must be the reading's: a figure the arithmetic proved and
  # the table reader then read differently is not proven.
  same <- isTRUE(all.equal(unname(round(tx$amount, 2)), unname(round(rd$A, 2))))
  ckl$checks$reader_agrees <- list(ok = same, why = if (same) "The table reader's figures are the ones the arithmetic proved."
                                   else "The table reader read some figures differently from the arithmetic.")
  ckl$checks$dates_settled <- list(ok = !dt$ambiguous, why = if (!dt$ambiguous) "The dates read one way only."
                                   else "The dates read as day-month and as month-day equally well.")
  # A row under the heading but before the first dated row, or after the last,
  # that prints a figure where the figures run and names no balance or total is a
  # transaction whose date did not read: never left out quietly.
  edge <- c(if (!is.na(tt$hr)) tt$pre[tt$pre > tt$hr], tt$post)
  lost <- edge[vapply(edge, function(i) !nzchar(tt$acls[i]) && any(tt$T[i, mcols] %in% c("money", "serial")), logical(1))]
  ckl$checks$lines_accounted <- list(ok = !length(lost), why = if (!length(lost)) "Every row with a figure is a transaction or a summary row."
    else sprintf("Row %d of the file prints a figure but no date, so it is not part of the table.", lost[1]))
  cd <- .ar_tab_candidate(source, tpl, parsed, ckl, rd, rl, basis, tt, kind, ctx)
  cd$hroles <- hroles
  cd
}

# .ar_tab_template(...) -- the candidate as a template list in today's schema.
.ar_tab_template <- function(ctx, tt, rows, kind, mcols, mk, mk_for, rd, dt) {
  hn <- .ar_tab_names(tt)
  cols <- list(date = list(source = hn[tt$dj], format = if (identical(dt$fmt, "excel_serial")) "%Y-%m-%d" else dt$fmt))
  role_of <- stats::setNames(rd$roles, mcols)
  for (j in mcols) {
    r <- role_of[[as.character(j)]]
    if (r %in% c("amount", "debit", "credit", "balance")) cols[[r]] <- list(source = hn[j])
  }
  txt <- which(kind == "text")
  extras <- list(); fields <- stats::setNames(rep(NA_character_, ncol(tt$m)), seq_len(ncol(tt$m)))
  if (length(txt)) {
    chars <- vapply(txt, function(j) sum(nchar(tt$m[rows, j])), 0)
    desc <- txt[which.max(chars)]
    cols$description <- list(source = hn[desc]); fields[as.character(desc)] <- "description"
    k <- 0L
    for (j in setdiff(txt, desc)) {
      hd <- tolower(tt$heads[j])
      nm <- if (grepl("particular", hd)) "particulars" else if (grepl("\\bcode\\b", hd)) "code"
            else if (grepl("reference|\\bref\\b", hd)) "reference" else if (grepl("\\btype\\b", hd)) "type"
            else if (grepl("payee|other party|\\bparty\\b", hd)) "other_party" else ""
      if (!nzchar(nm) || !is.null(cols[[nm]]) || (nm == "type" && length(mk))) { k <- k + 1L; nm <- paste0("text", k) }
      if (startsWith(nm, "text")) extras[[nm]] <- list(source = hn[j]) else cols[[nm]] <- list(source = hn[j])
      fields[as.character(j)] <- nm
    }
  }
  for (j in which(kind == "date2")) { extras$date2 <- list(source = hn[j]); fields[as.character(j)] <- "date2" }
  oth <- 0L
  for (j in mcols) {
    r <- role_of[[as.character(j)]]
    if (r == "other") { oth <- oth + 1L; nm <- paste0("other", oth); extras[[nm]] <- list(source = hn[j]); fields[as.character(j)] <- nm }
    else fields[as.character(j)] <- r
  }
  fields[as.character(tt$dj)] <- "date"
  style <- if (any(rd$roles %in% c("debit", "credit"))) "debit_credit_cols"
           else if (length(mk) == 1L && !is.na(mk_for) && identical(role_of[[as.character(mk_for)]], "amount")) "type_dc"
           else if (identical(rd$conv, "U")) "unsigned" else "signed"
  tpl <- list(id = NA_character_, bank = ctx$bank %||% NA_character_, statement_type = NA_character_,
              format = ctx$input$kind, version = 1L, currency = "NZD", columns = cols,
              amount_sign = style, decimal_mark = ctx$decimal, unsigned_default = "debit",
              fingerprint = list(header_contains_all = as.list(unname(hn[nzchar(tt$heads)]))),
              extras = if (length(extras)) extras else NULL)
  if (!is.na(ctx$g$delim)) tpl$delimiter <- ctx$g$delim
  if (style == "type_dc") {
    cols$type <- list(source = hn[mk]); tpl$columns <- cols
    tok <- unique(toupper(tt$m[rows, mk])); tok <- tok[nzchar(tok)]
    dset <- toupper(lex("debit_markers")); cset <- toupper(lex("credit_markers"))
    if (any(tok %in% dset)) tpl$type_debit_value <- tok[tok %in% dset][1]
    if (any(tok %in% cset)) tpl$type_credit_value <- tok[tok %in% cset][1]
  }
  if (style == "type_dc") fields[as.character(mk)] <- "type"
  tpl$auto <- list(roles = rd$roles, conv = rd$conv, liab = isTRUE(rd$liab), dir = rd$dir, engine = AUTO_READ_VERSION)
  roles_sig <- unname(fields[!is.na(fields)])
  roles_sig[startsWith(roles_sig, "other")] <- "other"
  sk <- sort(intersect(unique(as.vector(.ar_values(tt$m[rows, mcols, drop = FALSE])$SK)),
                       c("CR", "DR", "OD", "()", "-lead", "-trail", "+")))
  bcol <- which(rd$roles == "balance")
  nb <- if (length(bcol)) sum(nzchar(tt$m[rows, mcols[bcol]])) else 0L
  tpl$signature <- list(kind = ctx$input$kind, roles = roles_sig,
    date_format = cols$date$format,
    money_style = if (style == "type_dc") "type_dc" else if (style == "debit_credit_cols") "debit_credit_cols"
                  else if (rd$conv %in% c("U", "B")) "unsigned" else if (any(sk %in% c("CR", "DR"))) "dr_cr_suffix" else "signed",
    sign_markers = sk, balance_freq = if (!length(bcol)) "none" else if (nb == length(rows)) "every" else if (nb) "some" else "none",
    newest_first = identical(rd$dir, "new"),
    heading_tokens = utils::head(sort(unique(tolower(unlist(regmatches(tt$heads, gregexpr("[A-Za-z]{2,}", tt$heads)))))), 40),
    producer = "", rel_x = round((seq_len(sum(!is.na(fields))) ) / max(1, sum(!is.na(fields))), 2),
    extras = names(extras %||% list()))
  tpl$id <- paste0("auto_", .ar_hash(paste(unlist(tpl$signature[c("kind", "roles", "date_format", "money_style", "newest_first")]), collapse = "|")))
  tpl$auto$fields <- unname(fields)
  tpl
}

# .ar_tab_names(tt) -- one unique, non-empty name per column (the heading, or
# "col<j>"), the way the template refers to it.
.ar_tab_names <- function(tt) {
  h <- tt$heads
  h[!nzchar(h)] <- paste0("col", which(!nzchar(h)))
  make.unique(h)
}

# .ar_tab_parse(...) -- parse_statement() on the table's transaction rows, handed
# over as an already-cut sheet (dates of a serial column as ISO), so the engine's
# own amount styles, extras and metadata apply unchanged.
.ar_tab_parse <- function(ctx, tt, rows, kind, mcols, dt, tpl) {
  hn <- .ar_tab_names(tt)
  df <- as.data.frame(tt$m[rows, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- hn
  if (identical(dt$fmt, "excel_serial")) df[[hn[tt$dj]]] <- ifelse(is.na(dt$iso), "", dt$iso)
  # A spreadsheet stores 9543.45 as 9543.450000000001: the cents are the figure.
  for (j in mcols) {
    v <- df[[hn[j]]]
    long <- grepl("^-?[0-9]+[.][0-9]{3,}$", v)
    if (any(long)) df[[hn[j]]][long] <- sprintf("%.2f", round(as.numeric(v[long]), 2))
  }
  pre <- apply(tt$m[tt$pre, , drop = FALSE], 1, function(r) paste(r[nzchar(r)], collapse = " "))
  shim <- list(kind = "excel", path = ctx$input$path %||% "", sha256 = ctx$input$sha256 %||% NA_character_,
               table = df, meta = list(preamble = if (length(pre)) pre else character(0)))
  tp <- tpl; tp$format <- "excel"
  p <- safe(parse_statement(shim, tp), NULL)
  if (is.null(p)) return(NULL)
  if (!is.null(ctx$md)) for (k in c("period_start", "period_end", "account_number", "account_name"))
    if (is.na(p$header[[k]] %||% NA) && !is.null(ctx$md[[k]])) p$header[[k]] <- ctx$md[[k]]
  src <- if (!is.null(ctx$g$lines)) sprintf("csv:line=%d", ctx$g$lines[rows]) else sprintf("xlsx:row=%d", rows)
  p$provenance$source_ref <- src
  p
}

# .ar_tab_candidate(...) -- the candidate in the shape .ar_decide() works on.
.ar_tab_candidate <- function(source, tpl, parsed, ckl, rd, rl, basis, tt, kind, ctx) {
  ck <- ckl$checks
  oks <- vapply(ck, function(x) x$ok, NA)
  failing <- names(oks)[oks %in% FALSE]
  passed <- !length(failing) && ckl$proof_kind != "none"
  sc <- ckl$score %||% c(links = 0, held = 0)
  tx <- parsed$transactions
  fields <- tpl$auto$fields
  cols_df <- data.frame(page = 1L, field = ifelse(is.na(fields), "", fields),
    kind = ifelse(kind %in% c("date", "date2"), "date", ifelse(kind == "money", "money", "text")),
    x_min = seq_along(kind), x_max = seq_along(kind), ink_min = seq_along(kind), ink_max = seq_along(kind),
    heading = tt$heads, stringsAsFactors = FALSE)
  cols_df <- cols_df[nzchar(cols_df$field), , drop = FALSE]
  rownames(cols_df) <- NULL
  proof <- list(kind = ckl$proof_kind, links = as.integer(sc[["links"]]), held = as.integer(sc[["held"]]),
                unique = identical(basis, "arithmetic"), pages_with_rows = if (nrow(tx)) 1L else integer(0),
                pages_used = 1L, derived = 0L,
                one_row = length(ckl$chain$one_row %||% integer(0)),
                rows_covered = length(ckl$chain$covered %||% integer(0)),
                direction_note = rl$note %||% character(0))
  why <- if (passed) .ar_proven_why(proof, rd) else if (length(failing)) ck[[failing[1]]]$why
         else "Nothing in the file adds up to prove the reading."
  list(source = source, passed = passed, failing = failing, why = why, template = tpl,
       parsed = parsed, tx = tx, checks = .ar_checks_df(ck), proof = proof,
       columns = cols_df, rd = rd, basis = basis, signature = tpl$signature,
       no_balance = rd$b == 0L, notes = rl$note %||% character(0))
}
