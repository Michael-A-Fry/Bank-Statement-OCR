# auto_read.R -- the automatic reader (spec, Appendix A1): read a statement from
# its CONTENT, prove the reading with the statement's own arithmetic, and say
# plainly when it cannot.
#
#   auto_read(input, layouts = list(), bank = NULL, opts = list()) -> a reading
#
# Geometry proposes (R/auto_read_pdf.R), arithmetic decides (R/auto_read_prove.R),
# and the engine's own table reader assembles the rows: parse_pdf_table() for a
# PDF and parse_statement() for a spreadsheet, so continuation lines, split rows,
# summary lines, the year of a year-less date and amounts filled from the balance
# all behave exactly as they do for a hand-made template. Every figure that comes
# out is then checked again, on what the table reader actually produced.
#
# Outcomes (section 4):
#   proven        every hard check passed and exactly one reading passes
#   layout_match  no running balance and no printed totals, but the reading is a
#                 proven layout handed in, and nothing contradicts it
#   check         read, not proven -- the reason is in `why`
#   unread        nothing usable -- the reason is in `why`
#
# Never throws: an error becomes "unread" with the reason. Deterministic: the same
# input and layouts give the same reading.

AUTO_READ_VERSION <- "1.0.0"

auto_read <- function(input, layouts = list(), bank = NULL, opts = list()) {
  t0 <- proc.time()[["elapsed"]]
  rd <- tryCatch(.ar_read(input, layouts %||% list(), bank, opts %||% list()),
    error = function(e) .ar_unread(paste0("The reader stopped on this file (", conditionMessage(e), ").")))
  rd$secs <- round(proc.time()[["elapsed"]] - t0, 3)
  rd$engine <- AUTO_READ_VERSION
  rd
}

.ar_read <- function(input, layouts, bank, opts) {
  kind <- input$kind %||% NA_character_
  if (identical(kind, "pdf")) return(.ar_read_pdf(input, layouts, bank, opts))
  if (kind %in% c("delimited", "excel")) return(.ar_read_tabular(input, layouts, bank, opts))
  .ar_unread("The file is not a PDF, CSV or Excel statement.")
}

# ---- the reading object -----------------------------------------------------------------

.ar_empty_tx <- function() data.frame(row_id = integer(0), date = character(0), amount = numeric(0),
                                      stringsAsFactors = FALSE)

.ar_checks_df <- function(ck) {
  if (!length(ck)) return(data.frame(check = character(0), ok = logical(0), why = character(0)))
  data.frame(check = names(ck), ok = vapply(ck, function(x) x$ok, NA),
             why = vapply(ck, function(x) x$why, ""), stringsAsFactors = FALSE, row.names = NULL)
}

.ar_unread <- function(why, candidates = NULL, checks = NULL) {
  list(outcome = "unread", why = why, template = NULL, parsed = NULL, recon = NULL,
       transactions = .ar_empty_tx(),
       proof = list(kind = "none", links = 0L, held = 0L, unique = FALSE, pages_with_rows = integer(0),
                    pages_used = integer(0), derived = 0L),
       checks = checks %||% .ar_checks_df(list()),
       candidates = candidates %||% data.frame(source = character(0), passed = logical(0), why = character(0)),
       columns = data.frame(page = integer(0), field = character(0), kind = character(0),
                            x_min = numeric(0), x_max = numeric(0), ink_min = numeric(0),
                            ink_max = numeric(0), heading = character(0)),
       matched_layout = NULL, notes = character(0))
}

# ---- PDF ------------------------------------------------------------------------------------

# .ar_pdf_context(input) -- what every attempt at reading this PDF shares: the band
# frame (the first page's size), the line tolerance, the statement's metadata, the
# date formats and sign words, the decimal mark the document votes for, and what
# the statement says about itself outside its rows.
.ar_pdf_context <- function(input) {
  wl <- input$words %||% list()
  np <- length(wl)
  pw <- suppressWarnings(as.numeric(input$page_width %||% rep(NA_real_, np)))
  ph <- suppressWarnings(as.numeric(input$page_height %||% rep(NA_real_, np)))
  frame <- list(width = if (length(pw) && isTRUE(pw[1] > 0)) pw[1] else .A4_W,
                height = if (length(ph) && isTRUE(ph[1] > 0)) ph[1] else .A4_H)
  ocr <- as.logical(input$page_ocr %||% rep(FALSE, np)); ocr[is.na(ocr)] <- FALSE
  # OCR word boxes hug the ink, so the tops of one printed line wander with the
  # letters; the line tolerance grows with the type size there.
  row_tol <- PARAM_PDF_ROW_TOL
  if (any(ocr)) {
    hh <- unlist(lapply(wl[ocr], function(w) if (!is.null(w) && nrow(w)) w$height))
    hm <- suppressWarnings(stats::median(hh, na.rm = TRUE))
    if (isTRUE(hm > 0)) row_tol <- max(PARAM_PDF_ROW_TOL, round(0.45 * hm, 1))
  }
  txt <- unlist(lapply(wl, function(w) if (!is.null(w) && nrow(w)) as.character(w$text)))
  core <- txt[grepl(.AR_MONEY_RX, txt, perl = TRUE)]
  ncomma <- sum(grepl(",[0-9]{2}[)]?[-+]?$", core)); ndot <- sum(grepl("[.][0-9]{2}[)]?[-+]?$", core))
  decimal <- if (ncomma > ndot && ncomma >= 3L) "comma" else "auto"
  list(input = input, np = np, pw = pw, ph = ph, frame = frame, ocr = ocr, row_tol = row_tol,
       md = safe(extract_metadata(input), NULL), fmts = .ar_date_formats(), markers = .ar_markers(),
       decimal = decimal, pages_text = input$pages %||% character(0))
}

# .ar_pdf_pages(ctx, thr_mult) -- every page typed and measured (cached per cell
# threshold, since the repair search re-measures with other splits).
.ar_pdf_pages <- function(ctx, thr_mult = 1) {
  wl <- ctx$input$words %||% list()
  pgs <- lapply(seq_len(ctx$np), function(p) {
    pg <- .ar_page(wl[[p]], p, ctx$frame, ctx$pw[p], ctx$ph[p], ctx$row_tol, ctx$fmts, ctx$markers)
    if (is.null(pg)) return(NULL)
    pg$ocr <- isTRUE(ctx$ocr[p])
    if (thr_mult != 1 && nrow(pg$ph)) {
      thr <- pg$thr * thr_mult
      pg$ph$standalone <- (pg$ph$gap_before > thr | is.infinite(pg$ph$gap_before)) &
                          (pg$ph$gap_after > thr | is.infinite(pg$ph$gap_after))
      pg$thr <- thr
    }
    pg
  })
  pgs
}

# .ar_read_pdf(input, layouts, bank, opts) -- the content reading first; then each
# layout handed in, registered to this document; then, only when nothing passes, a
# bounded repair search. The outcome rules are applied to the candidates together.
.ar_read_pdf <- function(input, layouts, bank, opts) {
  wl <- input$words %||% list()
  if (!length(wl) || all(vapply(wl, function(w) is.null(w) || !nrow(w), logical(1)))) {
    why <- if (isTRUE((input$meta$scanned_no_ocr %||% 0L) > 0L))
      "The PDF is a scan and this machine could not read its text (no OCR)."
    else "The PDF has no readable text on any page."
    return(.ar_unread(why))
  }
  ctx <- .ar_pdf_context(input)
  ctx$liab <- .ar_liability_evidence(ctx$pages_text)
  ctx$bank <- bank
  base <- .ar_pdf_pages(ctx)
  cands <- list()
  cands[["content"]] <- .ar_pdf_attempt(ctx, base, list(), "content")
  for (ly in layouts) {
    info <- .ar_layout_info(ly)
    if (is.null(info) || !(info$kind %in% c("pdf", "scan"))) next
    src <- paste0("layout:", info$ref)
    if (!is.null(cands[[src]])) next
    cands[[src]] <- .ar_pdf_attempt(ctx, base, list(), src, forced = info)
  }
  # Repair search: bounded, fixed order, and only when nothing above passed.
  if (!any(vapply(cands, function(cd) isTRUE(cd$passed), logical(1)))) {
    reps <- list(
      "repair:wider_cells"   = list(thr = 1.6),
      "repair:narrower_cells" = list(thr = 0.6),
      "repair:no_page_shift" = list(shift = FALSE))
    for (nm in names(reps)) {
      r <- reps[[nm]]
      pg2 <- if (!is.null(r$thr)) .ar_pdf_pages(ctx, r$thr) else base
      cands[[nm]] <- .ar_pdf_attempt(ctx, pg2, list(shift = r$shift), nm)
    }
  }
  .ar_decide(cands, layouts, ctx)
}

# .ar_pdf_attempt(ctx, pgs, mopts, source, forced) -- one candidate reading, start to
# finish: column model, roles, template, the table reader, and every hard check on
# what it produced. `forced` (a layout) fixes the roles and conventions instead of
# searching for them.
.ar_pdf_attempt <- function(ctx, pgs, mopts, source, forced = NULL) {
  fail <- function(why) list(source = source, passed = FALSE, status = "unread", why = why)
  model <- .ar_model(pgs, mopts)
  if (is.null(model)) return(fail("No line on any page prints a date with a figure beside it, so no transaction table was found."))
  K <- length(model$cols)
  V <- .ar_values(model$cells, ctx$decimal)
  headings <- .ar_headings(model)
  hroles <- vapply(model$cols, function(cl) safe(.wa_money_role(.ar_heading_over(headings, cl$x, cl$x1)), NA_character_), "")
  if (!is.null(forced)) {
    rl <- .ar_forced_reading(V, model$anchors, forced, ctx$decimal)
    if (is.null(rl$chosen)) return(fail(rl$why))
  } else rl <- .ar_roles(V, model$anchors, ctx$liab, ctx$decimal, hroles)
  rd <- rl$chosen
  basis <- if (!is.null(rd)) "arithmetic" else "none"
  if (is.null(rd) && rl$n_distinct > 1L) {
    # Two readings both fit: show the one the headings favour, never call it proven.
    agree <- vapply(rl$distinct, function(r) sum(!is.na(hroles) & hroles == r$roles), 0)
    rd <- rl$distinct[[which.max(agree)]]; basis <- "ambiguous"
  }
  if (is.null(rd) && !is.null(rl$best)) { rd <- rl$best; basis <- "broken" }
  if (is.null(rd)) {
    vr <- .ar_vote_roles(K, hroles)
    if (is.null(vr)) return(fail("Found the table but could not tell which column of figures is which."))
    am <- .ar_amounts(V, vr$roles, "S", isTRUE(ctx$liab$liability))
    b <- which(vr$roles == "balance")
    rd <- list(roles = vr$roles, conv = "S", liab = isTRUE(ctx$liab$liability), dir = "old",
               A = am$A, b = if (length(b)) b else 0L,
               bal = if (length(b)) .ar_sem(V$S[, b], V$SK[, b], isTRUE(ctx$liab$liability)) else rep(NA_real_, nrow(V$S)),
               score = c(links = 0, held = 0, failed = 0, unknown = 0, ambiguous = 0), chain = NULL)
    basis <- vr$by
  }
  cs <- .ar_columns(model, rd$roles, headings)
  if (is.null(cs)) return(fail("Found the table but could not measure its columns."))
  tpl <- .ar_pdf_template(ctx, model, cs, rd, headings)
  parsed <- .ar_parse_pdf(ctx, model, tpl)
  post <- .ar_post_pdf(ctx, model, tpl, rd, parsed)
  ck <- .ar_pdf_checks(ctx, model, cs, tpl, rd, rl, post, basis)
  .ar_candidate(source, tpl, post, ck, rd, rl, basis, model, cs, headings, ctx)
}

# .ar_forced_reading(V, anchors, info, decimal) -- a layout's conventions applied to
# this document's figure columns, matched in order. The layout must have as many
# figure columns as the page shows.
.ar_forced_reading <- function(V, anchors, info, decimal) {
  K <- ncol(V$S)
  fig <- info$roles[info$roles %in% c("debit", "credit", "amount", "balance", "other")]
  if (length(fig) != K)
    return(list(chosen = NULL, why = sprintf("The layout has %d column(s) of figures; this statement shows %d.", length(fig), K)))
  am <- .ar_amounts(V, fig, info$conv, info$liab)
  b <- which(fig == "balance"); b <- if (length(b)) b else 0L
  bal <- if (b > 0L) .ar_sem(V$S[, b], V$SK[, b], info$liab) else rep(NA_real_, nrow(V$S))
  apts <- .ar_anchor_points(anchors, nrow(V$S), info$dir, info$liab, b, decimal)
  ch <- .ar_chain(am$A, am$uns, bal, apts, info$dir)
  A <- if (any(am$uns)) ifelse(am$uns, ch$signs * am$A, am$A) else am$A
  rd <- list(roles = fig, conv = info$conv, liab = info$liab, dir = info$dir, score = .ar_chain_score(ch),
             chain = ch, A = A, bal = bal, b = b, na_rows = sum(is.na(am$A)))
  ok <- rd$score[["failed"]] == 0 && rd$score[["ambiguous"]] == 0
  list(chosen = rd, n_distinct = 1L, distinct = list(rd), best = rd,
       note = character(0), forced_ok = ok)
}

# .ar_pdf_template(ctx, model, cs, rd, headings) -- the candidate as a template list
# in today's schema, so the table reader, reconciliation and outputs work on it
# unchanged. table$columns is the reference page's boxes; table$columns_by_page
# holds every page's own.
.ar_pdf_template <- function(ctx, model, cs, rd, headings) {
  pages <- sort(unique(model$rows$page))
  ref <- as.integer(names(which.max(table(model$rows$page))))
  boxes <- lapply(seq_len(ctx$np), function(p) if (p %in% pages) .ar_boxes(model, cs, p) else NULL)
  to_list <- function(bx) {
    if (is.null(bx)) return(NULL)
    out <- list()
    for (i in seq_len(nrow(bx))) out[[bx$field[i]]] <- list(x_min = bx$x_min[i], x_max = bx$x_max[i])
    out
  }
  cbp <- lapply(boxes, to_list)
  core_fields <- c("date", "description", "amount", "debit", "credit", "balance",
                   "particulars", "code", "reference", "other_party", "type")
  ref_cols <- cbp[[ref]]
  extras_names <- setdiff(names(ref_cols), core_fields)
  # Every page's box list carries every field, so no page loses a column it simply
  # did not print on.
  allf <- unique(unlist(lapply(cbp, names)))
  # Date format: the one that reads the most dates in the date column.
  fv <- unlist(strsplit(model$rows$date_fmts[nzchar(model$rows$date_fmts)], "|", fixed = TRUE))
  dfmt <- if (length(fv)) {
    tab <- table(fv); top <- names(tab)[tab == max(tab)]
    order_ref <- vapply(ctx$fmts, `[[`, "", "fmt")
    top[order(match(top, order_ref))][1]
  } else "%d/%m/%Y"
  style <- if (any(rd$roles %in% c("debit", "credit"))) "debit_credit_cols"
           else if (identical(rd$conv, "U")) "unsigned" else "signed"
  keep_dateless <- any(is.na(model$rows$date))
  tbl <- list(row_tol = ctx$row_tol, date_format = dfmt, amount_sign = style,
              decimal_mark = ctx$decimal, unsigned_default = "debit",
              keep_dateless_rows = keep_dateless,
              ref_width = ctx$frame$width, ref_height = ctx$frame$height,
              columns = ref_cols[intersect(names(ref_cols), core_fields)],
              columns_by_page = cbp,
              extras = if (length(extras_names)) ref_cols[extras_names] else NULL)
  tpl <- list(id = NA_character_, bank = ctx$bank %||% NA_character_, statement_type = NA_character_,
              format = "pdf", version = 1L, currency = "NZD", table = tbl,
              auto = list(roles = rd$roles, conv = rd$conv, liab = isTRUE(rd$liab), dir = rd$dir,
                          engine = AUTO_READ_VERSION))
  tpl$signature <- .ar_signature_pdf(ctx, model, cs, rd, headings, tpl, boxes[[ref]])
  tpl$id <- paste0("auto_", .ar_hash(paste(unlist(tpl$signature[c("kind", "roles", "date_format",
                                                                 "money_style", "newest_first")]), collapse = "|")))
  tpl$boxes <- boxes
  tpl
}

.ar_hash <- function(s) {
  h <- safe(as.character(openssl::sha256(s)), NULL) %||% safe(digest::digest(s, algo = "sha256", serialize = FALSE), "")
  substr(h, 1, 12)
}

# .ar_signature_pdf(...) -- the layout signature in the shared contract's shape.
.ar_signature_pdf <- function(ctx, model, cs, rd, headings, tpl, ref_boxes) {
  roles <- unname(cs$field[!(cs$field %in% c("weekday"))])
  roles[startsWith(roles, "other")] <- "other"
  ink <- ref_boxes[ref_boxes$field %in% cs$field[!(cs$field %in% "weekday")], , drop = FALSE]
  span <- range(c(ink$ink_min, ink$ink_max))
  rel <- round((ink$ink_max - span[1]) / max(1, diff(span)), 2)
  sk <- unique(as.vector(.ar_values(model$cells, ctx$decimal)$SK))
  sk <- sort(intersect(sk, c("CR", "DR", "OD", "()", "-lead", "-trail", "+")))
  money_style <- if (identical(tpl$table$amount_sign, "debit_credit_cols")) "debit_credit_cols"
    else if (rd$conv %in% c("U", "B")) "unsigned"
    else if (any(sk %in% c("CR", "DR"))) "dr_cr_suffix" else "signed"
  bf <- if (rd$b > 0L) { nb <- sum(!is.na(model$cells[, rd$b])); if (nb == nrow(model$cells)) "every" else if (nb) "some" else "none" } else "none"
  ht <- if (!is.null(headings)) unique(tolower(unlist(regmatches(headings$text, gregexpr("[A-Za-z]{2,}", headings$text))))) else character(0)
  list(kind = if (any(ctx$ocr)) "scan" else "pdf", roles = roles, date_format = tpl$table$date_format,
       money_style = money_style, sign_markers = sk, balance_freq = bf,
       newest_first = identical(rd$dir, "new"),
       heading_tokens = utils::head(sort(ht), 40), producer = as.character(ctx$input$meta$pdf_doc$producer %||% "")[1],
       rel_x = rel, extras = setdiff(names(tpl$table$extras %||% list()), character(0)))
}

# .ar_parse_pdf(ctx, model, tpl) -- the engine's table reader on this document,
# cropped to each page's table: the lines of the table region, and on them only
# the words inside the table's outer boxes. Words already in the band frame, so
# the reader rescales nothing. The statement's metadata comes from the whole file.
.ar_parse_pdf <- function(ctx, model, tpl) {
  cin <- ctx$input
  cin$words <- lapply(seq_len(ctx$np), function(p) {
    pg <- model$pgs[[p]]; bx <- tpl$boxes[[p]]; rg <- model$regions[[as.character(p)]]
    if (is.null(pg) || is.null(bx) || is.null(rg)) return(data.frame(x = numeric(0), y = numeric(0),
      width = numeric(0), height = numeric(0), text = character(0), stringsAsFactors = FALSE))
    w <- pg$w
    keep <- w$line %in% rg$lines & w$cx >= min(bx$x_min) & w$cx <= max(bx$x_max)
    w[keep, c("x", "y", "width", "height", "text", intersect("ocr_conf", names(w))), drop = FALSE]
  })
  cin$page_width <- rep(ctx$frame$width, ctx$np); cin$page_height <- rep(ctx$frame$height, ctx$np)
  tpl$boxes <- NULL
  safe(parse_pdf_table(cin, tpl, meta = ctx$md), NULL)
}

# .ar_post_pdf(ctx, model, tpl, rd, parsed) -- the table reader's rows, finished:
#   * a date printed once per day is carried down to the rows under it (flagged);
#   * on a card or loan, plain figures are what is owed, so they flip (balances
#     first, then the amounts that were read);
#   * an unsigned column takes the sign the balance settled for each row (flagged);
#   * an amount the reader filled from the balance is recomputed from THIS
#     reading's balances and anchors (the reader assumes oldest first and takes
#     its opening balance from the metadata, which may be another figure).
.ar_post_pdf <- function(ctx, model, tpl, rd, parsed) {
  if (is.null(parsed)) return(list(parsed = NULL, tx = NULL, page = integer(0)))
  tx <- parsed$transactions
  n <- nrow(tx)
  page <- suppressWarnings(as.integer(sub("^pdf:p", "", parsed$provenance$source_ref %||% character(0))))
  if (n) {
    f <- tx$flags; f[is.na(f)] <- ""
    nod <- is.na(tx$date) & (is.na(tx$date_raw) | !nzchar(trimws(tx$date_raw)))
    if (any(nod)) {
      for (i in seq_len(n)) if (nod[i] && i > 1L && !is.na(tx$date[i - 1L])) tx$date[i] <- tx$date[i - 1L]
      f <- .ar_addflag(f, nod & !is.na(tx$date), "date_carried")
    }
    derived <- grepl("amount_from_balance", f, fixed = TRUE)
    if (rd$b > 0L && isTRUE(rd$liab)) {
      v <- .num(tx$balance_raw, ctx$decimal)
      tx$balance <- ifelse(is.na(v), tx$balance, .ar_sem(v, .ar_sign_kind(tx$balance_raw), TRUE))
    }
    if (any(rd$roles == "amount") && isTRUE(rd$liab)) {
      v <- .num(tx$amount_raw, ctx$decimal)
      tx$amount <- ifelse(is.na(v) | derived, tx$amount, .ar_sem(v, .ar_sign_kind(tx$amount_raw), TRUE))
    }
    if (identical(rd$conv, "B") && n == nrow(model$rows) && !is.null(rd$chain)) {
      sg <- rd$chain$signs
      tx$amount <- ifelse(!is.na(sg) & !is.na(tx$amount) & !derived, sg * abs(tx$amount), tx$amount)
      f <- .ar_addflag(f, !is.na(sg) & !derived, "sign_from_balance")
    }
    if (any(derived)) {
      op <- .ar_opening_value(model$anchors, rd, ctx$decimal)
      prev <- if (identical(rd$dir, "new")) c(tx$balance[-1], op) else c(op, tx$balance[-n])
      tx$amount[derived] <- round(tx$balance[derived] - prev[derived], 2)
    }
    tx$direction <- .direction(tx$amount)
    tx$flags <- f
    parsed$transactions <- tx
    op <- .ar_opening_value(model$anchors, rd, ctx$decimal)
    cl <- .ar_closing_value(model$anchors, rd, ctx$decimal)
    if (!is.na(op)) parsed$header$opening_balance <- op
    if (!is.na(cl)) parsed$header$closing_balance <- cl
  }
  list(parsed = parsed, tx = tx, page = page)
}

.ar_addflag <- function(f, cond, tok) ifelse(cond, ifelse(nzchar(f), paste0(f, ",", tok), tok), f)

# The opening / closing balance this reading uses: an in-table anchor first, then
# a summary box. NA when the statement prints neither.
.ar_opening_value <- function(anchors, rd, decimal) .ar_end_value(anchors, rd, decimal, "open")
.ar_closing_value <- function(anchors, rd, decimal) .ar_end_value(anchors, rd, decimal, "close")
.ar_end_value <- function(anchors, rd, decimal, cls) {
  n <- 1e9
  pts <- .ar_anchor_points(anchors, n, rd$dir, isTRUE(rd$liab), rd$b, decimal)
  want <- if (cls == "open") 0 else n
  v <- pts$val[pts$pos == want]
  if (length(v)) v[1] else NA_real_
}

# ---- the hard checks (spec section 4, step 8) -------------------------------------------

# .ar_pdf_checks(...) -- every check, on what the table reader produced. Each is
# list(ok, why): ok NA means "does not apply to this statement".
.ar_pdf_checks <- function(ctx, model, cs, tpl, rd, rl, post, basis) {
  tx <- post$tx
  n <- if (is.null(tx)) 0L else nrow(tx)
  ck <- list()
  add <- function(name, ok, why) ck[[name]] <<- list(ok = ok, why = why)
  add("rows_read", n > 0L, if (n > 0L) sprintf("%d transaction row(s) read.", n)
      else "The table reader produced no rows from the columns found.")
  if (n == 0L) return(list(checks = ck, chain = NULL, proof_kind = "none"))
  # Every row the column model found is a row the reader produced, page by page.
  mp <- table(factor(model$rows$page, levels = seq_len(ctx$np)))
  pp <- table(factor(post$page, levels = seq_len(ctx$np)))
  bad <- which(as.integer(mp) != as.integer(pp))
  add("rows_match_columns", !length(bad), if (!length(bad)) "Every row the columns show was read."
      else sprintf("On page %d the columns show %d row(s) but %d were read.", bad[1], as.integer(mp[bad[1]]), as.integer(pp[bad[1]])))
  # Every page with transaction-shaped lines gave rows.
  shaped <- which(vapply(model$pgs, function(pg) !is.null(pg) && length(.ar_seed_lines(pg)) > 0L, logical(1)))
  miss <- setdiff(shaped, unique(post$page))
  add("pages_with_rows", !length(miss), if (!length(miss)) "Every page with transactions gave rows."
      else sprintf("Page %s prints transaction lines but gave no rows.", paste(miss, collapse = ", ")))
  # The balance chain, on the output.
  bal <- if (rd$b > 0L) tx$balance else rep(NA_real_, n)
  apts <- .ar_anchor_points(model$anchors, n, rd$dir, isTRUE(rd$liab), rd$b, ctx$decimal)
  ch <- .ar_chain(tx$amount, rep(FALSE, n), bal, apts, rd$dir)
  sc <- .ar_chain_score(ch)
  row_steps <- rd$b > 0L && sum(!is.na(bal)) >= 1L
  proof_kind <- if (sc[["links"]] == 0) "none" else if (row_steps) "chain" else "totals"
  if (sc[["links"]] == 0) {
    add("balance_chain", NA, "No running balance and no opening and closing balance are printed, so nothing can be added up.")
  } else {
    bad <- which(!(ch$steps$ok %in% TRUE))
    why <- if (!length(bad)) sprintf("All %d balance step(s) hold to the cent.", sc[["links"]]) else {
      s1 <- ch$steps[bad[1], ]
      rows <- if (s1$to > s1$from) ch$ord[(s1$from + 1):s1$to] else integer(0)
      where <- if (length(rows)) sprintf("row %d (page %d)", rows[1], post$page[rows[1]]) else "a page break"
      if (s1$unknown > 0) sprintf("A balance step at %s cannot be checked: an amount in it was not read.", where)
      else sprintf("The balance does not add up at %s: it moves by %s but the rows add to %s.", where,
                   format(s1$expected, nsmall = 2), format(round(s1$got, 2), nsmall = 2))
    }
    add("balance_chain", !length(bad), why)
  }
  # The chain continues across every page break.
  pg_first <- vapply(split(seq_len(n), post$page), function(ix) ix[1], 0L)
  pg_first <- pg_first[-1]
  if (proof_kind == "chain" && length(pg_first)) {
    gap <- setdiff(pg_first, ch$covered)
    add("chain_across_pages", !length(gap), if (!length(gap)) "The balance carries across every page break."
        else sprintf("The balance does not carry across the break before row %d.", gap[1]))
  } else add("chain_across_pages", NA, "One page, or no running balance.")
  # Printed opening + movements = printed closing.
  op <- .ar_opening_value(model$anchors, rd, ctx$decimal); cl <- .ar_closing_value(model$anchors, rd, ctx$decimal)
  if (!is.na(op) && !is.na(cl) && !anyNA(tx$amount)) {
    ok <- abs(round(op + sum(tx$amount) - cl, 2)) < PARAM_MONEY_TOL
    add("opening_closing", ok, if (ok) "Opening balance plus every movement equals the closing balance."
        else sprintf("Opening %s plus the movements (%s) is %s, not the printed closing %s.", format(op, nsmall = 2),
                     format(round(sum(tx$amount), 2), nsmall = 2), format(round(op + sum(tx$amount), 2), nsmall = 2), format(cl, nsmall = 2)))
  } else add("opening_closing", NA, "The statement does not print both an opening and a closing balance.")
  add("printed_totals", .ar_totals_ok(model, tx, post$page, rd, ctx)$ok, .ar_totals_ok(model, tx, post$page, rd, ctx)$why)
  # Dates: readable, in order, inside the period.
  d <- suppressWarnings(as.Date(tx$date))
  add("dates_readable", !anyNA(d), if (!anyNA(d)) "Every date reads." else sprintf("%d date(s) could not be read.", sum(is.na(d))))
  if (!is.null(model$date2)) add("dates_in_order", NA, "Two date columns: the transaction dates need not be in order.")
  else if (!anyNA(d)) {
    dd <- as.numeric(diff(d))
    ok <- if (identical(rd$dir, "new")) all(dd <= 0) else all(dd >= 0)
    add("dates_in_order", ok, if (ok) "The dates run in order." else
        sprintf("The dates go backwards at row %d.", which(if (identical(rd$dir, "new")) dd > 0 else dd < 0)[1] + 1L))
  } else add("dates_in_order", NA, "Not every date reads.")
  p0 <- .plausible_period_date(ctx$md$period_start); p1 <- .plausible_period_date(ctx$md$period_end)
  if (!is.na(p0) && !is.na(p1) && p1 >= p0 && !anyNA(d)) {
    slack <- if (!is.null(model$date2)) 45 else 0
    out <- which(d < p0 - slack | d > p1)
    add("dates_in_period", !length(out), if (!length(out)) "Every date is inside the statement period."
        else sprintf("Row %d is dated outside the statement period.", out[1]))
  } else add("dates_in_period", NA, "No statement period was read.")
  wu <- .ar_words_used(model, tpl)
  add("words_used_once", wu$ok, wu$why)
  if (identical(rd$conv, "B")) {
    uns <- sum(is.na(rd$chain$signs[!is.na(rd$A) | TRUE][seq_len(n)]) & !is.na(tx$amount))
    add("signs_settled", uns == 0L, if (uns == 0L) "Every unsigned amount's sign is settled by the balance."
        else sprintf("%d unsigned amount(s) could take either sign.", uns))
  } else add("signs_settled", NA, "Every amount prints its own sign or sits in its own column.")
  nd <- sum(grepl("amount_from_balance", tx$flags, fixed = TRUE))
  add("no_derived_amounts", nd == 0L, if (nd == 0L) "No amount was filled in from the balance."
      else sprintf("%d amount(s) could not be read and were filled in from the balance.", nd))
  na_amt <- sum(is.na(tx$amount))
  add("amounts_read", na_amt == 0L, if (na_amt == 0L) "Every amount reads." else sprintf("%d amount(s) could not be read.", na_amt))
  uq <- switch(basis,
    arithmetic = list(TRUE, "No other reading of the columns fits the arithmetic."),
    ambiguous = list(FALSE, sprintf("%d different readings of the columns all fit the arithmetic.", rl$n_distinct)),
    broken = list(FALSE, "No reading of the columns makes the balance add up."),
    list(FALSE, "Nothing on the statement proves which column is which."))
  add("unique", uq[[1]], uq[[2]])
  # Every row is proven by a step (or by the totals). A text PDF may leave one row
  # outside the chain when no opening balance is printed; a scan may not, because
  # OCR can misread a digit and only a step would show it.
  unc <- setdiff(seq_len(n), ch$covered)
  if (proof_kind == "totals" && isTRUE(ck$opening_closing$ok)) unc <- integer(0)
  allow <- if (any(ctx$ocr) || proof_kind != "chain") 0L else if (sc[["held"]] >= 2) 1L else 0L
  add("rows_proven", length(unc) <= allow, if (!length(unc)) "Every row is inside a step that adds up."
      else sprintf("%d row(s) are outside every balance step (row %d first).", length(unc), unc[1]))
  list(checks = ck, chain = ch, proof_kind = proof_kind, score = sc)
}

# .ar_totals_ok(model, tx, page, rd, ctx) -- printed totals agree with the rows: each
# total must equal its side's movements over the whole statement, over its own page,
# or over every page up to its own.
.ar_totals_ok <- function(model, tx, page, rd, ctx) {
  tots <- Filter(function(a) identical(a$class, "total"), model$anchors)
  if (!length(tots) || anyNA(tx$amount)) return(list(ok = NA, why = "No printed totals to check."))
  deb <- function(ix) round(sum(-tx$amount[ix][tx$amount[ix] < 0]), 2)
  crd <- function(ix) round(sum(tx$amount[ix][tx$amount[ix] > 0]), 2)
  n <- nrow(tx)
  scopes <- function(a) {
    s <- list(seq_len(n), which(page == a$page))
    if (a$in_table) s <- c(s, list(seq_len(min(n, a$before_rows))), list(which(page <= a$page)))
    s
  }
  checked <- 0L
  for (a in tots) {
    side <- .ar_total_side(a$label, isTRUE(rd$liab))
    if (!nzchar(side)) next
    want <- list()
    dj <- which(rd$roles == "debit"); cj <- which(rd$roles == "credit")
    if (side == "both") {
      if (length(dj) && !is.na(a$figs[dj])) want$debit <- abs(.num(a$figs[dj], ctx$decimal))
      if (length(cj) && !is.na(a$figs[cj])) want$credit <- abs(.num(a$figs[cj], ctx$decimal))
    } else if (isTRUE(a$n_money == 1L)) want[[side]] <- abs(.num(a$value_text, ctx$decimal))
    for (sd in names(want)) {
      if (is.na(want[[sd]])) next
      f <- if (sd == "debit") deb else crd
      hit <- any(vapply(scopes(a), function(ix) abs(f(ix) - want[[sd]]) < PARAM_MONEY_TOL, logical(1)))
      checked <- checked + 1L
      if (!hit) return(list(ok = FALSE, why = sprintf("The printed total \"%s\" (%s) does not match the %s read.",
                                                     a$label, format(want[[sd]], nsmall = 2),
                                                     if (sd == "debit") "money out" else "money in")))
    }
  }
  if (!checked) return(list(ok = NA, why = "No printed totals to check."))
  list(ok = TRUE, why = sprintf("%d printed total(s) match the rows.", checked))
}

# .ar_words_used(model, tpl) -- every word on a table row ends up in exactly one
# column: none sits just outside the table's outer boxes (a word far away is a
# sidebar, not the table), and no two columns' print overlaps.
.ar_words_used <- function(model, tpl) {
  for (p in seq_along(tpl$boxes)) {
    bx <- tpl$boxes[[p]]; if (is.null(bx)) next
    if (any(bx$conflict)) return(list(ok = FALSE, why = sprintf("On page %d two columns' print overlaps, so no boundary separates them cleanly.", p)))
    pg <- model$pgs[[p]]; rg <- model$regions[[as.character(p)]]
    ls <- c(model$rows$line[model$rows$page == p], rg$cont)
    w <- pg$w[pg$w$line %in% ls, , drop = FALSE]
    lo <- min(bx$x_min); hi <- max(bx$x_max)
    stray <- w[(w$cx < lo & w$cx >= lo - 40) | (w$cx > hi & w$cx <= hi + 40), , drop = FALSE]
    if (nrow(stray)) return(list(ok = FALSE, why = sprintf("On page %d the word \"%s\" sits just outside every column.", p, stray$text[1])))
  }
  list(ok = TRUE, why = "Every word on the table rows falls in exactly one column.")
}

# ---- candidates and the decision ------------------------------------------------------------

.ar_candidate <- function(source, tpl, post, ckl, rd, rl, basis, model, cs, headings, ctx) {
  ck <- ckl$checks
  oks <- vapply(ck, function(x) x$ok, NA)
  failing <- names(oks)[oks %in% FALSE]
  passed <- !length(failing) && ckl$proof_kind != "none"
  sc <- ckl$score %||% c(links = 0, held = 0)
  cols_df <- do.call(rbind, lapply(seq_along(tpl$boxes), function(p) {
    bx <- tpl$boxes[[p]]; if (is.null(bx)) return(NULL)
    kind <- ifelse(bx$field %in% c("date", "date2", "weekday"), "date",
              ifelse(bx$field %in% c("debit", "credit", "amount", "balance") | startsWith(bx$field, "other"), "money", "text"))
    hd <- vapply(seq_len(nrow(bx)), function(i) .ar_heading_over(headings, bx$ink_min[i] - model$shift[p], bx$ink_max[i] - model$shift[p]), "")
    data.frame(page = p, field = bx$field, kind = kind, x_min = bx$x_min, x_max = bx$x_max,
               ink_min = bx$ink_min, ink_max = bx$ink_max, heading = hd, stringsAsFactors = FALSE)
  }))
  tpl$boxes <- NULL
  derived <- if (!is.null(post$tx)) sum(grepl("amount_from_balance", post$tx$flags, fixed = TRUE)) else 0L
  proof <- list(kind = ckl$proof_kind, links = as.integer(sc[["links"]]), held = as.integer(sc[["held"]]),
                unique = identical(basis, "arithmetic"),
                pages_with_rows = sort(unique(post$page)),
                pages_used = sort(unique(model$rows$page)), derived = as.integer(derived),
                one_row = length(ckl$chain$one_row %||% integer(0)),
                rows_covered = length(ckl$chain$covered %||% integer(0)),
                direction_note = rl$note %||% character(0))
  why <- if (passed) .ar_proven_why(proof, rd) else if (length(failing)) ck[[failing[1]]]$why
         else "Nothing on the statement adds up to prove the reading."
  list(source = source, passed = passed, failing = failing, why = why, template = tpl,
       parsed = post$parsed, tx = post$tx, checks = .ar_checks_df(ck), proof = proof,
       columns = cols_df, rd = rd, basis = basis, signature = tpl$signature,
       no_balance = rd$b == 0L, notes = rl$note %||% character(0))
}

.ar_proven_why <- function(proof, rd) {
  base <- if (proof$kind == "chain")
    sprintf("The running balance checks on all %d step(s) and no other reading of the columns fits.", proof$links)
  else "Opening balance plus every movement equals the closing balance, and no other reading fits."
  if (identical(rd$dir, "new")) base <- paste(base, "The statement lists the newest transaction first.")
  base
}

# .ar_decide(cands, layouts, ctx) -- the outcome rules, applied to every candidate.
.ar_decide <- function(cands, layouts, ctx) {
  cdf <- data.frame(source = names(cands),
                    passed = vapply(cands, function(cd) isTRUE(cd$passed), logical(1)),
                    why = vapply(cands, function(cd) cd$why %||% "", ""), stringsAsFactors = FALSE,
                    row.names = NULL)
  key <- function(cd) if (is.null(cd$tx) || !nrow(cd$tx)) "" else
    paste(cd$tx$date, sprintf("%.2f", cd$tx$amount), collapse = "|")
  passed <- Filter(function(cd) isTRUE(cd$passed), cands)
  content <- cands[["content"]]
  if (length(passed)) {
    keys <- unique(vapply(passed, key, ""))
    pick <- if (isTRUE(content$passed)) content else passed[[1]]
    if (length(keys) == 1L) {
      return(.ar_finish(pick, "proven", pick$why, cdf, .ar_match_layout(pick, layouts)))
    }
    return(.ar_finish(pick, "check", sprintf("%d different readings each pass every check, so the reading is not unique.", length(keys)),
                      cdf, NULL))
  }
  # No running balance and no printed totals: a proven layout handed in may carry it.
  lm <- Filter(function(cd) startsWith(cd$source, "layout:") && !is.null(cd$tx) && .ar_layout_match_ok(cd), cands)
  if (length(lm)) {
    cd <- lm[[1]]
    ref <- sub("^layout:", "", cd$source)
    info <- .ar_layout_info(Filter(function(l) identical(.ar_layout_info(l)$ref, ref), layouts)[[1]])
    if (isTRUE(info$proven) && (is.null(content$tx) || identical(key(content), key(cd)) || !isTRUE(content$basis == "arithmetic")))
      return(.ar_finish(cd, "layout_match", sprintf("No running balance or totals are printed; the reading matches the proven layout %s and nothing contradicts it.", ref),
                        cdf, ref))
  }
  # Show the most useful reading: the content one when it read rows, else any.
  show <- content
  if (is.null(show$tx) || !nrow(show$tx)) {
    alt <- Filter(function(cd) !is.null(cd$tx) && nrow(cd$tx) > 0, cands)
    if (length(alt)) show <- alt[[1]]
  }
  if (is.null(show$tx) || !nrow(show$tx)) return(.ar_unread(content$why %||% "Nothing could be read.", cdf, show$checks))
  sc <- show$proof
  broken <- show$proof$kind != "none" && sc$links >= 2 && sc$held < 0.5 * sc$links
  outcome <- if (broken) "unread" else "check"
  .ar_finish(show, outcome, show$why, cdf, NULL)
}

# .ar_layout_match_ok(cd) -- a layout candidate that may stand without arithmetic:
# nothing to add up at all, and every other check passes.
.ar_layout_match_ok <- function(cd) {
  if (!identical(cd$proof$kind, "none")) return(FALSE)
  ck <- cd$checks
  bad <- ck$check[ck$ok %in% FALSE]
  all(bad %in% c("unique", "rows_proven"))
}

.ar_finish <- function(cd, outcome, why, cdf, matched) {
  recon <- if (!is.null(cd$parsed)) safe(reconcile(cd$parsed, cd$template), NULL) else NULL
  tpl <- cd$template
  if (!is.null(tpl)) tpl$auto$outcome <- outcome
  list(outcome = outcome, why = why, template = tpl, parsed = cd$parsed, recon = recon,
       transactions = cd$tx %||% .ar_empty_tx(), proof = cd$proof, checks = cd$checks,
       candidates = cdf, columns = cd$columns, matched_layout = matched,
       notes = cd$notes %||% character(0))
}

# ---- layouts --------------------------------------------------------------------------------

# .ar_layout_info(ly) -- what a layout handed in says: its reference
# ("<id>@<version>"), kind, roles and conventions, and whether it is proven. A
# layout is a template list, optionally with a `layout` block (R/layouts.R); one
# without the block is a proven reading's own template.
.ar_layout_info <- function(ly) {
  if (!is.list(ly)) return(NULL)
  sig <- ly$signature %||% ly$layout$signature
  if (is.null(sig)) return(NULL)
  id <- ly$layout$id %||% ly$id %||% "layout"
  ver <- ly$layout$version %||% ly$version %||% 1L
  au <- ly$auto %||% list()
  roles <- au$roles %||% {
    r <- sig$roles[sig$roles %in% c("debit", "credit", "amount", "balance", "other")]; r }
  conv <- au$conv %||% (if (identical(sig$money_style, "unsigned")) "U" else "S")
  status <- ly$layout$status %||% (if (identical(au$outcome, "proven") || is.null(ly$layout)) "proven" else "provisional")
  list(ref = paste0(id, "@", ver), kind = sig$kind %||% "pdf", roles = unlist(roles), conv = conv,
       liab = isTRUE(au$liab), dir = if (isTRUE(sig$newest_first)) "new" else (au$dir %||% "old"),
       proven = status %in% c("proven", "confirmed"), sig = sig)
}

# .ar_sig_match(a, b) -- two signatures name the same layout family: same kind
# family (a scan and a PDF of one design are one family), same column roles in
# order, date format, money style and order, and column positions within 0.08 of
# the table's width.
.ar_sig_match <- function(a, b) {
  fam <- function(k) if (k %in% c("pdf", "scan")) "pdf" else k
  if (is.null(a) || is.null(b)) return(FALSE)
  if (!identical(fam(a$kind %||% ""), fam(b$kind %||% ""))) return(FALSE)
  if (!identical(unlist(a$roles), unlist(b$roles))) return(FALSE)
  if (!identical(a$date_format, b$date_format) || !identical(a$money_style, b$money_style)) return(FALSE)
  if (!identical(isTRUE(a$newest_first), isTRUE(b$newest_first))) return(FALSE)
  ra <- unlist(a$rel_x); rb <- unlist(b$rel_x)
  if (length(ra) == length(rb) && length(ra) && max(abs(ra - rb)) > 0.08) return(FALSE)
  TRUE
}

.ar_match_layout <- function(cd, layouts) {
  for (ly in layouts) {
    info <- .ar_layout_info(ly)
    if (!is.null(info) && .ar_sig_match(cd$signature, info$sig)) return(info$ref)
  }
  NULL
}
