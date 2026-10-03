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
#
# opts$roles -- a person's fix from Please check: the roles of the figure columns,
# left to right (debit, credit, amount, balance, other), as the reading's own
# template$auto$roles lists them. Only that assignment is read; the arithmetic
# still has to prove it, and no learned layout or repair stands in for it.

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
  # Step 1, normalise: a page whose text runs down the page is turned upright, so
  # every page is read in one frame.
  for (p in seq_len(np)) {
    rw <- .ar_upright(wl[[p]], pw[p], ph[p])
    if (!is.null(rw)) { wl[[p]] <- rw; tmp <- pw[p]; pw[p] <- ph[p]; ph[p] <- tmp }
  }
  input$words <- wl
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
  md <- safe(extract_metadata(input), NULL)
  if (!is.null(md)) md$periods <- .ar_periods(input$pages, md)
  list(input = input, np = np, pw = pw, ph = ph, frame = frame, ocr = ocr, row_tol = row_tol,
       md = md, fmts = .ar_date_formats(), markers = .ar_markers(),
       decimal = decimal, pages_text = input$pages %||% character(0))
}

# A scanned page whose words were read with a median confidence under this is
# noise, not text (image clean-up can turn a page's paper grain into thousands of
# junk "words"), and is read again.
.AR_OCR_NOISE_CONF <- 50

# .ar_reocr(input) -- re-read a scanned page that came back as noise, straight
# from its picture without the image clean-up; the new reading is kept only when
# Tesseract is surer of it. Returns the input and a note per page re-read.
.ar_reocr <- function(input) {
  notes <- character(0)
  path <- input$path %||% ""
  ocr <- as.logical(input$page_ocr %||% logical(0)); ocr[is.na(ocr)] <- FALSE
  if (!any(ocr) || !nzchar(path) || !file.exists(path) || !isTRUE(safe(ocr_available(), FALSE)))
    return(list(input = input, notes = notes))
  for (p in which(ocr)) {
    w <- input$words[[p]]
    cf <- suppressWarnings(stats::median(as.numeric(w$ocr_conf), na.rm = TRUE))
    if (is.null(w) || !isTRUE(cf < .AR_OCR_NOISE_CONF)) next
    r <- safe(ocr_pdf_page(path, .ar_file_page(input, p), preprocess = FALSE), NULL)
    if (is.null(r) || !isTRUE(r$ok) || is.null(r$words) || !nrow(r$words)) next
    cf2 <- suppressWarnings(stats::median(as.numeric(r$words$ocr_conf), na.rm = TRUE))
    if (!isTRUE(cf2 > cf)) next
    input$words[[p]] <- r$words
    if (length(input$pages) >= p) input$pages[p] <- paste(r$text, collapse = "\n")
    if (isTRUE(r$width > 0)) input$page_width[p] <- r$width
    if (isTRUE(r$height > 0)) input$page_height[p] <- r$height
    notes <- c(notes, sprintf("Page %d of the scan read as noise (confidence %.0f) and was read again without image clean-up (confidence %.0f).", p, cf, cf2))
  }
  list(input = input, notes = notes)
}

# .ar_file_page(input, p) -- page p of this input as a page of the file on disk: a
# statement cut out of a bundle (.subinput_pages) carries its pages' numbers in
# the file as page_map, so a page read again from the picture is the right one.
.ar_file_page <- function(input, p) {
  pm <- input$page_map
  if (length(pm) >= p) as.integer(pm[p]) else as.integer(p)
}

# .ar_upright(w, pw, ph) -- a page set on its side (a /Rotate the text layer does
# not undo) has words taller than they are wide; turned a quarter turn the way
# that puts its dates on the left, upright. NULL when the page is upright already.
.ar_upright <- function(w, pw, ph) {
  if (is.null(w) || nrow(w) < 10L || !isTRUE(pw > 0) || !isTRUE(ph > 0)) return(NULL)
  long <- nchar(as.character(w$text)) >= 4L & !is.na(w$width) & !is.na(w$height)
  if (sum(long) < 5L || !isTRUE(stats::median(w$height[long] / pmax(w$width[long], 0.1)) >= 1.5)) return(NULL)
  turn <- function(cw) {
    out <- w
    out$width <- w$height; out$height <- w$width
    if (cw) { out$x <- w$y; out$y <- pw - (w$x + w$width) }
    else { out$x <- ph - (w$y + w$height); out$y <- w$x }
    out
  }
  # The turn that leaves dates at the left end of their lines.
  left_dates <- function(r) {
    d <- nzchar(.ar_date_fmts(as.character(r$text), .ar_date_formats()))
    if (!any(d)) return(0)
    mean(r$x[d]) < mean(r$x)
  }
  a <- turn(TRUE); b <- turn(FALSE)
  if (isTRUE(left_dates(a) >= left_dates(b))) a else b
}

# .ar_periods(pages, md) -- the statement periods a bundle of consecutive
# statements prints, one per statement: the metadata's own period, plus every
# other printed range that joins the chain end to start (a shared boundary day
# allowed). A range that does not join up -- a loan term, a tax year -- is not a
# statement period and widens nothing.
.ar_periods <- function(pages, md) {
  first <- c(.plausible_period_date(md$period_start), .plausible_period_date(md$period_end))
  if (anyNA(first)) return(list())
  dash <- paste0("-", intToUtf8(0x2013), intToUtf8(0x2014))
  date_rx <- safe(lex("date_regex"), NULL)
  conn <- paste(safe(lex("period_connectives"), "to"), collapse = "|")
  if (is.null(date_rx)) return(list(as.character(first)))
  per_rx <- sprintf("(?:%s)\\s*(?:%s|[%s])\\s*(?:%s)", date_rx, conn, dash, date_rx)
  txt <- enc2utf8(paste(pages %||% character(0), collapse = "\n"))
  hits <- unique(regmatches(txt, gregexpr(per_rx, txt, perl = TRUE))[[1]])
  pb <- lapply(hits, function(h) {
    ds <- regmatches(h, gregexpr(date_rx, h))[[1]]
    if (length(ds) < 2) return(NULL)
    v <- c(.plausible_period_date(ds[1]), .plausible_period_date(ds[2]))
    if (anyNA(v) || v[1] > v[2]) NULL else v
  })
  pb <- Filter(Negate(is.null), pb)
  keep <- list(first)
  repeat {
    lo <- min(vapply(keep, function(v) as.numeric(v[1]), 0)); hi <- max(vapply(keep, function(v) as.numeric(v[2]), 0))
    add <- Filter(function(v) (as.numeric(v[1]) >= hi && as.numeric(v[1]) <= hi + 1) ||
                              (as.numeric(v[2]) <= lo && as.numeric(v[2]) >= lo - 1), pb)
    add <- Filter(function(v) !any(vapply(keep, function(k) identical(k, v), logical(1))), add)
    if (!length(add)) break
    keep <- c(keep, add[1])
  }
  lapply(keep, as.character)
}

# .ar_pdf_pages(ctx, thr_mult) -- every page typed and measured (cached per cell
# threshold, since the repair search re-measures with other splits).
.ar_pdf_pages <- function(ctx, thr_mult = 1) {
  wl <- ctx$input$words %||% list()
  pgs <- lapply(seq_len(ctx$np), function(p) {
    pg <- .ar_page(wl[[p]], p, ctx$frame, ctx$pw[p], ctx$ph[p], ctx$row_tol, ctx$fmts, ctx$markers,
                   ocr = isTRUE(ctx$ocr[p]))
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
  ro <- .ar_reocr(input)
  input <- ro$input
  ctx <- .ar_pdf_context(input)
  ctx$notes <- ro$notes
  ctx$bank <- bank
  ctx$roles <- opts$roles
  base <- .ar_pdf_pages(ctx)
  model <- .ar_model(base, list())
  # The account type is read from the print OUTSIDE the transaction rows: an
  # everyday account paying off its card ("CREDIT CARD PAYMENT") is not a card.
  ctx$liab <- .ar_liability_evidence(if (is.null(model)) ctx$pages_text else .ar_outside_text(model))
  cands <- list()
  cands[["content"]] <- .ar_pdf_attempt(ctx, base, list(), "content", model = model)
  # A person's roles are read on their own: no layout or repair stands in for them.
  if (!is.null(ctx$roles)) return(.ar_decide(cands, list(), ctx))
  # Each layout handed in, registered to this document: its conventions on the
  # columns found here. A layout whose conventions are the content reading's own
  # (when the arithmetic chose them) would read the same figures, so it is not
  # read twice.
  K <- if (is.null(model)) 0L else length(model$cols)
  seen <- if (identical(cands$content$basis, "arithmetic")) .ar_conv_key(cands$content$rd) else ""
  for (ly in layouts) {
    info <- .ar_layout_info(ly)
    if (is.null(info) || !(info$kind %in% c("pdf", "scan"))) next
    src <- paste0("layout:", info$ref)
    if (!is.null(cands[[src]])) next
    fig <- info$roles[info$roles %in% c("debit", "credit", "amount", "balance", "other")]
    if (length(fig) != K || !.ar_layout_near(cands$content$signature, info$sig)) next
    ck <- .ar_conv_key(list(roles = fig, conv = info$conv, liab = info$liab, dir = info$dir))
    if (ck %in% seen) next
    seen <- c(seen, ck)
    cands[[src]] <- .ar_pdf_attempt(ctx, base, list(), src, forced = info, model = model)
    if (sum(startsWith(names(cands), "layout:")) >= .AR_MAX_LAYOUTS) break
  }
  # Repair search: bounded, fixed order, and only when nothing above passed. A
  # repair that changes nothing on this document (no cell splits differently, no
  # page was shifted) is not read again.
  if (!any(vapply(cands, function(cd) isTRUE(cd$passed), logical(1))) && any(ctx$ocr)) {
    # On a scan, first: the rows the arithmetic points at, read again.
    fixed <- .ar_reocr_rows(ctx, cands$content)
    if (!is.null(fixed)) {
      ctx2 <- ctx; ctx2$input$words <- fixed$words
      ctx2$notes <- c(ctx$notes, fixed$notes)
      cands[["repair:reocr_rows"]] <- .ar_pdf_attempt(ctx2, .ar_pdf_pages(ctx2), list(), "repair:reocr_rows")
    }
  }
  if (!any(vapply(cands, function(cd) isTRUE(cd$passed), logical(1)))) {
    reps <- list(
      "repair:wider_cells"   = list(thr = 1.6),
      "repair:narrower_cells" = list(thr = 0.6),
      "repair:no_page_shift" = list(shift = FALSE))
    split_key <- function(pgs) paste(unlist(lapply(pgs, function(pg) if (!is.null(pg)) pg$ph$standalone)), collapse = "")
    k0 <- split_key(base)
    for (nm in names(reps)) {
      r <- reps[[nm]]
      if (!is.null(r$thr)) {
        pg2 <- .ar_pdf_pages(ctx, r$thr)
        if (identical(split_key(pg2), k0)) next
        cands[[nm]] <- .ar_pdf_attempt(ctx, pg2, list(), nm)
      } else {
        if (is.null(model) || all(model$shift == 0)) next
        cands[[nm]] <- .ar_pdf_attempt(ctx, base, list(shift = FALSE), nm)
      }
    }
  }
  .ar_decide(cands, layouts, ctx)
}

# .ar_outside_text(model) -- every printed line that is not part of a table region
# (rows, their wrapped lines and the summary lines among them).
.ar_outside_text <- function(model) {
  unlist(lapply(Filter(Negate(is.null), model$pgs), function(pg) {
    rg <- model$regions[[as.character(pg$page)]]
    pg$lines$raw[!(pg$lines$line %in% (rg$lines %||% integer(0)))]
  }))
}

# At most this many of a bank's layouts are read against one document: the ones
# of the same design, in the order handed in.
.AR_MAX_LAYOUTS <- 5L

# .ar_layout_near(a, b) -- is a layout (signature b) of the same design as this
# document (signature a), whatever roles each gives the columns? Same kind family,
# date style and column positions within 0.08 of the table's width, and at least
# half their heading words shared. Only such a layout is worth reading against it.
.ar_layout_near <- function(a, b) {
  if (is.null(a) || is.null(b)) return(FALSE)
  fam <- function(k) if (k %in% c("pdf", "scan")) "pdf" else k
  if (!identical(fam(a$kind %||% ""), fam(b$kind %||% ""))) return(FALSE)
  if (!identical(a$date_format, b$date_format)) return(FALSE)
  ra <- unlist(a$rel_x); rb <- unlist(b$rel_x)
  if (length(ra) != length(rb) || (length(ra) && max(abs(ra - rb)) > 0.08)) return(FALSE)
  ha <- unlist(a$heading_tokens); hb <- unlist(b$heading_tokens)
  !length(ha) || !length(hb) || length(intersect(ha, hb)) >= 0.5 * length(union(ha, hb))
}

# .ar_reocr_rows(ctx, cd) -- step 9's re-OCR, aimed by the arithmetic: on a scan,
# each row whose amount was filled in from the balance, or that alone breaks a
# balance step, has its money-in / money-out cells read again from the page
# picture (one line, digits only). A figure read this way replaces the row's
# figure words only when it is EXACTLY the movement the balance requires, so a
# re-read can only ever confirm the arithmetic, never move a figure the page does
# not print. Returns patched words and notes, or NULL when nothing was confirmed.
.ar_reocr_rows <- function(ctx, cd) {
  path <- ctx$input$path %||% ""
  if (is.null(cd$tx) || is.null(cd$rows) || is.null(cd$chain) || !nzchar(path) || !file.exists(path) ||
      nrow(cd$tx) != nrow(cd$rows) || !isTRUE(safe(ocr_available(), FALSE))) return(NULL)
  tx <- cd$tx; ch <- cd$chain
  want <- rep(NA_real_, nrow(tx))
  der <- which(grepl("amount_from_balance", tx$flags, fixed = TRUE))
  want[der] <- tx$amount[der]
  st <- ch$steps
  one <- which(st$to - st$from == 1L & !(st$ok %in% TRUE) & st$unknown == 0L)
  for (q in one) { i <- ch$ord[st$to[q]]; want[i] <- st$expected[q] }
  todo <- which(!is.na(want) & want != 0)
  if (!length(todo) || length(todo) > 6L) return(NULL)
  mcols <- cd$columns[cd$columns$field %in% c("amount", "debit", "credit"), , drop = FALSE]
  words <- ctx$input$words; notes <- character(0)
  # Each page's picture is drawn once and deleted on the way out: it is a picture
  # of a client's statement.
  prefix <- tempfile("arcrop_")
  on.exit(unlink(Sys.glob(paste0(prefix, "*")), force = TRUE), add = TRUE)
  pics <- list()
  for (i in todo) {
    p <- cd$rows$page[i]
    if (!isTRUE(ctx$ocr[p])) next
    mc <- mcols[mcols$page == p, , drop = FALSE]
    if (!nrow(mc)) next
    sx <- if (isTRUE(ctx$pw[p] > 0)) ctx$pw[p] / ctx$frame$width else 1
    sy <- if (isTRUE(ctx$ph[p] > 0)) ctx$ph[p] / ctx$frame$height else 1
    box <- c(x0 = min(mc$x_min) * sx - 12, x1 = max(mc$x_max) * sx + 12,
             y0 = (cd$rows$y[i] - 3) * sy, y1 = (cd$rows$y1[i] + 3) * sy)
    key <- as.character(p)
    if (is.null(pics[[key]])) {
      fp <- .ar_file_page(ctx$input, p)
      safe(system2("pdftoppm", c("-png", "-r", 300, "-f", fp, "-l", fp, path, paste0(prefix, "_p", p)),
                   stdout = FALSE, stderr = FALSE), 1L)
      pics[[key]] <- Sys.glob(paste0(prefix, "_p", p, "*.png"))[1]
    }
    got <- .ar_ocr_crop(pics[[key]], box, paste0(prefix, "_strip.png"))
    if (is.null(got) || !nrow(got)) next
    got$v <- .num(got$text)
    # The figure must end where the column it belongs to ends (figures are set
    # flush right): nearest right edge among the money columns, within 6pt.
    side <- if (want[i] < 0) c("debit", "amount") else c("credit", "amount")
    allm <- cd$columns[cd$columns$page == p & cd$columns$kind == "money", , drop = FALSE]
    col_of <- vapply((got$x + got$width) / sx, function(r) {
      d <- abs(allm$ink_max - r); if (!length(d) || min(d) > 6) NA_character_ else allm$field[which.min(d)] }, "")
    ok <- which(!is.na(got$v) & abs(abs(got$v) - abs(want[i])) < PARAM_MONEY_TOL &
                grepl(.AR_MONEY_RX, got$text, perl = TRUE) & col_of %in% side)
    if (length(ok) != 1L) next
    w <- words[[p]]
    # The old figure words in the money cells of this row go; the re-read figure
    # takes their place.
    wcx <- (w$x + w$width / 2) / sx; wx1 <- (w$x + w$width) / sx
    old <- !is.na(w$y) & w$y + w$height / 2 >= box[["y0"]] & w$y + w$height / 2 <= box[["y1"]] &
      (vapply(wcx, function(x) any(x >= mc$x_min & x <= mc$x_max), logical(1)) |
       vapply(wx1, function(x) any(abs(x - mc$ink_max) <= 6), logical(1)))
    nw <- w[rep(1L, 1L), , drop = FALSE]
    nw$text <- got$text[ok]; nw$x <- got$x[ok]; nw$width <- got$width[ok]
    nw$y <- if (any(old)) w$y[which(old)[1]] else cd$rows$y[i] * sy
    nw$height <- got$height[ok]
    if ("ocr_conf" %in% names(nw)) nw$ocr_conf <- got$conf[ok]
    words[[p]] <- rbind(w[!old, , drop = FALSE], nw)
    notes <- c(notes, sprintf("Row %d's figure was read again from the page and reads %s, as the balance requires.", i, got$text[ok]))
  }
  if (!length(notes)) return(NULL)
  list(words = words, notes = notes)
}

# .ar_ocr_crop(img, box, crop, dpi) -- one strip of a page picture (box in points)
# read again by Tesseract as a single line of figures; `crop` is where the strip
# is written (the caller deletes it).
.ar_ocr_crop <- function(img, box, crop, dpi = 300) {
  if (is.null(img) || is.na(img) || !file.exists(img) || !requireNamespace("magick", quietly = TRUE)) return(NULL)
  k <- dpi / 72
  g <- sprintf("%dx%d+%d+%d", as.integer(ceiling((box[["x1"]] - box[["x0"]]) * k)),
               as.integer(ceiling((box[["y1"]] - box[["y0"]]) * k)),
               as.integer(floor(box[["x0"]] * k)), as.integer(floor(box[["y0"]] * k)))
  ok <- safe(with_image_scratch({
    # Black on white: on the paper grain of a scan Tesseract loses a decimal point.
    im <- magick::image_convert(magick::image_crop(magick::image_read(img), g), type = "Grayscale")
    im <- magick::image_threshold(magick::image_threshold(im, "white", "60%"), "black", "60%")
    magick::image_write(magick::image_border(im, "white", "20x20"), crop, format = "png"); TRUE }), FALSE)
  if (!isTRUE(ok)) return(NULL)
  out <- safe(system2("tesseract", c(crop, "stdout", "--psm", "7", "-c",
                                     "tessedit_char_whitelist=0123456789.,-", "tsv"),
                      stdout = TRUE, stderr = FALSE), NULL)
  if (is.null(out) || length(out) < 2L) return(NULL)
  tsv <- safe(utils::read.table(text = paste(out, collapse = "\n"), sep = "\t", header = TRUE, quote = "",
                                comment.char = "", stringsAsFactors = FALSE, fill = TRUE,
                                colClasses = c(text = "character")), NULL)
  if (is.null(tsv) || !nrow(tsv)) return(NULL)
  tsv <- tsv[!is.na(tsv$conf) & tsv$conf >= 0 & nzchar(trimws(tsv$text)), , drop = FALSE]
  if (!nrow(tsv)) return(NULL)
  data.frame(text = trimws(tsv$text), x = box[["x0"]] + (tsv$left - 20) / k, width = tsv$width / k,
             height = tsv$height / k, cx = box[["x0"]] + (tsv$left - 20 + tsv$width / 2) / k,
             conf = as.numeric(tsv$conf), stringsAsFactors = FALSE)
}

# .ar_conv_key(rd) -- a reading's conventions as one string: roles, sign
# convention, card or loan, and order.
.ar_conv_key <- function(rd) paste(c(rd$roles, rd$conv, isTRUE(rd$liab), rd$dir), collapse = "|")

# .ar_pdf_attempt(ctx, pgs, mopts, source, forced, model) -- one candidate reading,
# start to finish: column model (or the one handed in), roles, template, the table
# reader, and every hard check on what it produced. `forced` (a layout) fixes the
# roles and conventions instead of searching for them.
.ar_pdf_attempt <- function(ctx, pgs, mopts, source, forced = NULL, model = .ar_model(pgs, mopts)) {
  fail <- function(why) list(source = source, passed = FALSE, status = "unread", why = why)
  if (is.null(model)) return(fail("No line on any page prints a date with a figure beside it, so no transaction table was found."))
  K <- length(model$cols)
  V <- .ar_values(model$cells, ctx$decimal)
  headings <- .ar_headings(model)
  hroles <- .ar_heading_roles(headings, model)
  if (!is.null(forced)) {
    rl <- .ar_forced_reading(V, model$anchors, forced, ctx$decimal)
    if (is.null(rl$chosen)) return(fail(rl$why))
  } else {
    if (!is.null(ctx$roles) && length(ctx$roles) != K)
      return(fail(sprintf("The roles given are for %d column(s) of figures; this statement shows %d.", length(ctx$roles), K)))
    rl <- .ar_roles(V, model$anchors, ctx$liab, ctx$decimal, hroles, texts = model$rows$raw, only = ctx$roles)
  }
  rd <- rl$chosen
  basis <- if (!is.null(rd)) "arithmetic" else "none"
  if (is.null(rd) && rl$n_distinct > 1L) {
    # Two readings both fit: show the one the headings and the account type
    # favour, never call it proven.
    agree <- vapply(rl$distinct, function(r) sum(!is.na(hroles) & hroles == r$roles) +
                      0.5 * identical(isTRUE(r$liab), isTRUE(ctx$liab$liability)), 0)
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
  parsed <- .ar_parse_pdf(ctx, model, tpl, rd)
  post <- .ar_post_pdf(ctx, model, tpl, rd, parsed)
  ck <- .ar_pdf_checks(ctx, model, cs, tpl, rd, rl, post, basis)
  cd <- .ar_candidate(source, tpl, post, ck, rd, rl, basis, model, cs, headings, ctx)
  cd$hroles <- hroles
  cd$notes <- c(ctx$notes, cd$notes)
  cd$chain <- ck$chain
  cd$rows <- model$rows[, c("page", "y", "y1")]
  cd
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
  list(chosen = rd, n_distinct = 1L, distinct = list(rd), best = rd, note = character(0))
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
       rel_x = rel, extras = as.character(names(tpl$table$extras %||% list())))
}

# .ar_parse_pdf(ctx, model, tpl, rd) -- the engine's table reader on this document,
# cropped to each page's table: the lines of the table region, and on them only
# the words inside the table's outer boxes. Words already in the band frame, so
# the reader rescales nothing. The statement's metadata comes from the whole file.
.ar_parse_pdf <- function(ctx, model, tpl, rd) {
  cin <- ctx$input
  cin$words <- lapply(seq_len(ctx$np), function(p) {
    pg <- model$pgs[[p]]; bx <- tpl$boxes[[p]]; rg <- model$regions[[as.character(p)]]
    if (is.null(pg) || is.null(bx) || is.null(rg)) return(data.frame(x = numeric(0), y = numeric(0),
      width = numeric(0), height = numeric(0), text = character(0), stringsAsFactors = FALSE))
    w <- pg$w
    # Summary lines are the model's anchors, never rows: they are not handed on.
    keep <- w$line %in% setdiff(rg$lines, rg$anchors) & w$cx >= min(bx$x_min) & w$cx <= max(bx$x_max)
    w[keep, c("x", "y", "width", "height", "text", intersect("ocr_conf", names(w))), drop = FALSE]
  })
  cin$page_width <- rep(ctx$frame$width, ctx$np); cin$page_height <- rep(ctx$frame$height, ctx$np)
  tpl$boxes <- NULL
  # A body row whose amount was removed (only its balance is printed) is still a
  # row: it is handed over as one, so the table reader keeps it, and its amount
  # comes back unread or derived from the balance -- never silently dropped.
  # Only when the balance carries on from it: a footer that merely prints a figure
  # where the balance column runs ("Interest rate 4.25") breaks the step after it.
  mv <- which(tpl$auto$roles %in% c("amount", "debit", "credit"))
  bare <- if (length(mv)) which(rowSums(!is.na(model$cells[, mv, drop = FALSE])) == 0L) else integer(0)
  ch <- rd$chain
  if (length(bare) && !is.null(ch) && nrow(ch$steps)) bare <- bare[vapply(bare, function(i) {
    t <- match(i, ch$ord); nx <- which(ch$steps$from == t)
    !length(nx) || !(ch$steps$ok[nx[1]] %in% FALSE)
  }, logical(1))]
  force <- lapply(bare, function(i) list(page = model$rows$page[i], y_min = model$rows$y[i], y_max = model$rows$y1[i]))
  safe(parse_pdf_table(cin, tpl, force_rows = if (length(force)) force else NULL, meta = ctx$md), NULL)
}

# .ar_post_pdf(ctx, model, tpl, rd, parsed) -- the table reader's rows, finished: a
# date printed once per day is carried down to the rows under it (flagged), the
# figures are put in this reading's terms (.ar_settle), and the opening and closing
# balances the reading used go in the header.
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
    tx$flags <- f
    tx <- .ar_settle(tx, rd, model$cells, ctx$decimal, model$anchors, aligned = n == nrow(model$rows))
    parsed$transactions <- tx
    op <- .ar_opening_value(model$anchors, rd, ctx$decimal, n)
    cl <- .ar_closing_value(model$anchors, rd, ctx$decimal, n)
    if (!is.na(op)) parsed$header$opening_balance <- op
    if (!is.na(cl)) parsed$header$closing_balance <- cl
  }
  list(parsed = parsed, tx = tx, page = page)
}

# .ar_settle(tx, rd, cells, decimal, anchors, aligned) -- the reader's rows put in
# this reading's terms (shared by every kind of statement):
#   * on a card or loan, plain figures are what is owed, so they flip (balances
#     first, then the amounts that were read);
#   * an unsigned column takes the sign the balance settled for each row (flagged);
#   * a 0.00 printed in the money-out column stays a money-out zero;
#   * an amount filled from the balance is recomputed from THIS reading's balances
#     and anchors (the table reader assumes oldest first and takes its opening
#     balance from the metadata, which may be another figure).
# `aligned`: the rows are exactly the model's rows, one for one, so per-row facts
# from the model (settled signs, which column a figure sat in) apply.
.ar_settle <- function(tx, rd, cells, decimal, anchors, aligned) {
  n <- nrow(tx)
  if (!n) return(tx)
  f <- tx$flags; f[is.na(f)] <- ""
  derived <- grepl("amount_from_balance", f, fixed = TRUE)
  if (rd$b > 0L && isTRUE(rd$liab)) {
    v <- .num(tx$balance_raw, decimal)
    tx$balance <- ifelse(is.na(v), tx$balance, .ar_sem(v, .ar_sign_kind(tx$balance_raw), TRUE))
  }
  if (any(rd$roles == "amount") && isTRUE(rd$liab)) {
    v <- .num(tx$amount_raw, decimal)
    tx$amount <- ifelse(is.na(v) | derived, tx$amount, .ar_sem(v, .ar_sign_kind(tx$amount_raw), TRUE))
  }
  if (identical(rd$conv, "B") && aligned && !is.null(rd$chain)) {
    sg <- rd$chain$signs
    tx$amount <- ifelse(!is.na(sg) & !is.na(tx$amount) & !derived, sg * abs(tx$amount), tx$amount)
    f <- .ar_addflag(f, !is.na(sg) & !derived, "sign_from_balance")
  }
  dj <- which(rd$roles == "debit"); cj <- which(rd$roles == "credit")
  if (length(dj) && aligned) {
    z <- !is.na(tx$amount) & tx$amount == 0 & !is.na(cells[, dj]) &
      (if (length(cj)) is.na(cells[, cj]) else TRUE)
    # Negated at run time: the byte compiler folds a literal -0 into 0.
    tx$amount[z] <- -abs(tx$amount[z])
  }
  if (any(derived)) {
    op <- .ar_opening_value(anchors, rd, decimal, n)
    prev <- if (identical(rd$dir, "new")) c(tx$balance[-1], op) else c(op, tx$balance[-n])
    tx$amount[derived] <- round(tx$balance[derived] - prev[derived], 2)
  }
  tx$direction <- .direction(tx$amount)
  tx$flags <- f
  tx
}

.ar_addflag <- function(f, cond, tok) ifelse(cond, ifelse(nzchar(f), paste0(f, ",", tok), tok), f)

# The opening / closing balance this reading uses: an in-table anchor first, then
# a summary box. NA when the statement prints neither.
.ar_opening_value <- function(anchors, rd, decimal, n) .ar_end_value(anchors, rd, decimal, "open", n)
.ar_closing_value <- function(anchors, rd, decimal, n) .ar_end_value(anchors, rd, decimal, "close", n)
.ar_end_value <- function(anchors, rd, decimal, cls, n) {
  pts <- .ar_anchor_points(anchors, n, rd$dir, isTRUE(rd$liab), rd$b, decimal)
  want <- if (cls == "open") 0 else n
  v <- pts$val[pts$pos == want & startsWith(pts$src, cls)]
  if (!length(v)) return(NA_real_)
  if (cls == "open") v[1] else v[length(v)]
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
  # Every page with transaction-shaped lines gave rows: lines with a date and a
  # figure, or -- a page whose dates did not read at all -- three or more lines
  # that each print two figures standing apart.
  figure_lines <- function(pg) {
    m <- pg$ph[pg$ph$kind == "money" & pg$ph$standalone, , drop = FALSE]
    cnt <- table(m$line); ln <- as.integer(names(cnt)[cnt >= 2L])
    sum(!pg$lines$summary[match(ln, pg$lines$line)] & !pg$lines$footer[match(ln, pg$lines$line)])
  }
  shaped <- which(vapply(model$pgs, function(pg) !is.null(pg) &&
                           (length(.ar_seed_lines(pg)) > 0L || figure_lines(pg) >= 3L), logical(1)))
  miss <- setdiff(shaped, unique(post$page))
  add("pages_with_rows", !length(miss), if (!length(miss)) "Every page with transactions gave rows."
      else sprintf("Page %s prints transaction lines but gave no rows.", paste(miss, collapse = ", ")))
  wu <- .ar_words_used(model, tpl)
  add("words_used_once", wu$ok, wu$why)
  # Every line shaped like a transaction (a date with a figure beside it) is a row
  # or a summary line: one left outside the table would be a row silently lost.
  stray <- unlist(lapply(model$pgs, function(pg) {
    if (is.null(pg)) return(NULL)
    used <- c(model$rows$line[model$rows$page == pg$page],
              vapply(Filter(function(a) a$page == pg$page, model$anchors), function(a) a$line, 0))
    s <- setdiff(.ar_seed_lines(pg), used)
    if (length(s)) sprintf("page %d (\"%s\")", pg$page, substr(pg$lines$raw[match(s[1], pg$lines$line)], 1, 40))
  }))
  add("lines_accounted", !length(stray), if (!length(stray)) "Every dated line with a figure is a row or a summary line."
      else sprintf("A dated line with a figure on %s is not part of the table.", stray[1]))
  # A line printing a date where the dates run, inside the table or just around it,
  # that is neither a row nor a summary line takes its date away with it: the rows
  # beside it then carry or borrow the wrong one.
  dl <- .ar_dated_lines_left(model)
  add("dated_lines_used", !length(dl), if (!length(dl)) "Every line with a date in the date column is a row or a summary line."
      else sprintf("The dated line \"%s\" on page %d is not part of any row.", substr(dl[[1]]$raw, 1, 40), dl[[1]]$page))
  pl <- .ar_page_labels_ok(ctx$pages_text, ctx$np)
  if (!isFALSE(pl$ok) && .ar_carried_off_end(model))
    pl <- list(ok = FALSE, why = "The table ends by carrying its balance forward to a page that is not in the file.")
  add("pages_complete", pl$ok, pl$why)
  ds <- .ar_dates_settled(model$rows$date, model$rows$date_fmts, tpl$table$date_format, rd$dir)
  add("dates_settled", ds, if (ds) "The dates read one way only."
      else "The dates read as day-month and as month-day equally well.")
  ar <- .ar_arith_checks(tx, post$page, model$anchors, rd, rl, basis, ctx$decimal, ctx$md,
                         two_dates = !is.null(model$date2), strict = any(ctx$ocr))
  # Checked after the arithmetic, so a broken balance is the reason given when it
  # is the cause.
  ra <- .ar_reader_agrees(model, rd, tx)
  cd <- .ar_carried_dates_ok(tx, post$page)
  ar$checks <- c(ck, ar$checks, list(reader_agrees = list(ok = ra$ok, why = ra$why),
                                     dates_carried = list(ok = cd$ok, why = cd$why)))
  ar
}

# .ar_dates_settled(dates, fmts, chosen, dir) -- FALSE when another date format
# reads every printed date too, gives different dates, and those run in order just
# as well ("03/04" is 3 April or 4 March): then the order the voting picked is a
# guess, and a guessed date is never proven.
.ar_dates_settled <- function(dates, fmts, chosen, dir) {
  has <- !is.na(dates) & nzchar(fmts)
  if (!any(has)) return(TRUE)
  cand <- Reduce(intersect, strsplit(fmts[has], "|", fixed = TRUE))
  alt <- setdiff(cand, chosen)
  if (!length(alt) || !(chosen %in% cand)) return(TRUE)
  rd_one <- function(f) {
    yl <- !grepl("%[Yy]", f)
    iso <- if (yl) parse_date(paste(dates[has], "2000"), paste(f, "%Y"))$iso else parse_date(dates[has], f)$iso
    as.Date(iso)
  }
  inord <- function(d) !anyNA(d) && (if (identical(dir, "new")) all(diff(d) <= 0) else all(diff(d) >= 0))
  d0 <- rd_one(chosen)
  for (f in alt) {
    d1 <- rd_one(f)
    if (!identical(d0, d1) && inord(d1)) return(FALSE)
  }
  TRUE
}

# .ar_dated_lines_left(model) -- lines with a date where the date column runs, from
# two line pitches above each page's table to just below it, that are neither a
# row nor a summary line. Each as list(page, raw).
.ar_dated_lines_left <- function(model) {
  out <- list()
  for (pg in Filter(Negate(is.null), model$pgs)) {
    rg <- model$regions[[as.character(pg$page)]]
    if (is.null(rg) || !nrow(pg$ph)) next
    used <- c(model$rows$line[model$rows$page == pg$page],
              vapply(Filter(function(a) a$page == pg$page, model$anchors), function(a) a$line, 0))
    near <- pg$lines$line[pg$lines$y >= rg$y0 - 2.5 * rg$pitch & pg$lines$y <= rg$y1 + 1.5 * rg$pitch]
    d <- pg$ph[pg$ph$kind == "date" & abs(pg$ph$x - model$shift[pg$page] - model$dcol$x) <= model$tol &
               pg$ph$line %in% setdiff(near, used), , drop = FALSE]
    if (nrow(d)) out[[length(out) + 1L]] <- list(page = pg$page, raw = pg$lines$raw[match(d$line[1], pg$lines$line)])
  }
  out
}

# .ar_reader_agrees(model, rd, tx) -- the table reader's rows are the column model's
# rows, figure for figure and date for date. The arithmetic proved the model's
# figures; a row the reader assembled differently (a date taken from the line
# below, a figure from another row) is not what was proven.
.ar_reader_agrees <- function(model, rd, tx) {
  n <- nrow(tx); m <- nrow(model$rows)
  if (n != m) return(list(ok = FALSE, why = sprintf("The table reader put together %d row(s) where the columns show %d.", n, m)))
  der <- grepl("amount_from_balance", tx$flags %||% rep("", n), fixed = TRUE)
  A <- rd$A %||% rep(NA_real_, n)
  same <- (is.na(A) & is.na(tx$amount)) | (!is.na(A) & !is.na(tx$amount) & abs(A - tx$amount) < PARAM_MONEY_TOL)
  # An unsigned figure the balance could not sign has no proven amount to agree
  # with; the sign check holds it back.
  if (identical(rd$conv, "B")) same <- same | is.na(A)
  bad <- which(!der & !same)
  if (length(bad)) return(list(ok = FALSE, why = sprintf("The table reader read row %d's amount differently from the columns.", bad[1])))
  # The model's date phrase (a weekday printed before it aside) must be the date
  # the reader read on that row, and a row the model saw no date on has none.
  md <- vapply(strsplit(ifelse(is.na(model$rows$date), "", model$rows$date), "\\s+"), function(tk)
    paste(tk[nzchar(tk) & !grepl(.AR_WEEKDAY_RX, tolower(tk), perl = TRUE)], collapse = " "), "")
  dr <- gsub("\\s+", " ", trimws(ifelse(is.na(tx$date_raw), "", as.character(tx$date_raw))))
  dbad <- which(nzchar(md) != nzchar(dr) | (nzchar(md) & !mapply(grepl, md, dr, MoreArgs = list(fixed = TRUE))))
  if (length(dbad)) return(list(ok = FALSE, why = sprintf("The table reader read row %d's date differently from the columns.", dbad[1])))
  list(ok = TRUE, why = "The table reader's rows are the columns' rows, figure for figure and date for date.")
}

# .ar_carried_dates_ok(tx, page) -- a date is carried down only on a layout that
# prints each day's date once. A layout that prints the same date on two rows in a
# row of one page prints every row's date, so a row there without one is a date
# lost (an OCR miss), not one to carry.
.ar_carried_dates_ok <- function(tx, page) {
  n <- nrow(tx)
  car <- grepl("date_carried", tx$flags %||% rep("", n), fixed = TRUE)
  if (!any(car) || n < 2L) return(list(ok = NA, why = "No date is carried down."))
  pg <- if (length(page) == n) page else rep(1L, n)
  rep_print <- any(!car[-1] & !car[-n] & pg[-1] == pg[-n] & tx$date[-1] == tx$date[-n], na.rm = TRUE)
  if (rep_print) return(list(ok = FALSE, why = sprintf("Row %d has no date, but this statement prints a date on every row.", which(car)[1])))
  list(ok = TRUE, why = sprintf("%d row(s) take the date printed above them (one date per day).", sum(car)))
}

# .ar_page_labels_ok(pages, np) -- "Page k of N" printed on the pages must run
# without a gap: a page missing from the file (first, last or between) is caught
# here when the rows either side of it still add up. A bundle's statements each
# number from 1. NA when no page prints such a label.
.ar_page_labels_ok <- function(pages, np) {
  rx <- "(?i)\\bpage\\s*:?\\s*([0-9]{1,3})\\s*(?:of|/)\\s*([0-9]{1,3})\\b"
  lab <- lapply(seq_along(pages), function(p) {
    m <- regmatches(pages[p], regexec(rx, pages[p], perl = TRUE))[[1]]
    if (length(m) == 3L) c(p, as.integer(m[2]), as.integer(m[3])) else NULL
  })
  lab <- do.call(rbind, Filter(Negate(is.null), lab))
  if (is.null(lab)) return(list(ok = NA, why = "No page prints its page number."))
  bad <- function(why) list(ok = FALSE, why = why)
  if (any(lab[, 2] < 1L | lab[, 2] > lab[, 3])) return(bad(sprintf("Page %d is numbered %d of %d.", lab[1, 1], lab[1, 2], lab[1, 3])))
  if (lab[1, 2] > lab[1, 1]) return(bad(sprintf("The file starts at the statement's page %d: page(s) before it are missing.", lab[1, 2])))
  for (i in seq_len(nrow(lab))[-1]) {
    e <- lab[i - 1L, 2] + (lab[i, 1] - lab[i - 1L, 1])
    ok <- if (e <= lab[i - 1L, 3]) lab[i, 2] == e && lab[i, 3] == lab[i - 1L, 3] else lab[i, 2] == 1L
    if (!ok) return(bad(sprintf("Page %d of the file is numbered %d of %d: a page is missing before it.", lab[i, 1], lab[i, 2], lab[i, 3])))
  }
  L <- nrow(lab)
  if (lab[L, 2] + (np - lab[L, 1]) < lab[L, 3])
    return(bad(sprintf("The last page is numbered %d of %d: page(s) after it are missing.", lab[L, 2], lab[L, 3])))
  list(ok = TRUE, why = "The pages are all there, numbered without a gap.")
}

# .ar_carried_off_end(model) -- the table's last summary line carries the balance
# forward ("Balance carried forward") after the last row: the next page is not in
# the file, even when no page prints its number.
.ar_carried_off_end <- function(model) {
  a <- Filter(function(a) isTRUE(a$in_table), model$anchors)
  if (!length(a)) return(FALSE)
  last <- a[[length(a)]]
  identical(last$class, "close") && last$before_rows >= nrow(model$rows) &&
    grepl(.AR_CARRY_RX, .ar_norm_label(last$label), perl = TRUE)
}

# .ar_arith_checks(tx, page, anchors, rd, rl, basis, decimal, md, two_dates, strict)
# -- the checks every kind of statement shares, on the rows as finally read: the
# balance chain, opening + movements = closing, printed totals, dates, signs,
# derived amounts and uniqueness. `strict` (a scan) allows no row outside a step.
.ar_arith_checks <- function(tx, page, anchors, rd, rl, basis, decimal, md, two_dates = FALSE,
                             strict = FALSE) {
  n <- nrow(tx)
  ck <- list()
  add <- function(name, ok, why) ck[[name]] <<- list(ok = ok, why = why)
  bal <- if (rd$b > 0L) tx$balance else rep(NA_real_, n)
  apts <- .ar_anchor_points(anchors, n, rd$dir, isTRUE(rd$liab), rd$b, decimal)
  ch <- .ar_chain(tx$amount, rep(FALSE, n), bal, apts, rd$dir)
  sc <- .ar_chain_score(ch)
  row_steps <- rd$b > 0L && sum(!is.na(bal)) >= 1L
  proof_kind <- if (sc[["links"]] == 0) "none" else if (row_steps) "chain" else "totals"
  where_row <- function(r) if (length(page) >= r && !is.na(page[r])) sprintf("row %d (page %d)", r, page[r]) else sprintf("row %d", r)
  if (sc[["links"]] == 0) {
    add("balance_chain", NA, "No running balance and no opening and closing balance are printed, so nothing can be added up.")
  } else {
    bad <- which(!(ch$steps$ok %in% TRUE))
    why <- if (!length(bad)) sprintf("All %d balance step(s) hold to the cent.", sc[["links"]]) else {
      s1 <- ch$steps[bad[1], ]
      rows <- if (s1$to > s1$from) ch$ord[(s1$from + 1):s1$to] else integer(0)
      where <- if (length(rows)) where_row(rows[1]) else "a page break"
      if (s1$unknown > 0) sprintf("A balance step at %s cannot be checked: an amount in it was not read.", where)
      else sprintf("The balance does not add up at %s: it moves by %s but the rows add to %s.", where,
                   format(s1$expected, nsmall = 2), format(round(s1$got, 2), nsmall = 2))
    }
    add("balance_chain", !length(bad), why)
  }
  # The chain continues across every page break.
  pg_first <- if (length(page) == n && n) vapply(split(seq_len(n), page), function(ix) ix[1], 0L)[-1] else integer(0)
  if (proof_kind == "chain" && length(pg_first)) {
    gap <- setdiff(pg_first, ch$covered)
    add("chain_across_pages", !length(gap), if (!length(gap)) "The balance carries across every page break."
        else sprintf("The balance does not carry across the break before row %d.", gap[1]))
  } else add("chain_across_pages", NA, "One page, or no running balance.")
  # Printed opening + movements = printed closing.
  op <- .ar_opening_value(anchors, rd, decimal, n); cl <- .ar_closing_value(anchors, rd, decimal, n)
  if (isTRUE(ch$sections > 1L)) {
    add("opening_closing", NA, sprintf("%d accounts, each checked from its own opening to its own closing balance.", ch$sections))
  } else if (!is.na(op) && !is.na(cl) && !anyNA(tx$amount)) {
    ok <- abs(round(op + sum(tx$amount) - cl, 2)) < PARAM_MONEY_TOL
    add("opening_closing", ok, if (ok) "Opening balance plus every movement equals the closing balance."
        else sprintf("Opening %s plus the movements (%s) is %s, not the printed closing %s.", format(op, nsmall = 2),
                     format(round(sum(tx$amount), 2), nsmall = 2), format(round(op + sum(tx$amount), 2), nsmall = 2), format(cl, nsmall = 2)))
  } else add("opening_closing", NA, "The statement does not print both an opening and a closing balance.")
  tt <- .ar_totals_ok(anchors, tx, page, rd, decimal)
  add("printed_totals", tt$ok, tt$why)
  # Dates: readable, in order, inside the period.
  d <- suppressWarnings(as.Date(tx$date))
  add("dates_readable", !anyNA(d), if (!anyNA(d)) "Every date reads." else sprintf("%d date(s) could not be read.", sum(is.na(d))))
  if (two_dates) add("dates_in_order", NA, "Two date columns: the transaction dates need not be in order.")
  else if (!anyNA(d)) {
    # Each account section of a combined statement runs in order on its own.
    tpos <- if (identical(rd$dir, "new")) n - seq_len(n) + 1L else seq_len(n)
    sec <- 1L + vapply(tpos, function(t) sum(ch$breaks < t), 0L)
    back <- vapply(seq_len(n)[-1], function(i) sec[i] == sec[i - 1L] &&
                     (if (identical(rd$dir, "new")) d[i] > d[i - 1L] else d[i] < d[i - 1L]), logical(1))
    ok <- !any(back)
    add("dates_in_order", ok, if (ok) "The dates run in order." else
        sprintf("The dates go backwards at row %d.", which(back)[1] + 1L))
  } else add("dates_in_order", NA, "Not every date reads.")
  # Inside the printed period: a bundle of statements prints one period per
  # statement, and a row may fall in any of them.
  per <- md$periods %||% list(c(md$period_start %||% NA, md$period_end %||% NA))
  per <- Filter(function(pp) !is.na(pp[1]) && !is.na(pp[2]) && pp[2] >= pp[1],
                lapply(per, function(pp) c(.plausible_period_date(pp[1]), .plausible_period_date(pp[2]))))
  if (length(per) && !anyNA(d)) {
    slack <- if (two_dates) 45 else 0
    inside <- Reduce(`|`, lapply(per, function(pp) d >= pp[1] - slack & d <= pp[2]))
    out <- which(!inside)
    add("dates_in_period", !length(out), if (!length(out)) "Every date is inside the statement period."
        else sprintf("Row %d is dated outside the statement period.", out[1]))
  } else add("dates_in_period", NA, "No statement period was read.")
  if (identical(rd$conv, "B")) {
    uns <- sum(is.na(rd$chain$signs[seq_len(n)]) & !is.na(tx$amount))
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
  # Every row is proven by a step (or by the totals). A text PDF may leave its
  # FIRST row outside the chain when no opening balance is printed before it (its
  # figure is printed text, and the next step starts from its balance); never any
  # other row, and never on a scan, where OCR can misread a digit and only a step
  # would show it. A row after the last printed balance is unproven, full stop.
  unc <- setdiff(seq_len(n), ch$covered)
  if (proof_kind == "totals" && isTRUE(ck$opening_closing$ok)) unc <- integer(0)
  first <- ch$ord[1]
  no_open <- !nrow(apts) || !any(apts$pos == 0)
  if (!strict && proof_kind == "chain" && sc[["held"]] >= 2 && no_open && identical(unc, first)) unc <- integer(0)
  add("rows_proven", !length(unc), if (!length(unc)) "Every row is inside a step that adds up."
      else sprintf("%d row(s) are outside every balance step (row %d first).", length(unc), unc[1]))
  list(checks = ck, chain = ch, proof_kind = proof_kind, score = sc)
}

# .ar_totals_ok(anchors, tx, page, rd, decimal) -- printed totals agree with the rows: each
# total must equal its side's movements over the whole statement, over its own page,
# or over every page up to its own.
.ar_totals_ok <- function(anchors, tx, page, rd, decimal) {
  tots <- Filter(function(a) identical(a$class, "total"), anchors)
  if (!length(tots) || anyNA(tx$amount)) return(list(ok = NA, why = "No printed totals to check."))
  deb <- function(ix) round(sum(-tx$amount[ix][tx$amount[ix] < 0]), 2)
  crd <- function(ix) round(sum(tx$amount[ix][tx$amount[ix] > 0]), 2)
  n <- nrow(tx)
  # A total adds up the whole statement, its own page, everything printed before
  # it, or its own section (the rows since the total before it).
  scopes <- function(a, from) {
    s <- list(seq_len(n), which(page == a$page))
    if (a$in_table) s <- c(s, list(seq_len(min(n, a$before_rows))), list(which(page <= a$page)),
                           list(seq_len(n)[seq_len(n) > from & seq_len(n) <= a$before_rows]))
    s
  }
  checked <- 0L
  from <- 0L
  for (a in tots) {
    sec_from <- from
    if (a$in_table) from <- a$before_rows
    side <- .ar_total_side(a$label, isTRUE(rd$liab))
    if (!nzchar(side)) next
    want <- list()
    dj <- which(rd$roles == "debit"); cj <- which(rd$roles == "credit")
    if (side == "both") {
      if (length(dj) && !is.na(a$figs[dj])) want$debit <- abs(.num(a$figs[dj], decimal))
      if (length(cj) && !is.na(a$figs[cj])) want$credit <- abs(.num(a$figs[cj], decimal))
    } else if (isTRUE(a$n_money == 1L)) want[[side]] <- abs(.num(a$value_text, decimal))
    for (sd in names(want)) {
      if (is.na(want[[sd]])) next
      f <- if (sd == "debit") deb else crd
      hit <- any(vapply(scopes(a, sec_from), function(ix) abs(f(ix) - want[[sd]]) < PARAM_MONEY_TOL, logical(1)))
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
    # A mark with no letter, digit or sign in it (an OCR speck, a stray colon)
    # carries nothing a cell could lose.
    mark <- !grepl("[A-Za-z0-9()+-]", w$text)
    stray <- w[!mark & ((w$cx < lo & w$cx >= lo - 40) | (w$cx > hi & w$cx <= hi + 40)), , drop = FALSE]
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
    if (length(keys) > 1L)
      return(.ar_finish(pick, "check", sprintf("%d different readings each pass every check, so the reading is not unique.", length(keys)),
                        cdf, NULL))
    # A layout's conventions settle what this page cannot (which way a card runs)
    # only once the layout itself is proven: a statement that does not prove itself
    # is never converted on a provisional layout's word.
    own <- Filter(function(cd) !startsWith(cd$source, "layout:") ||
                    isTRUE(.ar_layout_ref_info(sub("^layout:", "", cd$source), layouts)$proven), passed)
    if (!length(own))
      return(.ar_finish(pick, "check", sprintf("Only the provisional layout %s reads it this way; it is not proven yet.",
                                               sub("^layout:", "", pick$source)), cdf, NULL))
    pick <- if (isTRUE(content$passed)) content else own[[1]]
    return(.ar_finish(pick, "proven", pick$why, cdf, .ar_match_layout(pick, layouts)))
  }
  # No running balance and no printed totals: a proven layout handed in may carry
  # it, when this document matches the layout (same design, same conventions) and
  # nothing contradicts it -- no heading names a column the other way, the content
  # reading (when the headings chose it) reads the same figures, and every layout
  # that qualifies reads the same figures.
  lm <- Filter(function(cd) {
    if (!startsWith(cd$source, "layout:") || is.null(cd$tx) || !nrow(cd$tx) || !.ar_layout_match_ok(cd)) return(FALSE)
    info <- .ar_layout_ref_info(sub("^layout:", "", cd$source), layouts)
    ht <- list(unlist(cd$signature$heading_tokens), unlist(info$sig$heading_tokens))
    same_words <- !length(ht[[1]]) || !length(ht[[2]]) ||
      length(intersect(ht[[1]], ht[[2]])) >= 0.5 * length(union(ht[[1]], ht[[2]]))
    isTRUE(info$proven) && .ar_sig_match(cd$signature, info$sig) && same_words &&
      !.ar_heading_contradicts(cd$hroles, cd$rd$roles)
  }, cands)
  if (length(lm) && length(unique(vapply(lm, key, ""))) == 1L) {
    cd <- lm[[1]]
    ref <- sub("^layout:", "", cd$source)
    voted <- isTRUE(content$basis == "heading") && !is.null(content$tx) && nrow(content$tx)
    if (!voted || identical(key(content), key(cd)))
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

# .ar_heading_contradicts(hroles, roles) -- a heading over a column names the
# other side of the money (or the balance) from the role a reading gives it.
.ar_heading_contradicts <- function(hroles, roles) {
  if (is.null(hroles) || length(hroles) != length(roles)) return(FALSE)
  side <- c("debit", "credit", "balance")
  any(!is.na(hroles) & hroles %in% side & roles %in% c(side, "amount") & hroles != roles &
        !(roles == "amount" & hroles %in% c("debit", "credit")))
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
# without the block is a proven reading's own template. Its conventions come from
# the template's `auto` block (roles, sign convention, card or loan, order), which
# the signature alone does not carry.
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
  # A retired layout is kept on file but never read against a statement again.
  if (identical(status, "retired")) return(NULL)
  list(ref = paste0(id, "@", ver), kind = sig$kind %||% "pdf", roles = unlist(roles), conv = conv,
       liab = isTRUE(au$liab), dir = if (isTRUE(sig$newest_first)) "new" else (au$dir %||% "old"),
       proven = status %in% c("proven", "confirmed"), sig = sig)
}

# .ar_layout_ref_info(ref, layouts) -- what the layout handed in as "<id>@<version>"
# says, or NULL.
.ar_layout_ref_info <- function(ref, layouts) {
  for (ly in layouts) {
    info <- .ar_layout_info(ly)
    if (!is.null(info) && identical(info$ref, ref)) return(info)
  }
  NULL
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
