# parse_pdf_table.R -- extract a transaction table from PDF word boxes using a
# declarative `format: pdf` template. Same forensic contract as the delimited
# path: verbatim descriptions, redactions honoured, deterministic, never crashes.
#
# Deliberately simple + generic (no per-statement code):
#   * words are grouped into visual ROWS by y-position (row_tol),
#   * each word is placed in a COLUMN by which x-band its centre falls in,
#   * a row is KEPT only if its date cell parses as a real date -- which cleanly
#     ignores headers, annotations, footers and section-header "gaps".
#   Multi-page tables are stitched by processing every page the same way.

# .pdf_cell(rw, cspec) -- text of the words whose centre falls in a column band,
# left-to-right, space-joined; NA when the band is empty or unmapped.
.pdf_cell <- function(rw, cspec) {
  if (is.null(cspec) || is.null(cspec$x_min) || is.null(cspec$x_max)) return(NA_character_)
  cx <- rw$x + rw$width / 2
  sel <- rw[cx >= cspec$x_min & cx <= cspec$x_max, , drop = FALSE]
  if (!nrow(sel)) return(NA_character_)
  paste(sel$text[order(sel$x)], collapse = " ")
}

# .clean_money(x) -- strip currency symbols / thousands separators, keep sign and
# decimal so parse_amount can read it. The verbatim value is retained separately.
.clean_money <- function(x) gsub(",", "", gsub("[^0-9.,()+-]", "", as.character(x)))

parse_pdf_table <- function(input, template) {
  t <- template$table %||% list()
  cols <- t$columns %||% list()
  extras_cols <- t$extras %||% list()
  region <- t$region %||% list()
  row_tol <- suppressWarnings(as.numeric(t$row_tol %||% 3)); if (is.na(row_tol)) row_tol <- 3
  date_fmt <- t$date_format %||% "%d/%m/%Y"
  style <- t$amount_sign %||% "signed"
  words_by_page <- input$words %||% list()

  recs <- list()
  for (p in seq_along(words_by_page)) {
    w <- words_by_page[[p]]
    if (is.null(w) || !nrow(w)) next
    w <- as.data.frame(w, stringsAsFactors = FALSE)
    if (!is.null(region$x_min)) w <- w[(w$x + w$width) >= region$x_min, , drop = FALSE]
    if (!is.null(region$x_max)) w <- w[w$x <= region$x_max, , drop = FALSE]
    if (!is.null(region$y_min)) w <- w[w$y >= region$y_min, , drop = FALSE]
    if (!is.null(region$y_max)) w <- w[w$y <= region$y_max, , drop = FALSE]
    if (!nrow(w)) next
    w <- w[order(w$y, w$x), , drop = FALSE]
    grp <- cumsum(c(TRUE, diff(w$y) > row_tol))
    for (g in unique(grp)) {
      rw <- w[grp == g, , drop = FALSE]
      rec <- list(page = p,
        date = .pdf_cell(rw, cols$date), description = .pdf_cell(rw, cols$description),
        amount = .pdf_cell(rw, cols$amount), balance = .pdf_cell(rw, cols$balance),
        particulars = .pdf_cell(rw, cols$particulars), code = .pdf_cell(rw, cols$code),
        reference = .pdf_cell(rw, cols$reference), other_party = .pdf_cell(rw, cols$other_party),
        type = .pdf_cell(rw, cols$type),
        raw = paste(rw$text[order(rw$x)], collapse = " "))
      for (ef in names(extras_cols)) rec[[paste0("x.", ef)]] <- .pdf_cell(rw, extras_cols[[ef]])
      recs[[length(recs) + 1L]] <- rec
    }
  }

  # Keep only rows whose date cell parses -> the actual transaction rows.
  keep <- vapply(recs, function(r)
    !is.na(suppressWarnings(parse_date(r$date, date_fmt)$iso)), logical(1))
  recs <- recs[keep]
  n <- length(recs)
  getc <- function(f) if (n == 0) character(0) else
    vapply(recs, function(r) r[[f]] %||% NA_character_, character(1))

  if (n == 0) {
    date_iso <- character(0); date_raw <- character(0); description <- character(0)
    amt <- list(value = numeric(0), direction = character(0), raw = character(0))
  } else {
    d <- parse_date(getc("date"), date_fmt); date_iso <- d$iso; date_raw <- d$raw
    amt_raw <- getc("amount")
    amt <- parse_amount(.clean_money(amt_raw), style, list()); amt$raw <- amt_raw
    description <- clean_description(getc("description"))
  }
  vb <- function(f) if (n == 0) character(0) else blank_to_na(getc(f))
  has_bal <- !is.null(cols$balance)
  balance <- if (n == 0 || !has_bal) rep(NA_real_, n) else parse_amount(.clean_money(getc("balance")), "signed")$value
  balance_raw <- if (n == 0 || !has_bal) rep(NA_character_, n) else getc("balance")

  redacted <- if (n == 0) logical(0) else
    grepl("REDACTED", getc("amount"), ignore.case = TRUE) |
    grepl("REDACTED", getc("description"), ignore.case = TRUE)
  flags <- if (n == 0) character(0) else ifelse(redacted, "redacted", "")
  if (n > 0) amt$value[redacted] <- NA_real_

  core <- coerce_core(data.frame(
    row_id = seq_len(n), date = date_iso, date_raw = date_raw, description = description,
    amount = if (n == 0) numeric(0) else amt$value,
    amount_raw = if (n == 0) character(0) else amt$raw,
    direction = if (n == 0) character(0) else amt$direction,
    balance = balance, balance_raw = balance_raw,
    particulars = vb("particulars"), code = vb("code"), reference = vb("reference"),
    other_party = vb("other_party"), type = vb("type"),
    currency = rep(template$currency %||% "NZD", n), flags = flags,
    stringsAsFactors = FALSE))

  if (length(extras_cols) && n > 0) {
    ex <- list(row_id = seq_len(n))
    for (ef in names(extras_cols)) ex[[ef]] <- blank_to_na(getc(paste0("x.", ef)))
    extras <- data.frame(ex, stringsAsFactors = FALSE, check.names = FALSE)
  } else extras <- data.frame(row_id = integer(0))

  header <- list(
    bank = template$bank %||% NA_character_, statement_type = template$statement_type %||% NA_character_,
    template_id = template$id %||% NA_character_, template_version = template$version %||% NA,
    account_number = NA_character_, account_name = NA_character_,
    period_start = NA_character_, period_end = NA_character_,
    opening_balance = NA_real_, closing_balance = NA_real_,
    currency = template$currency %||% "NZD",
    source_file = basename(input$path), source_sha256 = input$sha256,
    page_count = input$meta$page_count %||% NA_integer_, row_count = n,
    ocr_pages = input$meta$ocr_pages %||% 0L,
    ocr_min_confidence = input$meta$ocr_min_conf %||% NA_real_)

  pages_v <- if (n == 0) integer(0) else vapply(recs, function(r) as.integer(r$page), integer(1))
  provenance <- data.frame(row_id = seq_len(n),
    source_ref = if (n == 0) character(0) else sprintf("pdf:p%d", pages_v),
    raw = getc("raw"), stringsAsFactors = FALSE)

  list(transactions = core, extras = extras, header = header,
       provenance = provenance, source_line_count = NA_integer_)
}
