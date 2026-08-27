# draft.R -- "generate as much of the template as possible" from a single file, so an accountant
# who hits an unsupported statement only has to CONFIRM, not build.

.draft_id <- function(path) gsub("[^a-z0-9]+", "_", tolower(tools::file_path_sans_ext(basename(path))))

.slug <- function(s) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(s %||% ""))))

.compose_id <- function(bank, type, suffix, fallback) {
  parts <- Filter(nzchar, c(.slug(bank), .slug(type)))
  if (!length(parts) || identical(.slug(bank), "new_bank"))
    parts <- Filter(nzchar, .slug(fallback))
  out <- paste(c(parts, suffix), collapse = "_")
  if (!nzchar(.slug(out %||% ""))) out <- paste0("statement_", suffix)
  out
}

# The bank NAME to pre-fill, read from what the document says about ITSELF, because it is saved into
# the template: walking a constant in declaration order answered "ANZ" on a real ASB statement,
# because ANZ was one of the payees. In order of how much the document asserts: MASTHEAD, ISSUER'S
# IMPRINT, SOLE MENTION, then nothing decisive.
.KNOWN_BANKS <- c(anz = "ANZ", asb = "ASB", bnz = "BNZ", kiwibank = "Kiwibank",
  westpac = "Westpac", tsb = "TSB", sbs = "SBS", cooperative = "Co-operative Bank",
  "co-operative" = "Co-operative Bank", heartland = "Heartland", rabobank = "Rabobank",
  "the co-operative" = "Co-operative Bank")

.BANK_IMPRINT_RX <- "bank|banking|limited|ltd|new zealand"
# Institution words that make a masthead line a BANK's NAME rather than a product or a person.
# "Banking" is deliberately absent: it turned a guide titled "Internet Banking" into a bank.
.BANK_INSTITUTION_RX <- "\\b(bank|bancorp|credit union|building society)\\b"

.banks_named <- function(txt, template = "\\b%s\\b") {
  if (!length(txt) || !nzchar(txt)) return(character(0))
  hit <- vapply(names(.KNOWN_BANKS), function(kw)
    grepl(sprintf(template, kw), txt, perl = TRUE), logical(1))
  unique(unname(.KNOWN_BANKS[hit]))
}

# The TOP of page one as lines, taken from the word boxes because the text layer's line ORDER is not
# reliable - on the ASB statement the top-right account block comes out first.
.masthead_lines <- function(input) {
  wl <- input$words %||% list()
  w <- if (length(wl)) wl[[1]] else NULL
  if (!is.null(w) && nrow(w)) {
    ph <- suppressWarnings(as.numeric((input$page_height %||% NA)[1]))
    if (!isTRUE(ph > 0)) ph <- 841.89
    w <- as.data.frame(w, stringsAsFactors = FALSE)
    w <- w[w$y <= ph * 0.25, , drop = FALSE]
    if (nrow(w)) {
      w <- w[order(w$y, w$x), , drop = FALSE]
      # Capped at the first few rows as well as the top quarter: on a compact one-page statement the
      # transaction table can start inside the top quarter, and a payee there reads as a masthead.
      return(unname(utils::head(vapply(split(seq_len(nrow(w)), .group_rows(w$y, PARAM_PDF_ROW_TOL)),
        function(ix) paste(w$text[ix], collapse = " "), character(1)), 8L)))
    }
  }
  ln <- trimws(unlist(strsplit((input$pages %||% character(0))[1] %||% "", "\n", fixed = TRUE)))
  utils::head(ln[nzchar(ln)], 12L)
}

.BANK_NAME_SUFFIX <- "limited|ltd|nz|new zealand|group|corporation|corp|inc|plc"

# A masthead line that names an INSTITUTION, trimmed to the name itself. Short, wordy-not-numeric and
# not a person. A line that BEGINS with the institution word is the document's title, not a bank.
.masthead_bank_name <- function(lines) {
  for (ln in lines) {
    ln <- trimws(gsub("\\s+", " ", ln))
    w <- unlist(regmatches(ln, gregexpr("[A-Za-z][A-Za-z'&.-]*", ln)))
    if (length(w) < 2L || length(w) > 6L || nchar(ln) > 40L) next
    if (grepl("[0-9]", ln) || isTRUE(.fp_has_pii(ln))) next
    m <- regexpr(.BANK_INSTITUTION_RX, tolower(ln), perl = TRUE)
    if (m < 0L) next
    if (m == 1L) {
      if (grepl("^[a-z]+\\s+of\\s+[a-z]", tolower(ln))) return(ln)
      next
    }
    end <- m + attr(m, "match.length") - 1L
    rest <- substr(ln, end + 1L, nchar(ln))
    suf <- regexpr(paste0("^(?:[[:space:],]+(?:", .BANK_NAME_SUFFIX, ")\\b)+"),
                   tolower(rest), perl = TRUE)
    return(trimws(substr(ln, 1L, end + if (suf > 0L) attr(suf, "match.length") else 0L)))
  }
  ""
}

.sniff_bank <- function(input, fallback = "New bank") {
  txt <- tolower(paste(unlist(c(input$text, input$pages,
    if (!is.null(input$table)) names(input$table))), collapse = " "))
  mast <- safe(.masthead_lines(input), character(0))
  named <- .banks_named(tolower(paste(mast, collapse = " ")))
  if (length(named) == 1L) return(named)
  if (!length(named)) {
    own <- safe(.masthead_bank_name(mast), "")
    if (nzchar(own)) return(own)
  }
  imprint <- .banks_named(txt, paste0("\\b%1$s\\b[[:space:],]*(?:", .BANK_IMPRINT_RX,
                                      ")\\b|\\b%1$s\\.(?:co\\.)?nz\\b"))
  if (length(imprint) == 1L) return(imprint)
  everywhere <- .banks_named(txt)
  if (length(everywhere) == 1L) return(everywhere)
  fallback
}

# `cells` are the date CELLS the drafted band actually produces, so the format is chosen from the
# very strings the reader will hand to parse_date; the old every-page scan read the address block.
.guess_pdf_date_format <- function(input, band, cells = NULL) {
  toks <- trimws(as.character(cells %||% character(0)))
  toks <- toks[nzchar(toks)]
  if (!length(toks)) {
    wl <- input$words %||% list(); if (!length(wl)) return("%d/%m/%Y")
    for (w in wl) {
      if (is.null(w) || !nrow(w)) next
      w <- as.data.frame(w, stringsAsFactors = FALSE)
      cx <- w$x + w$width / 2
      toks <- c(toks, w$text[cx >= band$x_min & cx <= band$x_max])
    }
    toks <- trimws(toks[nzchar(toks)])
    if (!length(toks)) return("%d/%m/%Y")
  }
  f <- detect_date_format(toks)
  if (nzchar(f)) return(f)
  joined <- paste(utils::head(toks, 30), collapse = " ")
  if (grepl("[0-9]{1,2}\\s+[A-Za-z]{3,9}", joined) && !grepl("[0-9]{1,2}[/.-][0-9]", joined))
    return("%d %b")
  "%d/%m/%Y"
}

# THE CURRENCY IS ON THE MONEY, NOT IN A CONSTANT: every drafted template used to say NZD, so on a
# pound statement the tiles totalled pounds as dollars. The glyphs are \u escapes because on a
# C-locale box a pound sign in a .R file is two bytes of unknown encoding, and two parallel vectors
# not one named vector because an argument tag becomes a symbol held in the NATIVE encoding.
.CUR_GLYPH     <- c("$",      "\u00a3", "\u20ac", "\u00a5")
.CUR_OF_GLYPH  <- c("dollar", "GBP",    "EUR",    "JPY")
.CUR_CODES  <- c("NZD", "AUD", "USD", "GBP", "EUR", "JPY")
.CUR_DOLLAR <- c("NZD", "AUD", "USD")
# Deliberately the shape .WA_MONEY calls money, so the currency is read off the very figures the
# drafter matched. .CUR_MARK is an ASCII-only negation, so no glyph is written into a pattern.
.CUR_FIGURE <- "-?[0-9][0-9,]*\\.[0-9]{2}"
.CUR_MARK   <- "[^0-9A-Za-z[:space:].,'()+-]{1,3}"

# Read from the MONEY CELLS the callers pass, never from the description: "VISA PURCHASE USD 25.00" is
# a foreign purchase. A template carries ONE currency, so evidence that disagrees leaves `default`.
.draft_currency <- function(cells, default = "NZD") {
  s <- as.character(cells %||% character(0))
  s <- s[!is.na(s) & nzchar(s)]
  if (!length(s)) return(default)
  # Declare the cells UTF-8 wherever their bytes are valid UTF-8: read.csv under a C locale hands them
  # back UNdeclared, and an undeclared string never matches a declared one.
  enc <- Encoding(s)
  enc[enc == "unknown" & validUTF8(s)] <- "UTF-8"
  Encoding(s) <- enc
  found <- function(rx) unlist(regmatches(s, gregexpr(rx, s)), use.names = FALSE)
  codes <- unique(substr(found(paste0(
    "\\b(?:", paste(.CUR_CODES, collapse = "|"), ")[[:space:]]?-?(?:",
    .CUR_MARK, ")?", .CUR_FIGURE)), 1L, 3L))
  # Matched with .WA_MONEY's own ASCII-only negated class, so what is collected is exactly the symbol
  # the money test accepted.
  gly <- unique(sub(paste0(.CUR_FIGURE, "$"), "", found(paste0(.CUR_MARK, .CUR_FIGURE))))
  gcur <- .CUR_OF_GLYPH[match(gly, .CUR_GLYPH)]
  gcur <- gcur[!is.na(gcur)]
  named <- unique(c(codes, setdiff(gcur, "dollar")))
  if (length(named) != 1L) return(default)            # nothing named, or two
  if ("dollar" %in% gcur && !(named %in% .CUR_DOLLAR)) return(default)
  named
}

# Pin BOTH the indicator column and the token that means a debit, so a drafted template never falls
# back to the blind "D" default, which silently mis-signs a bank writing "d" / "DR" / "Debit".
.draft_type_dc <- function(style, headers, df, cols) {
  if (!identical(style, "type_dc")) return(list(cols = cols, keys = list()))
  tv <- detect_type_dc_values(headers, df)
  if (!is.null(tv$column) && is.null(cols$type)) cols$type <- list(source = tv$column)
  keys <- list()
  if (!is.null(tv$debit))  keys$type_debit_value  <- tv$debit
  if (!is.null(tv$credit)) keys$type_credit_value <- tv$credit
  list(cols = cols, keys = keys)
}

# Pinning all n headers made a single renamed column in a later export drop the match to
# "unsupported". All-but-one, never below 2, tolerates a minor header change while staying distinctive.
.draft_min_score <- function(n) if (n <= 2L) max(1L, n) else as.integer(n - 1L)

# THE shared "guess the template" brain for a tabular source, used by .draft_delimited, .draft_excel
# and column_profile.R's mapping hint, so the drafter and the hint cannot disagree.
.derive_mapping <- function(h, df, date_default = "%d/%m/%Y") {
  mapcol <- function(field) { c <- guess_mapping(h, field); if (identical(c, "(none)")) NULL else c }
  cols <- list(); date_format <- NULL
  dcol <- mapcol("date")
  if (!is.null(dcol)) {
    fmt <- if (!is.null(df)) detect_date_format(df[[dcol]]) else ""
    date_format <- if (nzchar(fmt)) fmt else date_default
    cols$date <- list(source = dcol, format = date_format)
  }
  for (f in c("amount", "description", "particulars", "code", "reference", "type", "other_party", "balance")) {
    cc <- mapcol(f); if (!is.null(cc)) cols[[f]] <- list(source = cc)
  }
  style <- detect_amount_style(h, df)
  if (identical(style, "debit_credit_cols")) {
    dc <- h[grepl("debit|withdrawal|money out|paid out", tolower(h))][1]
    cc <- h[grepl("credit|deposit|money in|paid in", tolower(h))][1]
    if (!is.na(dc)) cols$debit <- list(source = dc)
    if (!is.na(cc)) cols$credit <- list(source = cc)
  }
  tdc <- .draft_type_dc(style, h, df, cols)
  mcell <- unlist(lapply(c("amount", "debit", "credit", "balance"), function(f)
    if (!is.null(cols[[f]]) && !is.null(df)) as.character(df[[cols[[f]]$source]])),
    use.names = FALSE)
  list(cols = tdc$cols, amount_sign = style, keys = tdc$keys, date_format = date_format,
       currency = safe(.draft_currency(mcell), "NZD"))
}

.draft_delimited <- function(path, id, bank) {
  delim <- detect_delimiter(path)
  df <- tryCatch(utils::read.csv(path, sep = if (identical(delim, "\t")) "\t" else delim,
    colClasses = "character", stringsAsFactors = FALSE, check.names = FALSE, nrows = 50L),
    error = function(e) NULL)
  h <- if (!is.null(df)) names(df) else character(0)
  dm <- .derive_mapping(h, df, "%d/%m/%Y")
  out <- list(id = .compose_id(bank, "everyday", "csv", id), bank = bank, statement_type = "everyday", format = "delimited",
    version = 1, min_score = .draft_min_score(length(h)),
    fingerprint = list(header_contains_all = as.list(h)), delimiter = delim,
    columns = dm$cols, amount_sign = dm$amount_sign,
    currency = dm$currency %||% "NZD", origin = "user")
  utils::modifyList(out, dm$keys)
}

.draft_pdf <- function(input, id, bank) {
  # Say so plainly rather than propose a template that reads nothing: a PDF that never prints a dated
  # money line is a guide. safe(TRUE): a failure to run the check must never become a refusal.
  if (!isTRUE(safe(pdf_has_transaction_rows(input), TRUE))) return(NULL)
  sug <- safe(suggest_pdf_columns(input), data.frame())
  cols <- list()
  if (!is.null(sug) && nrow(sug)) {
    # Every band the sniffer returns is already labelled with the role the statement's own heading
    # gives it. Assigning by name collided - four clusters called "amount" left ONE band, silently
    # the last - so a repeated role is a defect, not something to absorb.
    for (i in seq_len(nrow(sug))) cols[[sug$field[i]]] <- list(x_min = sug$x_min[i], x_max = sug$x_max[i])
  }
  style <- if (!is.null(cols$debit) || !is.null(cols$credit)) "debit_credit_cols" else "signed"
  # Skeleton fallback: even when the sniffer recognises nothing, still open the toolkit with starter
  # bands, because a readable PDF must never dead-end with "couldn't read this file".
  page_w <- suppressWarnings(as.numeric((input$page_width %||% NA)[1]))
  if (is.na(page_w) || page_w <= 0) page_w <- 595.28
  if (is.null(cols$date))        cols$date        <- list(x_min = 0, x_max = round(page_w * 0.16))
  if (is.null(cols$description)) cols$description <- list(x_min = round(page_w * 0.17), x_max = round(page_w * 0.55))
  if (style != "debit_credit_cols" && is.null(cols$amount))
    cols$amount <- list(x_min = round(page_w * 0.72), x_max = round(page_w * 0.95))
  date_band <- cols$date
  # Prefer the distinctive multi-word phrases header_phrases returns, and NEVER fall back to a bare
  # generic word: it sits on every statement, so the template would match unseen PDFs.
  fp <- safe(header_phrases(input), character(0))
  ref_w <- suppressWarnings(as.numeric((input$page_width  %||% NA)[1]))
  ref_h <- suppressWarnings(as.numeric((input$page_height %||% NA)[1]))
  tbl <- list(row_tol = 3,
      date_format = safe(.guess_pdf_date_format(input, date_band,
        attr(sug, "date_cells")), "%d/%m/%Y") %||% "%d/%m/%Y",
      amount_sign = style, columns = cols)
  if (!is.na(ref_w) && ref_w > 0) tbl$ref_width  <- round(ref_w, 2)
  if (!is.na(ref_h) && ref_h > 0) tbl$ref_height <- round(ref_h, 2)
  list(id = .compose_id(bank, "statement", "pdf", id), bank = bank, statement_type = "statement", format = "pdf",
    version = 1, min_score = max(1L, length(fp)),
    fingerprint = list(page_contains_all = as.list(fp)),
    table = tbl, currency = safe(.draft_currency(attr(sug, "money_cells")), "NZD"),
    origin = "user")
}

# read_input has already picked the sheet, skipped any preamble and fixed serial dates, so this
# mirrors the delimited path. NULL is an honest "can't draft".
.draft_excel <- function(input, id, bank) {
  df <- input$table
  if (is.null(df) || !nrow(df) || !ncol(df)) return(NULL)
  h <- names(df)
  dm <- .derive_mapping(h, df, "%Y-%m-%d")
  if (is.null(dm$cols$date)) return(NULL)   # no date column -> not a transaction table
  if (is.null(dm$cols$amount) && is.null(dm$cols$debit)) return(NULL)   # nothing to read money from
  out <- list(id = .compose_id(bank, "everyday", "xlsx", id), bank = bank, statement_type = "everyday", format = "excel",
    version = 1, min_score = .draft_min_score(length(h)),
    fingerprint = list(header_contains_all = as.list(h)),
    columns = dm$cols, amount_sign = dm$amount_sign,
    currency = dm$currency %||% "NZD", origin = "user")
  utils::modifyList(out, dm$keys)
}

draft_template <- function(path, bank = "New bank") {
  input <- tryCatch(read_input(path), error = function(e) NULL)
  if (is.null(input)) return(NULL)
  id <- .draft_id(path)
  bank <- .sniff_bank(input, fallback = bank)
  if (identical(input$kind, "delimited")) return(.draft_delimited(path, id, bank))
  if (identical(input$kind, "pdf"))       return(.draft_pdf(input, id, bank))
  if (identical(input$kind, "excel"))     return(.draft_excel(input, id, bank))
  NULL
}

# The human-facing transactions from a draft, using the same display shaping as the outputs.
# `preview_pages` parses only the first few PDF pages; the real convert on Save does every page.
draft_preview <- function(path, template, preview_pages = NULL) {
  tryCatch({
    input <- read_input(path)
    if (!is.null(preview_pages) && identical(input$kind %||% "", "pdf")) {
      np <- length(input$pages %||% input$words %||% list())
      if (np > preview_pages) input <- .subinput_pages(input, seq_len(preview_pages))
    }
    parsed <- parse_statement(input, template)
    display_transactions(parsed$transactions, parsed$extras)
  }, error = function(e) NULL)
}
