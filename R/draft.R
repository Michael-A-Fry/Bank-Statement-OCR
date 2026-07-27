# draft.R -- "generate as much of the template as possible" from a single file,
# so an accountant who hits an unsupported statement only has to CONFIRM, not
# build. draft_template() reads the file, auto-detects everything it can, and
# returns a ready template list (origin "user"). The guided UI shows a live
# preview from it and lets the user fix at most one or two things, then Save.

.draft_id <- function(path) gsub("[^a-z0-9]+", "_", tolower(tools::file_path_sans_ext(basename(path))))

# .slug(s) -- a safe lower-snake token for building template ids.
.slug <- function(s) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(s %||% ""))))

# .compose_id(bank, type, suffix, fallback) -- an id that SAYS what the template is
# ("anz_everyday_pdf") instead of a raw filename. Falls back to the filename slug
# when the bank is still the generic placeholder, so an id is always produced.
.compose_id <- function(bank, type, suffix, fallback) {
  parts <- Filter(nzchar, c(.slug(bank), .slug(type)))
  if (!length(parts) || identical(.slug(bank), "new_bank"))
    parts <- Filter(nzchar, .slug(fallback))
  out <- paste(c(parts, suffix), collapse = "_")
  if (!nzchar(.slug(out %||% ""))) out <- paste0("statement_", suffix)
  out
}

# .sniff_bank(input, fallback) -- the bank NAME to pre-fill, read from what the
# document says about ITSELF. Detection still matches on the template's fingerprint
# phrases, never on this; but the name is shown, saved into the template file and
# put in front of the user with a Confirm next to it, so it has to be evidence.
#
# It used to walk .KNOWN_BANKS in declaration order and return the first whole-word
# hit ANYWHERE in the text. On a real ASB statement that answered "ANZ", because
# ANZ is one of the payees in the bill-payment list. A wrong answer the user is
# invited to confirm is worse than no answer, so the order of a constant decides
# nothing any more. In order of how much the document is actually asserting:
#
#   1. THE MASTHEAD. A name at the top of page 1 is the issuer; a name in a payee
#      or transaction line is somebody being paid. One bank named up there wins
#      outright. A masthead that names no bank the tool knows but does print a
#      "<something> Bank" line IS that bank -- which is how a bank nobody has
#      taught it ("Kowhai Bank NZ") gets its own name instead of a payee's.
#   2. THE ISSUER'S IMPRINT. Every statement carries the legal entity, the website,
#      or both ("ASB Bank Limited", "asb.co.nz", "ANZ Bank New Zealand Limited").
#      A payee line never does. One bank imprinted wins.
#   3. A SOLE MENTION -- exactly one known bank named anywhere at all.
#   4. Nothing decisive: the tool does not know, and leaves the caller's fallback
#      alone rather than picking the alphabetically-first candidate.
.KNOWN_BANKS <- c(anz = "ANZ", asb = "ASB", bnz = "BNZ", kiwibank = "Kiwibank",
  westpac = "Westpac", tsb = "TSB", sbs = "SBS", cooperative = "Co-operative Bank",
  "co-operative" = "Co-operative Bank", heartland = "Heartland", rabobank = "Rabobank",
  "the co-operative" = "Co-operative Bank")

# Words a bank puts next to its own name and a payee list never does. "nz" alone is
# deliberately absent: it would make the payee line "Sam BNZ NZ Ltd" an imprint.
.BANK_IMPRINT_RX <- "bank|banking|limited|ltd|new zealand"
# Institution words that make a masthead line a BANK's NAME rather than a product
# or a person. Kept narrow on purpose -- this rung fires only when no known bank is
# named, so a loose match here would invent a bank out of a heading. "Banking" is
# deliberately absent: it is how a heading describes an ACTIVITY, and it turned the
# cover of a how-to guide titled "Internet Banking" into a bank called "Internet
# Banking". A bank's own name says Bank.
.BANK_INSTITUTION_RX <- "\\b(bank|bancorp|credit union|building society)\\b"

# .banks_named(txt) -- the distinct bank NAMES whose keyword appears as a whole
# word in `txt`. Keywords are plain words (the hyphen in "co-operative" is literal
# outside a character class), so no metacharacter escaping is needed.
.banks_named <- function(txt, template = "\\b%s\\b") {
  if (!length(txt) || !nzchar(txt)) return(character(0))
  hit <- vapply(names(.KNOWN_BANKS), function(kw)
    grepl(sprintf(template, kw), txt, perl = TRUE), logical(1))
  unique(unname(.KNOWN_BANKS[hit]))
}

# .masthead_lines(input) -- what is printed at the TOP of page one, as lines. Taken
# from the word boxes (top quarter of the page) because that is literally "the top
# of the page"; the text layer's line ORDER is not reliable -- on the ASB statement
# the top-right account block comes out first and the masthead several lines down.
# Falls back to the first lines of the page text when there are no word boxes.
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
      # Capped at the first few rows as well as the top quarter: on a compact
      # one-page statement the transaction table itself can start inside the top
      # quarter, and a payee named there would be reading a masthead off a
      # transaction line -- the very thing this function exists to stop.
      return(unname(utils::head(vapply(split(seq_len(nrow(w)), .group_rows(w$y, PARAM_PDF_ROW_TOL)),
        function(ix) paste(w$text[ix], collapse = " "), character(1)), 8L)))
    }
  }
  ln <- trimws(unlist(strsplit((input$pages %||% character(0))[1] %||% "", "\n", fixed = TRUE)))
  utils::head(ln[nzchar(ln)], 12L)
}

# Words that belong to a bank's NAME after the institution word ("Bank Limited",
# "Bank New Zealand"). Anything else after it says what the DOCUMENT is, not who
# issued it, so the name stops there: "Dummy Bank Statement" is a statement from
# Dummy Bank, not from a bank called "Dummy Bank Statement".
.BANK_NAME_SUFFIX <- "limited|ltd|nz|new zealand|group|corporation|corp|inc|plc"

# .masthead_bank_name(lines) -- a masthead line that names an INSTITUTION ("Kowhai
# Bank NZ"), trimmed to the name itself, or "". Short, wordy-not-numeric and not a
# person: a statement heading, never an account holder (the same PII rule the
# fingerprint drafter applies). A line that BEGINS with the institution word is the
# document's title ("Bank Statement"), not a bank, and is skipped.
.masthead_bank_name <- function(lines) {
  for (ln in lines) {
    ln <- trimws(gsub("\\s+", " ", ln))
    w <- unlist(regmatches(ln, gregexpr("[A-Za-z][A-Za-z'&.-]*", ln)))
    if (length(w) < 2L || length(w) > 6L || nchar(ln) > 40L) next
    if (grepl("[0-9]", ln) || isTRUE(.fp_has_pii(ln))) next
    m <- regexpr(.BANK_INSTITUTION_RX, tolower(ln), perl = TRUE)
    if (m < 0L) next
    # A line STARTING with the institution word is the document's own title ("Bank
    # Statement") -- unless it is the "Bank of <somewhere>" shape, which is a real
    # bank's real name and is the whole line.
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

# .guess_pdf_date_format(input, band, cells) -- sniff the date column's style.
# `cells` are the date CELLS the drafted band actually produces on the transaction
# page (suggest_pdf_columns measures them while it is there), so the format is
# chosen from the very strings the reader will hand to parse_date. The old
# every-page scan of everything in the same x-range read the address block and the
# account summary as if they were dates and reached "%d %b" by luck.
# Falls back to that scan only when there are no measured cells (a hand-drawn band).
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
  # Ask the shared detector first -- it is the one the delimited path uses, it
  # validates with the reader's own parse_date, and it knows the year-less forms.
  f <- detect_date_format(toks)
  if (nzchar(f)) return(f)
  # Nothing fit every cell (a stray non-date line in the band). Fall back to the
  # shape the majority of them carry: day + month-name, unless a numeric date is
  # what is printed.
  joined <- paste(utils::head(toks, 30), collapse = " ")
  if (grepl("[0-9]{1,2}\\s+[A-Za-z]{3,9}", joined) && !grepl("[0-9]{1,2}[/.-][0-9]", joined))
    return("%d %b")
  "%d/%m/%Y"
}

# .draft_type_dc(style, headers, df, cols) -- when the amount style is a D/C
# indicator column, pin BOTH the indicator column and the token that means a
# debit, so a drafted template never falls back to the blind "D" default (which
# silently mis-signs a bank that writes "d" / "DR" / "Debit"). Returns the
# (possibly-augmented) columns plus the type_debit_value / type_credit_value keys
# to fold onto the template. A no-op for every other style.
.draft_type_dc <- function(style, headers, df, cols) {
  if (!identical(style, "type_dc")) return(list(cols = cols, keys = list()))
  tv <- detect_type_dc_values(headers, df)
  # Map the indicator column when the generic name-match missed it (a header like
  # "debit_credit" doesn't contain "type").
  if (!is.null(tv$column) && is.null(cols$type)) cols$type <- list(source = tv$column)
  keys <- list()
  if (!is.null(tv$debit))  keys$type_debit_value  <- tv$debit
  if (!is.null(tv$credit)) keys$type_credit_value <- tv$credit
  list(cols = cols, keys = keys)
}

# .draft_min_score(n) -- fingerprint threshold for an auto-drafted delimited/excel
# template. Pinning ALL n headers (min_score == n) made a single added/renamed
# column in a later export drop the match to "unsupported", pushing analysts to
# re-draft near-duplicates. Requiring all-but-one (never below 2, and never below
# n when n <= 2) tolerates a minor header change while staying distinctive -- a
# different bank would have to share all-but-one of the exact column names to
# collide, which the ambiguity check then catches anyway.
.draft_min_score <- function(n) if (n <= 2L) max(1L, n) else as.integer(n - 1L)

# .derive_mapping(h, df, date_default) -- THE shared "guess the template" brain for
# a tabular (delimited/excel) source: map each canonical field to a source column,
# sniff the date format, detect the amount style, resolve the debit/credit column
# sources and the type_dc indicator values. Returns list(cols, amount_sign, keys,
# date_format). Used by .draft_delimited, .draft_excel AND column_profile.R's
# suggested-mapping hint, so the drafter and the hint can never disagree.
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
  list(cols = tdc$cols, amount_sign = style, keys = tdc$keys, date_format = date_format)
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
    columns = dm$cols, amount_sign = dm$amount_sign, currency = "NZD", origin = "user")
  utils::modifyList(out, dm$keys)
}

.draft_pdf <- function(input, id, bank) {
  # Say so plainly rather than propose a template that reads nothing. A PDF that
  # never prints a dated money line is a guide, a form or an account summary --
  # draft_template returns NULL and the caller tells the user in words, the same
  # way an Excel workbook with no transaction table already does. safe(TRUE): a
  # failure to run the check must never turn into a refusal.
  if (!isTRUE(safe(pdf_has_transaction_rows(input), TRUE))) return(NULL)
  sug <- safe(suggest_pdf_columns(input), data.frame())
  cols <- list()
  if (!is.null(sug) && nrow(sug)) {
    # Every band the sniffer returns is already labelled with the role the
    # statement's own column heading gives it (see suggest_pdf_columns). Assigning
    # by name used to collide -- four clusters all called "amount" left ONE amount
    # band, silently the last one -- so a repeated role is a defect, not something
    # to absorb.
    for (i in seq_len(nrow(sug))) cols[[sug$field[i]]] <- list(x_min = sug$x_min[i], x_max = sug$x_max[i])
  }
  style <- if (!is.null(cols$debit) || !is.null(cols$credit)) "debit_credit_cols" else "signed"
  # Skeleton fallback: even when the auto-sniffer recognises nothing (an unusual
  # layout, or a page whose money/date shapes it didn't match), STILL open the
  # toolkit with sensible starter bands the user can drag into place -- a readable
  # PDF must never dead-end with "couldn't read this file". Only missing essentials
  # are filled, so a good auto-detection is left untouched.
  page_w <- suppressWarnings(as.numeric((input$page_width %||% NA)[1]))
  if (is.na(page_w) || page_w <= 0) page_w <- 595.28
  if (is.null(cols$date))        cols$date        <- list(x_min = 0, x_max = round(page_w * 0.16))
  if (is.null(cols$description)) cols$description <- list(x_min = round(page_w * 0.17), x_max = round(page_w * 0.55))
  if (style != "debit_credit_cols" && is.null(cols$amount))
    cols$amount <- list(x_min = round(page_w * 0.72), x_max = round(page_w * 0.95))
  date_band <- cols$date
  # Fingerprint: prefer the distinctive multi-word phrases header_phrases now
  # returns. NEVER fall back to a bare generic word like "Balance" (it sits on
  # essentially every statement, so the template would match unseen PDFs and turn
  # a correct "unsupported" verdict into a silently-wrong parse). When nothing
  # distinctive is found, leave whatever was found (even empty) -- validate_template
  # then makes the analyst add a specific phrase before the template can save.
  fp <- safe(header_phrases(input), character(0))
  # Record the page size the bands were drawn in, so a differently-sized copy of
  # the statement (a rescan, another export) is normalised to this space at parse
  # time instead of dropping rows. Falls back to A4 when the size is unknown.
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
    table = tbl, currency = "NZD", origin = "user")
}

# .draft_excel -- the sheet-aware Excel draft. read_input has already picked the
# sheet, skipped any preamble and fixed serial dates, so drafting mirrors the
# delimited path: map the headers, sniff the date format and amount style.
# Returns NULL (honest "can't draft") when there's no date or no money column.
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
    columns = dm$cols, amount_sign = dm$amount_sign, currency = "NZD", origin = "user")
  utils::modifyList(out, dm$keys)
}

# draft_template(path, bank) -> a template list (or NULL if unsupported kind).
draft_template <- function(path, bank = "New bank") {
  input <- tryCatch(read_input(path), error = function(e) NULL)
  if (is.null(input)) return(NULL)
  id <- .draft_id(path)
  # Recognise the bank from the document itself for a friendlier default name/id
  # (naming hint only -- see .sniff_bank). Keeps the caller's guess otherwise.
  bank <- .sniff_bank(input, fallback = bank)
  if (identical(input$kind, "delimited")) return(.draft_delimited(path, id, bank))
  if (identical(input$kind, "pdf"))       return(.draft_pdf(input, id, bank))
  if (identical(input$kind, "excel"))     return(.draft_excel(input, id, bank))
  NULL
}

# draft_preview(path, template) -> the human-facing transactions from a draft, or
# NULL. Lets the guided UI show "here's what we'll pull out -- does this look
# right?". Uses the same display shaping as the outputs (no verbatim *_raw noise,
# debit/credit surfaced when the statement splits them) so the toolkit preview
# matches exactly what the workbook / CSV will contain.
# `preview_pages`: for the LIVE toolkit preview, parse only the first few PDF pages
# instead of the whole statement -- the user is confirming that date / amount / etc.
# are read correctly, which the first pages already show, and a big statement parses
# in a fraction of the time (the real convert, run on Save, still does every page).
# NULL = parse everything (the default, e.g. for the final preview).
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
