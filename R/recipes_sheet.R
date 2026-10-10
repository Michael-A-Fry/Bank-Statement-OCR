# recipes_sheet.R -- recipes for SPREADSHEET exports (kind: excel or csv): a bank's
# Excel or CSV download read with its recipe, and proven by the same arithmetic as
# every other reading (R/auto_read_tabular.R does the proving; this file only cuts
# the table out the way the recipe says).
#
# A spreadsheet recipe says, in the workbook's own words:
#   sheet:   which sheet(s) hold the statement: by a name pattern (sheet: {name:
#            "Statement*"}), or -- the default -- every visible sheet that prints
#            the table's heading row. Hidden sheets ("CancelBeforeSave") are never
#            read. A sheet's NAME is never used to recognise a design: banks name
#            sheets after the customer or the account.
#   table:   header  -- the heading words, each a cell of ONE row (leading and
#                       trailing spaces do not count; " Date" is "Date"). The row
#                       is found by these words, wherever it is.
#            columns -- each role under its heading (any order: a spreadsheet's
#                       columns are matched by name, not by position).
#            skip    -- rows to ignore (a calculation-support row, an instruction).
#            ends_at -- rows that end the table (a footnote, a disclosure, a
#                       pending-items section).
#            split_by -- the heading of a column naming the account: a workbook
#                       that lists several accounts in one table is read as one
#                       statement per account.
#   opening: {label: "Opening Balance as at"} -- an opening balance printed above
#            the table: the figure on that row (or just below it).
#   dates:   the format the date cells print (%d/%m/%Y, US %m/%d/%Y, %m-%d-%y ...);
#            a cell holding an Excel date is read as that date whatever the format.
#   money:   style debit_credit_cols (money out / money in columns), signed (one
#            amount), or type_words (one amount whose direction a type column
#            says: out_words: ["Withdrawal"], in_words: ["Deposit"]). Brackets,
#            a minus, a trailing OD or DR, a $ sign all read as the engine reads
#            them; a lone "." or "-" in a money column is nothing (zero).
#
# One workbook with one sheet per account (or one table split by an account
# column) is one statement per account: the first is the file's statement and the
# others its other accounts, each read and proven on its own. The file is proven
# only when every one of them is.

.RECIPE_SHEET_KINDS <- c("excel", "csv")

# .rc_sheet_kind(input) -- the recipe kind that reads this input, or NA.
.rc_sheet_kind <- function(input) {
  k <- as.character(input$kind %||% "")[1]
  if (identical(k, "excel")) "excel" else if (identical(k, "delimited")) "csv" else NA_character_
}

# .rc_cell_norm(s) -- a cell as a heading is compared: trimmed, spaces collapsed,
# lower case.
.rc_cell_norm <- function(s) {
  s <- as.character(s); s[is.na(s)] <- ""
  tolower(gsub("\\s+", " ", trimws(s)))
}

# .rc_validate_sheet(y, base) -- the spreadsheet part of .rc_validate (R/recipes.R):
# `base` is what every recipe has (id, version, bank, status, recognise words).
.rc_validate_sheet <- function(y, base) {
  bad <- function(...) list(error = sprintf(...))
  tb <- y$table %||% list()
  header <- trimws(as.character(unlist(tb$header)))
  if (!length(header) || any(!nzchar(header))) return(bad("`table: header:` must list the table's heading words."))
  if (anyDuplicated(.rc_cell_norm(header))) return(bad("heading \"%s\" is listed twice.", header[duplicated(.rc_cell_norm(header))][1]))
  cl <- tb$columns
  if (!is.list(cl) || !length(cl) || is.null(names(cl))) return(bad("`table: columns:` must name the columns."))
  fields <- names(cl)
  okf <- fields %in% .RECIPE_FIELDS | grepl(.RECIPE_EXTRA_RX, fields)
  if (any(!okf)) return(bad("column \"%s\" is not one of %s, particulars, code, reference, other_party, type, date2 or text1-text9.",
                            fields[!okf][1], paste(.RECIPE_FIELDS, collapse = ", ")))
  if (anyDuplicated(fields)) return(bad("column \"%s\" is named twice.", fields[duplicated(fields)][1]))
  if (!("date" %in% fields)) return(bad("the columns need a date."))
  if (!("description" %in% fields)) return(bad("the columns need a description."))
  has_amt <- "amount" %in% fields; has_dc <- any(c("debit", "credit") %in% fields)
  if (!has_amt && !has_dc) return(bad("the columns need an amount, or money out and money in."))
  if (has_amt && has_dc) return(bad("a table has one amount column or money-out and money-in columns, not both."))
  under <- trimws(vapply(cl, function(c) as.character((if (is.list(c)) c$under else NULL) %||% NA_character_)[1], ""))
  if (anyNA(under) || any(!nzchar(under))) return(bad("each column of a spreadsheet hangs `under:` its heading."))
  miss <- under[!(.rc_cell_norm(under) %in% .rc_cell_norm(header))]
  if (length(miss)) return(bad("column heading \"%s\" is not in `table: header:`.", miss[1]))
  if (anyDuplicated(.rc_cell_norm(under))) return(bad("two columns hang under \"%s\".", under[duplicated(.rc_cell_norm(under))][1]))
  split_by <- trimws(as.character(tb$split_by %||% NA_character_)[1])
  if (!is.na(split_by) && !(.rc_cell_norm(split_by) %in% .rc_cell_norm(header)))
    return(bad("`split_by:` heading \"%s\" is not in `table: header:`.", split_by))
  dt <- y$dates %||% list()
  dfmt <- as.character(dt$format %||% "")[1]
  known <- vapply(.ar_date_formats(tabular = TRUE), `[[`, "", "fmt")
  if (!(dfmt %in% known) && is.null(.rc_date_entry(dfmt))) return(bad("date format \"%s\" is not one the reader knows.", dfmt))
  # A spreadsheet's dates must print their year: nothing else in a workbook
  # settles it (the tabular reader's year_settled).
  if (!grepl("%[Yy]", dfmt)) return(bad("a spreadsheet's date format must print the year (got \"%s\").", dfmt))
  mo <- y$money %||% list()
  style <- as.character(mo$style %||% (if (has_amt) "signed" else "debit_credit_cols"))[1]
  if (has_dc && !identical(style, "debit_credit_cols")) return(bad("money out and money in columns are `style: debit_credit_cols`."))
  if (has_amt && !(style %in% c("signed", "type_words"))) return(bad("an amount column is read `style: signed` or `style: type_words`."))
  out_w <- trimws(as.character(unlist(mo$out_words))); in_w <- trimws(as.character(unlist(mo$in_words)))
  if (identical(style, "type_words")) {
    if (!("type" %in% fields)) return(bad("`style: type_words` needs the type column (`type: {under: ...}`) that says money in or out."))
    if (!length(out_w) || !length(in_w)) return(bad("`style: type_words` needs `out_words:` and `in_words:`."))
    if (length(intersect(.rc_cell_norm(out_w), .rc_cell_norm(in_w)))) return(bad("a type word cannot mean both money in and money out."))
  }
  order <- as.character(y$order %||% "oldest_first")[1]
  if (!(order %in% c("oldest_first", "newest_first"))) return(bad("`order:` must be oldest_first or newest_first."))
  sh <- y$sheet %||% list()
  sheet_name <- as.character(if (is.list(sh)) sh$name %||% NA_character_ else sh)[1]
  if (identical(sheet_name, "headings")) sheet_name <- NA_character_
  op <- y$opening %||% list()
  cols <- data.frame(field = fields, under = unname(under), x_min = NA_real_, x_max = NA_real_,
                     money = fields %in% .RECIPE_MONEY, stringsAsFactors = FALSE, row.names = NULL)
  c(base, list(header = header, cols = cols, anchored = TRUE, starts = character(0), period = NULL,
               ref_width = NA_real_, ends_at = as.character(unlist(tb$ends_at)), skip = as.character(unlist(tb$skip)),
               no_rows = character(0), split_by = split_by, sheet_name = sheet_name,
               opening_label = as.character(op$label %||% NA_character_)[1],
               date_format = dfmt, year = "printed", style = style, negative = character(0), positive = character(0),
               out_words = out_w, in_words = in_w,
               dir = if (identical(order, "newest_first")) "new" else "old", types = list()))
}

# ---- the workbook ----------------------------------------------------------------

# .rc_book(input) -> list of sheets, each list(name, cells (a character matrix, ""
# for blank), hidden). A CSV is one sheet. Kept per file while it is read.
.RC_BOOK <- new.env(parent = emptyenv())
.rc_book <- function(input) {
  key <- paste(input$sha256 %||% "", input$path %||% "", input$kind %||% "")
  if (identical(.RC_BOOK$key, key) && nzchar(input$path %||% "")) return(.RC_BOOK$book)
  book <- if (identical(input$kind, "excel")) .rc_book_excel(input) else {
    g <- .ar_tab_grid(input)
    if (is.null(g)) list() else list(list(name = "", cells = g$cells, hidden = FALSE))
  }
  assign("key", key, envir = .RC_BOOK); assign("book", book, envir = .RC_BOOK)
  book
}

.rc_book_excel <- function(input) {
  path <- input$path %||% ""
  if (!nzchar(path) || !file.exists(path) || !requireNamespace("readxl", quietly = TRUE)) {
    # An input built in memory: the one table read_input took.
    tb <- input$table
    if (is.null(tb) || !ncol(tb)) return(list())
    m <- rbind(names(tb), as.matrix(tb)); m[is.na(m)] <- ""; dimnames(m) <- NULL
    pre <- input$meta$preamble %||% character(0)
    if (length(pre)) { pm <- matrix("", length(pre), ncol(m)); pm[, 1] <- pre; m <- rbind(pm, m) }
    return(list(list(name = "", cells = m, hidden = FALSE)))
  }
  names <- safe(readxl::excel_sheets(path), character(0))
  hidden <- safe(.rc_hidden_sheets(path), character(0))
  lapply(names, function(sh) {
    raw <- safe(suppressMessages(as.data.frame(readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = "text",
                                                                  .name_repair = "minimal"), stringsAsFactors = FALSE)), NULL)
    m <- if (is.null(raw) || !nrow(raw) || !ncol(raw)) matrix("", 0, 0) else as.matrix(raw)
    m[is.na(m)] <- ""; dimnames(m) <- NULL
    list(name = sh, cells = m, hidden = sh %in% hidden)
  })
}

# .rc_hidden_sheets(path) -- the names of the sheets an .xlsx hides (state hidden
# or veryHidden). An old .xls cannot say: none are named.
.rc_hidden_sheets <- function(path) {
  files <- safe(utils::unzip(path, list = TRUE)$Name, character(0))
  if (!("xl/workbook.xml" %in% files)) return(character(0))
  d <- tempfile("xlsx_book_"); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  x <- safe(utils::unzip(path, files = "xl/workbook.xml", exdir = d), NULL)
  if (is.null(x) || !length(x)) return(character(0))
  s <- paste(readLines(x, warn = FALSE, encoding = "UTF-8"), collapse = "")
  tags <- regmatches(s, gregexpr("<sheet\\s[^>]*>", s))[[1]]
  tags <- tags[grepl("state=\"(hidden|veryHidden)\"", tags)]
  nm <- sub(".*\\sname=\"([^\"]*)\".*", "\\1", tags)
  nm <- gsub("&amp;", "&", gsub("&lt;", "<", gsub("&gt;", ">", gsub("&quot;", "\"", gsub("&apos;", "'", nm)))))
  nm
}

# .rc_book_text(book) -- every visible cell's words, as recognising reads them.
.rc_book_text <- function(book) {
  .rc_flat(paste(unlist(lapply(book, function(s) if (isTRUE(s$hidden)) NULL else s$cells[nzchar(s$cells)])), collapse = " | "))
}

# .rc_sheets_for(book, rc) -- the visible sheets the recipe reads (by its name
# pattern, else every one), each with the row its heading stands on.
.rc_sheets_for <- function(book, rc) {
  out <- list()
  for (s in book) {
    if (isTRUE(s$hidden) || !length(s$cells)) next
    if (!is.na(rc$sheet_name %||% NA) && !grepl(utils::glob2rx(rc$sheet_name), s$name, ignore.case = TRUE)) next
    h <- .rc_sheet_header(s$cells, rc$header)
    if (!is.null(h)) out[[length(out) + 1L]] <- c(s, list(hdr = h))
  }
  out
}

# .rc_sheet_header(cells, header) -> list(row, at = the column of each heading
# word, heads = that row's cells), or NULL: the first row on which every heading
# word is a cell of its own.
.rc_sheet_header <- function(cells, header) {
  want <- .rc_cell_norm(header)
  for (r in seq_len(min(nrow(cells), 300L))) {
    v <- .rc_cell_norm(cells[r, ])
    at <- match(want, v)
    if (!anyNA(at)) return(list(row = r, at = at, heads = trimws(cells[r, ])))
  }
  NULL
}

# ---- recognising -------------------------------------------------------------------

# .rc_sheet_fits(input, rc, book, txt) -> list(fits, score, why), as
# recipe_recognise() scores a PDF recipe.
.rc_sheet_fits <- function(rc, book, txt) {
  a <- vapply(rc$all, function(p) .rc_has_phrase(txt, p), logical(1))
  n <- vapply(rc$none, function(p) .rc_has_phrase(txt, p), logical(1))
  if (!all(a)) return(list(fits = FALSE, score = sum(a), why = sprintf("\"%s\" is not printed", rc$all[!a][1])))
  if (any(n)) return(list(fits = FALSE, score = 0, why = sprintf("\"%s\" is printed", rc$none[n][1])))
  if (!length(.rc_sheets_for(book, rc))) return(list(fits = FALSE, score = sum(a), why = "its heading row is not in the file"))
  list(fits = TRUE, score = length(rc$all) + 1L + length(rc$none) + length(rc$header) / 100, why = "fits")
}

# ---- reading -----------------------------------------------------------------------

# .rc_read_sheet(input, rc) -> a reading in auto_read()'s shape.
.rc_read_sheet <- function(input, rc) {
  if (!identical(.rc_sheet_kind(input), rc$kind))
    return(.rc_fail(rc, sprintf("The recipe reads %s files and this is not one.", if (identical(rc$kind, "csv")) "CSV" else "Excel")))
  book <- .rc_book(input)
  sheets <- .rc_sheets_for(book, rc)
  if (!length(sheets)) return(.rc_fail(rc, "The file does not print the recipe's heading row."))
  units <- list()
  for (s in sheets) units <- c(units, .rc_sheet_units(s, rc))
  if (!length(units)) return(.rc_fail(rc, "The recipe's table has no rows in this file."))
  # A visible sheet of dated rows the recipe does not read (pending items, another
  # account of another design) means what was read may not be the whole file.
  used <- vapply(sheets, `[[`, "", "name")
  stray <- 0L
  if (identical(input$kind, "excel")) for (s in book) {
    if (isTRUE(s$hidden) || s$name %in% used || !length(s$cells)) next
    if (.excel_dated_rows(as.data.frame(s$cells, stringsAsFactors = FALSE)) >= 2L) stray <- stray + 1L
  }
  hidden_rows <- if (identical(input$kind, "excel") && nzchar(input$path %||% "") && file.exists(input$path))
    safe(.excel_hidden_rows(input$path), NA_integer_) else 0L
  reads <- lapply(units, function(u) .rc_sheet_unit_read(input, rc, u, hidden_rows))
  ok <- vapply(reads, function(r) identical(r$outcome, "proven"), logical(1))
  k <- length(reads)
  main <- reads[[1]]
  if (k > 1L) {
    main$other_accounts <- lapply(seq_len(k)[-1], function(i)
      list(account = units[[i]]$account %||% NA_character_, title = NA_character_,
           rows = NROW(reads[[i]]$transactions), tx = reads[[i]]$transactions))
  }
  passed <- all(ok) && stray == 0L
  main$checks <- rbind(main$checks[, c("check", "ok", "why")],
    data.frame(check = "workbook_plain", ok = stray == 0L, why = if (stray == 0L)
      "Every sheet of dated rows is read with the recipe." else
      sprintf("The workbook holds %d other sheet(s) of dated rows that the recipe does not read.", stray),
      stringsAsFactors = FALSE),
    if (k > 1L) data.frame(check = "accounts_proven", ok = all(ok), why = if (all(ok))
      sprintf("The file holds %d accounts, each read and proven on its own.", k) else
      sprintf("Account %d of %d does not prove: %s", which(!ok)[1], k, reads[[which(!ok)[1]]]$why %||% ""),
      stringsAsFactors = FALSE))
  main$outcome <- if (passed) "proven" else "check"
  main$why <- if (passed) .rc_why(rc, if (k > 1L) sprintf("The file holds %d accounts, each proven on its own; the first is the statement and the others are kept beside it.", k)
                                      else main$why %||% "")
              else if (!ok[1]) main$why %||% "The recipe's reading does not prove."
              else if (!all(ok)) sprintf("Account %d of %d: %s", which(!ok)[1], k, reads[[which(!ok)[1]]]$why %||% "")
              else main$checks$why[main$checks$check == "workbook_plain"]
  if (!is.null(main$template)) main$template$auto$outcome <- main$outcome
  main$matched_recipe <- rc$ref; main$recipe_title <- rc$title; main$recipe_bank <- rc$bank
  main$candidates <- data.frame(source = paste0("recipe:", rc$ref), passed = passed, why = main$why, stringsAsFactors = FALSE)
  main$statements <- list(list(index = 1L, pages = "1-1", outcome = main$outcome, why = main$why, rows = NROW(main$transactions),
                               period_start = main$parsed$header$period_start %||% NA_character_,
                               period_end = main$parsed$header$period_end %||% NA_character_, empty = FALSE))
  main
}

# .rc_sheet_units(s, rc) -- the sheet's table cut as the recipe says: its body
# rows (skip rows and blank rows out, stopping at a row that ends the table), the
# rows above the heading, and -- split by the account column -- one unit per
# account.
.rc_sheet_units <- function(s, rc) {
  m <- s$cells; h <- s$hdr
  body <- if (h$row < nrow(m)) seq.int(h$row + 1L, nrow(m)) else integer(0)
  flat_row <- function(i) .rc_flat(paste(m[i, nzchar(m[i, ])], collapse = " | "))
  keep <- integer(0)
  dj <- match(.rc_cell_norm(rc$cols$under[rc$cols$field == "date"]), .rc_cell_norm(h$heads))
  dated <- FALSE
  for (i in body) {
    if (!any(nzchar(trimws(m[i, ])))) next
    f <- flat_row(i)
    # A row that ends the table ends it once the table has begun: the same words
    # printed above the first transaction (a calculation-support row) are skipped.
    if (length(rc$ends_at) && any(vapply(rc$ends_at, function(p) .rc_has_phrase(f, p), logical(1)))) {
      if (dated) break else next
    }
    if (nzchar(trimws(m[i, dj]))) dated <- TRUE
    # A row that prints the heading again is the heading, not a row.
    if (!anyNA(match(.rc_cell_norm(rc$header), .rc_cell_norm(m[i, ])))) next
    if (length(rc$skip) && any(vapply(rc$skip, function(p) .rc_has_phrase(f, p), logical(1)))) next
    keep <- c(keep, i)
  }
  pre <- if (h$row > 1L) seq_len(h$row - 1L) else integer(0)
  base <- list(sheet = s$name, m = m, hdr = h, pre = pre)
  if (is.na(rc$split_by %||% NA)) return(list(c(base, list(rows = keep, account = NA_character_))))
  j <- match(.rc_cell_norm(rc$split_by), .rc_cell_norm(h$heads))
  v <- trimws(m[keep, j])
  # A row with no account (a continuation) belongs to the account above it.
  for (t in seq_along(v)) if (!nzchar(v[t]) && t > 1L) v[t] <- v[t - 1L]
  accts <- unique(v[nzchar(v)])
  if (length(accts) <= 1L) return(list(c(base, list(rows = keep, account = if (length(accts)) accts else NA_character_))))
  lapply(accts, function(a) c(base, list(rows = keep[v == a], account = a)))
}

# .rc_money_cell(v, field) -- one money cell as a plain signed figure ("-290.00"):
# brackets, minus, OD / DR / CR, a $ sign as the engine reads them; "." or "-" alone
# is nothing (zero in a column of movements, no figure in the balance column).
.rc_money_cell <- function(v, field) {
  v <- trimws(as.character(v)); v[is.na(v)] <- ""
  out <- v
  dot <- v %in% c(".", "-", "\u2013")
  out[dot] <- if (identical(field, "balance")) "" else "0.00"
  num <- nzchar(v) & !dot & grepl("[0-9]", v)
  if (any(num)) {
    x <- .num(v[num])
    out[num] <- ifelse(is.na(x), v[num], sprintf("%.2f", round(x, 2) + 0))
  }
  out
}

# .rc_date_cells(v, fmt) -- the date cells as ISO dates: an Excel date number as
# that day, text under the recipe's format. A cell that reads neither way is left
# as printed (the reader then says so).
.rc_date_cells <- function(v, fmt) {
  v <- trimws(as.character(v)); v[is.na(v)] <- ""
  out <- v
  sv <- suppressWarnings(as.numeric(v))
  ser <- grepl(.AR_SERIAL_RX, v, perl = TRUE) & !is.na(sv) & sv >= .AR_SERIAL_LO & sv < .AR_SERIAL_HI
  out[ser] <- format(as.Date(floor(sv[ser]), origin = "1899-12-30"), "%Y-%m-%d")
  txt <- nzchar(v) & !ser
  if (any(txt)) {
    iso <- parse_date(v[txt], fmt)$iso
    # "2026-05-20 18:38:00": a date cell that also prints its time
    bare <- is.na(iso) & grepl("^\\S+\\s+[0-9]{1,2}:[0-9]{2}", v[txt])
    if (any(bare)) iso[bare] <- parse_date(sub("\\s+[0-9]{1,2}:[0-9]{2}.*$", "", v[txt][bare]), fmt)$iso
    out[txt] <- ifelse(is.na(iso), v[txt], iso)
  }
  out
}

# .rc_opening(u, label) -> list(value, rows): the opening balance printed above the
# table under `label` -- the last figure on its row, else the first figure on the
# three rows below it -- and the rows it took.
.rc_opening <- function(u, label) {
  m <- u$m
  for (i in u$pre) {
    if (!.rc_has_phrase(.rc_flat(paste(m[i, ], collapse = " | ")), label)) next
    for (r in c(i, seq.int(i + 1L, length.out = 3L))) {
      if (r > nrow(m) || (r > i && !(r %in% u$pre))) break
      cells <- trimws(m[r, ]); cells <- cells[nzchar(cells)]
      cells <- cells[grepl(.AR_TAB_MONEY_RX, cells, perl = TRUE) & !grepl(.AR_SERIAL_RX, cells, perl = TRUE) &
                     !grepl("^[0-9]{1,4}[-/.][0-9]{1,2}[-/.][0-9]{2,4}$", cells)]
      if (length(cells)) {
        x <- .num(if (r == i) utils::tail(cells, 1) else cells[1])
        if (!is.na(x)) return(list(value = x, rows = seq.int(i, r)))
      }
    }
  }
  NULL
}

# .rc_sheet_grid(u, rc) -> list(df, pre, roles) or list(error): the unit as the
# tabular reader is handed it -- the recipe's columns only (the description joined
# with the unheaded cells to its right), dates as ISO, figures signed.
.rc_sheet_grid <- function(u, rc) {
  m <- u$m; rows <- u$rows; h <- u$hdr
  heads <- .rc_cell_norm(h$heads)
  col_of <- function(under) match(.rc_cell_norm(under), heads)
  cols <- rc$cols
  j <- vapply(cols$under, col_of, 0L)
  out <- list(); nm <- character(0); roles <- character(0)
  get <- function(jj) { v <- m[rows, jj]; v[is.na(v)] <- ""; v }
  # money first, so the sign from a type word can be applied
  money <- list()
  for (k in which(cols$money)) money[[cols$field[k]]] <- .rc_money_cell(get(j[k]), cols$field[k])
  if (identical(rc$style, "type_words")) {
    ty <- .rc_cell_norm(get(j[cols$field == "type"]))
    a <- money$amount
    has <- nzchar(a) & !is.na(suppressWarnings(as.numeric(a)))
    o <- ty %in% .rc_cell_norm(rc$out_words); i <- ty %in% .rc_cell_norm(rc$in_words)
    if (any(has & !o & !i))
      return(list(error = sprintf("Row %d of the table has a type the recipe does not say is money in or out.", which(has & !o & !i)[1])))
    x <- abs(as.numeric(a[has])); x[o[has]] <- -x[o[has]]
    money$amount[has] <- sprintf("%.2f", x + 0)
  }
  dates <- .rc_date_cells(get(j[cols$field == "date"]), rc$date_format)
  # Rows with neither a date nor a figure are not rows (a detached status mark, a
  # spacer); a dated row printing no figure at all is an event, not a movement.
  mv <- intersect(names(money), c("debit", "credit", "amount"))
  any_fig <- Reduce(`|`, lapply(mv, function(f) nzchar(money[[f]])), FALSE)
  any_bal <- if (!is.null(money$balance)) nzchar(money$balance) else rep(FALSE, length(rows))
  desc <- get(j[cols$field == "description"])
  # The description is its cell and the unheaded cells to its right.
  dj <- j[cols$field == "description"]
  nxt <- dj + 1L
  while (nxt <= ncol(m) && !nzchar(heads[nxt])) {
    add <- trimws(get(nxt)); desc <- trimws(paste(desc, ifelse(nzchar(add), add, "")))
    nxt <- nxt + 1L
  }
  desc <- gsub("\\s+", " ", trimws(desc))
  is_row <- any_fig | (any_bal & nzchar(desc))
  # A row the description names an opening or closing balance is the reader's
  # balance point: its other words (a file name, a category) are not printed with
  # it, so the reader sees the label alone.
  anch <- nzchar(.ar_anchor_class(desc))
  keep <- is_row
  if (!any(keep)) return(list(error = "The recipe's table has no rows with figures."))
  sel <- function(v) v[keep]
  for (k in order(j)) {
    f <- cols$field[k]
    v <- if (f == "date") dates else if (f == "description") desc else if (f %in% names(money)) money[[f]] else {
      t <- trimws(get(j[k])); t[anch] <- ""; t }
    v <- sel(v)
    if (f %in% .RECIPE_MONEY) {
      if (!any(nzchar(v))) next           # a column with nothing in it on this account
      roles <- c(roles, f)
    } else if (!(f %in% c("date", "description"))) {
      # A words column the reader would take for figures, dates or D/C marks (a
      # code of digits, a time) is left out: it is not part of the proof.
      vv <- v[nzchar(v)]
      if (!length(vv) || all(grepl(.AR_TAB_MONEY_RX, vv, perl = TRUE)) || all(toupper(vv) %in% type_dc_domain()) ||
          all(nzchar(.ar_date_fmts(vv, .ar_date_formats(tabular = TRUE)))) || identical(rc$style, "type_words") && f == "type") next
    }
    out[[length(out) + 1L]] <- v
    head <- if (f == "description") "Description" else if (f == "date") "Date" else h$heads[j[k]]
    # Only the description is headed as one, so the reader takes that column.
    if (!(f %in% c("description")) && grepl("^(?:transaction |txn )?(?:description|details|narrative|narration)$", .ar_head_words(head), perl = TRUE))
      head <- paste("Other", head)
    nm <- c(nm, head)
  }
  if (!length(roles)) return(list(error = "The recipe's money columns are empty."))
  df <- as.data.frame(stats::setNames(out, make.unique(nm)), stringsAsFactors = FALSE, check.names = FALSE)
  # The rows above the heading, as the statement's own words (its account, its
  # period), with the opening balance put as a plain "Opening balance" line.
  pre_rows <- u$pre
  op <- if (!is.na(rc$opening_label %||% NA)) .rc_opening(u, rc$opening_label) else NULL
  if (!is.null(op)) pre_rows <- setdiff(pre_rows, op$rows)
  pre <- vapply(pre_rows, function(i) paste(trimws(m[i, nzchar(trimws(m[i, ]))]), collapse = " "), "")
  pre <- pre[nzchar(pre)]
  if (!is.null(op)) pre <- c(pre, sprintf("Opening balance %.2f", op$value + 0))
  list(df = df, pre = pre, roles = roles)
}

# .rc_sheet_unit_read(input, rc, u, hidden_rows) -- one account's table read by the
# tabular reader with the recipe's columns, and proven by its arithmetic.
.rc_sheet_unit_read <- function(input, rc, u, hidden_rows) {
  g <- .rc_sheet_grid(u, rc)
  if (!is.null(g$error)) return(.rc_fail(rc, g$error))
  shim <- list(kind = "excel", path = input$path %||% "", sha256 = input$sha256 %||% NA_character_, table = g$df,
               meta = list(preamble = g$pre, dated_sheets = 1L, hidden_rows = if (identical(input$kind, "excel")) hidden_rows else 0L))
  r <- tryCatch(.ar_read_tabular(shim, list(), NULL, list(roles = g$roles)),
                error = function(e) .ar_unread(paste0("The reader stopped on this table (", conditionMessage(e), ").")))
  if (!identical(r$outcome, "proven")) r$outcome <- if (identical(r$outcome, "unread")) "unread" else "check"
  if (!is.na(u$account %||% NA) && !is.null(r$parsed)) r$parsed$header$account_number <- u$account
  if (!is.data.frame(r$checks) || !all(c("check", "ok", "why") %in% names(r$checks)))
    r$checks <- data.frame(check = character(0), ok = logical(0), why = character(0), stringsAsFactors = FALSE)
  r$matched_recipe <- rc$ref; r$recipe_title <- rc$title
  r
}

# ---- drafting a spreadsheet design ----------------------------------------------------

# .rc_draft_sheet(reading, input, bank, id) -> the draft (a list ready for YAML), or
# list(error): the automatic reader's proven reading written down under the
# file's own headings. Nothing but headings is kept: no sheet name, no account.
.rc_draft_sheet <- function(reading, input, bank, id) {
  no <- function(why) list(error = why)
  kind <- .rc_sheet_kind(input)
  cl <- reading$columns
  if (!is.data.frame(cl) || !nrow(cl)) return(no("The reading found no columns."))
  tpl <- reading$template %||% list()
  if (identical(tpl$amount_sign, "unsigned")) return(no("Figures with no sign of their own are not written as a recipe, so far."))
  hd <- trimws(as.character(cl$heading)); hd[is.na(hd)] <- ""
  f <- as.character(cl$field)
  keep <- nzchar(f) & !grepl("^other[0-9]*$", f) & nzchar(hd) & !grepl("^col[0-9]+([.][0-9]+)?$", hd)
  need <- f %in% c("date", "description", .RECIPE_MONEY)
  if (any(need & !keep)) return(no("A column the reading needs prints no heading, so it cannot be written as a recipe."))
  f <- f[keep]; hd <- hd[keep]
  n_text <- 0L
  for (k in seq_along(f)) if (!(f[k] %in% .RECIPE_FIELDS) && !grepl(.RECIPE_EXTRA_RX, f[k]) || grepl("^text", f[k])) {
    n_text <- n_text + 1L; f[k] <- sprintf("text%d", n_text)
  }
  if (n_text > 9L || anyDuplicated(f) || anyDuplicated(tolower(hd))) return(no("The reading's columns cannot be named as a recipe's."))
  # The heading row must be printed as these words in the file itself.
  book <- .rc_book(input)
  if (!any(vapply(book, function(s) !isTRUE(s$hidden) && length(s$cells) && !is.null(.rc_sheet_header(s$cells, hd)), NA)))
    return(no("The table's headings are not one row of the file, so it cannot be written as a recipe."))
  dfmt <- as.character(tpl$columns$date$format %||% NA_character_)[1]
  if (is.na(dfmt) || !grepl("%[Yy]", dfmt)) return(no("The reading's dates print no year."))
  money <- if ("amount" %in% f) list(style = "signed") else list(style = "debit_credit_cols")
  if (identical(tpl$amount_sign, "type_dc")) {
    if (!("type" %in% f)) return(no("The reading's type column has no heading."))
    money <- list(style = "type_words", out_words = as.list(unique(lex("debit_markers"))), in_words = as.list(unique(lex("credit_markers"))))
  }
  y <- list(recipe = id, format = RECIPE_FORMAT, version = 1L, bank = bank,
            title = sprintf("%s %s export (drafted)", .layout_bank_display(bank), if (identical(kind, "csv")) "CSV" else "Excel"),
            kind = kind, status = "draft",
            recognise = list(all = as.list(hd)),
            table = list(header = as.list(hd), columns = stats::setNames(lapply(hd, function(h) list(under = h)), f)),
            dates = list(format = dfmt, year = "printed"),
            money = money,
            order = if (identical(tpl$auto$dir, "new")) "newest_first" else "oldest_first")
  y
}
