# recipes.R -- read a statement of a KNOWN design with its recipe, and prove the
# reading with the statement's own arithmetic, to the same bar as the automatic
# reader (R/auto_read*.R).
#
#   recipes_load(dirs)                       -> the recipes in those folders
#   recipes_default()                        -> the shipped recipes and the server's
#   recipe_recognise(input, recipes, bank)   -> list(recipe, scores, why)
#   recipe_read(input, recipe)               -> a reading, in auto_read()'s shape
#   recipe_first(input, bank, opts)          -> what auto_read() asks first
#
# A recipe is a small YAML file (recipes/<id>.yaml) saying how ONE bank design is
# printed: the words that recognise it, where each statement of a file starts,
# the table's heading words and its columns left to right, which lines end the
# table or are not transactions, how dates and their year are printed, and how
# money is signed. Its columns hang under their heading words, measured on each
# page, so a page printed a few points to one side still reads; fixed x-bands
# (the 1.x form) are accepted for a design with no heading row.
#
# Words a design may need, each optional (recipes/*.yaml has an example of each):
#   column `left_of: <heading>`     the first column, printed with no heading of its
#                                   own left of that heading (a card's date)
#   period `format:`                a period in the design's own pattern ("%Y%m%d")
#   `balances: {opening, closing}`  the design's own words for its two balances
#   table `subtotals:`              lines totalling part of the table, set aside
#   table `not_totals:`             summary lines that are not this table's totals
#   table `non_moving:`             words of a row that prints a figure but keeps
#                                   the balance before it ("NSF"): moves 0.00, flagged
#   dates `carried: true`           the date is printed on a day's first row only
#   dates `in_order: false`         rows listed in sections, each in date order
#   money `plus_means:`             what a plain figure in a signed column is, when
#                                   the arithmetic holds either way round
# A scan, or a page whose text layer is garbage and was OCR'd (R/ocr.R), is read
# with the same recipe: its words are matched with OCR's slack (.rc_tok_match), and
# its figures are proven by the arithmetic exactly as a text PDF's.
#
# WHY. The automatic reader works a design out from scratch every time, and on
# lookalikes of the 11 designs the team really uses it got about a third right
# first time. The old Qlik converter reads each of them with a fixed recipe and is
# reliable on them, but proves nothing. A recipe gives the fixed reading; the
# statement's own arithmetic still decides.
#
# THE RULE: the recipe proposes, the arithmetic decides. A recipe reading is
# "proven" only when, statement by statement:
#   * the arithmetic search over every assignment of the figure columns
#     (.ar_roles, the automatic reader's own) picks exactly ONE reading, and it is
#     the recipe's: the recipe never stands in for a proof;
#   * the reader's arithmetic and safety checks (.ar_arith_checks and the rest,
#     called here, not copied) all pass: the running balance on every step, or
#     opening + rows = closing, or printed totals that agree; dates inside the
#     period, the year settled, nothing counted twice, both ends printed;
#   * the recipe's own completeness checks pass: every dated line with a figure is
#     a row, a balance line or a line the recipe skips, every page with such lines
#     gave rows, and the table reader's rows are the lines the recipe found,
#     figure for figure.
# Anything less and auto_read() reads the file as it always has, with a note that
# the recipe did not prove (the bank may have changed the design). A recipe can
# never make a reading automatic that the arithmetic did not prove.

RECIPE_FORMAT <- 1L
.RECIPE_ID_RX <- "^[a-z][a-z0-9_]{0,39}$"
.RECIPE_FIELDS <- c("date", "description", "debit", "credit", "amount", "balance")
# The other columns a statement prints. Naming them keeps their words out of the
# description (a card's "Date Processed", a "Ref." or "Fee type" column), and
# particulars / code / reference / other_party / type come out as columns of their
# own. date2 and text1..text9 are kept as extras.
.RECIPE_EXTRA_RX <- "^(particulars|code|reference|other_party|type|date2|text[1-9])$"
.RECIPE_MONEY <- c("debit", "credit", "amount", "balance")
.RECIPE_STATUS <- c("draft", "proven", "retired")

# ---- loading --------------------------------------------------------------------

# recipes_dirs(cfg) -- where recipes are kept: recipes/ in the install (shipped
# with the product, replaced by an update), then the server's own folder when
# config/config.yaml names one (paths$recipes), which an update never touches.
recipes_dirs <- function(cfg = NULL) {
  root <- Sys.getenv("ENGINE_ROOT", "")
  shipped <- if (nzchar(root)) file.path(root, "recipes") else "recipes"
  if (is.null(cfg)) cfg <- safe(load_config(), list())
  server <- as.character(unlist(safe(cfg$paths$recipes, NULL)))
  # ...and the folder a conversion writes its drafts to (recipes_state_dir), while
  # that conversion runs: a draft is read by the next statement of its design.
  state <- as.character(.RECIPE_STATE$dir %||% NA_character_)
  d <- c(shipped, server, state)
  unique(d[!is.na(d) & nzchar(d)])
}
.RECIPE_STATE <- new.env(parent = emptyenv())

# recipes_load(dirs) -> the usable recipes, one per id: its highest version, and
# none for an id whose highest version is retired (a version is never edited; a
# change is a new file with a higher version). A file that is not a valid recipe
# is left out and named in attr(, "problems"), never half-used.
recipes_load <- function(dirs = recipes_dirs()) {
  found <- list(); problems <- character(0)
  for (d in unique(as.character(dirs))) {
    if (is.na(d) || !nzchar(d) || !dir.exists(d)) next
    for (f in sort(list.files(d, "[.]ya?ml$", full.names = TRUE))) {
      y <- tryCatch(yaml::read_yaml(f), error = function(e) e, warning = function(w) w)
      r <- if (inherits(y, "condition")) list(error = sprintf("it is not valid YAML (%s)", conditionMessage(y)))
           else .rc_validate(y)
      if (!is.null(r$error)) { problems <- c(problems, sprintf("%s: %s", basename(f), r$error)); next }
      r$file <- f
      dup <- vapply(found, function(o) identical(o$ref, r$ref), logical(1))
      # The same id and version in the shipped folder and the server's own: the
      # server's copy is the one used. An admin's change (off, an edit) is written
      # as the next version on the server, and a later update may ship a file with
      # that same number; the update must never undo what the admin decided (D2).
      if (any(dup) && !identical(dirname(found[[which(dup)[1]]]$file), dirname(f))) {
        j <- which(dup)[1]
        problems <- c(problems, sprintf("%s: %s is also defined by this server's %s, which is the one used.",
                                        basename(found[[j]]$file), r$ref, basename(f)))
        found[[j]] <- r
        next
      }
      if (any(dup)) {
        problems <- c(problems, sprintf("%s: %s is already defined by %s, so this copy is not used.",
                                        basename(f), r$ref, basename(found[[which(dup)[1]]]$file)))
        next
      }
      found[[length(found) + 1L]] <- r
    }
  }
  ids <- unique(vapply(found, `[[`, "", "id"))
  out <- list()
  for (id in sort(ids)) {
    vs <- Filter(function(r) identical(r$id, id), found)
    top <- vs[[which.max(vapply(vs, `[[`, 0L, "version"))]]
    if (!identical(top$status, "retired")) out[[length(out) + 1L]] <- top
  }
  structure(out, problems = problems)
}

# recipes_default() -- recipes_load() on recipes_dirs(), kept until a recipe file
# is added, changed or removed (keyed on the files' names and modified times).
.RECIPE_CACHE <- new.env(parent = emptyenv())
recipes_default <- function() {
  dirs <- recipes_dirs()
  files <- unlist(lapply(dirs[dir.exists(dirs)], list.files, pattern = "[.]ya?ml$", full.names = TRUE))
  key <- paste(c(dirs, files, as.character(as.numeric(file.mtime(files)))), collapse = "|")
  if (identical(.RECIPE_CACHE$key, key)) return(.RECIPE_CACHE$recipes)
  r <- recipes_load(dirs)
  assign("key", key, envir = .RECIPE_CACHE); assign("recipes", r, envir = .RECIPE_CACHE)
  r
}

# .rc_validate(y) -> the recipe in the reader's terms, or list(error = sentence).
# Everything a reading relies on is checked here, once, so a reading never meets a
# half-written recipe.
.rc_validate <- function(y) {
  bad <- function(...) list(error = sprintf(...))
  if (!is.list(y)) return(bad("it is not a recipe (a YAML mapping is expected)."))
  id <- as.character(y$recipe %||% "")[1]
  if (is.na(id) || !grepl(.RECIPE_ID_RX, id)) return(bad("its `recipe:` id must be lower-case letters, digits and _ (got \"%s\").", id))
  fmt <- suppressWarnings(as.integer(y$format %||% NA)[1])
  if (is.na(fmt)) return(bad("it says no `format:` (this reader reads format %d).", RECIPE_FORMAT))
  if (fmt != RECIPE_FORMAT) return(bad("it is recipe format %d; this reader reads format %d.", fmt, RECIPE_FORMAT))
  ver <- suppressWarnings(as.integer(y$version %||% NA)[1])
  if (is.na(ver) || ver < 1L) return(bad("its `version:` must be a whole number from 1."))
  bank <- .layout_slug(y$bank)
  if (is.na(bank)) return(bad("it names no `bank:`."))
  kind <- as.character(y$kind %||% "pdf")[1]
  if (!(kind %in% c("pdf", .RECIPE_SHEET_KINDS))) return(bad("kind \"%s\" is not one recipes read (pdf, excel or csv).", kind))
  status <- as.character(y$status %||% "draft")[1]
  if (!(status %in% .RECIPE_STATUS)) return(bad("its status must be one of %s.", paste(.RECIPE_STATUS, collapse = ", ")))
  rec <- y$recognise %||% list()
  all <- as.character(unlist(rec$all)); none <- as.character(unlist(rec$none))
  if (!length(all) || any(!nzchar(trimws(all)))) return(bad("`recognise: all:` must list the words that recognise the design."))
  # A spreadsheet's recipe names its columns by heading only (R/recipes_sheet.R).
  if (kind %in% .RECIPE_SHEET_KINDS)
    return(.rc_validate_sheet(y, list(id = id, version = ver, ref = paste0(id, "@", ver), bank = bank,
                                      title = as.character(y$title %||% id)[1], kind = kind, status = status,
                                      all = all, none = none)))
  tb <- y$table %||% list()
  header <- as.character(unlist(tb$header))
  cl <- tb$columns
  if (!is.list(cl) || !length(cl) || is.null(names(cl))) return(bad("`table: columns:` must name the columns, left to right."))
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
  under <- vapply(cl, function(c) as.character(c$under %||% NA_character_)[1], "")
  xmin <- vapply(cl, function(c) suppressWarnings(as.numeric(c$x_min %||% NA)[1]), 0)
  xmax <- vapply(cl, function(c) suppressWarnings(as.numeric(c$x_max %||% NA)[1]), 0)
  # left_of: the first column printed with no heading of its own, left of the first
  # heading (a card's date, printed left of "Details"). It runs from the page's left
  # edge to that heading.
  left <- vapply(cl, function(c) as.character(c$left_of %||% NA_character_)[1], "")
  if (any(!is.na(left[-1]))) return(bad("only the first column can be `left_of:` a heading."))
  if (!is.na(left[1])) {
    if (!is.na(under[1])) return(bad("the first column hangs `under:` a heading or stands `left_of:` one, not both."))
    if (!identical(unname(left[1]), unname(under[2]))) return(bad("the first column stands `left_of:` the heading of the column after it."))
  }
  anchored <- !is.na(under) | !is.na(left)
  # One way or the other for the whole table: a mix would leave the reader
  # measuring half the columns on the page and taking the rest on trust.
  if (any(anchored) && !all(anchored)) return(bad("either every column hangs `under:` a heading, or every column gives `x_min` and `x_max`."))
  if (all(anchored)) {
    if (!length(header)) return(bad("columns hang under heading words, so `table: header:` must list them."))
    lo <- !is.na(left)
    under_ <- under; under <- under[!lo]
    miss <- under[!(under %in% header)]
    if (length(miss)) return(bad("column heading \"%s\" is not in `table: header:`.", miss[1]))
    if (anyDuplicated(under)) return(bad("two columns hang under \"%s\".", under[duplicated(under)][1]))
    if (is.unsorted(match(under, header), strictly = TRUE)) return(bad("the columns must be listed in the order of their headings."))
    under <- under_
  } else {
    if (anyNA(xmin) || anyNA(xmax) || any(xmin >= xmax)) return(bad("each column's `x_min` must be left of its `x_max`."))
    if (is.unsorted(xmin, strictly = TRUE) || any(xmax[-length(xmax)] > xmin[-1])) return(bad("the columns' bands must run left to right without overlapping."))
  }
  dt <- y$dates %||% list()
  dfmt <- as.character(dt$format %||% "")[1]
  known <- vapply(.ar_date_formats(), `[[`, "", "fmt")
  # A recipe may also name a plain pattern the shared table leaves out ("17Jun26",
  # "260517"): it is read only in this recipe's own date column (.rc_date_entry).
  if (!(dfmt %in% known) && is.null(.rc_date_entry(dfmt)))
    return(bad("date format \"%s\" is not one the reader knows.", dfmt))
  has_year <- grepl("%[Yy]", dfmt)
  year <- as.character(dt$year %||% (if (has_year) "printed" else "period"))[1]
  if (!(year %in% c("printed", "period"))) return(bad("`dates: year:` must be printed or period."))
  if (has_year != identical(year, "printed")) return(bad("`dates: year:` is \"%s\" but the format %s a year.", year,
                                                         if (has_year) "prints" else "does not print"))
  # in_order: false -- the table lists its rows in sections that each run in date
  # order (a card's cardholders, each with their own charges), so the dates as a
  # whole need not. Every date must still be inside the statement period.
  # carried: true -- the date is printed on the first row of each day only; a row
  # with no date of its own is of the day above it (flagged date_carried). A row
  # with no date and none above it is still not a row.
  carried <- dt$carried %||% FALSE
  if (!is.logical(carried) || length(carried) != 1L || is.na(carried)) return(bad("`dates: carried:` must be true or false."))
  in_order <- dt$in_order %||% TRUE
  if (!is.logical(in_order) || length(in_order) != 1L || is.na(in_order)) return(bad("`dates: in_order:` must be true or false."))
  per <- y$period
  # period: format -- a period printed in a pattern the shared date table leaves
  # out ("From(YYYYMMDD):20211001"), read only after the recipe's period label.
  pfmt <- as.character((if (is.list(per)) per$format else NULL) %||% NA_character_)[1]
  if (!is.na(pfmt) && is.null(.rc_date_entry(pfmt))) return(bad("period format \"%s\" is not a plain date pattern.", pfmt))
  if (!is.na(pfmt) && !grepl("%[Yy]", pfmt)) return(bad("the period format must print a year."))
  # balances: the statement's own words for its opening and closing balance, where
  # the reader's vocabulary does not have them ("Previous Period Balance"). A line
  # printing one with a figure is that balance, never a row.
  bl <- y$balances %||% list()
  if (!is.list(bl) || length(setdiff(names(bl), c("opening", "closing"))))
    return(bad("`balances:` names an `opening:` and a `closing:` label."))
  bl_open <- as.character(unlist(bl$opening)); bl_close <- as.character(unlist(bl$closing))
  if (any(!nzchar(trimws(c(bl_open, bl_close))))) return(bad("a `balances:` label is empty."))
  if (identical(year, "period") && (is.null(per) || !nzchar(as.character(per$label %||% "")[1])))
    return(bad("the dates take their year from the period, so `period: label:` must say where it is printed."))
  mo <- y$money %||% list()
  style <- as.character(mo$style %||% (if (has_amt) "signed" else "debit_credit_cols"))[1]
  if (has_dc && !identical(style, "debit_credit_cols")) return(bad("money out and money in columns are `style: debit_credit_cols`."))
  if (has_amt && !identical(style, "signed")) return(bad("an amount column is read `style: signed` (its figures print their own sign)."))
  neg <- toupper(as.character(unlist(mo$negative))); pos <- toupper(as.character(unlist(mo$positive)))
  # A marker means here what it means to the engine's own number reader, or the
  # recipe and the figures would disagree about a sign.
  for (m in neg) if (!isTRUE(.num(paste("1.00", m)) < 0)) return(bad("the engine does not read \"%s\" as a negative figure.", m))
  for (m in pos) if (!isTRUE(.num(paste("1.00", m)) > 0)) return(bad("the engine does not read \"%s\" as a positive figure.", m))
  # plus_means: what a plain (unsigned) figure in a signed amount column is, as the
  # design prints it ("Positive transaction amounts represent deposits"). It settles
  # only what the arithmetic cannot: a reading and its exact negation both add up.
  plus <- as.character(mo$plus_means %||% NA_character_)[1]
  if (!is.na(plus) && !(plus %in% c("money_in", "money_out"))) return(bad("`money: plus_means:` must be money_in or money_out."))
  if (!is.na(plus) && !has_amt) return(bad("`money: plus_means:` is for a signed amount column."))
  order <- as.character(y$order %||% "oldest_first")[1]
  if (!(order %in% c("oldest_first", "newest_first"))) return(bad("`order:` must be oldest_first or newest_first."))
  # types: the transaction-type codes a description starts with ("DD", "BP"),
  # each with its meaning; the code fills the Type column, the description keeps it.
  types <- y$types %||% list()
  if (!is.list(types) || (length(types) && (is.null(names(types)) || any(!nzchar(names(types))) ||
      any(!grepl("^[A-Za-z0-9]{1,6}$", names(types))))))
    return(bad("`types:` must name codes of letters or digits, each with its meaning."))
  cols <- data.frame(field = fields, under = under, left_of = left, x_min = xmin, x_max = xmax,
                     money = fields %in% .RECIPE_MONEY, stringsAsFactors = FALSE, row.names = NULL)
  list(id = id, version = ver, ref = paste0(id, "@", ver), bank = bank,
       title = as.character(y$title %||% id)[1], kind = kind, status = status,
       all = all, none = none,
       starts = as.character(unlist(y$statement_starts)),
       period = if (is.null(per)) NULL else list(label = as.character(per$label)[1],
                                                open_start = as.character(per$open_start %||% NA_character_)[1],
                                                format = pfmt),
       opening = bl_open, closing = bl_close,
       header = header, cols = cols, anchored = all(anchored),
       ref_width = suppressWarnings(as.numeric(tb$ref_width %||% .A4_W)[1]),
       ends_at = as.character(unlist(tb$ends_at)), skip = as.character(unlist(tb$skip)),
       # subtotals: a line totalling PART of the table (one cardholder's charges). It
       # is set aside with its figure: the statement's whole still has to add up.
       subtotals = as.character(unlist(tb$subtotals)),
       # not_totals: lines outside the table that use the reader's words for a total
       # or a balance but are not this statement's (a portfolio summary's "TOTAL
       # DEPOSITS" across all accounts). They are not balance points.
       not_totals = as.character(unlist(tb$not_totals)),
       no_rows = as.character(unlist(tb$no_rows)),
       # non_moving: words marking a row that prints a figure but leaves the balance
       # where it was (TSB's "NSF": a payment refused for want of funds). Such a row
       # is kept, read as moving nothing, and flagged; the balance still has to hold.
       non_moving = as.character(unlist(tb$non_moving)),
       date_format = dfmt, year = year, in_order = in_order, carried = carried, style = style, negative = neg, positive = pos, plus_means = plus,
       dir = if (identical(order, "newest_first")) "new" else "old", types = types)
}

# ---- text helpers ---------------------------------------------------------------

.rc_norm <- function(s) {
  s <- tolower(.ascii_dashes(as.character(s)))
  s[is.na(s)] <- ""
  gsub("[[:space:]]+", " ", trimws(s))
}
# A printed word as a heading or phrase token: lower case, edge punctuation off.
.rc_tok <- function(s) gsub("^[[:punct:]]+|[[:punct:]]+$", "", .rc_norm(s))
# .rc_words(label) -- a label's words as tokens, each word on its own exactly as a
# printed word is (.rc_tok): "Vou. No./Trans. No." is vou, no./trans, no.
.rc_words <- function(label) {
  n <- .rc_norm(label)
  if (!nzchar(n)) return(character(0))
  .rc_tok(strsplit(n, " ", fixed = TRUE)[[1]])
}

# .rc_find_seq(tok, label, from) -- where the words of `label` stand one after
# another in `tok` (start index), searching from `from`; NA when they do not.
.rc_find_seq <- function(tok, label, from = 1L, fuzzy = FALSE) {
  lw <- .rc_words(label)
  m <- length(lw); n <- length(tok)
  if (!m || n < m || from > n - m + 1L) return(NA_integer_)
  if (!isTRUE(fuzzy)) {
    for (i in seq.int(from, n - m + 1L)) if (identical(tok[i:(i + m - 1L)], lw)) return(i)
    return(NA_integer_)
  }
  ok <- vapply(lw, function(p) .rc_tok_match(tok, p), logical(n))
  if (!is.matrix(ok)) ok <- matrix(ok, nrow = n)
  for (i in seq.int(from, n - m + 1L)) if (all(ok[cbind(i:(i + m - 1L), seq_len(m))])) return(i)
  NA_integer_
}

# .rc_tok_match(tok, p) -- which OCR'd words read as the word p. A scan's words are
# recognised, not typeset, so a heading or a phrase is allowed what OCR commonly
# does to a word: 0 for o, 1 or | for l, and one letter wrong, missing or extra in
# a word of four letters or more. A short word ("Date", "No.") must be exact after
# that, so two short labels are never taken for each other. The arithmetic still
# decides every reading; this only lets a recipe recognise its own design.
.rc_ocr_fold <- function(s) chartr("01|", "oll", s)
.rc_tok_match <- function(tok, p) {
  t <- .rc_ocr_fold(tok); q <- .rc_ocr_fold(p)
  hit <- t == q
  long <- !hit & nchar(q) >= 4L & abs(nchar(t) - nchar(q)) <= 1L
  if (any(long)) hit[long] <- as.vector(utils::adist(t[long], q)) <= 1
  hit
}

# .rc_header_spans(tok, x, x1, labels) -- the heading labels found on one line,
# left to right and in order: data.frame(label, x0, x1), or NULL when any is not
# there.
.rc_header_spans <- function(tok, x, x1, labels, fuzzy = FALSE) {
  out <- data.frame(label = labels, x0 = NA_real_, x1 = NA_real_, stringsAsFactors = FALSE)
  from <- 1L
  for (k in seq_along(labels)) {
    i <- .rc_find_seq(tok, labels[k], from, fuzzy)
    if (is.na(i)) return(NULL)
    m <- length(.rc_words(labels[k]))
    out$x0[k] <- x[i]; out$x1[k] <- x1[i + m - 1L]
    from <- i + m
  }
  out
}

# .rc_starts_with(label, phrases) -- does a line's label begin with one of the
# phrases (whole words, case-insensitive)?
.rc_starts_with <- function(label, phrases, fuzzy = FALSE) {
  if (!length(phrases)) return(FALSE)
  s <- paste0(.rc_tok(label), " ")
  if (any(startsWith(s, paste0(.rc_tok(phrases), " ")))) return(TRUE)
  if (!isTRUE(fuzzy)) return(FALSE)
  tok <- .rc_words(label)
  any(vapply(phrases, function(p) identical(.rc_find_seq(tok, p, 1L, TRUE), 1L), logical(1)))
}

# .rc_flat(s) -- text as its words only, space-separated with a space at each
# end, so a phrase is found as whole words by a plain substring search.
.rc_flat <- function(s) paste0(" ", trimws(gsub("[^a-z0-9%]+", " ", .rc_norm(s))), " ")

# .rc_has_phrase(flat, phrase) -- the phrase printed in this text (.rc_flat), as
# whole words.
.rc_has_phrase <- function(flat, phrase, fuzzy = FALSE) {
  p <- .rc_flat(phrase)
  if (!nzchar(trimws(p))) return(FALSE)
  if (grepl(p, flat, fixed = TRUE)) return(TRUE)
  if (!isTRUE(fuzzy)) return(FALSE)
  tok <- strsplit(trimws(flat), " ", fixed = TRUE)[[1]]
  !is.na(.rc_find_seq(tok, trimws(p), 1L, TRUE))
}

# ---- recognising ----------------------------------------------------------------

# .rc_line_tokens(input) -- every printed line of a PDF as heading tokens with
# their x positions, page by page, grouped exactly as the table reader groups rows.
.rc_line_tokens <- function(input) {
  lapply(input$words %||% list(), function(w) {
    if (is.null(w) || !nrow(w)) return(list())
    w <- as.data.frame(w, stringsAsFactors = FALSE)
    w <- w[!is.na(w$x) & !is.na(w$y), , drop = FALSE]
    w <- w[order(w$y, w$x), , drop = FALSE]
    g <- .group_rows(w$y, PARAM_PDF_ROW_TOL)
    tok <- .rc_tok(w$text); x <- w$x; x1 <- w$x + w$width
    lapply(split(seq_len(nrow(w)), g), function(ix) {
      ix <- ix[order(x[ix])]
      list(tok = tok[ix], x = x[ix], x1 = x1[ix])
    })
  })
}

# recipe_recognise(input, recipes, bank) -> list(recipe, scores, why). A recipe
# fits when every one of its `all` phrases is printed, none of its `none`
# phrases, and its table heading stands on one line of some page; and, when a
# bank is given, it is that bank's. The recipe is returned only when it is
# CLEARLY the best: the only one that fits, or the one that fits on more printed
# evidence than any other. Two fitting equally is no answer (NULL), and says so.
recipe_recognise <- function(input, recipes = recipes_default(), bank = NULL, drafts = FALSE) {
  none <- function(why, scores = NULL) list(recipe = NULL, scores = scores, why = why)
  if (!length(recipes)) return(none("No recipes are installed."))
  sk <- .rc_sheet_kind(input)
  if (!identical(input$kind %||% "", "pdf") && is.na(sk)) return(none("Recipes read PDFs, Excel and CSV files."))
  # A recipe reads one kind of file: a PDF's never reads a spreadsheet, or the
  # other way round.
  recipes <- Filter(function(rc) identical(rc$kind %||% "pdf", if (is.na(sk)) "pdf" else sk), recipes)
  if (!length(recipes)) return(none(if (is.na(sk)) "No recipe reads PDFs." else "No recipe reads this kind of spreadsheet."))
  # A scan (or a page whose text layer was unreadable and was OCR'd instead) is
  # recognised on its OCR'd words, with the slack OCR needs (.rc_tok_match).
  fz <- is.na(sk) && isTRUE(any(as.logical(input$page_ocr %||% FALSE)))
  book <- if (!is.na(sk)) .rc_book(input) else NULL
  txt <- if (!is.na(sk)) .rc_book_text(book) else .rc_flat(paste(.page_texts(input), collapse = " "))
  lines <- NULL
  bank_slug <- if (is.null(bank) || all(is.na(bank))) NA_character_ else .layout_slug(bank)
  sc <- lapply(recipes, function(rc) {
    row <- list(recipe = rc$ref, fits = FALSE, score = 0, why = "")
    if (!identical(rc$status, "proven") && !(isTRUE(drafts) && identical(rc$status, "draft"))) {
      row$why <- "a draft, not yet accepted"; return(row)
    }
    if (!is.na(sk)) {
      f <- .rc_sheet_fits(rc, book, txt)
      row$fits <- f$fits; row$score <- f$score; row$why <- f$why
      return(row)
    }
    a <- vapply(rc$all, function(p) .rc_has_phrase(txt, p, fz), logical(1))
    n <- vapply(rc$none, function(p) .rc_has_phrase(txt, p, fz), logical(1))
    if (!all(a)) { row$why <- sprintf("\"%s\" is not printed", rc$all[!a][1]); row$score <- sum(a); return(row) }
    if (any(n)) { row$why <- sprintf("\"%s\" is printed", rc$none[n][1]); return(row) }
    head_ok <- TRUE
    if (rc$anchored) {
      if (is.null(lines)) lines <<- .rc_line_tokens(input)
      head_ok <- any(vapply(unlist(lines, recursive = FALSE), function(l)
        !is.null(.rc_header_spans(l$tok, l$x, l$x1, rc$header, fz)), logical(1)))
    }
    if (!head_ok) { row$why <- "its table heading is not printed on one line"; row$score <- sum(a); return(row) }
    row$fits <- TRUE; row$score <- length(rc$all) + 1L + length(rc$none)
    row$why <- "fits"
    row
  })
  scores <- data.frame(recipe = vapply(sc, `[[`, "", "recipe"), fits = vapply(sc, `[[`, NA, "fits"),
                       score = vapply(sc, function(s) as.numeric(s$score), 0), why = vapply(sc, `[[`, "", "why"),
                       stringsAsFactors = FALSE)
  fit <- which(scores$fits)
  if (!length(fit)) return(none("No recipe fits this statement.", scores))
  # Every recipe that fits, best first. The chosen bank never hides one: a statement
  # of another bank's design is said to be so (.rc_bank_note), not read as unknown.
  fit <- fit[order(-scores$score[fit])]
  out <- list(recipe = recipes[[fit[1]]], fits = recipes[fit], scores = scores,
              why = sprintf("The statement is recipe %s's design.", recipes[[fit[1]]]$ref))
  if (!is.na(bank_slug) && !(bank_slug %in% c(recipes[[fit[1]]]$bank, .layout_slug(.layout_bank_display(recipes[[fit[1]]]$bank)))))
    out$bank_note <- sprintf("This reads as a %s %s statement, but %s was chosen.",
                             .layout_bank_display(recipes[[fit[1]]]$bank), recipes[[fit[1]]]$title,
                             .layout_bank_display(bank_slug))
  out
}

# ---- the auto_read() entry ---------------------------------------------------------

# recipe_first(input, bank, opts) -- what auto_read() asks before reading a file
# from scratch: NULL when no recipe is recognised; else the recipe's reading.
# opts$recipes: the recipes to use (a list), or FALSE for none; the default is
# recipes_default().
recipe_first <- function(input, bank = NULL, opts = list()) {
  rcs <- opts$recipes
  # options(bso.recipes = FALSE): read without recipes, as the automatic reader
  # alone would (measuring the reader; tests of what it learns).
  if (is.null(rcs) && isFALSE(getOption("bso.recipes"))) rcs <- FALSE
  if (isFALSE(rcs)) return(NULL)
  if (is.null(rcs)) rcs <- recipes_default()
  rg <- recipe_recognise(input, rcs, bank)
  # No accepted recipe fits: a DRAFT may (a design a person checked once). Its
  # reading is used only to fill Please check in; R/convert.R asks a person until
  # the draft has its proofs.
  if (is.null(rg$recipe) && !isFALSE(opts$drafts)) {
    rg <- recipe_recognise(input, Filter(function(r) identical(r$status, "draft"), rcs), bank, drafts = TRUE)
    if (!is.null(rg$recipe)) rg$draft <- TRUE
  }
  if (is.null(rg$recipe)) return(NULL)
  # Several recipes fit (an old and a new version of a design, two drafts of one
  # design): each reads it and the statement's own arithmetic decides. Readings that
  # prove with the SAME figures -> the newest recipe; with different figures -> a
  # person decides, since two readings that both add up cannot both be right.
  fits <- rg$fits %||% list(rg$recipe)
  reads <- lapply(fits, function(rc) recipe_read(input, rc))
  ok <- which(vapply(reads, function(r) identical(r$outcome, "proven"), logical(1)))
  pick <- if (!length(ok)) reads[[1]] else if (length(ok) == 1L) reads[[ok]] else {
    key <- function(r) paste(r$transactions$date, sprintf("%.2f", r$transactions$amount), collapse = "|")
    if (length(unique(vapply(reads[ok], key, ""))) == 1L) {
      newest <- order(-vapply(fits[ok], function(rc) rc$version, 0),
                      -vapply(fits[ok], function(rc) as.numeric(file.mtime(rc$file %||% "")), 0))[1]
      reads[[ok[newest]]]
    } else {
      r <- reads[[ok[1]]]
      r$outcome <- "check"
      r$why <- sprintf("Recipes %s each read this statement and add up, but give different figures, so a person decides.",
                       paste(vapply(fits[ok], `[[`, "", "ref"), collapse = " and "))
      # Both readings, for Please check to show side by side; the person's pick
      # (overrides$recipe, R/convert.R) is a proof for that recipe.
      r$recipe_choice <- lapply(ok, function(j) recipe_choice_summary(reads[[j]], fits[[j]]))
      r
    }
  }
  if (!is.null(rg$bank_note)) pick$notes <- c(pick$notes, rg$bank_note)
  if (isTRUE(rg$draft)) pick$draft <- TRUE
  pick
}

# recipe_choice_summary(rd, rc) -> one recipe's reading of a statement, in plain
# figures for a person to compare with another's: the recipe, how many rows, the
# money in and out, the closing balance, and its first and last rows. Nothing is
# kept: it lives on the result only.
recipe_choice_summary <- function(rd, rc) {
  tx <- rd$transactions %||% .ar_empty_tx()
  a <- suppressWarnings(as.numeric(tx$amount))
  h <- rd$parsed$header %||% list()
  pick <- unique(c(head(seq_len(nrow(tx)), 2L), tail(seq_len(nrow(tx)), 1L)))
  list(ref = rc$ref, title = rc$title %||% rc$ref, bank = rc$bank %||% NA_character_, draft = identical(rc$status, "draft"),
       rows = nrow(tx), money_in = round(sum(a[a > 0], na.rm = TRUE), 2), money_out = round(-sum(a[a < 0], na.rm = TRUE), 2),
       closing = suppressWarnings(as.numeric(h$closing_balance %||% NA)[1]),
       sample = data.frame(date = as.character(tx$date[pick]), description = as.character(tx$description[pick] %||% rep("", length(pick))),
                           amount = a[pick], stringsAsFactors = FALSE))
}

# recipe_note(rd) -- the sentence auto_read() records when a recipe was recognised
# but its reading did not prove, so the file was read from scratch instead.
recipe_note <- function(rd) {
  sprintf(paste("The statement looks like the design of recipe %s (%s), but read with it the statement does not prove",
                "(%s). The bank may have changed the design, so it was read without the recipe."),
          rd$matched_recipe %||% "?", rd$recipe_title %||% "", sub("[.]$", "", rd$why %||% "no reason given"))
}

# ---- reading ----------------------------------------------------------------------

# recipe_read(input, rc) -> a reading in auto_read()'s shape, with matched_recipe
# ("<id>@<version>"). outcome "proven" only as the header comment says; otherwise
# "check" with the first reason, and auto_read() does not use it.
recipe_read <- function(input, rc) {
  t0 <- proc.time()[["elapsed"]]
  out <- tryCatch(if (identical(rc$kind %||% "pdf", "pdf")) .rc_read(input, rc) else .rc_read_sheet(input, rc), error = function(e)
    .rc_fail(rc, paste0("The recipe reader stopped on this file (", conditionMessage(e), ").")))
  out$secs <- round(proc.time()[["elapsed"]] - t0, 3)
  out$engine <- AUTO_READ_VERSION
  out
}

.rc_fail <- function(rc, why, checks = NULL) {
  r <- .ar_unread(why, checks = checks)
  r$outcome <- "check"
  r$matched_recipe <- rc$ref; r$recipe_title <- rc$title
  r
}

# .rc_date_entry(fmt) -> list(fmt, rx, yearless) for a plain date pattern made of
# day, month and year parts and the separators " ./-", or NULL. Parts written with
# no separator between them are fixed width ("%y%m%d" is six digits).
.rc_date_entry <- function(fmt) {
  if (!is.character(fmt) || length(fmt) != 1L || !grepl("^(%[dmyYbB]|[ ./,-])+$", fmt) || !grepl("%d", fmt, fixed = TRUE))
    return(NULL)
  parts <- regmatches(fmt, gregexpr("%[dmyYbB]|[ ./,-]", fmt))[[1]]
  tight <- !grepl("[ ./,-]", fmt)
  rx <- vapply(parts, function(p) switch(p,
    "%d" = if (tight) "[0-9]{2}" else "[0-9]{1,2}", "%m" = if (tight) "[0-9]{2}" else "[0-9]{1,2}",
    "%y" = "[0-9]{2}", "%Y" = "[0-9]{4}", "%b" = "[A-Za-z]{3}", "%B" = "[A-Za-z]{3,9}",
    " " = " ", "." = "[.]", "/" = "/", "-" = "-", "," = ","), "")
  list(fmt = fmt, rx = paste0("^", paste(rx, collapse = ""), "$"), yearless = !grepl("%[yY]", fmt))
}

# .rc_fix_dates(input, rc) -> list(input, repaired): two ways a design's dates are
# printed slightly wrong, put right before anything is read (TSB designs):
#   * a tight date ("%d%b%y") printed with a space before its year, "19Sep 26":
#     the two words are one date ("19Sep26"), in the words and the page text;
#   * a date whose four-digit year lost its last digit, "02/01/202": repaired ONLY
#     when the statement's printed period allows exactly one year for it, and the
#     row is flagged (year_repaired). Otherwise it is left as printed, the line is
#     not a row, and the reading stops at a check -- the year is never guessed.
# repaired: data.frame(page, y) of every date put right the second way.
.rc_fix_dates <- function(input, rc) {
  none <- data.frame(page = integer(0), y = numeric(0))
  fmt <- rc$date_format %||% ""
  wl <- input$words %||% list()
  if (grepl("%[bB]", fmt) && grepl("%y", fmt) && !grepl("[ ./,-]", fmt)) {
    wl <- lapply(wl, function(w) {
      if (is.null(w) || nrow(w) < 2L) return(w)
      a <- which(grepl("^[0-9]{1,2}[A-Za-z]{3}$", w$text))
      drop <- integer(0)
      for (i in a) {
        j <- which(grepl("^[0-9]{2}$", w$text) & abs(w$y - w$y[i]) < 2 & w$x > w$x[i] &
                   w$x - (w$x[i] + w$width[i]) <= 1.5 * max(w$height[i], 1))
        j <- setdiff(j, drop)
        if (length(j) != 1L) next
        w$width[i] <- w$x[j] + w$width[j] - w$x[i]; w$text[i] <- paste0(w$text[i], w$text[j]); drop <- c(drop, j)
      }
      if (length(drop)) w <- w[-drop, , drop = FALSE]
      w
    })
    input$words <- wl
    if (length(input$pages)) input$pages <- gsub("\\b([0-9]{1,2}[A-Za-z]{3}) ([0-9]{2})(?![0-9.,])", "\\1\\2", input$pages, perl = TRUE)
  }
  if (!grepl("%Y", fmt, fixed = TRUE)) return(list(input = input, repaired = none))
  # the same pattern with a three-digit year
  parts <- regmatches(fmt, gregexpr("%[dmyYbB]|[ ./,-]", fmt))[[1]]
  tight <- !grepl("[ ./,-]", fmt)
  rxp <- vapply(parts, function(p) switch(p,
    "%d" = if (tight) "[0-9]{2}" else "[0-9]{1,2}", "%m" = if (tight) "[0-9]{2}" else "[0-9]{1,2}",
    "%y" = "[0-9]{2}", "%Y" = "[0-9]{3}", "%b" = "[A-Za-z]{3}", "%B" = "[A-Za-z]{3,9}",
    " " = " ", "." = "[.]", "/" = "/", "-" = "-", "," = ","), "")
  rx <- paste0("^", paste(rxp, collapse = ""), "$")
  hits <- lapply(wl, function(w) if (is.null(w) || !nrow(w)) integer(0) else which(grepl(rx, w$text)))
  if (!length(unlist(hits))) return(list(input = input, repaired = none))
  md <- safe(extract_metadata(input), list())
  ps <- .plausible_period_date(md$period_start %||% NA); pe <- .plausible_period_date(md$period_end %||% NA)
  if (is.na(ps) || is.na(pe) || pe < ps) return(list(input = input, repaired = none))
  rep <- list()
  for (p in seq_along(wl)) for (i in hits[[p]]) {
    t <- wl[[p]]$text[i]
    cand <- paste0(t, 0:9)
    d <- as.Date(cand, format = fmt, optional = TRUE)
    ok <- which(!is.na(d) & d >= ps & d <= pe)
    if (length(ok) != 1L) next
    wl[[p]]$text[i] <- cand[ok]
    rep[[length(rep) + 1L]] <- data.frame(page = p, y = wl[[p]]$y[i])
    if (length(input$pages) >= p) input$pages[p] <- sub(t, cand[ok], input$pages[p], fixed = TRUE)
  }
  input$words <- wl
  list(input = input, repaired = if (length(rep)) do.call(rbind, rep) else none)
}

.rc_read <- function(input, rc) {
  wl <- input$words %||% list()
  if (!length(wl) || all(vapply(wl, function(w) is.null(w) || !nrow(w), logical(1))))
    return(.rc_fail(rc, "The PDF has no readable text."))
  fx <- .rc_fix_dates(input, rc)
  input <- fx$input
  ctx <- .ar_pdf_context(input)
  ctx$repaired <- fx$repaired
  if (!(rc$date_format %in% vapply(ctx$fmts, `[[`, "", "fmt"))) ctx$fmts <- c(ctx$fmts, list(.rc_date_entry(rc$date_format)))
  np <- ctx$np
  # OCR'd pages are read with the slack OCR needs in their wording (.rc_tok_match);
  # their figures and dates are read as printed, and the arithmetic decides.
  rc$fuzzy <- isTRUE(any(ctx$ocr))
  # OCR'd words have a date printed tight ("17May26") cut into its pieces again
  # (.ar_ocr_split), so on a scan the recipe's date reads with a space between them.
  if (rc$fuzzy && grepl("%[bB]", rc$date_format) && !grepl("[ ./,-]", rc$date_format)) {
    rc$date_format <- gsub("(%[dmyYbB])(?=%)", "\\1 ", rc$date_format, perl = TRUE)
    if (!(rc$date_format %in% vapply(ctx$fmts, `[[`, "", "fmt"))) ctx$fmts <- c(ctx$fmts, list(.rc_date_entry(rc$date_format)))
  }
  pf <- rc$period$format %||% NA_character_
  if (rc$fuzzy && !is.na(pf) && grepl("%[bB]", pf) && !grepl("[ ./,-]", pf))
    rc$period$format <- gsub("(%[dmyYbB])(?=%)", "\\1 ", pf, perl = TRUE)
  # Where each statement of the file starts: every page printing the recipe's
  # start words. Pages before the first such page belong to the first statement
  # (a cover letter or notice). Two starts on one page cannot be cut by page.
  starts <- integer(0)
  if (length(rc$starts)) {
    lt <- .rc_line_tokens(ctx$input)
    cnt <- vapply(seq_len(np), function(p) {
      if (p > length(lt)) return(0)
      sum(vapply(lt[[p]], function(l) any(vapply(rc$starts, function(s) !is.na(.rc_find_seq(l$tok, s, 1L, rc$fuzzy)), logical(1))),
                 logical(1)))
    }, 0)
    if (any(cnt > 1)) return(.rc_fail(rc, sprintf("Page %d prints \"%s\" twice, so the statements on it cannot be told apart by page.",
                                                   which(cnt > 1)[1], rc$starts[1])))
    starts <- which(cnt > 0)
    # A design that prints its start words at the top of EVERY page ("Statement of
    # Accounts") marks no starts with them on a page numbered 2 or later.
    later <- grepl("[Pp]age\\s+([2-9]|[1-9][0-9]+)\\s+of\\s+[0-9]+", .page_texts(input))
    starts <- starts[!later[starts]]
  }
  # No start phrase found (or none named): each statement starts where the page
  # numbering restarts ("Page 1 of N"), the same signal the file splitter uses.
  if (!length(starts)) starts <- .segment_starts(input) %||% integer(0)
  if (!length(starts)) starts <- 1L
  starts[1] <- 1L
  ranges <- Map(function(a, b) seq.int(a, b), starts, c(starts[-1] - 1L, np))
  k <- length(ranges)
  units <- lapply(seq_len(k), function(i) .rc_statement(ctx, rc, ranges[[i]], k))
  if (k == 1L) return(.rc_finish_one(units[[1]], rc))
  .rc_finish_many(units, ranges, rc, np)
}

# .rc_statement(ctx, rc, pages, k) -- one statement (the file's `pages`), read and
# checked on its own pages, numbered 1.. as if it had been uploaded alone.
.rc_statement <- function(ctx, rc, pages, k) {
  sub <- .subinput_pages(ctx$input, pages)
  m <- length(pages)
  sctx <- ctx
  sctx$input <- sub; sctx$np <- m; sctx$pw <- ctx$pw[pages]; sctx$ph <- ctx$ph[pages]
  sctx$ocr <- ctx$ocr[pages]; sctx$pages_text <- sub$pages %||% character(0)
  sctx$aside <- Filter(function(a) a$page %in% pages, ctx$aside %||% list())
  rp <- ctx$repaired
  if (NROW(rp)) { rp <- rp[rp$page %in% pages, , drop = FALSE]; rp$page <- match(rp$page, pages); sctx$repaired <- rp }
  md <- if (k == 1L) ctx$md else safe(extract_metadata(sub), NULL)
  md <- md %||% list()
  pgs <- .rc_mark_balances(.ar_pdf_pages(sctx), rc)
  per <- .rc_period(pgs, rc)
  md <- .rc_md(md, per)
  sctx$md <- md
  # A page with no heading line continues the columns of the page before it, when
  # it holds dated rows in them: many designs print the headings on the first page
  # only, or open later pages with "<product> - continued". A page that holds no
  # such rows (terms, a letter, a payment slip) is set aside.
  tabs <- vector("list", length(pgs)); prev <- NULL
  for (j in seq_along(pgs)) {
    tabs[j] <- list(.rc_page(pgs[[j]], rc, prev = prev))
    if (!is.null(tabs[[j]])) prev <- tabs[[j]]
  }
  list(ctx = sctx, pgs = pgs, tabs = tabs, per = per, md = md, pages = pages)
}

# .rc_mark_balances(pgs, rc) -- every line printing one of the recipe's own
# opening or closing balance words with a figure is that balance (a summary line of
# that class), as a line in the reader's vocabulary would be.
.rc_mark_balances <- function(pgs, rc) {
  if (!length(rc$opening) && !length(rc$closing) && !length(rc$not_totals)) return(pgs)
  for (j in seq_along(pgs)) {
    pg <- pgs[[j]]
    if (is.null(pg) || is.null(pg$lines) || !nrow(pg$lines)) next
    money_lines <- unique(pg$ph$line[pg$ph$kind == "money"])
    fl <- vapply(pg$lines$raw, .rc_flat, "", USE.NAMES = FALSE)
    fz <- isTRUE(rc$fuzzy)
    nt <- vapply(seq_along(fl), function(i) any(vapply(rc$not_totals, function(p) .rc_has_phrase(fl[i], p, fz), logical(1))), logical(1))
    if (any(nt)) {
      pg$lines$aclass[nt] <- ""; pg$lines$summary[nt] <- FALSE
      if (!is.null(pg$seg) && nrow(pg$seg)) pg$seg <- pg$seg[!(pg$seg$line %in% pg$lines$line[nt]), , drop = FALSE]
    }
    for (i in which(pg$lines$line %in% money_lines & !nt)) {
      cl <- if (any(vapply(rc$opening, function(p) .rc_has_phrase(fl[i], p, fz), logical(1)))) "open"
            else if (any(vapply(rc$closing, function(p) .rc_has_phrase(fl[i], p, fz), logical(1)))) "close" else ""
      if (!nzchar(cl)) next
      pg$lines$aclass[i] <- cl; pg$lines$summary[i] <- TRUE
    }
    pgs[[j]] <- pg
  }
  pgs
}

# .rc_period(pgs, rc) -- the statement period, read after the recipe's label on
# the first line that prints it: list(found, start, end (Dates), start_text,
# end_text, open). `open`: the period prints the recipe's open-start word in place
# of a start date (a new account's first statement), and the start is then the
# day after the end a year before: a statement is issued at least once a year, so
# no row of it is older than that.
.rc_period <- function(pgs, rc) {
  none <- list(found = FALSE, start = as.Date(NA), end = as.Date(NA), start_text = NA_character_,
               end_text = NA_character_, open = FALSE)
  if (is.null(rc$period)) return(none)
  lab <- .rc_norm(rc$period$label)
  date_rx <- safe(lex("date_regex"), .DATE_RX)
  for (pg in Filter(Negate(is.null), pgs)) {
    raw <- pg$lines$raw
    hit <- which(vapply(.rc_norm(raw), function(s) grepl(lab, s, fixed = TRUE), logical(1)))
    for (i in hit) {
      # Found in lower case, cut from the line as printed: the period goes on the
      # output as the statement prints it.
      orig <- gsub("[[:space:]]+", " ", trimws(.ascii_dashes(raw[i])))
      s <- tolower(orig)
      at <- regexpr(lab, s, fixed = TRUE) + nchar(lab)
      rest <- substr(orig, at, nchar(orig))
      pf <- rc$period$format %||% NA_character_
      if (!is.na(pf)) {
        rx <- sub("\\$$", "", sub("^\\^", "", .rc_date_entry(pf)$rx))
        ds <- regmatches(rest, gregexpr(sprintf("(?<![0-9A-Za-z])%s(?![0-9A-Za-z])", rx), rest, perl = TRUE))[[1]]
        d <- as.Date(vapply(ds, function(x) format(as.Date(x, pf)), ""))
        d <- d[!is.na(d) & vapply(as.integer(format(d, "%Y")), function(yy) isTRUE(.plausible_year(yy)), logical(1))]
        ds <- ds[match(format(d, pf), ds)]
      } else {
        ds <- regmatches(rest, gregexpr(date_rx, rest, perl = TRUE))[[1]]
        d <- do.call(c, lapply(ds, .plausible_period_date))
      }
      open_w <- rc$period$open_start
      opens <- !is.na(open_w) && startsWith(tolower(trimws(rest)), .rc_norm(open_w))
      if (opens && length(d) >= 1L && !is.na(d[1])) {
        e <- d[1]
        lo <- seq(e, by = "-1 year", length.out = 2L)[2] + 1
        return(list(found = TRUE, start = lo, end = e, start_text = NA_character_, end_text = ds[1], open = TRUE))
      }
      if (length(d) >= 2L && !anyNA(d[1:2]) && d[1] <= d[2])
        return(list(found = TRUE, start = d[1], end = d[2], start_text = ds[1], end_text = ds[2], open = FALSE,
                    own_format = !is.na(pf)))
    }
  }
  none
}

# .rc_md(md, per) -- the statement's metadata with the recipe's period in it. The
# label the recipe names IS the period, so it is not in doubt (period_unsure).
.rc_md <- function(md, per) {
  if (!isTRUE(per$found)) return(md)
  md$period_start <- per$start_text
  md$period_end <- per$end_text
  md$periods <- list(c(format(per$start), format(per$end)))
  md$period_unsure <- FALSE
  md$period_source <- "recipe"
  md$period_open <- isTRUE(per$open)
  md
}

# .rc_header(pg, rc) -- the line printing the recipe's table heading on this page,
# and where each heading label sits: list(line, y, y1, spans), or NULL.
.rc_header <- function(pg, rc) {
  w <- pg$w; ln <- pg$lines
  tok <- .rc_tok(w$text)
  # Only a line printing the first heading word can be the heading row.
  first <- .rc_words(rc$header[1])[1]
  fz <- isTRUE(rc$fuzzy)
  cand <- unique(w$line[if (fz) .rc_tok_match(tok, first) else tok == first])
  for (i in which(ln$line %in% cand)) {
    ix <- which(w$line == ln$line[i])
    ix <- ix[order(w$x[ix])]
    sp <- .rc_header_spans(tok[ix], w$x[ix], w$x1[ix], rc$header, fz)
    if (!is.null(sp)) return(list(line = ln$line[i], y = ln$y[i], y1 = ln$y1[i], spans = sp))
  }
  NULL
}

# .rc_page(pg, rc) -- one page's table as the recipe sees it: the heading, every
# line under it down to the table's end (each one a row, a balance line, a line
# the recipe skips, a wrapped line, or a line that should not be there), and the
# page's column bands. NULL when the page prints no table heading.
#
# The bands are measured, not stored: each figure on a dated line belongs to the
# money column whose heading it stands under, every other word to the text column
# whose heading starts at or left of it, and a boundary goes half way across the
# gutter between two columns' print. A page printed further left or right moves
# its headings and its print together, so it reads the same.
.rc_page <- function(pg, rc, prev = NULL) {
  if (is.null(pg) || is.null(pg$lines) || !nrow(pg$lines)) return(NULL)
  cols <- rc$cols; K <- nrow(cols)
  hdr <- if (rc$anchored) .rc_header(pg, rc) else NULL
  cont <- rc$anchored && is.null(hdr) && !is.null(prev$spans)
  if (rc$anchored && is.null(hdr) && !cont) return(NULL)
  w <- pg$w; ph <- pg$ph; ln <- pg$lines[order(pg$lines$y), , drop = FALSE]
  h <- pg$h; tol <- max(2, 0.5 * h)
  money_cols <- which(cols$money); text_cols <- which(!cols$money)
  dcol <- which(cols$field == "date")
  if (cont) {
    sp <- prev$spans
  } else if (rc$anchored) {
    sp <- hdr$spans[match(cols$under, hdr$spans$label), c("x0", "x1")]
    # A column left of the first heading runs from the left edge up to it.
    lo <- which(!is.na(cols$left_of))
    if (length(lo)) { sp$x0[lo] <- 0; sp$x1[lo] <- sp$x0[lo + 1L] - tol }
  } else {
    sp <- data.frame(x0 = cols$x_min, x1 = cols$x_max)
  }
  right_of_date <- sp$x0[sp$x0 > sp$x0[dcol]]
  date_hi <- if (length(right_of_date)) min(right_of_date) else Inf
  mph <- ph[ph$kind == "money", , drop = FALSE]
  # The money column a figure stands under: its heading's span overlaps the
  # figure's print (most overlap wins). None: the figure is part of a text column
  # (a reference such as "INV 3219.05").
  mph$col <- if (nrow(mph)) vapply(seq_len(nrow(mph)), function(i) {
    ov <- pmin(mph$x1[i], sp$x1[money_cols] + tol) - pmax(mph$x[i], sp$x0[money_cols] - tol)
    if (!any(ov > 0)) return(0L)
    money_cols[which.max(ov)]
  }, 0L) else integer(0)
  # The date of a line: the first date in the date column that reads under the
  # recipe's format. Worked out for the whole page at once.
  dph <- ph[ph$kind == "date" & ph$x >= sp$x0[dcol] - tol & ph$x < date_hi - tol, , drop = FALSE]
  dph <- dph[vapply(strsplit(dph$fmts, "|", fixed = TRUE), function(f) rc$date_format %in% f, logical(1)), , drop = FALSE]
  dph <- dph[!duplicated(dph$line), , drop = FALSE]
  date_of <- function(l) {
    k <- match(l, dph$line)
    if (is.na(k)) NULL else dph[k, , drop = FALSE]
  }
  money_on <- split(mph$col, factor(mph$line, levels = ln$line))
  # Walk down from the heading. A table line is a row (a date in the date column
  # and a figure under a money heading), a balance or totals line (the reader's
  # own vocabulary), a line the recipe skips or that says there are no rows, or a
  # line wrapped under the one above. A line that is none of these, or one far
  # below the last, ends the table; so does a line starting with an `ends_at`
  # phrase -- but never before the page's first row, because a page can open with
  # the line the last page ended on (a balance carried forward).
  top <- if (!is.null(hdr)) hdr$y1 else -Inf
  below <- which(ln$y > top + 0.1 & (is.null(hdr) | ln$line != (hdr$line %||% -1L)))
  # A table with no heading row starts at its first row.
  if (is.null(hdr)) {
    first <- which(vapply(below, function(i) !is.null(date_of(ln$line[i])) && any(money_on[[as.character(ln$line[i])]] > 0), logical(1)))
    below <- if (length(first)) below[below >= below[first[1]]] else integer(0)
  }
  kind <- character(0); keep <- integer(0)
  last_y1 <- if (!is.null(hdr)) hdr$y1 else -Inf
  pitch <- NA_real_; last_row_y <- NA_real_; rows_here <- 0L
  # A dateless row takes the day above it: one on this page, or the last row of the
  # page before (a day running over the page break).
  carry_ok <- isTRUE(rc$carried) && !is.null(prev) && any(prev$region$kind == "row")
  labels <- vapply(ln$raw, .pdf_line_label, "", USE.NAMES = FALSE)
  for (i in below) {
    l <- ln$line[i]
    lab <- labels[i]
    d <- date_of(l)
    in_money <- any(money_on[[as.character(l)]] > 0)
    anchor <- nzchar(ln$aclass[i])
    is_end <- .rc_starts_with(lab, rc$ends_at, rc$fuzzy)
    gap <- ln$y[i] - last_y1
    far <- 2.5 * (if (is.na(pitch)) 2.5 * h else pitch)
    tight <- gap <= 1.2 * h
    if (is.finite(last_y1) && gap > far && !(is_end && gap <= 2 * far)) break
    k <- if (is_end && (rows_here > 0L || !anchor)) { if (anchor) "end" else "stop" }
         else if (.rc_starts_with(lab, rc$no_rows, rc$fuzzy)) "no_rows"
         else if (.rc_starts_with(lab, rc$skip, rc$fuzzy)) "skip"
         else if (.rc_starts_with(lab, rc$subtotals, rc$fuzzy)) "subtotal"
         else if (anchor) "anchor"
         else if (!is.null(d) && in_money) "row"
         else if (in_money && isTRUE(rc$carried) && (rows_here > 0L || carry_ok)) "row"
         else if (!is.null(d)) "dated_left"
         else if (in_money) "money_left"
         else if (tight && !isTRUE(ln$footer[i]) && is.finite(last_y1)) "cont"
         else "stop"
    if (identical(k, "stop")) break
    kind <- c(kind, k); keep <- c(keep, i)
    last_y1 <- ln$y1[i]
    if (identical(k, "row")) {
      if (!is.na(last_row_y)) pitch <- if (is.na(pitch)) ln$y[i] - last_row_y else stats::median(c(pitch, ln$y[i] - last_row_y))
      last_row_y <- ln$y[i]; rows_here <- rows_here + 1L
    }
    if (identical(k, "end")) break
  }
  reg <- data.frame(line = ln$line[keep], y = ln$y[keep], y1 = ln$y1[keep], raw = ln$raw[keep],
                    label = labels[keep], aclass = ln$aclass[keep],
                    kind = kind, stringsAsFactors = FALSE, row.names = NULL)
  if (cont && !any(reg$kind == "row")) return(NULL)
  bands <- .rc_bands(pg, reg$line[reg$kind == "row"], cols, sp, mph, tol, h, rc$anchored)
  list(page = pg$page, header = hdr, spans = sp, region = reg, bands = bands, mph = mph,
       date_of = date_of, h = h)
}

# .rc_bands(pg, rows, cols, sp, mph, tol, h, anchored) -> data.frame(field, x_min,
# x_max, lo, hi) and attr "conflict": each column's band on this page. lo/hi are
# the column's own print on the rows (its heading where it prints nothing here).
.rc_bands <- function(pg, rows, cols, sp, mph, tol, h, anchored) {
  K <- nrow(cols)
  if (!anchored) {
    out <- data.frame(field = cols$field, x_min = cols$x_min, x_max = cols$x_max, lo = cols$x_min, hi = cols$x_max,
                      stringsAsFactors = FALSE)
    attr(out, "conflict") <- NA_character_
    return(out)
  }
  lo <- rep(Inf, K); hi <- rep(-Inf, K)
  text_cols <- which(!cols$money)
  w <- pg$w
  # Figures under a money heading: that column's print.
  mm <- mph[mph$line %in% rows & mph$col > 0, , drop = FALSE]
  for (j in unique(mm$col)) {
    lo[j] <- min(mm$x[mm$col == j]); hi[j] <- max(mm$x1[mm$col == j])
  }
  used <- unlist(Map(seq.int, mm$i0, mm$i1))
  # Every other word: the text column whose heading starts at or left of it (the
  # leftmost text column for a word left of every heading).
  ww <- setdiff(which(w$line %in% rows), used)
  if (length(ww)) {
    pos <- findInterval(w$x[ww] + tol, sp$x0[text_cols])
    j <- text_cols[pmax(pos, 1L)]
    for (k in unique(j)) {
      lo[k] <- min(lo[k], w$x[ww][j == k]); hi[k] <- max(hi[k], w$x1[ww][j == k])
    }
  }
  none <- !is.finite(lo)
  lo[none] <- sp$x0[none]; hi[none] <- sp$x1[none]
  conflict <- NA_character_
  b <- numeric(max(0L, K - 1L))
  for (k in seq_len(K - 1L)) {
    if (hi[k] >= lo[k + 1L] && is.na(conflict))
      conflict <- sprintf("the print under \"%s\" runs into the print under \"%s\"", if (is.na(cols$under[k])) cols$field[k] else cols$under[k], cols$under[k + 1L])
    b[k] <- (hi[k] + lo[k + 1L]) / 2
  }
  pad <- 2 * h
  xmin <- c(min(lo[1], sp$x0[1]) - pad, b)
  xmax <- c(b, max(hi[K], sp$x1[K]) + pad)
  out <- data.frame(field = cols$field, x_min = xmin, x_max = xmax, lo = lo, hi = hi, stringsAsFactors = FALSE)
  attr(out, "conflict") <- conflict
  out
}

# .rc_in_band(ph, bands, field) -- the phrases whose centre lies in a column's band.
.rc_in_band <- function(x, x1, bands, field) {
  k <- which(bands$field == field)
  if (!length(k)) return(rep(FALSE, length(x)))
  cx <- (x + x1) / 2
  cx >= bands$x_min[k] & cx <= bands$x_max[k]
}

# .rc_collect(st, rc) -- the statement's rows (one per row line, in print order),
# their figure cells, and its balance points (anchors in the reader's shape): the
# summary box above the first table, and every balance, totals or skipped line
# inside the tables.
.rc_collect <- function(st, rc) {
  cols <- rc$cols; money <- cols$field[cols$money]; K <- length(money)
  rows <- list(); cells <- list(); anchors <- list(); n <- 0L
  problems <- list(); seen_table <- FALSE
  bal_k <- match("balance", money)
  mov_k <- which(money %in% c("debit", "credit", "amount"))
  for (j in seq_along(st$pgs)) {
    pg <- st$pgs[[j]]; tb <- st$tabs[[j]]
    if (is.null(pg)) next
    # Above the first table: the summary box. Every line naming an opening or
    # closing balance or a total (the reader's own vocabulary), as the reader
    # takes it.
    if (!seen_table) {
      ln <- pg$lines
      lim <- if (!is.null(tb)) (tb$header$y %||% min(tb$region$y, Inf)) else Inf
      for (i in which(ln$y < lim)) {
        l <- ln$line[i]
        mm <- pg$ph[pg$ph$line == l & pg$ph$kind == "money", , drop = FALSE]
        if (ln$summary[i] && nzchar(ln$aclass[i]) && nrow(mm)) {
          anchors[[length(anchors) + 1L]] <- list(page = j, line = l, y = ln$y[i], label = ln$label[i],
            class = ln$aclass[i], raw = ln$raw[i], in_table = FALSE, under_table = FALSE, before_rows = 0L,
            value_text = mm$text[nrow(mm)], n_money = nrow(mm), figs = rep(NA_character_, K))
          next
        }
        sg <- pg$seg[pg$seg$line == l, , drop = FALSE]
        for (s in seq_len(nrow(sg)))
          anchors[[length(anchors) + 1L]] <- list(page = j, line = l, y = ln$y[i], label = sg$label[s],
            class = sg$class[s], raw = ln$raw[i], in_table = FALSE, under_table = FALSE, before_rows = 0L,
            value_text = sg$value_text[s], n_money = 1L, figs = rep(NA_character_, K))
      }
    }
    if (is.null(tb)) next
    seen_table <- TRUE
    if (!is.na(attr(tb$bands, "conflict")))
      problems[[length(problems) + 1L]] <- list(check = "words_used_once", page = j,
        why = sprintf("On page %d %s, so no boundary separates the columns.", j, attr(tb$bands, "conflict")))
    reg <- tb$region
    # Each figure on the page in the band its centre falls in, worked out once.
    mph <- tb$mph
    cx <- (mph$x + mph$x1) / 2
    bi <- pmax(findInterval(cx, tb$bands$x_min), 1L)
    fld <- ifelse(cx >= tb$bands$x_min[bi] & cx <= tb$bands$x_max[bi], tb$bands$field[bi], NA_character_)
    on_line <- split(seq_len(nrow(mph)), factor(mph$line, levels = unique(c(reg$line, mph$line))))
    for (r in seq_len(nrow(reg))) {
      l <- reg$line[r]; kd <- reg$kind[r]
      q <- on_line[[as.character(l)]] %||% integer(0)
      figs <- vapply(money, function(f) {
        hit <- q[!is.na(fld[q]) & fld[q] == f]
        if (length(hit)) paste(mph$text[hit], collapse = " ") else NA_character_
      }, "")
      if (kd == "row") {
        d <- tb$date_of(l)
        # A row printing no date (recipe `carried`) has none here: the day above it is
        # carried down after the table reader (.rc_carry_dates), and flagged.
        if (is.null(d)) d <- list(text = NA_character_, fmts = NA_character_)
        n <- n + 1L
        rows[[n]] <- list(page = j, line = l, y = reg$y[r], y1 = reg$y1[r], raw = reg$raw[r],
                          date = d$text, date_fmts = d$fmts, date2 = NA_character_)
        cells[[n]] <- figs
        next
      }
      if (kd %in% c("anchor", "end")) {
        if (!length(q)) next
        anchors[[length(anchors) + 1L]] <- list(page = j, line = l, y = reg$y[r], label = reg$label[r],
          class = reg$aclass[r], raw = reg$raw[r], in_table = TRUE, under_table = TRUE, before_rows = n,
          value_text = mph$text[q[length(q)]], n_money = length(q), figs = figs)
        next
      }
      if (kd == "skip") {
        # A skipped line is never a row. A figure on it under money out or money in
        # would be money the statement moved, so it is not skipped silently; one
        # under the balance is the balance where the line stands: the opening
        # balance before any row, else a balance carried on.
        if (length(mov_k) && any(!is.na(figs[mov_k]))) {
          problems[[length(problems) + 1L]] <- list(check = "lines_accounted", page = j,
            why = sprintf("The line \"%s\" on page %d is one the recipe skips, but it prints money in or out.", substr(reg$raw[r], 1, 40), j))
          next
        }
        if (!is.na(bal_k) && !is.na(figs[bal_k])) {
          opening <- n == 0L
          anchors[[length(anchors) + 1L]] <- list(page = j, line = l, y = reg$y[r],
            label = if (opening) reg$label[r] else paste(reg$label[r], "(balance carried forward)"),
            class = if (opening) "open" else "close", raw = reg$raw[r], in_table = TRUE, under_table = TRUE,
            before_rows = n, value_text = figs[bal_k], n_money = 1L, figs = figs)
        }
        next
      }
      if (kd == "dated_left")
        problems[[length(problems) + 1L]] <- list(check = "dated_lines_used", page = j,
          why = sprintf("The dated line \"%s\" on page %d is not a row, a balance line or a line the recipe skips.", substr(reg$raw[r], 1, 40), j))
      if (kd == "money_left")
        problems[[length(problems) + 1L]] <- list(check = "lines_accounted", page = j,
          why = sprintf("The line \"%s\" on page %d prints a figure under a money heading but is not a row.", substr(reg$raw[r], 1, 40), j))
    }
  }
  nm <- .rc_non_moving(rows, cells, anchors, rc, bal_k, mov_k)
  for (i in nm) cells[[i]][mov_k] <- NA_character_
  pick <- function(f, proto) if (n) unlist(lapply(rows, `[[`, f)) else proto
  R <- data.frame(page = pick("page", integer(0)), line = pick("line", integer(0)), y = pick("y", numeric(0)),
                  y1 = pick("y1", numeric(0)), raw = pick("raw", character(0)), date = pick("date", character(0)),
                  date_fmts = pick("date_fmts", character(0)), date2 = rep(NA_character_, n), stringsAsFactors = FALSE)
  C <- if (n) matrix(unlist(cells), ncol = K, byrow = TRUE) else matrix(NA_character_, 0, K)
  list(rows = R, cells = C, anchors = anchors, problems = problems, roles = money, non_moving = nm)
}

# .rc_non_moving(rows, cells, anchors, rc, bal_k, mov_k) -> the rows (indices) that
# the recipe's `non_moving` words mark AND whose printed balance is exactly the
# balance before them (the row above, or the opening balance), while printing a
# figure under money out or in. Only both together make a row non-moving: the
# word alone never stops a figure from counting.
.rc_non_moving <- function(rows, cells, anchors, rc, bal_k, mov_k) {
  n <- length(rows)
  if (!n || !length(rc$non_moving) || is.na(bal_k) || !length(mov_k)) return(integer(0))
  bal <- vapply(cells, function(c) .num(c[bal_k]), 0)
  op <- NA_real_
  for (a in anchors) if (identical(a$class, "open") && isTRUE(a$before_rows == 0L)) { op <- .num(a$value_text); break }
  out <- integer(0)
  for (i in seq_len(n)) {
    fl <- .rc_flat(rows[[i]]$raw)
    if (!any(vapply(rc$non_moving, function(p) .rc_has_phrase(fl, p, isTRUE(rc$fuzzy)), logical(1)))) next
    if (all(is.na(cells[[i]][mov_k]))) next
    j <- if (identical(rc$dir, "new")) i + 1L else i - 1L
    prev <- if (j >= 1L && j <= n) bal[j] else op
    if (is.finite(bal[i]) && is.finite(prev) && abs(bal[i] - prev) < PARAM_MONEY_TOL) out <- c(out, i)
  }
  out
}

# .rc_seed_lines(st) -- on every page of the statement, the lines shaped like a
# transaction of THIS table (a date in its date column, a figure standing apart
# in one of its money columns), measured with the page's own bands, or with the
# nearest table page's where the page prints no heading. data.frame(page, line, raw).
.rc_seed_lines <- function(st) {
  tabs <- st$tabs
  have <- which(!vapply(tabs, is.null, logical(1)))
  out <- list()
  for (j in seq_along(st$pgs)) {
    pg <- st$pgs[[j]]
    if (is.null(pg) || !length(have)) next
    tb <- tabs[[have[which.min(abs(have - j))]]]
    s <- .ar_seed_lines(pg)
    if (!length(s)) next
    ph <- pg$ph[pg$ph$line %in% s, , drop = FALSE]
    d_in <- ph$kind == "date" & .rc_in_band(ph$x, ph$x1, tb$bands, "date")
    m_in <- ph$kind == "money" & ph$standalone & Reduce(`|`, lapply(intersect(tb$bands$field, .RECIPE_MONEY),
      function(f) .rc_in_band(ph$x, ph$x1, tb$bands, f)), FALSE)
    keep <- intersect(unique(ph$line[d_in]), unique(ph$line[m_in]))
    keep <- s[s %in% keep]
    if (length(keep)) out[[length(out) + 1L]] <- data.frame(page = j, line = keep,
      raw = pg$lines$raw[match(keep, pg$lines$line)], stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out) else data.frame(page = integer(0), line = integer(0), raw = character(0))
}

# .rc_template(st, rc, col, rd) -- the reading as a template in the engine's
# schema, so the table reader, reconciliation and outputs work on it unchanged.
.rc_template <- function(st, rc, rd) {
  m <- length(st$pgs)
  to_list <- function(b) {
    if (is.null(b)) return(NULL)
    out <- list()
    for (i in seq_len(nrow(b))) out[[b$field[i]]] <- list(x_min = b$x_min[i], x_max = b$x_max[i])
    out
  }
  cbp <- lapply(seq_len(m), function(j) if (is.null(st$tabs[[j]])) NULL else to_list(st$tabs[[j]]$bands))
  have <- which(!vapply(cbp, is.null, logical(1)))
  ref <- if (length(have)) cbp[[have[1]]] else list()
  # The table reader's own columns, and the rest (a second date, spare text) as
  # extras, as a hand-drawn fix hands them over (R/convert.R .override_boxes).
  ex <- ref[setdiff(names(ref), .BOX_CORE)]
  ref <- ref[intersect(names(ref), .BOX_CORE)]
  tbl <- list(row_tol = st$ctx$row_tol, date_format = rc$date_format,
              amount_sign = if (identical(rc$style, "debit_credit_cols")) "debit_credit_cols" else "signed",
              decimal_mark = st$ctx$decimal, unsigned_default = "debit", keep_dateless_rows = isTRUE(rc$carried),
              ref_width = st$ctx$frame$width, ref_height = st$ctx$frame$height,
              columns = ref, columns_by_page = cbp, extras = if (length(ex)) ex else NULL)
  list(id = paste0("recipe_", rc$id), bank = rc$bank, statement_type = NA_character_, format = "pdf",
       version = rc$version, currency = "NZD", table = tbl,
       auto = list(roles = rd$roles, conv = rd$conv, liab = isTRUE(rd$liab), dir = rd$dir,
                   engine = AUTO_READ_VERSION, recipe = rc$ref),
       recipe = list(id = rc$id, version = rc$version, ref = rc$ref, title = rc$title))
}

# .rc_parse(st, col, tpl) -- the engine's table reader on the statement's table
# lines only (rows and the lines wrapped under them), cropped to the bands. A row
# printing only a balance is handed over as a row, so its amount comes back
# unread or worked out from the balance (and is then held back), never dropped.
.rc_parse <- function(st, col, tpl) {
  cin <- st$ctx$input
  m <- length(st$pgs)
  cin$words <- lapply(seq_len(m), function(j) {
    pg <- st$pgs[[j]]; tb <- st$tabs[[j]]
    empty <- data.frame(x = numeric(0), y = numeric(0), width = numeric(0), height = numeric(0),
                        text = character(0), stringsAsFactors = FALSE)
    if (is.null(pg) || is.null(tb)) return(empty)
    ls <- tb$region$line[tb$region$kind %in% c("row", "cont")]
    w <- pg$w
    keep <- w$line %in% ls & w$cx >= min(tb$bands$x_min) & w$cx <= max(tb$bands$x_max)
    # A non-moving row's figure is not money moved: it is not handed over.
    nm <- col$non_moving[col$rows$page[col$non_moving] == j]
    if (length(nm)) {
      mb <- tb$bands[tb$bands$field %in% c("debit", "credit", "amount"), , drop = FALSE]
      inmv <- Reduce(`|`, lapply(seq_len(nrow(mb)), function(b) w$cx >= mb$x_min[b] & w$cx <= mb$x_max[b]), FALSE)
      keep <- keep & !(w$line %in% col$rows$line[nm] & inmv)
    }
    w[keep, c("x", "y", "width", "height", "text"), drop = FALSE]
  })
  cin$page_width <- rep(st$ctx$frame$width, m); cin$page_height <- rep(st$ctx$frame$height, m)
  mv <- which(col$roles %in% c("debit", "credit", "amount"))
  bare <- if (length(mv) && nrow(col$cells)) which(rowSums(!is.na(col$cells[, mv, drop = FALSE])) == 0L) else integer(0)
  force <- lapply(bare, function(i) list(page = col$rows$page[i], y_min = col$rows$y[i], y_max = col$rows$y1[i]))
  md <- st$md
  # A period that opens on the account's first day prints no start; the table
  # reader is handed the earliest start the recipe allows, so it can choose each
  # row's year, and the header keeps what the statement printed.
  if (isTRUE(st$per$open)) md$period_start <- format(st$per$start, "%d %b %Y")
  # A period printed in the recipe's own pattern ("From(YYYYMMDD):20211001") is
  # handed over as plain dates; the header keeps it as printed.
  own <- isTRUE(st$per$own_format)
  if (own) { md$period_start <- format(st$per$start, "%d %b %Y"); md$period_end <- format(st$per$end, "%d %b %Y") }
  p <- safe(parse_pdf_table(cin, tpl, force_rows = if (length(force)) force else NULL, meta = md), NULL)
  if (!is.null(p) && isTRUE(st$per$open)) p$header$period_start <- NA_character_
  if (!is.null(p) && own) { p$header$period_start <- st$per$start_text; p$header$period_end <- st$per$end_text }
  p
}

# .rc_reading(st, col, rc) -> list(rd, rl, basis, why): the arithmetic's reading.
# Every assignment of the figure columns is tried (.ar_roles, the automatic
# reader's own search). The recipe's is used only when it is the one the
# arithmetic picks; otherwise the recipe's assignment is shown, never proven.
.rc_reading <- function(col, rc, liab_ev, decimal) {
  V <- .ar_values(col$cells, decimal)
  roles <- col$roles
  # The heading words vote exactly as they do for the automatic reader: the
  # statement's own words, read by the same function, not the recipe's claim. They
  # only ever settle a reading against its exact negation.
  hroles <- if (rc$anchored) vapply(rc$cols$under[rc$cols$money], .wa_money_role, "") else rep(NA_character_, length(roles))
  rl <- .ar_roles(V, col$anchors, liab_ev, decimal, heading_roles = hroles, texts = col$rows$raw)
  rl <- .rc_plus_means(rl, rc, roles)
  ch <- rl$chosen
  if (!is.null(ch) && identical(as.character(ch$roles), roles) && identical(ch$dir, rc$dir) && identical(ch$conv, "S"))
    return(list(rd = ch, rl = rl, basis = "arithmetic", why = NULL, V = V))
  only <- .ar_roles(V, col$anchors, liab_ev, decimal, heading_roles = hroles, texts = col$rows$raw, only = roles)
  rd <- only$chosen %||% only$best
  basis <- if (rl$n_distinct > 1L) "ambiguous" else if (!is.null(ch)) "other" else if (!is.null(rd)) "broken" else "none"
  why <- if (identical(basis, "other"))
    sprintf("The statement's arithmetic reads its figure columns as %s, not as the recipe's %s.",
            paste(ch$roles, collapse = ", "), paste(roles, collapse = ", "))
  if (is.null(rd)) {
    am <- .ar_amounts(V, roles, "S", FALSE)
    b <- which(roles == "balance")
    rd <- list(roles = roles, conv = "S", liab = FALSE, dir = rc$dir, A = am$A, b = if (length(b)) b else 0L,
               bal = if (length(b)) .ar_sem(V$S[, b], V$SK[, b], FALSE) else rep(NA_real_, nrow(V$S)),
               score = c(links = 0, held = 0, failed = 0, unknown = 0, ambiguous = 0), chain = NULL)
  }
  list(rd = rd, rl = rl, basis = basis, why = why, V = V)
}

# .rc_plus_means(rl, rc, roles) -- the arithmetic left exactly two readings, the
# recipe's columns read one way and its exact negation (a signed amount column on
# an account that names no card or loan and whose rows say nothing either way).
# The recipe says what a plain figure means, so the one reading that agrees is
# taken; every other question stays the arithmetic's.
.rc_plus_means <- function(rl, rc, roles) {
  if (is.na(rc$plus_means %||% NA) || !is.null(rl$chosen) || rl$n_distinct != 2L) return(rl)
  d <- rl$distinct
  if (!all(vapply(d, function(r) identical(as.character(r$roles), roles) && identical(r$conv, "S"), logical(1)))) return(rl)
  a1 <- ifelse(is.na(d[[1]]$A), 0, d[[1]]$A); a2 <- ifelse(is.na(d[[2]]$A), 0, d[[2]]$A)
  if (!isTRUE(all.equal(a1, -a2)) || !identical(is.na(d[[1]]$A), is.na(d[[2]]$A))) return(rl)
  # A plain figure read as money in is the reading that keeps it as printed on an
  # account (liab FALSE); as money out, the one that reads the account as owed.
  want_liab <- identical(rc$plus_means, "money_out")
  pick <- Filter(function(r) identical(isTRUE(r$liab), want_liab), d)
  if (length(pick) != 1L) return(rl)
  rl$chosen <- pick[[1]]; rl$n_distinct <- 1L; rl$distinct <- pick
  rl$note <- c(rl$note, sprintf("the arithmetic holds either way round; recipe %s says a plain figure is %s",
                                rc$ref, if (want_liab) "money out" else "money in"))
  rl
}

# .rc_carry_dates(tx) -- each row the table reader kept with no date of its own
# (recipe `carried`) takes the date of the row above it, flagged date_carried, as
# the automatic reader does (.ar_post_pdf).
.rc_carry_dates <- function(tx) {
  n <- nrow(tx); if (!n) return(tx)
  f <- tx$flags %||% rep("", n); f[is.na(f)] <- ""
  nod <- is.na(tx$date) & (is.na(tx$date_raw) | !nzchar(trimws(tx$date_raw)))
  if (!any(nod)) return(tx)
  for (i in seq_len(n)) if (nod[i] && i > 1L && !is.na(tx$date[i - 1L])) tx$date[i] <- tx$date[i - 1L]
  tx$flags <- .ar_addflag(f, nod & !is.na(tx$date), "date_carried")
  tx
}

# .rc_unit(st, rc) -- one statement read, checked and decided.
# .rc_types(tx, types) -- a row whose description starts with one of the recipe's
# type codes ("DD 01-2345-... RATES") has that code in its Type column; the
# description keeps it, as printed. A Type the table itself printed stays.
.rc_types <- function(tx, types) {
  if (!length(types) || !NROW(tx) || !("description" %in% names(tx))) return(tx)
  if (!("type" %in% names(tx))) tx$type <- NA_character_
  lead <- sub("^([A-Za-z0-9]{1,6})\\s.*$", "\\1", trimws(as.character(tx$description)))
  hit <- lead %in% names(types) & grepl("^[A-Za-z0-9]{1,6}\\s", trimws(as.character(tx$description))) &
    (is.na(tx$type) | !nzchar(as.character(tx$type)))
  tx$type[hit] <- lead[hit]
  tx
}

.rc_unit <- function(st, rc) {
  ctx <- st$ctx
  col <- .rc_collect(st, rc)
  n <- nrow(col$rows)
  have <- which(!vapply(st$tabs, is.null, logical(1)))
  own_text <- .rc_own_text(st)
  if (!length(have)) return(list(passed = FALSE, why = sprintf("The table heading of recipe %s is not printed.", rc$ref),
                                 col = col, checks = .ar_checks_df(list(rows_read = list(ok = FALSE,
                                   why = "The recipe's table heading is printed on no page.")))))
  if (n == 0L) return(.rc_empty_unit(st, rc, col, own_text))
  liab_ev <- .ar_liability_evidence(own_text)
  ar <- .rc_reading(col, rc, liab_ev, ctx$decimal)
  rd <- ar$rd
  # A non-moving row moves nothing: that is the amount the balance proves for it.
  if (length(col$non_moving) && length(rd$A) == n) { rd$A[col$non_moving] <- 0; ar$rd <- rd }
  tpl <- .rc_template(st, rc, rd)
  parsed <- .rc_parse(st, col, tpl)
  tx <- parsed$transactions
  page <- suppressWarnings(as.integer(sub("^pdf:p", "", parsed$provenance$source_ref %||% character(0))))
  if (!is.null(tx) && nrow(tx)) {
    if (isTRUE(rc$carried)) tx <- .rc_carry_dates(tx)
    tx <- .ar_settle(tx, rd, col$cells, ctx$decimal, col$anchors, aligned = nrow(tx) == n)
    tx <- .rc_types(tx, rc$types)
    if (nrow(tx) == n) tx <- .rc_row_flags(tx, col, st$ctx$repaired)
    parsed$transactions <- tx
    op <- .ar_opening_value(col$anchors, rd, ctx$decimal, nrow(tx))
    cl <- .ar_closing_value(col$anchors, rd, ctx$decimal, nrow(tx))
    if (!is.na(op)) parsed$header$opening_balance <- op
    if (!is.na(cl)) parsed$header$closing_balance <- cl
  }
  ck <- .rc_checks(st, rc, col, ar, tx, page, tpl, own_text)
  oks <- vapply(ck$checks, function(x) x$ok, NA)
  failing <- names(oks)[oks %in% FALSE]
  passed <- !length(failing) && ck$proof_kind != "none"
  sc <- ck$score %||% c(links = 0, held = 0)
  proof <- list(kind = ck$proof_kind, links = as.integer(sc[["links"]]), held = as.integer(sc[["held"]]),
                unique = identical(ar$basis, "arithmetic"), pages_with_rows = sort(unique(page)),
                pages_used = sort(unique(col$rows$page)),
                derived = if (!is.null(tx)) sum(grepl("amount_from_balance", tx$flags, fixed = TRUE)) else 0L,
                one_row = length(ck$chain$one_row %||% integer(0)), rows_covered = length(ck$chain$covered %||% integer(0)),
                direction_note = ar$rl$note %||% character(0), empty = FALSE)
  # The arithmetic reading the columns another way than the recipe is the reason
  # that matters most (the design may have changed), so it is the one given.
  why <- if (passed) .ar_proven_why(proof, rd)
         else if (identical(ar$basis, "other")) ar$why
         else if (length(failing)) ck$checks[[failing[1]]]$why
         else "Nothing on the statement adds up to prove the reading."
  list(passed = passed, why = why, failing = failing, parsed = parsed, tx = tx, page = page, tpl = tpl,
       checks = .ar_checks_df(ck$checks), proof = proof, rd = rd, col = col)
}

# .rc_row_flags(tx, col, repaired) -- the recipe's own marks on the rows, once the
# table reader's rows are the recipe's one for one: a non-moving row moves 0.00
# (its amount was worked out from the unchanged balance) and says why; a row whose
# date was printed with its year cut short, repaired from the period, says so.
.rc_row_flags <- function(tx, col, repaired = NULL) {
  f <- tx$flags; f[is.na(f)] <- ""
  nm <- col$non_moving %||% integer(0)
  if (length(nm)) {
    # its figure was set aside on purpose: not a malformed, forced or derived row
    f[nm] <- vapply(strsplit(f[nm], ",", fixed = TRUE), function(t)
      paste(setdiff(t, c("amount_from_balance", "malformed", "forced", "")), collapse = ","), "")
    f <- .ar_addflag(f, seq_along(f) %in% nm, "non_moving")
    tx$amount[nm] <- 0
    if ("direction" %in% names(tx)) tx$direction[nm] <- .direction(0)
  }
  if (NROW(repaired)) {
    R <- col$rows
    hit <- vapply(seq_len(nrow(R)), function(i) any(repaired$page == R$page[i] & repaired$y >= R$y[i] - 1 & repaired$y <= R$y1[i] + 1), NA)
    f <- .ar_addflag(f, hit, "year_repaired")
  }
  tx$flags <- f
  tx
}

# .rc_own_text(st) -- the statement's own title and summary: every line above its
# first table (the reader asks it which currency and which kind of account).
.rc_own_text <- function(st) {
  out <- character(0)
  for (j in seq_along(st$pgs)) {
    pg <- st$pgs[[j]]; tb <- st$tabs[[j]]
    if (is.null(pg)) next
    lim <- if (!is.null(tb)) (tb$header$y %||% min(tb$region$y, Inf)) else Inf
    out <- c(out, pg$lines$raw[pg$lines$y < lim])
    if (!is.null(tb)) break
  }
  out
}

# .rc_checks(...) -- every check, on what the table reader produced. The
# arithmetic and safety checks are the automatic reader's own functions; the
# completeness checks are the same questions asked of the recipe's table.
.rc_checks <- function(st, rc, col, ar, tx, page, tpl, own_text) {
  ctx <- st$ctx; rd <- ar$rd; rl <- ar$rl
  n <- if (is.null(tx)) 0L else nrow(tx)
  m <- length(st$pgs)
  ck <- list()
  add <- function(name, ok, why) ck[[name]] <<- list(ok = ok, why = why)
  add("rows_read", n > 0L, if (n > 0L) sprintf("%d transaction row(s) read.", n)
      else "The table reader produced no rows from the recipe's columns.")
  if (n == 0L) return(list(checks = ck, chain = NULL, proof_kind = "none"))
  R <- col$rows
  mp <- table(factor(R$page, levels = seq_len(m))); pp <- table(factor(page, levels = seq_len(m)))
  bad <- which(as.integer(mp) != as.integer(pp))
  add("rows_match_columns", !length(bad), if (!length(bad)) "Every row the recipe's table shows was read."
      else sprintf("On page %d the recipe's table shows %d row(s) but %d were read.", bad[1], as.integer(mp[bad[1]]), as.integer(pp[bad[1]])))
  seeds <- .rc_seed_lines(st)
  miss <- setdiff(unique(seeds$page), unique(page))
  add("pages_with_rows", !length(miss), if (!length(miss)) "Every page with transactions gave rows."
      else sprintf("Page %s prints transaction lines but gave no rows (its table heading is not where the recipe expects it).",
                   paste(miss, collapse = ", ")))
  pb <- function(name) Filter(function(p) identical(p$check, name), col$problems)
  wu <- pb("words_used_once")
  add("words_used_once", !length(wu), if (!length(wu)) "Every word on the table rows falls in exactly one column." else wu[[1]]$why)
  # A dated line with a figure in this table's columns is a row or a balance line,
  # wherever it is printed on the statement's pages.
  used <- paste(R$page, R$line)
  akey <- paste(vapply(col$anchors, function(a) a$page, 0), vapply(col$anchors, function(a) a$line, 0))
  skey <- unlist(lapply(seq_len(m), function(j) { tb <- st$tabs[[j]]
    if (is.null(tb)) character(0) else paste(j, tb$region$line[tb$region$kind %in% c("skip", "no_rows", "subtotal")]) }))
  stray <- seeds[!(paste(seeds$page, seeds$line) %in% c(used, akey, skey)), , drop = FALSE]
  la <- pb("lines_accounted")
  add("lines_accounted", !nrow(stray) && !length(la),
      if (nrow(stray)) sprintf("A dated line with a figure on page %d (\"%s\") is outside the recipe's table.", stray$page[1], substr(stray$raw[1], 1, 40))
      else if (length(la)) la[[1]]$why
      else "Every dated line with a figure is a row or a balance line.")
  dl <- pb("dated_lines_used")
  add("dated_lines_used", !length(dl), if (!length(dl)) "Every line with a date in the date column is a row, a balance line or a line the recipe skips."
      else dl[[1]]$why)
  pl <- .ar_page_labels_ok(ctx$pages_text, m)
  if (!isFALSE(pl$ok) && .ar_carried_off_end(list(anchors = col$anchors, rows = R)))
    pl <- list(ok = FALSE, why = "The table ends by carrying its balance forward to a page that is not in the file.")
  add("pages_complete", pl$ok, pl$why)
  per <- .ar_period_dates(st$md)
  ds <- .ar_dates_settled(R$date, R$date_fmts, rc$date_format, rd$dir, periods = per)
  add("dates_settled", ds, if (ds) "The dates read one way only."
      else "The dates read as day-month and as month-day equally well, and nothing on the statement says which.")
  if (!is.null(rc$period) && !isTRUE(st$per$found))
    add("dates_in_period", FALSE, sprintf("The statement period is not printed after \"%s\", where the recipe expects it.", rc$period$label))
  ar_ck <- .ar_arith_checks(tx, page, col$anchors, rd, rl, if (identical(ar$basis, "other")) "none" else ar$basis,
                            ctx$decimal, st$md, two_dates = FALSE, strict = any(ctx$ocr),
                            yearless = !grepl("%[Yy]", rc$date_format), page_text = ctx$pages_text,
                            two_sided = .ar_two_sided(col$cells, rd$roles, ctx$decimal, aligned = n == nrow(R)))
  if (isFALSE(rc$in_order) && isFALSE(ar_ck$checks$dates_in_order$ok))
    ar_ck$checks$dates_in_order <- list(ok = NA, why = sprintf(
      "The rows are listed in sections, each in date order, so the dates as a whole need not be (recipe %s).", rc$ref))
  # The period check above, when it failed, stands.
  if (isFALSE(ck$dates_in_period$ok)) ar_ck$checks$dates_in_period <- ck$dates_in_period
  if (identical(ar$basis, "other")) ar_ck$checks$unique <- list(ok = FALSE, why = ar$why)
  ar_ck$checks$ends_printed <- if (identical(ar_ck$proof_kind, "none"))
      list(ok = NA, why = "Nothing adds up, so nothing is proven complete.")
    else .ar_ends_printed(.ar_anchor_points(col$anchors, n, rd$dir, isTRUE(rd$liab), rd$b, ctx$decimal),
                          .ar_totals_ok(col$anchors, tx, page, rd, ctx$decimal)$ok, pl$ok)
  sa <- ctx$aside %||% list()
  ar_ck$checks$sections_set_aside <- if (!length(sa)) list(ok = NA, why = "No pending, scheduled or uncleared section is printed.")
    else if (!identical(ar_ck$proof_kind, "none"))
      list(ok = TRUE, why = sprintf("The section \"%s\" is not transactions and was set aside; the balances add up without it.", substr(sa[[1]]$raw, 1, 50)))
    else list(ok = FALSE, why = sprintf("The section \"%s\" was set aside as not transactions, and nothing adds up to show which rows are the statement's.", substr(sa[[1]]$raw, 1, 50)))
  ar_ck$checks$currency_own <- .ar_currency_own(own_text)
  # A sign mark this design never prints (a "CR" on an everyday account's balance)
  # is one the recipe cannot say the meaning of.
  sk <- unique(as.vector(ar$V$SK))
  odd <- setdiff(intersect(sk, c("CR", "DR", "OD")), c(rc$negative, rc$positive))
  if (length(odd)) ar_ck$checks$signs_settled <- list(ok = FALSE, why = sprintf(
    "A figure carries \"%s\", which this design's statements never print, so the recipe cannot say which way it runs.", odd[1]))
  ra <- .ar_reader_agrees(list(rows = R), rd, tx)
  cd <- .ar_carried_dates_ok(tx, page)
  ot <- .rc_other_tables(st, c(used, akey, skey, paste(seeds$page, seeds$line)), ar_ck$checks$opening_closing$ok)
  ar_ck$checks <- c(ck[setdiff(names(ck), "dates_in_period")], ar_ck$checks,
                    list(reader_agrees = list(ok = ra$ok, why = ra$why), dates_carried = list(ok = cd$ok, why = cd$why),
                         other_tables = ot))
  ar_ck
}

# .rc_other_tables(st, known, oc_ok) -- lines shaped like a transaction (a date
# with a figure beside it) that are not in the recipe's table at all: another
# table on the page (a loan summary's "Interest owing as at ..."). As for the
# automatic reader, that is safe only when the statement's own printed opening and
# closing balances add up over the rows read, so none of its rows can be there.
.rc_other_tables <- function(st, known, oc_ok) {
  other <- unlist(lapply(seq_along(st$pgs), function(j) {
    pg <- st$pgs[[j]]
    if (is.null(pg)) return(NULL)
    s <- paste(j, .ar_seed_lines(pg))
    s[!(s %in% known)]
  }))
  if (!length(other)) return(list(ok = NA, why = "No other table on the pages looks like transactions."))
  pg1 <- as.integer(sub(" .*$", "", other[1]))
  if (isTRUE(oc_ok)) return(list(ok = TRUE, why = sprintf(paste("Lines on page %d look like transactions but are outside the recipe's",
    "table; the opening and closing balances confirm none of this statement's rows are among them."), pg1)))
  list(ok = FALSE, why = sprintf(paste("Lines on page %d look like transactions but are outside the recipe's table, and no printed",
                                       "opening and closing balance confirms they are not missing rows."), pg1))
}

# .rc_empty_unit(st, rc, col, own_text) -- a statement whose table prints no rows.
# It is proven empty only when it SAYS so (a `no_rows` line in its table), prints
# both its opening and closing balance and they are equal, any printed totals are
# nil, and no line on its pages is shaped like one of its transactions.
.rc_empty_unit <- function(st, rc, col, own_text) {
  ctx <- st$ctx
  ck <- list()
  add <- function(name, ok, why) ck[[name]] <<- list(ok = ok, why = why)
  says <- any(vapply(st$tabs, function(tb) !is.null(tb) && any(tb$region$kind == "no_rows"), logical(1)))
  add("rows_read", if (says) NA else FALSE, if (says) "The statement says it has no transactions for the period."
      else "The table reader found no rows, and the statement does not say it has none.")
  seeds <- .rc_seed_lines(st)
  add("lines_accounted", !nrow(seeds) && !length(col$problems), if (nrow(seeds))
      sprintf("A dated line with a figure on page %d (\"%s\") is outside the recipe's table.", seeds$page[1], substr(seeds$raw[1], 1, 40))
      else if (length(col$problems)) col$problems[[1]]$why else "No line on the statement is shaped like a transaction.")
  b <- match("balance", col$roles); b <- if (is.na(b)) 0L else b
  rd0 <- list(dir = rc$dir, liab = FALSE, b = b, roles = col$roles)
  op <- .ar_opening_value(col$anchors, rd0, ctx$decimal, 0L); cl <- .ar_closing_value(col$anchors, rd0, ctx$decimal, 0L)
  oc <- !is.na(op) && !is.na(cl) && abs(op - cl) < PARAM_MONEY_TOL
  add("opening_closing", oc, if (oc) "The opening balance equals the closing balance, as a statement with no transactions must."
      else if (is.na(op) || is.na(cl)) "The statement does not print both an opening and a closing balance."
      else sprintf("The opening balance %s is not the closing balance %s, so transactions are missing.", format(op, nsmall = 2), format(cl, nsmall = 2)))
  tots <- Filter(function(a) identical(a$class, "total"), col$anchors)
  tv <- unlist(lapply(tots, function(a) c(.num(a$value_text, ctx$decimal), .num(a$figs[!is.na(a$figs)], ctx$decimal))))
  tz <- !length(tv) || all(!is.na(tv) & abs(tv) < PARAM_MONEY_TOL)
  add("printed_totals", if (length(tv)) tz else NA, if (!length(tv)) "No printed totals to check."
      else if (tz) "The printed totals are nil." else "A printed total is not nil, so transactions are missing.")
  pl <- .ar_page_labels_ok(ctx$pages_text, length(st$pgs))
  add("pages_complete", pl$ok, pl$why)
  add("ends_printed", oc, if (oc) "The statement's opening and closing balances are printed."
      else "Nothing marks both ends of the statement.")
  ot <- .rc_other_tables(st, paste(seeds$page, seeds$line), oc)
  add("other_tables", ot$ok, ot$why)
  add("currency_own", .ar_currency_own(own_text)$ok, .ar_currency_own(own_text)$why)
  passed <- says && !any(vapply(ck, function(x) isFALSE(x$ok), logical(1)))
  rd <- list(roles = col$roles, conv = "S", liab = FALSE, dir = rc$dir, A = numeric(0), b = b, bal = numeric(0))
  tpl <- .rc_template(st, rc, rd)
  parsed <- .rc_parse(st, col, tpl)
  if (!is.null(parsed)) {
    if (!is.na(op)) parsed$header$opening_balance <- op
    if (!is.na(cl)) parsed$header$closing_balance <- cl
  }
  failing <- names(ck)[vapply(ck, function(x) isFALSE(x$ok), logical(1))]
  why <- if (passed) sprintf("The statement prints no transactions (\"%s\") and its opening balance equals its closing balance.",
                             rc$no_rows[1] %||% "no transactions")
         else if (length(failing)) ck[[failing[1]]]$why else "The table prints no rows and does not say why."
  proof <- list(kind = if (passed) "totals" else "none", links = 0L, held = 0L, unique = passed, pages_with_rows = integer(0),
                pages_used = integer(0), derived = 0L, one_row = 0L, rows_covered = 0L, direction_note = character(0),
                empty = passed)
  list(passed = passed, why = why, failing = failing, parsed = parsed, tx = parsed$transactions %||% .ar_empty_tx(),
       page = integer(0), tpl = tpl, checks = .ar_checks_df(ck), proof = proof, rd = rd, col = col)
}

# .rc_columns(st, rc) -- every page's bands, in the reading's `columns` shape.
.rc_columns <- function(st, rc, file_pages = NULL) {
  out <- lapply(seq_along(st$tabs), function(j) {
    tb <- st$tabs[[j]]; if (is.null(tb)) return(NULL)
    b <- tb$bands
    data.frame(page = if (is.null(file_pages)) j else file_pages[j], field = b$field,
               kind = ifelse(b$field == "date", "date", ifelse(b$field %in% .RECIPE_MONEY, "money", "text")),
               x_min = b$x_min, x_max = b$x_max, ink_min = b$lo, ink_max = b$hi,
               heading = if (rc$anchored) ifelse(is.na(rc$cols$under), "", rc$cols$under) else "", stringsAsFactors = FALSE)
  })
  out <- Filter(Negate(is.null), out)
  if (length(out)) do.call(rbind, out) else .ar_unread("")$columns
}

.rc_why <- function(rc, why) sprintf("Read with recipe %s (%s). %s", rc$ref, rc$title, why)

# .rc_finish_one(st, rc) -- a file holding one statement.
# .rc_examples(us, n) -- for each column of figures, the first `n` rows that print
# something in it, as printed: the row's date, its words and the figure, under the
# role the reading gave the column. Please check shows these lines to ask what a
# column is (as .ar_column_examples does for the automatic reader).
.rc_examples <- function(us, n = 2L) {
  out <- list()
  for (u in us) {
    col <- u$col
    if (is.null(col) || !NROW(col$rows) || !ncol(col$cells)) next
    roles <- u$rd$roles %||% col$roles
    desc <- if (NROW(u$tx) == nrow(col$rows)) u$tx$description else col$rows$raw
    for (k in seq_along(roles)) {
      have <- sum(vapply(out, function(e) sum(e$field == roles[k]), 0))
      hit <- utils::head(which(!is.na(col$cells[, k]) & grepl("[0-9]", col$cells[, k])), n - have)
      if (length(hit))
        out[[length(out) + 1L]] <- data.frame(field = roles[k], date = col$rows$date[hit],
          description = substr(as.character(desc[hit]), 1L, 40L), figure = col$cells[hit, k], stringsAsFactors = FALSE)
    }
  }
  if (length(out)) do.call(rbind, out) else NULL
}

.rc_finish_one <- function(st, rc) {
  u <- .rc_unit(st, rc)
  outcome <- if (isTRUE(u$passed)) "proven" else "check"
  tpl <- u$tpl
  if (!is.null(tpl)) tpl$auto$outcome <- outcome
  recon <- if (!is.null(u$parsed)) safe(reconcile(u$parsed, tpl), NULL) else NULL
  list(outcome = outcome, why = if (isTRUE(u$passed)) .rc_why(rc, u$why) else u$why, template = tpl,
       parsed = u$parsed, recon = recon, transactions = u$tx %||% .ar_empty_tx(), proof = u$proof,
       checks = u$checks,
       candidates = data.frame(source = paste0("recipe:", rc$ref), passed = isTRUE(u$passed), why = u$why %||% "",
                               stringsAsFactors = FALSE),
       columns = .rc_columns(st, rc), examples = safe(.rc_examples(list(u)), NULL), matched_layout = NULL, matched_recipe = rc$ref,
       recipe_title = rc$title, recipe_bank = rc$bank, notes = character(0), other_accounts = list(),
       statements = list(.rc_statement_summary(u, st$pages, 1L)))
}

.rc_statement_summary <- function(u, pages, i) {
  h <- u$parsed$header %||% list()
  list(index = i, pages = sprintf("%d-%d", min(pages), max(pages)), outcome = if (isTRUE(u$passed)) "proven" else "check",
       why = u$why, rows = NROW(u$tx), period_start = h$period_start %||% NA_character_,
       period_end = h$period_end %||% NA_character_, empty = isTRUE(u$proof$empty))
}

# .rc_finish_many(units, ranges, rc, np) -- a file holding several statements of
# the design, each read and proven on its own, then put together exactly as a
# split file is (bundle_combine). It is proven only when every statement is AND
# they follow on: in period order, each opens at the balance the one before it
# closed on (the rule convert_statement applies to a split file).
.rc_finish_many <- function(units, ranges, rc, np) {
  k <- length(units)
  us <- lapply(seq_len(k), function(i) { progress_page("table", max(ranges[[i]])); .rc_unit(units[[i]], rc) })
  readings <- lapply(seq_len(k), function(i) {
    u <- us[[i]]
    rec <- if (!is.null(u$parsed) && NROW(u$tx)) safe(reconcile(u$parsed, u$tpl), NULL) else NULL
    list(outcome = if (isTRUE(u$passed)) "proven" else "check", why = u$why, parsed = u$parsed,
         transactions = u$tx %||% .ar_empty_tx(), checks = u$checks, matched_layout = NULL,
         recon = rec %||% list(trust = list(level = if (isTRUE(u$passed)) "medium" else "low",
                                            score = if (isTRUE(u$passed)) 0.5 else 0)))
  })
  # An empty statement is part of the file: bundle_combine keeps it in the list.
  for (i in seq_len(k)) if (is.null(readings[[i]]$parsed))
    readings[[i]]$parsed <- list(transactions = .ar_empty_tx(), header = list())
  comb <- bundle_combine(readings, ranges, np)
  all_ok <- all(vapply(us, function(u) isTRUE(u$passed), logical(1)))
  join <- .rc_join(us)
  passed <- all_ok && isTRUE(join$ok)
  first_bad <- which(!vapply(us, function(u) isTRUE(u$passed), logical(1)))[1]
  why <- if (passed) .rc_why(rc, sprintf("The file holds %d statements, each proven on its own, and each opens at the balance the one before it closed on.", k))
         else if (!is.na(first_bad)) sprintf("Statement %d of %d (pages %d-%d): %s", first_bad, k, min(ranges[[first_bad]]),
                                             max(ranges[[first_bad]]), us[[first_bad]]$why)
         else join$why
  checks <- do.call(rbind, lapply(seq_len(k), function(i) {
    c0 <- us[[i]]$checks
    if (!NROW(c0)) return(NULL)
    c0$why <- sprintf("Statement %d: %s", i, c0$why)
    c0
  }))
  checks <- rbind(checks, data.frame(check = "statements_join", ok = join$ok, why = join$why, stringsAsFactors = FALSE))
  lead <- us[[which(vapply(us, function(u) NROW(u$tx) > 0L, logical(1)))[1] %||% 1L]]
  tpl <- lead$tpl
  if (!is.null(tpl)) tpl$auto$outcome <- if (passed) "proven" else "check"
  tx <- comb$parsed$transactions %||% .ar_empty_tx()
  pk <- vapply(us, function(u) u$proof$kind %||% "none", "")
  proof <- list(kind = if (all(pk[pk != "none"] == "chain") && any(pk == "chain")) "chain" else if (passed) "totals" else "none",
                links = sum(vapply(us, function(u) as.integer(u$proof$links %||% 0L), 0L)),
                held = sum(vapply(us, function(u) as.integer(u$proof$held %||% 0L), 0L)),
                unique = all(vapply(us, function(u) isTRUE(u$proof$unique), logical(1))),
                pages_with_rows = sort(unique(unlist(lapply(seq_len(k), function(i) ranges[[i]][us[[i]]$page])))),
                pages_used = sort(unique(unlist(ranges))),
                derived = sum(vapply(us, function(u) as.integer(u$proof$derived %||% 0L), 0L)), statements = k)
  cols <- do.call(rbind, lapply(seq_len(k), function(i) .rc_columns(units[[i]], rc, ranges[[i]])))
  list(outcome = if (passed) "proven" else "check", why = why, template = tpl, parsed = comb$parsed,
       recon = comb$recon, transactions = tx, proof = proof, checks = checks,
       candidates = data.frame(source = paste0("recipe:", rc$ref), passed = passed, why = why, stringsAsFactors = FALSE),
       columns = cols, examples = safe(.rc_examples(us), NULL), matched_layout = NULL, matched_recipe = rc$ref, recipe_title = rc$title, recipe_bank = rc$bank,
       notes = character(0), other_accounts = list(),
       statements = lapply(seq_len(k), function(i) .rc_statement_summary(us[[i]], ranges[[i]], i)))
}

# .rc_join(us) -> list(ok, why). In period order (never the file's order: a
# bundle may be saved newest first or shuffled) each statement opens at the
# balance the one before it closed on, to the cent, and starts the day after it
# ended. A statement missing from the
# middle, or two accounts' statements in one file, break it. Ordered by period
# end: a new account's first statement prints no start.
.rc_join <- function(us) {
  one <- function(u) {
    h <- u$parsed$header %||% list()
    list(start = .plausible_period_date(h$period_start %||% NA), end = .plausible_period_date(h$period_end %||% NA), open = suppressWarnings(as.numeric(h$opening_balance %||% NA)[1]),
         close = suppressWarnings(as.numeric(h$closing_balance %||% NA)[1]))
  }
  st <- lapply(us, one)
  okv <- vapply(st, function(s) !is.na(s$end) && is.finite(s$open) && is.finite(s$close), logical(1))
  if (!all(okv)) return(list(ok = FALSE, why = sprintf(
    "Statement %d does not print its period end and both its balances, so the statements cannot be shown to follow on.", which(!okv)[1])))
  o <- order(vapply(st, function(s) as.numeric(s$end), 0))
  st <- st[o]
  for (i in seq_len(length(st) - 1L)) {
    if (abs(st[[i]]$close - st[[i + 1L]]$open) >= PARAM_MONEY_TOL)
      return(list(ok = FALSE, why = sprintf(paste("Statement %d closes at %s but statement %d, the next by date, opens at %s: a statement may be",
                                                  "missing between them, or they are different accounts."),
                                            o[i], format(st[[i]]$close, nsmall = 2), o[i + 1L], format(st[[i + 1L]]$open, nsmall = 2))))
    # A month missing between two statements whose balances happen to agree (a
    # quiet month) is still a gap: the next statement starts after the day
    # following the last one's end. A person looks.
    nx <- st[[i + 1L]]$start
    if (!is.na(nx) && as.numeric(nx - st[[i]]$end) > 1)
      return(list(ok = FALSE, why = sprintf(paste("No statement covers %s to %s, between statement %d and statement %d:",
                                                  "a statement may be missing from the file."),
                                            format(st[[i]]$end + 1), format(nx - 1), o[i], o[i + 1L])))
  }
  list(ok = TRUE, why = "In date order, each statement opens at the balance the one before it closed on.")
}

# ---- drafts: a new design, learned from a person's answer --------------------------
#
# ALWAYS ASK ONCE. A statement no accepted recipe recognises is never converted
# without a person (R/convert.R): its reading -- the automatic reader's, or a
# draft recipe's -- is put on Please check, already filled in. When the person
# says it is right AND the statement's own arithmetic proved it, the design is
# written down as a DRAFT recipe: its table heading, its columns left to right
# (under their heading words, or as bands when a column has none), its date format
# and money style. The draft is kept only if it reads the same statement to the
# same figures. Each later statement a draft reads and a person confirms is a
# proof; after LAYOUT_PROVEN_AFTER proofs from LAYOUT_PROVEN_ACCOUNTS accounts
# (told apart by salted marks, never kept: R/layouts.R) a new version is written
# as proven, and statements of the design are read on their own from then on.
# A draft names nothing the statement prints but its table heading and its period
# label: no title line (it may be a person's name), no account number.

# recipes_state_dir(layouts_dir, cfg) -- where drafts are written: paths$recipes,
# else beside the layouts (templates/recipes on a server). Never the shipped recipes/.
recipes_state_dir <- function(layouts_dir = NULL, cfg = NULL) {
  if (is.null(cfg)) cfg <- safe(load_config(), list())
  d <- as.character(unlist(safe(cfg$paths$recipes, NULL)))[1]
  if (!is.na(d) && nzchar(d)) return(d)
  if (is.null(layouts_dir) || is.na(layouts_dir)) return(NULL)
  file.path(dirname(layouts_dir), "recipes")
}

# .rc_draft(reading, input, bank, id) -> the draft as a list ready for YAML, or
# list(error) saying why the reading cannot be written as a recipe.
.rc_draft <- function(reading, input, bank, id) {
  no <- function(why) list(error = why)
  if (!is.na(.rc_sheet_kind(input))) return(.rc_draft_sheet(reading, input, bank, id))
  if (!identical(input$kind %||% "", "pdf") || isTRUE(any(as.logical(input$page_ocr %||% FALSE))))
    return(no("Only a text PDF is written as a recipe, so far."))
  cl <- reading$columns
  if (!is.data.frame(cl) || !nrow(cl)) return(no("The reading found no columns."))
  cl <- cl[cl$page == min(cl$page), , drop = FALSE]
  cl <- cl[order(cl$x_min), , drop = FALSE]
  money <- c("debit", "credit", "amount", "balance")
  f <- as.character(cl$field); n_text <- 0L
  for (j in seq_along(f)) if (!(f[j] %in% c("date", "description", money, "date2", "particulars", "code",
                                            "reference", "other_party", "type"))) {
    n_text <- n_text + 1L; f[j] <- sprintf("text%d", n_text)
  }
  if (n_text > 9L || anyDuplicated(f)) return(no("The reading's columns cannot be named as a recipe's."))
  hd <- trimws(as.character(cl$heading)); hd[is.na(hd)] <- ""
  # The reader's heading for a column can take in a title line above it ("Your
  # transactions" over "Date"): a recipe's heading is the words on ONE line.
  if (all(nzchar(hd))) hd <- .rc_draft_header(input, cl$page[1], hd) %||% rep("", length(hd))
  anchored <- all(nzchar(hd)) && !anyDuplicated(tolower(hd))
  cols <- stats::setNames(lapply(seq_along(f), function(j)
    if (anchored) list(under = hd[j]) else list(x_min = round(cl$x_min[j], 1), x_max = round(cl$x_max[j], 1))), f)
  tb <- reading$template$table %||% list()
  dfmt <- as.character(tb$date_format %||% NA_character_)[1]
  if (is.na(dfmt)) return(no("The reading has no date format."))
  yearful <- grepl("%[yY]", dfmt)
  per <- NULL
  if (!yearful) {
    # the year comes from the period: the label it is printed under, as printed
    p1 <- .rc_flat(.page_texts(input)[1])
    labs <- as.character(unlist(.period_labels()))
    labs <- labs[order(-nchar(labs))]
    hit <- labs[vapply(labs, function(l) .rc_has_phrase(p1, l), logical(1))]
    if (!length(hit)) return(no("The dates print no year and no period label was found to take it from."))
    per <- list(label = hit[1])
  }
  y <- list(recipe = id, format = RECIPE_FORMAT, version = 1L, bank = bank,
            title = sprintf("%s statement (drafted)", .layout_bank_display(bank)), kind = "pdf", status = "draft",
            recognise = list(all = if (anchored) as.list(hd) else list(per$label %||% "statement")),
            period = per,
            table = c(if (anchored) list(header = as.list(hd)) else list(ref_width = round(input$page_width[1] %||% .A4_W, 1)),
                      list(columns = cols)),
            dates = list(format = dfmt, year = if (yearful) "printed" else "period"),
            money = list(style = if ("amount" %in% f) "signed" else "debit_credit_cols"),
            order = if (identical(reading$template$auto$dir, "new")) "newest_first" else "oldest_first")
  Filter(Negate(is.null), y)
}

# .rc_draft_header(input, page, hd) -- the column headings as printed on ONE line
# of the page: for each line, each heading's longest tail of words printed on it,
# in order; the first line where every column has one. NULL when there is none.
.rc_draft_header <- function(input, page, hd) {
  lines <- .rc_line_tokens(input)[[page]] %||% list()
  words <- lapply(hd, .rc_words)
  for (L in lines) {
    pick <- vapply(words, function(w) {
      for (a in seq_along(w)) { ph <- paste(w[a:length(w)], collapse = " ")
        if (nzchar(ph) && !is.na(.rc_find_seq(L$tok, ph))) return(ph) }
      ""
    }, "")
    if (all(nzchar(pick)) && !is.null(.rc_header_spans(L$tok, L$x, L$x1, pick))) return(pick)
  }
  NULL
}

# recipe_learn(reading, input, bank, file_sha, accounts, dir) -> list(action, ref,
# why). Called by R/convert.R when a person confirms a reading the arithmetic
# proved (or a draft read). action: "created" (a new draft), "evidence_added" (one
# more proof), "promoted" (proven: read on its own from now on), "none".
recipe_learn <- function(reading, input, bank, file_sha, accounts = NULL, dir = NULL) {
  out <- function(action, why, ref = NA_character_) list(action = action, ref = ref, why = why)
  tryCatch({
    b <- .layout_slug(bank)
    if (is.na(b)) return(out("none", "No bank was given, so no recipe is drafted."))
    if (is.null(dir)) return(out("none", "No folder is set up for drafted recipes."))
    sha <- tolower(trimws(as.character(file_sha %||% "")[1]))
    if (is.na(sha) || !grepl("^[0-9a-f]{64}$", sha)) return(out("none", "The statement has no fingerprint, so it cannot be counted."))
    release <- .layout_lock(dir)
    if (is.null(release)) return(out("none", "The drafted recipes are being changed by someone else, so nothing was learned this time."))
    on.exit(release(), add = TRUE)
    mine <- recipes_load(dir)
    rc <- NULL
    if (isTRUE(reading$draft) && !is.null(reading$matched_recipe))
      rc <- Filter(function(r) identical(r$ref, reading$matched_recipe), mine)[1][[1]]
    if (is.null(rc)) {
      # Every id ever used here, retired ones too: a new draft never reuses one.
      ids <- unique(c(vapply(mine, `[[`, "", "id"),
                      sub("@v?[0-9]+[.]ya?ml$", "", list.files(dir, "[.]ya?ml$"))))
      taken <- suppressWarnings(as.integer(sub(sprintf("^%s_draft_", b), "", ids[startsWith(ids, paste0(b, "_draft_"))])))
      id <- sprintf("%s_draft_%d", b, max(c(0L, taken), na.rm = TRUE) + 1L)
      y <- .rc_draft(reading, input, b, id)
      if (!is.null(y$error)) return(out("none", y$error))
      rc <- .rc_validate(y)
      if (!is.null(rc$error)) return(out("none", paste("The draft is not a valid recipe:", rc$error)))
      # Kept only if it reads this statement to exactly the reading's figures.
      rr <- recipe_read(input, rc)
      key <- function(t) paste(t$date, sprintf("%.2f", t$amount), collapse = "|")
      if (!identical(rr$outcome, "proven") || !identical(key(rr$transactions), key(reading$transactions)))
        return(out("none", "Written as a recipe, this design did not read the statement back to the same figures, so no recipe is drafted."))
      f <- file.path(dir, sprintf("%s@v1.yaml", id))
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      yaml::write_yaml(y, f)
      rc$file <- f
      .rc_evidence_add(dir, rc$id, sha, accounts)
      return(out("created", sprintf("This design was saved as draft recipe %s: statements like it come back filled in, and after %d checked from %d accounts they are read on their own.",
                                    rc$ref, LAYOUT_PROVEN_AFTER, LAYOUT_PROVEN_ACCOUNTS), rc$ref))
    }
    ev <- .rc_evidence_add(dir, rc$id, sha, accounts)
    if (length(ev$proved_by) >= LAYOUT_PROVEN_AFTER && length(ev$groups) >= LAYOUT_PROVEN_ACCOUNTS) {
      y <- yaml::read_yaml(rc$file)
      y$version <- as.integer(rc$version) + 1L; y$status <- "proven"
      f <- file.path(dir, sprintf("%s@v%d.yaml", rc$id, y$version))
      if (!file.exists(f)) yaml::write_yaml(y, f)
      return(out("promoted", sprintf("Recipe %s now has %d checked statements from %d accounts, so statements like it are read on their own.",
                                     rc$id, length(ev$proved_by), length(ev$groups)), sprintf("%s@%d", rc$id, y$version)))
    }
    out("evidence_added", sprintf("Draft recipe %s: %d of %d checked statements, from %d of %d accounts.", rc$ref,
                                  length(ev$proved_by), LAYOUT_PROVEN_AFTER, length(ev$groups), LAYOUT_PROVEN_ACCOUNTS), rc$ref)
  }, error = function(e) out("none", paste0("No recipe was drafted (", conditionMessage(e), ").")))
}

# .rc_evidence_add(dir, id, sha, accounts) -- one more checked statement for a
# draft: kept in <dir>/.evidence/<id>.yaml as the statements' fingerprints and the
# accounts' salted marks (R/layouts.R), never a number.
.rc_evidence_add <- function(dir, id, sha, accounts) {
  f <- file.path(dir, ".evidence", paste0(id, ".yaml"))
  ev <- if (file.exists(f)) safe(yaml::read_yaml(f), list()) else list()
  salt <- as.character(ev$salt %||% .layout_new_salt(id, sha))[1]
  acc <- .layout_accounts_add(list(salt = salt, groups = as.character(unlist(ev$groups))), .layout_marks(accounts, salt))
  ev <- list(salt = salt, groups = as.list(acc$groups), proved_by = as.list(unique(c(as.character(unlist(ev$proved_by)), sha))))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(ev, f)
  list(proved_by = unlist(ev$proved_by), groups = unlist(ev$groups))
}
