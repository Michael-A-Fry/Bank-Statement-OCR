# convert.R -- the front door: one statement file in, one result out, bank first
# and automatic (spec sections 4-6). NEVER throws: every failure becomes a
# `failed` result with an actionable message. Logs exactly one record per run.
#
#   read the file -> which bank (bank_identify + the person's pick, bank_pick)
#   -> that bank's learned layouts -> auto_read (content, proven by arithmetic)
#   -> outputs -> learn the layout when the reading is proven -> tracking -> run log
#
# The reader's outcome decides the status every caller already knows:
#   proven, layout_match -> "ok"
#   check                -> "needs_review", with the reader's reason
#   unread               -> "unsupported" (nothing usable read), or "failed" when
#                           the file itself could not be read
#
# ---- build provenance: making determinism PROVABLE ---------------------------
# "Same input + same learned state = same answer" can only be checked if every
# output says WHICH build and WHICH learned state produced it:
#   * engine_version()   -- the repo-root VERSION file (one line, e.g. "2.0.0");
#   * layouts_state_id() -- a hash of every learned layout file (R/layouts.R);
#   * the layout id@version the reading matched, its outcome and proof kind.
# All are stamped into the run log, the JSON output and the feed manifest.

# engine_version() -- the build stamp, read ONCE per session (the file cannot
# change under a running air-gapped install). Missing/unreadable -> "unknown":
# an honest "we don't know" beats a guessed number, and it never errors.
.VERSION_CACHE <- new.env(parent = emptyenv())
engine_version <- function() {
  if (!is.null(.VERSION_CACHE$v)) return(.VERSION_CACHE$v)
  # ENGINE_ROOT is how the scripts/tests point at the install; "." covers the
  # app + RUN-ME.bat, which always start in the repo root.
  cand <- unique(c(file.path(Sys.getenv("ENGINE_ROOT", "."), "VERSION"), "VERSION"))
  v <- NA_character_
  for (p in cand) {
    if (!file.exists(p)) next
    ln <- trimws(safe(safe_readlines(p), character(0)))
    ln <- ln[!is.na(ln) & nzchar(ln)]
    if (length(ln)) { v <- ln[1]; break }
  }
  .VERSION_CACHE$v <- if (is.na(v)) "unknown" else v
  .VERSION_CACHE$v
}

# .text_sha256(x) -- content hash of a string (same fallback order as
# file_sha256). NA when no hashing package is installed, never an error.
.text_sha256 <- function(x) {
  x <- paste(as.character(x), collapse = "\n")
  if (requireNamespace("openssl", quietly = TRUE))
    return(paste0(openssl::sha256(charToRaw(x))))
  if (requireNamespace("digest", quietly = TRUE))
    return(digest::digest(x, algo = "sha256", serialize = FALSE))
  NA_character_
}

# log_run(logdir, result) -- write THE run-log record: exactly ONE file per run,
# named by run_id, holding the FINAL outcome.
#
# Separate from convert_statement so a caller that may still change the outcome
# can build the record (result$run_log) and write it once the outcome is final.
# No record is ever rewritten.
log_run <- function(logdir, result) {
  rec <- result$run_log
  if (is.null(rec) || !length(rec)) return(invisible(NULL))
  safe(write_log_record(logdir, "runs", rec$run_id %||% result$run_id %||% "unknown", rec))
}

# .unreadable_reason(input) -- one sentence when the READER got nothing usable out
# of the file, NULL when it did. This is the line between "could not be read" and
# "read, but not as a statement", and the two need opposite answers: the first is
# the sender's problem (re-export, rescan, unlock), the second is a reading to
# check. Getting it wrong wastes the analyst's time on a file that can never
# convert, so each test below is a FACT about the bytes, never a judgement:
#   pdf         -- read_pdf sets `pages` to NULL only when pdftools could not read
#                  the document at all. A SCAN reads fine and comes back with empty
#                  pages plus scanned_no_ocr, and keeps its own (correct, louder)
#                  diagnostic -- so this must test the reader's verdict, not the
#                  emptiness of the text.
#   delimited   -- a delimited file is a table only if its lines SEPARATE INTO THE
#                  SAME SHAPE, line after line (see .delimited_tabular), under one
#                  of the four separators the reader itself tries. A header-only
#                  export is still a table, so it reaches the reader (which says
#                  it found no transaction rows) rather than being called
#                  unreadable.
#   excel       -- no sheet, or a sheet with no columns, is nothing to read.

# The narrowest a statement table can be: a date, a description and a figure, so
# three is the FEWEST fields a header row of a statement ever carries (the reader
# itself wants three). Used ONLY on a file with a single record, where there is no
# second line to compare a shape against -- everywhere else the repeated shape
# decides, and a two-column file with rows in it really is a table.
.MIN_TABLE_FIELDS <- 3L
# How far in to look for the table. The evidence is a repeated shape, so it is
# settled within the first couple of data rows of wherever the table starts;
# reading further only costs time on every conversion of a large export.
.TABULAR_SCAN_LINES <- 200L

# .delimited_tabular(lines, sep) -- do these lines hold a TABLE under `sep`?
#
# A separator CHARACTER is not evidence: one comma in "Use the transaction export,
# not the PDF." was enough to make that sentence "a statement layout we don't have
# a template for yet" -- the exact answer this guard exists to prevent, and an
# email body, meeting minutes and a line holding one ';' all did the same. What
# makes a file a table is that the same split gives the SAME SHAPE on line after
# line. Split with the reader's own quote-aware splitter so a quoted comma and a
# preamble are counted exactly as the reader will read them.
#
# `lines` MUST ARRIVE WITH ITS BLANK LINES STILL IN IT. The evidence this function
# weighs is ADJACENCY -- two matching records NEXT TO each other -- and a blank
# line is the commonest thing in the world separating two lines that are not next
# to each other. The caller used to strip blanks before calling, which quietly
# turned a note to self
#     Hi Beth,  /  <blank>  /  Use the transaction export, not the PDF.
# into two adjacent 2-field records, i.e. a table, i.e. "we don't have a template
# for this layout yet" plus an offer to build one. The guard's own comment below
# says the matching lines in an email are SCATTERED through prose; keeping the
# blanks is what leaves them scattered. A blank record splits into no fields at
# all, so it counts as 0 and breaks the run without being mistaken for content.
.delimited_tabular <- function(lines, sep) {
  recs <- .split_records(lines, seq_along(lines))
  n <- vapply(recs, function(r) length(.record_fields(r$text, sep)), integer(1))
  if (!length(n) || max(n) < 2L) return(FALSE)
  # Two or more CONSECUTIVE records splitting the same way: a table. Runs, not
  # totals -- an email whose greeting and sign-off both end in a comma has two
  # matching lines scattered through prose, and is not a table anywhere.
  r <- rle(n)
  if (any(r$lengths[r$values >= 2L] >= 2L)) return(TRUE)
  # A header with NO rows under it has nothing to repeat, so the shape cannot
  # decide it -- and the two candidates are indistinguishable byte for byte: a
  # header-only export (an empty period, perhaps under a preamble), or a sentence
  # with a comma in it. Fall back to the narrowest a statement table can be, on
  # the LAST record that carries content (a header is never followed by prose;
  # a trailing newline cannot cost a header-only export its one fallback).
  n[max(which(n > 0L))] >= .MIN_TABLE_FIELDS
}

.unreadable_reason <- function(input) {
  kind <- input$kind %||% NA_character_
  if (identical(kind, "pdf")) {
    if (is.null(input$pages) || !length(input$pages))
      return(paste("no text could be read from this PDF - it is damaged, encrypted,",
                   "or not a PDF at all"))
    return(NULL)
  }
  if (identical(kind, "delimited")) {
    raw <- input$lines %||% character(0)
    raw[is.na(raw)] <- ""
    content <- which(nzchar(trimws(raw)))
    if (!length(content)) return("this file is empty - there is nothing in it to read")
    # THE SHAPE TEST GETS THE FILE'S REAL LINE STRUCTURE, blanks included -- see
    # .delimited_tabular, which decides on ADJACENCY and so cannot be handed a
    # view of the file with the gaps taken out. The window is the first
    # .TABULAR_SCAN_LINES lines WITH CONTENT, with whatever separated them, so a
    # blank preamble cannot push the table out of view.
    head_lines <- raw[seq_len(content[min(length(content), .TABULAR_SCAN_LINES)])]
    for (s in c(",", "\t", ";", "|")) if (.delimited_tabular(head_lines, s)) return(NULL)
    return(paste("no rows of separated values were found in this file - it holds",
                 "text, but not a table"))
  }
  if (identical(kind, "excel")) {
    tbl <- input$table
    if (is.null(tbl) || !ncol(tbl))
      return("no worksheet with any data could be read from this workbook")
    return(NULL)
  }
  NULL
}

# ---- a person's fix from Please check ----------------------------------------------
#
# `overrides` is what the Please check screen sends back:
#   roles    named character vector (or list): the reading's column field, as in
#            result$columns$field ("debit", "credit", "other1", ...) -> the role
#            the person says it has: "debit" (money out), "credit" (money in),
#            "amount", "balance", or "other" / "ignore" (not money in or out).
#   columns  the last resort, from the column editor: data.frame(field, x_min,
#            x_max[, page]) -- edited boxes for a PDF, in the page's own points.
# A fix is read again from the statement and must still prove itself: a person
# decides what the columns ARE, the arithmetic decides whether that adds up.

.FIGURE_ROLES <- c("debit", "credit", "amount", "balance", "other")

# .figure_fields(roles) -- the field each figure column carries in a reading's
# `columns` (the reader names a column of "other" figures other1, other2, ...).
.figure_fields <- function(roles) {
  ifelse(roles == "other", paste0("other", cumsum(roles == "other")), roles)
}

# .override_roles(reading, map) -> list(roles, changes) or list(error).
.override_roles <- function(reading, map) {
  roles <- as.character(unlist(reading$template$auto$roles))
  if (!length(roles)) return(list(error = "This reading found no columns of figures to set."))
  m <- unlist(map)
  nm <- names(m)
  if (!length(m) || is.null(nm) || any(is.na(nm) | !nzchar(nm)))
    return(list(error = "Each role must name the column it is for."))
  fields <- .figure_fields(roles)
  to <- tolower(trimws(as.character(m)))
  to[to %in% c("ignore", "none")] <- "other"
  if (any(!(to %in% .FIGURE_ROLES)))
    return(list(error = sprintf("\"%s\" is not a role a column of figures can have.", as.character(m)[!(to %in% .FIGURE_ROLES)][1])))
  if (any(!(nm %in% fields)))
    return(list(error = sprintf("\"%s\" is not one of this statement's columns of figures, so it cannot be given a money role.", nm[!(nm %in% fields)][1])))
  new <- roles
  new[match(nm, fields)] <- to
  n <- function(r) sum(new == r)
  why <- if (n("balance") > 1L) "Only one column can be the balance."
    else if (n("amount") > 1L || n("debit") > 1L || n("credit") > 1L) "Only one column can be money out, one money in, and one the amount."
    else if (n("amount") == 1L && n("debit") + n("credit") > 0L) "A statement has one amount column or money-out and money-in columns, not both."
    else if (n("amount") + n("debit") + n("credit") == 0L) "At least one column must be money out, money in or the amount."
    else NULL
  if (!is.null(why)) return(list(error = why))
  changes <- lapply(which(new != roles), function(j)
    list(role_from = fields[j], role_to = new[j], dx_left = 0, dx_right = 0))
  list(roles = new, changes = changes)
}

.BOX_CORE <- c("date", "description", "amount", "debit", "credit", "balance",
               "particulars", "code", "reference", "other_party", "type")

# .override_boxes(input, reading, boxes) -> list(reading, changes) or list(error).
# The edited boxes are read with the engine's table reader. The arithmetic must
# then hold on EVERY row by itself: a balance printed on every row, every step of
# it adding up, from a printed opening balance to the printed closing one. Less
# than that is not a proof, and the boxes apply to this file only.
.override_boxes <- function(input, reading, boxes) {
  if (!identical(input$kind, "pdf")) return(list(error = "Column boxes can only be drawn on a PDF."))
  tpl <- reading$template
  if (!is.list(tpl) || !identical(tpl$format, "pdf")) return(list(error = "This reading has no columns to edit."))
  b <- tryCatch(as.data.frame(boxes, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(b) || !nrow(b) || !all(c("field", "x_min", "x_max") %in% names(b)))
    return(list(error = "The edited columns must each give a field and its left and right edges."))
  b$field <- as.character(b$field)
  b$x_min <- suppressWarnings(as.numeric(b$x_min)); b$x_max <- suppressWarnings(as.numeric(b$x_max))
  if (anyNA(b$x_min) || anyNA(b$x_max) || any(b$x_min >= b$x_max))
    return(list(error = "Each column's left edge must be left of its right edge."))
  ok_field <- b$field %in% c(.BOX_CORE, "date2", "weekday") | grepl("^(other|text)[0-9]{1,2}$", b$field)
  if (any(!ok_field)) return(list(error = sprintf("\"%s\" is not a column the reader knows.", b$field[!ok_field][1])))
  if (!("date" %in% b$field) || !any(c("amount", "debit", "credit") %in% b$field))
    return(list(error = "The columns need a date and at least one of money out, money in or the amount."))
  to_cols <- function(bx) {
    bx <- bx[order(bx$x_min), , drop = FALSE]
    stats::setNames(lapply(seq_len(nrow(bx)), function(i) list(x_min = bx$x_min[i], x_max = bx$x_max[i])), bx$field)
  }
  pages <- if ("page" %in% names(b)) suppressWarnings(as.integer(b$page)) else rep(NA_integer_, nrow(b))
  first <- if (all(is.na(pages))) b else b[!is.na(pages) & pages == min(pages, na.rm = TRUE), , drop = FALSE]
  cols <- to_cols(first)
  tpl$table$columns <- cols[intersect(names(cols), .BOX_CORE)]
  ex <- cols[setdiff(names(cols), .BOX_CORE)]
  tpl$table$extras <- if (length(ex)) ex else NULL
  tpl$table$columns_by_page <- if (all(is.na(pages))) NULL else lapply(seq_along(input$pages), function(p) {
    bp <- b[!is.na(pages) & pages == p, , drop = FALSE]
    if (nrow(bp)) to_cols(bp) else NULL
  })
  if (any(c("debit", "credit") %in% b$field)) tpl$table$amount_sign <- "debit_credit_cols"
  else if (identical(tpl$table$amount_sign, "debit_credit_cols")) tpl$table$amount_sign <- "signed"
  parsed <- parse_statement(input, tpl)
  recon <- reconcile(parsed, tpl)
  tx <- parsed$transactions
  n <- nrow(tx)
  k <- recon$kpis
  kst <- function(nm) { v <- k$status[k$name == nm]; if (length(v)) v[1] else "na" }
  nd <- sum(grepl("amount_from_balance", tx$flags %||% character(0), fixed = TRUE))
  # The year is stated, never guessed, here as in the reader (year_settled): a
  # year taken from a footer would be "proven" by a balance that holds either way.
  ny <- sum(grepl("date_year_inferred", tx$flags %||% character(0), fixed = TRUE))
  ck <- data.frame(check = c("rows_read", "amounts_read", "no_derived_amounts", "dates_readable",
                             "balance_chain", "opening_closing", "dates_in_period", "year_settled"),
    ok = c(n > 0L, n > 0L && !anyNA(tx$amount), nd == 0L,
           n > 0L && !anyNA(suppressWarnings(as.Date(tx$date))),
           n > 0L && !anyNA(tx$balance) && kst("running_balance_continuity") == "pass",
           kst("balance_reconciliation") == "pass", kst("dates_within_period") != "fail", ny == 0L),
    stringsAsFactors = FALSE)
  ck$why <- ifelse(ck$ok, "Holds with the edited columns.", c(
    "The edited columns read no rows.", "Some amounts could not be read in the edited columns.",
    "Some amounts were filled in from the balance.", "Some dates could not be read in the edited columns.",
    "The running balance is not printed on every row, or does not add up, with the edited columns.",
    "Opening balance plus the movements does not reach the printed closing balance.",
    "Some dates fall outside the statement period.",
    "Some dates print no year, and the only year on the page is in other text, such as a footer.")[seq_len(nrow(ck))])
  proven <- all(ck$ok)
  why <- if (proven) "With the edited columns the running balance holds on every row, from the opening balance to the closing balance."
         else paste("With the edited columns the reading still does not prove itself:", ck$why[!ck$ok][1])
  old <- reading$columns
  changes <- lapply(seq_len(nrow(first)), function(i) {
    o <- if (is.data.frame(old)) old[old$field == first$field[i], , drop = FALSE] else NULL
    if (is.null(o) || !nrow(o)) return(NULL)
    list(role_from = first$field[i], role_to = first$field[i],
         dx_left = round(first$x_min[i] - o$x_min[1], 1), dx_right = round(first$x_max[i] - o$x_max[1], 1))
  })
  changes <- Filter(function(ch) !is.null(ch) && (ch$dx_left != 0 || ch$dx_right != 0), changes)
  rd <- list(outcome = if (proven) "proven" else "check", why = why, template = tpl, parsed = parsed,
             recon = recon, transactions = tx,
             proof = list(kind = if (proven) "chain" else "none", links = max(0L, n - 1L), held = if (proven) max(0L, n - 1L) else 0L,
                          unique = FALSE, pages_with_rows = sort(unique(suppressWarnings(as.integer(sub("^pdf:p", "", parsed$provenance$source_ref %||% character(0)))))),
                          pages_used = seq_along(input$pages), derived = nd),
             checks = ck, candidates = data.frame(source = "person:boxes", passed = proven, why = why, stringsAsFactors = FALSE),
             columns = data.frame(page = ifelse(is.na(pages), 1L, pages), field = b$field,
                                  kind = ifelse(b$field %in% c("date", "date2", "weekday"), "date",
                                         ifelse(b$field %in% c("debit", "credit", "amount", "balance") | grepl("^other[0-9]*$", b$field), "money", "text")),
                                  x_min = b$x_min, x_max = b$x_max, ink_min = b$x_min, ink_max = b$x_max,
                                  heading = "", stringsAsFactors = FALSE),
             matched_layout = NULL, notes = character(0), engine = AUTO_READ_VERSION)
  list(reading = rd, changes = changes)
}

# .figures(reading) -- a reading's figures as one string: dates and amounts.
.figures <- function(rd) {
  tx <- rd$transactions
  if (!is.data.frame(tx) || !nrow(tx)) return("")
  paste(tx$date, sprintf("%.2f", round(tx$amount, 2) + 0), collapse = "|")
}

# .read_statement(input, layouts, bank, overrides, unless_auto) -> list(reading,
# fix). One statement, read from its content against the bank's layouts; then,
# when the person sent a fix, read again with it. `fix` is NULL without one, else
# list(kind, proven, changes) or list(kind, error). `unless_auto`: the fix is
# meant for another statement of a bundle, so one this statement's own reading
# already converts is left alone.
.read_statement <- function(input, layouts, bank, overrides, unless_auto = FALSE) {
  base <- auto_read(input, layouts, bank)
  if (unless_auto && base$outcome %in% .AUTO) return(list(reading = base, fix = NULL))
  has_roles <- length(overrides$roles) > 0L
  has_boxes <- NROW(overrides$columns) > 0L
  if (!has_roles && !has_boxes) return(list(reading = base, fix = NULL))
  rd <- base; changes <- list(); kind <- NA_character_
  if (has_roles) {
    o <- .override_roles(base, overrides$roles)
    if (!is.null(o$error)) return(list(reading = base, fix = list(kind = "roles", error = o$error)))
    # Roles the person leaves as shown still settle something when the reading was
    # not proven (which of two readings that both add up is meant); on a proven
    # reading they change nothing, and nothing is read again.
    if (length(o$changes) || !identical(base$outcome, "proven")) {
      rd <- auto_read(input, list(), bank, list(roles = o$roles))
      changes <- o$changes; kind <- "roles"
      # A fix that reads different figures from a reading the arithmetic already
      # proved has not proved anything: both add up, so the statement cannot say
      # which is right, and the proven one is not overruled on one person's word.
      if (identical(base$outcome, "proven") && identical(rd$outcome, "proven") &&
          !identical(.figures(rd), .figures(base))) {
        rd$outcome <- "check"
        rd$why <- "The roles given read different figures from a reading the statement's own arithmetic already proved, and both add up, so neither is proven."
      }
    }
  }
  if (!has_boxes && is.na(kind)) return(list(reading = base, fix = NULL))
  if (has_boxes) {
    bx <- tryCatch(.override_boxes(input, rd, overrides$columns),
                   error = function(e) list(error = paste0("The edited columns could not be read (", conditionMessage(e), ").")))
    if (!is.null(bx$error)) return(list(reading = rd, fix = list(kind = "boxes", error = bx$error)))
    rd <- bx$reading; changes <- c(changes, bx$changes); kind <- "boxes"
  }
  list(reading = rd, fix = list(kind = kind, proven = identical(rd$outcome, "proven"), changes = changes))
}

# .unit_overrides(overrides, i, k, pages) -> list(ov, unless_auto) or list(error):
# the part of a person's fix meant for statement i of a k-statement file, whose
# pages in the file are `pages`. A fix sent for one statement of a bundle must not
# re-read the others: their columns carry the same field names, and a proven
# statement re-read with another's roles would be broken by it. So a fix names its
# statement (`overrides$statement`), or else applies only to the statements that
# did not convert on their own. Box pages are the FILE's pages, as the screen
# draws them, renumbered here to the statement's own.
.unit_overrides <- function(overrides, i, k, pages) {
  if (is.null(overrides) || k <= 1L) return(list(ov = overrides, unless_auto = FALSE))
  st <- suppressWarnings(as.integer(unlist(overrides$statement)))
  if (length(st) && (anyNA(st) || any(st < 1L | st > k)))
    return(list(error = sprintf("The fix names a statement this file does not hold (it holds %d).", k)))
  if (length(st) && !(i %in% st)) return(list(ov = NULL, unless_auto = FALSE))
  ov <- overrides
  b <- tryCatch(as.data.frame(ov$columns, stringsAsFactors = FALSE), error = function(e) NULL)
  if (NROW(b) && "page" %in% names(b)) {
    pg <- suppressWarnings(as.integer(b$page))
    b <- b[is.na(pg) | pg %in% pages, , drop = FALSE]
    b$page <- match(suppressWarnings(as.integer(b$page)), pages)
    ov$columns <- if (nrow(b)) b else NULL
    if (!length(ov$roles) && is.null(ov$columns)) return(list(ov = NULL, unless_auto = FALSE))
  }
  list(ov = ov, unless_auto = !length(st))
}

# ---- spot checks --------------------------------------------------------------------

# .spot_pick(sha, rate) -- is this automatic conversion one a person should
# eyeball? Decided by the statement's own fingerprint, so it is deterministic (the
# same file is always, or never, picked) and, over many files, picked at `rate`.
.spot_pick <- function(sha, rate) {
  rate <- suppressWarnings(as.numeric(rate %||% 0)[1])
  if (is.na(rate) || rate <= 0 || is.na(sha) || !grepl("^[0-9a-f]{8}", sha)) return(FALSE)
  strtoi(substr(sha, 1L, 7L), 16L) / 16^7 < min(1, rate)
}

# spot_check_record(result, verdict, tracking_dir) -- a person's eyeball of a
# conversion picked for a spot check: "right", "wrong" or "cant_tell". Recorded
# with the layout and outcome it judges, never the statement's content.
spot_check_record <- function(result, verdict, tracking_dir = NULL) {
  tdir <- if (is.null(tracking_dir)) safe(tracking_dir(), NULL) else tracking_dir
  if (is.null(tdir) || isTRUE(is.na(tdir))) return(invisible(structure(FALSE, reason = "tracking is switched off")))
  st <- result$stamp %||% list()
  f <- list(event = "spot_check", spot_check = as.character(verdict %||% NA)[1],
            engine_version = engine_version(), state_id = st$layouts_state, outcome = st$outcome,
            proof_kind = st$proof_kind, kind = st$kind)
  if (!is.na(st$institution %||% NA)) f$institution <- st$institution
  if (!is.na(st$bank_code %||% NA)) f$bank_code <- st$bank_code
  ref <- st$layout %||% NA_character_
  if (!is.na(ref) && grepl("^[a-z0-9_]+@v?[0-9]+$", ref)) {
    f$layout_id <- sub("@v?[0-9]+$", "", ref); f$layout_version <- as.integer(sub("^.*@v?", "", ref))
  }
  track_record(Filter(Negate(is.null), f), tdir)
}

# ---- the outcome of one file ----------------------------------------------------------

# .log_scrub(x) -- a sentence fit for the run log. The reader's reasons can quote
# a line of the statement ("The dated line "..." is not part of any row"), and a
# quoted line can hold a name or an account number; the person sees the whole
# sentence on screen, the log keeps it without the quote or any long number.
.log_scrub <- function(x) {
  x <- gsub("\"[^\"]*\"", "\"...\"", as.character(x))
  gsub("[0-9][0-9 -]{3,}[0-9]", "#", x, perl = TRUE)
}

.AUTO <- c("proven", "layout_match")
# The reader's checks that, failing, say the figures are WRONG rather than unproven.
.CONTRADICTIONS <- c("balance_chain", "chain_across_pages", "opening_closing", "printed_totals")

# .zero_unsigned(df) -- a zero printed in the money-out column is read as a
# money-out zero (the reader keeps its direction); in the files it is 0.00, never
# -0.00, which a reader of the workbook would take for a figure.
.zero_unsigned <- function(df) {
  if (!is.data.frame(df)) return(df)
  for (nm in names(df)) if (is.double(df[[nm]])) {
    z <- !is.na(df[[nm]]) & df[[nm]] == 0
    if (any(z)) df[[nm]][z] <- 0
  }
  df
}

# .from_header(input, template) -- a CSV or workbook handed on from its
# column-heading row: the metadata capture and the layout hint read the first
# line (a sheet's first row) as the headings, and an export's first line can be a
# preamble naming the account and its holder. Attribute `header_found` says
# whether the reader's heading row was found; where it was not, a CSV keeps no
# lines and a workbook no heading names, rather than a preamble's.
.from_header <- function(input, template) {
  if (!(input$kind %||% "") %in% c("delimited", "excel")) return(input)
  hd <- tolower(trimws(as.character(unlist(template$fingerprint$header_contains_all))))
  has_all <- function(l) length(hd) > 0L && all(vapply(hd, grepl, NA, x = l, fixed = TRUE))
  at <- integer(0)
  if (identical(input$kind, "delimited")) {
    ln <- input$lines %||% character(0)
    at <- which(vapply(tolower(ln), has_all, NA))
    input$lines <- if (length(at)) ln[at[1]:length(ln)] else character(0)
  } else if (is.data.frame(input$table)) {
    tb <- input$table
    rows <- c(list(names(tb)), lapply(seq_len(nrow(tb)), function(i) as.character(unlist(tb[i, ]))))
    at <- which(vapply(rows, function(r) has_all(tolower(paste(r[!is.na(r)], collapse = "\t"))), NA))
    if (length(at)) {
      nm <- rows[[at[1]]]
      nm[is.na(nm)] <- ""
      input$table <- tb[setdiff(seq_len(nrow(tb)), seq_len(at[1] - 1L)), , drop = FALSE]
      names(input$table) <- nm
    } else names(input$table) <- sprintf("column%d", seq_along(tb))
  }
  attr(input, "header_found") <- length(at) > 0L
  input
}

# .unit_sha(sha, i, k) -- the fingerprint one statement is counted by as evidence
# for a layout: the file's own, or for statement i of a bundle a fingerprint of
# its own, so each statement of a bundle is one statement of evidence.
.unit_sha <- function(sha, i, k) {
  if (is.na(sha) || k <= 1L) return(sha)
  .text_sha256(paste0(sha, "#statement", i))
}

# .unit_accounts(reading, meta, input) -- the account numbers one statement shows,
# so the statements that prove a layout can be told apart by account (spec
# section 6: "from at least 2 different accounts"). The same set the metadata
# record's account_hash is made from (R/metadata_capture.R): the account number
# the reading took from the statement, and every account-shaped number printed on
# it. Handed to layout_learn() in memory only; R/layouts.R keeps a short salted
# mark of each, never a number. `meta` is the file's extract_metadata() for a
# single statement, NULL for one statement of a bundle (read from its own pages).
.unit_accounts <- function(reading, meta, input) {
  if (is.null(meta)) meta <- safe(extract_metadata(input), list())
  acc <- as.character(unlist(c(reading$parsed$header$account_number, meta$accounts)))
  acc[!is.na(acc) & nzchar(trimws(acc))]
}

# .bundle_date(x) -- a period start as a Date, or NA; never an error (a header date
# may arrive as text in the statement's own style).
.bundle_date <- function(x) {
  x <- as.character(x %||% NA)[1]
  if (is.na(x) || !nzchar(x)) return(as.Date(NA))
  for (f in c("%Y-%m-%d", "%d %b %Y", "%d %B %Y", "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y")) {
    d <- as.Date(x, format = f, optional = TRUE)
    if (!is.na(d)) return(d)
  }
  as.Date(NA)
}

# .bundle_joins(readings) -- do a bundle's statements join up into one unbroken run
# of the same account? Every statement proven with both its ends printed, each with
# a period start and an opening and closing balance, and, ordered by period, each
# opening equal to the previous closing to the cent.
.bundle_joins <- function(readings) {
  if (length(readings) < 2L) return(TRUE)
  one <- function(r) {
    h <- r$parsed$header %||% list()
    ck <- r$checks
    ends <- if (is.data.frame(ck)) ck$ok[ck$check == "ends_printed"] else logical(0)
    list(proven = identical(as.character(r$outcome %||% "")[1], "proven") && length(ends) == 1L && isTRUE(ends),
         start = .bundle_date(h$period_start),
         open = suppressWarnings(as.numeric(h$opening_balance %||% NA)[1]),
         close = suppressWarnings(as.numeric(h$closing_balance %||% NA)[1]))
  }
  st <- lapply(readings, one)
  if (!all(vapply(st, function(x) x$proven && !is.na(x$start) && is.finite(x$open) && is.finite(x$close), logical(1))))
    return(FALSE)
  st <- st[order(vapply(st, function(x) as.numeric(x$start), 0))]
  all(vapply(seq_len(length(st) - 1L), function(i) abs(st[[i]]$close - st[[i + 1L]]$open) < 0.005, logical(1)))
}

# convert_statement(path, bank, ...) -> result (build-contract sections 6, 7).
#   bank            the person's pick (an institution id or name), or NULL to take
#                   it from the statement (bank_pick); a confident disagreement
#                   between the two blocks learning and says so.
#   bank_confirmed  TRUE once the person has seen that disagreement and kept their
#                   pick: learning is then allowed.
#   layouts_dir     the learned-layout store (paths$layouts by default).
#   tracking_dir    where tracking events go (paths$tracking); NA switches it off.
#   overrides       a person's fix from Please check (see above).
#   confirm         TRUE: the person confirms a reading the arithmetic could not
#                   prove as right. It converts as "ok" for that file, is fed, and
#                   is held for an admin (R/fixes.R); it never teaches by itself.
#   log = FALSE     builds the run record on the result (result$run_log) WITHOUT
#                   writing it, for a caller that may still change the outcome.
convert_statement <- function(path, bank = NULL, outdir = "out", logdir = "logs",
                              formats = c("xlsx", "csv", "json"), log = TRUE,
                              layouts_dir = NULL, tracking_dir = NULL,
                              overrides = NULL, confirm = FALSE, bank_confirmed = FALSE,
                              requested_by = NULL) {
  # NOTHING THAT TOUCHES `path` HAPPENS OUTSIDE THE FUNNEL below. What is left up
  # here cannot throw for any input: safe() covers the hash and the config, and the
  # run id is built from values that are already safe.
  base <- "input"
  cfg <- safe(load_config(), .config_defaults())
  t0 <- Sys.time()
  who <- safe(as.character(requested_by %||% current_user())[1], "unknown")
  # run_id: content hash (first 10) + UTC second + a short random suffix, so the
  # same statement converted twice in one second still gets two records.
  sha <- safe(file_sha256(path), NA_character_)
  run_id <- paste0(substr(if (is.na(sha)) "na" else sha, 1, 10), "-",
                   format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"), "-",
                   paste0(sample(c(0:9, letters[1:6]), 4, replace = TRUE), collapse = ""))
  ldir <- if (is.null(layouts_dir)) safe(layouts_dir(cfg), NULL) else layouts_dir
  tdir <- if (is.null(tracking_dir)) safe(tracking_dir(cfg), NULL) else tracking_dir
  if (isTRUE(is.na(tdir))) tdir <- NULL
  result <- new_result(status = "failed", messages = character(0))
  # A bank is a name; one holding a long number is an account number typed into
  # the bank box, and the bank names the layout folder, the layout ids and the
  # run log. It is not used, and the bank is taken from the statement instead.
  bank_in <- safe(as.character(bank %||% NA_character_)[1], NA_character_)
  bank_numbered <- !is.na(bank_in) && grepl("[0-9][0-9 -]{3,}[0-9]", bank_in)
  if (bank_numbered) bank <- NULL
  # The stamp: what produced this answer, for the run log, the JSON and the feed.
  # The learned state and the picked bank are known before the file is opened, so
  # even a file that cannot be read says what it was converted against.
  stamp <- list(engine_version = engine_version(), reader_version = AUTO_READ_VERSION,
                layouts_state = if (is.null(ldir)) "unknown" else safe(layouts_state_id(ldir), "unknown"),
                layout = NA_character_, outcome = "unread",
                proof_kind = "none", institution = safe(.layout_slug(bank_pick(list(), bank)$bank), NA_character_),
                bank_code = NA_character_, bank_confidence = NA_character_, kind = NA_character_)
  facts <- list(rows = 0L, learn = character(0), statements = 1L, multi = FALSE, fix = NA_character_,
                pages = NA_integer_, period_start = NA_character_, period_end = NA_character_, n_accounts = NA_integer_,
                layout_sig = NA_character_, layout_hint = NA_character_, reason = NA_character_, tracked = FALSE)

  outcome <- tryCatch({
    base <- tools::file_path_sans_ext(basename(path %||% "input"))
    input <- read_input(path)
    # THE FILE ITSELF COULD NOT BE READ: the sender's problem, not a reading to
    # check. Raised through the funnel, which makes it a `failed` result.
    if (!is.null(why <- .unreadable_reason(input))) stop(why, call. = FALSE)
    stamp$kind <- if (identical(input$kind, "pdf") && isTRUE(any(input$page_ocr))) "scan" else input$kind
    meta <- extract_metadata(input)
    multi <- detect_multiple_statements(input, meta)

    # ---- the bank: the person's pick, pre-filled and checked from the document ----
    ident <- bank_identify(input)
    pick <- bank_pick(ident, bank, isTRUE(bank_confirmed))
    bank_id <- as.character(pick$bank %||% NA_character_)[1]
    bank_slug <- .layout_slug(bank_id)
    bank_name <- if (is.na(bank_id)) NA_character_ else .layout_bank_display(bank_slug, bank_id)
    stamp$institution <- bank_slug
    stamp$bank_code <- if (!is.na(ident$bank_code %||% NA) && grepl("^[0-9]{2}$", ident$bank_code)) ident$bank_code else NA_character_
    stamp$bank_confidence <- ident$confidence %||% "unknown"
    layouts <- if (!is.na(bank_slug) && !is.null(ldir)) layouts_load(ldir, bank_id) else list()
    # Stamped again beside the load: reading the file (OCR) can take long enough
    # for another conversion to learn in between.
    stamp$layouts_state <- if (is.null(ldir)) "unknown" else layouts_state_id(ldir)
    # A bank in question -- the statement names another bank, confidently or
    # enough to ask -- teaches nothing until the person keeps their pick: a layout
    # learned under the wrong bank would be handed to that bank's statements.
    bank_held <- !is.na(bank_slug) && (isTRUE(pick$block_learning) || isTRUE(pick$ask))
    learn_ok <- !is.na(bank_slug) && !is.null(ldir) && !bank_held

    # ---- read: the whole file, or each statement of a bundle on its own ----
    segs <- bundle_segments(input, meta)
    units <- if (is.null(segs)) list(list(input = input, pages = seq_along(input$pages %||% input$words %||% 1L)))
             else lapply(segs, function(pg) list(input = .subinput_pages(input, pg), pages = pg))
    k <- length(units)
    reads <- lapply(seq_len(k), function(i) {
      uo <- .unit_overrides(overrides, i, k, units[[i]]$pages)
      if (!is.null(uo$error)) {
        rd <- .read_statement(units[[i]]$input, layouts, bank_name, NULL)
        return(list(reading = rd$reading, fix = list(kind = "statement", error = uo$error)))
      }
      .read_statement(units[[i]]$input, layouts, bank_name, uo$ov, uo$unless_auto)
    })
    readings <- lapply(reads, `[[`, "reading")
    facts$statements <- k
    # Bundles are identified statement by statement (spec section 5): one that
    # names another bank than the pick never teaches the picked bank's layouts.
    # Even weakly: the person kept one bank for the whole file, so their word does
    # not cover a statement that points elsewhere.
    unit_bank <- vapply(seq_len(k), function(i) {
      if (k == 1L || is.na(bank_slug)) return(NA_character_)
      u <- bank_identify(units[[i]]$input)
      if (!is.na(u$institution %||% NA) && !identical(u$institution, bank_slug))
        as.character(u$display %||% u$institution)[1] else NA_character_
    }, "")
    # No running balance and no totals: the reading stands only on a learned
    # layout's word, and that word is the picked bank's. With the bank in
    # question it is not taken without a person.
    for (i in seq_len(k)) if (identical(readings[[i]]$outcome, "layout_match") && (bank_held || !is.na(unit_bank[i]))) {
      readings[[i]]$outcome <- "check"
      readings[[i]]$why <- sprintf(paste("No running balance or totals are printed, so only a learned layout of %s reads it,",
                                         "and the statement looks like another bank's; confirm the bank first."), bank_name)
    }
    if (k > 1L) {
      comb <- bundle_combine(readings, segs, length(input$pages %||% input$words))
      parsed <- comb$parsed; recon <- comb$recon
    } else {
      parsed <- readings[[1]]$parsed; recon <- readings[[1]]$recon
    }
    outcomes <- vapply(readings, function(r) as.character(r$outcome %||% "unread"), "")
    has_rows <- !is.null(parsed) && is.data.frame(parsed$transactions) && nrow(parsed$transactions) > 0L
    fixes <- lapply(reads, `[[`, "fix")
    fix_err <- unique(unlist(lapply(fixes, function(f) f$error)))
    # A fix sent for a bundle that reached none of its statements is said, not dropped.
    if (k > 1L && (length(overrides$roles) || NROW(overrides$columns)) && all(vapply(fixes, is.null, NA)))
      fix_err <- c(fix_err, "it reached none of this file's statements (each already proves itself, or the boxes are on other pages), so say which statement it is for.")
    fix_kind <- unique(unlist(lapply(fixes, function(f) f$kind)))
    facts$fix <- if (length(fix_kind)) fix_kind[1] else NA_character_
    # The whole file's reading: the weakest statement's.
    worst <- outcomes[order(match(outcomes, c("proven", "layout_match", "check", "unread")), decreasing = TRUE)][1]
    lead <- readings[[match(worst, outcomes)]]
    stamp$outcome <- worst
    stamp$proof_kind <- if (worst %in% .AUTO) (lead$proof$kind %||% "none") else "none"
    lref <- unique(unlist(lapply(readings, function(r) r$matched_layout)))
    stamp$layout <- if (length(lref) == 1L) lref else NA_character_
    why_of <- function(i) {
      w <- readings[[i]]$why %||% "The reader gave no reason."
      if (k > 1L) sprintf("Statement %d of %d (pages %d-%d): %s", i, k, min(units[[i]]$pages), max(units[[i]]$pages), w) else w
    }
    first_bad <- which(!(outcomes %in% .AUTO))[1]
    reason <- if (!is.na(first_bad)) why_of(first_bad) else lead$why %||% ""
    # A spreadsheet's layout hint is its column headings, from the heading row the
    # reader found: the first line or row can be a preamble naming the account and
    # its holder, and the hint is kept in the run log for good. Without a found
    # heading row there is no hint.
    hdr_input <- .from_header(input, lead$template)
    lsig <- if (isFALSE(attr(hdr_input, "header_found"))) list(signature = NA_character_, hint = NA_character_)
            else safe(layout_signature(hdr_input), list(signature = NA_character_, hint = NA_character_))
    lsig$hint <- .log_scrub(lsig$hint %||% NA_character_)
    facts$layout_sig <- lsig$signature %||% NA_character_
    facts$layout_hint <- lsig$hint %||% NA_character_

    # ---- the status ----
    ocr_pages <- suppressWarnings(as.integer(input$meta$ocr_pages %||% 0L)); if (is.na(ocr_pages)) ocr_pages <- 0L
    ocr_conf <- suppressWarnings(as.numeric(input$meta$ocr_min_conf %||% NA_real_))
    # A badly read scan cannot come back "ok": the arithmetic proves the figures,
    # but not a date digit OCR was unsure of. A page it could not measure at all
    # is no evidence of a good read either.
    ocr_poor <- ocr_pages > 0L && (!is.finite(ocr_conf) || ocr_conf < PARAM_OCR_PAGE_MIN_CONF)
    derived <- if (has_rows) sum(grepl("amount_from_balance", parsed$transactions$flags, fixed = TRUE)) else 0L
    box_fixed <- any(vapply(fixes, function(f) identical(f$kind, "boxes") && is.null(f$error), logical(1)))
    status <- if (!has_rows || all(outcomes == "unread")) "unsupported"
              else if (all(outcomes %in% .AUTO) && !ocr_poor) "ok"
              else "needs_review"
    # ONE FILE, SEVERAL STATEMENTS. Each statement of a bundle is read and proven on
    # its own. The file stays automatic only when the statements also JOIN UP: each
    # proven with both its ends printed, and, in date order, each one opening at the
    # balance the one before it closed on, to the cent. A statement missing from the
    # middle, or statements of different accounts, break the join and a person
    # looks. One missing from the very start or end of the file leaves no trace, but
    # nothing converted is wrong: the output is exactly the statements the file
    # holds, each proven. Each statement's own proof still teaches its layout.
    if (k > 1L && identical(status, "ok") && !.bundle_joins(readings)) {
      status <- "needs_review"; worst <- "check"
      stamp$outcome <- "check"; stamp$proof_kind <- "none"
      reason <- sprintf(paste("This file holds %d statements, and each adds up on its own; but they do not follow on",
                              "from each other (a statement may be missing between them, or they may be different",
                              "accounts), so confirm the file holds every statement it should."), k)
    }
    # Nothing read: the run log carries a structural fingerprint of the file
    # instead (R/layout.R, unread_fingerprint), so Admin -> Health can group the
    # unreadable files by what they look like rather than as one "(unknown)" row.
    if (identical(status, "unsupported")) {
      lsig <- safe(unread_fingerprint(input, stamp$kind, meta$pages_actual,
                                      lapply(readings, function(r) r$columns), lsig), lsig)
      lsig$hint <- .log_scrub(lsig$hint %||% NA_character_)
      facts$layout_sig <- lsig$signature %||% NA_character_
      facts$layout_hint <- lsig$hint %||% NA_character_
    }
    if (identical(status, "needs_review") && all(outcomes %in% .AUTO) && ocr_poor)
      reason <- sprintf("The figures add up, but the scan was read with low confidence (%s), so a date or a word may be misread.",
                        if (is.finite(ocr_conf)) sprintf("%.0f%%", ocr_conf) else "not measured")
    # A person can vouch for a reading the arithmetic could not PROVE, never for one
    # it CONTRADICTS: a balance that does not add up says the figures are wrong.
    contra <- unlist(lapply(readings, function(r) {
      ck <- r$checks
      if (is.data.frame(ck)) ck$why[ck$check %in% .CONTRADICTIONS & ck$ok %in% FALSE] else NULL
    }))
    # Nor for a reading the fix sent with it was meant to change: the reading on
    # screen is not the one the person vouched for.
    confirmed <- isTRUE(confirm) && identical(status, "needs_review") && !("unread" %in% outcomes) &&
                 !length(contra) && !length(fix_err)
    refused <- isTRUE(confirm) && identical(status, "needs_review") && !confirmed
    basis <- if (identical(status, "ok")) { if (box_fixed) "person" else if (all(outcomes == "layout_match")) "layout_match" else "proven" }
             else if (confirmed) "person" else "none"
    if (confirmed) { status <- "ok"; stamp$proof_kind <- "person" }

    # ---- learning: only what the arithmetic proved (spec section 6) ----
    learn <- vector("list", k)
    held <- NULL
    # The layouts this file has already counted towards. A bundle is nearly always
    # one account's statements, so one file is one piece of evidence per layout;
    # layout_learn() then also needs the proofs to come from two accounts it can
    # tell apart (spec section 6) before a layout is proven.
    credited <- character(0)
    for (i in seq_len(k)) {
      r <- readings[[i]]; f <- fixes[[i]]
      learn[[i]] <- if (!learn_ok) list(action = "none", why = if (is.na(bank_slug)) "No bank was given, so nothing is learned."
                                         else if (is.null(ldir)) "No layout store is set up, so nothing is learned." else pick$why)
        else if (!is.na(unit_bank[i]))
          list(action = "none", why = sprintf("Statement %d looks like %s, not %s, so nothing is learned from it.", i, unit_bank[i], bank_name))
        else if (!is.null(f$error)) list(action = "none", why = "The fix could not be read, so nothing is learned.")
        else if (identical(f$kind, "roles") && isTRUE(f$proven)) {
          # A person's fix that the arithmetic then proves teaches straight away.
          m <- layout_match(r$template$signature, layouts_load(ldir, bank_id))
          cr <- layout_correct(if (is.null(m)) NULL else m$id, r$template, dir = ldir, by = who, bank = bank_id)
          list(action = if (isTRUE(cr$ok)) "corrected" else "none", ref = cr$ref,
               why = if (isTRUE(cr$ok)) sprintf("The corrected reading proves itself, so layout %s now reads this way.", cr$ref) else cr$why)
        }
        else if (identical(f$kind, "boxes"))
          list(action = "none", why = "Edited column boxes apply to this file only: a layout does not remember positions.")
        else if (identical(r$outcome, "proven") && is.null(f)) {
          m <- if (length(credited)) layout_match(r$template$signature, layouts_load(ldir, bank_id)) else NULL
          if (!is.null(m) && m$id %in% credited)
            list(action = "none", ref = m$ref, id = m$id,
                 why = sprintf("Another statement of this file already counts towards layout %s; one file is one piece of evidence.", m$id))
          else layout_learn(r, pick, .unit_sha(sha, i, k), ldir,
                            accounts = .unit_accounts(r, if (k == 1L) meta else NULL, units[[i]]$input))
        }
        else list(action = "none", why = "Only a reading the arithmetic proved teaches a layout.")
      if (!is.na(learn[[i]]$id %||% NA)) credited <- c(credited, learn[[i]]$id)
      # One person's word the arithmetic could not back waits for an admin -- but
      # only a reading the arithmetic does not CONTRADICT ("check", never "unread").
      if (learn_ok && is.na(unit_bank[i]) && is.null(f$error) && !identical(f$kind, "boxes") && identical(r$outcome, "check") &&
          ((identical(f$kind, "roles") && !isTRUE(f$proven)) || confirmed) && is.list(r$template))
        held <- c(held, fix_hold(r$template, bank_id, if (confirmed && is.null(f)) "confirm" else "roles", who, ldir)$id)
    }
    facts$learn <- vapply(learn, function(l) as.character(l$action %||% "none"), "")
    for (i in seq_len(k)) if (is.null(readings[[i]]$matched_layout) && !is.na(learn[[i]]$ref %||% NA) &&
                              learn[[i]]$action %in% c("created", "evidence_added", "promoted", "corrected"))
      readings[[i]]$learned_layout <- learn[[i]]$ref

    # ---- the files ----
    if (has_rows) {
      parsed$transactions <- .zero_unsigned(parsed$transactions)
      parsed$extras <- .zero_unsigned(parsed$extras)
      # A card's 0.00 closing balance turned round is -0 too, and the JSON writes it so.
      parsed$header[] <- lapply(parsed$header, function(v) { if (is.double(v)) v[!is.na(v) & v == 0] <- 0; v })
      recon$kpis <- .zero_unsigned(recon$kpis)
      parsed$header$bank <- bank_name
      parsed$header$institution <- bank_slug
    }
    facts$rows <- if (has_rows && status %in% c("ok", "needs_review")) nrow(parsed$transactions) else 0L
    pick_mismatch <- !is.na(ident$institution %||% NA) && !is.na(bank_slug) && !identical(ident$institution, bank_slug)
    diag <- build_diagnostics(status, parsed = parsed, recon = recon,
      reading = list(outcome = worst, why = reason, derived = derived, fix_error = fix_err, kind = stamp$kind,
                     bank_why = if (isTRUE(pick$ask) || pick_mismatch) pick$why else NULL,
                     bank_blocked = bank_held || any(!is.na(unit_bank))),
      metadata = list(ink_minus_signs = input$meta$ink_minus_signs %||% 0L,
                      faint_minus_signs = input$meta$faint_minus_signs %||% 0L,
                      # Did the sign-from-ink scan RUN? Only askable of a PDF; a
                      # statement read without it that prints its minus as ink would
                      # be read with the signs inverted.
                      ink_scan_ran = is.null(input$meta$pdf_doc) || isTRUE(input$meta$ink_scan_ok),
                      multi = if (k > 1L) utils::modifyList(multi, list(likely_multiple = FALSE)) else multi,
                      bundle_unsplit = k == 1L && isTRUE(multi$likely_multiple) && !identical(status, "ok"),
                      pages = meta$pages_actual, max_page_pt = meta$max_page_pt,
                      scanned_no_ocr = input$meta$scanned_no_ocr %||% 0L,
                      ocr_tools = input$meta$ocr_tools_available %||% TRUE,
                      pdf_doc = input$meta$pdf_doc))
    if (status %in% c("ok", "needs_review")) {
      result$outputs <- write_outputs(parsed, recon, outdir, base, formats,
        diagnostics = diag, metadata = meta, build = stamp)
      # The governed feed's ONLY source of transaction values: the table the
      # workbook and CSV show, taken BEFORE the display-only spreadsheet guard, so
      # a feed row and a workbook row hold byte-identical values.
      result$feed_rows <- display_transactions(parsed$transactions, parsed$extras)
    }

    # ---- what the person reads ----
    n <- facts$rows
    msg <- if (identical(basis, "person") && confirmed)
      status_message("ok", sprintf("%d row(s), confirmed as right by %s", n, who),
                     "the arithmetic could not prove this reading, so it applies to this file only until an admin confirms it")
    else if (identical(status, "ok"))
      status_message("ok", sprintf("%d row(s); %s", n, if (k > 1L) sprintf("%d statements, each proven on its own.", k) else lead$why))
    else if (identical(status, "needs_review"))
      status_message("needs_review", sub("[.]$", "", reason), "check the reading, then confirm it or set the columns' roles")
    else status_message("unsupported", sub("[.]$", "", reason), "check the columns on Please check, or set the file aside")
    if (derived > 0L)
      msg <- c(msg, sprintf("%d amount(s) could not be read and were worked out from the running balance instead; each one is marked in the Flags column.", derived))
    if (length(fix_err)) msg <- c(sprintf("The fix was not applied: %s", fix_err[1]), msg)
    if (refused) msg <- c(if (length(contra))
      sprintf("This reading cannot be confirmed: the statement's own arithmetic contradicts it (%s) Set the columns' roles instead.", contra[1])
      else if (length(fix_err)) "This reading cannot be confirmed: the fix sent with it was not applied, so it is not the reading you meant. Correct the fix, then confirm."
      else "This reading cannot be confirmed: part of the statement could not be read. Set the columns' roles instead.", msg)
    if (isTRUE(pick$ask) || pick_mismatch) msg <- c(msg, pick$why)
    if (any(!is.na(unit_bank)))
      msg <- c(msg, sprintf("Statement %d looks like %s, not %s; nothing is learned from it.",
                            which(!is.na(unit_bank))[1], unit_bank[!is.na(unit_bank)][1], bank_name))
    if (bank_numbered)
      msg <- c(msg, "The bank given held a long number, like an account number, so it was not used; the bank was taken from the statement.")

    result$status <- status
    result$template_id <- stamp$layout %||% NA_character_
    result$outcome <- worst
    result$reason <- reason
    result$feed_basis <- basis
    result$bank <- list(bank = bank_id, display = bank_name, institution = ident$institution %||% NA_character_,
                        identified_display = ident$display %||% NA_character_, bank_code = stamp$bank_code,
                        confidence = stamp$bank_confidence, why = pick$why, ask = isTRUE(pick$ask),
                        block_learning = bank_held)
    # Column pages are the FILE's pages, as the screen draws them and as a box fix
    # names them; a statement of a bundle is read on its own pages 1..n.
    file_cols <- function(i) {
      cl <- readings[[i]]$columns
      if (!is.data.frame(cl) || !nrow(cl) || !("page" %in% names(cl))) return(cl)
      cl$page <- units[[i]]$pages[cl$page]
      cl
    }
    result$reading <- lapply(seq_len(k), function(i) {
      r <- readings[[i]]
      list(outcome = r$outcome, why = r$why, pages = units[[i]]$pages, proof = r$proof, checks = r$checks,
           candidates = r$candidates, columns = file_cols(i), matched_layout = r$matched_layout,
           learned_layout = r$learned_layout, roles = r$template$auto$roles, template = r$template,
           transactions = r$transactions, notes = r$notes, fix = fixes[[i]], learn = learn[[i]])
    })
    result$columns <- file_cols(1L)
    result$learn <- learn
    result$fix_held <- held
    result$person <- list(confirmed = confirmed, by = if (confirmed || length(fix_kind)) who else NA_character_,
                          fix = facts$fix, fix_proven = if (length(fix_kind)) all(vapply(fixes, function(f) isTRUE(f$proven), logical(1))) else NA)
    result$derived <- derived
    # A statement with no balance and no totals that converts on a proven layout's
    # word (layout_match) has no arithmetic of its own behind it, so it is
    # spot-checked twice as often (spec section 2).
    rate <- suppressWarnings(as.numeric(cfg$auto_reading$spot_check_rate %||% 0)[1])
    if (identical(basis, "layout_match")) rate <- min(1, 2 * rate)
    result$spot_check <- basis %in% c("proven", "layout_match") && .spot_pick(sha, rate)
    result$trust <- recon$trust %||% list(level = "low", score = 0, reasons = reason)
    result$kpis <- recon$kpis
    result$header <- parsed$header %||% list()
    result$diagnostics <- diag
    result$coverage <- if (has_rows) safe(field_coverage(parsed, lead$template), NULL) else NULL
    result$metadata <- c(meta, list(multiple = multi,
      split = if (k > 1L) list(n_statements = k, on = "page1_marker", statements = comb$statements) else NULL))
    result$messages <- msg
    facts$pages <- meta$pages_actual %||% NA_integer_
    facts$period_start <- result$header$period_start %||% meta$period_start %||% NA_character_
    facts$period_end <- result$header$period_end %||% meta$period_end %||% NA_character_
    facts$n_accounts <- meta$n_accounts %||% NA_integer_
    facts$multi <- isTRUE(multi$likely_multiple)
    facts$reason <- reason

    # ---- tracking: one event per statement read, no personal data ----
    if (!is.null(tdir)) for (i in seq_len(k)) {
      r <- readings[[i]]; f <- fixes[[i]]
      tf <- track_reading_fields(r, bank = list(institution = bank_slug, bank_code = stamp$bank_code),
                                 state_id = stamp$layouts_state, pages = length(units[[i]]$pages))
      tf$kind <- stamp$kind
      if (!is.null(r$learned_layout) && is.null(tf$layout_id)) {
        tf$layout_id <- sub("@v?[0-9]+$", "", r$learned_layout)
        tf$layout_version <- as.integer(sub("^.*@v?", "", r$learned_layout))
      }
      tf$learn_action <- learn[[i]]$action %||% "none"
      if (!is.null(f$kind) && is.null(f$error)) { tf$event <- "correction"; tf$correction <- f$changes }
      if (confirmed) { tf$event <- "confirm"; tf$proof_kind <- "person" }
      suppressWarnings(track_record(tf, tdir))
    }
    facts$tracked <- TRUE

    # LOCAL-ONLY metadata capture, built where every artifact is in scope and
    # written after the run log below. NEVER enters the feed.
    result$metadata_capture <- safe(capture_metadata(list(
      run_id = run_id, ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      requested_by = who, sha = sha, input = hdr_input, parsed = parsed, recon = recon,
      meta = meta, multi = multi, template = lead$template, status = status,
      layout_sig = lsig, coverage = result$coverage,
      elapsed_ms = as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000), cfg), NULL)
    result
  }, error = function(e) {
    # Everything that lands here failed before anything was read, so the cure is
    # the file's: same wording as the `unreadable` diagnostic.
    r <- new_result(status = "failed")
    r$messages <- status_message("failed", conditionMessage(e),
                                 "check the file opens, is the type it claims to be, and is not password-protected or damaged")
    r$reason <- conditionMessage(e)
    r$diagnostics <- build_diagnostics("failed", messages = r$messages)
    r
  })

  result <- outcome
  result$run_id <- run_id
  result$stamp <- stamp
  # A file that could not be read is a statement the tool did not read: counted,
  # or the Admin page's automatic rate would leave out every failure.
  if (!is.null(tdir) && !isTRUE(facts$tracked))
    safe(suppressWarnings(track_record(Filter(Negate(is.null), list(
      event = "convert", engine_version = stamp$engine_version, state_id = stamp$layouts_state,
      institution = if (is.na(stamp$institution)) NULL else stamp$institution,
      kind = if (is.na(stamp$kind)) NULL else stamp$kind, rows = 0L, outcome = "unread",
      proof_kind = "none", learn_action = "none")), tdir)))

  # ---- run log: one file per run (concurrency-safe, no shared append) ----
  # BUILT here, WRITTEN by log_run(). The bank is its institution id and two-digit
  # code only: an account number never reaches a log.
  result$run_log <- list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    run_id = run_id,
    kind = "statement",
    requested_by = who,
    # safe(): written after the funnel closes, so one of the few lines left that a
    # hostile `path` could still throw from.
    source_file = safe(basename(path %||% NA_character_), NA_character_),
    source_sha256 = sha,
    bank_hint = safe(as.character(bank %||% NA_character_)[1], NA_character_),
    institution = stamp$institution,
    bank_code = stamp$bank_code,
    bank_confidence = stamp$bank_confidence,
    file_kind = stamp$kind,
    layout = stamp$layout,
    outcome = stamp$outcome,
    proof_kind = stamp$proof_kind,
    feed_basis = result$feed_basis %||% "none",
    learn_action = paste(facts$learn, collapse = ","),
    person_fix = facts$fix,
    spot_check = isTRUE(result$spot_check),
    reason = .log_scrub(facts$reason),
    layout_signature = facts$layout_sig,
    layout_hint = facts$layout_hint,
    engine_version = stamp$engine_version,
    reader_version = stamp$reader_version,
    layouts_state = stamp$layouts_state,
    status = result$status,
    trust_level = result$trust$level %||% NA_character_,
    row_count = facts$rows,
    derived_amounts = as.integer(result$derived %||% 0L),
    kpi_fail_count = sum(result$kpis$status == "fail"),
    pages = facts$pages,
    statements = facts$statements,
    period_start = facts$period_start,
    period_end = facts$period_end,
    n_accounts = facts$n_accounts,
    multiple_statements = facts$multi,
    message = .log_scrub(paste(result$messages, collapse = " | "))
  )
  if (isTRUE(log)) log_run(logdir, result)

  # ---- metadata capture: LOCAL ONLY, kept forever (logs/metadata/), never fed ----
  safe(write_metadata_record(logdir, run_id, result$metadata_capture))
  result$metadata_capture <- NULL   # transient -- not part of the returned result

  result
}
