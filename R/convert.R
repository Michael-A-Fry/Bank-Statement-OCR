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
  # ONE record has nothing to repeat, so the shape cannot decide it -- and the
  # two candidates are indistinguishable byte for byte: a header-only export, or
  # a sentence with a comma in it. Fall back to the narrowest a statement table
  # can be, which is the closest thing to a fact available here. Counted over
  # records that carry CONTENT, so a trailing newline cannot cost a header-only
  # export its one fallback.
  sum(n > 0L) == 1L && max(n) >= .MIN_TABLE_FIELDS
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
  ck <- data.frame(check = c("rows_read", "amounts_read", "no_derived_amounts", "dates_readable",
                             "balance_chain", "opening_closing", "dates_in_period"),
    ok = c(n > 0L, n > 0L && !anyNA(tx$amount), nd == 0L,
           n > 0L && !anyNA(suppressWarnings(as.Date(tx$date))),
           n > 0L && !anyNA(tx$balance) && kst("running_balance_continuity") == "pass",
           kst("balance_reconciliation") == "pass", kst("dates_within_period") != "fail"),
    stringsAsFactors = FALSE)
  ck$why <- ifelse(ck$ok, "Holds with the edited columns.", c(
    "The edited columns read no rows.", "Some amounts could not be read in the edited columns.",
    "Some amounts were filled in from the balance.", "Some dates could not be read in the edited columns.",
    "The running balance is not printed on every row, or does not add up, with the edited columns.",
    "Opening balance plus the movements does not reach the printed closing balance.",
    "Some dates fall outside the statement period.")[seq_len(nrow(ck))])
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
                                         ifelse(b$field %in% c("debit", "credit", "amount", "balance") | startsWith(b$field, "other"), "money", "text")),
                                  x_min = b$x_min, x_max = b$x_max, ink_min = b$x_min, ink_max = b$x_max,
                                  heading = "", stringsAsFactors = FALSE),
             matched_layout = NULL, notes = character(0), engine = AUTO_READ_VERSION)
  list(reading = rd, changes = changes)
}

# .read_statement(input, layouts, bank, overrides) -> list(reading, fix). One
# statement, read from its content against the bank's layouts; then, when the
# person sent a fix, read again with it. `fix` is NULL without one, else
# list(kind, proven, changes) or list(kind, error).
.read_statement <- function(input, layouts, bank, overrides) {
  base <- auto_read(input, layouts, bank)
  has_roles <- length(overrides$roles) > 0L
  has_boxes <- NROW(overrides$columns) > 0L
  if (!has_roles && !has_boxes) return(list(reading = base, fix = NULL))
  rd <- base; changes <- list(); kind <- NA_character_
  if (has_roles) {
    o <- .override_roles(base, overrides$roles)
    if (!is.null(o$error)) return(list(reading = base, fix = list(kind = "roles", error = o$error)))
    rd <- auto_read(input, list(), bank, list(roles = o$roles))
    changes <- o$changes; kind <- "roles"
  }
  if (has_boxes) {
    bx <- tryCatch(.override_boxes(input, rd, overrides$columns),
                   error = function(e) list(error = paste0("The edited columns could not be read (", conditionMessage(e), ").")))
    if (!is.null(bx$error)) return(list(reading = rd, fix = list(kind = "boxes", error = bx$error)))
    rd <- bx$reading; changes <- c(changes, bx$changes); kind <- "boxes"
  }
  list(reading = rd, fix = list(kind = kind, proven = identical(rd$outcome, "proven"), changes = changes))
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

.AUTO <- c("proven", "layout_match")

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

# .unit_sha(sha, i, k) -- the fingerprint one statement is counted by as evidence
# for a layout: the file's own, or for statement i of a bundle a fingerprint of
# its own, so each statement of a bundle is one statement of evidence.
.unit_sha <- function(sha, i, k) {
  if (is.na(sha) || k <= 1L) return(sha)
  .text_sha256(paste0(sha, "#statement", i))
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
  # The stamp: what produced this answer, for the run log, the JSON and the feed.
  stamp <- list(engine_version = engine_version(), reader_version = AUTO_READ_VERSION,
                layouts_state = "unknown", layout = NA_character_, outcome = "unread",
                proof_kind = "none", institution = NA_character_, bank_code = NA_character_,
                bank_confidence = NA_character_, kind = NA_character_)
  facts <- list(rows = 0L, learn = character(0), statements = 1L, multi = FALSE, fix = NA_character_,
                pages = NA_integer_, period_start = NA_character_, period_end = NA_character_, n_accounts = NA_integer_,
                layout_sig = NA_character_, layout_hint = NA_character_, reason = NA_character_)

  outcome <- tryCatch({
    base <- tools::file_path_sans_ext(basename(path %||% "input"))
    input <- read_input(path)
    # THE FILE ITSELF COULD NOT BE READ: the sender's problem, not a reading to
    # check. Raised through the funnel, which makes it a `failed` result.
    if (!is.null(why <- .unreadable_reason(input))) stop(why, call. = FALSE)
    stamp$kind <- if (identical(input$kind, "pdf") && isTRUE(any(input$page_ocr))) "scan" else input$kind
    meta <- extract_metadata(input)
    multi <- detect_multiple_statements(input, meta)
    lsig <- safe(layout_signature(input), list(signature = NA_character_, hint = NA_character_))
    facts$layout_sig <- lsig$signature %||% NA_character_
    facts$layout_hint <- lsig$hint %||% NA_character_

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
    stamp$layouts_state <- if (is.null(ldir)) "unknown" else layouts_state_id(ldir)
    learn_ok <- !is.na(bank_slug) && !is.null(ldir) && !isTRUE(pick$block_learning)

    # ---- read: the whole file, or each statement of a bundle on its own ----
    segs <- bundle_segments(input, meta)
    units <- if (is.null(segs)) list(list(input = input, pages = seq_along(input$pages %||% input$words %||% 1L)))
             else lapply(segs, function(pg) list(input = .subinput_pages(input, pg), pages = pg))
    reads <- lapply(units, function(u) .read_statement(u$input, layouts, bank_name, overrides))
    readings <- lapply(reads, `[[`, "reading")
    k <- length(readings)
    facts$statements <- k
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

    # ---- the status ----
    ocr_pages <- suppressWarnings(as.integer(input$meta$ocr_pages %||% 0L)); if (is.na(ocr_pages)) ocr_pages <- 0L
    ocr_conf <- suppressWarnings(as.numeric(input$meta$ocr_min_conf %||% NA_real_))
    # A badly read scan cannot come back "ok": the arithmetic proves the figures,
    # but not a date digit OCR was unsure of. A page it could not measure at all
    # is no evidence of a good read either.
    ocr_poor <- ocr_pages > 0L && (!is.finite(ocr_conf) || ocr_conf < PARAM_OCR_PAGE_MIN_CONF)
    derived <- if (has_rows) sum(grepl("amount_from_balance", parsed$transactions$flags, fixed = TRUE)) else 0L
    box_proven <- identical(facts$fix, "boxes") && all(vapply(fixes, function(f) isTRUE(f$proven), logical(1)))
    status <- if (!has_rows || all(outcomes == "unread")) "unsupported"
              else if (all(outcomes %in% .AUTO) && !ocr_poor) "ok"
              else "needs_review"
    if (identical(status, "needs_review") && all(outcomes %in% .AUTO) && ocr_poor)
      reason <- sprintf("The figures add up, but the scan was read with low confidence (%s), so a date or a word may be misread.",
                        if (is.finite(ocr_conf)) sprintf("%.0f%%", ocr_conf) else "not measured")
    confirmed <- isTRUE(confirm) && identical(status, "needs_review") && !("unread" %in% outcomes)
    basis <- if (identical(status, "ok")) { if (box_proven) "person" else if (all(outcomes == "layout_match")) "layout_match" else "proven" }
             else if (confirmed) "person" else "none"
    if (confirmed) { status <- "ok"; stamp$proof_kind <- "person" }

    # ---- learning: only what the arithmetic proved (spec section 6) ----
    learn <- vector("list", k)
    held <- NULL
    for (i in seq_len(k)) {
      r <- readings[[i]]; f <- fixes[[i]]
      learn[[i]] <- if (!learn_ok) list(action = "none", why = if (is.na(bank_slug)) "No bank was given, so nothing is learned."
                                         else if (is.null(ldir)) "No layout store is set up, so nothing is learned." else pick$why)
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
        else if (identical(r$outcome, "proven") && is.null(f))
          layout_learn(r, pick, .unit_sha(sha, i, k), ldir)
        else list(action = "none", why = "Only a reading the arithmetic proved teaches a layout.")
      # One person's word the arithmetic could not back waits for an admin.
      if (learn_ok && is.null(f$error) && !identical(f$kind, "boxes") && !(r$outcome %in% .AUTO) &&
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
      parsed$header$bank <- bank_name
      parsed$header$institution <- bank_slug
    }
    facts$rows <- if (has_rows && status %in% c("ok", "needs_review")) nrow(parsed$transactions) else 0L
    pick_mismatch <- !is.na(ident$institution %||% NA) && !is.na(bank_slug) && !identical(ident$institution, bank_slug)
    diag <- build_diagnostics(status, parsed = parsed, recon = recon,
      reading = list(outcome = worst, why = reason, derived = derived, fix_error = fix_err,
                     bank_why = if (isTRUE(pick$ask) || pick_mismatch) pick$why else NULL,
                     bank_blocked = isTRUE(pick$block_learning) && !is.na(bank_slug)),
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
      status_message("needs_review", reason, "check the reading, then confirm it or set the columns' roles")
    else status_message("unsupported", reason, "check the columns on Please check, or set the file aside")
    if (derived > 0L)
      msg <- c(msg, sprintf("%d amount(s) could not be read and were filled in from the running balance; they are marked amount_from_balance in the flags column.", derived))
    if (length(fix_err)) msg <- c(sprintf("The fix was not applied: %s", fix_err[1]), msg)
    if (isTRUE(pick$ask) || pick_mismatch) msg <- c(msg, pick$why)

    result$status <- status
    result$template_id <- stamp$layout %||% NA_character_
    result$outcome <- worst
    result$reason <- reason
    result$feed_basis <- basis
    result$bank <- list(bank = bank_id, display = bank_name, institution = ident$institution %||% NA_character_,
                        identified_display = ident$display %||% NA_character_, bank_code = stamp$bank_code,
                        confidence = stamp$bank_confidence, why = pick$why, ask = isTRUE(pick$ask),
                        block_learning = isTRUE(pick$block_learning))
    result$reading <- lapply(seq_len(k), function(i) {
      r <- readings[[i]]
      list(outcome = r$outcome, why = r$why, pages = units[[i]]$pages, proof = r$proof, checks = r$checks,
           candidates = r$candidates, columns = r$columns, matched_layout = r$matched_layout,
           learned_layout = r$learned_layout, roles = r$template$auto$roles, template = r$template,
           transactions = r$transactions, notes = r$notes, fix = fixes[[i]], learn = learn[[i]])
    })
    result$columns <- readings[[1]]$columns
    result$learn <- learn
    result$fix_held <- held
    result$person <- list(confirmed = confirmed, by = if (confirmed || length(fix_kind)) who else NA_character_,
                          fix = facts$fix, fix_proven = if (length(fix_kind)) all(vapply(fixes, function(f) isTRUE(f$proven), logical(1))) else NA)
    result$derived <- derived
    result$spot_check <- basis %in% c("proven", "layout_match") &&
      .spot_pick(sha, cfg$auto_reading$spot_check_rate %||% 0)
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
      if (length(f$changes)) { tf$event <- "correction"; tf$correction <- f$changes }
      if (confirmed) { tf$event <- "confirm"; tf$proof_kind <- "person" }
      suppressWarnings(track_record(tf, tdir))
    }

    # LOCAL-ONLY metadata capture, built where every artifact is in scope and
    # written after the run log below. NEVER enters the feed.
    result$metadata_capture <- safe(capture_metadata(list(
      run_id = run_id, ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      requested_by = who, sha = sha, input = input, parsed = parsed, recon = recon,
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
    reason = facts$reason,
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
    message = paste(result$messages, collapse = " | ")
  )
  if (isTRUE(log)) log_run(logdir, result)

  # ---- metadata capture: LOCAL ONLY, kept forever (logs/metadata/), never fed ----
  safe(write_metadata_record(logdir, run_id, result$metadata_capture))
  result$metadata_capture <- NULL   # transient -- not part of the returned result

  result
}
