# review.R -- what Admin -> Review shows, and where its pages come from.
#
# "How do I go into admin, see what's been run and not worked and actually SEE it
# to fix it, accept it or reject it." Three lists, each opening a statement page:
#
#   Needs a look  the conversions that did not come out proven (Please check,
#                 nothing read, the file failed), newest first, from the run log.
#   Layouts       each learned layout, shown on a statement it read.
#   Held fixes    a person's fix the arithmetic could not prove (R/fixes.R), shown
#                 on the file it was held from, read the way the person set it.
#
# NOTHING NEW IS STORED. A page can only come from a file this server already
# keeps: an upload (R/uploads.R, deleted after the retention period) or an
# original the folder intake left in failed/ or processed/. The run log says which
# file a conversion read (its name and content hash), an upload record says which
# layout read it, and the run log names any fix the run held for an admin. When
# none of these has the file, the screen says so plainly.
#
# review_read() reads a kept file again with the engine's own reader, WITHOUT
# learning, logging, tracking or writing anything, so that its columns can be drawn
# the way Please check draws them. It runs in its own process (task "review",
# R/jobs.R), never in the app's.
#
# Every function returns a result; none throws.

# The newest runs Review reads off the run log. It wants the recent ones, and the
# app's one R process is shared by the whole team (see .adm_history in app.R).
REVIEW_RUNS_MAX <- 1000L

.rv_chr <- function(df, nm) {
  if (!is.data.frame(df) || !(nm %in% names(df))) return(rep(NA_character_, NROW(df)))
  as.character(df[[nm]])
}

# review_reason(reason, message) -- the run's reason as one plain sentence: the
# reader's own reason when it gave one, else the run's message without its status
# code ("failed: ...") and without the advice after it.
review_reason <- function(reason, message = NA_character_) {
  r <- trimws(as.character(reason))
  m <- as.character(message)
  m <- sub("^[a-z_]+:\\s*", "", sub("\\s*\\|.*$", "", m))
  m <- sub(";.*$", "", m)
  out <- ifelse(!is.na(r) & nzchar(r), r, trimws(m))
  out[is.na(out) | !nzchar(out)] <- "No reason was recorded."
  out <- sub("([^.!?])$", "\\1.", out)
  paste0(toupper(substr(out, 1L, 1L)), substring(out, 2L))
}

# review_needs_look(runs, limit) -> one row per FILE whose newest conversion did
# not come out proven or matched to a layout, newest first. The newest run of a
# file decides, so a file that was set right on Please check, or confirmed there,
# leaves the list. A file is its content hash (the same statement under two names
# is one file); a run with no hash is its own file.
review_needs_look <- function(runs, limit = 200L) {
  cols <- c("run_id", "ts", "file", "sha", "bank", "bank_hint", "status", "outcome", "reason")
  empty <- stats::setNames(data.frame(matrix(character(0), 0, length(cols)), stringsAsFactors = FALSE), cols)
  if (!is.data.frame(runs) || !nrow(runs)) return(empty)
  kind <- .rv_chr(runs, "kind")
  d <- data.frame(run_id = .rv_chr(runs, "run_id"), ts = .rv_chr(runs, "ts"),
                  file = basename(.rv_chr(runs, "source_file")), sha = .rv_chr(runs, "source_sha256"),
                  bank = .rv_chr(runs, "institution"), bank_hint = .rv_chr(runs, "bank_hint"),
                  status = .rv_chr(runs, "status"), outcome = .rv_chr(runs, "outcome"),
                  reason = review_reason(.rv_chr(runs, "reason"), .rv_chr(runs, "message")),
                  stringsAsFactors = FALSE)
  d <- d[is.na(kind) | kind == "statement", , drop = FALSE]
  if (!nrow(d)) return(empty)
  when <- .parse_stamp(d$ts)
  d <- d[order(-as.numeric(when), na.last = TRUE), , drop = FALSE]
  key <- ifelse(!is.na(d$sha) & nzchar(d$sha), d$sha, paste0("run:", d$run_id))
  d <- d[!duplicated(key), , drop = FALSE]
  d <- d[d$status %in% c("needs_review", "unsupported", "failed"), , drop = FALSE]
  rownames(d) <- NULL
  utils::head(d, limit)
}

# .rv_gone(keep_days) -- why a kept copy is no longer there.
.rv_gone <- function(keep_days) {
  k <- suppressWarnings(as.numeric(keep_days)[1])
  if (is.na(k) || k <= 0) "Its kept copy has been deleted."
  else sprintf("Kept files are deleted after %d days.", as.integer(k))
}

# review_kept(sha, name, run_id, uploads, uploads_dir, intake_root, keep_days) ->
# list(path, upload_id, status, why): where the file a run read is still kept.
#   1. an upload of the same bytes (or the upload that run was recorded under);
#   2. the folder intake's original of that name in failed/ or processed/, when it
#      is the same bytes.
# `why` says, in a sentence, why there is no path.
review_kept <- function(sha, name = NA_character_, run_id = NA_character_, uploads = NULL,
                        uploads_dir = NULL, intake_root = NULL, keep_days = NA) {
  none <- function(why) list(path = NA_character_, upload_id = NA_character_, status = NA_character_, why = why)
  tryCatch({
    sha <- as.character(sha %||% NA)[1]; name <- as.character(name %||% NA)[1]
    run_id <- as.character(run_id %||% NA)[1]
    gone <- FALSE
    if (is.data.frame(uploads) && nrow(uploads)) {
      hit <- (!is.na(sha) & .rv_chr(uploads, "sha256") %in% sha) |
             (!is.na(run_id) & .rv_chr(uploads, "run_id") %in% run_id)
      m <- uploads[hit, , drop = FALSE]
      for (i in seq_len(nrow(m))) {
        if (isTRUE(as.logical(m$purged[i]))) { gone <- TRUE; next }
        p <- upload_file_path(m$id[i], uploads_dir)
        if (!is.na(p) && file.exists(p))
          return(list(path = p, upload_id = m$id[i], status = as.character(m$status[i]), why = NA_character_))
        gone <- TRUE
      }
    }
    if (!is.null(intake_root) && !is.na(name) && nzchar(name)) {
      nm <- basename(gsub("\\\\", "/", name))
      for (sub in c("failed", "processed")) {
        p <- file.path(intake_root, sub, nm)
        if (nzchar(nm) && !(nm %in% c(".", "..")) && file.exists(p) &&
            (is.na(sha) || identical(safe(file_sha256(p), NA_character_), sha)))
          return(list(path = p, upload_id = NA_character_, status = NA_character_, why = NA_character_))
      }
    }
    none(if (gone) paste("This file is no longer kept.", .rv_gone(keep_days))
         else "This file was not kept. Only files converted on Convert are kept.")
  }, error = function(e) none("This file could not be found."))
}

# review_layout_example(id, uploads, keep_days) -> list(upload_id, why): the
# newest kept upload a layout read. The layout store holds no statement -- a
# layout is roles, formats and heading words, never a file -- so the example is
# the newest upload whose record names the layout (R/uploads.R `template`: the
# layout a conversion matched or learned).
review_layout_example <- function(id, uploads, uploads_dir = NULL, keep_days = NA) {
  id <- sub("@v?[0-9]+$", "", as.character(id %||% NA)[1])
  m <- if (is.data.frame(uploads) && nrow(uploads))
    uploads[!is.na(id) & sub("@v?[0-9]+$", "", .rv_chr(uploads, "template")) %in% id, , drop = FALSE]
  else NULL
  for (i in seq_len(NROW(m))) {
    if (isTRUE(as.logical(m$purged[i]))) next
    p <- upload_file_path(m$id[i], uploads_dir)
    if (!is.na(p) && file.exists(p))
      return(list(upload_id = m$id[i], path = p, why = NA_character_))
  }
  list(upload_id = NA_character_, path = NA_character_,
       why = if (NROW(m)) paste("The files it read are no longer kept.", .rv_gone(keep_days))
             else "It was learned from files that were not kept. Training keeps no files.")
}

# review_fix_run(fix, runs) -> the row of `runs` whose conversion held this fix, or
# NA. The run log names the fixes a run held (fix_held). A fix held before it did is
# found by its bank and its time: the run that confirmed or set the roles for that
# bank, finishing within two minutes after the fix was held.
review_fix_run <- function(fix, runs) {
  if (!is.data.frame(runs) || !nrow(runs) || is.null(fix$id)) return(NA_integer_)
  held <- .rv_chr(runs, "fix_held")
  hit <- which(vapply(strsplit(ifelse(is.na(held), "", held), ",", fixed = TRUE),
                      function(x) fix$id %in% trimws(x), logical(1)))
  if (length(hit)) return(hit[1])
  slug <- .layout_slug(fix$bank)
  t0 <- .parse_stamp(fix$held)
  if (is.na(slug) || is.na(t0)) return(NA_integer_)
  dt <- as.numeric(difftime(.parse_stamp(.rv_chr(runs, "ts")), t0, units = "secs"))
  person <- .rv_chr(runs, "person_fix") %in% "roles" | .rv_chr(runs, "proof_kind") %in% "person"
  ok <- which(.rv_chr(runs, "institution") %in% slug & person & !is.na(dt) & dt >= -5 & dt <= 120)
  if (!length(ok)) return(NA_integer_)
  ok[which.min(abs(dt[ok]))]
}

# review_fix_record(id, dir) -> the held fix as kept (id, bank, kind, by, held,
# template), or NULL.
review_fix_record <- function(id, dir = layouts_dir()) {
  p <- .fix_path(id, dir)
  if (is.null(p)) return(NULL)
  r <- tryCatch(yaml::read_yaml(p), error = function(e) NULL)
  if (!is.list(r) || !identical(as.character(r$id %||% "")[1], id)) return(NULL)
  r
}

# review_read(path, bank, layouts, roles) -> list(ok, kind, pages, units, why).
# The file read again for the picture, statement by statement as a conversion
# reads it (a bundle is split the way convert_statement() splits it, so the pages
# are the file's pages). `layouts`: the layouts to read it against; `roles`: a
# person's roles for the figure columns, read on their own as Please check reads
# them (no layout stands in for them). Each unit keeps only what the picture
# needs -- its pages, its columns, its outcome and reason -- never a transaction.
review_read <- function(path, bank = NULL, layouts = list(), roles = NULL) {
  tryCatch({
    input <- read_input(path)
    why <- .unreadable_reason(input)
    np <- length(input$pages %||% input$words %||% 1L)
    if (!is.null(why)) return(list(ok = FALSE, kind = input$kind, pages = np, units = list(), why = why))
    kind <- if (identical(input$kind, "pdf") && isTRUE(any(input$page_ocr))) "scan" else input$kind
    meta <- extract_metadata(input)
    segs <- bundle_segments(input, meta)
    units <- if (is.null(segs)) list(list(input = input, pages = seq_len(np)))
             else lapply(segs, function(pg) list(input = .subinput_pages(input, pg), pages = pg))
    roles <- as.character(unlist(roles))
    out <- lapply(units, function(u) {
      rd <- if (length(roles)) auto_read(u$input, list(), bank, list(roles = roles))
            else auto_read(u$input, layouts %||% list(), bank)
      cl <- rd$columns
      if (is.data.frame(cl) && nrow(cl) && "page" %in% names(cl)) cl$page <- u$pages[cl$page]
      list(outcome = as.character(rd$outcome %||% "unread")[1], why = as.character(rd$why %||% "")[1],
           pages = u$pages, columns = cl, matched_layout = rd$matched_layout %||% NA_character_)
    })
    list(ok = TRUE, kind = kind, pages = np, units = out, why = NA_character_)
  }, error = function(e) list(ok = FALSE, kind = NA_character_, pages = NA_integer_, units = list(),
                              why = paste0("It could not be read again (", conditionMessage(e), ").")))
}

# review_columns(rd) -> every unit's columns as one frame (they are on the file's
# own pages, so they never overlap), or NULL when nothing was found.
review_columns <- function(rd) {
  cl <- Filter(function(x) is.data.frame(x) && nrow(x), lapply(rd$units %||% list(), `[[`, "columns"))
  if (!length(cl)) return(NULL)
  do.call(rbind, cl)
}

# review_first_page(rd, unproven_first) -> the page to open on: the first page of
# the first statement that did not prove (when asked), else the first page with a
# column found on it, else page 1.
review_first_page <- function(rd, unproven_first = FALSE) {
  us <- rd$units %||% list()
  if (unproven_first) for (u in us)
    if (!(u$outcome %in% c("proven", "layout_match")) && length(u$pages)) {
      pc <- if (is.data.frame(u$columns) && nrow(u$columns)) sort(unique(u$columns$page)) else integer(0)
      return(as.integer(if (length(pc)) pc[1] else u$pages[1]))
    }
  cl <- review_columns(rd)
  if (!is.null(cl)) return(as.integer(min(cl$page)))
  1L
}
