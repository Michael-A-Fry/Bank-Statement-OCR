# batch.R -- convert a WHOLE CASE in one go.
#
# A case arrives as a folder of 10-50 statements. They may all be one bank, all
# different, or anything in between. convert_batch() runs each file through the
# ordinary front door and hands back one row per file, so the analyst triages
# ONCE at the end instead of babysitting thirty conversions.
#
# Two rules shape everything below.
#
# PUSH THROUGH ON FAILURE. A file that cannot be read never stops the rest: it
# becomes a row carrying its own status and its own reason. A 30-file case has to
# finish unattended, because the alternative is an analyst discovering at file 4
# that files 5-30 never ran.
#
# NOTHING NEW HAPPENS PER FILE. Every file goes through convert_document() --
# same detection, same reconciliation, same outputs, same ONE run-log record as a
# single conversion. There is no batch pipeline; there is a loop. That is the
# whole point: a batch answer and a single-file answer for the same statement can
# never disagree, because they are the same code.

# ---- plain words, taken from the one place they are written ------------------
#
# `failing_check` exists so a caller can SORT by it and fix every file that
# failed the same way together -- that sortability is the point of the column.
# It only works if the same failure produces the same STRING every time, so the
# wording comes from the shipped maps (CHECK_PLAIN / DIAG_PLAIN / STATUS_PLAIN in
# ui_labels.R) rather than a second copy here, which would drift away from what
# the screen says and quietly split one failure kind into two.
#
# ui_labels.R is UI copy and is deliberately NOT part of the R/ engine, so it is
# read from disk into a private environment instead of being assumed loaded. That
# keeps convert_batch() usable from a bare R console with no Shiny and no app.R.
# If the file cannot be found, the raw engine code is used instead: harder to
# read, still exact, still sortable -- never blank.
.plain_words <- function() {
  e <- new.env(parent = globalenv())          # ui_labels.R needs %||% from R/util.R
  # Same resolution order engine_version() uses: ENGINE_ROOT for the tests and the
  # scripts, "." for the app and RUN-ME.bat, which start in the repo root.
  cand <- unique(c(file.path(Sys.getenv("ENGINE_ROOT", "."), "ui_labels.R"), "ui_labels.R"))
  for (p in cand) {
    if (!file.exists(p)) next
    if (isTRUE(safe({ sys.source(p, envir = e); TRUE }, FALSE))) break
  }
  e
}

# .say(L, code, map) -- one engine code, in the words the screen uses. An unknown
# code, or no wording file at all, falls back to the code itself. Never blank: a
# blank cell in this column hides a file that needs attention.
.say <- function(L, code, map) {
  code <- as.character(code %||% NA_character_)[1]
  if (is.na(code) || !nzchar(code)) return(NA_character_)
  words <- L[[map]]
  v <- if (is.null(words)) NA_character_ else unname(words[code])
  if (is.na(v)) code else v
}

# The statement-index tag an auto-split run puts on every KPI name
# ("balance_reconciliation [statement 2]"). It says WHERE in a bundle the check
# failed, not WHAT failed, so it is stripped here -- grouping is by kind of
# failure, and a bundle would otherwise scatter across as many groups as it has
# statements. The per-statement detail is still in the result and the workbook.
.STATEMENT_TAG <- "[[:space:]]*\\[statement [0-9]+\\]$"

# .failing_check(res, L) -- the SINGLE most useful thing that went wrong, in plain
# words. Three tiers, most useful first; a converted-and-clean file gets NA,
# because nothing went wrong.
.failing_check <- function(res, L) {
  if (identical(res$status, "ok")) return(NA_character_)

  # 1. A failing reconciliation check is the most useful answer there is: it names
  #    the thing that did not add up, and it is the same failure on every file that
  #    has it. reconcile() lists checks in report order, so the first failure is
  #    the most important one.
  k <- res$kpis
  if (is.data.frame(k) && nrow(k)) {
    fail <- k$name[!is.na(k$status) & k$status == "fail"]
    # CHECK_PLAIN words each check as what it PROVES ("Row dates could be read"),
    # for use beside a separate pass/fail column. This frame has no such column,
    # so the bare phrase said the OPPOSITE of what happened. Prefixing is enough
    # and costs no second wording map to keep in step: the string is still
    # identical for every file that failed the same way, which is what makes the
    # column sortable, and that sortability is the whole point of it.
    if (length(fail))
      return(paste0("Failed: ", .say(L, sub(.STATEMENT_TAG, "", fail[1]), "CHECK_PLAIN")))
  }

  # 2. No check failed, so the problem is the file or the match itself -- it could
  #    not be read, no template fits, two templates fit, a scan had no text.
  #    build_diagnostics() already sorts most-severe-first; "info" rows are
  #    context, not a fault, and "none" is the explicit no-issues row.
  d <- res$diagnostics
  if (is.data.frame(d) && nrow(d)) {
    hit <- d$category[!is.na(d$severity) & d$severity != "info" & d$category != "none"]
    if (length(hit)) return(.say(L, hit[1], "DIAG_PLAIN"))
  }

  # 3. Held for review with nothing specific to point at -- a template that won by
  #    a hair, say. Say the verdict rather than leave the cell empty, so sorting
  #    still gathers these files together instead of losing them at the bottom.
  .say(L, res$status, "STATUS_PLAIN")
}

# .rows_of(res) -- how much DATA came out of this file.
#
# A statement is counted in transactions, read straight off the run-log record the
# engine just wrote, so the number on screen and the number on disk cannot
# disagree. A FORM (an IRD summary, a KiwiSaver letter) has no transactions, so it
# is counted in the labelled values actually read -- not the values its template
# declares, which would overstate a document where nothing was found.
.rows_of <- function(res) {
  v <- if (identical(res$kind, "form")) res$n_values %||% 0L else res$run_log$row_count %||% 0L
  v <- suppressWarnings(as.integer(v)[1])
  if (is.na(v)) 0L else v
}

# .trim_result(res, keep_rows) -- what goes in the `result` column.
#
# BOUNDED MEMORY. A 50-file case would otherwise hold fifty complete transaction
# tables in one object, for a frame whose job is triage. Nothing is lost by
# dropping them: the conversion has ALREADY written every row to the workbook /
# CSV / JSON named in result$outputs, so the in-memory copy is a copy.
# keep_rows = TRUE keeps them, for a caller that genuinely wants one object.
#
# The drop is STATED, never silent. A caller reading result$feed_rows must be able
# to tell "dropped to save memory" from "this statement had no transactions" --
# the two look identical otherwise, and guessing between them is exactly the kind
# of quiet wrongness this tool exists to prevent.
#
# The marker is named `dropped_feed_rows` and NOT `feed_rows_dropped` on purpose.
# R's `$` partially matches list names, so with the second name a caller asking
# for res$feed_rows would silently get the marker (TRUE) instead of NULL -- a
# reader would see a value where the data used to be. The prefix must not collide.
.trim_result <- function(res, keep_rows) {
  if (isTRUE(keep_rows) || is.null(res$feed_rows)) return(res)
  res$feed_rows <- NULL
  res$dropped_feed_rows <- TRUE
  res
}

# .failed_result(why) -- the result a file gets when even convert_document() could
# not produce one. convert_document() promises never to throw, so this should be
# unreachable; it exists because "should be unreachable" is not a guarantee, and
# one impossible error must not cost the other twenty-nine files their run. The
# reason is carried through verbatim rather than replaced with a tidy phrase.
.failed_result <- function(why) {
  list(status = "failed", template_id = NA_character_, kind = "statement",
       messages = status_message("failed", why,
                                 "check the file is readable and matches a template"))
}

# convert_batch(paths, ..., progress, keep_rows) -> data.frame, one row per file.
#
#   file           the path exactly as it was given (so the analyst can find it)
#   status         ok | needs_review | unsupported | failed
#   bank           the bank the matched template names; NA when nothing matched,
#                  and for a form, whose result carries none -- see template_id
#   template_id    the template that was USED; NA unless the file converted
#   rows           transactions read (labelled values, for a form)
#   trust          high | medium | low -- the same value the run log records; NA
#                  for a form, which has no reconciliation to have confidence in
#   failing_check  the single most useful thing that went wrong, in plain words
#   message        the engine's own status message for this file
#   result         the full result object convert_document() returned
#
# `...` is passed straight through to convert_document(), so every argument it
# takes (outdir, templates_dir, formats, logdir, bank, force_template, ...) works
# here unchanged and cannot drift as that function grows.
#
# `progress` is an optional function called as progress(i, n, file) just BEFORE
# file i is converted -- a plain callback rather than a Shiny call, so a
# maintainer can run a folder from the R console with no Shiny loaded. A callback
# that errors is ignored: a broken progress bar must not cost a case its run.
#
# ONE ROW PER PATH, in the order given: nothing is sorted, deduplicated or
# skipped, so the same inputs always give the same rows in the same order. (The
# run ids and timestamps inside `result` vary per attempt by design; no column of
# this frame does.)
convert_batch <- function(paths, ..., progress = NULL, keep_rows = FALSE) {
  paths <- as.character(paths %||% character(0))
  n <- length(paths)
  L <- .plain_words()                   # read once per batch, not once per file

  out <- data.frame(
    file          = paths,
    status        = rep(NA_character_, n),
    bank          = rep(NA_character_, n),
    template_id   = rep(NA_character_, n),
    rows          = rep(NA_integer_,   n),
    trust         = rep(NA_character_, n),
    failing_check = rep(NA_character_, n),
    message       = rep(NA_character_, n),
    stringsAsFactors = FALSE)
  results <- vector("list", n)

  for (i in seq_len(n)) {
    if (is.function(progress)) safe(progress(i, n, paths[i]))
    res <- tryCatch(convert_document(paths[i], ...),
                    error = function(e) .failed_result(conditionMessage(e)))

    out$status[i] <- as.character(res$status %||% "failed")[1]
    out$bank[i]   <- as.character(res$header$bank %||% NA_character_)[1]
    # The engine keeps the CLOSEST template on an unsupported result -- useful
    # evidence for whoever builds the missing one, but on this frame a template id
    # beside an unsupported row reads as "this template was used". The run log
    # blanks it for exactly that reason, so take the log's answer rather than
    # re-deriving the rule and risking the two saying different things.
    out$template_id[i]   <- as.character(res$run_log$detected_template %||% NA_character_)[1]
    out$rows[i]          <- .rows_of(res)
    out$trust[i]         <- as.character(res$trust$level %||% NA_character_)[1]
    out$failing_check[i] <- .failing_check(res, L)
    msg <- paste(as.character(res$messages %||% character(0)), collapse = " | ")
    out$message[i]       <- if (nzchar(msg)) msg else NA_character_
    results[[i]]         <- .trim_result(res, keep_rows)
  }

  out$result <- results
  rownames(out) <- NULL
  out
}

# The statuses convert_document() can return, worst-last -- the order a screen
# wants to read them in.
BATCH_STATUSES <- c("ok", "needs_review", "unsupported", "failed")

# batch_summary(batch) -> data.frame(status, n): the counts a screen prints when a
# case finishes.
#
# Every status is listed even when its count is zero, so the summary has the same
# shape every run and "0 failed" is SAID rather than inferred from a missing row.
# Anything unexpected is appended rather than dropped, and the counts always add
# up to the number of files -- a tally that quietly omits a file is worse than one
# with an unfamiliar word in it.
batch_summary <- function(batch) {
  s <- as.character(batch$status %||% character(0))
  s[is.na(s)] <- "?"
  lv <- c(BATCH_STATUSES, setdiff(unique(s), BATCH_STATUSES))
  data.frame(status = lv,
             n = vapply(lv, function(x) sum(s == x), integer(1), USE.NAMES = FALSE),
             stringsAsFactors = FALSE, row.names = NULL)
}
