# batch.R -- convert a WHOLE CASE in one go.
#
# A case is a folder of 10-50 statements, one bank or twenty. convert_batch()
# runs each file through the ordinary front door and hands back one row per file,
# so the analyst triages ONCE at the end instead of babysitting thirty
# conversions. Two rules shape it.
#
# PUSH THROUGH ON FAILURE. A file that cannot be read never stops the rest: it
# becomes a row carrying its own status and its own reason. The alternative is an
# analyst discovering at file 4 that files 5-30 never ran.
#
# NOTHING NEW HAPPENS PER FILE. Every file goes through convert_document() --
# same detection, same reconciliation, same outputs, same ONE run-log record as a
# single conversion. There is no batch pipeline, only a loop, so a batch answer
# and a single-file answer for the same statement cannot disagree.

# The statement-index tag an auto-split run puts on every KPI name
# ("balance_reconciliation [statement 2]"). It says WHERE in a bundle the check
# failed, not WHAT failed, so it is stripped here -- grouping is by kind of
# failure, and a bundle would otherwise scatter across as many groups as it has
# statements. The per-statement detail is still in the result and the workbook.
.STATEMENT_TAG <- "[[:space:]]*\\[statement [0-9]+\\]$"

# .failing_check(res) -- the SINGLE most useful thing that went wrong, as the
# engine's own CODE prefixed with the map that words it. Three tiers, most useful
# first; a converted-and-clean file gets NA, because nothing went wrong.
#
# The CODE, not the sentence: the words are UI copy (CHECK_PLAIN / DIAG_PLAIN /
# STATUS_PLAIN in ui_labels.R) that the screen already has loaded, and
# test-seams.R holds those maps to every code the engine can emit. Sorting --
# gathering every file that failed the same way so one fix clears them all -- is
# what this column is for, and codes sort just as well. The prefix says WHICH
# map, because a code appearing in two of them would render the wrong sentence.
.failing_check <- function(res) {
  if (identical(res$status, "ok")) return(NA_character_)

  # 1. A failing reconciliation check is the most useful answer there is: it names
  #    the thing that did not add up, and reconcile() lists checks in report
  #    order, so the first is the most important. CHECK_PLAIN words a check as
  #    what it PROVES ("Row dates could be read"), so the screen must say
  #    "Failed: <phrase>" -- this frame has no pass/fail column to read it beside.
  k <- res$kpis
  if (is.data.frame(k) && nrow(k)) {
    fail <- k$name[!is.na(k$status) & k$status == "fail"]
    if (length(fail)) return(paste0("check:", sub(.STATEMENT_TAG, "", fail[1])))
  }

  # 2. No check failed, so the problem is the file or the match itself. Ordered
  #    most-severe-first by build_diagnostics(); "info" rows are context, not a
  #    fault, and "none" is the explicit no-issues row.
  d <- res$diagnostics
  if (is.data.frame(d) && nrow(d)) {
    hit <- d$category[!is.na(d$severity) & d$severity != "info" & d$category != "none"]
    if (length(hit)) return(paste0("diag:", hit[1]))
  }

  # 3. Nothing specific to point at -- a template that won by a hair, say. Say the
  #    verdict rather than leave the cell empty and lose the file at the bottom.
  st <- as.character(res$status %||% NA_character_)[1]
  if (is.na(st) || !nzchar(st)) NA_character_ else paste0("status:", st)
}

# .rows_of(res) -- how much DATA came out. A statement is counted in transactions,
# read straight off the run-log record just written, so the number on screen and
# the number on disk cannot disagree. A FORM (an IRD summary, a KiwiSaver letter)
# has no transactions, so it is counted in the labelled values actually read --
# not the values its template declares, which would overstate an empty read.
.rows_of <- function(res) {
  v <- if (identical(res$kind, "form")) res$n_values %||% 0L else res$run_log$row_count %||% 0L
  v <- suppressWarnings(as.integer(v)[1])
  if (is.na(v)) 0L else v
}

# .failed_result(why) -- the result a file gets when even convert_document() could
# not produce one. It promises never to throw, so this should be unreachable; it
# exists because "should be unreachable" is not a guarantee, and one impossible
# error must not cost the other twenty-nine files their run. The reason is carried
# verbatim rather than replaced with a tidy phrase.
.failed_result <- function(why) {
  list(status = "failed", template_id = NA_character_, kind = "statement",
       messages = status_message("failed", why,
                                 "check the file is readable and matches a template"))
}

# convert_batch(paths, ..., progress) -> data.frame, one row per file.
#
#   file           the path exactly as it was given (so the analyst can find it)
#   status         ok | needs_review | unsupported | failed
#   bank           the bank the matched template names; NA when nothing matched,
#                  and for a form, whose result carries none -- see template_id
#   template_id    the template that was USED; NA unless the file converted
#   rows           transactions read (labelled values, for a form)
#   trust          high | medium | low -- as the run log records it; NA for a
#                  form, which has no reconciliation to have confidence in
#   failing_check  what went wrong, as the engine code the screen words (above)
#   message        the engine's own status message for this file
#   result         the full result object convert_document() returned
#
# ONE ROW PER PATH, in the order given: nothing is sorted, deduplicated or
# skipped. (Run ids and timestamps inside `result` vary per attempt by design; no
# column of this frame does.)
#
# `...` goes straight to convert_document(), so every argument it takes works here
# unchanged. It has no `...` of its own, so an argument THIS function does not
# take lands there and fails every file with "unused argument".
#
# `progress(i, n, file)` is called just BEFORE file i is converted -- a plain
# callback, not a Shiny call, so a folder can be run from the R console. One that
# errors is ignored: a broken progress bar must not cost a case its run.
#
# `result` is the WHOLE object, rows included, so a 50-file case holds fifty
# tables. Trimming is the caller's, because only the caller knows when it has
# finished with them (app.R writes the governed feed from the rows, then drops
# them and marks the result `dropped_feed_rows` -- that word order on purpose:
# `$` partially matches, so `feed_rows_dropped` would make res$feed_rows return
# the marker instead of NULL and a stated drop would read as data).
convert_batch <- function(paths, ..., progress = NULL) {
  paths <- as.character(paths %||% character(0))
  n <- length(paths)

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
    # The engine keeps the CLOSEST template on an unsupported result -- evidence
    # for whoever builds the missing one, but here a template id beside an
    # unsupported row reads as "this template was used". The run log blanks it for
    # that reason, so take the log's answer rather than re-derive the rule.
    out$template_id[i]   <- as.character(res$run_log$detected_template %||% NA_character_)[1]
    out$rows[i]          <- .rows_of(res)
    out$trust[i]         <- as.character(res$trust$level %||% NA_character_)[1]
    out$failing_check[i] <- .failing_check(res)
    msg <- paste(as.character(res$messages %||% character(0)), collapse = " | ")
    out$message[i]       <- if (nzchar(msg)) msg else NA_character_
    results[[i]]         <- res
  }

  out$result <- results
  rownames(out) <- NULL
  out
}

# The statuses convert_document() can return, worst-last -- the order a screen
# wants to read them in.
BATCH_STATUSES <- c("ok", "needs_review", "unsupported", "failed")

# batch_summary(batch) -> data.frame(status, n): the counts a screen prints when a
# case finishes. Every status is listed even at zero, so the shape never changes
# and "0 failed" is SAID rather than inferred from a missing row; anything
# unexpected is appended rather than dropped, and the counts always add up to the
# number of files -- a tally that quietly omits a file is the worse tally.
batch_summary <- function(batch) {
  s <- as.character(batch$status %||% character(0))
  s[is.na(s)] <- "?"
  lv <- c(BATCH_STATUSES, setdiff(unique(s), BATCH_STATUSES))
  data.frame(status = lv,
             n = vapply(lv, function(x) sum(s == x), integer(1), USE.NAMES = FALSE),
             stringsAsFactors = FALSE, row.names = NULL)
}
