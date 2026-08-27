# batch.R -- convert a WHOLE CASE in one go. convert_batch(paths, ...) runs each file through the
# ordinary front door and hands back one row per file. PUSH THROUGH ON FAILURE: a file that cannot be
# read becomes a row carrying its own status and reason. There is no batch pipeline, only a loop, so a
# batch answer and a single-file answer cannot disagree.

# The statement-index tag an auto-split run puts on a KPI name says WHERE in a bundle the check failed,
# not WHAT failed, so it is stripped - grouping is by kind of failure.
.STATEMENT_TAG <- "[[:space:]]*\\[statement [0-9]+\\]$"

# The single most useful thing that went wrong, as the engine's own CODE prefixed with the map that
# words it - sorting files that failed the same way is what this column is for, and the prefix stops a
# code that appears in two maps rendering the wrong sentence.
.failing_check <- function(res) {
  if (identical(res$status, "ok")) return(NA_character_)

  # 0. A form or a report has neither frame below, so every one fell through to tier 3 and printed its
  # own status back at itself.
  k <- as.character(res$kind %||% "statement")[1]
  if (k %in% c("form", "tables")) {
    plain <- .other_route_check(res, k)
    if (!is.na(plain)) return(plain)
  }

  # 1. A failing check names the thing that did not add up, and reconcile() lists checks in report order.
  k <- res$kpis
  if (is.data.frame(k) && nrow(k)) {
    fail <- k$name[!is.na(k$status) & k$status == "fail"]
    if (length(fail)) return(paste0("check:", sub(.STATEMENT_TAG, "", fail[1])))
  }

  # 2. No check failed, so the problem is the file or the match. Most-severe-first; "info" is context.
  d <- res$diagnostics
  if (is.data.frame(d) && nrow(d)) {
    hit <- d$category[!is.na(d$severity) & d$severity != "info" & d$category != "none"]
    if (length(hit)) return(paste0("diag:", hit[1]))
  }

  # 3. Nothing specific to point at. Say the verdict rather than leave the cell empty.
  st <- as.character(res$status %||% NA_character_)[1]
  if (is.na(st) || !nzchar(st)) NA_character_ else paste0("status:", st)
}

# What to look at on a form or a report, in plain words, or NA. Written in the SAME order convert_form()
# and convert_tables() weigh their own signals, so this column and the message beside it cannot point at
# different faults. A phrase, not a code: there is no map for these two routes.
.other_route_check <- function(res, kind) {
  n <- function(x) length(as.character(x %||% character(0)))
  # A count that was never recorded is not a count of nothing, and `if (NA > 0)` is an error: every
  # number here reads as 0 unless it is really a number.
  cnt <- function(x) {
    v <- suppressWarnings(as.integer(x %||% 0L)[1]); if (is.na(v)) 0L else v
  }
  if (isTRUE(res$ambiguous))
    return("more than one template fits this document")
  if (identical(kind, "form")) {
    if (cnt(res$n_conflicts) > 0)
      return(sprintf("%d label(s) appear more than once with different values",
                     cnt(res$n_conflicts)))
    if (cnt(res$required_missing) > 0)
      return(sprintf("%d required value(s) were not found",
                     cnt(res$required_missing)))
    return(NA_character_)
  }
  if (n(res$parse_fail_tables))
    return("a column of figures came out holding no figures at all")
  # Words inside a table that no column claimed: the sharpest sign the bands no longer fit, and the one
  # signal a report has that a full-looking column of wrong values would otherwise lack.
  if (cnt(res$unclaimed_words) > 0)
    return(sprintf("%d word(s) inside a table were not in any column",
                   cnt(res$unclaimed_words)))
  if (n(res$empty_tables))
    return(sprintf("%d table(s) came out empty", n(res$empty_tables)))
  if (n(res$weak_tables))
    return(sprintf("%d table(s) were found by position rather than by their heading",
                   n(res$weak_tables)))
  if (n(res$thin_tables))
    return(sprintf("%d table(s) have a column that came out mostly empty",
                   n(res$thin_tables)))
  NA_character_
}

# How much data came out, and OF WHAT: statement = transactions, form = the labelled VALUES actually
# read, report = the rows of its own TABLES. One bare number made a report that read 83 rows look
# identical to one that read nothing, so the LABEL travels beside it.
.how_much <- function(res) {
  k <- as.character(res$kind %||% "statement")[1]
  v <- switch(k,
              form   = res$n_values %||% 0L,
              tables = res$n_rows %||% res$run_log$n_table_rows %||% 0L,
              res$run_log$row_count %||% 0L)
  v <- suppressWarnings(as.integer(v)[1])
  list(n = if (is.na(v)) 0L else v,
       of = switch(k, form = "values", tables = "table rows",
                   statement = "transactions", "items"))
}

# Just the number, for callers that only want that.
.rows_of <- function(res) .how_much(res)$n

# The result a file gets when even convert_document() could not produce one. It promises never to throw,
# so this should be unreachable; one impossible error must not cost the other files their run.
.failed_result <- function(why) {
  list(status = "failed", template_id = NA_character_, kind = "statement",
       messages = status_message("failed", why,
                                 "check the file is readable and matches a template"))
}

# convert_batch(paths, ..., progress) -> one row per path, in the order given; nothing sorted,
# deduplicated or skipped. rows_of says what `rows` COUNTS, so a 0 reads as "nothing came out".
# `result` is the WHOLE object; app.R marks it `dropped_feed_rows` in that word order because `$`
# partially matches and `feed_rows_dropped` would make res$feed_rows return the marker.
convert_batch <- function(paths, ..., progress = NULL) {
  paths <- as.character(paths %||% character(0))
  n <- length(paths)

  out <- data.frame(
    file          = paths,
    status        = rep(NA_character_, n),
    bank          = rep(NA_character_, n),
    template_id   = rep(NA_character_, n),
    rows          = rep(NA_integer_,   n),
    rows_of       = rep(NA_character_, n),
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
    # The engine keeps the CLOSEST template on an unsupported result, but a template id beside an
    # unsupported row reads as "this template was used". Take the log's answer, not a second rule.
    out$template_id[i]   <- as.character(res$run_log$detected_template %||% NA_character_)[1]
    hm                   <- .how_much(res)
    out$rows[i]          <- hm$n
    out$rows_of[i]       <- hm$of
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

# The statuses convert_document() can return, worst last.
BATCH_STATUSES <- c("ok", "needs_review", "unsupported", "failed")

# Every status is listed even at zero, so "0 failed" is SAID rather than inferred; anything unexpected
# is appended rather than dropped.
batch_summary <- function(batch) {
  s <- as.character(batch$status %||% character(0))
  s[is.na(s)] <- "?"
  lv <- c(BATCH_STATUSES, setdiff(unique(s), BATCH_STATUSES))
  data.frame(status = lv,
             n = vapply(lv, function(x) sum(s == x), integer(1), USE.NAMES = FALSE),
             stringsAsFactors = FALSE, row.names = NULL)
}
