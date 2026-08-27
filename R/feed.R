# feed.R -- the analytics feed Qlik loads. write_feed() writes a flat, fully stamped transactions CSV
# IF the result clears the governance gate (reconciled + curated template); a one-row manifest is
# written for EVERY conversion so coverage is never silent. One file per statement, never a shared
# append. Qlik concatenates on field set rather than table name, so every row carries its own
# gate_result and accepted/withheld can never be pooled.

# THE FEED'S FIXED SCHEMA. Qlik loads these folders with a wildcard `LOAD *`
# (docs/operational/connecting-qlik.md), which concatenates two CSVs only when their field sets match
# EXACTLY; differ by one column and it invents a synthetic key, splitting the dashboard's totals in
# two. converted_ts_utc is named for its zone: a 09:30 NZ conversion arrived stamped 21:30.
FEED_CONTEXT_COLUMNS <- c(
  "run_id", "converted_ts_utc", "source_file", "source_sha256", "bank",
  "statement_type", "template_id", "template_version", "template_origin",
  "trust_level", "gate_result", "period_start", "period_end", "account_number",
  "statement_index")

FEED_CORE_COLUMNS <- c(
  "row_id", "date", "description", "amount", "debit", "credit", "direction",
  "balance", "particulars", "code", "reference", "other_party", "type",
  "currency", "flags")

.feed_key <- function(sha) if (is.null(sha) || is.na(sha) || !nzchar(sha)) NA_character_ else substr(sha, 1, 16)

# Write a feed CSV so a Qlik load can never read a half-written file: a per-process temp in the SAME
# directory, then an atomic rename. Warnings count as failures - a read-only share makes file() WARN
# while write.csv returns normally, so an error-only guard called it a success.
.atomic_write_csv <- function(df, path) {
  tmp <- paste0(path, ".", Sys.getpid(), ".part")
  why <- NA_character_
  ok <- withCallingHandlers(
    tryCatch({ utils::write.csv(df, tmp, row.names = FALSE, na = ""); TRUE },
             error = function(e) { why <<- conditionMessage(e); FALSE }),
    warning = function(w) { why <<- conditionMessage(w); invokeRestart("muffleWarning") })
  if (!is.na(why)) ok <- FALSE            # a warned-about write did not happen
  if (isTRUE(ok) && file.exists(tmp)) {
    if (!isTRUE(safe(file.rename(tmp, path), FALSE))) {
      safe(unlink(tmp)); ok <- FALSE
      if (is.na(why)) why <- sprintf("could not rename the temp file over %s", path)
    }
  } else {
    safe(unlink(tmp)); ok <- FALSE
    if (is.na(why)) why <- sprintf("no file was produced at %s", tmp)
  }
  structure(isTRUE(ok), why = why)
}

# Answer truthfully whether the file is GONE. unlink()'s return code is not the authority: on the
# documented Windows share a file another process holds open cannot be deleted, and unlink() never
# throws, so a file that refused to go looked exactly like one that went.
.removed <- function(path) {
  safe(unlink(path))
  !file.exists(path)
}

# An NA or blank level coalesces to the LOWEST trust (fail-closed), never letting an `if (NA)` throw
# inside the gate - which safe() would swallow, dropping both the feed row and the manifest.
.trust_ok <- function(level, min_trust) {
  level <- tolower(level %||% "")
  if (length(level) != 1 || is.na(level) || !nzchar(level)) level <- "low"
  min_trust <- tolower(min_trust %||% "high")
  switch(min_trust,
    any    = TRUE,
    medium = level %in% c("high", "medium"),
    level == "high")                       # default: high only
}

# Deliberately MACHINE-ONLY: governed analytics means the dashboard's contents are a function of the
# checks, not of who was in a hurry.
.feed_gate <- function(result, cfg, proven) {
  status <- result$status %||% "failed"
  origin <- if (isTRUE(proven)) "default" else "user"
  allowed <- unlist(cfg$feed$allowed_template_origins %||% list("default"))
  allowlist <- unlist(cfg$feed$template_allowlist %||% list())
  tid <- (result$template_id %||% NA_character_)[1]
  if (isTRUE(cfg$feed$require_status_ok) && !identical(status, "ok"))
    return(list(accept = FALSE, reason = paste0("withheld:", status), origin = origin))
  # 'medium' ships as the default because a clean statement with no running-balance column can never
  # reach 'high'. The gate does not decide policy, the config does.
  if (!.trust_ok(result$trust$level, cfg$feed$min_trust))
    return(list(accept = FALSE, reason = "withheld:low_trust", origin = origin))
  if (!(origin %in% allowed))
    return(list(accept = FALSE, reason = "withheld:not_proven", origin = origin))
  if (length(allowlist) && !(tid %in% allowlist))
    return(list(accept = FALSE, reason = "withheld:not_in_allowlist", origin = origin))
  list(accept = TRUE, reason = "accepted", origin = origin)
}

# The FIXED transaction half of a feed row set; the verbatim *_raw cells never leave the JSON. The
# extras used to be appended here and that split the dashboard: a card statement carrying fx_amount
# made a 32-column file among 30-column ones, and Qlik makes a second table rather than merging.
.feed_core_frame <- function(rows) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE, check.names = FALSE)
  n <- nrow(rows)
  core <- lapply(FEED_CORE_COLUMNS,
                 function(col) if (col %in% names(rows)) rows[[col]] else rep(NA, n))
  names(core) <- FEED_CORE_COLUMNS
  as.data.frame(core, stringsAsFactors = FALSE, check.names = FALSE)
}

.feed_extra_cols <- function(rows)
  setdiff(names(rows), c(FEED_CONTEXT_COLUMNS, FEED_CORE_COLUMNS, .DISPLAY_DROP_COLS))

# A template's extras in their own table rather than widening the transactions one, keyed by row_id
# so a dashboard can join them back. NULL when the template has none, which writes no file.
.feed_extras_frame <- function(rows) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE, check.names = FALSE)
  extra <- .feed_extra_cols(rows)
  if (!length(extra) || !nrow(rows)) return(NULL)
  key <- if ("row_id" %in% names(rows)) rows[["row_id"]] else seq_len(nrow(rows))
  cbind(data.frame(row_id = key, stringsAsFactors = FALSE),
        rows[, extra, drop = FALSE])
}

# R/split.R deliberately nulls the COMBINED header's account_number and widens the period to the whole
# bundle, so flattening that header onto every row would give rows with no account number and a
# twelve-month period. Each row carries its statement_index, so stamp that statement's own anchors.
.split_stamp <- function(field, statements, si, fallback) {
  idx <- vapply(statements, function(s) {
    v <- suppressWarnings(as.integer(s$index %||% NA_integer_)); if (length(v) != 1L) NA_integer_ else v
  }, integer(1))
  vals <- vapply(statements, function(s) {
    v <- s[[field]]
    if (is.null(v) || !length(v) || is.na(v[1])) NA_character_ else as.character(v)[1]
  }, character(1))
  m <- match(si, idx)
  hit <- !is.na(m) & !is.na(vals[m])
  out <- fallback
  out[hit] <- vals[m[hit]]
  out
}

# One file per feed event under <logs>/feed/, and not just the manifest: when the UNC share goes
# read-only the manifest write fails too and the feed's own folder cannot report its outage.
.log_feed_outcome <- function(logdir, id, rec) {
  if (is.null(logdir) || !nzchar(logdir)) return(invisible(NULL))
  safe(write_log_record(logdir, "feed", id, rec))
}

# Returns the gate result invisibly, carrying $accept, $reason, $gate_result, $written and
# $feed_file. `ts` is passed in so the call stays deterministic in tests.
write_feed <- function(result, config = load_config(), ts = NULL,
                       proven_ids = NULL, logdir = NULL) {
  if (!isTRUE(config$feed$enabled)) return(invisible(NULL))
  # STATEMENTS ONLY: "form" and "tables" produce figures with NO reconciliation behind them.
  # isTRUE(), not a bare %in%: a statement result carries no `kind`, and `NULL %in% c(...)` is
  # logical(0), which `if` rejects outright.
  if (isTRUE(result$kind %in% c("form", "tables"))) return(invisible(NULL))
  logdir <- logdir %||% (config$paths$logs %||% "logs")
  h <- result$header %||% list()
  sha <- h$source_sha256 %||% NA_character_
  key <- .feed_key(sha)
  run_id <- result$run_id %||% NA_character_
  if (is.null(ts)) ts <- utc_stamp()
  if (is.na(key)) {
    .log_feed_outcome(logdir, if (is.na(run_id)) "na" else run_id,
      list(ts = ts, run_id = run_id, source_file = h$source_file %||% NA_character_,
           source_sha256 = sha, template_id = (result$template_id %||% NA_character_)[1],
           gate_result = "skipped:no_source_hash", feed_written = FALSE,
           feed_file = NA_character_, manifest_written = FALSE,
           row_count = NA_integer_, engine_version = engine_version()))
    return(invisible(NULL))
  }

  if (is.null(proven_ids))
    proven_ids <- tryCatch(names(load_templates(config$paths$templates, strict = FALSE)),
                           error = function(e) character(0))
  tid <- (result$template_id %||% NA_character_)[1]
  proven <- !is.na(tid) && tid %in% proven_ids
  gate <- .feed_gate(result, config, proven)

  fdir <- config$feed$feed_dir %||% "feed"
  tx_dir  <- file.path(fdir, "transactions"); runs_dir <- file.path(fdir, "runs")
  rev_dir <- file.path(fdir, "review")
  for (d in c(tx_dir, runs_dir, if (isTRUE(config$feed$include_review_feed)) rev_dir))
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

  row_count <- suppressWarnings(as.integer(h$row_count %||% NA_integer_))

  rows <- result$feed_rows
  tx_written <- NA_character_
  tx_state <- "no_rows"
  tx_why <- NA_character_
  extras_why <- NA_character_   # a failed EXTRAS write is recorded, never fatal
  dest_dir <- if (gate$accept) tx_dir
              else if (isTRUE(config$feed$include_review_feed)) rev_dir else NULL
  if (!is.null(rows) && nrow(rows)) {
    if (is.null(dest_dir)) {
      tx_state <- "not_kept"          # withheld and the review feed is switched off
    } else {
      n <- nrow(rows)
      si <- if ("statement_index" %in% names(rows))
              suppressWarnings(as.integer(rows$statement_index)) else rep(NA_integer_, n)
      hdr1 <- function(v) as.character(v %||% NA_character_)[1]
      period_start <- rep(hdr1(h$period_start), n)
      period_end   <- rep(hdr1(h$period_end), n)
      account      <- rep(hdr1(h$account_number), n)
      sts <- result$metadata$split$statements
      if (!is.null(sts) && length(sts) && any(!is.na(si))) {
        period_start <- .split_stamp("period_start", sts, si, period_start)
        period_end   <- .split_stamp("period_end", sts, si, period_end)
        account      <- .split_stamp("account_number", sts, si, account)
      }
      ctx <- data.frame(
        run_id = run_id, converted_ts_utc = ts,
        source_file = h$source_file %||% NA_character_, source_sha256 = sha,
        bank = h$bank %||% NA_character_, statement_type = h$statement_type %||% NA_character_,
        template_id = tid, template_version = as.character(h$template_version %||% NA),
        template_origin = gate$origin, trust_level = result$trust$level %||% NA_character_,
        # Accepted and withheld rows were otherwise field-identical, and Qlik auto-concatenates
        # identical field sets whatever the table is called, so a withheld row could be summed into a
        # dashboard total with nothing saying so.
        gate_result = gate$reason,
        period_start = period_start, period_end = period_end,
        account_number = account,
        statement_index = si,
        stringsAsFactors = FALSE)
      stamped <- cbind(ctx[, FEED_CONTEXT_COLUMNS, drop = FALSE], .feed_core_frame(rows))
      f <- file.path(dest_dir, paste0(key, ".csv"))
      wrote <- .atomic_write_csv(stamped, f)
      if (isTRUE(wrote)) { tx_written <- f; tx_state <- "written" }
      else { tx_state <- "write_failed"; tx_why <- attr(wrote, "why") %||% NA_character_ }
      # A template's extras travel in their own folder so the transactions table keeps ONE field set
      # for ever. This block sits AFTER the if/else on purpose - written between them it stole the
      # `else`, so every ordinary run read write_failed.
      if (isTRUE(wrote)) {
        ex <- .feed_extras_frame(rows)
        if (!is.null(ex)) {
          ex <- cbind(run_id = ctx$run_id[1], ex, stringsAsFactors = FALSE)
          ex_dir <- file.path(dirname(dest_dir), "extras")
          if (!dir.exists(ex_dir)) dir.create(ex_dir, recursive = TRUE, showWarnings = FALSE)
          ex_ok <- .atomic_write_csv(ex, file.path(ex_dir, paste0(key, ".csv")))
          if (!isTRUE(ex_ok)) extras_why <- attr(ex_ok, "why") %||% NA_character_
        }
      }
    }
  }

  # A statement's key maps to exactly ONE feed file, so any prior row in the other folder must go: a
  # re-convert can flip the gate and a stale "accepted" row keeps feeding data nothing stands behind.
  # AFTER the write, because unlinking first left a failed write with no row anywhere. The sibling
  # goes even when the write FAILED - a wrong row on the dashboard is the cardinal failure - and the
  # removal is verified, with a survivor carried forward as `stale`.
  kept <- if (identical(tx_state, "written")) dirname(tx_written) else character(0)
  stale <- character(0)
  for (d in setdiff(c(tx_dir, rev_dir), kept)) {
    old <- file.path(d, paste0(key, ".csv"))
    if (file.exists(old) && !.removed(old)) stale <- c(stale, old)
  }

  # A write that failed is still reported: the conversion was accepted, the feed did not receive it.
  # At most ONE suffix, most serious first - everything downstream reads the verdict as its last
  # colon-segment, and a wrong figure still loading outranks a right one that never arrived.
  gate_result <- gate$reason
  if (length(stale)) {
    gate_result <- paste0(gate_result, ":stale_row_kept")
    why <- sprintf("an earlier row for this statement is still in %s and could not be removed",
                   paste(unique(dirname(stale)), collapse = "; "))
    tx_why <- if (is.na(tx_why)) why else paste0(tx_why, "; ", why)
  } else if (identical(tx_state, "write_failed")) {
    gate_result <- paste0(gate_result, ":write_failed")
  } else if (identical(tx_state, "no_rows") && !is.na(row_count) && row_count > 0) {
    gate_result <- paste0(gate_result, ":no_rows")
  }

  manifest <- data.frame(
    run_id = run_id, converted_ts_utc = ts,
    source_file = h$source_file %||% NA_character_, source_sha256 = sha,
    bank = h$bank %||% NA_character_, template_id = tid,
    template_origin = gate$origin, status = result$status %||% "failed",
    trust_level = result$trust$level %||% NA_character_, row_count = row_count,
    period_start = h$period_start %||% NA_character_, period_end = h$period_end %||% NA_character_,
    gate_result = gate_result,
    feed_file = if (!is.na(tx_written)) basename(tx_written) else NA_character_,
    engine_version = engine_version(),
    template_sha256 = result$run_log$template_sha256 %||% NA_character_,
    stringsAsFactors = FALSE)
  # Key the manifest by the statement's CONTENT hash, not the per-run id, so a re-convert overwrites
  # its own row and the coverage table counts each statement once.
  man_ok <- .atomic_write_csv(manifest, file.path(runs_dir, paste0(key, ".csv")))
  if (!isTRUE(man_ok) && is.na(tx_why)) tx_why <- attr(man_ok, "why") %||% NA_character_

  .log_feed_outcome(logdir, if (is.na(run_id)) key else run_id, list(
    ts = ts, run_id = run_id, source_file = h$source_file %||% NA_character_,
    source_sha256 = sha, template_id = tid, gate_result = gate_result,
    feed_written = identical(tx_state, "written"),
    feed_file = if (is.na(tx_written)) NA_character_ else basename(tx_written),
    feed_dir = fdir, manifest_written = isTRUE(man_ok),
    detail = tx_why, row_count = row_count, engine_version = engine_version()))

  gate$gate_result <- gate_result
  gate$written <- identical(tx_state, "written")
  gate$feed_file <- tx_written
  gate$manifest_written <- isTRUE(man_ok)
  invisible(gate)
}

# One CSV per statement and nothing removed one, while Qlik loads each folder with a wildcard: at 50
# conversions a day that is 18,000 files walked on every reload after a year. IT MOVES, IT NEVER
# DELETES - rows go to feed/archive/, which the wildcard does not reach.
archive_feed <- function(config = load_config(), keep_days = NULL,
                         now = as.numeric(Sys.time())) {
  out <- list(archived = 0L, kept = 0L)
  fdir <- config$feed$feed_dir %||% "feed"
  kd <- suppressWarnings(as.numeric(keep_days %||% config$feed$keep_days %||% NA)[1])
  if (!is.finite(kd) || kd <= 0 || !dir.exists(fdir)) return(out)
  cutoff <- now - kd * 86400
  for (sub in c("transactions", "review", "runs", "extras")) {
    d <- file.path(fdir, sub)
    if (!dir.exists(d)) next
    for (f in list.files(d, pattern = "\\.csv$", full.names = TRUE)) {
      mt <- suppressWarnings(as.numeric(file.info(f)$mtime))
      if (!is.finite(mt) || mt >= cutoff) { out$kept <- out$kept + 1L; next }
      adir <- file.path(fdir, "archive", sub)
      if (!dir.exists(adir)) dir.create(adir, recursive = TRUE, showWarnings = FALSE)
      if (isTRUE(safe(file.rename(f, file.path(adir, basename(f))), FALSE)))
        out$archived <- out$archived + 1L
      else out$kept <- out$kept + 1L
    }
  }
  out
}

feed_retention_note <- function(keep_days) {
  kd <- suppressWarnings(as.numeric(keep_days %||% NA)[1])
  if (!is.finite(kd) || kd <= 0)
    return("Every converted statement stays in the dashboard feed folder for good, so the reload gets slower as it fills.")
  sprintf(paste("Rows stay in the dashboard feed folder for %d day%s, then move to",
                "feed\\archive\\, where they are kept but no longer loaded."),
          as.integer(kd), if (as.integer(kd) == 1L) "" else "s")
}

# Feedback used to be write-only: a conversion marked WRONG kept feeding the dashboards for ever. A
# "wrong" verdict WITHDRAWS that run's rows - reversible, rows MOVE to feed/review. `rows` is NA when
# an accepted file could NOT be withdrawn.
retract_feed <- function(run_id, config = load_config(), logdir = NULL,
                         reason = "retracted:user_reported_wrong") {
  out <- list(files = 0L, rows = 0L, moved = character(0))
  run_id <- as.character(run_id %||% "")[1]
  if (is.na(run_id) || !nzchar(run_id)) return(out)
  fdir <- config$feed$feed_dir %||% "feed"
  tx_dir <- file.path(fdir, "transactions"); rev_dir <- file.path(fdir, "review")
  runs_dir <- file.path(fdir, "runs")
  if (!dir.exists(tx_dir)) return(out)
  logdir <- logdir %||% (config$paths$logs %||% "logs")
  keep_review <- isTRUE(config$feed$include_review_feed)
  stuck <- character(0)

  for (f in list.files(tx_dir, pattern = "\\.csv$", full.names = TRUE)) {
    df <- safe(utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE,
                               colClasses = "character"), NULL)
    if (is.null(df) || !nrow(df) || !("run_id" %in% names(df))) next
    if (!any(df$run_id == run_id, na.rm = TRUE)) next
    key <- sub("\\.csv$", "", basename(f))
    if ("gate_result" %in% names(df)) df$gate_result <- reason
    review_copy <- file.path(rev_dir, basename(f))
    if (keep_review) {
      if (!dir.exists(rev_dir)) dir.create(rev_dir, recursive = TRUE, showWarnings = FALSE)
      safe(.atomic_write_csv(df, review_copy))
    }
    # On the documented Windows share the file can refuse to go, and the alternative is telling the
    # one person who KNOWS the figures are wrong that they have been withdrawn while they are not.
    if (!.removed(f)) {
      # Drop the review copy again: leaving it would ADD a second, field-identical set of rows for a
      # statement whose accepted rows are still there, so a failed withdrawal double-counts money.
      if (keep_review) safe(unlink(review_copy))
      stuck <- c(stuck, basename(f))
      next
    }
    mf <- file.path(runs_dir, paste0(key, ".csv"))
    if (file.exists(mf)) {
      man <- safe(utils::read.csv(mf, stringsAsFactors = FALSE, check.names = FALSE,
                                  colClasses = "character"), NULL)
      if (!is.null(man) && nrow(man)) {
        if ("gate_result" %in% names(man)) man$gate_result <- reason
        if ("feed_file" %in% names(man)) man$feed_file <- NA_character_
        safe(.atomic_write_csv(man, mf))
      }
    }
    out$files <- out$files + 1L
    out$rows <- out$rows + sum(df$run_id == run_id, na.rm = TRUE)
    out$moved <- c(out$moved, basename(f))
  }
  if (out$files > 0L || length(stuck))
    .log_feed_outcome(logdir, paste0(run_id, "__retracted"), list(
      ts = utc_stamp(),
      run_id = run_id,
      gate_result = if (length(stuck)) paste0(reason, ":stale_row_kept") else reason,
      feed_written = FALSE,
      feed_file = paste(c(out$moved, stuck), collapse = "; "), feed_dir = fdir,
      manifest_written = TRUE,
      detail = if (length(stuck)) sprintf(
        "%d accepted feed file(s) could not be removed from %s and are still being loaded: %s",
        length(stuck), tx_dir, paste(stuck, collapse = "; ")) else NA_character_,
      row_count = out$rows, engine_version = engine_version()))
  if (length(stuck)) out$rows <- NA_integer_
  out
}
