# feed.R -- the analytics feed Qlik loads for dashboards.
#
# Accountants convert in the Shiny app; write_feed() is called as a side-effect of
# each conversion and, IF the result clears the governance gate (reconciled +
# proven/curated template), writes a flat, fully-stamped transactions CSV that a
# Qlik folder connection loads. A one-row manifest is written for EVERY conversion
# (accepted or withheld) so coverage is never silent. Concurrency-safe: one file per
# statement (content-hash keyed) / per run, never a shared append.
#
# It NEVER throws (callers still wrap in safe()); a feed problem must not affect the
# user's conversion. But it must never be SILENT either, so every call also writes
# a one-file-per-event outcome record under <logs>/feed/ -- see .log_feed_outcome().
#
# WHO CALLS IT: the Convert button only (app.R), which is the one place a person
# has looked at the verdict. Admin's bulk re-audit and the CLI scripts deliberately
# do NOT feed -- they exist to test templates over piles of files, and publishing
# those runs would put rows nobody reviewed into org-wide dashboards.
#
# WHERE ROWS GO: feed/transactions (accepted) and feed/review (withheld) are two
# folders on purpose -- review is meant to be loadable, so a team can see what is
# being held back and why. Because Qlik concatenates on field set rather than on
# table name, every row carries its own gate_result (below) so the two can never
# be silently pooled into one total.

# ---- the feed's FIXED schema -------------------------------------------------
# Qlik loads these folders with a wildcard `LOAD *` (docs/operational/connecting-qlik.md).
# A wildcard load concatenates two CSVs only when their field sets match EXACTLY;
# differ by one column and Qlik invents a synthetic key instead, quietly splitting
# the dashboard's totals in two. The old feed built its header from whatever the
# conversion happened to produce, so adding a bank template -- the charter's cheap
# path -- changed the column SET (26 vs 28 columns in two committed feed files)
# and, because display_transactions() splices debit/credit next to `amount`, the
# ORDER too.
#
# So: every feed row now starts with these columns, in this order, always present
# (empty where a statement doesn't have that field). Named template extras are
# APPENDED after them, keeping their own names -- an extra is real captured data
# and must stay identifiable, never collapsed into extra_1..n. Qlik concatenates
# the fixed part cleanly and a per-bank extra simply arrives as a mostly-null
# field, which is exactly what it is.
#
# converted_ts_utc IS NAMED FOR ITS ZONE, and that is the whole reason for the
# name. It has always been written in UTC (below) while the run log, the uploads
# and the feedback were written in the server's local time, so on a New Zealand
# box a 09:30 conversion arrived on the dashboard stamped 21:30 the PREVIOUS DAY
# -- and a Qlik expression grouping by date had no way of knowing. Every writer
# now stamps UTC (R/util.R), and this column says so in its own name so that a
# dashboard cannot mistake it for the analyst's wall clock.
FEED_CONTEXT_COLUMNS <- c(
  "run_id", "converted_ts_utc", "source_file", "source_sha256", "bank",
  "statement_type", "template_id", "template_version", "template_origin",
  "trust_level", "gate_result", "period_start", "period_end", "account_number",
  "statement_index")

FEED_CORE_COLUMNS <- c(
  "row_id", "date", "description", "amount", "debit", "credit", "direction",
  "balance", "particulars", "code", "reference", "other_party", "type",
  "currency", "flags")

# .feed_key(sha) -- the per-statement key: first 16 of the content hash, so a
# re-convert overwrites (idempotent) and two different files never collide.
.feed_key <- function(sha) if (is.null(sha) || is.na(sha) || !nzchar(sha)) NA_character_ else substr(sha, 1, 16)

# .atomic_write_csv(df, path) -- write a feed CSV so a Qlik load can NEVER read a
# half-written file: write to a per-process temp in the SAME directory, then
# rename over the target (atomic on a POSIX same-filesystem move). On any failure
# the temp is removed and the existing file is left untouched. Returns TRUE on
# success, with attr "why" carrying the reason on failure. Feed writes must never
# throw, so both errors and warnings are handled here.
#
# WHY warnings are captured, not just errors: a read-only share -- the failure this
# whole path exists for -- makes file() raise a WARNING and write.csv return
# normally, so an error-only guard would call a failed write a success. The
# message travels back with the FALSE so the feed log can say WHY, instead of it
# being printed as a loose console warning nobody connects to the feed.
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

# .removed(path) -- attempt to remove a feed file and answer, truthfully, whether
# it is GONE. Returns TRUE when the path no longer exists.
#
# WHY NOT just unlink(): unlink()'s return code is not the authority here. On the
# documented Windows deployment the feed folder is a share that Qlik reloads and
# analysts open in Excel, and a file another process holds open cannot be deleted
# at all; a share can also be granted create-but-not-delete rights. In both cases
# the row stays. unlink() itself never throws, so the safe() around the old call
# caught nothing -- the return value was simply dropped, and a feed file that
# refused to go looked exactly like one that went. file.exists() afterwards is the
# only check that cannot be fooled by either platform.
.removed <- function(path) {
  safe(unlink(path))
  !file.exists(path)
}

# .trust_ok(level, min_trust) -- does the trust level meet the floor? An NA / blank
# level coalesces to the LOWEST trust (fail-closed -> withheld), never letting an
# `if (NA)` throw inside the gate (which safe() would swallow, silently dropping
# both the feed row and the manifest).
.trust_ok <- function(level, min_trust) {
  level <- tolower(level %||% "")
  if (length(level) != 1 || is.na(level) || !nzchar(level)) level <- "low"
  min_trust <- tolower(min_trust %||% "high")
  switch(min_trust,
    any    = TRUE,
    medium = level %in% c("high", "medium"),
    level == "high")                       # default: high only
}

# .feed_gate(result, cfg, proven) -- decide accept/withhold + the reason.
# The gate is deliberately MACHINE-ONLY: no human can wave a conversion through,
# because "governed analytics" means the dashboard's contents are a function of
# the checks, not of who was in a hurry. A conversion that should be in the feed
# but isn't gets there by fixing the template (then re-converting), which is the
# durable fix and leaves an audit trail.
.feed_gate <- function(result, cfg, proven) {
  status <- result$status %||% "failed"
  origin <- if (isTRUE(proven)) "default" else "user"
  allowed <- unlist(cfg$feed$allowed_template_origins %||% list("default"))
  allowlist <- unlist(cfg$feed$template_allowlist %||% list())
  tid <- (result$template_id %||% NA_character_)[1]
  if (isTRUE(cfg$feed$require_status_ok) && !identical(status, "ok"))
    return(list(accept = FALSE, reason = paste0("withheld:", status), origin = origin))
  # min_trust is a documented, one-line config knob (config/config.example.yaml);
  # 'medium' ships as the default because a clean statement with no running
  # balance column can never reach 'high', and withholding every one of those
  # would empty the feed for whole banks. Raise it to 'high' for balance-proven
  # only -- the gate itself does not decide policy, the config does.
  if (!.trust_ok(result$trust$level, cfg$feed$min_trust))
    return(list(accept = FALSE, reason = "withheld:low_trust", origin = origin))
  if (!(origin %in% allowed))
    return(list(accept = FALSE, reason = "withheld:not_proven", origin = origin))
  if (length(allowlist) && !(tid %in% allowlist))
    return(list(accept = FALSE, reason = "withheld:not_in_allowlist", origin = origin))
  list(accept = TRUE, reason = "accepted", origin = origin)
}

# .feed_core_frame(rows) -- the FIXED transaction half of a feed row set: every
# core column in FEED_CORE_COLUMNS order, missing ones present but empty, AND
# NOTHING ELSE. `statement_index` and the verbatim *_raw cells are excluded here
# -- the first is context (stamped per row below), the second never leaves the
# JSON/Provenance record.
#
# THE EXTRAS USED TO BE APPENDED HERE, AND THAT SPLIT THE DASHBOARD. The comment
# above FEED_CONTEXT_COLUMNS explains that a wildcard `LOAD *` concatenates only
# on an EXACT field-set match -- and then this function appended each template's
# own extras, so a card statement carrying fx_amount / conversion_charge produced
# a 32-column file among 30-column ones. Measured on the real feed folder: 17
# files at 30 columns, 2 at 32. Qlik does not merge those; it makes a second
# table, and the unit's totals are short by whatever is in it, with nothing on
# any screen saying so. connecting-qlik.md names this exact symptom ("Totals
# split in two - a field set mismatch") two pages after the old comment here
# promised it was safe. One of the two had to go, and the promise was the wrong
# one to keep: a stable field set is worth more to a dashboard than an extra
# column, and the extra is not lost -- see .feed_extras_frame below.
.feed_core_frame <- function(rows) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE, check.names = FALSE)
  n <- nrow(rows)
  core <- lapply(FEED_CORE_COLUMNS,
                 function(col) if (col %in% names(rows)) rows[[col]] else rep(NA, n))
  names(core) <- FEED_CORE_COLUMNS
  as.data.frame(core, stringsAsFactors = FALSE, check.names = FALSE)
}

# .feed_extra_cols(rows) -- the template's own named columns, if any.
.feed_extra_cols <- function(rows)
  setdiff(names(rows), c(FEED_CONTEXT_COLUMNS, FEED_CORE_COLUMNS, .DISPLAY_DROP_COLS))

# .feed_extras_frame(rows) -- a template's extras, carried in their own table
# rather than widening the transactions one. Keyed by row_id so a dashboard that
# wants fx_amount can join it back, and named verbatim because an extra is real
# captured data and must stay identifiable. NULL when the template has none,
# which is the ordinary case and writes no file at all.
.feed_extras_frame <- function(rows) {
  rows <- as.data.frame(rows, stringsAsFactors = FALSE, check.names = FALSE)
  extra <- .feed_extra_cols(rows)
  if (!length(extra) || !nrow(rows)) return(NULL)
  key <- if ("row_id" %in% names(rows)) rows[["row_id"]] else seq_len(nrow(rows))
  cbind(data.frame(row_id = key, stringsAsFactors = FALSE),
        rows[, extra, drop = FALSE])
}

# .split_stamp(field, statements, si, fallback) -- per-statement value for each
# row of an auto-split bundle.
#
# WHY: R/split.R deliberately nulls the COMBINED header's account_number and
# widens the period to the whole bundle, because one value cannot describe five
# statements. The feed flattens the header onto EVERY row, so the moment a
# template opts into auto-split the dashboard would gain rows with no account
# number and a twelve-month period -- plausible, and wrong. Each row carries the
# `statement_index` it came from, and result$metadata$split$statements holds that
# statement's own anchors, so stamp those instead. Anything the split summary
# does not carry falls back to the combined header (never a guess).
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

# .log_feed_outcome(logdir, id, rec) -- one file per feed event, under
# <logs>/feed/, exactly like the run and feedback logs.
#
# WHY the LOG and not just the manifest: the documented deployment writes the feed
# to a UNC share. When that share goes read-only, the manifest write fails too --
# so the feed's own folder cannot report its own outage. The logs live with the
# app, so this record survives to say "the gate accepted it and the write failed",
# which is what turns a silent, permanent feed stoppage into a visible one.
.log_feed_outcome <- function(logdir, id, rec) {
  if (is.null(logdir) || !nzchar(logdir)) return(invisible(NULL))
  safe(write_log_record(logdir, "feed", id, rec))
}

# write_feed(result, config, ts) -> the gate result (invisibly), carrying
# $accept, $reason, $gate_result (the reason as recorded, with any write-failure
# suffix), $written and $feed_file. `result` is the convert_statement() return.
# `ts` is the ISO timestamp to stamp (pass it in so the call stays deterministic
# in tests; defaults to now).
write_feed <- function(result, config = load_config(), ts = NULL,
                       proven_ids = NULL, logdir = NULL) {
  if (!isTRUE(config$feed$enabled)) return(invisible(NULL))
  # STATEMENTS ONLY, AND THIS IS THE GATE THAT MAKES THAT TRUE.
  # "form" (labelled values) and "tables" (the multi-table document extractor,
  # R/doc_extract.R) both produce figures with NO reconciliation behind them --
  # nothing that could tell a right number from a wrong one. They are download-only
  # by design, and this line is where that design is actually enforced, ahead of
  # every other consideration in this function.
  #
  # isTRUE(), not a bare %in%: a statement result carries no `kind` at all, and
  # `NULL %in% c(...)` is logical(0), which `if` rejects outright -- that would
  # have thrown on every ordinary conversion instead of on none.
  if (isTRUE(result$kind %in% c("form", "tables"))) return(invisible(NULL))
  logdir <- logdir %||% (config$paths$logs %||% "logs")
  h <- result$header %||% list()
  sha <- h$source_sha256 %||% NA_character_
  key <- .feed_key(sha)
  run_id <- result$run_id %||% NA_character_
  # UTC + explicit zone, so the feed's converted_ts_utc is reproducible across
  # hosts (the workbook/CSV/JSON outputs already are); the "Z" marks it
  # unambiguously. One helper, used by every writer in the tool -- see R/util.R.
  if (is.null(ts)) ts <- utc_stamp()
  if (is.na(key)) {
    # Nothing to key the feed on. Refusing is right; refusing SILENTLY is not.
    .log_feed_outcome(logdir, if (is.na(run_id)) "na" else run_id,
      list(ts = ts, run_id = run_id, source_file = h$source_file %||% NA_character_,
           source_sha256 = sha, template_id = (result$template_id %||% NA_character_)[1],
           gate_result = "skipped:no_source_hash", feed_written = FALSE,
           feed_file = NA_character_, manifest_written = FALSE,
           row_count = NA_integer_, engine_version = engine_version()))
    return(invisible(NULL))
  }

  # "Proven" = the template is one of the curated/tested set (paths$templates).
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

  # --- the flat, stamped transactions (accepted -> dashboard; withheld -> review) --
  # Source: the PARSED table the workbook and CSV were written from, NOT a re-read
  # of the output CSV. See convert.R (result$feed_rows) for why.
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
      # [1] guards the rep(): a header field is contractually one value, and a
      # stray vector would otherwise recycle DIFFERENT values onto the rows.
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
        # Rows land in feed/transactions (accepted) or feed/review (withheld), but
        # the two tables were otherwise field-identical -- and Qlik auto-concatenates
        # identical field sets whatever the table is called, so a withheld row could
        # be summed into a dashboard total with nothing on it saying so. Every row
        # now carries its own verdict.
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
      # A template's own extra columns travel in their own folder, keyed by
      # run_id + row_id, so the transactions table keeps ONE field set for ever
      # and Qlik can still join fx_amount back if a dashboard wants it. Written
      # only when the template actually has extras, so the folder stays absent on
      # an ordinary deployment rather than filling with empty files. A failure
      # here must not fail the run: the figures are already safely in the
      # transactions table, so it is recorded and carried, not raised.
      # (This block sits AFTER the if/else above on purpose. Written between them
      # it stole the `else`, so every ordinary run -- the ones with no extras at
      # all -- was recorded as write_failed while its file sat there correctly
      # written. The suite caught it; a dangling else is why it could happen.)
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

  # A statement's key maps to exactly ONE feed file, so any prior row for this key
  # in the OTHER folder must go: a re-convert can flip the gate (accepted <->
  # withheld) and a stale "accepted" row would keep feeding Qlik data the current
  # conversion no longer stands behind. This runs AFTER the write, never before:
  # unlinking first meant a failed write left the statement with NO row anywhere,
  # and nothing said so. Removing the sibling even when the write FAILED is
  # deliberate -- a wrong row on the dashboard is the cardinal failure, a missing
  # one is merely visible (and the record below says exactly what happened).
  #
  # That reasoning only holds while the removal actually happens, and it is the one
  # step here that CANNOT be made to succeed: the old row sits on the share, not on
  # this box. So each removal is verified, and a survivor is carried forward as
  # `stale` -- an accepted row the tool no longer stands behind, still live on the
  # dashboard, which nothing used to say out loud.
  kept <- if (identical(tx_state, "written")) dirname(tx_written) else character(0)
  stale <- character(0)
  for (d in setdiff(c(tx_dir, rev_dir), kept)) {
    old <- file.path(d, paste0(key, ".csv"))
    if (file.exists(old) && !.removed(old)) stale <- c(stale, old)
  }

  # The gate's verdict AS RECORDED. A write that failed is still reported: the
  # conversion was accepted, the feed did not receive it.
  #
  # At most ONE suffix, most serious first, because everything downstream reads the
  # verdict as its last colon-segment (plain_feed in ui_labels.R, feed_health in
  # R/analytics.R). Most serious is stale_row_kept: a wrong figure that is still
  # being loaded outranks a right one that never arrived.
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

  # --- the manifest: one row per run (accepted AND withheld) -> the Qlik QA table --
  manifest <- data.frame(
    run_id = run_id, converted_ts_utc = ts,
    source_file = h$source_file %||% NA_character_, source_sha256 = sha,
    bank = h$bank %||% NA_character_, template_id = tid,
    template_origin = gate$origin, status = result$status %||% "failed",
    trust_level = result$trust$level %||% NA_character_, row_count = row_count,
    period_start = h$period_start %||% NA_character_, period_end = h$period_end %||% NA_character_,
    gate_result = gate_result,
    feed_file = if (!is.na(tx_written)) basename(tx_written) else NA_character_,
    # build provenance: which engine build + which template CONTENT produced these
    # figures, so a dashboard number can be reproduced years later.
    engine_version = engine_version(),
    template_sha256 = result$run_log$template_sha256 %||% NA_character_,
    stringsAsFactors = FALSE)
  # Key the manifest by the statement's CONTENT hash (like the transactions file),
  # not the per-run id: a re-convert then OVERWRITES its own manifest row instead
  # of adding another, so Qlik's coverage table counts each statement once (its
  # latest attempt), never N times for N re-runs. The run_id stays a column so the
  # latest attempt is still identifiable.
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

# ---- the feed folder's own retention -----------------------------------------
# archive_feed(config, keep_days, now) -> list(archived, kept).
#
# WHY: write_feed() writes one CSV per statement and NOTHING ever removed one,
# while docs/operational/connecting-qlik.md has Qlik load each folder with a
# wildcard. At 50 conversions a day that is about 18,000 files walked on every
# reload after a year and 55,000 after three -- a reload that gets slower every
# day, on a box nobody logs into, with no setting anywhere admitting the folder
# grows. It is the same fault as the uploads folder and it gets the same answer:
# a stated rule, generated from the setting that enforces it.
#
# IT MOVES, IT NEVER DELETES. Rows go to feed/archive/<folder>/, which the
# wildcard load does not reach, so the reload gets its year back and not one
# figure is lost -- a row wanted again is one folder away. That matters more here
# than anywhere: these are the figures a dashboard was built on, and this is
# evidence.
#
# Age comes from the file's own mtime, exactly as purge_uploads() takes it, and
# `now` is injectable for the same reason. Safe to run repeatedly.
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
      # A rename that fails leaves the row exactly where it is, still loaded --
      # visibly wrong rather than silently gone, which is the right way round.
      if (isTRUE(safe(file.rename(f, file.path(adir, basename(f))), FALSE)))
        out$archived <- out$archived + 1L
      else out$kept <- out$kept + 1L
    }
  }
  out
}

# feed_retention_note(keep_days) -- the ONE honest line about the feed folder,
# generated from the SAME setting archive_feed() enforces so the two can never
# drift, exactly as uploads_retention_note() does for saved statements.
feed_retention_note <- function(keep_days) {
  kd <- suppressWarnings(as.numeric(keep_days %||% NA)[1])
  if (!is.finite(kd) || kd <= 0)
    return("Every converted statement stays in the dashboard feed folder for good, so the reload gets slower as it fills.")
  sprintf(paste("Rows stay in the dashboard feed folder for %d day%s, then move to",
                "feed\\archive\\, where they are kept but no longer loaded."),
          as.integer(kd), if (as.integer(kd) == 1L) "" else "s")
}

# ---- retraction --------------------------------------------------------------
# retract_feed(run_id, config, ...) -> list(files, rows, moved).
#
# WHY: feedback used to be write-only. Beth could mark a conversion WRONG, the
# verdict was logged for maintenance -- and its rows kept feeding the dashboards
# for ever. The person best placed to know the figures are wrong had no way to
# stop them being used, which is the cardinal failure with a human already
# pointing at it.
#
# So a "wrong" verdict WITHDRAWS that run's rows from the accepted feed. It is
# deterministic (match on the run_id stamped in every row), logged (a feed record
# plus the manifest's own gate_result), reversible (rows are MOVED to
# feed/review, not deleted -- and a corrected re-convert restores them), and
# never silent. Withdrawal wins over keeping the review copy: if the review write
# fails the row still leaves the dashboard.
#
# `rows` is NA when at least one accepted file could NOT be withdrawn (see below).
# submit_feedback() passes that NA straight to the screen, whose existing wording
# for it -- "could not reach the feed folder to withdraw its rows, tell whoever
# looks after the server" -- is the true answer. A count is only ever returned for
# a withdrawal that really happened.
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
    # colClasses="character" for the same reason the feed is no longer built by
    # re-reading a CSV: type inference would rewrite the values on the way through.
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
    # The whole point of a retraction is that this file stops being loaded, and on
    # the documented Windows share it can refuse to go (open in the Qlik reload or
    # in Excel, or a create-but-not-delete permission). Verify it, because the
    # alternative is telling the one person who KNOWS the figures are wrong that
    # they have been withdrawn while they are still on the dashboard.
    if (!.removed(f)) {
      # Drop the review copy again. Leaving it would ADD a second, field-identical
      # set of rows for a statement whose accepted rows are still there -- turning
      # a withdrawal that failed into a dashboard that double-counts real money.
      # The attempt is not lost: it is in the feed log below, and the caller is
      # told the withdrawal did not happen.
      if (keep_review) safe(unlink(review_copy))
      stuck <- c(stuck, basename(f))
      next
    }
    # ...and say so in the manifest, so the Qlik coverage table shows the
    # statement as retracted rather than silently losing its accepted row.
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
  # A number that cannot be stood behind is not returned. Some rows may well have
  # gone, but while any accepted file is still there the honest answer to "was this
  # withdrawn" is "no", and NA is what the caller renders as exactly that.
  if (length(stuck)) out$rows <- NA_integer_
  out
}
