# split.R -- DETERMINISTIC split of a bundled upload into its statements, each
# read and proven by the automatic reader ON ITS OWN, with the outcome rolled up
# to the weakest statement.
#
# THE RULE. A wrongly-placed boundary is itself a silently-wrong outcome, so a
# bundle is split ONLY when:
#   1. the boundaries are located by a deterministic page marker (a "Page 1 of N"
#      reset), never a guess, and
#   2. the number of statements is INDEPENDENTLY CONFIRMED -- a DIFFERENT structural
#      count (distinct periods, or repeated opening/closing blocks) agrees on the
#      same count. This is checked before any reading.
# Each statement's own proof is added confidence but is NOT a substitute for the
# count check: a running balance is continuous across ANY cut, so a wrongly-placed
# boundary would still add up within each piece. When the count is not confirmed
# the file is read whole: R/convert.R accepts that whole-file reading only when the
# arithmetic proves every row (statements of different accounts break the chain
# and go to a person), and otherwise sends it to a person with the advice to
# split the file.
#
# Scope: PDF bundles (the format whose boundaries -- page-number resets, repeated
# header blocks -- are deterministically locatable). A CSV or Excel export is
# almost always one account and period, and is read whole.

# .page_texts(input) -- one text string per page, COMBINING the word boxes and the
# page text layer, so a boundary marker (e.g. a "Page 1 of N" footer) is found
# whichever layer carries it.
.page_texts <- function(input) {
  wl <- input$words %||% list()
  pg <- input$pages %||% character(0)
  np <- max(length(wl), length(pg))
  if (!np) return(character(0))
  vapply(seq_len(np), function(i) {
    w  <- if (i <= length(wl)) wl[[i]] else NULL
    wt <- if (!is.null(w) && is.data.frame(w) && nrow(w)) paste(w$text, collapse = " ") else ""
    pt <- if (i <= length(pg)) as.character(pg[i] %||% "") else ""
    trimws(paste(wt, pt))
  }, character(1))
}

# .segment_starts(input) -- the 1-based page indices where each statement STARTS:
# every page printing "Page 1 of N" (the SAME pattern extract_metadata counts, so
# the two agree). Always includes page 1 (leading pages belong to the first
# statement). NULL when the signal is unavailable (not a PDF).
.segment_starts <- function(input) {
  if (!identical(input$kind %||% "", "pdf")) return(NULL)
  txt <- .page_texts(input)
  if (!length(txt)) return(NULL)
  sort(unique(c(1L, which(grepl(.PAGE1_MARKER_RX, txt)))))
}

# .subinput_pages(input, pages) -- a standalone PDF input restricted to `pages`,
# with every per-page field subset and the page counts / OCR figures recomputed so
# the segment parses exactly as if it had been uploaded on its own.
.subinput_pages <- function(input, pages) {
  sub <- input
  take <- function(x) if (is.null(x)) NULL else x[pages]
  sub$pages       <- take(input$pages)
  sub$words       <- if (is.null(input$words)) NULL else input$words[pages]
  sub$page_width  <- take(input$page_width)
  sub$page_height <- take(input$page_height)
  sub$page_ocr    <- take(input$page_ocr)
  # where each page sits in the file on disk, for anything that reads it again
  sub$page_map    <- (input$page_map %||% seq_along(input$pages %||% input$words))[pages]
  m <- input$meta %||% list()
  m$page_count <- length(pages)
  ocr <- input$page_ocr %||% logical(0)
  if (length(ocr)) m$ocr_pages <- sum(as.logical(ocr[pages]), na.rm = TRUE)
  # ocr_min_conf is deliberately NOT narrowed to this segment: the input keeps only
  # a whole-document minimum, so every segment inherits it. Conservative on purpose
  # -- the OCR caveat can then only over-warn, never under-warn.
  sub$meta <- m
  sub
}

# .trust_rank(level) -- order trust so the weakest segment can be found.
.trust_rank <- function(level) match(level %||% "low", c("low", "medium", "high"))

# .bundle_period(statements) -> list(start, end, why)
#   start/end : the bundle's period as the statements PRINT it, or NA
#   why        : one plain sentence when it is NA, else NA
#
# THE BUG THIS REPLACES. The combined header took statement 1's start and
# statement k's end IN FILE ORDER. Bank bundles are routinely filed newest-first,
# so on samples/_private_staging/anz_0382004_multi.pdf that produced
# "20 Apr 2026 to 19 Feb 2026" -- a period that ENDS TWO MONTHS BEFORE IT STARTS --
# and it went out to the governed feed's run manifest as an `accepted` row. Nothing
# on screen, in the checks or in the feed noticed. Date order, not file order, is
# the only thing that can decide a span.
#
# SHOULD A BUNDLE PUBLISH ONE MERGED PERIOD AT ALL? Only when the parts JOIN UP.
# Here is the reasoning, because it is the interesting half of the fix:
#
# A period on a court-facing extract is a claim about COVERAGE -- "these figures
# are everything this account did between these two dates". For statements that
# meet end-to-start, min(start)..max(end) is exactly that claim and it is true.
# For statements that do NOT, it is false in the most dangerous direction: on the
# very sample above the three statements leave 18-19 Apr 2026 covered by no printed
# statement, so a merged span would silently assert two days of coverage the file
# does not have, and a transaction missing from that window could never be noticed.
# That is a plausible figure standing in for one nobody checked, which the charter
# calls the cardinal failure. An OVERLAP is worse still: overlapping sections are
# not one account's consecutive statements at all, so there is no single span to
# state. So: contiguous -> publish the true span; anything else -> publish nothing,
# and say why.
#
# Nothing is lost by publishing nothing. Every statement's own period is in
# `statements`, the feed stamps each transaction row with ITS OWN statement's
# period (.split_stamp, R/feed.R), and the card's split table shows them all. The
# empty field is the honest answer to a question the bundle does not answer.
#
# The rules are the ones .period_span (R/extract_metadata.R) already applies to
# several periods printed inside ONE statement, so the engine cannot hold two
# different opinions about when periods join up -- including that A SHARED
# BOUNDARY DAY IS NOT AN OVERLAP (banks print the previous closing date as the
# next opening date), and that one unreadable bound means the ORDER of the whole
# set is unknown, which is exactly when a guess produces a wrong answer.
.bundle_period <- function(statements) {
  none <- function(why) list(start = NA_character_, end = NA_character_, why = why)
  # Unreachable from split_bundle (it commits only with two statements or more),
  # but every `why` below can reach a screen, so none of them may be a code word.
  if (!length(statements))
    return(none("no statement period is stated for this file as a whole"))
  raw_s <- vapply(statements, function(s) as.character(s$period_start %||% NA)[1],
                  character(1), USE.NAMES = FALSE)
  raw_e <- vapply(statements, function(s) as.character(s$period_end   %||% NA)[1],
                  character(1), USE.NAMES = FALSE)
  ds <- do.call(c, lapply(raw_s, .tolerant_date))   # the SHARED tolerant parser
  de <- do.call(c, lapply(raw_e, .tolerant_date))   # (R/params.R), so one answer
  if (anyNA(ds) || anyNA(de))
    return(none(paste("the statements in this file do not all print a readable period, so they",
                      "could not be put in date order and no single period is stated for the",
                      "file as a whole - each statement's own period is listed above")))
  if (any(de < ds))
    return(none(paste("at least one statement in this file prints a period that ends before it",
                      "starts, so no single period is stated for the file as a whole - check",
                      "that statement against the source")))
  o  <- order(ds, de)
  ds <- ds[o]; de <- de[o]
  # Join up: each statement starts on, or the day after, the one before it ends.
  # `<` is an overlap; `> +1` is a gap. Both mean this is not one continuous span,
  # and they are told apart because they are different faults with different cures:
  # a gap is days nobody has a statement for (ask the bank for the missing one), an
  # overlap means these are not one account's consecutive statements at all.
  if (length(ds) > 1) {
    nxt <- ds[-1]; prv <- de[-length(de)]
    if (any(nxt < prv))
      return(none(paste("the statements in this file overlap in time, so they are not one",
                        "account's consecutive statements and no single period is stated for",
                        "the file as a whole - each statement's own period is listed above")))
    hole <- which(nxt > prv + 1)
    if (length(hole))
      return(none(sprintf(paste("no statement in this file covers %s, so no single period is",
                                "stated for the file as a whole - if this account should be",
                                "covered end to end, a statement is missing"),
                          paste(sprintf("%s to %s", format(prv[hole] + 1), format(nxt[hole] - 1)),
                                collapse = "; "))))
  }
  # Verbatim, as the statements print it -- never a reformatted date.
  list(start = raw_s[o][1], end = raw_e[o][length(o)], why = NA_character_)
}

# .count_agrees(k, meta) -- does an INDEPENDENT structural count (one the page
# markers did not themselves produce) agree that there are k statements? This is
# the corroboration that guards against a marker that legitimately repeats inside
# one statement, and a NECESSARY condition to split: a running balance is
# continuous across any cut, so a wrongly-placed boundary would still add up
# within each piece. Only an independent count can confirm the number.
.count_agrees <- function(k, meta) {
  counts <- meta$n_periods %||% NA
  op <- meta$n_opening_labels %||% NA; cl <- meta$n_closing_labels %||% NA
  # Blocks that all state the same two balances are one statement's, printed twice
  # (a loan summary box and the table's own lines): they count once.
  if (isTRUE(op > 1) && isTRUE(cl > 1)) counts <- c(counts, meta$n_balance_blocks %||% min(op, cl))
  counts <- counts[!is.na(counts) & counts > 1]
  length(counts) > 0 && any(counts == k)
}

# bundle_segments(input, meta) -> the page numbers of each statement in a bundle
# (a list, two or more), or NULL when the file is not to be split: one statement,
# not a PDF, or a count no independent signal confirms.
bundle_segments <- function(input, meta = NULL) {
  if (is.null(meta)) meta <- extract_metadata(input)
  starts <- .segment_starts(input)
  npages <- length(input$pages %||% input$words %||% list())
  if (is.null(starts) || length(starts) < 2L || !npages) return(NULL)
  ends <- c(starts[-1] - 1L, npages)
  if (!.count_agrees(length(starts), meta)) return(NULL)
  Map(function(a, b) seq.int(a, b), starts, ends)
}

# bundle_combine(readings, ranges, npages) -> list(parsed, recon, statements,
# n_statements) -- each statement's own reading (auto_read on its pages) put
# together in the shapes the single-statement path uses, so outputs, diagnostics
# and the feed are unchanged -- except transactions carry `statement_index` and
# trust is the weakest statement's. A statement that read nothing keeps its place
# in `statements` with no rows, so nothing about it goes unsaid.
bundle_combine <- function(readings, ranges, npages) {
  k <- length(readings)
  have <- which(vapply(readings, function(r) !is.null(r$parsed) && nrow(r$transactions) > 0L, logical(1)))
  txs <- lapply(have, function(i) {
    t <- readings[[i]]$parsed$transactions
    t$statement_index <- rep(i, nrow(t))
    t
  })
  combined_tx <- if (length(txs)) do.call(rbind, txs) else NULL
  if (!is.null(combined_tx)) {
    combined_tx$row_id <- seq_len(nrow(combined_tx))
    rownames(combined_tx) <- NULL
  }
  extras <- lapply(have, function(i) readings[[i]]$parsed$extras)
  combined_extras <- if (length(extras) && all(vapply(extras, function(e) !is.null(e) && ncol(e) > 0, logical(1))))
    safe(do.call(rbind, extras), NULL) else NULL
  # renumber the extras join key to match the recombined transactions (each
  # statement had its own 1..n row_id) so the JSON extras<->transactions join holds.
  if (!is.null(combined_extras) && "row_id" %in% names(combined_extras) && nrow(combined_extras))
    combined_extras$row_id <- seq_len(nrow(combined_extras))

  statements <- lapply(seq_len(k), function(i) {
    r <- readings[[i]]
    h <- r$parsed$header %||% list()
    list(index = i, pages = sprintf("%d-%d", min(ranges[[i]]), max(ranges[[i]])),
         period_start = h$period_start %||% NA_character_, period_end = h$period_end %||% NA_character_,
         opening_balance = h$opening_balance %||% NA_real_, closing_balance = h$closing_balance %||% NA_real_,
         account_hash = NA_character_, account_number = h$account_number %||% NA_character_,
         account_name = h$account_name %||% NA_character_, rows = nrow(r$transactions %||% data.frame()),
         outcome = r$outcome %||% "unread", why = r$why %||% NA_character_,
         layout = r$matched_layout %||% NA_character_, recipe = r$matched_recipe %||% NA_character_,
         trust_level = r$recon$trust$level %||% "low", trust_score = r$recon$trust$score %||% 0)
  })

  # Per-statement IDENTITY fields (account, balances, count) are emptied in the
  # combined header: the feed stamps header fields onto EVERY row, so one value
  # would mislabel other statements' rows. The truth is in `statements` (and each
  # row's statement_index). The PERIOD is published only when the statements join
  # up into one continuous span (.bundle_period).
  period <- .bundle_period(statements)
  header <- if (length(have)) readings[[have[1]]]$parsed$header else list()
  header$row_count       <- if (is.null(combined_tx)) 0L else nrow(combined_tx)
  header$n_statements    <- k
  header$page_count      <- npages
  header$period_start    <- period$start
  header$period_end      <- period$end
  header$account_number  <- NA_character_
  header$account_name    <- NA_character_
  header$opening_balance <- NA_real_
  header$closing_balance <- NA_real_
  header$stated_count    <- NA_integer_
  # A period that runs backwards is proof this code is broken, not a value to
  # report: refused here, and the caller's funnel turns it into a failed run.
  if (.period_runs_backwards(header$period_start, header$period_end))
    stop("internal: the bundle period was built backwards - refusing to publish it", call. = FALSE)

  prov <- lapply(have, function(i) readings[[i]]$parsed$provenance)
  combined_parsed <- list(
    transactions = combined_tx, extras = combined_extras, header = header,
    provenance = if (length(prov)) do.call(rbind, prov) else NULL,
    statements = statements, n_statements = k,
    source_line_count = NA_integer_, multiline_extra = 0L)
  if (!is.null(combined_parsed$provenance))
    combined_parsed$provenance$row_id <- seq_len(nrow(combined_parsed$provenance))

  # Every statement's checks, stacked and statement-tagged.
  kp <- lapply(seq_len(k), function(i) {
    ki <- readings[[i]]$recon$kpis
    if (is.null(ki) || !nrow(ki)) return(NULL)
    ki$name <- sprintf("%s [statement %d]", ki$name, i)
    ki
  })
  kp <- Filter(Negate(is.null), kp)
  kpis <- if (length(kp)) do.call(rbind, kp) else NULL
  if (!is.null(kpis)) rownames(kpis) <- NULL

  ranks   <- vapply(statements, function(s) .trust_rank(s$trust_level), integer(1))
  level   <- c("low", "medium", "high")[min(ranks)]
  weakest <- which(ranks == min(ranks))
  reasons <- c(
    sprintf("split into %d statements at their \"Page 1\" pages (pages %s); each read and proven on its own",
            k, paste(vapply(statements, function(s) s$pages, character(1)), collapse = ", ")),
    "statement count confirmed by an independent structural signal (period / balance-block count)",
    if (!is.na(period$why)) period$why,
    sprintf("overall trust is the weakest statement's (statement%s %s): %s",
            if (length(weakest) > 1) "s" else "", paste(weakest, collapse = ", "), level))
  rc <- lapply(readings, function(r) r$recon$trust)
  combined_recon <- list(
    kpis = kpis,
    trust = list(level = level,
                 score = min(vapply(statements, function(s) as.numeric(s$trust_score), numeric(1))),
                 reasons = reasons,
                 completeness_verified = all(vapply(rc, function(t) isTRUE(t$completeness_verified), logical(1))),
                 ocr_pages = sum(vapply(rc, function(t) as.integer(t$ocr_pages %||% 0L), integer(1))),
                 ocr_min_confidence = suppressWarnings(min(vapply(rc, function(t) as.numeric(t$ocr_min_confidence %||% NA_real_), numeric(1))))))
  list(parsed = combined_parsed, recon = combined_recon, statements = statements, n_statements = k)
}
