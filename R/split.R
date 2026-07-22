# split.R -- opt-in, DETERMINISTIC auto-split of a bundled upload into its
# constituent statements, each parsed and reconciled INDEPENDENTLY, with trust
# rolled up to the weakest segment.
#
# THE CHARTER RULE. A wrongly-placed boundary is itself a silently-wrong outcome,
# so auto-split runs ONLY when:
#   1. a template OPTS IN (a `split:` block), and
#   2. the boundaries are located by a DECLARED deterministic page marker (not a
#      guess -- a "Page 1 of N" reset, or a repeated opening-balance label), and
#   3. the number of statements is INDEPENDENTLY CONFIRMED -- a DIFFERENT structural
#      count (distinct periods, page-1 resets, or repeated opening/closing blocks)
#      agrees on the same count. This is necessary and is checked before any work.
# Per-segment reconciliation is reported as added confidence but is NOT a substitute
# for the count check: a running-balance column is continuous across ANY cut, so a
# wrongly-placed boundary would still reconcile within each piece -- reconciliation
# alone cannot prove a boundary is right, only an independent count can.
# When any of these is not satisfied the engine keeps its safe default: flag the
# bundle and refuse the merged parse (needs_review). Hidden values are never
# guessed; nothing is invented to make a segment reconcile.
#
# Scope: PDF bundles (the format whose boundaries -- page-number resets, repeated
# header blocks -- are deterministically locatable). A delimited/Excel export is
# almost always a single account/period; bundle-split for those is future work and
# falls through to flag-and-refuse today.

.SPLIT_SIGNALS <- c("page1_marker", "opening_label")

# .split_spec(template) -- normalise the opt-in `split:` block to a spec, or NULL
# when the template does not opt in. Accepts `split: true` (defaults) or a block
# with `on` / `min_statements`.
.split_spec <- function(template) {
  s <- template$split
  if (is.null(s) || isFALSE(s)) return(NULL)
  if (isTRUE(s)) s <- list()
  on <- tolower(as.character(s$on %||% "page1_marker"))
  on <- on[on %in% .SPLIT_SIGNALS]
  if (!length(on)) on <- "page1_marker"
  list(
    on             = on[1],
    min_statements = max(2L, suppressWarnings(as.integer(s$min_statements %||% 2L))))
}

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

# .segment_starts(input, spec) -- the 1-based page indices where each statement
# STARTS, located deterministically from the declared marker. Always includes
# page 1 (leading pages belong to the first statement). NULL when the signal is
# unavailable (e.g. not a PDF).
.segment_starts <- function(input, spec) {
  if (!identical(input$kind %||% "", "pdf")) return(NULL)
  txt <- .page_texts(input)
  if (!length(txt)) return(NULL)
  hit <- if (identical(spec$on, "page1_marker")) {
    grepl(.PAGE1_MARKER_RX, txt)             # SAME pattern extract_metadata counts, so they agree
  } else {                                   # opening_label: an opening-balance header per statement
    pat <- paste(c("opening balance", "balance brought forward"), collapse = "|")
    grepl(pat, txt, ignore.case = TRUE)
  }
  starts <- sort(unique(c(1L, which(hit))))
  starts
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
  m <- input$meta %||% list()
  m$page_count <- length(pages)
  ocr <- input$page_ocr %||% logical(0)
  if (length(ocr)) m$ocr_pages <- sum(as.logical(ocr[pages]), na.rm = TRUE)
  # per-page OCR confidence isn't retained on the input; carry the whole-document
  # minimum (conservative -- the OCR caveat can only over-warn, never under-warn).
  if (!is.null(m$redactions) && is.data.frame(m$redactions) && "page" %in% names(m$redactions)) {
    rd <- m$redactions[m$redactions$page %in% pages, , drop = FALSE]
    if (nrow(rd)) rd$page <- match(rd$page, pages)   # renumber to the segment's frame
    m$redactions <- rd
  }
  sub$meta <- m
  sub
}

# .trust_rank(level) -- order trust so the weakest segment can be found.
.trust_rank <- function(level) match(level %||% "low", c("low", "medium", "high"))

# .count_agrees(k, meta, on) -- does an INDEPENDENT structural count (one the split
# signal did not itself produce) agree that there are k statements? This is the
# corroboration that guards against a marker that legitimately repeats inside one
# statement. It is a NECESSARY condition to commit a split: per-segment
# reconciliation is NOT sufficient on its own, because a running-balance column is
# continuous across any cut, so a wrongly-placed boundary would still "reconcile"
# within each piece. Only an independent count can confirm the number of statements.
.count_agrees <- function(k, meta, on) {
  counts <- integer(0)
  if (!identical(on, "page1_marker")) counts <- c(counts, meta$page1_markers %||% NA)
  counts <- c(counts, meta$n_periods %||% NA)
  op <- meta$n_opening_labels %||% NA; cl <- meta$n_closing_labels %||% NA
  if (identical(on, "page1_marker") && isTRUE(op > 1) && isTRUE(cl > 1))
    counts <- c(counts, min(op, cl))
  counts <- counts[!is.na(counts) & counts > 1]
  length(counts) > 0 && any(counts == k)
}

# split_bundle(input, template, meta) -> a COMBINED result
#   list(parsed, recon, statements, n_statements, on)
# or NULL when it is not safe to split (caller then flag-and-refuses). `parsed` and
# `recon` are shaped exactly like the single-statement path, so the rest of the
# pipeline (outputs, diagnostics, coverage) is unchanged -- except transactions
# carry a `statement_index` column and trust is the weakest segment's.
split_bundle <- function(input, template, meta = NULL) {
  spec <- .split_spec(template)
  if (is.null(spec)) return(NULL)
  if (is.null(meta)) meta <- extract_metadata(input)

  starts <- .segment_starts(input, spec)
  npages <- length(input$pages %||% input$words %||% list())
  if (is.null(starts) || length(starts) < spec$min_statements || !npages) return(NULL)

  # page ranges: [start_i .. start_{i+1}-1], last runs to the final page.
  ends <- c(starts[-1] - 1L, npages)
  ranges <- Map(function(a, b) seq.int(a, b), starts, ends)
  k <- length(ranges)
  if (k < spec$min_statements) return(NULL)

  # COMMIT GATE (checked BEFORE the work): the segment count must be confirmed by an
  # INDEPENDENT structural signal. A running balance is continuous across any cut, so
  # per-segment reconciliation cannot prove a boundary is right -- only an independent
  # count can. Unconfirmed -> refuse, and the safe flag-and-refuse default takes over.
  if (!.count_agrees(k, meta, spec$on)) return(NULL)

  # Parse + reconcile each segment INDEPENDENTLY.
  segs <- lapply(seq_len(k), function(i) {
    si <- .subinput_pages(input, ranges[[i]])
    p  <- safe(parse_statement(si, template), NULL)
    if (is.null(p) || is.null(p$transactions) || !nrow(p$transactions)) return(NULL)
    r  <- safe(reconcile(p, template), NULL)
    if (is.null(r)) return(NULL)
    list(parsed = p, recon = r, pages = ranges[[i]])
  })
  if (any(vapply(segs, is.null, logical(1)))) return(NULL)   # a segment wouldn't parse -> refuse

  # ---- combine transactions (tagged with the statement they came from) ----
  txs <- lapply(seq_len(k), function(i) {
    t <- segs[[i]]$parsed$transactions
    t$statement_index <- i
    t
  })
  combined_tx <- do.call(rbind, txs)
  combined_tx$row_id <- seq_len(nrow(combined_tx))
  rownames(combined_tx) <- NULL
  extras <- lapply(segs, function(s) s$parsed$extras)
  combined_extras <- if (all(vapply(extras, function(e) !is.null(e) && ncol(e) > 0, logical(1))))
    safe(do.call(rbind, extras), NULL) else NULL
  # renumber the extras join key to match the recombined transactions (each segment
  # had its own 1..n row_id) so the JSON extras<->transactions join stays valid.
  if (!is.null(combined_extras) && "row_id" %in% names(combined_extras) && nrow(combined_extras))
    combined_extras$row_id <- seq_len(nrow(combined_extras))

  # ---- per-statement summary (period / balances / trust, per segment) ----
  statements <- lapply(seq_len(k), function(i) {
    h <- segs[[i]]$parsed$header; tr <- segs[[i]]$recon$trust
    list(index = i, pages = sprintf("%d-%d", min(segs[[i]]$pages), max(segs[[i]]$pages)),
         period_start = h$period_start, period_end = h$period_end,
         opening_balance = h$opening_balance, closing_balance = h$closing_balance,
         account_hash = NA_character_, rows = nrow(segs[[i]]$parsed$transactions),
         trust_level = tr$level, trust_score = tr$score)
  })

  # ---- combined header (summary; per-statement anchors live in `statements`) ----
  # Per-statement IDENTITY fields (account, balances, count) are nulled here: they
  # differ per statement, and the feed stamps header fields onto EVERY row, so a
  # single value would mislabel other statements' rows. The truth is in `statements`
  # (and each row's statement_index). The period is kept as the bundle's honest span.
  header <- segs[[1]]$parsed$header          # period_start inherited from statement 1
  header$row_count       <- nrow(combined_tx)
  header$n_statements    <- k
  header$page_count      <- npages
  header$period_end      <- segs[[k]]$parsed$header$period_end   # ...to statement k's end
  header$account_number  <- NA_character_   # differs per statement -> not one value
  header$opening_balance <- NA_real_        # per-segment in `statements`
  header$closing_balance <- NA_real_
  header$stated_count    <- NA_integer_

  combined_parsed <- list(
    transactions = combined_tx, extras = combined_extras, header = header,
    provenance = do.call(rbind, lapply(segs, function(s) s$parsed$provenance)),
    statements = statements, n_statements = k,
    source_line_count = NA_integer_, multiline_extra = 0L)
  # keep row_id provenance aligned to the recombined rows
  if (!is.null(combined_parsed$provenance))
    combined_parsed$provenance$row_id <- seq_len(nrow(combined_parsed$provenance))

  # ---- combined KPIs: every segment's checks, stacked and statement-tagged ----
  kpis <- do.call(rbind, lapply(seq_len(k), function(i) {
    ki <- segs[[i]]$recon$kpis
    ki$name <- sprintf("%s [statement %d]", ki$name, i)
    ki
  }))
  rownames(kpis) <- NULL

  # ---- roll trust up to the WEAKEST segment ----
  ranks   <- vapply(segs, function(s) .trust_rank(s$recon$trust$level), integer(1))
  level   <- c("low", "medium", "high")[min(ranks)]
  score   <- min(vapply(segs, function(s) s$recon$trust$score %||% 0, numeric(1)))
  weakest <- which(ranks == min(ranks))
  reasons <- c(
    sprintf("upload auto-split into %d statements at %s boundaries (pages %s); each reconciled independently",
            k, spec$on, paste(vapply(statements, function(s) s$pages, character(1)), collapse = ", ")),
    "statement count confirmed by an independent structural signal (period / page-1 / balance-block count)",
    sprintf("overall trust is the weakest statement's (statement%s %s): %s",
            if (length(weakest) > 1) "s" else "",
            paste(weakest, collapse = ", "), level))
  ocr_pages <- sum(vapply(segs, function(s) s$recon$trust$ocr_pages %||% 0L, integer(1)))
  combined_recon <- list(
    kpis = kpis,
    trust = list(level = level, score = score, reasons = reasons,
                 completeness_verified = all(vapply(segs,
                   function(s) isTRUE(s$recon$trust$completeness_verified), logical(1))),
                 ocr_pages = ocr_pages,
                 ocr_min_confidence = min(vapply(segs,
                   function(s) s$recon$trust$ocr_min_confidence %||% NA_real_, numeric(1)))))

  # A non-NULL return IS the "committed" signal; per-statement page ranges are in
  # `statements`, so no separate committed/boundaries fields are needed.
  list(parsed = combined_parsed, recon = combined_recon,
       statements = statements, n_statements = k, on = spec$on)
}
