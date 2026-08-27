# split.R -- opt-in, DETERMINISTIC auto-split of a bundled PDF into its constituent statements, each
# parsed and reconciled independently, with trust rolled up to the weakest segment. A wrongly-placed
# boundary is itself a silently-wrong outcome, so it runs ONLY when the boundaries come from a DECLARED
# deterministic page marker AND the number of statements is INDEPENDENTLY CONFIRMED by a different
# structural count - per-segment reconciliation is not a substitute, because a running balance is
# continuous across ANY cut. Otherwise the safe default stands: flag the bundle, refuse the parse.

.SPLIT_SIGNALS <- c("page1_marker", "opening_label")

# The split settings for a template; only an explicit `split: false` turns it off. It used to be
# opt-in, and the opt-in was the wrong lock: one of thirteen shipped templates declared a `split:`
# block, so a bundle read by any of the other twelve fell through to the merged parse. The commit gate
# in split_bundle() is what makes splitting safe, and defaulting on changes no existing outcome.
.split_spec <- function(template) {
  s <- template$split %||% TRUE
  if (isFALSE(s)) return(NULL)
  if (isTRUE(s)) s <- list()
  on <- tolower(as.character(s$on %||% "page1_marker"))
  on <- on[on %in% .SPLIT_SIGNALS]
  if (!length(on)) on <- "page1_marker"
  list(
    on             = on[1],
    min_statements = max(2L, suppressWarnings(as.integer(s$min_statements %||% 2L))))
}

# One text string per page, COMBINING the word boxes and the page text layer, so a boundary marker
# is found whichever layer carries it.
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

# The 1-based page indices where each statement STARTS. Always includes page 1; NULL when the
# signal is unavailable.
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

# A standalone PDF input restricted to `pages`, so the segment parses exactly as if uploaded alone.
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
  # ocr_min_conf is deliberately NOT narrowed to this segment: every segment inherits the whole-
  # document minimum, so the OCR caveat can only over-warn, never under-warn.
  if (!is.null(m$redactions) && is.data.frame(m$redactions) && "page" %in% names(m$redactions)) {
    rd <- m$redactions[m$redactions$page %in% pages, , drop = FALSE]
    if (nrow(rd)) rd$page <- match(rd$page, pages)   # renumber to the segment's frame
    m$redactions <- rd
  }
  sub$meta <- m
  sub
}

# Order trust so the weakest segment can be found.
.trust_rank <- function(level) match(level %||% "low", c("low", "medium", "high"))

# The bundle's period as the statements PRINT it, or NA with one plain sentence saying why. The combined
# header used to take statement 1's start and statement k's end IN FILE ORDER, and bank bundles are
# routinely filed newest-first, so one sample produced "20 Apr 2026 to 19 Feb 2026" and it went to the
# governed feed accepted. Date order, not file order, decides a span. A merged period is published only
# when the parts JOIN UP: a period on a court-facing extract is a claim about COVERAGE, and a gap makes
# it false in the most dangerous direction while an OVERLAP means these are not one account's
# consecutive statements. Nothing is lost by publishing nothing - each statement's own period is kept.
.bundle_period <- function(statements) {
  none <- function(why) list(start = NA_character_, end = NA_character_, why = why)
  # Unreachable from split_bundle, which commits only with two statements or more, but every `why`
  # below can reach a screen, so none may be a code word.
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
  # Join up: each statement starts on, or the day after, the one before it ends. `<` is an overlap,
  # `> +1` is a gap, told apart because they have different cures.
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
  # Verbatim, as the statements print it - never a reformatted date.
  list(start = raw_s[o][1], end = raw_e[o][length(o)], why = NA_character_)
}

# Does an INDEPENDENT structural count agree that there are k statements? This guards against a
# marker that legitimately repeats inside one statement, and it is a NECESSARY condition to commit:
# per-segment reconciliation is not sufficient, because a running balance is continuous across any cut.
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

# Returns a COMBINED result list(parsed, recon, statements, n_statements, on), or NULL when it is not
# safe to split. Shaped exactly like the single-statement path, except transactions carry a
# `statement_index` column and trust is the weakest segment's.
split_bundle <- function(input, template, meta = NULL) {
  spec <- .split_spec(template)
  if (is.null(spec)) return(NULL)
  if (is.null(meta)) meta <- extract_metadata(input)

  starts <- .segment_starts(input, spec)
  npages <- length(input$pages %||% input$words %||% list())
  if (is.null(starts) || length(starts) < spec$min_statements || !npages) return(NULL)

  # Page ranges: [start_i .. start_{i+1}-1], last runs to the final page.
  ends <- c(starts[-1] - 1L, npages)
  ranges <- Map(function(a, b) seq.int(a, b), starts, ends)
  k <- length(ranges)
  if (k < spec$min_statements) return(NULL)

  # COMMIT GATE, checked BEFORE the work: the segment count must be confirmed by an INDEPENDENT
  # structural signal. Unconfirmed means refuse, and flag-and-refuse takes over.
  if (!.count_agrees(k, meta, spec$on)) return(NULL)

  # Parse and reconcile each segment INDEPENDENTLY.
  segs <- lapply(seq_len(k), function(i) {
    si <- .subinput_pages(input, ranges[[i]])
    p  <- safe(parse_statement(si, template), NULL)
    if (is.null(p) || is.null(p$transactions) || !nrow(p$transactions)) return(NULL)
    r  <- safe(reconcile(p, template), NULL)
    if (is.null(r)) return(NULL)
    list(parsed = p, recon = r, pages = ranges[[i]])
  })
  if (any(vapply(segs, is.null, logical(1)))) return(NULL)   # a segment wouldn't parse -> refuse

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
  # Renumber the extras join key to match the recombined transactions - each segment had its own
  # 1..n row_id - so the JSON extras-to-transactions join stays valid.
  if (!is.null(combined_extras) && "row_id" %in% names(combined_extras) && nrow(combined_extras))
    combined_extras$row_id <- seq_len(nrow(combined_extras))

  statements <- lapply(seq_len(k), function(i) {
    h <- segs[[i]]$parsed$header; tr <- segs[[i]]$recon$trust
    list(index = i, pages = sprintf("%d-%d", min(segs[[i]]$pages), max(segs[[i]]$pages)),
         period_start = h$period_start, period_end = h$period_end,
         opening_balance = h$opening_balance, closing_balance = h$closing_balance,
         account_hash = NA_character_, rows = nrow(segs[[i]]$parsed$transactions),
         trust_level = tr$level, trust_score = tr$score)
  })

  # Per-statement IDENTITY fields (account, balances, count) are nulled here: they differ per
  # statement, and the feed stamps header fields onto EVERY row. The PERIOD is one of those fields,
  # published only when the statements join up into one continuous span.
  period <- .bundle_period(statements)
  header <- segs[[1]]$parsed$header
  header$row_count       <- nrow(combined_tx)
  header$n_statements    <- k
  header$page_count      <- npages
  header$period_start    <- period$start    # DATE order, and only if they join up
  header$period_end      <- period$end
  header$account_number  <- NA_character_   # differs per statement -> not one value
  header$opening_balance <- NA_real_        # per-segment in `statements`
  header$closing_balance <- NA_real_
  header$stated_count    <- NA_integer_
  # THE LAST GATE, and the reason a backwards period cannot come back silently. A period that runs
  # backwards is not a value to report, it is proof this function is broken, so it is refused here:
  # split_bundle is called through safe(), so the refusal drops the run onto flag-and-refuse.
  if (.period_runs_backwards(header$period_start, header$period_end))
    stop("internal: the bundle period was built backwards - refusing to publish it",
         call. = FALSE)

  combined_parsed <- list(
    transactions = combined_tx, extras = combined_extras, header = header,
    provenance = do.call(rbind, lapply(segs, function(s) s$parsed$provenance)),
    statements = statements, n_statements = k,
    source_line_count = NA_integer_, multiline_extra = 0L)
  # Keep row_id provenance aligned to the recombined rows.
  if (!is.null(combined_parsed$provenance))
    combined_parsed$provenance$row_id <- seq_len(nrow(combined_parsed$provenance))

  kpis <- do.call(rbind, lapply(seq_len(k), function(i) {
    ki <- segs[[i]]$recon$kpis
    ki$name <- sprintf("%s [statement %d]", ki$name, i)
    ki
  }))
  rownames(kpis) <- NULL

  # Roll trust up to the WEAKEST segment.
  ranks   <- vapply(segs, function(s) .trust_rank(s$recon$trust$level), integer(1))
  level   <- c("low", "medium", "high")[min(ranks)]
  score   <- min(vapply(segs, function(s) s$recon$trust$score %||% 0, numeric(1)))
  weakest <- which(ranks == min(ranks))
  reasons <- c(
    sprintf("upload auto-split into %d statements at %s boundaries (pages %s); each reconciled independently",
            k, spec$on, paste(vapply(statements, function(s) s$pages, character(1)), collapse = ", ")),
    "statement count confirmed by an independent structural signal (period / page-1 / balance-block count)",
    # Why the file has no one period, on the same screen as the split. An empty field with no
    # explanation reads as something the tool forgot to fill in.
    if (!is.na(period$why)) period$why,
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

  # A non-NULL return IS the "committed" signal; per-statement page ranges are in `statements`.
  list(parsed = combined_parsed, recon = combined_recon,
       statements = statements, n_statements = k, on = spec$on)
}
