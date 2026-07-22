# wizard_auto.R -- "generate as much of the wizard as possible" helpers, used by
# the Admin panel to turn an unsupported statement into a starting template so a
# maintainer's job is confirm-and-save, not build-from-nothing.
#
# Delimited/Excel drafting reuses the existing wizard_detect heuristics (delimiter,
# date format, amount style, column mapping). This file adds the PDF piece:
# SUGGEST column boxes by clustering the word positions on the transaction page.

# .money_rx / date-ish helpers (kept local + generic).
.WA_MONEY <- "^-?\\$?[0-9][0-9,]*\\.[0-9]{2}$"
.WA_DATE  <- "^[0-9]{1,2}([/. -][A-Za-z0-9]{2,9}([/. -][0-9]{2,4})?)?$"

# .fp_specific(ph) -- is a fingerprint phrase DISTINCTIVE enough to identify a
# bank layout (>=2 words, or a single long/branded token like "Debit/Withdrawal"),
# rather than a generic single word ("Balance") that sits on nearly every
# statement? Shared by the drafter and validate_template so they agree.
.fp_specific <- function(ph) {
  ph <- trimws(as.character(ph))
  nzchar(ph) & ((lengths(strsplit(ph, "\\s+")) >= 2) | (nchar(ph) >= 10))
}

# header_phrases(input, n) -> up to n DISTINCTIVE phrases present on the page, for
# use as a PDF template fingerprint (page_contains_all). Prefers MULTI-WORD
# phrases -- a branded title / statement heading, then the transaction table's
# header line -- because single generic words ("Balance","Amount") match almost
# any statement and turn a correct "unsupported" verdict into a wrong match. Falls
# back to the column-label keywords only as secondary anchors.
header_phrases <- function(input, n = 3) {
  txt <- paste(input$pages %||% character(0), collapse = "\n")
  lines <- trimws(unlist(strsplit(txt, "\n", fixed = TRUE)))
  lines <- gsub("\\s+", " ", lines[nzchar(lines)])
  if (!length(lines)) return(character(0))
  out <- character(0)
  # 1. a distinctive TITLE/brand line near the top: multi-word, no digits (not a
  # data row), not just column labels -- e.g. "Your transactions", a bank name.
  top <- utils::head(lines, 15L)
  titleish <- vapply(top, function(ln) {
    w <- unlist(regmatches(ln, gregexpr("[A-Za-z]+", ln)))
    nchar(ln) >= 10 && nchar(ln) <= 60 && length(w) >= 2 && !grepl("[0-9]", ln) &&
      (length(w) == 0 || mean(tolower(w) %in% .HDR_KEYS) < 0.5)
  }, logical(1))
  out <- c(out, utils::head(top[titleish], 2L))
  # 2. the transaction table's header LINE as a whole multi-word phrase (the column
  # labels together are far more specific than any one of them alone).
  score <- vapply(lines, function(ln) {
    w <- tolower(unlist(regmatches(ln, gregexpr("[A-Za-z]+", ln))))
    length(unique(w[w %in% .HDR_KEYS]))
  }, integer(1))
  best <- which.max(score)
  if (length(best) && score[best] >= 2 && nchar(lines[best]) <= 60)
    out <- c(out, lines[best])
  # 3. secondary anchors: individual column-label words (kept for continuity, but
  # after the distinctive phrases so a good fingerprint leads).
  if (length(best) && score[best] >= 1) {
    words <- unlist(regmatches(lines[best], gregexpr("[A-Za-z]+", lines[best])))
    out <- c(out, words[tolower(words) %in% .HDR_KEYS])
  }
  utils::head(unique(out[nzchar(out)]), n)
}

# .cluster_1d(x, gap) -- group sorted values where consecutive gaps exceed `gap`.
.cluster_1d <- function(x, gap) {
  if (!length(x)) return(integer(0))
  o <- order(x); xs <- x[o]
  g <- cumsum(c(TRUE, diff(xs) > gap))
  grp <- integer(length(x)); grp[o] <- g; grp
}

# suggest_pdf_columns(input, gap) -> data.frame(field, x_min, x_max, kind).
# Heuristic starting point ONLY -- the analyst confirms/redraws in the wizard.
# Picks the page with the most money tokens (the transaction table), clusters
# word x-centres into columns, and labels them: leftmost date-ish column = date;
# money columns left-to-right = the amount column(s), rightmost money column =
# balance; the span between date and the first money column = description.
suggest_pdf_columns <- function(input, gap = 18) {
  wl <- input$words %||% list()
  if (!length(wl)) return(data.frame())
  cnt <- vapply(wl, function(w) if (is.null(w) || !nrow(w)) 0L else
    sum(grepl(.WA_MONEY, w$text)), integer(1))
  pg <- which.max(cnt); if (!length(pg) || cnt[pg] == 0) return(data.frame())
  w <- as.data.frame(wl[[pg]], stringsAsFactors = FALSE)
  w$cx <- w$x + w$width / 2
  is_money <- grepl(.WA_MONEY, w$text)
  is_date  <- grepl(.WA_DATE, w$text) & grepl("[0-9]", w$text) & !is_money

  cols <- list()
  add <- function(field, sel, kind) {
    if (!any(sel)) return()
    cols[[length(cols) + 1L]] <<- data.frame(field = field,
      x_min = floor(min(w$x[sel])) - 3, x_max = ceiling(max(w$x[sel] + w$width[sel])) + 3,
      kind = kind, stringsAsFactors = FALSE)
  }
  if (any(is_money)) {
    mg <- .cluster_1d(w$cx[is_money], gap)
    centres <- tapply(w$cx[is_money], mg, mean)
    order_grp <- as.integer(names(sort(centres)))
    nmon <- length(order_grp)
    labels <- if (nmon >= 3) c(rep("amount", nmon - 1), "balance")
              else if (nmon == 2) c("amount", "balance") else "amount"
    idxmon <- which(is_money)
    for (k in seq_along(order_grp)) {
      selk <- rep(FALSE, nrow(w)); selk[idxmon[mg == order_grp[k]]] <- TRUE
      add(labels[k], selk, "money")
    }
  }
  if (any(is_date)) {
    dg <- .cluster_1d(w$cx[is_date], gap); idxd <- which(is_date)
    leftmost <- dg[which.min(w$cx[is_date])]
    seld <- rep(FALSE, nrow(w)); seld[idxd[dg == leftmost]] <- TRUE
    add("date", seld, "date")
  }
  if (!length(cols)) return(data.frame())
  out <- do.call(rbind, cols)
  drow <- out[out$field == "date", , drop = FALSE]
  mrows <- out[out$kind == "money", , drop = FALSE]
  if (nrow(drow) && nrow(mrows)) {
    out <- rbind(out, data.frame(field = "description",
      x_min = drow$x_max[1] + 1, x_max = min(mrows$x_min) - 1, kind = "text",
      stringsAsFactors = FALSE))
  }
  out <- out[order(out$x_min), , drop = FALSE]
  rownames(out) <- NULL
  out
}
