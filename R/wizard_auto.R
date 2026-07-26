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

# A line names a LAYOUT (a good, safe fingerprint) when it carries bank / product /
# statement BRANDING -- e.g. "Air New Zealand Airpoints Platinum Mastercard".
#
# WHY these words come from the LEXICON and not a constant: the list used to be a
# hardcoded ~25 words, so a bank the list had never heard of ("Tangata Finance")
# had its own masthead mistaken for a customer NAME and thrown away -- the drafter
# produced its weakest fingerprints for exactly the banks nobody had taught it yet.
# Teaching it a bank must be a dictionary edit, never a code change (charter: any
# layout via templates, no code), so it resolves through lex() like every other
# engine vocabulary. The built-in below IS the previously shipped list, so an
# absent (or not-yet-registered) lexicon category behaves exactly as before.
.FP_BRAND_DEFAULT <- c("bank", "card", "mastercard", "visa", "amex", "eftpos",
  "account", "statement", "everyday", "savings", "cheque", "current", "credit",
  "debit", "platinum", "gold", "classic", "standard", "airpoints", "rewards",
  "business", "personal", "transaction", "summary", "loan", "mortgage",
  "kiwibank", "westpac", "anz", "asb", "bnz", "tsb", "sbs", "rabobank",
  "heartland", "co-operative", "cooperative")

# .fp_brand_words() -- built-in default UNIONed with the lexicon's
# `fingerprint_brand_words` category (merge type "list", like debit_markers).
# safe() because lex() raises on a category R/lexicon.R has not registered yet:
# until it is, we fall back to the built-in rather than breaking drafting.
.fp_brand_words <- function() {
  extra <- safe(as.character(lex("fingerprint_brand_words")), character(0))
  extra <- tolower(trimws(extra[!is.na(extra)]))
  unique(c(.FP_BRAND_DEFAULT, extra[nzchar(extra)]))
}

# .fp_brand_rx() -- the brand vocabulary as one word-boundary alternation. Every
# word is regex-ESCAPED first so an admin can type a plain brand name ("M&S Bank",
# "Co-operative") without knowing regex, and a stray metacharacter in the
# dictionary can never break (or silently widen) the match.
.fp_brand_rx <- function() {
  w <- gsub("([^A-Za-z0-9 _-])", "\\\\\\1", .fp_brand_words())
  paste0("\\b(", paste(w, collapse = "|"), ")\\b")
}

# Personal TITLES. A heading carrying one is naming a PERSON, not a layout:
# "Account name: Mr John Smith" fingerprints ONE customer and (far worse) writes
# their name into a template file that is listed in Admin, shown in the YAML box
# and copied into the offline bundle. The title used to have to START the line, so
# a mid-line title was invisible -- it is now found ANYWHERE in the phrase.
.FP_TITLE_WORDS <- c("mr", "mrs", "ms", "miss", "dr", "mstr", "master",
                     "sir", "madam", "mx", "messrs")
# ...but a title is only a NAME when a name follows it. These words legitimately
# sit after "Dr"/"Master" on a statement ("Dr Cr", "Master Account"), and treating
# those as PII would reject honest column labels.
.FP_TITLE_NOT_NAME <- c("cr", "dr", "bal", "balance", "amount", "amt", "account",
                        "accounts", "no", "number", "ref", "reference", "total")

# .fp_has_pii(ph) -- does this fingerprint phrase name a PERSON? TRUE when a
# personal title appears anywhere in it followed by a name-shaped word. Shared by
# the drafter (which drops the candidate) and validate_template (which REJECTS it
# at load), so the hand-edited Advanced-YAML path is covered by the same rule.
.fp_has_pii <- function(ph) {
  vapply(as.character(ph), function(p) {
    if (is.na(p)) return(FALSE)
    w <- unlist(regmatches(p, gregexpr("[A-Za-z][A-Za-z'-]*", p)))
    if (length(w) < 2) return(FALSE)
    lw <- tolower(w)
    at <- which(lw %in% .FP_TITLE_WORDS)
    at <- at[at < length(lw)]                    # only a title with a word after it
    length(at) > 0 && any(!(lw[at + 1L] %in% .FP_TITLE_NOT_NAME))
  }, logical(1), USE.NAMES = FALSE)
}

# A line names a CUSTOMER (PII, and a per-person not per-layout fingerprint) when it
# carries a personal title anywhere, or is a bare <=5 word name with NO branding.
# (<=5, not <=4: a full name with middle names -- "John Michael Smith Jones" -- ran
# past the old cap and slipped through. The cost of the wider net is only that the
# drafter falls back to the column-header line, which is a better fingerprint
# anyway.) These must never enter a saved template's fingerprint.
.fp_is_customer <- function(ln, brand_rx = .fp_brand_rx()) {
  lo <- tolower(trimws(ln))
  w <- unlist(regmatches(ln, gregexpr("[A-Za-z]+", ln)))
  isTRUE(.fp_has_pii(ln)) ||
    (length(w) <= 5 && length(w) >= 1 && all(grepl("^[A-Za-z'-]+$", w)) && !grepl(brand_rx, lo))
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
  hdr_keys <- lex("header_keywords")   # from the lexicon (admin/ML-extendable)
  brand_rx <- .fp_brand_rx()           # ditto -- resolved once per draft
  # 1. a distinctive BRAND/product line near the top: multi-word, no digits (not a
  # data row), not just column labels, and NOT a customer name (PII). Brand lines
  # (e.g. "Air New Zealand Airpoints Platinum Mastercard") lead, because they
  # identify the layout and never carry PII.
  top <- utils::head(lines, 15L)
  titleish <- vapply(top, function(ln) {
    w <- unlist(regmatches(ln, gregexpr("[A-Za-z]+", ln)))
    nchar(ln) >= 10 && nchar(ln) <= 60 && length(w) >= 2 && !grepl("[0-9]", ln) &&
      (length(w) == 0 || mean(tolower(w) %in% hdr_keys) < 0.5) &&
      !.fp_is_customer(ln, brand_rx)             # never a customer / account-holder name
  }, logical(1))
  kept <- top[titleish]
  brandy <- grepl(brand_rx, tolower(kept))       # branded lines first (most distinctive)
  out <- c(out, utils::head(c(kept[brandy], kept[!brandy]), 2L))
  # 2. the transaction table's header LINE as a whole multi-word phrase (the column
  # labels together are far more specific than any one of them alone).
  score <- vapply(lines, function(ln) {
    w <- tolower(unlist(regmatches(ln, gregexpr("[A-Za-z]+", ln))))
    length(unique(w[w %in% hdr_keys]))
  }, integer(1))
  best <- which.max(score)
  if (length(best) && score[best] >= 2 && nchar(lines[best]) <= 60)
    out <- c(out, lines[best])
  # 3. secondary anchors: individual column-label words (kept for continuity, but
  # after the distinctive phrases so a good fingerprint leads).
  if (length(best) && score[best] >= 1) {
    words <- unlist(regmatches(lines[best], gregexpr("[A-Za-z]+", lines[best])))
    out <- c(out, words[tolower(words) %in% hdr_keys])
  }
  # Belt-and-braces: whatever route a phrase arrived by, it never leaves here
  # naming a person. validate_template enforces the same rule at save/load time;
  # this just means the drafter doesn't propose one for the analyst to approve.
  out <- unique(out[nzchar(out)])
  utils::head(out[!.fp_has_pii(out)], n)
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
