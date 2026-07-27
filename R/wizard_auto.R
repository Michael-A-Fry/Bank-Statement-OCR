# wizard_auto.R -- "generate as much of the wizard as possible" helpers, used by
# the Admin panel to turn an unsupported statement into a starting template so a
# maintainer's job is confirm-and-save, not build-from-nothing.
#
# Delimited/Excel drafting reuses the existing wizard_detect heuristics (delimiter,
# date format, amount style, column mapping). This file adds the PDF piece:
# SUGGEST column boxes by clustering the word positions on the transaction page.

# .money_rx / date-ish helpers (kept local + generic).
#
# The leading currency glyph is matched as "any run of non-number, non-letter
# characters", NOT as a literal -- the same ASCII-only negation R/column_profile.R
# uses, and for the same two reasons: a raw pound/euro sign in the source would be
# a non-ASCII literal (the deployment box runs a C locale), and pinning "$" made
# the sniffer blind to every figure on a statement printed in any other currency,
# so it found no money, no columns and no transaction table at all. The run is
# bounded because one glyph is one to three BYTES depending on the locale.
.WA_MONEY <- "^-?[^0-9A-Za-z[:space:].,'()+-]{0,3}-?[0-9][0-9,]*\\.[0-9]{2}$"
# An ordinal day ("1st November 2018") is a date; .normalise_date_str already folds
# the suffix away for the reader, so the drafter has to see it as one too.
.WA_DATE  <- paste0("^[0-9]{1,2}(st|nd|rd|th|ST|ND|RD|TH)?",
                    "([/. -][A-Za-z0-9]{2,9}([/. -][0-9]{2,4})?)?$")

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
  utils::head(out[.fp_offerable(out)], n)
}

# .fp_offerable(ph) -- would the SAVE accept this phrase, on its own, as a pdf
# template's identifying phrase? The toolkit's "identifying phrase" picker is built
# from header_phrases(), so a phrase this list offers is one the user can choose --
# and choosing "Date" or "details" got them a refusal from the same screen that had
# just suggested it. Asked as validate_template's own question at min_score 1 (its
# .fp_fingerprint_problems, R/templates.R), never as a second copy of the rule, so
# the picker and the save cannot drift apart. safe(): a rule that cannot be
# evaluated must drop the candidate, never propose an unvetted one.
.fp_offerable <- function(ph) {
  if (!length(ph)) return(logical(0))
  vapply(as.character(ph), function(p)
    length(safe(.fp_fingerprint_problems(list(p), 1L, "pdf"), "unavailable")) == 0L,
    logical(1), USE.NAMES = FALSE)
}

# .cluster_1d(x, gap) -- group sorted values where consecutive gaps exceed `gap`.
.cluster_1d <- function(x, gap) {
  if (!length(x)) return(integer(0))
  o <- order(x); xs <- x[o]
  g <- cumsum(c(TRUE, diff(xs) > gap))
  grp <- integer(length(x)); grp[o] <- g; grp
}

# ============================ THE COLUMN SNIFFER =============================
# suggest_pdf_columns(input, gap) -> data.frame(field, x_min, x_max, kind), plus
# attr "page" (the page it measured) and attr "date_cells" (the date cells that
# band actually produces). Heuristic starting point ONLY -- the analyst confirms
# or redraws in the toolkit -- but it has to be a starting point that READS ROWS.
#
# It measures the table the way the READER will read it, and that is the whole
# design. Three faults, each of which on its own emptied the preview on real ANZ
# and ASB statements this repo ships proven templates for:
#
#  1. THE MONTH WAS NOT IN THE DATE COLUMN. A date column prints "28 Apr" as two
#     words, and only "28" is date-SHAPED; clustering date-shaped tokens alone
#     produced a 15pt band that stopped before the month. The date cell then read
#     "28", "%d %b" parsed nothing, and every row was dropped for an unreadable
#     date -- from a band that looks perfectly plausible drawn on the page, which
#     is why redrawing it by hand did not recover it either.
#  2. MONEY WAS MEASURED OVER THE WHOLE PAGE. A statement prints money outside its
#     transaction table (ASB's "Balance summary" block, its "Balances other
#     accounts" list). Page-wide clustering found FIVE money columns on a
#     three-column table and labelled four of them "amount" -- and a list can only
#     hold one `amount`, so .draft_pdf silently kept whichever came last: bands
#     over the wrong block entirely.
#  3. THE COLUMN LABELS PRINTED ON THE PAGE WERE NEVER READ. Which money column is
#     the withdrawal and which the deposit is not something to infer from position
#     -- the statement says so, in its own header row. Reading it is how a
#     three-money-column table is drafted as debit/credit/balance instead of a
#     guess.
#
# So: pick the transaction page, group its words into visual rows with the reader's
# own .group_rows(), keep the rows that carry BOTH a date and a money value, and
# measure only those. Label the money columns from the header row when there is
# one, by position only when there is not.

# .wa_month_rx() -- the reader's own month spellings (.PDF_MONTH, R/parse_pdf_table.R)
# as a whole-token test, so the drafter and the reader can never disagree about
# what a month name is. Built on call, not at load, so this file does not depend on
# source() order.
.wa_month_rx <- function() paste0("^(?:", .PDF_MONTH, ")\\.?$")

# .wa_date_part(tok) -- is this token a PIECE of a printed date (a day/month/year
# number, a month name, a weekday)? Used to grow the date band across the whole
# printed date rather than stopping at its numeric head.
.wa_date_part <- function(tok) {
  lo <- tolower(trimws(as.character(tok)))
  grepl("^[0-9]{1,4}$", lo) | grepl(.WA_DATE, lo) |
    grepl(.wa_month_rx(), lo, perl = TRUE) |
    grepl("^(?:mon|tue|tues|wed|thu|thur|thurs|fri|sat|sun)(?:day)?\\.?$", lo)
}

# .wa_money_role(label) -- what the statement's own column heading calls this money
# column. The debit / credit wordings come from the LEXICON (the same two
# categories detect_amount_style() uses for CSV headers), so a bank that writes
# "Paid out" / "Money in" is taught in the dictionary, never in code. NA means the
# heading did not say, or said both -- the caller then falls back to position.
.wa_money_role <- function(label) {
  s <- tolower(trimws(label %||% ""))
  if (!nzchar(s)) return(NA_character_)
  rx <- function(cat, dflt) paste(safe(as.character(lex(cat)), dflt), collapse = "|")
  d <- grepl(rx("amount_style_debit_headers",  c("debit", "withdrawal")), s)
  cr <- grepl(rx("amount_style_credit_headers", c("credit", "deposit")), s)
  if (grepl("balance", s)) return("balance")
  if (d && !cr) return("debit")
  if (cr && !d) return("credit")
  if (grepl("amount|value", s)) return("amount")
  NA_character_
}

# .wa_positional_roles(n) -- the money columns labelled by POSITION, for a
# statement whose header row could not be read. Three money columns is the
# money-out / money-in / balance shape every NZ retail statement in this repo
# uses. Four or more, with nothing on the page saying which is which, is a guess
# -- and the tool is not a guesser, so it returns NULL and the caller says so.
.wa_positional_roles <- function(n) switch(as.character(n),
  "1" = "amount", "2" = c("amount", "balance"),
  "3" = c("debit", "credit", "balance"), NULL)

# .wa_page_frame(w) -- one page's word boxes with the derived fields every step
# below reads: centre-x, right edge, the READER's own visual row grouping
# (.group_rows, R/parse_pdf_table.R -- measuring the columns on any other notion of
# "row" would measure a table the reader will not see), and the two token tests.
.wa_page_frame <- function(w) {
  w <- as.data.frame(w, stringsAsFactors = FALSE)
  w <- w[order(w$y, w$x), , drop = FALSE]
  w$cx <- w$x + w$width / 2
  w$x1 <- w$x + w$width
  w$row <- .group_rows(w$y, PARAM_PDF_ROW_TOL)
  w$money <- grepl(.WA_MONEY, w$text)
  w$dateish <- grepl(.WA_DATE, w$text) & grepl("[0-9]", w$text) & !w$money
  w
}

# pdf_has_transaction_rows(input) -- does this PDF print a DATED MONEY LINE
# anywhere? That is the shape of a transaction, and it is the drafter's honesty
# check: a document that never prints one is a guide, a form or a summary, not a
# transaction table, and no amount of dragging column boxes over it will make one.
# Drafting it anyway produced a template that read nothing and said nothing --
# which is how "add a bank in two minutes" became a blank preview and no way
# forward. Shares its row test with suggest_pdf_columns, so what the drafter
# refuses and what the sniffer measures can never disagree.
pdf_has_transaction_rows <- function(input) {
  for (w in input$words %||% list()) {
    if (is.null(w) || !nrow(w)) next
    w <- .wa_page_frame(w)
    for (ix in split(seq_len(nrow(w)), w$row))
      if (any(w$money[ix]) && any(w$dateish[ix])) return(TRUE)
  }
  FALSE
}

suggest_pdf_columns <- function(input, gap = 18) {
  wl <- input$words %||% list()
  if (!length(wl)) return(data.frame())
  cnt <- vapply(wl, function(w) if (is.null(w) || !nrow(w)) 0L else
    sum(grepl(.WA_MONEY, w$text)), integer(1))
  pg <- which.max(cnt); if (!length(pg) || cnt[pg] == 0) return(data.frame())
  w <- .wa_page_frame(wl[[pg]])
  rows <- split(seq_len(nrow(w)), w$row)

  # THE HEADER ROW: the visual row naming the most distinct column keywords. Its
  # labels are where the column boundaries and the money roles come from -- the
  # statement telling us its own layout instead of us inferring one.
  hdr_keys <- safe(tolower(as.character(lex("header_keywords"))), character(0))
  hscore <- vapply(rows, function(ix) {
    wd <- tolower(unlist(regmatches(w$text[ix], gregexpr("[A-Za-z]+", w$text[ix]))))
    length(unique(wd[wd %in% hdr_keys]))
  }, integer(1))
  hrow <- if (length(hscore) && max(hscore) >= 2L) which.max(hscore) else NA_integer_
  hdr <- if (!is.na(hrow)) w[rows[[hrow]], , drop = FALSE] else NULL

  # THE TRANSACTION ROWS. A printed transaction carries a date AND a money value on
  # one baseline, BELOW the table's header row. All three conditions earn their
  # keep: a summary block carries money with no date, a heading carries neither,
  # and an "upcoming payments" / "other balances" table printed ABOVE the header on
  # the same page carries both -- and its columns are somewhere else entirely, so
  # measuring it drags the real bands sideways. When nothing on the page looks like
  # that (a staggered layout, a poor OCR, a continuation page with no header), fall
  # back rather than returning nothing.
  has_dm <- vapply(rows, function(ix) any(w$money[ix]) && any(w$dateish[ix]), logical(1))
  below <- if (is.na(hrow)) rep(TRUE, length(rows)) else seq_along(rows) > hrow
  is_txn <- if (any(has_dm & below)) has_dm & below else has_dm
  tw <- if (any(is_txn)) w[unlist(rows[is_txn], use.names = FALSE), , drop = FALSE] else w

  # .hdr_span(lo, hi) -- the header wording printed over an x-range, and its own
  # extent. A money column is right-aligned and its label left-aligned, so they
  # only ever partly overlap; any overlap is the label for that column.
  .hdr_span <- function(lo, hi) {
    if (is.null(hdr)) return(NULL)
    sel <- which(hdr$x1 >= lo & hdr$x <= hi)
    if (!length(sel)) return(NULL)
    list(text = paste(hdr$text[sel], collapse = " "),
         x_min = min(hdr$x[sel]), x_max = max(hdr$x1[sel]))
  }

  mi <- which(tw$money)
  if (!length(mi)) return(data.frame())
  mg <- .cluster_1d(tw$cx[mi], gap)
  ord <- as.integer(names(sort(tapply(tw$cx[mi], mg, mean))))
  money <- lapply(ord, function(k) {
    ix <- mi[mg == k]
    list(x_min = min(tw$x[ix]), x_max = max(tw$x1[ix]))
  })
  # Union each observed column of figures with its heading: the heading's left edge
  # is where the BANK says the column starts, which is the only honest place to put
  # the boundary against a left-aligned description that may run longer on a page we
  # were not shown.
  roles <- rep(NA_character_, length(money))
  for (k in seq_along(money)) {
    h <- .hdr_span(money[[k]]$x_min, money[[k]]$x_max)
    if (is.null(h)) next
    money[[k]]$x_min <- min(money[[k]]$x_min, h$x_min)
    money[[k]]$x_max <- max(money[[k]]$x_max, h$x_max)
    roles[k] <- .wa_money_role(h$text)
  }

  # A money column the heading NAMES but this page never printed a figure in. The
  # commonest is a Deposits column on a statement whose first page is all
  # withdrawals: without this the drafted template has no credit band at all, and
  # every deposit further into the document is a row with no money in any mapped
  # column -- dropped, silently, which is the one outcome the contract forbids.
  # Only when the heading is being read successfully for every column we DID see:
  # if it is not, the positional fallback is about to take over, and a band with no
  # figures in it would then be labelled by its position like a real one.
  if (!is.null(hdr) && !anyNA(roles)) {
    hg <- .cluster_1d(hdr$cx, gap)
    for (k in unique(hg)) {
      ix <- which(hg == k)
      r <- .wa_money_role(paste(hdr$text[ix], collapse = " "))
      if (is.na(r) || r %in% roles) next
      lo <- min(hdr$x[ix]); hi <- max(hdr$x1[ix])
      if (any(vapply(money, function(m) m$x_max >= lo && m$x_min <= hi, logical(1)))) next
      money <- c(money, list(list(x_min = lo, x_max = hi))); roles <- c(roles, r)
    }
    o <- order(vapply(money, `[[`, numeric(1), "x_min"))
    money <- money[o]; roles <- roles[o]
  }

  # A heading-derived set is only usable when it names every money column, names
  # none of them twice, and includes somewhere for the transaction's own figure to
  # go -- a table whose only money column is a running balance has been mis-read.
  if (anyNA(roles) || anyDuplicated(roles) ||
      !any(roles %in% c("amount", "debit", "credit")))
    roles <- .wa_positional_roles(length(money))
  if (is.null(roles)) return(data.frame())     # cannot say which column is which

  # THE DATE COLUMN: the leftmost cluster of date-shaped tokens, then GROWN along
  # each row over the rest of the printed date. "28 Apr" is two words and only the
  # first is date-shaped, so the cluster alone stops at the day -- which is the
  # fault that emptied the preview. Growth stops at the first token that is not a
  # date piece, or at the first gap wider than a printed space (0.75 of the line
  # height): a column gutter runs 11-28pt on these statements where the space
  # inside a date runs 2-4pt, so the date never reaches into the description and
  # takes a word the description is promised to reproduce verbatim.
  di <- which(tw$dateish)
  if (!length(di)) return(data.frame())
  dg <- .cluster_1d(tw$cx[di], gap)
  date_ix <- di[dg == dg[which.min(tw$cx[di])]]
  for (a in date_ix) {
    same <- which(tw$row == tw$row[a] & tw$x > tw$x[a])
    same <- same[order(tw$x[same])]
    prev <- a
    for (b in same) {
      lh <- suppressWarnings(as.numeric(tw$height[prev]))
      if (!isTRUE(lh > 0)) lh <- 10                  # unknown line height -> A4 body text
      if (tw$x[b] - tw$x1[prev] > 0.75 * lh) break
      if (!.wa_date_part(tw$text[b])) break
      date_ix <- c(date_ix, b); prev <- b
    }
  }
  date_ix <- unique(date_ix)
  dh <- .hdr_span(min(tw$x[date_ix]), max(tw$x1[date_ix]))
  date_col <- list(x_min = min(c(tw$x[date_ix], dh$x_min)),
                   x_max = max(c(tw$x1[date_ix], dh$x_max)))

  # THE DESCRIPTION: everything printed between the date and the first money
  # column. Its right-hand boundary is the first money column's LEFT edge, never a
  # midpoint -- descriptions grow rightwards, and a description promised verbatim
  # must not lose its last word to a money band that reached back for it.
  first_money <- money[[1]]$x_min
  desc_ix <- which(tw$cx > date_col$x_max & tw$cx < first_money)
  desc_col <- if (length(desc_ix))
    list(x_min = min(tw$x[desc_ix]), x_max = max(tw$x1[desc_ix])) else NULL

  cols <- list(list(field = "date", kind = "date", x_min = date_col$x_min, x_max = date_col$x_max))
  if (!is.null(desc_col)) cols[[length(cols) + 1L]] <-
    list(field = "description", kind = "text", x_min = desc_col$x_min, x_max = desc_col$x_max)
  for (k in seq_along(money)) cols[[length(cols) + 1L]] <-
    list(field = roles[k], kind = "money", x_min = money[[k]]$x_min, x_max = money[[k]]$x_max)

  # TILE THE BANDS. Measured extents are what THIS page happened to print; a longer
  # figure on a page we were not shown falls between two tight bands and is lost
  # without a word. Boundaries go at the midpoint between neighbours, except the
  # description / first-money boundary, which stays where the heading puts it
  # (above), and the two outer edges, which are set below.
  n <- length(cols)
  bnd <- numeric(n - 1L)
  for (i in seq_len(n - 1L)) {
    lo <- cols[[i]]$x_max; hi <- cols[[i + 1L]]$x_min
    b <- if (identical(cols[[i]]$field, "description")) hi else (lo + hi) / 2
    # A boundary must never cut into either column's own words. Extents can OVERLAP
    # (a heading printed wider than its figures), and then the midpoint is the only
    # available compromise -- but a boundary LEFT of the left column's last word,
    # which is what an un-clamped midpoint produced, silently truncates it.
    bnd[i] <- if (lo <= hi) min(max(b, lo), hi) else (lo + hi) / 2
  }
  left_edge <- cols[[1L]]$x_min - 3          # the date's own words, not the row's
  # The right edge follows the last money column and the balance-SIGN marker
  # printed against it ("17,731.96 OD"), and stops there. It used to run to the
  # right-hand margin of the text, which swallowed the hand-written notes some
  # statements print beside the table straight into the balance cell. The markers
  # are the lexicon's, so a bank that writes its overdrawn flag differently is
  # taught in the dictionary.
  mk <- toupper(unlist(lapply(c("overdrawn_markers", "dr_cr_suffix_debit",
    "dr_cr_suffix_credit"), function(k) safe(as.character(lex(k)), character(0)))))
  last <- cols[[n]]
  right_edge <- last$x_max + 3
  near <- which(tw$x >= last$x_max & tw$x - last$x_max <= 0.75 * tw$height &
                toupper(trimws(tw$text)) %in% mk)
  if (length(near)) right_edge <- max(right_edge, max(tw$x1[near]) + 3)
  for (i in seq_len(n)) {
    cols[[i]]$x_min <- if (i > 1L) bnd[i - 1L] else left_edge
    cols[[i]]$x_max <- if (i < n) bnd[i] else right_edge
  }

  out <- do.call(rbind, lapply(cols, function(c) data.frame(field = c$field,
    x_min = floor(c$x_min), x_max = ceiling(c$x_max), kind = c$kind,
    stringsAsFactors = FALSE)))
  out <- out[order(out$x_min), , drop = FALSE]
  rownames(out) <- NULL
  # The date cells this band actually yields, so the date-format sniff reads the
  # same strings the reader will (see .guess_pdf_date_format, R/draft.R) instead of
  # scanning every page for anything that falls in the same x-range.
  db <- out[out$field == "date", , drop = FALSE]
  cells <- vapply(split(seq_len(nrow(tw)), tw$row), function(ix) {
    s <- ix[tw$cx[ix] >= db$x_min[1] & tw$cx[ix] <= db$x_max[1]]
    if (!length(s)) "" else paste(tw$text[s][order(tw$x[s])], collapse = " ")
  }, character(1), USE.NAMES = FALSE)
  attr(out, "page") <- as.integer(pg)
  attr(out, "date_cells") <- cells[nzchar(cells)]
  out
}
