# doc_noise.R -- PAGE FURNITURE: the lines a report prints on every page.
#
# WHY THIS EXISTS. A report's disclaimer, its confidentiality stamp and its
# "Page 4 of 44" sit INSIDE the physical window of whatever table happens to be
# open on that page, so the reader claimed them as rows. Observed on a real
# extraction:
#
#   Tax Agent History
#     row 1 = disclaimer
#     row 2 = disclaimer
#     row 3 = IN CONFIDENCE
#     row 4 = the first real record
#
# Three fabricated records above the data, in a table nobody can tell is wrong by
# looking at it. That is the charter's cardinal failure -- silently wrong -- and
# it is not a template bug: no band, no end coordinate and no fill threshold can
# separate a disclaimer from a record, because on the page they are both just
# text inside the window.
#
# WHAT WAS THERE BEFORE, AND WHY IT COULD NOT WIN. .is_footer_noise()
# (R/parse_pdf_table.R) is a hand-maintained list of footer WORDINGS -- "page N
# of M", "continued on next page", "carried forward to". Every new report's
# footer is a code change, and until somebody makes it the footer becomes data.
# Its own comment records exactly that: a "Carried Forward to next page" footer
# was merged into a transaction description before a branch was added for it.
#
# THE EVIDENCE IS ALREADY ON THE PAGE, AND IT IS NOT THE WORDING. Furniture
# REPEATS: the same line, in the same margin, on page after page. Data does not.
# That is a fact about the document, so it needs no vocabulary, works on a report
# this tool has never seen, and never needs another branch. Digit runs are
# collapsed before comparing, so "Page 3 of 44" and "Page 4 of 44" are one line.
#
# TWO MECHANISMS, because neither covers the other:
#   1. RECURRENCE  -- generic, learned per document, no wording anywhere.
#   2. TEMPLATE RULES -- for the statutory paragraph that is printed ONCE, on a
#      cover page, and so never repeats. A template can name it; nothing here
#      knows what it says.
#
# NOTHING IS DELETED. This classifies; it does not mutate input$words. Every
# suppressed line is counted and reported (doc_noise_report), because a reader
# who cannot see what was withheld cannot audit it -- and the engine's promise is
# that content is never silently dropped.

# How close to the paper's edge a line must sit before recurrence can call it
# furniture. A running header lives in the top eighth; a footer block runs deeper
# because it carries the disclaimer as well as the page number. Deliberately
# conservative: a table's own repeated HEADER row also recurs page after page,
# and the margin test is most of what keeps this off it.
DOC_NOISE_TOP_RATIO    <- 0.12
DOC_NOISE_BOTTOM_RATIO <- 0.18
# How many pages a line must appear on. Two is the least that can distinguish
# "printed on every page" from "printed once", and a two-page report is common.
DOC_NOISE_MIN_PAGES <- 2L
# A one-page document has no cross-page evidence at all, so recurrence is not
# asked. Its furniture needs a template rule or it stays.
DOC_NOISE_MIN_DOC_PAGES <- 2L

# .noise_norm(s) -- the form two lines are compared in.
#
# Digit runs collapse to "#" so a page number does not make every page's footer a
# different line. Case and run-length of whitespace go. Nothing else: dropping
# punctuation would let "*** IN CONFIDENCE ***" and the words "in confidence"
# inside a real sentence collide.
.noise_norm <- function(s) {
  s <- tolower(trimws(as.character(s %||% "")))
  s <- gsub("[[:space:]]+", " ", s)
  gsub("[0-9]+", "#", s)
}

# HOW FAR A RECURRING LINE MAY MOVE BETWEEN PAGES AND STILL BE FURNITURE.
#
# THIS IS THE GUARD THAT MATTERS, and leaving it out was a real fault, caught by
# the suite: collapsing digit runs is what lets "Page 3 of 44" and "Page 4 of 44"
# be one line, and it also makes EVERY row of a numeric table normalise to the
# same string --
#
#   01/02/2024 Payment-17 -45.20 1,234.50   ->  #/#/# payment-# -#.# #,#.#
#   03/04/2024 Payment-42 -88.10 2,345.60   ->  #/#/# payment-# -#.# #,#.#
#
# so two unrelated transactions on consecutive pages looked like one line printed
# twice, and four real rows were suppressed from an 83-row table.
#
# Furniture is printed by the page template, so it lands at the SAME height every
# time. Rows land wherever the table happened to reach. A few points of slack for
# a footer that shifts with a one- or two-line disclaimer above it.
DOC_NOISE_Y_TOL <- 4

# .noise_has_money(tokens) -- does this line carry a figure.
#
# The second half of the guard above, and the one that does not depend on
# geometry: page furniture is a page number, a stamp and a paragraph of statute.
# It does not carry money. A line that does is a row of somebody's table, however
# faithfully it repeats. Uses the engine's own money reader, so "what counts as a
# figure" has exactly one definition in this codebase.
.noise_has_money <- function(tokens) {
  tk <- as.character(tokens %||% character(0))
  tk <- tk[!is.na(tk) & nzchar(tk)]
  if (!length(tk)) return(FALSE)
  any(vapply(tk, function(t) !is.na(.value_from_line(t, "money")), logical(1)),
      na.rm = TRUE)
}

# .noise_margin(top, page_height, top_ratio, bottom_ratio) -- is this line in the
# top or bottom margin of its page.
.noise_margin <- function(top, page_height, top_ratio, bottom_ratio) {
  h <- .doc_num(page_height, NA_real_)
  y <- .doc_num(top, NA_real_)
  if (is.na(h) || h <= 0 || is.na(y)) return(FALSE)
  y <= h * top_ratio || y >= h * (1 - bottom_ratio)
}

# .noise_rules(tmpl) -- the template's explicit rules, normalised to a list of
# list(match, value). An unusable rule is dropped rather than throwing: a typo in
# one noise rule must not stop a conversion.
#
# Three match modes, and no more. `regex` is the general case; the other two
# exist because a maintainer writing a disclaimer out in full should not have to
# escape it. Anything else is refused, so a template cannot smuggle in a mode the
# engine does not implement and quietly get no suppression at all.
.DOC_NOISE_MATCHES <- c("regex", "normalized_exact", "contains")

.noise_rules <- function(tmpl) {
  pn <- .doc_key(tmpl, "page_noise")
  if (!is.list(pn)) return(list())
  lines <- pn$lines %||% list()
  if (!is.list(lines)) return(list())
  out <- list()
  for (r in lines) {
    if (is.character(r) && length(r) == 1L) r <- list(match = "contains", value = r)
    if (!is.list(r)) next
    m <- tolower(trimws(as.character(r$match %||% "contains")[1]))
    v <- as.character(r$value %||% "")[1]
    if (is.na(v) || !nzchar(v)) next
    if (!(m %in% .DOC_NOISE_MATCHES)) next
    # A regex that does not compile is refused HERE, once, rather than throwing
    # once per line of every page.
    # suppressWarnings as well as tryCatch: an invalid PCRE class WARNS before it
    # errors, so an error-only guard let a warning escape to the console from a
    # rule this line is in the middle of refusing.
    if (identical(m, "regex") &&
        inherits(suppressWarnings(tryCatch(grepl(v, "", perl = TRUE),
                                           error = function(e) e)), "condition")) next
    out[[length(out) + 1L]] <- list(match = m, value = v)
  }
  out
}

# .noise_rule_hit(rule, text) -- does one explicit rule catch this line.
.noise_rule_hit <- function(rule, text) {
  txt <- as.character(text %||% "")[1]
  switch(rule$match,
    regex = isTRUE(safe(grepl(rule$value, txt, perl = TRUE), FALSE)),
    normalized_exact = identical(.noise_norm(txt), .noise_norm(rule$value)),
    contains = grepl(.noise_norm(rule$value), .noise_norm(txt), fixed = TRUE),
    FALSE)
}

# .noise_recurrence_cfg(tmpl) -- the recurrence settings, defaulted.
.noise_recurrence_cfg <- function(tmpl) {
  pn <- .doc_key(tmpl, "page_noise")
  rc <- if (is.list(pn)) (pn$recurrence %||% list()) else list()
  if (!is.list(rc)) rc <- list()
  on <- rc$use_repeated_margin_lines
  list(on = if (is.null(on)) TRUE else isTRUE(on),
       min_pages = max(2L, .doc_int(rc$minimum_pages, DOC_NOISE_MIN_PAGES)),
       top = .doc_num(rc$top_margin_ratio, DOC_NOISE_TOP_RATIO),
       bottom = .doc_num(rc$bottom_margin_ratio, DOC_NOISE_BOTTOM_RATIO))
}

# .noise_protected(tmpl) -- normalised wordings recurrence may NEVER suppress.
#
# A table's own repeated column header recurs in roughly the same place page
# after page, which is exactly the shape recurrence looks for. Suppressing it
# would be worse than the disease: the reader uses the header to find the table.
# So every table's name and every remembered header wording is protected, and the
# margin test is not trusted to be the only thing standing between them.
.noise_protected <- function(tmpl) {
  tabs <- .doc_key(tmpl, "tables")
  if (!is.list(tabs)) return(character(0))
  out <- character(0)
  for (tb in tabs) {
    if (!is.list(tb)) next
    nm <- as.character(tb$name %||% "")[1]
    if (!is.na(nm) && nzchar(nm)) out <- c(out, .noise_norm(nm))
    ht <- tb$anchor$header_text %||% character(0)
    ht <- as.character(unlist(ht))
    ht <- ht[!is.na(ht) & nzchar(ht)]
    if (length(ht)) out <- c(out, .noise_norm(paste(ht, collapse = " ")))
    # ...and each header word on its own, because a header line printed with an
    # extra column would not match the joined form.
    for (h in ht) out <- c(out, .noise_norm(h))
  }
  unique(out[nzchar(out)])
}

# doc_noise_mask(input, tmpl) -> the classifier for one document.
#
# list(keys, rules, pages, recurrence) where `keys` is the set of normalised line
# texts recurrence learned. Built ONCE per document and passed down, because it
# reads every page and must not be rebuilt per table.
doc_noise_mask <- function(input, tmpl = NULL) {
  rules <- .noise_rules(tmpl)
  cfg <- .noise_recurrence_cfg(tmpl)
  empty <- list(keys = character(0), rules = rules, pages = 0L, recurrence = cfg,
                protected = character(0))
  wl <- input$words %||% list()
  np <- length(wl)
  if (!np) return(empty)
  empty$pages <- np
  protected <- .noise_protected(tmpl)
  empty$protected <- protected
  if (!isTRUE(cfg$on) || np < DOC_NOISE_MIN_DOC_PAGES) return(empty)

  heights <- input$page_height %||% rep(NA_real_, np)
  frame_h <- .doc_frame(tmpl)$height
  # normalised text -> the pages it was seen on, the heights it sat at, and
  # whether any occurrence carried money.
  seen <- new.env(parent = emptyenv())
  for (p in seq_len(np)) {
    w <- .doc_page_words(input, p, tmpl)
    if (is.null(w) || !nrow(w)) next
    # After .doc_page_words has mapped this page into the template's frame, the
    # frame's height is the one the y values are in -- not the raw page's.
    ph <- if (is.null(tmpl)) .doc_num(heights[p], frame_h) else frame_h
    for (d in .doc_lines(w, NULL)) {
      if (.doc_is_rule(d)) next
      top <- .doc_num(attr(d, "top"), NA_real_)
      if (!.noise_margin(top, ph, cfg$top, cfg$bottom)) next
      k <- .noise_norm(.doc_line_text(d))
      if (!nzchar(k)) next
      cur <- if (exists(k, envir = seen, inherits = FALSE))
        get(k, envir = seen, inherits = FALSE) else
        list(pages = integer(0), ys = numeric(0), money = FALSE)
      cur$pages <- unique(c(cur$pages, p))
      cur$ys <- c(cur$ys, top)
      if (!cur$money) cur$money <- .noise_has_money(d$text)
      assign(k, cur, envir = seen)
    }
  }
  keys <- ls(seen)
  if (!length(keys)) return(empty)
  keep <- vapply(keys, function(k) {
    v <- get(k, envir = seen, inherits = FALSE)
    # THREE TESTS, and a line must pass all of them.
    #   repeats  -- printed on at least this many pages;
    #   still    -- at the same height on each, so a row that merely normalises
    #               like another row cannot qualify (see DOC_NOISE_Y_TOL);
    #   no money -- furniture carries a page number and a disclaimer, never a
    #               figure. A line with money on it is data, whatever it repeats.
    if (length(v$pages) < cfg$min_pages) return(FALSE)
    if (isTRUE(v$money)) return(FALSE)
    ys <- v$ys[is.finite(v$ys)]
    if (!length(ys)) return(FALSE)
    (max(ys) - min(ys)) <= DOC_NOISE_Y_TOL
  }, logical(1))
  keys <- keys[keep]
  # A protected wording is never learned, however often it recurs.
  if (length(protected)) keys <- keys[!(keys %in% protected)]
  # Deterministic: ls() is sorted, and so is what comes out of it.
  empty$keys <- sort(unique(keys))
  empty
}

# doc_is_noise(mask, text, top, page_height) -> TRUE when this line is furniture.
#
# The explicit rules are asked ANYWHERE on the page; recurrence only inside the
# margin it learned from. That asymmetry is deliberate: a statutory paragraph can
# be printed in the middle of a cover page, while a line that merely resembles a
# footer must not be suppressed in the body of a table.
doc_is_noise <- function(mask, text, top = NA_real_, page_height = NA_real_) {
  if (!is.list(mask)) return(FALSE)
  txt <- as.character(text %||% "")[1]
  if (is.na(txt) || !nzchar(trimws(txt))) return(FALSE)
  for (r in mask$rules %||% list()) if (.noise_rule_hit(r, txt)) return(TRUE)
  keys <- mask$keys %||% character(0)
  if (!length(keys)) return(FALSE)
  # The money guard again, at the point of use. The mask holds normalised TEXT,
  # and two different lines can share one normalised form -- which is the whole
  # reason the learner needed this test. A line with a figure on it is a row.
  if (.noise_has_money(strsplit(txt, "[[:space:]]+")[[1]])) return(FALSE)
  k <- .noise_norm(txt)
  if (!nzchar(k) || !(k %in% keys)) return(FALSE)
  cfg <- mask$recurrence %||% list(top = DOC_NOISE_TOP_RATIO,
                                   bottom = DOC_NOISE_BOTTOM_RATIO)
  # No usable geometry means the caller could not say where the line sat. The
  # line was learned from the margin, so a bare wording match is accepted -- the
  # recurrence evidence stands on its own.
  if (is.na(.doc_num(page_height, NA_real_)) || is.na(.doc_num(top, NA_real_)))
    return(TRUE)
  .noise_margin(top, page_height, cfg$top, cfg$bottom)
}

# ---------------------------------------------------------------------------
# ONE MASK PER DOCUMENT, not one per table.
#
# doc_noise_mask() reads every page. A twenty-table report would build it twenty
# times, and the auto-end probe would build it again for each -- so it is keyed by
# the document's own content hash (the same key read_input memoises on) plus
# enough of the template to tell two templates over one document apart.
#
# The cache is a pure function of its key: same bytes, same template, same answer,
# which is what keeps the output deterministic. Bounded, and cleared wholesale
# rather than aged, exactly as .INPUT_CACHE is -- a conversion works with one
# document and this only exists to stop the per-table rebuild.
.DOC_NOISE_CACHE <- new.env(parent = emptyenv())
.DOC_NOISE_CACHE_MAX <- 8L

.doc_cache_key <- function(input, tmpl, what) {
  sha <- as.character(input$sha256 %||% "")[1]
  if (is.na(sha) || !nzchar(sha)) return(NA_character_)
  tabs <- .doc_key(tmpl, "tables")
  paste(what, sha, length(input$words %||% list()),
        .doc_int(.doc_key(tmpl, "schema_version"), 1L),
        length(if (is.list(tabs)) tabs else list()),
        paste(names(if (is.list(tabs)) tabs else list()) %||% "", collapse = ","),
        length(.noise_rules(tmpl)), sep = "|")
}

.doc_cache_get <- function(key, build) {
  if (is.na(key)) return(build())
  if (exists(key, envir = .DOC_NOISE_CACHE, inherits = FALSE))
    return(get(key, envir = .DOC_NOISE_CACHE, inherits = FALSE))
  v <- build()
  if (length(ls(.DOC_NOISE_CACHE)) >= .DOC_NOISE_CACHE_MAX)
    rm(list = ls(.DOC_NOISE_CACHE), envir = .DOC_NOISE_CACHE)
  assign(key, v, envir = .DOC_NOISE_CACHE)
  v
}

# doc_noise_mask_for(input, tmpl) -- the cached mask for this document.
doc_noise_mask_for <- function(input, tmpl = NULL)
  .doc_cache_get(.doc_cache_key(input, tmpl, "noise"),
                 function() doc_noise_mask(input, tmpl))

# doc_section_index_for(input, tmpl) -- the cached section index (R/doc_structure.R).
# Here rather than there so both caches share one key and one bound.
doc_section_index_for <- function(input, tmpl = NULL)
  .doc_cache_get(.doc_cache_key(input, tmpl, "sections"),
                 function() doc_section_index(input, tmpl))

# doc_clear_structure_cache() -- drop both, for tests and for a re-read.
doc_clear_structure_cache <- function()
  rm(list = ls(.DOC_NOISE_CACHE), envir = .DOC_NOISE_CACHE)

# doc_noise_report(mask, dropped) -> one plain sentence, or NULL when nothing was
# suppressed. What the screen and the Diagnostics sheet say, so a reader can see
# that lines were withheld and how many.
doc_noise_report <- function(mask, dropped) {
  n <- .doc_int(dropped, 0L)
  if (is.na(n) || n <= 0L) return(NULL)
  learned <- length(mask$keys %||% character(0))
  ruled <- length(mask$rules %||% list())
  how <- character(0)
  if (learned > 0L) how <- c(how, sprintf("%d line%s that repeat in the page margins",
                                          learned, if (learned == 1L) "" else "s"))
  if (ruled > 0L) how <- c(how, sprintf("%d rule%s in the template",
                                        ruled, if (ruled == 1L) "" else "s"))
  sprintf("%d line%s of page furniture (headers, footers, disclaimers) %s not read as data, recognised by %s.",
          n, if (n == 1L) "" else "s", if (n == 1L) "was" else "were",
          if (length(how)) paste(how, collapse = " and ") else "the page margins")
}
