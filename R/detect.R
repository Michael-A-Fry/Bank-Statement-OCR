# detect.R -- deterministic bank/statement detection via fingerprint scoring.

# .header_fields(lines, template) -- locate the header row (via the shared
# locate_header helper so detection and the reader never diverge) and split it
# into trimmed field names.
.header_fields <- function(lines, template) {
  if (length(lines) == 0) return(character(0))
  hidx <- locate_header(lines, template)
  if (is.na(hidx)) return(character(0))
  # resolve_delimiter: a template may declare SEVERAL separators for one layout
  # (ASB publishes the same export as CSV and as tab-delimited). Resolved from the
  # header line here and from the SAME header line in read_delimited, so detection
  # and the reader can never disagree about how the file splits.
  delim <- resolve_delimiter(lines[hidx], template)
  fields <- utils::read.table(text = lines[hidx], sep = delim, quote = "\"",
                              stringsAsFactors = FALSE, colClasses = "character",
                              header = FALSE, check.names = FALSE,
                              comment.char = "")[1, , drop = TRUE]
  trimws(as.character(unlist(fields)))
}

# .fp_norm_ws(x) -- collapse every run of HORIZONTAL whitespace to a single space
# (and strip padding either side of a line break), leaving line breaks intact.
#
# WHY: a PDF page's text keeps the wide column padding pdftools renders
# ("Withdrawals      Deposits"), but the auto-drafter stores a fingerprint phrase
# already whitespace-collapsed ("Withdrawals Deposits"), and matching is a FIXED
# substring -- so a drafted template could not detect the very file it was drafted
# from. Folding BOTH sides here (rather than at draft time) also repairs templates
# already saved out in the field. Line breaks are deliberately kept: collapsing
# them too would let a phrase match across a line boundary, which would make
# detection LOOSER -- the wrong direction for a fail-closed engine. Curated
# single-spaced fingerprints are unaffected (nothing to collapse).
.fp_norm_ws <- function(x) {
  # useBytes throughout: a PDF's text layer is not always valid UTF-8, and a plain
  # gsub/trimws ERRORS on such a string -- detection must never crash on a file it
  # simply cannot read well. Byte-wise folding of ASCII whitespace is identical for
  # every well-formed page and merely survives the malformed ones.
  # \u00a0 is the non-breaking space some PDF producers emit where a plain space is
  # meant -- it must fold too, or the phrase and the page still disagree.
  x <- gsub("\u00a0", " ", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("[ \t\r\f\v]+", " ", x, useBytes = TRUE)
  x <- gsub(" *\n *", "\n", x, useBytes = TRUE)
  gsub("^[ \n]+|[ \n]+$", "", x, useBytes = TRUE)
}

# .score_template(input, template) -- fingerprint score for one template.
.score_template <- function(input, template) {
  fp <- template$fingerprint
  # PDF templates fingerprint on page text, not delimited headers.
  if (identical(template$format %||% "delimited", "pdf")) {
    need <- as.character(fp$page_contains_all %||% character(0))
    hay <- .fp_norm_ws(paste(input$pages %||% character(0), collapse = "\n"))
    hits <- vapply(need, function(ph)
      grepl(.fp_norm_ws(ph), hay, fixed = TRUE, useBytes = TRUE), logical(1))
    return(list(score = sum(hits), need = length(need), missing = need[!hits]))
  }
  # Excel templates fingerprint on the sheet's column names.
  if (identical(template$format %||% "delimited", "excel")) {
    need <- as.character(fp$header_contains_all %||% character(0))
    header <- names(input$table %||% list())
    hit <- tolower(need) %in% tolower(header)   # case-insensitive, like .pick()
    return(list(score = sum(hit), need = length(need), missing = need[!hit]))
  }
  need <- as.character(fp$header_contains_all %||% character(0))
  header <- .header_fields(input$lines %||% character(0), template)
  hit <- tolower(need) %in% tolower(header)     # case-insensitive, like .pick()
  score <- sum(hit)
  # filename_regex is a TIE-BREAKER, never part of the score that decides
  # eligibility: a template must earn its min_score on CONTENT (header evidence)
  # alone, so it can never win on the file NAME with zero header match, and the
  # same bytes under a different name always detect the same way. Reported
  # separately (`fn`) and only used to separate otherwise-tied eligible candidates.
  fr <- fp$filename_regex
  fn <- if (!is.null(fr) && nzchar(fr) && !is.null(input$path) &&
            grepl(fr, basename(input$path), perl = TRUE)) 1L else 0L
  list(score = score, need = length(need), missing = need[!hit], fn = fn)
}

# .bank_on_page(input, template) -- 1 when the template's OWN BANK NAME is printed
# in the header or footer of the statement's first page, else 0. A TIE-BREAKER ONLY:
# like filename_regex it never contributes to the score that decides eligibility, so
# a template still has to earn its place on content alone.
#
# WHY IT EXISTS. Measured: a template built in the toolkit from a "Kowhai Bank of
# Aotearoa" statement matched four sibling statements PERFECTLY (3 of 3 phrases), and
# auto-detect picked anz_everyday_pdf on all four -- confidently, matched = TRUE.
# The shipped ANZ template also scored 3, on GENERIC COLUMN HEADINGS ("Withdrawals",
# "Deposits") that half the banks in the country print, and the next key was
# "shipped before hand-built". So a template an analyst builds could never win a
# tie, generic shipped fingerprints tie with nearly everything, and the one template
# that actually named the bank lost to one whose bank appears nowhere on the page.
# That is the shape of "33% auto-pick success with three templates of our own".
#
# It strictly IMPROVES the protection the shipped-first rule was written for (a
# hand-built "anz_v2" beating the tested Westpac template on nothing but its name):
# on a Westpac statement "Westpac" is on the page and "ANZ" is not, so the tested
# template still wins -- now for a reason that is about the document.
#
# HEADER AND FOOTER ONLY, not the whole page: a transaction reading "TRANSFER TO ANZ
# 01-..." on a Kowhai statement must not count as evidence the statement is ANZ's.
# Banks print their name in the letterhead and the legal footer; transactions sit in
# the middle. The edges are found by CONTENT, not by a line count: the header is what
# sits above the first line carrying a money figure, the footer what sits below the
# last one. A fixed "first 12 lines" window reached straight into the transactions
# on a short page -- a one-row statement put "TRANSFER TO ANZ 400.00" inside its own
# "header" and handed ANZ the tie.
.BANK_STOPWORDS <- c("bank", "banking", "nz", "new", "zealand", "of", "the", "and",
                     "limited", "ltd", "group", "corporation", "sample", "generic",
                     "statement", "statements", "aotearoa")
# .bank_tokens(template) -- the distinctive words of a template's bank name.
.bank_tokens <- function(template) {
  b <- tolower(as.character(template$bank %||% ""))
  toks <- unlist(strsplit(gsub("[^a-z0-9 ]", " ", b), "[[:space:]]+"))
  toks[nzchar(toks) & !toks %in% .BANK_STOPWORDS & nchar(toks) >= 3L]
}
# .page_edge_lines(input) -- the first page's header and footer lines (see above).
.page_edge_lines <- function(input) {
  pg <- as.character((input$pages %||% character(0))[1])
  if (is.na(pg) || !nzchar(pg)) pg <- paste(utils::head(input$lines %||% character(0), 15), collapse = "\n")
  ln <- trimws(gsub("[[:space:]]+", " ", unlist(strsplit(pg %||% "", "\n", fixed = TRUE))))
  ln <- ln[nzchar(ln)]
  if (!length(ln)) return(character(0))
  money <- which(grepl("[0-9][0-9,]*\\.[0-9]{2}\\b", ln, perl = TRUE))
  if (length(money)) {
    top <- ln[seq_len(money[1] - 1L)]
    bot <- ln[seq.int(max(money) + 1L, length.out = length(ln) - max(money))]
  } else {
    top <- ln; bot <- ln            # no figures at all (a cover page): its two ends
  }
  unique(c(utils::head(top, 12), utils::tail(bot, 6)))
}
.bank_on_page <- function(input, template) {
  toks <- .bank_tokens(template)
  if (!length(toks)) return(0L)
  edge <- tolower(paste(.page_edge_lines(input), collapse = " "))
  if (!nzchar(edge)) return(0L)
  hit <- vapply(toks, function(t) grepl(paste0("\\b", t, "\\b"), edge, perl = TRUE), logical(1))
  as.integer(all(hit))
}
# .bank_line_on_page(input, template) -> the header or footer line that prints the
# template's bank name, exactly as printed, or NA. It is the phrase to suggest when a
# template needs something only its bank prints: the analyst can see it on the page,
# and no other bank's statement carries it.
.bank_line_on_page <- function(input, template) {
  toks <- .bank_tokens(template)
  if (!length(toks)) return(NA_character_)
  ln <- .page_edge_lines(input)
  hit <- vapply(ln, function(l) all(vapply(toks, function(t)
    grepl(paste0("\\b", t, "\\b"), tolower(l), perl = TRUE), logical(1))), logical(1))
  if (!any(hit)) return(NA_character_)
  # the shortest such line is the cleanest phrase (a letterhead, not a sentence)
  cand <- ln[hit]; cand[which.min(nchar(cand))]
}

# detect_statement(input, templates, hint_bank, hint_type)
# -> list(template_id, score, matched, candidates, detail)
# matched TRUE only if best score >= min_score AND strictly > 2nd best.
# Hints are HARD filters on bank / statement_type.
detect_statement <- function(input, templates, hint_bank = NULL, hint_type = NULL) {
  ids <- names(templates)
  keep <- rep(TRUE, length(ids))
  if (!is.null(hint_bank) && nzchar(hint_bank)) {
    keep <- keep & vapply(ids, function(i)
      tolower(templates[[i]]$bank %||% "") == tolower(hint_bank), logical(1))
  }
  if (!is.null(hint_type) && nzchar(hint_type)) {
    keep <- keep & vapply(ids, function(i)
      tolower(templates[[i]]$statement_type %||% "") == tolower(hint_type), logical(1))
  }
  ids <- ids[keep]

  if (length(ids) == 0) {
    return(list(template_id = NA_character_, score = 0, matched = FALSE,
                candidates = data.frame(id = character(0), score = numeric(0),
                                        stringsAsFactors = FALSE),
                eligible_ids = character(0), tied = character(0),
                detail = "no templates match the supplied hints"))
  }

  sc <- lapply(ids, function(i) .score_template(input, templates[[i]]))
  scores <- vapply(sc, function(s) s$score, numeric(1))
  fns    <- vapply(sc, function(s) as.numeric(s$fn %||% 0L), numeric(1))
  mins   <- vapply(ids, function(i) templates[[i]]$min_score %||% 1, numeric(1))
  # SHIPPED BEFORE HAND-BUILT. A shipped template has a golden test proving it
  # reads a real statement of that bank correctly; a template built in the app has
  # not been through that. So when the evidence is equal, the tested one wins.
  # This used to fall through to the alphabetical id tie-break, which meant a
  # hand-built "anz_v2" quietly beat the tested "westpac_everyday_pdf" on nothing
  # but its name -- a template's FILENAME deciding which figures reach a dashboard.
  defaults <- vapply(ids, function(i)
    as.numeric(!identical(templates[[i]]$origin %||% "default", "user")), numeric(1))
  # is the template's own bank printed on this statement? (see .bank_on_page)
  bank_ev <- vapply(ids, function(i)
    as.numeric(safe(.bank_on_page(input, templates[[i]]), 0L)), numeric(1))
  # ...UNLESS THE HAND-BUILT ONE IS A CORRECTION OF THIS ONE. A template saved from
  # the toolkit after opening a shipped template to fix it carries `refines: <id>`.
  # It is not a rival that happens to score the same -- it exists BECAUSE the
  # shipped one was wrong on this bank, and it will tie with it on every statement
  # (same fingerprint, copied from it). Preferring the tested one there means the
  # analyst's fix can never take effect, and nothing on screen explains why. So a
  # refinement outranks exactly the template it names, and nothing else. Governance
  # is untouched: it is still origin "user", so it still cannot reach the dashboards
  # until somebody promotes it.
  # A refinement ranks one step ABOVE the single template it names -- not above
  # every shipped template, and not merely level with it (level is a tie, and a tie
  # is what stopped her fix taking effect in the first place).
  for (i in seq_along(ids)) {
    r <- templates[[ids[i]]]$refines %||% NULL
    if (!is.null(r) && r %in% ids) defaults[i] <- defaults[ids == r][1] + 1
  }
  # order by CONTENT score, then shipped-over-hand-built, then the filename
  # tie-breaker, then id (so the outcome is still fully deterministic).
  ord <- order(scores, bank_ev, defaults, fns, ids,
               decreasing = c(TRUE, TRUE, TRUE, TRUE, FALSE), method = "radix")
  ids <- ids[ord]; scores <- scores[ord]; fns <- fns[ord]; sc <- sc[ord]
  mins <- mins[ord]; defaults <- defaults[ord]; bank_ev <- bank_ev[ord]

  # Eligibility is per-candidate: a template is a genuine contender only when it
  # meets its OWN min_score. Ambiguity (best strictly > 2nd) is then judged among
  # eligible candidates only, so a template that failed its own threshold can no
  # longer create a false tie that blocks a genuinely-matching template.
  eligible <- scores >= mins
  best_id <- ids[1]           # overall top scorer (for "closest ..." reporting)
  best_score <- scores[1]
  best_min <- mins[1]
  detail_plain <- NULL        # set only where a customer-facing screen reads it

  if (any(eligible)) {
    e_ids <- ids[eligible]; e_scores <- scores[eligible]; e_mins <- mins[eligible]
    e_fns <- fns[eligible]; e_def <- defaults[eligible]; e_bank <- bank_ev[eligible]
    win_id <- e_ids[1]; win_score <- e_scores[1]; win_min <- e_mins[1]
    second <- if (length(e_scores) >= 2) e_scores[2] else -Inf
    second_fn <- if (length(e_fns) >= 2) e_fns[2] else -Inf
    second_def <- if (length(e_def) >= 2) e_def[2] else -Inf
    second_bank <- if (length(e_bank) >= 2) e_bank[2] else -Inf
    # Unambiguous when the winner's CONTENT score strictly beats the runner-up, OR
    # they tie on content and something PRINCIPLED separates them: a shipped
    # (tested) template over a hand-built one first, then the filename hint. A
    # shipped template drawing level with a hand-built one is not a real question -
    # take the tested one and get on with it, rather than stopping the analyst to
    # choose between a template with a golden test and one without.
    unambiguous <- (win_score > second) ||
                   (win_score == second && e_bank[1] > second_bank) ||
                   (win_score == second && e_bank[1] == second_bank && e_def[1] > second_def) ||
                   (win_score == second && e_bank[1] == second_bank &&
                    e_def[1] == second_def && e_fns[1] > second_fn)
    matched <- unambiguous
    if (matched) {
      detail <- sprintf("matched %s (score %s/%s)", win_id, win_score, win_min)
      # margin over the runner-up: a THIN margin (won by 1) means a near-duplicate
      # template nearly matched too, so downstream should treat it as needs-review.
      margin <- if (is.finite(second)) win_score - second else Inf
      # SETTLED BY THE BANK'S OWN NAME. The winner's bank is printed in the header or
      # footer and the runner-up's is not: the runner-up is another bank's template
      # that happens to share column headings, not a near-duplicate variant of this
      # one -- which is the only thing the thin-margin review hold exists to catch.
      # Measured in production as every statement read by a template built here
      # being held "please double-check it", because shipped templates share
      # "Withdrawals" / "Deposits" with nearly everything.
      bank_clear <- isTRUE(e_bank[1] > 0) && isTRUE(second_bank == 0)
      return(list(template_id = win_id, score = win_score, matched = TRUE,
                  margin = margin, bank_clear = bank_clear,
                  runner_up = if (length(e_ids) >= 2) e_ids[2] else NA_character_,
                  candidates = data.frame(id = ids, score = scores,
                                          stringsAsFactors = FALSE),
                  eligible_ids = e_ids, tied = character(0),
                  detail = detail))
    }
    detail <- sprintf("ambiguous: %s and %s both score %s",
                      win_id, e_ids[2], win_score)
  } else {
    miss <- sc[[1]]$missing
    detail <- sprintf("closest %s score %s/%s%s", best_id, best_score, best_min,
      if (length(miss)) sprintf(" (missing %s)",
        paste(sprintf("'%s'", miss), collapse = ", ")) else "")
    # The SAME evidence, for the person holding the statement. `detail` is the log
    # line -- template id, score, threshold -- and it reaches the customer-facing
    # Diagnostics table, where a template id and a fraction are exactly what the
    # operational guide tells the analyst to report as a bug. So the identical fact
    # is also written out in words: the template's human name, and the wording that
    # was not on the page. Nothing is hidden; the id and the score stay in the run
    # log and the audit record.
    detail_plain <- sprintf("The closest we have is the %s template, but this file doesn't print %s.",
      template_display_name(templates[[best_id]]),
      if (length(miss)) paste(sprintf("\"%s\"", miss), collapse = " or ")
      else "the wording it looks for")
  }

  list(
    template_id = best_id,
    score = best_score,
    matched = FALSE,
    margin = NA_real_,
    runner_up = if (length(ids) >= 2) ids[2] else NA_character_,
    candidates = data.frame(id = ids, score = scores, stringsAsFactors = FALSE),
    # eligible_ids -- every template that met its OWN min_score, best first. Each
    # of them fits the wording on the page, so if the chosen one turns out to read
    # nothing, these are the ones worth trying.
    eligible_ids = ids[eligible],
    # tied -- the eligible templates on the TOP score. Non-empty only when the
    # match failed BECAUSE two or more fit equally well, which is the opposite
    # problem from "we have never seen this layout" and needs opposite advice:
    # not "build a template" (a third would tie too) but "retire a duplicate".
    tied = if (sum(eligible) >= 2) ids[eligible][scores[eligible] == max(scores[eligible])]
           else character(0),
    detail = detail,
    # the same fact as `detail`, in words, for the screens (NULL where nothing
    # customer-facing reads it, and callers fall back to `detail`)
    detail_plain = detail_plain
  )
}

