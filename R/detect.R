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
  # order by CONTENT score, then shipped-over-hand-built, then the filename
  # tie-breaker, then id (so the outcome is still fully deterministic).
  ord <- order(scores, defaults, fns, ids,
               decreasing = c(TRUE, TRUE, TRUE, FALSE), method = "radix")
  ids <- ids[ord]; scores <- scores[ord]; fns <- fns[ord]; sc <- sc[ord]
  mins <- mins[ord]; defaults <- defaults[ord]

  # Eligibility is per-candidate: a template is a genuine contender only when it
  # meets its OWN min_score. Ambiguity (best strictly > 2nd) is then judged among
  # eligible candidates only, so a template that failed its own threshold can no
  # longer create a false tie that blocks a genuinely-matching template.
  eligible <- scores >= mins
  best_id <- ids[1]           # overall top scorer (for "closest ..." reporting)
  best_score <- scores[1]
  best_min <- mins[1]

  if (any(eligible)) {
    e_ids <- ids[eligible]; e_scores <- scores[eligible]; e_mins <- mins[eligible]
    e_fns <- fns[eligible]; e_def <- defaults[eligible]
    win_id <- e_ids[1]; win_score <- e_scores[1]; win_min <- e_mins[1]
    second <- if (length(e_scores) >= 2) e_scores[2] else -Inf
    second_fn <- if (length(e_fns) >= 2) e_fns[2] else -Inf
    second_def <- if (length(e_def) >= 2) e_def[2] else -Inf
    # Unambiguous when the winner's CONTENT score strictly beats the runner-up, OR
    # they tie on content and something PRINCIPLED separates them: a shipped
    # (tested) template over a hand-built one first, then the filename hint. A
    # shipped template drawing level with a hand-built one is not a real question -
    # take the tested one and get on with it, rather than stopping the analyst to
    # choose between a template with a golden test and one without.
    unambiguous <- (win_score > second) ||
                   (win_score == second && e_def[1] > second_def) ||
                   (win_score == second && e_def[1] == second_def && e_fns[1] > second_fn)
    matched <- unambiguous
    if (matched) {
      detail <- sprintf("matched %s (score %s/%s)", win_id, win_score, win_min)
      # margin over the runner-up: a THIN margin (won by 1) means a near-duplicate
      # template nearly matched too, so downstream should treat it as needs-review.
      margin <- if (is.finite(second)) win_score - second else Inf
      return(list(template_id = win_id, score = win_score, matched = TRUE,
                  margin = margin,
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
    detail = detail
  )
}

