# detect.R -- deterministic bank/statement detection via fingerprint scoring.

# Via the shared locate_header helper, so detection and the reader never diverge about the header row.
.header_fields <- function(lines, template) {
  if (length(lines) == 0) return(character(0))
  hidx <- locate_header(lines, template)
  if (is.na(hidx)) return(character(0))
  # A template may declare SEVERAL separators. Resolved from the SAME header line read_delimited uses,
  # so detection and the reader cannot disagree about how the file splits.
  delim <- resolve_delimiter(lines[hidx], template)
  fields <- utils::read.table(text = lines[hidx], sep = delim, quote = "\"",
                              stringsAsFactors = FALSE, colClasses = "character",
                              header = FALSE, check.names = FALSE,
                              comment.char = "")[1, , drop = TRUE]
  trimws(as.character(unlist(fields)))
}

# Collapse every run of HORIZONTAL whitespace to a single space, leaving line breaks intact: a PDF page
# keeps pdftools' wide column padding while the drafter stores a phrase already collapsed, so a drafted
# template could not detect the file it came from. Folding line breaks would match across a boundary.
.fp_norm_ws <- function(x) {
  # useBytes throughout: a PDF's text layer is not always valid UTF-8 and a plain gsub ERRORS on one.
  # \\u00a0 is the non-breaking space some producers emit where a plain space is meant.
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
  # filename_regex is a TIE-BREAKER, never part of the eligibility score: a template must earn its
  # min_score on CONTENT alone, so the same bytes under a different name always detect alike.
  fr <- fp$filename_regex
  fn <- if (!is.null(fr) && nzchar(fr) && !is.null(input$path) &&
            grepl(fr, basename(input$path), perl = TRUE)) 1L else 0L
  list(score = score, need = length(need), missing = need[!hit], fn = fn)
}

# -> list(template_id, score, matched, candidates, detail). matched is TRUE only if the best score meets
# min_score AND strictly beats the second. Hints are HARD filters.
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
  # SHIPPED BEFORE HAND-BUILT: a shipped template has a golden test behind it, and the alphabetical id
  # tie-break let a hand-built "anz_v2" beat one on nothing but its name.
  defaults <- vapply(ids, function(i)
    as.numeric(!identical(templates[[i]]$origin %||% "default", "user")), numeric(1))
  # ...unless the hand-built one is a CORRECTION of this one. A template saved from the toolkit after
  # opening a shipped one carries `refines: <id>`: it exists BECAUSE the shipped one was wrong and it
  # ties on every statement, so preferring the tested one means the fix can never take effect. It ranks
  # one step ABOVE the template it names - level would be a tie, and a tie is what stopped the fix.
  for (i in seq_along(ids)) {
    r <- templates[[ids[i]]]$refines %||% NULL
    if (!is.null(r) && r %in% ids) defaults[i] <- defaults[ids == r][1] + 1
  }
  # Order by CONTENT score, then shipped-over-hand-built, then filename, then id: fully deterministic.
  ord <- order(scores, defaults, fns, ids,
               decreasing = c(TRUE, TRUE, TRUE, FALSE), method = "radix")
  ids <- ids[ord]; scores <- scores[ord]; fns <- fns[ord]; sc <- sc[ord]
  mins <- mins[ord]; defaults <- defaults[ord]

  # Eligibility is per-candidate - only a template meeting its OWN min_score is a contender - so one
  # that failed its own threshold cannot create a false tie.
  eligible <- scores >= mins
  best_id <- ids[1]           # overall top scorer (for "closest ..." reporting)
  best_score <- scores[1]
  best_min <- mins[1]
  detail_plain <- NULL        # set only where a customer-facing screen reads it

  if (any(eligible)) {
    e_ids <- ids[eligible]; e_scores <- scores[eligible]; e_mins <- mins[eligible]
    e_fns <- fns[eligible]; e_def <- defaults[eligible]
    win_id <- e_ids[1]; win_score <- e_scores[1]; win_min <- e_mins[1]
    second <- if (length(e_scores) >= 2) e_scores[2] else -Inf
    second_fn <- if (length(e_fns) >= 2) e_fns[2] else -Inf
    second_def <- if (length(e_def) >= 2) e_def[2] else -Inf
    # Unambiguous when the winner's CONTENT score strictly beats the runner-up, or they tie and something
    # principled separates them: a shipped template level with a hand-built one is not a real question.
    unambiguous <- (win_score > second) ||
                   (win_score == second && e_def[1] > second_def) ||
                   (win_score == second && e_def[1] == second_def && e_fns[1] > second_fn)
    matched <- unambiguous
    if (matched) {
      detail <- sprintf("matched %s (score %s/%s)", win_id, win_score, win_min)
      # Margin over the runner-up: a THIN margin means a near-duplicate nearly matched too.
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
    # The SAME evidence in words. `detail` is the log line - template id, score, threshold - and it
    # reaches the customer-facing Diagnostics table, where the guide says to report a raw code as a bug.
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
    # Every template that met its OWN min_score, best first: each fits the wording, so if the chosen one
    # reads nothing these are worth trying.
    eligible_ids = ids[eligible],
    # The eligible templates on the TOP score - non-empty only when the match failed BECAUSE two fit
    # equally well, which needs the opposite advice: not "build one" but "retire a duplicate".
    tied = if (sum(eligible) >= 2) ids[eligible][scores[eligible] == max(scores[eligible])]
           else character(0),
    detail = detail,
    # The same fact as `detail`, in words, for the screens.
    detail_plain = detail_plain
  )
}

