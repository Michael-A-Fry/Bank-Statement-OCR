# detect.R -- deterministic bank/statement detection via fingerprint scoring.

# .header_fields(lines, template) -- locate the header row (via the shared
# locate_header helper so detection and the reader never diverge) and split it
# into trimmed field names.
.header_fields <- function(lines, template) {
  if (length(lines) == 0) return(character(0))
  delim <- template$delimiter %||% ","
  hidx <- locate_header(lines, template)
  if (is.na(hidx)) return(character(0))
  fields <- utils::read.table(text = lines[hidx], sep = delim, quote = "\"",
                              stringsAsFactors = FALSE, colClasses = "character",
                              header = FALSE, check.names = FALSE,
                              comment.char = "")[1, , drop = TRUE]
  trimws(as.character(unlist(fields)))
}

# .score_template(input, template) -- fingerprint score for one template.
.score_template <- function(input, template) {
  fp <- template$fingerprint
  # PDF templates fingerprint on page text, not delimited headers.
  if (identical(template$format %||% "delimited", "pdf")) {
    need <- as.character(fp$page_contains_all %||% character(0))
    hay <- paste(input$pages %||% character(0), collapse = "\n")
    hits <- vapply(need, function(ph) grepl(ph, hay, fixed = TRUE), logical(1))
    return(list(score = sum(hits), need = length(need), missing = need[!hits]))
  }
  # Excel templates fingerprint on the sheet's column names.
  if (identical(template$format %||% "delimited", "excel")) {
    need <- as.character(fp$header_contains_all %||% character(0))
    header <- names(input$table %||% list())
    return(list(score = sum(need %in% header), need = length(need),
                missing = need[!(need %in% header)]))
  }
  need <- as.character(fp$header_contains_all %||% character(0))
  header <- .header_fields(input$lines %||% character(0), template)
  present <- sum(need %in% header)
  score <- present
  fr <- fp$filename_regex
  if (!is.null(fr) && nzchar(fr)) {
    if (grepl(fr, basename(input$path), perl = TRUE)) score <- score + 1
  }
  missing <- need[!(need %in% header)]
  list(score = score, need = length(need), missing = missing)
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
                detail = "no templates match the supplied hints"))
  }

  sc <- lapply(ids, function(i) .score_template(input, templates[[i]]))
  scores <- vapply(sc, function(s) s$score, numeric(1))
  mins   <- vapply(ids, function(i) templates[[i]]$min_score %||% 1, numeric(1))
  ord <- order(scores, ids, decreasing = c(TRUE, FALSE), method = "radix")
  ids <- ids[ord]; scores <- scores[ord]; sc <- sc[ord]; mins <- mins[ord]

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
    win_id <- e_ids[1]; win_score <- e_scores[1]; win_min <- e_mins[1]
    second <- if (length(e_scores) >= 2) e_scores[2] else -Inf
    unambiguous <- win_score > second
    matched <- unambiguous
    if (matched) {
      detail <- sprintf("matched %s (score %s/%s)", win_id, win_score, win_min)
      return(list(template_id = win_id, score = win_score, matched = TRUE,
                  candidates = data.frame(id = ids, score = scores,
                                          stringsAsFactors = FALSE),
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
    candidates = data.frame(id = ids, score = scores, stringsAsFactors = FALSE),
    detail = detail
  )
}
