# detect.R -- deterministic bank/statement detection via fingerprint scoring.

# .header_fields(lines, template) -- locate the header row (honouring an
# optional preamble.header_regex) and split it into trimmed field names.
.header_fields <- function(lines, template) {
  if (length(lines) == 0) return(character(0))
  delim <- template$delimiter %||% ","
  hidx <- 1L
  hr <- template$preamble$header_regex
  if (!is.null(hr) && nzchar(hr)) {
    m <- grep(hr, lines, perl = TRUE)
    if (length(m)) hidx <- m[1] else return(character(0))
  } else {
    # first non-empty line
    nz <- which(nzchar(trimws(lines)))
    if (length(nz)) hidx <- nz[1]
  }
  fields <- utils::read.table(text = lines[hidx], sep = delim, quote = "\"",
                              stringsAsFactors = FALSE, colClasses = "character",
                              header = FALSE, check.names = FALSE,
                              comment.char = "")[1, , drop = TRUE]
  trimws(as.character(unlist(fields)))
}

# .score_template(input, template) -- fingerprint score for one template.
.score_template <- function(input, template) {
  fp <- template$fingerprint
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
  ord <- order(scores, ids, decreasing = c(TRUE, FALSE), method = "radix")
  ids <- ids[ord]; scores <- scores[ord]; sc <- sc[ord]

  best_id <- ids[1]
  best_score <- scores[1]
  min_score <- templates[[best_id]]$min_score %||% 1
  second <- if (length(scores) >= 2) scores[2] else -Inf
  unambiguous <- best_score > second
  matched <- (best_score >= min_score) && unambiguous

  detail <- if (matched) {
    sprintf("matched %s (score %s/%s)", best_id, best_score, min_score)
  } else if (best_score < min_score) {
    miss <- sc[[1]]$missing
    sprintf("closest %s score %s/%s%s", best_id, best_score, min_score,
            if (length(miss)) sprintf(" (missing %s)",
              paste(sprintf("'%s'", miss), collapse = ", ")) else "")
  } else {
    sprintf("ambiguous: %s and %s both score %s", best_id, ids[2], best_score)
  }

  list(
    template_id = best_id,
    score = best_score,
    matched = matched,
    candidates = data.frame(id = ids, score = scores, stringsAsFactors = FALSE),
    detail = detail
  )
}
