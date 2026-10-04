# coverage.R -- "have I set this up right? what's right, what's wrong, what's
# present but missing?" A per-conversion field report that answers exactly that.
#
# For each core field it says one of:
#   populated  -- the reading took it from a column AND real data came out (good)
#   empty      -- the reading took it from a column BUT every row is blank
#                 (PRESENT BUT MISSING: usually the wrong column -> the thing to fix)
#   unmapped   -- the reading did not take it from a column of its own (fine)
#   partial    -- read, some rows populated, some blank (worth a glance)
# This is deterministic and reads the reading's own columns, so it never guesses.
#
# The notes and the summary line are shown on screen to staff who are not
# engineers, so they are plain sentences: no "template" (retired at 2.0.0), no
# schema field names, and nothing that claims what the FILE holds when all that
# is known is what was READ.

# .field_is_mapped(template, field) -- is this canonical field wired in the
# template (delimited columns / pdf table.columns, incl. debit+credit -> amount)?
.field_is_mapped <- function(template, field) {
  cols <- if (identical(template$format %||% "delimited", "pdf")) template$table$columns else template$columns
  if (is.null(cols)) return(FALSE)
  if (field == "amount") {
    return(!is.null(cols$amount) || (!is.null(cols$debit) && !is.null(cols$credit)))
  }
  !is.null(cols[[field]])
}

# field_coverage(parsed, template) -> data.frame(field, mapped, populated, empty,
# n, verdict, note) over the reporting-relevant core fields.
field_coverage <- function(parsed, template) {
  tx <- parsed$transactions
  n <- if (is.null(tx)) 0L else nrow(tx)
  fields <- c("date", "description", "amount", "direction", "balance",
              "particulars", "code", "reference", "other_party", "type", "currency")
  rows <- lapply(fields, function(f) {
    v <- if (n && f %in% names(tx)) tx[[f]] else rep(NA, n)
    pop <- if (!n) 0L else sum(!is.na(v) & nzchar(trimws(as.character(v))))
    mapped <- .field_is_mapped(template, f) ||
      f %in% c("direction", "currency")   # derived, always "available"
    verdict <- if (!mapped) "unmapped"
      else if (pop == 0 && n > 0) "empty"
      else if (pop < n) "partial"
      else "populated"
    note <- switch(verdict,
      empty = "Read as a column, but every row is blank - the column may be in the wrong place.",
      partial = sprintf("%d of %d rows blank - some statements just leave it empty.", n - pop, n),
      unmapped = "Not printed on this statement, or printed inside another column.",
      "")
    data.frame(field = f, mapped = mapped, populated = pop, empty = n - pop,
               n = n, verdict = verdict, note = note, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# coverage_summary(cov) -> one-line summary in plain words, e.g.
# "8 fields read in full, 1 read as a column but empty (check: balance), 2 not
# read as a column of their own." Field names as people say them ("other party",
# never "other_party").
coverage_summary <- function(cov) {
  if (is.null(cov) || !nrow(cov)) return("No fields were read.")
  pop <- sum(cov$verdict == "populated"); part <- sum(cov$verdict == "partial")
  emp <- gsub("_", " ", cov$field[cov$verdict == "empty"]); unm <- sum(cov$verdict == "unmapped")
  parts <- sprintf("%d field(s) read in full", pop)
  if (part) parts <- c(parts, sprintf("%d with some rows blank", part))
  if (length(emp)) parts <- c(parts, sprintf("%d read as a column but empty (check: %s)",
                                             length(emp), paste(emp, collapse = ", ")))
  if (unm) parts <- c(parts, sprintf("%d not read as a column of their own", unm))
  paste0(paste(parts, collapse = ", "), ".")
}
