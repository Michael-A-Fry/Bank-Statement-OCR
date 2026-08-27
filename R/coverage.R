# coverage.R -- a per-conversion field report. Per core field: populated, empty (mapped but every row
# blank - PRESENT BUT MISSING, usually a wrong column or band), unmapped, or partial.

# Is this canonical field wired in the template - including debit+credit mapping to amount?
.field_is_mapped <- function(template, field) {
  cols <- if (identical(template$format %||% "delimited", "pdf")) template$table$columns else template$columns
  if (is.null(cols)) return(FALSE)
  if (field == "amount") {
    return(!is.null(cols$amount) || (!is.null(cols$debit) && !is.null(cols$credit)))
  }
  !is.null(cols[[field]])
}

# Returns data.frame(field, mapped, populated, empty, n, verdict, note) over the core fields.
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
      empty = "Template maps this but every row is blank - likely the wrong column/box.",
      partial = sprintf("%d of %d rows blank - some statements just leave it empty.", n - pop, n),
      unmapped = "Not mapped - this statement/template doesn't include it.",
      "")
    data.frame(field = f, mapped = mapped, populated = pop, empty = n - pop,
               n = n, verdict = verdict, note = note, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# One-line human summary.
coverage_summary <- function(cov) {
  if (is.null(cov) || !nrow(cov)) return("no fields")
  pop <- sum(cov$verdict == "populated"); part <- sum(cov$verdict == "partial")
  emp <- cov$field[cov$verdict == "empty"]; unm <- sum(cov$verdict == "unmapped")
  parts <- sprintf("%d populated", pop)
  if (part) parts <- c(parts, sprintf("%d partial", part))
  if (length(emp)) parts <- c(parts, sprintf("%d present-but-empty (check: %s)",
                                             length(emp), paste(emp, collapse = ", ")))
  if (unm) parts <- c(parts, sprintf("%d not on this statement", unm))
  paste(parts, collapse = ", ")
}
