# extract_fields.R -- key-value ("mode: fields") extraction for form-style
# documents (IRD summaries, KiwiSaver/account summaries) where the useful data
# is labelled values, not a transaction table. This is the second extraction
# paradigm (build-contract side quest): the SAME declarative-template idea, but
# a template declares FIELDS (labels to find) instead of columns.
#
# Each field is a label-matcher spec (see R/labels.R): synonyms via `any_of`,
# `occurrence` (first/last/all) for repeats, `where` to scope to a page,
# `required` to flag a mandatory-but-missing field, and value types
# (money/date/date_range/text/regex). A bare string or `{label: "..."}` still
# works (back-compat), and a field auto-inherits base-dictionary synonyms when
# its name matches a dictionary key -- so "opening_balance" understands
# "balance brought forward" etc. with no extra config.
#
# Deliberately standalone: it does not touch the transaction pipeline, so the
# core stays unchanged.
#
# extract_fields(input, template, dict)
#   -> data.frame(field, label, value, raw, matched, required, flagged, conflict)

extract_fields <- function(input, template, dict = default_label_dict()) {
  fields <- template$fields %||% list()
  pages <- input$pages %||% character(0)

  rows <- lapply(names(fields), function(fn) {
    spec <- fields[[fn]]
    if (is.character(spec)) spec <- list(any_of = spec)

    # Inherit synonyms from the base dictionary: an explicit `dict:` key, else
    # the field's own name if it matches a dictionary entry.
    key <- spec$dict %||% fn
    if (!is.null(dict[[key]])) {
      spec$any_of <- unique(c(.spec_terms(spec), .spec_terms(dict[[key]])))
      if (is.null(spec$value) && is.null(spec$pattern)) spec$value <- dict[[key]]$value
    }

    res <- match_label(spec, pages, dict)
    required <- isTRUE(spec$required)
    terms <- .spec_terms(spec)
    data.frame(
      field    = fn,
      label    = res$term %||% (if (length(terms)) terms[1] else fn),
      value    = res$value,
      raw      = res$raw,
      matched  = res$matched,
      required = required,
      flagged  = required && !res$matched,   # required-but-missing -> flag
      conflict = res$conflict,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
