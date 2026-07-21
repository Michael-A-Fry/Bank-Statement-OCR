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
    region <- spec$region %||% spec$at   # a positional value box, if given

    if (!is.null(region)) {
      # POSITIONAL extraction: the value is read from a box on the page, NOT by
      # its label -- for forms where the value and its label are in completely
      # separate / very different places. Text-typed by default (grab the box);
      # money/date/date_range/regex coerce the box contents.
      r <- .field_from_region(input$words %||% list(), region, spec$value %||% "text")
      res <- list(value = r$value, raw = r$raw, matched = r$matched,
                  term = NA_character_, conflict = FALSE)
      label <- fn
    } else {
      # LABEL-based extraction. Inherit synonyms from the base dictionary: an
      # explicit `dict:` key, else the field's own name if it matches an entry.
      key <- spec$dict %||% fn
      if (!is.null(dict[[key]])) {
        spec$any_of <- unique(c(.spec_terms(spec), .spec_terms(dict[[key]])))
        if (is.null(spec$value) && is.null(spec$pattern)) spec$value <- dict[[key]]$value
      }
      res <- match_label(spec, pages, dict)
      terms <- .spec_terms(spec)
      label <- res$term %||% (if (length(terms)) terms[1] else fn)
    }
    required <- isTRUE(spec$required)
    data.frame(
      field    = fn,
      label    = label,
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

# .field_from_region(words_by_page, region, vtype) -> list(value, raw, matched).
# Reads the text inside a page-box (PDF word coordinates, points) in reading
# order and coerces it to the requested value type. `region` is a list with
# page (default 1) + any of x_min/x_max/y_min/y_max. This is the label-free
# counterpart to match_label, for "the value is over HERE, wherever its label is".
.field_from_region <- function(words_by_page, region, vtype = "text") {
  none <- list(value = NA_character_, raw = NA_character_, matched = FALSE)
  if (is.null(region)) return(none)
  pg <- suppressWarnings(as.integer(region$page %||% 1)); if (is.na(pg) || pg < 1) pg <- 1L
  w <- if (pg <= length(words_by_page)) words_by_page[[pg]] else NULL
  if (is.null(w) || !nrow(w)) return(none)
  w <- as.data.frame(w, stringsAsFactors = FALSE)
  keep <- rep(TRUE, nrow(w))
  if (!is.null(region$x_min)) keep <- keep & (w$x + w$width) >= region$x_min
  if (!is.null(region$x_max)) keep <- keep & w$x <= region$x_max
  if (!is.null(region$y_min)) keep <- keep & (w$y + w$height) >= region$y_min
  if (!is.null(region$y_max)) keep <- keep & w$y <= region$y_max
  sel <- w[keep, , drop = FALSE]
  if (!nrow(sel)) return(none)
  sel <- sel[order(round(sel$y / 3), sel$x), , drop = FALSE]   # reading order
  raw <- paste(sel$text, collapse = " ")
  val <- if (identical(vtype, "text")) raw else .value_from_line(raw, vtype)
  list(value = if (is.na(val) || !nzchar(val)) NA_character_ else val,
       raw = raw, matched = !is.na(val) && nzchar(val))
}
