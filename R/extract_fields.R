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
#   -> data.frame(field, label, value, other_values, raw, page, found_by,
#                 matched, required, flagged, conflict, n)
#
# WHAT EVERY COLUMN IS FOR, because a form has NO reconciliation behind it and
# these columns are the whole of its evidence:
#   conflict / other_values / n   the label was printed more than once. The tool
#       takes the first and these three say so IN THE FILE: how many times it was
#       read (`n`), and what the readings it did NOT take were (`other_values`).
#       `conflict` alone is a boolean nobody can act on -- it says a figure is
#       disputed without saying what disputes it, and the disputing figure is the
#       one thing a person holding the document can check in five seconds.
#   page / found_by               WHERE the value came from and HOW. The report
#       route's pairs have carried both for a long time (doc_pairs: page,
#       found_by = "its wording" / "its place on the page"); a form value carried
#       neither, so the two label-value paradigms produced different evidence for
#       the same kind of read. Same words, deliberately.

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
                  term = NA_character_, conflict = FALSE,
                  values = if (isTRUE(r$matched)) r$value else character(0),
                  n = if (isTRUE(r$matched)) 1L else 0L)
      label <- fn
      # A box IS a page number: it is the one thing the template had to declare.
      pg <- suppressWarnings(as.integer(region$page %||% 1L))
      page <- if (is.na(pg) || pg < 1L) 1L else pg
      found_by <- "its place on the page"
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
      page <- .field_page(pages, spec, res)
      found_by <- "its wording"
    }
    required <- isTRUE(spec$required)
    taken <- as.character(res$value %||% NA_character_)[1]
    seen <- as.character(res$values %||% character(0))
    # `occurrence: all` already puts every reading in the value itself, so there
    # is nothing left over to report and repeating them would only read as a
    # disagreement that is not there.
    other <- if (identical(spec$occurrence %||% "first", "all")) character(0)
             else setdiff(unique(seen[!is.na(seen) & nzchar(seen)]),
                          if (is.na(taken)) character(0) else taken)
    data.frame(
      field    = fn,
      label    = label,
      value    = res$value,
      # The readings this value BEAT. Empty on a clean field, so it costs a
      # clean form nothing and says the whole story on a disputed one.
      other_values = if (length(other)) paste(other, collapse = "; ") else "",
      raw      = res$raw,
      page     = as.integer(page),
      found_by = found_by,
      matched  = res$matched,
      required = required,
      flagged  = required && !res$matched,   # required-but-missing -> flag
      conflict = res$conflict,
      # How many times this label was read carrying a value (the count behind
      # `conflict`, and behind "we took the first of these").
      n        = as.integer(res$n %||% 0L),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# .field_scope_pages(n, where) -- the page NUMBERS match_label will search, the
# exact mirror of .pages_in_scope() (R/labels.R), which returns their TEXT.
# Two functions, one rule: if one changes the other has to, and a test pins them
# to the same answer.
.field_scope_pages <- function(n, where) {
  where <- where %||% "any"
  if (n == 0) return(integer(0))
  if (is.numeric(where)) {
    k <- as.integer(where)
    return(if (!is.na(k) && k >= 1 && k <= n) k else integer(0))
  }
  switch(as.character(where), page1 = 1L, last_page = as.integer(n), seq_len(n))
}

# .field_page(pages, spec, res) -> the page a matched value was read from, or NA.
#
# WHY IT IS DONE THIS WAY. match_label() joins every page in scope into ONE
# string before it looks for anything, so the page is lost inside it -- and that
# join is also what decides which line wins, so it cannot be taken apart without
# changing which FIGURE comes out. So the join is rebuilt here, line for line,
# with the page kept beside each line, and the label line match_label returned
# (`raw`) is looked up in it. Nothing about the value changes; the page is added
# beside it.
#
# The "x" is not a typo: strsplit() drops trailing empty fields, so a page
# ending in a newline would be counted one line short and every page after it
# would be named wrongly. Appending one non-newline character before counting
# keeps the empties, and the count then matches the joined split exactly.
.field_page <- function(pages, spec, res) {
  raw <- trimws(as.character(res$raw %||% NA_character_)[1])
  if (is.na(raw) || !nzchar(raw)) return(NA_integer_)
  idx <- .field_scope_pages(length(pages), spec$where)
  if (!length(idx)) return(NA_integer_)
  scope <- as.character(pages[idx])
  nl <- vapply(scope, function(p)
    length(strsplit(paste0(p, "x"), "\n", fixed = TRUE)[[1]]), integer(1))
  of_page <- rep(idx, nl)
  lines <- trimws(unlist(strsplit(paste(scope, collapse = "\n"), "\n", fixed = TRUE)))
  hit <- which(lines == raw)
  hit <- hit[hit <= length(of_page)]
  if (!length(hit)) return(NA_integer_)
  # The same line printed on two pages: which one was taken follows the same rule
  # match_label used to choose the value -- last for `occurrence: last`, and for a
  # conflict resolved with `on_conflict: last`; the first otherwise.
  last <- identical(spec$occurrence %||% "first", "last") ||
    (isTRUE(res$conflict) && identical(spec$on_conflict %||% "flag", "last"))
  as.integer(of_page[if (last) hit[length(hit)] else hit[1]])
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
