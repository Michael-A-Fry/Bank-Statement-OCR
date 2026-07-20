# extract_fields.R -- key-value ("mode: fields") extraction for form-style
# documents (IRD summaries, KiwiSaver/account summaries) where the useful data
# is labelled values, not a transaction table. This is the second extraction
# paradigm (build-contract side quest): the SAME declarative-template idea, but
# a template declares FIELDS (a label to find + optional regex) instead of
# columns, and the output is a named record.
#
# Deliberately standalone and simple: it does not touch the transaction pipeline
# (convert_statement / reconcile), so the core stays unchanged. Full wiring into
# a fields-mode output awaits a real IRD document to model against.
#
# extract_fields(input, template) -> data.frame(field, label, value, raw)

extract_fields <- function(input, template) {
  fields <- template$fields %||% list()
  pages <- input$pages %||% character(0)
  lines <- trimws(unlist(strsplit(paste(pages, collapse = "\n"), "\n", fixed = TRUE)))
  money_rx <- "-?\\$?-?[0-9][0-9,]*\\.[0-9]{2}"

  rows <- lapply(names(fields), function(fn) {
    spec <- fields[[fn]]
    label <- if (is.list(spec)) spec$label else as.character(spec)
    pat <- if (is.list(spec)) spec$pattern else NULL
    cand <- lines[grepl(label, lines, fixed = TRUE)]
    val <- NA_character_; raw <- NA_character_
    if (length(cand)) {
      if (!is.null(pat) && nzchar(pat)) {
        for (ln in cand) {
          m <- regmatches(ln, regexpr(pat, ln, perl = TRUE))
          if (length(m) && nzchar(m)) { val <- m; raw <- ln; break }
        }
      } else {
        # prefer a line that actually carries a money value (skips annotations),
        # then take the LAST money token on it (the value sits to the right).
        withmoney <- cand[grepl(money_rx, cand)]
        raw <- if (length(withmoney)) withmoney[1] else cand[1]
        m <- regmatches(raw, gregexpr(money_rx, raw))[[1]]
        if (length(m)) val <- m[length(m)]
      }
    }
    data.frame(field = fn, label = label, value = val, raw = raw, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
