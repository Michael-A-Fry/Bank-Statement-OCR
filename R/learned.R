# learned.R -- the tool remembers which template an analyst chose for a layout, and
# suggests it the next time a statement laid out like that one arrives.
#
# Asked for as "learn from corrections: when someone changes a suggestion, remember
# that layout -> template, so the same kind of statement comes up correctly next
# time. The list would be visible to the Admin and clearable there."
#
# WHAT "THE SAME KIND OF STATEMENT" MEANS -- learned_key(). Three things, none of
# them about the customer:
#   1. the file's format (pdf / delimited / excel);
#   2. its layout signature (R/layout.R): the column-header words, hashed;
#   3. which KNOWN banks' names are printed in its header or footer -- the bank names
#      of the templates loaded, never any text off the statement.
# The third is not optional. For a PDF the signature is built from column headings,
# and two banks that print "Date Details Withdrawals Deposits Balance" share it --
# which is exactly the collision behind "33% auto-pick". Keyed on the signature
# alone, correcting one bank's statement would start mis-suggesting for the other.
# Nothing stored here can identify a customer: header words, bank names, a hash.
#
# WHAT IS LEARNED. Only a choice that CHANGED the answer (the analyst's template is
# not the one detection picked) and only once the conversion with it produced
# transactions -- a template forced onto a file it could not read is not a lesson.
#
# WHERE. One JSON file, by default beside the templates built here
# (templates/statements_user/_learned_choices.json): team knowledge, kept with the
# team's templates, and in the one folder an update never replaces. The loaders read
# *.yaml only, so it is invisible to them. One app process writes it; written aside
# and renamed into place, so a reader never sees half a file.

.LEARNED_COLS <- c("key", "template", "format", "hint", "banks", "by", "first", "last", "times")

# learned_key(input, format, templates) -> list(key, hint, banks)
learned_key <- function(input, format, templates) {
  sig <- safe(layout_signature(input), NULL)
  banks <- unlist(lapply(templates %||% list(), function(t)
    if (isTRUE(safe(.bank_on_page(input, t), 0L) == 1L)) as.character(t$bank %||% NA) else NULL))
  banks <- sort(unique(banks[!is.na(banks) & nzchar(banks)]))
  raw <- paste(as.character(format %||% ""), as.character(sig$signature %||% ""),
               paste(banks, collapse = "|"), sep = "\u001f")
  list(key = substr(.str_hash(raw), 1, 24),
       hint = as.character(sig$hint %||% NA_character_)[1],
       banks = banks)
}

.learned_empty <- function()
  data.frame(key = character(0), template = character(0), format = character(0),
             hint = character(0), banks = character(0), by = character(0),
             first = character(0), last = character(0), times = integer(0),
             stringsAsFactors = FALSE)

# learned_load(path) -> data.frame, one row per remembered layout (empty if none).
learned_load <- function(path) {
  if (is.null(path) || !length(path) || is.na(path) || !file.exists(path)) return(.learned_empty())
  x <- safe(jsonlite::fromJSON(path, simplifyDataFrame = TRUE), NULL)
  if (!is.data.frame(x) || !nrow(x)) return(.learned_empty())
  for (c in .LEARNED_COLS) if (is.null(x[[c]]))
    x[[c]] <- if (identical(c, "times")) rep(NA_integer_, nrow(x)) else rep(NA_character_, nrow(x))
  x$times <- suppressWarnings(as.integer(x$times))
  x[, .LEARNED_COLS, drop = FALSE]
}

.learned_write <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  jsonlite::write_json(df, tmp, auto_unbox = TRUE, pretty = TRUE, na = "null")
  if (!file.rename(tmp, path)) { file.copy(tmp, path, overwrite = TRUE); unlink(tmp) }
  invisible(df)
}

# learned_record(path, key, template, ...) -- remember (or reinforce) a choice. The
# same layout chosen again counts up; a DIFFERENT template for it replaces the old
# one and starts the count again, because the latest correction is the one to trust.
learned_record <- function(path, key, template, format = NA_character_, hint = NA_character_,
                           banks = character(0), by = NA_character_, now = Sys.time()) {
  if (is.null(key) || is.na(key) || !nzchar(key) || is.null(template) || is.na(template) ||
      !nzchar(template)) return(invisible(NULL))
  df <- learned_load(path)
  ts <- format(now, "%Y-%m-%dT%H:%M:%S")
  i <- match(key, df$key)
  if (is.na(i)) {
    df <- rbind(df, data.frame(key = key, template = template, format = as.character(format),
      hint = as.character(hint), banks = paste(banks, collapse = " | "), by = as.character(by),
      first = ts, last = ts, times = 1L, stringsAsFactors = FALSE))
  } else {
    if (!identical(df$template[i], template)) {
      df$template[i] <- template; df$first[i] <- ts; df$times[i] <- 0L
    }
    df$by[i] <- as.character(by); df$last[i] <- ts
    df$times[i] <- (df$times[i] %||% 0L) + 1L
  }
  .learned_write(df, path)
}

# learned_lookup(df, key, templates, format) -> the remembered template id, or NA.
# NA, too, when that template is no longer loaded or cannot read this kind of file:
# a memory of a deleted template is not a suggestion.
learned_lookup <- function(df, key, templates, format) {
  if (is.null(df) || !NROW(df) || is.null(key) || is.na(key)) return(NA_character_)
  i <- match(key, df$key)
  if (is.na(i)) return(NA_character_)
  t <- templates[[df$template[i]]]
  if (is.null(t) || !identical(t$format %||% "delimited", format)) return(NA_character_)
  df$template[i]
}

# learned_forget(path, key) -- the Admin's "Forget".
learned_forget <- function(path, key) {
  df <- learned_load(path)
  .learned_write(df[df$key != key, , drop = FALSE], path)
}
