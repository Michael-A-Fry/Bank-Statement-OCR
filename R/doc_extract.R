# doc_extract.R -- the "document" template mode: ONE template per kind of
# document, carrying every table on it and every label/value pair, and the
# orchestration that turns an upload into files you can download.
#
# THREE MODES NOW, AND WHY THEY ARE SEPARATE.
#   statement (R/parse_pdf_table.R)  one table of transactions, checked against a
#                                    running balance, fed to the dashboards.
#   fields    (R/forms.R)            labelled values found by their WORDING. No
#                                    coordinates at all -- an IRD form's labels
#                                    are printed the same way every time.
#   document  (this file)            many tables of different shapes plus
#                                    label/value pairs, found by wording where
#                                    that works and by position where it does not.
#
# They share the fingerprint gate and nothing else, deliberately. The statement
# path is the one with money on it and it is not going to be destabilised by this.
#
# NO ROUTE TO THE FEED. A document template produces files to download and
# nothing more. write_feed() refuses `kind = "tables"` outright, so this cannot
# reach a dashboard even by accident: there is no reconciliation behind these
# numbers and nothing that could tell a wrong one from a right one, so publishing
# them as if there were would be the worst thing this tool could do.

# There is no is_document_template(). `mode: document` is still the discriminator,
# and template_kind() (R/templates.R) is what reads it -- one function answering
# "what kind of template is this" for all three kinds, which is what every screen,
# every save and every dispatch already asks. This was a second way to ask it that
# nothing called, and it answered a slightly different question (it also demanded
# at least one table or pair), so the two could disagree about the same file.

# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

.doc_box_problems <- function(box, where) {
  if (!is.list(box)) return(sprintf("%s is not a box", where))
  need <- c("x_min", "x_max", "y_min", "y_max")
  miss <- need[vapply(need, function(k) is.na(.doc_num(box[[k]])), logical(1))]
  if (length(miss))
    return(sprintf("%s is missing %s", where, paste(miss, collapse = "/")))
  if (.doc_num(box$x_min) >= .doc_num(box$x_max))
    return(sprintf("%s has no width (x_min is not left of x_max)", where))
  if (.doc_num(box$y_min) >= .doc_num(box$y_max))
    return(sprintf("%s has no height (y_min is not above y_max)", where))
  character(0)
}

# .doc_table_problems(tab, key) -- everything that would make this table read
# nothing, or read the wrong thing, said as the thing to go and fix.
.doc_table_problems <- function(tab, key) {
  p <- character(0)
  w <- function(...) sprintf("table '%s': %s", key, sprintf(...))
  if (!is.list(tab)) return(w("is not a mapping"))
  sp <- .doc_int(tab$start$page)
  if (is.na(sp) || sp < 1L) p <- c(p, w("start.page must be a page number"))
  ep <- .doc_int(tab$end$page, sp)
  if (!is.na(sp) && !is.na(ep) && ep < sp)
    p <- c(p, w("it ends on page %d, before it starts on page %d", ep, sp))
  cols <- .doc_columns(tab)
  if (!length(cols)) p <- c(p, w("has no columns drawn on it"))
  for (j in seq_along(cols)) {
    lo <- .doc_num(cols[[j]]$x_min); hi <- .doc_num(cols[[j]]$x_max)
    if (is.na(lo) || is.na(hi))
      p <- c(p, w("column %d has no left/right edge", j))
    else if (lo >= hi)
      p <- c(p, w("column %d has no width (x_min %.1f is not left of x_max %.1f)", j, lo, hi))
    ty <- as.character(cols[[j]]$type %||% "auto")[1]
    if (!(ty %in% .DOC_TYPES))
      p <- c(p, w("column %d has kind '%s', which is not one of %s", j, ty,
                  paste(.DOC_TYPES, collapse = "/")))
  }
  # OVERLAPPING BANDS ARE A SILENT DATA LOSS. A word can only land in one column,
  # and the earlier band wins -- so an overlap does not duplicate a value, it
  # DELETES it from the second column, and every row still looks complete.
  if (length(cols) > 1L) {
    ord <- order(vapply(cols, function(cc) .doc_num(cc$x_min, Inf), numeric(1)))
    cs <- cols[ord]
    for (j in seq_len(length(cs) - 1L)) {
      hi <- .doc_num(cs[[j]]$x_max, NA_real_); lo <- .doc_num(cs[[j + 1]]$x_min, NA_real_)
      if (!is.na(hi) && !is.na(lo) && lo < hi)
        p <- c(p, w("columns '%s' and '%s' overlap, so whatever falls in the overlap is read into the first and lost from the second",
                    as.character(cs[[j]]$name %||% j), as.character(cs[[j + 1]]$name %||% (j + 1))))
    }
  }
  mf <- .doc_num(tab$min_fill, 0.5)
  if (is.na(mf) || mf < 0 || mf > 1)
    p <- c(p, w("min_fill must be between 0 and 1"))
  hr <- .doc_int(tab$header_rows, 1L)
  if (is.na(hr) || hr < 0L) p <- c(p, w("header_rows must be 0 or more"))
  # doc_pages is the length of the document this table was DRAWN on, and it is
  # what tells the reader that a positional match on a document of a different
  # length is worth less (R/tables.R reads the table's own value first, then the
  # template's). A per-table override is allowed -- a template grown over several
  # examples has tables drawn on documents of different lengths -- so it is
  # checked here rather than silently ignored.
  if (!is.null(.doc_key(tab, "doc_pages"))) {
    dp <- .doc_int(tab$doc_pages)
    if (is.na(dp) || dp < 1L)
      p <- c(p, w("doc_pages must be the number of pages the example had"))
  }
  # A KEY NOTHING IMPLEMENTS IS A SILENT LOSS, NOT A FUTURE FEATURE.
  #
  # `occurrence: all` is meant to say "read this table wherever it appears, however
  # many times", and the design for it is settled (register A2). The reader for it
  # is NOT built: there is no doc_locate_repeats in R/tables.R, so a table carrying
  # the key is located exactly once, at the coordinates it was drawn at - while
  # .doc_table_optional() reads the same key to mean its absence is expected and
  # must not be reported. Between them, a template that used the key would lose
  # tables and say nothing, which is the cardinal failure of this product wearing
  # a feature's name.
  #
  # So it is refused at the door until the reader exists. The refusal names the
  # reader, so whoever builds it knows exactly which line to delete. Anything other
  # than `once`/`first` is refused too, rather than silently meaning `once`.
  occ <- as.character(.doc_key(tab, "occurrence") %||% "")[1]
  if (nzchar(occ)) {
    if (identical(occ, "all")) {
      if (!exists("doc_locate_repeats", mode = "function"))
        p <- c(p, w(paste("is set to be read wherever it appears, and the reader for",
                          "that is not built yet - it would be read once, where it was",
                          "drawn, and its absence anywhere else would go unreported.",
                          "Remove 'occurrence: all' until doc_locate_repeats() exists.")))
    } else if (!(occ %in% c("once", "first"))) {
      p <- c(p, w("occurrence must be 'once' or 'first' (or 'all' once that is built), not '%s'", occ))
    }
  }
  p
}

# .doc_pii_problems(tabs, prs) -- THE SAME PII GATE THE FINGERPRINT PASSES,
# applied to the two other places this mode captures page text VERBATIM.
#
# WHY IT MATTERS MORE HERE THAN ANYWHERE. The builder's FIRST gesture is "drag a
# box round the table's TITLE", and whatever words are in that box become the
# table's name. A pair's label_text is captured the same way. Both are written
# into templates/documents_user/, which is exactly what gets copied off the box
# when somebody backs the library up or hands it to a colleague.
#
# Measured before this existed: "Prepared for Mr John Smith" was REFUSED as a
# fingerprint phrase and accepted without a word as a table name. One rule, one
# wording, three places - so there is one thing to learn and not three.
.doc_pii_problems <- function(tabs, prs) {
  if (!exists(".fp_has_pii", mode = "function")) return(character(0))
  txt <- c(
    vapply(tabs %||% list(), function(tb)
      as.character(.doc_key(tb, "name") %||% "")[1], character(1)),
    names(tabs) %||% character(0),
    vapply(prs %||% list(), function(pr)
      as.character(.doc_key(pr, "label_text") %||% "")[1], character(1)))
  txt <- trimws(txt[!is.na(txt) & nzchar(trimws(txt))])
  if (!length(txt)) return(character(0))
  hit <- safe(.fp_has_pii(txt), rep(FALSE, length(txt)))
  if (!any(hit)) return(character(0))
  sprintf(paste0(
    "%s names a PERSON -- a template must describe the LAYOUT, never a customer. ",
    "Use the heading printed on the page instead."),
    paste(sprintf("'%s'", unique(txt[hit])), collapse = ", "))
}

# validate_document_template(t) -> character() of problems (empty = valid).
validate_document_template <- function(t) {
  p <- character(0)
  if (!is.list(t)) return("template is not a mapping")
  if (is.null(t$id) || !nzchar(trimws(as.character(t$id)))) p <- c(p, "missing 'id'")
  if (!identical(t$mode %||% "", "document")) p <- c(p, "mode must be 'document'")
  tabs <- t$tables %||% list(); prs <- t$pairs %||% list()
  if (!length(tabs) && !length(prs))
    p <- c(p, "this template finds nothing: add at least one table or one value")
  # The SAME fingerprint gate the other two modes pass. It matters as much here:
  # detection is all-phrases-must-appear with no reconciliation behind it, so a
  # fingerprint of words that sit on every report would extract one document with
  # another's column bands and present the result as a clean read.
  fp <- t$fingerprint$page_contains_all
  if (is.null(fp) || !length(fp))
    p <- c(p, paste0("fingerprint.page_contains_all is required: without identifying ",
                     "phrases this template can never be matched"))
  else p <- c(p, .fp_fingerprint_problems(fp, min_score = length(fp), fmt = "pdf"))

  p <- c(p, .doc_pii_problems(tabs, prs))

  keys <- names(tabs)
  if (length(tabs) && (is.null(keys) || any(!nzchar(keys))))
    p <- c(p, "every table needs a name")
  for (k in keys) p <- c(p, .doc_table_problems(tabs[[k]], k))
  for (k in names(prs)) {
    sp <- prs[[k]]
    if (!is.list(sp)) { p <- c(p, sprintf("value '%s' is not a mapping", k)); next }
    # THREE WAYS TO FIND A VALUE, and a template may use any of them:
    #   a value box + a label box   read by wording, placed by the offset between
    #   a value box alone           read from that spot on the page
    #   a label wording alone       read by the label matcher, no coordinates
    # So a missing value box is only a fault when there is no wording either --
    # that is a value the template can never find, which is the thing worth
    # refusing.
    vbox <- .doc_key(sp, "value"); lbox <- .doc_key(sp, "label")
    has_box <- is.list(vbox) && !is.na(.doc_num(.doc_key(vbox, "x_min")))
    has_word <- nzchar(trimws(as.character(.doc_key(sp, "label_text") %||% "")))
    if (!has_box && !has_word)
      p <- c(p, sprintf(paste("value '%s' has neither a box nor any wording to find",
                              "it by, so it can never be read"), k))
    if (has_box) p <- c(p, .doc_box_problems(vbox, sprintf("value '%s'", k)))
    if (!is.null(lbox)) p <- c(p, .doc_box_problems(lbox, sprintf("value '%s' label", k)))
    ty <- as.character(.doc_key(sp, "type") %||% "text")[1]
    if (!(ty %in% c("text", "money", "date", "date_range")))
      p <- c(p, sprintf("value '%s' has kind '%s', which is not one of text/money/date/date_range", k, ty))
    wr <- .doc_key(sp, "where"); wr <- if (is.list(wr)) .doc_key(wr, "where") else wr
    wr <- as.character(wr %||% "")[1]
    if (nzchar(wr) && !(wr %in% c("right", "left", "above", "below")))
      p <- c(p, sprintf(paste("value '%s' says the value sits '%s' of the label,",
                              "which is not one of right/left/above/below"), k, wr))
  }
  # Worksheet names come from the table names, and two tables that differ only in
  # punctuation would collide there. Caught here, where it can be renamed.
  if (length(tabs) > 1L) {
    nm <- .doc_norm(vapply(tabs, function(x) as.character(x$name %||% "")[1], character(1)))
    dup <- unique(nm[duplicated(nm) & nzchar(nm)])
    if (length(dup))
      p <- c(p, sprintf("two tables are called the same thing (%s) - they would land on one sheet",
                        paste(dup, collapse = ", ")))
  }
  p
}

# ---------------------------------------------------------------------------
# Load / save / detect
# ---------------------------------------------------------------------------

# load_document_templates(dir, user_dir) -> named list<template>, keyed by id.
# Lenient like the form loader (one bad template never breaks the others) and,
# like it, never SILENT: the skip reasons come back on attr(x, "load_errors").
#
# include_hidden mirrors load_template_set() and load_fields_templates(): a
# template flagged `hidden: true` is parked out of detection without being
# deleted, and only the Admin management view asks for them back.
load_document_templates <- function(dir = "templates/documents", user_dir = NULL,
                                    include_hidden = FALSE) {
  out <- list(); errors <- character(0)
  dirs <- c(dir, user_dir)
  # Shipped or built here -- see load_fields_templates() for why this is stamped.
  origins <- c(rep("default", length(dir)), rep("user", length(user_dir)))
  for (k in seq_along(dirs)) {
    d <- dirs[[k]]
    if (is.null(d) || !dir.exists(d)) next
    for (f in list.files(d, pattern = "\\.ya?ml$", full.names = TRUE)) {
      t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
      if (is.null(t) || !identical(t$mode %||% "", "document") || is.null(t$id)) next
      if (isTRUE(t$hidden) && !include_hidden) next
      probs <- validate_document_template(t)
      if (length(probs)) {
        errors <- c(errors, sprintf("%s: %s", t$id, paste(probs, collapse = "; ")))
        next
      }
      t$origin <- origins[[k]]
      if (is.null(out[[t$id]])) out[[t$id]] <- t
    }
  }
  attr(out, "load_errors") <- errors
  out
}

# save_document_template(t, dir) -> path. Validates first: an invalid template is
# never written, so the loader's leniency never has to cover for this screen.
save_document_template <- function(t, dir = "templates/documents_user") {
  .save_template_yaml(t, dir, validate_document_template, "document template")
}

# There is no document_template_ids() / delete_document_template(). There was a
# pair, written "so Admin can manage these too", and Admin never used it: it
# resolves the folder for whichever kind the id belongs to and then calls the
# kind-agnostic user_template_ids() / delete_user_template() (R/templates.R), which
# walk whatever directory they are handed. That pair is also strictly the more
# capable one -- delete_user_template() matches on the sanitised FILENAME as well
# as the id field, so a report template whose id was hand-edited in the YAML box is
# deletable through the live path and was not deletable through this one.

# .doc_fp_norm(x) -- ONE rule for comparing a typed phrase against a printed page.
#
# The two levels of this mode used to compare text differently: the fingerprint
# matched with grepl(fixed = TRUE) -- case, spacing and punctuation all
# significant -- while the header matcher normalised everything away. Measured
# against a page printing "QUARTERLY PORTFOLIO REPORT": the phrase typed as
# "Quarterly Portfolio Report" NEVER matched, and nothing on screen said why.
# That is almost certainly the original complaint, "I put a phrase printed on it,
# bang smack on front page, still used another template".
#
# So both sides are folded the same way: case, punctuation and runs of spaces
# stop mattering. LINE BREAKS DELIBERATELY DO NOT FOLD. Collapsing them too would
# let a phrase match across a line boundary, which makes detection LOOSER -- the
# wrong direction for a fail-closed engine.
#
# Byte-wise and wrapped in safe(): a PDF's text layer is not always valid UTF-8
# and detection must never crash on a file it merely reads badly.
.doc_fp_norm <- function(x) {
  x <- as.character(x %||% "")
  safe({
    x <- gsub("\u00a0", " ", x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("([A-Z])", "\\L\\1", x, perl = TRUE, useBytes = TRUE)
    x <- gsub("[^a-z0-9\n]+", " ", x, useBytes = TRUE)
    x <- gsub("[ \t]+", " ", x, useBytes = TRUE)
    x <- gsub(" *\n *", "\n", x, useBytes = TRUE)
    gsub("^[ \n]+|[ \n]+$", "", x, useBytes = TRUE)
  }, x)
}

# .doc_tpl_name(t) -- a report template said in words. template_display_name()
# appends "statement", which is exactly what these are not.
.doc_tpl_name <- function(t) {
  lab <- trimws(paste(trimws(as.character(t$bank %||% "")),
                      trimws(as.character(t$statement_type %||% ""))))
  if (nzchar(lab)) lab else as.character(t$id %||% "this template")[1]
}

# detect_document_template(input, dtemplates) -> THE SAME RETURN SHAPE
# detect_statement() has: template_id, matched, score, candidates, eligible_ids,
# tied, margin, runner_up, detail, detail_plain.
#
# The rule, and why each step of it is what it is, is .detect_by_fingerprint() in
# R/templates.R -- ONE detector for both of the other routes. It used to be
# eighty-five lines here and the same eighty-five lines in R/forms.R, which is how
# a fix to one route became a drift in the other. The only thing this route still
# owns is HOW ITS TEXT IS FOLDED (.doc_fp_norm) and how a report template is said
# in words (.doc_tpl_name).
detect_document_template <- function(input, dtemplates) {
  .detect_by_fingerprint(input, dtemplates, "document", .doc_fp_norm, .doc_tpl_name)
}

# ---------------------------------------------------------------------------
# Extraction
# ---------------------------------------------------------------------------

# extract_document(input, tmpl) -> list(tables, pairs, summary, report, long).
#
#   tables   named list, one per template table: name, rows, report, spilled,
#            anchor, confidence, detail, pages
#   pairs    one data.frame of every label/value pair (see doc_pairs)
#   summary  ONE ROW PER TABLE: how it was found, how sure, how many rows, how
#            many columns came out thin, how many words no column claimed. This
#            is the whole quality picture on one screen, and it is the thing to
#            read before trusting any of the numbers underneath it.
#   long     every table stacked into one long frame (table/name/page/row/
#            column/value) -- the shape that lets tables of different widths
#            live in one file.
#   unclaimed every word that fell inside a table's boundary and into no column
#            (table/page/x/y/text). See document_unclaimed().
#   misses   the keys of the tables this document does not carry.
#
# ABSENT IS NOT EMPTY. A table the reader could find no evidence of anywhere
# (`anchor = "not_found"`) gets NO entry here at all -- no sheet, no long rows, no
# per-column report -- rather than an entry full of zeroes that reads as "this
# table is here and has nothing in it". Those two are different facts and a
# spreadsheet cannot tell them apart. It is not silent: the key goes into `misses`
# and document_summary() prints one row per miss saying it is not on this copy.
extract_document <- function(input, tmpl) {
  tabs <- tmpl$tables %||% list()
  keys <- names(tabs) %||% character(0)
  out <- list(); misses <- character(0)
  for (k in keys) {
    tab <- tabs[[k]]
    loc <- doc_locate_table(input, tab, tmpl)
    if (identical(as.character(loc$anchor %||% "")[1], "not_found")) {
      misses <- c(misses, k)
      next
    }
    r <- doc_table_rows(input, tab, tmpl, loc)
    r$key <- k
    r$name <- as.character(tab$name %||% k)[1]
    out[[k]] <- r
  }
  pairs <- doc_pairs(input, tmpl)
  list(tables = out, pairs = pairs, misses = misses,
       summary = document_summary(out, misses, tabs),
       report = document_column_report(out),
       long = document_long(out),
       unclaimed = document_unclaimed(out))
}

# .doc_table_optional(tab) -- TRUE when this table is one the document is not
# expected to always carry, so its absence is not a fault.
#
# Two ways to say it, and the second is not this file's to invent: `optional: true`
# on the table, or a table set to be read wherever it appears (`occurrence: all`),
# where three-this-month-and-seven-next means absence is the ordinary case and a
# permanent needs_review is a warning nobody reads.
.doc_table_optional <- function(tab) {
  isTRUE(tab$optional) ||
    identical(as.character(tab$occurrence %||% "once")[1], "all")
}

# document_summary(tables, misses, specs) -> the one-row-per-table picture
# described above, plus one row per table that is NOT on this document.
document_summary <- function(tables, misses = character(0), specs = list()) {
  # AN EMPTY SUMMARY HAS TO HAVE THE SAME COLUMN TYPES AS A FULL ONE.
  #
  # It used to be ten character(0) columns, which reads as harmless and is not: a
  # template with values on it and no tables is a real thing to build (a form),
  # and every caller that adds up a number then meets sum(character(0)) --
  # "invalid 'type' (character) of argument". That is what "Read the whole
  # document" answered with when somebody had drawn only label/value pairs. A
  # zero-row frame is a shape, and a shape that changes with the row count is not
  # one.
  proto <- list(table = character(0), name = character(0), pages = character(0),
                rows = integer(0), columns = integer(0), thin_columns = integer(0),
                unclaimed_words = integer(0), found_by = character(0),
                confidence = numeric(0), note = character(0))
  miss_rows <- lapply(as.character(misses %||% character(0)), function(k) {
    tb <- specs[[k]]
    data.frame(table = k,
               name = as.character((tb$name %||% k))[1],
               pages = "", rows = 0L, columns = 0L, thin_columns = 0L,
               unclaimed_words = 0L, found_by = "not on this document",
               confidence = NA_real_,
               note = if (.doc_table_optional(tb %||% list()))
                        "this table is not on this copy, and it is set to be read wherever it appears"
                      else "this table is not on this copy",
               stringsAsFactors = FALSE)
  })
  if (!length(tables))
    return(if (length(miss_rows)) {
      out <- do.call(rbind, miss_rows); rownames(out) <- NULL; out
    } else as.data.frame(proto, stringsAsFactors = FALSE))
  found_words <- c(header = "its heading", first_column = "the rows it starts with",
                   position_same_page = "where it sat on the example",
                   position_other_page = "where it sat on the example (different length document)")
  rows <- lapply(names(tables), function(k) {
    r <- tables[[k]]
    # match(), not [[ ]]: indexing a named vector by a name it does not carry is
    # an ERROR, and "" is exactly what an unset anchor gives.
    fb <- unname(found_words[match(as.character(r$anchor %||% "")[1], names(found_words))])
    data.frame(table = k, name = r$name %||% k,
               pages = paste(unique(r$pages %||% integer(0)), collapse = ", "),
               rows = as.integer(r$n_rows %||% 0L),
               columns = as.integer(nrow(r$report %||% data.frame())),
               thin_columns = as.integer(sum(r$report$low_fill %in% TRUE)),
               unclaimed_words = as.integer(nrow(r$spilled %||% data.frame())),
               found_by = if (is.null(fb) || is.na(fb)) "not found" else fb,
               confidence = round(as.numeric(r$confidence %||% NA_real_), 2),
               note = as.character(r$detail %||% ""),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(rows, miss_rows)); rownames(out) <- NULL; out
}

# document_unclaimed(tables) -> every word that fell inside a table's boundary and
# into NO column: table, page, x, y, text.
#
# THIS IS THE BEST PIECE OF EVIDENCE THIS ROUTE PRODUCES, and it used to be
# computed and thrown away -- the only reader anywhere was an nrow() for the
# count. Measured on a fixture read through bands 120pt out of place, the spilled
# frame held the four real closing balances, in order, with their page and their
# position: exactly the figures the workbook was missing. A count says something
# is wrong; this says WHAT was lost and where it was printed.
document_unclaimed <- function(tables) {
  empty <- data.frame(table = character(0), page = integer(0), x = numeric(0),
                      y = numeric(0), text = character(0), stringsAsFactors = FALSE)
  if (!length(tables)) return(empty)
  parts <- lapply(names(tables), function(k) {
    s <- tables[[k]]$spilled
    if (is.null(s) || !nrow(s)) return(NULL)
    data.frame(table = rep(k, nrow(s)),
               page = as.integer(s$page), x = as.numeric(s$x), y = as.numeric(s$y),
               text = as.character(s$text), stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(empty)
  out <- do.call(rbind, parts)
  out <- out[order(out$table, out$page, out$y, out$x), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# document_column_report(tables) -> every table's per-column fill report, stacked.
document_column_report <- function(tables) {
  if (!length(tables))
    return(data.frame(table = character(0), column = character(0), type = character(0),
                      filled = integer(0), rows = integer(0), fill_rate = numeric(0),
                      may_be_blank = logical(0), low_fill = logical(0),
                      stringsAsFactors = FALSE))
  rows <- lapply(names(tables), function(k) {
    rp <- tables[[k]]$report
    if (is.null(rp) || !nrow(rp)) return(NULL)
    cbind(data.frame(table = rep(k, nrow(rp)), stringsAsFactors = FALSE), rp)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(document_column_report(list()))
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

# document_long(tables) -> every cell of every table as one long frame.
#
# WHY LONG AND NOT WIDE. Thirty tables of thirty different widths cannot share a
# wide layout without inventing columns that do not exist in most of them. Long is
# the only honest single-file shape: one row per cell, carrying which table, which
# page and which row it came from. The per-table sheets in the workbook are the
# readable form; this is the form you can load somewhere else.
document_long <- function(tables) {
  empty <- data.frame(table = character(0), table_name = character(0),
                      page = integer(0), row = integer(0), column = character(0),
                      value = character(0), stringsAsFactors = FALSE)
  if (!length(tables)) return(empty)
  parts <- lapply(names(tables), function(k) {
    r <- tables[[k]]$rows
    if (is.null(r) || !nrow(r)) return(NULL)
    vals <- setdiff(names(r), c("page", "row"))
    # The parsed "__value" companions are a convenience on the sheet, not separate
    # data -- carrying them here would double every row of the long file.
    vals <- vals[!grepl("__value$", vals)]
    if (!length(vals)) return(NULL)
    do.call(rbind, lapply(vals, function(cn) data.frame(
      table = rep(k, nrow(r)),
      table_name = rep(as.character(tables[[k]]$name %||% k)[1], nrow(r)),
      page = as.integer(r$page), row = as.integer(r$row),
      column = rep(cn, nrow(r)),
      value = as.character(r[[cn]]),
      stringsAsFactors = FALSE)))
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(empty)
  out <- do.call(rbind, parts)
  out <- out[order(out$table, out$page, out$row), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# Outputs
# ---------------------------------------------------------------------------

# The sheets the READER writes for itself, which therefore cannot also be a
# table's sheet. A table called "Report" used to destroy the whole workbook:
# openxlsx refuses the duplicate, the tryCatch swallowed it, the xlsx was silently
# never added to the outputs, and the per-table CSV fallback was gated on "no
# openxlsx" so it did not run either. Two roll-up CSVs, no workbook, no error.
.DOC_RESERVED_SHEETS <- c("Build", "Values", "Report")

# .doc_sheet_names(tables, reserved) -- table names turned into legal, unique
# worksheet names. Excel refuses : \ / ? * [ ] and anything over 31 characters,
# two sheets cannot share a name, and THE COMPARISON IS CASE-INSENSITIVE -- so
# "Fees" and "FEES" collide as surely as two identical names do.
.doc_sheet_names <- function(tables, reserved = .DOC_RESERVED_SHEETS) {
  raw <- vapply(names(tables), function(k)
    as.character(tables[[k]]$name %||% k)[1], character(1))
  # ':' sits LAST in the class on purpose: a class opening "[:" reads as the start
  # of a POSIX class name to PCRE, and the whole pattern then fails to compile.
  s <- gsub("[\\\\/?*\\[\\]:]", " ", raw, perl = TRUE)
  s <- trimws(gsub("\\s+", " ", s))
  s[!nzchar(s)] <- names(tables)[!nzchar(s)]
  s <- substr(s, 1, 31)
  # Claimed folded to lower case: the reader's own sheets first, then each table
  # in turn. A collision takes a numbered suffix, and the NAME is trimmed to make
  # room for it rather than the suffix being trimmed off the end.
  used <- tolower(as.character(reserved %||% character(0)))
  for (i in seq_along(s)) {
    cand <- s[i]; k <- 1L
    while (tolower(cand) %in% used) {
      k <- k + 1L
      suf <- paste0(" ", k)
      cand <- paste0(substr(s[i], 1, 31L - nchar(suf)), suf)
    }
    s[i] <- cand; used <- c(used, tolower(cand))
  }
  stats::setNames(s, names(tables))
}

# .doc_csv_safe(df) -- the formula guard, applied to EVERY character column.
#
# A cell reading "=1+1" reached the file verbatim and Excel evaluated it, so the
# reviewer saw 2 where the document printed =1+1: a wrong figure that looks right,
# delivered by the tool's own output. The statement path neutralises exactly this
# (.neutralize_formula, R/outputs.R) over a fixed list of text columns; a report
# has no fixed schema, so every character column is guarded instead. Nothing is
# stripped -- a leading quote is added, which Excel shows as the literal text.
# The JSON, as on the statement path, stays verbatim: it is never executed.
.doc_csv_safe <- function(df) {
  if (is.null(df) || !ncol(df)) return(df)
  for (nm in names(df))
    if (is.character(df[[nm]]) || is.factor(df[[nm]]))
      df[[nm]] <- .neutralize_formula(as.character(df[[nm]]))
  df
}

# .doc_build_df(build) -- the build stamp as a two-column field/value frame, the
# same shape .header_df() gives the statement workbook's Summary sheet.
.doc_build_df <- function(build) {
  b <- as.list(build %||% list())
  if (!length(b) || is.null(names(b)))
    return(data.frame(field = character(0), value = character(0),
                      stringsAsFactors = FALSE))
  b <- b[order(names(b))]
  data.frame(field = names(b),
             value = vapply(b, function(v)
               if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1],
               character(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}

# .doc_block_csv(path, blocks) -- several frames written into one CSV, each under
# its own one-line heading, separated by a blank line.
#
# Used for the report CSV only, which is the EVIDENCE file rather than the
# loadable one: the long CSV and the values CSV stay plain single-table files that
# read.csv() opens with no arguments, because that is the whole point of them.
.doc_block_csv <- function(path, blocks) {
  lines <- character(0)
  for (nm in names(blocks)) {
    df <- blocks[[nm]]
    if (is.null(df)) next
    tc <- textConnection("buf", "w", local = TRUE)
    utils::write.csv(.doc_csv_safe(df), tc, row.names = FALSE, na = "")
    close(tc)
    lines <- c(lines, if (length(lines)) "" else character(0), paste0("# ", nm), buf)
  }
  writeLines(lines, path)
  invisible(path)
}

# write_document_outputs(ext, outdir, basename, formats, build) -> the paths
# written, carrying attr(, "notes"): anything the writer had to say out loud.
#
# WHAT IS WRITTEN, AND WHY EACH.
#   .tables.xlsx       a sheet per table, plus Values (the label/value pairs),
#                      Report (how each table was found and how full it came out)
#                      and Build (what produced this file). This is the readable
#                      form: it is what gets opened.
#   .tables-long.csv   every cell, one per row, tagged with its table and page.
#                      This is the loadable form, and it stays a plain one-table
#                      CSV that read.csv() opens with no arguments.
#   .report.csv        WHAT PRODUCED THIS and HOW IT READ: the build stamp, then
#                      one row per table, then the per-column fill report. It used
#                      to be the column report alone, and on a machine with no
#                      openxlsx the per-table summary -- the one thing unique to
#                      this route -- was written NOWHERE AT ALL, while the comment
#                      here claimed nothing was dropped for want of a package.
#   .unclaimed.csv     every word inside a table that no column claimed, written
#                      only when there are some. Its existence is the signal.
#   .values.csv        the label/value pairs, when the template has any.
# When the workbook cannot be written -- openxlsx missing, OR openxlsx refusing
# the workbook -- a CSV PER TABLE is written instead, and the reason is said.
write_document_outputs <- function(ext, outdir, basename,
                                   formats = c("xlsx", "csv"), build = NULL) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0); notes <- character(0)
  tables <- ext$tables %||% list()
  build_df <- .doc_build_df(build)

  have_xlsx <- requireNamespace("openxlsx", quietly = TRUE)
  wrote_xlsx <- FALSE
  if ("xlsx" %in% formats && have_xlsx && length(tables)) {
    p <- file.path(outdir, paste0(basename, ".tables.xlsx"))
    sheets <- .doc_sheet_names(tables)
    ok <- tryCatch({
      wb <- openxlsx::createWorkbook()
      for (k in names(tables)) {
        openxlsx::addWorksheet(wb, sheets[[k]])
        openxlsx::writeData(wb, sheets[[k]], tables[[k]]$rows)
      }
      if (!is.null(ext$pairs) && nrow(ext$pairs)) {
        openxlsx::addWorksheet(wb, "Values"); openxlsx::writeData(wb, "Values", ext$pairs)
      }
      openxlsx::addWorksheet(wb, "Report")
      openxlsx::writeData(wb, "Report", ext$summary)
      openxlsx::writeData(wb, "Report", ext$report, startRow = nrow(ext$summary) + 3L)
      if (nrow(build_df)) {
        openxlsx::addWorksheet(wb, "Build")
        openxlsx::writeData(wb, "Build", build_df)
      }
      # THE SAME TWO LINES THE STATEMENT WRITER USES. Without them openxlsx stamps
      # the wall clock into docProps/core.xml and the ZIP container stamps each
      # entry's mod-time, so the same document read through the same template
      # produced a DIFFERENT file every run -- and "re-run it and show it produces
      # the same file", the check a maintainer runs to prove the tool has not
      # drifted, failed on this route for a reason that has nothing to do with the
      # figures. That is the worst kind of false alarm.
      wb$core <- .deterministic_core(wb$core)
      openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
      safe(.normalize_zip_timestamps(p))
      # ...and the file has to actually BE there. A path added for a workbook that
      # was never written is the same lie as a workbook written and never
      # mentioned, and it is the one a download button hits.
      fi <- file.info(p)
      isTRUE(!is.na(fi$size) && fi$size > 0 && !fi$isdir)
    }, error = function(e) FALSE)
    if (isTRUE(ok)) { paths <- c(paths, p); wrote_xlsx <- TRUE }
    else notes <- c(notes, paste("The workbook could not be written, so each table",
                                 "was written as its own spreadsheet file instead."))
  }

  if ("csv" %in% formats) {
    p <- file.path(outdir, paste0(basename, ".tables-long.csv"))
    utils::write.csv(.doc_csv_safe(ext$long), p, row.names = FALSE, na = "")
    paths <- c(paths, p)
    p <- file.path(outdir, paste0(basename, ".report.csv"))
    .doc_block_csv(p, list("What produced this file" = build_df,
                           "Every table on this document" = ext$summary,
                           "Every column of every table" = ext$report))
    paths <- c(paths, p)
    u <- ext$unclaimed %||% document_unclaimed(tables)
    if (!is.null(u) && nrow(u)) {
      p <- file.path(outdir, paste0(basename, ".unclaimed.csv"))
      utils::write.csv(.doc_csv_safe(u), p, row.names = FALSE, na = "")
      paths <- c(paths, p)
    }
  }
  # No workbook -> a CSV per table, so the per-table shape is still available.
  # Gated on whether one was actually WRITTEN, not on whether openxlsx exists: a
  # workbook openxlsx refused is a missing workbook too.
  if ("csv" %in% formats && !wrote_xlsx) {
    for (k in names(tables)) {
      r <- tables[[k]]$rows
      if (is.null(r) || !nrow(r)) next
      q <- file.path(outdir, sprintf("%s.%s.csv", basename,
                                     gsub("[^A-Za-z0-9_]+", "_", k)))
      utils::write.csv(.doc_csv_safe(r), q, row.names = FALSE, na = "")
      paths <- c(paths, q)
    }
  }
  if ("csv" %in% formats && !is.null(ext$pairs) && nrow(ext$pairs)) {
    p <- file.path(outdir, paste0(basename, ".values.csv"))
    utils::write.csv(.doc_csv_safe(ext$pairs), p, row.names = FALSE, na = "")
    paths <- c(paths, p)
  }
  attr(paths, "notes") <- notes
  paths
}

# ---------------------------------------------------------------------------
# The front door
# ---------------------------------------------------------------------------

# convert_tables(path, ...) -> a result list shaped like convert_form()'s, with
# `kind = "tables"`. Never throws.
#
# THE STATUS RULE. There is no reconciliation here, so "ok" cannot mean "the
# figures add up" -- it can only mean "every table was found, and none of them
# came out suspiciously empty". Anything less is needs_review, and it says which
# table and why. A template that matched but found NO rows at all is `unsupported`,
# because that is the same answer as having no template: this document is not the
# one the template was drawn on.
#
# WHAT ELSE DECIDES IT NOW, and both were computed and discarded before:
#   * UNCLAIMED WORDS. Words inside a table's own boundary that no column claimed.
#     Measured 4 on a read where every figure landed one column across, and 0 on
#     both correct reads of the same fixture - which makes it a far better signal
#     than a mostly-empty column, and it decided nothing. More than one word is
#     enough: a low bar is not noisy on this evidence.
#   * A TYPED COLUMN THAT PARSED NOTHING. A `money` column holding "Transaction",
#     "Savings", "TermDeposit" reports itself completely full, because fill asks
#     only whether a cell has any text at all - while the engine already knows not
#     one cell of it parsed as money and throws that away. A money column at a
#     parse rate of zero is a band over the wrong ink, not a thin column.
# Both are read defensively: R/tables.R owns the per-column report, and if it is
# not carrying parse counts on this build the clause simply does not fire.
.DOC_MAX_UNCLAIMED <- 1L

# .doc_parse_failures(rep) -> the tables holding a typed column that parsed
# nothing at all.
#
# R/tables.R owns the per-column report and decides what "parsed nothing" means:
# `wrong_kind` is its answer and is used as given. The parse-rate fallback is here
# only so this gate degrades to doing nothing, rather than erroring, on a report
# frame that predates those columns.
.doc_parse_failures <- function(rep) {
  if (is.null(rep) || !nrow(rep) || !("table" %in% names(rep))) return(character(0))
  if ("wrong_kind" %in% names(rep))
    return(unique(as.character(rep$table[rep$wrong_kind %in% TRUE])))
  if (!("parse_rate" %in% names(rep))) return(character(0))
  rate <- suppressWarnings(as.numeric(rep$parse_rate))
  filled <- suppressWarnings(as.integer(rep$filled %||% rep(0L, nrow(rep))))
  unique(as.character(rep$table[!is.na(rate) & rate <= 0 & !is.na(filled) & filled > 0L]))
}

convert_tables <- function(path, doc_dir = "templates/documents", user_doc_dir = NULL,
                           outdir = "out", formats = c("xlsx", "csv"),
                           template_id = NULL, run_id = NULL) {
  base <- tools::file_path_sans_ext(basename(path %||% "input"))
  res <- list(status = "failed", kind = "tables", template_id = NA_character_,
              outputs = character(0), messages = character(0),
              n_tables = 0L, n_rows = 0L)
  tryCatch({
    dtpls <- load_document_templates(doc_dir, user_doc_dir)
    input <- read_input(path)
    tmpl <- NULL; det <- NULL; ambiguous <- FALSE
    # FORCING A TEMPLATE, ALL THE WAY THROUGH. The argument has always existed and
    # nothing ever passed it; worse, an id that is not in the library fell through
    # to detection in silence, so "read it with THIS one" could quietly be
    # answered by a different template altogether.
    forced <- !is.null(template_id) && nzchar(as.character(template_id)[1])
    if (forced) {
      tmpl <- dtpls[[as.character(template_id)[1]]]
      if (is.null(tmpl)) {
        res$status <- "unsupported"
        res$messages <- status_message("unsupported",
          "the template you asked for is not in the library",
          "pick another one, or check it has not been removed")
        return(res)
      }
    } else {
      det <- detect_document_template(input, dtpls)
      # A TIE IS NOT AN ANSWER OF "NO". Two templates that both fit used to mean
      # "unsupported" on a document the library reads perfectly, and it named
      # neither. Read it with the best of them, say so, and send it to review.
      ambiguous <- !isTRUE(det$matched) && length(det$tied) >= 2
      if (!isTRUE(det$matched) && !ambiguous) {
        res$status <- "unsupported"
        res$detect <- det
        res$messages <- status_message("unsupported", "no document template matched",
                                       det$detail_plain %||% det$detail)
        return(res)
      }
      tmpl <- dtpls[[det$template_id]]
    }
    ext <- extract_document(input, tmpl)
    res$template_id <- as.character(tmpl$id %||% NA_character_)[1]
    res$template_version <- tmpl$version %||% NA
    res$template_sha256 <- template_sha256(tmpl)
    res$forced_template <- forced
    res$detect <- det
    res$ambiguous <- ambiguous
    res$extract <- ext
    res$tables <- ext$tables
    res$pairs <- ext$pairs
    res$n_tables <- length(ext$tables)
    res$n_rows <- sum(vapply(ext$tables, function(r) as.integer(r$n_rows %||% 0L), integer(1)))
    empty <- names(ext$tables)[vapply(ext$tables,
                                      function(r) (r$n_rows %||% 0L) < 1L, logical(1))]
    thin <- unique(ext$report$table[ext$report$low_fill %in% TRUE])
    weak <- names(ext$tables)[vapply(ext$tables, function(r)
      isTRUE((r$confidence %||% 1) < 0.5), logical(1))]
    # ABSENT, not empty: a table this document does not carry at all. An absence
    # is only a fault for a table that is PINNED here - one set to be read
    # wherever it appears is expected to be missing sometimes, and a permanent
    # needs_review is a warning nobody reads.
    tabs <- tmpl$tables %||% list()
    absent <- as.character(ext$misses %||% character(0))
    absent_pinned <- absent[!vapply(absent, function(k)
      .doc_table_optional(tabs[[k]] %||% list()), logical(1))]
    unclaimed <- as.integer(sum(as.integer(ext$summary$unclaimed_words %||% 0L),
                                na.rm = TRUE))
    spilt <- unique(as.character((ext$unclaimed %||% document_unclaimed(ext$tables))$table))
    parse_fail <- .doc_parse_failures(ext$report)
    res$empty_tables <- empty; res$thin_tables <- thin; res$weak_tables <- weak
    res$absent_tables <- absent; res$unclaimed_words <- unclaimed
    res$parse_fail_tables <- parse_fail
    # The build stamp: WHAT PRODUCED THIS FILE. A report converts in March and in
    # September the defence asks how row 14 was derived; the file used to answer
    # with sheet names and nothing else. Deliberately deterministic - the run id
    # is stamped only when the caller hands one over, and no wall clock is written
    # at all, because the same document read through the same template must still
    # produce the same bytes (the maintainer's drift check depends on it).
    build <- list(engine_version = engine_version(),
                  template_id = res$template_id,
                  template_version = as.character(res$template_version %||% NA),
                  template_sha256 = res$template_sha256,
                  source_file = basename(path %||% NA_character_),
                  source_sha256 = safe(file_sha256(path), NA_character_),
                  page_count = as.character(.doc_npages(input)))
    if (!is.null(run_id) && nzchar(as.character(run_id)[1]))
      build$run_id <- as.character(run_id)[1]
    res$build <- build
    res$outputs <- write_document_outputs(ext, outdir, base, formats, build = build)
    notes <- attr(res$outputs, "notes") %||% character(0)
    res$outputs <- as.character(res$outputs)

    nm <- function(keys) {
      out <- vapply(as.character(keys), function(k)
        as.character((tabs[[k]]$name %||% k))[1], character(1))
      paste(unname(out), collapse = ", ")
    }
    absent_note <- if (length(absent))
      sprintf(", and %s %s not on this copy", nm(absent),
              if (length(absent) == 1L) "is" else "are") else ""

    res$status <- if (res$n_rows == 0L) "unsupported"
                  else if (length(empty) || length(thin) || length(weak) ||
                           length(absent_pinned) || length(parse_fail) ||
                           ambiguous || unclaimed > .DOC_MAX_UNCLAIMED) "needs_review"
                  else "ok"
    # MATCHED, AND READ NOTHING -- kept as its two halves as well as joined.
    # The diagnostics table needs them apart (diagnostics_matched_empty, called by
    # convert_document once this result comes back), and taking them back out of
    # the message would be a second way to say one thing.
    #
    # "the toolkit" was the statement toolkit's name everywhere else in the
    # product, printed here to somebody holding a report. The tab is the same one
    # on both routes and it is called "Add a template".
    me_why <- sprintf(
      # THE TEMPLATE IN WORDS, not its id. An id is a maintainer's handle: the
      # person holding the document cannot check anything against one, and the
      # id is still on the run record where it is useful.
      "the %s fits this document but read no rows from any of its %d table(s)",
      .doc_tpl_name(tmpl), res$n_tables)
    me_fix <- paste("the tables have probably moved or been renamed on this copy -",
                    "open it on the \"Add a template\" tab and check where each table starts")
    if (res$n_rows == 0L) res$matched_empty <- list(why = me_why, fix = me_fix)
    res$messages <- if (res$n_rows == 0L)
      status_message("unsupported", me_why, me_fix)
      else if (ambiguous)
      status_message("needs_review",
        sprintf("%d templates fit this document equally well%s", length(det$tied), absent_note),
        "it was read with one of them - check the figures, and retire whichever template is the duplicate")
      else if (length(parse_fail))
      status_message("needs_review",
        sprintf("a column of figures on %s came out holding no figures at all", nm(parse_fail)),
        "the column edges there are over the wrong part of the page")
      else if (unclaimed > .DOC_MAX_UNCLAIMED)
      status_message("needs_review",
        sprintf("%d word(s) inside %s were not in any column", unclaimed, nm(spilt)),
        "they are listed in the unclaimed file beside the download - the column edges do not fit this document")
      else if (length(absent_pinned))
      status_message("needs_review",
        sprintf("%s %s not on this document at all", nm(absent_pinned),
                if (length(absent_pinned) == 1L) "is" else "are"),
        "check this is the same kind of document - the rest are unaffected")
      else if (length(empty))
      status_message("needs_review",
        sprintf("%d of %d table(s) came out empty: %s", length(empty), res$n_tables,
                nm(empty)),
        "those tables were not found on this document - the rest are unaffected")
      else if (length(weak))
      status_message("needs_review",
        sprintf("%d table(s) were found by position rather than by their heading: %s",
                length(weak), nm(weak)),
        "check those against the document - a heading that changed moves everything under it")
      else if (length(thin))
      status_message("needs_review",
        sprintf("%d table(s) have a column that came out mostly empty: %s",
                length(thin), nm(thin)),
        "check the column edges on those - a band drawn slightly too narrow loses the whole column")
      else status_message("ok", sprintf("%d table(s), %d row(s)%s",
                                        res$n_tables, res$n_rows, absent_note))
    # ...and anything the WRITER had to say is said too, rather than a file
    # quietly not being there.
    if (length(notes)) res$messages <- c(res$messages, notes)
    res
  }, error = function(e) {
    res$messages <- paste("error:", conditionMessage(e)); res
  })
}

# ---------------------------------------------------------------------------
# Building a template from what was proposed
# ---------------------------------------------------------------------------

# .doc_band_of(tb) -- a table's band, or NULL. Only the numbers that are actually
# set are written, so a template does not carry four keys where one was meant:
# an absent y_max means "to the bottom of the page", which is a different
# statement from "to 0".
.doc_band_of <- function(tb) {
  b <- tb$band
  if (!is.list(b) || !length(b)) return(NULL)
  out <- list()
  for (k in c("x_min", "x_max", "y_min", "y_max")) {
    v <- .doc_num(b[[k]], NA_real_)
    if (isTRUE(is.finite(v))) out[[k]] <- v
  }
  if (length(out)) out else NULL
}

# document_proposal_from_template(t) -> list(tables, pairs), the INVERSE of
# document_template_from_proposal() below.
#
# WHY IT EXISTS. A report template could be built and saved and never opened
# again: there was no way back from a saved template to the boxes on the page it
# was drawn from. So "this column is 4pt too far left" -- the thing that actually
# goes wrong -- meant either editing coordinates as text in a YAML box or
# throwing the template away and drawing all of it a second time. The statement
# half has had the way back for a long time (Admin -> "Redraw its columns"); this
# is the same door for the other half.
#
# The two forms are nearly the same shape already (the builder saves its drafts
# almost verbatim), so this is mostly re-keying: the template keys tables and
# values by their output name, and a draft carries that name INSIDE it.
document_proposal_from_template <- function(t) {
  empty <- list(tables = list(), pairs = list())
  if (!is.list(t)) return(empty)
  tabs <- t$tables %||% list()
  prs  <- t$pairs %||% list()
  keys <- names(tabs) %||% rep("", length(tabs))
  tables <- lapply(seq_along(tabs), function(i) {
    tb <- tabs[[i]]
    if (!is.list(tb)) return(NULL)
    # The NAME is what the builder shows and what the output column set is keyed
    # by. A template written by hand may leave it out, in which case the key it
    # was filed under is the name it was given.
    tb$name <- as.character(tb$name %||% keys[i] %||% sprintf("Table %d", i))[1]
    tb$columns <- .doc_columns(tb)
    tb$band <- .doc_band_of(tb)
    tb$header_rows <- .doc_int(tb$header_rows, 1L)
    tb$follow <- isTRUE(tb$follow)
    tb
  })
  pairs <- lapply(seq_along(prs), function(i) {
    pr <- prs[[i]]
    if (!is.list(pr)) return(NULL)
    # A pair's name is its KEY (the column heading in the output); label_text is
    # the wording printed on the page. They are different things and the builder
    # edits both, so both come back.
    pr$name <- as.character(pr$name %||% (names(prs) %||% "")[i] %||% "")[1]
    pr$type <- as.character(pr$type %||% "text")[1]
    pr
  })
  list(tables = Filter(Negate(is.null), tables),
       pairs  = Filter(Negate(is.null), pairs))
}

# Per-table keys the builder does not ask about and the reader does use. They were
# dropped on the way back out, which is the same defect the `band` comment below
# describes: a key set on the template, obeyed by the reader, and silently gone
# the first time the template was opened and saved.
#   row_tol / max_gap  how far apart words are still one line / one cell
#   doc_pages          the length of the example THIS table was drawn on
#   optional           this document does not always carry it, so absence is fine
#   occurrence         read it wherever it appears, not just where it was drawn
.DOC_TABLE_CARRY <- c("row_tol", "max_gap", "doc_pages", "optional", "occurrence")

# document_template_from_proposal(id, bank, type, phrases, tables, pairs,
#                                 doc_pages, base) -> a mode:document template.
# This is what the confirm-and-name screen saves: the proposals (see
# R/tables_detect.R) with the names a person gave them.
#
# `base` IS THE TEMPLATE THIS ONE IS BEING SAVED OVER, and it is the whole of the
# round-trip fix. Opening a saved template and saving it again used to REBUILD it
# from a whitelist, so everything the builder does not ask about was lost on the
# way past: `hidden: true` (a PARKED template silently returned to live detection
# and could start claiming documents again), `notes`, per-table `row_tol` and
# `max_gap`, and `version` -- which is a literal 1 here, so a template edited
# seven times was version 1 seven times and today's copy was indistinguishable
# from last month's in every record kept.
#
# So when a base is given, the loaded template is MODIFIED rather than rebuilt:
# it keeps everything it had, the builder replaces only what the builder owns
# (the tables, the values, the fingerprint, the name and the issuer), and the
# version goes UP by one. With no base -- a template being drawn for the first
# time -- the result is byte for byte what it always was.
document_template_from_proposal <- function(id, bank, statement_type, phrases,
                                            tables = list(), pairs = list(),
                                            doc_pages = NA_integer_, base = NULL) {
  keyed <- list()
  used <- character(0)
  for (tb in tables) {
    k <- doc_suggest_name(tb$name %||% "table")
    k <- make.unique(c(used, k), sep = "_")[length(used) + 1L]
    used <- c(used, k)
    keyed[[k]] <- list(
      name = as.character(tb$name %||% k)[1],
      start = list(page = .doc_int(tb$start$page, 1L), y = .doc_num(tb$start$y, 0)),
      end   = list(page = .doc_int(tb$end$page, .doc_int(tb$start$page, 1L)),
                   y = .doc_num(tb$end$y, 0)),
      # THE BAND IS CARRIED, and it was not. `end` is a place on the LAST page;
      # `band$y_max` is where the window closes on every page in between
      # (doc_locate_table), which is the whole reason the builder asks for it. It
      # was set on the draft, drawn on the screen, written in the "where it
      # starts and stops" panel -- and then dropped here, so the reader never saw
      # it and the footer came back in the rows anyway. A control that changes
      # nothing is worse than no control: it answers the question and the
      # question stays answered wrongly.
      band = .doc_band_of(tb),
      header_rows = .doc_int(tb$header_rows, 1L),
      follow = isTRUE(tb$follow),
      min_fill = .doc_num(tb$min_fill, 0.5),
      anchor = list(header_text = as.list(unlist(tb$anchor$header_text %||% list())),
                    first_column = as.list(unlist(tb$anchor$first_column %||% list()))),
      columns = lapply(.doc_columns(tb), function(cc) list(
        name = as.character(cc$name %||% "column")[1],
        x_min = .doc_num(cc$x_min), x_max = .doc_num(cc$x_max),
        type = as.character(cc$type %||% "auto")[1],
        may_be_blank = isTRUE(cc$may_be_blank))))
    for (ck in .DOC_TABLE_CARRY)
      if (!is.null(tb[[ck]])) keyed[[k]][[ck]] <- tb[[ck]]
  }
  pkeyed <- list()
  pused <- character(0)
  for (pr in pairs) {
    k <- doc_suggest_name(pr$name %||% pr$label_text %||% "value")
    k <- make.unique(c(pused, k), sep = "_")[length(pused) + 1L]
    pused <- c(pused, k)
    # WHERE, spelled out on the template. The two boxes already imply it, but a
    # person editing this file -- or the screen that lets them change it from
    # "to the right of it" to "under it" -- should not have to do coordinate
    # arithmetic to find out which side the tool will look on.
    rel <- if (is.list(pr$label) && is.list(pr$value))
             .doc_pair_rel(pr$label, pr$value) else NULL
    if (is.list(pr$where)) rel <- utils::modifyList(rel %||% list(), pr$where)
    else if (is.character(pr$where) && nzchar(pr$where[1]) && !is.null(rel))
      rel$where <- pr$where[1]
    pkeyed[[k]] <- list(label_text = as.character(pr$label_text %||% "")[1],
                        label = pr$label, value = pr$value, where = rel,
                        type = as.character(pr$type %||% "text")[1])
  }
  ph <- trimws(as.character(unlist(phrases %||% character(0))))
  ph <- ph[!is.na(ph) & nzchar(ph)]
  built <- list(id = gsub("[^A-Za-z0-9_]+", "_", as.character(id %||% "new_document")),
       bank = as.character(bank %||% "NewIssuer")[1],
       statement_type = as.character(statement_type %||% "report")[1],
       format = "pdf", mode = "document", version = 1,
       doc_pages = .doc_int(doc_pages),
       ref_width = .A4_W, ref_height = .A4_H,
       fingerprint = list(page_contains_all = as.list(ph)),
       # NO CURRENCY. A document template has no reconciliation behind it and
       # nothing in this mode ever reads one -- so stamping "NZD" on a report of
       # rupees or euros would be a false statement in a saved file, made by a
       # converter that never looks at it. A statement template declares its
       # currency because the balance proof spends it; this one does not.
       pairs = pkeyed, tables = keyed)
  if (!is.list(base) || !length(base)) return(built)
  # SAVING OVER AN EXISTING TEMPLATE: keep it, and change only what the builder
  # owns. `origin` is a load-time marker, not part of the template, and
  # save_document_template drops it anyway.
  out <- base
  out$origin <- NULL
  # Exactly what the builder owns, and nothing else. ref_width/ref_height are
  # NOT on this list: the frame is part of the template, and the A4 default this
  # function falls back to would relabel every band on a Letter-sized template
  # without converting one of them.
  for (k in c("id", "bank", "statement_type", "format", "mode", "doc_pages",
              "fingerprint", "pairs", "tables"))
    out[[k]] <- built[[k]]
  # ...and the version goes UP rather than back to 1. A number that is always 1
  # cannot tell today's template from last month's in any record that keeps it.
  v <- suppressWarnings(as.numeric(base$version %||% NA))
  out$version <- if (is.finite(v)) v + 1 else 1
  out
}
