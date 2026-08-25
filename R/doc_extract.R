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

# is_document_template(t) -- TRUE for a mode:document template.
is_document_template <- function(t) {
  identical(t$mode %||% "", "document") &&
    (length(t$tables %||% list()) > 0L || length(t$pairs %||% list()) > 0L)
}

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
  p
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
load_document_templates <- function(dir = "templates/documents", user_dir = NULL) {
  out <- list(); errors <- character(0)
  for (d in c(dir, user_dir)) {
    if (is.null(d) || !dir.exists(d)) next
    for (f in list.files(d, pattern = "\\.ya?ml$", full.names = TRUE)) {
      t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
      if (is.null(t) || !identical(t$mode %||% "", "document") || is.null(t$id)) next
      probs <- validate_document_template(t)
      if (length(probs)) {
        errors <- c(errors, sprintf("%s: %s", t$id, paste(probs, collapse = "; ")))
        next
      }
      if (is.null(out[[t$id]])) out[[t$id]] <- t
    }
  }
  attr(out, "load_errors") <- errors
  out
}

# save_document_template(t, dir) -> path. Validates first: an invalid template is
# never written, so the loader's leniency never has to cover for this screen.
save_document_template <- function(t, dir = "templates/documents_user") {
  t$origin <- NULL
  probs <- validate_document_template(t)
  if (length(probs)) stop("document template is not valid: ", paste(probs, collapse = "; "))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0(gsub("[^A-Za-z0-9_]+", "_", t$id), ".yaml"))
  yaml::write_yaml(t, path)
  invisible(path)
}

# document_template_ids(dir) / delete_document_template(id, dir) -- the same
# read/remove pair the other two modes have, so Admin can manage these too.
document_template_ids <- function(dir = "templates/documents_user") {
  if (!dir.exists(dir)) return(character(0))
  ids <- vapply(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE), function(f) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    as.character(t$id %||% NA_character_)[1]
  }, character(1))
  unname(ids[!is.na(ids)])
}

delete_document_template <- function(id, dir = "templates/documents_user") {
  if (!dir.exists(dir) || is.null(id) || !nzchar(id)) return(invisible(FALSE))
  hit <- FALSE
  for (f in list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (identical(as.character(t$id %||% "")[1], id) && file.remove(f)) hit <- TRUE
  }
  invisible(hit)
}

# detect_document_template(input, dtemplates) -> list(template_id, matched, score,
# detail). Identical rule to detect_form(): every identifying phrase must appear,
# and the winner must be strictly more specific than any other match -- a tie is
# NOT a match, because choosing arbitrarily between two templates is choosing
# arbitrarily between two sets of column bands.
detect_document_template <- function(input, dtemplates) {
  hay <- paste(input$pages %||% character(0), collapse = "\n")
  if (!length(dtemplates))
    return(list(template_id = NA_character_, matched = FALSE, score = 0,
                detail = "no document templates are installed"))
  full <- vapply(dtemplates, function(t) {
    need <- as.character(unlist(t$fingerprint$page_contains_all %||% character(0)))
    length(need) > 0 && all(vapply(need, function(ph) grepl(ph, hay, fixed = TRUE), logical(1)))
  }, logical(1))
  if (!any(full))
    return(list(template_id = NA_character_, matched = FALSE, score = 0,
                detail = "no document template's identifying phrases were all found"))
  need_len <- vapply(dtemplates, function(t)
    length(t$fingerprint$page_contains_all %||% character(0)), integer(1))
  cand <- which(full)
  best <- cand[which.max(need_len[cand])]
  matched <- sum(need_len[cand] == need_len[best]) == 1
  list(template_id = names(dtemplates)[best], matched = matched, score = need_len[best],
       detail = if (matched) "matched by identifying phrases"
                else "several document templates are equally specific here, so none was chosen")
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
extract_document <- function(input, tmpl) {
  tabs <- tmpl$tables %||% list()
  keys <- names(tabs) %||% character(0)
  out <- list()
  for (k in keys) {
    tab <- tabs[[k]]
    loc <- doc_locate_table(input, tab, tmpl)
    r <- doc_table_rows(input, tab, tmpl, loc)
    r$key <- k
    r$name <- as.character(tab$name %||% k)[1]
    out[[k]] <- r
  }
  pairs <- doc_pairs(input, tmpl)
  list(tables = out, pairs = pairs,
       summary = document_summary(out),
       report = document_column_report(out),
       long = document_long(out))
}

# document_summary(tables) -> the one-row-per-table picture described above.
document_summary <- function(tables) {
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
  if (!length(tables))
    return(as.data.frame(proto, stringsAsFactors = FALSE))
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
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
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

# .doc_sheet_names(tables) -- table names turned into legal, unique worksheet
# names. Excel refuses : \ / ? * [ ] and anything over 31 characters, and two
# sheets cannot share a name -- so a workbook built from raw table names simply
# fails to save, taking the CSVs with it if this is not done first.
.doc_sheet_names <- function(tables) {
  raw <- vapply(names(tables), function(k)
    as.character(tables[[k]]$name %||% k)[1], character(1))
  # ':' sits LAST in the class on purpose: a class opening "[:" reads as the start
  # of a POSIX class name to PCRE, and the whole pattern then fails to compile.
  s <- gsub("[\\\\/?*\\[\\]:]", " ", raw, perl = TRUE)
  s <- trimws(gsub("\\s+", " ", s))
  s[!nzchar(s)] <- names(tables)[!nzchar(s)]
  s <- substr(s, 1, 31)
  s <- make.unique(s, sep = " ")
  # make.unique can push a name back over 31 characters; trim from the FRONT of
  # the name, not the suffix, so the disambiguator survives.
  long <- nchar(s) > 31
  if (any(long)) s[long] <- substr(s[long], nchar(s[long]) - 30, nchar(s[long]))
  stats::setNames(s, names(tables))
}

# write_document_outputs(ext, outdir, basename, formats) -> the paths written.
#
# WHAT IS WRITTEN, AND WHY BOTH.
#   .tables.xlsx       a sheet per table, plus Values (the label/value pairs) and
#                      Report (how each table was found and how full it came out).
#                      This is the readable form: it is what gets opened.
#   .tables-long.csv   every cell, one per row, tagged with its table and page.
#                      This is the loadable form.
#   .report.csv        the per-column fill report on its own, so it can be read
#                      without opening the workbook.
# When openxlsx is not installed the workbook cannot be written, so a CSV PER
# TABLE is written instead -- nothing is dropped for want of a package.
write_document_outputs <- function(ext, outdir, basename,
                                   formats = c("xlsx", "csv")) {
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0)
  tables <- ext$tables %||% list()

  if ("csv" %in% formats) {
    p <- file.path(outdir, paste0(basename, ".tables-long.csv"))
    utils::write.csv(ext$long, p, row.names = FALSE, na = "")
    paths <- c(paths, p)
    p <- file.path(outdir, paste0(basename, ".report.csv"))
    utils::write.csv(ext$report, p, row.names = FALSE, na = "")
    paths <- c(paths, p)
  }

  have_xlsx <- requireNamespace("openxlsx", quietly = TRUE)
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
      openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) paths <- c(paths, p)
  }
  # No workbook -> a CSV per table, so the per-table shape is still available.
  if ("csv" %in% formats && (!have_xlsx || !("xlsx" %in% formats))) {
    for (k in names(tables)) {
      r <- tables[[k]]$rows
      if (is.null(r) || !nrow(r)) next
      q <- file.path(outdir, sprintf("%s.%s.csv", basename,
                                     gsub("[^A-Za-z0-9_]+", "_", k)))
      utils::write.csv(r, q, row.names = FALSE, na = "")
      paths <- c(paths, q)
    }
  }
  if ("csv" %in% formats && !is.null(ext$pairs) && nrow(ext$pairs)) {
    p <- file.path(outdir, paste0(basename, ".values.csv"))
    utils::write.csv(ext$pairs, p, row.names = FALSE, na = "")
    paths <- c(paths, p)
  }
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
convert_tables <- function(path, doc_dir = "templates/documents", user_doc_dir = NULL,
                           outdir = "out", formats = c("xlsx", "csv"),
                           template_id = NULL) {
  base <- tools::file_path_sans_ext(basename(path %||% "input"))
  res <- list(status = "failed", kind = "tables", template_id = NA_character_,
              outputs = character(0), messages = character(0),
              n_tables = 0L, n_rows = 0L)
  tryCatch({
    dtpls <- load_document_templates(doc_dir, user_doc_dir)
    input <- read_input(path)
    tmpl <- NULL
    if (!is.null(template_id) && !is.null(dtpls[[template_id]])) {
      tmpl <- dtpls[[template_id]]
    } else {
      det <- detect_document_template(input, dtpls)
      if (!isTRUE(det$matched)) {
        res$status <- "unsupported"
        res$messages <- status_message("unsupported", "no document template matched", det$detail)
        return(res)
      }
      tmpl <- dtpls[[det$template_id]]
    }
    ext <- extract_document(input, tmpl)
    res$template_id <- as.character(tmpl$id %||% NA_character_)[1]
    res$template_version <- tmpl$version %||% NA
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
    res$empty_tables <- empty; res$thin_tables <- thin; res$weak_tables <- weak
    res$outputs <- write_document_outputs(ext, outdir, base, formats)

    res$status <- if (res$n_rows == 0L) "unsupported"
                  else if (length(empty) || length(thin) || length(weak)) "needs_review"
                  else "ok"
    res$messages <- if (res$n_rows == 0L)
      status_message("unsupported",
        sprintf("%s matches this document but read no rows from any of its %d table(s)",
                res$template_id, res$n_tables),
        "the tables have probably moved or been renamed here - open it in the toolkit and check where they start")
      else if (length(empty))
      status_message("needs_review",
        sprintf("%d of %d table(s) came out empty: %s", length(empty), res$n_tables,
                paste(empty, collapse = ", ")),
        "those tables were not found on this document - the rest are unaffected")
      else if (length(weak))
      status_message("needs_review",
        sprintf("%d table(s) were found by position rather than by their heading: %s",
                length(weak), paste(weak, collapse = ", ")),
        "check those against the document - a heading that changed moves everything under it")
      else if (length(thin))
      status_message("needs_review",
        sprintf("%d table(s) have a column that came out mostly empty: %s",
                length(thin), paste(thin, collapse = ", ")),
        "check the column edges on those - a band drawn slightly too narrow loses the whole column")
      else status_message("ok", sprintf("%d table(s), %d row(s)", res$n_tables, res$n_rows))
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

# document_template_from_proposal(id, bank, type, phrases, tables, pairs, input)
# -> a mode:document template. This is what the confirm-and-name screen saves:
# the proposals (see R/tables_detect.R) with the names a person gave them.
document_template_from_proposal <- function(id, bank, statement_type, phrases,
                                            tables = list(), pairs = list(),
                                            doc_pages = NA_integer_) {
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
  list(id = gsub("[^A-Za-z0-9_]+", "_", as.character(id %||% "new_document")),
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
}
