# doc_extract.R -- the "document" template mode: ONE template per kind of document, carrying every
# table and label/value pair on it. NO ROUTE TO THE FEED: write_feed() refuses `kind = "tables"` -
# there is no reconciliation behind these numbers.

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
  # Overlapping bands are a silent data loss: a word lands in one column and the earlier band wins,
  # so an overlap DELETES the value from the second column while every row still looks complete.
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
  # doc_pages is the length of the document this table was DRAWN on, which tells the reader a
  # positional match on a document of a different length is worth less.
  if (!is.null(.doc_key(tab, "doc_pages"))) {
    dp <- .doc_int(tab$doc_pages)
    if (is.na(dp) || dp < 1L)
      p <- c(p, w("doc_pages must be the number of pages the example had"))
  }
  occ <- as.character(.doc_key(tab, "occurrence") %||% "")[1]
  if (nzchar(occ) && !(occ %in% c("once", "first", "all")))
    p <- c(p, w("occurrence must be 'once', 'first' or 'all', not '%s'", occ))
  p
}

# The same PII gate the fingerprint passes, applied to a table's name and a pair's label_text -
# both drag-captured verbatim into templates/documents_user/, which is what gets copied off the box.
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

validate_document_template <- function(t) {
  p <- character(0)
  if (!is.list(t)) return("template is not a mapping")
  if (is.null(t$id) || !nzchar(trimws(as.character(t$id)))) p <- c(p, "missing 'id'")
  if (!identical(t$mode %||% "", "document")) p <- c(p, "mode must be 'document'")
  tabs <- t$tables %||% list(); prs <- t$pairs %||% list()
  if (!length(tabs) && !length(prs))
    p <- c(p, "this template finds nothing: add at least one table or one value")
  # The same fingerprint gate the other two modes pass. Detection is all-phrases-must-appear with no
  # reconciliation behind it, so a generic fingerprint reads one document with another's bands.
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
    # Three ways to find a value: a value box plus a label box, a value box alone, or a label
    # wording alone. So a missing value box is only a fault when there is no wording either.
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
  if (length(tabs) > 1L) {
    nm <- .doc_norm(vapply(tabs, function(x) as.character(x$name %||% "")[1], character(1)))
    dup <- unique(nm[duplicated(nm) & nzchar(nm)])
    if (length(dup))
      p <- c(p, sprintf("two tables are called the same thing (%s) - they would land on one sheet",
                        paste(dup, collapse = ", ")))
  }
  p
}

# Lenient like the form loader - one bad template never breaks the others - and never silent: skip
# reasons come back on attr(x, "load_errors").
load_document_templates <- function(dir = "templates/documents", user_dir = NULL,
                                    include_hidden = FALSE) {
  out <- list(); errors <- character(0)
  dirs <- c(dir, user_dir)
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

save_document_template <- function(t, dir = "templates/documents_user") {
  .save_template_yaml(t, dir, validate_document_template, "document template")
}

# ONE rule for comparing a typed phrase against a printed page: case, punctuation and runs of spaces
# stop mattering on both sides. LINE BREAKS DELIBERATELY DO NOT FOLD - collapsing them would let a
# phrase match across a line boundary. Byte-wise: a PDF text layer is not always valid UTF-8.
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

.doc_tpl_name <- function(t) {
  lab <- trimws(paste(trimws(as.character(t$bank %||% "")),
                      trimws(as.character(t$statement_type %||% ""))))
  if (nzchar(lab)) lab else as.character(t$id %||% "this template")[1]
}

# Same return shape as detect_statement(). The rule is .detect_by_fingerprint() in R/templates.R;
# this route owns only how its text is folded and how a report template is named.
detect_document_template <- function(input, dtemplates) {
  .detect_by_fingerprint(input, dtemplates, "document", .doc_fp_norm, .doc_tpl_name)
}

# `long` stacks every table into one frame so tables of different widths can share a file. ABSENT IS
# NOT EMPTY: a table found nowhere gets NO entry rather than one full of zeroes; its key goes into
# `misses` instead.
extract_document <- function(input, tmpl) {
  tabs <- tmpl$tables %||% list()
  keys <- names(tabs) %||% character(0)
  out <- list(); misses <- character(0)
  for (k in keys) {
    tab <- tabs[[k]]
    places <- if (identical(as.character(tab$occurrence %||% "once")[1], "all"))
      doc_locate_repeats(input, tab, tmpl) else integer(0)
    if (length(places) > 1L) {
      got <- 0L
      for (i in seq_along(places)) {
        one <- tab
        span <- .doc_int(tab$end$page, .doc_int(tab$start$page, 1L)) -
                .doc_int(tab$start$page, 1L)
        one$start$page <- places[i]
        one$end$page   <- places[i] + max(0L, span)
        lo <- doc_locate_table(input, one, tmpl)
        if (identical(as.character(lo$anchor %||% "")[1], "not_found")) next
        rr <- doc_table_rows(input, one, tmpl, lo)
        kk <- if (i == 1L) k else sprintf("%s__p%d", k, places[i])
        rr$key <- kk
        rr$name <- sprintf("%s (page %d)", as.character(tab$name %||% k)[1], places[i])
        out[[kk]] <- rr
        got <- got + 1L
      }
      if (got == 0L) misses <- c(misses, k)
      next
    }
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

# Two ways to say the document does not always carry this table: `optional: true`, or `occurrence:
# all`, where absence is the ordinary case and a permanent needs_review is a warning nobody reads.
.doc_table_optional <- function(tab) {
  isTRUE(tab$optional) ||
    identical(as.character(tab$occurrence %||% "once")[1], "all")
}

document_summary <- function(tables, misses = character(0), specs = list()) {
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

# Every word that fell inside a table's boundary and into NO column. On a read through bands 120pt out
# of place this held the four real closing balances: a count says something is wrong, this says what.
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

document_long <- function(tables) {
  empty <- data.frame(table = character(0), table_name = character(0),
                      page = integer(0), row = integer(0), column = character(0),
                      value = character(0), stringsAsFactors = FALSE)
  if (!length(tables)) return(empty)
  parts <- lapply(names(tables), function(k) {
    r <- tables[[k]]$rows
    if (is.null(r) || !nrow(r)) return(NULL)
    vals <- setdiff(names(r), c("page", "row"))
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

# Sheets the reader writes for itself, which therefore cannot also be a table's sheet: a table called
# "Report" made openxlsx refuse the workbook, and the CSV fallback was gated on "no openxlsx".
.DOC_RESERVED_SHEETS <- c("Build", "Values", "Report")

.doc_sheet_names <- function(tables, reserved = .DOC_RESERVED_SHEETS) {
  raw <- vapply(names(tables), function(k)
    as.character(tables[[k]]$name %||% k)[1], character(1))
  s <- gsub("[\\\\/?*\\[\\]:]", " ", raw, perl = TRUE)
  s <- trimws(gsub("\\s+", " ", s))
  s[!nzchar(s)] <- names(tables)[!nzchar(s)]
  s <- substr(s, 1, 31)
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

# The formula guard, on EVERY character column: a cell reading "=1+1" reached the file verbatim and
# Excel showed 2. Nothing is stripped - a leading quote is added - and the JSON stays verbatim.
.doc_csv_safe <- function(df) {
  if (is.null(df) || !ncol(df)) return(df)
  for (nm in names(df))
    if (is.character(df[[nm]]) || is.factor(df[[nm]]))
      df[[nm]] <- .neutralize_formula(as.character(df[[nm]]))
  df
}

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

# Several frames in one CSV, each under a one-line heading. The report CSV only; long and values
# stay plain single-table files.
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

# Returns the paths written with attr "notes". .report.csv carries the build stamp and the per-column
# fill report; .unclaimed.csv is written only when there are unclaimed words, so its existence is the
# signal.
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
      # Without these openxlsx stamps the wall clock into docProps/core.xml and the ZIP stamps each
      # entry's mod-time, so the same document read through the same template differed every run.
      wb$core <- .deterministic_core(wb$core)
      openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
      safe(.normalize_zip_timestamps(p))
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

# Never throws. There is no reconciliation here, so "ok" can only mean every table was found and none
# came out suspiciously empty; a template that matched but found NO rows is `unsupported`. Two further
# signals: UNCLAIMED WORDS inside a table's own boundary, and A TYPED COLUMN THAT PARSED NOTHING.
.DOC_MAX_UNCLAIMED <- 1L

.doc_parse_failures <- function(rep) {
  if (is.null(rep) || !nrow(rep) || !("table" %in% names(rep))) return(character(0))
  if ("wrong_kind" %in% names(rep))
    return(unique(as.character(rep$table[rep$wrong_kind %in% TRUE])))
  if (!("parse_rate" %in% names(rep))) return(character(0))
  rate <- suppressWarnings(as.numeric(rep$parse_rate))
  filled <- suppressWarnings(as.integer(rep$filled %||% rep(0L, nrow(rep))))
  unique(as.character(rep$table[!is.na(rate) & rate <= 0 & !is.na(filled) & filled > 0L]))
}

# The tables holding a band sitting under ANOTHER of that table's headings. Such a run is
# needs_review, because the figures are real figures under the wrong names.
.doc_wrong_columns <- function(rep) {
  if (is.null(rep) || !nrow(rep) || !("table" %in% names(rep))) return(character(0))
  if (!("wrong_column" %in% names(rep))) return(character(0))
  unique(as.character(rep$table[rep$wrong_column %in% TRUE]))
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
      # A tie is not an answer of "no": two templates that both fit used to mean "unsupported" on a
      # document the library reads perfectly, naming neither.
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
    tabs <- tmpl$tables %||% list()
    absent <- as.character(ext$misses %||% character(0))
    absent_pinned <- absent[!vapply(absent, function(k)
      .doc_table_optional(tabs[[k]] %||% list()), logical(1))]
    unclaimed <- as.integer(sum(as.integer(ext$summary$unclaimed_words %||% 0L),
                                na.rm = TRUE))
    spilt <- unique(as.character((ext$unclaimed %||% document_unclaimed(ext$tables))$table))
    parse_fail <- .doc_parse_failures(ext$report)
    wrong_col  <- .doc_wrong_columns(ext$report)
    res$empty_tables <- empty; res$thin_tables <- thin; res$weak_tables <- weak
    res$absent_tables <- absent; res$unclaimed_words <- unclaimed
    res$parse_fail_tables <- parse_fail
    res$wrong_column_tables <- wrong_col
    # Deliberately deterministic: the run id is stamped only when the caller hands one over and no
    # wall clock is written, so the same document through the same template gives the same bytes.
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
                           length(wrong_col) ||
                           ambiguous || unclaimed > .DOC_MAX_UNCLAIMED) "needs_review"
                  else "ok"
    me_why <- sprintf(
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
      else if (length(wrong_col))
      status_message("needs_review",
        sprintf("the columns on %s do not line up with the headings this copy prints",
                nm(wrong_col)),
        "this copy has probably gained or lost a column - check the figures against the page before using them")
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
    if (length(notes)) res$messages <- c(res$messages, notes)
    res
  }, error = function(e) {
    res$messages <- paste("error:", conditionMessage(e)); res
  })
}

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

# The way back from a saved template to the boxes it was drawn from, so "this column is 4pt too far
# left" does not mean editing YAML. Mostly re-keying, since the template keys by output name.
document_proposal_from_template <- function(t) {
  empty <- list(tables = list(), pairs = list())
  if (!is.list(t)) return(empty)
  tabs <- t$tables %||% list()
  prs  <- t$pairs %||% list()
  keys <- names(tabs) %||% rep("", length(tabs))
  tables <- lapply(seq_along(tabs), function(i) {
    tb <- tabs[[i]]
    if (!is.list(tb)) return(NULL)
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
    pr$name <- as.character(pr$name %||% (names(prs) %||% "")[i] %||% "")[1]
    pr$type <- as.character(pr$type %||% "text")[1]
    pr
  })
  list(tables = Filter(Negate(is.null), tables),
       pairs  = Filter(Negate(is.null), pairs))
}

# Per-table keys the builder does not ask about and the reader does use. Dropped on the way out,
# they were silently gone the first time a template was opened and saved.
.DOC_TABLE_CARRY <- c("row_tol", "max_gap", "doc_pages", "optional", "occurrence")

# `base` is the template being saved over, and it is the whole round-trip fix: rebuilding from a
# whitelist lost `hidden: true` (a parked template returned to live detection), `notes`, per-table
# row_tol and max_gap, and `version`. With a base the loaded template is MODIFIED and version += 1.
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
      # The band is carried: `end` is a place on the LAST page while `band$y_max` is where the
      # window closes on every page between. Dropped here, the footer came back in the rows.
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
       # No currency: nothing in this mode ever reads one, so stamping "NZD" on a report of euros
       # would be a false statement in a saved file.
       pairs = pkeyed, tables = keyed)
  if (!is.list(base) || !length(base)) return(built)
  out <- base
  out$origin <- NULL
  for (k in c("id", "bank", "statement_type", "format", "mode", "doc_pages",
              "fingerprint", "pairs", "tables"))
    out[[k]] <- built[[k]]
  v <- suppressWarnings(as.numeric(base$version %||% NA))
  out$version <- if (is.finite(v)) v + 1 else 1
  out
}
