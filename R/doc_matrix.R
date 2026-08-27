# doc_matrix.R -- THE TABLE WHOSE COLUMNS ARE DATES.
#
# WHAT IS DIFFERENT ABOUT IT. An ordinary report table has a fixed set of named
# columns and a variable number of rows. A filing-period matrix is the other way
# round: the ROWS are a known-ish list of measures, and the COLUMNS are whatever
# periods this customer happens to have filed for.
#
#   Filing Period   31-Mar-18   31-Mar-19   31-Mar-20   Total
#   Return Type     IR3         IR3         IR3
#   Taxable Income  41,200.00   38,905.00   44,010.00   124,115.00
#   Refund          (1,204.55)              (880.10)
#
# So a template CANNOT name the columns. A drafted template did exactly that --
# it wrote `31-Mar-18` in as a permanent column name -- and that template is
# wrong for every customer with a different filing history and for the same
# customer next year. The years are DATA. They are read off the document every
# time and never stored.
#
# WHY THE OUTPUT IS LONG, NOT WIDE. A wide sheet would have a different column
# set per report, so two customers could not be stacked and last year's workbook
# would not line up with this year's. One row per (measure, period) gives one
# stable schema for every report this will ever read:
#
#   measure | filing_period | value | value__value | is_summary | page
#
# `value` is what the page prints, verbatim, per the engine's contract. The
# `__value` companion is the parsed number, exactly the naming rule
# doc_table_rows() already uses for a typed column, so there is one convention in
# this codebase rather than two.
#
# WHAT THIS FILE DOES NOT DO. It does not find the table on the page (the table's
# own start/end and R/tables.R do that), and it knows nothing about IRD: the
# anchor wording, the period format and the summary column's aliases all come
# from the template.

# The row-key band is where the measure names are. Defaulted only so a
# half-written template fails as a bad read rather than an error.
DOC_MATRIX_MIN_PERIODS <- 1L
# How much of the gutter either side of a header cell belongs to its column.
# Half, which is what .doc_bands does for ordinary tables: a figure set under a
# date header is often right-aligned and reaches left of the header word.
DOC_MATRIX_GUTTER_SHARE <- 0.5

# .mx_cfg(tab) -- the matrix block, defaulted. One place, so every function below
# reads the same shape whatever the template left out.
.mx_cfg <- function(tab) {
  m <- .doc_key(tab, "matrix")
  if (!is.list(m)) m <- list()
  rk <- if (is.list(m$row_key)) m$row_key else list()
  pd <- if (is.list(m$periods)) m$periods else list()
  sm <- if (is.list(m$summary_column)) m$summary_column else list()
  ou <- if (is.list(m$output)) m$output else list()
  list(
    anchor = as.character(unlist(m$header_anchor %||% character(0))),
    key_name = as.character(rk$name %||% "measure")[1],
    key_min = .doc_num(rk$x_min, -Inf),
    key_max = .doc_num(rk$x_max, NA_real_),
    p_type = tolower(as.character(pd$type %||% "date")[1]),
    p_fmts = as.character(unlist(pd$formats %||% list("%d-%b-%Y", "%d-%b-%y"))),
    p_min = .doc_num((pd$x_region %||% list())$x_min, NA_real_),
    p_max = .doc_num((pd$x_region %||% list())$x_max, Inf),
    sum_aliases = as.character(unlist(sm$aliases %||% list("Total", "Totals", "Summary"))),
    sum_optional = !isFALSE(sm$optional),
    name_col = as.character(ou$name_column %||% "filing_period")[1],
    value_col = as.character(ou$value_column %||% "value")[1],
    blank_rows = tolower(as.character(m$blank_rows %||% "drop")[1]))
}

# doc_is_matrix(tab) -- is this table a period matrix.
doc_is_matrix <- function(tab)
  identical(tolower(as.character(.doc_key(tab, "shape") %||% "")[1]), "matrix")

# .mx_text(d) -- one cell's words as printed.
.mx_text <- function(d) trimws(paste(d$text, collapse = " "))

# .mx_is_summary(txt, aliases) -- is this header cell the total column.
.mx_is_summary <- function(txt, aliases) {
  k <- .doc_norm(txt)
  any(nzchar(k) & k == .doc_norm(aliases))
}

# .mx_resolve_format(labels, fmts) -- ONE format that reads EVERY period label.
#
# ALL OR NOTHING, and this is the whole reason it is a function. Reading some
# headers under "%d-%b-%y" and the rest under "%d-%b-%Y" would put two periods
# under one name or invent a year, and the figures beneath them would be filed
# against the wrong return. The statement side settles this the same way
# (resolve_date_format, R/templates.R); the rule is repeated here because the
# population is the header set, not a column of rows.
.mx_resolve_format <- function(labels, fmts) {
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels)) return(NA_character_)
  for (f in fmts) {
    # $iso: parse_date returns list(iso, raw), and using the list itself made
    # every format "match" and every label "parse".
    got <- suppressWarnings(parse_date(labels, f))$iso
    if (all(!is.na(got))) return(f)
  }
  NA_character_
}

# doc_matrix_header(input, tab, tmpl) -> the period columns THIS document prints.
#
# list(page, y, cols) where cols is a data.frame(label, period, x_min, x_max,
# is_summary), or NULL when no header could be validated.
#
# The bands are derived from the header cells and are never read from the
# template. A report with three periods and one with twelve are the same
# template; so are 2018-2020 and 2021-2023.
doc_matrix_header <- function(input, tab, tmpl = NULL) {
  cfg <- .mx_cfg(tab)
  if (!length(cfg$anchor) || is.na(cfg$key_max)) return(NULL)
  np <- length(input$words %||% list())
  if (!np) return(NULL)
  p0 <- max(1L, .doc_int(tab$start$page, 1L))
  p1 <- min(np, .doc_int(tab$end$page, np))
  if (p1 < p0) p1 <- np
  akeys <- .doc_norm(cfg$anchor)
  for (p in seq.int(p0, p1)) {
    w <- .doc_page_words(input, p, tmpl)
    if (is.null(w) || !nrow(w)) next
    for (d in .doc_lines(w, tab$row_tol)) {
      key <- d[d$x + d$width / 2 < cfg$key_max, , drop = FALSE]
      if (!nrow(key) || !(.doc_norm(.mx_text(key)) %in% akeys)) next
      # The cells to the RIGHT of the row-key band are the periods.
      rest <- d[d$x + d$width / 2 >= cfg$key_max, , drop = FALSE]
      if (!nrow(rest)) next
      cells <- .doc_cells(rest, .doc_gap_for(w))
      if (length(cells) < DOC_MATRIX_MIN_PERIODS) next
      labs <- vapply(cells, .mx_text, character(1))
      los <- vapply(cells, function(z) min(z$x), numeric(1))
      his <- vapply(cells, function(z) max(z$x + z$width), numeric(1))
      is_sum <- vapply(labs, .mx_is_summary, logical(1), aliases = cfg$sum_aliases,
                       USE.NAMES = FALSE)
      # The period labels must ALL read as the declared type. A header that does
      # not is not this table's header, and accepting it would file every figure
      # under a name that is not a period.
      plabs <- labs[!is_sum]
      if (!length(plabs)) return(NULL)
      fmt <- if (identical(cfg$p_type, "date"))
        .mx_resolve_format(plabs, cfg$p_fmts) else "text"
      if (is.na(fmt)) next
      period <- rep(NA_character_, length(labs))
      period[!is_sum] <- if (identical(cfg$p_type, "date"))
        suppressWarnings(parse_date(plabs, fmt))$iso else plabs
      # Bands: each cell keeps its own extent widened to the middle of the gutter
      # on each side. A figure right-aligned under its date header still lands in
      # its own column, and a MISSING cell cannot shift its neighbours -- every
      # value is placed by where it sits, never by how many came before it.
      n <- length(cells)
      xmin <- numeric(n); xmax <- numeric(n)
      for (i in seq_len(n)) {
        left <- if (i == 1L) cfg$key_max else his[i - 1L]
        right <- if (i == n) .doc_num(cfg$p_max, his[n] + 40) else los[i + 1L]
        xmin[i] <- los[i] - (los[i] - left) * DOC_MATRIX_GUTTER_SHARE
        xmax[i] <- his[i] + (right - his[i]) * DOC_MATRIX_GUTTER_SHARE
      }
      return(list(page = as.integer(p), y = .doc_num(attr(d, "top")), format = fmt,
                  cols = data.frame(label = labs, period = period,
                                    x_min = xmin, x_max = xmax,
                                    is_summary = is_sum, stringsAsFactors = FALSE)))
    }
  }
  NULL
}

# doc_matrix_rows(input, tab, tmpl, hdr) -> the matrix in LONG form.
#
# CONTINUATION PAGES CARRY NO HEADER, which is the ordinary case for these
# returns: page 1 establishes the periods and pages 2-5 simply go on listing
# measures. So the column model found once is reused for every following page,
# and a page that DOES reprint the header has that line consumed rather than read
# as a measure called "Filing Period".
doc_matrix_rows <- function(input, tab, tmpl = NULL, hdr = NULL) {
  cfg <- .mx_cfg(tab)
  hdr <- hdr %||% doc_matrix_header(input, tab, tmpl)
  long <- data.frame(measure = character(0), period = character(0),
                     label = character(0), value = character(0),
                     parsed = numeric(0), is_summary = logical(0),
                     page = integer(0), stringsAsFactors = FALSE)
  if (is.null(hdr)) return(list(rows = long, header = NULL, n_measures = 0L,
                                n_periods = 0L, dropped_blank = 0L))
  np <- length(input$words %||% list())
  p_end <- min(np, .doc_int(tab$end$page, np))
  if (p_end < hdr$page) p_end <- np
  cols <- lapply(seq_len(nrow(hdr$cols)), function(i)
    list(x_min = hdr$cols$x_min[i], x_max = hdr$cols$x_max[i]))
  akeys <- .doc_norm(cfg$anchor)
  out <- list(); measures <- character(0); dropped <- 0L
  for (p in seq.int(hdr$page, p_end)) {
    w <- .doc_page_words(input, p, tmpl)
    if (is.null(w) || !nrow(w)) next
    for (d in .doc_lines(w, tab$row_tol)) {
      top <- .doc_num(attr(d, "top"), NA_real_)
      if (p == hdr$page && !is.na(top) && top <= hdr$y) next     # above the header
      if (.doc_is_rule(d)) next
      key <- d[d$x + d$width / 2 < cfg$key_max, , drop = FALSE]
      measure <- if (nrow(key)) .mx_text(key) else ""
      # A reprinted header is not a measure.
      if (nzchar(measure) && .doc_norm(measure) %in% akeys) next
      rest <- d[d$x + d$width / 2 >= cfg$key_max, , drop = FALSE]
      j <- if (nrow(rest)) .doc_col_of(rest$x + rest$width / 2, cols) else integer(0)
      cells <- rep(NA_character_, nrow(hdr$cols))
      for (k in which(!is.na(j))) {
        add <- trimws(rest$text[k])
        cells[j[k]] <- if (is.na(cells[j[k]])) add else paste(cells[j[k]], add)
      }
      filled <- sum(!is.na(cells) & nzchar(cells))
      # A LINE WITH NO MEASURE AND NO VALUES is not a row of anything.
      if (!nzchar(measure) && filled == 0L) next
      # A measure with no values at all: the template decides. Some of these
      # returns genuinely print a measure that is blank for every period, and
      # dropping it silently would lose the fact that it was asked and answered
      # with nothing.
      if (filled == 0L && identical(cfg$blank_rows, "drop")) {
        dropped <- dropped + 1L; next
      }
      if (nzchar(measure)) measures <- c(measures, measure)
      for (i in seq_len(nrow(hdr$cols))) {
        v <- cells[i]
        if (is.na(v) || !nzchar(v)) next
        out[[length(out) + 1L]] <- data.frame(
          measure = measure, period = hdr$cols$period[i], label = hdr$cols$label[i],
          value = v, parsed = .doc_num(safe(parse_amount(v)$value, NA_real_), NA_real_),
          is_summary = hdr$cols$is_summary[i], page = as.integer(p),
          stringsAsFactors = FALSE)
      }
    }
  }
  if (length(out)) long <- do.call(rbind, out)
  list(rows = long, header = hdr, n_measures = length(unique(measures)),
       n_periods = sum(!hdr$cols$is_summary), dropped_blank = dropped)
}

# doc_matrix_extract(input, tab, tmpl) -> the same shape doc_table_rows() returns,
# so doc_extract.R can put a matrix table through the ordinary outputs without
# knowing it is one.
#
# The frame's column NAMES come from the template (`filing_period` / `value` by
# default) so a site can call them what its analysts call them, while the SHAPE
# is fixed for every matrix this will ever read.
doc_matrix_extract <- function(input, tab, tmpl = NULL) {
  cfg <- .mx_cfg(tab)
  r <- doc_matrix_rows(input, tab, tmpl)
  empty <- data.frame(page = integer(0), row = integer(0), stringsAsFactors = FALSE)
  empty[[cfg$key_name]] <- character(0)
  empty[[cfg$name_col]] <- character(0)
  empty[[cfg$value_col]] <- character(0)
  empty[[paste0(cfg$value_col, "__value")]] <- numeric(0)
  empty$period_label <- character(0)
  empty$is_summary <- logical(0)
  if (!nrow(r$rows)) {
    detail <- if (is.null(r$header))
      "no filing-period header was found, so this matrix was not read"
      else "the filing-period header was found but no measures were read under it"
    return(list(rows = empty, report = .mx_report(cfg, r), spilled = empty[0, ],
                stops = data.frame(), row_fill = numeric(0), n_rows = 0L,
                n_header = 0L, anchor = if (is.null(r$header)) "not_found" else "header",
                confidence = 0, last_page = NA_integer_, last_y = NA_real_,
                detail = detail, matrix = r, pages = integer(0), header_rows = 1L))
  }
  rows <- data.frame(page = r$rows$page, row = seq_len(nrow(r$rows)),
                     stringsAsFactors = FALSE)
  rows[[cfg$key_name]] <- r$rows$measure
  rows[[cfg$name_col]] <- r$rows$period
  rows[[cfg$value_col]] <- r$rows$value
  rows[[paste0(cfg$value_col, "__value")]] <- r$rows$parsed
  rows$period_label <- r$rows$label
  rows$is_summary <- r$rows$is_summary
  detail <- sprintf(
    "read as a filing-period matrix: %d measure%s across %d period%s, found on page %d",
    r$n_measures, if (r$n_measures == 1L) "" else "s",
    r$n_periods, if (r$n_periods == 1L) "" else "s", r$header$page)
  if (any(r$header$cols$is_summary))
    detail <- paste0(detail, "; the last column is a total and is marked as one, not counted as a period")
  if (r$dropped_blank > 0L)
    detail <- paste0(detail, sprintf(
      "; %d measure%s printed with no value in any period and %s not read (blank_rows: drop)",
      r$dropped_blank, if (r$dropped_blank == 1L) "" else "s",
      if (r$dropped_blank == 1L) "was" else "were"))
  list(rows = rows, report = .mx_report(cfg, r), spilled = empty[0, ],
       stops = data.frame(), row_fill = rep(1, nrow(rows)), n_rows = nrow(rows),
       n_header = 1L, anchor = "header", confidence = 1,
       last_page = max(r$rows$page), last_y = NA_real_,
       detail = detail, matrix = r,
       pages = sort(unique(r$rows$page)), header_rows = 1L)
}

# .mx_report(cfg, r) -- the per-column report, in the shape the document outputs
# already expect. A matrix has a fixed set of output columns whatever the
# document's periods are, which is the point of the long shape.
.mx_report <- function(cfg, r) {
  nm <- c(cfg$key_name, cfg$name_col, cfg$value_col)
  n <- if (is.null(r$rows)) 0L else nrow(r$rows)
  data.frame(column = nm, type = c("text", "date", "money"),
             filled = rep(n, 3L), fill_rate = rep(if (n) 1 else 0, 3L),
             thin = rep(FALSE, 3L), stringsAsFactors = FALSE)
}
