
.TEMPLATE_COMMON <- c("id", "bank", "statement_type", "format", "version",
                      "min_score", "fingerprint", "currency")
.VALID_SIGN <- c("signed", "debit_credit_cols", "dr_cr_suffix", "type_dc", "unsigned")
.META_REGION_FIELDS <- c("opening_balance", "closing_balance", "period_start",
                         "period_end", "account_number", "account_name")

.FP_UNIVERSAL_COLUMNS <- c("date", "amount", "description", "balance", "details",
  "memo", "note", "notes", "type", "credit", "debit", "value", "total",
  "reference", "ref", "code", "id", "name", "number", "no", "account", "currency",
  "particulars", "payee", "comment", "comments", "category", "transaction",
  "deposit", "deposits", "withdrawal", "withdrawals")

# The load-time gate on a template's fingerprint. DISTINCTIVENESS: a fingerprint of words on every
# statement matches statements it has never seen and parses them with another bank's bands - the ASB
# PDF template scored its min_score of 2 on the bare words "Deposit" and "Balance", so a foreign bank's
# PDF was parsed as ASB with charges emitted as credits and the feed ACCEPTED it. Stricter for PDF: a
# PDF phrase matches anywhere in the page text, while a delimited phrase must equal a whole COLUMN
# NAME. NO PII: template files are listed in Admin and copied into the offline bundle.
.fp_fingerprint_problems <- function(ph, min_score, fmt) {
  problems <- character(0)
  ph <- trimws(as.character(unlist(ph %||% character(0))))
  ph <- ph[!is.na(ph) & nzchar(ph)]

  pii <- if (length(ph)) .fp_has_pii(ph) else logical(0)
  if (any(pii)) problems <- c(problems, sprintf(paste0(
    "fingerprint phrase %s names a PERSON -- a fingerprint must identify the ",
    "LAYOUT, never a customer. Use a statement heading or a column label instead."),
    paste(sprintf("'%s'", ph[pii]), collapse = ", ")))

  ms <- suppressWarnings(as.numeric(min_score %||% length(ph))[1])
  if (is.na(ms)) ms <- length(ph)

  if (identical(fmt, "pdf")) {
    n_generic <- if (length(ph)) sum(!.fp_specific(ph)) else 0L
    if (!length(ph) || n_generic >= ms) problems <- c(problems, paste0(
      "fingerprint.page_contains_all is too generic for a pdf template: min_score ",
      "can be reached by generic phrases alone -- add a distinctive phrase (a ",
      "statement heading or a multi-word column label, not single words like ",
      "'Balance'), or raise min_score so a distinctive one is always required"))
  } else {
    key <- gsub("[^a-z0-9]", "", tolower(ph))
    generic <- if (length(ph)) !.fp_specific(ph) & (key %in% .FP_UNIVERSAL_COLUMNS) else logical(0)
    if (!length(ph) || all(generic)) problems <- c(problems, paste0(
      "fingerprint.header_contains_all is too generic: every column name in it ",
      "(", paste(ph, collapse = ", "), ") is one every statement carries, so this ",
      "template would match other banks' files -- include at least one column name ",
      "that is specific to this layout"))
  }
  problems
}

# One detector for the form and report routes; same return shape as detect_statement(). The
# normaliser stays a parameter because .form_fp_norm and .doc_fp_norm are deliberate separate copies.
# Two things the per-route detectors could not say: a TIE returned "unsupported" on a document the
# library reads perfectly and named neither template, and a NEAR MISS said nothing actionable.
.detect_by_fingerprint <- function(input, templates, noun, norm, name_fn) {
  none <- function(detail, plain = NULL)
    list(template_id = NA_character_, matched = FALSE, score = 0,
         candidates = data.frame(id = character(0), score = numeric(0),
                                 need = numeric(0), stringsAsFactors = FALSE),
         eligible_ids = character(0), tied = character(0),
         margin = NA_real_, runner_up = NA_character_,
         detail = detail, detail_plain = plain)
  if (!length(templates))
    return(none(sprintf("no %s templates are installed", noun)))

  hay <- norm(paste(input$pages %||% character(0), collapse = "\n"))
  ids <- names(templates)
  if (is.null(ids)) ids <- as.character(seq_along(templates))
  ids[!nzchar(ids)] <- as.character(seq_along(templates))[!nzchar(ids)]
  names(templates) <- ids
  sc <- lapply(ids, function(i) {
    need <- as.character(unlist(templates[[i]]$fingerprint$page_contains_all %||%
                                  character(0)))
    hit <- if (!length(need)) logical(0) else
      vapply(need, function(ph) {
        k <- norm(ph)
        nzchar(k) && grepl(k, hay, fixed = TRUE, useBytes = TRUE)
      }, logical(1))
    list(score = sum(hit), need = length(need), missing = need[!hit])
  })
  scores <- vapply(sc, function(s) as.numeric(s$score), numeric(1))
  needs  <- vapply(sc, function(s) as.numeric(s$need), numeric(1))
  eligible <- needs > 0 & scores >= needs

  shipped <- vapply(ids, function(i)
    as.numeric(!identical(templates[[i]]$origin %||% "default", "user")), numeric(1))
  ord <- order(scores, needs, shipped, ids,
               decreasing = c(TRUE, TRUE, TRUE, FALSE), method = "radix")
  ids <- ids[ord]; scores <- scores[ord]; needs <- needs[ord]
  shipped <- shipped[ord]; eligible <- eligible[ord]; sc <- sc[ord]
  cand_df <- data.frame(id = ids, score = scores, need = needs,
                        stringsAsFactors = FALSE)

  if (!any(eligible)) {
    best <- ids[1]; miss <- sc[[1]]$missing
    return(list(template_id = NA_character_, matched = FALSE, score = 0,
      candidates = cand_df, eligible_ids = character(0), tied = character(0),
      margin = NA_real_, runner_up = if (length(ids) >= 2) ids[2] else NA_character_,
      detail = sprintf("closest %s score %g/%g%s", best, scores[1], needs[1],
        if (length(miss)) sprintf(" (missing %s)",
          paste(sprintf("'%s'", miss), collapse = ", ")) else ""),
      detail_plain = sprintf("The closest we have is the %s, but this file doesn't print %s.",
        name_fn(templates[[best]]),
        if (length(miss)) paste(sprintf("\"%s\"", miss), collapse = " or ")
        else "the wording it looks for")))
  }

  e_ids <- ids[eligible]; e_needs <- needs[eligible]; e_ship <- shipped[eligible]
  win <- e_ids[1]
  second_need <- if (length(e_needs) >= 2) e_needs[2] else -Inf
  second_ship <- if (length(e_ship) >= 2) e_ship[2] else -Inf
  matched <- (e_needs[1] > second_need) ||
             (e_needs[1] == second_need && e_ship[1] > second_ship)
  tied <- if (sum(eligible) >= 2) e_ids[e_needs == e_needs[1] & e_ship == e_ship[1]]
          else character(0)
  if (length(tied) < 2L) tied <- character(0)
  list(template_id = win, matched = matched, score = e_needs[1],
       candidates = cand_df, eligible_ids = e_ids, tied = tied,
       margin = if (is.finite(second_need)) e_needs[1] - second_need else Inf,
       runner_up = if (length(e_ids) >= 2) e_ids[2] else NA_character_,
       detail = if (matched) "matched by identifying phrases"
                else sprintf("%d %s templates are equally specific here (%s)",
                             length(tied), noun, paste(tied, collapse = ", ")),
       detail_plain = if (matched) NULL else
         sprintf("%d templates fit this document equally well, so it was read with the %s.",
                 length(tied), name_fn(templates[[win]])))
}

# THE ONE SAVER the three kinds share: strip the load-time origin, refuse an invalid template loudly,
# write <dir>/<id>.yaml safely. Only the statement saver had the slug guard, so a report template
# called "ACME Q1!" silently overwrote one called "ACME-Q1". THE SLUG GUARD: two distinct ids can
# sanitise to one file slug, so only overwrite when the existing file holds the SAME original id.
# Safely means a backup, a temp file and an atomic rename - a bare write_yaml truncates first.
.save_template_yaml <- function(t, dir, validate_fn, noun) {
  t$origin <- NULL   # origin is assigned at load time, not stored
  probs <- validate_fn(t)
  if (length(probs)) stop(noun, " is not valid: ", paste(probs, collapse = "; "))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  base_slug <- gsub("[^A-Za-z0-9_]+", "_", t$id %||% "user_template")
  slug <- base_slug; k <- 2L
  repeat {
    path <- file.path(dir, paste0(slug, ".yaml"))
    if (!file.exists(path)) break
    existing_id <- tryCatch(yaml::read_yaml(path)$id, error = function(e) NULL)
    if (identical(existing_id, t$id)) break
    slug <- paste0(base_slug, "_", k); k <- k + 1L
  }
  path <- file.path(dir, paste0(slug, ".yaml"))
  ok <- save_yaml_safely(t, path)
  if (!isTRUE(ok)) stop(attr(ok, "reason") %||% "the file could not be written")
  invisible(path)
}

# The ONE declared format that reads EVERY non-empty value, or NA when none does; a template may
# declare a LIST because one bank's export carries two date styles across eras. All-or-nothing:
# reading some rows under one format and the rest under another would swap day and month.
resolve_date_format <- function(values, formats) {
  fmts <- trimws(as.character(unlist(formats %||% character(0))))
  fmts <- fmts[!is.na(fmts) & nzchar(fmts)]
  if (!length(fmts)) return(NA_character_)
  if (length(fmts) == 1L) return(fmts[1])       # the ordinary case, behaviour unchanged
  v <- as.character(unlist(values))
  v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(fmts[1])               # nothing to judge on -> as declared
  for (f in fmts) if (all(!is.na(parse_date(v, f)$iso))) return(f)
  NA_character_
}

# The ONE declared delimiter, from a list because the same bank ships the same layout as CSV and as
# tab-delimited. A candidate wins only if the header row split by it contains EVERY column name the
# fingerprint names. The HEADER only: depending on data rows would let one ragged row reject it.
resolve_delimiter <- function(header_line, template) {
  d <- as.character(unlist(template$delimiter %||% ","))
  d <- d[!is.na(d) & nzchar(d)]
  if (!length(d)) return(",")
  if (length(d) == 1L) return(d[1])            # the ordinary case, behaviour unchanged
  need <- trimws(as.character(unlist(template$fingerprint$header_contains_all %||% character(0))))
  need <- need[!is.na(need) & nzchar(need)]
  hl <- as.character(header_line)[1]
  if (!length(need) || is.na(hl) || !nzchar(hl)) return(d[1])
  for (delim in d) {
    fields <- trimws(as.character(.record_fields(hl, delim)))
    if (all(tolower(need) %in% tolower(fields))) return(delim)
  }
  d[1]
}

.delimiter_problems <- function(spec) {
  if (is.null(spec)) return(character(0))
  d <- as.character(unlist(spec))
  bad <- is.na(d) | !nzchar(d)
  if (any(bad)) return("delimiter entries must each be a non-empty separator string")
  character(0)
}

# A LIST is allowed only where the reader resolves it; elsewhere it must be REJECTED loudly, because
# as.Date() recycles a format vector element-wise - row 1 as %Y/%m/%d, row 2 as %d/%m/%Y - which is a
# whole column of plausible, wrong dates.
.date_format_problems <- function(spec, where, allow_list) {
  if (is.null(spec)) return(character(0))
  f <- as.character(unlist(spec))
  if (length(f) > 1L && !allow_list)
    return(sprintf("%s must be a single date format, not a list", where))
  bad <- is.na(f) | !nzchar(trimws(f)) | !grepl("%", f, fixed = TRUE)
  if (any(bad))
    return(sprintf("%s entry %s is not a date format (expected strptime codes like %%d/%%m/%%Y)",
                   where, paste(sprintf("'%s'", f[bad]), collapse = ", ")))
  character(0)
}

.one_date_format <- function(spec) {
  f <- as.character(unlist(spec %||% character(0)))
  f <- f[!is.na(f) & nzchar(f)]
  if (!length(f)) NA_character_ else paste(f, collapse = " | ")
}

validate_template <- function(t) {
  problems <- character(0)
  if (!is.list(t)) return("template is not a mapping")
  fmt <- if (is.null(t$format)) "delimited" else t$format

  for (k in .TEMPLATE_COMMON)
    if (is.null(t[[k]])) problems <- c(problems, sprintf("missing key '%s'", k))
  valid_fmt <- c("delimited", "excel", "pdf")
  if (!is.null(t$format) && !(fmt %in% valid_fmt))
    problems <- c(problems, sprintf("format '%s' is not one of %s", fmt,
                                    paste(valid_fmt, collapse = "/")))

  dm <- t$decimal_mark %||% t$table$decimal_mark
  if (!is.null(dm) && !(dm %in% c("auto", "dot", "comma")))
    problems <- c(problems, sprintf("decimal_mark '%s' is not one of auto/dot/comma", dm))

  # A window that CLOSES BEFORE IT OPENS can never contain a statement, and the only sign is that
  # everything it should have caught comes back "no template for this layout yet".
  eff_from <- .tolerant_date(t$effective_from)
  eff_to   <- .tolerant_date(t$effective_to)
  if (!is.na(eff_from) && !is.na(eff_to) && eff_from > eff_to)
    problems <- c(problems, sprintf(
      paste("effective_from (%s) is later than effective_to (%s), so this template",
            "could never apply to any statement - swap the two dates, or clear the",
            "one you did not mean to set"),
      as.character(t$effective_from)[1], as.character(t$effective_to)[1]))

  if (identical(fmt, "pdf")) {
    if (!is.null(t$fingerprint) && is.null(t$fingerprint$page_contains_all))
      problems <- c(problems, "fingerprint.page_contains_all is required for pdf templates")
    if (!is.null(t$fingerprint) && !is.null(t$fingerprint$page_contains_all))
      problems <- c(problems, .fp_fingerprint_problems(
        t$fingerprint$page_contains_all, t$min_score, "pdf"))
    problems <- c(problems, .date_format_problems(t$table$date_format,
                                                  "table.date_format", allow_list = FALSE))
    tab <- t$table
    if (is.null(tab)) {
      problems <- c(problems, "pdf templates require a 'table' block")
    } else {
      for (k in c("date", "description"))
        if (is.null(tab$columns[[k]]))
          problems <- c(problems, sprintf("table.columns.%s is required", k))
      if (identical(tab$amount_sign, "debit_credit_cols")) {
        for (k in c("debit", "credit"))
          if (is.null(tab$columns[[k]]))
            problems <- c(problems, sprintf("table.amount_sign 'debit_credit_cols' requires table.columns.%s", k))
      } else if (is.null(tab$columns[["amount"]])) {
        problems <- c(problems, "table.columns.amount is required")
      }
      if (!is.null(tab$amount_sign) && !(tab$amount_sign %in% .VALID_SIGN))
        problems <- c(problems, sprintf("table.amount_sign '%s' is invalid", tab$amount_sign))
      if (!is.null(tab$metadata_regions)) {
        mr <- tab$metadata_regions
        if (!is.list(mr)) problems <- c(problems, "table.metadata_regions must be a mapping")
        else for (field in names(mr)) {
          if (!(field %in% .META_REGION_FIELDS))
            problems <- c(problems, sprintf("table.metadata_regions.%s is not a header field (one of %s)",
                                            field, paste(.META_REGION_FIELDS, collapse = "/")))
          reg <- mr[[field]]
          if (!is.list(reg) || is.null(reg$x_min) || is.null(reg$x_max))
            problems <- c(problems, sprintf("table.metadata_regions.%s is malformed (needs x_min and x_max)", field))
        }
      }
    }
  } else {
    if (is.null(t$columns)) problems <- c(problems, "missing key 'columns'")
    if (is.null(t$amount_sign)) problems <- c(problems, "missing key 'amount_sign'")
    if (!is.null(t$fingerprint) && is.null(t$fingerprint$header_contains_all))
      problems <- c(problems, "fingerprint.header_contains_all is required")
    if (!is.null(t$fingerprint) && !is.null(t$fingerprint$header_contains_all))
      problems <- c(problems, .fp_fingerprint_problems(
        t$fingerprint$header_contains_all, t$min_score, fmt))
    problems <- c(problems, .date_format_problems(t$columns$date$format,
                                                  "columns.date.format", allow_list = TRUE))
    problems <- c(problems, .delimiter_problems(t$delimiter))
    # A debit_credit_cols template has no single 'amount' column, so requiring one here would reject
    # the tool's own draft of a very common CSV shape.
    always <- if (identical(t$amount_sign, "debit_credit_cols")) c("date", "description")
              else c("date", "amount", "description")
    if (!is.null(t$columns)) for (k in always)
      if (is.null(t$columns[[k]])) problems <- c(problems, sprintf("columns.%s is required", k))
    if (!is.null(t$amount_sign) && !(t$amount_sign %in% .VALID_SIGN))
      problems <- c(problems, sprintf("amount_sign '%s' is not one of %s",
                                      t$amount_sign, paste(.VALID_SIGN, collapse = "/")))
    .has_col <- function(field) !is.null(t$columns) && !is.null(t$columns[[field]])
    if (identical(t$amount_sign, "debit_credit_cols")) for (k in c("debit", "credit"))
      if (!.has_col(k)) problems <- c(problems,
        sprintf("amount_sign 'debit_credit_cols' requires columns.%s", k))
    if (identical(t$amount_sign, "type_dc")) {
      if (!.has_col("type"))
        problems <- c(problems, "amount_sign 'type_dc' requires columns.type")
      if (is.null(t$type_debit_value) || !nzchar(trimws(as.character(t$type_debit_value))))
        problems <- c(problems, "amount_sign 'type_dc' requires type_debit_value (which indicator value means a debit)")
    }
  }

  if (!is.null(t$split) && !isFALSE(t$split)) {
    if (!identical(fmt, "pdf"))
      problems <- c(problems, "split is only supported for pdf templates")
    if (!isTRUE(t$split)) {
      if (!is.list(t$split)) problems <- c(problems, "split must be true or a mapping")
      else {
        on <- t$split$on
        if (!is.null(on) && !(tolower(as.character(on)[1]) %in% c("page1_marker", "opening_label")))
          problems <- c(problems, sprintf("split.on '%s' is not one of page1_marker/opening_label", on))
        ms <- suppressWarnings(as.integer(t$split$min_statements %||% 2L))
        if (is.na(ms) || ms < 2L)
          problems <- c(problems, "split.min_statements must be an integer >= 2")
      }
    }
  }

  extras <- if (identical(fmt, "pdf")) t$table$extras else t$extras
  if (!is.null(extras)) {
    if (!is.list(extras)) problems <- c(problems, "extras must be a mapping")
    else for (field in names(extras)) {
      fs <- extras[[field]]
      ok <- if (identical(fmt, "pdf"))
              is.list(fs) && !is.null(fs$x_min) && !is.null(fs$x_max)
            else { src <- if (is.list(fs)) fs$source else fs
                   !is.null(src) && nzchar(as.character(src)[1]) }
      if (!isTRUE(ok)) problems <- c(problems, sprintf("extras.%s is malformed", field))
    }
  }
  problems
}

# origin ("default" | "user") is stamped so the app can tell a curated team template from one an
# accountant created. strict=FALSE skips an invalid template with a warning, so one bad user
# template cannot break everyone's conversions.
load_templates <- function(dir, origin = "default", strict = TRUE) {
  if (!dir.exists(dir)) {
    if (strict) stop(sprintf("templates dir not found: %s", dir))
    return(list())
  }
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  templates <- list()
  errors <- character(0)
  for (f in files) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) e)
    if (inherits(t, "error")) {
      errors <- c(errors, sprintf("%s: YAML parse error: %s",
                                  basename(f), conditionMessage(t)))
      next
    }
    # An unfinished seed is not a template: the shipped seeds' bands are placeholders marked in
    # comments, and YAML drops comments, so once copied into statements_user/ nothing said so.
    if (isTRUE(t$draft)) {
      errors <- c(errors, sprintf("%s: this is an unfinished seed template - draw its bands in the toolkit, then delete the `draft: true` line",
                                  t$id %||% basename(f)))
      next
    }
    probs <- validate_template(t)
    if (length(probs)) {
      label <- if (!is.null(t$id)) t$id else basename(f)
      errors <- c(errors, sprintf("%s: %s", label, paste(probs, collapse = "; ")))
      next
    }
    if (!is.null(templates[[t$id]])) {
      errors <- c(errors, sprintf("%s: duplicate template id", t$id))
      next
    }
    t$origin <- origin
    templates[[t$id]] <- t
  }
  if (length(errors)) {
    if (strict) stop("Invalid template(s):\n", paste(errors, collapse = "\n"))
    warning("Skipped invalid user template(s):\n", paste(errors, collapse = "\n"))
  }
  # A skipped template DISAPPEARS - it stops matching, stops showing in Admin, and the only trace is
  # an R warning nobody reads. Failing closed is right; doing it silently is not.
  attr(templates, "load_errors") <- errors
  templates
}

# Curated defaults load first and WIN on any id clash, so a user template can never shadow a
# team-blessed one. A `hidden: true` user template drops out of detection without being deleted.
load_template_set <- function(default_dir = "templates/statements",
                              user_dir = "templates/statements_user",
                              include_hidden = FALSE) {
  d <- load_templates(default_dir, origin = "default", strict = TRUE)
  # Carry the skip reasons across the merge: subsetting a list DROPS its attributes, so a user
  # template dropped for a generic fingerprint would vanish with no explanation anywhere.
  errors <- as.character(attr(d, "load_errors") %||% character(0))
  if (!is.null(user_dir) && dir.exists(user_dir)) {
    u <- load_templates(user_dir, origin = "user", strict = FALSE)
    errors <- c(errors, as.character(attr(u, "load_errors") %||% character(0)))
    if (!include_hidden)
      u <- u[!vapply(u, function(t) isTRUE(t$hidden), logical(1))]
    for (id in names(u)) if (is.null(d[[id]])) d[[id]] <- u[[id]]
  }
  # Sample templates are worked examples: their generic wording could match or tie with a real
  # statement and mis-parse it with a demo's hardcoded bands. Still loadable for the walkthrough.
  out <- d[!vapply(d, function(t) isTRUE(t$sample), logical(1))]
  attr(out, "load_errors") <- errors   # re-attached AFTER the subset (which drops attrs)
  out
}

template_overview <- function(tset) {
  cols <- c("id", "bank", "type", "format", "amount_sign", "date_format", "origin", "hidden", "version")
  if (!length(tset))
    return(setNames(data.frame(matrix(character(0), 0, length(cols))), cols))
  rows <- lapply(tset, function(t) {
    is_pdf <- identical(t$format %||% "delimited", "pdf")
    data.frame(
      id          = t$id %||% NA_character_,
      bank        = t$bank %||% NA_character_,
      type        = t$statement_type %||% NA_character_,
      format      = t$format %||% "delimited",
      amount_sign = (if (is_pdf) t$table$amount_sign else t$amount_sign) %||% "signed",
      date_format = .one_date_format(if (is_pdf) t$table$date_format else t$columns$date$format),
      origin      = if (identical(t$origin %||% "default", "user")) "user" else "tested",
      hidden      = if (isTRUE(t$hidden)) "hidden" else "",
      version     = as.character(t$version %||% NA),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out[order(out$bank, out$type, out$id), , drop = FALSE]
}

# One library, three kinds. Admin read load_template_set() and nothing else, so a form or report
# template was invisible there - it could not be found, checked, hidden or deleted.

# Reads the template's own `mode`, which is what every loader and detector dispatches on.
template_kind <- function(t) {
  switch(as.character(t$mode %||% "statement")[1],
         fields = "fields", document = "document", "statement")
}

.TEMPLATE_KIND_LABEL <- c(statement = "Bank statement",
                          fields    = "Other \u00b7 form",
                          document  = "Other \u00b7 report")
.TEMPLATE_KIND_NOUN <- c(statement = "bank statement", fields = "form",
                         document  = "report")

template_library_name <- function(t) {
  if (is.null(t) || !is.list(t)) return(NA_character_)
  k <- template_kind(t)
  if (identical(k, "statement")) return(template_display_name(t))
  lab <- trimws(paste(trimws(as.character(t$bank %||% "")),
                      trimws(as.character(t$statement_type %||% ""))))
  if (nzchar(lab)) lab else as.character(t$id %||% NA_character_)
}

.template_reads <- function(t) {
  k <- template_kind(t)
  n1 <- function(n, one, many) sprintf("%d %s", n, if (n == 1L) one else many)
  if (identical(k, "fields")) return(n1(length(t$fields %||% list()), "value", "values"))
  if (identical(k, "document")) {
    nt <- length(t$tables %||% list()); np <- length(t$pairs %||% list())
    parts <- c(if (nt) n1(nt, "table", "tables"),
               if (np) n1(np, "value", "values"))
    return(if (length(parts)) paste(parts, collapse = " + ") else "nothing yet")
  }
  cols <- if (identical(t$format %||% "delimited", "pdf")) t$table$columns else t$columns
  n1(length(cols %||% list()), "column", "columns")
}

library_overview <- function(statements = list(), fields = list(), documents = list()) {
  cols <- c("kind", "name", "id", "reads", "origin", "hidden", "version")
  one <- function(t, k) data.frame(
    kind    = unname(.TEMPLATE_KIND_LABEL[[k]]),
    name    = template_library_name(t) %||% NA_character_,
    id      = as.character(t$id %||% NA_character_)[1],
    reads   = .template_reads(t),
    origin  = if (identical(t$origin %||% "default", "user")) "user" else "tested",
    hidden  = if (isTRUE(t$hidden)) "hidden" else "",
    version = as.character(t$version %||% NA),
    stringsAsFactors = FALSE)
  rows <- c(lapply(statements, one, k = "statement"),
            lapply(fields,     one, k = "fields"),
            lapply(documents,  one, k = "document"))
  if (!length(rows))
    return(setNames(data.frame(matrix(character(0), 0, length(cols))), cols))
  out <- do.call(rbind, rows); rownames(out) <- NULL
  ord <- match(out$kind, unname(.TEMPLATE_KIND_LABEL))
  out[order(ord, out$name, out$id), , drop = FALSE]
}

# The template said in words the person holding the statement can check. The id and version stay for
# the logs, since an id is a maintainer's handle an accountant cannot act on.
template_display_name <- function(t) {
  if (is.null(t) || !is.list(t)) return(NA_character_)
  lab <- trimws(paste(trimws(as.character(t$bank %||% "")),
                      trimws(as.character(t$statement_type %||% ""))))
  if (nzchar(lab)) paste(lab, "statement") else as.character(t$id %||% NA_character_)
}

template_yaml <- function(t) { t$origin <- NULL; yaml::as.yaml(t) }

# A structural signature: two templates sharing one are the same layout drafted twice. ONE BRANCH PER
# KIND, because built from the statement keys alone every form and report template collapsed to the
# same string and the duplicate grouper put the whole lot in one group.
.tpl_key <- function(x) gsub("[^a-z0-9]+", "", tolower(as.character(x %||% "")))

.template_shape <- function(t) {
  kind <- template_kind(t)
  if (identical(kind, "document")) return(.template_shape_document(t))
  if (identical(kind, "fields"))   return(.template_shape_fields(t))
  is_pdf <- identical(t$format %||% "delimited", "pdf")
  cols   <- if (is_pdf) t$table$columns else t$columns
  sign   <- if (is_pdf) t$table$amount_sign else t$amount_sign
  dfmt   <- .one_date_format(if (is_pdf) t$table$date_format else t$columns$date$format)
  if (is.na(dfmt)) dfmt <- ""
  colsig <- if (is.null(cols)) "" else paste(sort(vapply(names(cols), function(k) {
    c <- cols[[k]]
    if (is_pdf) sprintf("%s:%s-%s", k, c$x_min %||% "", c$x_max %||% "")
    else sprintf("%s:%s", k, (if (is.list(c)) c$source else c) %||% "")
  }, character(1))), collapse = "|")
  paste(t$format %||% "delimited", sign %||% "", dfmt %||% "", colsig, sep = "~~")
}

.template_fp_sig <- function(t) {
  ph <- unlist(t$fingerprint$page_contains_all %||%
               t$fingerprint$header_contains_all %||% character(0))
  paste(sort(unique(.tpl_key(ph))), collapse = "+")
}

.template_shape_document <- function(t) {
  tabs <- t$tables %||% list()
  nms <- names(tabs) %||% rep("", length(tabs))
  tsig <- if (!length(tabs)) character(0) else vapply(seq_along(tabs), function(i) {
    tb <- tabs[[i]]
    cols <- tb$columns %||% list()
    band <- if (!length(cols)) "" else paste(vapply(seq_along(cols), function(j) {
      cc <- cols[[j]]
      sprintf("%s-%s", cc$x_min %||% "", cc$x_max %||% "")
    }, character(1)), collapse = ",")
    title <- .tpl_key(tb$name %||% nms[i])
    if (!nzchar(title)) title <- .tpl_key(nms[i])
    paste0(title, "[", band, "]")
  }, character(1))
  prs <- .tpl_key(names(t$pairs %||% list()) %||% character(0))
  paste("document", .template_fp_sig(t), paste(sort(tsig), collapse = "|"),
        paste(sort(prs), collapse = ","), sep = "~~")
}

.template_shape_fields <- function(t) {
  flds <- t$fields %||% list()
  nms <- names(flds) %||% rep("", length(flds))
  wl <- if (!length(flds)) character(0) else vapply(seq_along(flds), function(i) {
    spec <- flds[[i]]
    if (is.character(spec)) spec <- list(any_of = spec)
    w <- if (is.list(spec)) unlist(spec$any_of %||% spec$label %||% character(0)) else character(0)
    if (!length(w)) w <- nms[i]
    paste(sort(unique(.tpl_key(w))), collapse = "/")
  }, character(1))
  paste("fields", paste(sort(wl), collapse = "|"), sep = "~~")
}

# Groups of templates that share a layout - the near-duplicates to consolidate.
duplicate_template_groups <- function(tset, user_only = TRUE) {
  if (user_only) tset <- Filter(function(t) identical(t$origin %||% "default", "user"), tset)
  if (length(tset) < 2) return(list())
  sig <- vapply(tset, .template_shape, character(1))
  groups <- split(names(tset), sig)
  unname(groups[vapply(groups, length, integer(1)) > 1L])
}

save_user_template <- function(template, dir = "templates/statements_user") {
  .save_template_yaml(template, dir, validate_template, "template")
}

user_template_ids <- function(dir = "templates/statements_user") {
  if (!dir.exists(dir)) return(character(0))
  ids <- vapply(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE), function(f) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL); t$id %||% NA_character_
  }, character(1))
  unname(ids[!is.na(ids)])
}

delete_user_template <- function(id, dir = "templates/statements_user") {
  if (!dir.exists(dir) || is.null(id) || !nzchar(id)) return(invisible(FALSE))
  safe_id <- gsub("[^A-Za-z0-9_]+", "_", id)
  hit <- FALSE
  for (f in list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (identical(t$id %||% "", id) || identical(tools::file_path_sans_ext(basename(f)), safe_id)) {
      if (file.remove(f)) hit <- TRUE
    }
  }
  invisible(hit)
}

set_user_template_hidden <- function(id, hidden = TRUE, dir = "templates/statements_user") {
  if (!(id %in% user_template_ids(dir))) stop("only user-created templates can be hidden")
  for (f in list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (identical(t$id %||% "", id)) {
      t$origin <- NULL
      if (isTRUE(hidden)) t$hidden <- TRUE else t$hidden <- NULL
      ok <- save_yaml_safely(t, f)
      if (!isTRUE(ok)) stop(attr(ok, "reason") %||% "the file could not be written")
      return(invisible(isTRUE(hidden)))
    }
  }
  stop("template not found: ", id)
}

# There is no rename_user_template(): a template id is a stable handle the run log, the feed manifest
# and every audit record point back to, so renaming one would leave those records naming a template
# that no longer exists.
