# templates.R -- load + validate declarative per-bank YAML templates.

# Keys every template declares, regardless of format.
.TEMPLATE_COMMON <- c("id", "bank", "statement_type", "format", "version",
                      "min_score", "fingerprint", "currency")
.VALID_SIGN <- c("signed", "debit_credit_cols", "dr_cr_suffix", "type_dc", "unsigned")
# Header values a PDF template may pin with a drawn box (table.metadata_regions),
# for statements whose label wording the dictionary doesn't recognise.
.META_REGION_FIELDS <- c("opening_balance", "closing_balance", "period_start",
                         "period_end", "account_number", "account_name")

# validate_template(t) -> character vector of problems (length 0 if valid).
# Format-aware: delimited/excel carry columns+amount_sign at the top level and
# fingerprint on the header; pdf carries them under `table:` and fingerprints on
# page text.
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

  # decimal_mark is optional; when present it must name a known locale.
  dm <- t$decimal_mark %||% t$table$decimal_mark
  if (!is.null(dm) && !(dm %in% c("auto", "dot", "comma")))
    problems <- c(problems, sprintf("decimal_mark '%s' is not one of auto/dot/comma", dm))

  if (identical(fmt, "pdf")) {
    if (!is.null(t$fingerprint) && is.null(t$fingerprint$page_contains_all))
      problems <- c(problems, "fingerprint.page_contains_all is required for pdf templates")
    tab <- t$table
    if (is.null(tab)) {
      problems <- c(problems, "pdf templates require a 'table' block")
    } else {
      for (k in c("date", "description"))
        if (is.null(tab$columns[[k]]))
          problems <- c(problems, sprintf("table.columns.%s is required", k))
      # amount source depends on the sign style, exactly like the delimited path:
      # debit_credit_cols needs debit+credit bands; everything else needs amount.
      if (identical(tab$amount_sign, "debit_credit_cols")) {
        for (k in c("debit", "credit"))
          if (is.null(tab$columns[[k]]))
            problems <- c(problems, sprintf("table.amount_sign 'debit_credit_cols' requires table.columns.%s", k))
      } else if (is.null(tab$columns[["amount"]])) {
        problems <- c(problems, "table.columns.amount is required")
      }
      if (!is.null(tab$amount_sign) && !(tab$amount_sign %in% .VALID_SIGN))
        problems <- c(problems, sprintf("table.amount_sign '%s' is invalid", tab$amount_sign))
      # metadata_regions (optional): each entry pins a header value to a drawn box.
      # The field must be one we actually wire into the header, and the box needs an
      # x-band. Absent metadata_regions leaves validation exactly as it was.
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
    # date + description are always required; the money column depends on the
    # sign style, exactly like the pdf branch above. A debit_credit_cols template
    # (separate money-in / money-out columns) has NO single 'amount' column -- it
    # supplies debit + credit instead (checked below), so requiring 'amount' here
    # would reject the tool's own draft of a very common CSV shape.
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
      # The debit token is mandatory: without it the reader falls back to a blind
      # "D", which silently flips the sign on any bank that writes it differently.
      if (is.null(t$type_debit_value) || !nzchar(trimws(as.character(t$type_debit_value))))
        problems <- c(problems, "amount_sign 'type_dc' requires type_debit_value (which indicator value means a debit)")
    }
  }

  # extras: {source} for delimited/excel, {x_min,x_max} bands for pdf.
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

# load_templates(dir, origin, strict) -> named list<template> keyed by id.
# origin ("default" | "user") is stamped on each template so the app and reports
# can tell a curated team template from one an accountant created. strict=TRUE
# (curated set): an invalid template is a HARD error. strict=FALSE (user set):
# an invalid/duplicate template is skipped with a warning, so one bad user
# template can never break everyone's conversions.
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
  templates
}

# load_template_set(default_dir, user_dir, include_hidden) -> merged templates.
# Curated defaults load first and WIN on any id clash (a user template can never
# shadow a team-blessed one). User templates fill in the rest and are marked
# origin="user". A user template flagged `hidden: true` is EXCLUDED by default, so
# it stops taking part in detection/conversion without being deleted (a cluttered
# pile of near-duplicate drafts can be parked, not lost). include_hidden = TRUE
# returns them too, for the Admin management view that can un-hide them.
load_template_set <- function(default_dir = "templates", user_dir = "templates_user",
                              include_hidden = FALSE) {
  d <- load_templates(default_dir, origin = "default", strict = TRUE)
  if (!is.null(user_dir) && dir.exists(user_dir)) {
    u <- load_templates(user_dir, origin = "user", strict = FALSE)
    if (!include_hidden)
      u <- u[!vapply(u, function(t) isTRUE(t$hidden), logical(1))]
    for (id in names(u)) if (is.null(d[[id]])) d[[id]] <- u[[id]]
  }
  d
}

# template_overview(tset) -- a flat data.frame summarising every loaded template,
# for the Admin overview and the Convert "what's covered" panel. `origin` reads as
# "tested" (a shipped, golden-file-tested default) or "user" (built on this box).
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
      date_format = (if (is_pdf) t$table$date_format else t$columns$date$format) %||% NA_character_,
      origin      = if (identical(t$origin %||% "default", "user")) "user" else "tested",
      hidden      = if (isTRUE(t$hidden)) "hidden" else "",
      version     = as.character(t$version %||% NA),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out[order(out$bank, out$type, out$id), , drop = FALSE]
}

# template_yaml(t) -- the template rendered as YAML text for preview/edit, with the
# load-time `origin` marker stripped so it round-trips cleanly.
template_yaml <- function(t) { t$origin <- NULL; yaml::as.yaml(t) }

# .template_shape(t) -- a structural signature: format + amount_sign + date_format
# + the (sorted) column mapping. Two templates with the same shape read a
# statement identically, differing only in id / bank label / fingerprint -- i.e.
# they are the same layout drafted more than once.
.template_shape <- function(t) {
  is_pdf <- identical(t$format %||% "delimited", "pdf")
  cols   <- if (is_pdf) t$table$columns else t$columns
  sign   <- if (is_pdf) t$table$amount_sign else t$amount_sign
  dfmt   <- if (is_pdf) t$table$date_format else t$columns$date$format
  colsig <- if (is.null(cols)) "" else paste(sort(vapply(names(cols), function(k) {
    c <- cols[[k]]
    if (is_pdf) sprintf("%s:%s-%s", k, c$x_min %||% "", c$x_max %||% "")
    else sprintf("%s:%s", k, (if (is.list(c)) c$source else c) %||% "")
  }, character(1))), collapse = "|")
  paste(t$format %||% "delimited", sign %||% "", dfmt %||% "", colsig, sep = "~~")
}

# duplicate_template_groups(tset, user_only) -> list of id-vectors, one per group
# of templates that share a layout (see .template_shape). Only groups with >1
# member are returned -- these are the "heap of near-duplicates" to consolidate:
# keep one, hide or delete the rest. user_only ignores shipped templates (you
# can't delete those anyway). Deterministic; safe to show (ids + shapes only).
duplicate_template_groups <- function(tset, user_only = TRUE) {
  if (user_only) tset <- Filter(function(t) identical(t$origin %||% "default", "user"), tset)
  if (length(tset) < 2) return(list())
  sig <- vapply(tset, .template_shape, character(1))
  groups <- split(names(tset), sig)
  unname(groups[vapply(groups, length, integer(1)) > 1L])
}

# save_user_template(template, dir) -> path. Validates first (fail loud), writes
# <dir>/<id>.yaml. This is how the guided flow persists an accountant's template.
save_user_template <- function(template, dir = "templates_user") {
  template$origin <- NULL   # origin is assigned at load time, not stored
  probs <- validate_template(template)
  if (length(probs)) stop("template is not valid: ", paste(probs, collapse = "; "))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  id <- gsub("[^A-Za-z0-9_]+", "_", template$id %||% "user_template")
  path <- file.path(dir, paste0(id, ".yaml"))
  yaml::write_yaml(template, path)
  invisible(path)
}

# user_template_ids(dir) -> ids of templates that live in the user dir (the only
# ones the app may delete / rename -- shipped "tested" templates are read-only).
user_template_ids <- function(dir = "templates_user") {
  if (!dir.exists(dir)) return(character(0))
  ids <- vapply(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE), function(f) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL); t$id %||% NA_character_
  }, character(1))
  unname(ids[!is.na(ids)])
}

# delete_user_template(id, dir) -> TRUE if a user template file was removed. Only
# ever touches the user dir; shipped templates cannot be deleted from the app.
delete_user_template <- function(id, dir = "templates_user") {
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

# set_user_template_hidden(id, hidden, dir) -> the new hidden state. Writes (or
# clears) `hidden: true` in the user template's own file, keeping its filename.
# Hidden templates drop out of detection/conversion (load_template_set default)
# but stay on disk and in the Admin management view, so they can be un-hidden or
# merged later. Only user templates -- shipped ones are read-only.
set_user_template_hidden <- function(id, hidden = TRUE, dir = "templates_user") {
  if (!(id %in% user_template_ids(dir))) stop("only user-created templates can be hidden")
  for (f in list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)) {
    t <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (identical(t$id %||% "", id)) {
      t$origin <- NULL
      if (isTRUE(hidden)) t$hidden <- TRUE else t$hidden <- NULL
      yaml::write_yaml(t, f)
      return(invisible(isTRUE(hidden)))
    }
  }
  stop("template not found: ", id)
}

# rename_user_template(old_id, new_id, dir) -> new id. Saves under the new id and
# removes the old file. Only for user templates.
rename_user_template <- function(old_id, new_id, dir = "templates_user") {
  ids <- user_template_ids(dir)
  if (!(old_id %in% ids)) stop("only user-created templates can be renamed")
  t <- NULL
  for (f in list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)) {
    cand <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (identical(cand$id %||% "", old_id)) { t <- cand; break }
  }
  if (is.null(t)) stop("template not found: ", old_id)
  t$id <- gsub("[^A-Za-z0-9_]+", "_", new_id)
  save_user_template(t, dir)
  if (!identical(t$id, old_id)) delete_user_template(old_id, dir)
  invisible(t$id)
}
