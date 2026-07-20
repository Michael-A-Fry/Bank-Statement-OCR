# templates.R -- load + validate declarative per-bank YAML templates.

# Keys every template declares, regardless of format.
.TEMPLATE_COMMON <- c("id", "bank", "statement_type", "format", "version",
                      "min_score", "fingerprint", "currency")
.VALID_SIGN <- c("signed", "debit_credit_cols", "dr_cr_suffix", "type_dc")

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
    }
  } else {
    if (is.null(t$columns)) problems <- c(problems, "missing key 'columns'")
    if (is.null(t$amount_sign)) problems <- c(problems, "missing key 'amount_sign'")
    if (!is.null(t$fingerprint) && is.null(t$fingerprint$header_contains_all))
      problems <- c(problems, "fingerprint.header_contains_all is required")
    if (!is.null(t$columns)) for (k in c("date", "amount", "description"))
      if (is.null(t$columns[[k]])) problems <- c(problems, sprintf("columns.%s is required", k))
    if (!is.null(t$amount_sign) && !(t$amount_sign %in% .VALID_SIGN))
      problems <- c(problems, sprintf("amount_sign '%s' is not one of %s",
                                      t$amount_sign, paste(.VALID_SIGN, collapse = "/")))
    .has_col <- function(field) !is.null(t$columns) && !is.null(t$columns[[field]])
    if (identical(t$amount_sign, "debit_credit_cols")) for (k in c("debit", "credit"))
      if (!.has_col(k)) problems <- c(problems,
        sprintf("amount_sign 'debit_credit_cols' requires columns.%s", k))
    if (identical(t$amount_sign, "type_dc") && !.has_col("type"))
      problems <- c(problems, "amount_sign 'type_dc' requires columns.type")
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

# load_template_set(default_dir, user_dir) -> merged templates. Curated defaults
# load first and WIN on any id clash (a user template can never shadow a
# team-blessed one). User templates fill in the rest and are marked origin="user".
load_template_set <- function(default_dir = "templates", user_dir = "templates_user") {
  d <- load_templates(default_dir, origin = "default", strict = TRUE)
  if (!is.null(user_dir) && dir.exists(user_dir)) {
    u <- load_templates(user_dir, origin = "user", strict = FALSE)
    for (id in names(u)) if (is.null(d[[id]])) d[[id]] <- u[[id]]
  }
  d
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
