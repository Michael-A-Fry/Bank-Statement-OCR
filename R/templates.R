# templates.R -- load + validate declarative per-bank YAML templates.

# Required top-level keys every template must declare.
.TEMPLATE_REQUIRED <- c(
  "id", "bank", "statement_type", "format", "version",
  "min_score", "fingerprint", "columns", "amount_sign", "currency"
)

# validate_template(t) -> character vector of problems ("" length if valid).
validate_template <- function(t) {
  problems <- character(0)
  if (!is.list(t)) return("template is not a mapping")

  for (k in .TEMPLATE_REQUIRED) {
    if (is.null(t[[k]])) problems <- c(problems, sprintf("missing key '%s'", k))
  }
  if (!is.null(t$fingerprint) &&
      is.null(t$fingerprint$header_contains_all)) {
    problems <- c(problems, "fingerprint.header_contains_all is required")
  }
  if (!is.null(t$columns)) {
    for (k in c("date", "amount", "description")) {
      if (is.null(t$columns[[k]])) {
        problems <- c(problems, sprintf("columns.%s is required", k))
      }
    }
  }
  valid_sign <- c("signed", "debit_credit_cols", "dr_cr_suffix", "type_dc")
  if (!is.null(t$amount_sign) && !(t$amount_sign %in% valid_sign)) {
    problems <- c(problems, sprintf("amount_sign '%s' is not one of %s",
                                    t$amount_sign, paste(valid_sign, collapse = "/")))
  }
  valid_fmt <- c("delimited", "excel", "pdf")
  if (!is.null(t$format) && !(t$format %in% valid_fmt)) {
    problems <- c(problems, sprintf("format '%s' is not one of %s",
                                    t$format, paste(valid_fmt, collapse = "/")))
  }
  problems
}

# load_templates(dir) -> named list<template> keyed by id.
# An invalid template is a HARD error at load time, listed by file/id.
load_templates <- function(dir) {
  if (!dir.exists(dir)) stop(sprintf("templates dir not found: %s", dir))
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
    templates[[t$id]] <- t
  }
  if (length(errors)) {
    stop("Invalid template(s):\n", paste(errors, collapse = "\n"))
  }
  templates
}
