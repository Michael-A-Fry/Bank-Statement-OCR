#!/usr/bin/env Rscript
# run.R -- thin CLI. Usage: Rscript run.R <file> [bank] [outdir]

.script_dir <- function() {
  args <- commandArgs(FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) return(dirname(normalizePath(sub("^--file=", "", m[1]))))
  getwd()
}

local({
  root <- .script_dir()
  for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
    source(f)
  }
  for (p in c("yaml", "jsonlite", "openxlsx")) {
    suppressWarnings(suppressMessages(requireNamespace(p, quietly = TRUE)))
  }

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    cat("Usage: Rscript run.R <file> [bank] [outdir]\n")
    quit(status = 2)
  }
  file    <- args[[1]]
  bank    <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else NULL
  outdir  <- if (length(args) >= 3 && nzchar(args[[3]])) args[[3]] else "out"

  res <- convert_statement(
    path = file, bank = bank, outdir = outdir,
    templates_dir = file.path(root, "templates"),
    requested_by = "cli",
    logdir = file.path(root, "logs")
  )

  cat(sprintf("status:      %s\n", res$status))
  cat(sprintf("template:    %s\n", res$template_id %||% NA))
  cat(sprintf("trust:       %s (score %s)\n",
              res$trust$level %||% NA, res$trust$score %||% NA))
  if (!is.null(res$kpis)) {
    cat("checks:\n")
    for (i in seq_len(nrow(res$kpis))) {
      cat(sprintf("  - %-28s %s  %s\n",
                  res$kpis$name[i], res$kpis$status[i], res$kpis$detail[i]))
    }
  }
  if (length(res$outputs)) {
    cat("outputs:\n")
    for (nm in names(res$outputs)) cat(sprintf("  - %-5s %s\n", nm, res$outputs[[nm]]))
  }
  if (length(res$messages)) cat(sprintf("message:     %s\n", paste(res$messages, collapse = " | ")))

  quit(status = if (res$status %in% c("ok", "needs_review")) 0 else 1)
})
