#!/usr/bin/env Rscript
# run.R -- thin CLI. Usage: Rscript run.R <file> [bank] [outdir]

# Force a UTF-8 locale FIRST -- the same guard, in the same words, as app.R and
# tests/run_tests.R. ALL THREE need it, because all three source the same engine
# and the engine reads bank statements, which carry pound signs, euro signs,
# macrons and accented payees. On a host whose default locale is C/ASCII (this
# container, and any bare Windows service account) R cannot translate those bytes,
# and the failure is not a crash: a value comes back NA, or short of a byte, with
# nothing but a console warning to say so.
#
# WHY THE CLI ESPECIALLY. docs/operational/investigating-a-wrong-conversion.md
# tells a maintainer to reproduce a disputed conversion with `Rscript run.R`. If
# the CLI ran in a different locale from the app, the re-run could disagree with
# the conversion it was meant to reproduce -- so the incident procedure would be
# comparing two different engines and calling the difference a finding. The suite
# needs it for the mirror-image reason: a test that passes in a locale nobody
# deploys in has proved nothing.
#
# The guard is belt to the engine's braces, not a substitute for them: R/labels.R
# is written to be locale-independent on its own (byte matching, no multibyte
# character classes). Neither alone is trusted.
suppressWarnings(for (.loc in c("C.UTF-8", "C.utf8", "en_US.UTF-8", "en_US.utf8"))
  if (nzchar(Sys.setlocale("LC_CTYPE", .loc))) break)

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
    templates_dir = file.path(root, "templates", "statements"),
    requested_by = "cli",
    logdir = file.path(root, "logs")
  )

  # FIRST, because it is the handle for everything else. The incident procedure
  # (docs/operational/investigating-a-wrong-conversion.md) says to open
  # logs/runs/<run_id>.json for the re-run, and this CLI printed no way to know
  # which of the hundreds of records it had just written. Always present: the id
  # is built before the funnel opens and stamped on the result after it closes,
  # so a failed run prints one too -- and it has a log record to match.
  cat(sprintf("run id:      %s\n", res$run_id %||% NA))
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
