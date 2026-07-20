#!/usr/bin/env Rscript
# serve_inbox.R -- the FOLDER option: no web access needed, only shared folders.
# Users (or Qlik) drop statements into inbox/; this converts each one into
# outbox/<name>/ (xlsx + csv + json) and moves the original to processed/. Run it
# on a schedule (Task Scheduler / cron, e.g. every 2 minutes) OR pass "loop" to
# keep polling.
#
#   Rscript scripts/serve_inbox.R          # process the inbox once, then exit
#   Rscript scripts/serve_inbox.R loop     # keep polling every 30s
#
# Concurrency is safe: each conversion writes its own out folder + its own log
# file (see docs/architecture/deployment-integration-plan.md).

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)
for (d in c("inbox", "outbox", "processed", "failed")) if (!dir.exists(d)) dir.create(d)

process_once <- function() {
  files <- list.files("inbox", full.names = TRUE,
                      pattern = "\\.(csv|tsv|tdv|txt|xlsx|xlsm|pdf)$", ignore.case = TRUE)
  for (f in files) {
    base <- tools::file_path_sans_ext(basename(f))
    res <- tryCatch(convert_statement(f, outdir = file.path("outbox", base),
      templates_dir = "templates", user_templates_dir = "templates_user",
      logdir = "logs", requested_by = Sys.getenv("USERNAME", "inbox")),
      error = function(e) NULL)
    ok <- !is.null(res) && res$status %in% c("ok", "needs_review")
    dest <- file.path(if (ok) "processed" else "failed", basename(f))
    suppressWarnings(file.rename(f, dest))
    cat(sprintf("%s  %-14s  %s\n", format(Sys.time(), "%H:%M:%S"),
                res$status %||% "failed", basename(f)))
  }
  length(files)
}

if (length(commandArgs(TRUE)) && identical(commandArgs(TRUE)[1], "loop")) {
  cat("serve_inbox: polling inbox/ every 30s (Ctrl-C to stop)\n")
  repeat { process_once(); Sys.sleep(30) }
} else {
  n <- process_once(); cat(sprintf("done: %d file(s) processed\n", n))
}
