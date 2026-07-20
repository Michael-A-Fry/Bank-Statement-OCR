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

# Self-locate: a boot service / scheduled task may launch this with a different
# working directory. Resolve the app folder from this script's own path and run
# from there, so it never sources an empty R/ and silently processes nothing.
.script_dir <- function() {
  args <- commandArgs(FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) return(dirname(dirname(normalizePath(sub("^--file=", "", m[1])))))
  getwd()
}
setwd(.script_dir())

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)
for (d in c("inbox", "outbox", "processed", "failed", "stuck", "logs"))
  if (!dir.exists(d)) dir.create(d)

# .stable(f) -- TRUE once a file's size has settled, i.e. it is not still being
# copied into inbox/ (a network copy in progress would otherwise be converted
# mid-write and spuriously fail). Compares size across a short pause.
.stable <- function(f) {
  s1 <- file.info(f)$size
  Sys.sleep(1)
  s2 <- file.info(f)$size
  !is.na(s1) && !is.na(s2) && s1 == s2
}

# .move(f, dest_dir) -- move the original out of inbox/. file.rename fails across
# volumes (a common case when inbox/ is a network share and processed/ is local);
# fall back to copy+unlink, and if even that fails, park it in stuck/ so it is
# NOT reprocessed on the next poll (which would duplicate outputs forever).
.move <- function(f, dest_dir) {
  dest <- file.path(dest_dir, basename(f))
  if (suppressWarnings(file.rename(f, dest))) return(TRUE)
  if (suppressWarnings(file.copy(f, dest, overwrite = TRUE)) && suppressWarnings(file.remove(f)))
    return(TRUE)
  # last resort: try to get it out of inbox/ so the loop stops re-touching it
  stuck <- file.path("stuck", basename(f))
  moved <- suppressWarnings(file.rename(f, stuck)) ||
           (suppressWarnings(file.copy(f, stuck, overwrite = TRUE)) &&
            suppressWarnings(file.remove(f)))
  cat(sprintf("%s  !! could not move %s out of inbox (%s)\n",
              format(Sys.time(), "%H:%M:%S"), basename(f),
              if (moved) "parked in stuck/" else "STILL IN INBOX -- check permissions"))
  FALSE
}

process_once <- function() {
  files <- list.files("inbox", full.names = TRUE,
                      pattern = "\\.(csv|tsv|tdv|txt|xlsx|xlsm|pdf)$", ignore.case = TRUE)
  done <- 0L
  for (f in files) {
    if (!.stable(f)) {
      cat(sprintf("%s  %-14s  %s\n", format(Sys.time(), "%H:%M:%S"), "still-copying", basename(f)))
      next   # come back for it next poll, once the copy has finished
    }
    base <- tools::file_path_sans_ext(basename(f))
    res <- tryCatch(convert_statement(f, outdir = file.path("outbox", base),
      templates_dir = "templates", user_templates_dir = "templates_user",
      logdir = "logs", requested_by = Sys.getenv("USERNAME", "inbox")),
      error = function(e) NULL)
    ok <- !is.null(res) && res$status %in% c("ok", "needs_review")
    .move(f, if (ok) "processed" else "failed")
    done <- done + 1L
    cat(sprintf("%s  %-14s  %s\n", format(Sys.time(), "%H:%M:%S"),
                res$status %||% "failed", basename(f)))
  }
  done
}

if (length(commandArgs(TRUE)) && identical(commandArgs(TRUE)[1], "loop")) {
  cat("serve_inbox: polling inbox/ every 30s (Ctrl-C to stop)\n")
  # A stray error must never kill the long-lived poller -- log it and keep going.
  repeat {
    tryCatch(process_once(),
             error = function(e) cat(sprintf("%s  !! poll error: %s\n",
               format(Sys.time(), "%H:%M:%S"), conditionMessage(e))))
    Sys.sleep(30)
  }
} else {
  n <- process_once(); cat(sprintf("done: %d file(s) processed\n", n))
}
