#!/usr/bin/env Rscript
# run_app.R -- start the web app for the whole team on the VM.
# Everyone then opens http://<vm-name-or-ip>:8100 in a browser. Nobody but this
# VM needs R installed.
#
#   Rscript scripts/run_app.R            # from the repo root
#   BSO_PORT=8100 Rscript scripts/run_app.R
#
# Keep it running as a service (see docs/SETUP-AND-DEPLOYMENT.md):
#   Windows: NSSM or Task Scheduler "at startup"
#   Linux:   a small systemd unit
# Self-locate: a boot service (systemd / Task Scheduler) may launch this with a
# different working directory. Resolve the app folder from this script's own path
# and run from there, so the app never serves an empty/wrong directory.
.script_dir <- function() {
  args <- commandArgs(FALSE)
  m <- grep("^--file=", args, value = TRUE)
  if (length(m)) return(dirname(dirname(normalizePath(sub("^--file=", "", m[1])))))
  getwd()
}
app_dir <- .script_dir()
setwd(app_dir)

port <- suppressWarnings(as.integer(Sys.getenv("BSO_PORT", "8100")))
if (is.na(port)) port <- 8100L
cat(sprintf("Statement Studio — starting on port %d (from %s). Open http://<this-vm>:%d\n",
            port, app_dir, port))
shiny::runApp(app_dir, host = "0.0.0.0", port = port, launch.browser = FALSE)
