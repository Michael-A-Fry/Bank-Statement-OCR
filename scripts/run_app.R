#!/usr/bin/env Rscript
# run_app.R -- start the web app for the whole team on the VM.
# Everyone then opens http://<vm-name-or-ip>:8100 in a browser. Nobody but this
# VM needs R installed.
#
#   Rscript scripts/run_app.R            # from the repo root
#   BSO_PORT=8100 Rscript scripts/run_app.R
#
# Keep it running as a service (see docs/operational/running-and-keeping-it-up.md):
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

# Port: from the config file (app.port), overridable by the BSO_PORT env var, so the
# one config file drives it too. Load the config here (before runApp) for the port.
suppressWarnings(tryCatch({
  source(file.path(app_dir, "R", "util.R"))
  source(file.path(app_dir, "R", "config.R"))
}, error = function(e) NULL))
cfg_port <- tryCatch(as.integer(load_config()$app$port), error = function(e) NA_integer_)
if (length(cfg_port) != 1 || is.na(cfg_port)) cfg_port <- 8100L
port <- suppressWarnings(as.integer(Sys.getenv("BSO_PORT", as.character(cfg_port))))
if (is.na(port)) port <- cfg_port
cat(sprintf("Statement Studio — starting on port %d (from %s). Open http://<this-vm>:%d\n",
            port, app_dir, port))
shiny::runApp(app_dir, host = "0.0.0.0", port = port, launch.browser = FALSE)
