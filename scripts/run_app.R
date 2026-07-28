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
  source(file.path(app_dir, "R", "jobs.R"))     # for the concurrency line printed below
}, error = function(e) NULL))
.cfg <- tryCatch(load_config(), error = function(e) NULL)
# A config.yaml that does not parse reverts to built-in defaults -- including the
# placeholder admin password and the default Qlik feed folder. This script is how
# the server is actually started, so the operator must see it here, on the console,
# not discover it weeks later from a dashboard pointing at the wrong folder.
.cfg_err <- tryCatch(config_error(.cfg), error = function(e) NULL)
if (!is.null(.cfg_err)) {
  cat("\n*** SETTINGS FILE NOT LOADED ***\n", .cfg_err, "\n",
      "Statement Studio is starting on BUILT-IN DEFAULTS: the placeholder admin\n",
      "password and the default analytics-feed folder. Fix the file and restart.\n\n",
      sep = "", file = stderr())
}
if (isTRUE(tryCatch(admin_password_is_default(.cfg), error = function(e) FALSE)))
  cat("Statement Studio: Admin is CLOSED - no admin password is set. Set app.admin_password\n",
      "in config/config.yaml or the BSO_ADMIN_PASSWORD environment variable, then restart.\n",
      sep = "", file = stderr())
cfg_port <- tryCatch(as.integer(.cfg$app$port), error = function(e) NA_integer_)
if (length(cfg_port) != 1 || is.na(cfg_port)) cfg_port <- 8100L
port <- suppressWarnings(as.integer(Sys.getenv("BSO_PORT", as.character(cfg_port))))
if (is.na(port)) port <- cfg_port
cat(sprintf("Statement Studio — starting on port %d (from %s). Open http://<this-vm>:%d\n",
            port, app_dir, port))
# How many statements this server will convert at once. Say it HERE, where the
# operator is looking: each conversion is its own R process now, so this number
# is the load the box will actually take, and anyone past it is told they are
# queuing. Worth seeing on the console the day someone raises it in config.yaml.
suppressWarnings(tryCatch({
  .cap <- job_set_max_concurrent(.cfg$app$max_concurrent_jobs)
  cat(sprintf("Converting up to %d statement(s) at once (app.max_concurrent_jobs). Anyone past that is queued and told so.\n",
              as.integer(.cap)))
}, error = function(e) NULL))
# Match app.R's upload ceiling here too: this script is how the server actually
# starts, and Shiny's 5 MB default would reject the scanned statements the OCR path
# exists for before they ever reach the engine.
.max_mb <- suppressWarnings(as.numeric(tryCatch(.cfg$app$max_upload_mb, error = function(e) NA)))
if (!is.finite(.max_mb) || .max_mb <= 0) .max_mb <- 200
options(shiny.maxRequestSize = .max_mb * 1024^2)
shiny::runApp(app_dir, host = "0.0.0.0", port = port, launch.browser = FALSE)
