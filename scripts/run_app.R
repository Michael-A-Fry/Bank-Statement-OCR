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
port <- suppressWarnings(as.integer(Sys.getenv("BSO_PORT", "8100")))
if (is.na(port)) port <- 8100L
cat(sprintf("Bank Statement OCR — starting on port %d. Open http://<this-vm>:%d\n", port, port))
shiny::runApp(".", host = "0.0.0.0", port = port, launch.browser = FALSE)
