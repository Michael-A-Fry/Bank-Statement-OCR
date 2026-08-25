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

# ---------------------------------------------------------------------------
# A LINE PER START, IN A FILE THAT SURVIVES THE WINDOW CLOSING
#
# On the server this is started by Task Scheduler at boot, as a service account,
# with no console attached to anything. Everything this script prints then goes
# nowhere -- so "the task says it ran and nothing is listening" has no evidence
# behind it at all, and the operator is reduced to guessing between a missing
# firewall rule, a taken port, a settings file that did not parse and an R
# library that lost a package.
#
# One appended line per start, in logs\startup.log, is the difference between
# guessing and reading. It is the FIRST thing this script sets up, before
# anything that can fail, so a failure has somewhere to be written down.
# ---------------------------------------------------------------------------
.log_path <- file.path(app_dir, "logs", "startup.log")
try(dir.create(dirname(.log_path), showWarnings = FALSE, recursive = TRUE), silent = TRUE)
.boot <- function(...) {
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", paste0(...))
  cat(line, "\n", sep = "")
  try(cat(line, "\n", sep = "", file = .log_path, append = TRUE), silent = TRUE)
  invisible(NULL)
}
# Keep it from growing without bound: this file is appended to on every boot and
# every restart-on-failure, and a service that flaps writes a line a minute.
try(if (file.exists(.log_path) && file.info(.log_path)$size > 512000) {
  keep <- utils::tail(readLines(.log_path, warn = FALSE), 500)
  writeLines(c("... earlier lines trimmed ...", keep), .log_path)
}, silent = TRUE)
.ver <- tryCatch(trimws(readLines(file.path(app_dir, "VERSION"), warn = FALSE)[1]),
                 error = function(e) "?")
.boot("---- starting Statement Studio ", .ver, " ----")
.boot("folder  ", app_dir)
.boot("account ", tryCatch(Sys.info()[["user"]], error = function(e) "?"),
      "   R ", paste0(R.version$major, ".", R.version$minor))

# Port: from the config file (app.port), overridable by the BSO_PORT env var, so the
# one config file drives it too. Load the config here (before runApp) for the port.
suppressWarnings(tryCatch({
  source(file.path(app_dir, "R", "util.R"))
  source(file.path(app_dir, "R", "config.R"))
  source(file.path(app_dir, "R", "jobs.R"))     # for the concurrency line printed below
}, error = function(e) NULL))
# Templates all live under templates\ now. An install updated from an older
# version still has the old folders at the root of the app, holding every layout
# and every report puller the team built -- so move them once, before anything
# reads a template, and RECORD IT: a folder full of somebody's work changing place
# is exactly the kind of thing that must not happen quietly on a box nobody logs
# into. Moves only, never deletes, never overwrites; silent when there is nothing
# to do. Rules and reasoning in R/config.R.
for (.m in tryCatch(migrate_template_layout(app_dir), error = function(e) character(0)))
  .boot("templates ", .m)

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
  .boot("SETTINGS FILE NOT LOADED - running on built-in defaults: ", .cfg_err)
} else {
  .boot("settings config", .Platform$file.sep, "config.yaml read")
}
if (isTRUE(tryCatch(admin_password_is_default(.cfg), error = function(e) FALSE))) {
  cat("Statement Studio: Admin is CLOSED - no admin password is set. Set app.admin_password\n",
      "in config/config.yaml or the BSO_ADMIN_PASSWORD environment variable, then restart.\n",
      sep = "", file = stderr())
  .boot("Admin is CLOSED - no admin password set")
}
cfg_port <- tryCatch(as.integer(.cfg$app$port), error = function(e) NA_integer_)
if (length(cfg_port) != 1 || is.na(cfg_port)) cfg_port <- 8100L
port <- suppressWarnings(as.integer(Sys.getenv("BSO_PORT", as.character(cfg_port))))
if (is.na(port)) port <- cfg_port
# THE ADDRESS, WITH THIS MACHINE'S OWN NAME IN IT.
#
# It used to print `http://<this-vm>:8100` -- a placeholder the reader had to
# know to substitute, printed to somebody who has just installed the thing and
# does not yet know what the network calls this box. The machine knows its own
# name. Print the address people can actually copy, and keep the placeholder
# beside it for the case where the name is not resolvable off the server.
.host <- tryCatch(as.character(Sys.info()[["nodename"]])[1], error = function(e) "")
if (!length(.host) || is.na(.host) || !nzchar(.host)) .host <- "<this-server>"
.boot("port    ", port, "   address http://", .host, ":", port, "/")
cat(sprintf("Statement Studio - starting on port %d (from %s).\n", port, app_dir))
cat(sprintf("  Open  http://%s:%d/   (or http://<this-server-name-or-ip>:%d/)\n",
            .host, port, port))

# IS THE PORT ACTUALLY FREE?
#
# A port already in use makes Shiny die with a low-level socket error that names
# no port and suggests no remedy. At boot, under a scheduled task with no
# console, it dies invisibly and the task reports whatever the shell reports.
# Ask first, and say it in a sentence someone can act on. The socket is opened
# and shut again straight away; Shiny takes the port a moment later.
.port_free <- function(p) {
  s <- tryCatch(serverSocket(p), error = function(e) NULL)
  if (is.null(s)) return(FALSE)
  try(close(s), silent = TRUE)
  TRUE
}
if (!.port_free(port)) {
  msg <- sprintf(paste("PORT %d IS ALREADY IN USE on this machine, so Statement Studio",
                       "cannot start. Either another copy of it is already running",
                       "(check Task Manager for Rscript.exe), or another program has",
                       "taken the port. Find out which with:  netstat -ano | findstr :%d",
                       "Then stop that program, or set a different app.port in",
                       "config\\config.yaml and open the matching firewall rule."),
                 port, port)
  .boot("FAILED TO START - ", msg)
  cat("\n*** ", msg, " ***\n\n", sep = "", file = stderr())
  quit(status = 2L, save = "no")
}
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
.boot("uploads up to ", .max_mb, " MB")
# host = "0.0.0.0" is EVERY network card, which is what lets anybody but this
# machine reach it. It is also why the firewall rule is not optional.
.boot("listening on every network card - the firewall rule decides who gets in")
# If runApp itself falls over, that is the one failure worth a line in the file:
# by then the console has gone and the task is about to report a number.
tryCatch(
  shiny::runApp(app_dir, host = "0.0.0.0", port = port, launch.browser = FALSE),
  error = function(e) {
    .boot("STOPPED WITH AN ERROR - ", conditionMessage(e))
    quit(status = 3L, save = "no")
  })
.boot("---- stopped ----")
