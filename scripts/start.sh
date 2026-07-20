#!/usr/bin/env bash
# start.sh -- launch the app for the team. Share http://<this-vm>:${BSO_PORT:-8100}
cd "$(dirname "$0")/.."
exec Rscript scripts/run_app.R
