# start.ps1 -- launch the app for the team. Share http://<this-vm>:8100
Set-Location (Join-Path $PSScriptRoot "..")
Rscript scripts\run_app.R
