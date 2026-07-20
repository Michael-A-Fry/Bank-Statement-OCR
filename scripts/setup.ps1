# setup.ps1 -- one-command setup for a Windows VM (run in PowerShell).
#   powershell -ExecutionPolicy Bypass -File scripts\setup.ps1
#   powershell -ExecutionPolicy Bypass -File scripts\setup.ps1 -Start
# Installs R (via winget if available), the R packages, creates folders, runs
# the tests, and prints the URL to share. Re-runnable.
param([switch]$Start)
$ErrorActionPreference = "Stop"
Set-Location (Join-Path $PSScriptRoot "..")
$port = if ($env:BSO_PORT) { $env:BSO_PORT } else { "8100" }

Write-Host "==> Bank Statement OCR - setup" -ForegroundColor Cyan

# 1. R
$rscript = (Get-Command Rscript.exe -ErrorAction SilentlyContinue)
if (-not $rscript) {
  if (Get-Command winget -ErrorAction SilentlyContinue) {
    Write-Host "==> Installing R via winget..."
    winget install --id RProject.R -e --accept-source-agreements --accept-package-agreements
    $rbin = Get-ChildItem "C:\Program Files\R" -Directory | Sort-Object Name -Descending | Select-Object -First 1
    $env:Path += ";$($rbin.FullName)\bin"
  } else {
    Write-Host "!! R not found and winget unavailable. Install R from https://cran.r-project.org/bin/windows/base/ then re-run." -ForegroundColor Yellow
    exit 1
  }
}

# 2. Packages
Write-Host "==> Ensuring R packages..."
Rscript -e "pkgs<-c('shiny','DT','yaml','jsonlite','openxlsx','readxl','pdftools'); miss<-pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]; if(length(miss)){install.packages(miss,repos='https://cloud.r-project.org')} else cat('all packages present\n')"
Write-Host "   (For scanned-PDF OCR, also install Tesseract + Poppler for Windows and add them to PATH. Text PDF / CSV / Excel work without them.)"

# 3. Folders
"logs","out","inbox","outbox","processed","failed","templates_user" | ForEach-Object { New-Item -ItemType Directory -Force -Path $_ | Out-Null }

# 4. Verify
Write-Host "==> Running the test suite..."
Rscript tests\run_tests.R
if ($LASTEXITCODE -ne 0) { Write-Host "!! Tests reported problems." -ForegroundColor Yellow; exit 1 }

$ip = (Get-NetIPAddress -AddressFamily IPv4 | Where-Object { $_.PrefixOrigin -ne 'WellKnown' } | Select-Object -First 1).IPAddress
if (-not $ip) { $ip = "<this-vm>" }
Write-Host ""
Write-Host "======================================================================"
Write-Host " Ready. Start it with:   powershell -File scripts\start.ps1"
Write-Host " Then share this URL:    http://${ip}:${port}"
Write-Host " Full guide:             docs\SETUP-AND-DEPLOYMENT.md"
Write-Host "======================================================================"

if ($Start) { Write-Host "==> Starting the app..."; Rscript scripts\run_app.R }
