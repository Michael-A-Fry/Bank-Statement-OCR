# setup.ps1 -- one-command setup for Bank Statement OCR on Windows.
# Run from a normal PowerShell window, or via Bank-Statement-OCR-Menu.bat:
#   powershell -ExecutionPolicy Bypass -File scripts\setup.ps1
#   powershell -ExecutionPolicy Bypass -File scripts\setup.ps1 -Start
#
# This version does NOT require Administrator rights.
# It prefers an existing R installation and does not try to upgrade/uninstall R.
# Package installation is blocking; tests are advisory by default.
# Use -StrictTests if you want setup to fail when any test fails.

param(
  [switch]$Start,
  [switch]$SkipTests,
  [switch]$StrictTests,
  [switch]$NoPause
)

$ErrorActionPreference = "Stop"

function Resolve-AppDir {
  if ($PSScriptRoot) {
    return (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
  }
  return (Get-Location).Path
}

function Resolve-Rscript {
  $cmd = Get-Command Rscript.exe -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }

  $candidates = @()
  $roots = @(
    (Join-Path $env:ProgramFiles "R"),
    (Join-Path ${env:ProgramFiles(x86)} "R"),
    (Join-Path $env:LOCALAPPDATA "Programs\R"),
    (Join-Path $env:USERPROFILE "AppData\Local\Programs\R")
  ) | Where-Object { $_ -and (Test-Path $_) }

  foreach ($root in $roots) {
    $candidates += Get-ChildItem $root -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue
  }

  $preferred = $candidates |
    Where-Object { $_.FullName -notmatch "rtools" } |
    Sort-Object @{ Expression = { if ($_.FullName -match "\\x64\\") { 0 } else { 1 } } }, FullName |
    Select-Object -First 1

  if ($preferred) { return $preferred.FullName }

  if (Get-Command winget -ErrorAction SilentlyContinue) {
    Write-Host "==> Rscript.exe was not found. Attempting a user-scope R install via winget..." -ForegroundColor Yellow
    winget install --id RProject.R -e --scope user --accept-source-agreements --accept-package-agreements

    $cmd = Get-Command Rscript.exe -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }

    foreach ($root in $roots) {
      if (Test-Path $root) {
        $candidates += Get-ChildItem $root -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue
      }
    }

    $preferred = $candidates |
      Where-Object { $_.FullName -notmatch "rtools" } |
      Sort-Object @{ Expression = { if ($_.FullName -match "\\x64\\") { 0 } else { 1 } } }, FullName |
      Select-Object -First 1

    if ($preferred) { return $preferred.FullName }
  }

  throw "Rscript.exe was not found. Install R for Windows from https://cran.r-project.org/bin/windows/base/ then re-run this script."
}

try {
  $AppDir = Resolve-AppDir
  Set-Location $AppDir

  $port = if ($env:BSO_PORT) { $env:BSO_PORT } else { "8100" }

  Write-Host "==> Bank Statement OCR - setup" -ForegroundColor Cyan
  Write-Host "==> App folder: $AppDir"

  $Rscript = Resolve-Rscript
  Write-Host "==> Rscript: $Rscript"
  & $Rscript --version

  $rbin = Split-Path $Rscript -Parent
  if ($env:Path -notlike "*$rbin*") { $env:Path = "$rbin;$env:Path" }

  Write-Host "==> Ensuring project folders..."
  foreach ($d in @("logs","out","inbox","outbox","processed","failed","templates_user")) {
    New-Item -ItemType Directory -Force -Path (Join-Path $AppDir $d) | Out-Null
  }

  Write-Host "==> Ensuring R packages in the user library..."

  $tempR = Join-Path $env:TEMP "bso_install_packages.R"
  $rCode = @'
options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c("shiny", "DT", "yaml", "jsonlite", "openxlsx", "readxl", "pdftools", "magick", "testthat")

userlib <- Sys.getenv("R_LIBS_USER")
if (!nzchar(userlib)) {
  userlib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", paste(R.version$major, sub("\\..*", "", R.version$minor), sep = "."))
}

dir.create(userlib, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(userlib, .libPaths())))

cat("R version:", R.version.string, "\n")
cat("User library:", userlib, "\n")
cat("All library paths:\n")
print(.libPaths())

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, lib = userlib, dependencies = TRUE)
} else {
  cat("All required packages already present.\n")
}

still_missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) {
  stop("Packages still missing after install: ", paste(still_missing, collapse = ", "))
}

cat("All required packages are available.\n")
'@

  $utf8NoBom = New-Object System.Text.UTF8Encoding($false)
  [System.IO.File]::WriteAllText($tempR, $rCode, $utf8NoBom)

  & $Rscript $tempR
  if ($LASTEXITCODE -ne 0) { throw "R package installation failed." }

  Write-Host "   Note: for scanned-PDF OCR, Tesseract and Poppler must also be installed and on PATH. Text PDF / CSV / Excel can work without them."

  if (-not $SkipTests) {
    if (Test-Path (Join-Path $AppDir "tests\run_tests.R")) {
      Write-Host "==> Running the test suite..."
      & $Rscript "tests\run_tests.R"
      $testExit = $LASTEXITCODE
      if ($testExit -ne 0) {
        if ($StrictTests) {
          throw "Tests reported problems. Run without -StrictTests to treat test failures as advisory."
        } else {
          Write-Host ""
          Write-Host "WARNING: Some tests reported problems, but setup will continue." -ForegroundColor Yellow
          Write-Host "The app dependencies were installed successfully. This is usually OK for normal use." -ForegroundColor Yellow
          Write-Host "Use -StrictTests if you want failed tests to stop setup." -ForegroundColor Yellow
        }
      }
    } else {
      Write-Host "==> No tests\run_tests.R found; skipping tests." -ForegroundColor Yellow
    }
  } else {
    Write-Host "==> Skipping tests because -SkipTests was supplied." -ForegroundColor Yellow
  }

  $ip = $null
  try {
    $ip = (Get-NetIPAddress -AddressFamily IPv4 -ErrorAction Stop |
      Where-Object { $_.IPAddress -notlike "169.254.*" -and $_.IPAddress -ne "127.0.0.1" } |
      Select-Object -First 1).IPAddress
  } catch {}
  if (-not $ip) { $ip = "<this-pc-or-vm>" }

  Write-Host ""
  Write-Host "======================================================================"
  Write-Host " Setup complete."
  Write-Host " Start it with:          powershell -ExecutionPolicy Bypass -File scripts\start.ps1"
  Write-Host " Local URL:              http://localhost:$port"
  Write-Host " Team URL:               http://${ip}:$port"
  Write-Host " Full guide:             docs\operational\README.md"
  Write-Host "======================================================================"

  if ($Start) {
    Write-Host "==> Starting the app..."
    & $Rscript "scripts\run_app.R"
  }
}
catch {
  Write-Host ""
  Write-Host "SETUP FAILED" -ForegroundColor Red
  Write-Host $_.Exception.Message -ForegroundColor Red
  Write-Host ""
  Write-Host "Tip: run this from an already-open PowerShell window so the error remains visible." -ForegroundColor Yellow
  if (-not $NoPause) { Read-Host "Press Enter to close" | Out-Null }
  exit 1
}
