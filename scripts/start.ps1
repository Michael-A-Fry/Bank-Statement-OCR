# start.ps1 -- launch the Bank Statement OCR app for the team.
# Run from a normal PowerShell window:
#   powershell -ExecutionPolicy Bypass -File scripts\start.ps1

param(
  [switch]$NoPause
)

$ErrorActionPreference = "Stop"

function Resolve-AppDir {
  if ($PSScriptRoot) { return (Resolve-Path (Join-Path $PSScriptRoot "..")).Path }
  return (Get-Location).Path
}

function Resolve-Rscript {
  $cmd = Get-Command Rscript.exe -ErrorAction SilentlyContinue
  if ($cmd) { return $cmd.Source }

  $roots = @(
    (Join-Path $env:ProgramFiles "R"),
    (Join-Path ${env:ProgramFiles(x86)} "R"),
    (Join-Path $env:LOCALAPPDATA "Programs\R"),
    (Join-Path $env:USERPROFILE "AppData\Local\Programs\R")
  ) | Where-Object { $_ -and (Test-Path $_) }

  $candidate = foreach ($root in $roots) {
    Get-ChildItem $root -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue
  } | Where-Object { $_.FullName -notmatch "rtools" } | Sort-Object FullName -Descending | Select-Object -First 1

  if ($candidate) { return $candidate.FullName }
  throw "Rscript.exe not found. Run scripts\setup.ps1 first, or install R for Windows."
}

try {
  $AppDir = Resolve-AppDir
  Set-Location $AppDir

  if (-not (Test-Path (Join-Path $AppDir "app.R")) -and -not (Test-Path (Join-Path $AppDir "server.R"))) {
    throw "The app folder '$AppDir' does not contain app.R or server.R."
  }

  $Rscript = Resolve-Rscript
  $port = if ($env:BSO_PORT) { $env:BSO_PORT } else { "8100" }

  Write-Host "==> Starting Bank Statement OCR" -ForegroundColor Cyan
  Write-Host "==> App folder: $AppDir"
  Write-Host "==> Rscript: $Rscript"
  Write-Host "==> URL: http://localhost:$port"
  Write-Host ""

  & $Rscript "scripts\run_app.R"
}
catch {
  Write-Host ""
  Write-Host "START FAILED" -ForegroundColor Red
  Write-Host $_.Exception.Message -ForegroundColor Red
  Write-Host ""
  if (-not $NoPause) { Read-Host "Press Enter to close" | Out-Null }
  exit 1
}
