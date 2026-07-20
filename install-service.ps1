# install-service.ps1 -- create a Windows Scheduled Task for Bank Statement OCR.
#
# Admin mode, machine-wide at boot:
#   powershell -ExecutionPolicy Bypass -File scripts\install-service.ps1 -Machine
#
# Non-admin mode, current user at logon:
#   powershell -ExecutionPolicy Bypass -File scripts\install-service.ps1
#
# Optional folder poller:
#   powershell -ExecutionPolicy Bypass -File scripts\install-service.ps1 -Inbox
#
# Remove tasks:
#   Unregister-ScheduledTask -TaskName BankStatementsApp -Confirm:$false
#   Unregister-ScheduledTask -TaskName BankStatementsInbox -Confirm:$false

param(
  [switch]$Inbox,
  [switch]$Machine,
  [switch]$NoPause
)

$ErrorActionPreference = "Stop"

function Test-IsAdmin {
  $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
  $principal = New-Object Security.Principal.WindowsPrincipal($identity)
  return $principal.IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
}

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

  foreach ($d in @("logs","out","inbox","outbox","processed","failed","templates_user")) {
    New-Item -ItemType Directory -Force -Path (Join-Path $AppDir $d) | Out-Null
  }

  $Rscript = Resolve-Rscript
  $port = if ($env:BSO_PORT) { $env:BSO_PORT } else { "8100" }
  $isAdmin = Test-IsAdmin

  if ($Machine -and -not $isAdmin) {
    throw "-Machine requires an Administrator PowerShell. Re-run without -Machine to install a current-user logon task instead."
  }

  Write-Host "==> App folder : $AppDir" -ForegroundColor Cyan
  Write-Host "==> Rscript    : $Rscript"
  Write-Host "==> Web port   : $port"

  $appAction = New-ScheduledTaskAction -Execute $Rscript -Argument "scripts\run_app.R" -WorkingDirectory $AppDir
  $settings = New-ScheduledTaskSettingsSet -RestartCount 999 -RestartInterval (New-TimeSpan -Minutes 1) -ExecutionTimeLimit ([TimeSpan]::Zero) -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries

  if ($Machine) {
    $principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" -LogonType ServiceAccount -RunLevel Highest
    $trigger = New-ScheduledTaskTrigger -AtStartup
    Write-Host "==> Registering machine-wide startup task as SYSTEM..."
  } else {
    $user = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name
    $principal = New-ScheduledTaskPrincipal -UserId $user -LogonType Interactive -RunLevel LeastPrivilege
    $trigger = New-ScheduledTaskTrigger -AtLogOn -User $user
    Write-Host "==> Registering current-user logon task for $user..." -ForegroundColor Yellow
    Write-Host "    This does not require admin rights, but it starts when this user logs on, not at machine boot."
  }

  Register-ScheduledTask -TaskName "BankStatementsApp" -Action $appAction -Trigger $trigger -Principal $principal -Settings $settings -Force | Out-Null
  Start-ScheduledTask -TaskName "BankStatementsApp"
  Write-Host "==> Installed and started 'BankStatementsApp'."

  if ($Inbox) {
    $inAction = New-ScheduledTaskAction -Execute $Rscript -Argument "scripts\serve_inbox.R" -WorkingDirectory $AppDir
    $inTrigger = New-ScheduledTaskTrigger -Once -At (Get-Date).AddMinutes(1) -RepetitionInterval (New-TimeSpan -Minutes 2) -RepetitionDuration ([TimeSpan]::MaxValue)
    $inSettings = New-ScheduledTaskSettingsSet -MultipleInstances IgnoreNew -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries
    Register-ScheduledTask -TaskName "BankStatementsInbox" -Action $inAction -Trigger $inTrigger -Principal $principal -Settings $inSettings -Force | Out-Null
    Start-ScheduledTask -TaskName "BankStatementsInbox"
    Write-Host "==> Installed and started 'BankStatementsInbox' folder poller."
  }

  Write-Host ""
  Write-Host "======================================================================"
  Write-Host " Scheduled task installed."
  Write-Host " Open locally: http://localhost:$port"
  Write-Host " Manage: Task Scheduler -> BankStatementsApp"
  Write-Host "======================================================================"
}
catch {
  Write-Host ""
  Write-Host "INSTALL-SERVICE FAILED" -ForegroundColor Red
  Write-Host $_.Exception.Message -ForegroundColor Red
  Write-Host ""
  if (-not $NoPause) { Read-Host "Press Enter to close" | Out-Null }
  exit 1
}
