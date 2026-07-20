# install-service.ps1 -- make the Bank Statement OCR app start AUTOMATICALLY on
# every server boot (Windows / Task Scheduler). Run ONCE in an ADMIN PowerShell:
#
#   powershell -ExecutionPolicy Bypass -File scripts\install-service.ps1           # web app at boot
#   powershell -ExecutionPolicy Bypass -File scripts\install-service.ps1 -Inbox    # + folder poller every 2 min
#
# After this the app comes online by itself whenever the machine powers on, and
# restarts itself if it ever crashes. No NSSM, no extra software needed.
#
#   Manage:  open "Task Scheduler" -> BankStatementsApp
#   Stop:    Stop-ScheduledTask   -TaskName BankStatementsApp
#   Update:  replace the folder, then  Restart-ScheduledTask -TaskName BankStatementsApp
#   Remove:  Unregister-ScheduledTask -TaskName BankStatementsApp -Confirm:$false
#
# Env override: set BSO_PORT before running to change the port (default 8100).
param([switch]$Inbox)
$ErrorActionPreference = "Stop"

# Must be admin to register a machine-wide, run-at-boot task.
$admin = ([Security.Principal.WindowsPrincipal] `
  [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole(
  [Security.Principal.WindowsBuiltinRole]::Administrator)
if (-not $admin) { throw "Please run this in an Administrator PowerShell window." }

$AppDir  = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
$Rscript = (Get-Command Rscript.exe -ErrorAction SilentlyContinue).Source
if (-not $Rscript) { throw "Rscript.exe not found on PATH. Run scripts\setup.ps1 first." }

Write-Host "==> App folder : $AppDir"
Write-Host "==> Rscript    : $Rscript"

# Ensure the folders the task writes to exist.
foreach ($d in "logs","out","inbox","outbox","processed","failed") {
  New-Item -ItemType Directory -Force -Path (Join-Path $AppDir $d) | Out-Null
}
if ($env:BSO_PORT) {
  [Environment]::SetEnvironmentVariable("BSO_PORT", $env:BSO_PORT, "Machine")
  Write-Host "==> Web port   : $($env:BSO_PORT)"
} else {
  Write-Host "==> Web port   : 8100 (default)"
}

# Run as SYSTEM so it starts at boot with no one logged in.
$principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" -LogonType ServiceAccount -RunLevel Highest

# --- Web app: at every startup, kept alive, WITH the working directory set ---
$appAction = New-ScheduledTaskAction -Execute $Rscript `
  -Argument "scripts\run_app.R" -WorkingDirectory $AppDir
$appTrigger = New-ScheduledTaskTrigger -AtStartup
$appSettings = New-ScheduledTaskSettingsSet `
  -RestartCount 999 -RestartInterval (New-TimeSpan -Minutes 1) `
  -ExecutionTimeLimit ([TimeSpan]::Zero) `
  -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries
Register-ScheduledTask -TaskName "BankStatementsApp" -Action $appAction `
  -Trigger $appTrigger -Principal $principal -Settings $appSettings -Force | Out-Null
Start-ScheduledTask -TaskName "BankStatementsApp"
Write-Host "==> Installed 'BankStatementsApp' (web app), runs at boot and started now."

if ($Inbox) {
  # Folder poller every 2 minutes; IgnoreNew stops a slow batch overlapping itself.
  $inAction = New-ScheduledTaskAction -Execute $Rscript `
    -Argument "scripts\serve_inbox.R" -WorkingDirectory $AppDir
  $inTrigger = New-ScheduledTaskTrigger -Once -At (Get-Date) `
    -RepetitionInterval (New-TimeSpan -Minutes 2) -RepetitionDuration ([TimeSpan]::MaxValue)
  $inSettings = New-ScheduledTaskSettingsSet -MultipleInstances IgnoreNew `
    -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries
  Register-ScheduledTask -TaskName "BankStatementsInbox" -Action $inAction `
    -Trigger $inTrigger -Principal $principal -Settings $inSettings -Force | Out-Null
  Write-Host "==> Installed 'BankStatementsInbox' (folder poller, every 2 min)."
}

Write-Host ""
Write-Host "======================================================================"
Write-Host " Auto-start is on. The app returns by itself after any reboot."
Write-Host " Open:   http://<this-vm>:$(if($env:BSO_PORT){$env:BSO_PORT}else{'8100'})"
Write-Host " Manage: Task Scheduler -> BankStatementsApp"
Write-Host "======================================================================"
