# =============================================================================
#  Statement Studio - BUILD THE OFFLINE PACKAGE (PowerShell)
#
#  Same job as make-bundle.bat: gather the whole app plus every package and
#  installer the air-gapped server needs into ONE folder, StatementStudio-offline.
#
#  WHY THIS EXISTS ALONGSIDE THE .bat
#  On a managed build, AppLocker or a Software Restriction Policy commonly blocks
#  .bat/.cmd outright - the window flashes and vanishes, and "Run as
#  administrator" reports "blocked by your system administrator". Elevation does
#  not lift a policy block, and nothing inside a .bat can work around it.
#  PowerShell is usually still allowed, so this is the same build with a target a
#  shortcut can call:
#
#      powershell.exe -ExecutionPolicy Bypass -NoProfile -File "<path>\make-bundle.ps1"
#
#  -ExecutionPolicy Bypass lifts PowerShell's OWN policy for this one launch. It
#  is not an admin action and changes nothing on the machine.
#  Setup instructions: docs/operational/first-time-setup.md, step 1.
#
#  This needs NO administrator rights. If something asks you to elevate, that is
#  a sign the folder is in the wrong place, not that the build needs it.
# =============================================================================

# 'Stop' makes every error terminating, which is what you want for a build - but
# a terminating error would leave this script BEFORE the Read-Host at the bottom,
# so the window would close on its own and we would have reproduced the exact
# symptom this file exists to cure. Everything below therefore runs inside one
# try/finally: whatever happens, the window waits and you get to read it.
$ErrorActionPreference = 'Stop'
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$log  = Join-Path $here 'make-bundle.log'
$rc   = 1

function Say { param($m) Write-Host $m; Add-Content -Path $log -Value $m -ErrorAction SilentlyContinue }

# The first thing we do is leave a mark on disk, for the same reason the .bat
# does: if this never runs, there is no log, and that distinguishes "blocked
# before line 1" from "ran and failed".
try { Set-Content -Path $log -Value "[$(Get-Date -Format s)] make-bundle.ps1 started in $here" }
catch {
  Write-Host ''
  Write-Host "[X] Could not write make-bundle.log into $here"
  Write-Host "    Copy this folder somewhere writable (e.g. C:\Temp) and try again -"
  Write-Host "    building in place from a zip or a read-only share cannot work."
  Write-Host ''
  Read-Host 'Press Enter to close'; exit 1
}

try {

Say ''
Say '============================================================'
Say '  Statement Studio  -  building the offline package'
Say '============================================================'
Say ''

# --- find R on this PC (it needs internet; the SERVER does not) --------------
# Same order the .bat uses: PATH first, then the usual install root, newest last
# so the highest version wins. x64 preferred, 32-bit accepted.
$rscript = $null
$onPath = Get-Command Rscript.exe -ErrorAction SilentlyContinue
if ($onPath) { $rscript = $onPath.Source }
if (-not $rscript) {
  foreach ($root in @("$env:ProgramFiles\R", "${env:ProgramFiles(x86)}\R")) {
    if (-not (Test-Path $root)) { continue }
    Get-ChildItem $root -Directory -Filter 'R-*' -ErrorAction SilentlyContinue |
      Sort-Object Name | ForEach-Object {
        foreach ($rel in @('bin\x64\Rscript.exe', 'bin\Rscript.exe')) {
          $try = Join-Path $_.FullName $rel
          if (Test-Path $try) { $rscript = $try }
        }
      }
  }
}

if (-not $rscript) {
  Say '[X] R was not found on this PC.'
  Say '    Install R for Windows from https://cran.r-project.org/bin/windows/base/'
  Say '    then run this again.'
  throw 'R not found'
}
Say "Using R: $rscript"
Say ''
Say 'Downloading packages and installers (a few minutes)...'
Say ''

$builder = Join-Path $here 'scripts\bundle-offline.R'
if (-not (Test-Path $builder)) {
  Say "[X] Cannot find $builder"
  Say '    Run this from the app folder - the one holding app.R, R\ and scripts\.'
  throw 'builder script not found'
}

# Let R's own output go straight to the console so a proxy or download failure is
# visible as it happens, rather than surfacing minutes later as one exit code.
#
# 'Stop' is relaxed for exactly this call. R writes ordinary progress and package
# warnings to stderr, and some PowerShell hosts surface native stderr as an error
# record - which under 'Stop' would abort a build that was going perfectly well,
# on the strength of a warning R prints every time. The EXIT CODE is what says
# whether the build worked, so that is what we read.
$prev = $ErrorActionPreference
$ErrorActionPreference = 'Continue'
& $rscript $builder
$ErrorActionPreference = $prev
# A native command that never launched leaves $LASTEXITCODE unset; treat that as
# a failure rather than reading it as success.
$rc = if ($null -eq $LASTEXITCODE) { 1 } else { $LASTEXITCODE }
Say "bundle-offline.R exited with $rc"

Say ''
if ($rc -ne 0) {
  Say "[X] Build failed (code $rc). Check the messages above - usually no internet,"
  Say '    or a proxy blocking the download.'
  throw "bundle-offline.R failed with $rc"
}

Say '============================================================'
Say '  Done. Next:'
Say '    1) copy the whole StatementStudio-offline folder to the server'
Say '    2) run RUN-ME.bat inside it'
Say ''
Say '  Read the manifest before you walk away:'
Say '    StatementStudio-offline\offline\manifest.txt'
Say '  Anything listed MISSING there will be missing on the server too.'
Say '============================================================'
$rc = 0

} catch {
  # Anything at all - including an error PowerShell raised for us - lands here
  # with its message on screen and in the log, instead of a window that vanishes.
  Say ''
  Say "[X] $($_.Exception.Message)"
  Say ''
  Say "    The full transcript of this attempt is in:"
  Say "    $log"
  if ($rc -eq 0) { $rc = 1 }
} finally {
  Say ''
  Read-Host 'Press Enter to close' | Out-Null
}
exit $rc
