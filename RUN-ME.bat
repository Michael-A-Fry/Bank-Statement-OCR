@echo off
rem ==========================================================================
rem  Statement Studio - the ONLY file you run on the server. Double-click it.
rem
rem  It installs and uses its OWN private copy of R INSIDE this folder, so it is
rem  completely isolated from anything already on the machine. Whatever R or
rem  RStudio (old or new) is already installed is IGNORED and left exactly as it
rem  is - nothing is upgraded, replaced, or removed, and this private R does not
rem  become the machine default.
rem
rem  First run : installs everything (private R, packages, OCR) OFFLINE, starts.
rem  Every run after that : just starts.
rem
rem  Works wherever this folder lives. No internet needed. When it is running,
rem  open the http://... URL it prints. Press Ctrl-C in this window to stop.
rem
rem  RUN-ME.bat /service  = same thing with NO "Press any key" pauses. Use this
rem  from Task Scheduler: a pause leaves the task stuck at "Running" long after R
rem  has exited, which defeats restart-on-failure. A human double-click gets the
rem  pauses (so an error message stays on screen).
rem  See docs/operational/running-and-keeping-it-up.md.
rem ==========================================================================
setlocal enableextensions
title Statement Studio
set "APP=%~dp0"
if "%APP:~-1%"=="\" set "APP=%APP:~0,-1%"
set "NOPAUSE="
if /i "%~1"=="/service" set "NOPAUSE=1"
if /i "%~1"=="-service" set "NOPAUSE=1"
set "BUNDLE=%APP%\offline"
set "RUNTIME=%APP%\R-runtime"
set "RLIB=%APP%\R-lib"
set "RSCRIPT=%RUNTIME%\bin\x64\Rscript.exe"
if not exist "%RSCRIPT%" if exist "%RUNTIME%\bin\Rscript.exe" set "RSCRIPT=%RUNTIME%\bin\Rscript.exe"

rem --- private, isolated R : install our OWN copy; never touch the server's ---
if not exist "%RSCRIPT%" call :installR
if not exist "%RSCRIPT%" goto :rfail

rem --- keep R fully app-local so the old R/RStudio environment can't leak in --
if not exist "%RLIB%" mkdir "%RLIB%"
set "R_LIBS_USER=%RLIB%"
set "R_LIBS_SITE="
set "R_PROFILE_USER=%APP%\.none"
set "R_ENVIRON_USER=%APP%\.none"

rem --- config : restore-or-seed it, and keep a backup OUTSIDE the folder so an
rem     update (even replacing the whole folder) never loses your settings --------
call :configSync

rem --- dictionaries : the words the TEAM taught the tool. Same restore-or-seed
rem     treatment as config, for the same reason (see :dictSync) ----------------
call :dictSync

rem --- first run only : packages + OCR (guarded by a marker) -----------------
set "SETUPFAIL="
if not exist "%BUNDLE%\.installed" call :firstRun
if defined SETUPFAIL goto :setupfail

rem --- start ----------------------------------------------------------------
echo Starting Statement Studio... open the http://... URL below in a browser.
echo Leave this window open; press Ctrl-C to stop.
echo(
"%RSCRIPT%" "%APP%\scripts\run_app.R"
echo(
call :maybePause
endlocal
goto :eof

:maybePause
if not defined NOPAUSE pause
goto :eof

:installR
echo First run: installing a private copy of R inside this folder ^(offline^)...
set "RINST="
for %%F in ("%BUNDLE%\prereqs\R-*-win.exe") do if not defined RINST set "RINST=%%~fF"
if not defined RINST goto :eof
rem  Silent + non-invasive: /DIR keeps R inside this folder; !recordversion means
rem  it does NOT register as the machine's R, and !associate means it does NOT grab
rem  .RData file types - so RStudio and any existing R are untouched.
"%RINST%" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /NOICONS /SP- /DIR="%RUNTIME%" /MERGETASKS="!recordversion,!associate"
set "RSCRIPT=%RUNTIME%\bin\x64\Rscript.exe"
if not exist "%RSCRIPT%" if exist "%RUNTIME%\bin\Rscript.exe" set "RSCRIPT=%RUNTIME%\bin\Rscript.exe"
goto :eof

:firstRun
rem  The .installed marker means "setup finished, never do it again". It used to be
rem  written UNCONDITIONALLY, so a failed install was frozen in place: every later
rem  launch skipped setup, and the documented remedy ("run RUN-ME.bat again") could
rem  never actually re-run it. Write the marker ONLY when the installer reports
rem  success, so a broken setup retries instead of staying broken for ever.
echo First run: installing packages and OCR tools ^(a few minutes, offline^)...
pushd "%BUNDLE%"
"%RSCRIPT%" install-on-pc.R
set "RC=%ERRORLEVEL%"
popd
if not "%RC%"=="0" set "SETUPFAIL=%RC%"
if defined SETUPFAIL goto :eof
type nul > "%BUNDLE%\.installed"
echo(
echo Setup complete.
echo(
goto :eof

:configSync
rem  Config lives in config\config.yaml. We also keep a copy under %LOCALAPPDATA%
rem  so it survives replacing the whole app folder on an update:
rem   - no config here yet + a backup exists -> restore it (keeps your settings)
rem   - no config + no backup               -> seed from config.example.yaml
rem   - config present                      -> refresh the backup
set "CFGBAK="
if defined LOCALAPPDATA set "CFGBAK=%LOCALAPPDATA%\StatementStudio\config.yaml"
if not exist "%APP%\config\config.yaml" if defined CFGBAK if exist "%CFGBAK%" copy /y "%CFGBAK%" "%APP%\config\config.yaml" >nul
if not exist "%APP%\config\config.yaml" if exist "%APP%\config\config.example.yaml" copy /y "%APP%\config\config.example.yaml" "%APP%\config\config.yaml" >nul
if not defined CFGBAK goto :eof
if not exist "%APP%\config\config.yaml" goto :eof
if not exist "%LOCALAPPDATA%\StatementStudio" mkdir "%LOCALAPPDATA%\StatementStudio" >nul 2>&1
copy /y "%APP%\config\config.yaml" "%CFGBAK%" >nul 2>&1
goto :eof

:dictSync
rem  dictionaries\labels.yaml and dictionaries\lexicon.yaml are the wordings and
rem  recognition markers the TEAM taught the tool through Admin. They are live
rem  state, not shipped files: the bundle carries only *.example.yaml, so replacing
rem  the app folder on an update cannot revert them (statements that reconciled
rem  last week would quietly stop reconciling). Same three rules as config:
rem   - none here + a backup exists -> restore it   (keeps the taught words)
rem   - none here + no backup       -> seed from the shipped .example.yaml
rem   - present                     -> refresh the backup
rem  The app also writes labels.yaml.bak / lexicon.yaml.bak before each Admin save.
if not exist "%APP%\dictionaries" mkdir "%APP%\dictionaries" >nul 2>&1
set "DICTBAK="
if defined LOCALAPPDATA set "DICTBAK=%LOCALAPPDATA%\StatementStudio\dictionaries"
call :dictOne labels.yaml
call :dictOne lexicon.yaml
goto :eof

:dictOne
if not exist "%APP%\dictionaries\%~1" if defined DICTBAK if exist "%DICTBAK%\%~1" copy /y "%DICTBAK%\%~1" "%APP%\dictionaries\%~1" >nul
if not exist "%APP%\dictionaries\%~1" if exist "%APP%\dictionaries\%~n1.example%~x1" copy /y "%APP%\dictionaries\%~n1.example%~x1" "%APP%\dictionaries\%~1" >nul
if not defined DICTBAK goto :eof
if not exist "%APP%\dictionaries\%~1" goto :eof
if not exist "%DICTBAK%" mkdir "%DICTBAK%" >nul 2>&1
copy /y "%APP%\dictionaries\%~1" "%DICTBAK%\%~1" >nul 2>&1
goto :eof

:setupfail
echo(
echo [X] Offline setup did NOT complete ^(code %SETUPFAIL%^) - the app was NOT started.
echo     Some R packages could not be installed from offline\repo, so the tool
echo     would not run correctly. Scroll up: the installer named which ones.
echo     - Most often the bundle was built under a different R x.y. Rebuild it with
echo       make-bundle.bat on the internet PC and copy the whole folder here again.
echo     - Then just run this file again: setup RETRIES until it succeeds.
echo(
call :maybePause
endlocal
exit /b 1

:rfail
echo(
echo [X] Could not set up the private R.
echo     - If a Windows permission prompt appeared and was declined, run this
echo       file again and accept it.
echo     - If offline\prereqs has no R-*-win.exe installer, rebuild the package
echo       with make-bundle.bat on an internet PC and copy the whole
echo       'StatementStudio-offline' folder here again.
echo(
call :maybePause
endlocal
rem  Exit NON-ZERO so a scheduled task records a failure (and can restart) rather
rem  than reporting a clean run that never started anything.
exit /b 1
