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
rem ==========================================================================
setlocal enableextensions
title Statement Studio
set "APP=%~dp0"
if "%APP:~-1%"=="\" set "APP=%APP:~0,-1%"
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

rem --- first run only : packages + OCR (guarded by a marker) -----------------
if not exist "%BUNDLE%\.installed" call :firstRun

rem --- start ----------------------------------------------------------------
echo Starting Statement Studio... open the http://... URL below in a browser.
echo Leave this window open; press Ctrl-C to stop.
echo(
"%RSCRIPT%" "%APP%\scripts\run_app.R"
echo(
pause
endlocal
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
echo First run: installing packages and OCR tools ^(a few minutes, offline^)...
pushd "%BUNDLE%"
"%RSCRIPT%" install-on-pc.R
popd
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

:rfail
echo(
echo [X] Could not set up the private R.
echo     - If a Windows permission prompt appeared and was declined, run this
echo       file again and accept it.
echo     - If offline\prereqs has no R-*-win.exe installer, rebuild the package
echo       with make-bundle.bat on an internet PC and copy the whole
echo       'StatementStudio-offline' folder here again.
echo(
pause
endlocal
goto :eof
