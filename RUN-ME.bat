@echo off
rem ==========================================================================
rem  Statement Studio - the ONLY file you run on the server. Double-click it.
rem
rem  First run : installs everything (R, packages, OCR) OFFLINE, then starts.
rem  Every run after that : just starts.
rem
rem  Works wherever this folder lives. No internet needed. When it's running,
rem  open the http://... URL it prints. Press Ctrl-C in this window to stop.
rem ==========================================================================
setlocal enableextensions enabledelayedexpansion
title Statement Studio
set "APP=%~dp0"
if "%APP:~-1%"=="\" set "APP=%APP:~0,-1%"
set "BUNDLE=%APP%\offline"

rem --- R : use an existing install, else install it silently from the bundle -
call :findR
if not defined RSCRIPT (
  echo First run: installing R ^(offline^)...
  set "RINST="
  for %%F in ("%BUNDLE%\prereqs\R-*-win.exe") do if not defined RINST set "RINST=%%~fF"
  if defined RINST "!RINST!" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART
  call :findR
)
if not defined RSCRIPT (
  echo(
  echo [X] R is not installed and no installer was found in offline\prereqs.
  echo     Rebuild the package with make-bundle.bat on an internet PC, then copy
  echo     the whole 'StatementStudio-offline' folder here again.
  echo(
  pause & endlocal & exit /b 1
)

rem --- first run only : packages + OCR + config (guarded by a marker) --------
if not exist "%BUNDLE%\.installed" (
  echo First run: installing packages and OCR tools ^(a few minutes, offline^)...
  pushd "%BUNDLE%"
  "!RSCRIPT!" install-on-pc.R
  popd
  if not exist "%APP%\config\config.yaml" if exist "%APP%\config\config.example.yaml" (
    copy /y "%APP%\config\config.example.yaml" "%APP%\config\config.yaml" >nul
  )
  type nul > "%BUNDLE%\.installed"
  echo(
  echo Setup complete.
  echo(
)

rem --- start ----------------------------------------------------------------
echo Starting Statement Studio... open the http://... URL below in a browser.
echo Leave this window open; press Ctrl-C to stop.
echo(
"!RSCRIPT!" "%APP%\scripts\run_app.R"
echo(
pause
goto :eof

:findR
set "RSCRIPT="
for /f "delims=" %%R in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%R"
if defined RSCRIPT goto :eof
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%~fD\bin\x64\Rscript.exe" set "RSCRIPT=%%~fD\bin\x64\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
goto :eof
