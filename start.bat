@echo off
rem ==========================================================================
rem  Statement Studio - start the app. Double-click, or run from a terminal.
rem  Serves on the port in config\config.yaml (default 8100); open
rem  http://<this-host>:<port> in a browser. Ctrl-C in this window stops it.
rem  Locates itself, so it works wherever this folder lives. Run
rem  offline-setup.bat once first.
rem ==========================================================================
setlocal enableextensions
set "APP=%~dp0"
if "%APP:~-1%"=="\" set "APP=%APP:~0,-1%"

call :findR
if not defined RSCRIPT (
  echo [X] R not found. Run offline-setup.bat first ^(or install R^).
  pause & goto :eof
)
echo Starting Statement Studio from %APP% ...
echo Open the URL shown below in a browser. Press Ctrl-C here to stop.
echo(
"%RSCRIPT%" "%APP%\scripts\run_app.R"
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
