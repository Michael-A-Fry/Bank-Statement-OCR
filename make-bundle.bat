@echo off
rem ==========================================================================
rem  Statement Studio - BUILD THE OFFLINE PACKAGE. Run this ONCE, on a normal
rem  Windows PC that HAS internet. Double-click it.
rem
rem  It gathers the whole app plus every package and installer the air-gapped
rem  server needs into ONE folder:  StatementStudio-offline
rem
rem  Then: copy that whole folder to the server and double-click RUN-ME.bat
rem  inside it. That's the entire setup - two double-clicks, no internet on the
rem  server.
rem
rem  No version-matching to worry about: this PC's R ships inside the bundle and
rem  the server installs and uses that exact R privately, so the packages always
rem  match. This PC just needs ANY recent R with internet.
rem ==========================================================================
setlocal enableextensions enabledelayedexpansion
title Statement Studio - build offline package
set "HERE=%~dp0"
if "%HERE:~-1%"=="\" set "HERE=%HERE:~0,-1%"

echo(
echo ============================================================
echo   Statement Studio  -  building the offline package
echo ============================================================
echo(

rem --- find R (needs internet-connected R installed on THIS PC) --------------
call :findR
if not defined RSCRIPT (
  echo [X] R was not found on this PC.
  echo     Install R for Windows from https://cran.r-project.org/bin/windows/base/
  echo     ^(match the version your server will run^), then run this again.
  echo(
  pause & endlocal & exit /b 1
)
echo Using R: !RSCRIPT!
echo(
echo Downloading packages and installers ^(a few minutes^)...
echo(

"!RSCRIPT!" "%HERE%\scripts\bundle-offline.R"
set "RC=%ERRORLEVEL%"
echo(
if not "%RC%"=="0" (
  echo [X] Build failed ^(code %RC%^). Check the messages above - usually no internet
  echo     or a proxy blocking the download.
  echo(
  pause & endlocal & exit /b %RC%
)

echo ============================================================
echo   Done. Next:
echo     1^) copy the whole 'StatementStudio-offline' folder to the server
echo     2^) double-click RUN-ME.bat inside it
echo ============================================================
echo(
pause
endlocal
goto :eof

:findR
set "RSCRIPT="
for /f "delims=" %%R in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%R"
if defined RSCRIPT goto :eof
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%~fD\bin\x64\Rscript.exe" set "RSCRIPT=%%~fD\bin\x64\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
goto :eof
