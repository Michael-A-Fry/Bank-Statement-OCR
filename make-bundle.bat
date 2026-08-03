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

rem --- DID THIS SCRIPT EVEN START? -----------------------------------------
rem  Every failure below ends in `pause`, so the window cannot close on its own.
rem  A window that flashes and vanishes therefore means the script never ran at
rem  all -- almost always AppLocker / Software Restriction Policy on a managed
rem  build, which kills the interpreter before line 1 and (when you use "Run as
rem  administrator") says "blocked by your system administrator".
rem  That is invisible from inside a .bat, so the FIRST thing we do is leave a
rem  mark on disk. No make-bundle.log next to this file after a flash = the
rem  script was blocked, not broken. Nothing here needs admin rights.
set "LOG=%HERE%\make-bundle.log"
> "%LOG%" echo [%DATE% %TIME%] make-bundle.bat started in "%HERE%"
if not exist "%LOG%" (
  rem  Cannot even write beside ourselves - read-only folder, or running from a
  rem  zip / network share nobody has extracted. Say so rather than pressing on.
  echo(
  echo [X] Could not write make-bundle.log into "%HERE%".
  echo     Copy this folder to a normal writable place ^(e.g. C:\Temp^) first -
  echo     building in place from a zip or a read-only share cannot work.
  echo(
  pause & endlocal & exit /b 1
)

echo(
echo ============================================================
echo   Statement Studio  -  building the offline package
echo ============================================================
echo(

rem --- find R (needs internet-connected R installed on THIS PC) --------------
call :findR
if not defined RSCRIPT (
  >> "%LOG%" echo [X] R not found on PATH or under "%ProgramFiles%\R"
  echo [X] R was not found on this PC.
  echo     Install R for Windows from https://cran.r-project.org/bin/windows/base/
  echo     ^(match the version your server will run^), then run this again.
  echo(
  echo     If R IS installed, this PC's policy may be hiding it. You can build
  echo     without this script - open Command Prompt and run, in one line:
  echo         "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" "%HERE%\scripts\bundle-offline.R"
  echo     ^(adjust the R version folder to the one you have^).
  echo(
  pause & endlocal & exit /b 1
)
>> "%LOG%" echo Using R: !RSCRIPT!
echo Using R: !RSCRIPT!
echo(
echo Downloading packages and installers ^(a few minutes^)...
echo(

"!RSCRIPT!" "%HERE%\scripts\bundle-offline.R"
set "RC=%ERRORLEVEL%"
echo(
>> "%LOG%" echo bundle-offline.R exited with %RC%
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
