@echo off
rem ==========================================================================
rem  Statement Studio - one-shot OFFLINE setup for an air-gapped Windows box.
rem  Double-click it, or run it from a terminal. Works wherever this folder is
rem  (it locates itself). No internet needed.
rem
rem  It: finds R (or installs it from the bundle) -> installs all R packages
rem  offline -> sets up Poppler + Tesseract for scanned-PDF OCR -> creates
rem  config\config.yaml -> runs a smoke test.
rem
rem  Needs the 'bso-offline' bundle (built by scripts\bundle-offline.R on an
rem  internet machine) placed next to, or inside, this folder.
rem ==========================================================================
setlocal enableextensions enabledelayedexpansion
title Statement Studio - offline setup
set "APP=%~dp0"
if "%APP:~-1%"=="\" set "APP=%APP:~0,-1%"

echo(
echo ============================================================
echo   Statement Studio  -  offline setup
echo   App folder: %APP%
echo ============================================================
echo(

rem --- locate the offline bundle (a folder with repo\ and prereqs\) ----------
set "BUNDLE="
for %%D in ("%APP%\bso-offline" "%APP%\..\bso-offline" "%APP%") do (
  if exist "%%~fD\repo\" if exist "%%~fD\prereqs\" if not defined BUNDLE set "BUNDLE=%%~fD"
)
if not defined BUNDLE (
  echo [X] Offline bundle not found.
  echo     Put the 'bso-offline' folder next to this app folder ^(it must contain
  echo     'repo\' and 'prereqs\'^), then run this again.
  goto :end
)
echo Bundle : %BUNDLE%

rem --- 1. R : use an existing install, else install silently from the bundle -
call :findR
if not defined RSCRIPT (
  echo ==^> R not found. Installing it from the bundle ^(silent^)...
  set "RINST="
  for %%F in ("%BUNDLE%\prereqs\R-*-win.exe") do if not defined RINST set "RINST=%%~fF"
  if not defined RINST ( echo [X] No R installer in prereqs\. & goto :end )
  "!RINST!" /VERYSILENT /SUPPRESSMSGBOXES /NORESTART
  call :findR
)
if not defined RSCRIPT ( echo [X] R still not found after install. & goto :end )
echo R      : !RSCRIPT!
echo(

rem --- 2. R packages (+ Poppler + Tesseract) -- all offline, via the bundle --
echo ==^> Installing R packages and OCR tools ^(offline^)...
pushd "%BUNDLE%"
"!RSCRIPT!" install-on-pc.R
popd
echo(

rem --- 3. config : create config\config.yaml from the example if missing -----
if not exist "%APP%\config\config.yaml" (
  if exist "%APP%\config\config.example.yaml" (
    copy /y "%APP%\config\config.example.yaml" "%APP%\config\config.yaml" >nul
    echo Created config\config.yaml  --  now EDIT it: admin_password, shiny_url.
  ) else (
    echo [!] config\config.example.yaml not found -- skipping config.
  )
) else (
  echo config\config.yaml already present -- left unchanged.
)
echo(

rem --- 4. smoke test --------------------------------------------------------
echo ==^> Smoke test ^(tests\run_tests.R^)...
pushd "%APP%"
"!RSCRIPT!" tests\run_tests.R
popd
echo(
echo ============================================================
echo   Setup finished. Next:
echo     1^) edit  config\config.yaml   ^(admin_password, shiny_url^)
echo     2^) run   start.bat            ^(serves on the configured port^)
echo ============================================================
goto :end

:findR
set "RSCRIPT="
for /f "delims=" %%R in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%R"
if defined RSCRIPT goto :eof
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%~fD\bin\x64\Rscript.exe" set "RSCRIPT=%%~fD\bin\x64\Rscript.exe"
if not defined RSCRIPT for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%~fD\bin\Rscript.exe" set "RSCRIPT=%%~fD\bin\Rscript.exe"
goto :eof

:end
echo(
pause
endlocal
