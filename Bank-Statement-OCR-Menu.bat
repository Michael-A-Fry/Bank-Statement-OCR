@echo off
setlocal EnableExtensions EnableDelayedExpansion

rem ================================================================
rem Bank Statement OCR - Friendly Setup Menu
rem Double-click this file from the repository root OR from scripts\.
rem ================================================================

set "BATDIR=%~dp0"
set "APPDIR=%BATDIR%"
if exist "%BATDIR%..\app.R" set "APPDIR=%BATDIR%..\"
if exist "%BATDIR%app.R" set "APPDIR=%BATDIR%"

pushd "%APPDIR%" >nul 2>&1
if errorlevel 1 (
  echo.
  echo ERROR: Could not change to the app folder:
  echo   %APPDIR%
  echo.
  pause
  exit /b 1
)

if not exist "app.R" if not exist "server.R" (
  echo.
  echo ERROR: This folder does not look like the Bank Statement OCR app folder.
  echo It must contain app.R or server.R.
  echo.
  echo Current folder:
  cd
  echo.
  echo Put this BAT file either in the repository root or in the scripts folder.
  echo.
  pause
  popd >nul 2>&1
  exit /b 1
)

set "PORT=%BSO_PORT%"
if "%PORT%"=="" set "PORT=8100"

:MENU
cls
echo ================================================================
echo             Bank Statement OCR - Easy Launcher
echo ================================================================
echo.
echo App folder:
echo   %CD%
echo.
echo Web address after start:
echo   http://localhost:%PORT%
echo.
echo Choose an option:
echo.
echo   1. First-time setup / repair packages
echo   2. First-time setup, then start the app
echo   3. Start the app
echo   4. Install auto-start for this Windows user ^(no admin^)
echo   5. Open the app in the browser
echo   6. Diagnostics - find Rscript.exe
echo   7. Exit
echo.
set /p "CHOICE=Type 1-7 and press Enter: "

if "%CHOICE%"=="1" goto SETUP
if "%CHOICE%"=="2" goto SETUPSTART
if "%CHOICE%"=="3" goto START
if "%CHOICE%"=="4" goto AUTOSTART
if "%CHOICE%"=="5" goto OPENAPP
if "%CHOICE%"=="6" goto DIAG
if "%CHOICE%"=="7" goto END

echo.
echo Please choose a number from 1 to 7.
pause
goto MENU

:SETUP
call :RUNPS "scripts\setup.ps1"
goto MENU

:SETUPSTART
call :RUNPS "scripts\setup.ps1" "-Start"
goto MENU

:START
call :RUNPS "scripts\start.ps1"
goto MENU

:AUTOSTART
call :RUNPS "scripts\install-service.ps1"
goto MENU

:OPENAPP
start "" "http://localhost:%PORT%"
goto MENU

:DIAG
cls
echo ================================================================
echo Diagnostics - find Rscript.exe
echo ================================================================
echo.
echo Current folder:
cd
echo.
echo Checking for app file...
if exist "app.R" echo   OK: app.R found
if exist "server.R" echo   OK: server.R found
if not exist "app.R" if not exist "server.R" echo   ERROR: app.R/server.R not found

echo.
echo Checking Rscript on PATH...
where Rscript.exe 2>nul
if errorlevel 1 echo   Rscript.exe is not on PATH. That is OK if setup.ps1 can find it under Program Files.

echo.
echo Looking under C:\Program Files\R ...
if exist "C:\Program Files\R" (
  dir /s /b "C:\Program Files\R\Rscript.exe" 2>nul
) else (
  echo   C:\Program Files\R not found.
)
echo.
pause
goto MENU

:RUNPS
set "SCRIPT=%~1"
set "ARG1=%~2"
cls
echo ================================================================
echo Running: %SCRIPT% %ARG1%
echo ================================================================
echo.
if not exist "%SCRIPT%" (
  echo ERROR: Could not find %SCRIPT%
  echo Make sure the revised PowerShell scripts are in the scripts folder.
  echo.
  pause
  exit /b 1
)
powershell -NoProfile -ExecutionPolicy Bypass -File "%SCRIPT%" %ARG1%
set "RC=%ERRORLEVEL%"
echo.
if not "%RC%"=="0" (
  echo The command ended with error code %RC%.
  echo If you see a package error such as 'there is no package called DT', choose option 1.
) else (
  echo Done.
)
echo.
pause
exit /b %RC%

:END
popd >nul 2>&1
endlocal
exit /b 0
