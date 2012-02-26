rem @echo off
@REM Script to build a classpath

set JENA_DIRROOT=%1%
set JENA_CP=

if "%JENA_DIRROOT%" == ""  goto noRoot

set JENA_LIBDIR=%JENA_DIRROOT%\lib
set JENA_CLSDIR=%JENA_DIRROOT%\classes

for %%f in (%JENA_LIBDIR%\*.jar) do call :oneStep %%f

if EXIST %JENA_CLSDIR% call :addClasses
goto :theEnd

:oneStep
REM echo "ARG: %1"
if "%JENA_CP%" == "" (set JENA_CP=%1) else (set JENA_CP=%JENA_CP%;%1)
exit /B

:addClasses
if "%JENA_CP%" == "" (set JENA_CP=%JENA_CLSDIR%) else (set JENA_CP=%JENA_CLSDIR%;%JENA_CP%)
exit /B

:noRoot
echo No directory for root of installation
exit /B

:theEnd
rem echo %JENA_CP%
exit /B
