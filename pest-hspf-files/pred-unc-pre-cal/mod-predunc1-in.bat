@echo off
setlocal enabledelayedexpansion
set INTEXTFILE=predunc1.in
set OUTTEXTFILE=predunc%1.in
set SEARCHTEXT=XXX
set REPLACETEXT=%1
set OUTPUTLINE=

for /f "tokens=1,* delims=¶" %%A in ( '"type %INTEXTFILE%"') do (
SET string=%%A
SET modified=!string:%SEARCHTEXT%=%REPLACETEXT%!

echo !modified! >> %OUTTEXTFILE%
)
