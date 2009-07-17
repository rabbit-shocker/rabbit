@echo off
if "%OS%" == "Windows_NT" goto WinNT
start rubyw -S rabbit --logger-type gui %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofruby
:WinNT
start rubyw -S rabbit --logger-type gui %*
goto endofruby
:endofruby
