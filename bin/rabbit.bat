@echo off
if "%OS%" == "Windows_NT" goto WinNT
start rubyw -S rabbit %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofruby
:WinNT
start rubyw -S rabbit %*
goto endofruby
:endofruby
