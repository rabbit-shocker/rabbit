@echo off

@echo ruby setup.rb

rem run installer
ruby setup.rb
if errorlevel 1 goto error

rem install check & show welcome message
ruby -rrabbit/rabbit -e "puts \"*** Successfully installed Rabbit #{Rabbit::VERSION}!\""
if errorlevel 1 goto error

goto end

:error
@echo *** Failed to install Rabbit... (error code: %errorlevel%)

:end
pause
