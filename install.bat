ruby setup.rb config
if not %errorlevel%==0 goto end
ruby setup.rb setup
if not %errorlevel%==0 goto end
ruby setup.rb install
if not %errorlevel%==0 goto end

:end
pause
