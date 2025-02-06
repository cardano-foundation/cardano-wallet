set PATH=%PATH%;C:\Users\hal\AppData\Local\Microsoft\WinGet\Links

REM ------------- mithril -------------

SET mithril-tar=mithril-2450.0-windows-x64.tar
echo %mithril-tar%
SET mithril-package=%mithril-tar%.gz
echo %mithril-package%
wget https://github.com/input-output-hk/mithril/releases/download/2450.0/%mithril-package%
7z x .\%mithril-package%
tar xf .\%mithril-tar%
.\mithril-client.exe --version
for /f "delims=" %%i in ('.\mithril-client.exe cdb snapshot list --json ^| jq -r .[0].digest') do set digest=%%i
echo %digest%
.\mithril-client.exe cdb download --download-dir . %digest%
REM delete the old db folder
REM this is stupid but will do for now
mkdir %NODE_DB_DIR%
rd /q /s %NODE_DB_DIR%
REM move the new db folder entirely into the old db folder
move .\db %NODE_DB_DIR%
ls %NODE_DB_DIR%

REM ------------- ruby tests -------------

cd test\e2e
call bundle install
call bundle exec rake get_latest_windows_tests[%BUILDKITE_BRANCH%,bins,any,latest]
cd ..\..

echo %NODE_DB_DIR%
echo %WALLET_DB_DIR%
test\e2e\bins\cardano-wallet-integration-test-e2e.exe
exit /b %ERRORLEVEL%
