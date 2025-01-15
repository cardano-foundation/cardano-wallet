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
.\mithril-client.exe cdb download --download-dir %NODE_DB_DIR% %digest%
move %NODE_DB_DIR%\db\* %NODE_DB_DIR%
ls %NODE_DB_DIR%

REM ------------- ruby tests -------------

cd test\e2e

echo "Running Ruby tests"
echo "Setting up preprod environment"
rm -rf state\configs\preprod
REM copy the preprod config from the root config folder instead of deleting the link

call bundle exec rake setup[preprod,%BUILDKITE_BRANCH%]
echo "Displaying versions"
call bundle exec rake display_versions
echo "Starting node and wallet"
call bundle exec rake start_node_and_wallet[preprod]
echo "Waiting for node to sync"
call bundle exec rake wait_until_node_synced
echo "Running tests"
call bundle exec rake spec SPEC_OPTS="-e 'Stake Pools'"
echo "Stopping node and wallet"
call bundle exec rake stop_node_and_wallet[preprod]