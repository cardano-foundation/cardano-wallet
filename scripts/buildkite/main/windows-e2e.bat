set PATH=%PATH%;C:\Users\hal\AppData\Local\Microsoft\WinGet\Links
buildkite-agent artifact download "windows-tests/cardano-wallet-tests-win64/*" .
cd cardano-wallet-tests-win64
cardano-wallet-integration-test-e2e.exe
exit /b %ERRORLEVEL%
