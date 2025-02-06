set PATH=%PATH%;C:\Users\hal\AppData\Local\Microsoft\WinGet\Links

cd test\e2e
call bundle install
call bundle exec rake get_latest_windows_tests[%BUILDKITE_BRANCH%,bins,any,latest]
bins\cardano-wallet-integration-test-e2e.exe
exit /b %ERRORLEVEL%
