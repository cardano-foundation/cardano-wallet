# Run Flaky Tests

Some tests might have been disabled in CI with the `flakyBecauseOf` helper.

Run them locally using 

```bash
RUN_FLAKY_TESTS=1 stack test cardano-wallet:integration
```

or on Windows:
```bash
set RUN_FLAKY_TESTS=1
cardano-wallet-test-integration.exe
```
