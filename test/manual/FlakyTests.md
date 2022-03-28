# Run Flaky Tests

Some tests might have been disabled in CI with the `flakyBecauseOf` helper.

Run them locally using:

```bash
RUN_FLAKY_TESTS=1 cabal test cardano-wallet:integration
```

Running integration tests under Wine is not possible because ouroboros-network doesn't fully work under Wine.
