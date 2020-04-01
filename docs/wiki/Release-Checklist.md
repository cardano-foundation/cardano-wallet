# Checklist to follow upon publishing the release

## Create the release

- [ ] Bump all package versions to the next version, e.g.

  ```
  $ set OLD_VERSION "2020.3.16"
  $ set NEW_VERSION "2020.4.1"
  $ find nix -name "*.nix" -type file | xargs perl -i -pe "s/$OLD_VERSION/$NEW_VERSION/"
  $ find lib -name "*.cabal" -type file | xargs perl -i -pe "s/$OLD_VERSION/$NEW_VERSION/"
  ```

- [ ] Tag and sign the release commit with a proper tag (make sure Hydra's job has started building `master`!)

  ```
  $ VERSION=<new-version> git tag -s -m $VERSION $VERSION
  ```

> :warning: We use a slightly different notation between `.cabal` and git tags! Git tags follows the following format: `vYYYY-MM-DD` (notice the `v` and hyphens) whereas cabal version are written as: `YYYY.MM.DD`.

- [ ] Trigger a release build on CI (Travis) and wait for the build artifacts to be published on github

  ```
  $ git push origin --tags
  ```

## Create the release notes

- [ ] Verify all PRs since the last release have a corresponding milestone (hint: we can filter PR by merge date on github using `merged:>yyyy-mm-dd` filter).

- [ ] List of all the stories and corresponding PRs included in the release. A useful script for this [here](https://gist.github.com/KtorZ/5098d42611658c65a5df835b0e73336f)

- [ ] Write release notes in the [release page](https://github.com/input-output-hk/cardano-wallet/releases), following the [RELEASE_TEMPLATE](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/RELEASE_TEMPLATE.md).



## Verify release artifacts

- [ ] Verify that the documentations have been correctly exported on [gh-pages](https://github.com/input-output-hk/cardano-wallet/tree/gh-pages)

- [ ] Verify that the [CLI manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface) is up-to-date / Update the [CLI manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface)


## Manual ad-hoc verifications

- [ ] Execute all [manual scenarios](https://github.com/input-output-hk/cardano-wallet/tree/master/test/manual) on the binaries to be released.

- [ ] Verify that sensitive fields listed in [Cardano/Wallet/Api/Server](https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core/src/Cardano/Wallet/Api/Server.hs#L333-L340) are still accurate and aren't missing any new ones.
```
          "passphrase"
        , "old_passphrase"
        , "new_passphrase"
        , "mnemonic_sentence"
        , "mnemonic_second_factor"
```

- [ ] Verify latest [buildkite nightly](https://buildkite.com/input-output-hk/cardano-wallet-nightly) and make sure the results are fine.

- [ ] Manually run the disabled `LAUNCH` tests in:
    - [ ] `lib/jormungandr/test/integration/Test/Integration/Jormungandr/Scenario/CLI/Launcher.hs`


## Publication

- [ ] Once everyone has signed off (i.e. Tech lead, QA & Release manager), publish the release draft.

- [ ] Update the "Compatibility Matrix" in the README.md (keep info about last 3 versions of `cardano-wallet`).

- [ ] Add the release to the [automated migration tests](https://github.com/input-output-hk/cardano-wallet/blob/master/nix/migration-tests.nix#L44-L61) (keep only the last 3 versions).  
