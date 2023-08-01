# Release Checklist

- [ ] Make a copy of this `Release Checklist Template` document as a new page on cardano-wallet's wiki called `Release vYYYY-MM-DD`. Follow all the next steps using that newly created document as a "status tracker".

## Check additional automated tests
- [ ] Verify that the latest E2E tests are fine (See "E2E *" workflows on https://github.com/cardano-foundation/cardano-wallet/actions).
- [ ] Verify latest [buildkite nightly](https://buildkite.com/cardano-foundation/cardano-wallet-nightly) and make sure the results are fine. [Benchmark charts](http://ec2-44-197-192-237.compute-1.amazonaws.com:5555) may be helpful in analysis.

## Prepare the release
- [ ] Make sure `cardano-wallet` points to correct revisions of
  dependent low-level libs in [`cabal.project`](https://github.com/cardano-foundation/cardano-wallet/blob/master/cabal.project). Use `cardano-node` as guidance.

  - [ ] Verify that target repositories point to appopriate revisions for `persistent`, `cardano-addresses`, `bech32`, ...)

  - [ ] If you have updated `cabala.project`, execute
  ```
  ./nix/regenerate.sh --cache=/dev/null
  # This will fetch and update the sha256 hashes.
  ```

- [ ] Fetch the tip of `master`:

  ```shell
  > git checkout master
  > git pull
  ```

- [ ] Create a new branch for the release:

  ```shell
  > git checkout -b your-name/bump-release/YYYY-MM-DD
  ```

- [ ] From the **root** of the repository, run:

  ```shell
  > ./scripts/make_release.sh
  ```

  This will bump the version in `.cabal` and `.nix` files, and the
  swagger spec files, and generate release notes.

  ```admonish
   If you get GitHub API rate limit errors, you can
   set the `GITHUB_API_TOKEN` environment variable. To create a
   _personal access token_, go to your
   [Github Settings](https://github.com/settings/tokens).
   No scope is required for this token, only public access (as it
   is simply used to read publicly available data from the Github
   API).
  ```

- [ ] Verify that the script modified the compatibility matrix in README.md correctly.

- [ ] Open a pull request to submit the modified files

- [ ] Get it merged

- [ ] Trigger a release build on CI (GitHub Actions) and wait for the
  build artifacts to be published on the GitHub release page.

    ```shell
    > git checkout master
    > git pull
    > git tag --sign -m "vYYYY-MM-DD" vYYYY-MM-DD
    > git push origin vYYYY-MM-DD
    ```

  Where `YYYY-MM-DD` should be replaced by the actual date of the release.


## Create the release notes

- [ ] Write release notes in the
  [release page](https://github.com/cardano-foundation/cardano-wallet/releases)
  based on [Next Release Notes](https://docs.google.com/document/d/1jMHvrtZ36_diLzPsAPzu88YvrX8z9YthZ7Xw_-7nanE/edit#heading=h.1ulc1nu4qh1i)


## Verify release artifacts

- [ ] Verify that the documentations have been correctly exported on
  [gh-pages](https://github.com/cardano-foundation/cardano-wallet/tree/gh-pages)

  - [ ] Make a commit with redirects to the documentation for the release like [this one](https://github.com/cardano-foundation/cardano-wallet/commit/3abd8d3fe86bcf279d91d5745b7360892fad1cd4).
   ```
    > git checkout gh-pages
    > git pull origin gh-pages
    > cd releases
    > ./make_redirects.sh vYYYY-MM-DD
    ## push changes to gh-pages after
   ```

- [ ] Make sure the [Command-Line Interface](../user-guide/cli.md) manual is up to date.


## Manual ad-hoc verifications

- [ ] Execute all [manual scenarios](https://github.com/cardano-foundation/cardano-wallet/tree/master/test/manual) on the binaries to be released.

- [ ] Verify that sensitive fields listed in [Cardano/Wallet/Api/Server](https://github.com/cardano-foundation/cardano-wallet/blob/master/lib/wallet/api/http/Cardano/Wallet/Api/Http/Shelley/Server.hs#L695) are still accurate and aren't missing any new ones.
  ```
  sensitive =
      [ "passphrase"
      , "old_passphrase"
      , "new_passphrase"
      , "mnemonic_sentence"
      , "mnemonic_second_factor"
      ]
  ```

## Publication

- [ ] Once everyone has signed off (i.e. Tech lead, QA & Release manager), publish the release draft.

- [ ] If there are any changes to the release checklist update this document.
