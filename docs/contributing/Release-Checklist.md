# Release Checklist

## Preparing the release

- Fetch the tip of `master`:

  ```shell
  $ git checkout master
  $ git pull
  ```

- Make sure `cardano-wallet` points to correct revisions of dependencies in
  [`stack.yaml`](https://github.com/input-output-hk/cardano-wallet/blob/master/stack.yaml) and
  [`cabal.project`](https://github.com/input-output-hk/cardano-wallet/blob/master/cabal.project).

  Verify that the content of [`stack.yaml`][] and [`cabal.project`][] correspond to the `cardano-node` version.

  Verify that target repositories point to appropriate revisions for `persistent`, `cardano-addresses`, `bech32`, ...)

- Create a new branch for the release:

  ```shell
  $ git checkout -b your-name/vYYYY-MM-DD/bump-release
  ```

- From the **root** of the repository, run:

  ```shell
  $ ./scripts/make_release.sh
  ```

  This will bump the version in `.cabal` and `.nix` files, and the
  swagger spec files, and generate release notes.

  > :bulb: Note: If you get GitHub API rate limit errors, you can
  > set the `GITHUB_API_TOKEN` environment variable. To create a
  > _personal access token_, go to your
  > [Github Settings](https://github.com/settings/tokens).
  > No scope is required for this token, only public access (as it
  > is simply used to read publicly available data from the Github
  > API).

- Verify that the script modified the compatibility matrix in README.md correctly.

- Open a pull request to submit the modified files.

- Get it merged.

- Create and push a signed release tag on the `HEAD` of `master`.

  ```shell
  $ git tag -s -m vYYYY-MM-DD vYYYY-MM-DD
  $ git push origin refs/tags/vYYYY-MM-DD
  ```

  Where `YYYY-MM-DD` should be replaced by the actual date of the release.

  This will trigger a release build on CI (GitHub Actions).

- Wait for the release build to finish and then check that
  build artifacts have been published on the GitHub release page.

## Create the release notes

- Write release notes in the
  [release page](https://github.com/input-output-hk/cardano-wallet/releases)
  using the previously generated release notes. Fill in the empty
  sections.

- Remove items that are irrelevant to users (e.g. pure
  refactoring, improved testing)

- Make sure the items that the script put in the "Unclassified"
  section are moved to an appropriate section (or removed).

- You may want to polish the language of the PR titles to make it
  sound like actual release notes.


## Verify release artifacts

- Verify that the documentations have been correctly exported on
  [gh-pages](https://github.com/input-output-hk/cardano-wallet/tree/gh-pages)

- Make sure the [[cli]] manual is up to date.


## Manual ad-hoc verifications

- Execute all [manual scenarios](https://github.com/input-output-hk/cardano-wallet/tree/master/test/manual) on the binaries to be released.

- Verify that sensitive fields listed in [Cardano/Wallet/Api/Server](https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core/src/Cardano/Wallet/Api/Server.hs#L409) are still accurate and aren't missing any new ones.
  ```
  sensitive =
      [ "passphrase"
      , "old_passphrase"
      , "new_passphrase"
      , "mnemonic_sentence"
      , "mnemonic_second_factor"
      ]
  ```

- Verify latest [buildkite nightly](https://buildkite.com/input-output-hk/cardano-wallet-nightly) and make sure the results are fine. [Benchmark charts](http://cardano-wallet-benchmarks.herokuapp.com/) may be helpful in analysis.

## Publication

- Once everyone has signed off (i.e. Tech lead, QA & Release manager), publish the release draft.

- Add the release to the [automated migration tests](https://github.com/input-output-hk/cardano-wallet/blob/master/nix/migration-tests.nix#L44-L61) (keep only the last 10 versions). See the header of the file as to how to generate the SHA256 hashes.
