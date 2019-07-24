# Checklist to follow upon publishing the release

## Create the release

- [ ] Bump all package versions to the next version

  ```
  $ find . -name "*.cabal" ! -path "*.stack-work*" | xargs sed -i "s/<old-version>/<new-version>/"
  ```

- [ ] Tag and sign the release commit with a proper tag

  ```
  $ VERSION=<new-version> git tag -s -m $VERSION $VERSION
  ```

- [ ] Trigger a release build on CI and wait for the build artifacts to be published on github

  ```
  $ git push origin --tags
  ```

## Create the release notes

- [ ] Verify all PRs since the last release have a corresponding milestone (hint: we can filter PR by merge date on github using `merged:>yyyy-mm-dd` filter).

- [ ] List of all the stories and corresponding PRs included in the release. Here below is a small script to do this quickly using the Github API

  <details>
    <summary>make_changelog.sh</summary>

  ```
  # Automatically generate CHANGELOG from merged PRs
  #
  # Usage: make_changelog.sh <previous-release> <current-release>
  #
  # Examples:
  #
  #   make_changelog.sh 2019-07-02 2019-07-24

  API_TOKEN=<github-api-token>

  PULL_REQUESTS=$(curl -X GET \
    -H "Authorization: token $API_TOKEN" \
    -H "Accept: application/vnd.github.v3+json" \
    https://api.github.com/search/issues?q=repo:input-output-hk/cardano-wallet+is:pr+is:merged+merged:%3E$1+merged:%3C$2)

  PULL_REQUESTS=$(echo $PULL_REQUESTS | jq '.items | map({number:.number,title:.title,milestone:.milestone.title}) | group_by(.milestone)')
  PULL_REQUESTS=$(echo $PULL_REQUESTS | jq '.[] | map([.milestone,"#"+(.number | tostring),.title] | join(" | "))')

  printf "%s\n" "$PULL_REQUESTS" | sed 's/"//g'
  ```

  </details>

- [ ] Write release notes in the [release page](https://github.com/input-output-hk/cardano-wallet/releases), following the [RELEASE_TEMPLATE](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/RELEASE_TEMPLATE.md).



## Verify release artifacts

- [ ] Download the binaries and their checksum files from the [release page](https://github.com/input-output-hk/cardano-wallet/releases) and verify the checksums are correct (`sha256sum`).

- [ ] Verify that the documentations have been correctly exported on [gh-pages](https://github.com/input-output-hk/cardano-wallet/tree/gh-pages)

- [ ] Verify that the [CLI manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface) is up-to-date / Update the [CLI manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface)


## Manual ad-hoc verifications

- [ ] Execute all [manual scenarios](https://github.com/input-output-hk/cardano-wallet/tree/master/lib/core/test/manual/) on the binaries to be released.

- [ ] Verify that sensitive fields listed in [Cardano/Wallet/Api/Server](https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core/src/Cardano/Wallet/Api/Server.hs#L180-L187) are still accurate and aren't missing any new ones.

- [ ] Verify latest [buildkite nightly](https://buildkite.com/input-output-hk/cardano-wallet-nightly) and make sure the results are fine.