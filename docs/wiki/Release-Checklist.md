### Checklist to follow upon publishing the release

 - [ ] Make sure the [release page](https://github.com/input-output-hk/cardano-wallet/releases) for the particular release has all the relevant information: 
   - [ ] **Release notes** - follow the [RELEASE_TEMPLATE](https://github.com/input-output-hk/cardano-wallet/blob/master/.github/RELEASE_TEMPLATE.md)
   - [ ] **Changelog** - list of all the stories and corresponding PRs included in the release
 - [ ] Download the binaries and their checksum files from the [release page](https://github.com/input-output-hk/cardano-wallet/releases) and verify the checksums are correct (`sha256sum`). 
 - [ ] Execute all [manual scenarios](https://github.com/input-output-hk/cardano-wallet/tree/master/lib/core/test/manual/) on the binaries to be released.
 - [ ] Verify latest [buildkite nightly](https://buildkite.com/input-output-hk/cardano-wallet-nightly) and make sure the results are fine.
 - [ ] Verify that the documentations have been correctly exported on [gh-pages](https://github.com/input-output-hk/cardano-wallet/tree/gh-pages)
 - [ ] Verify that the [CLI manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface) is up-to-date / Update the [CLI manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface) 
 - [ ] Verify that sensitive fields listed in [Cardano/Wallet/Api/Server](https://github.com/input-output-hk/cardano-wallet/blob/master/lib/core/src/Cardano/Wallet/Api/Server.hs#L180-L187) are still accurate and aren't missing any new ones.