### Checklist to follow upon publishing the release

 - [ ] Make sure the [release page](https://github.com/input-output-hk/cardano-wallet/releases) for the particular release has all the relevant information: 
   - [ ] **Release notes** (including short Overview, Installation/Run instructions, Information about supported platforms) 
   - [ ] **Changelog** - list of all the stories and corresponding PRs included in the release
 - [ ] Download the binaries and their checksum files from the [release page](https://github.com/input-output-hk/cardano-wallet/releases) and verify the checksums are correct. 
 - [ ] Execute all [manual scenarios](https://github.com/input-output-hk/cardano-wallet/tree/master/lib/core/test/manual/) on the binaries to be released.
 - [ ] Verify latest [buildkite nightly](https://buildkite.com/input-output-hk/cardano-wallet-nightly) and make sure the results are fine.
 - [ ] Publish a pinned version of the API documentation to [gh-pages](https://github.com/input-output-hk/cardano-wallet/tree/gh-pages)
 - [ ] Publish a pinned version of the Haddock documentation to [gh-pages](https://github.com/input-output-hk/cardano-wallet/tree/gh-pages)
