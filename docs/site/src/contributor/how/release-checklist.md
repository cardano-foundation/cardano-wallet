> This is the latest release checklist template that one needs to go through in order to check quality of the release

## Prepare the release process

- [ ] Made a copy of the latest release
      [Release-vYYYY-MM-DD]()
      documented as a new page on cardano-wallet's wiki called [Release-vYYYY-MM-DD']()
      and substituted the version number in the title and the links.
- [ ] Picked up the latest green commgit from the release pipeline and prove it's green by linking the successful buildkite and github actions builds here.
    - [ ] [release pipeline build](https://buildkite.com/cardano-foundation/cardano-wallet-release/)
    - [ ] check for performance regressions on the `release commit` artifacts:
    - [ ] check that the [restoration benchmarks](https://buildkite.com/cardano-foundation/cardano-wallet-restoration-benchmarks) ran in
- [ ] Unblock the release block on [release pipeline build](https://buildkite.com/cardano-foundation/cardano-wallet-release/)


## Update the release page

- [ ] Fix the release note on the draft release automatically created by the release pipeline
    https://github.com/cardano-foundation/cardano-wallet/releases

## Check out the docker image is on the docker hub

- [ ] link: https://hub.docker.com/r/cardanofoundation/cardano-wallet/tags
- [ ] Report the link in the release notes
     [link to docker hub](https://hub.docker.com/layers/cardanofoundation/cardano-wallet/)

## Check out the bump.sh portal has the new release

- [ ] https://bump.sh/hal-cardano-foundation/doc/cardano-wallet-backend

## Check sensitive fields in the API

- [ ] Verify that sensitive fields listed in
  [Cardano/Wallet/Api/Server](https://github.com/cardano-foundation/cardano-wallet/blob/89faf170f388f9b475974896c349ad7676f0f44c/lib/exe/lib/Cardano/Wallet/Application/Server.hs#L128)
  are still accurate and aren't missing any new ones.
  ```
  sensitive =
      [ "passphrase"
      , "old_passphrase"
      , "new_passphrase"
      , "mnemonic_sentence"
      , "mnemonic_second_factor"
      ]
  ```
- [ ] Check out the documentation has been published by the [github action](https://github.com/cardano-foundation/cardano-wallet/actions/)

## Publication

- [ ] Once two engineers have signed off, publish the release draft.

- [ ] Merge the (administrative) commits created for the release tag back into the `master` branch.

- [ ] Remember to leave this checklist in an up-to-date status for the next releaser
