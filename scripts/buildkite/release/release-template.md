## Node Compatibility

Compatible with [`cardano-node@$NODE_TAG`](https://github.com/input-output-hk/cardano-node/releases/tag/$NODE_TAG).

## Docker Image

FIX THIS LINK BY INSPECTING DOCKERHUB !
[Image](https://hub.docker.com/layers/cardanofoundation/cardano-wallet/$CABAL-VERSION/images/$DOCKER_SHA)

## Changes

${CHANGES}

### Fixed

### Added

### Changed

### Removed

## API changes

FIX THIS LINK BY INSPECTING BUMP SH !
[bump.hs](https://bump.sh/hal-cardano-foundation/doc/cardano-wallet-backend/changes/$BUMP_CHANGES_ID)

## Known Issues

* (ADP-2953) Revision of `cardano-node` is not reported within version in release bundle binary for Windows.
* (ADP-2635) Database connections do not seem to gracefully terminate on stopping the wallet.
* (ADP-2298) `Deposit_returned` is falsely reported on some incoming transactions (intermittently).
* (ADP-1831) `cardano-wallet` version from docker image does not report revision.

## Signatures

| Name                                | Role              | Approval |
| ----------------------------------- | ----------------- | -------- |
| Heinrich Apfelmus @HeinrichApfelmus | Software Engineer |          |
| Paolo Veronelli @paolino            | Software Engineer |          |
| Pawel Jakubas @paweljakubas         | Software Engineer |          |
| Johannes Lund @Anviking             | Software Engineer |          |
| David Clark @david-a-clark          | Product Manager   |          |
