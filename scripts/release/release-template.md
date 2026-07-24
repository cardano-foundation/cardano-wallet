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

$API_CHANGES

## Known Issues

* (ADP-2635) Database connections do not seem to gracefully terminate on stopping the wallet.
* (ADP-2298) `Deposit_returned` is falsely reported on some incoming transactions (intermittently).

## Signatures

| Name                                | Role              | Approval |
| ----------------------------------- | ----------------- | -------- |
| Paolo Veronelli @paolino            | Software Engineer |          |
| Pawel Jakubas @paweljakubas         | Software Engineer |          |
