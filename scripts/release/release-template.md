## Node Compatibility

Compatible with [`cardano-node@$NODE_TAG`](https://github.com/IntersectMBO/cardano-node/releases/tag/$NODE_TAG).

This is the node version the wallet is built and tested against. The local state
query protocol is version-sensitive, so other node versions are not supported
even when the wallet starts and connects successfully.

<!-- If a specific node version is known NOT to work with this release, name it
here with the symptom. Delete this comment otherwise. -->

## Docker Image

```
$ docker pull cardanofoundation/cardano-wallet:${RELEASE_CABAL_VERSION}
$ docker run --rm cardanofoundation/cardano-wallet:${RELEASE_CABAL_VERSION} version
```

[All tags on Docker Hub](https://hub.docker.com/r/cardanofoundation/cardano-wallet/tags?name=${RELEASE_CABAL_VERSION})

## Changes

${CHANGES}

### Fixed

### Added

### Changed

### Removed

## API changes

$API_CHANGES

## Known Issues

<!-- Issues open at the time of releasing that a user could hit, one per line:

* ([#NNNN](https://github.com/cardano-foundation/cardano-wallet/issues/NNNN)) Short description of the symptom.

Check them against the issue tracker each release; a closed issue listed here
tells a user to expect a bug that no longer exists. Write "None." if there are
none. -->

## Signatures

| Name                                | Role              | Approval |
| ----------------------------------- | ----------------- | -------- |
| Paolo Veronelli @paolino            | Software Engineer |          |
| Pawel Jakubas @paweljakubas         | Software Engineer |          |
