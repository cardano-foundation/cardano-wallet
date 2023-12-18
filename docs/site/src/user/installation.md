# Installation

There are a number of ways to obtain cardano-wallet.

## Daedalus installer

The `cardano-wallet` backend is included in the [Daedalus](https://daedaluswallet.io) installation. This is convenient if you already have Daedalus installed, but the version of `cardano-wallet` may not be the latest.

## Pre-built binaries from GitHub release page

Release builds of `cardano-wallet` for Linux, macOS, and Windows are available at:
https://github.com/cardano-foundation/cardano-wallet/releases

These release bundles include the recommended version of `cardano-node`, according to the [release matrix](https://github.com/cardano-foundation/cardano-wallet#latest-releases).

### Direct download URLS

The release packages can be downloaded directly from the github servers using a command-line tool like `wget` or `cURL`. For example, one can download and unpack a pre-packaged linux binary for `cardano-wallet@v2020-04-07` with:

```
> curl -L https://github.com/cardano-foundation/cardano-wallet/releases/download/v2020-04-07/cardano-wallet-v2020-04-07-linux64.tar.gz | tar xvz
```

## Docker

See [Docker](installation/use-docker.md).

## Nix/NixOS

See [NixOS](installation/use-nixos.md).

## Compile from source

See [Building](../developer-guide/building.md).

If you feel brave enough and want to compile everything from sources, please refer to
[Building](../developer-guide/building.md)
, or the equivalent documentation in each source repository (instructions often appear in `README.md`).

As a pre-requisite, you may want to install and configure [Nix](https://nixos.org/) or [cabal](https://www.haskell.org/cabal/), depending on your weapon of choice.

## Summary

| Repository                               | Releases                            | Linux | MacOS | Windows |
| ---------------------------------------- | ----------------------------------- | ----- | ----- | ------- |
| [cardano-node][cardano-node]             | [releases][release-cardano-node]    | ✔️     | ✔️     | ✔️       |
| [cardano-db-sync][cardano-db-sync]       | [releases][release-cardano-db-sync] | ✔️     | ✔️     | ❌       |
| [cardano-submit-api][cardano-submit-api] | [releases][release-cardano-node]    | ✔️     | ✔️     | ❌       |
| [cardano-graphql][cardano-graphql]       | [releases][release-cardano-graphql] | ✔️     | ✔️     | ❌       |
| [cardano-rosetta][cardano-rosetta]       | [releases][release-cardano-rosetta] | ✔️     | ✔️     | ❌       |
| [cardano-wallet][cardano-wallet]         | [releases][release-cardano-wallet]  | ✔️     | ✔️     | ✔️       |

[cardano-node]: https://github.com/IntersectMBO/cardano-node
[cardano-db-sync]: https://github.com/IntersectMBO/cardano-db-sync
[cardano-submit-api]: https://github.com/IntersectMBO/cardano-node/tree/master/cardano-submit-api
[cardano-rosetta]: https://github.com/input-output-hk/cardano-rosetta
[cardano-graphql]: https://github.com/cardano-foundation/cardano-graphql
[cardano-wallet]: https://github.com/cardano-foundation/cardano-wallet

[release-cardano-node]: https://github.com/IntersectMBO/cardano-node/releases
[release-cardano-db-sync]: https://github.com/IntersectMBO/cardano-db-sync/releases
[release-cardano-graphql]: https://github.com/cardano-foundation/cardano-graphql/releases
[release-cardano-rosetta]: https://github.com/input-output-hk/cardano-rosetta/releases
[release-cardano-wallet]: https://github.com/cardano-foundation/cardano-wallet/releases

[cardano-node]: https://github.com/IntersectMBO/cardano-node
[cardano-db-sync]: https://github.com/IntersectMBO/cardano-db-sync
[cardano-submit-api]: https://github.com/input-output-hk/cardano-rest

[doc-cardano-db-sync]: https://github.com/IntersectMBO/cardano-db-sync/blob/master/nix/docker.nix#L1-L35
[doc-cardano-rest]: https://github.com/input-output-hk/cardano-rest/wiki/Docker
[doc-cardano-graphql]: https://github.com/cardano-foundation/cardano-graphql/wiki/Docker
[doc-cardano-rosetta]: https://github.com/input-output-hk/cardano-rosetta
