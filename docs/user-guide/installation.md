---
weight: 3
title: Installation Instructions
---

## Using Docker (recommended)

Docker images are continuously built and deployed on [dockerhub](https://hub.docker.com/u/inputoutput) under specific tags. Using docker provides **the fastest** and **easiest** user experience for setting up the Cardano stack. You should prefer this solution over building from sources unless you have really good reasons not to. The following images are available for each component of the Adrestia architecture:

| Repository                                                           | Tags                          | Documentation               |
| :---                                                                 | :---:                         | :---:                       |
| [inputoutput/cardano-node][inputoutput-cardano-node]                 | `master`, `MAJOR.MINOR.PATCH` | [link][doc-cardano-node]    |
| [inputoutput/cardano-db-sync][inputoutput-cardano-db-sync]           | `master`, `MAJOR.MINOR.PATCH` | [link][doc-cardano-db-sync] |
| [inputoutput/cardano-graphql][inputoutput-cardano-graphql]           | `master`, `MAJOR.MINOR.PATCH` | [link][doc-cardano-graphql] |
| [inputoutput/cardano-explorer-api][inputoutput-cardano-explorer-api] | `master`, `MAJOR.MINOR.PATCH` | [link][doc-cardano-rest]    |
| [inputoutput/cardano-submit-api][inputoutput-cardano-submit-api]     | `master`, `MAJOR.MINOR.PATCH` | [link][doc-cardano-rest]    |
| [inputoutput/cardano-wallet][inputoutput-cardano-wallet]             | `byron`, `YYYY.MM.DD-byron`   | [link][doc-cardano-wallet]  |

Each `MAJOR.MINOR.PATCH` or `YYYY.MM.DD-byron` tag must match actual releases of the corresponding component. Refer to each component release notes to know which release tags are available. For example, in order to use `cardano-node@1.10.0`, one can simply run:

```
docker pull inputoutput/cardano-node:1.10.0
```

Similarly, one can pull `cardano-wallet@v2020-04-07` with:

```
docker pull inputoutput/cardano-wallet:v2020.4.7-byron
```

{{<hint info>}}
ℹ️  _About version compatibility_

For version compatibility between components, please refer to compatibility matrix on each component main page (e.g. [cardano-wallet#latest-releases](https://github.com/input-output-hk/cardano-wallet#latest-releases)).
{{</hint>}}

Some leaf components like `cardano-submit-api` also provide example setup via [docker-compose](https://docs.docker.com/compose/). Those are useful for a quick start and for development, and also gives a baseline for development. See for example [input-output-hk/cardano-rest](https://github.com/input-output-hk/cardano-rest/blob/master/docker-compose.yml).

## Pre-compiled Artifacts / Building From Sources

In case you prefer using raw binary instead, some components do provide pre-compiled release artifacts for each release. These can be downloaded directly from the github servers, via the UI or using a command-line tool like `wget` or `cURL`. For example, one can download a pre-packaged linux binary for `cardano-wallet@v2020-04-07` via:

```
curl -L https://github.com/input-output-hk/cardano-wallet/releases/download/v2020-04-07/cardano-wallet-v2020-04-07-linux64.tar.gz | tar xz 

./cardano-wallet-byron-linux64/cardano-wallet --help
The CLI is a proxy to the wallet server, which is required for most commands.
Commands are turned into corresponding API calls, and submitted to an
up-and-running server. Some commands do not require an active server and can be
run offline (e.g. 'mnemonic generate').

[...]
```



If you feel brave enough and want to compile everything from sources, please refer to each repository's documentation. As a pre-requisite, you may want to install and configure [Nix](https://nixos.org/), [stack](https://docs.haskellstack.org/en/stable/README/) or [cabal](https://www.haskell.org/cabal/) depending on your weapon of choice. Build instructions are available on each repository's main README.

Repository                           | Releases                            | Linux | MacOS | Windows |
---                                  | ---                                 | ---   | --    | ---     |
[cardano-node][cardano-node]         | [releases][release-cardano-node]    | ✔️     | ✔️     | ✔️       |
[cardano-db-sync][cardano-db-sync]   | [releases][release-cardano-db-sync] | ✔️     | ✔️     | ❌      |
[cardano-submit-api][cardano-rest]   | [releases][release-cardano-rest]    | ✔️     | ✔️     | ❌      |
[cardano-explorer-api][cardano-rest] | [releases][release-cardano-rest]    | ✔️     | ✔️     | ❌      |
[cardano-graphql][cardano-graphql]   | [releases][release-cardano-graphql] | ✔️     | ✔️     | ❌      |
[cardano-wallet][cardano-wallet]     | [releases][release-cardano-wallet]  | ✔️     | ✔️     | ✔️       |

[cardano-node]: https://github.com/input-output-hk/cardano-node
[cardano-db-sync]: https://github.com/input-output-hk/cardano-db-sync
[cardano-rest]: https://github.com/input-output-hk/cardano-rest
[cardano-graphql]: https://github.com/input-output-hk/cardano-graphql
[cardano-wallet]: https://github.com/input-output-hk/cardano-wallet

[release-cardano-node]: https://github.com/input-output-hk/cardano-node/releases
[release-cardano-db-sync]: https://github.com/input-output-hk/cardano-db-sync/releases
[release-cardano-rest]: https://github.com/input-output-hk/cardano-rest/releases
[release-cardano-graphql]: https://github.com/input-output-hk/cardano-graphql/releases
[release-cardano-wallet]: https://github.com/input-output-hk/cardano-wallet/releases

[cardano-node]: https://github.com/input-output-hk/cardano-node
[cardano-db-sync]: https://github.com/input-output-hk/cardano-db-sync
[cardano-explorer-api]: https://github.com/input-output-hk/cardano-rest
[cardano-submit-api]: https://github.com/input-output-hk/cardano-rest
[cardano-graphql]: https://github.com/input-output-hk/cardano-graphql
[cardano-wallet]: https://github.com/input-output-hk/cardano-wallet

[doc-cardano-node]: https://github.com/input-output-hk/cardano-node/blob/master/nix/docker.nix#L1-L25
[doc-cardano-db-sync]: https://github.com/input-output-hk/cardano-db-sync/blob/master/nix/docker.nix#L1-L35
[doc-cardano-rest]: https://github.com/input-output-hk/cardano-rest/wiki/Docker
[doc-cardano-graphql]: https://github.com/input-output-hk/cardano-graphql/wiki/Docker
[doc-cardano-wallet]: https://github.com/input-output-hk/cardano-wallet/wiki/Docker
[inputoutput-cardano-node]: https://hub.docker.com/r/inputoutput/cardano-node
[inputoutput-cardano-db-sync]: https://hub.docker.com/r/inputoutput/cardano-db-sync
[inputoutput-cardano-graphql]: https://hub.docker.com/r/inputoutput/cardano-graphql
[inputoutput-cardano-submit-api]: https://hub.docker.com/r/inputoutput/cardano-submit-api
[inputoutput-cardano-explorer-api]: https://hub.docker.com/r/inputoutput/cardano-explorer-api
[inputoutput-cardano-wallet]: https://hub.docker.com/r/inputoutput/cardano-wallet
