<p align="center">
  <big><strong>Cardano Wallet</strong></big>
</p>

<p align="center">
  <img width="200" src=".github/images/cardano-logo.png"/>
</p>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/releases"><img src="https://img.shields.io/github/release-pre/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
  <a href="https://buildkite.com/input-output-hk/cardano-wallet"><img src="https://img.shields.io/buildkite/7ea3dac7a16f066d8dfc8f426a9a9f7a2131e899cd96c444cf/master?label=BUILD&style=for-the-badge"/></a>
  <a href="https://buildkite.com/input-output-hk/cardano-wallet-nightly"><img src="https://img.shields.io/buildkite/59ea9363b8526e867005ca8839db47715bc5f661f36e490143/master?label=BENCHMARK&style=for-the-badge" /></a>
  <a href="https://travis-ci.org/input-output-hk/cardano-wallet"><img src="https://img.shields.io/travis/input-output-hk/cardano-wallet.svg?label=DOCS&style=for-the-badge" /></a>
  <a href="https://github.com/input-output-hk/cardano-wallet/actions?query=workflow%3A%22cardano-wallet+Windows+Tests%22"><img src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-wallet/cardano-wallet%20Windows%20Tests?label=Windows&style=for-the-badge" /></a>

  <!--
  <a href="https://coveralls.io/github/input-output-hk/cardano-wallet?branch=HEAD"><img src="https://img.shields.io/coveralls/github/input-output-hk/cardano-wallet/HEAD?style=for-the-badge" /></a>
  -->
</p>

<hr/>

## Overview

Cardano Wallet helps you manage your Ada. You can use it to send and
receive payments on the [Cardano](https://www.cardano.org) blockchain.

This project provides an HTTP Application Programming Interface (API)
and command-line interface (CLI) for working with your wallet.

It can be used as a component of a frontend such as
[Daedalus](https://daedaluswallet.io), which provides a friendly user
interface for wallets. Most users who would like to use Cardano should
start with Daedalus.

> :information_source: This source code repository contains the next major version of Cardano
> Wallet, which has been completely rewritten for the
> [Shelley](https://roadmap.cardano.org/) phase.
>
> :bulb: The Byron version of Cardano Wallet is in the
> [cardano-sl](https://github.com/input-output-hk/cardano-sl)
> repository.

## Getting Started

```
wget https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/docker-compose.yml
NETWORK=testnet docker-compose up
```

Fantastic! The server is up-and-running, waiting for HTTP requests on `localhost:8090/v2` e.g.:

```
curl http://localhost:8090/v2/network/information
```

or to be accessed via CLI, e.g.:

```
docker run --network host --rm inputoutput/cardano-wallet network information
```

See also [Wiki - Docker](https://github.com/input-output-hk/cardano-wallet/wiki/Docker) for more information about using docker.

## How to install (Linux / Windows / Mac OS)

See **Installation Instructions** for each available [release](https://github.com/input-output-hk/cardano-wallet/releases).

> ### Latest releases
>
> | cardano-wallet | cardano-node (compatible versions) | SMASH (compatible versions)
> | --- | --- | ---
> | `master` branch | [1.24.2](https://github.com/input-output-hk/cardano-node/releases/tag/1.24.2) | [1.2.0](https://github.com/input-output-hk/smash/releases/tag/1.2.0)
> | [v2021-01-12](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2021-01-12) | [1.24.2](https://github.com/input-output-hk/cardano-node/releases/tag/1.24.2) | [1.2.0](https://github.com/input-output-hk/smash/releases/tag/1.2.0)
> | [v2020-12-21](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-12-21) | [1.24.2](https://github.com/input-output-hk/cardano-node/releases/tag/1.24.2) | [1.2.0](https://github.com/input-output-hk/smash/releases/tag/1.2.0)
> | [v2020-12-08](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-12-08) | [1.24.2](https://github.com/input-output-hk/cardano-node/releases/tag/1.24.2) | [1.2.0](https://github.com/input-output-hk/smash/releases/tag/1.2.0)

## How to build from sources

See [Wiki - Building](https://github.com/input-output-hk/cardano-wallet/wiki/Building)

## How to test

See [Wiki - Testing](https://github.com/input-output-hk/cardano-wallet/wiki/Testing)

## Documentation

| Link                                                                                               | Audience                                                     |
| ---                                                                                                | ---                                                          |
| [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/edge)                     | Users of the Cardano Wallet API                              |
| [Wiki](https://github.com/input-output-hk/cardano-wallet/wiki)                                     | Anyone interested in the project and our development process |
| [CLI Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface) | Users of the Cardano Wallet API                              |

<hr/>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
</p>
