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
  <a href="https://github.com/input-output-hk/cardano-wallet/actions?query=workflow%3Awindows"><img src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-wallet/windows?label=Windows&style=for-the-badge" /></a>
  <a href="https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs"><img src="https://img.shields.io/endpoint?style=for-the-badge&url=https%3A%2F%2Fhydra.iohk.io%2Fjob%2FCardano%2Fcardano-wallet%2Frequired%2Fshield" /></a>
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

See also [Docker](https://input-output-hk.github.io/cardano-wallet/user-guide/Docker) for more information about using docker.

NixOS users can also use the [NixOS service](https://input-output-hk.github.io/cardano-wallet/user-guide/NixOS).

## How to install (Linux / Windows / Mac OS)

See **Installation Instructions** for each available [release](https://github.com/input-output-hk/cardano-wallet/releases).

> ### Latest releases
>
> | cardano-wallet | cardano-node (compatible versions) | SMASH (compatible versions)
> | --- | --- | ---
> | `master` branch | [1.34.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.34.0) | [1.6.1](https://github.com/input-output-hk/smash/releases/tag/1.6.1)
> | [v2022-01-18](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2022-01-18) | [1.33.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.33.0) | [1.6.1](https://github.com/input-output-hk/smash/releases/tag/1.6.1)
> | [v2021-12-15](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2021-12-15) | [1.32.1](https://github.com/input-output-hk/cardano-node/releases/tag/1.32.1) | [1.6.1](https://github.com/input-output-hk/smash/releases/tag/1.6.1)
> | [v2021-11-11](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2021-11-11) | [1.30.1](https://github.com/input-output-hk/cardano-node/releases/tag/1.30.1) | [1.6.1](https://github.com/input-output-hk/smash/releases/tag/1.6.1)

## How to build from sources

See [Building](https://input-output-hk.github.io/cardano-wallet/developers/Building)

## How to test

See [Testing](https://input-output-hk.github.io/cardano-wallet/contributing/Testing)

## Documentation

| Link                                                                                               | Audience                                                     |
| ---                                                                                                | ---                                                          |
| [User Guide](https://input-output-hk.github.io/cardano-wallet/user-guide) | Users of Cardano Wallet                              |
| [CLI Manual](https://input-output-hk.github.io/cardano-wallet/user-guide/cli) | Users of the Cardano Wallet API                              |
| [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/edge)                     | Users of the Cardano Wallet API                              |
| [Cardano Wallet Documentation](https://input-output-hk.github.io/cardano-wallet/)                                     | Anyone interested in the project and our development process |
| [Adrestia Documentation](https://input-output-hk.github.io/adrestia/)                                     | Anyone interested in the project and our development process |

<hr/>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
</p>
