<p align="center">
  <big><strong>Cardano Wallet</strong></big>
</p>

<p align="center">
  <img width="200" src=".github/images/cardano-logo.png"/>
</p>

<p align="center">
    <a href="https://github.com/cardano-foundation/cardano-wallet/releases">
        <img src="https://img.shields.io/github/release-pre/cardano-foundation/cardano-wallet.svg?style=for-the-badge"  />
    </a>
    <a href="https://buildkite.com/cardano-foundation/cardano-wallet">
        <img src="https://img.shields.io/buildkite/da223f1dbf24e8a64a27f50a49190ce7a9ee867d221c20d70a/master?label=BUILD&style=for-the-badge"/>
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/publish.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/publish.yml?label=Docs&style=for-the-badge&branch=master"  />
    </a>
    <a href="https://buildkite.com/cardano-foundation/cardano-wallet-nightly">
        <img src="https://img.shields.io/buildkite/94de95cfe78b09c547cb109b0a44e6cd489341ea9e2c224ead/master?label=BENCHMARKS&style=for-the-badge"  />
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/windows.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/windows.yml?label=Windows unit tests&style=for-the-badge&branch=master"  />
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-docker.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/e2e-docker.yml?label=E2E Docker&style=for-the-badge&branch=master"  />
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-linux.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/e2e-linux.yml?label=E2E Linux&style=for-the-badge&branch=master"  />
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-macos.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/e2e-macos.yml?label=E2E MacOs&style=for-the-badge&branch=master"  />
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-windows.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/e2e-windows.yml?label=E2E Windows&style=for-the-badge&branch=master" />
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/docker_linux.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/docker_linux.yml?label=Docker-compose Linux&style=for-the-badge&branch=master"  />
    </a>
</p>


<hr/>

## Overview

Cardano Wallet is software that helps you manage your Ada. You can use it to send and receive payments on the [Cardano blockchain](https://www.cardano.org).

This project provides an HTTP Application Programming Interface (API)
and command-line interface (CLI) for working with your wallet.

It can be used as a component of a frontend such as
[Daedalus](https://daedaluswallet.io), which provides a friendly user
interface for wallets. Most users who would like to use Cardano should
start with Daedalus.

## Quickstart

The `cardano-wallet` executable is an HTTP server that manages your wallet(s). Here is one way to start the server using Docker:

```
wget https://raw.githubusercontent.com/cardano-foundation/cardano-wallet/master/docker-compose.yml
NETWORK=mainnet docker-compose up
```

Fantastic! The server is up-and-running, waiting for HTTP requests on `localhost:8090/v2` e.g.:

```
curl http://localhost:8090/v2/network/information
```

or to be accessed via CLI, e.g.:

```
docker run --network host --rm cardanofoundation/cardano-wallet network information
```

See also [Docker](https://cardano-foundation.github.io/cardano-wallet/user-guide/Docker) for more information about using docker.

NixOS users can also use the [NixOS service](https://cardano-foundation.github.io/cardano-wallet/user-guide/NixOS).

## Obtaining `cardano-wallet`

### Executables (Linux / Windows / Mac OS)

We provide executables as part of our [releases](https://github.com/cardano-foundation/cardano-wallet/releases). Please also see the installation instructions highlighted in the release notes.

> **Latest releases**
>
> | cardano-wallet | cardano-node (compatible versions) |
> | --- | --- |
> | [v2023-12-13](https://github.com/cardano-foundation/cardano-wallet/releases/tag/v2023-12-13) | [8.1.2](https://github.com/input-output-hk/cardano-node/releases/tag/8.1.2) |
> | [v2023-07-18](https://github.com/cardano-foundation/cardano-wallet/releases/tag/v2023-07-18) | [8.1.1](https://github.com/input-output-hk/cardano-node/releases/tag/8.1.1) |
> | [v2023-04-14](https://github.com/cardano-foundation/cardano-wallet/releases/tag/v2023-04-14) | [1.35.4](https://github.com/input-output-hk/cardano-node/releases/tag/1.35.4) |
> | [v2022-12-14](https://github.com/cardano-foundation/cardano-wallet/releases/tag/v2022-12-14) | [1.35.4](https://github.com/input-output-hk/cardano-node/releases/tag/1.35.4) |

### Building from source

See [Building](https://cardano-foundation.github.io/cardano-wallet/contributor/what/building.html)

### Testing

See [Testing](https://cardano-foundation.github.io/cardano-wallet/contributor/how/testing.html)

## History

The `cardano-wallet` repository was introduced during the [Shelley phase](https://roadmap.cardano.org/) of the Cardano blockchain.
Previously, during the Byron phase, the wallet was part of the [cardano-sl](https://github.com/input-output-hk/cardano-sl) repository. (This is useful to know — sometimes the ghosts of the past come back to haunt us in the form of obscure bugs.)

## Documentation

| Link                                                                                               | Audience                                                     |
| ---                                                                                                | ---                                                          |
| [Documentation](https://cardano-foundation.github.io/cardano-wallet/)                                     |  |
| • [User Manual](https://cardano-foundation.github.io/cardano-wallet/user) | Users of Cardano Wallet                              |
| &nbsp;&nbsp;⤷ [CLI Manual](https://cardano-foundation.github.io/cardano-wallet/user/cli) | Users of the Cardano Wallet API                              |
| &nbsp;&nbsp;⤷ [API Documentation](https://cardano-foundation.github.io/cardano-wallet/api/edge)                     | Users of the Cardano Wallet API                              |
| • [Design Documents](https://cardano-foundation.github.io/cardano-wallet/design)                     | Anyone interested in wallet design and specifications|
| &nbsp;&nbsp;⤷ [Specifications](https://cardano-foundation.github.io/cardano-wallet/design/specs)                     | Anyone interested in wallet design and specifications|
| • [Contributor Manual](https://cardano-foundation.github.io/cardano-wallet/contributor)                     | Anyone interested in the project and our development process |
| [Adrestia Documentation](https://input-output-hk.github.io/adrestia/)                                     | Anyone interested in the project and our development process |

<hr/>

<p align="center">
  <a href="https://github.com/cardano-foundation/cardano-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/cardano-foundation/cardano-wallet.svg?style=for-the-badge" /></a>
</p>
