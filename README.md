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
    <a href="https://buildkite.com/cardano-foundation/cardano-wallet-release">
        <img src="https://img.shields.io/buildkite/782570d86cfcb9cdc2f5fc5048a770777ec980bcfc135f5fe6/master?label=RELEASE-CANDIDATE&style=for-the-badge"  />
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/windows.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/windows.yml?label=Windows unit tests&style=for-the-badge&branch=master"  />
    </a>
    <a href="https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-windows.yml">
        <img src="https://img.shields.io/github/actions/workflow/status/cardano-foundation/cardano-wallet/e2e-windows.yml?label=E2E Windows&style=for-the-badge&branch=master" />
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

The `cardano-wallet` executable is an HTTP server that manages your wallet(s).



### Docker

Select the network you want to run the wallet on, `private`, `sanchonet`, `preprod` or `mainnet`
by changing the working directory to `run/` + network + `/docker`.

Then
- start a wallet with `./run.sh start`
- stop a wallet with `./run.sh stop`
- inspect the logs with `./run.sh logs`
- run a simple application that fully synchronize a node and then stop it with `./run.sh sync`

### Variables
Accepeted variables for the start command are:
- `WALLET_PORT` (default 8090): the port the wallet will listen to on your host, in case of absence a random port will be used
- `WALLET_DB` (default `./databases/wallet-db`): the directory where the wallet database will be stored
- `NODE_DB` (default `./databases/node-db`): the directory where the node database will be stored
- `NODE_SOCKET_DIR` (default `./.`): the directory where the node socket will be created
- `NODE_CONFIGS` (default `./configs`): the directory where the node configuration files will be retrieved from
- `WALLET_TAG` (default 2024.7.7): the tag of the wallet image to use, can be `release-candidate`

For example, to start a wallet on `private` network:

```bash
cd run/private/docker
WALLET_PORT=8090 ./run.sh start
curl http://localhost:8090/v2/network/information | jq
```

Then you can inspect the logs with

```bash
./run.sh logs
```

And stop the services with

```bash
./run.sh stop
```

See also [Docker](https://cardano-foundation.github.io/cardano-wallet/user-guide/installation/use-docker.html) for more information about using docker.

### .env file

You can set your variables populating the `.env` file in your working directory.

```bash

WALLET_PORT=8090
WALLET_DB=./my-databases/wallet-db
NODE_DB=./my-databases/node-db
NODE_SOCKET_DIR=/tmp/cardano-node-socket
NODE_CONFIGS=./my-configs-i-just-copied-from-a-malicious-site
WALLET_TAG=2024.7.7

```


### Nix

You can opt to **nix** by changing the working directory to `run/` + network + `/nix` .

The nix script serve only as a template, you can modify it to suit your needs.
It actually support one only command `sync` that starts a wallet and a node and
use the wallet api to wait for the node to sync, then it stops the wallet and the node.

For example, to sync a node on `private` using nix:

```bash
cd run/private/nix
./run.sh sync
```

`WALLET_TAG` is not supported in the nix script. You will run the code that you just cloned.

NixOS users can also use the [NixOS service](https://cardano-foundation.github.io/cardano-wallet/user-guide/installation/use-nixos.html).

### Running on mainnet

**Take care when running on mainnet, as the docker compose will expose the wallet port to the world**

**Prerequisites**:
    - 200GB of disk space: the history
    - 24GB of RAM: the utx set in the node process heap


## Obtaining `cardano-wallet`

### Executables (Linux / Windows / Mac OS)

We provide executables as part of our [releases](https://github.com/cardano-foundation/cardano-wallet/releases). Please also see the installation instructions highlighted in the release notes.

### Building from source

See [Building](https://cardano-foundation.github.io/cardano-wallet/contributor/what/building.html)

### Testing

See [Testing](https://cardano-foundation.github.io/cardano-wallet/contributor/how/testing.html)

## History

The `cardano-wallet` repository was introduced during the [Shelley phase](https://roadmap.cardano.org/) of the Cardano blockchain.
Previously, during the Byron phase, the wallet was part of the [cardano-sl](https://github.com/input-output-hk/cardano-sl) repository. (This is useful to know — sometimes the ghosts of the past come back to haunt us in the form of obscure bugs.)

## Documentation

| Link                                                                                             | Audience                                                     |
| ------------------------------------------------------------------------------------------------ | ------------------------------------------------------------ |
| [Documentation](https://cardano-foundation.github.io/cardano-wallet/)                            |                                                              |
| • [User Manual](https://cardano-foundation.github.io/cardano-wallet/user)                        | Users of Cardano Wallet                                      |
| &nbsp;&nbsp;⤷ [CLI Manual](https://cardano-foundation.github.io/cardano-wallet/user/cli)         | Users of the Cardano Wallet API                              |
| &nbsp;&nbsp;⤷ [API Documentation](https://cardano-foundation.github.io/cardano-wallet/api/edge)  | Users of the Cardano Wallet API                              |
| • [Design Documents](https://cardano-foundation.github.io/cardano-wallet/design)                 | Anyone interested in wallet design and specifications        |
| &nbsp;&nbsp;⤷ [Specifications](https://cardano-foundation.github.io/cardano-wallet/design/specs) | Anyone interested in wallet design and specifications        |
| • [Contributor Manual](https://cardano-foundation.github.io/cardano-wallet/contributor)          | Anyone interested in the project and our development process |
| [Adrestia Documentation](https://input-output-hk.github.io/adrestia/)                            | Anyone interested in the project and our development process |

<hr/>

<p align="center">
  <a href="https://github.com/cardano-foundation/cardano-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/cardano-foundation/cardano-wallet.svg?style=for-the-badge" /></a>
</p>
