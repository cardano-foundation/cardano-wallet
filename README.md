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
</p>

<p align="center">

[![Contributors][contributors-shield]][contributors-url] [![Forks][forks-shield]][forks-url] [![Stargazers][stars-shield]][stars-url] [![Issues][issues-shield]][issues-url] [![MIT License][license-shield]][license-url]

</p>
<hr/>

# Cardano-Wallet

Cardano Wallet helps you manage your Ada. You can use it to send and receive payments on the [Cardano](https://www.cardano.org) blockchain.

This project provides an HTTP Application Programming Interface (API) and command-line interface (CLI) for working with your wallet.

It can be used as a component of a frontend such as [Daedalus](https://daedaluswallet.io), which provides a friendly user interface for wallets. Most users who would like to use Cardano should start with Daedalus.

> :information_source: This source code repository contains the next major version of Cardano Wallet, which has been completely rewritten for the [Shelley](https://cardanoroadmap.com/) phase.

> :bulb: The Byron version of Cardano Wallet is in the [cardano-sl](https://github.com/input-output-hk/cardano-sl) repository.

## Getting Started
There are a number of ways to use cardano-wallet. You can compile from the Haskell source code, run with Docker or down the compiled executables. You can also connect it to either the jörmungandr node or the cardano-node. Here we will cover how to get up an running for each particular method.

### Docker

Use the following commands to run the cardano-wallet and cardano-node at the same time using docker:

### Testnet
To start the docker containers for cardano-wallet and cardano-node:
```console
$ NETWORK=testnet TESTNETFILE=/app/config/testnet-genesis.yaml docker-compose up
```

Note that `TESTNETFILE` is required when running the above command, this is only for the `testnet` environment.

You can verify that it is running:

```console
$ curl http://localhost:8090/v2/network/information
{
  "network_tip": {
    "epoch_number": 57,
    "slot_number": 1929
  },
  "node_tip": {
    "height": {
      "quantity": 50829,
      "unit": "block"
    },
    "epoch_number": 2,
    "slot_number": 8659
  },
  "sync_progress": {
    "status": "syncing",
    "progress": {
      "quantity": 4.13,
      "unit": "percent"
    }
  },
  "next_epoch": {
    "epoch_start_time": "2020-05-09T20:20:16Z",
    "epoch_number": 58
  }
}
```
To stop the docker containers:
```console
$ docker-compose down
```

### Mainnet
To start the docker containers for cardano-wallet and cardano-node:
```console
$ NETWORK=mainnet docker-compose up
```
Check it is running by making a request.
```console
$ curl http://localhost:8090/v2/network/parameters/latest
{
  "slot_length": {
    "quantity": 20,
    "unit": "second"
  },
  "epoch_stability": {
    "quantity": 2160,
    "unit": "block"
  },
  "genesis_block_hash": "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb",
  "blockchain_start_time": "2017-09-23T21:44:51Z",
  "epoch_length": {
    "quantity": 21600,
    "unit": "slot"
  },
  "active_slot_coefficient": {
    "quantity": 100,
    "unit": "percent"
  }
}

```
To stop the docker containers:
```console
$ docker-compose down
```

### Switching from one environment to another
When switching between running testnet on docker to mainnet on docker, you should clear the volumes using the commands below or alter the docker-compose file to have persistent volumes mapped to your drive outside of the docker container.  
NB! It is imperative that you keep wallet recovery information basked-up outside of the docker volumes as you may choose to remove the volumes at some stage. Wallets should be restored via the API as needed. You will receive an ID back when you restore a wallet. You can also list the active wallets managed by cardano-wallet.

Use with caution!
```console
$ docker-compose down
$ docker volume rm cardano-wallet_node-ipc
$ docker volume rm cardano-wallet_node-db
```

#### Prerequisites
You will need to have Docker and Docker Compose installed to use this method to run cardano-wallet. Go to [https://docs.docker.com/get-docker/](https://docs.docker.com/get-docker/) to find out more. 

### Build from source-code

See [Wiki - Building](https://github.com/input-output-hk/cardano-wallet/wiki/Building)

#### Prerequisites

### Executables and releases (Linux / Windows / Mac OS)
Executables are built for each release and are available via Github Releases.  
Detailed **Installation Instructions** are available for each available [release](https://github.com/input-output-hk/cardano-wallet/releases).

> ### Latest releases

> cardano-wallet                                                                            | jörmungandr (compatible versions)                                              | cardano-node (compatible versions)
> ----------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ | -----------------------------------------------------------------------------
> `master` branch                                                                           | [v0.8.18](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.18) | [1.11.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.11.0)
> [v2020-04-07](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-04-07) | [v0.8.15](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.15) | [1.9.3](https://github.com/input-output-hk/cardano-node/releases/tag/1.9.3)
> [v2020-04-01](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-04-01) | [v0.8.15](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.15) | [1.9.3](https://github.com/input-output-hk/cardano-node/releases/tag/1.9.3)
> [v2020-03-16](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-03-16) | [v0.8.14](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.14) | [1.6.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.6.0)


## Documentation
This readme includes some basic documentation to get you started, however, detailed documentation is available via the following links:

Link                                                                                               | Audience
-------------------------------------------------------------------------------------------------- | ------------------------------------------------------------
[API Documentation](https://input-output-hk.github.io/cardano-wallet/api/edge)                     | Users of the Cardano Wallet API
[Haddock Documentation](https://input-output-hk.github.io/cardano-wallet/haddock/edge)             | Haskell Developers using the `cardano-wallet` as a library
[Wiki](https://github.com/input-output-hk/cardano-wallet/wiki)                                     | Anyone interested in the project and our development process
[CLI Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface) | Users of the Cardano Wallet API

--------------------------------------------------------------------------------

### Rest Endpoint Summary

- /wallets
- /wallets/{walletId}
- /wallets/{walletId}/tip
- /wallets/{walletId}/statistics/utxos
- /wallets/{walletId}/passphrase
- /wallets/{walletId}/transactions
- /wallets/{walletId}/payment-fees
- /wallets/{walletId}/transactions/{transactionId}
- /wallets/{walletId}/addresses
- /stake-pools
- /stake-pools/{stakePoolId}/wallets/{walletId}
- /stake-pools/\*/wallets/{walletId}
- /wallets/{walletId}/delegation-fees
- /wallets/{walletId}/coin-selections/random
- /byron-wallets
- /byron-wallets/{walletId}
- /byron-wallets/{walletId}/passphrase
- /byron-wallets/{walletId}/tip
- /byron-wallets/{walletId}/addresses
- /byron-wallets/{walletId}/transactions
- /byron-wallets/{walletId}/payment-fees
- /byron-wallets/{walletId}/transactions/{transactionId}
- /byron-wallets/{walletId}/migrations
- /byron-wallets/{sourceWalletId}/migrations/{targetWalletId}
- /byron-wallets/{walletId}/statistics/utxos
- /network/information
- /network/clock
- /network/parameters/{epochId}
- /proxy/transactions

### CLI Summary

```console
Usage: cardano-wallet COMMAND
  Cardano Wallet Command-Line Interface (CLI)

Available OPTIONS:
  -h | --help

Available COMMANDS:
  launch                Launch and monitor a wallet server and its chain producers.
  serve                 Serve an HTTP API that listens for commands/actions.
  mnemonic
    generate            Generate BIP-39 mnemonic words
    reward-credentials  Derive reward account private key from mnemonic.
  wallet
    list                List all known wallets
    create
      from-mnemonic     Create a new wallet using a mnemonic
      from-public-key   Create a wallet using a public account key
    get                 Fetch a particular wallet
    utxo                Get a wallet's UTxO distribution
    update
      passphrase        Update a wallet's master passphrase
      name              Update a wallet's name
    delete              Forget a wallet and its metadata
  transaction
    create              Create a transaction from a known wallet
    fees                Estimate fees for a transaction
    list                List the transactions associated with a wallet
    submit              Submit an externally-signed transaction
    forget              Forget a pending transaction with specified id
  address
    list                List all known addresses of a wallet
  stake-pool
      list              List all known stake pools
  network
    information         View network information
    parameters          View network parameters
    clock               View NTP offset
  key
    root                Extract root extended private key from a mnemonic sentence.
    child               Derive child keys.
    public              Extract public key from a private key.
    inspect             Show information about a key.
  version               Show the program's current version
```

## Testing

See [Wiki - Testing](https://github.com/input-output-hk/cardano-wallet/wiki/Testing)


## Examples

### Create a new Address from mnemonic

Generate BIP-39 mnemonic words using the CLI via docker exec.

```console
$ docker exec cardano-wallet_cardano-wallet_1 cardano-wallet-byron mnemonic generate
effort wink follow warm utility brush series rubber hawk antenna squirrel mystery wise village palace
```

Note that in the above example "cardano-wallet_cardano-wallet_1" is the name of the running container. If this is not the container name for you, use `docker ps` to find the container name for cardano-wallet and replace in the example.

### Restore mnemonic to wallet

```console
$ curl -H "Content-Type: application/json" -d '{"name":"BobsTestWallet","mnemonic_sentence":["effort","wink","follow","warm","utility","brush","series","rubber","hawk","antenna","squirrel","mystery","wise","village","palace"],"passphrase":"MyVerySecurePassphrase","style":"ledger"}' -X POST http://localhost:8090/v2/byron-wallets
{
  "passphrase": {
    "last_updated_at": "2020-05-06T07:19:29.657398038Z"
  },
  "state": {
    "status": "syncing",
    "progress": {
      "quantity": 0,
      "unit": "percent"
    }
  },
  "discovery": "sequential",
  "balance": {
    "total": {
      "quantity": 0,
      "unit": "lovelace"
    },
    "available": {
      "quantity": 0,
      "unit": "lovelace"
    }
  },
  "name": "BobsTestWallet",
  "id": "99e475b4133545753bba9d9a4631dfdf5efccf7b",
  "tip": {
    "height": {
      "quantity": 0,
      "unit": "block"
    },
    "epoch_number": 0,
    "slot_number": 0
  }
}
```

View wallet information:

```console
$ curl http://localhost:8090/v2/byron-wallets/99e475b4133545753bba9d9a4631dfdf5efccf7b |jq
{
  "passphrase": {
    "last_updated_at": "2020-05-06T07:19:29.657398038Z"
  },
  "state": {
    "status": "syncing",
    "progress": {
      "quantity": 30.97,
      "unit": "percent"
    }
  },
  "discovery": "sequential",
  "balance": {
    "total": {
      "quantity": 0,
      "unit": "lovelace"
    },
    "available": {
      "quantity": 0,
      "unit": "lovelace"
    }
  },
  "name": "BobsTestWallet",
  "id": "99e475b4133545753bba9d9a4631dfdf5efccf7b",
  "tip": {
    "height": {
      "quantity": 382982,
      "unit": "block"
    },
    "epoch_number": 17,
    "slot_number": 16826
  }
}
```

Get receive addresses for this wallet

```console
$ curl http://localhost:8090/v2/byron-wallets/99e475b4133545753bba9d9a4631dfdf5efccf7b/addresses |jq
[
  {
    "state": "unused",
    "id": "2cWKMJemoBajZ6SkG7njnmD6BiBJ32UtGoWEtkFaCvjuuxu6pP4NdZfwdop5W4R6e1QMg"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBahtjNkn4DNjv8tEAy8XcNxtngM9XZj5iNbAbLzhiznhpoYAgZjEPmXheEbb"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBakbeH6pGK23ekgCwAzEeiR1coixLoKbxHCA23fCUDCfiwPvPYRbwU8J1YAb"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBajCwqqqfuGDANqbve71fmM4vaprWHLpRcVGMV8pRnC4tzSfwbQtbDxhjh7Y"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBamCGgrh1Ju2od6pny1FVaUUENW4sBko8H49cnkDaW8iwB6WFnxX3DTtG1hf"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBahSF7NkcNetSzWxfy6AztwmFiBYE9F7NehxcNqjT3vZfpNHJ2DYK6GDbc4S"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBajpwEjmxBqKwNoE5BvjpYBP6CudcefjurR7Dc7Co3JPmmZn2heovZgsN6PG"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBakhKMC5p1MJZQLD7gvFRU8pWbe91L4fBnGs9Yunr5VGnWqE6AT9dcY8kY7M"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBahEYfTngtJ8bkAWdmQG5jYi8MJ5zN8QcA4TT8nujCDZ926vw3kQCnS94BQ4"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBakGFnE86NTvKPsrEd6q2uHTYWGemUh43wH2xLbUXHsyCmbdW1NsCZJU7L35"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBajQ6DYyj4Wmem9vnCtzQVj58Af6MX4kVbkePPC3oxuDGEPbBdtBrGgDtJtH"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBaiX5CKMp9rtrd7sF5C4jLH1A4hn4kMyZasNpoMhFZCTVBJVSaMSkyMmoR19"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBakTopJQmTQjtEutrjKnShceoponVGFEs95sJTDxk7u7doLVXP5zYASRiVmT"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBaj6x3NQSHsL9tsDHPV3ME4JToK12uzK1smVkt1ypKLNiaUEFV4gPvfFZGtD"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBakXM4jv5HMd2fcHaKx1SeYfvEubTkPURiAr65rBGEe3UGJ5XJbbRgPHq67j"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBair2XHYh9jEtJBhVr7y2Eu5osXSLcXJqccWZih6SaBRSLy12UNUbquo97LC"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBaiHCLZXP1Y41hky4tvktM4bdjCxN1rxJE6a7cdWszipnDh5TUkU2JB5ETxL"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBaj4PrUhMBtBY8A5VZYjZvFcxKqDD44CikMgcj7j4LqsEf6cxzQXAMq93v5E"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBakSdqP98ECHPSishg8eqMQh1gA1U9DjqEfpSq7spMV6U7gFYu6seZ3Y6Jri"
  },
  {
    "state": "unused",
    "id": "2cWKMJemoBainheMr6wREuG7HwjXQaBTA4GCS3GSLT3WW7k29KfCdxSgx1vfo53wpxqJE"
  }
]
```

<!-- MARKDOWN LINKS & IMAGES --> <!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->

[contributors-shield]: https://img.shields.io/github/contributors/input-output-hk/cardano-wallet.svg?style=flat-square
[contributors-url]: https://github.com/input-output-hk/cardano-wallet/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/input-output-hk/cardano-wallet.svg?style=flat-square
[forks-url]: https://github.com/input-output-hk/cardano-wallet/network/members
[issues-shield]: https://img.shields.io/github/issues/input-output-hk/cardano-wallet.svg?style=flat-square
[issues-url]: https://github.com/input-output-hk/cardano-wallet/issues
[license-shield]: https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=flat-square
[license-url]: https://github.com/input-output-hk/cardano-wallet/blob/master/LICENSE.txt
[stars-shield]: https://img.shields.io/github/stars/input-output-hk/cardano-wallet.svg?style=flat-square
[stars-url]: https://github.com/input-output-hk/cardano-wallet/stargazers



<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
</p>
