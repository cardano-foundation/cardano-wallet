<p align="center">
  <big><strong>Cardano Wallet</strong></big>
</p>

<p align="center">
  <img width="200" src=".github/images/cardano-logo.png"/>
</p>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/releases"><img src="https://img.shields.io/github/release-pre/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
  <a href="https://travis-ci.org/input-output-hk/cardano-wallet"><img src="https://img.shields.io/travis/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
  <a href="https://coveralls.io/github/input-output-hk/cardano-wallet"><img src="https://img.shields.io/coveralls/github/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
</p>

<hr/>

Cardano Wallet helps you manage your Ada. You can use it to send and
receive payments on the [Cardano](https://www.cardano.org) blockchain.

This project provides an HTTP Application Programming Interface (API)
and command-line interface (CLI) for working with your wallet.

It can be used as a component of a frontend such as
[Daedalus](https://daedaluswallet.io), which provides a friendly user
interface for wallets. Most users who would like to use Cardano should
start with Daedalus.

## Development

This source code repository contains the next major version of Cardano
Wallet, which has been completely rewritten for the
[Shelley](https://cardanoroadmap.com/) phase.

The Byron version of Cardano Wallet is in the
[cardano-sl](https://github.com/input-output-hk/cardano-sl)
repository.

## How to build

Use [Haskell Stack](https://haskellstack.org/) to build this project:

```
$ stack build --test --no-run-tests
```

## How to test

#### unit

```
$ stack test cardano-wallet:unit
```

#### integration

##### pre-requisites

1. Install our fork of [cardano-http-bridge](https://github.com/KtorZ/cardano-http-bridge)

```
$ cargo install --branch cardano-wallet-integration --git https://github.com/KtorZ/cardano-http-bridge.git
```

2. Install [cardano-sl@cardano-node-simple](https://github.com/input-output-hk/cardano-sl)

```
$ git clone git@github.com:input-output-hk/cardano-sl.git
$ cd cardano-sl
$ stack install cardano-sl-node:exe:cardano-node-simple
```

Alternatively, if you're running on linux, you may use a pre-compiled version:

```
$ curl -L -o cardano-node-simple-3.0.1.tar.gz https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/test/data/cardano-node-simple/cardano-node-simple-3.0.1.tar.gz
$ tar xzf cardano-node-simple-3.0.1.tar.gz -C /usr/local/bin && rm cardano-node-simple-3.0.1.tar.gz
```

3. Import the initial testnet chain bootstrap for the `cardano-http-bridge`

```
$ curl -L -o hermes-testnet.tar.gz https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/test/data/cardano-http-bridge/hermes-testnet.tar.gz
$ tar xzf hermes-testnet.tar.gz -C $HOME && rm hermes-testnet.tar.gz
```

##### test

```
$ stack test cardano-wallet:integration
```


## Documentation

 * Users of the Cardano Wallet API can refer to the [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/).
 * Development-related information can be found in the [Wiki](https://github.com/input-output-hk/cardano-wallet/wiki).
 * To help understand the source code, refer to the [Haddock Documentation](https://input-output-hk.github.io/cardano-wallet/haddock/).


<hr/>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
</p>
