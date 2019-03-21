<p align="center">
  <big><strong>Cardano Wallet</strong></big>
</p>

<p align="center">
  <img width="200" src=".github/images/cardano-logo.png"/>
</p>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/releases"><img src="https://img.shields.io/github/release/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
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

    stack build --test


## Documentation

 * Users of the Cardano Wallet API can refer to the [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/).
 * Development-related information can be found in the [Wiki](https://github.com/input-output-hk/cardano-wallet/wiki).
 * To help understand the source code, refer to the [Haddock Documentation](https://input-output-hk.github.io/cardano-wallet/haddock/).


<hr/>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
</p>
