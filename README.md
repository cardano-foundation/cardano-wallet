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
> [Shelley](https://cardanoroadmap.com/) phase.
>
> :bulb: The Byron version of Cardano Wallet is in the
> [cardano-sl](https://github.com/input-output-hk/cardano-sl)
> repository.

## How to install (Linux / Windows / Mac OS)

See **Installation Instructions** for each available [release](https://github.com/input-output-hk/cardano-wallet/releases).

> ### Latest releases
>
> | cardano-wallet                                                                            | jörmungandr (compatible versions)                                              | cardano-node (compatible versions)
> | ---                                                                                       | ---                                                                            | ---
> | `master` branch                                                                           | [v0.8.15](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.15) | [1.9.1](https://github.com/input-output-hk/cardano-node/releases/tag/1.9.1) [1.9.2](https://github.com/input-output-hk/cardano-node/releases/tag/1.9.2)
> | [v2020-03-16](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-03-16) | [v0.8.14](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.14) | [1.6.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.6.0)
> | [v2020-03-11](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-03-11) | [v0.8.13](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.13) | [1.6.0](https://github.com/input-output-hk/cardano-node/releases/tag/1.6.0)
> | [v2020-02-17](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-02-17) | [v0.8.9](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.9)   | N/A
> | [v2020-01-27](https://github.com/input-output-hk/cardano-wallet/releases/tag/v2020-01-27) | [v0.8.7](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.7)   | N/A

## How to build from sources

See [Wiki - Building](https://github.com/input-output-hk/cardano-wallet/wiki/Building)

## How to test

See [Wiki - Testing](https://github.com/input-output-hk/cardano-wallet/wiki/Testing)

## Documentation

| Link                                                                                               | Audience                                                     |
| ---                                                                                                | ---                                                          |
| [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/edge)                     | Users of the Cardano Wallet API                              |
| [Haddock Documentation](https://input-output-hk.github.io/cardano-wallet/haddock/edge)             | Haskell Developers using the `cardano-wallet` as a library   |
| [Wiki](https://github.com/input-output-hk/cardano-wallet/wiki)                                     | Anyone interested in the project and our development process |
| [CLI Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface) | Users of the Cardano Wallet API                              |

<hr/>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=for-the-badge" /></a>
</p>
