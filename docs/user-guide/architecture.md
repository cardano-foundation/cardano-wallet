---
weight: 1
title: Architecture
---

## High-Level Diagram

{{< mermaid >}}
erDiagram
  CARDANO-NODE ||--|{ CARDANO-WALLET : depends-on
  CARDANO-NODE ||--|{ CARDANO-DB-SYNC : depends-on

  CARDANO-DB-SYNC ||--|{ SMASH : depends-on
  CARDANO-DB-SYNC ||--|{ CARDANO-GRAPHQL : depends-on
  CARDANO-DB-SYNC ||--|{ CARDANO-ROSETTA : depends-on

  CARDANO-GRAPHQL ||--|{ EXPLORER : depends-on

  SMASH ||--|{ CARDANO-WALLET: connects-to
  CARDANO-WALLET ||--|{ DAEDALUS : depends-on

{{< /mermaid >}}

## Components

### [cardano-node][cardano-node]

The core [cardano-node][cardano-node], which participates in the Cardano network, and maintains the state of the Cardano blockchain ledger.

{{<hint info>}}
Supported environments: Linux (64-bits), MacOS (64-bits), Windows (64-bits), Docker
{{< /hint >}}

### [cardano-wallet][cardano-wallet]

[cardano-wallet][cardano-wallet] An HTTP REST API is recommended for 3rd party wallets and small exchanges who do not want to manage UTxOs for transactions themselves. Use it to send and receive payments from hierarchical deterministic wallets on the Cardano blockchain via HTTP REST or a command-line interface.

{{<hint info>}}
Supported environments: Linux (64-bits), MacOS (64-bits), Windows (64-bits), Docker
{{< /hint >}}

### [cardano-db-sync][cardano-db-sync]

This application stores blockchain data fetched from [cardano-node][cardano-node] in a PostgreSQL database to enable higher-level interfaces for blockchain exploration. It powers [cardano-graphql][cardano-graphql].

{{<hint info>}}
Supported environments: Linux (64-bits), MacOS (64-bits), Docker
{{< /hint >}}

### [cardano-graphql][cardano-graphql]

A GraphQL API for Cardano, which also serves as the backend of
[Cardano Explorer](https://explorer.cardano.org/).

{{<hint info>}}
Supported environments: Linux (64-bits), MacOS (64-bits), Docker
{{< /hint >}}

### [cardano-submit-api][]

A small HTTP API for submitting transactions to a local [cardano-node][].

The transaction must be fully signed and CBOR-encoded. This could be done by [cardano-cli][], for example.

### [cardano-rest][cardano-rest]

[cardano-rest][] is DEPRECATED. The [explorer-api][cardano-rest] and [submit-api][cardano-rest] will cease to function at the time of Alonzo hard-fork.

The following tools replace [cardano-rest][]:
 - [cardano-graphql][]
 - [cardano-rosetta][]
 - [cardano-submit-api][] (part of the [cardano-node][] repository)
 - [cardano-wallet][]

Users with an existing integration to [cardano-rest][] are encouraged to look at the [Migration-Guide](https://input-output-hk.github.io/cardano-rest/migration-guide/).

## Choosing the right component

{{<mermaid>}}
graph TD
QMakeTx{Do you need to <br/> make transactions?}
QManageUTxO{Do you want to <br/>implement your own wallet?}
QAlreadyIntegrated{Do you already have<br/>an integration with<br/>cardano-sl?}

GraphQL{cardano-graphql}
Rest{cardano-rest}
SDK{SDK}
Wallet{cardano-wallet}

QMakeTx-->|yes| QManageUTxO
QMakeTx-->|no| QAlreadyIntegrated
QAlreadyIntegrated-->|yes| Rest
QAlreadyIntegrated-->|no| GraphQL
QManageUTxO-->|yes| SDK
QManageUTxO-->|no| Wallet
{{</mermaid>}}

## Notes

See also [input-output-hk/adrestia][adrestia].

[adrestia]: https://github.com/input-output-hk/adrestia
[cardano-graphql]: https://github.com/input-output-hk/cardano-graphql
[cardano-db-sync]: https://github.com/input-output-hk/cardano-db-sync
[cardano-node]: https://github.com/input-output-hk/cardano-node
[cardano-rest]: https://github.com/input-output-hk/cardano-rest
[cardano-sl-explorer]: https://cardanodocs.com/technical/explorer/api/
[cardano-wallet]: https://github.com/input-output-hk/cardano-wallet
[cardano-rosetta]: https://github.com/input-output-hk/cardano-rosetta
[cardano-submit-api]: https://github.com/input-output-hk/cardano-node/tree/master/cardano-submit-api
[cardano-cli]: https://docs.cardano.org/projects/cardano-node/en/latest/reference/cardano-node-cli-reference.html
