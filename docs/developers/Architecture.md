---
page:
  headHtml: |
    <snippet var="js.mermaid" />
---

# Architecture Diagram

```mermaid
%%{init: {'theme': 'forest' } }%%
erDiagram
  CARDANO-NODE ||--|{ CARDANO-WALLET : depends-on
  SMASH ||--|{ CARDANO-WALLET: connects-to
  CARDANO-WALLET ||--|{ CARDANO-LAUNCHER : depends-on
  CARDANO-LAUNCHER ||--|{ DAEDALUS : depends-on
```

This is how the software components fit together in the [Daedalus][] scenario.

See also: [Adrestia Architecture][adrestia].

## Node

The core [cardano-node][], which participates in the Cardano network, and maintains the state of the Cardano blockchain ledger.

## Wallet Backend

[cardano-wallet][] An HTTP REST API is recommended for 3rd party wallets and small exchanges who do not want to manage UTxOs for transactions themselves. Use it to send and receive payments from hierarchical deterministic wallets on the Cardano blockchain via HTTP REST or a command-line interface.

## Cardano Launcher

[cardano-launcher][] is a TypeScript package which handles the details of starting and stopping the Node and Wallet Backend.

## Daedalus

[Daedalus][] is a user-friendly desktop application to manage Cardano wallets.

## SMASH

[SMASH][] is a proxy for stake pool metadata.

[adrestia]: https://input-output-hk.github.io/adrestia/code/Adrestia-Architecture
[cardano-node]: https://github.com/input-output-hk/cardano-node
[cardano-wallet]: https://github.com/input-output-hk/cardano-wallet
[cardano-launcher]: https://github.com/input-output-hk/cardano-launcher
[daedalus]: https://github.com/input-output-hk/daedalus
[SMASH]: https://github.com/input-output-hk/smash
