---
order: 1
title: How to create a wallet
---

{{<hint warning>}}
**Difficulty:** beginner

**Requires:**
- 📦 cardano-wallet >= `v2020-03-11`
{{</hint>}}

The easiest and most common way of managing your funds on the Cardano blockchain is through a [[hierarchical-deterministic-wallets]]. One can create a wallet using the following endpoint of [[http-api]]:

[`POST /byron-wallets`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postByronWallet)

There are several wallet types available:
 - random
 - icarus
 - trezor
 - ledger

The basic difference between them is that for a `random` wallet user needs to [[how-to-create-addresses|create new address]] manually, whereas for sequential wallets like `icarus`, `trezor` and `ledger` addresses are [[how-to-create-addresses#listing-addresses-in-sequential-wallets|generated automatically]]) by the wallet.

{{<hint danger>}}
Please note that `random` wallets are considered **deprecated** and should not be used by new applications.
{{</hint>}}

Note also that you can have many wallets being operated by a single `cardano-wallet` server.

See more on [[hierarchical-deterministic-wallets]] and [[byron-address-format]].
