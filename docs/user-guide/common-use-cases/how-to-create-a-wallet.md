---
weight: 1
title: How to create a wallet
---

{{<tabs>}}

{{<tab "using cardano-wallet">}}

The easiest and most common way of managing your funds on Cardano blockchain is through the wallet.
One can create a wallet using the following endpoint of [cardano-wallet](https://github.com/input-output-hk/cardano-wallet) backend:

[`POST /byron-wallets`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postByronWallet)

There are several wallet types available:
 - random
 - icarus
 - trezor
 - ledger

The basic difference between them is that `random` wallet needs an explicit index to derive new address and therefore [creating new address]({{< ref "how-to-get-target-address-for-incoming-transaction.md" >}}) for it needs to be invoked by the end user. `Icarus`, `trezor` and `ledger` are sequential wallets, so addresses for them are [generated automatically]({{< ref "how-to-get-target-address-for-incoming-transaction.md#listing-addresses-in-sequential-wallets" >}}) by the wallet.

Note that you can have many wallets being operated by single `cardano-wallet` server.

See more on [HD wallets]({{< ref "hierarchical-deterministic-wallets.md" >}}) and [addresses]({{< ref "addresses-byron.md" >}}).
{{</tab>}}

{{</tabs>}}
