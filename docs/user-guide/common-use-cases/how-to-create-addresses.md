---
order: 3
title: How to "create" addresses
---

::: {.highlight-block}
**Difficulty:** beginner

**Requires:**
- 📦 cardano-wallet >= `v2020-04-01`
:::

Once you have a wallet you can manage your funds. In order to receive a transaction you need to provide an address associated with your wallet to the sender.

## Sequential wallets (Icarus & Shelley)

Since Icarus, wallets use sequential derivation which must satisfy very specific rules: a wallet is not allowed to use addresses beyond a certain limit before previously generated addresses have been used. This means that, at a given point in a time, a wallet has both a minimum and a maximum number of possible unused addresses. By default, the maximum number of consecutive unused addresses is set to `20`.

Therefore, address management is entirely done by the server and users aren't allowed to fiddle with them. The list of available addresses can be fetched from the server at any time via:

[`GET /byron-wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listByronAddresses)

This list automatically expands when new addresses become available so that there's always `address_pool_gap` consecutive unused addresses available (where `address_pool_gap` can be configured when a wallet is first restored / created).

::: {.highlight-block}
Alternatively, this endpoint can also be reached from the command-line:

```
$ cardano-wallet address list WALLET_ID
```
:::

## Random wallets (Legacy Byron)

Address creation is only allowed for wallets using random derivation. These are the legacy wallets from _cardano-sl_.

For `random` wallets user needs to invoke the following wallet endpoint to create new addresses:

[`POST /byron-wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/createAddress)

In order to list existing addresses another endpoint can be used.

[`GET /byron-wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listByronAddresses)


::: {.highlight-block}
Alternatively, these endpoints can also be reached from the command-line:

```
$ cardano-wallet address create WALLET_ID
$ cardano-wallet address list WALLET_ID
```
:::
