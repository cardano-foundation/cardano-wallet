---
order: 2
title: How to manage wallets
---

::: {.highlight-block}
**Difficulty:** beginner

**Requires:**
- 📦 cardano-wallet >= `v2020-04-01`
:::

Once you created a wallet you can manage it with `cardano-wallet` endpoints. There are several operations available.

### List all wallets
[`GET /v2/byron-wallets`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listByronWallets)

### Get specific wallet details
[`GET /v2/byron-wallets/{walletId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getByronWallet)

### Update wallet metadata
[`PUT /v2/byron-wallets/{walletId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/putByronWallet)

### Update wallet passphrase
[`PUT /v2/byron-wallets/{walletId}/passphrase`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/putByronWalletPassphrase)

### Get wallet UTxO
[`GET /v2/byron-wallets/{walletId}/statistics/utxos`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getByronUTxOsStatistics)

See more about [UTxO]({{< ref "utxo.md" >}}).

### Delete wallet
[`DELETE /v2/byron-wallets/{walletId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/deleteByronWallet)
