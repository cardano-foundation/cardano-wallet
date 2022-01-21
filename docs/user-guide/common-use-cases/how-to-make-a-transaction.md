---
order: 4
title: How to make a transaction
---

## New transaction
::: {.highlight-block}
**Difficulty:** beginner

**Requires:**
- 📦 cardano-wallet >= `v2020-04-01`
:::

Assuming you have already created a wallet, you can send a transaction by using the following endpoint:

[`POST /v2/byron-wallets/{walletId}/transactions`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postByronTransaction)

Behind the scene, the wallet engine will select necessary inputs from the wallet, generate a change address within the wallet, sign and submit the transaction. A transaction can have multiple outputs, possibly to the same address. Note that in Byron, addresses are necessarily base58-encoded (as an enforced convention).

Once submitted through the wallet, a can be tracked via:

[`GET /v2/byron-wallets/{walletId}/transactions`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listByronTransactions)

Which returns a list of all transactions for this particular wallet. Optional range filters can be provided. A transaction will go through a succession of states, starting as “Pending”. If a transaction stays pending for too long (because rejected by a mempool, or because lost in translation due to multiple chain switches), users may decide to forget it using:

[`DELETE /v2/byron-wallets/{walletId}/transactions/{transactionId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/deleteByronTransaction)

For more information about transactions lifecycle, have a look at [[About-Transactions-Lifecycle]].

## Signed and serialized transactions 
::: {.highlight-block}
**Difficulty:** advanced

**Requires:**
- 📦 cardano-transactions >= `1.0.0`
- 📦 cardano-submit-api >= `2.0.0` OR cardano-wallet >= `v2020-04-01`
:::

Alternatively, `cardano-wallet` and `cardano-submit-api` allows clients to submit already signed and serialized transactions as a raw bytes blob. This can be done by submitting such serialized data as an `application/octet-stream` to either of:

- cardano-wallet: [`POST   /v2/proxy/transactions`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postExternalTransaction)
- cardano-submit-api: [`POST    /api/submit/tx`](https://input-output-hk.github.io/cardano-rest/submit-api/#operation/postTransaction)

In this scenario, the server engine will verify that the transaction is structurally well-formed and forward it to its associated node. If the transaction belongs to a known wallet, it will eventually show up in the wallet your wallet.

Such transactions can be constructed from raw data using either [cardano-transactions library or command-line interface](https://github.com/input-output-hk/cardano-transactions). Examples and documentation excerpts are available on the corresponding Github repository.

