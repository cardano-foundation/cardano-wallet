---
weight: 3
title: How to get target address for incoming transaction
---

{{<hint warning>}}
**Difficulty:** beginner

**Requires:**
- 📦 cardano-wallet >= `v2020-04-01`
{{</hint>}}

Once you have a wallet you can manage your funds. In order to receive a transaction you need to provide an address associated with your wallet to the sender.

## Random wallets

For `random` wallets user needs to invoke the following wallet endpoint to create new addresses:

[`POST /byron-wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/createAddress)

In order to list existing addresses another endpoint can be used:

[`GET /byron-wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listByronAddresses)


## Sequential wallets {#listing-addresses-in-sequential-wallets}

For sequential wallets such as `icarus`, `trezor`, `ledger` there is no need for the user to explicitly create new addresses. Unused and used addresses can be retrieved via endpoint:

[`GET /byron-wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listByronAddresses)
