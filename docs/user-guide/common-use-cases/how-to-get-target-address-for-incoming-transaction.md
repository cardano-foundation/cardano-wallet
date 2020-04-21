---
weight: 3
title: How to get target address for incoming transaction
---

Once you have a wallet you can manage your funds. In order to receive a transaction you need to provide an address associated with your wallet to the sender.

## Random wallets

For `random` wallets user needs to invoke wallet endpoint to create new address:

[`POST /byron-wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/createAddress)

In order to list existing addresses another endpoint can be used:

[`GET /wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listAddresses)


## Sequential wallets {#listing-addresses-in-sequential-wallets}

For sequential wallets such as `icarus`, `trezor`, `ledger` there is no need for the user to explicitely create new addresses. Unused and used addresses can be retrived via endpoint:

[`GET /wallets/{walletId}/addresses`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listAddresses)
