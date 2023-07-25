# How to make a transaction

## Pre-requisites

- [how to start a server](start-wallet-server.md)
- [how to create a wallet](create-a-wallet.md)
- In order to be able to send transactions, our wallet must have funds. In case of `preview` and `preprod` [testnets](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Old transaction workflow

Assuming you have already created a wallet, you can send a transaction by using the following endpoint:

 - [`POST /wallets/{walletId}/transactions`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postTransaction) - transaction from **Shelley** wallet
 - [`POST /byron-wallets/{walletId}/transactions`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postByronTransaction) - transaction from **Byron** wallet

Behind the scene, the wallet engine will select necessary inputs from the wallet, generate a change address within the wallet, sign and submit the transaction. A transaction can have multiple outputs, possibly to the same address. Note that in Byron, addresses are necessarily base58-encoded (as an enforced convention).

## New transaction workflow

[New transaction workflow](https://cardano-foundation.github.io/cardano-wallet/api/edge/#tag/Transactions-New) decouples creation of the transaction into separate steps:
 - Construct: [`POST /wallets/{walletId}/transactions-construct`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/constructTransaction)
 - Sign: [`POST /wallets/{walletId}/transactions-sign`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/signTransaction)
 - Submit: [`POST /wallets/{walletId}/transactions-submit`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/submitTransaction)

Behind the scene, on the **construct** step, the wallet engine will select necessary inputs belonging to the wallet, generate a change address of the wallet and calculate the `fee` for this particular transaction.

**Sign** and **submit** are now invoked as separate steps. This allows for presenting the actual transaction `fee` to the end user. In the old workflow, when all those steps where done in one go, showing precise fee was not possible and it had to be estimated using separate wallet endpoint. Because of the random nature of coin-selection algorithm such estimation might not have been always the same as the actual transaction fee.

Here is very basic example sending out 10₳ and 1 `asset` from the wallet:
1. Construct.
```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \
-d '{"payments":
[{"address":"addr_test1qrv60y8vwu8cke6j83tgkfjttmtv0ytfvnhggp6f4gl5kf0l0dw5r75vk42mv3ykq8vyjeaanvpytg79xqzymqy5acmq5k85dg",
"amount":{"quantity":10000000,"unit":"lovelace"},
"assets":[{"policy_id":"b518eee977e1c8e3ce020e745be63b8b14498c565f5b59653e104ec7",
           "asset_name":"4163757374696330",
           "quantity":1}]}]}' \
-H "Content-Type: application/json"
```
2. Sign.

```admonish note
The response from **construct** will give us information like `fee` or `coin selection` details. It also returns a CBOR-encoded `transaction` represented in base64 encoding. This is what we need to feed into **sign**'s payload.
```

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-sign \
-d '{"passphrase":"Secure Passphrase",
     "transaction":"hKYAgYJYIJs6ATvbNwo5xpOkjHfUzr9Cv4zuFLFicFwWwpPC4ltBBQ2AAYKCWDkA0Tt2d1mVEvi5ZUp1k6RAHcWWqmNX0+gr0ea9nv97XUH6jLVVtkSWAdhJZ72bAkWjxTAETYCU7jaCGgCYloChWBy1GO7pd+HI484CDnRb5juLFEmMVl9bWWU+EE7HoUhBY3VzdGljMAGCWDkAILE1lnWHTaOk26BI/mHKGcjdgw9DcIsWT4W0YxcKHXESwr9eLTleQaAYVejg2GktTDXVp7ygo4CCGrYB1PChWBy1GO7pd+HI484CDnRb5juLFEmMVl9bWWU+EE7Hp0hBY3VzdGljMQFIQWN1c3RpYzIBSEFjdXN0aWM0AURCYXNzBUVEcnVtcwZJRHJ1bXNPbmx5AUhFbGVjdHJpYwYCGgAC2VUDGgMDcO8OgKD19g=="}' \
-H "Content-Type: application/json"
```
3. Submit.

```admonish note
   Again, we are feeding **submit**'s payload with CBOR-encoded `transaction` returned by **sign** request.
```

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-submit \
-d '{"transaction":"hKYAgYJYIJs6ATvbNwo5xpOkjHfUzr9Cv4zuFLFicFwWwpPC4ltBBQ2AAYKCWDkA0Tt2d1mVEvi5ZUp1k6RAHcWWqmNX0+gr0ea9nv97XUH6jLVVtkSWAdhJZ72bAkWjxTAETYCU7jaCGgCYloChWBy1GO7pd+HI484CDnRb5juLFEmMVl9bWWU+EE7HoUhBY3VzdGljMAGCWDkAILE1lnWHTaOk26BI/mHKGcjdgw9DcIsWT4W0YxcKHXESwr9eLTleQaAYVejg2GktTDXVp7ygo4CCGrYB1PChWBy1GO7pd+HI484CDnRb5juLFEmMVl9bWWU+EE7Hp0hBY3VzdGljMQFIQWN1c3RpYzIBSEFjdXN0aWM0AURCYXNzBUVEcnVtcwZJRHJ1bXNPbmx5AUhFbGVjdHJpYwYCGgAC2VUDGgMDcO8OgKEAgYJYIASQMgPsYJvlhj+L/ttWXivY8xL/Pzun5qalDwy+pVTpWECQQJAildc3KiO1u86KTH+qSg45K7/wckT4KPE21a819POFIf15NVDY9tsGAkT9uCBRyJ13m0h01ZsA9TWVso8J9fY="}' \
-H "Content-Type: application/json"

{
  "id": "35dd9db1f61822ac82f4690ea4fe12426bf6e534aff8e13563fce55a4d502772"
}
```

We can monitor status of submitted transaction using [GET /wallets/{walletId}/transactions/{transactionId}](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getTransaction) endpoint.
```
> curl -X GET http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions/35dd9db1f61822ac82f4690ea4fe12426bf6e534aff8e13563fce55a4d502772 | jq

{
  "inserted_at": {
    "height": {
      "quantity": 2975821,
      "unit": "block"
    },
    "time": "2021-10-08T11:25:32Z",
    "epoch_number": 161,
    "absolute_slot_number": 39323116,
    "slot_number": 140716
  },
  "status": "in_ledger",
....
```

## Transaction history

Note that all transactions made from and to any wallet are available in the transaction history.
We can always display details of a particular transaction as well as list all transactions that are known to a wallet.

For instance, transactions can be tracked via:

 - [`GET /wallets/{walletId}/transactions`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/listTransactions) - transactions from **Shelley** wallet
 - [`GET /byron-wallets/{walletId}/transactions`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/listByronTransactions) - transactions from **Byron** wallet

Which returns a list of all transactions for this particular wallet. Optional range filters can be provided. A transaction will go through a succession of states, starting as “Pending”. If a transaction stays pending for too long (because rejected by a mempool, or because lost in translation due to multiple chain switches), users may decide to forget it using:

 - [`DELETE /wallets/{walletId}/transactions/{transactionId}`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/deleteTransaction)  - transactions from **Shelley** wallet
 - [`DELETE /byron-wallets/{walletId}/transactions/{transactionId}`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/deleteByronTransaction)  - transactions from **Byron** wallet

For more information about transactions lifecycle, have a look at
[transaction lifecycle](../../concepts/transaction-lifecycle.md).



## Signed and serialized transactions

Alternatively, `cardano-wallet` allows clients to submit already signed and serialized transactions as a raw bytes blob. This can be done by submitting such serialized data as an `application/octet-stream` :

[`POST /proxy/transactions`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/postExternalTransaction)

In this scenario, the server engine will verify that the transaction is structurally well-formed and forward it to the node instance associated with it. If the transaction belongs to a known wallet, it will eventually show up in the wallet.
