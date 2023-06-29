# Plutus Application Backend

## Pre-requisites

 - Install [Plutus Application Backend](https://github.com/input-output-hk/plutus/tree/master/plutus-pab).
 - In order to be able to balance Plutus transaction we need funds on the wallet. In case of [Testnet](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Overview

This guide is to show how to invoke Plutus contracts with cardano-wallet.

## Workflow

Once you have created a smart contract with [PAB](https://github.com/input-output-hk/plutus/tree/master/plutus-pab) you can execute it via cardano-wallet.

There are three endpoints that need to be invoked to follow the workflow:

 - [POST /wallets/{walletId}/transactions-balance](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/balanceTransaction) - for balancing transaction from PAB.
 - [POST /wallets/{walletId}/transactions-sign](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/signTransaction) - for signing transaction.
 - [POST
/wallets/{walletId}/transactions-submit](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/submitTransaction) - for submitting transaction to the network.

### Balance transaction

Plutus Application Backend provides a payload of an unbalanced transaction. This transaction needs to be balanced with wallet's inputs such that it can be submitted to the network. The response from this endpoint returns `fee` and `coin_selection` of the balanced transaction as well as CBOR-encoded `transaction` represented in base64 encoding. We will need the returned `transaction` value to pass on to [POST /wallets/{walletId}/transactions-sign](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/signTransaction) endpoint.

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-balance \
-d '{"transaction":"84a500800d80018183581d704d72cf569a339a18a7d9302313983f56e0d96cd45bdcb1d6512dca6a1a001e84805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec02000e80a10481d87980f5f6","redeemers":[],"inputs":[]}' \
-H "Content-Type: application/json" | jq .transaction

hKYAgYJYIJs6ATvbNwo5xpOkjHfUzr9Cv4zuFLFicFwWwpPC4ltBAw2AAYKDWB1wTXLPVpozmhin2TAjE5g/VuDZbNRb3LHWUS3KahoAHoSAWCCSORjkA79Dw0tO9rSOsu4Eur7RcyDY0bn/mtCG6G9E7IJYOQDsKgV69YfvMZdbfIT11OqtWL9bv7n++Jx0f+TDFwodcRLCv14tOV5BoBhV6ODYaS1MNdWnvKCjgBq2bfNOAhoAApgxDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaEEgdh5gPX2
```

### Sign transaction

Once the transaction is balanced we need to sign it using our wallet's secure passphrase and pass the previously returned CBOR-encoded `transaction` . The sign endpoint will again return CBOR-encoded `transaction` which is needed to be passed further to [POST
/wallets/{walletId}/transactions-submit](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/submitTransaction) endpoint.

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-sign \
-d '{"passphrase":"Secure Passphrase",
"transaction":"hKYAgYJYIJs6ATvbNwo5xpOkjHfUzr9Cv4zuFLFicFwWwpPC4ltBAw2AAYKDWB1wTXLPVpozmhin2TAjE5g/VuDZbNRb3LHWUS3KahoAHoSAWCCSORjkA79Dw0tO9rSOsu4Eur7RcyDY0bn/mtCG6G9E7IJYOQDsKgV69YfvMZdbfIT11OqtWL9bv7n++Jx0f+TDFwodcRLCv14tOV5BoBhV6ODYaS1MNdWnvKCjgBq2bfNOAhoAApgxDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaEEgdh5gPX2"}' \
-H "Content-Type: application/json" | jq .transaction

hKYAgYJYIJs6ATvbNwo5xpOkjHfUzr9Cv4zuFLFicFwWwpPC4ltBAw2AAYKDWB1wTXLPVpozmhin2TAjE5g/VuDZbNRb3LHWUS3KahoAHoSAWCCSORjkA79Dw0tO9rSOsu4Eur7RcyDY0bn/mtCG6G9E7IJYOQDsKgV69YfvMZdbfIT11OqtWL9bv7n++Jx0f+TDFwodcRLCv14tOV5BoBhV6ODYaS1MNdWnvKCjgBq2bfNOAhoAApgxDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaIAgYJYIAVUtVUzi4FodRYiBmO9mD5hQGo2YjjYDoCgw5gn+/w9WEAyVhMWNiK88QKW6HBXIVxQyu0E+9epkIQbCQwNjKur5ORLojHxIZtZDDfkT6caz0yxp92t4Y7rDwsDw4geMOkJBIHYeYD19g==
```

### Submit transaction

We have our balanced and signed CBOR-encoded `transaction` represented in base64 encoding. Now we can submit it to the network with [POST
/wallets/{walletId}/transactions-submit](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/submitTransaction) endpoint.

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-submit \
-d '{"transaction":"hKYAgYJYIJs6ATvbNwo5xpOkjHfUzr9Cv4zuFLFicFwWwpPC4ltBAw2AAYKDWB1wTXLPVpozmhin2TAjE5g/VuDZbNRb3LHWUS3KahoAHoSAWCCSORjkA79Dw0tO9rSOsu4Eur7RcyDY0bn/mtCG6G9E7IJYOQDsKgV69YfvMZdbfIT11OqtWL9bv7n++Jx0f+TDFwodcRLCv14tOV5BoBhV6ODYaS1MNdWnvKCjgBq2bfNOAhoAApgxDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaIAgYJYIAVUtVUzi4FodRYiBmO9mD5hQGo2YjjYDoCgw5gn+/w9WEAyVhMWNiK88QKW6HBXIVxQyu0E+9epkIQbCQwNjKur5ORLojHxIZtZDDfkT6caz0yxp92t4Y7rDwsDw4geMOkJBIHYeYD19g=="}' \
-H "Content-Type: application/json"

{"id":"c287cd5a752ff632e07747109193ed8f8b8e446211563951e7f8e470ed859782"}
```

We can monitor status of the submitted transaction using [GET /wallets/{walletId}/transactions/{transactionId}](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getTransaction) endpoint.

```
> curl -X GET http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions/c287cd5a752ff632e07747109193ed8f8b8e446211563951e7f8e470ed859782 | jq

{
  "status": "in_ledger",
...
"amount": {
  "quantity": 2170033,
  "unit": "lovelace"
},
"inserted_at": {
  "height": {
    "quantity": 3324947,
    "unit": "block"
  },
  "epoch_number": 187,
  "time": "2022-02-16T11:22:12Z",
  "absolute_slot_number": 50641316,
  "slot_number": 226916
},
...
```
