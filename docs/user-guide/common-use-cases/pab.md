---
order: 8
title: Using cardano-wallet with Plutus Application Backend
---

## Pre-requisites
 - [[how-to-start-wallet-server]]
 - [[how-to-create-a-wallet]]
 - Install [Plutus Application Backend](https://github.com/input-output-hk/plutus/tree/master/plutus-pab).
 - In order to be able to balance Plutus transaction we need funds on the wallet. In case of [Testnet](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Overview
This guide is to show how to invoke Plutus contracts with cardano-wallet.

## Workflow
Once you have created a smart contract with [PAB](https://github.com/input-output-hk/plutus/tree/master/plutus-pab) you can execute it via cardano-wallet.

There are three endpoints that need to be invoked to follow the workflow:

 - [POST /wallets/{walletId}/transactions-balance](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/balanceTransaction) - for balancing transaction from PAB.
 - [POST /wallets/{walletId}/transactions-sign](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/signTransaction) - for signing transaction.
 - [POST   
/wallets/{walletId}/transactions-submit](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/submitTransaction) - for submitting transaction to the network.

### Balance transaction

Plutus Application Backend provides a payload of an unbalanced transaction. This transaction needs to be balanced with wallet's inputs such that it can be submitted to the network. The response from this endpoint returns `fee` and `coin_selection` of the balanced transaction as well as CBOR-encoded `transaction` represented in base64 encoding. We will need the returned `transaction` value to pass on to [POST /wallets/{walletId}/transactions-sign](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/signTransaction) endpoint.

```
$ curl -X POST http://localhost:8090/v2/wallets/1f82e83772b7579fc0854bd13db6a9cce21ccd95/transactions-balance \
-d '{"transaction":
{"cborHex":"84a500800d80018183581d704d72cf569a339a18a7d9302313983f56e0d96cd45bdcb1d6512dca6a1a001e84805820923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec02000e80a10481d87980f5f6",
"description":"",
"type":"Tx AlonzoEra"},
"inputs":[]}' \
-H "Content-Type: application/json" | jq .transaction

hKYAgYJYIACL3OShD8xU3XMcglayOyc72ZGWvU19iiSLqEPzLUQ6AA2BglggAIvc5KEPzFTdcxyCVrI7JzvZkZa9TX2KJIuoQ/MtRDoAAYKCWDkA87i32rzLSeZJCB3GHrOB/Shg/B+3I7ucILBfkyiKiq2biY2B6ZIS5v5pD/BMyrCeg3/pX6imiDEaArTdZoNYHXBNcs9WmjOaGKfZMCMTmD9W4Nls1FvcsdZRLcpqGgAehIBYIJI5GOQDv0PDS072tI6y7gS6vtFzINjRuf+a0Ibob0TsAhoAEdGtDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaEEgdh5gPX2
```

### Sign transaction

Once the transaction is balanced we need to sign it using our wallet's secure passphrase and pass the previously returned CBOR-encoded `transaction`. The sign endpoint will again return CBOR-encoded `transaction` which is needed to be passed further to [POST   
/wallets/{walletId}/transactions-submit](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/submitTransaction) endpoint.

```
$ curl -X POST http://localhost:8090/v2/wallets/1f82e83772b7579fc0854bd13db6a9cce21ccd95/transactions-sign \
-d '{"passphrase":"Secure Passphrase",
"transaction":"hKYAgYJYIAQh6enKOwhicVBXDNo6ukKgvJJnMVFIbrvczELZao8sAQ2BglggBCHp6co7CGJxUFcM2jq6QqC8kmcxUUhuu9zMQtlqjywBAYKCWDkA87i32rzLSeZJCB3GHrOB/Shg/B+3I7ucILBfkyiKiq2biY2B6ZIS5v5pD/BMyrCeg3/pX6imiDEaBoFcr4NYHXBNcs9WmjOaGKfZMCMTmD9W4Nls1FvcsdZRLcpqGgAehIBYIJI5GOQDv0PDS072tI6y7gS6vtFzINjRuf+a0Ibob0TsAhoAEdGtDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaEEgdh5gPX2"}' \
-H "Content-Type: application/json" | jq .transaction

hKYAgYJYIAQh6enKOwhicVBXDNo6ukKgvJJnMVFIbrvczELZao8sAQ2BglggBCHp6co7CGJxUFcM2jq6QqC8kmcxUUhuu9zMQtlqjywBAYKCWDkA87i32rzLSeZJCB3GHrOB/Shg/B+3I7ucILBfkyiKiq2biY2B6ZIS5v5pD/BMyrCeg3/pX6imiDEaBoFcr4NYHXBNcs9WmjOaGKfZMCMTmD9W4Nls1FvcsdZRLcpqGgAehIBYIJI5GOQDv0PDS072tI6y7gS6vtFzINjRuf+a0Ibob0TsAhoAEdGtDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaIAgYJYIBaZLkF/espB4YMd23QgZlpQ0TQkXR8ESalTVqlS4HFQWEAUwu5e080HmdIBoPAeb+L22gFtvlY7j8urDPoRIqxb0d41LF/MWNYXQy9yahWdqyHzCg+P7mduRuGrXAFUR0EBBIHYeYD19g==
```

### Submit transaction

We have our balanced and signed CBOR-encoded `transaction` represented in base64 encoding. Now we can submit it to the network with [POST   
/wallets/{walletId}/transactions-submit](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/submitTransaction) endpoint.

```
$ curl -X POST http://localhost:8090/v2/wallets/1f82e83772b7579fc0854bd13db6a9cce21ccd95/transactions-submit \  
-d '{"transaction":"hKYAgYJYIAQh6enKOwhicVBXDNo6ukKgvJJnMVFIbrvczELZao8sAQ2BglggBCHp6co7CGJxUFcM2jq6QqC8kmcxUUhuu9zMQtlqjywBAYKCWDkA87i32rzLSeZJCB3GHrOB/Shg/B+3I7ucILBfkyiKiq2biY2B6ZIS5v5pD/BMyrCeg3/pX6imiDEaBoFcr4NYHXBNcs9WmjOaGKfZMCMTmD9W4Nls1FvcsdZRLcpqGgAehIBYIJI5GOQDv0PDS072tI6y7gS6vtFzINjRuf+a0Ibob0TsAhoAEdGtDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaIAgYJYIBaZLkF/espB4YMd23QgZlpQ0TQkXR8ESalTVqlS4HFQWEAUwu5e080HmdIBoPAeb+L22gFtvlY7j8urDPoRIqxb0d41LF/MWNYXQy9yahWdqyHzCg+P7mduRuGrXAFUR0EBBIHYeYD19g=="}' \  
-H "Content-Type: application/json"

{"id":"148cb83bb2943e54d878a86fdc4309d952ca5fd62b3463fbb72a94180ebe8e82"}
```

We can monitor status of the submitted transaction using [GET /wallets/{walletId}/transactions/{transactionId}](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getTransaction) endpoint.

```
$ curl -X GET http://localhost:8090/v2/wallets/1f82e83772b7579fc0854bd13db6a9cce21ccd95/transactions/148cb83bb2943e54d878a86fdc4309d952ca5fd62b3463fbb72a94180ebe8e82 | jq

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
