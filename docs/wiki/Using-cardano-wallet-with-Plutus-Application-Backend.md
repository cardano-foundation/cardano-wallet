# Overview
This guide is to show how to invoke Plutus contracts with cardano-wallet. 

# Pre-requisites
 - Start [cardano-node](https://github.com/input-output-hk/cardano-node/releases) and [cardano-wallet](https://github.com/input-output-hk/cardano-wallet/releases) following instructions from release notes. (Or use handy [docker-compose](https://github.com/input-output-hk/cardano-wallet#getting-started) to start both.)
 - Install [Plutus Application Backend](https://github.com/input-output-hk/plutus/tree/master/plutus-pab).

In order to be able to balance Plutus transaction we need funds on the wallet. In case of [Testnet](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/). 

In order to create new wallet in a first place we need to invoke [create wallet](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postWallet) endpoint, for instance:

```
$ curl -X POST http://localhost:8090/v2/wallets \
-d '{"mnemonic_sentence":["slab","praise","suffer","rabbit","during","dream","arch","harvest","culture","book","owner","loud","wool","salon","table","animal","vivid","arrow","dirt","divide","humble","tornado","solution","jungle"],
     "passphrase":"Secure Passphrase",
     "name":"My Test Wallet",
     "address_pool_gap":20}' \
-H "Content-Type: application/json"
```

# Workflow
Once you have created a smart contract with [PAB](https://github.com/input-output-hk/plutus/tree/master/plutus-pab) you can execute it via cardano-wallet.

There are three endpoints that need to be invoked to follow the workflow:

 - [Balance](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/balanceTransaction) - for balancing transaction from PAB.
 - [Sign](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/signTransaction) - for signing transaction.
 - [Submit](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postExternalTransaction) - for submitting transaction to the network.

## Balance transaction

Plutus Application Backend provides payload of unbalanced transaction. This transaction needs to be balanced with wallet inputs such that it can be submitted to the network. The response from this endpoint returns `fee` and `coin_selection` of balanced transaction as well as CBOR-encoded `transaction` represented in base64 encoding. We will need that returned `transaction` value to pass on to [sign](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/signTransaction) endpoint.

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

## Sign transaction

Once transaction is balanced we need to sign it using our wallet's secure passphrase passing previously returned CBOR-encoded `transaction`. The sign endpoint again will returned CBOR-encoded `transaction` which need to pass further to [submit](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/postExternalTransaction) endpoint.

```
$ curl -X POST http://localhost:8090/v2/wallets/1f82e83772b7579fc0854bd13db6a9cce21ccd95/transactions-sign \
-d '{"passphrase":"Secure Passphrase",
"transaction":"hKYAgYJYIAQh6enKOwhicVBXDNo6ukKgvJJnMVFIbrvczELZao8sAQ2BglggBCHp6co7CGJxUFcM2jq6QqC8kmcxUUhuu9zMQtlqjywBAYKCWDkA87i32rzLSeZJCB3GHrOB/Shg/B+3I7ucILBfkyiKiq2biY2B6ZIS5v5pD/BMyrCeg3/pX6imiDEaBoFcr4NYHXBNcs9WmjOaGKfZMCMTmD9W4Nls1FvcsdZRLcpqGgAehIBYIJI5GOQDv0PDS072tI6y7gS6vtFzINjRuf+a0Ibob0TsAhoAEdGtDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaEEgdh5gPX2"}' \
-H "Content-Type: application/json" | jq .transaction

hKYAgYJYIAQh6enKOwhicVBXDNo6ukKgvJJnMVFIbrvczELZao8sAQ2BglggBCHp6co7CGJxUFcM2jq6QqC8kmcxUUhuu9zMQtlqjywBAYKCWDkA87i32rzLSeZJCB3GHrOB/Shg/B+3I7ucILBfkyiKiq2biY2B6ZIS5v5pD/BMyrCeg3/pX6imiDEaBoFcr4NYHXBNcs9WmjOaGKfZMCMTmD9W4Nls1FvcsdZRLcpqGgAehIBYIJI5GOQDv0PDS072tI6y7gS6vtFzINjRuf+a0Ibob0TsAhoAEdGtDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaIAgYJYIBaZLkF/espB4YMd23QgZlpQ0TQkXR8ESalTVqlS4HFQWEAUwu5e080HmdIBoPAeb+L22gFtvlY7j8urDPoRIqxb0d41LF/MWNYXQy9yahWdqyHzCg+P7mduRuGrXAFUR0EBBIHYeYD19g==
```

## Submit transaction

We have our balanced and signed CBOR-encoded `transaction` represented in base64 encoding. Before passing it further we need to decode it to binary format: 

```
$ base64 -d > tx.bin <<< hKYAgYJYIAQh6enKOwhicVBXDNo6ukKgvJJnMVFIbrvczELZao8sAQ2BglggBCHp6co7CGJxUFcM2jq6QqC8kmcxUUhuu9zMQtlqjywBAYKCWDkA87i32rzLSeZJCB3GHrOB/Shg/B+3I7ucILBfkyiKiq2biY2B6ZIS5v5pD/BMyrCeg3/pX6imiDEaBoFcr4NYHXBNcs9WmjOaGKfZMCMTmD9W4Nls1FvcsdZRLcpqGgAehIBYIJI5GOQDv0PDS072tI6y7gS6vtFzINjRuf+a0Ibob0TsAhoAEdGtDoALWCAvUOolRvjOAgykW/zyq+sC/xivIoNGb4iK5IkYSz0tOaIAgYJYIBaZLkF/espB4YMd23QgZlpQ0TQkXR8ESalTVqlS4HFQWEAUwu5e080HmdIBoPAeb+L22gFtvlY7j8urDPoRIqxb0d41LF/MWNYXQy9yahWdqyHzCg+P7mduRuGrXAFUR0EBBIHYeYD19g==
```

Now we can submit it to the network:
```
$ curl --header "Content-Type: application/octet-stream" -X POST --data-binary "@tx.bin" http://localhost:8090/v2/proxy/transactions

{"id":"148cb83bb2943e54d878a86fdc4309d952ca5fd62b3463fbb72a94180ebe8e82"}
```

Once transaction is accepted by the ledger we can look it up in our wallet's transaction history.
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