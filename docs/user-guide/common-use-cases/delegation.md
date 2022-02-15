

---
order: 6
title: Delegation
---

## Pre-requisites

 - [[how-to-start-wallet-server]]
 - [[how-to-create-a-wallet]]
 - In order to be able to send transactions we need funds on the wallet. In case of [Testnet](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Overview

Delegation is the process by which ada holders delegate the stake associated with their ada to a stake pool. Cardano wallet allows listing all stake pools that are operating on the blockchain and then "joining" or "quitting" them via special delegation transaction.

Delegation is supported only for **Shelley** wallets.

## Listing stake pools

Before joining any stake pool we can first list all available stake pools that are operating on our blockchain. Stake pools are ordered by `non_myopic_member_rewards` which makes pools which will produce potentially the best rewards in the future higher in the ranking. The ordering can also depend on the `?stake` query parameter which says how much stake we want to delegate.

[`GET /stake-pools`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listStakePools)

```
$ curl -X GET http://localhost:8090/v2/stake-pools?stake=1000000000
```

## Joining stake pool

Once we select a stake pool we can "join" it. This operation will "virtually" add our wallet balance to the stake of this particular stake pool. Joining a pool for the first time will incur `fee` and `deposit` required for registering our stake key on the blockchain. This deposit will be returned to us if we choose to quit stake pool all together.

> :information_source: The amount of the deposit is controlled by Cardano network parameter. It is currently 2₳ on `mainnet` and `testnet`. Deposit is taken only once, when joining stake pool for the first time.

We can join stake pool using old transaction workflow:

[`PUT /stake-pools/{stakePoolId}/wallets/{walletId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/joinStakePool)

Or new transaction workflow, where we can **construct** delegation transaction and then **sign** and **submit** it to the network:
 - Construct: [`POST /wallets/{walletId}/transactions-construct`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/constructTransaction)
 - Sign: [`POST /wallets/{walletId}/transactions-sign`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/signTransaction)
 - Submit: [`POST /wallets/{walletId}/transactions-submit`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/submitTransaction)

Exemplary construct delegation request may look as follows:
```
$ curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \  
-d '{"delegations":
     [{"join":{"pool":"pool1mgjlw24rg8sp4vrzctqxtf2nn29rjhtkq2kdzvf4tcjd5pl547k",
               "stake_key_index":"0H"}}]}' \  
-H "Content-Type: application/json"
```

Refer to [[how-to-make-a-transaction]] for details on signing and submitting it to the network.

> :information_source: Note that information about `deposit_taken` and `deposit_returned` is available in the wallet transaction history. Refer to [[how-to-make-a-transaction]].

## Joining another stake pool

Joining another stake pool doesn't differ from the previous process at all. The only difference is that, behind the scenes, `deposit` is not taken from the wallet, as it was already taken when joining any stake pool for the first time.

## Withdrawing rewards

Our wallet accumulates rewards from delegating to a stake pool on special rewards account associated with it. We can see how many rewards we have on the wallet balance at any time.

[`GET /wallets/{walletId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/getWallet)

Just look up `balance` while getting your wallet details:
```
$ curl -X GET http://localhost:8090/v2/wallets/1ceb45b37a94c7022837b5ca14045f11a5927c65 | jq .balance

{
  "total": {
    "quantity": 14883796944,
    "unit": "lovelace"
  },
  "available": {
    "quantity": 14876107482,
    "unit": "lovelace"
  },
  "reward": {
    "quantity": 7689462, <----------- accumulated rewards
    "unit": "lovelace"
  }
}
```

We can withdraw those wallets when making any transaction. In the both, old and new transaction workflow, it is as simple as adding `{ "withdrawal": "self" }` to the transaction payload.

In particular in [new transaction workflow](https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Transactions-New) we can just withdraw our rewards:

```
$ curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \  
-d '{"withdrawal":"self"}' \  
-H "Content-Type: application/json"
```

or withdraw rewards while doing any other transaction:

```
$ curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \  
-d '{"payments":[{"address":"addr_test1qrtez7vn0d8xp495ggypmu2kyt7tt6qyva2spm0f5a3ewn0v474mcs4q8e9g55yknx3729kyg5dl69x5596ee9tvnynq7ffety","amount":{"quantity":1000000,"unit":"lovelace"}}],
     "withdrawal":"self"}' \  
-H "Content-Type: application/json"
```

As a result, after we sign and submit such transaction, the rewards will be added to our available balance (or just spend in case the amount of the transaction we're doing is bigger than our available balance and rewards are enough to compensate).

> :information_source: Note that information about `withdrawals` is also available in the wallet transaction history. Refer to [[how-to-make-a-transaction]].

## Quitting stake pool

At any point we can decide to quit delegation all together. Quitting stake pool will cause de-registration of the stake key associated to our wallet and our deposit will be returned. After that we're not longer going to receive rewards.

Quitting can be done in old transaction workflow:

[`DELETE /stake-pools/*/wallets/{walletId}`](https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/quitStakePool)

> :information_source: Note that all rewards will be withdrawn automatically upon quitting a stake pool.

or [new transaction workflow](https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Transactions-New), for instance:

Just quitting:
```
$ curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \  
-d '{"delegations":[{"quit":{"stake_key_index":"0H"}}]}' \  
-H "Content-Type: application/json"
```

Quitting and withdrawing rewards in the same transaction:
```
$ curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \  
-d '{"withdrawal":"self",
     "delegations":[{"quit":{"stake_key_index":"0H"}}]}' \  
-H "Content-Type: application/json"
```

> :information_source: Note that all rewards will be withdrawn only if you add {"withdrawal":"self"} to the payload. You can do it in a single transaction though as above.

> :information_source: Refer to [[how-to-make-a-transaction]] for details on signing and submitting it to the network.
