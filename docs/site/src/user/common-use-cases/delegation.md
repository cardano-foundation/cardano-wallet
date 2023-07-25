# How to delegate

## Pre-requisites


- [how to start a server](start-wallet-server.md)
- [how to create a wallet](create-a-wallet.md)
- In order to be able to send transactions, our wallet must have funds. In case of `preview` and `preprod` [testnets](https://testnets.cardano.org/en/testnets/cardano/overview/) we can request tADA from the [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/).

## Overview

Delegation is the process by which ada holders delegate the stake associated with their ada to a stake pool. Cardano wallet allows listing all stake pools that are operating on the blockchain and then "join" or "quit" them via special delegation transaction.

Delegation is supported only for **Shelley** wallets. **Shared** wallets will support it too in the near future.

## Listing stake pools

Before joining any stake pool we can first list all available stake pools that are operating on our blockchain. Stake pools are ordered by `non_myopic_member_rewards` which gives higher ranking and hence favors the pools potentially producing the best rewards in the future. The ordering could be influenced by the `?stake` query parameter which says how much stake we want to delegate.

 - [`GET /stake-pools`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/listStakePools)

```
> curl -X GET http://localhost:8090/v2/stake-pools?stake=1000000000
```

## Joining stake pool

Once we select a stake pool we can "join" it. This operation will "virtually" add our wallet balance to the stake of this particular stake pool. Joining a pool for the first time will incur `fee` and `deposit` required for registering our stake key on the blockchain. This deposit will be returned to us if we quit stake pool all together.

```admonish note
The amount of the deposit is controlled by Cardano network parameter. It is currently 2₳ on `mainnet` and `testnet`. Deposit is taken only once, when joining stake pool for the first time. Rejoining another stake pool does not incur another deposit.
```

We can join stake pool using old transaction workflow:

 - [`PUT /stake-pools/{stakePoolId}/wallets/{walletId}`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/joinStakePool)

Or new transaction workflow, where we can **construct** delegation transaction and then **sign** and **submit** it to the network:
 - Construct: [`POST /wallets/{walletId}/transactions-construct`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/constructTransaction)
 - Sign: [`POST /wallets/{walletId}/transactions-sign`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/signTransaction)
 - Submit: [`POST /wallets/{walletId}/transactions-submit`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/submitTransaction)

Exemplary construct delegation request may look as follows:
```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \
-d '{"delegations":
     [{"join":{"pool":"pool1mgjlw24rg8sp4vrzctqxtf2nn29rjhtkq2kdzvf4tcjd5pl547k",
               "stake_key_index":"0H"}}]}' \
-H "Content-Type: application/json"
```

Refer to [how-to-make-a-transaction](how-to-make-a-transaction.md)
for details on signing and submitting it to the network.

```admonish note
Information about `deposit_taken` and `deposit_returned` is available in the wallet transaction history. Refer to [how-to-make-a-transaction](how-to-make-a-transaction.md)
```

## Joining another stake pool

Joining another stake pool doesn't differ from the previous process at all. The only difference is that, behind the scenes, `deposit` is not taken from the wallet, as it was already taken when joining for the first time.

## Withdrawing rewards

Our wallet accumulates rewards from delegating to a stake pool on special rewards account associated with it. We can see how many rewards we have on the wallet balance at any time.

 - [`GET /wallets/{walletId}`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getWallet)

Just look up `balance` while getting your wallet details:
```
> curl -X GET http://localhost:8090/v2/wallets/1ceb45b37a94c7022837b5ca14045f11a5927c65 | jq .balance

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

We can withdraw those rewards when making any transaction. In the both, old and new transaction workflow, it is as simple as adding `{ "withdrawal": "self" }` to the transaction payload.

In particular in [new transaction workflow](https://cardano-foundation.github.io/cardano-wallet/api/edge/#tag/Transactions-New) we can just withdraw our rewards:

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \
-d '{"withdrawal":"self"}' \
-H "Content-Type: application/json"
```

or withdraw rewards while doing any other transaction:

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \
-d '{"payments":[{"address":"addr_test1qrtez7vn0d8xp495ggypmu2kyt7tt6qyva2spm0f5a3ewn0v474mcs4q8e9g55yknx3729kyg5dl69x5596ee9tvnynq7ffety","amount":{"quantity":1000000,"unit":"lovelace"}}],
     "withdrawal":"self"}' \
-H "Content-Type: application/json"
```

As a result, after we sign and submit such transaction, the rewards will be added to our available balance (or just spend in case the amount of the transaction we're doing is bigger than our available balance and rewards are enough to compensate).

```admonish note
Information about `withdrawals` is also available in the wallet transaction history. Refer to [how-to-make-a-transaction](how-to-make-a-transaction.md)
```

## Quitting stake pool

At any point we can decide to quit delegation all together. Quitting stake pool will cause de-registration of the stake key associated to our wallet and our deposit will be returned. After that we're not longer going to receive rewards.

Quitting can be done in old transaction workflow:

 - [`DELETE /stake-pools/*/wallets/{walletId}`](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/quitStakePool)

```admonish note
All rewards will be withdrawn automatically upon quitting a stake pool.
```

or [new transaction workflow](https://cardano-foundation.github.io/cardano-wallet/api/edge/#tag/Transactions-New), for instance:

Just quitting:

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \
-d '{"delegations":[{"quit":{"stake_key_index":"0H"}}]}' \
-H "Content-Type: application/json"
```

Quitting and withdrawing rewards in the same transaction. It should be noted that quitting can be realized only when all rewards are withdrawn:

```
> curl -X POST http://localhost:8090/v2/wallets/1b0aa24994b4181e79116c131510f2abf6cdaa4f/transactions-construct \
-d '{"withdrawal":"self",
     "delegations":[{"quit":{"stake_key_index":"0H"}}]}' \
-H "Content-Type: application/json"
```

After constructing a transaction that undelegates from a stake pool, I should receive a CBOR-encoded `transaction`, `fee`, and `coin_selection` in the response.
I can now **sign** and **submit** such a transaction just like in
[how-to-make-a-transaction](how-to-make-a-transaction.md).

```admonish note
All rewards will be withdrawn only if you add {"withdrawal":"self"} to the payload. You can achieve this with a single transaction, as shown above.
```