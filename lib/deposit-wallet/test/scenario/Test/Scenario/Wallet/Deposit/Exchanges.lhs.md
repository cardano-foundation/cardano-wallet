# Use case: Centralized Exchange

This document describes how a centralized exchange (CEX) can use the Deposit Wallet to

1. Assign an address to a customer ID.
2. Track deposits at this address.
3. Track deposits at all addresses.
4. Create payments to a different wallet.

# Scenarios, Haskell

In this section, we describe the scenarios using Haskell.

```haskell
module Test.Scenario.Wallet.Deposit.Exchanges
    ( scenarioRestore
    , scenarioStart
    , scenarioCreateAddressList
    , scenarioTrackDepositOne
    , scenarioTrackDepositAll
    , scenarioCreatePayment
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv
    , XPub
    )
import Cardano.Wallet.Deposit.IO
    ( WalletEnv
    , WalletInstance
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    , Credentials (..)
    , ResolvedTx (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , Value
    , TxId
    , lessOrEqual
    )
import Control.Tracer
    ( nullTracer
    )
import Test.Scenario.Blockchain
    ( ScenarioEnv
    , ada
    , assert
    , payFromFaucet
    , signTx
    , submitTx
    )

import qualified Cardano.Wallet.Deposit.IO as Wallet
import qualified Data.Map as Map
```

We use a function `depositFundsAt` to make a deposit at a given address.

```haskell
depositFundsAt :: ScenarioEnv -> Address -> Value -> IO ()
depositFundsAt env address value = payFromFaucet env [(address, value)]
```

We ignore the mapping from TxId when retrieving the customer history
```haskell
getCustomerDeposits :: Customer -> WalletInstance -> IO [(TxId, ValueTransfer)]
getCustomerDeposits customer w =
    Map.toList <$> Wallet.getCustomerDeposits w customer Nothing
```

## 0. Start a Wallet

A `WalletInstance` denotes a mutable wallet that is actively synchronizing to the blockchain, continuously writes its state to a database file, and responds to queries.

In order to create a fresh wallet, or in order to restore a wallet from its public key all the way from genesis, use the function `withWalletInit`. In addition to the public key, this function expects a number which equals the numerically largest customer ID previously handled with this wallet.

```haskell
scenarioRestore
    :: XPub -> WalletEnv IO -> IO ()
scenarioRestore xpub env = do
    let knownCustomerCount = 127
    Wallet.withWalletInit nullTracer env (XPubCredentials xpub) knownCustomerCount $ \w -> do
        value <- Wallet.availableBalance w
        assert $ value == ada 0
```

In order to load the wallet state from a database file and resume operation from that state use the function `withWalletLoad`.

```haskell
scenarioStart
    :: WalletEnv IO -> IO ()
scenarioStart env =
    Wallet.withWalletLoad nullTracer env $ \w -> do
        value <- Wallet.availableBalance w
        assert $ value == ada 0
```

## 1. Assign an address to a customer ID

A `Customer` is represented by a numeric customer ID.
Given such a customer ID, the function `customerAddress` will create an address and add the association between the customer and this address to the wallet state.

(The mapping from customer ID to address is deterministic and based on the [BIP-32][] address derivation scheme.)

  [bip-32]: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki


The function `listCustomers` returns the associations between customers and addresses recorded in the current wallet state.

```haskell
scenarioCreateAddressList
    :: WalletInstance -> IO ()
scenarioCreateAddressList w = do
    let customer = 31
    Just address <- Wallet.customerAddress customer w
    customers <- Wallet.listCustomers w
    assert $ (customer, address) `elem` customers
```

## 2. Track deposits at this address

As soon as an association between customer and address has been added to the wallet state using `customerAddress`, the wallet will track deposits sent to this address.

The function `getCustomerDeposits` returns a summary for each transaction that is related to this customer. For every summary, the `received` field records the total deposit made by the customer at this address in this transaction.

(The `spent` field has informative purpose only, and records whether the wallet has moved any funds out of this address.)

The following scenario illustrates how `getCustomerDeposits` records deposits:

```haskell
scenarioTrackDepositOne
    :: ScenarioEnv -> WalletInstance -> IO ()
scenarioTrackDepositOne env w = do
    Just address <- Wallet.customerAddress customer w

    -- no deposits
    txsummaries0 <- getCustomerDeposits customer w
    assert $ null txsummaries0

    -- first deposit
    depositFundsAt env address coin
    txsummaries1 <- getCustomerDeposits customer w
    assert $ map (received . snd) txsummaries1 == [coin]

    -- second deposit
    depositFundsAt env address coin
    txsummaries2 <- getCustomerDeposits customer w
    assert $ map (received . snd) txsummaries2 == [coin, coin]
  where
    customer = 7 :: Customer
    coin = ada 12
```

## 3. Track deposits at all addresses

A centralized exchange typically wants to monitor all transactions in a recent time window for activity in order to synchronize customer deposits on the blockchain ledger with the exchange ledger recording customer balances.

This is a task for the `getCustomerHistories` function — it returns a mapping from customers to `TxSummaries` that record the entire activity within the given time interval.

The time interval is specified by a `from` and a `to` point on the blockchain. We note that the `from` argument is exclusive while the `to` argument is inclusive.
We use the type `ChainPoint` to specify points on the blockchain — this type uses both a slot number and a block header to uniquely identify a block. We do this in order to allow atomic operations — in the event that the `to` or `from` point are no longer part of the consensus chain, the `getCustomerHistories` functions throws an exception.

The wallet is synchronized to a particular point on the blockchain — use `getWalletTip` to query it.

```haskell
scenarioTrackDepositAll
    :: ScenarioEnv -> WalletInstance -> IO ()
scenarioTrackDepositAll env w = do
    Just address1 <- Wallet.customerAddress customer1 w
    Just address2 <- Wallet.customerAddress customer2 w

    depositFundsAt env address1 coin
    depositFundsAt env address2 coin
    depositFundsAt env address1 (coin <> coin)

    history <- Wallet.getAllDeposits w Nothing
    assert $
        Map.map received history
      ==
        Map.fromList
            [ (customer1, coin <> coin <> coin)
            , (customer2, coin)
            ]
  where
    customer1, customer2 :: Customer
    customer1 = 1
    customer2 = 2
    coin = ada 3
```

## 4. Create payments to a different wallet

The `createPayment` function allows you to move funds from one wallet to other addresses, e.g. in order to process customer withdrawals. If the wallet has sufficient funds, this function creates a transaction body which sends the given values to the given addresses.

The transaction body needs to be signed. Given a transaction body, the function `getBIP32PathsForOwnedInputs` will provide you with all [BIP-32][] address derivation paths of all inputs that are owned by the wallet, and which therefore require a signature.

```haskell
scenarioCreatePayment
    :: XPrv -> ScenarioEnv -> Address -> WalletInstance -> IO ()
scenarioCreatePayment xprv env destination w = do
    -- deposit some funds at customer address
    Just address1 <- Wallet.customerAddress customer w
    depositFundsAt env address1 (coin <> coin)
    value1 <- Wallet.availableBalance w
    assert $ value1 == (coin <> coin)

    -- createPayment
    Right (ResolvedTx txUnsigned _) <- Wallet.createPayment [(destination, coin)] w
    paths <- Wallet.getBIP32PathsForOwnedInputs txUnsigned w
    let tx = signTx xprv paths txUnsigned
    submitTx env tx

    -- funds have been moved out of the wallet
    value2 <- Wallet.availableBalance w
    assert $ (value2 <> coin) `lessOrEqual` value1

    -- but the original deposit amount is still recorded
    txsummaries <- getCustomerDeposits customer w
    assert $ value1 `elem` map (received . snd) txsummaries
  where
    customer :: Customer
    customer = 17
    coin = ada 5
```
