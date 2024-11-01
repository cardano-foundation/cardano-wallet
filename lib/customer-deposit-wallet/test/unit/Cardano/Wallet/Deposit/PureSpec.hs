{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Property tests for the deposit wallet.
-}
module Cardano.Wallet.Deposit.PureSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub
    , generate
    , toXPub
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( LookupTimeFromSlot
    )
import Cardano.Wallet.Deposit.Time
    ( unsafeUTCTimeOfSlot
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Property
    , (.&&.)
    , (=/=)
    , (===)
    )

import qualified Cardano.Wallet.Deposit.Pure as Wallet
import qualified Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Write as Write
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

timeFromSlot :: LookupTimeFromSlot
timeFromSlot = unsafeUTCTimeOfSlot

spec :: Spec
spec = do
    describe "UTxO availableBalance" $ do
        it "rollForward twice"
            prop_availableBalance_rollForward_twice
        it "rollBackward . rollForward"
            prop_availableBalance_rollForward_rollBackward

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
prop_availableBalance_rollForward_twice :: Property
prop_availableBalance_rollForward_twice =
    Wallet.availableBalance w2 === Write.mkAda 3
  where
    w0 = emptyWalletWith17Addresses
    Just addr1 = Wallet.customerAddress 1 w0
    Just addr2 = Wallet.customerAddress 2 w0

    tx1 = payFromFaucet [(addr1, Write.mkAda 1)]
    block1 = Read.mockNextBlock Read.GenesisPoint [tx1]
    chainPoint1 = Read.getChainPoint block1
    w1 = Wallet.rollForwardOne timeFromSlot (Read.EraValue block1) w0

    tx2 = payFromFaucet [(addr2, Write.mkAda 2)]
    block2 = Read.mockNextBlock chainPoint1 [tx2]
    w2 = Wallet.rollForwardOne timeFromSlot (Read.EraValue block2) w1

prop_availableBalance_rollForward_rollBackward :: Property
prop_availableBalance_rollForward_rollBackward =
        Wallet.availableBalance
            (fst $ Wallet.rollBackward timeFromSlot chainPoint0 w3)
            === Wallet.availableBalance w0
    .&&.
        Wallet.availableBalance
            (fst $ Wallet.rollBackward timeFromSlot chainPoint1 w3)
            === Wallet.availableBalance w1
    .&&.
        Wallet.availableBalance
            (fst $ Wallet.rollBackward timeFromSlot chainPoint2 w3)
            === Wallet.availableBalance w2
    .&&.
        Wallet.availableBalance w3
            =/= Wallet.availableBalance w2
    .&&.
        Wallet.availableBalance w3
            `Read.lessOrEqual` Wallet.availableBalance w2
  where
    w0 = emptyWalletWith17Addresses
    Just addr1 = Wallet.customerAddress 1 w0
    Just addr2 = Wallet.customerAddress 2 w0
    chainPoint0 = Read.GenesisPoint

    tx1 = payFromFaucet [(addr1, Write.mkAda 1)]
    block1 = Read.mockNextBlock chainPoint0 [tx1]
    w1 = Wallet.rollForwardOne timeFromSlot (Read.EraValue block1) w0
    chainPoint1 = Read.getChainPoint block1

    tx2 = payFromFaucet [(addr2, Write.mkAda 2)]
    block2 = Read.mockNextBlock chainPoint1 [tx2]
    chainPoint2 = Read.getChainPoint block2
    w2 = Wallet.rollForwardOne timeFromSlot (Read.EraValue block2) w1

    tx3 = spendOneTxOut (Wallet.availableUTxO w2)
    block3 = Read.mockNextBlock chainPoint2 [tx3]
    w3 = Wallet.rollForwardOne timeFromSlot (Read.EraValue block3) w2

emptyWalletWith17Addresses :: Wallet.WalletState
emptyWalletWith17Addresses =
    Wallet.fromXPubAndGenesis testXPub 17 testGenesis

testXPub :: XPub
testXPub =
    toXPub
    $ generate (B8.pack "random seed for a testing xpub lala") B8.empty

{-----------------------------------------------------------------------------
    Test blockchain
------------------------------------------------------------------------------}

testGenesis :: Read.GenesisData
testGenesis = Read.mockGenesisDataMainnet

spendOneTxOut :: UTxO.UTxO -> Write.Tx
spendOneTxOut utxo =
    Write.mkTx txBody
  where
    txBody = Write.TxBody
        { Write.spendInputs = Set.singleton . fst . head $ Map.toList utxo
        , Write.collInputs = mempty
        , Write.txouts = Map.empty
        , Write.collRet = Nothing
        , Write.expirySlot = Nothing
        }

payFromFaucet :: [(Write.Address, Write.Value)] -> Write.Tx
payFromFaucet destinations =
    Write.mkTx txBody
  where
    toTxOut (addr, value) = Write.mkTxOut addr value
    txBody = Write.TxBody
        { Write.spendInputs = mempty
        , Write.collInputs = mempty
        , Write.txouts =
            Map.fromList $ zip [toEnum 0..] $ map toTxOut destinations
        , Write.collRet = Nothing
        , Write.expirySlot = Nothing
        }
