{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Property tests for the deposit wallet.
module Cardano.Wallet.Deposit.PureSpec
    ( spec
    , testOnWallet
    ) where

import Prelude

import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Wallet.Deposit.Pure
    ( Credentials
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( LookupTimeFromSlot
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( createMnemonicFromWords
    , credentialsFromMnemonics
    )
import Cardano.Wallet.Deposit.Testing.DSL
    ( InterpreterState (..)
    , ScenarioP
    , assert
    , availableBalance
    , block
    , deposit
    , deposit_
    , existsTx
    , historyByCustomer
    , historyByTime
    , interpret
    , newHistoryByTime
    , rollBackward
    , rollForward
    , withdrawal
    )
import Cardano.Wallet.Deposit.Testing.DSL.ByTime
    ( atBlock
    , byCustomerFromByTime
    , deposited
    , forCustomer
    , inTx
    , newByTime
    , withdrawn
    )
import Cardano.Wallet.Deposit.Time
    ( unsafeUTCTimeOfSlot
    )
import Control.Monad.Trans.State
    ( StateT
    )
import Data.Maybe
    ( fromJust
    )
import Data.Time
    ( UTCTime
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

timeFromSlot :: LookupTimeFromSlot
timeFromSlot = unsafeUTCTimeOfSlot

unsafeTimeForSlot :: Read.Slot -> Read.WithOrigin UTCTime
unsafeTimeForSlot = fromJust . timeFromSlot

testOnWallet
    :: ScenarioP
        (IO ())
        (StateT (Wallet.WalletState, InterpreterState) IO)
        ()
    -> IO ()
testOnWallet =
    interpret
        emptyWalletWith17Addresses
        id
        unsafeTimeForSlot

spec :: Spec
spec = do
    describe "UTxO availableBalance" $ do
        it
            "rollForward twice"
            prop_availableBalance_rollForward_twice
        it
            "rollBackward . rollForward"
            prop_availableBalance_rollForward_rollBackward
    describe "history by time" $ do
        it "is empty after initialization"
            $ testOnWallet
            $ do
                ht0 <- historyByTime
                assert $ ht0 `shouldBe` mempty
                hc0 <- historyByCustomer
                assert $ hc0 `shouldBe` mempty
        it "reports a tx after a rollforward"
            $ testOnWallet
            $ do
                tx1 <- existsTx
                deposit_ tx1 1 100
                b1 <- block [tx1]
                rollForward [b1]
                h1 <- historyByTime
                h1' <- newHistoryByTime $ newByTime $ do
                    atBlock b1 $ do
                        forCustomer 1 $ do
                            inTx tx1 $ deposited 100
                assert $ h1 `shouldBe` h1'
                hc1 <- historyByCustomer
                assert $ hc1 `shouldBe` byCustomerFromByTime h1'
                balance <- availableBalance
                assert $ balance `shouldBe` 100_000_000
        it "reports multiple blocks after a rollforward"
            $ testOnWallet
            $ do
                tx1 <- existsTx
                deposit_ tx1 1 100
                b1 <- block [tx1]
                tx2 <- existsTx
                deposit_ tx2 1 200
                b2 <- block [tx2]
                rollForward [b1, b2]
                h1 <- historyByTime
                h1' <- newHistoryByTime $ newByTime $ do
                    atBlock b1 $ do
                        forCustomer 1 $ do
                            inTx tx1 $ deposited 100
                    atBlock b2 $ do
                        forCustomer 1 $ do
                            inTx tx2 $ deposited 200
                assert $ h1 `shouldBe` h1'
                hc1 <- historyByCustomer
                assert $ hc1 `shouldBe` byCustomerFromByTime h1'
                balance <- availableBalance
                assert $ balance `shouldBe` 300_000_000
        it "reports withdrawals in separate blocks from deposits"
            $ testOnWallet
            $ do
                tx1 <- existsTx
                w1 <- deposit tx1 1 100
                b1 <- block [tx1]
                tx2 <- existsTx
                withdrawal tx2 w1
                b2 <- block [tx2]
                rollForward [b1, b2]
                h1 <- historyByTime
                h1' <- newHistoryByTime $ newByTime $ do
                    atBlock b1 $ do
                        forCustomer 1 $ do
                            inTx tx1 $ deposited 100
                    atBlock b2 $ do
                        forCustomer 1 $ do
                            inTx tx2 $ withdrawn 100
                assert $ h1 `shouldBe` h1'
                hc1 <- historyByCustomer
                assert $ hc1 `shouldBe` byCustomerFromByTime h1'
                balance <- availableBalance
                assert $ balance `shouldBe` 0
        it "reports withdrawals in the same block as deposits"
            $ testOnWallet
            $ do
                tx1 <- existsTx
                w1 <- deposit tx1 1 100
                tx2 <- existsTx
                withdrawal tx2 w1
                b1 <- block [tx1, tx2]
                rollForward [b1]
                h1 <- historyByTime
                h1' <- newHistoryByTime $ newByTime $ do
                    atBlock b1 $ do
                        forCustomer 1 $ do
                            inTx tx1 $ deposited 100
                            inTx tx2 $ withdrawn 100
                assert $ h1 `shouldBe` h1'
                hc1 <- historyByCustomer
                assert $ hc1 `shouldBe` byCustomerFromByTime h1'
                balance <- availableBalance
                assert $ balance `shouldBe` 0

        it "is empty after a full rollback"
            $ testOnWallet
            $ do
                tx1 <- existsTx
                deposit_ tx1 1 100
                b1 <- block [tx1]
                rollForward [b1]
                rollBackward Nothing
                h1 <- historyByTime
                assert $ h1 `shouldBe` mempty
                hc1 <- historyByCustomer
                assert $ hc1 `shouldBe` mempty
                balance <- availableBalance
                assert $ balance `shouldBe` 0
        it "contains the blocks not rolled back after a partial rollback"
            $ testOnWallet
            $ do
                tx1 <- existsTx
                deposit_ tx1 1 100
                b1 <- block [tx1]
                tx2 <- existsTx
                deposit_ tx2 1 200
                b2 <- block [tx2]
                rollForward [b1, b2]
                rollBackward $ Just b1
                h1 <- historyByTime
                h1' <- newHistoryByTime $ newByTime $ do
                    atBlock b1 $ do
                        forCustomer 1 $ do
                            inTx tx1 $ deposited 100
                assert $ h1 `shouldBe` h1'
                hc1 <- historyByCustomer
                assert $ hc1 `shouldBe` byCustomerFromByTime h1'
                balance <- availableBalance
                assert $ balance `shouldBe` 100_000_000

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
        .&&. Wallet.availableBalance
            (fst $ Wallet.rollBackward timeFromSlot chainPoint1 w3)
        === Wallet.availableBalance w1
        .&&. Wallet.availableBalance
            (fst $ Wallet.rollBackward timeFromSlot chainPoint2 w3)
        === Wallet.availableBalance w2
        .&&. Wallet.availableBalance w3
        =/= Wallet.availableBalance w2
        .&&. Wallet.availableBalance w3
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
    Wallet.fromCredentialsAndGenesis testCredentials 17 testGenesis

seed :: SomeMnemonic
seed = case createMnemonicFromWords
    "vital minimum victory start lunch find city peanut shiver soft hedgehog artwork mushroom loud found"
    of
    Right seed' -> seed'
    Left e -> error $ show e

testCredentials :: Credentials
testCredentials =
    credentialsFromMnemonics seed mempty

{-----------------------------------------------------------------------------
    Test blockchain
------------------------------------------------------------------------------}

testGenesis :: Read.GenesisData
testGenesis = Read.mockGenesisDataMainnet

spendOneTxOut :: UTxO.UTxO -> Write.Tx
spendOneTxOut utxo =
    Write.mkTx txBody
  where
    txBody =
        Write.TxBody
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
    txBody =
        Write.TxBody
            { Write.spendInputs = mempty
            , Write.collInputs = mempty
            , Write.txouts =
                Map.fromList $ zip [toEnum 0 ..] $ map toTxOut destinations
            , Write.collRet = Nothing
            , Write.expirySlot = Nothing
            }
