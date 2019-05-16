{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.MVarSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DB
    ( DBLayer (..) )
import Cardano.Wallet.DB.MVar
    ( newDBLayer )
import Cardano.Wallet.DBSpec
    ( DummyStateMVar (..)
    , DummyTarget
    , lrp
    , prop_createListWallet
    , prop_createWalletTwice
    , prop_isolation
    , prop_parallelPut
    , prop_putBeforeInit
    , prop_readAfterDelete
    , prop_readAfterPut
    , prop_removeWalletTwice
    , prop_sequentialPut
    , readTxHistoryF
    , unions
    )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), Tx (..), TxMeta (..) )
import Data.Map.Strict
    ( Map )
import Test.Hspec
    ( Spec, before, describe, it )
import Test.QuickCheck
    ( checkCoverage, property )

spec :: Spec
spec = do
    before (newDBLayer :: IO (DBLayer IO DummyStateMVar DummyTarget)) $
        describe "Extra Properties about DB initialization" $ do
        it "createWallet . listWallets yields expected results"
            (property . prop_createListWallet)
        it "creating same wallet twice yields an error"
            (property . prop_createWalletTwice)
        it "removing the same wallet twice yields an error"
            (property . prop_removeWalletTwice)

    before (newDBLayer :: IO (DBLayer IO DummyStateMVar DummyTarget)) $
        describe "put . read yields a result" $ do
        it "Checkpoint"
            (property . (prop_readAfterPut putCheckpoint readCheckpoint))
        it "Wallet Metadata"
            (property . (prop_readAfterPut putWalletMeta readWalletMeta))
        it "Tx History"
            (property . (prop_readAfterPut putTxHistory readTxHistoryF))
        it "Private Key"
            (property . (prop_readAfterPut putPrivateKey readPrivateKey))

    before (newDBLayer :: IO (DBLayer IO DummyStateMVar DummyTarget)) $
        describe "can't put before wallet exists" $ do
        it "Checkpoint"
            (property . (prop_putBeforeInit putCheckpoint readCheckpoint Nothing))
        it "Wallet Metadata"
            (property . (prop_putBeforeInit putWalletMeta readWalletMeta Nothing))
        it "Tx History"
            (property . (prop_putBeforeInit putTxHistory readTxHistoryF (pure mempty)))
        it "Private Key"
            (property . (prop_putBeforeInit putPrivateKey readPrivateKey Nothing))

    before (newDBLayer :: IO (DBLayer IO DummyStateMVar DummyTarget)) $
        describe "put doesn't affect other resources" $ do
        it "Checkpoint vs Wallet Metadata & Tx History & Private Key"
            (property . (prop_isolation putCheckpoint
                readWalletMeta
                readTxHistoryF
                readPrivateKey)
            )
        it "Wallet Metadata vs Tx History & Checkpoint & Private Key"
            (property . (prop_isolation putWalletMeta
                readTxHistoryF
                readCheckpoint
                readPrivateKey)
            )
        it "Tx History vs Checkpoint & Wallet Metadata & Private Key"
            (property . (prop_isolation putTxHistory
                readCheckpoint
                readWalletMeta
                readPrivateKey)
            )

    before (newDBLayer :: IO (DBLayer IO DummyStateMVar DummyTarget)) $
        describe "can't read after delete" $ do
        it "Checkpoint"
            (property . (prop_readAfterDelete readCheckpoint Nothing))
        it "Wallet Metadata"
            (property . (prop_readAfterDelete readWalletMeta Nothing))
        it "Tx History"
            (property . (prop_readAfterDelete readTxHistoryF (pure mempty)))
        it "Private Key"
            (property . (prop_readAfterDelete readPrivateKey Nothing))

    before (newDBLayer :: IO (DBLayer IO DummyStateMVar DummyTarget)) $
        describe "sequential puts replace values in order" $ do
        it "Checkpoint"
            (checkCoverage . (prop_sequentialPut putCheckpoint readCheckpoint lrp))
        it "Wallet Metadata"
            (checkCoverage . (prop_sequentialPut putWalletMeta readWalletMeta lrp))
        it "Tx History"
            (checkCoverage . (prop_sequentialPut putTxHistory readTxHistoryF unions))
        it "Private Key"
            (checkCoverage . (prop_sequentialPut putPrivateKey readPrivateKey lrp))

    before (newDBLayer :: IO (DBLayer IO DummyStateMVar DummyTarget)) $
        describe "parallel puts replace values in _any_ order" $ do
        it "Checkpoint"
            (checkCoverage . (prop_parallelPut putCheckpoint readCheckpoint
                (length . lrp @Maybe)))
        it "Wallet Metadata"
            (checkCoverage . (prop_parallelPut putWalletMeta readWalletMeta
                (length . lrp @Maybe)))
        it "Tx History"
            (checkCoverage . (prop_parallelPut putTxHistory readTxHistoryF
                (length . unions @(Map (Hash "Tx") (Tx, TxMeta)))))
        it "Private Key"
            (checkCoverage . (prop_parallelPut putPrivateKey readPrivateKey
                (length . lrp @Maybe)))
