{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DB.Store.UTxOHistory.StoreSpec (spec) where

import Prelude

import Cardano.DB.Sqlite
    ( ForeignKeysSetting (..)
    , runQuery
    )
import Cardano.Wallet.DB.Arbitrary
    (
    )
import Cardano.Wallet.DB.Fixtures
    ( WalletProperty
    , initializeWalletTable
    , logScale
    , withDBInMemory
    )
import Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( DeltaUTxOHistory (AppendBlock, Prune, Rollback)
    , UTxOHistory
    , empty
    )
import Cardano.Wallet.DB.Store.UTxOHistory.ModelSpec
    ( genDelta
    , genSlot
    , genSlotNo
    , genUTxO
    )
import Cardano.Wallet.DB.Store.UTxOHistory.Store
    ( mkStoreUTxOHistory
    )
import Fmt
    ( Buildable (..)
    )
import Test.Hspec
    ( Spec
    , around
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , frequency
    , property
    )
import Test.Store
    ( prop_StoreUpdate
    )

spec :: Spec
spec = around (withDBInMemory ForeignKeysEnabled) $ do
    describe "UTxOHistory store" $ do
        it "respects store laws"
            $ property . prop_StoreUTxOHistoryLaws

genDeltas :: UTxOHistory -> Gen DeltaUTxOHistory
genDeltas history =
    frequency
        [ (10, AppendBlock <$> genSlotNo history (1, 1, 4) <*> genDelta history)
        , (3, Rollback <$> genSlot history (2, 4, 1))
        , (5, Prune <$> genSlotNo history (1, 4, 1))
        ]

prop_StoreUTxOHistoryLaws :: WalletProperty
prop_StoreUTxOHistoryLaws db wid =
    prop_StoreUpdate
        (runQuery db)
        setupStore
        (empty <$> genUTxO (empty mempty))
        (logScale . genDeltas)
  where
    setupStore = do
        initializeWalletTable wid
        pure $ mkStoreUTxOHistory wid

instance Buildable DeltaUTxOHistory where
    build = build . show
