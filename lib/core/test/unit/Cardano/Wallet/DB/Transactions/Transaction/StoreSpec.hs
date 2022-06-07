{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Transactions.Transaction.StoreSpec(
    spec,
) where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Fixtures
    ( StoreProperty, logScale, withDBInMemory, withStoreProp )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Transactions.Transaction.Model
    ( DeltaTxHistory (..), TxHistoryF (..), mkTxHistory )
import Cardano.Wallet.DB.Transactions.Transaction.Store
    ( mkStoreTransactions )
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( arbitrary, elements, frequency, property )

import qualified Data.Map.Strict as Map

spec :: Spec
spec = around withDBInMemory $ do
    describe "Transactions store" $ do
        it "respects store laws"
            $ property . prop_StoreMetaLaws


prop_StoreMetaLaws :: StoreProperty
prop_StoreMetaLaws = withStoreProp $ \runQ ->
    prop_StoreUpdates
        runQ
        mkStoreTransactions
        (pure mempty)
        (logScale . genDeltas)

--------------------------------------------------------------------------------
--  generate deltas
--------------------------------------------------------------------------------
genDeltas :: GenDelta DeltaTxHistory
genDeltas (TxHistoryF history) =
    frequency
        [ (8, ExpandTxHistory . mkTxHistory <$> arbitrary)
        , (1, DeleteTxHistory . TxId <$> arbitrary)
        , (2, DeleteTxHistory
                <$> if null history
                    then TxId <$> arbitrary
                    else elements (Map.keys history)
          )
        ]
