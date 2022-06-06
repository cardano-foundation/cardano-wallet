{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments, TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Cardano.Wallet.DB.Stores.TransactionsSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( SqliteContext, runQuery )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Transactions.Delta
    ( DeltaTxHistory (ExpandTxHistory, PruneTxHistory, AgeTxHistory) )
import Cardano.Wallet.DB.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Data.DBVar
    ()
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, Property, arbitrary, frequency, property, scale, elements )
import Test.QuickCheck.Monadic
    ( monadicIO, run )
import Cardano.Wallet.DB.Stores.Fixtures ( withDBInMemory, initializeWallet )
import Cardano.Wallet.DB.Transactions.Types (TxHistory, txHistory_relations, TxHistoryF (TxHistoryF))
import qualified Data.Map.Strict as Map
import Cardano.Wallet.DB.Sqlite.Types (getTxId)
import Data.Foldable (toList)
import Cardano.Wallet.DB.Sqlite.Schema (txMetaSlotExpires)
import Data.Maybe (catMaybes)

spec :: Spec
spec = around withDBInMemory $ do
    describe "Transactions store" $ do
        it "respects store laws" $
            property . prop_StoreTransactionsLaws

--------------------------------------------------------------------------------
-- store laws
--------------------------------------------------------------------------------
prop_StoreTransactionsLaws
    :: SqliteContext
    -> WalletId
    -> Property
prop_StoreTransactionsLaws db wid = monadicIO $ do
    run . runQuery db $ initializeWallet wid
    prop_StoreUpdates
        do runQuery db
        do mkStoreTransactions wid
        do pure mempty
        do genDeltas wid

genDeltas ::  WalletId -> GenDelta DeltaTxHistory
genDeltas wid history
    = scale (floor @Double . log . fromIntegral . succ)
        $ frequency
        [ (10, ExpandTxHistory wid <$> arbitrary)
        , (1, PruneTxHistory <$> arbitrary)
                -- probably just dropped
        , (4, genPrune history)
                -- will try to prune a valid id
        , (1, AgeTxHistory <$> arbitrary)
                -- probably just dropped
        , (4, genAge history)
                -- will age valid ids
        ]

genAge ::TxHistory -> Gen DeltaTxHistory
genAge (TxHistoryF history) =
    case catMaybes $ txMetaSlotExpires . fst <$> toList history of
        [] -> AgeTxHistory <$> arbitrary
        xs -> AgeTxHistory <$> elements xs

genPrune :: TxHistory -> Gen DeltaTxHistory
genPrune history =
    case fmap getTxId $ Map.keys $ txHistory_relations history of
        [] -> PruneTxHistory <$> arbitrary
        xs -> PruneTxHistory <$> elements xs

--------------------------------------------------------------------------------
-- transactions delta semantics
--------------------------------------------------------------------------------


prop_UpdateTransactionsSemantics
    :: SqliteContext
    -> WalletId
    -> Property
prop_UpdateTransactionsSemantics db wid = monadicIO $ do
    run . runQuery db $ initializeWallet wid
    genUpdates arbitrary
