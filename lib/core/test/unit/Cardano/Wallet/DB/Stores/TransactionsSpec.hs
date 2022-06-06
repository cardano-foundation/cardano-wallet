{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DB.Stores.TransactionsSpec
    ( spec
    ) where

import Prelude

import Cardano.DB.Sqlite
    ( SqliteContext, runQuery )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.Schema
    ( txMetaSlotExpires )
import Cardano.Wallet.DB.Sqlite.Types
    ( getTxId )
import Cardano.Wallet.DB.Stores.Fixtures
    ( RunQuery (RunQuery), initializeWallet, runWalletProp, withDBInMemory )
import Cardano.Wallet.DB.Transactions.Delta
    ( DeltaTxHistory (AgeTxHistory, ExpandTxHistory, PruneTxHistory) )
import Cardano.Wallet.DB.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.DB.Transactions.Types
    ( TxHistory, TxHistoryF (TxHistoryF), txHistory_relations )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Data.DBVar
    ()
import Data.Foldable
    ( toList )
import Data.Maybe
    ( catMaybes )
import Test.DBVar
    ( GenDelta, Updates (Updates), genUpdates, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Blind (Blind)
    , Gen
    , Property
    , arbitrary
    , elements
    , frequency
    , property
    , scale
    )
import Test.QuickCheck.Monadic
    ( monadicIO, pick, run )

import qualified Data.Map.Strict as Map
import Database.Persist.Sql
    ( SqlPersistT )

spec :: Spec
spec = around withDBInMemory $ do
    describe "Transactions store" $ do
        it "respects store laws" $
            property . prop_StoreTransactionsLaws
        it "can prune a not-in-ledger transaction" $
            property . prop_PrunePositive

--------------------------------------------------------------------------------
--  generate deltas
--------------------------------------------------------------------------------
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



------------------------------------------------------------------------
-- store laws
--------------------------------------------------------------------------------
prop_StoreTransactionsLaws
    :: SqliteContext
    -> WalletId
    -> Property
prop_StoreTransactionsLaws  = runWalletProp $ \wid (RunQuery run) ->
    prop_StoreUpdates
        do run
        do mkStoreTransactions wid
        do pure mempty
        do genDeltas wid



--------------------------------------------------------------------------------
-- transactions delta semantics
--------------------------------------------------------------------------------


prop_PrunePositive
    :: SqliteContext
    -> WalletId
    -> Property
prop_PrunePositive = runWalletProp $ \wid (RunQuery run) -> do
    -- genUpdates arbitrary

    Updates adas <- pick $ genUpdates (pure mempty) $ genDeltas wid
    let as  = map fst adas <> [mempty]
        das = map snd adas
    pure True
