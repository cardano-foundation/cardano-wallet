{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Cardano.Wallet.DB.Transactions.StoreSpec
    ( spec
    ) where

import Prelude


import Cardano.DB.Sqlite
    ( SqliteContext, newInMemorySqliteContext, runQuery )
import Cardano.Wallet.DB.Arbitrary
    ()
import Cardano.Wallet.DB.Sqlite.Schema
    ( migrateAll )
import Cardano.Wallet.DB.Transactions.Delta
    ( DeltaTxHistory (ExpandTxHistory) )
import Cardano.Wallet.DB.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( bracket )
import Control.Tracer
    ( nullTracer )
import Data.DBVar
    ()
import Test.DBVar
    ( GenDelta, prop_StoreUpdates )
import Test.Hspec
    ( Spec, around, describe, it )
import Test.QuickCheck
    ( Gen, Property, arbitrary, frequency, property )
import Test.QuickCheck.Monadic
    ( monadicIO )

spec :: Spec
spec = around withDBInMemory $ do
    describe "Update" $ do
        it "mkStoreTransactions" $
            property . prop_StoreTransactions 

--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------
prop_StoreTransactions
    :: SqliteContext
    -> WalletId
    -> Property
prop_StoreTransactions db wid =
    monadicIO $ prop_StoreUpdates 
        do runQuery db 
        do mkStoreTransactions wid
        do pure mempty
        do genDeltas wid 

genDeltas ::  WalletId -> GenDelta DeltaTxHistory
genDeltas wid history = frequency 
    [ (1, genExpand wid )
    ]

genExpand :: WalletId -> Gen DeltaTxHistory
genExpand wid  = ExpandTxHistory wid <$> arbitrary

--------------------------------------------------------------------------------
-- DB setup
--------------------------------------------------------------------------------
withDBInMemory :: (SqliteContext -> IO a) -> IO a
withDBInMemory action = bracket newDBInMemory fst (action . snd)

newDBInMemory :: IO (IO (), SqliteContext)
newDBInMemory = newInMemorySqliteContext nullTracer [] migrateAll

