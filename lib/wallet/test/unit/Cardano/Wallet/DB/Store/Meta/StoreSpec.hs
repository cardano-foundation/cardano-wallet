{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Cardano.Wallet.DB.Store.Meta.StoreSpec (spec) where

import Cardano.DB.Sqlite
  ( ForeignKeysSetting (..)
  , runQuery
  )
import Cardano.Slotting.Slot
  ( SlotNo
  )
import Cardano.Wallet.DB.Arbitrary
  (
  )
import Cardano.Wallet.DB.Fixtures
  ( WalletProperty
  , assertWith
  , initializeWalletTable
  , logScale
  , queryLaw
  , withDBInMemory
  , withInitializedWalletProp
  )
import Cardano.Wallet.DB.Sqlite.Schema
  ( TxMeta (..)
  )
import Cardano.Wallet.DB.Sqlite.Types
  ( TxId (TxId)
  )
import Cardano.Wallet.DB.Store.Meta.Layer
  ( QueryTxMeta (..)
  , mkQueryStoreTxMeta
  )
import Cardano.Wallet.DB.Store.Meta.Model
  ( DeltaTxMetaHistory (..)
  , TxMetaHistory (..)
  )
import Cardano.Wallet.DB.Store.Meta.ModelSpec
  ( genExpand
  , genRollback
  )
import Cardano.Wallet.DB.Store.Meta.Store
  ( mkStoreMetaTransactions
  )
import Cardano.Wallet.Primitive.Types
  ( Range (..)
  , SortOrder (Ascending, Descending)
  , WalletId
  )
import Control.Monad
  ( forM_
  , (<=<)
  )
import Data.Foldable
  ( toList
  )
import Data.Map.Strict qualified as Map
import Data.Store
  ( Store (..)
  )
import GHC.Natural
  ( Natural
  )
import Test.Hspec
  ( Spec
  , around
  , describe
  , it
  )
import Test.QuickCheck
  ( Gen
  , arbitrary
  , elements
  , frequency
  , property
  )
import Test.QuickCheck.Monadic
  ( forAllM
  , pick
  )
import Test.Store
  ( prop_StoreUpdate
  )
import Prelude

spec :: Spec
spec = around (withDBInMemory ForeignKeysEnabled) $ do
  describe "meta-transactions store" $ do
    it "respects store laws"
      $ property . prop_StoreMetaLaws
  describe "mkQueryStoreTxMeta"
    $ it "respects query law"
    $ property . prop_QueryLaw

genDeltas :: WalletId -> TxMetaHistory -> Gen DeltaTxMetaHistory
genDeltas wid history =
  frequency
    [ (10, Expand <$> genExpand wid arbitrary)
    , (3, genRollback history)
    ]

prop_StoreMetaLaws :: WalletProperty
prop_StoreMetaLaws db wid =
  prop_StoreUpdate
    (runQuery db)
    setupStore
    (pure mempty)
    (logScale . genDeltas wid)
  where
    setupStore = do
      initializeWalletTable wid
      pure mkStoreMetaTransactions

prop_QueryLaw :: WalletProperty
prop_QueryLaw =
  withInitializedWalletProp $ \wid runQ ->
    forAllM (genExpand wid arbitrary) $ \history -> do
      runQ $ writeS mkQueryStoreTxMeta history
      unknownTxId <- TxId <$> pick arbitrary
      let
        txIds = unknownTxId : Map.keys (relations history)
      forM_ txIds $ \txId -> do
        assertWith "GetOne"
          <=< runQ
          $ queryLaw mkQueryStoreTxMeta history
          $ GetOne txId
      range <- pick $ genRange history
      limit <- pick $ genLimit history
      order <- pick genSortOrder
      assertWith "GetSome"
        <=< runQ
        $ queryLaw mkQueryStoreTxMeta history
        $ GetSome range limit order
      slot <- pick $ genSlot history
      assertWith "GetAfterSlot"
        <=< runQ
        $ queryLaw mkQueryStoreTxMeta history
        $ GetAfterSlot slot

genSortOrder :: Gen SortOrder
genSortOrder = elements [Ascending, Descending]

genRange :: TxMetaHistory -> Gen (Range SlotNo)
genRange (TxMetaHistory history) =
  Range
    <$> elements slots
    <*> elements slots
  where
    slots = Nothing : map (Just . txMetaSlot) (toList history)

genLimit :: TxMetaHistory -> Gen (Maybe Natural)
genLimit (TxMetaHistory history) =
  elements $ Nothing : (Just <$> [1 .. fromIntegral (length history)])

genSlot :: TxMetaHistory -> Gen SlotNo
genSlot (TxMetaHistory history) = do
  unknownSlot <- arbitrary
  elements $ unknownSlot : (txMetaSlot <$> toList history)
