{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Store.Meta.Layer
    ( QueryTxMeta (..)
    , mkQueryStoreTxMeta
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..)
    , TxMeta (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..)
    )
import Cardano.Wallet.DB.Store.Meta.Model
    ( DeltaTxMetaHistory
    , TxMetaHistory (..)
    )
import Cardano.Wallet.DB.Store.Meta.Store
    ( mkStoreMetaTransactions
    )
import Cardano.Wallet.Primitive.Slotting
    ( SlotNo
    )
import Cardano.Wallet.Primitive.Types
    ( SortOrder (..)
    )
import Cardano.Wallet.Primitive.Types.Range
    ( Range (..)
    )
import Data.Foldable
    ( toList
    )
import Data.List
    ( sortOn
    )
import Data.Maybe
    ( catMaybes
    )
import Data.Ord
    ( Down (..)
    )
import Data.Set
    ( Set
    )
import Data.Store
    ( Query (..)
    , Store
    , mkQueryStore
    )
import Database.Persist.Sql
    ( Entity (entityVal)
    , Filter
    , PersistQueryRead (selectFirst)
    , SelectOpt (..)
    , SqlPersistT
    , selectList
    , (<=.)
    , (==.)
    , (>.)
    , (>=.)
    )
import GHC.Natural
    ( Natural
    )

import qualified Cardano.Wallet.DB.Sqlite.Schema as DB
import qualified Cardano.Wallet.Primitive.Types.Range as Range
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    DB for 'TxMeta'
------------------------------------------------------------------------------}

-- | A query to the 'TxMeta' database.
data QueryTxMeta b where
    GetSome
        :: Range SlotNo
        -> Maybe Natural
        -> SortOrder
        -> QueryTxMeta [DB.TxMeta]
    GetAfterSlot :: SlotNo -> QueryTxMeta (Set TxId)
    GetOne :: TxId -> QueryTxMeta (Maybe DB.TxMeta)

filterMetas :: Bool -> Range SlotNo -> [Filter TxMeta]
filterMetas _ (Range Nothing Nothing) = []
filterMetas True (Range (Just low) Nothing) = [TxMetaSlot >=. low]
filterMetas False (Range (Just low) Nothing) = [TxMetaSlot >. low]
filterMetas _ (Range  Nothing (Just high)) = [TxMetaSlot <=. high]
filterMetas b (Range low high) = filterMetas b (Range Nothing high)
    <> filterMetas b (Range low Nothing)

limitMetas :: Maybe Natural -> [SelectOpt record]
limitMetas Nothing = []
limitMetas (Just l) = [LimitTo $ fromIntegral l]

orderMetas :: SortOrder -> [SelectOpt TxMeta]
orderMetas Ascending = [Asc TxMetaSlot, Asc TxMetaTxId]
orderMetas Descending = [Desc TxMetaSlot, Asc TxMetaTxId]

-- | A 'QueryStore' for 'TxMeta'.
mkQueryStoreTxMeta :: Store (SqlPersistT IO) QueryTxMeta DeltaTxMetaHistory
mkQueryStoreTxMeta =
    mkQueryStore query' mkStoreMetaTransactions
  where
    query' :: forall b. QueryTxMeta b -> (SqlPersistT IO) b
    query' = \case
        GetSome range limit order ->
            fmap entityVal
                <$> selectList @DB.TxMeta
                    (filterMetas True range)
                    (limitMetas limit <> orderMetas order)
        GetOne txId ->
            fmap entityVal
                <$> selectFirst @_ @_ @DB.TxMeta
                    [TxMetaTxId ==. txId]
                    []
        GetAfterSlot slot ->
            foldMap ((Set.singleton . txMetaTxId) . entityVal)
                <$> selectList @DB.TxMeta
                    (filterMetas False $ Range (Just slot) Nothing)
                    []

instance Query QueryTxMeta where
    type World QueryTxMeta = TxMetaHistory
    query :: QueryTxMeta b -> World QueryTxMeta -> b
    query q (TxMetaHistory allTransactions) = case q of
        GetSome range mlimit order ->
            let whichMeta DB.TxMeta{..} =
                    and
                        $ catMaybes
                            [ (txMetaSlot >=)
                                <$> Range.inclusiveLowerBound range
                            , (txMetaSlot <=)
                                <$> Range.inclusiveUpperBound range
                            ]
                reorder = case order of
                    Ascending -> sortOn ((,) <$> txMetaSlot <*> txMetaTxId)
                    Descending -> sortOn ((,) <$> Down . txMetaSlot <*> txMetaTxId)
            in  maybe id (take . fromIntegral) mlimit
                    $ reorder
                    $ filter whichMeta
                    $ toList allTransactions
        GetOne ti -> Map.lookup ti allTransactions
        GetAfterSlot slot ->
            let whichMeta DB.TxMeta{..} = txMetaSlot > slot
            in  Set.fromList
                    $ fmap txMetaTxId
                    $ filter whichMeta
                    $ toList allTransactions
