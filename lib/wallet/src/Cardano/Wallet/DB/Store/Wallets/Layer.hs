{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Implementation of a 'QueryStore' for 'TxWalletsHistory'.
-}
module Cardano.Wallet.DB.Store.Wallets.Layer
    ( QueryTxWalletsHistory (..)
    , QueryStoreTxWalletsHistory
    , newQueryStoreTxWalletsHistory
    ) where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo
    )
import Cardano.Wallet.DB.Sqlite.Schema
    ( CBOR
    , TxMeta (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..)
    )
import Cardano.Wallet.DB.Store.Meta.Layer
    ( QueryTxMeta (..)
    , mkQueryStoreTxMeta
    )
import Cardano.Wallet.DB.Store.Transactions.Layer
    ( mkQueryStoreTxSet
    )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( TxRelation
    )
import Cardano.Wallet.DB.Store.Wallets.Model
    ( DeltaTxWalletsHistory (..)
    , TxWalletsHistory
    )
import Cardano.Wallet.DB.Store.Wallets.Store
    ( mkStoreTxWalletsHistory
    )
import Cardano.Wallet.Primitive.Types
    ( Range (..)
    , SortOrder
    )
import Data.Store
    ( Query (..)
    , Store (..)
    , mkQueryStore
    )
import Data.Word
    ( Word32
    )
import Database.Persist.Sql
    ( SqlPersistT
    )
import GHC.Natural
    ( Natural
    )

import qualified Cardano.Wallet.DB.Store.Transactions.Layer as TxSet
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W

{-----------------------------------------------------------------------------
    Query type
------------------------------------------------------------------------------}
data QueryTxWalletsHistory b where
    GetByTxId
        :: TxId
        -> QueryTxWalletsHistory (Maybe (Either TxRelation CBOR))
    GetTxOut
        :: (TxId, Word32)
        -> QueryTxWalletsHistory (Maybe W.TxOut)
    OneMeta
        :: TxId
        -> QueryTxWalletsHistory (Maybe TxMeta)
    SomeMetas
        :: Range SlotNo
        -> Maybe Natural
        -> SortOrder
        -> QueryTxWalletsHistory [TxMeta]

instance Query QueryTxWalletsHistory where
    type World QueryTxWalletsHistory = TxWalletsHistory
    query q (txs,metas) = case q of
        GetByTxId txid -> query (TxSet.GetByTxId txid) txs
        GetTxOut key -> query (TxSet.GetTxOut key) txs
        OneMeta txId -> query (GetOne txId) metas
        SomeMetas range limit order -> flip query metas
            $ GetSome range limit order

{-----------------------------------------------------------------------------
    Query Store type
------------------------------------------------------------------------------}
type QueryStoreTxWalletsHistory =
    Store (SqlPersistT IO) QueryTxWalletsHistory DeltaTxWalletsHistory

newQueryStoreTxWalletsHistory
    :: Store (SqlPersistT IO) QueryTxWalletsHistory DeltaTxWalletsHistory
newQueryStoreTxWalletsHistory =
    mkQueryStore query' wallets
  where
    txs = mkQueryStoreTxSet
    metas = mkQueryStoreTxMeta
    wallets = mkStoreTxWalletsHistory txs metas

    query' :: forall b. QueryTxWalletsHistory b -> (SqlPersistT IO) b
    query' = \case
        GetByTxId txid -> queryS txs $ TxSet.GetByTxId txid
        GetTxOut key -> queryS txs $ TxSet.GetTxOut key
        OneMeta txId -> queryS metas $ GetOne txId
        SomeMetas range limit order -> queryS metas
            $ GetSome range limit order
