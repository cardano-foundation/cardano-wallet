
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DB.Transactions.Meta.Store where

import Prelude

import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..), TxMeta (..) )
import Cardano.Wallet.DB.Transactions.Meta.Model
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( SomeException )
import Control.Monad
    ( void )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Data.Foldable
    ( Foldable (toList) )
import Database.Persist.Sql
    ( Entity (entityVal)
    , PersistEntity (keyFromRecordM)
    , PersistQueryRead (selectFirst)
    , SelectOpt (Desc)
    , SqlPersistT
    , deleteWhere
    , deleteWhereCount
    , repsertMany
    , selectList
    , updateWhere
    , (<-.)
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    )

import Control.Arrow
    ( (&&&) )
import qualified Data.Map.Strict as Map
import Data.Maybe
    ( fromJust )

import qualified Cardano.Wallet.Primitive.Types.Tx as W

mkStoreTransactionsMeta
    :: WalletId
    -> Store (SqlPersistT IO) (DeltaTxMetaHistoryAny)
mkStoreTransactionsMeta wid =
    Store
        { loadS = load wid
        , writeS = write wid
        , updateS = update wid
        }

update
    :: WalletId
    -> TxMetaHistory
    -> DeltaTxMetaHistoryAny
    -> SqlPersistT IO ()
update wid _ change = case change of
    DeltaTxMetaHistoryAny (ExpandTxMetaHistory txs) -> putMetas txs
    DeltaTxMetaHistoryAny (PruneTxMetaHistory tid) -> do
        let filt = [ TxMetaWalletId ==. wid, TxMetaTxId ==. tid ]
        selectFirst ((TxMetaStatus ==. W.InLedger) : filt) [] >>= \case
            Just _ -> pure () -- marked in ledger - refuse to delete
            Nothing -> void
                $ deleteWhereCount
                $ (TxMetaStatus <-. [W.Pending, W.Expired]) : filt
    DeltaTxMetaHistoryAny (AgeTxMetaHistory tip) ->
        updateWhere
            [ TxMetaWalletId ==. wid
            , TxMetaStatus ==. W.Pending
            , TxMetaSlotExpires <=. Just tip
            ]
            [TxMetaStatus =. W.Expired]
    DeltaTxMetaHistoryAny (RollBackTxMetaHistory nearestPoint) -> do
        deleteWhere
            [ TxMetaWalletId ==. wid
            , TxMetaDirection ==. W.Incoming
            , TxMetaSlot >. nearestPoint
            ]
        updateWhere
            [ TxMetaWalletId ==. wid
            , TxMetaDirection ==. W.Outgoing
            , TxMetaSlot >. nearestPoint
            ]
            [ TxMetaStatus =. W.Pending
            , TxMetaSlot =. nearestPoint
            ]

write :: WalletId -> TxMetaHistory -> SqlPersistT IO ()
write wid txs = do
    deleteWhere [ TxMetaWalletId ==. wid ]
    putMetas txs

load :: WalletId -> SqlPersistT IO (Either SomeException TxMetaHistory)
load wid = Right
    . TxMetaHistory
    . Map.fromList
    . fmap ((txMetaTxId &&& id) . entityVal) <$> selectList
        [ TxMetaWalletId ==. wid]
        [ Desc TxMetaSlot ]

-- | Insert multiple transactions, removing old instances first.
putMetas :: TxMetaHistory -> SqlPersistT IO ()
putMetas (TxMetaHistory metas) = repsertMany
        [(fromJust keyFromRecordM x, x) | x <- toList metas]


