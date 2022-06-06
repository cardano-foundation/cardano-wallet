
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Wallet.DB.Transactions.Store where

import Prelude

import Cardano.DB.Sqlite
    ( dbChunked' )
import Cardano.Wallet.DB.Sqlite.Schema
    ( EntityField (..)
    , Key (..)
    , TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (TxId) )
import Cardano.Wallet.DB.Transactions.Delta
    ( DeltaTxHistory (..) )
import Cardano.Wallet.DB.Transactions.Select
    ( selectWalletTxRelation )
import Cardano.Wallet.DB.Transactions.Types
    ( TxHistory, TxHistoryF (TxHistoryF), TxRelationF (..) )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( SomeException )
import Control.Monad
    ( void )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Data.Foldable
    ( Foldable (toList), fold )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Database.Persist.Sql
    ( PersistEntity (PersistEntityBackend)
    , PersistQueryRead (selectFirst)
    , PersistStoreWrite (repsertMany)
    , SqlBackend
    , SqlPersistT
    , deleteWhere
    , deleteWhereCount
    , updateWhere
    , (<-.)
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    )

import qualified Cardano.Wallet.Primitive.Types.Tx as W

mkStoreTransactions :: WalletId -> Store (SqlPersistT IO) DeltaTxHistory
mkStoreTransactions wid =
    Store
        { loadS = load wid
        , writeS = write wid
        , updateS = update wid
        }

update :: WalletId -> TxHistory -> DeltaTxHistory -> SqlPersistT IO ()
update wid _ change = case change of
    ExpandTxHistory txs -> putTxs txs
    PruneTxHistory tid -> do
        let filt = [ TxMetaWalletId ==. wid, TxMetaTxId ==. (TxId tid) ]
        selectFirst ((TxMetaStatus ==. W.InLedger) : filt) [] >>= \case   
            Just _ -> pure () -- marked in ledger - refuse to delete
            Nothing -> void
                $ deleteWhereCount
                $ (TxMetaStatus <-. [W.Pending, W.Expired]) : filt
    AgeTxHistory tip ->
        updateWhere isExpired [TxMetaStatus =. W.Expired]
          where
            isExpired =
                [ TxMetaWalletId ==. wid
                , TxMetaStatus ==. W.Pending
                , TxMetaSlotExpires <=. Just tip 
                ]
    RollBackTxHistory nearestPoint -> do
        updateWhere
            [ TxMetaWalletId ==. wid
            , TxMetaDirection ==. W.Outgoing
            , TxMetaSlot >. nearestPoint
            ]
            [ TxMetaStatus =. W.Pending
            , TxMetaSlot =. nearestPoint
            ]
        deleteWhere
            [ TxMetaWalletId ==. wid
            , TxMetaDirection ==. W.Incoming
            , TxMetaSlot >. nearestPoint
            ]

write :: WalletId -> TxHistory -> SqlPersistT IO ()
write wid txs = do  
    deleteWhere [ TxMetaWalletId ==. wid ]
    putTxs txs

load :: WalletId -> SqlPersistT IO (Either SomeException TxHistory)
load wid = Right <$> selectWalletTxRelation wid

-- | Insert multiple transactions, removing old instances first.
putTxs :: TxHistory -> SqlPersistT IO ()
putTxs (TxHistoryF mrs) = do
    let (metas,fold -> TxRelationF {..}) = unzip $ toList mrs
    repsertX
        do metas
        do \TxMeta {..} -> TxMetaKey txMetaTxId txMetaWalletId
    repsertX
        do runIdentity <$> txRelation_ins
        do \TxIn{..} -> TxInKey txInputTxId txInputSourceTxId txInputSourceIndex
    repsertX
        do runIdentity <$> txRelation_colls
        do \TxCollateral{..} ->
              TxCollateralKey
                  txCollateralTxId
                  txCollateralSourceTxId
                  txCollateralSourceIndex
    repsertX
        do fst <$> txRelation_outs
        do \TxOut{..} ->
            TxOutKey txOutputTxId txOutputIndex
    repsertX
        do txRelation_outs >>= snd
        do \TxOutToken{..} ->  TxOutTokenKey
                  txOutTokenTxId
                  txOutTokenTxIndex
                  txOutTokenPolicyId
                  txOutTokenName
    repsertX
        do fst <$> txRelation_collouts
        do \TxCollateralOut{..} ->
            TxCollateralOutKey txCollateralOutTxId
    repsertX
        do txRelation_collouts >>= snd
        do \TxCollateralOutToken{..} ->
            TxCollateralOutTokenKey
                txCollateralOutTokenTxId
                txCollateralOutTokenPolicyId
                txCollateralOutTokenName
    repsertX
        do txRelation_withdraws
        do \TxWithdrawal{..} ->
            TxWithdrawalKey txWithdrawalTxId txWithdrawalAccount

repsertX
    :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
    => [record]
    -> (record -> Key record)
    -> SqlPersistT IO ()

repsertX xs f = dbChunked' repsertMany [(f x,x) | x <- xs ]
