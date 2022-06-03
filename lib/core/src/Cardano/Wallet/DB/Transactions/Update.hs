{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}

{- |
 Copyright: © 2018-2020 IOHK
 License: Apache-2.0
-}
module Cardano.Wallet.DB.Transactions.Update (updateTxHistory, putTxs) where

import Cardano.DB.Sqlite
    ( dbChunked' )
import Cardano.Wallet.DB.Sqlite.Schema
    ( Key (..)
    , TxCollateral (..)
    , TxCollateralOut (..)
    , TxCollateralOutToken (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxOutToken (..)
    , TxWithdrawal (..)
    )
import Cardano.Wallet.DB.Transactions.Model
    ( mkTxHistory )
import Data.Functor.Identity
    ( runIdentity )
import Database.Persist.Sql
    ( PersistEntity (PersistEntityBackend)
    , SqlBackend
    , SqlPersistT
    , repsertMany
    )
import Prelude

import Cardano.Wallet.DB.Transactions.Types
    ( TxMetaRelationF (..), TxRelationF (..), TxMetaRelation )
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Data.Foldable (fold)


updateTxHistory :: W.WalletId -> [(W.Tx, W.TxMeta)] -> SqlPersistT IO ()
updateTxHistory wid = putTxs . mkTxHistory wid

-- | Insert multiple transactions, removing old instances first.
putTxs :: TxMetaRelation -> SqlPersistT IO ()
putTxs (TxMetaRelationF mrs) = do
    let (metas,fold -> TxRelationF {..}) = unzip mrs
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

repsertX :: (PersistEntity record, PersistEntityBackend record ~ SqlBackend)
    => [record]
    -> (record -> Key record)
    -> SqlPersistT IO ()

repsertX xs f = dbChunked' repsertMany [(f x,x) | x <- xs ]
