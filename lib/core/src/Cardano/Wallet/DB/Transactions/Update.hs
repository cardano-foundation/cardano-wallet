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

{- |
 Copyright: © 2018-2020 IOHK
 License: Apache-2.0
-}
module Cardano.Wallet.DB.Transactions.Update (updateTxHistory) where

import Cardano.DB.Sqlite (
    dbChunked',
 )
import Cardano.Wallet.DB.Sqlite.Schema (
    Key (..),
    TxCollateral (..),
    TxCollateralOut (..),
    TxCollateralOutToken (..),
    TxIn (..),
    TxMeta (..),
    TxOut (..),
    TxOutToken (..),
    TxWithdrawal (..),
 )
import Cardano.Wallet.DB.Transactions.Model (mkTxHistory)
import Database.Persist.Sql (
    SqlPersistT,
    repsertMany,
 )
import Prelude

import Cardano.Wallet.DB.Transactions.Types (TxRelation)
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import Data.Generics.Product (position)
import Data.Functor.Identity (runIdentity)
import Data.Generics.Internal.VL ((^.))

{-------------------------------------------------------------------------------
    SQLite database operations
-------------------------------------------------------------------------------}
updateTxHistory :: W.WalletId -> [(W.Tx, W.TxMeta)] -> SqlPersistT IO ()
updateTxHistory wid = putTxs . mkTxHistory wid

-- | Insert multiple transactions, removing old instances first.
putTxs :: TxRelation -> SqlPersistT IO ()
putTxs es = do
    let ( txMetas,
          txIns,
          txCollateralIns,
          txOuts,
          txOutTokens,
          txCollateralOuts,
          txCollateralOutTokens,
          txWithdrawals
            ) = flatTxRelation es
    dbChunked'
        repsertMany
        [ (TxMetaKey txMetaTxId txMetaWalletId, m)
          | m@TxMeta{..} <- txMetas
        ]
    dbChunked'
        repsertMany
        [ (TxInKey txInputTxId txInputSourceTxId txInputSourceIndex, i)
          | i@TxIn{..} <- txIns
        ]
    dbChunked'
        repsertMany
        [ ( TxCollateralKey
                txCollateralTxId
                txCollateralSourceTxId
                txCollateralSourceIndex,
            i
          )
          | i@TxCollateral{..} <- txCollateralIns
        ]
    dbChunked'
        repsertMany
        [ (TxOutKey txOutputTxId txOutputIndex, o)
          | o@TxOut{..} <- txOuts
        ]
    dbChunked'
        repsertMany
        [ ( TxOutTokenKey
                txOutTokenTxId
                txOutTokenTxIndex
                txOutTokenPolicyId
                txOutTokenName,
            o
          )
          | o@TxOutToken{..} <- txOutTokens
        ]
    dbChunked'
        repsertMany
        [ (TxCollateralOutKey txCollateralOutTxId, o)
          | o@TxCollateralOut{..} <- txCollateralOuts
        ]
    dbChunked'
        repsertMany
        [ ( TxCollateralOutTokenKey
                txCollateralOutTokenTxId
                txCollateralOutTokenPolicyId
                txCollateralOutTokenName,
            o
          )
          | o@TxCollateralOutToken{..} <- txCollateralOutTokens
        ]
    dbChunked'
        repsertMany
        [ (TxWithdrawalKey txWithdrawalTxId txWithdrawalAccount, w)
          | w@TxWithdrawal{..} <- txWithdrawals
        ]
    where
    flatTxRelation es = (,,,,,,,)
        do es ^. position @1
        do runIdentity <$> es ^. position @2
        do runIdentity <$> es ^. position @3
        do fst <$> (es ^. position @4)
        do snd =<< (es ^. position @4)
        do fst <$> (es ^. position @5)
        do snd =<< (es ^. position @5)
        do es ^. position @6