{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant ^." -}
{- HLINT ignore "Use fst" -}
{- HLINT ignore "Use snd" -}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of the DBLayer which uses Persistent and SQLite.

module Cardano.Wallet.DB.Stores.TxHistory.Update
where

import Prelude

import Cardano.DB.Sqlite
    ( dbChunked' )
import Cardano.Wallet.DB.Sqlite.TH
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
import Database.Persist.Sql
    ( repsertMany )
import Database.Persist.Sqlite
    ( SqlPersistT )





{-------------------------------------------------------------------------------
    SQLite database operations
-------------------------------------------------------------------------------}




-- | Insert multiple transactions, removing old instances first.
putTxs
    :: [TxMeta]
    -> [TxIn]
    -> [TxCollateral]
    -> [TxOut]
    -> [TxOutToken]
    -> [TxCollateralOut]
    -> [TxCollateralOutToken]
    -> [TxWithdrawal]
    -> SqlPersistT IO ()
putTxs
    txMetas
    txIns
    txCollateralIns
    txOuts
    txOutTokens
    txCollateralOuts
    txCollateralOutTokens
    txWithdrawals = do
        dbChunked' repsertMany
            [ (TxMetaKey txMetaTxId txMetaWalletId, m)
            | m@TxMeta{..} <- txMetas]
        dbChunked' repsertMany
            [ (TxInKey txInputTxId txInputSourceTxId txInputSourceIndex, i)
            | i@TxIn{..} <- txIns ]
        dbChunked' repsertMany
            [ ( TxCollateralKey
                txCollateralTxId
                txCollateralSourceTxId
                txCollateralSourceIndex
              , i
              )
            | i@TxCollateral{..} <- txCollateralIns ]
        dbChunked' repsertMany
            [ (TxOutKey txOutputTxId txOutputIndex, o)
            | o@TxOut{..} <- txOuts ]
        dbChunked' repsertMany
            [ ( TxOutTokenKey
                txOutTokenTxId
                txOutTokenTxIndex
                txOutTokenPolicyId
                txOutTokenName
              , o
              )
            | o@TxOutToken{..} <- txOutTokens ]
        dbChunked' repsertMany
            [ (TxCollateralOutKey txCollateralOutTxId, o)
            | o@TxCollateralOut{..} <- txCollateralOuts ]
        dbChunked' repsertMany
            [ ( TxCollateralOutTokenKey
                txCollateralOutTokenTxId
                txCollateralOutTokenPolicyId
                txCollateralOutTokenName
              , o
              )
            | o@TxCollateralOutToken{..} <- txCollateralOutTokens ]
        dbChunked' repsertMany
            [ (TxWithdrawalKey txWithdrawalTxId txWithdrawalAccount, w)
            | w@TxWithdrawal{..} <- txWithdrawals ]



