{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2022 IOHK
License: Apache-2.0

Implementation of a 'QueryStore' for 'TxSet'.

-}
module Cardano.Wallet.DB.Store.Transactions.Layer
    ( QueryTxSet (..)
    , mkQueryStoreTxSet
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite.Types
    ( TxId )
import Cardano.Wallet.DB.Store.Transactions.Model
    ( DeltaTxSet, TxRelation, fromTxCollateralOut, fromTxOut )
import Control.Applicative
    ( (<|>) )
import Data.Word
    ( Word32 )
import Database.Persist.Sql
    ( SqlPersistT )

import Cardano.Wallet.DB.Store.QueryStore
    ( QueryStore (..) )
import qualified Cardano.Wallet.DB.Store.Transactions.Store as TxSet
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W

{-----------------------------------------------------------------------------
    DB for 'TxSet'
------------------------------------------------------------------------------}
data QueryTxSet b where
    GetByTxId :: TxId -> QueryTxSet (Maybe TxRelation)
    GetTxOut :: (TxId, Word32) -> QueryTxSet (Maybe W.TxOut)

-- | Implementation of a 'QueryStore' for 'TxSet'.
mkQueryStoreTxSet :: QueryStore (SqlPersistT IO) QueryTxSet DeltaTxSet
mkQueryStoreTxSet = QueryStore
    { queryS = \case
        GetByTxId txid -> TxSet.selectTx txid
        GetTxOut key -> do
            mout <- TxSet.selectTxOut key
            mcollateralOut <- TxSet.selectTxCollateralOut key
            pure $
                    (fromTxOut <$> mout)
                <|> (fromTxCollateralOut <$> mcollateralOut)
    , store = TxSet.mkStoreTransactions
    }
