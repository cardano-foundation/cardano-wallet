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
    ( DeltaTxSet, TxRelation )
import Database.Persist.Sql
    ( SqlPersistT )

import Cardano.Wallet.DB.Store.QueryStore
    ( QueryStore (..) )
import qualified Cardano.Wallet.DB.Store.Transactions.Store as TxSet

{-----------------------------------------------------------------------------
    DB for 'TxSet'
------------------------------------------------------------------------------}
data QueryTxSet b where
    GetByTxId :: TxId -> QueryTxSet (Maybe TxRelation)

-- | Implementation of a 'QueryStore' for 'TxSet'.
mkQueryStoreTxSet :: QueryStore (SqlPersistT IO) QueryTxSet DeltaTxSet
mkQueryStoreTxSet = QueryStore
    { queryS = \case
        GetByTxId txid -> TxSet.selectTx txid
    , store = TxSet.mkStoreTransactions
    }
