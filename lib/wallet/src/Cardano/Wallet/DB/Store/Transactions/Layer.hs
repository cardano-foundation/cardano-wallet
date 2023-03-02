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
    ( ReadTxSet (..)
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
data ReadTxSet b where
    GetByTxId :: TxId -> ReadTxSet (Maybe TxRelation)

-- | Implementation of a 'QueryStore' for 'TxSet'.
mkQueryStoreTxSet :: QueryStore (SqlPersistT IO) ReadTxSet DeltaTxSet
mkQueryStoreTxSet = QueryStore
    { queryS = \case
        GetByTxId txid -> TxSet.selectTx txid
    , store = TxSet.mkStoreTransactions
    }
