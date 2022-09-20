{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: 2022 IOHK
License: Apache-2.0

Implementation of a 'Store' for 'TxHistoryWithCBOR'.

-}
module Cardano.Wallet.DB.Store.TransactionsWithCBOR.Store
 where

import Prelude

import Cardano.Wallet.DB.Store.CBOR.Store
    ( mkStoreCBOR )
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions )
import Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model
    ( DeltaTx (Append, DeleteTx), TxHistoryWithCBOR (TxHistoryWithCBOR) )
import Control.Monad.Except
    ( ExceptT (..), runExceptT )
import Data.DBVar
    ( Store (..) )
import Database.Persist.Sql
    ( SqlPersistT )

import qualified Cardano.Wallet.DB.Store.CBOR.Model as CBOR
import qualified Cardano.Wallet.DB.Store.Transactions.Model as Txs
import qualified Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model as WithCBOR

mkStoreTransactionsWithCBOR
    :: Store (SqlPersistT IO) WithCBOR.DeltaTx
mkStoreTransactionsWithCBOR =
    Store
    { loadS = runExceptT $ TxHistoryWithCBOR
                <$> ExceptT (loadS mkStoreTransactions)
                <*> ExceptT (loadS mkStoreCBOR)
    , writeS = \(TxHistoryWithCBOR txs cbors) -> do
                writeS mkStoreTransactions txs
                writeS mkStoreCBOR cbors
    , updateS = \(TxHistoryWithCBOR oldtxs oldcbors) -> \case
        Append (TxHistoryWithCBOR newtxs newcbors) -> do
            updateS mkStoreTransactions oldtxs (Txs.Append newtxs)
            updateS mkStoreCBOR oldcbors (CBOR.Append newcbors)
        DeleteTx tid  -> do
            updateS mkStoreTransactions oldtxs (Txs.DeleteTx tid)
            updateS mkStoreCBOR oldcbors (CBOR.DeleteTx tid)
    }

