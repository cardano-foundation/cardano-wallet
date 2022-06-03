
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.DB.Transactions.Store where

import Prelude

import Cardano.Wallet.DB.Transactions.Delete
    ( deletePendingOrExpiredTx, taintExpiredTx )
import Cardano.Wallet.DB.Transactions.Delta
    ( DeltaTxHistory (AgeTxHistory, ExpandTxHistory, PruneTxHistory) )
import Cardano.Wallet.DB.Transactions.Select
    ( selectWalletTxRelation )
import Cardano.Wallet.DB.Transactions.Types
    ( TxHistory )
import Cardano.Wallet.DB.Transactions.Update
    ( putTxs )
import Cardano.Wallet.DB.Unstored
    ( overWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( SomeException )
import Control.Monad
    ( void )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Database.Persist.Sql
    ( SqlPersistT )

mkStoreTransactions :: WalletId -> Store (SqlPersistT IO) DeltaTxHistory
mkStoreTransactions wid =
    Store
        { loadS = load wid
        , writeS = write wid
        , updateS = update wid
        }

update :: WalletId -> TxHistory -> DeltaTxHistory -> SqlPersistT IO ()
update wid _ change = overWallet wid $ \_wallet -> case change of
    ExpandTxHistory txs -> putTxs txs
    PruneTxHistory tid -> void $ deletePendingOrExpiredTx wid tid
    AgeTxHistory tip -> taintExpiredTx wid tip

write :: WalletId -> TxHistory -> SqlPersistT IO ()
write = error "write tx history not implemented"

load :: WalletId -> SqlPersistT IO (Either SomeException TxHistory)
load wid = Right <$> selectWalletTxRelation wid



