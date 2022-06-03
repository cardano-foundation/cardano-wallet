{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.DB.Transactions.Store where

import Cardano.Wallet.DB
    ( ErrNoSuchWallet (ErrNoSuchWallet) )
import Cardano.Wallet.DB.Transactions.Select
    ( selectWalletTxRelation )
import Cardano.Wallet.DB.Transactions.Types
    ( DeltaTxHistory (DeltaTxHistory), TxHistory (TxHistory) )
import Cardano.Wallet.DB.Transactions.Update
    ( putTxs )
import Cardano.Wallet.DB.Unstored
    ( selectWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( Exception (toException), SomeException, throw )
import Control.Monad.Exception.Unchecked
    ( Unchecked (Unchecked) )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Database.Persist.Sql
    ( SqlPersistT )
import Prelude

mkStoreTransactions :: WalletId -> Store (SqlPersistT IO) DeltaTxHistory
mkStoreTransactions wid =
    Store
        { loadS = load wid
        , writeS = write wid
        , updateS = update wid
        }

update :: WalletId -> TxHistory -> DeltaTxHistory -> SqlPersistT IO ()
update wid _ (DeltaTxHistory (TxHistory txs)) =
    selectWallet wid >>= \case
        Nothing -> throw 
            $ toException
            $ Unchecked
            $ ErrNoSuchWallet wid
        Just _ -> putTxs txs

write :: WalletId -> TxHistory -> SqlPersistT IO ()
write = error "write tx history not implemented"

load :: WalletId -> SqlPersistT IO (Either SomeException TxHistory)
load wid = Right . TxHistory <$> selectWalletTxRelation wid



