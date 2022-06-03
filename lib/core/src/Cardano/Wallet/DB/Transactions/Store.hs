{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.DB.Transactions.Store where

import Cardano.Wallet.DB
    ( ErrNoSuchWallet (ErrNoSuchWallet) )
import Cardano.Wallet.DB.Transactions.Select
    ()
import Cardano.Wallet.DB.Transactions.Types
    ( DeltaTxHistory (DeltaTxHistory), TxHistory (TxHistory) )
import Cardano.Wallet.DB.Transactions.Update
    ( updateTxHistory )
import Cardano.Wallet.DB.Unstored
    ( selectWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( SomeException, throw, Exception )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Database.Persist.Sql
    ( SqlPersistT )
import Prelude
import Data.Typeable (Typeable)

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
            $ Exceptional 
            $ ErrNoSuchWallet wid
        Just _ -> updateTxHistory wid txs

write :: WalletId -> TxHistory -> SqlPersistT IO ()
write = error "write tx history not implemented"

load :: WalletId -> SqlPersistT IO (Either SomeException TxHistory)
load = fmap (Right . TxHistory) . undefined

newtype  Exceptional a = Exceptional a 
    deriving (Show,Eq)

instance (Typeable a, Show a) 
    => Exception (Exceptional a)

