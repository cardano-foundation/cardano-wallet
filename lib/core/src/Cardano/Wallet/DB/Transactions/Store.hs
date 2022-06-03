{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Wallet.DB.Store.Transaction.Store where

import Cardano.Wallet.DB
    ( ErrNoSuchWallet (ErrNoSuchWallet) )
import Cardano.Wallet.DB.Transactions.Types
    ( DeltaTxHistory (DeltaTxHistory), TxHistory (TxHistory) )
import Cardano.Wallet.DB.Transactions.Update
    ( updateTxHistory )
import Cardano.Wallet.DB.Unstored
    ( selectWallet )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( SomeException, throw )
import Data.DBVar
    ( Store (Store, loadS, updateS, writeS) )
import Database.Persist.Sql
    ( SqlPersistT )
import Prelude

mkStoreTransactions :: WalletId -> Store (SqlPersistT IO) DeltaTxHistory
mkStoreTransactions wid = Store
    {loadS = load wid, writeS = write wid, updateS = update wid}

update :: WalletId -> TxHistory -> DeltaTxHistory -> SqlPersistT IO ()
update wid _ (DeltaTxHistory (TxHistory txs)) =
    selectWallet wid >>= \case
        Nothing -> throw $ ErrNoSuchWallet wid
        Just _ -> updateTxHistory wid txs

write :: WalletId -> TxHistory -> SqlPersistT IO ()
write = error "write tx history not implemented"

load :: WalletId -> SqlPersistT IO (Either SomeException TxHistory)
load = error "load tx history not implemented"
-- data Store m da = Store
--     { loadS   :: m (Either SomeException (Base da))
--     , writeS  :: Base da -> m ()
--     , updateS
--         :: Base da -- old value
--         -> da -- delta to new value
--         -> m () -- write new value
--     }
-- | A delta can be optionally applied.
