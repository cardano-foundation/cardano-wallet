{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.UTxOHistory.Store
    ( mkStoreUTxOHistory
    ) where

import Prelude

import Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( DeltaUTxOHistory (..), UTxOHistory)
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Data.Store
    ( UpdateStore, mkUpdateStore )
import Database.Persist.Sql
    ( SqlPersistT )
import GHC.Exception
    ( SomeException )

-- | Create a 'Store' for 'UTxOHistory' using the given 'WalletId' as a key.
mkStoreUTxOHistory
    :: WalletId
    -> UpdateStore (SqlPersistT IO) DeltaUTxOHistory
mkStoreUTxOHistory wid = mkUpdateStore
    (load wid)
    (write wid)
    (update wid)

load :: WalletId -> SqlPersistT IO (Either SomeException UTxOHistory)
load _wid = undefined

write :: WalletId -> UTxOHistory -> SqlPersistT IO ()
write _wid = undefined

update :: WalletId -> Maybe UTxOHistory -> DeltaUTxOHistory -> SqlPersistT IO ()
update _wid = undefined
