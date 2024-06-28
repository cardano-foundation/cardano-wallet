{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
module Cardano.Wallet.Deposit.IO.Address.Store
    ( mkStoreAddressState
    )
    where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Pure.Address
    ( AddressState (..)
    )
import Control.Exception
    ( Exception
    , SomeException (SomeException)
    )
import Data.Delta
    ( Delta (..)
    )
import Data.Store
    ( UpdateStore
    , mkUpdateStore
    , updateLoad
    )
import Database.Table
    ( Col (..)
    , Row
    , Table
    , (:.)
    )

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.Map.Strict as Map
import qualified Database.SQLite.Simple as Sqlite
import qualified Database.Table.SQLite.Simple as Sql

{-----------------------------------------------------------------------------
    Delta type
    TODO: move out
------------------------------------------------------------------------------}

data DeltaAddressState
    = InsertAddressCustomer Read.Address Customer

instance Delta DeltaAddressState where
    type Base DeltaAddressState = AddressState
    apply (DeltaAddressState addr c) s@AddressStateC{addresses}
        = s {addresses = Map.insert addr c addresses}

{-----------------------------------------------------------------------------
    Database schema
------------------------------------------------------------------------------}

type TableAddressesMap =
    Table "deposit_addresses"
    :. Col "address" Address
    :. Col "customer" Customer

tableAddressesMap :: Proxy TableAddressesMap
tableAddressesMap = Proxy

type TableAddressState =
    Table "deposit_xpub_change"
    :. Col "xpub" XPub
    :. Col "change" Address

tableAddressState :: Proxy TableAddressState
tableAddressState = Proxy

{-----------------------------------------------------------------------------
    Store
------------------------------------------------------------------------------}

mkStoreAddressState :: UpdateStore Sql.SqlM DeltaAddressState
mkStoreAddressState = mkUpdateStore loadS' writeS' updateS'

data ErrStoreAddressState
    = ErrTableAddressState
    deriving Show

instance Exception ErrStoreAddressState

loadS' :: Sql.SqlM (Either SomeException AddressState)
loadS' = do
    -- TODO: use `catch` or some form of wrapping the exception
    addresses <- Map.fromList $ Sql.selectAll tableAddressesMap
    [(stateXPub, change)] <- Sql.selectAll tableAddressState
    pure $ AddressStateC{addresses, stateXPub, change}

writeS' :: AddressState -> Sql.SqlM ()
writeS' AddressStateC{addresses,stateXPub,change} = do
    Sql.deleteAll tableAddressesMap
    Sql.insertMany (Map.toList addresses) tableAddressesMap
    Sql.deleteAll tableAddressState
    Sql.insertOne (stateXPub, change) tableAddressState

updateS'
    :: Maybe AddressState
    -> DeltaAddressState
    -> Sql.SqlM ()
updateS' _ (InsertAddressCustomer addr customer) =
    Sql.insertOne (addr, customer) tableAddressesMap
