{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Cardano.Wallet.DB.Store.PrivateKey.Store
    ( mkStorePrivateKey
    , DeltaPrivateKey
    , StorePrivateKey
    ) where

import Prelude

import Cardano.Wallet.Address.Keys.PersistPrivateKey
    ( serializeXPrv
    , unsafeDeserializeXPrv
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( HashedCredentials
    , RootCredentials (..)
    )
import Control.Exception
    ( SomeException (..)
    )
import Data.Delta
    ( Replace
    )
import Data.Functor
    ( (<&>)
    )
import Data.Store
    ( SimpleStore
    , mkSimpleStore
    )
import Database.Persist
    ( Entity (entityVal)
    , Filter
    , PersistQueryRead (selectFirst)
    , insert_
    )
import Database.Persist.Sql
    ( SqlPersistT
    , deleteWhere
    )

import qualified Cardano.Wallet.DB.Sqlite.Schema as Schema

-- | A 'Store' for 'PrivateKey'.
type StorePrivateKey k =
    SimpleStore
        (SqlPersistT IO)
        (Maybe (HashedCredentials k))

-- | A 'Delta' for 'PrivateKey'.
type DeltaPrivateKey k = Replace (Maybe (HashedCredentials k))

-- | Construct a 'StorePrivateKey' for a given 'KeyFlavor'.  ATM a WalletId is
-- required to be able to store the private key.  This limitation is due to the
-- fact that the table for the private key requires a foreign key to the table
-- of the wallet.
mkStorePrivateKey
    :: forall k
     . KeyFlavorS k
    -> WalletId
    -> StorePrivateKey k
mkStorePrivateKey kF wid = mkSimpleStore load write
  where
    load
        :: SqlPersistT
            IO
            (Either SomeException (Maybe (HashedCredentials k)))
    load = do
        keys <- selectFirst [] []
        pure $ Right $ keys <&> \key ->
            privateKeyFromEntity $ entityVal key
      where
        privateKeyFromEntity :: Schema.PrivateKey -> HashedCredentials k
        privateKeyFromEntity (Schema.PrivateKey _ k h) =
            uncurry RootCredentials $ unsafeDeserializeXPrv kF (k, h)

    write :: Maybe (HashedCredentials k) -> SqlPersistT IO ()
    write (Just key) = do
        deleteWhere ([] :: [Filter Schema.PrivateKey])
        insert_ (mkPrivateKeyEntity key)
    write Nothing = deleteWhere ([] :: [Filter Schema.PrivateKey])

    mkPrivateKeyEntity (RootCredentials k h) =
        Schema.PrivateKey
            { Schema.privateKeyWalletId = wid
            , Schema.privateKeyRootKey = root
            , Schema.privateKeyHash = hash
            }
      where
        (root, hash) = serializeXPrv kF (k, h)
