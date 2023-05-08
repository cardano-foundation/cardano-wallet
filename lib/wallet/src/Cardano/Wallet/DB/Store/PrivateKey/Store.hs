{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Cardano.Wallet.DB.Store.PrivateKey.Store
    ( mkStorePrivateKey
    , DeltaPrivateKey
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Address.Derivation
    ( Depth (..) )
import Cardano.Wallet.Address.Keys.PersistPrivateKey
    ( serializeXPrv, unsafeDeserializeXPrv )
import Cardano.Wallet.DB.Errors
    ( ErrWalletNotInitialized (ErrWalletNotInitialized) )
import Cardano.Wallet.Flavor
    ( KeyFlavorS )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( PassphraseHash )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Control.Exception
    ( SomeException (..) )
import Data.Delta
    ( Replace )
import Data.Store
    ( SimpleStore, mkSimpleStore )
import Database.Persist
    ( Entity (entityVal), Filter, PersistQueryRead (selectFirst), insert_ )
import Database.Persist.Sql
    ( SqlPersistT, deleteWhere )

import qualified Cardano.Wallet.DB.Sqlite.Schema as Schema

-- | A 'PrivateKey' for a given 'KeyFlavor'.
data PrivateKey k = PrivateKey (k 'RootK XPrv) PassphraseHash

-- | A 'Store' for 'PrivateKey'.
type StorePrivateKey k = SimpleStore (SqlPersistT IO) (PrivateKey k)

-- | A 'Delta' for 'PrivateKey'.
type DeltaPrivateKey k = Replace (PrivateKey k)

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
        load :: SqlPersistT IO (Either SomeException (PrivateKey k))
        load = do
            keys <- selectFirst [] []
            case keys of
                Nothing -> pure $ Left $ SomeException ErrWalletNotInitialized
                Just key -> pure $ Right $ privateKeyFromEntity $ entityVal key
            where
                privateKeyFromEntity :: Schema.PrivateKey -> PrivateKey k
                privateKeyFromEntity (Schema.PrivateKey _ k h) =
                    uncurry PrivateKey $ unsafeDeserializeXPrv kF (k, h)

        write :: PrivateKey k -> SqlPersistT IO ()
        write key = do
            deleteWhere ([] :: [Filter Schema.PrivateKey])
            insert_ (mkPrivateKeyEntity key)

        mkPrivateKeyEntity (PrivateKey k h) =
            Schema.PrivateKey
                { Schema.privateKeyWalletId = wid
                , Schema.privateKeyRootKey = root
                , Schema.privateKeyHash = hash
                }
          where
            (root, hash) = serializeXPrv kF (k, h)
