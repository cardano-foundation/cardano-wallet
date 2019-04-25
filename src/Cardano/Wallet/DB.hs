{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Database / Pesistence layer for the wallet backend. This is where we define
-- the interface allowing us to store and fetch various data on our wallets.

module Cardano.Wallet.DB
    ( -- * Interface
      DBLayer(..)
    , PrimaryKey(..)

      -- * Errors
    , ErrNoSuchWallet(..)
    , ErrWalletAlreadyExists(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( Hash, Tx, TxMeta, WalletId, WalletMetadata )
import Control.Monad.Trans.Except
    ( ExceptT )
import Data.Map.Strict
    ( Map )


-- | A Database interface for storing various things in a DB. In practice,
-- we'll need some extra contraints on the wallet state that allows us to
-- serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@)
data DBLayer m s = DBLayer
    { createWallet
        :: PrimaryKey WalletId
        -> Wallet s
        -> WalletMetadata
        -> ExceptT ErrWalletAlreadyExists m ()
        -- ^ Initialize a database entry for a given wallet. 'putCheckpoint',
        -- 'putWalletMeta' or 'putTxHistory' will actually all fail if they are
        -- called _first_ on a wallet.

    , removeWallet
        :: PrimaryKey WalletId
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Remove a given wallet and all its associated data (checkpoints,
        -- metadata, tx history ...)

    , listWallets
        :: m [PrimaryKey WalletId]
        -- ^ Get the list of all known wallets in the DB, possibly empty.

    , putCheckpoint
        :: PrimaryKey WalletId
        -> Wallet s
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readCheckpoint
        :: PrimaryKey WalletId
        -> m (Maybe (Wallet s))
        -- ^ Fetch the most recent checkpoint of a given wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , putWalletMeta
        :: PrimaryKey WalletId
        -> WalletMetadata
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Replace an existing wallet metadata with the given one.
        --
        -- If the wallet doesn't exist, this operation returns an error

    , readWalletMeta
        :: PrimaryKey WalletId
        -> m (Maybe WalletMetadata)
        -- ^ Fetch a wallet metadata, if they exist.
        --
        -- Return 'Nothing' if there's no such wallet.

    , putTxHistory
        :: PrimaryKey WalletId
        -> Map (Hash "Tx") (Tx, TxMeta)
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readTxHistory
        :: PrimaryKey WalletId
        -> m (Map (Hash "Tx") (Tx, TxMeta))
        -- ^ Fetch the current transaction history of a known wallet.
        --
        -- Returns an empty map if the wallet isn't found.

    , withLock
        :: forall e a. ()
        => ExceptT e m a
        -> ExceptT e m a
    }

-- | Can't perform given operation because there's no wallet
newtype ErrNoSuchWallet
    = ErrNoSuchWallet WalletId -- Wallet is gone or doesn't exist yet
    deriving (Eq, Show)

-- | Forbidden operation was executed on an already existing wallet
newtype ErrWalletAlreadyExists
    = ErrWalletAlreadyExists WalletId -- Wallet already exists in db
    deriving (Eq, Show)

-- | A primary key which can take many forms depending on the value. This may
-- become a type family as we move forward, but for now, it illustrate that
-- some queries are ran against some sort of store;
--
-- As a matter of fact, we may manipulate multiple wallets at the same time, so,
-- functions like 'enqueueCheckpoint' needs to be associated to a corresponding
-- wallet. Some other may not because they are information valid for all wallets
-- (like for instance, the last known network tip).
newtype PrimaryKey key = PrimaryKey key
    deriving (Eq, Ord)
