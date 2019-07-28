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
    , cleanDB

      -- * Errors
    , ErrNoSuchWallet(..)
    , ErrWalletAlreadyExists(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, XPrv )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( DefineTx (..)
    , Hash
    , Range (..)
    , SlotId (..)
    , SortOrder (..)
    , TxMeta
    , WalletId
    , WalletMetadata
    )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.Map.Strict
    ( Map )


-- | A Database interface for storing various things in a DB. In practice,
-- we'll need some extra contraints on the wallet state that allows us to
-- serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@)
data DBLayer m s t = DBLayer
    { createWallet
        :: PrimaryKey WalletId
        -> Wallet s t
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
        -> Wallet s t
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readCheckpoint
        :: PrimaryKey WalletId
        -> m (Maybe (Wallet s t))
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
        :: (DefineTx t)
        => PrimaryKey WalletId
        -> Map (Hash "Tx") (Tx t, TxMeta)
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readTxHistory
        :: PrimaryKey WalletId
        -> SortOrder
        -> Range SlotId
        -> m [(Hash "Tx", (Tx t, TxMeta))]
        -- ^ Fetch the current transaction history of a known wallet, ordered by
        -- descending slot number.
        --
        -- Returns an empty list if the wallet isn't found.

    , putPrivateKey
        :: PrimaryKey WalletId
        -> (Key 'RootK XPrv, Hash "encryption")
        -> ExceptT ErrNoSuchWallet m ()
        -- ^ Store or replace a private key for a given wallet. Note that wallet
        -- _could_ be stored and manipulated without any private key associated
        -- to it. A private key is only seldomly required for very specific
        -- operations (like transaction signing).

    , readPrivateKey
        :: PrimaryKey WalletId
        -> m (Maybe (Key 'RootK XPrv, Hash "encryption"))
        -- ^ Read a previously stored private key and its associated passphrase
        -- hash.

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

-- | Clean a database by removing all wallets.
cleanDB :: Monad m => DBLayer m s t -> m ()
cleanDB db = listWallets db >>= mapM_ (runExceptT . removeWallet db)
