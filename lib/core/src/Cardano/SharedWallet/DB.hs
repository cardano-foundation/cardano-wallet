{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Database / Persistence layer for the shared wallet.

module Cardano.SharedWallet.DB
    ( -- * Interface
      DBLayer (..)

      -- * Errors
    , ErrNoSuchSharedWallet (..)
    , ErrSharedWalletAlreadyExists (..)
    ) where

import Prelude

import Cardano.SharedWallet.Shared
    ( SharedWallet )
import Cardano.Wallet.Primitive.Types
    ( GenesisParameters, WalletId, WalletMetadata )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.Trans.Except
    ( ExceptT )

-- | A Database interface for storing shared wallet state in DB.
--
-- To use it, you will need the NamedFieldPuns extension and wrap operations
-- with @atomically@:
--
-- Example:
--
-- >>> :set -XNamedFieldPuns
-- >>> DBLayer{atomically,putPoolProduction} = db
-- >>> atomically $ removeSharedWallet walletId
--
-- This gives you the power to also run /multiple/ operations atomically.
data DBLayer m k = forall stm. (MonadFail stm, MonadIO stm) => DBLayer
    { initializeSharedState
        :: WalletId
        -> SharedWallet k
        -> WalletMetadata
        -> GenesisParameters
        -> ExceptT ErrSharedWalletAlreadyExists stm ()
        -- ^ Create the shared wallet.

    , removeSharedWallet
        :: WalletId
        -> ExceptT ErrNoSuchSharedWallet stm ()
        -- ^ Remove a given share wallet and all its associated data.

    , readSharedWalletState
        :: WalletId
        -> stm (Maybe (SharedWallet k))
        -- ^ Fetch the most recent state of a given shared wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , readSharedWalletMetadata
        :: WalletId
        -> stm (Maybe WalletMetadata)
        -- ^ Fetch the most recent metadata of a given shared wallet.
        --
        -- Return 'Nothing' if there's no such wallet.

    , cleanDB
        :: stm ()
        -- ^ Clean a database

    , atomically
        :: forall a. stm a -> m a
        -- ^ Run an operation.
        --
        -- For a Sqlite DB, this would be "run a query inside a transaction".
    }

-- | Can't perform given operation because there's no wallet
newtype ErrNoSuchSharedWallet
    = ErrNoSuchSharedWallet WalletId -- Wallet is gone or doesn't exist yet
    deriving (Eq, Show)

-- | Forbidden operation was executed on an already existing wallet
newtype ErrSharedWalletAlreadyExists
    = ErrSharedWalletAlreadyExists WalletId -- Wallet already exists in db
    deriving (Eq, Show)
