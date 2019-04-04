{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Database / Pesistence layer for the wallet backend. This is where we define
-- the interface allowing us to store and fetch various data on our wallets.

module Cardano.Wallet.DB
    ( DBLayer(..)
    , PrimaryKey(..)
    , ErrPutTxHistory(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( Hash, Tx, TxMeta, WalletId )
import Control.Monad.Trans.Except
    ( ExceptT )
import Data.Map.Strict
    ( Map )


-- | A Database interface for storing various things in a DB. In practice,
-- we'll need some extra contraints on the wallet state that allows us to
-- serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@)
data DBLayer m s = DBLayer
    { putCheckpoint
        :: PrimaryKey WalletId
        -> Wallet s
        -> m ()
        -- ^ Replace the current checkpoint for a given wallet. We do not handle
        -- rollbacks yet, and therefore only stores the latest available
        -- checkpoint.

    , readCheckpoint
        :: PrimaryKey WalletId
        -> m (Maybe (Wallet s))
        -- ^ Fetch the most recent checkpoint of a given wallet. Return 'Nothing'
        -- if there's no such wallet.

    , readWallets
        :: m [PrimaryKey WalletId]
        -- ^ Get the list of all known wallets in the DB, possibly empty.

    , putTxHistory
        :: PrimaryKey WalletId
        -> Map (Hash "Tx") (Tx, TxMeta)
        -> ExceptT ErrPutTxHistory m ()
        -- ^ Augments the transaction history for a known wallet.
        --
        -- If an entry for a particular transaction already exists it is not
        -- altered nor merged (just ignored).
        --
        -- If the wallet doesn't exist, this operation returns an error.

    , readTxHistory
        :: PrimaryKey WalletId
        -> m (Map (Hash "Tx") (Tx, TxMeta))
        -- ^ Fetch the current transaction history of a known wallet. Returns an
        -- empty map if the wallet isn't found.
    }

-- | Error while trying to insert transaction history in the DB.
newtype ErrPutTxHistory
    = ErrNoSuchWallet WalletId
    deriving (Show, Eq)

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
