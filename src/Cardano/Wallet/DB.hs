-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Database / Pesistence layer for the wallet backend. This is where we define
-- the interface allowing us to store and fetch various data on our wallets.

module Cardano.Wallet.DB
    ( DBLayer(..)
    , PrimaryKey(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Model
    ( Wallet, WalletId )
import Data.List.NonEmpty
    ( NonEmpty )


-- | A Database interface for storing various things in a DB. In practice,
-- we'll need some extra contraints on the wallet state that allows us to
-- serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@)
data DBLayer m s = DBLayer
    -- Wallet checkpoints, checkpoints are handled as a bounded FIFO, where we
    -- eventually store @k@ values (e.g. k=2160) at the same time.
    { putCheckpoints
        :: PrimaryKey WalletId
        -> NonEmpty (Wallet s)
        -> m ()

    , readCheckpoints
        :: PrimaryKey WalletId
        -> m (Maybe (NonEmpty (Wallet s)))

    , readWallets
        :: m [PrimaryKey WalletId]
    }

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
