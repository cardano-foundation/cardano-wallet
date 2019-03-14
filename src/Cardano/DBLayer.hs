{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.DBLayer
    ( DBLayer(..)
    ) where

import Cardano.Wallet
    ( Wallet )
import Control.Monad.Except
    ( ExceptT )
import Data.Word
    ( Word64 )
import GHC.TypeLits
    ( Symbol )


-- | A Database interface for storing various things in a DB. In practice,
-- we'll need some extra contraints on the wallet state that allows us to
-- serialize and unserialize it (e.g. @forall s. (Serialize s) => ...@)
data DBLayer m = forall s. DBLayer
    -- Wallet checkpoints, checkpoints are handled as a bounded FIFO, where we
    -- eventually store @k@ values (e.g. k=2160) at the same time.
    { enqueueCheckpoint -- Add a checkpoint on top of the queue
        :: PrimaryKey "wallet"
        -> Wallet s
        -> ExceptT ErrEnqueueCheckpoint m ()
    , dequeueCheckpoints -- Discard a number of checkpoints from the end
        :: PrimaryKey "wallet"
        -> Word64
        -> ExceptT ErrDequeueCheckpoints m ()
    , checkpoints --
        :: PrimaryKey "wallet"
        -> ExceptT ErrCheckpoints m [Wallet s]
    }

-- | A primary key which can take many forms depending on the value. This may
-- become a type family as we move forward, but for now, it illustrate that
-- some queries are ran against some sort of store;
--
-- As a matter of fact, we may manipulate multiple wallets at the same time, so,
-- functions like 'enqueueCheckpoint' needs to be associated to a corresponding
-- wallet. Some other may not because they are information valid for all wallets
-- (like for instance, the last known network tip).
data PrimaryKey (resource :: Symbol)

data ErrEnqueueCheckpoint
data ErrDequeueCheckpoints
data ErrCheckpoints
