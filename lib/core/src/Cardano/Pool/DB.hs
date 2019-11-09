{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Database / Persistence layer for the pool production.

module Cardano.Pool.DB
    ( -- * Interface
      DBLayer (..)

      -- * Errors
    , ErrPointAlreadyExists (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader, EpochNo (..), PoolId, SlotId (..) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( ExceptT )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )

-- | A Database interface for storing pool production in DB.
--
-- To use it, you will need the NamedFieldPuns extension and wrap operations
-- with @atomically@:
--
-- Example:
-- >>> :set -XNamedFieldPuns
-- >>> DBLayer{atomically,putPoolProduction} = db
-- >>> atomically $ putPoolProduction blockHeader pool
--
-- This gives you the power to also run /multiple/ operations atomically.
data DBLayer m = forall stm. MonadFail stm => DBLayer
    { putPoolProduction
        :: BlockHeader
        -> PoolId
        -> ExceptT ErrPointAlreadyExists stm ()
        -- ^ Write for a given slot id the id of stake pool that produced a
        -- a corresponding block

    , readPoolProduction
        :: EpochNo
        -> stm (Map PoolId [BlockHeader])
        -- ^ Read the all stake pools together with corresponding slot ids
        -- for a given epoch.

    , putStakeDistribution
        :: EpochNo
        -> [(PoolId, Quantity "lovelace" Word64)]
        -> stm ()
        -- ^ Replace an existing distribution for the given epoch by the one
        -- given as argument.
        --
        -- If there's no existing distribution, simply inserts it.

    , readStakeDistribution
        :: EpochNo
        -> stm [(PoolId, Quantity "lovelace" Word64)]

    , readPoolProductionCursor
        :: Int -> stm [BlockHeader]
        -- ^ Read the latest @k@ blockheaders in ascending order. The tip will
        -- be the last element in the list.
        --
        -- This is useful for the @NetworkLayer@ to know how far we have synced.

    , rollbackTo
        :: SlotId -> stm ()
        -- ^ Remove all entries of slot ids newer than the argument

    , cleanDB
        :: stm ()
        -- ^ Clean a database

    , atomically
        :: forall a. stm a -> m a
        -- ^ Run an operation.
        --
        -- For a Sqlite DB, this would be "run a query inside a transaction".
    }

-- | Forbidden operation was executed on an already existing slot
newtype ErrPointAlreadyExists
    = ErrPointAlreadyExists BlockHeader -- Point already exists in db
    deriving (Eq, Show)
