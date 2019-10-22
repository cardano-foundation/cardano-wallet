{-# LANGUAGE DataKinds #-}
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
import Control.Monad.Trans.Except
    ( ExceptT )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )

-- | A Database interface for storing pool production in DB.
data DBLayer m = DBLayer
    { putPoolProduction
        :: BlockHeader
        -> PoolId
        -> ExceptT ErrPointAlreadyExists m ()
        -- ^ Write for a given slot id the id of stake pool that produced a
        -- a corresponding block

    , readPoolProduction
        :: EpochNo
        -> m (Map PoolId [BlockHeader])
        -- ^ Read the all stake pools together with corresponding slot ids
        -- for a given epoch.

    , putStakeDistribution
        :: EpochNo
        -> [(PoolId, Quantity "lovelace" Word64)]
        -> m ()
        -- ^ Replace an existing distribution for the given epoch by the one
        -- given as argument.
        --
        -- If there's no existing distribution, simply inserts it.

    , readStakeDistribution
        :: EpochNo
        -> m [(PoolId, Quantity "lovelace" Word64)]

    , readPoolProductionCursor
        :: Int -> m [BlockHeader]
        -- ^ Read the latest @k@ blockheaders in ascending order. The tip will
        -- be the last element in the list.
        --
        -- This is useful for the @NetworkLayer@ to know how far we have synced.

    , rollbackTo
        :: SlotId -> m ()
        -- ^ Remove all entries of slot ids newer than the argument

    , cleanDB
        :: m ()
        -- ^ Clean a database
    }

-- | Forbidden operation was executed on an already existing slot
newtype ErrPointAlreadyExists
    = ErrPointAlreadyExists BlockHeader -- Point already exists in db
    deriving (Eq, Show)
