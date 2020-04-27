-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- High-level interface for dealing with stake pools.
module Cardano.Pool
    ( StakePoolLayer (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( PoolId, StakePool, StakePoolMetadata )
import Control.Monad.Trans.Except
    ( ExceptT )

-- | @StakePoolLayer@ is a thin layer ontop of the DB. It is /one/ value that
-- can easily be passed to the API-server, where it can be used in a simple way.
data StakePoolLayer e m = StakePoolLayer
    { listStakePools
        :: ExceptT e m [(StakePool, Maybe StakePoolMetadata)]

    , knownStakePools
        :: m [PoolId]
        -- ^ Get a list of known pools that doesn't require fetching things from
        -- any registry. This list comes from the registration certificates
        -- that have been seen on chain.
    }
