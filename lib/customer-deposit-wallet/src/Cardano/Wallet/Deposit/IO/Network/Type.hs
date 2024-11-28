{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (..)
    , ErrPostTx (..)
    , mapBlock
    , ChainFollower (..)
    ) where

import Prelude

import Cardano.Wallet.Deposit.Read
    ( Slot
    , WithOrigin
    )
import Cardano.Wallet.Network
    ( ChainFollower (..)
    , mapChainFollower
    )
import Control.Monad.Class.MonadTime
    ( UTCTime
    )
import Control.Tracer
    ( Tracer
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Text
    ( Text
    )
import Data.Void
    ( Void
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Cardano.Wallet.Deposit.Time as Time
import qualified Cardano.Wallet.Deposit.Write as Write

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}

data NetworkEnv m block = NetworkEnv
    { chainSync
        :: Tracer m ChainFollowLog
        -> ChainFollower m Read.ChainPoint Read.ChainPoint (NonEmpty block)
        -> m Void
    -- ^ Run the chain-sync mini-protocol (forever).
    , postTx
        :: Write.Tx
        -> m (Either ErrPostTx ())
    -- ^ Post a transaction to the Cardano network.
    , currentPParams
        :: m (Read.EraValue Read.PParams)
    -- ^ Current protocol paramters.
    , getTimeTranslation
        :: m Time.TimeTranslation
        -- ^ Get the current 'TimeInterpreter' from the Cardano node.
    , slotToUTCTime
        :: m (Slot -> (Maybe (WithOrigin UTCTime)))

    }

mapBlock
    :: Functor m
    => (block1 -> block2)
    -> NetworkEnv m block1
    -> NetworkEnv m block2
mapBlock f env@NetworkEnv{chainSync} = env
    { chainSync = \tr follower ->
        chainSync tr (mapChainFollower id id id (fmap f) follower)
    }

{-------------------------------------------------------------------------------
    Errors
-------------------------------------------------------------------------------}

-- | Error while trying to send a transaction to the network.
data ErrPostTx
    = ErrPostTxValidationError Text
    | ErrPostTxMempoolFull
    deriving (Eq, Show, Generic)

{-------------------------------------------------------------------------------
    Logging
-------------------------------------------------------------------------------}

-- | Higher level log of a chain follower.
-- -- Includes computed statistics about synchronization progress.
data ChainFollowLog
    = -- = MsgChainSync (ChainSyncLog BlockHeader ChainPoint)

      -- | MsgFollowStats (FollowStats Rearview)
      MsgStartFollowing
    deriving (Eq, Show, Generic)
