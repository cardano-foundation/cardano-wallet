{-# LANGUAGE DeriveGeneric #-}
module Cardano.Wallet.Deposit.IO.Network.Type
    ( NetworkEnv (..)
    , ChainFollower (..)
    ) where

import Prelude

import Cardano.Wallet.Network
    ( ChainFollower (..) )
import Control.Tracer
    ( Tracer )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Text
    ( Text )
import Data.Void
    ( Void )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Deposit.Read as Read

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
        :: Read.Tx -> m (Either ErrPostTx ())
        -- ^ Post a transaction to the Cardano network.

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
    -- = MsgChainSync (ChainSyncLog BlockHeader ChainPoint)
    -- | MsgFollowStats (FollowStats Rearview)
    = MsgStartFollowing
    deriving (Eq, Show, Generic)
