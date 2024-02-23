{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Wallet.Network
    ( -- * Interface
      NetworkLayer (..)

      -- * Errors
    , ErrPostTx (..)
    , ErrFetchBlock (..)

      -- * Chain following
    , ChainFollower (..)
    , mapChainFollower
    , ChainFollowLog (..)
    , ChainSyncLog (..)
    , mapChainSyncLog
    , withFollowStatsMonitoring

      -- * Light-mode
    , LightLayer (..)
    , LightBlocks

      -- * Logging (for testing)
    , FollowStats (..)
    , Rearview (..)
    , emptyStats
    , updateStats
    ) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Network.Logging
    ( ChainFollowLog (..)
    , ChainSyncLog (..)
    , FollowStats (..)
    , Rearview (..)
    , emptyStats
    , mapChainSyncLog
    , updateStats
    , withFollowStatsMonitoring
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , TimeInterpreter
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Cardano.Wallet.Primitive.Types.Block
    ( Block
    , BlockHeader
    , ChainPoint (..)
    )
import Cardano.Wallet.Primitive.Types.BlockSummary
    ( LightSummary
    )
import Cardano.Wallet.Primitive.Types.Checkpoints.Policy
    ( CheckpointPolicy
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.ProtocolParameters
    ( ProtocolParameters
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( SlottingParameters
    )
import Cardano.Wallet.Primitive.Types.StakePoolSummary
    ( StakePoolsSummary
    )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( SealedTx
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    )
import Control.Tracer
    ( Tracer
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Fmt
    ( pretty
    )
import GHC.Generics
    ( Generic
    )
import Internal.Cardano.Write.Tx
    ( MaybeInRecentEra
    )

import qualified Internal.Cardano.Write.Tx as Write

{-----------------------------------------------------------------------------
    NetworkLayer
------------------------------------------------------------------------------}
-- | Interface for network capabilities.
data NetworkLayer m block = NetworkLayer
    { chainSync
        :: Tracer IO ChainFollowLog
        -> ChainFollower m ChainPoint BlockHeader (NonEmpty block)
        -> m ()
    -- ^ Connect to the node and run the ChainSync protocol.
    -- The callbacks provided in the 'ChainFollower' argument
    -- are used to handle intersection finding,
    -- the arrival of new blocks, and rollbacks.
    , fetchBlock
        :: ChainPoint
        -> m (Either ErrFetchBlock block)
    -- ^ Connect to the node and try to retrieve
    -- the block at a given 'ChainPoint'.
    , currentNodeTip
        :: m BlockHeader
    -- ^ Get the current tip from the chain producer
    , currentNodeEra
        :: m AnyCardanoEra
    -- ^ Get the era the node is currently in.
    , currentProtocolParameters
        :: m ProtocolParameters
    -- ^ Get the last known protocol parameters. In principle, these can
    -- only change once per epoch.
    , currentProtocolParametersInRecentEras
        :: m (MaybeInRecentEra Write.PParams)
        -- ^ Get the last known protocol parameters for recent eras.

    , currentSlottingParameters
        :: m SlottingParameters
    -- ^ Get the last known slotting parameters. In principle, these can
    -- only change once per era.
    , watchNodeTip
        :: (BlockHeader -> m ())
        -> m ()
    -- ^ Register a callback for when the node tip changes.
    -- This function should never finish, unless the callback throws an
    -- exception, which will be rethrown by this function.
    , postTx
        :: SealedTx
        -> ExceptT ErrPostTx m ()
    -- ^ Broadcast a transaction to the chain producer
    , stakeDistribution
        :: Coin -- Stake to consider for rewards
        -> m StakePoolsSummary
    , getCachedRewardAccountBalance
        :: RewardAccount
        -- Either reward account from key hash or script hash
        -> m Coin
    -- ^ Return the cached reward balance of an account.
    --
    -- If there is no cached value, it will return `Coin 0`, and add the
    -- account to the internal set of observed account, such that it will be
    -- fetched later.
    , fetchRewardAccountBalances
        :: Set RewardAccount
        -> m (Map RewardAccount Coin)
    -- ^ Fetch the reward account balance of a set of accounts without
    -- any caching.
    , timeInterpreter
        :: TimeInterpreter (ExceptT PastHorizonException m)
    , syncProgress
        :: SlotNo
        -> m (SyncProgress)
    -- ^ Compute the ratio between the provided 'SlotNo' and the slot
    -- corresponding to the current wall-clock time.
    --
    -- Unlike using 'Cardano.Wallet.Primitive.SyncProgress.syncProgress'
    -- after retrieving a 'timeInterpreter', this function will return
    -- 'NotResponding' rather than block in the edge case when the era
    -- history has not yet been fetched from the node on startup.
    }

instance Functor m => Functor (NetworkLayer m) where
    fmap f nl =
        nl
            { chainSync = \tr follower ->
                chainSync nl tr $ mapChainFollower id id id (fmap f) follower
            , fetchBlock =
                fmap (fmap f) . fetchBlock nl
            }

{-----------------------------------------------------------------------------
    ChainFollower
------------------------------------------------------------------------------}
-- | A collection of callbacks to use with the 'chainSync' function.
data ChainFollower m point tip blocks = ChainFollower
    { checkpointPolicy :: Integer -> CheckpointPolicy
    -- ^ The policy for creating and pruning checkpoints that
    -- is used by the 'ChainFollower'.
    -- The argument of this field is the @epochStability@.
    --
    -- Exposing this policy here enables any chain synchronizer
    -- which does not retrieve full blocks, such as 'lightSync',
    -- to specifically target those block heights at which
    -- the 'ChainFollower' intends to create checkpoints.
    , readChainPoints :: m [point]
    -- ^ Callback for reading the local tip. Used to negotiate the
    -- intersection with the node.
    --
    -- A response of [] is interpreted as `Origin` -- i.e. the chain will be
    -- served from genesis.
    , rollForward :: blocks -> tip -> m ()
    -- ^ Callback for rolling forward.
    --
    -- Implementors _may_ delete old checkpoints while rolling forward.
    , rollBackward :: point -> m point
    -- ^ Roll back to the requested slot, or further, and return the point
    -- actually rolled back to.
    --
    -- __Example 1:__
    --
    -- If the follower stores checkpoints for all blocks, we can always roll
    -- back to the requested point exactly.
    --
    -- @
    -- -- If
    -- knownSlots follower `shouldReturn` [0,1,2,3]
    -- let requested = SlotNo 2
    -- -- Then
    -- actual <- rollBackward follower requested
    -- knownSlots follower shouldReturn` [0,1,2]
    -- actual `shouldBe` SlotNo 2
    -- @
    --
    -- Note that the slotNos are unlikely to be consecutive in real life,
    -- but this doesn't matter, as ouroboros-network asks us to rollback to
    -- points, corresponding to blocks.
    --
    -- __Example 2:__
    --
    -- @
    -- -- If
    -- knownSlots follower `shouldReturn` [0,9,10]
    -- let requested = SlotNo 2
    -- -- Then
    -- actual <- rollBackward follower requested
    -- knownSlots follower shouldReturn` [0]
    -- actual `shouldBe` SlotNo 0
    -- @
    }

mapChainFollower
    :: Functor m
    => (point1 -> point2)
    -- ^ Covariant
    -> (point2 -> point1)
    -- ^ Contravariant
    -> (tip2 -> tip1)
    -- ^ Contravariant
    -> (blocks2 -> blocks1)
    -- ^ Contravariant
    -> ChainFollower m point1 tip1 blocks1
    -> ChainFollower m point2 tip2 blocks2
mapChainFollower fpoint12 fpoint21 ftip fblocks cf =
    ChainFollower
        { checkpointPolicy = checkpointPolicy cf
        , readChainPoints = map fpoint12 <$> readChainPoints cf
        , rollForward = \bs tip -> rollForward cf (fblocks bs) (ftip tip)
        , rollBackward = fmap fpoint12 . rollBackward cf . fpoint21
        }

{-----------------------------------------------------------------------------
    LightSync
------------------------------------------------------------------------------}
-- | Interface for light-mode synchronization.
newtype LightLayer m block = LightLayer
    { lightSync
        :: Maybe
            ( ChainFollower m ChainPoint BlockHeader (LightBlocks m Block)
              -> m ()
            )
    -- ^ Connect to a data source that offers an efficient
    -- query @Address -> Transactions@.
    }

-- | In light-mode, we receive either a list of blocks as usual,
-- or a 'LightSummary' of blocks.
type LightBlocks m block = Either (NonEmpty block) (LightSummary m)

{-------------------------------------------------------------------------------
    Errors
-------------------------------------------------------------------------------}

-- | Error while trying to send a transaction
data ErrPostTx = ErrPostTxValidationError Text | ErrPostTxMempoolFull
    deriving (Generic, Show, Eq)

instance ToText ErrPostTx where
    toText = \case
        ErrPostTxValidationError msg -> msg
        ErrPostTxMempoolFull ->
            "mempool was full and refused posted transaction"

-- | Error while trying to retrieve a block
newtype ErrFetchBlock = ErrNoBlockAt ChainPoint
    deriving (Generic, Show, Eq)

instance ToText ErrFetchBlock where
    toText = \case
        ErrNoBlockAt pt ->
            "no block found at ChainPoint " <> pretty pt
