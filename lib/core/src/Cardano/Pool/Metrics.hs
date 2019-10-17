{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module can fold over a blockchain to collect metrics about
-- Stake pools.
--
-- It interacts with:
-- - "Cardano.Wallet.Network" which provides the chain
-- - "Cardano.Pool.DB" - which can persist the metrics
-- - "Cardano.Wallet.Api.Server" - which presents the results in an endpoint
module Cardano.Pool.Metrics
    ( StakePoolLayer (..)
    , ErrListStakePools (..)
    , newStakePoolLayer

    , activityForEpoch
    , combineMetrics
    , ErrMetricsInconsistency (..)

    -- * Helper
    , withinSameTip
    , ErrWithinSameTip (..)

    , State (..)
    , applyBlock
    )
    where

import Prelude

import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Wallet.Network
    ( ErrNetworkUnavailable, NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), EpochNo (..), PoolId (..), SlotId (..) )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE, withExceptT )
import Control.Retry
    ( RetryPolicyM, retrying )
import Data.Either
    ( isLeft )
import Data.Map.Merge.Strict
    ( WhenMatched, WhenMissing, mergeA, traverseMissing, zipWithMatched )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), blockListF', fmt, (+|), (|+) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.Map.Strict as Map

data ErrListStakePools
    = ErrListStakePoolsStakeIsUnreachable ErrNetworkUnavailable
    | ErrListStakePoolsMetricsIsUnsynced (Quantity "percent" Percentage)
    | ErrListStakePoolInconsistencyErr ErrMetricsInconsistency

newtype StakePoolLayer m = StakePoolLayer
     { listStakePools
         :: ExceptT ErrListStakePools m
            [(PoolId, (Quantity "lovelace" Word64, Quantity "block" Natural))]
     }

newStakePoolLayer
    :: NetworkLayer IO tx (BlockHeader, PoolId)
    -> DBLayer IO
    -> IO (StakePoolLayer IO)
newStakePoolLayer nl db = do
    return $ StakePoolLayer
        { listStakePools = do
            -- TODO: Use withinSameTip here!
            (epochNo, stakeDistr) <- withE1 $ stakeDistribution nl

            production <- liftIO $ convert <$> readPoolProduction db epochNo

            case combineMetrics stakeDistr production of
                Left e -> throwE $ ErrListStakePoolInconsistencyErr e
                Right r -> return $ Map.toList r
        }
  where
    withE1 = withExceptT ErrListStakePoolsStakeIsUnreachable

    -- TODO: It might be cleaner to change the DB-layer instead of having
    -- this Map.map here.
    convert = Map.map (Quantity . fromIntegral . length)

-- | For a given epoch, and state, this function returns /how many/ blocks
-- each pool produced.
activityForEpoch :: EpochNo -> State -> Map PoolId Int
activityForEpoch epoch s =
    Map.filter (> 0)
    $ Map.map (length . filter slotInCurrentEpoch)
    (activity s)
  where
    slotInCurrentEpoch = ((epoch ==) . epochNumber)

data ErrMetricsInconsistency
    = ErrMetricsInconsistencyBlockProducerNotInStakeDistr
        PoolId
        (Quantity "block" Natural)
    deriving (Show, Eq)

-- | Combines two different sources of data into one:
--
-- 1. A stake-distribution map
-- 2. A pool-production map
--
-- If a pool has produced a block without existing in the stake-distribution,
-- i.e it exists in (2) but not (1), this function will return
-- @Left ErrMetricsInconsistencyBlockProducerNotInStakeDistr@.
--
-- If a pool is in (1) but not (2), it simply means it has produced 0 blocks so
-- far.
combineMetrics
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Natural)
    -> Either
        ErrMetricsInconsistency
        (Map PoolId (Quantity "lovelace" Word64, Quantity "block" Natural))
combineMetrics =
    mergeA
        stakeButNoActivity
        activityButNoStake
        stakeAndActivity
  where
    stakeButNoActivity
        :: WhenMissing
            (Either ErrMetricsInconsistency)
            PoolId
            (Quantity "lovelace" Word64)
            (Quantity "lovelace" Word64, Quantity "block" Natural)
    stakeButNoActivity = traverseMissing $ \_k stake ->
        pure (stake, Quantity 0)

    activityButNoStake
        :: WhenMissing
            (Either ErrMetricsInconsistency)
            PoolId
            (Quantity "block" Natural)
            (Quantity "lovelace" Word64, Quantity "block" Natural)
    activityButNoStake = traverseMissing $ \pool count ->
        Left $ ErrMetricsInconsistencyBlockProducerNotInStakeDistr pool count

    stakeAndActivity
        :: WhenMatched
            (Either ErrMetricsInconsistency)
            PoolId
            (Quantity "lovelace" Word64)
            (Quantity "block" Natural)
            (Quantity "lovelace" Word64, Quantity "block" Natural)
    stakeAndActivity =
        zipWithMatched (\_k stake productions -> (stake, productions))

-- | Runs an action by making sure that the network header hasn't changed before
-- and after the action. This is useful in order to combine data from different
-- sources and make sure that all data retrieval and computations happens on a
-- same version.
--
-- The action __must__ be retry-able (i.e. read-only) for it will be retried if
-- the tip has changed before and after the call.
withinSameTip
    :: forall m header a. (MonadIO m, Eq header)
    => RetryPolicyM m
        -- ^ Retrying policy to command what to do in case the action wasn't
        -- executed within the same tip.
    -> (ErrWithinSameTip -> m a)
        -- ^ On failure
    -> m header
        -- ^ A getter for that header.
    -> (header -> m a)
        -- ^ The action to run
    -> m a
withinSameTip policy liftE getTip action = do
    let shouldRetry = const (pure . isLeft)
    res <- retrying policy shouldRetry (const trial)
    case res of
        Left e -> liftE e
        Right r -> return r
  where
    trial :: m (Either ErrWithinSameTip a)
    trial = do
        start <- getTip
        a <- action start
        end <- getTip
        pure $ if (start /= end)
            then Left ErrWithinSameTipMaxRetries
            else Right a

data ErrWithinSameTip
    = ErrWithinSameTipMaxRetries
        -- ^ Retried too many times
    deriving (Generic, Show, Eq)

--
-- Internals
--

-- | In-memory state keeping track of which pool produced blocks at which slots.
data State = State
    { tip :: BlockHeader
      -- ^ The blockHeader of the most recently applied block. Used to resume
      -- restoration from @NetworkLayer@.
    , activity :: Map PoolId [SlotId]
    -- ^ Mapping from pools to the slots where pool produced blocks.
    --
    -- This is needed internally to support rollback, but publicly, only the
    -- /length of/ the SlotId-list is likely needed.
    } deriving (Eq, Show, Generic)

instance Buildable State where
    build (State t m) =
        fmt ("Stakepool metrics at tip: "+|t|+"\n") <>
        blockListF'
            mempty
            (\(k,v) -> fmt (""+|k|+": "+|length v|+"") )
            (Map.toList m)

applyBlock :: (BlockHeader, PoolId) -> State -> State
applyBlock (newTip, poolId) (State _prevTip prevMap) =
        State newTip (Map.alter alter poolId prevMap)
  where
    slot = slotId newTip
    alter = \case
        Nothing -> Just [slot]
        Just slots -> Just (slot:slots)



