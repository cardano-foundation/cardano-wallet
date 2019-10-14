{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module can fold over a blockchain to collect metrics about
-- Stake pools.
--
-- It interacts with:
-- - "Cardano.Wallet.Network" which provides the chain
-- - "Cardano.Wallet.DB" - which can persist the metrics
-- - "Cardano.Wallet.Api.Server" - which presents the results in an endpoint
module Cardano.Wallet.StakePool.Metrics
    ( activityForEpoch

    , State (..)
    , applyBlock
    , worker
    , combineMetrics

    , StakePoolLayer (..)
    , newStakePoolLayer
    , ErrListStakePools (..)
    , ErrInconsistency (..)
    )
    where

import Prelude

import Cardano.BM.Trace
    ( Trace, appendName, logInfo )
import Cardano.Wallet.Network
    ( ErrInconsistentTips (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , NetworkLayer (..)
    , atomically
    , follow
    , onSameTip
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), EpochNo, PoolId (..), SlotId (..), Stake )
import Control.Concurrent
    ( forkIO )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newMVar, readMVar )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE, withExceptT )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Merge.Strict
    ( WhenMissing, mergeA, traverseMissing, zipWithMatched )
import Data.Map.Strict
    ( Map )
import Data.Text
    ( Text )
import Fmt
    ( Buildable (..), blockListF', fmt, (+|), (|+) )
import GHC.Generics
    ( Generic )

import qualified Data.Map.Strict as Map

--
-- Metrics
--

data ErrListStakePools
    = ErrListStakePoolsStakeIsUnreachable ErrNetworkUnavailable
    | ErrListStakePoolsMetricsIsUnsynced
    | ErrListStakePoolInconsistencyErr ErrInconsistentTips

data ErrInconsistency
    = ErrInconsistencyBlockProducerNotInStakeDistr PoolId Int
    | ErrInconsistencyNetworkTipUnavailible ErrNetworkTip
    | ErrInconsistencyTipChanged BlockHeader BlockHeader

newtype StakePoolLayer m = StakePoolLayer
    { listStakePools
        :: ExceptT ErrListStakePools m [(PoolId, (Stake, Int))]
    }

newStakePoolLayer
    :: NetworkLayer IO tx (BlockHeader, PoolId)
    -> Trace IO Text
    -> IO (StakePoolLayer IO)
newStakePoolLayer nl tr = do
    mvar <- worker nl tr
    return $ StakePoolLayer
        { listStakePools = Map.toList <$>
            combineMetrics nl (liftIO $ readMVar mvar)
        }

-- | Combines two different sources of data: stake distribution and pool activity
-- together.
combineMetrics
    :: MonadIO m
    => NetworkLayer m t (BlockHeader, PoolId)
    -> m State
    -> ExceptT ErrListStakePools m (Map PoolId (Stake, Int))
combineMetrics nl getActivityState =
    atomically onSameTip nl ErrListStakePoolInconsistencyErr 5
    $ do
    (epoch, distr) <- withExceptT ErrListStakePoolsStakeIsUnreachable
        $ stakeDistribution nl

    s <- lift getActivityState

    m <- case activityForEpoch epoch s of
        Just x -> return x
        Nothing -> throwE ErrListStakePoolsMetricsIsUnsynced

    mergeA
        stakeButNoActivity
        activityButNoStake
        (zipWithMatched (\_k stake act -> (stake, act)))
        distr m
  where
    -- What to do when we only find a pool in one of the two data-sources:
    stakeButNoActivity
        :: Monad m
        => WhenMissing (ExceptT ErrListStakePools m) PoolId Stake (Stake, Int)
    stakeButNoActivity = traverseMissing $ \_k stake -> pure (stake, 0)

    activityButNoStake
        :: Monad m
        => WhenMissing (ExceptT ErrListStakePools m) PoolId Int (Stake, Int)
    activityButNoStake = traverseMissing $ \_pool _count -> error "no"

    -- In case we wanted to calculate approximate performance (AP):
    --
    -- AP = (n / N) * (S / s)
    --   where
    --     n is the number of blocks produced by the pool in the last epoch
    --     N is the number of slots in the epoch
    --     s is the stake owned by the pool in the last epoch
    --     S is the total stake delegated to pools in the last epoch
    --
    -- we can retrive
    --     - S from the total stake in the distribution.
    --     - s from the distribution.
    --     - n from the activity-state (tip, activity)
    --     - N from the slotNumber of the tip in the activity state
    --


-- | Start a worker to keep track of stake-pool-metrics
worker
    :: NetworkLayer IO tx (BlockHeader, PoolId)
    -> Trace IO Text
    -> IO (MVar State)
worker nl tr = do
    let ((block0Header, _),_) = staticBlockchainParameters nl
    let tr' = appendName "stakepool-metrics-collector" tr
    logInfo tr' "worker started..."
    let s0 = State block0Header Map.empty
    mvar <- newMVar s0
    void $ forkIO $ follow nl tr' [block0Header] (advance mvar) rollback fst
    return mvar
  where
    advance
        :: MVar State -> NonEmpty (BlockHeader, PoolId) -> BlockHeader -> ExceptT () IO ()
    advance mvar blocks _ = do
        liftIO $ modifyMVar_ mvar $ \s -> do
            (return $ foldl (flip applyBlock) s blocks )

    rollback :: SlotId -> ExceptT () IO ()
    rollback _slot = error "rollback unimplemented"

--
-- The following in memory model should likely be removed when we switch to
-- Sqlite model:

-- | For a given epoch, and state, this function returns /how many/ blocks
-- each pool produced.
activityForEpoch :: EpochNo -> State -> Maybe (Map PoolId Int)
activityForEpoch epoch s = guardEpoch $
     Map.filter (> 0)
    $ Map.map (length . filter slotInCurrentEpoch)
    (activity s)
  where
    slotInCurrentEpoch = ((epoch ==) . epochNumber)
    guardEpoch a =
        if epoch > (epochNumber . slotId . tip $ s)
        then Nothing
        else Just a

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
