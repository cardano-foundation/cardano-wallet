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
    ( Block (..)
    , combineMetrics
    , ErrMetricsInconsistency (..)

    , worker

    -- * Helper
    , withinSameTip
    , ErrWithinSameTip (..)
    )
    where

import Prelude

import Cardano.BM.Trace
    ( Trace )
import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Wallet.Network
    ( FollowAction (..)
    , NetworkLayer (networkTip, stakeDistribution)
    , follow
    , staticBlockchainParameters
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), PoolId (..), SlotId (..) )
import Control.Monad
    ( forM_, unless )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT, throwE, withExceptT )
import Control.Retry
    ( RetryPolicyM, limitRetries, retrying )
import Data.Either
    ( isLeft )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Merge.Strict
    ( WhenMatched, WhenMissing, mergeA, traverseMissing, zipWithMatched )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.Map.Strict as Map

data Block = Block
    { header :: BlockHeader
    , producer :: PoolId
    } deriving (Eq, Show, Generic)

data ErrMetricsWorker
    = ErrMetricsWorkerTipTooVolatile
    | ErrMetricsWorkerStakeDistributionUnavailable
    | ErrMetricsWorkerDataInconsistencyBlockWithoutProducer
    | ErrMetricsWorkerDataInconsistencyDuplicateBlock
    | ErrMetricsWorkerTipIsUnreachable
    deriving (Show, Eq)

-- | @worker@ follows the chain and puts pool productions and stake
-- distributions to a @DBLayer@, such that the data in the @DB@ always is
-- consistent.
--
-- The pool productions and stake distrubtions in the DB can /never/ be from
-- different forks.
worker
    :: NetworkLayer IO t Block
    -> DBLayer IO
    -> Trace IO Text
    -> IO ()
worker nl db tr = do
    -- Read the k latest headers the DB knows about
    let (_block0, bp) = staticBlockchainParameters nl
    let k = fromIntegral . getQuantity . view #getEpochStability $ bp
    knownHeaders <- readCursor db k

    follow nl tr knownHeaders advance' rollback header
  where

    advance'
        :: NonEmpty Block
        -> BlockHeader
        -> IO (FollowAction ErrMetricsWorker)
    advance' bs h = handleResult <$> runExceptT (advance bs h)
      where
        handleResult = \case
            Left ErrMetricsWorkerTipTooVolatile
                -> Retry
            Left e
                -> ExitWith e
                -- TODO: Make all cases explicit here.
            Right ()
                -> Continue

    advance
        :: NonEmpty Block
        -> BlockHeader
        -> ExceptT ErrMetricsWorker IO ()
    advance blocks nodeTip = do
        -- Get a stake distribution and next batch of blocks such that we know
        -- they are from the same chain.
        --
        -- Assumtion: We don't expect the node to switch from tip A to tip B,
        -- and back to tip A.
        --
        -- If the tip is still the same after we have fetched the stake
        -- distribution, we conclude that the stake-distrubtion is from the same
        -- chain as the tip.
        consistentDistr <- withinSameTip (limitRetries 3) liftE getTip $ \nodeTip2 -> do
            distr <- withExceptT (const ErrMetricsWorkerStakeDistributionUnavailable)
                $ stakeDistribution nl
            unless (nodeTip == nodeTip2) $
                -- The tip has changed. We cannot guarantee that the blocks and
                -- the stake-distribution are on the same chain.
                throwE ErrMetricsWorkerTipTooVolatile
            return (fst distr, Map.toList $ snd distr)

        liftIO $ uncurry (putStakeDistribution db) consistentDistr

        forM_ blocks $ \block ->
            -- TODO: We might want to batch the insertion to the DB.
            -- TODO: What do we do if the production already exists?!
            withExceptT
                (const ErrMetricsWorkerDataInconsistencyDuplicateBlock)
                $ putPoolProduction db (header block) (producer block)
          where
            liftE ErrWithinSameTipMaxRetries =
                throwE ErrMetricsWorkerTipTooVolatile

    rollback :: SlotId -> IO (FollowAction ErrMetricsWorker)
    rollback point = do
        liftIO $ rollbackTo db point
        return Continue

    getTip = withExceptT (const ErrMetricsWorkerTipIsUnreachable) $
        networkTip nl

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
