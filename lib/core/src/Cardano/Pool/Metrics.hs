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
    ( -- * Types
      Block (..)

      -- * Following the chain
    , monitorStakePools

      -- * Combining Metrics
    , ErrMetricsInconsistency (..)
    , combineMetrics
    )
    where

import Prelude

import Cardano.BM.Trace
    ( Trace, logDebug, logNotice )
import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists )
import Cardano.Wallet.Network
    ( ErrNetworkTip
    , ErrNetworkUnavailable
    , FollowAction (..)
    , NetworkLayer (networkTip, stakeDistribution)
    , follow
    , staticBlockchainParameters
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), PoolId (..), SlotId (..) )
import Control.Monad
    ( forM_, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( runExceptT, throwE, withExceptT )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Merge.Strict
    ( mergeA, traverseMissing, zipWithMatched )
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

-- | 'monitorStakePools' follows the chain and puts pool productions and stake
-- distributions to a 'DBLayer', such that the data in the database is always
-- consistent.
--
-- The pool productions and stake distrubtions in the db can /never/ be from
-- different forks such that it's safe for readers to access it.
--
-- FIXME: This last statement is only true if 'putStakeProduction' and
-- 'putPoolProduction' are running in the same db transaction, which would be
-- the case if we make 'SqlPersistT' the top-level monad for the 'DBLayer'
-- instead of 'IO'.
monitorStakePools
    :: NetworkLayer IO t Block
    -> DBLayer IO
    -> Trace IO Text
    -> IO ()
monitorStakePools nl db tr = do
    cursor <- initCursor
    follow nl tr cursor forward backward header
  where
    initCursor :: IO [BlockHeader]
    initCursor = do
        let (_block0, bp) = staticBlockchainParameters nl
        let k = fromIntegral . getQuantity . view #getEpochStability $ bp
        readCursor db k

    backward
        :: SlotId
        -> IO (FollowAction ErrMonitorStakePools)
    backward point = do
        liftIO $ rollbackTo db point
        return Continue

    forward
        :: NonEmpty Block
        -> BlockHeader
        -> IO (FollowAction ErrMonitorStakePools)
    forward blocks nodeTip = handler $ do
        (ep, dist) <- withExceptT ErrMonitorStakePoolsNetworkUnavailable $
            stakeDistribution nl
        currentTip <- withExceptT ErrMonitorStakePoolsNetworkTip $
            networkTip nl
        when (nodeTip /= currentTip) $ throwE ErrMonitorStakePoolsWrongTip
        -- FIXME Do the next operation in a database transaction
        lift $ putStakeDistribution db ep (Map.toList dist)
        forM_ blocks $ \b -> withExceptT ErrMonitorStakePoolsPoolAlreadyExists $
            putPoolProduction db (header b) (producer b)
      where
        handler action = runExceptT action >>= \case
            Left ErrMonitorStakePoolsNetworkUnavailable{} -> do
                logNotice tr "Network is not available."
                pure RetryLater
            Left ErrMonitorStakePoolsNetworkTip{} -> do
                logNotice tr "Network is not available."
                pure RetryLater
            Left ErrMonitorStakePoolsWrongTip{} -> do
                logDebug tr "Race condition when fetching stake distribution."
                pure RetryImmediately
            Left e@ErrMonitorStakePoolsPoolAlreadyExists{} ->
                pure (ExitWith e)
            Right () ->
                pure Continue

-- | Internal error data-type used to drive the 'forward' logic
data ErrMonitorStakePools
    = ErrMonitorStakePoolsNetworkUnavailable ErrNetworkUnavailable
    | ErrMonitorStakePoolsPoolAlreadyExists ErrPointAlreadyExists
    | ErrMonitorStakePoolsNetworkTip ErrNetworkTip
    | ErrMonitorStakePoolsWrongTip
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
    stakeButNoActivity = traverseMissing $ \_k stake ->
        pure (stake, Quantity 0)

    activityButNoStake = traverseMissing $ \pool count ->
        Left $ ErrProducerNotInDistribution pool count

    stakeAndActivity =
        zipWithMatched (\_k stake productions -> (stake, productions))

-- | Possible errors returned by 'combineMetrics'.
data ErrMetricsInconsistency
    = ErrProducerNotInDistribution PoolId (Quantity "block" Natural)
        -- ^ Somehow, we tried to combine invalid metrics together and passed
        -- a passed a block production that doesn't match the producers found in
        -- the stake activity.
        --
        -- Note that the opposite case is okay as we only observe pools that
        -- have produced blocks. So it could be the case that a pool exists in
        -- the distribution but not in the production! (In which case, we'll
        -- assign it a production of '0').
    deriving (Show, Eq)
