{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    , StakePool (..)

    -- * Listing stake-pools from the DB
    , StakePoolLayer (..)
    , newStakePoolLayer
    , ErrListStakePools (..)

      -- * Following the chain
    , monitorStakePools

      -- * Combining Metrics
    , ErrMetricsInconsistency (..)
    , combineMetrics
    , calculatePerformance
    )
    where

import Prelude

import Cardano.BM.Trace
    ( Trace, logDebug, logInfo, logNotice )
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
    ( BlockHeader (..), EpochNo (..), PoolId (..), SlotId (..) )
import Control.Monad
    ( forM, forM_, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT, throwE, withExceptT )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( foldl', sortOn )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, zipWithMatched )
import Data.Map.Strict
    ( Map )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Block = Block
    { header :: BlockHeader
    , producer :: PoolId
    } deriving (Eq, Show, Generic)

data StakePool = StakePool
    { poolId :: PoolId
    , stake :: Quantity "lovelace" Word64
    , production :: Quantity "block" Word64
    , apparentPerformance :: Double
    } deriving (Generic)

--------------------------------------------------------------------------------
-- Stake Pool Monitoring
--------------------------------------------------------------------------------

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
    :: Trace IO Text
    -> NetworkLayer IO t Block
    -> DBLayer IO
    -> IO ()
monitorStakePools tr nl db = do
    cursor <- initCursor
    logInfo tr $ mconcat
        [ "Start monitoring stake pools. Currently at "
        , case cursor of
            [] -> "genesis"
            _  -> pretty (last cursor)
        ]
    follow nl tr cursor forward backward header
  where
    initCursor :: IO [BlockHeader]
    initCursor = do
        let (_, bp) = staticBlockchainParameters nl
        let k = fromIntegral . getQuantity . view #getEpochStability $ bp
        readPoolProductionCursor db k

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
        liftIO $ logInfo tr $ "Writing stake-distribution for epoch " <> pretty ep
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

--------------------------------------------------------------------------------
-- StakePoolLayer
--------------------------------------------------------------------------------

-- | @StakePoolLayer@ is a thin layer ontop of the DB. It is /one/ value that
-- can easily be passed to the API-server, where it can be used in a simple way.
newtype StakePoolLayer m = StakePoolLayer
      { listStakePools
          :: ExceptT ErrListStakePools m [StakePool]
      }

data ErrListStakePools
     = ErrMetricsIsUnsynced (Quantity "percent" Percentage)
     | ErrListStakePoolsMetricsInconsistency ErrMetricsInconsistency
     | ErrListStakePoolsErrNetworkTip ErrNetworkTip

newStakePoolLayer
     :: DBLayer IO
     -> NetworkLayer IO t block
     -> Trace IO Text
     -> IO (StakePoolLayer IO)
newStakePoolLayer db nl tr = do
     return $ StakePoolLayer
        { listStakePools = do
            nodeTip <- withExceptT ErrListStakePoolsErrNetworkTip
                $ networkTip nl
            let nodeEpoch = nodeTip ^. #slotId . #epochNumber

            distr <- liftIO $ Map.fromList <$>
                readStakeDistribution db nodeEpoch

            prod <- liftIO $ count <$>
                readPoolProduction db nodeEpoch

            when (Map.null distr || Map.null prod) $ do
                computeProgress nodeTip >>= throwE . ErrMetricsIsUnsynced

            perfs <- liftIO $
                readPoolsPerformances db nodeEpoch

            case combineMetrics distr prod perfs of
                Right x -> return
                    $ sortOn (Down . apparentPerformance)
                    $ map (uncurry mkStakePool)
                    $ Map.toList x
                Left e ->
                    throwE $ ErrListStakePoolsMetricsInconsistency e
        }
  where
    poolProductionTip = readPoolProductionCursor db 1 >>= \case
        [x] -> return $ Just x
        _ -> return Nothing

    mkStakePool
        :: PoolId
        -> ( Quantity "lovelace" Word64
           , Quantity "block" Word64
           , Double
           )
        -> StakePool
    mkStakePool poolId (stake, production, apparentPerformance) =
        StakePool{poolId,stake,production,apparentPerformance}

    computeProgress
        :: BlockHeader -- ^ The node tip, which respresents 100%.
        -> ExceptT e IO (Quantity "percent" Percentage)
    computeProgress nodeTip = liftIO $ do
        mDbTip <- poolProductionTip
        Quantity <$> case mDbTip of
            Nothing -> return minBound
            Just dbTip -> do
                liftIO $ logDebug tr $ mconcat
                    [ "The node tip is:\n"
                    , pretty nodeTip
                    , ",\nbut the last pool production stored in the db"
                    , " is from:\n"
                    , pretty dbTip
                    ]
                return $ progress dbTip nodeTip

    progress :: BlockHeader -> BlockHeader -> Percentage
    progress tip target =
        let
            s0 = getQuantity $ tip ^. #blockHeight
            s1 = getQuantity $ target ^. #blockHeight
        in toEnum $ round $ 100 * (toD s0) / (toD s1)
      where
        toD :: Integral i => i -> Double
        toD = fromIntegral

readPoolsPerformances
    :: DBLayer IO
    -> EpochNo
    -> IO (Map PoolId Double)
readPoolsPerformances db (EpochNo epochNo) = do
    let range = [max 0 (fromIntegral epochNo - 14) .. fromIntegral epochNo]
    fmap avg $ forM range $ \ep -> calculatePerformance
        <$> liftIO (Map.fromList <$> readStakeDistribution db ep)
        <*> liftIO (count <$> readPoolProduction db ep)
  where
    -- | We compute performances on a range of epochs, because we don't know
    -- whether we have enough details
    avg :: [Map PoolId Double] -> Map PoolId Double
    avg performances = Map.map (/ len) $ Map.unionsWith (+) performances
      where
        len = fromIntegral (length performances)

-- | Calculate pool performance over the given data.
calculatePerformance
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Word64)
    -> Map PoolId Double
calculatePerformance mStake mProd =
    let
        stakeButNotProd = traverseMissing $ \_ _ -> 0
        prodButNoStake  = dropMissing
        stakeAndProd sTotal nTotal = zipWithMatched $ \_ s n ->
            if (nTotal == 0 || s == Quantity 0) then
                0
            else
                (double n / nTotal) * (sTotal / double s)
    in
        Map.merge
            stakeButNotProd
            prodButNoStake
            (stakeAndProd (sumQ mStake) (sumQ mProd))
            mStake
            mProd
  where
    double :: Integral a => Quantity any a -> Double
    double (Quantity a) = fromIntegral a

    sumQ :: Integral a => Map k (Quantity any a) -> Double
    sumQ = fromIntegral . foldl' (\y (Quantity x) -> (y + x)) 0 . Map.elems

-- | Combines three different sources of data into one:
--
-- 1. A stake-distribution map
-- 2. A pool-production map
-- 3. A pool-performance map
--
-- If a pool has produced a block without existing in the stake-distribution,
-- i.e it exists in (2) but not (1), this function will return
-- @Left ErrMetricsInconsistency@.
--
-- If a pool is in (1) but not (2), it simply means it has produced 0 blocks so
-- far.
--
-- Similarly, if we do have metrics about a pool in (3), but this pool is
-- unknown from (1) & (2), this function also returns
-- @Left ErrMetricsInconsistency@.
--
-- If a pool is in (1+2) but not in (3), it simply means it has produced 0
-- blocks so far.
combineMetrics
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Word64)
    -> Map PoolId Double
    -> Either
        ErrMetricsInconsistency
        ( Map PoolId
            ( Quantity "lovelace" Word64
            , Quantity "block" Word64
            , Double
            )
        )
combineMetrics mStake mProd mPerf = do
    let errMissingLeft = ErrProducerNotInDistribution
    mActivity <- zipWithRightDefault (,) errMissingLeft (Quantity 0) mStake mProd
    zipWithRightDefault unzipZip3 errMissingLeft 0 mActivity mPerf
  where
    unzipZip3 :: (a,b) -> c -> (a,b,c)
    unzipZip3 (a,b) c = (a,b,c)

-- | Possible errors returned by 'combineMetrics'.
newtype ErrMetricsInconsistency
    = ErrProducerNotInDistribution PoolId
        -- ^ Somehow, we tried to combine invalid metrics together and passed
        -- a passed a block production that doesn't match the producers found in
        -- the stake activity.
        --
        -- Note that the opposite case is okay as we only observe pools that
        -- have produced blocks. So it could be the case that a pool exists in
        -- the distribution but not in the production! (In which case, we'll
        -- assign it a production of '0').
    deriving (Show, Eq)

-- | Combine two maps with the given zipping function. It defaults when elements
-- of the first map (left) are not present in the second (right), but returns an
-- error when elements of the second (right) map are not present in the first
-- (left).
--
-- Example:
--
-- @
-- let m1 = Map.fromList [(1, 'a'), (2, 'b'), (3, 'c')]
-- let m2 = Map.fromList [(2, True)]
-- @
--
-- >>> zipWithRightDefault (,) ErrMissing False m1 m2
-- Right (Map.fromList [(1, ('a', False)), (2, ('b', True)), (3, ('c', False))])
--
-- >>> zipWithRightDefault (,) ErrMissing False m2 m1
-- Left (ErrMissing 1)
zipWithRightDefault
    :: Ord k
    => (l -> r -> a)
    -> (k -> errMissingLeft)
    -> r
    -> Map k l
    -> Map k r
    -> Either errMissingLeft (Map k a)
zipWithRightDefault combine onMissing rZero =
    Map.mergeA leftButNotRight rightButNotLeft bothPresent
  where
    leftButNotRight = traverseMissing $ \_k l -> pure (combine l rZero)
    rightButNotLeft = traverseMissing $ \k _r -> Left (onMissing k)
    bothPresent     = zipWithMatched  $ \_k l r -> (combine l r)

-- | Count elements inside a 'Map'
count :: Map k [a] -> Map k (Quantity any Word64)
count = Map.map (Quantity . fromIntegral . length)
