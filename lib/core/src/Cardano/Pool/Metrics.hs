{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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

      -- * Logging
    , StakePoolLayerMsg (..)
    )
    where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( DefinePrivacyAnnotation (..), DefineSeverity (..) )
import Cardano.BM.Trace
    ( Trace, logDebug, logInfo, logNotice )
import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists )
import Cardano.Pool.Metadata
    ( RegistryLog
    , StakePoolMetadata (..)
    , getRegistryZipUrl
    , getStakePoolMetadata
    )
import Cardano.Wallet.Logging
    ( logTrace )
import Cardano.Wallet.Network
    ( ErrNetworkTip
    , ErrNetworkUnavailable
    , FollowAction (..)
    , NetworkLayer (networkTip, stakeDistribution)
    , follow
    , staticBlockchainParameters
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochLength (..)
    , EpochNo (..)
    , PoolId (..)
    , PoolOwner (..)
    , PoolRegistrationCertificate (..)
    , SlotId (..)
    , SlotNo (unSlotNo)
    )
import Control.Monad
    ( forM, forM_, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), mapExceptT, runExceptT, throwE, withExceptT )
import Control.Tracer
    ( contramap )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( foldl', nub, nubBy, sortOn )
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
import Data.Text.Class
    ( ToText (..) )
import Data.Vector.Shuffle
    ( shuffleWith )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import System.Random
    ( StdGen )

import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Information from a block relevant to monitoring stake pools.
data Block = Block
    { header :: BlockHeader
    , producer :: PoolId
    -- ^ The stake pool that minted this block.
    , poolRegistrations :: ![PoolRegistrationCertificate]
    -- ^ Any stake pools that were registered in this block.
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
monitorStakePools
    :: Trace IO Text
    -> NetworkLayer IO t Block
    -> DBLayer IO
    -> IO ()
monitorStakePools tr nl DBLayer{..} = do
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
        atomically $ readPoolProductionCursor k

    backward
        :: SlotId
        -> IO (FollowAction ErrMonitorStakePools)
    backward point = do
        liftIO . atomically $ rollbackTo point
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

        liftIO $ logInfo tr $ "Writing stake-distribution for epoch " <> pretty ep

        let registrations = concatMap poolRegistrations blocks
        liftIO $ forM_ registrations $ \registration ->
            logInfo tr $ "Discovered stake pool registration: "
                <> pretty registration

        mapExceptT atomically $ do
            lift $ putStakeDistribution ep (Map.toList dist)
            forM_ blocks $ \b ->
                withExceptT ErrMonitorStakePoolsPoolAlreadyExists $
                    putPoolProduction (header b) (producer b)

            lift $ mapM_ (uncurry putStakePoolOwner) $ concat
                [ [ (reg ^. #poolId, owner) | owner <- reg ^. #poolOwners ]
                | reg <- registrations ]

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
          :: ExceptT ErrListStakePools m [(StakePool, Maybe StakePoolMetadata)]
      }

data ErrListStakePools
     = ErrMetricsIsUnsynced (Quantity "percent" Percentage)
     | ErrListStakePoolsMetricsInconsistency ErrMetricsInconsistency
     | ErrListStakePoolsErrNetworkTip ErrNetworkTip

newStakePoolLayer
    :: DBLayer IO
    -> NetworkLayer IO t Block
    -> Trace IO StakePoolLayerMsg
    -> StakePoolLayer IO
newStakePoolLayer db@DBLayer{..} nl tr = StakePoolLayer
    { listStakePools = do
        lift $ logTrace tr MsgListStakePoolsBegin
        stakePools <- sortKnownPools
        meta <- lift $ findMetadata (map (^. #poolId) stakePools)
        pure $ zip stakePools meta
    }
  where
    sortKnownPools :: ExceptT ErrListStakePools IO [StakePool]
    sortKnownPools = do
        nodeTip <- withExceptT ErrListStakePoolsErrNetworkTip
            $ networkTip nl
        let nodeEpoch = nodeTip ^. #slotId . #epochNumber
        let genesisEpoch = block0 ^. #header . #slotId . #epochNumber

        (distr, prod) <- liftIO . atomically $ (,)
            <$> (Map.fromList <$> readStakeDistribution nodeEpoch)
            <*> (count <$> readPoolProduction nodeEpoch)

        when (Map.null distr || Map.null prod) $ do
            computeProgress nodeTip >>= throwE . ErrMetricsIsUnsynced

        if nodeEpoch == genesisEpoch
        then do
            seed <- liftIO $ atomically readSystemSeed
            combineWith (sortArbitrarily seed) distr prod mempty

        else do
            let tip = nodeTip ^. #slotId
            perfs <- liftIO $ readPoolsPerformances db epochLength tip
            combineWith (pure . sortByPerformance) distr prod perfs

    -- For each pool, look up its metadata. If metadata could not be found for a
    -- pool, the result will be 'Nothing'.
    findMetadata :: [PoolId] -> IO [Maybe StakePoolMetadata]
    findMetadata poolIds = do
        owners <- atomically $ mapM readStakePoolOwners poolIds
        -- note: this will become simpler once we cache metadata in the database
        let owners' = nub $ concat owners
        url <- getRegistryZipUrl
        let tr' = contramap (fmap MsgRegistry) tr
        getStakePoolMetadata tr' url owners' >>= \case
            Left _ -> do
                logTrace tr MsgMetadataUnavailable
                pure $ replicate (length poolIds) Nothing
            Right metas -> do
                let res = associateMetadata
                        (Map.fromList $ zip poolIds owners)
                        (zip owners' metas)
                mapM_ (logTrace tr . fst) res
                pure (map snd res)

    (block0, bp) = staticBlockchainParameters nl
    epochLength = bp ^. #getEpochLength

    combineWith
        :: ([StakePool] -> IO [StakePool])
        -> Map PoolId (Quantity "lovelace" Word64)
        -> Map PoolId (Quantity "block" Word64)
        -> Map PoolId Double
        -> ExceptT ErrListStakePools IO [StakePool]
    combineWith sortResults distr prod perfs =
        case combineMetrics distr prod perfs of
            Right x -> lift
                $ sortResults
                $ map (uncurry mkStakePool)
                $ Map.toList x
            Left e ->
                throwE $ ErrListStakePoolsMetricsInconsistency e

    poolProductionTip :: IO (Maybe BlockHeader)
    poolProductionTip = atomically $ readPoolProductionCursor 1 >>= \case
        [x] -> return $ Just x
        _ -> return Nothing

    sortByPerformance :: [StakePool] -> [StakePool]
    sortByPerformance = sortOn (Down . apparentPerformance)

    sortArbitrarily :: StdGen -> [StakePool] -> IO [StakePool]
    sortArbitrarily = shuffleWith

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
        logTrace tr $ MsgComputedProgress mDbTip nodeTip
        pure $ Quantity $ maybe minBound (`progress` nodeTip) mDbTip

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
    :: DBLayer m
    -> EpochLength
    -> SlotId
    -> m (Map PoolId Double)
readPoolsPerformances DBLayer{..} (EpochLength el) tip = do
    atomically $ fmap avg $ forM historicalEpochs $ \ep -> calculatePerformance
        (slotsInEpoch ep)
        <$> (Map.fromList <$> readStakeDistribution ep)
        <*> (count <$> readPoolProduction ep)
  where
    currentEpoch = tip ^. #epochNumber

    historicalEpochs :: [EpochNo]
    historicalEpochs
        | currentEpoch > window = [currentEpoch - window .. currentEpoch]
        | otherwise             = [0..currentEpoch]
      where
        window = 14

    slotsInEpoch :: EpochNo -> Int
    slotsInEpoch e =
        if e == currentEpoch
        then fromIntegral $ unSlotNo $ tip ^. #slotNumber
        else fromIntegral el

    -- | Performances are computed over many epochs to cope with the fact that
    -- our data is sparse (regarding stake distribution at least).
    --
    -- So the approach is the following:
    --
    -- 1. Compute performances, if available, for the last `n` epochs
    -- 2. Compute the average performance for all epochs for which we had data
    avg :: [Map PoolId Double] -> Map PoolId Double
    avg performances =
        Map.map (/ len) . Map.unionsWith (+) $ performances
      where
        len = fromIntegral $ length $ filter (not . Map.null) performances

-- | Calculate pool apparent performance over the given data. The performance
-- is a 'Double' between 0 and 1 as:
--
-- @
--     p = n / N * S / s
--   where
--     n = number of blocks produced in an epoch e
--     N = number of slots in e
--     s = stake owned by the pool in e
--     S = total stake delegated to pools in e
-- @
--
-- Note that, this apparent performance is clamped to [0,1] as it may in
-- practice, be greater than 1 if a stake pool produces more than it is
-- expected.
calculatePerformance
    :: Int
    -> Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Word64)
    -> Map PoolId Double
calculatePerformance nTotal mStake mProd =
    let
        stakeButNotProd = traverseMissing $ \_ _ -> 0
        prodButNoStake  = dropMissing
        stakeAndProd sTotal = zipWithMatched $ \_ s n ->
            if (nTotal == 0 || s == Quantity 0) then
                0
            else
                min 1 ((double n / fromIntegral nTotal) * (sTotal / double s))
    in
        Map.merge
            stakeButNotProd
            prodButNoStake
            (stakeAndProd (sumQ mStake))
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

-- | Given a mapping from 'PoolId' -> 'PoolOwner' and a mapping between
-- 'PoolOwner' <-> 'StakePoolMetadata', return a matching 'StakePoolMeta' entry
-- for every 'PoolId'.
--
-- If there is no metadata for a pool, it returns Nothing for that 'PoolId'.
-- If there is different metadata submitted by multiple owners of a pool, it returns Nothing.
-- If there is one unique metadata for a pool, it returns 'Just' the metadata for that 'PoolId'.
--
-- It also provides a log message for each association.
associateMetadata
    :: Map PoolId [PoolOwner]
    -- ^ Ordered mapping from pool to owner(s).
    -> [(PoolOwner, Maybe StakePoolMetadata)]
    -- ^ Association between owner and metadata
    -> [(StakePoolLayerMsg, Maybe StakePoolMetadata)]
associateMetadata poolOwners ownerMeta =
    map (uncurry getResult . fmap associate) $ Map.toList poolOwners
  where
    -- Filter the metadata to just the entries which were submitted by the given
    -- owners.
    associate :: [PoolOwner] -> [(PoolOwner, StakePoolMetadata)]
    associate owners = [(a, b) | (a, Just b) <- ownerMeta, a `elem` owners]

    -- Ensure that there is exactly one unique metadata per pool.
    -- Produces a log message and validated result.
    getResult
        :: PoolId
        -> [(PoolOwner, StakePoolMetadata)]
        -> (StakePoolLayerMsg, Maybe StakePoolMetadata)
    getResult pid metas = case nubBy sameMeta metas of
        [(owner, meta)] -> (MsgMetadataUsing pid owner meta, Just meta)
        [] -> (MsgMetadataMissing pid, Nothing)
        metas' -> (MsgMetadataMultiple pid metas', Nothing)

    sameMeta (_, a) (_, b) = a == b

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data StakePoolLayerMsg
    = MsgRegistry RegistryLog
    | MsgListStakePoolsBegin
    | MsgMetadataUnavailable
    | MsgMetadataUsing PoolId PoolOwner StakePoolMetadata
    | MsgMetadataMissing PoolId
    | MsgMetadataMultiple PoolId [(PoolOwner, StakePoolMetadata)]
    | MsgComputedProgress (Maybe BlockHeader) BlockHeader
    deriving (Show, Eq)

instance DefinePrivacyAnnotation StakePoolLayerMsg
instance DefineSeverity StakePoolLayerMsg where
    defineSeverity ev = case ev of
        MsgRegistry msg -> defineSeverity msg
        MsgListStakePoolsBegin -> Debug
        MsgMetadataUnavailable -> Notice
        MsgComputedProgress{} -> Debug
        MsgMetadataUsing{} -> Debug
        MsgMetadataMissing{} -> Debug
        MsgMetadataMultiple{} -> Debug

instance ToText StakePoolLayerMsg where
    toText = \case
        MsgRegistry msg -> toText msg
        MsgListStakePoolsBegin -> "Listing stake pools"
        MsgMetadataUnavailable -> "Stake pool metadata is unavailable"
        MsgComputedProgress (Just dbTip) nodeTip -> mconcat
            [ "The node tip is:\n"
            , pretty nodeTip
            , ",\nbut the last pool production stored in the db"
            , " is from:\n"
            , pretty dbTip
            ]
        MsgComputedProgress Nothing _nodeTip -> ""
        MsgMetadataUsing pid owner _ ->
            "Using stake pool metadata from " <>
            toText owner <> " for " <> toText pid
        MsgMetadataMissing pid ->
            "No stake pool metadata for " <> toText pid
        MsgMetadataMultiple pid _ ->
            "Multiple different metadata registered for " <> toText pid
