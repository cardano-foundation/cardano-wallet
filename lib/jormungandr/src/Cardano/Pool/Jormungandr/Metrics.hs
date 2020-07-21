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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module can fold over a blockchain to collect metrics about
-- Stake pools.
--
-- It interacts with:
-- - "Cardano.Wallet.Network" which provides the chain
-- - "Cardano.Pool.DB" - which can persist the metrics
-- - "Cardano.Wallet.Api.Server" - which presents the results in an endpoint
module Cardano.Pool.Jormungandr.Metrics
    ( -- * Types
      Block (..)
    , StakePoolLayer (..)

    -- * Listing stake-pools from the DB
    , newStakePoolLayer
    , ErrListStakePools (..)

      -- * Following the chain
    , monitorStakePools

      -- * Combining Metrics
    , combineMetrics

      -- * Associating metadata
    , associateMetadata

      -- * Logging
    , StakePoolLog (..)
    )
    where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists )
import Cardano.Pool.Jormungandr.Metadata
    ( RegistryLog
    , StakePoolMetadata
    , getMetadataConfig
    , getStakePoolMetadata
    , sameStakePoolMetadata
    )
import Cardano.Pool.Jormungandr.Performance
    ( readPoolsPerformances )
import Cardano.Pool.Jormungandr.Ranking
    ( EpochConstants (..), unsafeMkNonNegative )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Network
    ()
import Cardano.Wallet.Network
    ( ErrCurrentNodeTip
    , ErrNetworkUnavailable
    , FollowAction (..)
    , FollowExit (..)
    , FollowLog
    , GetStakeDistribution
    , NetworkLayer (currentNodeTip, stakeDistribution)
    , follow
    , timeInterpreter
    )
import Cardano.Wallet.Primitive.Slotting
    ( epochOf )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , CertificatePublicationTime (..)
    , EpochNo (..)
    , PoolId
    , PoolLifeCycleStatus
    , PoolOwner (..)
    , PoolRegistrationCertificate (..)
    , ProtocolParameters
    , SlotNo
    , StakePool (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.Arrow
    ( first )
import Control.Monad
    ( forM, forM_, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), mapExceptT, runExceptT, throwE, withExceptT )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( nub, nubBy, sortOn, (\\) )
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
import Data.Ratio
    ( (%) )
import Data.Text.Class
    ( ToText (..) )
import Data.Vector.Shuffle
    ( shuffleWith )
import Data.Word
    ( Word32, Word64 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import System.Random
    ( StdGen )

import qualified Cardano.Pool.Jormungandr.Ranking as Ranking
import qualified Data.List.NonEmpty as NE
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


-- | @StakePoolLayer@ is a thin layer ontop of the DB. It is /one/ value that
-- can easily be passed to the API-server, where it can be used in a simple way.
data StakePoolLayer e m = StakePoolLayer
    { getPoolLifeCycleStatus
        :: PoolId
        -> IO PoolLifeCycleStatus

    , listStakePools
        :: ExceptT e m [(StakePool, Maybe StakePoolMetadata)]

    , knownStakePools
        :: m [PoolId]
        -- ^ Get a list of known pools that doesn't require fetching things from
        -- any registry. This list comes from the registration certificates
        -- that have been seen on chain.
    }

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
    :: (GetStakeDistribution t Block IO ~ GetStakeDistribution Jormungandr Block IO)
    => Tracer IO StakePoolLog
    -> (Block, Quantity "block" Word32)
        -- ^ Genesis block and 'k'
    -> NetworkLayer IO t Block
    -> DBLayer IO
    -> IO ()
monitorStakePools tr (block0, Quantity k) nl db@DBLayer{..} = do
    cursor <- initCursor
    traceWith tr $ MsgStartMonitoring cursor
    follow nl trFollow cursor forward header >>= \case
        FollowInterrupted -> pure ()
        FollowFailure ->
            monitorStakePools tr (block0, Quantity k) nl db
        FollowRollback point -> do
            traceWith tr $ MsgRollingBackTo point
            liftIO . atomically $ rollbackTo point
            monitorStakePools tr (block0, Quantity k) nl db
  where
    initCursor :: IO [BlockHeader]
    initCursor = do
        let sl0 = block0 ^. #header . #slotNo
        atomically $ do
            forM_ (poolRegistrations block0)
                $ \r@PoolRegistrationCertificate{poolId} -> do
                readPoolRegistration poolId >>= \case
                    Nothing -> do
                        let cpt = CertificatePublicationTime sl0 minBound
                        putPoolRegistration cpt r
                    Just {} -> pure ()
            readPoolProductionCursor (max 100 (fromIntegral k))

    forward
        :: NonEmpty Block
        -> (BlockHeader, ProtocolParameters)
        -> IO (FollowAction ErrMonitorStakePools)
    forward blocks (nodeTip, _params) = handler $ do
        let epochOf' = liftIO . timeInterpreter nl . epochOf
        epochs <- NE.nub <$> mapM (epochOf' . (view (#header . #slotNo))) blocks

        distributions <- forM epochs $ \ep -> do
            liftIO $ traceWith tr $ MsgStakeDistribution ep
            withExceptT ErrMonitorStakePoolsNetworkUnavailable $
                (ep,) <$> (stakeDistribution nl ep)

        currentTip <- withExceptT ErrMonitorStakePoolsCurrentNodeTip $
            currentNodeTip nl
        when (nodeTip /= currentTip) $ throwE ErrMonitorStakePoolsWrongTip

        mapExceptT atomically $ do
            forM_ distributions $ \(ep, dist) ->
                lift $ putStakeDistribution ep (Map.toList dist)
            forM_ blocks $ \b -> do
                forM_ (poolRegistrations b) $ \pool -> do
                    let cpt = CertificatePublicationTime
                            (b ^. #header . #slotNo)
                            minBound
                    lift $ putPoolRegistration cpt pool
                    liftIO $ traceWith tr $ MsgStakePoolRegistration pool
                withExceptT ErrMonitorStakePoolsPoolAlreadyExists $
                    putPoolProduction (header b) (producer b)
      where
        handler action = runExceptT action >>= \case
            Left e -> do
                traceWith tr (MsgApplyError e)
                pure $ case e of
                    ErrMonitorStakePoolsNetworkUnavailable{} -> RetryLater
                    ErrMonitorStakePoolsCurrentNodeTip{} -> RetryLater
                    ErrMonitorStakePoolsWrongTip{} -> RetryImmediately
                    ErrMonitorStakePoolsPoolAlreadyExists{} -> ExitWith e
            Right () ->
                pure Continue

    trFollow = contramap MsgFollow tr

-- | Internal error data-type used to drive the 'forward' logic
data ErrMonitorStakePools
    = ErrMonitorStakePoolsNetworkUnavailable ErrNetworkUnavailable
    | ErrMonitorStakePoolsPoolAlreadyExists ErrPointAlreadyExists
    | ErrMonitorStakePoolsCurrentNodeTip ErrCurrentNodeTip
    | ErrMonitorStakePoolsWrongTip
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- StakePoolLayer
--------------------------------------------------------------------------------

data ErrListStakePools
     = ErrMetricsIsUnsynced (Quantity "percent" Percentage)
     | ErrListStakePoolsCurrentNodeTip ErrCurrentNodeTip
     deriving (Show)

newStakePoolLayer
    :: Tracer IO StakePoolLog
    -> BlockHeader
    -- ^ Genesis block header
    -> (EpochNo -> Quantity "lovelace" Word64 -> EpochConstants)
    -> DBLayer IO
    -> NetworkLayer IO Jormungandr Block
    -> FilePath
    -- ^ A directory to cache downloaded stake pool metadata. Will be created if
    -- it does not exist.
    -> StakePoolLayer ErrListStakePools IO
newStakePoolLayer tr block0H getEpCst db@DBLayer{..} nl metadataDir = StakePoolLayer
    { getPoolLifeCycleStatus =
        liftIO . atomically . readPoolLifeCycleStatus
    , listStakePools = do
        lift $ traceWith tr MsgListStakePoolsBegin
        stakePools <- sortKnownPools
        meta <- lift $ findMetadata (map (first (^. #poolId)) stakePools)
        pure $ zip (map fst stakePools) meta
    , knownStakePools =
        atomically listRegisteredPools
    }
  where
    epochOf' = liftIO . timeInterpreter nl . epochOf
    sortKnownPools :: ExceptT ErrListStakePools IO [(StakePool, [PoolOwner])]
    sortKnownPools = do
        nodeTip <- withExceptT ErrListStakePoolsCurrentNodeTip
            $ currentNodeTip nl
        nodeEpoch <- epochOf' $ nodeTip ^. #slotNo
        genesisEpoch <- epochOf' $ block0H ^. #slotNo

        (distr, prod, prodTip) <- liftIO . atomically $ (,,)
            <$> (Map.fromList <$> readStakeDistribution nodeEpoch)
            <*> readTotalProduction
            <*> readPoolProductionTip

        when (Map.null distr || Map.null prod) $ do
            liftIO $ traceWith tr $ MsgComputedProgress prodTip nodeTip
            throwE $ ErrMetricsIsUnsynced $ computeProgress prodTip nodeTip

        if nodeEpoch == genesisEpoch
        then do
            seed <- liftIO $ atomically readSystemSeed
            let epCst = getEpCst 0
            combineWith (sortArbitrarily seed) epCst distr prod mempty

        else do
            currentEpoch <- epochOf' $ prodTip ^. #slotNo
            perfs <- liftIO $ readPoolsPerformances db currentEpoch
            let epCst = getEpCst currentEpoch
            combineWith (pure . sortByDesirability) epCst distr prod perfs

    readPoolProductionTip = readPoolProductionCursor 1 <&> \case
        []  -> block0H
        h:_ -> h

    -- For each pool, look up its metadata. If metadata could not be found for a
    -- pool, the result will be 'Nothing'.
    findMetadata :: [(PoolId, [PoolOwner])] -> IO [Maybe StakePoolMetadata]
    findMetadata pools = do
        -- note: this will become simpler once we cache metadata in the database
        let (poolIds, owners) = unzip pools
        let owners' = nub $ concat owners
        cfg <- getMetadataConfig metadataDir
        let tr' = contramap MsgRegistry tr
        getStakePoolMetadata tr' cfg owners' >>= \case
            Left _ -> do
                traceWith tr MsgMetadataUnavailable
                pure $ replicate (length poolIds) Nothing
            Right metas -> do
                let res = associateMetadata pools (zip owners' metas)
                mapM_ (traceWith tr . fst) res
                pure $ map snd res

    combineWith
        :: ([(StakePool, [PoolOwner])] -> IO [(StakePool, [PoolOwner])])
        -> (Quantity "lovelace" Word64 -> EpochConstants)
        -> Map PoolId (Quantity "lovelace" Word64)
        -> Map PoolId (Quantity "block" Word64)
        -> Map PoolId Double
        -> ExceptT ErrListStakePools IO [(StakePool, [PoolOwner])]
    combineWith sortResults mkEpCst distr prod perfs = do
        liftIO $ do
            traceWith tr $ MsgUsingTotalStakeForRanking totalStake
            traceWith tr $ MsgUsingRankingEpochConstants epConstants
        let ps = combineMetrics distr prod perfs
        lift $ do
            let len = fromIntegral (length ps)
            let avg = if null ps
                    then 0
                    else sum ((\(_,_,c) -> c) <$> (Map.elems ps)) / len
            ns <- readNewcomers db (Map.keys ps) avg
            pools <- atomically $
                Map.traverseMaybeWithKey mergeRegistration (ps <> ns)
            sortResults $ Map.elems pools
      where
        totalStake =
            Quantity $ Map.foldl' (\a (Quantity b) -> a + b) 0 distr

        epConstants = mkEpCst totalStake

        mergeRegistration poolId (stake, production, performance) =
            fmap (mkStakePool . snd) <$> readPoolRegistration poolId
          where
            mkStakePool PoolRegistrationCertificate{poolCost,poolMargin,poolOwners} =
                ( StakePool
                    { poolId
                    , stake
                    , production
                    , performance
                    , cost = poolCost
                    , margin = poolMargin
                    , pledge = Quantity 0 -- 'pledge' only exists in cardano-node's realm
                    , saturation =
                        Ranking.saturation epConstants totalStake stake
                    , desirability =
                        Ranking.desirability epConstants $ Ranking.Pool
                            (unsafeMkPercentage 0) -- pool leader pledge
                            poolCost
                            poolMargin
                            (unsafeMkNonNegative performance)
                    }
                , poolOwners
                )

    sortByDesirability :: [(StakePool, a)] -> [(StakePool, a)]
    sortByDesirability = sortOn (Down . view #desirability . fst)

    sortArbitrarily :: StdGen -> [a] -> IO [a]
    sortArbitrarily = shuffleWith

    computeProgress
        :: BlockHeader -- ^ ... / denominator
        -> BlockHeader -- ^ numerator /...
        -> Quantity "percent" Percentage
    computeProgress prodTip nodeTip =
        if s1 == 0
        then Quantity minBound
        else Quantity . unsafeMkPercentage $ (toW s0) % (toW s1)
      where
        s0 = getQuantity $ prodTip ^. #blockHeight
        s1 = getQuantity $ nodeTip ^. #blockHeight
        toW :: Integral i => i -> Integer
        toW = fromIntegral

readNewcomers
    :: Monad m
    => DBLayer m
    -> [PoolId]
    -> Double
    -> m (Map PoolId (Quantity "lovelace" Word64, Quantity "block" Word64, Double))
readNewcomers DBLayer{..} elders avg = do
    pids <- atomically listRegisteredPools
    pure $ Map.fromList $ zip
        (pids \\ elders)
        (repeat (Quantity 0, Quantity 0, avg))

-- | Combines three different sources of data into one:
--
-- 1. A stake-distribution map
-- 2. A pool-production map
-- 3. A pool-performance map
--
-- If a pool is in 2 or 3 but not in 1, it means that the pool has been
-- de-registered.
--
-- If a pool is in (1+2) but not in (3), it simply means it has produced 0
-- blocks so far.
combineMetrics
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Word64)
    -> Map PoolId Double
    -> Map PoolId
        ( Quantity "lovelace" Word64
        , Quantity "block" Word64
        , Double
        )
combineMetrics mStake mProd mPerf =
    let
        mActivity = zipWithRightDefault (,) (Quantity 0) mStake mProd
    in
        zipWithRightDefault unzipZip3 0 mActivity mPerf
  where
    unzipZip3 :: (a,b) -> c -> (a,b,c)
    unzipZip3 (a,b) c = (a,b,c)

-- | Combine two maps with the given zipping function. It defaults when elements
-- of the first map (left) are not present in the second (right), and discards
-- elements in the second (right) map are not present in the first (left).
--
-- Example:
--
-- @
-- let m1 = Map.fromList [(1, 'a'), (2, 'b'), (3, 'c')]
-- let m2 = Map.fromList [(2, True)]
-- @
--
-- >>> zipWithRightDefault (,) False m1 m2
-- Map.fromList [(1, ('a', False)), (2, ('b', True)), (3, ('c', False))])
zipWithRightDefault
    :: Ord k
    => (l -> r -> a)
    -> r
    -> Map k l
    -> Map k r
    -> Map k a
zipWithRightDefault combine rZero =
    Map.merge leftButNotRight rightButNotLeft bothPresent
  where
    leftButNotRight = traverseMissing $ \_k l -> pure (combine l rZero)
    rightButNotLeft = dropMissing
    bothPresent     = zipWithMatched  $ \_k l r -> (combine l r)

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
    :: [(PoolId, [PoolOwner])]
    -- ^ Ordered mapping from pool to owner(s).
    -> [(PoolOwner, Maybe StakePoolMetadata)]
    -- ^ Association between owner and metadata
    -> [(StakePoolLog, Maybe StakePoolMetadata)]
associateMetadata poolOwners ownerMeta =
    map (uncurry getResult . fmap associate) poolOwners
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
        -> (StakePoolLog, Maybe StakePoolMetadata)
    getResult pid metas = case nubBy sameMeta metas of
        [(owner, meta)] -> (MsgMetadataUsing pid owner meta, Just meta)
        [] -> (MsgMetadataMissing pid, Nothing)
        metas' -> (MsgMetadataMultiple pid metas', Nothing)

    sameMeta (_, a) (_, b) = sameStakePoolMetadata a b

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Messages associated with stake pool layer.
data StakePoolLog
    = MsgRegistry RegistryLog
    | MsgListStakePoolsBegin
    | MsgMetadataUnavailable
    | MsgMetadataUsing PoolId PoolOwner StakePoolMetadata
    | MsgMetadataMissing PoolId
    | MsgMetadataMultiple PoolId [(PoolOwner, StakePoolMetadata)]
    | MsgComputedProgress BlockHeader BlockHeader
    | MsgStartMonitoring [BlockHeader]
    | MsgFollow FollowLog
    | MsgStakeDistribution EpochNo
    | MsgStakePoolRegistration PoolRegistrationCertificate
    | MsgRollingBackTo SlotNo
    | MsgApplyError ErrMonitorStakePools
    | MsgUsingRankingEpochConstants EpochConstants
    | MsgUsingTotalStakeForRanking (Quantity "lovelace" Word64)
    deriving (Show, Eq)

instance HasPrivacyAnnotation StakePoolLog
instance HasSeverityAnnotation StakePoolLog where
    getSeverityAnnotation ev = case ev of
        MsgRegistry msg -> getSeverityAnnotation msg
        MsgListStakePoolsBegin -> Debug
        MsgMetadataUnavailable -> Notice
        MsgComputedProgress{} -> Debug
        MsgMetadataUsing{} -> Debug
        MsgMetadataMissing{} -> Debug
        MsgMetadataMultiple{} -> Debug
        MsgStartMonitoring _ -> Info
        MsgFollow msg -> getSeverityAnnotation msg
        MsgStakeDistribution _ -> Info
        MsgStakePoolRegistration _ -> Info
        MsgRollingBackTo _ -> Info
        MsgUsingRankingEpochConstants ec
            | oddlySmallRewards ec
                -> Notice
            | otherwise
                -> Debug
        MsgUsingTotalStakeForRanking s
            | oddlySmallTotalStake s
                -> Notice
            | otherwise
                -> Debug
        MsgApplyError e -> case e of
            ErrMonitorStakePoolsNetworkUnavailable{} -> Notice
            ErrMonitorStakePoolsCurrentNodeTip{} -> Notice
            ErrMonitorStakePoolsWrongTip{} -> Debug
            ErrMonitorStakePoolsPoolAlreadyExists{} -> Debug

instance ToText StakePoolLog where
    toText = \case
        MsgRegistry msg -> toText msg
        MsgListStakePoolsBegin -> "Listing stake pools"
        MsgMetadataUnavailable -> "Stake pool metadata is unavailable"
        MsgComputedProgress prodTip nodeTip -> mconcat
            [ "The node tip is:\n"
            , pretty nodeTip
            , ",\nbut the last pool production stored in the db"
            , " is from:\n"
            , pretty prodTip
            ]
        MsgMetadataUsing pid owner _ ->
            "Using stake pool metadata from " <>
            toText owner <> " for " <> toText pid
        MsgMetadataMissing pid ->
            "No stake pool metadata for " <> toText pid
        MsgMetadataMultiple pid _ ->
            "Multiple different metadata registered for " <> toText pid
        MsgStartMonitoring cursor -> mconcat
            [ "Monitoring stake pools. Currently at "
            , case cursor of
                [] -> "genesis"
                _  -> pretty (last cursor)
            ]
        MsgFollow msg ->
            toText msg
        MsgStakeDistribution ep ->
            "Writing stake-distribution for epoch " <> pretty ep
        MsgStakePoolRegistration pool ->
            "Discovered stake pool registration: " <> pretty pool
        MsgUsingRankingEpochConstants ec
            | oddlySmallRewards ec -> mconcat
                [ "The total rewards for this epoch are oddly small. "
                , "This may cause stake pool ranking to be unreliable. "
                , "Epoch constants are: "
                , pretty ec
                ]
            | otherwise ->
                "Using ranking epoch constants: " <> pretty ec
        MsgUsingTotalStakeForRanking s
            | oddlySmallTotalStake s -> mconcat
                [ "The total stake is oddly small ("
                , pretty s
                , "). This may result in unreliable epoch constants."
                ]
            | otherwise -> mconcat
                [ "The total stake used to determine epoch constants was: "
                , pretty s
                ]
        MsgRollingBackTo point ->
            "Rolling back to " <> pretty point
        MsgApplyError e -> case e of
            ErrMonitorStakePoolsNetworkUnavailable{} ->
                "Network is not available."
            ErrMonitorStakePoolsCurrentNodeTip{} ->
                "Network is not available."
            ErrMonitorStakePoolsWrongTip{} ->
                "Race condition when fetching stake distribution."
            ErrMonitorStakePoolsPoolAlreadyExists{} ->
                ""

oddlySmallRewards :: EpochConstants -> Bool
oddlySmallRewards ec = (ec ^. #totalRewards) <= Quantity 100

oddlySmallTotalStake :: Quantity "lovelace" Word64 -> Bool
oddlySmallTotalStake = (<= Quantity 100)
