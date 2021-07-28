{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module provides tools to collect a consistent view of stake pool data,
-- as provided through @StakePoolLayer@.
module Cardano.Wallet.Shelley.Pools
    ( StakePoolLayer (..)
    , newStakePoolLayer
    , monitorStakePools
    , monitorMetadata

    -- * Logs
    , StakePoolLog (..)
    )
    where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Pool.DB
    ( DBLayer (..), ErrPointAlreadyExists (..), readPoolLifeCycleStatus )
import Cardano.Pool.Metadata
    ( Manager
    , StakePoolMetadataFetchLog
    , UrlBuilder
    , defaultManagerSettings
    , fetchDelistedPools
    , fetchFromRemote
    , healthCheck
    , identityUrlBuilder
    , newManager
    , registryUrlBuilder
    , toHealthCheckSMASH
    )
import Cardano.Wallet.Api.Types
    ( ApiT (..), HealthCheckSMASH (..), toApiEpochInfo )
import Cardano.Wallet.Byron.Compatibility
    ( toByronBlockHeader )
import Cardano.Wallet.Network
    ( ChainFollowLog (..), ChainFollower (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException (..)
    , TimeInterpreter
    , epochOf
    , interpretQuery
    , unsafeExtendSafeZone
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , CertificatePublicationTime (..)
    , ChainPoint (..)
    , EpochNo (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , PoolCertificate (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , PoolMetadataGCStatus (..)
    , PoolMetadataSource (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , RewardParams (..)
    , RewardProvenancePool (..)
    , Settings (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StakePoolMetadata
    , StakePoolMetadataHash
    , StakePoolsSummary (..)
    , getPoolRegistrationCertificate
    , getPoolRetirementCertificate
    , unSmashServer
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Registry
    ( AfterThreadLog, traceAfterThread )
import Cardano.Wallet.Shelley.Compatibility
    ( StandardCrypto
    , fromAllegraBlock
    , fromAlonzoBlock
    , fromMaryBlock
    , fromShelleyBlock
    , getProducer
    , toShelleyBlockHeader
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.Monad
    ( forM, forM_, forever, void, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Retry
    ( RetryStatus (..), constantDelay, retrying )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.List
    ( nub, (\\) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, mapMissing, zipWithMatched )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Set
    ( Set )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock.POSIX
    ( getPOSIXTime, posixDayLength )
import Data.Tuple.Extra
    ( dupe )
import Data.Void
    ( Void )
import Data.Word
    ( Word64 )
import Fmt
    ( fixedF, pretty )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, HardForkBlock (..) )
import System.Random
    ( RandomGen, randoms )
import UnliftIO.Concurrent
    ( forkFinally, killThread, threadDelay )
import UnliftIO.Exception
    ( finally )
import UnliftIO.IORef
    ( IORef, newIORef, readIORef, writeIORef )
import UnliftIO.STM
    ( TBQueue
    , TVar
    , newTBQueue
    , readTBQueue
    , readTVarIO
    , writeTBQueue
    , writeTVar
    )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified UnliftIO.STM as STM

{-------------------------------------------------------------------------------
    Stake Pool Layer
-------------------------------------------------------------------------------}
data StakePoolLayer = StakePoolLayer
    { getPoolLifeCycleStatus
        :: PoolId
        -> IO PoolLifeCycleStatus

    , knownPools
        :: IO (Set PoolId)

    -- | List pools based given the the amount of stake the user intends to
    --   delegate, which affects the size of the rewards and the ranking of
    --   the pools.
    --
    -- Pools with a retirement epoch earlier than or equal to the specified
    -- epoch will be excluded from the result.
    --
    , listStakePools
        :: EpochNo
        -- Exclude all pools that retired in or before this epoch.
        -> Coin
        -> IO [Api.ApiStakePool]

    , forceMetadataGC :: IO ()

    , putSettings :: Settings -> IO ()

    , getSettings :: IO Settings

    , getGCMetadataStatus :: IO PoolMetadataGCStatus
    }

newStakePoolLayer
    :: forall sc. ()
    => TVar PoolMetadataGCStatus
    -> NetworkLayer IO (CardanoBlock sc)
    -> DBLayer IO
    -> IO ()
    -> IO StakePoolLayer
newStakePoolLayer gcStatus nl db@DBLayer {..} restartSyncThread = do
    pure $ StakePoolLayer
        { getPoolLifeCycleStatus = _getPoolLifeCycleStatus
        , knownPools = _knownPools
        , listStakePools = _listPools
        , forceMetadataGC = _forceMetadataGC
        , putSettings = _putSettings
        , getSettings = _getSettings
        , getGCMetadataStatus = _getGCMetadataStatus
        }
  where
    _getPoolLifeCycleStatus
        :: PoolId -> IO PoolLifeCycleStatus
    _getPoolLifeCycleStatus pid =
        liftIO $ atomically $ readPoolLifeCycleStatus pid

    _knownPools
        :: IO (Set PoolId)
    _knownPools =
        Set.fromList <$> liftIO (atomically listRegisteredPools)

    _putSettings :: Settings -> IO ()
    _putSettings settings = do
        atomically (gcMetadata >> putSettings settings)
        restartSyncThread

      where
        -- clean up metadata table if the new sync settings suggests so
        gcMetadata = do
            oldSettings <- readSettings
            case (poolMetadataSource oldSettings, poolMetadataSource settings) of
                (_, FetchNone) -> -- this is necessary if it's e.g. the first time
                                  -- we start the server with the new feature
                                  -- and the database has still stuff in it
                    removePoolMetadata
                (old, new)
                    | old /= new -> removePoolMetadata
                _ -> pure ()

    _getSettings :: IO Settings
    _getSettings = liftIO $ atomically readSettings

    _listPools
        :: EpochNo
        -- Exclude all pools that retired in or before this epoch.
        -> Coin
        -> IO [Api.ApiStakePool]
    _listPools currentEpoch userStake = do
        mnlData <- stakeDistribution nl
        case mnlData of
                -- we seem to be in the Byron era and cannot stake
            Nothing -> pure []
            Just nlData -> do
                dbData <- readPoolDbData db currentEpoch
                seed   <- atomically readSystemSeed
                sortByReward seed
                    <$> listPools (timeInterpreter nl) nlData dbData userStake
      where
        -- Sort by non-myopic member rewards where pools with equal rewards
        -- appear in random order.
        -- This randomness is stable for identical requests,
        -- but different for different instances of the server.
        sortByReward
            :: RandomGen g => g -> [Api.ApiStakePool] -> [Api.ApiStakePool]
        sortByReward seed = sortRandomOn seed (Down . rewards)
            where rewards = view (#metrics . #nonMyopicMemberRewards)

    _forceMetadataGC :: IO ()
    _forceMetadataGC = do
        -- We force a GC by resetting last sync time to 0 (start of POSIX
        -- time) and have the metadata thread restart.
        lastGC <- atomically readLastMetadataGC
        STM.atomically $ writeTVar gcStatus $ maybe NotStarted Restarting lastGC
        atomically $ putLastMetadataGC $ fromIntegral @Int 0
        restartSyncThread

    _getGCMetadataStatus =
        readTVarIO gcStatus

-- | Sort a list in a way where ties are broken randomly using a random seed.
sortRandomOn :: (RandomGen g, Ord b) => g -> (a -> b) -> [a] -> [a]
sortRandomOn seed f
    = map snd
    . L.sortOn (\(nonce,a) -> (f a, nonce))
    . zip (randoms seed :: [Int])

{-------------------------------------------------------------------------------
    Rewards and scoring

    Heinrich:
        The local state queries to the node that compute rewards and related
        information are currently (2021-08) very slow.
        We duplicate the computation of the non-myopic member rewards
        here in order to be better able to cache local state query results.
        Ideally, these computations should only be done in the ledger code.
-------------------------------------------------------------------------------}

percentOf :: Percentage -> Coin -> Coin
percentOf = fractionOf . getPercentage

fractionOf :: Rational -> Coin -> Coin
fractionOf r (Coin x) = Coin . round $ r * fromIntegral x

oneMinus :: Percentage -> Percentage
oneMinus p = unsafeMkPercentage $ 1 - getPercentage p

proportionTo :: Coin -> Coin -> Rational
proportionTo _        (Coin 0) = 0
proportionTo (Coin x) (Coin y) = fromIntegral x / fromIntegral y

z0 :: RewardParams -> Rational
z0 RewardParams{nOpt} = 1 / fromIntegral nOpt

nonMyopicMemberReward
    :: RewardParams
    -> RewardProvenancePool
    -> Bool -- ^ The pool ranks in the top @nOpt@ pools
    -> Coin -- ^ stake that the member wants to delegate
    -> Coin
nonMyopicMemberReward rp RewardProvenancePool{..} isTop tcoin
    | ownerStake < ownerPledge = Coin 0
    | otherwise
        = afterFees cost margin
        $ performanceEstimate `fractionOf` optimalRewards rp s sigma_nonmyopic
  where
    s     = ownerStakeRelative
    sigma = stakeRelative
    t     = tcoin `proportionTo` (totalStake rp)

    sigma_nonmyopic
        | isTop      = max (getPercentage sigma + t) (z0 rp)
        | otherwise  = getPercentage s + t

-- | Subtract fixed and margin fees from a 'Coin'.
afterFees :: Coin -> Percentage -> Coin -> Coin
afterFees cost margin x = case x `Coin.subtract` cost of
    Just y  -> oneMinus margin `percentOf` y
    Nothing -> Coin 0

-- | Optimal rewards for a stake pool
-- according to Eq.(2) of Section 5.5.3 in SL-D1.
--
-- > optimalRewards s sigma
--
-- NOTE: This computation uses 'Double' internally
-- and is only suitable for the purpose of ranking,
-- not for computing actual monetary rewards.
optimalRewards :: RewardParams -> Percentage -> Rational -> Coin
optimalRewards params s sigma = Coin . round
    $ factor * (fromIntegral . unCoin . r $ params)
  where
    factor = 1 / (1 + a0_)
        * ( sigma' + s' * a0_ * (sigma' - s'*(z0_-sigma')/z0_) / z0_ )

    z0_, a0_, sigma', s' :: Double
    z0_    = fromRational (z0 params)
    a0_    = fromRational (a0 params)
    sigma' = min (fromRational sigma) z0_
    s'     = min (fromRational $ getPercentage s) z0_

-- | The desirabilty of a pool is equal to the member rewards at saturation
desirability :: RewardParams -> RewardProvenancePool -> Coin
desirability rp RewardProvenancePool{..}
    = afterFees cost margin
    $ performanceEstimate `fractionOf` optimalRewards rp ownerStakeRelative (z0 rp)

-- | The saturation of a pool is the ratio of the current pool stake
-- to the fully saturated stake.
poolSaturation :: RewardParams -> RewardProvenancePool -> Double
poolSaturation rp RewardProvenancePool{stakeRelative}
    = fromRational (getPercentage stakeRelative) / fromRational (z0 rp)

data PoolScore = PoolScore
    { _desirability :: Coin
    , _nonMyopicMemberReward :: Coin
    }

-- | Compute the desirability and non-myopic rewards for all pools.
--
-- To compute the non-myopic rewards, we need to know all pools
-- in order to rank them by desirability,
-- and we need to know the stake that the user wants to delegate.
scorePools
    :: Ord poolId
    => RewardParams
    -> Map poolId (RewardProvenancePool, a)
    -> Coin -- ^ Stake that the user wants to delegate
    -> Map poolId (PoolScore, RewardProvenancePool, a)
scorePools params pools t
    = Map.fromList $ zipWith doScore sortedByDesirability areTop
  where
    RewardParams{nOpt} = params
    areTop = replicate nOpt True ++ repeat False

    doScore (d, (pid, pool, a)) isTop = (pid, (score, pool, a))
      where
        score = PoolScore
            { _nonMyopicMemberReward
                = nonMyopicMemberReward params pool isTop t
            , _desirability = d
            }

    sortedByDesirability
        = L.sortOn (Down . fst)
        . map (\(pid,(pool, a)) -> (desirability params pool, (pid, pool, a)))
        $ Map.toList pools

{-------------------------------------------------------------------------------
    List Stake Pools
-------------------------------------------------------------------------------}
-- | Create a presentable list of stake pools
-- by combining LocalStateQuery data and DB data
-- and then scoring the pools.
listPools
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> StakePoolsSummary
    -> Map PoolId PoolDbData
    -> Coin
    -> IO [Api.ApiStakePool]
listPools ti StakePoolsSummary{rewardParams,pools} dbData userStake =
    traverse mkApiPool $ Map.toList $ scorePools rewardParams allPools userStake
  where
    allPools
        = Map.merge lsqButNoDb dbButNoLsq bothPresent pools dbData
    bothPresent = zipWithMatched (\_pid pool db -> (pool, db))
    lsqButNoDb  = dropMissing
    -- When a pool is registered (with a registration certificate) but not
    -- currently active (and therefore not causing pool metrics data to be
    -- available over local state query), we use a default value of /zero/
    -- for all stake pool metric values so that the pool can still be
    -- included in the list of all known stake pools:
    dbButNoLsq = mapMissing $ \_ db ->
        let ownerStake_ = poolPledge $ registrationCert db
            poolDefault = RewardProvenancePool
                { stakeRelative = minBound
                , ownerPledge = poolPledge $ registrationCert db
                    -- To get a first impression of the pool desirability
                    -- and non-myopic member rewards,
                    -- we assume that the pool onwer is honest and honors
                    -- their pledge.
                , ownerStake = ownerStake_
                , ownerStakeRelative = unsafeMkPercentage 
                    $ ownerStake_ `proportionTo` (totalStake rewardParams)
                , cost = poolCost $ registrationCert db
                , margin = poolMargin $ registrationCert db
                , performanceEstimate = 1
                }
        in (poolDefault, db)

    mkApiPool
        :: (PoolId, (PoolScore, RewardProvenancePool, PoolDbData))
        -> IO Api.ApiStakePool
    mkApiPool (pid, (score, pool, db)) = do
        let mRetirementEpoch = retirementEpoch <$> retirementCert db
        retirementEpochInfo <- traverse
            (interpretQuery (unsafeExtendSafeZone ti) . toApiEpochInfo)
            mRetirementEpoch
        let pledge = poolPledge $ registrationCert db
        pure $ Api.ApiStakePool
            { Api.id = (ApiT pid)
            , Api.metrics = Api.ApiStakePoolMetrics
                { Api.nonMyopicMemberRewards =
                    Api.coinToQuantity $ _nonMyopicMemberReward score
                , Api.desirabilityScore = fromIntegral $ unCoin $ _desirability score
                , Api.relativeStake = Quantity $ stakeRelative pool
                , Api.ownerStake = Api.coinToQuantity $ ownerStake pool
                , Api.saturation = poolSaturation rewardParams pool
                , Api.producedBlocks =
                    (fmap fromIntegral . nProducedBlocks) db
                }
            , Api.metadata =
                ApiT <$> metadata db
            , Api.cost =
                Api.coinToQuantity $ poolCost $ registrationCert db
            , Api.pledge =
                Api.coinToQuantity pledge
            , Api.margin =
                Quantity $ poolMargin $ registrationCert db
            , Api.retirement =
                retirementEpochInfo
            , Api.flags =
                [ Api.Delisted | delisted db ]
                ++ [ Api.OwnerStakeLowerThanPledge | ownerStake pool < pledge ]
            }

{-------------------------------------------------------------------------------
    Pool Database
    obtained by chain following
-------------------------------------------------------------------------------}
-- | Stake pool-related data that has been read from the database.
data PoolDbData = PoolDbData
    { registrationCert :: PoolRegistrationCertificate
    , retirementCert :: Maybe PoolRetirementCertificate
    , nProducedBlocks :: Quantity "block" Word64
    , metadata :: Maybe StakePoolMetadata
    , delisted :: Bool
    }

-- | Combines all the chain-following data into a single map
combineChainData
    :: Map PoolId PoolRegistrationCertificate
    -> Map PoolId PoolRetirementCertificate
    -> Map PoolId (Quantity "block" Word64)
    -> Map StakePoolMetadataHash StakePoolMetadata
    -> Set PoolId
    -> Map PoolId PoolDbData
combineChainData registrationMap retirementMap prodMap metaMap delistedSet =
    Map.map mkPoolDbData $
        Map.merge
            registeredNoProductions
            notRegisteredButProducing
            bothPresent
            registrationMap
            prodMap
  where
    registeredNoProductions = traverseMissing $ \_k cert ->
        pure (cert, Quantity 0)

    -- Ignore blocks produced by BFT nodes.
    notRegisteredButProducing = dropMissing

    bothPresent = zipWithMatched $ const (,)

    mkPoolDbData
        :: (PoolRegistrationCertificate, Quantity "block" Word64)
        -> PoolDbData
    mkPoolDbData (registrationCert, n) =
        PoolDbData registrationCert mRetirementCert n meta delisted
      where
        metaHash = snd <$> poolMetadata registrationCert
        meta = flip Map.lookup metaMap =<< metaHash
        mRetirementCert =
            Map.lookup (view #poolId registrationCert) retirementMap
        delisted = view #poolId registrationCert `Set.member` delistedSet

readPoolDbData :: DBLayer IO -> EpochNo -> IO (Map PoolId PoolDbData)
readPoolDbData DBLayer {..} currentEpoch = atomically $ do
    lifeCycleData <- listPoolLifeCycleData currentEpoch
    let registrationCertificates = lifeCycleData
            & mapMaybe getPoolRegistrationCertificate
            & fmap (first (view #poolId) . dupe)
            & Map.fromList
    let retirementCertificates = lifeCycleData
            & mapMaybe getPoolRetirementCertificate
            & fmap (first (view #poolId) . dupe)
            & Map.fromList
    combineChainData
        registrationCertificates
        retirementCertificates
        <$> readTotalProduction
        <*> readPoolMetadata
        <*> (Set.fromList <$> readDelistedPools)

--
-- Monitoring stake pool
--

monitorStakePools
    :: Tracer IO StakePoolLog
    -> NetworkParameters
    -> NetworkLayer IO (CardanoBlock StandardCrypto)
    -> DBLayer IO
    -> IO ()
monitorStakePools tr (NetworkParameters gp sp _pp) nl DBLayer{..} =
    monitor =<< mkLatestGarbageCollectionEpochRef
  where
    monitor latestGarbageCollectionEpochRef = do
        let rollForward = forward latestGarbageCollectionEpochRef

        let rollback point = do
                liftIO . atomically $ rollbackTo $ pseudoPointSlot point
                -- The DB will always rollback to the requested slot, so we
                -- return it.
                return point

            -- See NOTE [PointSlotNo]
            pseudoPointSlot :: ChainPoint -> SlotNo
            pseudoPointSlot ChainPointAtGenesis = SlotNo 0
            pseudoPointSlot (ChainPoint slot _) = slot

            toChainPoint :: BlockHeader -> ChainPoint
            toChainPoint (BlockHeader  0 _ _ _) = ChainPointAtGenesis
            toChainPoint (BlockHeader sl _ h _) = ChainPoint sl h

        chainSync nl (contramap MsgChainMonitoring tr) $ ChainFollower
            { readLocalTip = map toChainPoint <$> initCursor
            , rollForward  = rollForward
            , rollBackward = rollback
            }

    GenesisParameters  { getGenesisBlockHash  } = gp
    SlottingParameters { getSecurityParameter } = sp

    -- In order to prevent the pool database from growing too large over time,
    -- we regularly perform garbage collection by removing retired pools from
    -- the database.
    --
    -- However, there is no benefit to be gained from collecting garbage more
    -- than once per epoch. In order to avoid invoking the garbage collector
    -- too frequently, we keep a reference to the latest epoch for which
    -- garbage collection was performed.
    --
    mkLatestGarbageCollectionEpochRef :: IO (IORef EpochNo)
    mkLatestGarbageCollectionEpochRef = newIORef minBound

    initCursor :: IO [BlockHeader]
    initCursor = atomically $ listHeaders (max 100 k)
      where k = fromIntegral $ getQuantity getSecurityParameter

    forward
        :: IORef EpochNo
        -> NonEmpty (CardanoBlock StandardCrypto)
        -> BlockHeader
        -> IO ()
    forward latestGarbageCollectionEpochRef blocks _ = do
        atomically $ forAllAndLastM blocks forAllBlocks forLastBlock
      where
        forAllBlocks = \case
            BlockByron _ -> do
                pure ()
            BlockShelley blk -> do
                forEachShelleyBlock (fromShelleyBlock gp blk) (getProducer blk)
            BlockAllegra blk ->
                forEachShelleyBlock (fromAllegraBlock gp blk) (getProducer blk)
            BlockMary blk ->
                forEachShelleyBlock (fromMaryBlock gp blk) (getProducer blk)
            BlockAlonzo blk ->
                forEachShelleyBlock (fromAlonzoBlock gp blk) (getProducer blk)

        forLastBlock = \case
            BlockByron blk ->
                putHeader (toByronBlockHeader gp blk)
            BlockShelley blk ->
                putHeader (toShelleyBlockHeader getGenesisBlockHash blk)
            BlockAllegra blk ->
                putHeader (toShelleyBlockHeader getGenesisBlockHash blk)
            BlockMary blk ->
                putHeader (toShelleyBlockHeader getGenesisBlockHash blk)
            BlockAlonzo blk ->
                putHeader (toShelleyBlockHeader getGenesisBlockHash blk)

        forEachShelleyBlock (blk, certificates) poolId = do
            let header = view #header blk
            let slot = view #slotNo header
            handleErr (putPoolProduction header poolId)
            garbageCollectPools slot latestGarbageCollectionEpochRef
            putPoolCertificates slot certificates

        handleErr action = runExceptT action
            >>= \case
                Left e ->
                    liftIO $ traceWith tr $ MsgErrProduction e
                Right () ->
                    pure ()

        -- | Like 'forM_', except runs the second action for the last element as
        -- well (in addition to the first action).
        forAllAndLastM :: (Monad m)
            => NonEmpty a
            -> (a -> m b) -- ^ action to run for all elements
            -> (a -> m c) -- ^ action to run for the last element
            -> m ()
        {-# INLINE forAllAndLastM #-}
        forAllAndLastM ne a1 a2 = go (NE.toList ne)
          where
            go []  = pure ()
            go [x] = a1 x >> a2 x >> go []
            go (x:xs) = a1 x >> go xs

    -- Perform garbage collection for pools that have retired.
    --
    -- To avoid any issues with rollback, we err on the side of caution and
    -- only garbage collect pools that retired /at least/ two epochs before
    -- the current epoch.
    --
    -- Rationale:
    --
    -- When crossing an epoch boundary, there is a period of time during which
    -- blocks from the previous epoch are still within the rollback window.
    --
    -- If we've only just crossed the boundary from epoch e into epoch e + 1,
    -- this means that blocks from epoch e are still within the rollback
    -- window. If the chain we've just been following gets replaced with
    -- another chain in which the retirement certificate for some pool p is
    -- revoked (with a registration certificate that appears before the end
    -- of epoch e), then the retirement for pool p will have been cancelled.
    --
    -- However, assuming the rollback window is always guaranteed to be less
    -- than one epoch in length, retirements that occurred two epochs ago can
    -- no longer be affected by rollbacks. Therefore, if we see a retirement
    -- that occurred two epochs ago that has not subsquently been superseded,
    -- it should be safe to garbage collect that pool.
    --
    garbageCollectPools currentSlot latestGarbageCollectionEpochRef = do
        let ti = timeInterpreter nl
        liftIO (runExceptT (interpretQuery ti (epochOf currentSlot))) >>= \case
            Left _ -> return ()
            Right currentEpoch | currentEpoch < 2 -> return ()
            Right currentEpoch -> do
                    let latestRetirementEpoch = currentEpoch - 2
                    latestGarbageCollectionEpoch <-
                        liftIO $ readIORef latestGarbageCollectionEpochRef
                    -- Only perform garbage collection once per epoch:
                    when (latestRetirementEpoch > latestGarbageCollectionEpoch) $ do
                        liftIO $ do
                            writeIORef
                                latestGarbageCollectionEpochRef
                                latestRetirementEpoch
                            traceWith tr $
                                MsgStakePoolGarbageCollection $
                                PoolGarbageCollectionInfo
                                    {currentEpoch, latestRetirementEpoch}
                        void $ removeRetiredPools latestRetirementEpoch

    -- For each pool certificate in the given list, add an entry to the
    -- database that associates the certificate with the specified slot
    -- number and the relative position of the certificate in the list.
    --
    -- The order of certificates within a slot is significant: certificates
    -- that appear later take precedence over those that appear earlier on.
    --
    -- Precedence is determined by the 'readPoolLifeCycleStatus' function.
    --
    putPoolCertificates slot certificates = do
        let publicationTimes =
                CertificatePublicationTime slot <$> [minBound ..]
        forM_ (publicationTimes `zip` certificates) $ \case
            (publicationTime, Registration cert) -> do
                liftIO $ traceWith tr $ MsgStakePoolRegistration cert
                putPoolRegistration publicationTime cert
            (publicationTime, Retirement cert) -> do
                liftIO $ traceWith tr $ MsgStakePoolRetirement cert
                putPoolRetirement publicationTime cert


-- | Worker thread that monitors pool metadata and syncs it to the database.
monitorMetadata
    :: TVar PoolMetadataGCStatus
    -> Tracer IO StakePoolLog
    -> SlottingParameters
    -> DBLayer IO
    -> IO ()
monitorMetadata gcStatus tr sp db@(DBLayer{..}) = do
    settings <- atomically readSettings
    manager <- newManager defaultManagerSettings

    health <- case poolMetadataSource settings of
        FetchSMASH uri -> do
            let checkHealth _ = toHealthCheckSMASH
                    <$> healthCheck trFetch (unSmashServer uri) manager

                maxRetries = 8
                retryCheck RetryStatus{rsIterNumber} b
                    | rsIterNumber < maxRetries = pure
                        (b == Unavailable || b == Unreachable)
                    | otherwise = pure False

                ms = (* 1_000_000)
                baseSleepTime = ms 15

            retrying (constantDelay baseSleepTime) retryCheck checkHealth

        _ -> pure NoSmashConfigured

    if  | health == Available || health == NoSmashConfigured -> do
            case poolMetadataSource settings of
                FetchNone -> do
                    STM.atomically $ writeTVar gcStatus NotApplicable

                FetchDirect -> do
                    STM.atomically $ writeTVar gcStatus NotApplicable
                    void $ fetchMetadata manager [identityUrlBuilder]

                FetchSMASH (unSmashServer -> uri) -> do
                    STM.atomically $ writeTVar gcStatus NotStarted
                    let getDelistedPools =
                            fetchDelistedPools trFetch uri manager
                    tid <- forkFinally
                        (gcDelistedPools gcStatus tr db getDelistedPools)
                        (traceAfterThread (contramap MsgGCThreadExit tr))
                    void $ fetchMetadata manager [registryUrlBuilder uri]
                        `finally` killThread tid

        | otherwise ->
            traceWith tr MsgSMASHUnreachable
  where
    trFetch = contramap MsgFetchPoolMetadata tr

    fetchMetadata
        :: Manager
        -> [UrlBuilder]
        -> IO Void
    fetchMetadata manager strategies = do
        inFlights <- STM.atomically $ newTBQueue maxInFlight
        settings <- atomically readSettings
        endlessly [] $ \keys -> do
            refs <- nub . (\\ keys) <$> atomically (unfetchedPoolMetadataRefs limit)
            when (null refs) $ do
                traceWith tr $ MsgFetchTakeBreak blockFrequency
                threadDelay blockFrequency
            forM refs $ \k@(pid, url, hash) -> k <$ withAvailableSeat inFlights (do
                fetchFromRemote trFetch strategies manager pid url hash >>= \case
                    Nothing ->
                        atomically $ do
                            settings' <- readSettings
                            when (settings == settings') $ putFetchAttempt (url, hash)
                    Just meta -> do
                        atomically $ do
                            settings' <- readSettings
                            when (settings == settings') $ putPoolMetadata hash meta
                )
      where
        -- Twice 'maxInFlight' so that, when removing keys currently in flight,
        -- we are left with at least 'maxInFlight' keys.
        limit = fromIntegral (2 * maxInFlight)
        maxInFlight = 10

        endlessly :: Monad m => a -> (a -> m a) -> m Void
        endlessly zero action = action zero >>= (`endlessly` action)

        -- | Run an action asyncronously only when there's an available seat.
        -- Seats are materialized by a bounded queue. If the queue is full,
        -- then there's no seat.
        withAvailableSeat :: TBQueue () -> IO a -> IO ()
        withAvailableSeat q action = do
            STM.atomically $ writeTBQueue q ()
            void $ action `forkFinally` const (STM.atomically $ readTBQueue q)

    -- NOTE
    -- If there's no metadata, we typically need not to retry sooner than the
    -- next block. So waiting for a delay that is roughly the same order of
    -- magnitude as the (slot length / active slot coeff) sounds sound.
    blockFrequency = ceiling (1/f) * toMicroSecond slotLength
      where
        toMicroSecond = (`div` 1000000) . fromEnum
        slotLength = unSlotLength $ getSlotLength sp
        f = unActiveSlotCoefficient (getActiveSlotCoefficient sp)

gcDelistedPools
    :: TVar PoolMetadataGCStatus
    -> Tracer IO StakePoolLog
    -> DBLayer IO
    -> IO (Maybe [PoolId])  -- ^ delisted pools fetcher
    -> IO ()
gcDelistedPools gcStatus tr DBLayer{..} fetchDelisted = forever $ do
    lastGC <- atomically readLastMetadataGC
    currentTime <- getPOSIXTime

    case lastGC of
        Nothing -> pure ()
        Just gc -> STM.atomically $ writeTVar gcStatus (HasRun gc)

    let timeSinceLastGC = fmap (currentTime -) lastGC
        sixHours = posixDayLength / 4
    when (maybe True (> sixHours) timeSinceLastGC) $ do
        delistedPools <- fmap (fromMaybe []) fetchDelisted
        STM.atomically $ writeTVar gcStatus (HasRun currentTime)
        atomically $ do
            putLastMetadataGC currentTime
            putDelistedPools delistedPools

    -- Sleep for 60 seconds. This is useful in case
    -- something else is modifying the last sync time
    -- in the database.
    let ms = (* 1_000_000)
    let sleepTime = ms 60
    traceWith tr $ MsgGCTakeBreak sleepTime
    threadDelay sleepTime
    pure ()

{-------------------------------------------------------------------------------
    Logging
-------------------------------------------------------------------------------}
data StakePoolLog
    = MsgExitMonitoring AfterThreadLog
    | MsgChainMonitoring ChainFollowLog
    | MsgStakePoolGarbageCollection PoolGarbageCollectionInfo
    | MsgStakePoolRegistration PoolRegistrationCertificate
    | MsgStakePoolRetirement PoolRetirementCertificate
    | MsgErrProduction ErrPointAlreadyExists
    | MsgFetchPoolMetadata StakePoolMetadataFetchLog
    | MsgFetchTakeBreak Int
    | MsgGCTakeBreak Int
    | MsgGCThreadExit AfterThreadLog
    | MsgSMASHUnreachable
    deriving (Show, Eq)

data PoolGarbageCollectionInfo = PoolGarbageCollectionInfo
    { currentEpoch :: EpochNo
        -- ^ The current epoch at the point in time the garbage collector
        -- was invoked.
    , latestRetirementEpoch :: EpochNo
        -- ^ The latest retirement epoch for which garbage collection will be
        -- performed. The garbage collector will remove all pools that have an
        -- active retirement epoch equal to or earlier than this epoch.
    }
    deriving (Eq, Show)

instance HasPrivacyAnnotation StakePoolLog
instance HasSeverityAnnotation StakePoolLog where
    getSeverityAnnotation = \case
        MsgExitMonitoring msg -> getSeverityAnnotation msg
        MsgChainMonitoring msg -> getSeverityAnnotation msg
        MsgStakePoolGarbageCollection{} -> Debug
        MsgStakePoolRegistration{} -> Debug
        MsgStakePoolRetirement{} -> Debug
        MsgErrProduction{} -> Error
        MsgFetchPoolMetadata e -> getSeverityAnnotation e
        MsgFetchTakeBreak{} -> Debug
        MsgGCTakeBreak{} -> Debug
        MsgGCThreadExit{} -> Debug
        MsgSMASHUnreachable{} -> Warning

instance ToText StakePoolLog where
    toText = \case
        MsgExitMonitoring msg ->
            "Stake pool monitor exit: " <> toText msg
        MsgChainMonitoring msg -> toText msg
        MsgStakePoolGarbageCollection info -> mconcat
            [ "Performing garbage collection of retired stake pools. "
            , "Currently in epoch "
            , toText (currentEpoch info)
            , ". Removing all pools that retired in or before epoch "
            , toText (latestRetirementEpoch info)
            , "."
            ]
        MsgStakePoolRegistration cert ->
            "Discovered stake pool registration: " <> pretty cert
        MsgStakePoolRetirement cert ->
            "Discovered stake pool retirement: " <> pretty cert
        MsgErrProduction (ErrPointAlreadyExists blk) -> mconcat
            [ "Couldn't store production for given block before it conflicts "
            , "with another block. Conflicting block header is: ", pretty blk
            ]
        MsgFetchPoolMetadata e ->
            toText e
        MsgFetchTakeBreak delay -> mconcat
            [ "Taking a little break from fetching metadata, "
            , "back to it in about "
            , pretty (fixedF 1 (toRational delay / 1000000)), "s"
            ]
        MsgGCTakeBreak delay -> mconcat
            [ "Taking a little break from GCing delisted metadata pools, "
            , "back to it in about "
            , pretty (fixedF 1 (toRational delay / 1_000_000)), "s"
            ]
        MsgGCThreadExit msg ->
            "GC thread has exited: " <> toText msg
        MsgSMASHUnreachable -> mconcat
            ["The SMASH server is unreachable or unhealthy."
            , "Metadata monitoring thread aborting."
            ]
