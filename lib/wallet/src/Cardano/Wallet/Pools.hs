{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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
module Cardano.Wallet.Pools
    ( StakePoolLayer (..)
    , StakePool (..)
    , StakePoolMetrics (..)
    , StakePoolFlag (..)
    , EpochInfo (..)
    , toEpochInfo
    , withNodeStakePoolLayer
    , withStakePoolDbLayer
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
import Cardano.Pool.DB.Log
    ( PoolDbLog )
import Cardano.Pool.Metadata
    ( HealthCheckSMASH (..)
    , StakePoolMetadataFetchLog
    , UrlBuilder
    , fetchDelistedPools
    , fetchFromRemote
    , healthCheck
    , identityUrlBuilder
    , registryUrlBuilder
    , toHealthCheckSMASH
    )
import Cardano.Pool.Metadata.Types
    ( PoolMetadataGCStatus (..), StakePoolMetadata, StakePoolMetadataHash )
import Cardano.Pool.Types
    ( PoolId (..), StakePoolsSummary (..) )
import Cardano.Wallet.Byron.Compatibility
    ( toByronBlockHeader )
import Cardano.Wallet.Network
    ( ChainFollowLog (..), ChainFollower (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException (..)
    , Qry
    , TimeInterpreter
    , epochOf
    , interpretQuery
    , neverFails
    , timeOfEpoch
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
    , PoolLifeCycleStatus (..)
    , PoolMetadataSource (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , Settings (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
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
    , fromBabbageBlock
    , fromMaryBlock
    , fromShelleyBlock
    , getBabbageProducer
    , getProducer
    , toBabbageBlockHeader
    , toShelleyBlockHeader
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forM_, forever, void, when, (>=>) )
import Control.Monad.Cont
    ( ContT (ContT) )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Monad.Trans.State
    ( State, evalState, state )
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
    ( dropMissing, traverseMissing, zipWithAMatched, zipWithMatched )
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
import Data.Time
    ( UTCTime )
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
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( Manager )
import Network.HTTP.Client.TLS
    ( newTlsManager )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, HardForkBlock (..) )
import System.Exit
    ( ExitCode )
import System.Random
    ( RandomGen, random )
import UnliftIO.Concurrent
    ( forkFinally, forkIOWithUnmask, killThread, threadDelay )
import UnliftIO.Exception
    ( finally )
import UnliftIO.IORef
    ( IORef, newIORef, readIORef, writeIORef )
import UnliftIO.MVar
    ( modifyMVar_, newMVar )
import UnliftIO.STM
    ( TBQueue
    , TVar
    , newTBQueue
    , newTVarIO
    , readTBQueue
    , readTVarIO
    , writeTBQueue
    , writeTVar
    )

import qualified Cardano.Pool.DB as PoolDb
import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Cardano.Wallet.Checkpoints.Policy as CP
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified UnliftIO.STM as STM

data StakePoolLayer = StakePoolLayer
    { getPoolLifeCycleStatus :: PoolId -> IO PoolLifeCycleStatus

    , knownPools :: IO (Set PoolId)

    -- | List pools based given the the amount of stake the user intends to
    --   delegate, which affects the size of the rewards and the ranking of
    --   the pools.
    --
    -- Pools with a retirement epoch earlier than or equal to the specified
    -- epoch will be excluded from the result.
    --
    , listStakePools
        :: EpochNo -- Exclude all pools that retired in or before this epoch.
        -> Coin
        -> IO [StakePool]

    , forceMetadataGC :: IO ()

    , putSettings :: Settings -> IO ()

    , getSettings :: IO Settings

    , getGCMetadataStatus :: IO PoolMetadataGCStatus
    }

withNodeStakePoolLayer
    :: Tracer IO StakePoolLog
    -> Maybe Settings
    -> PoolDb.DBLayer IO
    -> NetworkParameters
    -> [PoolCertificate] -- Shelley genesis pools
    -> NetworkLayer IO (CardanoBlock StandardCrypto)
    -> ContT ExitCode IO StakePoolLayer
withNodeStakePoolLayer tr settings dbLayer@DBLayer{..} netParams genesisPools netLayer = lift do
    gcStatus <- newTVarIO NotStarted
    forM_ settings $ atomically . putSettings
    void $ forkFinally
        (monitorStakePools tr netParams genesisPools netLayer dbLayer)
        (traceAfterThread (contramap MsgExitMonitoring tr))

    -- fixme: needs to be simplified as part of ADP-634
    let NetworkParameters{slottingParameters} = netParams
        startMetadataThread = forkIOWithUnmask
            ($ monitorMetadata gcStatus tr slottingParameters dbLayer)
    metadataThread <- newMVar =<< startMetadataThread
    let restartMetadataThread = modifyMVar_ metadataThread $
            killThread >=> const startMetadataThread
    newStakePoolLayer gcStatus netLayer dbLayer restartMetadataThread

withStakePoolDbLayer
    :: Tracer IO PoolDbLog
    -> Maybe FilePath
    -> Maybe (Pool.DBDecorator IO)
    -> NetworkLayer IO block
    -> ContT r IO (PoolDb.DBLayer IO)
withStakePoolDbLayer poolDbTracer databaseDir poolDbDecorator netLayer =
    ContT $ Pool.withDecoratedDBLayer
        (fromMaybe Pool.undecoratedDB poolDbDecorator)
        poolDbTracer
        (Pool.defaultFilePath <$> databaseDir)
        (neverFails "withPoolsMonitoring never forecasts into the future" $
            timeInterpreter netLayer)

newStakePoolLayer
    :: forall sc. ()
    => TVar PoolMetadataGCStatus
    -> NetworkLayer IO (CardanoBlock sc)
    -> DBLayer IO
    -> IO ()
    -> IO StakePoolLayer
newStakePoolLayer gcStatus nl db@DBLayer{..} restartSyncThread =
    pure StakePoolLayer
        { getPoolLifeCycleStatus = _getPoolLifeCycleStatus
        , knownPools = _knownPools
        , listStakePools = _listPools
        , forceMetadataGC = _forceMetadataGC
        , putSettings = _putSettings
        , getSettings = _getSettings
        , getGCMetadataStatus = _getGCMetadataStatus
        }
  where
    _getPoolLifeCycleStatus :: PoolId -> IO PoolLifeCycleStatus
    _getPoolLifeCycleStatus pid = atomically $ readPoolLifeCycleStatus pid

    _knownPools :: IO (Set PoolId)
    _knownPools = Set.fromList <$> liftIO (atomically listRegisteredPools)

    _getSettings :: IO Settings
    _getSettings = atomically readSettings

    _putSettings :: Settings -> IO ()
    _putSettings settings =
        atomically (gcMetadata >> putSettings settings) *> restartSyncThread
      where
        -- clean up metadata table if the new sync settings suggests so
        gcMetadata = do
            oldSettings <- readSettings
            case ( poolMetadataSource oldSettings
                 , poolMetadataSource settings ) of
                (_, FetchNone) ->
                    -- this is necessary if it's e.g. the first time
                    -- we start the server with the new feature
                    -- and the database has still stuff in it
                    removePoolMetadata
                (old, new) | old /= new -> removePoolMetadata
                _ -> pure ()

    _listPools
        :: EpochNo
        -- Exclude all pools that retired in or before this epoch.
        -> Coin
        -> IO [StakePool]
    _listPools currentEpoch userStake = do
        rawLsqData <- stakeDistribution nl userStake
        dbData <- readPoolDbData db currentEpoch
        seed <- atomically readSystemSeed
        sortByReward seed . map snd . Map.toList <$>
            combineDbAndLsqData
                (timeInterpreter nl)
                (nOpt rawLsqData)
                (combineLsqData rawLsqData)
                dbData
      where
        -- Sort by non-myopic member rewards, making sure to also randomly sort
        -- pools that have equal rewards.
        --
        -- NOTE: we discard the final value of the random generator because we
        -- do actually want the order to be stable between two identical
        -- requests. The order simply needs to be different between different
        -- instances of the server.
        sortByReward
            :: RandomGen g
            => g
            -> [StakePool]
            -> [StakePool]
        sortByReward g0 =
            map stakePool
            . L.sortOn (Down . rewards)
            . L.sortOn randomWeight
            . evalState' g0
            . traverse withRandomWeight
          where
            evalState' :: s -> State s a -> a
            evalState' = flip evalState

            withRandomWeight :: RandomGen g => a -> State g (Int, a)
            withRandomWeight a = do
                weight <- state random
                pure (weight, a)

            rewards = view (#metrics . #nonMyopicMemberRewards) . stakePool
            (randomWeight, stakePool) = (fst, snd)

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

--
-- Data Combination functions
--

-- | Stake pool-related data that has been read from the node using a local
--   state query.
data PoolLsqData = PoolLsqData
    { nonMyopicMemberRewards :: Coin
    , relativeStake :: Percentage
    , saturation :: Double
    } deriving (Eq, Show, Generic)

-- | Stake pool-related data that has been read from the database.
data PoolDbData = PoolDbData
    { registrationCert :: PoolRegistrationCertificate
    , retirementCert :: Maybe PoolRetirementCertificate
    , nProducedBlocks :: Quantity "block" Word64
    , poolDbMetadata :: Maybe StakePoolMetadata
    , delisted :: Bool
    }

-- | Top level combine-function that merges DB and LSQ data.
combineDbAndLsqData
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> Int -- ^ nOpt; desired number of pools
    -> Map PoolId PoolLsqData
    -> Map PoolId PoolDbData
    -> IO (Map PoolId StakePool)
combineDbAndLsqData ti nOpt lsqData =
    Map.mergeA lsqButNoDb dbButNoLsq bothPresent lsqData
  where
    bothPresent = zipWithAMatched mkApiPool

    lsqButNoDb = dropMissing

    -- When a pool is registered (with a registration certificate) but not
    -- currently active (and therefore not causing pool metrics data to be
    -- available over local state query), we use a default value of /zero/
    -- for all stake pool metric values so that the pool can still be
    -- included in the list of all known stake pools:
    --
    dbButNoLsq = traverseMissing $ \k -> mkApiPool k PoolLsqData
        { nonMyopicMemberRewards = freshmanMemberRewards
        , relativeStake = minBound
        , saturation = 0
        }

    -- To give a chance to freshly registered pools that haven't been part of
    -- any leader schedule, we assign them the average reward of the top @k@
    -- pools.
    freshmanMemberRewards
        = Coin
        $ average
        $ L.take nOpt
        $ L.sortOn Down
        $ map (unCoin . view #nonMyopicMemberRewards)
        $ Map.elems lsqData
      where
        average [] = 0
        average xs = round $ double (sum xs) / double (length xs)

        double :: Integral a => a -> Double
        double = fromIntegral

    mkApiPool :: PoolId -> PoolLsqData -> PoolDbData -> IO StakePool
    mkApiPool pid (PoolLsqData prew pstk psat) dbData = do
        let mRetirementEpoch = retirementEpoch <$> retirementCert dbData
        retirementEpochInfo <- traverse
            (interpretQuery (unsafeExtendSafeZone ti) . toEpochInfo)
            mRetirementEpoch
        pure StakePool
            { id = pid
            , metrics = StakePoolMetrics
                { nonMyopicMemberRewards = Coin.toQuantity prew
                , relativeStake = Quantity pstk
                , saturation = psat
                , producedBlocks = (fmap fromIntegral . nProducedBlocks) dbData
                }
            , metadata = poolDbMetadata dbData
            , cost = Coin.toQuantity $ poolCost $ registrationCert dbData
            , pledge = Coin.toQuantity $ poolPledge $ registrationCert dbData
            , margin = Quantity $ poolMargin $ registrationCert dbData
            , retirement = retirementEpochInfo
            , flags = [ Delisted | delisted dbData ]
            }

-- | Combines all the LSQ data into a single map.
--
-- This is the data we can ask the node for the most recent version of, over the
-- local state query protocol.
--
-- Calculating e.g. the nonMyopicMemberRewards ourselves through chain-following
-- would be completely impractical.
combineLsqData :: StakePoolsSummary -> Map PoolId PoolLsqData
combineLsqData StakePoolsSummary{nOpt, rewards, stake} =
    Map.merge stakeButNoRewards rewardsButNoStake bothPresent stake rewards
  where
    -- calculate the saturation from the relative stake
    sat s = fromRational $ getPercentage s / (1 / fromIntegral nOpt)

    -- If we fetch non-myopic member rewards of pools using the wallet
    -- balance of 0, the resulting map will be empty. So we set the rewards
    -- to 0 here:
    stakeButNoRewards = traverseMissing $ \_k s -> pure $ PoolLsqData
        { nonMyopicMemberRewards = Coin 0
        , relativeStake = s
        , saturation = sat s
        }

    -- TODO: This case seems possible on shelley_testnet, but why, and how
    -- should we treat it?
    --
    -- The pool with rewards but not stake didn't seem to be retiring.
    rewardsButNoStake = traverseMissing $ \_k r -> pure $ PoolLsqData
        { nonMyopicMemberRewards = r
        , relativeStake = noStake
        , saturation = sat noStake
        }
      where
        noStake = unsafeMkPercentage 0

    bothPresent = zipWithMatched  $ \_k s r -> PoolLsqData r s (sat s)

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
    registeredNoProductions = traverseMissing $ \_k cert ->
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
    -> [PoolCertificate] -- Shelley genesis pools; not present on mainnet
    -> NetworkLayer IO (CardanoBlock StandardCrypto)
    -> DBLayer IO
    -> IO ()
monitorStakePools tr (NetworkParameters gp sp _pp) genesisPools nl DBLayer{..} =
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

        -- Write genesis pools to DB. These are specific to the integration test
        -- cluster, and is always set to [] in the cardano-wallet executable.
        --
        -- Had they not been unique to the integration tests, this code as-is
        -- could have been run twice just by restarting the wallet server, which
        -- may or may not have been safe.
        let pseudoGenesisSlotNo = SlotNo 0
        atomically $ do
            putPoolCertificates pseudoGenesisSlotNo genesisPools

        chainSync nl (contramap MsgChainMonitoring tr) $ ChainFollower
            { checkpointPolicy = CP.defaultPolicy
            , readChainPoints = map toChainPoint <$> initCursor
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
    forward latestGarbageCollectionEpochRef blocks _ =
        atomically $ forAllAndLastM blocks forAllBlocks forLastBlock
      where
        forAllBlocks = \case
            BlockByron _ ->
                pure ()
            BlockShelley blk ->
                forEachShelleyBlock
                    (fromShelleyBlock gp blk) (getProducer blk)
            BlockAllegra blk ->
                forEachShelleyBlock
                    (fromAllegraBlock gp blk) (getProducer blk)
            BlockMary blk ->
                forEachShelleyBlock
                    (fromMaryBlock gp blk) (getProducer blk)
            BlockAlonzo blk ->
                forEachShelleyBlock
                    (fromAlonzoBlock gp blk) (getProducer blk)
            BlockBabbage blk ->
                forEachShelleyBlock
                    (fromBabbageBlock gp blk) (getBabbageProducer blk)

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
            BlockBabbage blk ->
                putHeader (toBabbageBlockHeader getGenesisBlockHash blk)

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
    -- that occurred two epochs ago that has not subsequently been superseded,
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
monitorMetadata gcStatus tr sp db@DBLayer{..} = do
    settings <- atomically readSettings
    manager <- newTlsManager

    health <- case poolMetadataSource settings of
        FetchSMASH uri -> do
            let checkHealth _ = toHealthCheckSMASH
                    <$> healthCheck trFetch (unSmashServer uri) manager

                maxRetries = 8
                retryCheck RetryStatus{rsIterNumber} b
                    | rsIterNumber < maxRetries =
                        pure (b == Unavailable || b == Unreachable)
                    | otherwise = pure False

                ms = (* 1_000_000)
                baseSleepTime = ms 15

            retrying (constantDelay baseSleepTime) retryCheck checkHealth

        _ -> pure NoSmashConfigured

    if health == Available || health == NoSmashConfigured
        then do
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
        else
            traceWith tr MsgSMASHUnreachable
  where
    trFetch = contramap MsgFetchPoolMetadata tr

    fetchMetadata :: Manager -> [UrlBuilder] -> IO Void
    fetchMetadata manager strategies = do
        inFlights <- STM.atomically $ newTBQueue maxInFlight
        settings <- atomically readSettings
        endlessly [] $ \keys -> do
            refs <- nub . (\\ keys) <$> atomically (unfetchedPoolMetadataRefs limit)
            when (null refs) $ do
                traceWith tr $ MsgFetchTakeBreak blockFrequency
                threadDelay blockFrequency
            forM refs $ \k@(pid, url, hash) -> k <$ withAvailableSeat inFlights (
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

        -- | Run an action asynchronously only when there's an available seat.
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
        toMicroSecond = (`div` 1_000_000) . fromEnum
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

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data StakePool = StakePool
    { id :: !(PoolId)
    , metrics :: !StakePoolMetrics
    , metadata :: !(Maybe StakePoolMetadata)
    , cost :: !(Quantity "lovelace" Natural)
    , margin :: !(Quantity "percent" Percentage)
    , pledge :: !(Quantity "lovelace" Natural)
    , retirement :: !(Maybe EpochInfo)
    , flags :: ![StakePoolFlag]
    }
    deriving (Eq, Generic, Show)

data EpochInfo = EpochInfo
    { epochNumber :: !EpochNo
    , epochStartTime :: !UTCTime
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

toEpochInfo :: EpochNo -> Qry EpochInfo
toEpochInfo ep = EpochInfo ep . fst <$> timeOfEpoch ep

data StakePoolFlag = Delisted
    deriving stock (Eq, Generic, Show)
    deriving anyclass NFData

data StakePoolMetrics = StakePoolMetrics
    { nonMyopicMemberRewards :: !(Quantity "lovelace" Natural)
    , relativeStake :: !(Quantity "percent" Percentage)
    , saturation :: !Double
    , producedBlocks :: !(Quantity "block" Natural)
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

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

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

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
            , pretty (fixedF 1 (toRational delay / 1_000_000)), "s"
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
