{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    ( StakePoolMetadataFetchLog
    , defaultManagerSettings
    , fetchFromRemote
    , identityUrlBuilder
    , newManager
    , registryUrlBuilder
    )
import Cardano.Wallet
    ( ErrListPools (..) )
import Cardano.Wallet.Api.Types
    ( ApiT (..) )
import Cardano.Wallet.Byron.Compatibility
    ( toByronBlockHeader )
import Cardano.Wallet.Network
    ( ErrCurrentNodeTip (..)
    , ErrNetworkUnavailable (..)
    , FollowAction (..)
    , FollowExit (..)
    , FollowLog
    , NetworkLayer (..)
    , follow
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException (..)
    , TimeInterpreter
    , epochOf
    , epochPred
    , firstSlotInEpoch
    , startTime
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader (..)
    , CertificatePublicationTime (..)
    , Coin (..)
    , EpochNo (..)
    , GenesisParameters (..)
    , PoolCertificate (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , PoolMetadataSource (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , ProtocolParameters (..)
    , Settings (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StakePoolMetadata
    , StakePoolMetadataHash
    , getPoolRegistrationCertificate
    , getPoolRetirementCertificate
    , unSmashServer
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , StandardCrypto
    , getProducer
    , poolCertsFromShelleyBlock
    , toCardanoBlockHeader
    , toShelleyBlockHeader
    )
import Cardano.Wallet.Shelley.Network
    ( NodePoolLsqData (..) )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Control.Concurrent
    ( threadDelay )
import Control.Exception
    ( SomeException (..), bracket, mask_, try )
import Control.Monad
    ( forM, forM_, forever, void, when, (<=<) )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), mapExceptT, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.State
    ( State, evalState, state )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Bifunctor
    ( first )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.IORef
    ( IORef, newIORef, readIORef, writeIORef )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, zipWithAMatched, zipWithMatched )
import Data.Maybe
    ( catMaybes )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Set
    ( Set )
import Data.Text.Class
    ( ToText (..) )
import Data.Tuple.Extra
    ( dupe )
import Data.Word
    ( Word64 )
import Fmt
    ( fixedF, pretty )
import GHC.Conc
    ( TVar, ThreadId, killThread, newTVarIO, readTVarIO, writeTVar )
import GHC.Generics
    ( Generic )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, HardForkBlock (..) )
import System.Random
    ( RandomGen, random )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Conc as STM

--
-- Stake Pool Layer
--

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
        -> ExceptT ErrListPools IO [Api.ApiStakePool]

    , putSettings :: Settings -> IO ()

    , getSettings :: IO Settings
    }

newStakePoolLayer
    :: forall sc. ()
    => NetworkLayer IO (IO Shelley) (CardanoBlock sc)
    -> DBLayer IO
    -> IO ThreadId
    -> IO StakePoolLayer
newStakePoolLayer nl db@DBLayer {..} worker = do
    tid <- worker
    tvTid <- newTVarIO tid
    pure $ StakePoolLayer
        { getPoolLifeCycleStatus = _getPoolLifeCycleStatus
        , knownPools = _knownPools
        , listStakePools = _listPools
        , putSettings = _putSettings tvTid
        , getSettings = _getSettings
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

    -- In order to apply the settings, we have to restart the
    -- metadata sync thread as well to avoid race conditions.
    _putSettings :: TVar ThreadId -> Settings -> IO ()
    _putSettings tvTid settings = do
        bracket
            killSyncThread
            (\_ -> restartSyncThread)
            (\_ -> atomically (gcMetadata >> writeSettings))
      where
        -- kill syncing thread, so we can apply the settings cleanly
        killSyncThread = do
            tid <- readTVarIO tvTid
            killThread tid

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

        writeSettings = putSettings settings

        restartSyncThread = do
            tid <- worker
            STM.atomically $ writeTVar tvTid tid

    _getSettings = liftIO $ atomically readSettings

    _listPools
        :: EpochNo
        -- Exclude all pools that retired in or before this epoch.
        -> Coin
        -> ExceptT ErrListPools IO [Api.ApiStakePool]
    _listPools currentEpoch userStake = do
        tip <- withExceptT fromErrCurrentNodeTip $ currentNodeTip nl
        rawLsqData <- mapExceptT (fmap (first ErrListPoolsNetworkError))
            $ stakeDistribution nl tip userStake
        let lsqData = combineLsqData rawLsqData
        dbData <- liftIO $ readPoolDbData db currentEpoch
        seed <- liftIO $ atomically readSystemSeed
        r <- liftIO $ try $
            sortByReward seed
            . map snd
            . Map.toList
            <$> combineDbAndLsqData
                (timeInterpreter nl)
                (nOpt rawLsqData)
                lsqData
                dbData
        case r of
            Left e@(PastHorizon{}) ->
                throwE (ErrListPoolsPastHorizonException e)
            Right r' -> pure r'
      where
        fromErrCurrentNodeTip :: ErrCurrentNodeTip -> ErrListPools
        fromErrCurrentNodeTip = \case
            ErrCurrentNodeTipNetworkUnreachable e ->
                ErrListPoolsNetworkError e
            ErrCurrentNodeTipNotFound ->
                ErrListPoolsNetworkError $ ErrNetworkUnreachable "tip not found"

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
            -> [Api.ApiStakePool]
            -> [Api.ApiStakePool]
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

--
-- Data Combination functions
--

-- | Stake pool-related data that has been read from the node using a local
--   state query.
data PoolLsqData = PoolLsqData
    { nonMyopicMemberRewards :: Quantity "lovelace" Word64
    , relativeStake :: Percentage
    , saturation :: Double
    } deriving (Eq, Show, Generic)

-- | Stake pool-related data that has been read from the database.
data PoolDbData = PoolDbData
    { registrationCert :: PoolRegistrationCertificate
    , retirementCert :: Maybe PoolRetirementCertificate
    , nProducedBlocks :: Quantity "block" Word64
    , metadata :: Maybe StakePoolMetadata
    }

-- | Top level combine-function that merges DB and LSQ data.
combineDbAndLsqData
    :: forall m. (Monad m)
    => TimeInterpreter m
    -> Int -- ^ nOpt; desired number of pools
    -> Map PoolId PoolLsqData
    -> Map PoolId PoolDbData
    -> m (Map PoolId Api.ApiStakePool)
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
    dbButNoLsq = traverseMissing $ \k db ->
        mkApiPool k lsqDefault db
      where
        lsqDefault = PoolLsqData
            { nonMyopicMemberRewards = freshmanMemberRewards
            , relativeStake = minBound
            , saturation = 0
            }

    -- To give a chance to freshly registered pools that haven't been part of
    -- any leader schedule, we assign them the average reward of the top @k@
    -- pools.
    freshmanMemberRewards
        = Quantity
        $ average
        $ L.take nOpt
        $ L.sort
        $ map (Down . getQuantity . nonMyopicMemberRewards)
        $ Map.elems lsqData
      where
        average [] = 0
        average xs = round $ double (sum xs) / double (length xs)

        double :: Integral a => a -> Double
        double = fromIntegral

    mkApiPool
        :: PoolId
        -> PoolLsqData
        -> PoolDbData
        -> m Api.ApiStakePool
    mkApiPool pid (PoolLsqData prew pstk psat) dbData = do
        let mRetirementEpoch = retirementEpoch <$> retirementCert dbData
        retirementEpochInfo <- traverse toApiEpochInfo mRetirementEpoch
        pure $ Api.ApiStakePool
            { Api.id = (ApiT pid)
            , Api.metrics = Api.ApiStakePoolMetrics
                { Api.nonMyopicMemberRewards = fmap fromIntegral prew
                , Api.relativeStake = Quantity pstk
                , Api.saturation = psat
                , Api.producedBlocks =
                    (fmap fromIntegral . nProducedBlocks) dbData
                }
            , Api.metadata =
                ApiT <$> metadata dbData
            , Api.cost =
                fmap fromIntegral $ poolCost $ registrationCert dbData
            , Api.pledge =
                fmap fromIntegral $ poolPledge $ registrationCert dbData
            , Api.margin =
                Quantity $ poolMargin $ registrationCert dbData
            , Api.retirement = retirementEpochInfo
            }

    toApiEpochInfo ep = do
        time <- ti $ startTime =<< firstSlotInEpoch ep
        return $ Api.ApiEpochInfo (ApiT ep) time

-- | Combines all the LSQ data into a single map.
--
-- This is the data we can ask the node for the most recent version of, over the
-- local state query protocol.
--
-- Calculating e.g. the nonMyopicMemberRewards ourselves through chain-following
-- would be completely impractical.
combineLsqData
    :: NodePoolLsqData
    -> Map PoolId PoolLsqData
combineLsqData NodePoolLsqData{nOpt, rewards, stake} =
    Map.merge stakeButNoRewards rewardsButNoStake bothPresent stake rewards
  where
    -- calculate the saturation from the relative stake
    sat s = fromRational $ (getPercentage s) / (1 / fromIntegral nOpt)

    -- If we fetch non-myopic member rewards of pools using the wallet
    -- balance of 0, the resulting map will be empty. So we set the rewards
    -- to 0 here:
    stakeButNoRewards = traverseMissing $ \_k s -> pure $ PoolLsqData
        { nonMyopicMemberRewards = Quantity 0
        , relativeStake = s
        , saturation = (sat s)
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
    -> Map PoolId PoolDbData
combineChainData registrationMap retirementMap prodMap metaMap =
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
        PoolDbData registrationCert mRetirementCert n meta
      where
        metaHash = snd <$> poolMetadata registrationCert
        meta = flip Map.lookup metaMap =<< metaHash
        mRetirementCert =
            Map.lookup (view #poolId registrationCert) retirementMap

readPoolDbData :: DBLayer IO -> EpochNo -> IO (Map PoolId PoolDbData)
readPoolDbData DBLayer {..} currentEpoch = atomically $ do
    lifeCycleData <- listPoolLifeCycleData currentEpoch
    let registrationCertificates = lifeCycleData
            & fmap getPoolRegistrationCertificate
            & catMaybes
            & fmap (first (view #poolId) . dupe)
            & Map.fromList
    let retirementCertificates = lifeCycleData
            & fmap getPoolRetirementCertificate
            & catMaybes
            & fmap (first (view #poolId) . dupe)
            & Map.fromList
    combineChainData
        registrationCertificates
        retirementCertificates
        <$> readTotalProduction
        <*> readPoolMetadata

--
-- Monitoring stake pool
--

monitorStakePools
    :: forall t.
       Tracer IO StakePoolLog
    -> GenesisParameters
    -> NetworkLayer IO t (CardanoBlock StandardCrypto)
    -> DBLayer IO
    -> IO ()
monitorStakePools tr gp nl DBLayer{..} =
    monitor =<< mkLatestGarbageCollectionEpochRef
  where
    monitor latestGarbageCollectionEpochRef = loop
      where
        loop = do
            cursor <- initCursor
            traceWith tr $ MsgStartMonitoring cursor
            let followTrace = contramap MsgFollow tr
            let forwardHandler = forward latestGarbageCollectionEpochRef
            follow nl followTrace cursor forwardHandler getHeader >>= \case
                FollowInterrupted ->
                    traceWith tr MsgHaltMonitoring
                FollowFailure ->
                    traceWith tr MsgCrashMonitoring
                FollowRollback point -> do
                    traceWith tr $ MsgRollingBackTo point
                    liftIO . atomically $ rollbackTo point
                    loop

    GenesisParameters
        { getGenesisBlockHash
        , getEpochStability
        } = gp

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
      where k = fromIntegral $ getQuantity getEpochStability

    getHeader :: CardanoBlock StandardCrypto -> BlockHeader
    getHeader = toCardanoBlockHeader gp

    forward
        :: IORef EpochNo
        -> NonEmpty (CardanoBlock StandardCrypto)
        -> (BlockHeader, ProtocolParameters)
        -> IO (FollowAction ())
    forward latestGarbageCollectionEpochRef blocks _ = do
        atomically $ forAllAndLastM blocks forAllBlocks forLastBlock
        pure Continue
      where
        forAllBlocks = \case
            BlockByron _ -> do
                pure ()
            BlockShelley blk -> do
                let (slot, certificates) = poolCertsFromShelleyBlock blk
                let header = toShelleyBlockHeader getGenesisBlockHash blk
                handleErr (putPoolProduction header (getProducer blk))
                garbageCollectPools slot latestGarbageCollectionEpochRef
                putPoolCertificates slot certificates

        forLastBlock = \case
            BlockByron blk ->
                putHeader (toByronBlockHeader gp blk)
            BlockShelley blk ->
                putHeader (toShelleyBlockHeader getGenesisBlockHash blk)

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
        -- #2196: We need the try. Arguably we shouldn't need it.
        liftIO (try @SomeException (timeInterpreter nl (epochOf currentSlot))) >>= \case
            Left _ -> return ()
            Right currentEpoch -> do
                let subtractTwoEpochs = epochPred <=< epochPred
                forM_ (subtractTwoEpochs currentEpoch) $ \latestRetirementEpoch -> do
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
    :: Tracer IO StakePoolLog
    -> SlottingParameters
    -> DBLayer IO
    -> IO ()
monitorMetadata tr sp DBLayer{..} = do
    settings <- atomically readSettings
    manager <- newManager defaultManagerSettings
    let fetcher fetchStrategies = fetchFromRemote trFetch fetchStrategies manager
    forever $ do
        (refs, successes) <- case poolMetadataSource settings of
            FetchNone -> pure ([], [])
            FetchDirect -> fetchThem $ fetcher [identityUrlBuilder]
            FetchSMASH (unSmashServer -> uri) -> fetchThem $ fetcher [registryUrlBuilder uri]

        when (null refs || null successes) $ do
            traceWith tr $ MsgFetchTakeBreak blockFrequency
            threadDelay blockFrequency
  where
    trFetch = contramap MsgFetchPoolMetadata tr
    -- We mask this entire section just in case, although the database
    -- operations runs masked anyway. Unfortunately we cannot run
    -- @fetchMetadata@ from within atomically, so here we go.
    fetchThem fetchMetadata = mask_ $ do
        refs <- atomically (unfetchedPoolMetadataRefs 100)
        successes <- fmap catMaybes $ forM refs $ \(pid, url, hash) -> do
            fetchMetadata pid url hash >>= \case
                Nothing -> Nothing <$ do
                    atomically $ putFetchAttempt (url, hash)

                Just meta -> Just hash <$ do
                    atomically $ putPoolMetadata hash meta
        pure (refs, successes)
    -- NOTE
    -- If there's no metadata, we typically need not to retry sooner than the
    -- next block. So waiting for a delay that is roughly the same order of
    -- magnitude as the (slot length / active slot coeff) sounds sound.
    blockFrequency = ceiling (1/f) * toMicroSecond slotLength
      where
        toMicroSecond = (`div` 1000000) . fromEnum
        slotLength = unSlotLength $ getSlotLength sp
        f = unActiveSlotCoefficient (getActiveSlotCoefficient sp)

data StakePoolLog
    = MsgFollow FollowLog
    | MsgStartMonitoring [BlockHeader]
    | MsgHaltMonitoring
    | MsgCrashMonitoring
    | MsgRollingBackTo SlotNo
    | MsgStakePoolGarbageCollection PoolGarbageCollectionInfo
    | MsgStakePoolRegistration PoolRegistrationCertificate
    | MsgStakePoolRetirement PoolRetirementCertificate
    | MsgErrProduction ErrPointAlreadyExists
    | MsgFetchPoolMetadata StakePoolMetadataFetchLog
    | MsgFetchTakeBreak Int
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
        MsgFollow e -> getSeverityAnnotation e
        MsgStartMonitoring{} -> Info
        MsgHaltMonitoring{} -> Info
        MsgCrashMonitoring{} -> Error
        MsgRollingBackTo{} -> Info
        MsgStakePoolGarbageCollection{} -> Debug
        MsgStakePoolRegistration{} -> Info
        MsgStakePoolRetirement{} -> Info
        MsgErrProduction{} -> Error
        MsgFetchPoolMetadata e -> getSeverityAnnotation e
        MsgFetchTakeBreak{} -> Debug

instance ToText StakePoolLog where
    toText = \case
        MsgFollow e ->
            toText e
        MsgStartMonitoring cursor -> mconcat
            [ "Monitoring stake pools. Currently at "
            , case cursor of
                [] -> "genesis"
                _  -> pretty (last cursor)
            ]
        MsgHaltMonitoring ->
            "Stopping stake pool monitoring as requested."
        MsgCrashMonitoring -> mconcat
            [ "Chain follower exited with error. "
            , "Worker will no longer monitor stake pools."
            ]
        MsgRollingBackTo point ->
            "Rolling back to " <> pretty point
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
