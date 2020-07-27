{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
import Cardano.Wallet.Api.Types
    ( ApiT (..) )
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
    ( SlotParameters, epochStartTime, slotParams )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader
    , CertificatePublicationTime (..)
    , Coin (..)
    , EpochNo (..)
    , GenesisParameters (..)
    , PoolCertificate (..)
    , PoolId
    , PoolLifeCycleStatus (..)
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , ProtocolParameters (..)
    , SlotLength (..)
    , SlotNo
    , StakePoolMetadata
    , StakePoolMetadataHash
    , StakePoolMetadataUrl
    , getPoolRegistrationCertificate
    , getPoolRetirementCertificate
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
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
import Control.Monad
    ( forM, forM_, forever, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, withExceptT )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.List
    ( sortOn )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, zipWithMatched )
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
import Data.Word
    ( Word64 )
import Fmt
    ( fixedF, pretty )
import GHC.Generics
    ( Generic )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, HardForkBlock (..) )
import Ouroboros.Consensus.Shelley.Protocol
    ( TPraosCrypto )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Data.List as L
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

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
        -> ExceptT ErrNetworkUnavailable IO [Api.ApiStakePool]
    }

newStakePoolLayer
    :: forall sc.
       GenesisParameters
    -> NetworkLayer IO (IO Shelley) (CardanoBlock sc)
    -> DBLayer IO
    -> StakePoolLayer
newStakePoolLayer gp nl db@DBLayer {..} =
        StakePoolLayer
            { getPoolLifeCycleStatus = _getPoolLifeCycleStatus
            , knownPools = _knownPools
            , listStakePools = _listPools
            }
  where
    _getPoolLifeCycleStatus
        :: PoolId -> IO PoolLifeCycleStatus
    _getPoolLifeCycleStatus pid =
        liftIO $ atomically $ readPoolLifeCycleStatus pid

    _knownPools
        :: IO (Set PoolId)
    _knownPools =
        Map.keysSet <$> liftIO (readPoolDbData db)

    _listPools
        :: EpochNo
        -- Exclude all pools that retired in or before this epoch.
        -> Coin
        -> ExceptT ErrNetworkUnavailable IO [Api.ApiStakePool]
    _listPools currentEpoch userStake = do
        tip <- withExceptT fromErrCurrentNodeTip $ currentNodeTip nl
        lsqData <- combineLsqData <$> stakeDistribution nl tip userStake
        dbData <- liftIO $ readPoolDbData db
        pp <- liftIO $ getProtocolParameters nl
        -- TODO:
        -- Use a more efficient way of filtering out retired pools.
        -- See: https://jira.iohk.io/projects/ADP/issues/ADP-383
        return
            . sortOn (Down . (view (#metrics . #nonMyopicMemberRewards)))
            . filter (not . poolIsRetired)
            . map snd
            . Map.toList
            $ combineDbAndLsqData (slotParams gp, pp) lsqData dbData
      where
        fromErrCurrentNodeTip :: ErrCurrentNodeTip -> ErrNetworkUnavailable
        fromErrCurrentNodeTip = \case
            ErrCurrentNodeTipNetworkUnreachable e -> e
            ErrCurrentNodeTipNotFound -> ErrNetworkUnreachable "tip not found"

        epochIsInFuture :: EpochNo -> Bool
        epochIsInFuture = (> currentEpoch)

        poolIsRetired :: Api.ApiStakePool -> Bool
        poolIsRetired =
            maybe False (not . epochIsInFuture) . poolRetirementEpoch

        poolRetirementEpoch :: Api.ApiStakePool -> Maybe EpochNo
        poolRetirementEpoch p = p
            & view #retirement
            & fmap (view (#epochNumber . #getApiT))

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
    :: (SlotParameters, ProtocolParameters)
    -> Map PoolId PoolLsqData
    -> Map PoolId PoolDbData
    -> Map PoolId Api.ApiStakePool
combineDbAndLsqData (sp, pp) lsqData =
    Map.merge lsqButNoDb dbButNoLsq bothPresent lsqData
  where
    bothPresent = zipWithMatched mkApiPool

    lsqButNoDb = dropMissing

    -- When a pool is registered (with a registration certificate) but not
    -- currently active (and therefore not causing pool metrics data to be
    -- available over local state query), we use a default value of /zero/
    -- for all stake pool metric values so that the pool can still be
    -- included in the list of all known stake pools:
    --
    dbButNoLsq = traverseMissing $ \k db ->
        pure $ mkApiPool k lsqDefault db
      where
        lsqDefault = PoolLsqData
            { nonMyopicMemberRewards = freshmenMemberRewards
            , relativeStake = minBound
            , saturation = 0
            }

    -- To give a chance to freshly registered pools that haven't been part of
    -- any leader schedule, we assign them the average reward of the top @k@
    -- pools.
    freshmenMemberRewards
        = Quantity
        $ average
        $ L.take (fromIntegral $ desiredNumberOfStakePools pp)
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
        -> Api.ApiStakePool
    mkApiPool pid (PoolLsqData prew pstk psat) dbData =
        Api.ApiStakePool
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
        , Api.retirement =
            toApiEpochInfo . retiredIn <$> retirementCert dbData
        }

    toApiEpochInfo ep =
        Api.ApiEpochInfo (ApiT ep) (epochStartTime sp ep)

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

-- TODO:
--
-- This function currently executes a total of (2n + 1) database queries, where
-- n is the total number of pools with entries in the pool registrations table.
--
-- Specifically:
--
--    1.  We first execute a query to determine the complete set of all pools
--        (including those that may have retired).
--
--    2.  For each pool, we determine its current life-cycle status by executing
--        a pair of queries to fetch:
--
--          a. The most recent registration certificate.
--          b. The most recent retirement certificate.
--
-- This is almost certainly not optimal.
--
-- If performance becomes a problem, we should investigate ways to reduce the
-- number of queries required:
--
--    See: https://jira.iohk.io/browse/ADP-383
--
-- Additionally, we can consider performing garbage collection of retired pools
-- from the database:
--
--    See: https://jira.iohk.io/browse/ADP-376
--
readPoolDbData :: DBLayer IO -> IO (Map PoolId PoolDbData)
readPoolDbData DBLayer {..} = atomically $ do
    pools <- listRegisteredPools
    lifeCycleStatuses <- mapM readPoolLifeCycleStatus pools
    let mkCertificateMap
            :: forall a . (PoolLifeCycleStatus -> Maybe a) -> Map PoolId a
        mkCertificateMap f = Map.fromList
            [(p, c) | (p, Just c) <- zip pools (f <$> lifeCycleStatuses)]
    combineChainData
        (mkCertificateMap getPoolRegistrationCertificate)
        (mkCertificateMap getPoolRetirementCertificate)
        <$> readTotalProduction
        <*> readPoolMetadata

--
-- Monitoring stake pool
--

monitorStakePools
    :: forall t sc. (TPraosCrypto sc)
    => Tracer IO StakePoolLog
    -> GenesisParameters
    -> NetworkLayer IO t (CardanoBlock sc)
    -> DBLayer IO
    -> IO ()
monitorStakePools tr gp nl db@DBLayer{..} = do
    cursor <- initCursor
    traceWith tr $ MsgStartMonitoring cursor
    follow nl (contramap MsgFollow tr) cursor forward getHeader >>= \case
        FollowInterrupted -> traceWith tr MsgHaltMonitoring
        FollowFailure -> traceWith tr MsgCrashMonitoring
        FollowRollback point -> do
            traceWith tr $ MsgRollingBackTo point
            liftIO . atomically $ rollbackTo point
            monitorStakePools tr gp nl db
  where
    GenesisParameters
        { getGenesisBlockHash
        , getEpochStability
        } = gp

    initCursor :: IO [BlockHeader]
    initCursor = atomically $ readPoolProductionCursor (max 100 k)
      where k = fromIntegral $ getQuantity getEpochStability

    getHeader :: CardanoBlock sc -> BlockHeader
    getHeader = toCardanoBlockHeader gp

    forward
        :: NonEmpty (CardanoBlock sc)
        -> (BlockHeader, ProtocolParameters)
        -> IO (FollowAction ())
    forward blocks (_nodeTip, _pparams) = do
        atomically $ forM_ blocks $ \case
            BlockByron _ -> pure ()
            BlockShelley blk -> do
                let (slot, certificates) = poolCertsFromShelleyBlock blk
                let header = toShelleyBlockHeader getGenesisBlockHash blk
                runExceptT (putPoolProduction header (getProducer blk))
                    >>= \case
                        Left e ->
                            liftIO $ traceWith tr $ MsgErrProduction e
                        Right () ->
                            pure ()

                -- A single block can contain multiple certificates relating to the
                -- same pool.
                --
                -- The /order/ in which certificates appear is /significant/:
                -- certificates that appear later in a block /generally/ take
                -- precedence over certificates that appear earlier on.
                --
                -- We record /all/ certificates within the database, together with
                -- the order in which they appeared.
                --
                -- Precedence is determined by the 'readPoolLifeCycleStatus'
                -- function.
                --
                let publicationTimes =
                        CertificatePublicationTime slot <$> [minBound ..]
                forM_ (publicationTimes `zip` certificates) $ \case
                    (publicationTime, Registration cert) -> do
                        liftIO $ traceWith tr $ MsgStakePoolRegistration cert
                        putPoolRegistration publicationTime cert
                    (publicationTime, Retirement cert) -> do
                        liftIO $ traceWith tr $ MsgStakePoolRetirement cert
                        putPoolRetirement publicationTime cert
        pure Continue

monitorMetadata
    :: Tracer IO StakePoolLog
    -> GenesisParameters
    -> (StakePoolMetadataUrl
        -> StakePoolMetadataHash
        -> IO (Either String StakePoolMetadata))
    -> DBLayer IO
    -> IO ()
monitorMetadata tr gp fetchMetadata DBLayer{..} = forever $ do
    refs <- atomically (unfetchedPoolMetadataRefs 100)
    successes <- fmap catMaybes $ forM refs $ \(url, hash) -> do
        traceWith tr $ MsgFetchPoolMetadata hash url
        fetchMetadata url hash >>= \case
            Left msg -> Nothing <$ do
                traceWith tr $ MsgFetchPoolMetadataFailure url msg
                atomically $ putFetchAttempt (url, hash)

            Right meta -> Just hash <$ do
                traceWith tr $ MsgFetchPoolMetadataSuccess url meta
                atomically $ putPoolMetadata hash meta

    when (null refs || null successes) $ do
        traceWith tr $ MsgFetchTakeBreak blockFrequency
        threadDelay blockFrequency
  where
    -- NOTE
    -- If there's no metadata, we typically need not to retry sooner than the
    -- next block. So waiting for a delay that is roughly the same order of
    -- magnitude as the (slot length / active slot coeff) sounds sound.
    blockFrequency = ceiling (1/f) * toMicroSecond slotLength
      where
        toMicroSecond = (`div` 1000000) . fromEnum
        slotLength = unSlotLength $ getSlotLength gp
        f = unActiveSlotCoefficient (getActiveSlotCoefficient gp)

data StakePoolLog
    = MsgFollow FollowLog
    | MsgStartMonitoring [BlockHeader]
    | MsgHaltMonitoring
    | MsgCrashMonitoring
    | MsgRollingBackTo SlotNo
    | MsgStakePoolRegistration PoolRegistrationCertificate
    | MsgStakePoolRetirement PoolRetirementCertificate
    | MsgErrProduction ErrPointAlreadyExists
    | MsgFetchPoolMetadata StakePoolMetadataHash StakePoolMetadataUrl
    | MsgFetchPoolMetadataSuccess StakePoolMetadataUrl StakePoolMetadata
    | MsgFetchPoolMetadataFailure StakePoolMetadataUrl String
    | MsgFetchTakeBreak Int
    deriving (Show, Eq)

instance HasPrivacyAnnotation StakePoolLog
instance HasSeverityAnnotation StakePoolLog where
    getSeverityAnnotation = \case
        MsgFollow e -> getSeverityAnnotation e
        MsgStartMonitoring{} -> Info
        MsgHaltMonitoring{} -> Info
        MsgCrashMonitoring{} -> Error
        MsgRollingBackTo{} -> Info
        MsgStakePoolRegistration{} -> Info
        MsgStakePoolRetirement{} -> Info
        MsgErrProduction{} -> Error
        MsgFetchPoolMetadata{} -> Info
        MsgFetchPoolMetadataSuccess{} -> Info
        MsgFetchPoolMetadataFailure{} -> Warning
        MsgFetchTakeBreak{} -> Info -- TODO Lower to "Debug"

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
        MsgStakePoolRegistration cert ->
            "Discovered stake pool registration: " <> pretty cert
        MsgStakePoolRetirement cert ->
            "Discovered stake pool retirement: " <> pretty cert
        MsgErrProduction (ErrPointAlreadyExists blk) -> mconcat
            [ "Couldn't store production for given block before it conflicts "
            , "with another block. Conflicting block header is: ", pretty blk
            ]
        MsgFetchPoolMetadata hash url -> mconcat
            [ "Fetching metadata with hash ", pretty hash, " from ", toText url
            ]
        MsgFetchPoolMetadataSuccess url meta -> mconcat
            [ "Successfully fetched metadata from ", toText url
            , ": ", T.pack (show meta)
            ]
        MsgFetchPoolMetadataFailure url msg -> mconcat
            [ "Failed to fetch metadata from ", toText url, ": ", T.pack msg
            ]
        MsgFetchTakeBreak delay -> mconcat
            [ "Taking a little break from fetching metadata, "
            , "back to it in about "
            , pretty (fixedF 1 (toRational delay / 1000000)), "s"
            ]
