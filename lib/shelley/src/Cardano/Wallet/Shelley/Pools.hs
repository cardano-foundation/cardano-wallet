{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
    ( DBLayer (..), ErrPointAlreadyExists (..) )
import Cardano.Wallet.Api.Types
    ( ApiT (..) )
import Cardano.Wallet.Network
    ( ErrNetworkUnavailable
    , FollowAction (..)
    , FollowExit (..)
    , FollowLog
    , NetworkLayer (..)
    , follow
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , BlockHeader
    , Coin (..)
    , GenesisParameters (..)
    , PoolCertificate (..)
    , PoolId
    , PoolRegistrationCertificate (..)
    , PoolRetirementCertificate (..)
    , ProtocolParameters
    , SlotId
    , SlotLength (..)
    , StakePoolMetadata
    , StakePoolMetadataHash
    , StakePoolMetadataUrl
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , ShelleyBlock
    , fromShelleyBlock'
    , getProducer
    , toBlockHeader
    , toPoint
    )
import Cardano.Wallet.Shelley.Network
    ( NodePoolLsqData (..) )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage, unsafeRunExceptT )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( forM, forM_, forever, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Control.Tracer
    ( Tracer, contramap, traceWith )
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
import Data.Text.Class
    ( ToText (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( fixedF, pretty )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

--
-- Stake Pool Layer
--

data StakePoolLayer = StakePoolLayer
    { knownPools
        :: IO [PoolId]

    -- | List pools based given the the amount of stake the user intends to
    -- delegate, which affects the size of the rewards and the ranking of the
    -- pools.
    , listStakePools
        :: Coin
        -> ExceptT ErrNetworkUnavailable IO [Api.ApiStakePool]
    }

newStakePoolLayer
    :: GenesisParameters
    -> NetworkLayer IO (IO Shelley) b
    -> DBLayer IO
    -> StakePoolLayer
newStakePoolLayer gp nl db = StakePoolLayer
    { knownPools = _knownPools
    , listStakePools = _listPools
    }
  where
    _knownPools
        :: IO [PoolId]
    _knownPools = do
        tip <- getTip
        let dummyCoin = Coin 0
        res <- runExceptT $ map fst . Map.toList
            . combineLsqData <$> stakeDistribution nl tip dummyCoin
        case res of
            Right x -> return x
            Left _e -> return []

    _listPools
        :: Coin
        -> ExceptT ErrNetworkUnavailable IO [Api.ApiStakePool]
    _listPools userStake = do
            tip <- liftIO getTip
            lsqData <- combineLsqData <$> stakeDistribution nl tip userStake
            chainData <- liftIO $ readDBPoolData db
            return
              . sortOn (Down . (view (#metrics . #nonMyopicMemberRewards)))
              . map snd
              . Map.toList
              $ combineDbAndLsqData lsqData chainData

    -- Note: We shouldn't have to do this conversion.
    el = getEpochLength gp
    gh = getGenesisBlockHash gp
    getTip = fmap (toPoint gh el) . liftIO $ unsafeRunExceptT $ currentNodeTip nl

--
-- Data Combination functions
--
--

-- | Stake Pool data fields that we can fetch from the node over Local State
-- Query.
data PoolLsqMetrics = PoolLsqMetrics
    { nonMyopicMemberRewards :: Quantity "lovelace" Word64
    , relativeStake :: Percentage
    , saturation :: Double
    } deriving (Eq, Show, Generic)

-- | Stake Pool data fields that we read from the DB.
data PoolDBMetrics = PoolDBMetrics
    { regCert :: PoolRegistrationCertificate
    , nProducedBlocks :: Quantity "block" Word64
    , metadata :: Maybe StakePoolMetadata
    }

-- | Top level combine-function that merges DB and LSQ data.
combineDbAndLsqData
    :: Map PoolId PoolLsqMetrics
    -> Map PoolId PoolDBMetrics
    -> Map PoolId Api.ApiStakePool
combineDbAndLsqData =
    Map.merge lsqButNoChain chainButNoLsq bothPresent
  where
    lsqButNoChain = traverseMissing $ \k lsq -> pure $ mkApiPool k lsq Nothing

    -- In case our chain following has missed a retirement certificate, we
    -- treat the lsq data as the source of truth, and dropMissing here.
    chainButNoLsq = dropMissing

    bothPresent = zipWithMatched  $ \k lsq chain -> mkApiPool k lsq (Just chain)

    mkApiPool
        :: PoolId
        -> PoolLsqMetrics
        -> Maybe PoolDBMetrics
        -> Api.ApiStakePool
    mkApiPool
        pid
        (PoolLsqMetrics prew pstk psat)
        dbData
        = Api.ApiStakePool
        { Api.id = (ApiT pid)
        , Api.metrics = Api.ApiStakePoolMetrics
            { Api.nonMyopicMemberRewards = mapQ fromIntegral prew
            , Api.relativeStake = Quantity pstk
            , Api.saturation = psat
            , Api.producedBlocks = maybe (Quantity 0)
                    (mapQ fromIntegral . nProducedBlocks) dbData
            }
        , Api.metadata = dbData >>= metadata >>= (return . ApiT)
        , Api.cost = mapQ fromIntegral . poolCost . regCert <$> dbData
        , Api.margin = Quantity . poolMargin . regCert <$> dbData
        }

    mapQ f (Quantity x) = Quantity $ f x

-- | Combines all the LSQ data into a single map.
--
-- This is the data we can ask the node for the most recent version of, over the
-- local state query protocol.
--
-- Calculating e.g. the nonMyopicMemberRewards ourselves through chain-following
-- would be completely impractical.
combineLsqData
    :: NodePoolLsqData
    -> Map PoolId PoolLsqMetrics
combineLsqData NodePoolLsqData{nOpt, rewards, stake} =
    Map.merge stakeButNoRewards rewardsButNoStake bothPresent stake rewards
  where
    -- calculate the saturation from the relative stake
    sat s = fromRational $ (getPercentage s) / (1 / fromIntegral nOpt)

    -- If we fetch non-myopic member rewards of pools using the wallet
    -- balance of 0, the resulting map will be empty. So we set the rewards
    -- to 0 here:
    stakeButNoRewards = traverseMissing $ \_k s -> pure $ PoolLsqMetrics
        { nonMyopicMemberRewards = Quantity 0
        , relativeStake = s
        , saturation = (sat s)
        }

    -- TODO: This case seems possible on shelley_testnet, but why, and how
    -- should we treat it?
    --
    -- The pool with rewards but not stake didn't seem to be retiring.
    rewardsButNoStake = traverseMissing $ \_k r -> pure $ PoolLsqMetrics
        { nonMyopicMemberRewards = r
        , relativeStake = noStake
        , saturation = sat noStake
        }
      where
        noStake = unsafeMkPercentage 0

    bothPresent       = zipWithMatched  $ \_k s r -> PoolLsqMetrics r s (sat s)

-- | Combines all the chain-following data into a single map
-- (doesn't include metadata)
combineChainData
    :: Map PoolId PoolRegistrationCertificate
    -> Map PoolId (Quantity "block" Word64)
    -> Map PoolId
        (PoolRegistrationCertificate, Quantity "block" Word64)
combineChainData =
    Map.merge registeredNoProductions notRegisteredButProducing bothPresent
  where
    registeredNoProductions  = traverseMissing $ \_k cert ->
        pure (cert, Quantity 0)

    -- Ignore blocks produced by BFT nodes.
    notRegisteredButProducing = dropMissing

    bothPresent = zipWithMatched $ const (,)

-- NOTE: If performance becomes a problem, we could try replacing all
-- the individual DB queries, and combbination functions with a single
-- hand-written Sqlite query.
readDBPoolData
    :: DBLayer IO
    -> IO (Map PoolId PoolDBMetrics)
readDBPoolData DBLayer{..} = atomically $ do
    pools <- listRegisteredPools
    registrations <- mapM readPoolRegistration pools
    let certMap = Map.fromList
            [(poolId, cert) | (poolId, Just cert) <- zip pools registrations]
    prodMap <- readTotalProduction
    metaMap <- readPoolMetadata
    return $ Map.map (lookupMetaIn metaMap) (combineChainData certMap prodMap)
  where
    lookupMetaIn
        :: Map StakePoolMetadataHash StakePoolMetadata
        -> (PoolRegistrationCertificate, Quantity "block" Word64)
        -> PoolDBMetrics
    lookupMetaIn m (cert, n) =
        let
            metaHash = snd <$> poolMetadata cert
            meta = flip Map.lookup m =<< metaHash
        in
            PoolDBMetrics cert n meta

--
-- Monitoring stake pool
--

monitorStakePools
    :: Tracer IO StakePoolLog
    -> GenesisParameters
    -> NetworkLayer IO t ShelleyBlock
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
        , getEpochLength
        , getEpochStability
        } = gp

    initCursor :: IO [BlockHeader]
    initCursor = atomically $ readPoolProductionCursor (max 100 k)
      where k = fromIntegral $ getQuantity getEpochStability

    getHeader :: ShelleyBlock -> BlockHeader
    getHeader = toBlockHeader getGenesisBlockHash getEpochLength

    forward
        :: NonEmpty ShelleyBlock
        -> (BlockHeader, ProtocolParameters)
        -> IO (FollowAction ())
    forward blocks (_nodeTip, _pparams) = do
        atomically $ forM_ blocks $ \blk -> do
            let (slot, registrations) = fromShelleyBlock' getEpochLength blk
            runExceptT (putPoolProduction (getHeader blk) (getProducer blk)) >>= \case
                Left e   -> liftIO $ traceWith tr $ MsgErrProduction e
                Right () -> pure ()
            forM_ registrations $ \case
                Registration pool -> do
                    liftIO $ traceWith tr $ MsgStakePoolRegistration pool
                    putPoolRegistration slot pool
                Retirement cert -> do
                    liftIO $ traceWith tr $ MsgStakePoolRetirement cert
        pure Continue

monitorMetadata
    :: Tracer IO StakePoolLog
    -> GenesisParameters
    -> (StakePoolMetadataUrl -> StakePoolMetadataHash -> IO (Either String StakePoolMetadata))
    -> DBLayer IO
    -> IO ()
monitorMetadata tr gp fetchMetadata DBLayer{..} = forever $ do
    refs <- atomically (unfetchedPoolMetadataRefs 100)

    successes <- fmap catMaybes $ forM refs $ \(url, hash) -> do
        traceWith tr $ MsgFetchPoolMetadata hash url
        fetchMetadata url hash >>= \case
            Left msg -> Nothing <$ do
                traceWith tr $ MsgFetchPoolMetadataFailure hash msg

            Right meta -> Just hash <$ do
                traceWith tr $ MsgFetchPoolMetadataSuccess hash meta
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
    | MsgRollingBackTo SlotId
    | MsgStakePoolRegistration PoolRegistrationCertificate
    | MsgStakePoolRetirement PoolRetirementCertificate
    | MsgErrProduction ErrPointAlreadyExists
    | MsgFetchPoolMetadata StakePoolMetadataHash StakePoolMetadataUrl
    | MsgFetchPoolMetadataSuccess StakePoolMetadataHash StakePoolMetadata
    | MsgFetchPoolMetadataFailure StakePoolMetadataHash String
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
        MsgCrashMonitoring ->
            "Chain follower exited with error. Worker will no longer monitor stake pools."
        MsgRollingBackTo point ->
            "Rolling back to " <> pretty point
        MsgStakePoolRegistration pool ->
            "Discovered stake pool registration: " <> pretty pool
        MsgStakePoolRetirement cert ->
            "Discovered stake pool retirement: " <> pretty cert
        MsgErrProduction (ErrPointAlreadyExists blk) -> mconcat
            [ "Couldn't store production for given block before it conflicts "
            , "with another block. Conflicting block header is: ", pretty blk
            ]
        MsgFetchPoolMetadata hash url -> mconcat
            [ "Fetching metadata with hash ", pretty hash, " from ", toText url
            ]
        MsgFetchPoolMetadataSuccess hash meta -> mconcat
            [ "Successfully fetched metadata with hash ", pretty hash
            , ": ", T.pack (show meta)
            ]
        MsgFetchPoolMetadataFailure hash msg -> mconcat
            [ "Failed to fetch metadata with hash ", pretty hash, ": ", T.pack msg
            ]
        MsgFetchTakeBreak delay -> mconcat
            [ "Taking a little break from fetching metadata, back to it in about "
            , pretty (fixedF 1 (toRational delay / 1000000)), "s"
            ]
