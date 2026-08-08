{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- Background worker that periodically fetches and caches DRep off-chain
-- metadata from CIP-0119 anchor URLs.
module Cardano.Wallet.DRep.Worker
    ( WorkerConfig (..)
    , defaultWorkerConfig
    , monitorDRepMetadata
    , runOneCycle
    ) where

import Cardano.Pool.DB
    ( DBLayer (..)
    )
import Cardano.Wallet.DRep.Metadata
    ( FetchError (FetchHttpError)
    , fetchDRepMetadata
    )
import Cardano.Wallet.Network
    ( NetworkLayer
    , listDReps
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRepAnchor (..)
    , DRepRegistration (..)
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( forM_
    , when
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( NominalDiffTime
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    , getPOSIXTime
    )
import Network.HTTP.Client
    ( Manager
    )
import System.Timeout
    ( timeout
    )
import Prelude

import qualified Data.ByteArray.Encoding as BA
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T

-- | Configuration record for 'monitorDRepMetadata' and 'runOneCycle'.
data WorkerConfig = WorkerConfig
    { workerIpfsGateway :: !String
    -- ^ IPFS gateway base URL for resolving ipfs:// anchor URLs.
    , workerIntervalMicros :: !Int
    -- ^ Interval between fetch cycles, in microseconds.
    , workerFetchTimeoutMicros :: !Int
    -- ^ Per-request timeout for DRep metadata fetches, in microseconds.
    , workerGCIntervalSeconds :: !NominalDiffTime
    -- ^ How often to run the DRep metadata GC.
    , workerGetTime :: !(IO POSIXTime)
    -- ^ Clock action; injectable for testing.
    }

-- | Default 'WorkerConfig' with a 30-second fetch timeout and 24-hour GC
-- interval, using the real system clock.
defaultWorkerConfig :: String -> Int -> WorkerConfig
defaultWorkerConfig ipfsGateway intervalMicros =
    WorkerConfig
        { workerIpfsGateway = ipfsGateway
        , workerIntervalMicros = intervalMicros
        , workerFetchTimeoutMicros = 30_000_000
        , workerGCIntervalSeconds = 86400
        , workerGetTime = getPOSIXTime
        }

-- | Worker that fetches CIP-0119 metadata for all DReps with unfetched anchors.
--
-- Runs continuously, sleeping 'workerIntervalMicros' between each fetch cycle.
-- The first cycle runs immediately on startup.
monitorDRepMetadata
    :: NetworkLayer IO block
    -> DBLayer IO
    -> Manager
    -> WorkerConfig
    -> IO ()
monitorDRepMetadata netLayer db manager cfg =
    loop
  where
    loop = do
        runOneCycle netLayer db manager cfg
        threadDelay (workerIntervalMicros cfg)
        loop

-- | Run a single fetch+GC cycle without looping.  Exported for testing.
runOneCycle
    :: NetworkLayer IO block
    -> DBLayer IO
    -> Manager
    -> WorkerConfig
    -> IO ()
runOneCycle netLayer db manager cfg = case db of
    DBLayer
        { atomically
        , getAllDRepMetadata
        , recentlyFailedDRepHashes
        , putDRepMetadata
        , putDRepFetchAttempt
        } -> do
            mRegs <- listDReps netLayer
            case mRegs of
                Nothing -> pure ()
                Just regs -> do
                    cached <- atomically getAllDRepMetadata
                    failed <- atomically recentlyFailedDRepHashes
                    let cachedHashes = Map.keysSet cached
                    let toFetch =
                            [ anchor
                            | reg <- regs
                            , Just anchor <- [drepRegAnchor reg]
                            , let h = hexBS (drepAnchorHash anchor)
                            , h `Set.notMember` cachedHashes
                            , h `Set.notMember` failed
                            ]
                    forM_ toFetch $ \anchor -> do
                        let url = drepAnchorUrl anchor
                            hash = drepAnchorHash anchor
                            hexH = hexBS hash
                        mResult <-
                            timeout (workerFetchTimeoutMicros cfg)
                                $ runExceptT
                                $ fetchDRepMetadata
                                    (workerIpfsGateway cfg)
                                    manager
                                    url
                                    hash
                        let result =
                                fromMaybe
                                    (Left (FetchHttpError "fetch timed out"))
                                    mResult
                        case result of
                            Right meta -> atomically $ putDRepMetadata hexH meta
                            Left _ -> atomically $ putDRepFetchAttempt (url, hexH)
                    runGCIfDue db regs cfg

-- | Remove drep_metadata rows no longer referenced by any active DRep,
-- but only if the GC interval has elapsed since the last run.
runGCIfDue
    :: DBLayer IO
    -> [DRepRegistration]
    -> WorkerConfig
    -> IO ()
runGCIfDue db regs cfg = case db of
    DBLayer
        { atomically
        , readLastDRepMetadataGC
        , putLastDRepMetadataGC
        , removeStaleMetadata
        } -> do
            now <- workerGetTime cfg
            mLast <- atomically readLastDRepMetadataGC
            let elapsed = maybe (workerGCIntervalSeconds cfg) (\t -> now - t) mLast
            when (elapsed >= workerGCIntervalSeconds cfg) $ do
                let liveHashes =
                        Set.fromList
                            [ hexBS (drepAnchorHash anchor)
                            | reg <- regs
                            , Just anchor <- [drepRegAnchor reg]
                            ]
                atomically $ removeStaleMetadata liveHashes
                atomically $ putLastDRepMetadataGC now

hexBS :: ByteString -> Text
hexBS = T.decodeUtf8 . BA.convertToBase BA.Base16
