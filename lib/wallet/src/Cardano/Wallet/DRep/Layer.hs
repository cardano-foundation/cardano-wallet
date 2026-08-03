{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- DRepLayer: service facade that merges live ledger state with cached
-- off-chain metadata, analogous to StakePoolLayer.
module Cardano.Wallet.DRep.Layer
    ( DRepLayer (..)
    , DRepInfo (..)
    , newDRepLayer
    ) where

import Cardano.Pool.DB
    ( DBLayer (..)
    )
import Cardano.Wallet.Network
    ( NetworkLayer
    , listDReps
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRepAnchor (..)
    , DRepMetadata
    , DRepRegistration (..)
    )
import Control.Concurrent
    ( forkIO
    )
import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , diffUTCTime
    , getCurrentTime
    )
import Prelude

import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE

-- | A DRep registration enriched with cached off-chain metadata.
data DRepInfo = DRepInfo
    { drepInfoReg      :: !DRepRegistration
    , drepInfoMetadata :: !(Maybe DRepMetadata)
    } deriving (Eq, Show)

-- | Service facade for DRep data. Merges live LSQ data with cached metadata.
data DRepLayer m = DRepLayer
    { listDRepInfos  :: m [DRepInfo]
    , getDRepMetadata :: Text -> m (Maybe DRepMetadata)
    -- ^ Look up full CIP-0119 metadata for a bech32 DRep ID. Returns Nothing
    -- if the DRep has no anchor or the worker has not yet fetched its metadata.
    }

-- 15 minutes, matching the metadata worker interval.
lsqCacheTtl :: NominalDiffTime
lsqCacheTtl = 900

-- | Build a 'DRepLayer' from a 'NetworkLayer' (live LSQ) and a 'DBLayer'
-- (cached metadata). The LSQ result is cached for 'lsqCacheTtl' seconds to
-- avoid hitting the node on every API call.
newDRepLayer :: NetworkLayer IO block -> DBLayer IO -> IO (DRepLayer IO)
newDRepLayer netLayer db = do
    cacheRef <- newIORef Nothing
    -- Pre-warm the LSQ cache immediately so the first API request is fast.
    _ <- forkIO $ warmCache cacheRef
    pure DRepLayer
        { listDRepInfos  = fetchAndMerge cacheRef
        , getDRepMetadata = lookupMetadata
        }
  where
    lookupMetadata drepId = case db of
        DBLayer { atomically, getDRepAnchorHash, getDRepMetadata } -> do
            mHash <- atomically $ getDRepAnchorHash drepId
            case mHash of
                Nothing -> pure Nothing
                Just h  -> atomically $ getDRepMetadata h

    fetchAndMerge
        :: IORef (Maybe (UTCTime, [DRepRegistration]))
        -> IO [DRepInfo]
    fetchAndMerge cacheRef = case db of
        DBLayer { atomically, getAllDRepMetadata } -> do
            regs   <- cachedLSQ cacheRef
            cached <- atomically getAllDRepMetadata
            pure $ map (enrich cached) regs

    warmCache :: IORef (Maybe (UTCTime, [DRepRegistration])) -> IO ()
    warmCache cacheRef = do
        mRegs <- listDReps netLayer
        now   <- getCurrentTime
        writeIORef cacheRef (Just (now, fromMaybe [] mRegs))

    cachedLSQ
        :: IORef (Maybe (UTCTime, [DRepRegistration]))
        -> IO [DRepRegistration]
    cachedLSQ cacheRef = do
        now    <- getCurrentTime
        cached <- readIORef cacheRef
        case cached of
            Just (ts, regs) | diffUTCTime now ts < lsqCacheTtl ->
                pure regs
            _ -> do
                mRegs <- listDReps netLayer
                let regs = fromMaybe [] mRegs
                writeIORef cacheRef (Just (now, regs))
                pure regs

    enrich :: Map Text DRepMetadata -> DRepRegistration -> DRepInfo
    enrich cached reg =
        let meta = do
                anchor <- drepRegAnchor reg
                Map.lookup (hexBS (drepAnchorHash anchor)) cached
        in  DRepInfo { drepInfoReg = reg, drepInfoMetadata = meta }

hexBS :: B8.ByteString -> Text
hexBS = TE.decodeUtf8 . BA.convertToBase BA.Base16
