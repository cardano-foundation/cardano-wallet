{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- Background worker that periodically fetches and caches DRep off-chain
-- metadata from CIP-0119 anchor URLs.
module Cardano.Wallet.DRep.Worker
    ( monitorDRepMetadata
    ) where

import Cardano.Pool.DB
    ( DBLayer (..)
    )
import Cardano.Wallet.DRep.Metadata
    ( fetchDRepMetadata
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
    )
import Control.Monad.Trans.Except
    ( runExceptT
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Client
    ( Manager
    )
import Prelude

import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE

-- | Worker that fetches CIP-0119 metadata for all DReps with unfetched anchors.
--
-- Runs continuously, sleeping 'intervalMicros' between each fetch cycle.
-- The first cycle runs immediately on startup.
monitorDRepMetadata
    :: NetworkLayer IO block
    -> DBLayer IO
    -> Manager
    -> Int
    -- ^ Interval between fetch cycles, in microseconds.
    -> IO ()
monitorDRepMetadata netLayer db manager intervalMicros =
    loop
  where
    DBLayer
        { atomically
        , getAllDRepMetadata
        , recentlyFailedDRepHashes
        , putDRepMetadata
        , putDRepFetchAttempt
        } = db

    loop = do
        runCycle
        threadDelay intervalMicros
        loop

    runCycle = do
        mRegs <- listDReps netLayer
        case mRegs of
            Nothing   -> pure ()
            Just regs -> do
                cached <- atomically getAllDRepMetadata
                failed <- atomically recentlyFailedDRepHashes
                let cachedHashes = Map.keysSet cached
                let toFetch =
                        [ (reg, anchor)
                        | reg <- regs
                        , Just anchor <- [drepRegAnchor reg]
                        , let h = hexBS (drepAnchorHash anchor)
                        , h `Set.notMember` cachedHashes
                        , h `Set.notMember` failed
                        ]
                forM_ toFetch $ \(_reg, anchor) -> do
                    let url  = drepAnchorUrl anchor
                        hash = drepAnchorHash anchor
                        hexH = hexBS hash
                    result <- runExceptT $ fetchDRepMetadata manager url hash
                    case result of
                        Right meta -> atomically $ putDRepMetadata hexH meta
                        Left _     -> atomically $ putDRepFetchAttempt (url, hexH)

hexBS :: ByteString -> Text
hexBS = TE.decodeUtf8 . BA.convertToBase BA.Base16
