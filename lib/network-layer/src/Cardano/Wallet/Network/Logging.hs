{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Cardano.Wallet.Network.Logging
    ( -- * Chain following
      ChainFollowLog (..)
    , ChainSyncLog (..)
    , mapChainSyncLog
    , withFollowStatsMonitoring

      -- * Logging (for testing)
    , FollowStats (..)
    , Rearview (..)
    , emptyStats
    , updateStats
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Network.Logging.Aggregation
    ( FollowStats (..)
    , Rearview (..)
    , emptyStats
    , flushStats
    , overCurrent
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Control.Concurrent.Class.MonadSTM
    ( atomically
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( newTMVarIO
    , putTMVar
    , takeTMVar
    )
import Control.Tracer
    ( Tracer
    , contramapM
    , traceWith
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time.Clock
    ( getCurrentTime
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )
import Safe
    ( headMay
    )
import UnliftIO.Async
    ( race_
    )
import UnliftIO.Concurrent
    ( threadDelay
    )

import qualified Cardano.Wallet.Read as Read
import qualified Data.List.NonEmpty as NE

-- | Low-level logs of the ChainSync mini-protocol
data ChainSyncLog block point
    = MsgChainFindIntersect [point]
    | MsgChainRollForward (NonEmpty block) point
    | MsgChainRollBackward point Int
    | MsgChainTip point
    | MsgLocalTip point
    | MsgTipDistance Natural
    deriving (Show, Eq, Generic)

mapChainSyncLog
    :: (b1 -> b2)
    -> (p1 -> p2)
    -> ChainSyncLog b1 p1
    -> ChainSyncLog b2 p2
mapChainSyncLog f g = \case
    MsgChainFindIntersect points -> MsgChainFindIntersect (g <$> points)
    MsgChainRollForward blocks tip ->
        MsgChainRollForward (f <$> blocks) (g tip)
    MsgChainRollBackward point n -> MsgChainRollBackward (g point) n
    MsgChainTip point -> MsgChainTip (g point)
    MsgLocalTip point -> MsgLocalTip (g point)
    MsgTipDistance d -> MsgTipDistance d

type BlockHeader = Read.EraValue Read.BHeader

instance ToText (ChainSyncLog BlockHeader Read.ChainPoint) where
    toText = \case
        MsgChainFindIntersect cps ->
            mconcat
                [ "Requesting intersection using "
                , toText (length cps)
                , " points"
                , maybe "" ((", the latest being " <>) . Read.prettyChainPoint)
                    (headMay cps)
                ]
        MsgChainRollForward headers tip ->
            let buildRange (x :| []) = x
                buildRange xs = NE.head xs <> ".." <> NE.last xs
                slots =
                    Read.prettySlotNo . Read.applyEraFun Read.getEraSlotNo
                    <$> headers
            in  mconcat
                    [ "ChainSync roll forward: "
                    , "applying blocks at slots ["
                    , buildRange slots
                    , "]"
                    , ", tip is "
                    , Read.prettyChainPoint tip
                    ]
        MsgChainRollBackward b 0 ->
            "ChainSync roll backward: " <> Read.prettyChainPoint b
        MsgChainRollBackward b bufferSize ->
            mconcat
                [ "ChainSync roll backward: "
                , Read.prettyChainPoint b
                , ", handled inside pipeline buffer with remaining length "
                , toText bufferSize
                ]
        MsgChainTip tip ->
            "Node tip is " <> Read.prettyChainPoint tip
        MsgLocalTip point ->
            "Synchronized with point: " <> Read.prettyChainPoint point
        MsgTipDistance d -> "Distance to chain tip: " <> toText d <> " blocks"

instance HasPrivacyAnnotation (ChainSyncLog block point)

instance HasSeverityAnnotation (ChainSyncLog block point) where
    getSeverityAnnotation = \case
        MsgChainFindIntersect{} -> Debug
        MsgChainRollForward{} -> Debug
        MsgChainRollBackward{} -> Debug
        MsgChainTip{} -> Debug
        MsgLocalTip{} -> Debug
        MsgTipDistance{} -> Debug

-- | Higher level log of a chain follower.
-- Includes computed statistics about synchronization progress.
data ChainFollowLog
    = MsgChainSync (ChainSyncLog BlockHeader Read.ChainPoint)
    | MsgFollowStats (FollowStats Rearview)
    | MsgStartFollowing
    deriving (Show, Eq, Generic)

instance ToText ChainFollowLog where
    toText = \case
        MsgChainSync msg -> toText msg
        MsgFollowStats s -> toText s
        MsgStartFollowing -> "Chain following starting."

instance HasPrivacyAnnotation ChainFollowLog
instance HasSeverityAnnotation ChainFollowLog where
    getSeverityAnnotation = \case
        MsgChainSync msg -> getSeverityAnnotation msg
        MsgFollowStats s -> getSeverityAnnotation s
        MsgStartFollowing -> Info

-- | Update the current statistics based on a new log message.
updateStats
    :: ChainSyncLog block Read.ChainPoint
    -> FollowStats Rearview
    -> FollowStats Rearview
updateStats msg s = case msg of
    MsgChainRollForward blocks _tip ->
        s{blocksApplied = overCurrent (+ NE.length blocks) (blocksApplied s)}
    MsgChainRollBackward _ 0 ->
        -- rolled back in a way that could not be handled by the pipeline buffer
        s{rollbacks = overCurrent (1 +) (rollbacks s)}
    MsgLocalTip point ->
        s{localTip = overCurrent (const point) (localTip s)}
    _ -> s

-- | Monitors health and statistics by inspecting the messages
-- submitted to a 'ChainSyncLog' tracer.
--
-- Statistics are computed in regular time intervals.
-- In order to do that, the monitor runs in separate thread.
-- The results are submitted to the outer 'ChainFollowLog' tracer.
withFollowStatsMonitoring
    :: Tracer IO ChainFollowLog
    -> (SlotNo -> IO SyncProgress)
    -> (Tracer IO (ChainSyncLog BlockHeader Read.ChainPoint) -> IO ())
    -> IO ()
withFollowStatsMonitoring tr calcSyncProgress act = do
    t0 <- getCurrentTime
    var <- newTMVarIO $ emptyStats t0
    let trChainSyncLog = flip contramapM tr $ \msg -> do
            atomically $ do
                s <- takeTMVar var
                putTMVar var $! updateStats msg s
            pure $ MsgChainSync msg
    traceWith trChainSyncLog $ MsgLocalTip Read.GenesisPoint
    race_
        (act trChainSyncLog)
        (loop var startupDelay)
  where
    loop var delay = do
        threadDelay delay
        t <- getCurrentTime
        s <- flushStats t calcSyncProgress var
        traceWith tr $ MsgFollowStats s
        let delay' =
                if (current (prog s)) == Ready
                    then restoredDelay
                    else syncingDelay
        loop var delay'

    -- \| Delay from launch to the first status update
    startupDelay = 5 * second
    -- \| Delay between status updates when restored
    restoredDelay = 5 * minute
    -- \| Delay between status updates when not restored
    syncingDelay = 30 * second

    second = 1000 * 1000
    minute = 60 * second
