{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monitoring.Folder
    ( mkFoldingMonitor
    )
where

import Prelude

import Control.Comonad
    ( Comonad (duplicate, extract)
    , ($>)
    )
import Control.Foldl
    ( Fold
    )
import Control.Monitoring.Monitor
    ( MonitorState (..)
    , PullingOrNotPulling (..)
    )
import Control.Monitoring.Tracing
    ( NotPulling (..)
    , Pulling (..)
    , Tracing (..)
    )
import UnliftIO
    ( MonadIO
    , atomically
    , liftIO
    , modifyTVar
    , newEmptyTMVarIO
    , newTVarIO
    , putTMVar
    , readTVarIO
    , takeTMVar
    )

import qualified Control.Foldl as F

-- | Create a NotPulling tracer from a `Fold`
mkFoldingMonitor
    :: MonadIO m
    => m ctx
    -- ^ How to get the context for each trace
    -> Fold (ctx, a) b
    -- ^ How to fold the context and the value
    -> MonitorState
    -> m (PullingOrNotPulling m a b)
mkFoldingMonitor getContext how state = do
    resultVar <- liftIO $ newTVarIO how
    let updateResult a =
            modifyTVar resultVar $ \how' -> F.fold (duplicate how') [a]
    block <- liftIO newEmptyTMVarIO
    let
        observeTracing = liftIO $ extract <$> readTVarIO resultVar
        step =
            liftIO
                $ atomically
                $ takeTMVar block >>= updateResult
        tracePulling a = do
            ctx <- getContext
            liftIO $ atomically $ putTMVar block (ctx, a)
        traceNotPulling a = do
            ctx <- getContext
            liftIO $ atomically $ updateResult (ctx, a)
    let notPulling =
            NotPulling
                { tracingNotPulling =
                    Tracing
                        { traceTracing = traceNotPulling
                        , observeTracing
                        }
                , pulling = pulling
                }
        pulling =
            Pulling
                { tracingPulling =
                    Tracing
                        { traceTracing = tracePulling
                        , observeTracing
                        }
                , pullPulling = step
                , notPulling = step $> notPulling
                }
    pure $ case state of
        PullingState -> MkNotPulling notPulling
        NotPullingState -> MkPulling pulling
