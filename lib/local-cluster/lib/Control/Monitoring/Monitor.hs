{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | A monitor that can switch between pulling and not-pulling
module Control.Monitoring.Monitor
    ( MonitorState (..)
    , Monitor (..)
    , PullingOrNotPulling (..)
    , mkMonitor
    , natMonitor
    )
where

import Prelude

import Control.Monad
    ( when
    )
import Control.Monitoring.Tracing
    ( NotPulling (..)
    , Pulling (..)
    , Tracing (..)
    , observeTracing
    , pullPulling
    , traceTracing
    , tracingNotPulling
    , tracingPulling
    )
import Data.Bifunctor
    ( Bifunctor (..)
    )
import Data.Profunctor
    ( Profunctor (..)
    , dimap
    , rmap
    )
import UnliftIO
    ( MonadIO
    , atomically
    , liftIO
    , newTVarIO
    , readTVarIO
    , writeTVar
    )

-- | The state of the monitor
data MonitorState = PullingState | NotPullingState
    deriving stock (Show, Read)

-- | A trace monitor that can switch between pulling and not-pulling
-- * `trace` is tracing in both states
-- * `switch` switches between pulling and not pulling and vice versa
-- * `observe` observes the current state which is kept however we switch
-- * `pull` is a no-op when not pulling
-- * `kill` tries to kill the program by placing a bomb on the next trace
data Monitor m a b = Monitor
    { trace :: a -> m ()
    -- ^ Trace a value
    , switch :: m ()
    -- ^ Switch between pulling and not-pulling
    , observe :: m (b, MonitorState)
    -- ^ Observe the current state
    , pull :: m ()
    -- ^ Pull the next trace, when in pulling state
    , kill :: m ()
    -- ^ Try to kill the program
    }

-- | Natural transformation of monitors
natMonitor :: (forall x. m x -> n x) -> Monitor m a b -> Monitor n a b
natMonitor nat Monitor{..} =
    Monitor
        { trace = nat . trace
        , switch = nat switch
        , observe = nat observe
        , pull = nat pull
        , kill = nat kill
        }

instance Monad m => Functor (Monitor m a) where
    fmap = rmap

instance Monad m => Profunctor (Monitor m) where
    dimap f g Monitor{..} =
        Monitor
            { trace = trace . f
            , switch
            , observe = fmap (first g) observe
            , pull
            , kill
            }

-- | Initial state of the monitor
data PullingOrNotPulling m a b
    = MkPulling (Pulling m a b)
    | MkNotPulling (NotPulling m a b)

eitherPulling
    :: (Pulling m a b -> c)
    -> (NotPulling m a b -> c)
    -> PullingOrNotPulling m a b
    -> c
eitherPulling f _ (MkPulling p) = f p
eitherPulling _ g (MkNotPulling p) = g p

-- | Create a monitor from a tracer in either pulling or not-pulling state
mkMonitor
    :: MonadIO m
    => PullingOrNotPulling m a b
    -> m (Monitor m a b)
mkMonitor tracer = do
    tracerVar <- liftIO $ newTVarIO tracer
    let r = liftIO $ readTVarIO tracerVar
        w = liftIO . atomically . writeTVar tracerVar
    k <- liftIO $ newTVarIO False
    let pull' = do
            t <- r
            eitherPulling pullPulling (const $ pure ()) t
    pure
        $ Monitor
            { trace = \a -> do
                t <- r
                kill <- liftIO $ readTVarIO k
                when kill $ error "Killed"
                eitherPulling
                    (traceTracing . tracingPulling)
                    (traceTracing . tracingNotPulling)
                    t
                    a
            , switch = do
                t <- r
                case t of
                    MkPulling Pulling{notPulling} ->
                        notPulling >>= w . MkNotPulling
                    MkNotPulling NotPulling{pulling} ->
                        w $ MkPulling pulling
            , observe = do
                t <- r
                eitherPulling
                    (fmap (,PullingState) . observeTracing . tracingPulling)
                    (fmap (,NotPullingState) . observeTracing . tracingNotPulling)
                    t
            , pull = pull'
            , kill = do
                liftIO $ atomically $ writeTVar k True
                pull'
            }
