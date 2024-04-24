{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | A monitor that can switch between pausing and running
module Control.Monitoring.Monitor
    ( Monitor (..)
    , hoistMonitor
    , mkMonitor
    , monitorTracer
    )
where

import Prelude

import Control.Monad
    ( when
    )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    )
import Control.Monitoring.Concurrent
    ( Register (..)
    , newRegister
    )
import Control.Monitoring.Tracing
    ( AnyTracing
    , MonitorState (..)
    )
import Control.Tracer
    ( Tracer (..)
    )
import Data.Bifunctor
    ( Bifunctor (..)
    )
import Data.Profunctor
    ( Profunctor (..)
    , dimap
    , rmap
    )

import qualified Control.Monitoring.Tracing as Tracing

-- | The state of the monitor
-- | A trace monitor that can switch between pausing and running
-- * `trace` is tracing in both states
-- * `switch` switches between pausing and not pausing and vice versa
-- * `observe` observes the current state which is kept however we switch
-- * `step` is a no-op when not pausing
-- * `kill` tries to kill the program by placing a bomb on the next trace
data Monitor m a b = Monitor
    { trace :: a -> m ()
    -- ^ Trace a value
    , switch :: m ()
    -- ^ Switch between pausing and running
    , observe :: m (b, MonitorState)
    -- ^ Observe the current state
    , step :: m ()
    -- ^ Pull the next trace, when in pausing state
    , kill :: m ()
    -- ^ Try to kill the program
    }

-- | Natural transformation of monitors
hoistMonitor :: (forall x. m x -> n x) -> Monitor m a b -> Monitor n a b
hoistMonitor nat Monitor{..} =
    Monitor
        { trace = nat . trace
        , switch = nat switch
        , observe = nat observe
        , step = nat step
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
            , step
            , kill
            }

-- | Create a monitor from a tracer in either pausing or running state
mkMonitor
    :: MonadSTM m
    => AnyTracing c b
    -- ^ The initial state of the monitor
    -> (a -> m c)
    -- ^ Contextualize the tracing
    -> m (Monitor m a b)
mkMonitor anyTracing addCtx = do
    Register readTracing changeTracing <- newRegister anyTracing
    let block e = changeTracing $ \s -> case Tracing.tracingState s of
            Step -> Nothing
            _ -> Just $ Tracing.trace e s
    Register readKill changeKill <- newRegister False
    let step' = changeTracing $ pure . Tracing.step
    pure
        $ Monitor
            { trace = \event -> do
                kill <- readKill
                when kill $ error "Killed"
                ctxed <- addCtx event
                block ctxed
            , switch = changeTracing $ pure . Tracing.switch
            , observe = do
                m <- readTracing
                pure (Tracing.observe m, Tracing.tracingState m)
            , step = step'
            , kill = changeKill (const $ Just True) >> step'
            }

-- | Extract the `Tracer` from a `Monitor`
monitorTracer :: Monitor m a b -> Tracer m a
monitorTracer = Tracer . trace
