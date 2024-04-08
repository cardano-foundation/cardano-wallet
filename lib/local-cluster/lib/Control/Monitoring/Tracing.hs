{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data types for a tracing that can be observed and. The tracing can be
-- either pulling or not pulling.
-- Pulling means that the program will be blocked until an explicit trace pull
-- is performed. Not pulling means that the program will not be blocked on tracing
module Control.Monitoring.Tracing
    ( Tracing (..)
    , Pulling (..)
    , NotPulling (..)
    )
where

import Prelude

import Data.Profunctor
    ( Profunctor (..)
    , dimap
    , lmap
    , rmap
    )

-- | A tracing controller. It accumulates the trace of `a` in `b` and can
-- control the program via `step`.
data Tracing m a b = Tracing
    { observeTracing :: m b
    -- ^ Observe the current trace
    , traceTracing :: a -> m ()
    -- ^ The tracer
    }

instance Monad m => Functor (Tracing m a) where
    fmap = rmap

instance Monad m => Profunctor (Tracing m) where
    dimap f g Tracing{..} =
        Tracing
            { observeTracing = fmap g observeTracing
            , traceTracing = lmap f traceTracing
            }

data Pulling m a b = Pulling
    { tracingPulling :: Tracing m a b
    , notPulling :: m (NotPulling m a b)
    , pullPulling :: m ()
    }

instance Monad m => Profunctor (Pulling m) where
    dimap f g Pulling{..} =
        Pulling
            { tracingPulling = dimap f g tracingPulling
            , notPulling = dimap f g <$> notPulling
            , pullPulling
            }

data NotPulling m a b = NotPulling
    { tracingNotPulling :: Tracing m a b
    , pulling :: Pulling m a b
    }

instance Monad m => Profunctor (NotPulling m) where
    dimap f g NotPulling{..} =
        NotPulling
            { tracingNotPulling = dimap f g tracingNotPulling
            , pulling = dimap f g pulling
            }
