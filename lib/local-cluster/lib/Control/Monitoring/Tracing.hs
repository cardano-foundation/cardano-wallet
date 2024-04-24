{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data types for a tracing that can be observed and. The tracing can be
-- either Pausing or not Pausing.
-- Pausing means that the program will be blocked until an explicit trace pull
-- is performed. Not Pausing means that the program will not be blocked on tracing
module Control.Monitoring.Tracing
    ( AnyTracing (..)
    , Tracing (..)
    , State (..)
    , StateS (..)
    , MonitorState (..)
    , tracingState
    , observe
    , step
    , switch
    , trace
    , withTracingState
    )
where

import Prelude

import Data.Profunctor
    ( Profunctor (..)
    , dimap
    )

-- | The state of the tracing at the type level and single type value level
data MonitorState
    = Wait
    -- ^ Allowing a single trace before going into `Step`
    | Step
    -- ^ Wating for a step before going into `Wait`
    | Run
    -- ^ Tracing freely
    deriving stock (Show, Eq)

-- | Tracing state along with its observation
data Tracing state a b = Tracing
    { observation :: b
    -- ^ Observe the tracing
    , state :: State state a b
    -- ^ The future of this tracing
    }

-- | States of a tracing machine as arrows to the next state
data State (state :: MonitorState) a b where
    Stepping
        :: { stepS :: Tracing 'Wait a b
           , switchS :: Tracing 'Run a b
           }
        -> State Step a b
    Waiting
        :: { traceW :: a -> Tracing 'Step a b
           , switchW :: Tracing 'Run a b
           }
        -> State Wait a b
    Running
        :: { traceR :: a -> Tracing 'Run a b
           , switchR :: Tracing 'Step a b
           }
        -> State Run a b

instance Profunctor (State state) => Profunctor (Tracing state) where
    dimap f g Tracing{..} =
        Tracing
            { observation = g observation
            , state = dimap f g state
            }

instance Profunctor (State 'Step) where
    dimap f g Stepping{..} =
        Stepping
            { stepS = dimap f g stepS
            , switchS = dimap f g switchS
            }

instance Profunctor (State 'Wait) where
    dimap f g Waiting{..} =
        Waiting
            { traceW = dimap f g . traceW . f
            , switchW = dimap f g switchW
            }

instance Profunctor (State 'Run) where
    dimap f g Running{..} =
        Running
            { traceR = dimap f g . traceR . f
            , switchR = dimap f g switchR
            }

-- | Feed an input to a waiting machine
traceWC :: a -> Tracing 'Wait a b -> Tracing 'Step a b
traceWC a Tracing{state = Waiting{..}} = traceW a

-- | Step a stepping machine
stepSC :: Tracing 'Step a b -> Tracing 'Wait a b
stepSC Tracing{state = Stepping{..}} = stepS

-- | Feed an input to a running machine
traceRC :: a -> Tracing 'Run a b -> Tracing 'Run a b
traceRC a Tracing{state = Running{..}} = traceR a

-- | Switch a waiting machine to a running machine
switchWC :: Tracing 'Wait a b -> Tracing 'Run a b
switchWC Tracing{state = Waiting{..}} = switchW

-- | Switch a stepping machine to a running machine
switchSC :: Tracing 'Step a b -> Tracing 'Run a b
switchSC Tracing{state = Stepping{..}} = switchS

-- | Switch a running machine to a stepping machine
switchRC :: Tracing 'Run a b -> Tracing 'Step a b
switchRC Tracing{state = Running{..}} = switchR

--- API

-- | Singletons for the state of the tracing
data StateS (state :: MonitorState) where
    WaitS :: StateS 'Wait
    StepS :: StateS 'Step
    RunS :: StateS 'Run

-- | A tracing in any state
data AnyTracing a b = forall x. AnyTracing (StateS x) (Tracing x a b)

-- | Get the state of a tracing at the value level
tracingState :: AnyTracing a b -> MonitorState
tracingState (AnyTracing WaitS _) = Wait
tracingState (AnyTracing StepS _) = Step
tracingState (AnyTracing RunS _) = Run

withTracingState :: (forall w . StateS w -> b) -> MonitorState -> b
withTracingState f Wait = f WaitS
withTracingState f Step = f StepS
withTracingState f Run = f RunS

-- | Feed an input into any tracing, silently ignoring the input if the tracing
-- is in `StepT` state
-- * tracing in `WaitT` state will be switched to `StepT` state
-- * tracing in `RunT` will stay into `RunT` state
-- * tracing in `StepT` will stay into `StepT` state
trace :: a -> AnyTracing a b -> AnyTracing a b
trace a (AnyTracing WaitS m) = AnyTracing StepS (traceWC a m)
trace a (AnyTracing RunS m) = AnyTracing RunS (traceRC a m)
trace _ m = m

-- | Step any tracing, doing nothing if the tracing is in `WaitT` state or
-- `RunT` state
-- * tracing in `StepT` state will be switched to `WaitT` state
-- * tracing in `WaitT` will stay into `WaitT` state
-- * tracing in `RunT` will stay into `RunT` state
step :: AnyTracing a b -> AnyTracing a b
step (AnyTracing StepS m) = AnyTracing WaitS (stepSC m)
step m = m

-- | Switch any tracing
-- * switch the tracing to `RunT` state in case of `WaitT` state or `StepT` state
-- * switch the tracing to `StepT` state in case of `RunT` state
switch :: AnyTracing a b -> AnyTracing a b
switch (AnyTracing WaitS m) = AnyTracing RunS (switchWC m)
switch (AnyTracing StepS m) = AnyTracing RunS (switchSC m)
switch (AnyTracing RunS m) = AnyTracing StepS (switchRC m)

-- | Observe any tracing
observe :: AnyTracing a b -> b
observe (AnyTracing _ Tracing{..}) = observation
