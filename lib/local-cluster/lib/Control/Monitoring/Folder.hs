{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monitoring.Folder
    ( mkTracingFromFold
    , mkTracingFromMoore
    , mooreToFold
    , foldToMoore
    )
where

import Prelude

import Control.Comonad
    ( Comonad (extract)
    )
import Control.Foldl
    ( Fold (..)
    )
import Control.Monitoring.Tracing
    ( State (..)
    , StateS (..)
    , Tracing (..)
    )
import Data.Machine
    ( Moore (..)
    )

consume :: Fold a b -> a -> Fold a b
consume (Fold f s e) a = Fold f (f s a) e

-- | Create a machine from a `Fold` in a given initial state
mkTracingFromFold :: forall w a b. Fold a b -> StateS w -> Tracing w a b
mkTracingFromFold = go
  where
    go :: Fold a b -> StateS w' -> Tracing w' a b
    go f w =
        Tracing
            { observation = extract f
            , state = case w of
                WaitS ->
                    Waiting
                        { traceW = \a -> go (consume f a) StepS
                        , switchW = go f RunS
                        }
                StepS ->
                    Stepping
                        { stepS = go f WaitS
                        , switchS = go f RunS
                        }
                RunS ->
                    Running
                        { traceR = \a -> go (consume f a) RunS
                        , switchR = go f StepS
                        }
            }

-- | Convert a `Moore` machine to a `Fold`
mooreToFold :: Moore a b -> Fold a b
mooreToFold m = Fold (\(Moore _ f) -> f) m (\(Moore b _) -> b)

-- | Convert a `Fold` to a `Moore` machine
foldToMoore :: Fold a b -> Moore a b
foldToMoore f = Moore (extract f) $ foldToMoore . consume f

-- | Create a tracing from a `Moore` machine in a given initial state
mkTracingFromMoore :: forall w a b. Moore a b -> StateS w -> Tracing w a b
mkTracingFromMoore = mkTracingFromFold . mooreToFold
