{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright: © 2021-2023 IOHK, 2024-2025 Cardano Foundation
License: Apache-2.0

Internal representation of 'Embedding'
in terms of state machines.
-}
module Data.Delta.Embedding.Internal (
      Machine (..)
    , idle
    , pairMachine
    , fromState
    ) where

import Prelude

import Data.Delta.Core
    ( Delta (..)
    )
import Data.Semigroupoid
    ( Semigroupoid (..)
    )

{-------------------------------------------------------------------------------
    Machine (state machines) with efficient composition
-------------------------------------------------------------------------------}
-- Strict pair.
-- If a value of this type is in WHNF, so are the two components.
-- data StrictPair a b = !a :*: !b
-- infixr 1 :*:

-- | A state machine that maps deltas to deltas.
-- This machine always carries a state of type 'Base'@ db@ around.
data Machine da db = Machine
    { state_ :: !(Base db)
    , step_  :: (Base da, da) -> (db, Machine da db)
    }

-- | Composition of 'Machine'
instance Semigroupoid Machine where
    (Machine c fbc) `o` (Machine b fab) = Machine c $ \ada ->
        case fab ada of
            (db, mab) -> case fbc (b,db) of
                (dc, mbc) -> (dc, mbc `o` mab)

-- | Identity machine starting from a base type.
idle :: Delta da => Base da -> Machine da da
idle a0 = Machine a0 $ \(a1,da) -> let a2 = apply da a1 in (da, idle a2)

-- | Pair two 'Machine's.
pairMachine
    :: Machine da1 db1 -> Machine da2 db2 -> Machine (da1,da2) (db1,db2)
pairMachine (Machine s1 step1) (Machine s2 step2) =
    Machine (s1,s2) $ \((a1,a2), (da1,da2)) ->
        let (db1, m1) = step1 (a1,da1)
            (db2, m2) = step2 (a2,da2)
        in  ((db1,db2), pairMachine m1 m2)

-- | Create a 'Machine' from a specific state @s@,
-- and the built-in state 'Base'@ db@.
fromState
    :: Delta db
    => ((Base da, da) -> (Base db, s) -> (db, s))
    -> (Base db, s)
    -> Machine da db
fromState step (b,s0) = Machine b $ \ada ->
    case step ada (b,s0) of
        (db,s1) -> (db, fromState step (apply db b,s1))
