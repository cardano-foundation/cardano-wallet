{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Copyright: © 2022–2023 IOHK
-- License: Apache-2.0
--
-- Properties of the delegations-history model.
module Cardano.Wallet.Delegation.Properties
    ( Step (..)
    , properties
    )
where

import Prelude

import Cardano.Wallet.Delegation.Model
    ( History
    , Operation (..)
    , Status (..)
    , applyTransition
    , slotOf
    , status
    )
import Test.QuickCheck
    ( Gen
    , Property
    , conjoin
    , counterexample
    , forAll
    , (===)
    )

-- | A step of the history, with both states and the change to compute
-- new from old.
data Step slot drep pool = Step
    { old_ :: History slot drep pool
    , new_ :: History slot drep pool
    , delta_ :: Operation slot drep pool
    }
    deriving (Show)

setsTheFuture
    :: (Ord slot, Show slot, Show drep, Show pool, Eq drep, Eq pool)
    => (History slot drep pool -> Gen slot)
    -> Step slot drep pool
    -> (slot -> Operation slot drep pool)
    -> (Status drep pool -> Status drep pool)
    -> Property
setsTheFuture genSlot Step{old_=history, new_=history', delta_} op transition =
    let x = slotOf delta_
        old = status x history
    in  conjoin
        [ delta_ === op x
        , forAll (genSlot history') $ \y ->
            let new = status y history'
            in  case compare y x of
                    LT -> new === status y history
                    _ -> new === transition old
        ]

-- | Properties replicated verbatim from specifications.
-- See 'specifications/Cardano/Wallet/Delegation.agda'.
properties
    :: (Show slot, Show drep, Show pool, Ord slot, Eq drep, Eq pool)
    => (History slot drep pool -> Gen slot)
    -> Step slot drep pool
    -> Property
properties genSlot step =
    let that msg = counterexample ("falsified: " <> msg)
        setsTheFuture' op =
            counterexample (show step)
                . setsTheFuture genSlot step op
    in  case delta_ step of
            ApplyTransition t _ ->
                that "ApplyTransition invariant is respected"
                    $ setsTheFuture'
                        (ApplyTransition t)
                        (applyTransition t)
            Rollback _ ->
                that "Rollback invariant is respected"
                    $ setsTheFuture'
                        Rollback
                        id
