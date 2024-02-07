{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}

-- |
-- Copyright: © 2022–2023 IOHK
-- License: Apache-2.0
--
-- Properties of the delegations-history model.
module Cardano.Wallet.Delegation.Properties
    ( GenSlot
    , Step (..)
    , properties
    )
where

import Prelude

import Cardano.Wallet.Delegation.Model
    ( History
    , Operation (..)
    , Status (..)
    , slotOf
    , status
    )
import Control.Applicative
    ( (<|>)
    )
import Test.QuickCheck
    ( Gen
    , Property
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

-- | Compute a not so random slot from a 'History' of delegations.
type GenSlot slot drep pool = History slot drep pool -> Gen slot

property'
    :: (Ord a, Show a, Show drep, Show pool, Eq drep, Eq pool)
    => (History a drep pool -> Gen a)
    -> Step a drep pool
    -> (Status drep pool -> Status drep pool -> Property)
    -> Property
property' genSlot Step{old_ = xs, new_ = xs', delta_ = diff} change =
    let x = slotOf diff
        old = status x xs
    in  forAll (genSlot xs') $ \y ->
            let new = status y xs'
            in  case compare y x of
                    LT -> new === status y xs
                    _ -> change old new

precond
    :: (Eq drep, Eq pool, Show drep, Show pool)
    => (Status drep pool -> (Bool, Maybe x))
    -> (Maybe x -> Status drep pool)
    -> Status drep pool
    -> Status drep pool
    -> Property
precond check target old new
    | (fst $ check old) = counterexample "new target"
        $ new === (target (snd $ check old))
    | otherwise = counterexample "no changes"
        $ new === old

-- | Properties replicated verbatim from specifications. See
-- 'specifications/Cardano/Wallet/delegation.lean'
properties
    :: (Show slot, Show drep, Show pool, Ord slot, Eq drep, Eq pool)
    => (History slot drep pool -> Gen slot)
    -> Step slot drep pool
    -> Property
properties genSlot step =
    let that msg = counterexample ("falsified: " <> msg)
        prop cond target =
            counterexample (show step)
                $ property' genSlot step
                $ precond cond target
    in  case delta_ step of
            Deregister _ ->
                that "deregister invariant is respected"
                    $ prop
                        ( \case
                            Active _ _ -> (True, Nothing)
                            _ -> (False, Nothing)
                        )
                        (const Inactive)
            VoteAndDelegate v p _ -> do
                that "delegate and/or vote invariant is respected"
                    $ prop
                        ( \case
                            Inactive -> (True, Just Inactive)
                            Active v' p'-> (True, Just (Active v' p'))
                        )
                        $ \case
                            Just (Active v' p') -> Active (v <|> v') (p <|> p')
                            Just Inactive -> Active v p
                            _ -> error "VoteAndDelegate branch broke"
            Rollback _ ->
                that "rollback invariant is respected"
                    $ property' genSlot step (===)
