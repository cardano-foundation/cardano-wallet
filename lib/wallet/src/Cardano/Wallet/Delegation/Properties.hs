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
import Test.QuickCheck
    ( Gen
    , Property
    , counterexample
    , forAll
    , (===)
    )

-- | A step of the history, with both states and the change to compute
-- new from old.
data Step slot pool = Step
    { old_ :: History slot pool
    , new_ :: History slot pool
    , delta_ :: Operation slot pool
    }
    deriving (Show)

-- | Compute a not so random slot from a 'History' of delegations.
type GenSlot slot pool = History slot pool -> Gen slot

property'
    :: (Ord slot, Show slot, Eq (Status pool), Show (Status pool))
    => GenSlot slot pool
    -> Step slot pool
    -> (Status pool -> Status pool -> Property)
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
    :: (Eq a, Show a)
    => (a -> (Bool, Maybe (Status pool)))
    -> (Maybe (Status pool) -> a)
    -> a
    -> a
    -> Property
precond check target old new
    | (fst $ check old) = counterexample "new target" $ new === (target (snd $ check old))
    | otherwise = counterexample "no changes" $ new === old

-- | Properties replicated verbatim from specifications. See
-- 'specifications/Cardano/Wallet/delegation.lean'
properties
    :: (Ord slot, Eq (Status pool), Show slot, Show (Status pool), Show pool)
    => GenSlot slot pool
    -> Step slot pool
    -> Property
properties c s =
    let that msg = counterexample ("falsified: " <> msg)
        prop cond target =
            counterexample (show s)
                $ property' c s
                $ precond cond target
    in  case delta_ s of
        Register _ ->
            that "register invariant is respected"
                $ prop
                    (\case
                        Inactive -> (True, Nothing)
                        _ -> (False, Nothing)
                    )
                    (const Registered)
        Deregister _ ->
            that "deregister invariant is respected"
                $ prop
                    ( \case
                        Registered -> (True, Nothing)
                        Active _ -> (True, Nothing)
                        Voted _ -> (True, Nothing)
                        ActiveAndVoted _ _ -> (True, Nothing)
                        _ -> (False, Nothing)
                    )
                    (const Inactive)
        Delegate p _ -> do
            that "delegate invariant is respected"
                $ prop
                    ( \case
                        Registered -> (True, Just Registered)
                        Active p' -> (True, Just (Active p'))
                        Voted v -> (True, Just (Voted v))
                        ActiveAndVoted p' v -> (True, Just (ActiveAndVoted p' v))
                        _ -> (False, Nothing)
                    )
                    ( \case
                         Just Registered -> Active p
                         Just (Active _) -> Active p
                         Just (Voted v) -> ActiveAndVoted p v
                         Just (ActiveAndVoted _ v) -> ActiveAndVoted p v
                         _ -> error "Delegate branch broke"
                    )
        Vote v _ -> do
            that "vote invariant 1 is respected"
                $ prop
                    ( \case
                        Registered -> (True, Just Registered)
                        Voted v' -> (True, Just (Voted v'))
                        Active p -> (True, Just (Active p))
                        ActiveAndVoted p v' -> (True, Just (ActiveAndVoted p v'))
                        _ -> (False, Nothing)
                    )
                    ( \case
                         Just Registered -> Voted v
                         Just (Voted _) -> Voted v
                         Just (Active p) -> ActiveAndVoted p v
                         Just (ActiveAndVoted p _) -> ActiveAndVoted p v
                         _ -> error "Vote branch broke"
                    )
        DelegateAndVote p v _ -> do
            that "delegate and vote invariant is respected"
                $ prop
                    ( \case
                        Registered -> (True, Nothing)
                        Active _ -> (True, Nothing)
                        Voted _ -> (True, Nothing)
                        ActiveAndVoted _ _ -> (True, Nothing)
                        _ -> (False, Nothing)
                    )
                    (const (ActiveAndVoted p v))
        Rollback _ ->
            that "rollback invariant is respected"
                $ property' c s (===)
