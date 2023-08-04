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
import Prelude

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
property' genSlot Step {old_ = xs, new_ = xs', delta_ = diff} change =
  let
    x = slotOf diff
    old = status x xs
  in
    forAll (genSlot xs') $ \y ->
      let
        new = status y xs'
      in
        case compare y x of
          LT -> new === status y xs
          _ -> change old new

precond :: (Eq a, Show a) => (a -> Bool) -> a -> a -> a -> Property
precond check target old new
  | check old = counterexample "new target" $ new === target
  | otherwise = counterexample "no changes" $ new === old

-- | Properties replicated verbatim from specifications. See
-- 'specifications/Cardano/Wallet/delegation.lean'
properties
  :: (Ord slot, Eq (Status pool), Show slot, Show (Status pool), Show pool)
  => GenSlot slot pool
  -> Step slot pool
  -> Property
properties c s =
  let
    that msg = counterexample ("falsified: " <> msg)
    prop cond target =
      counterexample (show s)
        $ property' c s
        $ precond cond target
  in
    case delta_ s of
      Register _ ->
        that "register invariant is respected"
          $ prop
            (== Inactive)
            Registered
      Deregister _ ->
        that "deregister invariant is respected"
          $ prop
            ( \case
                Registered -> True
                Active _ -> True
                _ -> False
            )
            Inactive
      Delegate p _ ->
        that "delegate invariant is respected"
          $ prop
            ( \case
                Registered -> True
                Active _ -> True
                _ -> False
            )
            (Active p)
      Rollback _ ->
        that "rollback invariant is respected"
          $ property' c s (===)
