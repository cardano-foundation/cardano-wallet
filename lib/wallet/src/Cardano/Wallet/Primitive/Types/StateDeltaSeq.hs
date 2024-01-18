{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides the 'StateDeltaSeq' type and related functions.
--
-- The 'StateDeltaSeq' type provides a way to model an abstract linear sequence
-- of state transitions, where each transition consists of an initial state
-- value, a delta value, and a final state value.
--
-- Such sequences are __contiguous__, such that the final state of any given
-- transition is the initial state of the next transition in the sequence:
--
-- @
--    transition_0_1: state_0 -> delta_0_1 -> state_1
--    transition_1_2: state_1 -> delta_1_2 -> state_2
--    transition_2_3: state_2 -> delta_2_3 -> state_3
--    ...
--    transition_p_q: state_p -> delta_p_q -> state_q
-- @
--
-- By itself, the 'StateDeltaSeq` type does not maintain any invariant relating
-- to the validity of individual transitions, and does not maintain any
-- knowledge of how to apply delta values to state values.
--
-- Instead, the 'StateDeltaSeq' type only ensures that transition sequences are
-- contiguous (see above). Ensuring and preserving validity of transitions is
-- the responsibility of the consumer.
--
-- However, when given a transition function of type 'state -> delta -> state',
-- the 'StateDeltaSeq' type does provide several functions for constructing
-- sequences that are valid, and verifying that existing sequences are valid.
--
-- Basic usage:
--
--    - Use 'fromStateDeltas' to construct a sequence.
--    - Use 'applyDelta' to extend a sequence.
--    - Use 'isValid' to verify a sequence.
--    - Use 'toTransitionList' to list all transitions of a sequence.
--
module Cardano.Wallet.Primitive.Types.StateDeltaSeq
    (
    -- * Types
      StateDeltaSeq

    -- * Constructors
    , fromState
    , fromStateDeltas
    , fromStateDeltasUnchecked

    -- * Indicators
    , isPrefixOf
    , isSuffixOf
    , isValid
    , isValidM

    -- * Conversions
    , toDeltaList
    , toStateList
    , toTransitionList

    -- * Views
    , headState
    , lastState

    -- * Maps
    , mapDeltas
    , mapStates
    , mapStatesDeltas

    -- * Counts
    , countTransitions
    , countTransitionsWhere
    , countEmptyTransitions
    , countEmptyTransitionsWhere

    -- * Extension
    , applyDelta
    , applyDeltas
    , applyDeltaM
    , applyDeltasM

    -- * Shrinking
    , dropEmptyTransition
    , dropEmptyTransitions
    , dropEmptyTransitionWhere
    , dropEmptyTransitionsWhere
    , dropHead
    , dropLast
    , prefixes
    , suffixes

    ) where

import Prelude hiding
    ( head
    , iterate
    , seq
    , tail
    )

import Control.Applicative
    ( ZipList (..)
    )
import Control.Monad
    ( foldM
    )
import Control.Monad.Extra
    ( allM
    )
import Control.Monad.Identity
    ( Identity (..)
    )
import Data.Bifoldable
    ( Bifoldable (..)
    )
import Data.Bifunctor
    ( Bifunctor (..)
    )
import Data.Coerce
    ( coerce
    )
import Data.Function
    ( on
    )
import Data.Functor
    ( (<&>)
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Sequence
    ( Seq (Empty, (:<|), (:|>))
    )

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The 'StateDeltaSeq' type provides a way to model an abstract sequence of
--   state transitions, where each transition consists of an initial state
--   value, a delta value, and a final state value.
--
-- Such sequences are __contiguous__, such that the final state of any given
-- transition is the initial state of the next transition in the sequence:
--
-- @
--    transition_0_1: state_0 -> delta_0_1 -> state_1
--    transition_1_2: state_1 -> delta_1_2 -> state_2
--    transition_2_3: state_2 -> delta_2_3 -> state_3
--    ...
--    transition_p_q: state_p -> delta_p_q -> state_q
-- @
--
data StateDeltaSeq state delta = StateDeltaSeq
    { head :: state
    , tail :: (Seq (delta, state))
    }
    deriving Eq

-- | The type of list elements returned by 'toStateDeltaList'.
--
data StateDeltaListItem state delta
    = State !state
    | Delta !delta
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Bifoldable StateDeltaSeq where
    bifoldMap f g s = head <> F.foldMap (uncurry (<>)) tail
      where
        StateDeltaSeq {head, tail} = mapStatesDeltas f g s

instance Bifunctor StateDeltaSeq where
    bimap = mapStatesDeltas
    first = mapStates
    second = mapDeltas

instance Foldable (StateDeltaSeq state) where
    foldMap f s = F.foldMap f (toDeltaList s)
    length = countTransitions

instance Functor (StateDeltaSeq state) where
    fmap = mapDeltas

instance (Show state, Show delta) => Show (StateDeltaSeq state delta) where
    show = show . NE.toList . toStateDeltaList

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

-- | Constructs a 'StateDeltaSeq' from an initial state.
--
-- The resultant sequence will have no transitions.
--
-- To add a transition to the sequence, use the 'applyDelta' function.
--
fromState :: s -> StateDeltaSeq s d
fromState state = StateDeltaSeq state Seq.empty

-- | Constructs a 'StateDeltaSeq' from an initial state and a sequence of
--   deltas, according to a given state transition function.
--
-- To add further transitions to the sequence, use the 'applyDelta' function.
--
fromStateDeltas :: (s -> d -> s) -> s -> [d] -> StateDeltaSeq s d
fromStateDeltas next s ds = applyDeltas next ds (fromState s)

-- | Constructs a 'StateDeltaSeq' from an initial state and a sequence of
--   deltas, without a state transition function to ensure validity.
--
fromStateDeltasUnchecked :: s -> [(d, s)] -> StateDeltaSeq s d
fromStateDeltasUnchecked head deltaStates =
    StateDeltaSeq head (Seq.fromList deltaStates)

--------------------------------------------------------------------------------
-- Counts
--------------------------------------------------------------------------------

-- | Counts the total number of transitions in a 'StateDeltaSeq'.
--
countTransitions :: StateDeltaSeq s d -> Int
countTransitions StateDeltaSeq {tail} = Seq.length tail

-- | Counts the number of transitions in a 'StateDeltaSeq' for which the given
--   indicator function returns 'True'.
--
countTransitionsWhere :: ((s, d, s) -> Bool) -> StateDeltaSeq s d -> Int
countTransitionsWhere f s = length $ findTransitionsWhere f s

-- | Counts the number of empty transitions in a 'StateDeltaSeq'.
--
-- A transition is empty if its initial state is equal to its final state.
--
countEmptyTransitions :: Eq s => StateDeltaSeq s d -> Int
countEmptyTransitions = countEmptyTransitionsWhere (const True)

-- | Counts the number of empty transitions in a 'StateDeltaSeq' for which the
--   given indicator function returns 'True'.
--
-- A transition is empty if its initial state is equal to its final state.
--
countEmptyTransitionsWhere :: Eq s => (d -> Bool) -> StateDeltaSeq s d -> Int
countEmptyTransitionsWhere f s = length $ emptyTransitionsWhere f s

--------------------------------------------------------------------------------
-- Indicators
--------------------------------------------------------------------------------

-- | Returns 'True' if (and only if) the first sequence is a prefix of the
--   second sequence.
--
-- If the sequences are identical, this function returns 'True'.
--
isPrefixOf :: (Eq s, Eq d) => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isPrefixOf = L.isPrefixOf `on` toTransitionList

-- | Returns 'True' if (and only if) the first sequence is a suffix of the
--   second sequence.
--
-- If the sequences are identical, this function returns 'True'.
--
isSuffixOf :: (Eq s, Eq d) => StateDeltaSeq s d -> StateDeltaSeq s d -> Bool
isSuffixOf = L.isSuffixOf `on` toTransitionList

-- | Returns 'True' if (and only if) the given sequence is valid according to
--   the given state transition function.
--
isValid :: (Eq s) => (s -> d -> s) -> StateDeltaSeq s d -> Bool
isValid next = runIdentity . isValidM (coerce next)

-- | Returns 'True' if (and only if) the given sequence is valid according to
--   the given monadic state transition function.
--
isValidM :: (Monad m, Eq s) => (s -> d -> m s) -> StateDeltaSeq s d -> m Bool
isValidM next = allM (\(si, d, sj) -> (==) sj <$> next si d) . toTransitionList

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- | Generates the complete list of deltas for a given 'StateDeltaSeq'.
--
toDeltaList :: StateDeltaSeq s d -> [d]
toDeltaList = fmap fst . F.toList . tail

-- | Generates the complete list of states for a given 'StateDeltaSeq'.
--
toStateList :: StateDeltaSeq s d -> NonEmpty s
toStateList StateDeltaSeq {head, tail} = head :| (snd <$> F.toList tail)

-- | Converts the given 'StateDeltaSeq' to an alternating list of states and
--   deltas.
--
toStateDeltaList :: StateDeltaSeq s d -> NonEmpty (StateDeltaListItem s d)
toStateDeltaList s = NE.fromList $ interleave
    (State <$> F.toList (toStateList s))
    (Delta <$> F.toList (toDeltaList s))

-- | Converts the given 'StateDeltaSeq' to a list of transitions.
--
-- For any consecutive pair of transitions in the resultant list, the final
-- state of the first transition is guaranteed to be identical to the initial
-- state of the second transition.
--
toTransitionList :: StateDeltaSeq s d -> [(s, d, s)]
toTransitionList s = getZipList $ (,,)
    <$> ZipList states
    <*> ZipList deltas
    <*> ZipList (drop 1 states)
  where
    deltas = F.toList $ toDeltaList s
    states = F.toList $ toStateList s

--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

-- | Views the head (initial) state of a 'StateDeltaSeq'.
--
headState :: StateDeltaSeq s d -> s
headState StateDeltaSeq {head} = head

-- | Views the last (final) state of a 'StateDeltaSeq'.
--
lastState :: StateDeltaSeq s d -> s
lastState StateDeltaSeq {head, tail} = case tail of
    Empty -> head
    _ :|> (_, s) -> s

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

-- | Applies the given function to all delta values of a 'StateDeltaSeq'.
--
-- To verify whether the resulting sequence is valid, use 'isValid'.
--
mapDeltas :: (d1 -> d2) -> StateDeltaSeq s d1 -> StateDeltaSeq s d2
mapDeltas f StateDeltaSeq {head, tail} = StateDeltaSeq
    {head, tail = first f <$> tail}

-- | Applies the given function to all state values of a 'StateDeltaSeq'.
--
-- To verify whether the resulting sequence is valid, use 'isValid'.
--
mapStates :: (s1 -> s2) -> StateDeltaSeq s1 d -> StateDeltaSeq s2 d
mapStates f StateDeltaSeq {head, tail} = StateDeltaSeq
    {head = f head, tail = second f <$> tail}

-- | Transforms both the state and delta values of a 'StateDeltaSeq'.
--
-- To verify whether the resulting sequence is valid, use 'isValid'.
--
mapStatesDeltas
    :: (s1 -> s2) -> (d1 -> d2) -> StateDeltaSeq s1 d1 -> StateDeltaSeq s2 d2
mapStatesDeltas f g StateDeltaSeq {head, tail} = StateDeltaSeq
    {head = f head, tail = bimap g f <$> tail}

--------------------------------------------------------------------------------
-- Extension
--------------------------------------------------------------------------------

-- | Extends a 'StateDeltaSeq' with an additional delta, according to the given
--   state transition function.
--
-- If the original sequence was valid according to the given state transition
-- function, then the resulting sequence will also be valid.
--
-- To verify whether the resulting sequence is valid, use 'isValid'.
--
applyDelta :: (s -> d -> s) -> d -> StateDeltaSeq s d -> StateDeltaSeq s d
applyDelta next delta = runIdentity . applyDeltaM (coerce next) delta

-- | Extends a 'StateDeltaSeq' with an additional delta, according to the given
--   monadic state transition function.
--
-- If the original sequence was valid according to the given state transition
-- function, then the resulting sequence will also be valid.
--
-- To verify whether the resulting sequence is valid, use 'isValid'.
--
applyDeltaM
    :: Functor m
    => (s -> d -> m s)
    -> d
    -> StateDeltaSeq s d
    -> m (StateDeltaSeq s d)
applyDeltaM next delta seq@StateDeltaSeq {head, tail} =
    next (lastState seq) delta <&> \state ->
        StateDeltaSeq {head, tail = tail :|> (delta, state)}

-- | Extends a 'StateDeltaSeq' with multiple additional deltas, according to
--   the given state transition function.
--
-- See 'applyDelta'.
--
applyDeltas
    :: Foldable f
    => (s -> d -> s)
    -> f d
    -> StateDeltaSeq s d
    -> StateDeltaSeq s d
applyDeltas next deltas seq = F.foldl' (flip (applyDelta next)) seq deltas

-- | Extends a 'StateDeltaSeq' with multiple additional deltas, according to
--   the given monadic state transition function.
--
-- See 'applyDeltas'.
--
applyDeltasM
    :: (Foldable f, Monad m)
    => (s -> d -> m s)
    -> f d
    -> StateDeltaSeq s d
    -> m (StateDeltaSeq s d)
applyDeltasM next deltas seq = foldM (flip (applyDeltaM next)) seq deltas

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

-- | Removes the head (left-most) transition of a 'StateDeltaSeq'.
--
dropHead :: StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
dropHead StateDeltaSeq {tail} = case tail of
    Empty -> Nothing
    (_, head) :<| xs -> Just StateDeltaSeq {head, tail = xs}

-- | Removes the last (right-most) transition of a 'StateDeltaSeq'.
--
dropLast :: StateDeltaSeq s d -> Maybe (StateDeltaSeq s d)
dropLast StateDeltaSeq {head, tail} = case tail of
    Empty -> Nothing
    xs :|> _ -> Just StateDeltaSeq {head, tail = xs}

-- | Lists all proper prefixes of the given 'StateDeltaSeq'.
--
-- The list is sorted into ascending order of length, such that each element is
-- a proper prefix of the subsequent element:
--
-- @
--    [ []
--    , [transition_0_1]
--    , [transition_0_1, transition_1_2]
--    , [transition_0_1, transition_1_2, transition_2_3]
--    , ...
-- ]
-- @
--
-- The original sequence is not included in the result.
--
prefixes :: StateDeltaSeq s d -> [StateDeltaSeq s d]
prefixes = iterateMaybe dropLast

-- | Lists all proper suffixes of the given 'StateDeltaSeq'.
--
-- The list is sorted into ascending order of length, such that each element is
-- a proper suffix of the subsequent element:
--
-- @
--    [                                               []
--    ,                                 [transition_r_s]
--    ,                 [transition_q_r, transition_r_s]
--    , [transition_p_q, transition_q_r, transition_r_s]
--    , ...
--    ]
-- @
--
-- The original sequence is not included in the result.
--
suffixes :: StateDeltaSeq s d -> [StateDeltaSeq s d]
suffixes = iterateMaybe dropHead

-- | For a given sequence 's', generates all proper subsequences of 's' where
--   exactly one empty transition has been removed.
--
dropEmptyTransition
    :: Eq s => StateDeltaSeq s d -> [StateDeltaSeq s d]
dropEmptyTransition = dropEmptyTransitionWhere (const True)

-- | For a given sequence 's', generates all proper subsequences of 's' where
--   exactly one empty transition matching the given indicator function has
--   been removed.
--
dropEmptyTransitionWhere
    :: Eq s => (d -> Bool) -> StateDeltaSeq s d -> [StateDeltaSeq s d]
dropEmptyTransitionWhere f s@StateDeltaSeq {head, tail} =
    StateDeltaSeq head . flip Seq.deleteAt tail <$> emptyTransitionsWhere f s

-- | Removes all empty transitions from a 'StateDeltaSeq'.
--
dropEmptyTransitions
    :: Eq s => StateDeltaSeq s d -> StateDeltaSeq s d
dropEmptyTransitions = dropEmptyTransitionsWhere (const True)

-- | Removes all empty transitions that match the given indicator function
--   from a 'StateDeltaSeq'.
--
dropEmptyTransitionsWhere
    :: Eq s => (d -> Bool) -> StateDeltaSeq s d -> StateDeltaSeq s d
dropEmptyTransitionsWhere f s@StateDeltaSeq {head, tail} = StateDeltaSeq head $
    F.foldl' (flip Seq.deleteAt) tail (reverse $ emptyTransitionsWhere f s)

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

-- | Finds the indices of empty transitions that match the given indicator
--   function.
--
emptyTransitionsWhere :: Eq s => (d -> Bool) -> StateDeltaSeq s d -> [Int]
emptyTransitionsWhere f =
    findTransitionsWhere $ \(si, d, sj) -> si == sj && f d

-- | Finds the indices of all transitions that match the given indicator
--   function.
--
findTransitionsWhere :: ((s, d, s) -> Bool) -> StateDeltaSeq s d -> [Int]
findTransitionsWhere f s = fst <$>
    filter
        (f . snd)
        (zip [0 ..] (toTransitionList s))

-- | Interleaves two lists together in an alternating fashion.
--
-- The head of the first list appears first in the resulting list.
--
-- All items are preserved.
--
interleave :: [a] -> [a] -> [a]
interleave (a1 : a1s) (a2 : a2s) = a1 : a2 : interleave a1s a2s
interleave (     a1s) [        ] = a1s
interleave [        ] (     a2s) = a2s

-- | Repeatedly applies a given function to an initial value until the result
--   is 'Nothing'.
--
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f =
    loop []
  where
    loop !as !a = maybe as (\p -> loop (p : as) p) (f a)
