{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Data types that represents a history of delegations and its changes.
module Cardano.Wallet.Delegation.Model
    ( Operation (..)
    , Transition (..)
    , slotOf
    , Status (..)
    , History
    , status
    ) where

import Prelude

import Data.Delta
    ( Delta (..)
    )
import Data.Function
    ( (&)
    )
import Data.Map.Strict
    ( Map
    )

import qualified Data.Map.Strict as Map

data Transition drep pool
    = VoteAndDelegate (Maybe drep) (Maybe pool)
    | Deregister
    deriving (Show)

-- | Delta type for the delegation 'History'.
data Operation slot drep pool
    = ApplyTransition (Transition drep pool) slot
    | Rollback slot
    deriving (Show)

-- | Target slot of each 'Operation'.
slotOf :: Operation slot drep pool -> slot
slotOf (Rollback x) = x
slotOf (ApplyTransition _ x) = x

-- | Valid state for the delegations, independent of time.
data Status drep pool
    = Inactive
    | Active (Maybe drep) (Maybe pool)
    deriving (Show, Eq)

-- | Delegation history implementation.
type History slot drep pool = Map slot (Status drep pool)

instance (Ord slot, Eq pool, Eq drep) => Delta (Operation slot drep pool) where
    type Base (Operation slot drep pool) = History slot drep pool
    apply r hist = hist' & if miss == wanted then id else Map.insert slot wanted
      where
        slot = slotOf r
        hist' = cut (< slot) hist
        miss = status slot hist'
        wanted = case r of
            ApplyTransition t _ -> applyTransition t $ status slot hist
            Rollback _ -> status slot hist

applyTransition :: Transition drep pool -> Status drep pool -> Status drep pool
applyTransition Deregister _ = Inactive
applyTransition (VoteAndDelegate d p) (Active d' p') = Active d'' p''
    where
        d'' = insertIfJust d d'
        p'' = insertIfJust p p'
applyTransition (VoteAndDelegate d p) _ = Active d p

insertIfJust :: Maybe a -> Maybe a -> Maybe a
insertIfJust (Just y) _ = Just y
insertIfJust Nothing x = x

type Change slot drep pool = History slot drep pool -> History slot drep pool

cut :: (slot -> Bool) -> Change slot drep pool
cut op = fst . Map.spanAntitone op

-- | Status of the delegation at a given slot.
status :: Ord slot => slot -> Map slot (Status drep pool) -> Status drep pool
status x = maybe Inactive snd . Map.lookupMax . cut (<= x)
