{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
--
-- Data types that represents a history of delegations and its changes.
module Cardano.Wallet.Delegation.Model
    ( Operation (..)
    , slotOf
    , Status (..)
    , History
    , status
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.DRep
    ( DRep
    )
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

-- | Delta type for the delegation 'History'.
data Operation slot pool
    = Register slot
    | Deregister slot
    | Delegate pool slot
    | DelegateAndVote pool DRep slot
    | Vote DRep slot
    | Rollback slot
    deriving (Show)

-- | Target slot of each 'Operation'.
slotOf :: Operation slot pool -> slot
slotOf (Register x) = x
slotOf (Deregister x) = x
slotOf (Delegate _ x) = x
slotOf (DelegateAndVote _ _ x) = x
slotOf (Vote _ x) = x
slotOf (Rollback x) = x

-- | Valid state for the delegations, independent of time.
data Status pool
    = Inactive
    | Registered
    | Active pool
    | ActiveAndVoted pool DRep
    | Voted DRep
    deriving (Show, Eq)

-- | Delegation history implementation.
type History slot pool = Map slot (Status pool)

instance (Ord slot, Eq pool) => Delta (Operation slot pool) where
    type Base (Operation slot pool) = History slot pool
    apply r hist = hist' & if miss == wanted then id else Map.insert slot wanted
      where
        slot = slotOf r
        hist' = cut (< slot) hist
        miss = status slot hist'
        wanted = transition r $ status slot hist

transition :: Operation slot pool -> Status pool -> Status pool
transition (Register _) Inactive = Registered
transition (Delegate p _) Registered = Active p
transition (Delegate p _) (Active _) = Active p
transition (Vote v _) Registered = Voted v
transition (DelegateAndVote p v _) Registered = ActiveAndVoted p v
transition (Vote v _) (Voted _) = Voted v
transition (Delegate p _) (Voted v) = ActiveAndVoted p v
transition (DelegateAndVote p v _) (Voted _) = ActiveAndVoted p v
transition (Vote v _) (Active p) = ActiveAndVoted p v
transition (DelegateAndVote p v _) (Active _) = ActiveAndVoted p v
transition (DelegateAndVote p v _) (ActiveAndVoted _ _) = ActiveAndVoted p v
transition (Delegate p _) (ActiveAndVoted _ v) = ActiveAndVoted p v
transition (Vote v _) (ActiveAndVoted p _) = ActiveAndVoted p v
transition (Deregister _) _ = Inactive
transition _ s = s

type Change slot pool = History slot pool -> History slot pool

cut :: (slot -> Bool) -> Change slot pool
cut op = fst . Map.spanAntitone op

-- | Status of the delegation at a given slot.
status :: Ord slot => slot -> History slot pool -> Status pool
status x = maybe Inactive snd . Map.lookupMax . cut (<= x)
