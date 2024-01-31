{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
    , DRep (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    , VoteAction (..)
    ) where

import Prelude

import Control.DeepSeq
    ( NFData (..)
    )
import Data.ByteString
    ( ByteString
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
import GHC.Generics
    ( Generic
    )

import qualified Data.Map.Strict as Map

-- | Delta type for the delegation 'History'.
data Operation slot pool
    = Register slot
    | Deregister slot
    | Delegate pool slot
    | Rollback slot
    deriving (Show)

-- | Target slot of each 'Operation'.
slotOf :: Operation slot pool -> slot
slotOf (Register x) = x
slotOf (Deregister x) = x
slotOf (Delegate _ x) = x
slotOf (Rollback x) = x

-- | Valid state for the delegations, independent of time.
data Status pool = Inactive | Registered | Active pool
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
transition (Deregister _) _ = Inactive
transition _ s = s

type Change slot pool = History slot pool -> History slot pool

cut :: (slot -> Bool) -> Change slot pool
cut op = fst . Map.spanAntitone op

-- | Status of the delegation at a given slot.
status :: Ord slot => slot -> History slot pool -> Status pool
status x = maybe Inactive snd . Map.lookupMax . cut (<= x)

newtype DRepKeyHash = DRepKeyHash { getDRepKeyHash :: ByteString }
    deriving (Generic, Eq, Ord)

instance NFData DRepKeyHash

newtype DRepScriptHash = DRepScriptHash { getDRepScriptHash :: ByteString }
    deriving (Generic, Eq, Ord)

instance NFData DRepScriptHash

data DRep =
    DRepFromKeyHash DRepKeyHash | DRepFromScriptHash DRepScriptHash
    deriving (Eq, Generic)
    deriving anyclass NFData

-- | Vote action.
data VoteAction
    = Abstain
    | NoConfidence
    | VoteTo !DRep
    deriving (Eq, Generic)
    deriving anyclass NFData
