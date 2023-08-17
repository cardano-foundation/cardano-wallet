{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Provides the 'UTxOSelection' type, which represents a selection of UTxO
-- entries from a UTxO set.
--
-- It consists of a pair of UTxO sets:
--
--    - the selected set: UTxOs that have already been selected;
--    - the leftover set: UTxOs that have not yet been selected.
--
-- To construct a 'UTxOSelection' where none of the UTxOs are selected, use
-- the 'fromIndex' function.
--
-- To construct a 'UTxOSelection' where some of the UTxOs are selected, use
-- either the 'fromIndexFiltered' or the 'fromIndexPair' functions.
--
-- To select an element (and move it from the leftover set to the selected
-- set), use the 'select' function.
--
-- A 'UTxOSelection' can be promoted to a 'UTxOSelectionNonEmpty', indicating
-- that the selected set contains at least one UTxO. To promote a selection,
-- either use the 'toNonEmpty' function to assert that it is non-empty, or use
-- the 'select' function to select a single entry.
--
module Cardano.CoinSelection.UTxOSelection
    (
      -- * Classes
      IsUTxOSelection

      -- * Types
    , UTxOSelection
    , UTxOSelectionNonEmpty

      -- * Construction and deconstruction
    , empty
    , fromIndex
    , fromIndexFiltered
    , fromIndexPair
    , toIndexPair

      -- * Promotion and demotion
    , fromNonEmpty
    , toNonEmpty

      -- * Indicator functions
    , isEmpty
    , isNonEmpty
    , isMember
    , isLeftover
    , isSelected
    , isSubSelectionOf
    , isProperSubSelectionOf

      -- * Accessor functions
    , availableBalance
    , availableMap
    , availableSize
    , leftoverBalance
    , leftoverSize
    , leftoverIndex
    , leftoverList
    , leftoverMap
    , selectedBalance
    , selectedSize
    , selectedIndex
    , selectedList
    , selectedMap

      -- * Modification
    , select
    , selectMany

    ) where

import Prelude

import Cardano.CoinSelection.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Control.Monad
    ( ap, (<=<) )
import Data.Bool
    ( bool )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( over )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe )
import Data.Tuple
    ( swap )
import GHC.Generics
    ( Generic )

import qualified Cardano.CoinSelection.UTxOIndex as UTxOIndex
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class HasUTxOSelectionState s u where

    -- | Retrieves the internal state from a selection.
    state :: s u -> State u

    -- | Reconstructs a selection from an internal state.
    fromState :: State u -> s u

class HasUTxOSelectionState s u => IsUTxOSelection s u where

    -- | The type of the list of selected UTxOs.
    type SelectedList s u

    -- | Retrieves a list of the selected UTxOs.
    selectedList :: s u -> SelectedList s u

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The internal state of a selection.
--
data State u = State
    { leftover :: !(UTxOIndex u)
      -- ^ UTxOs that have not yet been selected.
    , selected :: !(UTxOIndex u)
      -- ^ UTxOs that have already been selected.
    }
    deriving (Eq, Generic, Show)

-- | A selection for which 'isNonEmpty' may be 'False'.
--
newtype UTxOSelection u = UTxOSelection (State u)
    deriving (Eq, Generic, Show)

-- | A selection for which 'isNonEmpty' must be 'True'.
--
newtype UTxOSelectionNonEmpty u = UTxOSelectionNonEmpty (State u)
    deriving (Eq, Generic, Show)

instance HasUTxOSelectionState UTxOSelection u where
    state (UTxOSelection s) = s
    fromState = UTxOSelection

instance HasUTxOSelectionState UTxOSelectionNonEmpty u where
    state (UTxOSelectionNonEmpty s) = s
    fromState = UTxOSelectionNonEmpty

instance IsUTxOSelection UTxOSelection u where
    type SelectedList UTxOSelection u = [(u, TokenBundle)]
    selectedList = UTxOIndex.toList . selectedIndex

instance IsUTxOSelection UTxOSelectionNonEmpty u where
    type SelectedList UTxOSelectionNonEmpty u = NonEmpty (u, TokenBundle)
    selectedList = NE.fromList . UTxOIndex.toList . selectedIndex

--------------------------------------------------------------------------------
-- Construction and deconstruction
--------------------------------------------------------------------------------

-- | A completely empty selection with no selected or leftover UTxOs.
--
empty :: UTxOSelection u
empty = fromIndex UTxOIndex.empty

-- | Creates a selection where none of the UTxOs are selected.
--
-- All UTxOs in the index will be added to the leftover set.
--
fromIndex :: UTxOIndex u -> UTxOSelection u
fromIndex i = UTxOSelection State
    { leftover = i
    , selected = UTxOIndex.empty
    }

-- | Creates a selection from an index and a filter.
--
-- All UTxOs that match the given filter will be added to the selected set,
-- whereas all UTxOs that do not match will be added to the leftover set.
--
fromIndexFiltered :: Ord u => (u -> Bool) -> UTxOIndex u -> UTxOSelection u
fromIndexFiltered f =
    UTxOSelection . uncurry State . swap . UTxOIndex.partition f

-- | Creates a selection from a pair of indices.
--
-- The 1st index in the pair represents the leftover set.
-- The 2nd index in the pair represents the selected set.
--
-- Any items that are in both sets are removed from the leftover set.
--
fromIndexPair :: Ord u => (UTxOIndex u, UTxOIndex u) -> UTxOSelection u
fromIndexPair (leftover, selected) =
    UTxOSelection State
        { leftover = leftover `UTxOIndex.difference` selected
        , selected
        }

-- | Converts a selection to a pair of indices.
--
-- The 1st index in the pair represents the leftover set.
-- The 2nd index in the pair represents the selected set.
--
toIndexPair :: IsUTxOSelection s u => s u -> (UTxOIndex u, UTxOIndex u)
toIndexPair s = (leftoverIndex s, selectedIndex s)

--------------------------------------------------------------------------------
-- Promotion and demotion
--------------------------------------------------------------------------------

-- | Demotes a non-empty selection to an ordinary selection.
--
fromNonEmpty :: UTxOSelectionNonEmpty u -> UTxOSelection u
fromNonEmpty = UTxOSelection . state

-- | Promotes an ordinary selection to a non-empty selection.
--
-- Returns 'Nothing' if the the selected set is empty.
--
toNonEmpty :: IsUTxOSelection s u => s u -> Maybe (UTxOSelectionNonEmpty u)
toNonEmpty s = bool Nothing (Just $ fromState $ state s) (isNonEmpty s)

--------------------------------------------------------------------------------
-- Indicator functions
--------------------------------------------------------------------------------

-- | Returns 'True' if and only if the selected set is empty.
--
isEmpty :: IsUTxOSelection s u => s u -> Bool
isEmpty = (== 0) . selectedSize

-- | Returns 'True' if and only if the selected set is non-empty.
--
isNonEmpty :: IsUTxOSelection s u => s u -> Bool
isNonEmpty = not . isEmpty

-- | Returns 'True' if the given 'InputId' is a member of either set.
--
-- Otherwise, returns 'False'.
--
isMember :: IsUTxOSelection s u => Ord u => u -> s u -> Bool
isMember u s = isLeftover u s || isSelected u s

-- | Returns 'True' iff. the given 'InputId' is a member of the leftover set.
--
isLeftover :: IsUTxOSelection s u => Ord u => u -> s u -> Bool
isLeftover u = UTxOIndex.member u . leftoverIndex

-- | Returns 'True' iff. the given 'InputId' is a member of the selected set.
--
isSelected :: IsUTxOSelection s u => Ord u => u -> s u -> Bool
isSelected u = UTxOIndex.member u . selectedIndex

-- | Returns 'True' iff. the first selection is a sub-selection of the second.
--
-- A selection 's1' is a sub-selection of selection 's2' if (and only if) it
-- is possible to transform 's1' into 's2' through zero or more applications
-- of the 'select' function.
--
isSubSelectionOf
    :: IsUTxOSelection s1 u
    => IsUTxOSelection s2 u
    => Ord u
    => s1 u
    -> s2 u
    -> Bool
isSubSelectionOf s1 s2 = state (selectMany toSelect s1) == state s2
  where
    toSelect = fst <$> Map.toList
        (selectedMap s2 `Map.difference` selectedMap s1)

-- | Returns 'True' iff. the first selection is a proper sub-selection of the
--   second.
--
-- A selection 's1' is a proper sub-selection of selection 's2' if (and only
-- if) it is possible to transform 's1' into 's2' through one or more
-- applications of the 'select' function.
--
isProperSubSelectionOf
    :: IsUTxOSelection s1 u
    => IsUTxOSelection s2 u
    => Ord u
    => s1 u
    -> s2 u
    -> Bool
isProperSubSelectionOf s1 s2 = state s1 /= state s2 && s1 `isSubSelectionOf` s2

--------------------------------------------------------------------------------
-- Accessor functions
--------------------------------------------------------------------------------

-- | Computes the available balance.
--
-- The available balance is the sum of the selected and the leftover balances.
--
-- It predicts what 'selectedBalance' would be if every single UTxO were
-- selected.
--
-- This result of this function remains constant over applications of 'select'
-- and 'selectMany':
--
-- >>> availableBalance s == availableBalance (selectMany is s)
--
availableBalance :: IsUTxOSelection s u => s u -> TokenBundle
availableBalance s = leftoverBalance s <> selectedBalance s

-- | Computes the complete map of all available UTxOs.
--
-- The available UTxO set is the union of the selected and leftover UTxO sets.
--
-- It predicts what 'selectedMap' would be if every single UTxO were selected.
--
-- This result of this function remains constant over applications of 'select'
-- and 'selectMany':
--
-- >>> availableMap s == availableMap (selectMany is s)
--
availableMap :: IsUTxOSelection s u => Ord u => s u -> Map u TokenBundle
availableMap s = leftoverMap s <> selectedMap s

-- | Computes the size of the available UTxO set.
--
availableSize :: IsUTxOSelection s u => s u -> Int
availableSize s = leftoverSize s + selectedSize s

-- | Retrieves the balance of leftover UTxOs.
--
leftoverBalance :: IsUTxOSelection s u => s u -> TokenBundle
leftoverBalance = UTxOIndex.balance . leftoverIndex

-- | Retrieves the size of the leftover UTxO set.
--
leftoverSize :: IsUTxOSelection s u => s u -> Int
leftoverSize = UTxOIndex.size . leftoverIndex

-- | Retrieves an index of the leftover UTxOs.
--
leftoverIndex :: IsUTxOSelection s u => s u -> UTxOIndex u
leftoverIndex = leftover . state

-- | Retrieves a map of the leftover UTxOs.
--
leftoverMap :: IsUTxOSelection s u => s u -> Map u TokenBundle
leftoverMap = UTxOIndex.toMap . leftoverIndex

-- | Retrieves a list of the leftover UTxOs.
--
leftoverList :: IsUTxOSelection s u => s u -> [(u, TokenBundle)]
leftoverList = UTxOIndex.toList . leftoverIndex

-- | Retrieves the balance of selected UTxOs.
--
selectedBalance :: IsUTxOSelection s u => s u -> TokenBundle
selectedBalance = UTxOIndex.balance . selectedIndex

-- | Retrieves the size of the selected UTxO set.
--
selectedSize :: IsUTxOSelection s u => s u -> Int
selectedSize = UTxOIndex.size . selectedIndex

-- | Retrieves an index of the selected UTxOs.
--
selectedIndex :: IsUTxOSelection s u => s u -> UTxOIndex u
selectedIndex = selected . state

-- | Retrieves a map of the selected UTxOs.
--
selectedMap :: IsUTxOSelection s u => s u -> Map u TokenBundle
selectedMap = UTxOIndex.toMap . selectedIndex

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Moves a single entry from the leftover set to the selected set.
--
-- Returns 'Nothing' if the given entry is not a member of the leftover set.
--
select
    :: IsUTxOSelection s u
    => Ord u
    => u
    -> s u
    -> Maybe (UTxOSelectionNonEmpty u)
select = (toNonEmpty <=<) . withState . selectState

-- | Moves multiple entries from the leftover set to the selected set.
--
selectMany
    :: IsUTxOSelection s u
    => Ord u
    => Foldable f
    => f u
    -> s u
    -> s u
selectMany = ap fromMaybe . withState . flip (F.foldrM selectState)

--------------------------------------------------------------------------------
-- Modification (Internal)
--------------------------------------------------------------------------------

-- | Moves a single entry from the leftover set to the selected set.
--
selectState :: Ord u => u -> State u -> Maybe (State u)
selectState u s =
    updateFields <$> UTxOIndex.lookup u (leftover s)
  where
    updateFields b = s
        & over #leftover (UTxOIndex.delete u)
        & over #selected (UTxOIndex.insert u b)

-- | Applies the given function to the internal state.
--
withState
    :: Functor f
    => IsUTxOSelection s u
    => (State u -> f (State u))
    -> s u
    -> f (s u)
withState f = fmap fromState . f . state
