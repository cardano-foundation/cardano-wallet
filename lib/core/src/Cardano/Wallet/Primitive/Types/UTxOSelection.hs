{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
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
module Cardano.Wallet.Primitive.Types.UTxOSelection
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
    , leftoverBalance
    , leftoverSize
    , leftoverIndex
    , leftoverList
    , leftoverUTxO
    , selectedBalance
    , selectedSize
    , selectedIndex
    , selectedList
    , selectedUTxO

      -- * Modification
    , select
    , selectMany

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
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
import Data.Maybe
    ( fromMaybe )
import Data.Tuple
    ( swap )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class HasUTxOSelectionState u where

    -- | Retrieves the internal state from a selection.
    state :: u -> State

    -- | Reconstructs a selection from an internal state.
    fromState :: State -> u

class HasUTxOSelectionState u => IsUTxOSelection u where

    -- | The type of the list of selected UTxOs.
    type SelectedList u

    -- | Retrieves a list of the selected UTxOs.
    selectedList :: u -> SelectedList u

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The internal state of a selection.
--
data State = State
    { leftover :: !UTxOIndex
      -- ^ UTxOs that have not yet been selected.
    , selected :: !UTxOIndex
      -- ^ UTxOs that have already been selected.
    }
    deriving (Eq, Generic, Show)

-- | A selection for which 'isNonEmpty' may be 'False'.
--
newtype UTxOSelection = UTxOSelection State
    deriving (Eq, Generic, Show)

-- | A selection for which 'isNonEmpty' must be 'True'.
--
newtype UTxOSelectionNonEmpty = UTxOSelectionNonEmpty State
    deriving (Eq, Generic, Show)

instance HasUTxOSelectionState UTxOSelection where
    state (UTxOSelection s) = s
    fromState s = UTxOSelection s

instance HasUTxOSelectionState UTxOSelectionNonEmpty where
    state (UTxOSelectionNonEmpty s) = s
    fromState s = UTxOSelectionNonEmpty s

instance IsUTxOSelection UTxOSelection where
    type SelectedList UTxOSelection = [(TxIn, TxOut)]
    selectedList = UTxOIndex.toList . selectedIndex

instance IsUTxOSelection UTxOSelectionNonEmpty where
    type SelectedList UTxOSelectionNonEmpty = NonEmpty (TxIn, TxOut)
    selectedList = NE.fromList . UTxOIndex.toList . selectedIndex

--------------------------------------------------------------------------------
-- Construction and deconstruction
--------------------------------------------------------------------------------

-- | A completely empty selection with no selected or leftover UTxOs.
--
empty :: UTxOSelection
empty = fromIndex UTxOIndex.empty

-- | Creates a selection where none of the UTxOs are selected.
--
-- All UTxOs in the index will be added to the leftover set.
--
fromIndex :: UTxOIndex -> UTxOSelection
fromIndex i = UTxOSelection State
    { leftover = i
    , selected = UTxOIndex.empty
    }

-- | Creates a selection from an index and a filter.
--
-- All UTxOs that match the given filter will be added to the selected set,
-- whereas all UTxOs that do not match will be added to the leftover set.
--
fromIndexFiltered :: (TxIn -> Bool) -> UTxOIndex -> UTxOSelection
fromIndexFiltered f =
    UTxOSelection . uncurry State . swap . UTxOIndex.partition f

-- | Creates a selection from a pair of indices.
--
-- The 1st index in the pair represents the leftover set.
-- The 2nd index in the pair represents the selected set.
--
-- Any items that are in both sets are removed from the leftover set.
--
fromIndexPair :: (UTxOIndex, UTxOIndex) -> UTxOSelection
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
toIndexPair :: IsUTxOSelection u => u -> (UTxOIndex, UTxOIndex)
toIndexPair s = (leftoverIndex s, selectedIndex s)

--------------------------------------------------------------------------------
-- Promotion and demotion
--------------------------------------------------------------------------------

-- | Demotes a non-empty selection to an ordinary selection.
--
fromNonEmpty :: UTxOSelectionNonEmpty -> UTxOSelection
fromNonEmpty = UTxOSelection . state

-- | Promotes an ordinary selection to a non-empty selection.
--
-- Returns 'Nothing' if the the selected set is empty.
--
toNonEmpty :: IsUTxOSelection u => u -> Maybe UTxOSelectionNonEmpty
toNonEmpty s = bool Nothing (Just $ fromState $ state s) (isNonEmpty s)

--------------------------------------------------------------------------------
-- Indicator functions
--------------------------------------------------------------------------------

-- | Returns 'True' if and only if the selected set is empty.
--
isEmpty :: IsUTxOSelection u => u -> Bool
isEmpty = (== 0) . selectedSize

-- | Returns 'True' if and only if the selected set is non-empty.
--
isNonEmpty :: IsUTxOSelection u => u -> Bool
isNonEmpty = not . isEmpty

-- | Returns 'True' if the given 'TxIn' is a member of either set.
--
-- Otherwise, returns 'False'.
--
isMember :: IsUTxOSelection s => TxIn -> s -> Bool
isMember i s = isLeftover i s || isSelected i s

-- | Returns 'True' iff. the given 'TxIn' is a member of the leftover set.
--
isLeftover :: IsUTxOSelection s => TxIn -> s -> Bool
isLeftover i = UTxOIndex.member i . leftoverIndex

-- | Returns 'True' iff. the given 'TxIn' is a member of the selected set.
--
isSelected :: IsUTxOSelection s => TxIn -> s -> Bool
isSelected i = UTxOIndex.member i . selectedIndex

-- | Returns 'True' iff. the first selection is a sub-selection of the second.
--
-- A selection 's1' is a sub-selection of selection 's2' if (and only if) it
-- is possible to transform 's1' into 's2' through zero or more applications
-- of the 'select' function.
--
isSubSelectionOf
    :: IsUTxOSelection u1 => IsUTxOSelection u2 => u1 -> u2 -> Bool
isSubSelectionOf u1 u2 = state (selectMany toSelect u1) == state u2
  where
    toSelect :: [TxIn]
    toSelect = fst <$> UTxO.toList
        (selectedUTxO u2 `UTxO.difference` selectedUTxO u1)

-- | Returns 'True' iff. the first selection is a proper sub-selection of the
--   second.
--
-- A selection 's1' is a proper sub-selection of selection 's2' if (and only
-- if) it is possible to transform 's1' into 's2' through one or more
-- applications of the 'select' function.
--
isProperSubSelectionOf
    :: IsUTxOSelection u1 => IsUTxOSelection u2 => u1 -> u2 -> Bool
isProperSubSelectionOf u1 u2 = state u1 /= state u2 && u1 `isSubSelectionOf` u2

--------------------------------------------------------------------------------
-- Accessor functions
--------------------------------------------------------------------------------

-- | Retrieves the balance of leftover UTxOs.
--
leftoverBalance :: IsUTxOSelection u => u -> TokenBundle
leftoverBalance = UTxOIndex.balance . leftoverIndex

-- | Retrieves the size of the leftover UTxO set.
--
leftoverSize :: IsUTxOSelection u => u -> Int
leftoverSize = UTxOIndex.size . leftoverIndex

-- | Retrieves an index of the leftover UTxOs.
--
leftoverIndex :: IsUTxOSelection u => u -> UTxOIndex
leftoverIndex = leftover . state

-- | Retrieves the leftover UTxO set.
--
leftoverUTxO :: IsUTxOSelection u => u -> UTxO
leftoverUTxO = UTxOIndex.toUTxO . leftoverIndex

-- | Retrieves a list of the leftover UTxOs.
--
leftoverList :: IsUTxOSelection u => u -> [(TxIn, TxOut)]
leftoverList = UTxOIndex.toList . leftoverIndex

-- | Retrieves the balance of selected UTxOs.
--
selectedBalance :: IsUTxOSelection u => u -> TokenBundle
selectedBalance = UTxOIndex.balance . selectedIndex

-- | Retrieves the size of the selected UTxO set.
--
selectedSize :: IsUTxOSelection u => u -> Int
selectedSize = UTxOIndex.size . selectedIndex

-- | Retrieves an index of the selected UTxOs.
--
selectedIndex :: IsUTxOSelection u => u -> UTxOIndex
selectedIndex = selected . state

-- | Retrieves the selected UTxO set.
--
selectedUTxO :: IsUTxOSelection u => u -> UTxO
selectedUTxO = UTxOIndex.toUTxO . selectedIndex

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Moves a single entry from the leftover set to the selected set.
--
-- Returns 'Nothing' if the given entry is not a member of the leftover set.
--
select :: IsUTxOSelection u => TxIn -> u -> Maybe UTxOSelectionNonEmpty
select = (toNonEmpty <=<) . withState . selectState

-- | Moves multiple entries from the leftover set to the selected set.
--
selectMany :: IsUTxOSelection u => Foldable f => f TxIn -> u -> u
selectMany = ap fromMaybe . withState . flip (F.foldrM selectState)

--------------------------------------------------------------------------------
-- Modification (Internal)
--------------------------------------------------------------------------------

-- | Moves a single entry from the leftover set to the selected set.
--
selectState :: TxIn -> State -> Maybe State
selectState i s =
    updateFields <$> UTxOIndex.lookup i (leftover s)
  where
    updateFields o = s
        & over #leftover (UTxOIndex.delete i)
        & over #selected (UTxOIndex.insert i o)

-- | Applies the given function to the internal state.
--
withState :: Functor f => IsUTxOSelection u => (State -> f State) -> u -> f u
withState f = fmap fromState . f . state
